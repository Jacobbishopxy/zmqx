# Concurrency / lifecycle performance overhead review

Input note: `/home/xiey/Code/zmqx/plan.md` and `progress.md` were not present, so this review is based on the current source and tests.

## Correct / already good

- Blocking `zmq_poll` uses the interruptible FFI path, with the unsafe FFI path reserved for timeout `0` (`lib/Zmqx/Internal/Functions.hs:623-635`). This avoids pinning a capability during long blocking polls.
- Blocking receives wait on the ZMQ FD via `threadWaitRead` rather than spin-sleeping (`lib/Zmqx/Core/Socket.hs:370-377`).
- The run guard is fail-fast (`tryTakeMVar`) instead of queuing concurrent `run` calls (`lib/Zmqx/Core/Context.hs:154-164`), and `test/RunGuard.hs:28-80` covers nested/concurrent rejection and recovery.

## Prioritized findings

### P1 — Polling rebuilds and reallocates the poll set on every wait iteration

- **Evidence:** `poll_` calls `prepareSockets` at the top of every loop (`lib/Zmqx/Core/Poll.hs:289-290`). `prepareSockets` partitions all requested sockets, builds `Set`s, allocates a new `StorableArray`, maps to `Zmq_pollitem`, and builds a new primitive array (`lib/Zmqx/Core/Poll.hs:161-190`). Ready extraction then scans the storable array and inserts ready sockets into a `Set` (`lib/Zmqx/Core/Poll.hs:202-214`).
- **Likely impact:** High for large poll sets or idle/long-lived polls: O(n) allocation/setup per loop, plus O(k log k) ready-set construction. With REQ sockets this combines with the 10ms slice below, so an idle mixed poll can repeatedly rebuild arrays about 100 times/second.
- **Confidence:** High; the allocation and rebuild path is directly in the hot loop.
- **Confirm/refute metric:** Criterion or standalone benchmark of `pollFor` over N idle sockets (e.g. 10/100/1000) with `+RTS -s` allocation/GC stats and CPU profile; compare against a cached/prepared poll-set variant.

### P1 — REQ input polling forces periodic 10ms wakeups and O(number of REQs) probes

- **Evidence:** `reqProbeSliceUs = 10_000` (`lib/Zmqx/Core/Poll.hs:267-271`). If any input REQ is present, blocking `zmq_poll` timeouts are capped to that slice (`lib/Zmqx/Core/Poll.hs:304-321`), and each loop probes every input REQ via nonblocking `receiveManyDontWait` (`lib/Zmqx/Core/Poll.hs:216-229`, `lib/Zmqx/Core/Poll.hs:323-324`). When only REQs remain to be checked, the loop uses `threadDelay` for the same 10ms slice (`lib/Zmqx/Core/Poll.hs:273-299`).
- **Likely impact:** High for many REQ sockets: idle CPU wakeups, repeated nonblocking recv attempts, MVar lock traffic, and valid-reply latency quantized by the probe cadence in some paths.
- **Confidence:** High for overhead; latency impact depends on workload and libzmq wakeup behavior.
- **Confirm/refute metric:** Measure idle CPU/context-switches and number of recv probes/sec while polling 1/10/100 idle REQs; measure p50/p99 time from valid reply send to `pollFor` readiness. The current `ReqPoll` tests use 100ms delayed replies and 1000ms timeouts (`test/ReqPoll.hs:82-100`, `test/ReqPoll.hs:122-140`), so they validate correctness but not overhead.

### P2 — Socket finalizer registry compaction is O(n) and can amplify under high churn

- **Evidence:** Every socket open registers a finalizer by prepending to a per-context `IORef` list (`lib/Zmqx/Core/Socket.hs:137-145`). Each weak finalizer runs `idempotentClose >> compactSocketFinalizers` (`lib/Zmqx/Core/SocketFinalizer.hs:39-47`). Compaction atomically swaps out the whole list, filters it, then appends survivors back (`lib/Zmqx/Core/SocketFinalizer.hs:55-64`), and liveness is checked by reading each finalizer's `closedRef` (`lib/Zmqx/Core/SocketFinalizer.hs:66-68`). `pendingSockets` also compacts before counting (`lib/Zmqx/Core/Context.hs:149-152`).
- **Likely impact:** Medium-to-high for socket churn or many sockets: parallel opens contend on one `IORef`; GC finalizers and diagnostics scan the whole registry; many weak finalizers firing individually can produce repeated full-list scans.
- **Confidence:** High for O(n) scans/contention; medium for worst-case amplification because it depends on GC/finalizer scheduling.
- **Confirm/refute metric:** Open/drop 1k/10k sockets and record total GC/finalizer time, `pendingSockets` latency, and parallel open throughput. Existing finalizer tests use small counts (32 dead sockets, 64 live sockets: `test/FinalizerRegistry.hs:53-95`) and focus on correctness/interruption safety, not scaling.

### P2 — Strict uninterruptible context teardown can create long, uncancellable shutdown tails

- **Evidence:** Both global `run` and explicit `withContext` perform cleanup under `uninterruptibleMask_` (`lib/Zmqx/Core/Context.hs:103-105`, `lib/Zmqx/Core/Context.hs:138-141`). Teardown shuts down the context, compacts finalizers, runs all registered socket finalizers, clears the registry, and loops on `zmq_ctx_term` until success (`lib/Zmqx/Core/Context.hs:182-210`). The module comments explicitly note that child threads using sockets must be stopped by the caller or termination may block (`lib/Zmqx/Core/Context.hs:117-122`, `lib/Zmqx/Core/Context.hs:173-181`).
- **Likely impact:** Medium in normal cases, high in services with many sockets or leaked/blocked worker threads: shutdown latency grows with finalizer count and cannot be interrupted while inside the cleanup section.
- **Confidence:** High for cancellation/tail-latency risk; workload-dependent for observed severity.
- **Confirm/refute metric:** Teardown latency vs socket count; async-exception delivery latency while teardown is running; scenario with a child thread still using a socket at context exit.

### P3 — Per-socket `MVar` is a hidden serialization point

- **Evidence:** Each socket gets an `MVar` lock (`lib/Zmqx/Core/Socket.hs:137-145`), and `usingSocket` wraps operations with `withMVar` (`lib/Zmqx/Core/Socket.hs:167-170`). Bind/connect/send/receive paths use this wrapper (`lib/Zmqx/Core/Socket.hs:172-224`, `lib/Zmqx/Core/Socket.hs:328-340`), and REQ poll probing also reaches `receiveManyDontWait` (`lib/Zmqx/Core/Poll.hs:216-229`).
- **Likely impact:** Medium when multiple Haskell threads share one socket or when a poller probes a socket concurrently with application sends/receives. This may be required for safety because ZMQ sockets are not generally thread-safe, but it is still a scalability ceiling.
- **Confidence:** High that serialization exists; medium on impact because correct usage may already be one owner thread per socket.
- **Confirm/refute metric:** ThreadScope/eventlog blocked-on-MVar time and throughput as producer/consumer thread count per socket increases; compare with one-socket-per-thread design.

### P3 — `monitor` creates a hidden PAIR socket with no explicit lifecycle boundary in the returned API

- **Evidence:** `monitor` generates a unique inproc endpoint, enables `ZMQ_EVENT_ALL`, opens an internal PAIR socket in the same context, connects it, and returns only a `receiveEvent` action that closes over that PAIR (`lib/Zmqx/Core/Monitor.hs:34-55`). Unknown/malformed events are skipped by recursive receive (`lib/Zmqx/Core/Monitor.hs:47-54`).
- **Likely impact:** Low-to-medium normally, higher if monitors are created repeatedly or retained for many sockets: each monitor consumes a socket/finalizer entry until the closure is GC'd or the context tears down, and high event volume pays decode/recursive filtering overhead.
- **Confidence:** Medium; lifecycle cost depends on caller retention and GC timing.
- **Confirm/refute metric:** Track `pendingSockets`/socket count after creating and dropping/retaining N monitors; measure monitor event throughput and allocation under high connection churn. Current `MonitorEvent` only tests decode behavior (`test/MonitorEvent.hs:1-38`), not monitor socket lifecycle.

## Test coverage gaps for these overheads

- Poll tests cover correctness on small sets (`test/ItemsPollAuto.hs:35-60`) but not large poll sets, idle CPU, or allocation rates.
- `ReqPoll` covers stale/valid REQ behavior but uses coarse sleeps/timeouts (`test/ReqPoll.hs:82-100`, `test/ReqPoll.hs:122-140`), not probe cadence overhead.
- `RunGuard` covers guard correctness but not shutdown cost with many sockets or live child threads (`test/RunGuard.hs:28-80`).
- Finalizer tests cover GC/interruption safety at small portable counts, not registry scaling (`test/FinalizerRegistry.hs:53-95`).
