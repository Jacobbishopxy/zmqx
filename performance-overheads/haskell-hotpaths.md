# Code Context

## Files Retrieved
1. `lib/Zmqx/Core/Socket.hs` (lines 76-128, 137-170, 203-377, 475-593, 615-650) - central send/receive, locking, blocking, frame and debug paths.
2. `lib/Zmqx/Internal/Functions.hs` (lines 163-218, 266-293, 411-456, 566-637) - ByteString/FFI copying, send wrappers, socket options, poll wrapper.
3. `lib/Zmqx/Internal/Bindings/Functions.hs` (lines 135-157, 221-255, 270-278) - safe/unsafe/interruptible FFI imports for recv/send/poll.
4. `lib/Zmqx/Core/Poll.hs` (lines 86-110, 143-235, 262-366) - public poll-set representation and polling loop.
5. `lib/Zmqx/Req.hs` (lines 128-223) - REQ send/receive buffering and timeout path.
6. `lib/Zmqx/Dealer.hs` (lines 111-167) - representative role API send/receive/receivesFor pattern used across roles.
7. `lib/Zmqx/EventLoop.hs` (lines 445-516, 561-681) - public event-loop hot paths.
8. `lib/Zmqx/Error.hs` (lines 26-43) - normal error representation and exception-based short circuiting.
9. `lib/Zmqx/Monad.hs` (lines 49-126) - ReaderT/liftIO wrapper API.
10. `lib/Zmqx/Core/Context.hs` (lines 45-113, 154-199) and `lib/Zmqx/Core/SocketFinalizer.hs` (lines 31-84) - context/socket lifecycle bookkeeping.
11. `zmqx.cabal` (lines 36-106) - exposed modules/dependencies/options; no benchmark stanza found.

## Key Code

### Prioritized likely overheads

1. **Receive path copies every frame and allocates/frees a `zmq_msg_t` per frame** — High confidence.  
   `receiveOne/receiveMany` repeatedly call `receive*DontWait` (`lib/Zmqx/Core/Socket.hs:282-340`); each received frame goes through `zhs_with_frame = bracket zmq_msg_init ... zmq_msg_close; zmq_msg_free` (`lib/Zmqx/Core/Socket.hs:529-586`). `zmq_msg_init` itself `malloc`s (`lib/Zmqx/Internal/Functions.hs:197-201`), and `zmq_msg_data` returns a fresh Haskell `ByteString` using `ByteString.packCStringLen`, copying payload bytes (`lib/Zmqx/Internal/Functions.hs:163-167`). Multipart receive recurses and builds lists (`lib/Zmqx/Core/Socket.hs:344-357`).  
   **Why it matters:** allocation/free + payload copy dominate small-message throughput and increase allocation proportional to bytes received.  
   **Benchmark/profile:** inproc PAIR or PUSH/PULL: receive-only and roundtrip for 0/8/64/1K/64K-byte frames, single vs 8-frame messages; collect `+RTS -s`, heap profile by cost centre around `zmq_msg_data`/`zhs_with_frame`.

2. **Every socket operation takes an `MVar` lock and `keepAlive`; multipart sends/receives add `mask_` around frame loops** — High confidence.  
   Socket records carry `lock :: MVar ()` (`lib/Zmqx/Core/Socket.hs:76-82`), and `usingSocket` wraps operations in `withMVar` (`lib/Zmqx/Core/Socket.hs:167-170`). Send paths call it per public send/multipart send (`lib/Zmqx/Core/Socket.hs:203-240,254-278`); receive paths similarly (`lib/Zmqx/Core/Socket.hs:295-340`). Multipart sends wrap first+rest in `mask_` (`lib/Zmqx/Core/Socket.hs:227-267`).  
   **Why it matters:** uncontended `MVar` is still atomic synchronization; it serializes all access and can become a bottleneck if callers share sockets. Masking has small but measurable per-message overhead.  
   **Benchmark/profile:** direct role API microbench with one producer/consumer vs multiple Haskell threads sharing a socket; compare single-frame vs multipart; inspect eventlog for MVar contention.

3. **Blocking send/receive loops use nonblocking FFI plus readiness waits and socket-option probes** — High confidence.  
   Dealer/Req-style sends first try `sendOneDontWait`/`sendManyDontWait`; on `EAGAIN` they call `blockUntilCanSend` and loop (`lib/Zmqx/Dealer.hs:111-133`, `lib/Zmqx/Req.hs:128-153`). `blockUntilEvent` fetches `ZMQ_FD`, waits with `threadWaitRead`, then repeatedly calls `ZMQ_EVENTS` until the desired bit is set (`lib/Zmqx/Core/Socket.hs:360-377`).  
   **Why it matters:** backpressure path includes scheduler wakeups and extra `getsockopt` FFI calls; good for nonblocking correctness but can add latency under HWM pressure.  
   **Benchmark/profile:** force HWM pressure with small `sendQueueSize`, slow receiver, and measure tail latency/CPU for direct blocking roles; compare Router fallback behavior (`Socket.sendMany` uses blocking `zmq_send`) where applicable.

4. **`poll` rebuilds arrays/sets on every call and returns a closure over a `Set`** — High confidence.  
   `Sockets` stores a Haskell list (`lib/Zmqx/Core/Poll.hs:86-92`). `prepareSockets` partitions into `Set`s, computes `length`, builds a `StorableArray` with `newListArray`, and a primitive array with `arrayFromListN` every poll (`lib/Zmqx/Core/Poll.hs:143-190`). Ready sockets are accumulated in `Set SomeSocket` (`lib/Zmqx/Core/Poll.hs:202-235`) and exposed as `Ready (forall a. Socket a -> Bool)` (`lib/Zmqx/Core/Poll.hs:117-119,352-354`). `zmq_poll` then calls `getBounds`/`withStorableArray` each time (`lib/Zmqx/Internal/Functions.hs:623-635`).  
   **Why it matters:** high allocation for tight polling loops; membership checks are tree lookups, not direct array/index tests. Single-socket `receivesFor` in role modules pays this setup just to perform a timeout receive (`lib/Zmqx/Dealer.hs:156-167`).  
   **Benchmark/profile:** poll N={1,8,64,512} sockets with timeout 0 and no readiness; poll one ready among N; measure allocations with `+RTS -s`; compare `receivesFor socket 0` vs `receiveManyDontWait`.

5. **REQ-specific buffering/probing adds IORef traffic, periodic 10ms wakeups, and extra receives** — High confidence.  
   REQ stores `IORef (Maybe NonEmpty ByteString)` in `Extra` (`lib/Zmqx/Core/Socket.hs:95-104`). Normal receives read/write it (`lib/Zmqx/Req.hs:158-180`). Poll preparation/probing reads and writes those buffers and may call `receiveManyDontWait` under `mask_` (`lib/Zmqx/Core/Poll.hs:181-235`). Poll loops cap waits to `reqProbeSliceUs = 10_000` and sleep/retry when only REQs are involved (`lib/Zmqx/Core/Poll.hs:267-323`). `Req.receivesFor` uses `System.Timeout.timeout` for positive timeouts (`lib/Zmqx/Req.hs:187-223`).  
   **Why it matters:** correctness workaround for correlated REQ replies trades throughput/latency for polling, timers, and IORef allocation.  
   **Benchmark/profile:** REQ/REP ping-pong with `pollFor`/`receivesFor` at timeout 0, 1ms, 10ms, negative; measure p50/p99 latency and allocation; include stale/correlated reply scenarios if possible.

6. **Normal public operations use exception machinery to return expected `Left Error`s** — Medium confidence.  
   `catchingOkErrors` is `try` around every public operation; `throwOkError` throws an `OkError` for nonfatal errors (`lib/Zmqx/Error.hs:34-43`). Lower-level wrappers enrich and throw on common conditions such as `EAGAIN`, `ETERM`, `EINTR` (`lib/Zmqx/Core/Socket.hs:491-522`, `lib/Zmqx/Core/Poll.hs:356-366`).  
   **Why it matters:** success-path `try` overhead is small but present; error-heavy paths pay exception allocation/unwinding and `zmq_strerror`/Text construction.  
   **Benchmark/profile:** compare success-only loops to forced `EAGAIN`/`ETERM` loops; heap profile errors separately from success path.

7. **EventLoop API adds STM/MVar command round-trips and millisecond polling sleeps** — High confidence for `Zmqx.EventLoop` users.  
   Public `sends` allocates a reply `MVar`, writes a `TQueue`, then busy-waits with `tryTakeMVar`, `tryReadMVar`, and `threadDelay 1000` (`lib/Zmqx/EventLoop.hs:561-589`). Worker send retries every 1ms (`lib/Zmqx/EventLoop.hs:497-516`). Receiver polling uses `Poll.pollFor` with a 10ms slice and traverses all receiver runtimes to test readiness (`lib/Zmqx/EventLoop.hs:445-462`); mailbox reads with positive timeout poll/sleep up to 1ms (`lib/Zmqx/EventLoop.hs:634-681`).  
   **Why it matters:** safer shutdown semantics but high latency/allocation compared with direct socket calls; callback/mailbox delivery introduces STM overhead.  
   **Benchmark/profile:** direct `Dealer.sends/receives` vs `EventLoop.sends/recv`; mailbox vs callback; 1, 16, 128 endpoints; record latency histograms and allocation.

8. **Lifecycle management is allocation-heavy for rapid open/close workloads** — Medium confidence.  
   `openSocketIn` allocates an `MVar`, weak finalizer, IORefs and updates a finalizer list (`lib/Zmqx/Core/Socket.hs:137-154`; `lib/Zmqx/Core/SocketFinalizer.hs:31-47,70-84`). Compaction drains and appends lists under `mask` (`lib/Zmqx/Core/SocketFinalizer.hs:55-64`). Context termination compacts, reverses, and runs all finalizers (`lib/Zmqx/Core/Context.hs:182-198`).  
   **Why it matters:** not per-message, but expensive for tests/services that churn sockets or contexts.  
   **Benchmark/profile:** open/connect/close 1K sockets inside one context and with repeated contexts; profile `compactSocketFinalizers` and list retention.

9. **Abstraction wrappers may survive in polymorphic hot loops without INLINE/specialisation** — Medium/low confidence.  
   Public `Zmqx.send/receive` dispatch through typeclasses (`lib/Zmqx/Core/Socket.hs:109-128`; `lib/Zmqx.hs:144-162`), and `Zmqx.Monad` wraps all calls in `ReaderT`/`liftIO` (`lib/Zmqx/Monad.hs:49-126`). No `INLINE` pragmas were seen on these thin wrappers.  
   **Why it matters:** usually optimized away at monomorphic call sites, but dictionary passing and ReaderT/liftIO overhead can show in extremely small-message tight loops.  
   **Benchmark/profile:** compare role-specific `Zmqx.Dealer.send` vs polymorphic `Zmqx.send` vs `Zmqx.Monad.send` in identical loops; inspect Core (`-ddump-simpl`) for inlining.

10. **Debug mode is intentionally very expensive per frame** — High confidence when built with `--flag debug`.  
    `debugPrintFrames` formats all bytes via `ByteString.unpack`, Text builders, UTF-8 decode, and a global stderr `MVar` (`lib/Zmqx/Core/Socket.hs:615-650`).  
    **Why it matters:** dominates throughput and serializes logging; benchmark only with debug disabled unless measuring tracing.  
    **Benchmark/profile:** run all throughput numbers without `--flag debug`; if debug is needed, benchmark separately.

## Architecture

Public role modules (`Req`, `Dealer`, `Rep`, `Router`, `Pub`, `Sub`, etc.) mostly delegate to `Zmqx.Core.Socket` for frame send/receive and to `Zmqx.Core.Poll` for timeout receive helpers. `Zmqx.hs` re-exports polymorphic typeclass wrappers; `Zmqx.Monad` adds a `ReaderT Context` API. `Zmqx.Internal.Functions` wraps the C FFI imports from `Zmqx.Internal.Bindings.Functions`, converting return codes to `Either Zmq_error` and copying data into/out of Haskell `ByteString`s. Context/socket lifetime is managed with global/explicit contexts plus weak socket finalizers.

## Start Here

Start at `lib/Zmqx/Core/Socket.hs`, especially lines 203-340 and 529-593: this is the shared hot path for nearly every public socket API and explains most allocation/locking/copying behavior.

## Supervisor coordination

No blocking decisions. Suggested next step is to add a small `criterion`/`tasty-bench` benchmark suite or standalone benchmark executable using `inproc://` sockets, built with `-O2 -rtsopts`, and run with `+RTS -s` plus selected heap/cost-centre profiles.