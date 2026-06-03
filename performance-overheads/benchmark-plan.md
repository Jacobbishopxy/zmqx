# Performance Measurement Coverage Review and Benchmark Plan

## Scope and evidence checked

- Requested `/home/xiey/Code/zmqx/plan.md` and `/home/xiey/Code/zmqx/progress.md` were not present, so this review is based on the repository state directly.
- Cabal/package structure: `cabal.project`, `zmqx.cabal`, `test/test.cabal`.
- Scenario and regression executables under `test/`.
- Performance-related docs/comments in `README.md`, `docs/quickstart.md`, `docs/examples.md`, `lib/Zmqx/EventLoop.hs`, and `lib/Zmqx/Core/Poll.hs`.

## What is already good

- The repository has broad correctness coverage across the important messaging shapes: Req/Rep, Dealer/Router, Pub/Sub, broker/proxy, polling, EventLoop send/receive/transceiver/safety, RunGuard, and `Zmqx.Monad` are registered as test suites in `test/test.cabal:249-348`.
- The tests mostly use `inproc://` endpoints, which is a good foundation for repeatable microbenchmarks once iteration counts and timing are added; examples include `test/ReqRepAuto.hs:25`, `test/DealerRouterAuto.hs:21`, and `test/ItemsPollAuto.hs:65-66`.
- The EventLoop and poll implementation explicitly document/encode potential latency sources that should be measured: worker-owned sockets and mailbox/callback semantics (`lib/Zmqx/EventLoop.hs:19-25`, `lib/Zmqx/EventLoop.hs:119-130`), receiver polling slices (`lib/Zmqx/EventLoop.hs:445-457`), send retry/wait loops (`lib/Zmqx/EventLoop.hs:497-516`, `lib/Zmqx/EventLoop.hs:576-589`), and REQ-specific poll probing (`lib/Zmqx/Core/Poll.hs:143-158`, `lib/Zmqx/Core/Poll.hs:267-287`).

## Prioritized measurement gaps

### P0 — No benchmark infrastructure or baseline comparisons

- `cabal.project` only includes the library and `test/` packages (`cabal.project:1-2`).
- `zmqx.cabal` defines a library and ends at the `libzmq` pkg-config dependency; there is no `benchmark` stanza or benchmark dependency (`zmqx.cabal:36-115`).
- `test/test.cabal` defines only `test-suite` executables (`test/test.cabal:41-348`) and its shared dependencies do not include `criterion`, `tasty-bench`, `gauge`, `weigh`, `zeromq4-haskell`, or a dedicated baseline package (`test/test.cabal:18-35`).
- No existing executable compares `zmqx` against direct `zeromq4-haskell` or raw `libzmq` usage under the same transport, socket options, and payload dimensions.

Expected risk: regressions in wrapper overhead, allocations, FFI path cost, or EventLoop indirection can pass the full test suite because correctness tests do not record latency/throughput.

### P0 — Current scenario tests hide latency and throughput overheads

The automated scenario tests mostly send one or a few messages with large correctness timeouts:

- Req/Rep: one request and one reply (`test/ReqRepAuto.hs:33-39`).
- Dealer/Router: one multipart request and one reply (`test/DealerRouterAuto.hs:30-40`).
- Pub/Sub: one filtered-out message and one delivered message, plus a fixed 100 ms sleep (`test/PubSubAuto.hs:30-38`).
- Broker/proxy: single forwarded request/reply or publication with fixed sleeps (`test/BrokerAuto.hs:41-65`, `test/ProxyAuto.hs:42-51`).
- Task pipeline: only three workloads (`test/TaskPipelineAuto.hs:22-47`).
- Polling: two sockets and two messages, with wide `pollFor` windows (`test/ItemsPollAuto.hs:35-60`, `test/ItemsPollAuto.hs:79-88`; `test/PollOut.hs:38-51`).

Expected risk: per-message overhead, batching behavior, fairness under many sockets/clients, slow subscriber warmup, high-water-mark drops, GC pressure, and tail latency are invisible.

### P0 — EventLoop hot-path overhead is unmeasured

EventLoop adds queues, STM/MVar synchronization, mailbox delivery, optional callbacks, and bounded retry/sleep loops:

- Receiver polling is capped at 10 ms slices (`lib/Zmqx/EventLoop.hs:445-457`).
- Shutdown-aware sends retry every 1 ms on `EAGAIN` (`lib/Zmqx/EventLoop.hs:497-516`).
- Public send waits poll an MVar/worker result and sleep 1 ms between checks (`lib/Zmqx/EventLoop.hs:576-589`).
- Positive-timeout `recv` loops with monotonic-clock checks and sleeps up to 1 ms (`lib/Zmqx/EventLoop.hs:634-680`).
- Mailbox delivery drops newest messages when full, but there is no measured drop-rate/backpressure profile (`lib/Zmqx/EventLoop.hs:468-479`).

Expected risk: an EventLoop path may be semantically correct but add millisecond-scale tail latency compared with direct socket use, especially under contention or when callbacks are slow.

### P1 — Polling and REQ probe behavior lack scaling measurements

`Zmqx.Core.Poll` rebuilds prepared poll arrays/sets per call and applies special REQ probing:

- Poll set preparation partitions sockets and allocates/constructs arrays (`lib/Zmqx/Core/Poll.hs:161-190`).
- REQ input polling may probe directly and sleeps/retries in 10 ms slices (`lib/Zmqx/Core/Poll.hs:216-235`, `lib/Zmqx/Core/Poll.hs:267-287`).
- The correctness test covers stale replies and direct polling but with one or two REQ sockets and fixed delays/timeouts (`test/ReqPoll.hs:20-22`, `test/ReqPoll.hs:38-50`, `test/ReqPoll.hs:82-100`, `test/ReqPoll.hs:133-145`).

Expected risk: poll overhead may grow nonlinearly with socket count or REQ mix, and stale-reply mitigation may introduce avoidable latency.

### P1 — RunGuard and monad-style overheads are asserted but not quantified

Docs position the direct API as lower abstraction overhead (`README.md:28-32`, `docs/quickstart.md:25-35`) and the monad API as same runtime semantics (`README.md:42-48`, `docs/quickstart.md:167-168`). Current tests only check behavior:

- `RunGuard` checks nested/concurrent rejection and recovery, not guard-entry overhead or contention cost (`test/RunGuard.hs:28-94`).
- `ZmqxMonad` performs one pair round-trip per style/stack (`test/ZmqxMonad.hs:45-63`, `test/ZmqxMonad.hs:67-93`).

Expected risk: repeated `run`/`withContext` setup, transformer lifting, and global guard contention could be significant in small-message workloads without being visible.

### P2 — Lifecycle/finalizer and docs have no performance signal

- Finalizer tests exercise 32/64 sockets for correctness and interruption safety (`test/FinalizerRegistry.hs:47-67`, `test/FinalizerRegistry.hs:88-131`) but do not measure open/close throughput, GC time, or registry compaction cost.
- Docs list test commands and example coverage (`README.md:64-71`, `docs/examples.md:63-95`) but no benchmark command, baseline, or performance caveat.

## Recommended benchmark layout

Add a dedicated benchmark area rather than overloading correctness tests:

1. `bench/` or a `benchmark` stanza in `zmqx.cabal` for stable benchmarks.
2. A custom high-throughput harness for multi-threaded/end-to-end scenarios.
3. Optional baseline targets:
   - `zeromq4-haskell` baseline behind a Cabal flag if compatible with the active GHC.
   - Raw `libzmq` baseline via a small C executable or direct C FFI harness using the same endpoints/options.

Keep correctness tests unchanged; benchmarks should be opt-in and not part of `cabal test all`.

## Recommended microbenchmarks

### 1. Direct API send/receive baseline

Compare `zmqx` direct API vs `zeromq4-haskell` and raw `libzmq`:

- Socket pairs: PAIR/PAIR, REQ/REP, PUSH/PULL.
- Transports: `inproc://`, `ipc://`, `tcp://127.0.0.1`.
- Payload sizes: 0 B, 16 B, 64 B, 1 KiB, 64 KiB, 1 MiB.
- Multipart frames: 1, 2, 8, 32 frames.
- Signals: one-way latency, round-trip latency, messages/sec, MB/sec, allocations/message, CPU/message.

### 2. Req/Rep and REQ relaxed/correlate polling

Build from the existing stale-reply scenario (`test/ReqPoll.hs:20-22`):

- Normal ping-pong RTT vs stale-reply path.
- `receivesFor` vs `pollFor` + `receives`.
- One REQ vs many REQs in one poll set.
- Timeout values: 0 ms, 1 ms, 10 ms, 100 ms, negative/blocking.
- Signals: p50/p95/p99 RTT, timeout overshoot, stale-reply recovery time, CPU while waiting.

### 3. Dealer/Router multipart routing

Build from `test/DealerRouterAuto.hs:30-40`:

- N dealers: 1, 8, 64, 256.
- Multipart sizes: route id + 1/4/16 payload frames.
- Router echo vs broker forward.
- Signals: throughput, fairness per dealer, p99 latency, allocation/GC rate.

### 4. Pub/Sub fanout and filtering

Build from `test/PubSubAuto.hs:26-38` and `test/ProxyAuto.hs:31-51`:

- Subscribers: 1, 8, 64.
- Filters: one matching filter, many disjoint filters, empty filter.
- Warmup strategy: explicit subscription handshake where possible, not fixed 100 ms sleeps.
- High-rate publisher with configurable HWM.
- Signals: delivery throughput, slow-joiner loss, drop count, p99 subscriber lag.

### 5. Poll API scaling

Build from `test/ItemsPollAuto.hs:79-88` and `test/PollOut.hs:38-51`:

- Poll sets: 1, 8, 64, 512 sockets.
- Ready ratios: 0%, 1 ready, 50%, 100%.
- Mixed `POLLIN`/`POLLOUT` and mixed REQ/non-REQ sets.
- Signals: `pollFor 0` cost, ready detection latency, array/set allocation, timeout accuracy.

### 6. EventLoop send/receive/mailbox/callback overhead

Build from EventLoop correctness tests (`test/EventLoopSendAuto.hs:90-120`, `test/EventLoopReceiveAuto.hs:52-103`, `test/EventLoopTransceiverAuto.hs:35-75`):

- Direct `Push.send` to `Pull.receive` vs `EventLoop.send` to the same topology.
- Direct receive vs EventLoop mailbox `recv`.
- Callback delivery with no-op callback, cheap MVar callback, and deliberately slow callback.
- Mailbox capacities: 1, 4, 1024; producers faster than consumers.
- Concurrent senders: 1, 2, 8, 32 threads.
- Signals: enqueue-to-send latency, receive-to-mailbox latency, p99/p999 tail, drops when full, worker CPU, contention/context switches.

### 7. Monad and RunGuard overhead

Build from `test/ZmqxMonad.hs:45-93` and `test/RunGuard.hs:28-94`:

- `withContext` direct vs `runZmqxT ctx` vs `runZmqx` vs a `ReaderT` stack over `ZmqxT`.
- Measure repeated `open/bind/connect/send/receive` setup and steady-state send/receive inside already-open sockets.
- Measure `run` entry/exit and failed concurrent `run` rejection under 1/8/32 competing threads.
- Signals: ns/op for wrapper functions, setup/teardown time, contention time, allocations.

### 8. Lifecycle/finalizer overhead

Build from `test/FinalizerRegistry.hs:47-67` and `test/FinalizerRegistry.hs:88-131`:

- Open/close socket throughput for 1, 32, 256, 1024 sockets under explicit close/GC finalization.
- Registry compaction time after major GC.
- Signals: sockets/sec, GC pause, pending registry length over time, allocation retained bytes.

## Recommended end-to-end benchmarks

1. **Req/Rep service RTT and throughput**
   - One client/server and N clients/servers.
   - Compare direct `zmqx`, EventLoop transceiver, `zeromq4-haskell`, and raw `libzmq`.
   - Expected signal: wrapper overhead ratio and p99 latency under request concurrency.

2. **Dealer/Router broker**
   - N clients, M workers, broker forwarding all frames.
   - Include direct broker loop and EventLoop-managed broker if feasible.
   - Expected signal: broker throughput, fairness, queue buildup, routing overhead.

3. **Pub/Sub fanout/proxy**
   - Publisher through optional XSub/XPub proxy to subscribers.
   - Expected signal: max sustainable publication rate, subscriber lag/loss, filter overhead.

4. **Push/Pull task pipeline**
   - Replace the current three-message correctness workload (`test/TaskPipelineAuto.hs:22-47`) with configurable messages and workers.
   - Run with zero simulated work to expose messaging overhead, then with controlled synthetic work to check amortization.
   - Expected signal: pipeline throughput, worker balancing, overhead fraction when work is small.

5. **EventLoop trader-style workflow**
   - Make the finite `TraderDemoFrame` path configurable by order count and broadcast interval.
   - Expected signal: command ack latency, report publication latency, mailbox/callback tail latency under concurrent order flow.

## Commands and tools

After adding benchmark targets, use commands in this style:

```sh
# Build optimized benchmarks
cabal build --enable-benchmarks --ghc-options='-O2 -rtsopts -with-rtsopts=-T'

# Criterion/tasty-bench style microbenchmarks
cabal bench zmqx:bench-zmqx-overheads --benchmark-options '--csv bench-results.csv --output bench-results.html'

# Custom throughput harness examples
cabal run zmqx-throughput -- --pattern req-rep --transport inproc --messages 1000000 --payload 64 --clients 1
cabal run zmqx-throughput -- --pattern dealer-router --transport tcp --messages 1000000 --clients 64 --workers 8
cabal run zmqx-throughput -- --pattern event-loop-mailbox --messages 1000000 --payload 64 --senders 8 --mailbox-capacity 1024

# RTS allocation/GC summaries
cabal run zmqx-throughput -- --pattern req-rep --messages 1000000 +RTS -s -T -A64m -N -RTS

# Time/profile hot paths
cabal run zmqx-throughput --enable-profiling -- --pattern event-loop-send +RTS -p -hy -RTS

# Eventlog/thread scheduling analysis
cabal run zmqx-throughput -- --pattern event-loop-send +RTS -l-au -N -RTS
# then inspect with eventlog2html or ThreadScope

# Linux CPU/context-switch counters
perf stat -e cycles,instructions,cache-misses,context-switches cabal run zmqx-throughput -- --pattern poll --sockets 512 --ready-ratio 0.01
```

Recommended libraries/tools:

- `criterion` or `tasty-bench` for small, stable microbenchmarks.
- A custom harness using `GHC.Clock.getMonotonicTimeNSec` for multi-threaded throughput/tail-latency runs.
- RTS `+RTS -s -T`, heap/profile/eventlog options, `eventlog2html`, ThreadScope, and `perf stat` for CPU/context-switch evidence.
- Persist raw outputs as CSV/JSON with metadata: git commit, GHC version, libzmq version, CPU model, OS, transport, socket options, RTS flags.

## Success criteria / expected signal

For each benchmark, report at least:

- Throughput: messages/sec and MB/sec.
- Latency: mean, p50, p95, p99, and max/p999 for long runs.
- Cost: allocations/message, CPU cycles/message, context switches/sec.
- Correctness-adjacent counters: timeout overshoot, retries/EAGAIN, dropped mailbox messages, Pub/Sub lost messages after warmup.
- Baseline ratio: `zmqx direct / raw libzmq`, `zmqx direct / zeromq4-haskell`, `EventLoop / zmqx direct`, and `Zmqx.Monad / withContext direct`.

Initial practical thresholds can be informational rather than gating CI. Once stable, add optional CI perf smoke runs with broad regression alarms, e.g. >20% slowdown in direct API microbenchmarks or >2x tail-latency increase in EventLoop send/recv compared with the previous recorded baseline.
