# TP-012: Reduce EventLoop coordination latency — Status

**Current Step:** Step 5: Documentation & Delivery
**Status:** ✅ Complete
**Last Updated:** 2026-06-02
**Review Level:** 2
**Review Counter:** 8
**Iteration:** 2
**Size:** M

> **Hydration:** Checkboxes represent meaningful outcomes, not individual code changes. Workers expand steps when runtime discoveries warrant it.

---

### Step 0: Preflight
**Status:** ✅ Complete

- [x] Required files and paths exist
- [x] Existing EventLoop targeted tests pass
- [x] Baseline EventLoop benchmark captured

---

### Step 1: EventLoop wait strategy plan
**Status:** ✅ Complete

- [x] Current wait paths mapped
- [x] Replaceable polling sleeps identified
- [x] Shutdown/failure/timeout invariants defined
- [x] Benchmark/test evidence defined

---

### Step 2: Implement lower-latency coordination
**Status:** ✅ Complete

- [x] Avoidable public wait polling sleeps replaced
- [x] Worker retry sleep overhead reduced where safe
- [x] Mailbox/callback semantics preserved
- [x] Startup validation preserved
- [x] Poll timeout rounding avoids busy spin with 1ms EventLoop slice
- [x] Positive mailbox timeout avoids leaked sleeper threads on early completion

---

### Step 3: Add EventLoop latency regression coverage and benchmark evidence
**Status:** ✅ Complete

- [x] EventLoop latency/safety test added
- [x] Test registered in `test/test.cabal`
- [x] EventLoop benchmark evidence captured
- [x] Performance docs updated if commands changed

---

### Step 4: Testing & Verification
**Status:** ✅ Complete

- [x] Targeted EventLoop tests passing
- [x] EventLoop benchmark smoke with RTS summary run
- [x] FULL test suite passing
- [x] Build passes
- [x] All failures fixed

---

### Step 5: Documentation & Delivery
**Status:** ✅ Complete

- [x] "Must Update" docs modified if needed
- [x] "Check If Affected" docs reviewed
- [x] Discoveries logged
- [x] Remaining EventLoop tradeoffs logged if needed

---

## Reviews

| # | Type | Step | Verdict | File |
|---|------|------|---------|------|
| R001 | plan | Step 1 | APPROVE | `.reviews/R001-plan-step1.md` |
| R002 | plan | Step 2 | UNAVAILABLE | *(no file; reviewer produced no output)* |
| R003 | code | Step 2 | REVISE | `.reviews/R003-code-step2.md` |
| R004 | code | Step 2 | APPROVE | `.reviews/R004-code-step2.md` |
| R005 | plan | Step 3 | APPROVE | `.reviews/R005-plan-step3.md` |
| R006 | code | Step 3 | APPROVE | `.reviews/R006-code-step3.md` |
| R007 | plan | Step 4 | APPROVE | `.reviews/R007-plan-step4.md` |
| R008 | code | Step 4 | APPROVE | `.reviews/R008-code-step4.md` |

---

## Discoveries

| Discovery | Disposition | Location |
|-----------|-------------|----------|
| `performance-overheads/` report directory is absent in this worktree; prompt marked those report files as optional "if present", so EventLoop context comes from `docs/performance.md`, `taskplane-tasks/CONTEXT.md`, source, tests, and benchmarks. | Informational | Preflight |
| `Control.Concurrent.STM.registerDelay` throws `registerDelay: requires -threaded` in this Cabal test configuration, so EventLoop positive mailbox waits use a pre-check plus bracketed cancellable `forkIO` timer instead of `registerDelay`. | Implemented | `lib/Zmqx/EventLoop.hs` |
| `Poll.pollFor` floored sub-millisecond remaining deadlines to `0`, which made a 1ms EventLoop receiver slice spin until its deadline; timeout rounding now blocks for at least 1ms while the deadline is in the future. | Implemented | `lib/Zmqx/Core/Poll.hs` |

---

## Execution Log

| Timestamp | Action | Outcome |
|-----------|--------|---------|
| 2026-06-02 | Task staged | PROMPT.md and STATUS.md created |
| 2026-06-02 16:41 | Task started | Runtime V2 lane-runner execution |
| 2026-06-02 16:41 | Step 0 started | Preflight |
| 2026-06-03 | Preflight EventLoop targeted tests | `cabal test test-event-loop-send-auto test-event-loop-receive-auto test-event-loop-transceiver-auto test-event-loop-safety-auto` passed (4/4) before changes. |
| 2026-06-03 | Baseline EventLoop benchmark | `cabal run --enable-optimization=2 zmqx-overheads -- --scenario event-loop --messages 50 --warmup 5 --frames 2 --payload-bytes 64 +RTS -s` passed before changes: elapsed_ms=511.708, throughput=97.712 msg/s, p50=10211.014us, p95=10273.629us, max=11138.124us, allocated=3,554,392 bytes. |
| 2026-06-03 | Step 0 completed | Preflight evidence captured; project one-commit policy observed (no per-step commit). |
| 2026-06-03 | Step 1 started | Wait strategy planning. |
| 2026-06-03 | Step 1 plan review | APPROVE (`.reviews/R001-plan-step1.md`). |
| 2026-06-03 | Step 2 started | Implement lower-latency coordination. |
| 2026-06-03 | Step 2 plan review | UNAVAILABLE (reviewer produced no output); proceeding with approved Step 1 plan. |
| 2026-06-03 | Step 2 targeted tests | Initial `registerDelay` approach failed without `-threaded`; replaced with a bracketed cancellable timer. Existing EventLoop send/receive/transceiver/safety suites then passed after Step 2 and reviewer revisions. |
| 2026-06-03 | Step 2 code review | APPROVE on re-review (`.reviews/R004-code-step2.md`). |
| 2026-06-03 | Step 3 plan review | APPROVE (`.reviews/R005-plan-step3.md`). |
| 2026-06-03 | Step 3 latency test | `cabal test test-event-loop-latency-auto` passed after adding mailbox wake/shutdown, idle transceiver ack, and slow callback ordering coverage. |
| 2026-06-03 | Step 3 EventLoop benchmark | `cabal run --enable-optimization=2 zmqx-overheads -- --scenario event-loop --messages 50 --warmup 5 --frames 2 --payload-bytes 64 +RTS -s` passed after changes: elapsed_ms=62.944, throughput=794.358 msg/s, p50=1255.076us, p95=1350.237us, max=1417.851us, allocated=1,067,432 bytes; baseline was elapsed_ms=511.708, p50=10211.014us, p95=10273.629us, max=11138.124us, allocated=3,554,392 bytes. |
| 2026-06-03 | Step 3 performance docs | `docs/performance.md` caveat updated from mailbox polling to receiver polling slices/mailbox delivery; benchmark commands unchanged. |
| 2026-06-03 | Step 3 code review | APPROVE (`.reviews/R006-code-step3.md`). |
| 2026-06-03 | Step 4 plan review | APPROVE (`.reviews/R007-plan-step4.md`). |
| 2026-06-03 | Step 4 targeted tests | `cabal test test-event-loop-latency-auto test-event-loop-send-auto test-event-loop-receive-auto test-event-loop-transceiver-auto test-event-loop-safety-auto` passed (5/5). |
| 2026-06-03 | Step 4 EventLoop benchmark smoke | `cabal run --enable-optimization=2 zmqx-overheads -- --scenario event-loop --messages 50 --warmup 5 --frames 2 --payload-bytes 64 +RTS -s` passed: elapsed_ms=62.483, throughput=800.219 msg/s, p50=1244.384us, p95=1338.854us, max=1362.141us, allocated=1,065,976 bytes. |
| 2026-06-03 | Step 4 full suite | `cabal test all` passed. |
| 2026-06-03 | Step 4 build | `cabal build` passed (`Up to date`). |
| 2026-06-03 | Step 4 failure status | No failures remain after the non-threaded `registerDelay` issue was replaced with the bracketed cancellable timer. |
| 2026-06-03 | Step 4 code review | APPROVE (`.reviews/R008-code-step4.md`). |
| 2026-06-03 | Step 5 must-update docs | `docs/performance.md` updated for the EventLoop caveat; benchmark command names/options did not change. |
| 2026-06-03 | Step 5 check-if-affected docs | `docs/quickstart.md` reviewed; existing EventLoop usage/callback guidance remains accurate. `performance-overheads/` is absent in this worktree. |
| 2026-06-03 | Step 5 discoveries | Non-threaded `registerDelay` behavior and `Poll.pollFor` deadline rounding discovery logged. |
| 2026-06-03 | Step 5 tradeoffs | EventLoop command wakeup fairness and positive-timeout timer strategy future work logged to `taskplane-tasks/CONTEXT.md`. |
| 2026-06-03 | Task completed | All implementation, regression coverage, benchmark evidence, full test suite, build, docs, and review gates completed. |
| 2026-06-02 16:51 | Worker iter 1 | done in 580s, tools: 51 |
| 2026-06-02 17:23 | Worker iter 2 | done in 1906s, tools: 132 |
| 2026-06-02 17:23 | Task complete | .DONE created |

---

## Blockers

*None*

---

## Notes

### Reviewer suggestions

- Step 3 should include coverage or benchmark evidence that idle receiver polling does not burn CPU and that positive-timeout mailbox waits return promptly without accumulating per-call sleeper threads.

### Step 1 wait-path map

- Public `send`/`sends`: `queueSend` atomically writes `Send` to `eventLoopCommands` when `eventLoopAccepting` is true, then `waitForSendReply` repeatedly `tryTakeMVar reply`, checks `tryReadMVar eventLoopWorkerDone`, and sleeps `threadDelay 1000` between polls. Worker exceptions are put in the reply MVar and then rethrown by `runWorker`.
- Worker command loop: with no receivers, `nextCommand` blocks on `readTQueue`; with receivers, it uses `tryReadTQueue`, then `Poll.pollFor sockets receiverPollSliceMs` (`receiverPollSliceMs = 10`) before returning to the command queue. This can delay public send ack while the worker is in a receiver poll slice.
- Receiver polling/delivery: `pollAndDeliverReceivers` polls input sockets, delivers one ready multipart receive per registered runtime, drops newest mailbox messages when `TBQueue` is full via `tryWriteMailbox`, and runs callbacks on the worker thread.
- Worker send retry: selected socket roles use `Socket.sendManyDontWait`; on would-block it sleeps `senderRetrySliceUs = 1000` and rechecks `eventLoopAccepting`, preserving shutdown escape from blocked sends.
- Public mailbox `recv`: negative timeout blocks in STM on `stoppedAlternative accepting orElse readTBQueue`; zero checks once; positive timeout loops via `pollMailboxOnce`, sleeps up to `pollSleepUs` (max 1000us), and rechecks the monotonic deadline.
- Shutdown/failure: `stopEventLoop` atomically flips `eventLoopAccepting` false, writes `Stop`, then `readMVar eventLoopWorkerDone`; `runWorker` records `Either SomeException ()` in `eventLoopWorkerDone` and flips accepting false; `stoppedResult` rereads worker result so public waiters surface worker exceptions or `ETERM`.

### Step 1 selected wait strategy

- Replace public send reply polling by moving command replies and worker completion into STM (`TMVar`/`TVar Maybe`) so `waitForSendReply` can block on `reply` `orElse` worker completion with no `threadDelay 1000`. Keep the existing MVar-only test hook behavior by delaying before entering the STM wait when the test env var is set.
- Replace positive-timeout mailbox polling sleeps with STM `registerDelay`: wait on `stoppedAlternative`, `readTBQueue`, or a timeout TVar in one `atomically`, avoiding 1ms public `recv` polling. Negative and zero timeout semantics remain unchanged.
- Keep receiver socket polling in `Poll.pollFor`, but lower the EventLoop worker receiver slice from 10ms to a short bounded slice (1ms) so commands queued while the worker is polling receivers are observed promptly without introducing an uninterruptible wait or busy loop.
- Reduce worker send retry sleep from 1000us to a smaller bounded slice (100us) for shutdown-aware nonblocking sends. This preserves backpressure behavior (still retries only after `sendManyDontWait` says not ready) and improves shutdown/ack latency without changing direct socket APIs.
- Do not modify `Zmqx.Core.Poll` for this task unless tests show a shared poll hook is necessary; the current EventLoop latency issue can be addressed in `Zmqx.EventLoop.hs`.

### Step 1 invariants to preserve

- Worker failure: any exception in send, receive, poll, or callback is recorded exactly once before `eventLoopAccepting` becomes false; public send/recv/stopped calls rethrow that exception rather than returning a synthetic `ETERM`.
- Loop closure: `stopEventLoop` flips accepting false and queues `Stop`; callers that try to queue after that get `ETERM`, and callers already waiting are released by worker completion or accepting=false STM alternatives.
- Timeout semantics: `recv timeoutMs < 0` waits until message or stop, `recv 0` remains a nonblocking mailbox check, and `recv timeoutMs > 0` returns `Right Nothing` only after the requested millisecond timeout fires with no message/stop.
- Mailbox/callback semantics: mailbox delivery remains bounded and drops the newest message when full; callbacks still run synchronously on the worker thread and can terminate the worker on exception.
- Startup validation: duplicate names, invalid mailbox capacities, and context mismatches are checked before the worker takes ownership of sockets.
- Context termination/ETERM: libzmq `ETERM`/poll OK-errors still flow through existing socket/Poll error handling; worker completion remains the source of truth for public waiters.

### Step 1 evidence plan

- Add `test/EventLoopLatencyAuto.hs` with low-flakiness tests around semantics rather than strict scheduler timing: positive-timeout mailbox `recv` releases when a message arrives before timeout, queued transceiver send ack remains prompt while receivers are present (generous upper bound only), slow callback blocks subsequent delivery in order, and shutdown still releases waiting public operations.
- Re-run existing EventLoop correctness suites after implementation: send, receive, transceiver, safety.
- Benchmark command for before/after comparison: `cabal run --enable-optimization=2 zmqx-overheads -- --scenario event-loop --messages 50 --warmup 5 --frames 2 --payload-bytes 64 +RTS -s`; compare latency p50/p95/max and RTS allocation to Step 0 baseline.
- Full verification gate remains `cabal test all` plus `cabal build`.
| 2026-06-02 16:47 | Review R001 | plan Step 1: APPROVE |
| 2026-06-02 16:58 | Review R003 | code Step 2: REVISE |
| 2026-06-02 17:04 | Review R004 | code Step 2: APPROVE |
| 2026-06-02 17:06 | Review R005 | plan Step 3: APPROVE |
| 2026-06-02 17:13 | Review R006 | code Step 3: APPROVE |
| 2026-06-02 17:15 | Review R007 | plan Step 4: APPROVE |
| 2026-06-02 17:20 | Review R008 | code Step 4: APPROVE |
