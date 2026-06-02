## Code Review: Step 5: Testing & Verification

### Verdict: REVISE

### Summary
The requested full baseline hash was not present in this worktree, so I used the existing matching `6d77acc865f41aab7ee525f4314fba784dce8e81` for the Step 5 delta and also reviewed the complete TP-004 implementation diff. The prompt's verification commands all passed (`cabal test ...event-loop...`, `cabal test test-dealer-router-auto test-contextual-open`, and `cabal build`); no `.pi`/`package.json` typecheck/lint/format-check commands were configured. However, the lifecycle implementation still misses a stated safety requirement for pending sends during shutdown.

### Issues Found
1. **[lib/Zmqx/EventLoop.hs:424] [important]** — `handleCommand` calls `sendWithSender`, which delegates to the socket's potentially blocking `Socket.send_` (lines 476-478). If that send is waiting for a peer/backpressure, `stopEventLoop` only flips `accepting` and queues `Stop` (lines 376-384), but the worker cannot process `Stop`, `eventLoopWorkerDone` is never filled, the bracket exit blocks, and the public `EventLoop.send` waiter is not unblocked. This violates Step 3's “stop/exit unblocks pending public send waiters” outcome. I reproduced this with a forked `EventLoop.send` against an unconnected `PUSH`; a 2s `System.Timeout` around `withEventLoop` returned `Nothing`. Fix: make worker sends shutdown-aware/nonblocking (for example, poll send readiness in short slices while observing loop stop state / stop commands and reply `ETERM` on shutdown, or introduce a nonblocking send capability instead of invoking blocking `Socket.send_` directly), then add a regression test for shutdown while a send is blocked.

### Pattern Violations
- None.

### Test Gaps
- The safety tests cover pending `recv` shutdown and queued send failures, but not a send that is actively blocked in the worker on an unwritable/unconnected socket. Add this regression because it is the case that still deadlocks.

### Suggestions
- Consider wrapping the new transceiver/safety suite `main` bodies in a top-level timeout like `EventLoopReceiveAuto` to make future deadlocks fail fast.
