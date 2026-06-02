## Code Review: Step 2: Implement lifecycle and sender command routing

### Verdict: REVISE

### Summary
The main lifecycle/send path is implemented and `cabal build` passes; there are no configured `.pi`/`package.json` typecheck, lint, or format-check commands in this worktree. The R005 direct throwing-send hang is mostly addressed, but the worker completion signal is consumed during cleanup, which can still strand queued send callers after a worker failure.

### Issues Found
1. **[lib/Zmqx/EventLoop.hs:140] [important]** — `stopEventLoop` uses `takeMVar eventLoopWorkerDone`, but `waitForSendReply` depends on `tryReadMVar eventLoopWorkerDone` to wake callers whose commands will never receive a reply after the worker fails. If command A throws, command B is already queued, and bracket cleanup consumes the worker-done `Left` before B observes it, B's reply MVar stays empty and it can spin forever. Fix by making worker completion a non-consuming latched signal for all observers (for example use `readMVar` in cleanup, or a `TVar`/broadcast-style status) so pending sends can still observe and rethrow the worker failure.

### Pattern Violations
- None.

### Test Gaps
- Step 3 should include a regression with multiple concurrent/queued sends where one socket operation throws, verifying all waiting callers complete and cleanup remains joinable.

### Suggestions
- The polling `threadDelay` wait could eventually be replaced with an STM/blocking wait over a reply/status primitive, but that is not required for this step once the completion signal is not consumed.
