## Code Review: Step 2: Implement lifecycle and sender command routing

### Verdict: REVISE

### Summary
The lifecycle and command-queue path is mostly implemented, and `cabal build` passes. No `.pi` task-runner or `package.json` typecheck/lint/format-check commands are configured, but there is still a race in the reply/status polling that can return the wrong result for an accepted send.

### Issues Found
1. **[lib/Zmqx/EventLoop.hs:198] [important]** — `waitForSendReply` checks the per-command reply MVar once and then trusts `eventLoopWorkerDone`; if the worker fills this command's reply and then exits/fails between those two checks, the caller can return `Left stoppedLoopError` or throw a later worker exception instead of the result of its own accepted send. Fix by re-checking/prefering the reply after observing `workerDone` (or by replacing the polling with an STM/blocking primitive that atomically waits for reply vs terminal status and gives command replies precedence when present).

### Pattern Violations
- None.

### Test Gaps
- Add a Step 3 regression for an accepted send racing with loop shutdown and/or a later worker failure, verifying the caller receives its command's actual reply rather than a stopped-loop result.

### Suggestions
- Consider replacing the 1ms polling loop with an STM latch or other blocking wait to remove these ordering races more directly.
