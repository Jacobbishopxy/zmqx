## Code Review: Step 3: Sender tests

### Verdict: REVISE

### Summary
The new test suite builds and `cabal test test-event-loop-send-auto` passes. However, one hydrated Step 3 outcome is not actually covered: the accepted-send/worker-failure race from R009, because the only successful send completes before any failing command is queued.

### Issues Found
1. **[test/EventLoopSendAuto.hs:135] [important]** — `queuedFailureRaceTest` sends and unwraps the successful `push` command synchronously before starting the illegal `rep` sends. That verifies “accepted before failure” behavior, but it cannot catch the R009 race where an already-accepted send reply loses to a later worker terminal status while the caller is still waiting; an implementation with the old bug would still pass this test. Fix by queuing at least one valid `push` send concurrently with/just before the failing `rep` commands, waiting for that send’s own result, and asserting it returns `Right ()` and the payload is received even when the worker subsequently fails.

### Pattern Violations
- None.

### Test Gaps
- Context-mismatch rejection is still not covered; this was suggestion-level in R011, not the blocking issue above.

### Suggestions
- Consider checking the specific `ENOENT`/`ETERM` errno values in the missing/stopped assertions for stronger regression signal.

### Quality Checks
- No `.pi` task-runner config or `package.json` static-check scripts were present. I ran `cabal build test:test-event-loop-send-auto` and `cabal test test-event-loop-send-auto`; both passed.
