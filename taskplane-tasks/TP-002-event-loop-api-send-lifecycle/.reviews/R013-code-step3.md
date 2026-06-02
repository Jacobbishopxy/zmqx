## Code Review: Step 3: Sender tests

### Verdict: REVISE

### Summary
The new suite compiles and the targeted sender test passes, and it covers the basic global/explicit/missing/stopped paths. However, the R012/R009 accepted-send race regression is still not exercised strongly enough: the test can pass with the old workerDone-first reply-wait bug restored.

### Issues Found
1. **[test/EventLoopSendAuto.hs:141] [important]** — `queuedFailureRaceTest` still gives the accepted `push` send a 1ms head start before any failing `rep` sends are queued. In practice that allows the accepted sender thread to complete before the worker fails; I verified in a throwaway copy that reverting `waitForSendReply` to the pre-R009 workerDone-first ordering still lets `test-event-loop-send-auto` pass. Fix by making the regression fail against that old ordering: ensure an accepted `push` caller is still awaiting its own reply when the worker terminal status is published (or use enough controlled concurrent accepted sends / a small internal test hook to make that ordering deterministic), then assert the accepted send gets `Right ()` and its payload is received.

### Pattern Violations
- None.

### Test Gaps
- The context-mismatch rejection path remains untested; this was suggestion-level in R011, not the blocking issue here.

### Suggestions
- Consider inspecting the queued illegal-send results at line 158 rather than only checking that their MVars filled, so the test also verifies they observe the worker failure path.
- No `.pi` task-runner config or `package.json` static-check scripts were present. I ran `cabal build test:test-event-loop-send-auto` and `cabal test test-event-loop-send-auto`; both passed.
