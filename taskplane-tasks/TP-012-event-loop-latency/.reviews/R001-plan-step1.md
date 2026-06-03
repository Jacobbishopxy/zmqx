## Plan Review: Step 1: EventLoop wait strategy plan

### Verdict: APPROVE

### Summary
The plan maps the relevant EventLoop wait paths and selects bounded/event-driven replacements that preserve the key shutdown and worker-failure invariants. The STM-based send reply/worker completion wait and `registerDelay` mailbox timeout approach directly address the 1ms polling loops without introducing an obvious uninterruptible wait, and the evidence plan covers latency plus compatibility scenarios.

### Issues Found
None.

### Missing Items
- None.

### Suggestions
- When implementing the STM send wait, preserve the current left-biased behavior of consuming a ready command reply before falling back to worker completion if both are observable.
- When converting positive mailbox timeouts to `registerDelay`, guard the millisecond-to-microsecond conversion for very large `Int` values so timeout semantics do not accidentally overflow.
