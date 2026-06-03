## Code Review: Step 2: Implement lower-latency coordination

### Verdict: REVISE

### Summary
The STM send-reply path preserves the important reply-before-worker-result behavior, and `cabal build` plus the existing targeted EventLoop suites passed. However, the 1ms receiver poll slice interacts badly with the current `Poll.pollFor` deadline rounding, and the new mailbox timeout helper leaks sleeping timer threads on early completion. No declared typecheck/lint/format-check commands were configured in `.pi/taskplane-config.json`, and there is no `package.json` fallback.

### Issues Found
1. **[lib/Zmqx/EventLoop.hs:459] [important]** — Lowering `receiverPollSliceMs` to `1` can turn idle receiver loops into a busy spin for normal pollable sockets. `Zmqx.Core.Poll.pollFor` floors the remaining nanoseconds to milliseconds (`lib/Zmqx/Core/Poll.hs:320`), so after even a tiny amount of elapsed time a 1ms deadline becomes `remainingMs = 0`; `zmq_poll` is then called nonblocking and `retryOrTimeout` loops until the deadline. Fix by updating the poll timeout calculation to sleep/block for at least 1ms while the deadline is still in the future (or otherwise avoid the 1ms slice until `Poll` handles sub-ms remainders without spinning).
2. **[lib/Zmqx/EventLoop.hs:645] [important]** — Positive mailbox `recv` now unconditionally calls `newTimeoutFlag`, which forks a sleeping thread before checking whether a message or stop is already available. If the wait completes early, that thread remains until the full timeout expires; repeated receives with long positive timeouts can accumulate many sleeping threads and undermine the latency/overhead goal. Use `registerDelay` as planned, or make the timer cancellable/bracketed (and ideally avoid arming it for an already-ready mailbox/stop case).

### Pattern Violations
- The mailbox timeout implementation deviates from the approved Step 1 strategy of using STM `registerDelay` and introduces an ad hoc `forkIO` timer.

### Test Gaps
- Step 3 should include coverage or benchmark evidence that idle receiver polling does not burn CPU and that positive-timeout mailbox waits return promptly without accumulating per-call sleeper threads.

### Suggestions
- The TMVar-based send wait looks sound and keeps the left-biased reply handling requested in the Step 1 plan.
