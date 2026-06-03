## Plan Review: Step 1: REQ probe behavior plan

### Verdict: APPROVE

### Summary
The plan maps the current REQ buffer/probe lifecycle across `Req.receivesFor`, REQ-only `pollFor`, and mixed poll sets, and it chooses a concrete event-gated probe design aimed at removing the fixed 10ms slice. The key stale/correlated-reply invariant is preserved because REQ `revents` are only used as wakeups; readiness is still reported only after an existing buffer or a successful nonblocking receive buffers a valid reply.

### Issues Found
None.

### Missing Items
None.

### Suggestions
- When implementing, keep the existing `test-req-poll` stale-then-valid `pollFor` scenario prominent in the evidence, since it specifically exercises the riskier path where a REQ wakeup can validate to no accepted reply before a later valid reply arrives.
- In the new probe test, loose elapsed-time bounds are appropriate for CI, but also record enough benchmark evidence in STATUS.md to show the change reduced wakeups/probe work rather than merely changing timeout behavior.
