## Plan Review: Step 2: Implement lower-overhead REQ polling

### Verdict: APPROVE

### Summary
The Step 2 plan carries forward the previously approved event-gated design: input REQ sockets may wake `zmq_poll`, but readiness is still reported only after an existing buffer or a successful nonblocking receive buffers a valid reply. It covers the key outcomes for this step: removing the fixed 10ms probe cadence, preserving `Req.receivesFor` and `pollFor` semantics, handling mixed poll sets, and keeping buffer updates masked.

### Issues Found
None.

### Missing Items
None.

### Suggestions
- While implementing the retry path after a REQ `revents` wakeup validates to no reply, watch for immediate repeated wakeups from stale/correlated replies; if libzmq leaves such a condition signaled, add a small measured fallback/backoff rather than introducing a busy loop.
- Keep the timeout conversion behavior explicit for `pollFor 0`, finite deadlines, and blocking `poll`, since this is where the event-gated design replaces the old 10ms slice.
