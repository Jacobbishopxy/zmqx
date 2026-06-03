## Plan Review: Step 2: Implement lower-allocation poll internals

### Verdict: APPROVE

### Summary
The Step 2 plan follows the approved Step 1 representation design: keep `Sockets` abstract, preserve `Ready (forall a. Socket a -> Bool)`, move poll preparation into an internal prepared representation, and avoid changing blocking `zmq_poll` interruptibility. It also carries forward the important REQ safeguards by treating input REQs specially, probing buffers dynamically, and preserving mixed REQ/non-REQ readiness.

### Issues Found
None.

### Missing Items
None.

### Suggestions
- When scanning `revents`, preserve the current “any non-zero revents means ready” behavior unless there is a deliberate, reviewed semantic change.
- If the pollitem buffer is stack-allocated, consider a heap fallback or clear bounds rationale for very large poll sets so scaling improves without introducing a new stack-pressure limit.
- Keep the template-copy/revents-reset and raw socket `keepAlive` coverage explicit in the implementation, since those are the easiest parts of this optimization to regress.
