## Plan Review: Step 1: Poll representation plan

### Verdict: APPROVE

### Summary
The plan covers the required allocation map, selects a compatible internal prepared representation, and explicitly preserves the public `Sockets`/`Ready` API. The REQ handling plan matches the current safeguards in `Zmqx.Core.Poll` by keeping input REQs out of `zmq_poll`, checking the message buffer, probing with the existing EFSM handling, and preserving mixed-set readiness.

### Issues Found
None.

### Missing Items
None.

### Suggestions
- While implementing prepared `Sockets`, avoid making repeated `pollInAlso` construction accidentally O(n²) for larger poll sets if possible; a cheap builder representation plus one prepared poll buffer may be preferable if array-copying during construction shows up in benchmarks.
- When replacing `StorableArray` marshalling, make the reset/copy of `revents` and the `keepAlive` coverage explicit in the code so stale readiness bits or raw socket lifetimes cannot regress.
- Include at least one timeout-0 assertion in the new/targeted poll evidence, since this path switches to the unsafe `zmq_poll` binding and is easy to regress separately from positive timeouts.
