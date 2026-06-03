## Code Review: Step 3: Add poll scaling regression coverage and benchmark evidence

### Verdict: APPROVE

### Summary
The prepared poll-set refactor preserves the public `Sockets`/`Ready` API, keeps input REQ sockets on the direct-probe path, resets pollitem templates before each `zmq_poll`, and leaves blocking waits on the interruptible FFI binding. The new `test-poll-scaling-auto` target is registered and covers idle, one-ready, many-ready, mixed POLLIN/POLLOUT, and mixed REQ/non-REQ readiness; benchmark evidence is recorded in `STATUS.md`. No declared typecheck/lint/format commands were configured in `.pi/taskplane-config.json` (and there is no `package.json`); I additionally ran `cabal build`, `cabal test test-poll-scaling-auto`, and `cabal test test-items-poll-auto test-poll-out test-req-poll`, all passing.

### Issues Found
None.

### Pattern Violations
- None.

### Test Gaps
- Non-blocking `pollFor ... 0` is still not explicitly asserted in the new scaling suite; existing behavior appears preserved, so this is a non-blocking coverage suggestion.

### Suggestions
- Consider a future heap fallback or documented bound for the stack-allocated pollitem buffer in `lib/Zmqx/Core/Poll.hs:190` if very large poll sets become a supported target.
- If poll-set construction shows up in future profiles, revisit `pollInAlso`/`pollOutAlso` rebuilding prepared arrays on each append (`lib/Zmqx/Core/Poll.hs:109` and `lib/Zmqx/Core/Poll.hs:119`).
