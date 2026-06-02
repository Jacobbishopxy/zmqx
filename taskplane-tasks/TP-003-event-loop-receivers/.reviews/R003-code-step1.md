## Code Review: Step 1: Receiver design and poll integration

### Verdict: APPROVE

### Summary
The Step 1 implementation matches the approved design: receiver registration carries mailbox/callback modes, receiver wrappers carry both receive and `PollIn` capabilities, the poll set is built with `Zmqx.Core.Poll`, and context validation now covers receivers. No configured `.pi`/`package.json` typecheck/lint/format commands were present; I ran `cabal build` and the sender regression `cabal test test-event-loop-send-auto`, and both passed.

### Issues Found
None.

### Pattern Violations
- None.

### Test Gaps
- Receiver behavior tests are intentionally deferred to Step 3; no Step 1-blocking gap found.

### Suggestions
- `lib/Zmqx/EventLoop.hs:105` defines an `Eq ReceiverMode` instance where `Callback _ == Callback _` is always `False`, which violates the usual reflexivity law. Consider dropping the `Eq` instance or choosing a law-abiding compatibility behavior before downstream code relies on it.
