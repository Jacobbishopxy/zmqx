## Code Review: Step 2: Implement reviewed large-send path

### Verdict: APPROVE

### Summary
The implementation matches the approved copy-backed large-send design: frames at or above 64KiB use `zmq_msg_init_data`/`zmq_msg_send`, smaller frames keep the existing `zmq_send` path, and `ZMQ_DONTWAIT`/`ZMQ_SNDMORE` flags are forwarded through one helper. Allocation/init/send cleanup is masked and exception-safe, and the C finalizer remains a minimal `free` callback that does not call back into Haskell. No declared typecheck/lint/format-check commands are configured; `cabal build` and the targeted send/poll suites I ran passed.

### Issues Found
None.

### Pattern Violations
None.

### Test Gaps
- Step 3 still needs the planned large-payload corruption/lifetime tests and benchmark evidence; this is not blocking for the Step 2 implementation checkpoint.

### Suggestions
- Consider documenting the 64KiB threshold rationale near `largeSendFrameThreshold` once Step 3 benchmark evidence confirms or adjusts it.
