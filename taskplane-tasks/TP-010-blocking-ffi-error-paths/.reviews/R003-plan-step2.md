## Plan Review: Step 2: Implement the reviewed hot-path optimization

### Verdict: APPROVE

### Summary
The Step 2 plan is consistent with the approved Step 1 direction: implement only the receive-side fused-errno `zmq_msg_recv`/`ZMQ_DONTWAIT` wrapper and leave send-side/HWM and blocking FFI behavior unchanged. This should satisfy the task's hot-path goal while preserving public `Either Error` semantics, existing exception conversion boundaries, and scheduler/cancellation behavior for operations that can block.

### Issues Found
- None.

### Missing Items
- None.

### Suggestions
- Keep the new binding name/comment DONTWAIT-specific so it is not accidentally reused for a potentially blocking receive path.
- When implementing, ensure the existing `zmq_msg_recv_dontwait` errno classification remains unchanged and only the source of the errno value changes.
