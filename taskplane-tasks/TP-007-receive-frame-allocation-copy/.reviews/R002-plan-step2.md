## Plan Review: Step 2: Implement receive message-object optimization

### Verdict: APPROVE

### Summary
The plan targets the right hot-path issue by replacing per-frame heap `malloc`/`free` of `zmq_msg_t` storage with scoped allocation while retaining `zmq_msg_init`/`zmq_msg_close`, copied `ByteString` payloads, multipart flow, and public API behavior. It also explicitly defers zero-copy receive, which is the correct safety boundary for this task.

### Issues Found
None.

### Missing Items
- None.

### Suggestions
- When implementing, keep the stack-allocated message pointer strictly scoped to `zhs_with_frame` and ensure `zmq_msg_close` still runs on exceptions; do not call `zmq_msg_free` for stack-owned storage.
- Consider documenting that any new `zmq_msg_init_at` helper has borrowed/stack storage semantics distinct from the existing heap-allocating `zmq_msg_init` helpers.
