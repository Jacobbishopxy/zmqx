## Plan Review: Step 1: Receive-path optimization plan

### Verdict: APPROVE

### Summary
The plan maps the shared receive path well enough and selects a conservative optimization: move `zmq_msg_t` storage out of per-frame heap `malloc`/`free` while preserving `zmq_msg_init`/`zmq_msg_close`, payload copying, public APIs, multipart ordering, and current error handling. Deferring zero-copy payload receive is the right safety choice for this task, and the planned correctness plus benchmark evidence matches the PROMPT's completion criteria.

### Issues Found
None.

### Missing Items
None.

### Suggestions
- When implementing `alloca`/`zmq_msg_init_at`, keep the existing `bracket`/masking cleanup shape so async exceptions and `EAGAIN`/`EINTR`/`ETERM` paths still close initialized messages exactly once.
- Include the `Core.Poll`/REQ-buffer path in the targeted receive-poll verification, since it also calls `Socket.receiveManyDontWait`.
- Add an ownership comment near the helper noting that stack-allocated `zmq_msg_t` storage must not be passed to `zmq_msg_free`, and that `ByteString.packCStringLen` must complete before `zmq_msg_close`.
