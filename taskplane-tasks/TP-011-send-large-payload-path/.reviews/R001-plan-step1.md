## Plan Review: Step 1: Large-send ownership and API plan

### Verdict: APPROVE

### Summary
The Step 1 notes cover the required send-path mapping, compare the relevant ownership designs, and choose a conservative copy-backed `zmq_msg_init_data` path with `zmq_send` fallback rather than unsafe retention of Haskell memory. The ownership/finalizer model is sound for implementation review: libzmq owns only C `malloc` storage after a successful message send, cleanup is required on failed/interrupted sends, and public role-level retry/multipart behavior stays above the shared helper.

### Issues Found
None.

### Missing Items
None.

### Suggestions
- In implementation, make the cleanup bracket/masking cover the allocation/copy/init phase as well as `zmq_msg_send`, so async exceptions cannot leak the copied large buffer before the `zmq_msg_t` is fully initialized.
- Consider including a sliced/non-trivial-origin large `ByteString` in the corruption tests if convenient; it is a useful guard against copying from the wrong pointer when exercising arbitrary public `ByteString` inputs.
