## Plan Review: Step 1: Blocking/error-path optimization plan

### Verdict: REVISE

### Summary
The path mapping, FFI safety constraints, and rejected alternatives are generally sound, and the fused-errno wrapper is a reasonable low-blast-radius target. However, the plan selects both receive and send wrapper changes while the evidence plan only measures/tests the empty-receive side, leaving the required saturated send/HWM case undefined.

### Issues Found
1. **[Severity: important]** — `PROMPT.md:79` requires correctness and benchmark evidence for saturated send/HWM, and `STATUS.md:114` plans to route `zmq_send__unsafe` through the new fused wrapper, but `STATUS.md:116` only defines empty-receive and success-path evidence. Add a concrete saturated-send/HWM correctness/measurement plan, or narrow the selected optimization to the measured receive path and explicitly defer send wrapping.

### Missing Items
- Saturated send/HWM evidence for the send-side EAGAIN path if `zmq_send__unsafe` will be modified.

### Suggestions
- Clarify that any `zmq_send` wrapper preserves the caller-supplied flags (`ZMQ_DONTWAIT`, `ZMQ_SNDMORE`, or no flag for existing wont-block paths) rather than forcing nonblocking behavior inside C.
