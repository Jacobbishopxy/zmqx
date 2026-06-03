## Plan Review: Step 3: Add REQ probe regression coverage and benchmark evidence

### Verdict: APPROVE

### Summary
The Step 3 plan covers the required outcomes for this checkpoint: add and register dedicated REQ probe regression coverage, re-run the existing stale-reply `ReqPoll` scenarios, and capture post-change benchmark evidence. This is also consistent with the Step 1 test intent and the Step 2 code-review note that the dedicated probe test is the remaining evidence gap.

### Issues Found
None.

### Missing Items
None.

### Suggestions
- Include the Step 1 mixed REQ/non-REQ poll case in the new test if it can be done deterministically, especially the path where an invalid/stale REQ wakeup occurs while another socket is ready.
- Record benchmark evidence in STATUS.md with enough before/after context to show both idle timeout/probe-overhead impact and valid-reply latency, not just that the benchmark command ran.
