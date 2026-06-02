## Plan Review: Step 3: Receiver tests

### Verdict: REVISE

### Summary
The proposed test executable covers the main mailbox/callback receive paths and registration in `test/test.cabal`. However, it omits the stopped-loop `recv` behavior, which is an explicit task completion requirement and was part of the Step 1/2 semantics now implemented.

### Issues Found
1. **[Severity: important]** — The Step 3 plan covers timeout, missing receiver, and non-mailbox failures, but not stopped-loop behavior. `PROMPT.md:120` requires public `recv` timeout/missing/stopped semantics to be tested, and `STATUS.md:121` notes the implementation observes loop shutdown. Add a test where a mailbox `recv` is pending or invoked after the loop has been stopped and assert it returns `Left` with the stopped-loop error rather than hanging or timing out.

### Missing Items
- Stopped/shutdown `recv` test coverage for mailbox receivers.

### Suggestions
- Make the stopped test deterministic by forking a blocking `recv loop endpoint (-1)` inside `withEventLoop`, letting the bracket exit, then asserting the forked result is the expected stopped error.
