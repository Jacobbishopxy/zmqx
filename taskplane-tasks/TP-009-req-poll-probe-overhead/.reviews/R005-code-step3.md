## Code Review: Step 3: Add REQ probe regression coverage and benchmark evidence

### Verdict: APPROVE

### Summary
The new `ReqPollProbeAuto` executable covers the required idle timeout, valid reply readiness/buffer-drain, and mixed REQ/non-REQ polling paths, and it is registered in `test/test.cabal`. Benchmark evidence for the new `req-poll-idle` scenario and valid-reply latency is recorded in STATUS.md; note that the requested `git diff deac3cc1307a30da91f1d9af3abe691b0b767677..HEAD` was empty because the changes are uncommitted, so I reviewed the working-tree diff.

Quality checks: no typecheck/lint/format commands are configured in `.pi/taskplane-config.json`, and there is no `package.json`. I additionally ran `cabal build zmqx-overheads`, `cabal test test-req-poll-probe-auto test-req-poll --test-show-details=direct`, `cabal test test-items-poll-auto test-poll-out --test-show-details=direct`, and a `req-poll-idle` benchmark smoke; all passed.

### Issues Found
None.

### Pattern Violations
- None found.

### Test Gaps
- None blocking. Existing `test-req-poll` still covers stale/correlated replies, while the new test covers the Step 3 probe regression cases.

### Suggestions
- Since `req-poll-idle` is now a public benchmark scenario, update `docs/performance.md` in the delivery/documentation step to list it and clarify recommended small `--messages`/`--timeout-ms` values.
- Consider documenting whether `req-poll-idle` is intentionally excluded from `--scenario all`; including it with current global defaults would be very slow, but the current help text does not explain the distinction.
