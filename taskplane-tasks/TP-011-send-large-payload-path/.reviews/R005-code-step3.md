## Code Review: Step 3: Add large-payload send tests and benchmark evidence

### Verdict: APPROVE

### Summary
The requested `git diff 796dbbdee2dc4ca2ee00595a777f802fbcd2f3ea..HEAD` is empty, so I reviewed the current unstaged/untracked worktree changes. The new large-payload test covers large single-frame and multipart sends over PAIR and DEALER/ROUTER, the suite is registered, and the adjusted benchmark notes support limiting the optimized path to large multipart prefix frames. No declared typecheck/lint/format-check commands are configured; `cabal build` and `cabal test test-large-payload-send-auto` passed.

### Issues Found
None.

### Pattern Violations
None.

### Test Gaps
- The GC-pressure checks still keep the original expected `ByteString`s live until after receive, so they are stronger corruption/order tests than premature-finalization tests. This is acceptable for this copy-backed design, but a future true zero-copy path should add a test that drops sender-side references and validates by independently computed hashes/bytes.

### Suggestions
- Step 4's benchmark sweep should resolve the noisy direct 1MiB rerun noted in `STATUS.md` and include boundary sizes near the 64KiB threshold if convenient.
