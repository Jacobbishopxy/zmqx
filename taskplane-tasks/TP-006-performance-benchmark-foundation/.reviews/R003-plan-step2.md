## Plan Review: Step 2: Add opt-in benchmark harness

### Verdict: APPROVE

### Summary
The Step 2 plan is aligned with the approved Step 1 design: it keeps the benchmark harness opt-in, adds a `bench/`/Cabal target, implements reusable helpers, and covers the required overhead scenarios. The planned dependency constraint is also appropriate for a measurement-only foundation and should not change library behavior.

### Issues Found
None.

### Missing Items
- None.

### Suggestions
- Make the Cabal opt-in mechanism explicit during implementation and verify plain `cabal build` / `cabal test all` do not build or run the benchmark executable by default.
- Add RTS options support for the benchmark executable if the documented allocation workflow will use `+RTS -s`.
