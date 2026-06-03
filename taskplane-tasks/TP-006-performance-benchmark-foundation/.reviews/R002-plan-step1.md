## Plan Review: Step 1: Benchmark design plan

### Verdict: APPROVE

### Summary
The revised Step 1 plan in `STATUS.md` addresses the prior R001 gap by documenting the opt-in layout, executable shape, scenario matrix, output/metadata fields, and smoke-versus-regression guidance. It covers all required overhead families and includes practical command examples while preserving the stated no-library-behavior-change goal.

### Issues Found
None.

### Missing Items
- None.

### Suggestions
- In Step 2, make the Cabal wiring explicitly opt-in (for example, `optional-packages: bench/` or an equivalent benchmark target that is not built by plain `cabal build`) so the default build/test behavior remains stable.
- Ensure the benchmark executable is built with RTS options enabled if docs will recommend `+RTS -s` allocation capture.
