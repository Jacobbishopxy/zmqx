## Plan Review: Step 1: Benchmark design plan

### Verdict: REVISE

### Summary
The task requirements for Step 1 call for a concrete benchmark design plan, but `STATUS.md` currently only repeats the high-level checklist and contains no actual layout choice, scenario matrix, output schema, or smoke-vs-regression guidance. Without those details, I cannot confirm the benchmark harness will remain opt-in, preserve default `cabal build` / `cabal test all` behavior, or cover each required overhead family with practical commands.

### Issues Found
1. **[Severity: important]** — The Step 1 design plan is not documented in the reviewed artifacts. Add a concise plan to `STATUS.md` or another task artifact covering the chosen opt-in Cabal/package layout, executable/CLI structure, required benchmark scenarios, output fields/metadata, and which measurements are only smoke-quality versus useful for regression comparison.

### Missing Items
- Chosen opt-in benchmark layout and how it avoids changing default build/test behavior.
- Benchmark scenario matrix covering direct send/receive, multipart receive, poll scaling, REQ poll timeout/probe behavior, EventLoop latency, and lifecycle/finalizer scaling.
- Output field definition for throughput, practical latency percentiles, RTS allocation summaries, and benchmark metadata.
- Guidance distinguishing smoke checks from metrics stable enough for baseline/regression comparison.

### Suggestions
- Include example commands for the planned benchmark executable(s), including small smoke runs and optimized/RTS-allocation runs, so Step 2 implementation has a clear target.
