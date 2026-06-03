# Task: TP-006 - Performance benchmark foundation

**Created:** 2026-06-02
**Size:** M

## Review Level: 1 (Plan Only)

**Assessment:** Adds opt-in benchmark infrastructure and baseline measurement artifacts without changing library behavior. It spans Cabal/package wiring and new benchmark sources, but the change is reversible and should not affect default builds or tests.
**Score:** 3/8 — Blast radius: 1, Pattern novelty: 1, Security: 0, Reversibility: 1

## Canonical Task Folder

```
taskplane-tasks/TP-006-performance-benchmark-foundation/
├── PROMPT.md   ← This file (immutable above --- divider)
├── STATUS.md   ← Execution state (worker updates this)
├── .reviews/   ← Reviewer output (created by the orchestrator runtime)
└── .DONE       ← Created when complete
```

## Mission

Create a repeatable, opt-in performance benchmark foundation for the overheads identified in the parallel review: receive allocation/copy cost, send path cost, poll scaling, REQ polling, EventLoop latency, and lifecycle/finalizer scaling. This task establishes measurement before optimization so later TPs can prove improvements and avoid trading correctness for speed.

## Dependencies

- **None**

## Context to Read First

**Tier 2 (area context):**
- `taskplane-tasks/CONTEXT.md`

**Relevant source and report files:**
- `performance-overheads/haskell-hotpaths.md` — prioritized local hot path findings, if present
- `performance-overheads/ffi-boundary.md` — FFI/C-boundary findings, if present
- `performance-overheads/benchmark-plan.md` — recommended benchmark matrix, if present
- `performance-overheads/concurrency-lifecycle.md` — concurrency/lifecycle overhead findings, if present
- `cabal.project`, `zmqx.cabal`, `test/test.cabal` — current package layout and commands
- `lib/Zmqx/Core/Socket.hs`, `lib/Zmqx/Core/Poll.hs`, `lib/Zmqx/EventLoop.hs`, `lib/Zmqx/Core/Context.hs` — benchmark targets

## Environment

- **Workspace:** `/home/xiey/Code/zmqx`
- **Services required:** None; benchmarks should use local `inproc://` or loopback endpoints only

## File Scope

- `cabal.project`
- `bench/*`
- `docs/performance.md`
- `README.md`
- `docs/examples.md`
- `taskplane-tasks/CONTEXT.md`

## Steps

### Step 0: Preflight

- [ ] Required files and paths exist
- [ ] `libzmq` is available through `pkg-config`
- [ ] Current correctness baseline passes or pre-existing failures are recorded: `cabal build` and `cabal test all`

### Step 1: Benchmark design plan

> **Plan-review checkpoint** — confirm the benchmark package is opt-in, does not change library behavior, and covers each overhead family with practical commands.

- [ ] Choose a benchmark package layout that keeps default `cabal build`/`cabal test all` behavior stable
- [ ] Define a small CLI or executable set for direct API, poll, REQ poll, EventLoop, and lifecycle measurements
- [ ] Define output fields for throughput, latency percentiles where practical, allocations via RTS flags, and benchmark metadata
- [ ] Document which measurements are smoke-quality versus reliable enough for regression comparison

**Artifacts:**
- STATUS.md discovery notes if the package layout differs from this prompt

### Step 2: Add opt-in benchmark harness

- [ ] Add a `bench/` package or benchmark target wired from `cabal.project`
- [ ] Implement reusable benchmark helpers for unique inproc endpoints, warmup, timed loops, payload generation, and summary output
- [ ] Implement initial benchmark scenarios for direct send/receive, multipart receive, poll scaling, REQ poll timeout/probe behavior, EventLoop send/recv, and socket lifecycle churn
- [ ] Keep benchmark code dependency-light; prefer `base`, `bytestring`, `time`, and existing project dependencies unless a stronger dependency is justified in STATUS.md

**Artifacts:**
- `bench/` (new)
- `cabal.project` (modified)

### Step 3: Capture baseline and document usage

- [ ] Run representative smoke benchmarks with optimized build settings and RTS allocation summaries
- [ ] Record baseline command examples and interpretation guidance in `docs/performance.md`
- [ ] Update `README.md` or `docs/examples.md` with a concise pointer to the benchmark docs
- [ ] Log any benchmark gaps or unstable metrics in `taskplane-tasks/CONTEXT.md` Technical Debt / Future Work rather than expanding scope

**Artifacts:**
- `docs/performance.md` (new)
- `README.md` (modified if pointer is added)
- `docs/examples.md` (modified if benchmark index is added)
- `taskplane-tasks/CONTEXT.md` (modified only for future-work discoveries)

### Step 4: Testing & Verification

- [ ] Run benchmark target help/smoke command, for example `cabal run zmqx-overheads -- --help` or the actual executable name chosen in Step 1
- [ ] Run at least one direct API, one poll, one EventLoop, and one lifecycle smoke benchmark with small iteration counts
- [ ] Run FULL test suite: `cabal test all`
- [ ] Run build: `cabal build`
- [ ] Fix all failures introduced by this task

### Step 5: Documentation & Delivery

- [ ] "Must Update" docs modified
- [ ] "Check If Affected" docs reviewed
- [ ] Discoveries logged in STATUS.md

## Documentation Requirements

**Must Update:**
- `docs/performance.md` — benchmark commands, output fields, and baseline interpretation

**Check If Affected:**
- `README.md` — add a short pointer to performance docs if appropriate
- `docs/examples.md` — list benchmark executable/commands if useful
- `taskplane-tasks/CONTEXT.md` — only for benchmark gaps or future work discovered during execution

## Completion Criteria

- [ ] Opt-in benchmark package/target exists and is wired into Cabal
- [ ] Smoke scenarios cover direct send/receive, poll, REQ polling, EventLoop, and lifecycle overhead families
- [ ] Benchmark docs explain how to run optimized and RTS-allocation measurements
- [ ] `cabal build` and `cabal test all` pass

## Git Commit Convention

Commits happen at **step boundaries**. All commits for this task MUST include the task ID:

- **Step completion:** `feat(TP-006): complete Step N — description`
- **Docs:** `docs(TP-006): description`
- **Bug fixes:** `fix(TP-006): description`
- **Hydration:** `hydrate: TP-006 expand Step N checkboxes`

## Do NOT

- Optimize library code in this task; this TP creates measurement infrastructure only
- Make benchmarks part of the default automated test sweep
- Add network-sensitive defaults that require external services or non-local endpoints
- Treat noisy benchmark numbers as hard CI gates yet
- Modify protected Taskplane config or unrelated task packets

---

## Amendments (Added During Execution)

<!-- Workers add amendments here if issues discovered during execution. -->
