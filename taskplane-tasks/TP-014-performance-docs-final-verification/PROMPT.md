# Task: TP-014 - Performance docs and final verification

**Created:** 2026-06-02
**Size:** S

## Review Level: 1 (Plan Only)

**Assessment:** Final performance documentation and verification after the optimization tasks. It may make small docs or benchmark-output polish changes but should not introduce new library behavior.
**Score:** 3/8 — Blast radius: 1, Pattern novelty: 1, Security: 0, Reversibility: 1

## Canonical Task Folder

```
taskplane-tasks/TP-014-performance-docs-final-verification/
├── PROMPT.md   ← This file (immutable above --- divider)
├── STATUS.md   ← Execution state (worker updates this)
├── .reviews/   ← Reviewer output (created by the orchestrator runtime)
└── .DONE       ← Created when complete
```

## Mission

Finalize the performance-overhead remediation series by running the full validation sweep, summarizing before/after benchmark evidence, and documenting how users and maintainers should measure zmqx performance going forward. This task should not add new optimizations; it is for verification, docs, and future-work triage after TP-006 through TP-013.

## Dependencies

- **Task:** TP-009 (REQ poll probe optimization must be complete)
- **Task:** TP-011 (large-payload send path task must be complete)
- **Task:** TP-012 (EventLoop latency task must be complete)
- **Task:** TP-013 (lifecycle/finalizer scaling task must be complete)

## Context to Read First

**Tier 2 (area context):**
- `taskplane-tasks/CONTEXT.md`

**Relevant source and report files:**
- `docs/performance.md` — benchmark docs from TP-006 and later tasks
- `performance-overheads/*.md` — original overhead analysis, if present
- `bench/*` — final benchmark harness
- `README.md`, `docs/examples.md`, `docs/quickstart.md` — user-facing docs that may need performance pointers/caveats
- STATUS.md files for TP-006 through TP-013 — benchmark evidence and deferred work notes
- `cabal.project`, `zmqx.cabal`, `test/test.cabal` — final build/test surface

## Environment

- **Workspace:** `/home/xiey/Code/zmqx`
- **Services required:** None

## File Scope

- `docs/performance.md`
- `README.md`
- `docs/examples.md`
- `docs/quickstart.md`
- `bench/*`
- `taskplane-tasks/CONTEXT.md`
- `taskplane-tasks/TP-006-performance-benchmark-foundation/STATUS.md`
- `taskplane-tasks/TP-007-receive-frame-allocation-copy/STATUS.md`
- `taskplane-tasks/TP-008-poll-prepared-set/STATUS.md`
- `taskplane-tasks/TP-009-req-poll-probe-overhead/STATUS.md`
- `taskplane-tasks/TP-010-blocking-ffi-error-paths/STATUS.md`
- `taskplane-tasks/TP-011-send-large-payload-path/STATUS.md`
- `taskplane-tasks/TP-012-event-loop-latency/STATUS.md`
- `taskplane-tasks/TP-013-lifecycle-finalizer-scaling/STATUS.md`

## Steps

### Step 0: Preflight

- [ ] Required files and paths exist
- [ ] TP-009, TP-011, TP-012, and TP-013 are complete
- [ ] Benchmark harness and performance docs from TP-006 exist

### Step 1: Final verification plan

> **Plan-review checkpoint** — confirm this task is limited to verification, documentation, and future-work triage.

- [ ] Inventory benchmark evidence and STATUS discoveries from TP-006 through TP-013
- [ ] Choose a small final benchmark matrix that exercises each optimized area without requiring long-running CI-style performance gates
- [ ] Plan user-facing documentation updates for benchmark commands, caveats, and expected overhead tradeoffs
- [ ] Identify unresolved optimization ideas that should be logged as future work instead of implemented here

**Artifacts:**
- STATUS.md notes with final verification matrix and doc plan

### Step 2: Run final benchmark and correctness sweep

- [ ] Run final smoke benchmark matrix covering receive, send, poll, REQ poll, EventLoop, and lifecycle scenarios
- [ ] Run FULL test suite: `cabal test all`
- [ ] Run build: `cabal build`
- [ ] Record command outputs, failures, or environmental limitations in STATUS.md

**Artifacts:**
- STATUS.md benchmark and validation notes

### Step 3: Update performance docs and user-facing pointers

- [ ] Update `docs/performance.md` with final benchmark commands, interpretation guidance, and summarized before/after evidence where available
- [ ] Add or adjust concise pointers in `README.md`, `docs/examples.md`, or `docs/quickstart.md` only where they improve discoverability
- [ ] Document remaining known overheads as tradeoffs or future work, not as completed fixes
- [ ] Keep docs honest about benchmark noise, machine dependency, and non-goals

**Artifacts:**
- `docs/performance.md` (modified)
- `README.md`, `docs/examples.md`, or `docs/quickstart.md` (modified only if needed)

### Step 4: Delivery and future-work triage

- [ ] Log unresolved performance work in `taskplane-tasks/CONTEXT.md` Technical Debt / Future Work
- [ ] Confirm no source-code optimizations were added by this final docs task
- [ ] Confirm task STATUS files contain enough benchmark evidence for future maintainers
- [ ] Discoveries logged in STATUS.md

**Artifacts:**
- `taskplane-tasks/CONTEXT.md` (modified if future work remains)
- STATUS.md (updated)

## Documentation Requirements

**Must Update:**
- `docs/performance.md` — final benchmark usage, summary, caveats, and remaining overheads

**Check If Affected:**
- `README.md` — add/adjust performance docs pointer if missing or stale
- `docs/examples.md` — add/adjust benchmark executable listing if missing or stale
- `docs/quickstart.md` — only if user-facing performance caveats are needed
- `taskplane-tasks/CONTEXT.md` — log unresolved future work

## Completion Criteria

- [ ] Final benchmark smoke matrix has been run or limitations are clearly documented
- [ ] `cabal build` and `cabal test all` pass
- [ ] Performance docs are up to date and discoverable
- [ ] Remaining overheads/future work are logged without expanding scope
- [ ] No new library behavior was introduced by this task

## Git Commit Convention

Commits happen at **step boundaries**. All commits for this task MUST include the task ID:

- **Step completion:** `feat(TP-014): complete Step N — description`
- **Docs:** `docs(TP-014): description`
- **Bug fixes:** `fix(TP-014): description`
- **Hydration:** `hydrate: TP-014 expand Step N checkboxes`

## Do NOT

- Implement additional source-code optimizations in this final verification task
- Hide benchmark regressions; record them honestly with environment details
- Add hard CI performance gates unless explicitly approved in an amendment
- Rewrite broad docs unrelated to performance measurement or overhead tradeoffs
- Modify protected Taskplane config or unrelated task packets

---

## Amendments (Added During Execution)

<!-- Workers add amendments here if issues discovered during execution. -->
