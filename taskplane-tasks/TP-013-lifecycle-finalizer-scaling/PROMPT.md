# Task: TP-013 - Improve lifecycle and finalizer registry scaling

**Created:** 2026-06-02
**Size:** M

## Review Level: 2 (Plan and Code)

**Assessment:** Optimizes context/socket lifecycle bookkeeping and teardown behavior. It should not change public API semantics, but resource cleanup correctness and async-exception behavior require plan and code review.
**Score:** 4/8 — Blast radius: 1, Pattern novelty: 2, Security: 0, Reversibility: 1

## Canonical Task Folder

```
taskplane-tasks/TP-013-lifecycle-finalizer-scaling/
├── PROMPT.md   ← This file (immutable above --- divider)
├── STATUS.md   ← Execution state (worker updates this)
├── .reviews/   ← Reviewer output (created by the orchestrator runtime)
└── .DONE       ← Created when complete
```

## Mission

Reduce lifecycle overhead for workloads that open/close many sockets or contexts while preserving deterministic cleanup and finalizer safety. The parallel review found that every socket open registers a weak finalizer in a per-context list, compaction scans/filter/appends the full list, finalizers may trigger repeated scans, and context teardown runs cleanup under strict masking. This task should make registry operations scale better and measure socket churn/teardown improvements.

## Dependencies

- **Task:** TP-006 (benchmark foundation must exist so lifecycle changes can be measured)

## Context to Read First

**Tier 2 (area context):**
- `taskplane-tasks/CONTEXT.md`

**Relevant source and report files:**
- `performance-overheads/concurrency-lifecycle.md` — finalizer registry and teardown findings, if present
- `performance-overheads/haskell-hotpaths.md` — lifecycle allocation findings, if present
- `docs/performance.md` — benchmark commands from TP-006
- `lib/Zmqx/Core/Context.hs` — context lifecycle, global run guard, teardown
- `lib/Zmqx/Core/Socket.hs` — `openSocketIn` registration path
- `lib/Zmqx/Core/SocketFinalizer.hs` — finalizer registry and compaction
- `test/FinalizerRegistry.hs`, `test/RunGuard.hs` — existing lifecycle correctness tests

## Environment

- **Workspace:** `/home/xiey/Code/zmqx`
- **Services required:** None

## File Scope

- `lib/Zmqx/Core/Context.hs`
- `lib/Zmqx/Core/Socket.hs`
- `lib/Zmqx/Core/SocketFinalizer.hs`
- `test/FinalizerScalingAuto.hs`
- `test/FinalizerRegistry.hs`
- `test/RunGuard.hs`
- `test/test.cabal`
- `bench/*`
- `docs/performance.md`
- `taskplane-tasks/CONTEXT.md`

## Steps

### Step 0: Preflight

- [ ] Required files and paths exist
- [ ] TP-006 lifecycle benchmark runs
- [ ] Baseline socket churn, pending-socket, and teardown benchmark numbers are captured before changes

### Step 1: Lifecycle registry design plan

> **Plan-review checkpoint** — confirm cleanup remains deterministic and interruption-safe.

- [ ] Map socket open, explicit close, GC weak finalizer, `pendingSockets`, and context teardown paths
- [ ] Identify the highest-value scaling issue to fix first: registry data structure, compaction frequency, finalizer-triggered scans, pending count, or teardown traversal
- [ ] Choose a design that avoids repeated O(n) scans where practical without leaking sockets or finalizers
- [ ] Define correctness tests for explicit close, GC finalization, context shutdown, interrupted cleanup, and many-socket churn

**Artifacts:**
- STATUS.md notes with chosen design and preserved cleanup invariants

### Step 2: Implement scalable lifecycle bookkeeping

- [ ] Reduce registry compaction/scanning overhead for high-churn workloads
- [ ] Preserve idempotent close behavior and weak finalizer safety
- [ ] Preserve `pendingSockets` semantics or document any intentionally clarified semantics
- [ ] Preserve context teardown behavior for live sockets and blocked child threads

**Artifacts:**
- `lib/Zmqx/Core/SocketFinalizer.hs` (modified)
- `lib/Zmqx/Core/Context.hs` (modified if teardown/pending behavior changes)
- `lib/Zmqx/Core/Socket.hs` (modified if registration behavior changes)

### Step 3: Add lifecycle scaling coverage and benchmark evidence

> **Code review checkpoint** — review cleanup diff plus scaling/correctness tests and benchmark evidence.

- [ ] Add an automated lifecycle scaling test that opens/closes many sockets without relying on external services
- [ ] Register the test in `test/test.cabal`
- [ ] Run existing `FinalizerRegistry` and `RunGuard` tests
- [ ] Run lifecycle benchmark before/after or compare against TP-006 baseline with identical settings

**Artifacts:**
- `test/FinalizerScalingAuto.hs` (new)
- `test/test.cabal` (modified)
- `bench/*` (modified only if needed)
- `docs/performance.md` (modified only if command names/options change)

### Step 4: Testing & Verification

- [ ] Run targeted tests: `cabal test test-finalizer-scaling-auto test-finalizer-registry test-run-guard`
- [ ] Run lifecycle benchmark smoke with RTS allocation summary
- [ ] Run FULL test suite: `cabal test all`
- [ ] Run build: `cabal build`
- [ ] Fix all failures introduced by this task

### Step 5: Documentation & Delivery

- [ ] "Must Update" docs modified if benchmark usage changed
- [ ] "Check If Affected" docs reviewed
- [ ] Discoveries logged in STATUS.md
- [ ] Remaining lifecycle/teardown tradeoffs logged to `taskplane-tasks/CONTEXT.md`

## Documentation Requirements

**Must Update:**
- `docs/performance.md` — only if benchmark command names/options change

**Check If Affected:**
- `taskplane-tasks/CONTEXT.md` — log deferred lifecycle/finalizer future work
- `performance-overheads/*.md` — read-only context; do not rewrite unless explicitly useful for delivery notes

## Completion Criteria

- [ ] Lifecycle registry or teardown overhead is measurably improved for socket churn, or a reviewed measurement records why no safe change was worthwhile
- [ ] Explicit close, GC finalization, `pendingSockets`, and context teardown behavior remain correct
- [ ] New lifecycle scaling test passes
- [ ] Lifecycle benchmark evidence is recorded in STATUS.md
- [ ] `cabal build` and `cabal test all` pass

## Git Commit Convention

Commits happen at **step boundaries**. All commits for this task MUST include the task ID:

- **Step completion:** `feat(TP-013): complete Step N — description`
- **Tests:** `test(TP-013): description`
- **Bug fixes:** `fix(TP-013): description`
- **Hydration:** `hydrate: TP-013 expand Step N checkboxes`

## Do NOT

- Leak sockets or weaken finalizer idempotence
- Hide teardown hangs by making cleanup silently incomplete
- Remove interruption-safety guarantees from existing tests
- Add external-service dependencies to lifecycle tests
- Skip socket-churn/teardown measurement

---

## Amendments (Added During Execution)

<!-- Workers add amendments here if issues discovered during execution. -->
