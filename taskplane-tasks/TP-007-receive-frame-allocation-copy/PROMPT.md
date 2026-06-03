# Task: TP-007 - Reduce receive frame allocation and copy overhead

**Created:** 2026-06-02
**Size:** M

## Review Level: 2 (Plan and Code)

**Assessment:** Optimizes the shared receive hot path used by most socket roles. The public API should remain stable, but the task touches FFI-adjacent memory lifecycle code and needs both design review and code review.
**Score:** 4/8 — Blast radius: 1, Pattern novelty: 2, Security: 0, Reversibility: 1

## Canonical Task Folder

```
taskplane-tasks/TP-007-receive-frame-allocation-copy/
├── PROMPT.md   ← This file (immutable above --- divider)
├── STATUS.md   ← Execution state (worker updates this)
├── .reviews/   ← Reviewer output (created by the orchestrator runtime)
└── .DONE       ← Created when complete
```

## Mission

Reduce allocation and FFI overhead in the receive path while preserving current `ByteString` ownership semantics and public API behavior. The parallel review found that every frame receive allocates/frees a `zmq_msg_t` and copies payload bytes through `ByteString.packCStringLen`; this task should remove avoidable per-frame message-object churn first and only change payload-copy behavior if it can be proven memory-safe.

## Dependencies

- **Task:** TP-006 (benchmark foundation must exist so receive-path changes can be measured)

## Context to Read First

**Tier 2 (area context):**
- `taskplane-tasks/CONTEXT.md`

**Relevant source and report files:**
- `performance-overheads/haskell-hotpaths.md` — receive hot-path findings, if present
- `performance-overheads/ffi-boundary.md` — `zmq_msg_t` and ByteString-copy findings, if present
- `docs/performance.md` — benchmark commands from TP-006
- `lib/Zmqx/Core/Socket.hs` — shared receive implementation
- `lib/Zmqx/Internal/Functions.hs` — message allocation/data helpers
- `lib/Zmqx/Internal/Bindings/Functions.hs` and `lib/Zmqx/Internal/Bindings/Types.hsc` — FFI types/imports
- `test/test.cabal` and existing receive-oriented tests (`ReqRepAuto`, `DealerRouterAuto`, `ItemsPollAuto`)

## Environment

- **Workspace:** `/home/xiey/Code/zmqx`
- **Services required:** None

## File Scope

- `lib/Zmqx/Core/Socket.hs`
- `lib/Zmqx/Internal/Functions.hs`
- `lib/Zmqx/Internal/Bindings/Functions.hs`
- `lib/Zmqx/Internal/Bindings/Types.hsc`
- `c/*`
- `test/ReceivePathAuto.hs`
- `test/test.cabal`
- `bench/*`
- `docs/performance.md`
- `taskplane-tasks/CONTEXT.md`

## Steps

### Step 0: Preflight

- [ ] Required files and paths exist
- [ ] TP-006 benchmark target exists and at least one receive benchmark runs
- [ ] Baseline receive benchmark numbers are captured before changes

### Step 1: Receive-path optimization plan

> **Plan-review checkpoint** — confirm the plan preserves public API behavior, exception semantics, multipart semantics, and memory ownership.

- [ ] Map the current single-frame and multipart receive paths from role modules through `Zmqx.Core.Socket`
- [ ] Choose an optimization that reduces avoidable `zmq_msg_t` allocation/free churn, such as stack allocation or reuse inside a receive loop
- [ ] Explicitly decide whether payload copies remain unchanged; any zero-copy receive design must prove `ByteString` lifetime safety and should be deferred if uncertain
- [ ] Define targeted correctness and benchmark evidence required before completion

**Artifacts:**
- STATUS.md notes with chosen design and deferred alternatives

### Step 2: Implement receive message-object optimization

- [ ] Refactor receive helpers to avoid one Haskell `malloc`/`free` per frame where safe
- [ ] Preserve multipart ordering, `more` detection, and current `Left Error` behavior for `EAGAIN`, `ETERM`, and `EINTR`
- [ ] Keep debug-frame output behavior unchanged when built with `--flag debug`
- [ ] Add or update comments around FFI memory ownership so future send/receive tasks do not accidentally introduce unsafe lifetime bugs

**Artifacts:**
- `lib/Zmqx/Core/Socket.hs` (modified)
- `lib/Zmqx/Internal/Functions.hs` (modified if helper ownership changes)
- `lib/Zmqx/Internal/Bindings/*` or `c/*` (modified only if necessary)

### Step 3: Add receive-path regression coverage and benchmark evidence

> **Code review checkpoint** — review the full receive-path diff plus tests/bench evidence.

- [ ] Add a focused automated test covering single-frame and multipart receive behavior across at least two role pairs
- [ ] Register the test in `test/test.cabal`
- [ ] Run the receive benchmark before/after or compare against TP-006 recorded baseline with identical settings
- [ ] Update `docs/performance.md` only if benchmark invocation or interpretation changes

**Artifacts:**
- `test/ReceivePathAuto.hs` (new)
- `test/test.cabal` (modified)
- `bench/*` (modified only if benchmark coverage gaps are discovered)
- `docs/performance.md` (modified only if needed)

### Step 4: Testing & Verification

- [ ] Run targeted tests: `cabal test test-receive-path-auto` plus existing receive/poll suites affected by the change
- [ ] Run receive benchmark smoke with RTS allocation summary
- [ ] Run FULL test suite: `cabal test all`
- [ ] Run build: `cabal build`
- [ ] Fix all failures introduced by this task

### Step 5: Documentation & Delivery

- [ ] "Must Update" docs modified if benchmark usage changed
- [ ] "Check If Affected" docs reviewed
- [ ] Discoveries logged in STATUS.md
- [ ] Any unsafe or deferred zero-copy receive ideas recorded as future work in `taskplane-tasks/CONTEXT.md`

## Documentation Requirements

**Must Update:**
- `docs/performance.md` — only if benchmark command names/options change

**Check If Affected:**
- `taskplane-tasks/CONTEXT.md` — log deferred receive zero-copy or FFI ownership discoveries
- `performance-overheads/*.md` — read-only context; do not rewrite unless explicitly useful for delivery notes

## Completion Criteria

- [ ] Receive path has less avoidable per-frame message-object allocation or a documented reason why the safer optimization was not viable
- [ ] Public receive API, multipart behavior, and error semantics remain compatible
- [ ] New receive-path regression test passes
- [ ] Receive benchmark evidence is recorded in STATUS.md
- [ ] `cabal build` and `cabal test all` pass

## Git Commit Convention

Commits happen at **step boundaries**. All commits for this task MUST include the task ID:

- **Step completion:** `feat(TP-007): complete Step N — description`
- **Tests:** `test(TP-007): description`
- **Bug fixes:** `fix(TP-007): description`
- **Hydration:** `hydrate: TP-007 expand Step N checkboxes`

## Do NOT

- Change public API types or role-module semantics
- Introduce zero-copy receive unless memory lifetime is proven and reviewed
- Weaken exception, cancellation, or context-termination behavior
- Skip before/after measurement
- Modify unrelated EventLoop, poll, send, or lifecycle code beyond what the receive refactor requires

---

## Amendments (Added During Execution)

<!-- Workers add amendments here if issues discovered during execution. -->
