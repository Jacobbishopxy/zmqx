# Task: TP-005 - EventLoop docs, examples, and final verification

**Created:** 2026-06-02
**Size:** S

## Review Level: 1 (Plan Only)

**Assessment:** Final polish and verification after the EventLoop implementation tasks. It touches docs/tests and may make small API polish changes, but should not introduce new behavior.
**Score:** 3/8 — Blast radius: 1, Pattern novelty: 1, Security: 0, Reversibility: 1

## Canonical Task Folder

```
taskplane-tasks/TP-005-event-loop-docs-final-verification/
├── PROMPT.md   ← This file (immutable above --- divider)
├── STATUS.md   ← Execution state (worker updates this)
├── .reviews/   ← Reviewer output (created by the orchestrator runtime)
└── .DONE       ← Created when complete
```

## Mission

Finalize the EventLoop MVP by documenting how it fits Zmqx's existing API styles, adding discoverable examples, and running the final validation sweep. This task should not expand EventLoop behavior beyond TP-002 through TP-004; it is for polish, docs, and full verification.

## Dependencies

- **Task:** TP-004 (transceivers and lifecycle safety must be complete)

## Context to Read First

**Tier 2 (area context):**
- `taskplane-tasks/CONTEXT.md`

**Relevant source files:**
- `README.md` — top-level API style overview
- `docs/quickstart.md` — user-facing examples and context-mode guidance
- `docs/examples.md` — automated/demo test index
- `lib/Zmqx/EventLoop.hs` and `lib/Zmqx.hs` — final public API surface
- `test/test.cabal` and EventLoop tests — validation targets

## Environment

- **Workspace:** `/home/xiey/Code/zmqx`
- **Services required:** None

## File Scope

- `README.md`
- `docs/quickstart.md`
- `docs/examples.md`
- `lib/Zmqx/EventLoop.hs`
- `lib/Zmqx.hs`
- `test/test.cabal`
- `test/EventLoop*.hs`

## Steps

### Step 0: Preflight

- [ ] Required files and paths exist
- [ ] TP-004 is complete and EventLoop targeted tests pass

### Step 1: Documentation plan

> **Plan-review checkpoint** — confirm docs scope is limited to explaining the implemented MVP and both context modes.

- [ ] Identify the final EventLoop API names and semantics from the implementation
- [ ] Plan concise docs for the two context modes: `run` + `withEventLoop`, and `withContext` + `withEventLoopIn`
- [ ] Plan examples for sender, receiver mailbox, callback, and transceiver only if implemented by prior tasks

**Artifacts:**
- STATUS.md discovery notes if implementation semantics differ from planned API

### Step 2: Update user-facing docs

- [ ] Update `README.md` API styles section to mention optional `Zmqx.EventLoop`
- [ ] Update `docs/quickstart.md` with minimal EventLoop examples covering both context modes
- [ ] Update `docs/examples.md` with EventLoop automated regression suites
- [ ] Keep docs concise; defer advanced broker patterns to future work notes if needed

**Artifacts:**
- `README.md` (modified)
- `docs/quickstart.md` (modified)
- `docs/examples.md` (modified)

### Step 3: Public API polish

- [ ] Confirm `Zmqx.EventLoop` is exposed in `zmqx.cabal`
- [ ] Confirm top-level `Zmqx` re-exports are discoverable and do not create confusing name conflicts
- [ ] Confirm Haddock comments mention exclusive socket ownership, callback threading, bounded mailbox behavior, and context mismatch behavior
- [ ] Make only small API polish changes needed for clarity; do not add new feature scope

**Artifacts:**
- `lib/Zmqx/EventLoop.hs` (modified if needed)
- `lib/Zmqx.hs` (modified if needed)
- `zmqx.cabal` (modified if needed)

### Step 4: Final testing and verification

- [ ] Run all EventLoop targeted tests
- [ ] Run full automated suite: `cabal test all`
- [ ] Run build: `cabal build`
- [ ] Fix all failures within the implemented MVP scope

### Step 5: Documentation & Delivery

- [ ] Discoveries logged in STATUS.md
- [ ] If non-blocking future work remains, add concise notes to `taskplane-tasks/CONTEXT.md` Technical Debt / Future Work rather than expanding this task

## Documentation Requirements

**Must Update:**
- `README.md` — mention EventLoop as optional high-level reactor API
- `docs/quickstart.md` — minimal EventLoop usage, including both context modes
- `docs/examples.md` — list new EventLoop automated tests

**Check If Affected:**
- `lib/Zmqx/EventLoop.hs` — Haddock polish if implementation docs are incomplete
- `taskplane-tasks/CONTEXT.md` — only for future work discoveries

## Completion Criteria

- [ ] Docs explain EventLoop's relationship to direct API, monad API, and both context modes
- [ ] EventLoop test suites are discoverable in docs/examples
- [ ] `cabal test all` passes
- [ ] `cabal build` passes
- [ ] No new EventLoop feature scope introduced

## Git Commit Convention

Commits happen at step boundaries. All commits for this task MUST include the task ID:

- **Step completion:** `feat(TP-005): complete Step N — description`
- **Docs:** `docs(TP-005): description`
- **Bug fixes:** `fix(TP-005): description`
- **Hydration:** `hydrate: TP-005 expand Step N checkboxes`

## Do NOT

- Add new EventLoop capabilities beyond the MVP already implemented by TP-002 through TP-004
- Rewrite broad docs unrelated to EventLoop
- Skip the full automated test sweep
- Modify protected Taskplane config or unrelated task packets

---

## Amendments (Added During Execution)

<!-- Workers add amendments here if issues discovered during execution. -->
