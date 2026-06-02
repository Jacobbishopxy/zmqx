# Task: TP-002 - EventLoop API, lifecycle, and send path

**Created:** 2026-06-02
**Size:** M

## Review Level: 2 (Plan and Code)

**Assessment:** Introduces a new high-level reactor module and dependency while keeping scope to sender command routing and lifecycle. The pattern is new for this repo, but reversible and does not touch security-sensitive behavior.
**Score:** 4/8 — Blast radius: 2, Pattern novelty: 2, Security: 0, Reversibility: 0

## Canonical Task Folder

```
taskplane-tasks/TP-002-event-loop-api-send-lifecycle/
├── PROMPT.md   ← This file (immutable above --- divider)
├── STATUS.md   ← Execution state (worker updates this)
├── .reviews/   ← Reviewer output (created by the orchestrator runtime)
└── .DONE       ← Created when complete
```

## Mission

Create the foundational `Zmqx.EventLoop` API and implement the worker-owned sender command path. The result should support both existing context modes: `Zmqx.run`/normal `open` via `withEventLoop`, and explicit `withContext`/`openWith` via `withEventLoopIn`.

## Dependencies

- **None**

## Context to Read First

**Tier 2 (area context):**
- `taskplane-tasks/CONTEXT.md`

**Relevant source files:**
- `lib/Zmqx.hs` — public re-export style and global API conventions
- `lib/Zmqx/Core/Context.hs` — global vs explicit context modes
- `lib/Zmqx/Core/Socket.hs` — `Socket`, `context`, capability classes, and send helpers
- `lib/Zmqx/Core/Poll.hs` — poll API to preserve for later receiver work
- `zmqx.cabal` — library module/dependency surface
- `test/test.cabal` and existing `*Auto.hs` tests — test-suite conventions

## Environment

- **Workspace:** `/home/xiey/Code/zmqx`
- **Services required:** None

## File Scope

- `zmqx.cabal`
- `lib/Zmqx.hs`
- `lib/Zmqx/EventLoop.hs`
- `test/EventLoopSendAuto.hs`
- `test/test.cabal`

## Steps

### Step 0: Preflight

- [ ] Required files and paths exist
- [ ] Dependencies satisfied
- [ ] Confirm the prior standalone `XSub CanSends` fix is already committed and not part of this task

### Step 1: Design and API foundation

> **Plan-review checkpoint** — confirm the API shape, context handling, and lifecycle invariants before implementation.

- [ ] Add `stm` to the library dependencies in `zmqx.cabal`
- [ ] Create exposed module `Zmqx.EventLoop`
- [ ] Define the public MVP API: `EventLoop`, `EventLoopSpec`, `ReceiverMode`, `emptySpec`, `addSender`, `withEventLoop`, `withEventLoopIn`, and `send`
- [ ] Re-export the EventLoop API from `Zmqx` without disrupting existing exports

**Artifacts:**
- `zmqx.cabal` (modified)
- `lib/Zmqx/EventLoop.hs` (new)
- `lib/Zmqx.hs` (modified)

### Step 2: Implement lifecycle and sender command routing

- [ ] Implement an event-loop worker that owns registered sender sockets while running
- [ ] Implement public `send` as an STM/MVar command sent to the worker; public calls must not directly touch registered sender sockets
- [ ] Implement `withEventLoop` using the active global context and `withEventLoopIn` using the supplied explicit context
- [ ] Validate all registered socket contexts against the selected loop context before starting
- [ ] Reject missing endpoints and stopped loops with `Left Error` rather than throwing for normal user-visible failures

**Artifacts:**
- `lib/Zmqx/EventLoop.hs` (modified)

### Step 3: Sender tests

- [ ] Add an automated sender test for global-context mode: `Zmqx.run` + normal `open` + `withEventLoop`
- [ ] Add an automated sender test for explicit-context mode: `withContext` + `openWith` + `withEventLoopIn`
- [ ] Cover missing sender and send-after-loop-exit behavior
- [ ] Register the test suite in `test/test.cabal`

**Artifacts:**
- `test/EventLoopSendAuto.hs` (new)
- `test/test.cabal` (modified)

### Step 4: Testing & Verification

> **Code review checkpoint** — review the complete TP-002 diff before final verification.

- [ ] Run targeted test: `cabal test test-event-loop-send-auto`
- [ ] Run build: `cabal build`
- [ ] Fix all failures

### Step 5: Documentation & Delivery

- [ ] Add concise Haddock comments documenting exclusive ownership of registered sockets while the loop runs
- [ ] Discoveries logged in STATUS.md

## Documentation Requirements

**Must Update:**
- `lib/Zmqx/EventLoop.hs` — Haddock comments for new public API and invariants

**Check If Affected:**
- `README.md` — defer broader docs to TP-005 unless a small note is necessary
- `docs/quickstart.md` — defer broader docs to TP-005 unless a small note is necessary

## Completion Criteria

- [ ] EventLoop sender API supports global and explicit context modes
- [ ] Public send path is worker-owned and command-based
- [ ] Context mismatch is rejected before loop start
- [ ] Targeted EventLoop sender test passes
- [ ] `cabal build` passes

## Git Commit Convention

Commits happen at step boundaries. All commits for this task MUST include the task ID:

- **Step completion:** `feat(TP-002): complete Step N — description`
- **Bug fixes:** `fix(TP-002): description`
- **Tests:** `test(TP-002): description`
- **Hydration:** `hydrate: TP-002 expand Step N checkboxes`

## Do NOT

- Implement receiver polling or transceiver support in this task; leave that for TP-003/TP-004
- Introduce a third context mode
- Let public `EventLoop.send` directly use registered application sockets
- Skip context validation
- Modify Taskplane config or task packets outside this task unless required by orchestrator bookkeeping

---

## Amendments (Added During Execution)

<!-- Workers add amendments here if issues discovered during execution. -->
