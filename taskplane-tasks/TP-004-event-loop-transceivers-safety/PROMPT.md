# Task: TP-004 - EventLoop transceivers and lifecycle safety

**Created:** 2026-06-02
**Size:** M

## Review Level: 2 (Plan and Code)

**Assessment:** Adds transceiver support and hardens lifecycle/error semantics for the new EventLoop abstraction. The work is centered on the EventLoop module and tests, but it exercises both send and receive paths together.
**Score:** 4/8 — Blast radius: 2, Pattern novelty: 2, Security: 0, Reversibility: 0

## Canonical Task Folder

```
taskplane-tasks/TP-004-event-loop-transceivers-safety/
├── PROMPT.md   ← This file (immutable above --- divider)
├── STATUS.md   ← Execution state (worker updates this)
├── .reviews/   ← Reviewer output (created by the orchestrator runtime)
└── .DONE       ← Created when complete
```

## Mission

Complete the EventLoop MVP by supporting transceiver endpoints and strengthening lifecycle safety. A transceiver should be usable as both a named sender and a named receiver, while preserving the same worker-owned socket invariant and both context modes.

## Dependencies

- **Task:** TP-003 (receiver mailboxes and callbacks must be complete)

## Context to Read First

**Tier 2 (area context):**
- `taskplane-tasks/CONTEXT.md`

**Relevant source files:**
- `lib/Zmqx/EventLoop.hs` — EventLoop implementation from TP-002/TP-003
- `lib/Zmqx/Dealer.hs`, `lib/Zmqx/Router.hs`, `lib/Zmqx/Pair.hs` — transceiver-capable socket examples
- `test/DealerRouterAuto.hs`, `test/ContextualOpen.hs` — existing round-trip and explicit context patterns
- `test/test.cabal` — test-suite conventions

## Environment

- **Workspace:** `/home/xiey/Code/zmqx`
- **Services required:** None

## File Scope

- `lib/Zmqx/EventLoop.hs`
- `test/EventLoopTransceiverAuto.hs`
- `test/EventLoopSafetyAuto.hs`
- `test/test.cabal`

## Steps

### Step 0: Preflight

- [ ] Required files and paths exist
- [ ] TP-003 is complete and sender/receiver tests pass

### Step 1: Transceiver and safety design

> **Plan-review checkpoint** — confirm transceiver routing, duplicate-name handling, and shutdown semantics before implementation.

- [ ] Define `addTransceiver` constraints and semantics for sockets that support send, receive, and poll-in
- [ ] Decide duplicate-name handling across sender/receiver/transceiver registrations and document it
- [ ] Confirm all public operations after loop exit fail safely without touching worker-owned sockets
- [ ] Confirm worker failures are surfaced after cleanup rather than leaving blocked waiters

**Artifacts:**
- `lib/Zmqx/EventLoop.hs` (modified)

### Step 2: Implement transceiver support

- [ ] Add `addTransceiver` to the public API and `Zmqx` re-export surface
- [ ] Route public `send` to transceiver endpoints as well as sender-only endpoints
- [ ] Deliver incoming transceiver messages via the same mailbox/callback receiver modes
- [ ] Validate transceiver socket contexts against the selected loop context

**Artifacts:**
- `lib/Zmqx/EventLoop.hs` (modified)
- `lib/Zmqx.hs` (modified if needed)

### Step 3: Harden lifecycle and registry behavior

- [ ] Reject duplicate endpoint names deterministically before loop start
- [ ] Ensure stop/exit unblocks pending public `send`/`recv` waiters
- [ ] Ensure exceptions from callbacks or worker receive/send paths are captured and surfaced without deadlocking cleanup
- [ ] Keep bracketed `withEventLoop` / `withEventLoopIn` as the primary lifecycle; do not expose a long-lived mutable start/stop object unless already unavoidable

**Artifacts:**
- `lib/Zmqx/EventLoop.hs` (modified)

### Step 4: Tests

- [ ] Add a transceiver round-trip test, preferably using `Dealer`/`Router` or `Pair`, with multipart frames
- [ ] Add explicit-context transceiver coverage
- [ ] Add duplicate endpoint name and context-mismatch safety tests
- [ ] Add shutdown/unblock regression coverage for pending `recv` if not already covered by TP-003
- [ ] Register new test suites in `test/test.cabal`

**Artifacts:**
- `test/EventLoopTransceiverAuto.hs` (new)
- `test/EventLoopSafetyAuto.hs` (new)
- `test/test.cabal` (modified)

### Step 5: Testing & Verification

> **Code review checkpoint** — review the complete TP-004 diff before final verification.

- [ ] Run EventLoop targeted tests: `cabal test test-event-loop-send-auto test-event-loop-receive-auto test-event-loop-transceiver-auto test-event-loop-safety-auto`
- [ ] Run relevant existing tests: `cabal test test-dealer-router-auto test-contextual-open`
- [ ] Run build: `cabal build`
- [ ] Fix all failures

### Step 6: Documentation & Delivery

- [ ] Haddock comments document transceiver semantics, duplicate-name behavior, and lifecycle safety
- [ ] Discoveries logged in STATUS.md

## Documentation Requirements

**Must Update:**
- `lib/Zmqx/EventLoop.hs` — transceiver and lifecycle safety docs

**Check If Affected:**
- `docs/quickstart.md` — defer broader examples to TP-005 unless needed to clarify semantics
- `README.md` — defer broader docs to TP-005 unless needed to clarify public exports

## Completion Criteria

- [ ] EventLoop transceiver endpoints support send and receive delivery
- [ ] Duplicate-name and context-mismatch behavior is deterministic and tested
- [ ] Shutdown unblocks pending public operations
- [ ] EventLoop targeted tests pass
- [ ] `cabal build` passes

## Git Commit Convention

Commits happen at step boundaries. All commits for this task MUST include the task ID:

- **Step completion:** `feat(TP-004): complete Step N — description`
- **Bug fixes:** `fix(TP-004): description`
- **Tests:** `test(TP-004): description`
- **Hydration:** `hydrate: TP-004 expand Step N checkboxes`

## Do NOT

- Introduce coroutine/async scheduler behavior
- Permit direct public operations to bypass the worker and touch registered sockets
- Weaken the two-context design from TP-002
- Hide callback exceptions by deadlocking or silently losing worker failure state

---

## Amendments (Added During Execution)

<!-- Workers add amendments here if issues discovered during execution. -->
