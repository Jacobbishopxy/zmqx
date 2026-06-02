# Task: TP-003 - EventLoop receiver mailboxes and callbacks

**Created:** 2026-06-02
**Size:** M

## Review Level: 2 (Plan and Code)

**Assessment:** Extends the new EventLoop worker to poll receiver sockets and deliver multipart messages through mailboxes or callbacks. This adapts existing polling patterns and must preserve REQ polling behavior by using `Zmqx.Core.Poll` rather than raw `zmq_poll`.
**Score:** 4/8 — Blast radius: 2, Pattern novelty: 2, Security: 0, Reversibility: 0

## Canonical Task Folder

```
taskplane-tasks/TP-003-event-loop-receivers/
├── PROMPT.md   ← This file (immutable above --- divider)
├── STATUS.md   ← Execution state (worker updates this)
├── .reviews/   ← Reviewer output (created by the orchestrator runtime)
└── .DONE       ← Created when complete
```

## Mission

Implement EventLoop receiver support: the worker polls registered receiving sockets, reads complete multipart messages with `receives`, and delivers them either to a bounded mailbox for public `recv` or to a callback executed on the loop worker. This should build on TP-002 without changing the two-context API shape.

## Dependencies

- **Task:** TP-002 (EventLoop API, lifecycle, and send path must be complete)

## Context to Read First

**Tier 2 (area context):**
- `taskplane-tasks/CONTEXT.md`

**Relevant source files:**
- `lib/Zmqx/EventLoop.hs` — EventLoop foundation from TP-002
- `lib/Zmqx/Core/Poll.hs` — use existing poll helpers and preserve REQ buffering behavior
- `lib/Zmqx/Core/Socket.hs` — receiving capability classes and socket context field
- `test/ReceivesFor.hs`, `test/ItemsPollAuto.hs`, `test/ReqPoll.hs` — existing receive/poll test patterns
- `test/test.cabal` — test-suite conventions

## Environment

- **Workspace:** `/home/xiey/Code/zmqx`
- **Services required:** None

## File Scope

- `lib/Zmqx/EventLoop.hs`
- `test/EventLoopReceiveAuto.hs`
- `test/test.cabal`

## Steps

### Step 0: Preflight

- [ ] Required files and paths exist
- [ ] TP-002 is complete and its EventLoop sender tests pass

### Step 1: Receiver design and poll integration

> **Plan-review checkpoint** — confirm receiver delivery semantics and how the loop builds/polls existential receiver sets before implementation.

- [ ] Confirm `ReceiverMode` supports bounded mailbox delivery and callback delivery
- [ ] Build the poll set from registered receivers using existing `Zmqx.Core.Poll.pollIn` / `pollInAlso`
- [ ] Ensure receiver context validation uses the same selected loop context as TP-002
- [ ] Define public `recv :: EventLoop -> Text -> Int -> IO (Either Error (Maybe [ByteString]))` semantics: timeout returns `Right Nothing`; missing/stopped/non-mailbox endpoints return `Left Error`

**Artifacts:**
- `lib/Zmqx/EventLoop.hs` (modified)

### Step 2: Implement receiver delivery

- [ ] Worker polls receiver sockets and reads full multipart messages via `receives_`
- [ ] Mailbox delivery is bounded; under overflow, choose and document a deterministic behavior that does not deadlock the loop
- [ ] Callback delivery runs on the event-loop worker; document callbacks must be quick/nonblocking
- [ ] Public `recv` reads from the named mailbox with timeout and unblocks cleanly when the loop exits
- [ ] Preserve sender command responsiveness while receivers are being polled, using a bounded poll slice or wakeup design

**Artifacts:**
- `lib/Zmqx/EventLoop.hs` (modified)

### Step 3: Receiver tests

- [ ] Add global-context mailbox receive test with multipart frames
- [ ] Add explicit-context mailbox receive test
- [ ] Add callback delivery test
- [ ] Cover timeout, missing receiver, and receiver registered without mailbox behavior
- [ ] Register the test suite in `test/test.cabal`

**Artifacts:**
- `test/EventLoopReceiveAuto.hs` (new)
- `test/test.cabal` (modified)

### Step 4: Testing & Verification

> **Code review checkpoint** — review the complete TP-003 diff before final verification.

- [ ] Run targeted sender regression: `cabal test test-event-loop-send-auto`
- [ ] Run targeted receiver test: `cabal test test-event-loop-receive-auto`
- [ ] Run build: `cabal build`
- [ ] Fix all failures

### Step 5: Documentation & Delivery

- [ ] Haddock comments document mailbox/callback semantics and shutdown behavior
- [ ] Discoveries logged in STATUS.md

## Documentation Requirements

**Must Update:**
- `lib/Zmqx/EventLoop.hs` — receiver, mailbox, callback, timeout, and shutdown semantics

**Check If Affected:**
- `docs/quickstart.md` — defer broader examples to TP-005 unless needed to clarify semantics

## Completion Criteria

- [ ] EventLoop can receive multipart messages into mailboxes
- [ ] EventLoop can run receiver callbacks
- [ ] Public `recv` timeout/missing/stopped semantics are tested
- [ ] TP-002 sender tests still pass
- [ ] `cabal build` passes

## Git Commit Convention

Commits happen at step boundaries. All commits for this task MUST include the task ID:

- **Step completion:** `feat(TP-003): complete Step N — description`
- **Bug fixes:** `fix(TP-003): description`
- **Tests:** `test(TP-003): description`
- **Hydration:** `hydrate: TP-003 expand Step N checkboxes`

## Do NOT

- Implement transceiver-specific public routing in this task; leave that to TP-004
- Roll a raw `zmq_poll` receiver path that bypasses `Zmqx.Core.Poll` REQ handling
- Let callbacks perform long blocking work without documenting the loop-thread risk
- Skip sender regression tests

---

## Amendments (Added During Execution)

<!-- Workers add amendments here if issues discovered during execution. -->
