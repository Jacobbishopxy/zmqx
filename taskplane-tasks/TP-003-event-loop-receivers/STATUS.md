# TP-003: EventLoop receiver mailboxes and callbacks — Status

**Current Step:** Step 5: Documentation & Delivery
**Status:** ✅ Complete
**Last Updated:** 2026-06-02
**Review Level:** 2
**Review Counter:** 10
**Iteration:** 1
**Size:** M

> **Hydration:** Checkboxes represent meaningful outcomes, not individual code changes. Workers expand steps when runtime discoveries warrant it.

---

### Step 0: Preflight
**Status:** ✅ Complete

- [x] Required files and paths exist
- [x] TP-002 complete and sender tests pass

---

### Step 1: Receiver design and poll integration
**Status:** ✅ Complete

- [x] Receiver mailbox/callback semantics confirmed
- [x] Existential receiver poll set uses existing `Zmqx.Core.Poll`
- [x] Receiver context validation uses selected loop context
- [x] Public `recv` semantics defined
- [x] R001: Bounded mailbox API/design records capacity and a bounded primitive
- [x] R001: Public `recv` timeout/missing/stopped/non-mailbox semantics recorded explicitly

---

### Step 2: Implement receiver delivery
**Status:** ✅ Complete

- [x] Worker polls receiver sockets and reads multipart messages
- [x] Bounded mailbox delivery implemented and documented
- [x] Callback delivery implemented and documented
- [x] Public `recv` implemented with timeout/shutdown behavior
- [x] Sender command responsiveness preserved

---

### Step 3: Receiver tests
**Status:** ✅ Complete

- [x] Global-context mailbox test added
- [x] Explicit-context mailbox test added
- [x] Callback delivery test added
- [x] Timeout/missing/non-mailbox behavior covered
- [x] R006: Stopped/shutdown `recv` behavior covered by planned test
- [x] Test suite registered

---

### Step 4: Testing & Verification
**Status:** ✅ Complete

- [x] `cabal test test-event-loop-send-auto` passes
- [x] `cabal test test-event-loop-receive-auto` passes
- [x] `cabal build` passes
- [x] All failures fixed

---

### Step 5: Documentation & Delivery
**Status:** ✅ Complete

- [x] Haddock comments updated
- [x] Discoveries logged

---

## Reviews

| # | Type | Step | Verdict | File |
|---|------|------|---------|------|
| R001 | Plan | 1 | REVISE | `.reviews/R001-plan-step1.md` |
| R002 | Plan | 1 | APPROVE | inline |
| R003 | Code | 1 | APPROVE | inline |
| R004 | Plan | 2 | APPROVE | inline |
| R005 | Code | 2 | APPROVE | inline |
| R006 | Plan | 3 | REVISE | `.reviews/R006-plan-step3.md` |
| R007 | Plan | 3 | APPROVE | inline |
| R008 | Code | 3 | APPROVE | inline |
| R009 | Plan | 4 | APPROVE | inline |
| R010 | Code | 4 | APPROVE | inline |

---

## Discoveries

| Discovery | Disposition | Location |
|-----------|-------------|----------|
| `Control.Concurrent.STM.registerDelay` requires threaded RTS and broke the receiver test executable | Replaced `recv` timeouts with monotonic-clock polling plus STM nonblocking mailbox checks | `lib/Zmqx/EventLoop.hs` |
| `ReceiverMode` cannot compare callback functions lawfully under a precise `Eq` instance | Kept compatibility `Eq` and made callback-mode comparison reflexive/coarse (`Callback _ == Callback _`) | `lib/Zmqx/EventLoop.hs` |

---

## Execution Log

| Timestamp | Action | Outcome |
|-----------|--------|---------|
| 2026-06-02 | Task staged | PROMPT.md and STATUS.md created |
| 2026-06-02 04:35 | Task started | Runtime V2 lane-runner execution |
| 2026-06-02 04:35 | Step 0 started | Preflight |
| 2026-06-02 | Step 0 item completed | Required source, test, context, prompt, and status files exist |
| 2026-06-02 | Step 0 item completed | TP-002 status complete at `taskplane-tasks/TP-002-event-loop-api-send-lifecycle/STATUS.md`; `cabal test test-event-loop-send-auto` passed |
| 2026-06-02 | Step 0 completed | Preflight checks passed |
| 2026-06-02 | Step 1 started | Receiver design and poll integration |
| 2026-06-02 | Plan review R001 | REVISE: bounded mailbox API and explicit `recv` semantics needed |
| 2026-06-02 | Step 1 R001 item completed | Planned `Mailbox Int` capacity and bounded STM mailbox primitive |
| 2026-06-02 | Step 1 R001 item completed | Recorded `recv` success, timeout, missing, stopped, and non-mailbox outcomes |
| 2026-06-02 | Plan review R002 | APPROVE |
| 2026-06-02 | Step 1 item completed | Added per-receiver `Mailbox Int` and `Callback` modes plus `addReceiver`; `cabal build` passes |
| 2026-06-02 | Step 1 item completed | Added existential receiver/runtime wrappers carrying `CanPoll 'PollIn` and building `Poll.Sockets` with `pollIn`/`pollInAlso` |
| 2026-06-02 | Step 1 item completed | Extended selected-context validation to registered receivers with role-specific mismatch errors |
| 2026-06-02 | Step 1 item completed | Added public `recv` timeout, missing, stopped, and non-mailbox semantics; `cabal build` passes |
| 2026-06-02 | Step 1 targeted regression | `cabal test test-event-loop-send-auto` passed |
| 2026-06-02 | Code review R003 | APPROVE |
| 2026-06-02 | Step 1 completed | Code review approved |
| 2026-06-02 | Step 2 started | Implement receiver delivery |
| 2026-06-02 | Plan review R004 | APPROVE |
| 2026-06-02 | Step 2 item completed | Worker polls Step 1 receiver poll sets in bounded slices and reads ready sockets through `Socket.receives_`; `cabal build` passes |
| 2026-06-02 | Step 2 item completed | Mailbox delivery uses bounded `TBQueue` capacity and drops newest messages on overflow without blocking; Haddock documents behavior |
| 2026-06-02 | Step 2 item completed | Callback receivers invoke callbacks on the worker thread and Haddocks document quick/nonblocking and exception behavior |
| 2026-06-02 | Step 2 item completed | `recv` waits on mailbox `TBQueue`s with timeout alternatives and observes loop shutdown through the accepting TVar |
| 2026-06-02 | Step 2 item completed | Worker checks commands before each receiver poll and caps receiver poll slices at 10ms; `cabal test test-event-loop-send-auto` passed |
| 2026-06-02 | Code review R005 | APPROVE |
| 2026-06-02 | Step 2 completed | Code review approved |
| 2026-06-02 | Step 3 started | Receiver tests |
| 2026-06-02 | Plan review R006 | REVISE: stopped-loop `recv` coverage missing |
| 2026-06-02 | Step 3 R006 item completed | Revised receiver test plan to include deterministic stopped-loop `recv` coverage |
| 2026-06-02 | Plan review R007 | APPROVE |
| 2026-06-02 | Step 3 item completed | Added global-context mailbox receiver test with multipart frames; targeted receiver test passes |
| 2026-06-02 | Step 3 item completed | Added explicit-context mailbox receiver test with multipart frames; targeted receiver test passes |
| 2026-06-02 | Step 3 item completed | Added callback receiver test delivering multipart frames through an `MVar`; targeted receiver test passes |
| 2026-06-02 | Step 3 item completed | Covered timeout, missing receiver, and non-mailbox callback receiver `recv` results; targeted receiver test passes |
| 2026-06-02 | Step 3 R006 coverage implemented | Added stopped-loop blocking `recv` regression asserting `ETERM`; targeted receiver test passes |
| 2026-06-02 | Step 3 item completed | Registered `test-event-loop-receive-auto` in `test/test.cabal`; `cabal test test-event-loop-receive-auto` passed |
| 2026-06-02 | Code review R008 | APPROVE |
| 2026-06-02 | Step 3 completed | Code review approved |
| 2026-06-02 | Step 4 started | Testing & Verification |
| 2026-06-02 | Plan review R009 | APPROVE |
| 2026-06-02 | Step 4 item completed | `cabal test test-event-loop-send-auto` passed |
| 2026-06-02 | Step 4 item completed | `cabal test test-event-loop-receive-auto` passed |
| 2026-06-02 | Step 4 item completed | `cabal build` passed |
| 2026-06-02 | Step 4 item completed | No failures remained after sender regression, receiver test, and build |
| 2026-06-02 | Code review R010 | APPROVE |
| 2026-06-02 | Step 4 completed | Code review approved |
| 2026-06-02 | Step 5 started | Documentation & Delivery |
| 2026-06-02 | Step 5 item completed | Updated EventLoop Haddocks for receiver ownership, polling, mailbox/callback, overflow, and shutdown behavior; `cabal build` passes |
| 2026-06-02 | Step 5 item completed | Logged registerDelay/threaded RTS and `ReceiverMode` callback equality discoveries |
| 2026-06-02 | Step 5 completed | Documentation and delivery status complete |
| 2026-06-02 | Task completed | All TP-003 steps complete |
| 2026-06-02 05:23 | Worker iter 1 | done in 2851s, tools: 167 |
| 2026-06-02 05:23 | Task complete | .DONE created |

---

## Blockers

*None*

---

## Notes

- Step 1 plan: register receivers by endpoint with per-receiver modes. `Mailbox Int` carries a positive public capacity and will allocate a bounded STM mailbox; `Callback ([ByteString] -> IO ())` delivers frames on the worker thread. Public `Zmqx.EventLoop.recv loop endpoint timeoutMs` returns `Right (Just frames)` for mailbox messages, `Right Nothing` on timeout, and `Left Error` for missing receivers, stopped loops, or receivers registered without mailbox delivery. Build an existential input poll set through `Zmqx.Core.Poll.pollIn`/`pollInAlso`; validate receiver contexts with the selected loop context; keep EventLoop `send`/`recv` helpers in `Zmqx.EventLoop` while re-exporting the `addReceiver` builder from top-level `Zmqx` beside `addSender`.
- R001 suggestion: the existential receiver wrapper should carry both `CanPoll 'PollIn a` and multipart receive capability so the same socket can be polled, checked through `Ready`, and read through `receives_`.
- R003 suggestion: consider replacing the compatibility `Eq ReceiverMode` instance or making callback comparison law-abiding before downstream code relies on it.
- Step 2 plan: when receivers exist, the worker will drain queued commands first, otherwise poll registered receivers through the Step 1 `Poll.Sockets` set in short slices; ready sockets will be read with `Socket.receives_` and delivered by bounded mailbox write or worker-thread callback. Full mailboxes will drop the newest message deterministically so the loop never blocks.
- Step 3 plan: add one automated test executable covering global and explicit context mailbox receivers with multipart payloads, callback delivery via an `MVar`, timeout, missing, non-mailbox, and stopped-loop `recv` failures, plus register it as `test-event-loop-receive-auto`.
- R006 suggestion: make stopped-loop coverage deterministic by forking a blocking `recv loop endpoint (-1)` inside `withEventLoop`, allowing the bracket to exit, then asserting the forked result is a stopped-loop `Left` rather than a hang or timeout.
- Step 4 plan: rerun the sender regression, receiver auto test, and full `cabal build`; if any command fails, fix failures before marking the all-failures checkbox.
