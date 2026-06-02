# TP-004: EventLoop transceivers and lifecycle safety — Status

**Current Step:** Step 6: Documentation & Delivery
**Status:** ✅ Complete
**Last Updated:** 2026-06-02
**Review Level:** 2
**Review Counter:** 11
**Iteration:** 2
**Size:** M

> **Hydration:** Checkboxes represent meaningful outcomes, not individual code changes. Workers expand steps when runtime discoveries warrant it.

---

### Step 0: Preflight
**Status:** ✅ Complete

- [x] Required files and paths exist
- [x] TP-003 complete and sender/receiver tests pass

---

### Step 1: Transceiver and safety design
**Status:** ✅ Complete

- [x] `addTransceiver` constraints and semantics defined
- [x] Duplicate-name behavior decided and documented
- [x] Post-exit public operation safety confirmed
- [x] Worker failure surfacing confirmed

---

### Step 2: Implement transceiver support
**Status:** ✅ Complete

- [x] `addTransceiver` added to public API/re-exports
- [x] Public `send` routes to transceivers
- [x] Incoming transceiver messages use receiver modes
- [x] Context validation covers transceivers

---

### Step 3: Harden lifecycle and registry behavior
**Status:** ✅ Complete

- [x] Duplicate names rejected before loop start
- [x] Stop/exit unblocks waiters
- [x] Worker/callback exceptions captured and surfaced safely
- [x] Bracketed lifecycle remains primary API

---

### Step 4: Tests
**Status:** ✅ Complete

- [x] Transceiver multipart round-trip test added
- [x] Explicit-context transceiver test added
- [x] Duplicate-name/context-mismatch tests added
- [x] Shutdown/unblock regression covered
- [x] Test suites registered

---

### Step 5: Testing & Verification
**Status:** ✅ Complete

- [x] EventLoop targeted tests pass
- [x] Existing relevant tests pass
- [x] `cabal build` passes
- [x] All failures fixed
- [x] Shutdown-aware blocked sends return `ETERM` instead of deadlocking
- [x] Blocked-send shutdown regression test passes

---

### Step 6: Documentation & Delivery
**Status:** ✅ Complete

- [x] Haddock comments updated
- [x] Discoveries logged

---

## Reviews

| # | Type | Step | Verdict | File |
|---|------|------|---------|------|
| R001 | Plan | 1 | APPROVE | inline |
| R002 | Code | 1 | APPROVE | inline |
| R003 | Plan | 2 | APPROVE | inline |
| R004 | Code | 2 | APPROVE | inline |
| R005 | Plan | 3 | APPROVE | inline |
| R006 | Code | 3 | APPROVE | inline |
| R007 | Plan | 4 | APPROVE | inline |
| R008 | Code | 4 | APPROVE | inline |
| R009 | Plan | 5 | APPROVE | inline |
| R010 | Code | 5 | REVISE | .reviews/R010-code-step5.md |
| R011 | Code | 5 | APPROVE | inline |

---

## Discoveries

| Discovery | Disposition | Location |
|-----------|-------------|----------|
| Worker-owned blocking sends must observe shutdown instead of waiting inside `Socket.send_` | Fixed with shutdown-aware nonblocking retry slices and blocked-send regression | `lib/Zmqx/EventLoop.hs`, `test/EventLoopSafetyAuto.hs` |

---

## Execution Log

| Timestamp | Action | Outcome |
|-----------|--------|---------|
| 2026-06-02 | Task staged | PROMPT.md and STATUS.md created |
| 2026-06-02 05:23 | Task started | Runtime V2 lane-runner execution |
| 2026-06-02 05:23 | Step 0 started | Preflight |
| 2026-06-02 | Step 0 item completed | Required source, test, context, prompt, and TP-003 status paths exist |
| 2026-06-02 | Step 0 item completed | TP-003 status is complete; `cabal test test-event-loop-send-auto test-event-loop-receive-auto` passed |
| 2026-06-02 | Step 0 completed | Preflight checks passed |
| 2026-06-02 | Step 1 started | Transceiver and safety design |
| 2026-06-02 | Plan review R001 | APPROVE |
| 2026-06-02 | Step 1 item completed | Documented `addTransceiver` as a worker-owned send/receive/poll-in endpoint using public `send` and receiver delivery modes |
| 2026-06-02 | Step 1 item completed | Documented endpoint names as one namespace with deterministic pre-worker startup failure on duplicates |
| 2026-06-02 | Step 1 item completed | Documented post-exit public operations as state/result checks that avoid registered sockets |
| 2026-06-02 | Step 1 item completed | Documented worker failures as recorded results that wake and surface to public waiters |
| 2026-06-02 | Code review R002 | APPROVE |
| 2026-06-02 | Step 1 completed | Code review approved transceiver and safety design docs |
| 2026-06-02 | Step 2 started | Implement transceiver support |
| 2026-06-02 | Plan review R003 | APPROVE |
| 2026-06-02 | Step 2 item completed | Added `addTransceiver` to `Zmqx.EventLoop` and the `Zmqx` re-export surface |
| 2026-06-02 05:37 | Worker iter 1 | done in 818s, tools: 52 |
| 2026-06-02 | Step 2 item completed | Public `send` now routes to sender-only and transceiver endpoints through the worker sender map |
| 2026-06-02 | Step 2 item completed | Transceiver endpoints are prepared as receiver runtimes and use mailbox/callback delivery modes |
| 2026-06-02 | Step 2 item completed | Context validation checks transceiver sockets against the selected loop context |
| 2026-06-02 | Step 2 verification | `cabal build` passed for transceiver support changes |
| 2026-06-02 | Code review R004 | APPROVE |
| 2026-06-02 | Step 2 completed | Code review approved transceiver implementation |
| 2026-06-02 | Step 3 started | Harden lifecycle and registry behavior |
| 2026-06-02 | Plan review R005 | APPROVE |
| 2026-06-02 | Step 3 item completed | Duplicate endpoint names are tracked during spec construction and rejected before worker startup |
| 2026-06-02 | Step 3 item completed | Stopped public waiters synchronize with worker completion so stop/exit wakes them safely |
| 2026-06-02 | Step 3 item completed | Worker result capture now records failures before closing accepting state for callback/receive/poll exception surfacing |
| 2026-06-02 | Step 3 item completed | Public lifecycle remains limited to bracketed `withEventLoop` / `withEventLoopIn` helpers |
| 2026-06-02 | Step 3 verification | `cabal build` passed for lifecycle hardening changes |
| 2026-06-02 | Code review R006 | APPROVE |
| 2026-06-02 | Step 3 completed | Code review approved lifecycle and registry hardening |
| 2026-06-02 | Step 4 started | Tests |
| 2026-06-02 | Plan review R007 | APPROVE |
| 2026-06-02 | Step 4 item completed | Added global-context transceiver multipart round-trip coverage |
| 2026-06-02 | Step 4 item completed | Added explicit-context transceiver round-trip coverage |
| 2026-06-02 | Step 4 item completed | Added duplicate endpoint and transceiver context-mismatch safety coverage |
| 2026-06-02 | Step 4 item completed | Added shutdown regression coverage for pending `recv` waiters |
| 2026-06-02 | Step 4 item completed | Registered `test-event-loop-transceiver-auto` and `test-event-loop-safety-auto`; both new suites passed |
| 2026-06-02 | Code review R008 | APPROVE |
| 2026-06-02 | Step 4 completed | Code review approved transceiver and safety tests |
| 2026-06-02 | Step 5 started | Testing & Verification |
| 2026-06-02 | Plan review R009 | APPROVE |
| 2026-06-02 | Step 5 item completed | `cabal test test-event-loop-send-auto test-event-loop-receive-auto test-event-loop-transceiver-auto test-event-loop-safety-auto` passed |
| 2026-06-02 | Step 5 item completed | `cabal test test-dealer-router-auto test-contextual-open` passed |
| 2026-06-02 | Step 5 item completed | `cabal build` passed |
| 2026-06-02 | Step 5 item completed | No failures remained after verification |
| 2026-06-02 | Code review R010 | REVISE: blocked worker sends can deadlock shutdown; add shutdown-aware send path and regression |
| 2026-06-02 | Step 5 revision completed | Blocking sender roles now send in nonblocking retry slices and return `ETERM` when shutdown flips accepting false |
| 2026-06-02 | Step 5 revision completed | Added blocked-send shutdown regression to `test-event-loop-safety-auto`; suite passed |
| 2026-06-02 | Step 5 re-verification | EventLoop targeted tests, relevant existing tests, and `cabal build` passed after R010 fixes |
| 2026-06-02 | Code review R011 | APPROVE |
| 2026-06-02 | Step 5 completed | Code review approved verification and R010 shutdown fix |
| 2026-06-02 | Step 6 started | Documentation & Delivery |
| 2026-06-02 | Step 6 item completed | Haddock comments updated for transceiver namespace and shutdown-aware send semantics |
| 2026-06-02 | Step 6 item completed | Discoveries table records blocked-send shutdown finding and fix |
| 2026-06-02 | Step 6 completed | Documentation and delivery status finalized |
| 2026-06-02 | Task completed | All TP-004 steps complete |
| 2026-06-02 06:22 | Worker iter 2 | done in 2698s, tools: 155 |
| 2026-06-02 06:22 | Task complete | .DONE created |

---

## Blockers

*None*

---

## Notes

- Step 1 plan: add a first-class transceiver registration whose socket must satisfy `CanSend`, `CanReceives`, and `CanPoll 'PollIn`; store transceivers separately in the spec but merge them into the worker sender lookup and receiver polling/delivery runtime so public `send` and `recv` still route through the worker.
- Step 1 plan: reject duplicate endpoint names deterministically at loop start across sender, receiver, and transceiver registries; track duplicates in the immutable spec so validation can fail before worker startup without changing the builder API.
- Step 1 plan: keep `withEventLoop`/`withEventLoopIn` bracket ownership; once `accepting` becomes false, public `send`/`recv` return stopped errors or rethrow worker failures without touching registered sockets.
- Step 1 plan: worker send/receive/callback exceptions should set `accepting` false, finish/throw through `workerDone`, and wake public waiters rather than block cleanup.
- Review R001: plan Step 1 APPROVE.
- Review R002: code Step 1 APPROVE.
- Review R004: code Step 2 APPROVE.
- Review R005: plan Step 3 APPROVE.
- Review R006: code Step 3 APPROVE.
- Review R007: plan Step 4 APPROVE.
- Review R008: code Step 4 APPROVE.
- Review R009: plan Step 5 APPROVE.
- Review R010: code Step 5 REVISE; required fix is shutdown-aware handling for a worker send blocked on an unwritable/unconnected socket.
- Review R011: code Step 5 APPROVE.
- Review R010 suggestion: consider top-level timeouts around new transceiver/safety suite `main` bodies to make future deadlocks fail fast.
- Step 2 plan: introduce `Transceiver` storage plus `addTransceiver`; reuse sender command handling by merging sender-only and transceiver send capabilities, and reuse receiver preparation/polling by converting transceivers into receiver runtimes with the requested `ReceiverMode`.
- Step 2 plan: extend context validation to transceivers and re-export only the new builder from `Zmqx`, leaving existing top-level socket `send`/`receives` aliases unchanged.
| 2026-06-02 05:35 | Review R003 | plan Step 2: APPROVE |
| 2026-06-02 05:42 | Review R004 | code Step 2: APPROVE |
| 2026-06-02 05:45 | Review R005 | plan Step 3: APPROVE |
| 2026-06-02 05:53 | Review R006 | code Step 3: APPROVE |
| 2026-06-02 05:55 | Review R007 | plan Step 4: APPROVE |
| 2026-06-02 06:02 | Review R008 | code Step 4: APPROVE |
| 2026-06-02 06:03 | Review R009 | plan Step 5: APPROVE |
| 2026-06-02 06:10 | Review R010 | code Step 5: REVISE |
| 2026-06-02 06:19 | Review R011 | code Step 5: APPROVE |
