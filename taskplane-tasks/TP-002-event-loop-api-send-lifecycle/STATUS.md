# TP-002: EventLoop API, lifecycle, and send path — Status

**Current Step:** Step 5: Documentation & Delivery
**Status:** ✅ Complete
**Last Updated:** 2026-06-02
**Review Level:** 2
**Review Counter:** 16
**Iteration:** 3
**Size:** M

> **Hydration:** Checkboxes represent meaningful outcomes, not individual code changes. Workers expand steps when runtime discoveries warrant it.

---

### Step 0: Preflight
**Status:** ✅ Complete

- [x] Required files and paths exist
- [x] Dependencies satisfied
- [x] Prior `XSub CanSends` fix is committed and out of scope

---

### Step 1: Design and API foundation
**Status:** ✅ Complete

- [x] `stm` dependency added
- [x] `Zmqx.EventLoop` exposed module created
- [x] Public MVP API defined
- [x] EventLoop API re-exported from `Zmqx`
- [x] Public exposure strategy for EventLoop `send` vs existing `Zmqx.send` documented and implemented
- [x] Signature-level API shape for loop actions, sender keys, and normal `Left Error` failures documented

---

### Step 2: Implement lifecycle and sender command routing
**Status:** ✅ Complete

- [x] Worker owns registered sender sockets while running
- [x] Public `send` uses worker command path
- [x] Both `withEventLoop` and `withEventLoopIn` implemented
- [x] Socket context validation implemented
- [x] Normal missing/stopped failures return `Left Error`
- [x] Send command exceptions always complete caller replies and expose worker failure instead of hanging
- [x] Worker completion signal remains observable to all queued send callers during cleanup
- [x] Accepted send replies take precedence over worker terminal status races

---

### Step 3: Sender tests
**Status:** ✅ Complete

- [x] Global-context sender test added
- [x] Explicit-context sender test added
- [x] Missing/stopped behavior covered
- [x] Send-command throw/no-hang cleanup regression covered
- [x] Accepted/queued send worker-failure race regression covered
- [x] Test suite registered
- [x] Concurrent accepted `push` send keeps its own `Right ()` result while queued `rep` sends fail the worker
- [x] Deterministic accepted-send race regression fails old workerDone-first ordering

---

### Step 4: Testing & Verification
**Status:** ✅ Complete

- [x] Targeted test `cabal test test-event-loop-send-auto` passes
- [x] Build `cabal build` passes
- [x] All failures fixed

---

### Step 5: Documentation & Delivery
**Status:** ✅ Complete

- [x] Haddock comments added for new API and socket ownership invariant
- [x] Discoveries logged

---

## Reviews

| # | Type | Step | Verdict | File |
|---|------|------|---------|------|
| R001 | Plan | 1 | REVISE | `.reviews/R001-plan-step1.md` |
| R002 | Plan | 1 | APPROVE | `.reviews/R002-plan-step1.md` |
| R003 | Code | 1 | APPROVE | `.reviews/R003-code-step1.md` |
| R004 | Plan | 2 | UNAVAILABLE | inline |
| R005 | Code | 2 | REVISE | `.reviews/R005-code-step2.md` |
| R006 | Code | 2 | UNAVAILABLE | inline |
| R007 | Code | 2 | REVISE | `.reviews/R007-code-step2.md` |
| R008 | Code | 2 | UNAVAILABLE | inline |
| R009 | Code | 2 | REVISE | `.reviews/R009-code-step2.md` |
| R010 | Code | 2 | APPROVE | `.reviews/R010-code-step2.md` |
| R011 | Plan | 3 | APPROVE | `.reviews/R011-plan-step3.md` |
| R012 | Code | 3 | REVISE | `.reviews/R012-code-step3.md` |
| R013 | Code | 3 | REVISE | `.reviews/R013-code-step3.md` |
| R014 | Code | 4 | UNAVAILABLE | inline |
| R015 | Code | 4 | UNAVAILABLE | inline |
| R016 | Code | 4 | APPROVE | `.reviews/R016-code-step4.md` |

---

## Discoveries

| Discovery | Disposition | Location |
|-----------|-------------|----------|
| Context-mismatch rejection remains a suggestion-level test gap from reviews | Logged for future test hardening; not blocking TP-002 | `test/EventLoopSendAuto.hs` |
| Environment-variable delay hook is useful for the deterministic race regression but is production-visible | Logged as future cleanup to CPP-gate or replace with an internal-only hook | `lib/Zmqx/EventLoop.hs` |

---

## Execution Log

| Timestamp | Action | Outcome |
|-----------|--------|---------|
| 2026-06-02 | Task staged | PROMPT.md and STATUS.md created |
| 2026-06-02 03:10 | Task started | Runtime V2 lane-runner execution |
| 2026-06-02 03:10 | Step 0 started | Preflight |
| 2026-06-02 | Step 0 completed | Preflight checks passed |
| 2026-06-02 | Step 1 started | Design and API foundation |
| 2026-06-02 | Plan review R001 | REVISE: resolve EventLoop `send` exposure strategy |
| 2026-06-02 | Step 1 plan revised | Existing `Zmqx.send` remains socket-only; loop send stays under `Zmqx.EventLoop.send` |
| 2026-06-02 | Plan review R002 | APPROVE |
| 2026-06-02 | Step 1 item completed | Added `stm` library dependency |
| 2026-06-02 | Step 1 item completed | Created and exposed `Zmqx.EventLoop` module |
| 2026-06-02 | Step 1 item completed | Defined MVP EventLoop signatures; `cabal build` confirmed the API scaffold compiles |
| 2026-06-02 | Step 1 item completed | Re-exported EventLoop foundation from `Zmqx`; `Zmqx.send` remains unchanged |
| 2026-06-02 | Step 1 R001 item completed | EventLoop `send` stays in `Zmqx.EventLoop`; top-level `Zmqx.send` remains the existing socket send |
| 2026-06-02 | Step 1 R001 item completed | Documented loop action style, `Text` sender keys, and normal `Left Error` failure shape |
| 2026-06-02 | Code review R003 | APPROVE |
| 2026-06-02 | Step 1 completed | Code review approved |
| 2026-06-02 | Step 2 started | Lifecycle and sender command routing |
| 2026-06-02 | Plan review Step 2 | UNAVAILABLE; proceeding with caution |
| 2026-06-02 | Step 2 item completed | Worker thread now retains registered senders while loop is running |
| 2026-06-02 | Step 2 item completed | Public `Zmqx.EventLoop.send` enqueues STM commands and waits on an MVar reply |
| 2026-06-02 | Step 2 item completed | Implemented global-context and explicit-context loop brackets |
| 2026-06-02 | Step 2 item completed | Registered sender contexts are validated before worker startup |
| 2026-06-02 | Step 2 item completed | Missing sender and stopped-loop sends return `Left Error` values |
| 2026-06-02 | Code review R005 | REVISE: send command exceptions can strand callers |
| 2026-06-02 | Step 2 R005 item completed | Per-command exception replies and worker-done observation added; `cabal build` passes |
| 2026-06-02 | Code review R006 | UNAVAILABLE; retried |
| 2026-06-02 | Code review R007 | REVISE: worker completion MVar consumed during cleanup |
| 2026-06-02 | Step 2 R007 item completed | Cleanup now uses `readMVar` so worker completion remains latched for queued send waiters; `cabal build` passes |
| 2026-06-02 | Code review R008 | UNAVAILABLE; retried |
| 2026-06-02 | Code review R009 | REVISE: accepted send reply can lose race to worker terminal status |
| 2026-06-02 | Step 2 R009 item completed | `waitForSendReply` re-checks command replies before acting on worker terminal status; `cabal build` passes |
| 2026-06-02 | Code review R010 | APPROVE |
| 2026-06-02 | Step 2 completed | Code review approved |
| 2026-06-02 | Step 3 started | Sender tests |
| 2026-06-02 | Step 3 hydrated | Added R005/R007/R009/R010 failure and race regression test outcomes |
| 2026-06-02 | Plan review R011 | APPROVE |
| 2026-06-02 | Step 3 item completed | Added global-context event-loop sender test |
| 2026-06-02 | Step 3 item completed | Added explicit-context event-loop sender test |
| 2026-06-02 | Step 3 item completed | Added missing-sender and send-after-stop coverage |
| 2026-06-02 | Step 3 item completed | Added bounded throwing-send no-hang cleanup regression |
| 2026-06-02 | Step 3 item completed | Added accepted-send plus concurrent queued failure completion regression |
| 2026-06-02 | Step 3 item completed | Registered `test-event-loop-send-auto` suite |
| 2026-06-02 | Code review R012 | REVISE: accepted send/failure race regression was not concurrent enough |
| 2026-06-02 | Step 3 R012 item completed | Concurrent accepted push send now asserts its own `Right ()` result while queued rep sends fail worker; targeted test passes |
| 2026-06-02 | Code review R013 | REVISE: accepted send/failure regression still would pass old ordering |
| 2026-06-02 04:15 | Worker iter 1 | done in 3885s, tools: 166 |
| 2026-06-02 | Step 3 R013 item completed | Added gated deterministic accepted-send race regression with reply-delay hook; targeted test passes |
| 2026-06-02 04:20 | Worker iter 2 | done in 319s, tools: 24 |
| 2026-06-02 04:20 | Step 4 started | Testing & Verification |
| 2026-06-02 | Step 4 item completed | `cabal test test-event-loop-send-auto` passed |
| 2026-06-02 | Step 4 item completed | `cabal build` passed |
| 2026-06-02 | Step 4 item completed | No failures remained after targeted test and build |
| 2026-06-02 | Code review R014/R015 | UNAVAILABLE; retried with Step 4 baseline |
| 2026-06-02 | Code review R016 | APPROVE |
| 2026-06-02 | Step 4 completed | Code review approved |
| 2026-06-02 | Step 5 started | Documentation & Delivery |
| 2026-06-02 | Step 5 item completed | Added EventLoop Haddocks for public API and exclusive sender ownership; `cabal build` passed |
| 2026-06-02 | Step 5 item completed | Logged review-discovered follow-up items in Discoveries |
| 2026-06-02 | Step 5 completed | Documentation and delivery status complete |
| 2026-06-02 | Task completed | All TP-002 steps complete |
| 2026-06-02 04:35 | Worker iter 3 | done in 871s, tools: 47 |
| 2026-06-02 04:35 | Task complete | .DONE created |

---

## Blockers

*None*

---

## Notes

- R001 suggestion: keep Step 1 focused on opaque public types/stable signatures and document socket-ownership invariant from the start; leave worker-thread internals for Step 2.
- Step 1 revised plan: keep the existing top-level `Zmqx.send :: CanSend socket => socket -> ByteString -> IO (Either Error ())` intact. Expose the new event-loop send as `Zmqx.EventLoop.send :: EventLoop -> Text -> ByteString -> IO (Either Error ())`. Re-export EventLoop types/builders/lifecycle helpers from `Zmqx`, but do not re-export `Zmqx.EventLoop.send` unqualified from `Zmqx` to avoid changing or ambiguating existing `Zmqx.send` callers. Callers needing loop sends import `Zmqx.EventLoop` qualified or directly. `EventLoopSpec` maps `Text` sender keys/endpoints to registered `CanSend` sockets; `withEventLoop`/`withEventLoopIn` bracket a loop for an action `EventLoop -> IO a`; normal missing sender/stopped-loop failures return `Left Error` using `ENOENT`/`ETERM`-style errors.
- R002 suggestions: keep the export policy explicit in EventLoop docs/Haddocks, and state that receiver polling is intentionally out of scope for TP-002.
- R003 suggestion: when Step 2 replaces placeholder lifecycle/send behavior, keep ownership Haddocks accurate for both global and explicit context modes.
- R005 test gap to address in Step 3: add a regression for a send command whose socket operation fails/throws, verifying `Zmqx.EventLoop.send` does not hang and finalization remains joinable.
- R005 suggestion: consider separating stop-requested state from worker-done state so stop can join/report worker completion even if the worker sets stopped itself.
- R007 test gap to address in Step 3: include multiple concurrent/queued sends where one socket operation throws and all waiting callers complete while cleanup remains joinable.
- R007 suggestion: polling `threadDelay` wait could later be replaced with an STM/blocking wait over a reply/status primitive.
- R009 test gap to address in Step 3: add an accepted-send race regression around loop shutdown and/or later worker failure so callers receive their command reply rather than stopped/terminal status.
- R009 suggestion: consider replacing the polling loop with an STM latch or other blocking wait to remove ordering races more directly.
- R010 test gap: Step 3 still needs automated coverage for global/explicit sends, missing/stopped behavior, and failure/race regressions.
- R010 suggestion: consider replacing the 1ms polling loop in `waitForSendReply` with a blocking STM/MVar wait in later cleanup.
- R011 suggestions: consider adding context-mismatch rejection coverage, and use bounded timeouts for no-hang/race regressions.
- R012 suggestion: consider checking specific `ENOENT`/`ETERM` errno values for missing/stopped assertions.
- R013 suggestion: inspect queued illegal-send results rather than only filled MVars so the test verifies worker-failure observation.
| 2026-06-02 03:16 | Review R001 | plan Step 1: REVISE |
| 2026-06-02 03:19 | Review R002 | plan Step 1: APPROVE |
| 2026-06-02 03:26 | Review R003 | code Step 1: APPROVE |
| 2026-06-02 03:35 | Review R005 | code Step 2: REVISE |
| 2026-06-02 03:42 | Review R007 | code Step 2: REVISE |
| 2026-06-02 03:50 | Review R009 | code Step 2: REVISE |
| 2026-06-02 03:55 | Review R010 | code Step 2: APPROVE |
| 2026-06-02 03:58 | Review R011 | plan Step 3: APPROVE |
| 2026-06-02 04:07 | Review R012 | code Step 3: REVISE |
| 2026-06-02 04:13 | Review R013 | code Step 3: REVISE |
| 2026-06-02 04:32 | Review R016 | code Step 4: APPROVE |
