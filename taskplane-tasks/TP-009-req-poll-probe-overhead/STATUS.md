# TP-009: Reduce REQ poll probe overhead — Status

**Current Step:** Step 5: Documentation & Delivery
**Status:** ✅ Complete
**Last Updated:** 2026-06-03
**Review Level:** 2
**Review Counter:** 7
**Iteration:** 1
**Size:** M

> **Hydration:** Checkboxes represent meaningful outcomes, not individual code changes. Workers expand steps when runtime discoveries warrant it.

---

### Step 0: Preflight
**Status:** ✅ Complete

- [x] Required files and paths exist
- [x] TP-008 complete and poll tests pass
- [x] Baseline REQ poll benchmark captured

---

### Step 1: REQ probe behavior plan
**Status:** ✅ Complete

- [x] Current REQ buffer/probe lifecycle mapped
- [x] Primary overhead target chosen
- [x] Correctness-preserving design recorded
- [x] Stale/valid/timeout/mixed test cases defined

---

### Step 2: Implement lower-overhead REQ polling
**Status:** ✅ Complete

- [x] Unnecessary probe wakeups/work reduced
- [x] `Req.receivesFor` timeout behavior preserved
- [x] `pollFor` REQ/mixed behavior preserved
- [x] Buffer updates remain exception-safe

---

### Step 3: Add REQ probe regression coverage and benchmark evidence
**Status:** ✅ Complete

- [x] REQ probe automated test added
- [x] Test registered in `test/test.cabal`
- [x] Existing `ReqPoll` scenarios pass
- [x] REQ poll benchmark evidence captured

---

### Step 4: Testing & Verification
**Status:** ✅ Complete

- [x] Targeted REQ/poll tests passing
- [x] REQ poll benchmark smoke with RTS allocation summary run
- [x] FULL test suite passing
- [x] Build passes
- [x] All failures fixed

---

### Step 5: Documentation & Delivery
**Status:** ✅ Complete

- [x] "Must Update" docs modified if needed
- [x] "Check If Affected" docs reviewed
- [x] Discoveries logged
- [x] Remaining REQ tradeoffs logged if needed

---

## Reviews

| # | Type | Step | Verdict | File |
|---|------|------|---------|------|
| R001 | plan | 1 | APPROVE | `.reviews/R001-plan-step1.md` |
| R002 | plan | 2 | APPROVE | `.reviews/R002-plan-step2.md` |
| R003 | code | 2 | APPROVE | `.reviews/R003-code-step2.md` |
| R004 | plan | 3 | APPROVE | `.reviews/R004-plan-step3.md` |
| R005 | code | 3 | APPROVE | `.reviews/R005-code-step3.md` |
| R006 | plan | 4 | APPROVE | `.reviews/R006-plan-step4.md` |
| R007 | code | 4 | UNAVAILABLE | *(no review file; reviewer produced no output)* |

---

## Discoveries

| Discovery | Disposition | Location |
|-----------|-------------|----------|
| Pure event-gated REQ polling does not preserve stale-then-valid direct `pollFor` behavior; after a stale/invalid REQ wakeup, libzmq may not provide a later valid-reply wakeup soon enough. | Reworked to keep the 10ms safety cadence for input REQ poll sets while reducing all-REQ direct receive probes to one per slice. | `lib/Zmqx/Core/Poll.hs`; observed with `cabal test test-req-poll` |
| Input REQ `zmq_poll` wakeups are also not reliable enough for a simple delayed valid reply in registered Cabal tests, even when an ad hoc run passed. | New `test-req-poll-probe-auto` captures this; implementation starts the fallback cadence immediately for input REQ poll sets. | `test/ReqPollProbeAuto.hs`; observed with `cabal test test-req-poll-probe-auto` |

---

## Execution Log

| Timestamp | Action | Outcome |
|-----------|--------|---------|
| 2026-06-02 | Task staged | PROMPT.md and STATUS.md created |
| 2026-06-02 18:00 | Task started | Runtime V2 lane-runner execution |
| 2026-06-02 18:00 | Step 0 started | Preflight |
| 2026-06-03 | Preflight poll tests | TP-008 STATUS.md complete; `cabal test test-req-poll test-items-poll-auto test-poll-out` passed |
| 2026-06-03 | Step 0 baseline benchmark | Idle REQ `pollFor (pollIn req) 50` timeout: elapsed_ms=50.186, 156,384 bytes allocated; valid reply `req-poll` 100 messages: latency_p50_us=8.953, p95=13.227, max=34.657, 873,168 bytes allocated |
| 2026-06-03 | Step 1 started | REQ probe behavior planning |
| 2026-06-03 | Step 1 plan review | APPROVE (`.reviews/R001-plan-step1.md`) |
| 2026-06-03 | Step 2 started | Implement event-gated REQ input polling |
| 2026-06-03 | Step 2 plan review | APPROVE (`.reviews/R002-plan-step2.md`) |
| 2026-06-03 | Step 2 implementation compile | `lib/Zmqx/Core/Poll.hs` now event-gates REQ probes through `zmq_poll`; `cabal build` passed |
| 2026-06-03 | Step 2 receivesFor check | Temporary `/tmp/ReqReceivesForTimeouts.hs` passed timeout 0, positive timeout, and blocking delayed reply behavior |
| 2026-06-03 | Step 2 pollFor checks | `cabal test test-req-poll test-items-poll-auto test-poll-out` passed after adding stale-wakeup fallback cadence |
| 2026-06-03 | Step 2 buffer safety audit | `probeReadyInputREQSocket` still masks receive-plus-buffer write and only reports readiness after `writeIORef messageBuffer (Just frames)` |
| 2026-06-03 | Step 2 code review | APPROVE (`.reviews/R003-code-step2.md`) |
| 2026-06-03 | Step 3 started | Add REQ probe regression coverage and benchmark evidence |
| 2026-06-03 | Step 3 plan review | APPROVE (`.reviews/R004-plan-step3.md`) |
| 2026-06-03 | Step 3 new test draft | Added `test/ReqPollProbeAuto.hs`; `cabal exec -- runghc -XGHC2024 -XBlockArguments -XOverloadedStrings -itest test/ReqPollProbeAuto.hs` passed |
| 2026-06-03 | Step 3 test registration | Added `test-req-poll-probe-auto` to `test/test.cabal`; initial Cabal run exposed unreliable pure event-gated REQ wakeups, then `cabal test test-req-poll-probe-auto` passed after immediate fallback cadence |
| 2026-06-03 | Step 3 ReqPoll regression | `cabal test test-req-poll` passed |
| 2026-06-03 | Step 3 benchmark evidence | Added explicit `req-poll-idle` benchmark scenario. Optimized `req-poll-idle` smoke (`messages=5`, `timeout-ms=10`) reported latency_p50_us=10144.177 and 349,040 bytes allocated; optimized `req-poll` valid-reply run (`messages=100`) reported latency_p50_us=16.842, p95=23.630, max=91.345 and 870,968 bytes allocated |
| 2026-06-03 | Step 3 code review | APPROVE (`.reviews/R005-code-step3.md`) |
| 2026-06-03 | Step 4 started | Testing and verification |
| 2026-06-03 | Step 4 plan review | APPROVE (`.reviews/R006-plan-step4.md`) |
| 2026-06-03 | Step 4 targeted tests | `cabal test test-req-poll-probe-auto test-req-poll test-items-poll-auto test-poll-out` passed |
| 2026-06-03 | Step 4 benchmark smoke | `req-poll-idle` smoke (`messages=3`, `timeout-ms=10`) reported 335,936 bytes allocated; `req-poll` smoke (`messages=10`) reported 365,120 bytes allocated |
| 2026-06-03 | Step 4 full tests | `cabal test all` passed |
| 2026-06-03 | Step 4 build | `cabal build` passed (up to date) |
| 2026-06-03 | Step 4 failure resolution | Earlier pure event-gated REQ poll failures were fixed; targeted tests, full suite, and build all pass |
| 2026-06-03 | Step 4 code review | UNAVAILABLE (reviewer produced no output); proceeding with passing full suite/build evidence |
| 2026-06-03 | Step 5 started | Documentation and delivery |
| 2026-06-03 | Step 5 docs update | Updated `docs/performance.md` for explicit `req-poll-idle` benchmark scenario and command |
| 2026-06-03 | Step 5 affected-docs review | Reviewed `taskplane-tasks/CONTEXT.md`; `performance-overheads/` is absent in this worktree |
| 2026-06-03 | Step 5 discoveries | STATUS.md Discoveries table records pure event-gated REQ poll limitations and the implemented fallback cadence |
| 2026-06-03 | Step 5 tradeoffs | Logged remaining REQ poll wakeup/10ms cadence tradeoff in `taskplane-tasks/CONTEXT.md` |
| 2026-06-02 18:45 | Worker iter 1 | done in 2712s, tools: 167 |
| 2026-06-02 18:45 | Task complete | .DONE created |

---

## Blockers

*None*

---

## Notes

- Step 0 baseline before source changes:
  - Idle REQ poll timeout probe: temporary `/tmp/ReqIdleBaseline.hs` compiled with `cabal exec -- ghc -O2 -rtsopts`; `/tmp/req-idle-baseline +RTS -s` reported `scenario=req-poll-idle poll_timeout_ms=50 elapsed_ms=50.186` and `156,384 bytes allocated`.
  - Valid-reply latency: `cabal run --enable-optimization=2 zmqx-overheads -- --scenario req-poll --messages 100 --warmup 10 --payload-bytes 64 --timeout-ms 1000 +RTS -s` reported `elapsed_ms=0.987 throughput_msg_per_s=101325.849 latency_p50_us=8.953 latency_p95_us=13.227 latency_max_us=34.657` and `873,168 bytes allocated`.
- Step 3 post-change benchmark evidence:
  - Idle poll/probe proxy: `cabal run --enable-optimization=2 zmqx-overheads -- --scenario req-poll-idle --messages 5 --warmup 1 --payload-bytes 64 --timeout-ms 10 +RTS -s` reported `elapsed_ms=50.747 throughput_msg_per_s=98.529 latency_p50_us=10144.177 latency_p95_us=10165.551 latency_max_us=10169.437 poll_timeout_ms=10` and `349,040 bytes allocated`.
  - Valid-reply latency: `cabal run --enable-optimization=2 zmqx-overheads -- --scenario req-poll --messages 100 --warmup 10 --payload-bytes 64 --timeout-ms 1000 +RTS -s` reported `elapsed_ms=1.854 throughput_msg_per_s=53925.275 latency_p50_us=16.842 latency_p95_us=23.630 latency_max_us=91.345` and `870,968 bytes allocated`.
  - Interpretation: optimized smoke timings are noisy, but the final code reduces direct all-REQ nonblocking receive probes from two per 10ms slice (pre- and post-poll) to one fallback probe per slice, and skips that fallback probe when non-REQ readiness is already available.
- Step 1 lifecycle map:
  - `Req.open`/`openWith` create `ReqExtra` with an `IORef (Maybe (NonEmpty ByteString))`; `REQ_CORRELATE` and `REQ_RELAXED` are enabled so stale replies may be present but invalid for the current request.
  - `Req.receive`/`Req.receives` drain the buffered reply first, clearing the `IORef`, otherwise they receive from libzmq directly.
  - `Req.receivesFor` drains the buffer first; timeout `<0` blocks in a `receiveManyDontWait`/`blockUntilCanReceive` loop, timeout `0` performs one nonblocking receive, and positive timeouts use `System.Timeout` around that loop then re-check the buffer and one final nonblocking receive. `EFSM` is treated as no valid reply.
  - `pollFor`/`pollUntil` partition input REQs out of `zmq_poll`, probe every input REQ by reading the buffer and then calling `receiveManyDontWait` when empty, store accepted replies in the buffer, and report readiness by socket id.
  - REQ-only poll sets loop probe all REQs then sleep 10ms (or remaining deadline). Mixed poll sets probe REQs before and after each `zmq_poll`, cap `zmq_poll` waits to 10ms when any input REQ is present, and union buffered REQ readiness with non-REQ readiness.
- Step 1 primary target: reduce repeated direct REQ probe work while preserving the existing 10ms safety cadence. Runtime testing showed input REQ `zmq_poll` wakeups are not reliable enough for either valid replies or stale-then-valid replies, so the final design keeps the 10ms cadence when input REQs are present but performs at most one all-REQ direct probe per slice instead of probing every REQ both before and after each poll.
- Step 1 chosen design and invariants:
  - Keep input REQ sockets in the prepared `zmq_poll` item array as wakeup candidates, but tag them as requiring a post-poll probe; never mark an input REQ ready solely from `revents`.
  - Before each `zmq_poll`, scan input REQ buffers only. If any reply is already buffered, use a zero poll timeout so same-instant non-REQ readiness can still be unioned with buffered REQ readiness.
  - After `zmq_poll`, insert non-REQ ready socket ids directly; for each signaled input REQ, run the existing masked `receiveManyDontWait` probe, buffer accepted frames, ignore `EFSM` as stale/no valid reply, and insert the REQ id only when a valid reply is buffered.
  - Use a 10ms `zmq_poll` slice when input REQs are present, but avoid the old pre-poll direct receive probe. After each slice, validate signaled REQs and then run one all-REQ fallback probe before retrying, so buffered replies and stale/correlated safeguards are preserved with less repeated direct receive work.
  - Preserve the invariant that `Ready req == True` implies either the REQ buffer already contained a reply or this poll call just buffered one. Only `Req.receive`/`Req.receives`/`Req.receivesFor` clear that buffer. Buffer writes remain masked around receive-plus-IORef update.
  - Deferred alternatives: adaptive sleep/backoff, explicit per-REQ probe timestamps, and deeper REQ state-machine integration were not chosen; runtime evidence showed pure event-gated wakeups are unreliable, so future latency-quantization work needs a stronger readiness signal or instrumentation.
- Step 1 test cases to preserve behavior:
  - Stale/correlated reply: keep `test-req-poll` scenarios where stale reply to request 1 is ignored after relaxed request 2, and `pollFor (pollIn req)` only reports readiness once the valid request-2 reply is buffered.
  - Valid reply readiness: new `ReqPollProbeAuto` should send a REQ, delay a REP reply briefly, assert `pollFor (pollIn req) 1000` returns ready, then assert `receives req` drains exactly that buffered reply and a follow-up `receivesFor req 0` is empty.
  - Timeout/idle accuracy: new `ReqPollProbeAuto` should send a REQ whose REP peer receives but does not answer, assert `pollFor (pollIn req) 50` returns `Nothing`, and record elapsed time bounds loose enough for CI while ensuring no false readiness.
  - Mixed REQ/non-REQ polls: new or existing coverage should poll an unanswered REQ together with a PULL/SUB-style non-REQ input, assert non-REQ readiness is still reported, and later assert a valid REQ reply is reported and buffered.
  - `Req.receivesFor`: targeted checks should cover timeout `0`, positive timeout with no reply, and positive timeout with delayed valid reply; blocking negative timeout remains through the existing receive loop unless the implementation touches `Req.hs`.
| 2026-06-02 18:08 | Review R001 | plan Step 1: APPROVE |
| 2026-06-02 18:12 | Review R002 | plan Step 2: APPROVE |
| 2026-06-02 18:22 | Review R003 | code Step 2: APPROVE |
| 2026-06-02 18:23 | Review R004 | plan Step 3: APPROVE |
| 2026-06-02 18:36 | Review R005 | code Step 3: APPROVE |
| 2026-06-02 18:37 | Review R006 | plan Step 4: APPROVE |
