# TP-010: Optimize blocking FFI and expected-error paths — Status

**Current Step:** Step 5: Documentation & Delivery
**Status:** ✅ Complete
**Last Updated:** 2026-06-03
**Review Level:** 2
**Review Counter:** 8
**Iteration:** 2
**Size:** M

> **Hydration:** Checkboxes represent meaningful outcomes, not individual code changes. Workers expand steps when runtime discoveries warrant it.

---

### Step 0: Preflight
**Status:** ✅ Complete

- [x] Required files and paths exist
- [x] TP-007 complete and receive tests pass
- [x] Baseline EAGAIN/backpressure benchmark captured

---

### Step 1: Blocking/error-path optimization plan
**Status:** ✅ Complete

- [x] Current expected-error paths mapped
- [x] Measured optimization target chosen
- [x] FFI/cancellation safety constraints defined
- [x] Correctness and benchmark evidence defined
- [x] R001 revision: saturated-send/HWM evidence handled or send-side optimization deferred

---

### Step 2: Implement the reviewed hot-path optimization
**Status:** ✅ Complete

- [x] Selected optimization implemented
- [x] Public error results preserved
- [x] Blocking scheduler behavior preserved
- [x] C wrappers kept minimal if used

---

### Step 3: Add backpressure/error-path regression coverage and benchmark evidence
**Status:** ✅ Complete

- [x] Backpressure/empty-receive test added
- [x] Test registered in `test/test.cabal`
- [x] Success/EAGAIN benchmark evidence captured
- [x] Performance docs updated if commands changed

---

### Step 4: Testing & Verification
**Status:** ✅ Complete

- [x] Targeted backpressure/poll/role tests passing
- [x] EAGAIN/backpressure benchmark smoke with RTS summary run
- [x] FULL test suite passing
- [x] Build passes
- [x] All failures fixed

---

### Step 5: Documentation & Delivery
**Status:** ✅ Complete

- [x] "Must Update" docs modified if needed
- [x] "Check If Affected" docs reviewed
- [x] Discoveries logged
- [x] Rejected high-risk alternatives logged if needed

---

## Reviews

| # | Type | Step | Verdict | File |
|---|------|------|---------|------|
| 1 | Plan | Step 1 | REVISE | `.reviews/R001-plan-step1.md` |
| 2 | Plan | Step 1 | APPROVE | inline reviewer |
| 3 | Plan | Step 2 | APPROVE | inline reviewer |
| 4 | Code | Step 2 | APPROVE | inline reviewer |
| 5 | Plan | Step 3 | APPROVE | inline reviewer |
| 6 | Code | Step 3 | APPROVE | inline reviewer |
| 7 | Plan | Step 4 | APPROVE | inline reviewer |
| 8 | Code | Step 4 | APPROVE | inline reviewer |

---

## Discoveries

| Discovery | Disposition | Location |
|-----------|-------------|----------|
| Fusing receive-side errno through a C wrapper with an out parameter was slower/noisier than baseline because it added per-call pointer plumbing; returning negative errno directly kept the wrapper minimal and produced the best smoke result. | Implemented the negative-errno wrapper and retained benchmark evidence in STATUS notes. | `c/zmq-wrapper.c`, `lib/Zmqx/Internal/Functions.hs`, STATUS.md Step 3/4 logs |
| Send-side fused errno wrappers need a saturated-send/HWM benchmark fixture before changing `zmq_send__unsafe`; role send behavior stayed unchanged in this task. | Deferred to technical debt and logged in `taskplane-tasks/CONTEXT.md` in the next delivery item. | Step 1/3 notes |

---

## Execution Log

| Timestamp | Action | Outcome |
|-----------|--------|---------|
| 2026-06-02 | Task staged | PROMPT.md and STATUS.md created |
| 2026-06-02 18:45 | Task started | Runtime V2 lane-runner execution |
| 2026-06-02 18:45 | Step 0 started | Preflight |
| 2026-06-03 | Preflight paths checked | Required source/test/bench/docs/task files exist; `performance-overheads/*.md` files are absent and treated as optional per prompt wording. |
| 2026-06-02 18:46 | Worker iter 1 | done in 47s, tools: 9 |
| 2026-06-03 | TP-007 dependency verified | TP-007 STATUS is complete; `cabal test test-receive-path-auto test-req-rep-auto test-dealer-router-auto test-items-poll-auto test-req-poll test-recv-for` passed. |
| 2026-06-03 | Baseline EAGAIN benchmark captured | `cabal run --enable-optimization=2 zmqx-overheads -- --scenario req-poll-idle --messages 1000 --warmup 10 --timeout-ms 0 --payload-bytes 64 +RTS -s`: elapsed 10.864ms, throughput 92047.230 msg/s, p50 9.362us, p95 15.606us, max 421.332us, RTS allocated 6,581,912 bytes. Timeout-dominated smoke (`--messages 20 --timeout-ms 1`) also passed: elapsed 23.846ms, allocated 437,104 bytes. |
| 2026-06-03 | Step 1 plan review | R001 requested saturated-send/HWM handling; plan narrowed to receive-side fused errno wrapper and second plan review approved. |
| 2026-06-03 | Step 2 plan review | APPROVE for receive-side fused errno wrapper implementation. |
| 2026-06-03 | Step 2 implementation | Added minimal `zmqx_msg_recv_errno` C wrapper and unsafe binding; `zmq_msg_recv_dontwait` now uses one FFI crossing for receive-side errors. `cabal build` passed after updating the `Zmqx.Internal.Bindings` export list. |
| 2026-06-03 | Public receive/error semantics smoke | `cabal test test-receive-path-auto test-req-poll` passed after the fused receive errno change. |
| 2026-06-03 | Blocking/poll behavior smoke | `cabal test test-poll-out test-req-rep-auto test-dealer-router-auto` passed; blocking wait helpers and send-side imports remain unchanged. |
| 2026-06-03 | C wrapper scope checked | Diff adds one 10-line `zmqx_msg_recv_errno` wrapper and one header declaration; send-side wrapper/imports are unchanged. |
| 2026-06-03 | Step 2 code review | APPROVE for receive-side fused errno wrapper diff. |
| 2026-06-03 | Step 3 plan review | APPROVE for empty-receive regression test and benchmark evidence plan. |
| 2026-06-03 | Backpressure/empty-receive test added | Created `test/BlockingBackpressureAuto.hs` covering a `REQ` empty `receivesFor 0` probe after a ROUTER has accepted the request, then a normal reply success path. |
| 2026-06-03 | Backpressure test registered | Added `test-suite test-blocking-backpressure-auto` to `test/test.cabal` with `bytestring` dependency. |
| 2026-06-03 | Benchmark evidence captured | Out-param wrapper variant measured worse, so the wrapper was tightened to return negative errno directly. Final EAGAIN smoke `cabal run --enable-optimization=2 zmqx-overheads -- --scenario req-poll-idle --messages 1000 --warmup 10 --timeout-ms 0 --payload-bytes 64 +RTS -s` reported elapsed 10.477ms / 95,445.430 msg/s / p50 8.837us / p95 15.542us / 6,578,864 bytes allocated; immediate rerun reported elapsed 10.850ms / 92,163.376 msg/s / p50 9.393us / p95 15.162us / 6,582,496 bytes. Step 0 baseline was 10.864ms / 92,047.230 msg/s / p50 9.362us / p95 15.606us / 6,581,912 bytes. Success-path smoke `direct --messages 1000 --warmup 10 --payload-bytes 64 +RTS -s` reported elapsed 2.289ms / 436,892.610 msg/s / p50 0.778us / p95 5.368us / 3,705,896 bytes. |
| 2026-06-03 | Performance docs reviewed | `docs/performance.md` already documents `zmqx-overheads`, `req-poll-idle`, `direct`, and `+RTS -s`; no command or option names changed, so no docs edit was needed. |
| 2026-06-03 | Step 3 targeted smoke | `cabal test test-blocking-backpressure-auto test-req-poll test-receive-path-auto` passed after the final negative-errno wrapper adjustment. |
| 2026-06-03 | Step 3 code review | APPROVE for FFI/error diff, regression test, and benchmark evidence. |
| 2026-06-03 | Step 4 plan review | APPROVE for targeted/full test and benchmark smoke gate. |
| 2026-06-03 | Step 4 targeted tests | `cabal test test-blocking-backpressure-auto test-poll-out test-req-rep-auto test-dealer-router-auto test-req-poll test-receive-path-auto` passed. |
| 2026-06-03 | Step 4 benchmark smoke | `cabal run --enable-optimization=2 zmqx-overheads -- --scenario req-poll-idle --messages 100 --warmup 5 --timeout-ms 0 --payload-bytes 64 +RTS -s` reported elapsed 1.264ms / 79,126.444 msg/s / p50 9.793us / p95 21.373us / 928,032 bytes allocated. |
| 2026-06-03 | Full suite | `cabal test all` passed, including the new `test-blocking-backpressure-auto` suite. |
| 2026-06-03 | Build gate | `cabal build` passed (`Up to date`). |
| 2026-06-03 | Failure cleanup | Fixed the missing `Zmqx.Internal.Bindings` export caught by the first build, removed the new test's redundant import warning, and replaced the slower out-param wrapper variant with the final negative-errno wrapper before rerunning tests/benchmarks. No task-introduced failures remain. |
| 2026-06-03 | Step 4 code review | APPROVE for testing/verification evidence. |
| 2026-06-03 | Step 5 Must Update docs | `docs/performance.md` commands/options were unchanged; no docs modification required. |
| 2026-06-03 | Step 5 Check If Affected docs | `taskplane-tasks/CONTEXT.md` reviewed for deferred FFI/error-path notes; optional `performance-overheads/` directory is absent in this worktree. |
| 2026-06-03 | Discoveries logged | Recorded the out-param wrapper measurement result and deferred send-side/HWM benchmark gap in the Discoveries table. |
| 2026-06-03 | Deferred alternative logged | Added `Send-side EAGAIN/HWM benchmark fixture` technical debt to `taskplane-tasks/CONTEXT.md`. |
| 2026-06-03 | Task complete | All steps complete; final single TP commit prepared. |
| 2026-06-02 19:28 | Worker iter 2 | done in 2487s, tools: 165 |
| 2026-06-02 19:28 | Task complete | .DONE created |

---

## Blockers

*None*

---

## Notes

### Step 1 optimization plan

- Current expected-error path map: role-level blocking sends (`Pair`, `Push`, `Dealer`, `Req`, EventLoop; `Pub`/`XPub` convert `False` to `Left EAGAIN`) first call `Socket.sendOneDontWait`/`sendManyDontWait`, which use unsafe non-blocking `zmq_send` with `ZMQ_DONTWAIT`; a `-1` result currently does a second FFI call to `zmq_errno` before mapping `EAGAIN` to `False` and mapping `EINTR`/`ETERM`/`EHOSTUNREACH` to existing ok-error boundaries. Public receive and timeout/poll paths (`receiveOneDontWait`, `receiveManyDontWait`, `Req.receivesFor`, `Poll.pollFor` drains) call unsafe non-blocking `zmq_msg_recv` with `ZMQ_DONTWAIT`; empty queues currently cross FFI once for `zmq_msg_recv` and again for `zmq_errno`, then map `EAGAIN` to `Nothing`/`Again`. Blocking receives retry `receive*DontWait`, then wait via `threadWaitRead` on `ZMQ_FD` and re-check `ZMQ_EVENTS`; blocking sends retry `send*DontWait`, then wait similarly for `ZMQ_POLLOUT`. Wont-block multipart tails reuse non-blocking `zmq_msg_recv`/`zmq_send` without `ZMQ_DONTWAIT` after readiness and still route non-EAGAIN errno through the same exception/ok-error mapping.
- Revised chosen measured target after R001: add one minimal C wrapper for non-blocking `zmq_msg_recv` that performs the libzmq call and returns negative `zmq_errno()` on `-1`, then bind it as unsafe FFI and route `zmq_msg_recv_dontwait` through the fused wrapper. This targets the baseline `req-poll-idle --timeout-ms 0` EAGAIN-heavy receive/probe path (10.864ms/1000 probes, 6,581,912 RTS bytes) by removing one Haskell FFI crossing per expected receive-side `EAGAIN`. Send-side `zmq_send__unsafe` is explicitly deferred because this task has no saturated-send/HWM benchmark fixture yet; preserving send behavior will be verified by existing role/poll tests, but no send FFI wrapper will be changed. Rejected for this task: direct interruptible blocking send/receive changes (larger cancellation/scheduler blast radius), reducing `ZMQ_EVENTS` probes (riskier readiness semantics), send-side fused wrappers without HWM evidence, and removing exception wrapping for public `Left Error`s (API/semantics risk without direct benchmark isolation).
- Safety constraints: the fused wrapper must be used only for `zmq_msg_recv` calls that already pass `ZMQ_DONTWAIT`; truly blocking APIs (`zmq_msg_recv`/`zmq_send` without `DONTWAIT`, send-side `zmq_send__unsafe`, `zmq_poll`, `zmq_ctx_term`, bind/connect/monitor/proxy) keep their existing imports and errno handling. The Haskell retry loops, `threadWaitRead`, `ZMQ_FD`, `ZMQ_EVENTS`, masking around multipart partial sends/receives, and async-exception behavior are left unchanged. The C wrapper must be tiny and portable, returning the original non-negative libzmq result on success and negative errno only on failure; Haskell still enriches and classifies the same receive errno values (`EAGAIN`, `EINTR`, `ETERM`, etc.).
- Evidence plan: add `test-blocking-backpressure-auto` covering an empty-receive timeout (`Right Nothing` from `receivesFor ... 0`), a normal success receive after the empty probe, and a public send/receive success path; keep `test-poll-out`, `test-req-rep-auto`, `test-dealer-router-auto`, and `test-req-poll` passing to cover readiness, multipart role behavior, send success/blocking behavior, and REQ probe behavior. Benchmark evidence will rerun the same optimized EAGAIN-heavy receive smoke as Step 0 (`req-poll-idle --messages 1000 --warmup 10 --timeout-ms 0 --payload-bytes 64 +RTS -s`) plus a success-path smoke (`direct` or `all`) and record elapsed/throughput/RTS allocation in STATUS.md. Saturated send/HWM evidence is not required for the selected implementation because send-side wrappers are deferred; that gap is logged as a rejected high-risk/deferred alternative for `CONTEXT.md`. `EINTR`/`ETERM` semantics are validated by preserving the exact receive errno classification and by the full suite/context lifecycle tests rather than by adding brittle signal/context-termination fixtures in this task.
- R001 suggestion noted: if send-side wrappers are revisited later, any wrapper must preserve the caller-supplied flags (`ZMQ_DONTWAIT`, `ZMQ_SNDMORE`, or no flag for existing wont-block paths`) rather than forcing flags inside C.
| 2026-06-02 18:53 | Review R001 | plan Step 1: REVISE |
| 2026-06-02 18:56 | Review R002 | plan Step 1: APPROVE |
| 2026-06-02 18:58 | Review R003 | plan Step 2: APPROVE |
| 2026-06-02 19:05 | Review R004 | code Step 2: APPROVE |
| 2026-06-02 19:08 | Review R005 | plan Step 3: APPROVE |
| 2026-06-02 19:18 | Review R006 | code Step 3: APPROVE |
| 2026-06-02 19:19 | Review R007 | plan Step 4: APPROVE |
| 2026-06-02 19:25 | Review R008 | code Step 4: APPROVE |
