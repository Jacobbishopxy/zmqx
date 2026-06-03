# TP-011: Optimize large-payload send path — Status

**Current Step:** Step 6: Documentation & Delivery
**Status:** ✅ Complete
**Last Updated:** 2026-06-03
**Review Level:** 3
**Review Counter:** 10
**Iteration:** 3
**Size:** L

> **Hydration:** Checkboxes represent meaningful outcomes, not individual code changes. Workers expand steps when runtime discoveries warrant it.

---

### Step 0: Preflight
**Status:** ✅ Complete

- [x] Required files and paths exist
- [x] TP-010 complete and send/backpressure tests pass
- [x] Baseline small/large send benchmark captured

---

### Step 1: Large-send ownership and API plan
**Status:** ✅ Complete

- [x] Current send paths mapped
- [x] Viable designs compared
- [x] Threshold/fallback policy defined
- [x] Error/retry/multipart/lifetime invariants defined
- [x] Corruption/lifetime test plan defined

---

### Step 2: Implement reviewed large-send path
**Status:** ✅ Complete

- [x] Approved path implemented with conservative fallback
- [x] Public API preserved
- [x] Multipart flags/order preserved
- [x] Finalizer/C callback ownership documented if used

---

### Step 3: Add large-payload send tests and benchmark evidence
**Status:** ✅ Complete

- [x] Large single-frame and multipart test added
- [x] Test registered in `test/test.cabal`
- [x] Small/large send benchmark evidence captured
- [x] Small-payload regression assessment recorded

---

### Step 4: Test-review and stress validation
**Status:** ✅ Complete

- [x] Repeated large-payload sends run with GC pressure where practical
- [x] Multipart stress run completed
- [x] Payload-size benchmark sweep run
- [x] Performance docs updated if needed

---

### Step 5: Testing & Verification
**Status:** ✅ Complete

- [x] Targeted large-payload and role tests passing
- [x] Large-payload benchmark smoke with RTS summary run
- [x] FULL test suite passing
- [x] Build passes
- [x] All failures fixed

---

### Step 6: Documentation & Delivery
**Status:** ✅ Complete

- [x] "Must Update" docs modified if needed
- [x] "Check If Affected" docs reviewed
- [x] Discoveries logged
- [x] Deferred zero-copy/platform work logged if needed

---

## Reviews

| # | Type | Step | Verdict | File |
|---|------|------|---------|------|
| 1 | Plan | Step 1 | APPROVE | `R001-plan-step1.md` |
| 2 | Plan | Step 2 | APPROVE | `R002-plan-step2.md` |
| 3 | Code | Step 2 | APPROVE | `R003-code-step2.md` |
| 4 | Plan | Step 3 | APPROVE | `R004-plan-step3.md` |
| 5 | Code | Step 3 | APPROVE | `R005-code-step3.md` |
| 7 | Plan | Step 4 | APPROVE | `R007-plan-step4.md` |
| 8 | Code | Step 4 | APPROVE | `R008-code-step4.md` |
| 9 | Plan | Step 5 | APPROVE | `R009-plan-step5.md` |
| 10 | Code | Step 5 | APPROVE | `R010-code-step5.md` |

---

## Discoveries

| Discovery | Disposition | Location |
|-----------|-------------|----------|
| Copy-backed `zmq_msg_init_data` helps large multipart prefix frames but regressed/noisily affected large direct single-frame sends when applied broadly. | Kept optimization limited to frames >=64KiB with `ZMQ_SNDMORE`; documented benchmark interpretation. | `lib/Zmqx/Core/Socket.hs`, `docs/performance.md`, Step 3/4 notes |
| True zero-copy over Haskell `ByteString` storage still needs a separate ownership/finalizer design because libzmq callbacks can run on arbitrary threads. | Deferred to future work; TP-011 uses C `malloc` copy plus C-only `free2` instead. | `taskplane-tasks/CONTEXT.md`, `c/utils.c` |

---

## Execution Log

| Timestamp | Action | Outcome |
|-----------|--------|---------|
| 2026-06-02 | Task staged | PROMPT.md and STATUS.md created |
| 2026-06-02 19:28 | Task started | Runtime V2 lane-runner execution |
| 2026-06-02 19:28 | Step 0 started | Preflight |
| 2026-06-03 | Preflight dependency/tests | TP-010 STATUS is complete; `cabal test test-blocking-backpressure-auto test-poll-out test-req-rep-auto test-dealer-router-auto test-req-poll test-receive-path-auto` passed. |
| 2026-06-03 | Baseline send benchmarks | Optimized `zmqx-overheads` direct/multipart runs captured 64B and 1MiB payloads before source changes; see Notes Step 0 baseline. |
| 2026-06-03 | Step 1 started | Large-send ownership/API plan. |
| 2026-06-03 | Step 1 plan review | APPROVE for copy-backed `zmq_msg_init_data` large-frame candidate, threshold/fallback policy, and rejected unsafe zero-copy approaches. |
| 2026-06-03 | Step 2 started | Implement reviewed large-send path. |
| 2026-06-03 | Step 2 plan review | APPROVE to implement the reviewed copy-backed large-frame helper with conservative fallback. |
| 2026-06-03 | Step 2 implementation | Added a 64KiB-threshold copy-backed `zmq_msg_init_data` send path with fallback to existing `zmq_send` before any send attempt if message initialization fails; `cabal build` passed. |
| 2026-06-03 | Public API check | Public role modules and `Zmqx`/`Zmqx.Monad` send signatures were not changed; only internal message-send helpers were exported through `Zmqx.Internal`. |
| 2026-06-03 | Multipart/order smoke | `sendFrameWith` preserves caller-supplied `ZMQ_DONTWAIT`/`ZMQ_SNDMORE` flags; `cabal test test-dealer-router-auto test-req-rep-auto test-req-poll test-poll-out` passed after the change. |
| 2026-06-03 | Finalizer ownership documented | Added code comments for the copy-backed `zmq_msg_init_data` ownership path and C-only `free2` callback; no Haskell finalizer is handed to libzmq. |
| 2026-06-03 | Step 2 code review | APPROVE for implementation ownership, fallback, and flag preservation. |
| 2026-06-03 | Step 3 started | Add large-payload send tests and benchmark evidence. |
| 2026-06-03 | Step 3 plan review | APPROVE for large single/multipart tests and before/after-style benchmark comparison against Step 0 baseline. |
| 2026-06-03 | Large payload test added | Created `test/LargePayloadSendAuto.hs` covering PAIR and DEALER/ROUTER large single-frame, large multipart, GC pressure, and small fallback cases. |
| 2026-06-03 | Large payload test registered | Added `test-suite test-large-payload-send-auto` to `test/test.cabal`. |
| 2026-06-03 | Large payload test smoke | `cabal test test-large-payload-send-auto` passed. |
| 2026-06-03 | Post-change send benchmarks | Re-ran optimized direct/multipart 64B and 1MiB benchmark commands; see Notes Step 3 benchmark comparison. |
| 2026-06-03 | Small/regression assessment | Initial all-large-frame message path regressed direct 1MiB sends, so the final implementation limits the copy-backed path to large multipart prefix frames (`SNDMORE`) and leaves single/final frames on `zmq_send`. |
| 2026-06-03 | Step 3 code review | APPROVE for large-payload tests, benchmark evidence, and SNDMORE-only fallback adjustment. |
| 2026-06-03 | Step 4 started | Stress validation and test-review evidence. |
| 2026-06-02 20:05 | Worker iter 1 | done in 2211s, tools: 120 |
| 2026-06-02 20:05 | Exit intercept reprompt | Supervisor provided instructions (776 chars) — reprompting worker |
| 2026-06-02 20:06 | Exit intercept timeout | Supervisor did not respond within 60s — closing session |
| 2026-06-02 20:06 | Worker iter 2 | done in 63s, tools: 5 |
| 2026-06-02 20:06 | Soft progress | Iteration 2: 0 new checkboxes but uncommitted source changes detected — not counting as stall |
| 2026-06-02 20:28 | Worker iter 3 | done in 1324s, tools: 85 |
| 2026-06-02 20:28 | Task complete | .DONE created |

---

## Blockers

*None*

---

## Notes

### Step 0 baseline send benchmarks

Commands used `cabal run --enable-optimization=2 zmqx-overheads -- ... +RTS -s` before any TP-011 source changes.

- direct 64B, 200 messages, warmup 10: elapsed 0.542ms, throughput 368,741 msg/s, p50 0.763us, p95 6.456us, max 25.926us, RTS allocated 971,144 bytes.
- direct 1MiB, 50 messages, warmup 5: elapsed 27.705ms, throughput 1,804.750 msg/s, p50 517.366us, p95 580.982us, max 2,025.790us, RTS allocated 115,809,592 bytes.
- multipart 4x64B, 200 messages, warmup 10: elapsed 1.085ms, throughput 184,350 msg/s, p50 4.907us, p95 8.965us, max 21.729us, RTS allocated 1,435,864 bytes.
- multipart 4x1MiB, 50 messages, warmup 5: elapsed 179.991ms, throughput 277.792 msg/s, p50 3,440.585us, p95 4,257.072us, max 5,851.421us, RTS allocated 288,934,896 bytes.

### Step 1 large-send plan

- Current send path map: public `Zmqx.send`/`Zmqx.sends` and `Zmqx.Monad.send`/`sends` dispatch through `Socket.CanSend`/`CanSends` instances. `Pair`, `Push`, `Dealer`, and `Req` retry `Socket.sendOneDontWait` / `sendManyDontWait` after `blockUntilCanSend`; `Pub`/`XPub` use the same non-blocking helpers but surface `EAGAIN` as an ok-error instead of blocking; `Rep`/`XSub` use `sendOneWontBlock` / `sendManyWontBlock`; `Router.sends` first tries `sendManyDontWait` and falls back to blocking `sendMany` for peer HWM. `EventLoop.send`/`sends` queue to a worker, then either delegates to the socket role's `sends_` or performs shutdown-aware retries around `Socket.sendManyDontWait`. In `lib/Zmqx/Core/Socket.hs`, single-frame paths converge on `zhs_send_frame`, `zhs_send_frame_dontwait`, or `zhs_send_frame_wontblock`; multipart paths send the first frame with `ZMQ_SNDMORE` and `mask_`, then use wont-block tail sends preserving first/rest order. All current hot paths call `zmq_send`/`zmq_send__unsafe` through `sendwith`, which pins the `ByteString` only for the FFI call with `unsafeUseAsCStringLen`; existing `zmq_send_const`, `zmq_msg_init_data`, and `zmq_msg_send` helpers are internal and unused by public sends.
- Viable design comparison: (1) keep `zmq_send` everywhere is the safety baseline and fallback; it copies/queues from a `ByteString` that is only pinned during the FFI call, so lifetime is safe but large frames pay libzmq's copy/queue overhead. (2) `zmq_send_const` is rejected for arbitrary public `ByteString`s: its man page says libzmq does not copy or deallocate constant memory, and a successful call only means queued, so `unsafeUseAsCStringLen` lifetime is too short and there is no callback to release a retained `ForeignPtr`. (3) true zero-copy `zmq_msg_init_data` over the original `ByteString` with a retained `ForeignPtr`/Haskell callback is deferred as unsafe/novel: libzmq may invoke the free function on an arbitrary thread, and proving StablePtr/ForeignPtr ownership plus context-shutdown ordering needs a separate design. (4) copy-backed `zmq_msg_init_data` using C `malloc` + `copyBytes` + existing `free2`, then `zmq_msg_send`, is the approved candidate to benchmark for large frames: Haskell memory is no longer retained after the copy, libzmq owns the C buffer after a successful `zmq_msg_send`, and failure cleanup can `zmq_msg_close`/free the message. It may or may not beat `zmq_send`, so it must be thresholded and easy to disable/fallback based on benchmark evidence.
- Threshold/fallback policy: introduce one internal threshold constant in `Core.Socket` (frames `>= 64 * 1024` bytes) and route only large frames carrying `ZMQ_SNDMORE` through the copy-backed message path; zero-length, smaller frames, and final/single-frame sends keep the existing `zmq_send`/`zmq_send__unsafe` path because the measured copy-backed candidate regressed direct 1MiB single-frame sends. Safe blocking sends use safe `zmq_msg_send`; `DONTWAIT` and wont-block sends use unsafe `zmq_msg_send__unsafe` just as the existing `zmq_send` variants do. If `zmq_msg_init_data` fails before any send attempt, fall back to the original `zmq_send` path for that frame to preserve behavior; once `zmq_msg_send` has been attempted, report its errno and clean up the message on failure rather than retrying a second send. Step 4 will sweep medium/large sizes and can further raise or disable the threshold if the multipart win does not hold.
- Error/retry/multipart/lifetime invariants: role modules and public `send`/`sends` signatures stay unchanged; their existing retry loops, `blockUntilCanSend`, Pub/XPub ok-error behavior, Router blocking fallback, and EventLoop shutdown-aware retry loop stay above the shared frame helper. The optimized helper must accept the same `Zmq_send_option` flags so `ZMQ_DONTWAIT` and `ZMQ_SNDMORE` combinations are preserved exactly. Multipart code remains masked and keeps current ordering: first frame with `SNDMORE`, if non-blocking first frame returns `EAGAIN` no tail is sent, and tails use the existing wont-block flags. ByteString lifetime safety comes from copying into C `malloc` storage before `zmq_msg_send`; after a successful send, libzmq owns that C buffer and will invoke `free2`, while Haskell frees only the heap-allocated `zmq_msg_t` container. On failed `zmq_msg_send` or async interruption, Haskell must `zmq_msg_close` then free the container so the C buffer is released. No Haskell `ForeignPtr` or StablePtr finalizer may be called from libzmq in this task.
- Correctness/lifetime test plan: add `test/LargePayloadSendAuto.hs` registered as `test-large-payload-send-auto`. Cover at least `PAIR` and `DEALER`/`ROUTER` role pairs with (a) single large frames at/above the threshold and (b) multipart messages whose first, middle, and final frames are all large enough to exercise `SNDMORE` paths. Payloads should be deterministic but non-uniform (different byte patterns per role/frame), compare exact bytes/lists after receive, and run `performGC` plus temporary allocation between send and receive where practical to expose premature release/corruption. Include a below-threshold small frame smoke to prove fallback still works. Step 4 will repeat the large and multipart tests and benchmark small/medium/large payload sizes to support or disable the threshold.

### Step 3 benchmark comparison

Post-change commands matched Step 0 (`cabal run --enable-optimization=2 zmqx-overheads -- ... +RTS -s`). The initially broader all-large-frame message path improved multipart but regressed direct 1MiB single-frame sends (36.017ms vs 27.705ms baseline), so the final code limits the optimized path to large frames with `ZMQ_SNDMORE` and uses the existing `zmq_send` path for single/final frames.

Final adjusted evidence:

- direct 64B, 200 messages: elapsed 0.489ms, throughput 408,785 msg/s, p50 0.760us, p95 4.956us, allocated 981,080 bytes (baseline 0.542ms / 368,741 msg/s / 971,144 bytes).
- direct 1MiB, 50 messages: adjusted reruns were 28.454ms and 37.625ms (allocation about 115,805,000 bytes) after falling back to `zmq_send`; this is treated as unchanged/noisy versus the 27.705ms baseline rather than an optimized case.
- multipart 4x64B, 200 messages: elapsed 1.120ms, throughput 178,507 msg/s, p50 5.162us, p95 9.918us, allocated 1,454,120 bytes (baseline 1.085ms / 184,350 msg/s / 1,435,864 bytes), a tiny difference considered smoke-run noise with small frames below threshold.
- multipart 4x1MiB, 50 messages: elapsed 93.035ms, throughput 537.434 msg/s, p50 1,776.810us, p95 1,973.628us, allocated 289,038,848 bytes (baseline 179.991ms / 277.792 msg/s / 288,934,896 bytes).

### Step 4 stress validation evidence

- GC-pressure repeated large send run: `for i in 1 2 3 4 5; do cabal test test-large-payload-send-auto --test-show-details=direct; done` passed 5/5 on 2026-06-03. The test performs `performGC`, allocates temporary 16x32KiB garbage, and runs `performGC` between large send and receive checks.
- Multipart stress run: `for i in 1 2 3 4 5 6 7 8 9 10; do cabal test test-large-payload-send-auto --test-show-details=direct; done` passed 10/10 on 2026-06-03. Each repetition covers PAIR large multipart, ROUTER-to-DEALER large multipart, and DEALER-to-ROUTER large multipart exact-byte/order checks.
- Payload-size benchmark sweep with `cabal run --enable-optimization=2 zmqx-overheads -- ... +RTS -s` passed on 2026-06-03. Direct results: 64B/200 = 0.486ms, 411,856 msg/s, 969,368 allocated; 64KiB/100 = 8.355ms, 11,969 msg/s, 14,381,464 allocated; 256KiB/75 = 12.250ms, 6,122 msg/s, 42,484,208 allocated; 1MiB/50 = 26.372ms, 1,895.967 msg/s, 115,812,616 allocated. Multipart 4-frame results: 64B/200 = 1.791ms, 111,655 msg/s, 1,465,000 allocated; 64KiB/100 = 12.440ms, 8,039 msg/s, 35,438,784 allocated; 256KiB/75 = 35.027ms, 2,141 msg/s, 105,713,496 allocated; 1MiB/50 = 106.011ms, 471.651 msg/s, 289,038,848 allocated. The sweep included small, exactly-threshold (64KiB), medium (256KiB), and large (1MiB) payload sizes.
- `docs/performance.md` now documents the 64KiB multipart-prefix-only large send path, threshold-adjacent benchmark examples, and how to interpret direct single-frame noise separately from multipart prefix measurements.

### Step 5 verification evidence

- Targeted role tests passed on 2026-06-03: `cabal test test-large-payload-send-auto test-req-rep-auto test-dealer-router-auto test-pub-sub-auto test-task-pipeline-auto test-mut-worker-auto test-lb-worker-auto --test-show-details=direct`.
- Large benchmark smoke passed on 2026-06-03: `cabal run --enable-optimization=2 zmqx-overheads -- --scenario multipart --messages 10 --warmup 1 --frames 4 --payload-bytes 1048576 +RTS -s` produced elapsed 31.151ms, throughput 321.019 msg/s, and RTS allocation summary with 58,054,008 bytes allocated.
- Full suite passed on 2026-06-03: `cabal test all --test-show-details=direct` completed successfully.
- Build passed on 2026-06-03: `cabal build` reported `Up to date`.
- No Step 5 failures remained after targeted tests, benchmark smoke, full suite, and build.

### Step 6 delivery notes

- Must-update docs: `docs/performance.md` was updated with the TP-011 large send-path threshold/caveat and benchmark examples.
- Check-if-affected docs reviewed: `taskplane-tasks/CONTEXT.md` was read for deferred-work placement; `performance-overheads/` is not present in this worktree, so no read-only overhead report update was applicable.
- Discoveries logged in the table above for multipart-only thresholding and deferred true zero-copy ownership work.
- Deferred zero-copy work logged in `taskplane-tasks/CONTEXT.md` as **Send zero-copy ownership design**.
