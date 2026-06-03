# TP-007: Reduce receive frame allocation and copy overhead — Status

**Current Step:** Step 5: Documentation & Delivery
**Status:** ✅ Complete
**Last Updated:** 2026-06-02
**Review Level:** 2
**Review Counter:** 7
**Iteration:** 2
**Size:** M

> **Hydration:** Checkboxes represent meaningful outcomes, not individual code changes. Workers expand steps when runtime discoveries warrant it.

---

### Step 0: Preflight
**Status:** ✅ Complete

- [x] Required files and paths exist
- [x] TP-006 benchmark target available
- [x] Baseline receive benchmark captured

---

### Step 1: Receive-path optimization plan
**Status:** ✅ Complete

- [x] Current receive paths mapped
- [x] Message-object allocation optimization chosen
- [x] Payload-copy/lifetime decision recorded
- [x] Correctness and benchmark evidence defined

---

### Step 2: Implement receive message-object optimization
**Status:** ✅ Complete

- [x] Per-frame message-object churn reduced where safe
- [x] Multipart and error semantics preserved
- [x] Debug-frame behavior preserved
- [x] FFI ownership comments updated if needed

---

### Step 3: Add receive-path regression coverage and benchmark evidence
**Status:** ✅ Complete

- [x] Focused receive-path automated test added
- [x] Test registered in `test/test.cabal`
- [x] Receive benchmark evidence captured
- [x] Performance docs updated if commands changed

---

### Step 4: Testing & Verification
**Status:** ✅ Complete

- [x] Targeted receive/poll tests passing
- [x] Receive benchmark smoke with RTS allocation summary run
- [x] FULL test suite passing
- [x] Build passes
- [x] All failures fixed

---

### Step 5: Documentation & Delivery
**Status:** ✅ Complete

- [x] "Must Update" docs modified if needed
- [x] "Check If Affected" docs reviewed
- [x] Discoveries logged
- [x] Deferred unsafe ideas logged as future work if needed

---

## Reviews

| # | Type | Step | Verdict | File |
|---|------|------|---------|------|
| 1 | Plan | Step 1 | APPROVE | inline reviewer |
| 2 | Plan | Step 2 | APPROVE | inline reviewer |
| 3 | Code | Step 2 | APPROVE | inline reviewer |
| 4 | Plan | Step 3 | APPROVE | `.reviews/R004-plan-step3.md` |
| 5 | Code | Step 3 | APPROVE | `.reviews/R005-code-step3.md` |
| 6 | Plan | Step 4 | APPROVE | `.reviews/R006-plan-step4.md` |
| 7 | Code | Step 4 | APPROVE | `.reviews/R007-code-step4.md` |

---

## Discoveries

| Discovery | Disposition | Location |
|-----------|-------------|----------|
| RTS allocation summaries do not directly measure the removed foreign-heap `malloc`/`free` for receive `zmq_msg_t` containers, so receive benchmark bytes are contextual rather than proof of the C allocation reduction. | Recorded in TP-007 benchmark notes; future benchmark instrumentation could add C allocation counters if needed. | `STATUS.md` Notes |

---

## Execution Log

| Timestamp | Action | Outcome |
|-----------|--------|---------|
| 2026-06-02 | Task staged | PROMPT.md and STATUS.md created |
| 2026-06-02 15:24 | Task started | Runtime V2 lane-runner execution |
| 2026-06-02 15:24 | Step 0 started | Preflight |
| 2026-06-02 | Step 1 plan review | APPROVE |
| 2026-06-02 | Step 2 plan review | APPROVE |
| 2026-06-02 | Step 2 code review | APPROVE |
| 2026-06-02 15:43 | Worker iter 1 | done in 1144s, tools: 79 |
| 2026-06-02 16:01 | Worker iter 2 | done in 1070s, tools: 76 |
| 2026-06-02 16:01 | Task complete | .DONE created |

---

## Blockers

*None*

---

## Notes

- Step 0 preflight: required source/docs/test paths exist; optional `performance-overheads/*.md` files are absent, so context will come from repo docs/source.
- TP-006 benchmark target `zmqx-overheads` is available (`cabal run zmqx-overheads -- --help` succeeded).
- Baseline receive benchmarks captured before source changes with optimized `zmqx-overheads`: direct 1000 msgs/payload64 elapsed 2.036ms, throughput 491203.769 msg/s, 3,529,848 bytes allocated; multipart 1000 msgs/3 frames/payload64 elapsed 3.862ms, throughput 258931.988 msg/s, 4,724,624 bytes allocated.
- Receive path map: public `Zmqx.receive/receives/receivesFor` delegates to role modules (`Pair`, `Req`, `Rep`, `Dealer`, `Router`, `Pull`, `Sub`, `XPub`, `XSub`); roles call `Socket.receiveOne`, `Socket.receiveMany`, or `Socket.receiveManyDontWait` after optional poll/REQ-buffer logic; shared receive helpers call `zhs_recv_frame_dontwait`, `zhs_recv_frame_wontblock`, and `zhs_recv_frame_wontblock_`, each currently going through `zhs_with_frame` -> `zmq_msg_init` (heap malloc) -> `zmq_msg_recv_dontwait` -> `zmq_msg_data`/`zmq_msg_more` -> `zmq_msg_close` + `zmq_msg_free`.
- Chosen optimization: keep the existing per-frame `zmq_msg_init`/`zmq_msg_close` lifecycle and error handling, but allocate the `zmq_msg_t` storage with `alloca` in `zhs_with_frame` via a new internal `zmq_msg_init_at` helper; this removes the Haskell heap `malloc`/`free` pair on every receive frame without changing socket API, libzmq message ownership, or multipart control flow.
- Payload-copy decision: keep `zmq_msg_data` using `ByteString.packCStringLen`; copied payloads preserve current ownership semantics after `zmq_msg_close`, while zero-copy receive would require attaching a finalizer to libzmq-owned message storage or moving message ownership into a `ByteString` finalizer and is deferred as unsafe for this task.
- Evidence plan: add an automated `test-receive-path-auto` covering single-frame and multipart receives for `Pair` and a second role pair (`Dealer`/`Router` or `Push`/`Pull`), run targeted receive/poll suites plus `cabal test all`, and rerun matching optimized direct/multipart receive benchmarks with `+RTS -s` against the Step 0 baseline.
- Step 2 implementation: `zhs_with_frame` now allocates receive `zmq_msg_t` storage with `alloca` and initializes it through `zmq_msg_init_at`, removing receive-frame `malloc`/`zmq_msg_free` churn while keeping `zmq_msg_close`; `cabal build` compiled the changed library.
- Multipart/error smoke after Step 2: `cabal test test-req-rep-auto test-dealer-router-auto test-items-poll-auto test-req-poll` passed, covering multipart role flows plus polling/REQ timeout behavior.
- Debug-frame behavior check: `cabal test test-dealer-router-auto --flag debug` passed and emitted the expected `>>`/`<<` frame trace output; debug receive still flows through `receiveManyDontWait` before returning the first frame.
- FFI ownership comments updated in `Zmqx.Internal.Functions`: heap `zmq_msg_init` storage still requires `zmq_msg_free`; new `zmq_msg_init_at` caller-managed storage must only be `zmq_msg_close`d and never freed by `zmq_msg_free`.
- Added `test/ReceivePathAuto.hs` with single-frame and multipart receive assertions for `PAIR` and `PUSH`/`PULL` role pairs.
- Registered `test-receive-path-auto` in `test/test.cabal` with the shared test properties and explicit `bytestring` dependency.
- Post-change receive benchmark evidence (optimized, same 1000-message/payload64 settings as Step 0): direct rerun `cabal run --enable-optimization=2 zmqx-overheads -- --scenario direct --messages 1000 --payload-bytes 64 +RTS -s` reported elapsed 2.102ms, throughput 475822.733 msg/s, 3,695,752 RTS bytes allocated; multipart rerun `cabal run --enable-optimization=2 zmqx-overheads -- --scenario multipart --messages 1000 --payload-bytes 64 --frames 3 +RTS -s` reported elapsed 4.088ms, throughput 244623.302 msg/s, 5,269,648 RTS bytes allocated. Baseline was direct 2.036ms / 491203.769 msg/s / 3,529,848 bytes and multipart 3.862ms / 258931.988 msg/s / 4,724,624 bytes; RTS totals are kept as contextual evidence because the removed `malloc`/`free` pair is foreign-heap churn rather than GHC heap allocation.
- `docs/performance.md` reviewed for Step 3; benchmark executable names/options and RTS guidance are unchanged, so no performance-doc content change was needed.
- Targeted Step 4 receive/poll tests passed: `cabal test test-receive-path-auto test-req-rep-auto test-dealer-router-auto test-items-poll-auto test-req-poll test-recv-for`.
- Step 4 benchmark smoke passed: `cabal run --enable-optimization=2 zmqx-overheads -- --scenario all --messages 5 --warmup 1 --sockets 2 --frames 2 --payload-bytes 64 +RTS -s`; receive scenarios reported direct 0.012ms / 423872.499 msg/s and multipart 0.020ms / 250150.090 msg/s, with overall RTS allocation summary 1,503,584 bytes allocated.
- Full default suite passed: `cabal test all` ran 23 automated test suites including `test-receive-path-auto`.
- Build gate passed: `cabal build` reported `Up to date` after the test/build runs.
- No task-introduced failures remain after targeted tests, benchmark smoke, full suite, and build gates all passed.
- Step 5 Must Update docs check: `docs/performance.md` was reviewed; benchmark names/options and interpretation guidance were unchanged, so no content modification was needed.
- Step 5 Check If Affected docs reviewed: `taskplane-tasks/CONTEXT.md` had TP-006 future-work notes and no TP-007 receive zero-copy item before this step; optional `performance-overheads/` report files are absent in this worktree.
- Deferred unsafe receive zero-copy idea logged in `taskplane-tasks/CONTEXT.md` as future work with the owned-message/finalizer lifetime concern.
| 2026-06-02 15:42 | Review R004 | plan Step 3: APPROVE |
| 2026-06-02 15:51 | Review R005 | code Step 3: APPROVE |
| 2026-06-02 15:53 | Review R006 | plan Step 4: APPROVE |
| 2026-06-02 15:58 | Review R007 | code Step 4: APPROVE |
