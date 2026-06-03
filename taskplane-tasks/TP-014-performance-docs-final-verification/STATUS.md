# TP-014: Performance docs and final verification — Status

**Current Step:** Step 4: Delivery and future-work triage
**Status:** ✅ Complete
**Last Updated:** 2026-06-03
**Review Level:** 1
**Review Counter:** 3
**Iteration:** 1
**Size:** S

> **Hydration:** Checkboxes represent meaningful outcomes, not individual code changes. Workers expand steps when runtime discoveries warrant it.

---

### Step 0: Preflight
**Status:** ✅ Complete

- [x] Required files and paths exist
- [x] TP-009, TP-011, TP-012, and TP-013 complete
- [x] Benchmark harness and performance docs exist

---

### Step 1: Final verification plan
**Status:** ✅ Complete

- [x] TP-006 through TP-013 evidence inventoried
- [x] Final benchmark matrix chosen
- [x] Documentation update plan recorded
- [x] Unresolved future work identified

---

### Step 2: Run final benchmark and correctness sweep
**Status:** ✅ Complete

- [x] Final smoke benchmark matrix run
- [x] FULL test suite passing
- [x] Build passes
- [x] Outputs/limitations recorded

---

### Step 3: Update performance docs and user-facing pointers
**Status:** ✅ Complete

- [x] `docs/performance.md` updated
- [x] README/examples/quickstart pointers updated if needed
- [x] Remaining overheads documented honestly
- [x] Benchmark caveats documented

---

### Step 4: Delivery and future-work triage
**Status:** ✅ Complete

- [x] Unresolved performance work logged in CONTEXT.md
- [x] No source-code optimizations added by this task
- [x] Task STATUS evidence checked
- [x] Discoveries logged

---

## Reviews

| # | Type | Step | Verdict | File |
|---|------|------|---------|------|
| R001 | plan | Step 1 | APPROVE | `.reviews/R001-plan-step1.md` |
| R002 | plan | Step 2 | APPROVE | `.reviews/R002-plan-step2.md` |
| R003 | plan | Step 3 | APPROVE | `.reviews/R003-plan-step3.md` |

---

## Discoveries

| Discovery | Disposition | Location |
|-----------|-------------|----------|
| Final verification confirmed benchmark evidence is still smoke/trend-oriented and manual rather than persisted/statistical. | Documented in `docs/performance.md`; existing CONTEXT benchmark statistics/persistence item updated and reconfirmed. | `docs/performance.md`, `taskplane-tasks/CONTEXT.md` |
| User-facing performance pointers were mostly already present: README links the performance page and examples list the harness; quickstart does not need benchmark content. | Adjusted `docs/examples.md` pointer to mention the final smoke matrix and caveats; left README/quickstart otherwise unchanged. | `README.md`, `docs/examples.md`, `docs/quickstart.md` |
| Optional `performance-overheads/` report directory is absent in this worktree. | No report edits made; final documentation uses task STATUS evidence and current benchmark output. | Step 4 delivery check |

---

## Execution Log

| Timestamp | Action | Outcome |
|-----------|--------|---------|
| 2026-06-02 | Task staged | PROMPT.md and STATUS.md created |
| 2026-06-02 20:28 | Task started | Runtime V2 lane-runner execution |
| 2026-06-02 20:28 | Step 0 started | Preflight |
| 2026-06-03 04:30 | Step 0 completed | Preflight files, dependency task statuses, and benchmark docs/harness verified |
| 2026-06-03 04:30 | Step 1 started | Final verification plan |
| 2026-06-03 04:30 | Review R001 | plan Step 1: APPROVE |
| 2026-06-03 04:35 | Step 1 completed | Evidence inventory, final matrix, docs plan, and future-work triage recorded |
| 2026-06-03 04:35 | Step 2 started | Final benchmark and correctness sweep |
| 2026-06-03 04:35 | Review R002 | plan Step 2: APPROVE |
| 2026-06-03 04:50 | Step 2 completed | Final smoke matrix, full test suite, and build passed; limitations recorded |
| 2026-06-03 04:50 | Step 3 started | Performance docs and user-facing pointers |
| 2026-06-03 04:50 | Review R003 | plan Step 3: APPROVE |
| 2026-06-03 05:00 | Step 3 completed | Performance docs, examples pointer, remaining overheads, and caveats updated |
| 2026-06-03 05:00 | Step 4 started | Delivery and future-work triage |
| 2026-06-03 05:05 | Step 4 completed | Future-work triage, no-source-optimization check, STATUS evidence check, and discoveries completed |
| 2026-06-03 05:05 | Task complete | TP-014 final performance docs and verification complete |
| 2026-06-02 20:44 | Worker iter 1 | done in 957s, tools: 96 |
| 2026-06-02 20:44 | Task complete | .DONE created |

---

## Blockers

*None*

---

## Notes

- Step 0 verification: required prompt-scope files exist (`taskplane-tasks/CONTEXT.md`, docs files, cabal files, TP-006..TP-013 STATUS files) and `bench/` contains `bench.cabal`, `Bench/Helpers.hs`, and `Main.hs`.
- Step 0 dependency verification: TP-009, TP-011, TP-012, and TP-013 STATUS headers all report `**Status:** ✅ Complete`.
- Step 0 benchmark-doc verification: TP-006 STATUS reports complete, `docs/performance.md` exists, and opt-in benchmark harness files `bench/bench.cabal`, `bench/Bench/Helpers.hs`, and `bench/Main.hs` exist.

### Step 1 evidence inventory

- TP-006 established opt-in `zmqx-overheads` scenarios for direct, multipart receive/send, poll, REQ poll, EventLoop, lifecycle, plus `docs/performance.md`; gaps logged: no repeated-sample/statistical baseline store and EventLoop benchmark is end-to-end.
- TP-007 reduced receive `zmq_msg_t` storage churn via stack allocation while preserving payload copies; receive benchmarks were noisy/slightly slower in smoke, with the important caveat that RTS allocation does not measure removed foreign `malloc`/`free` churn; true receive zero-copy remains deferred.
- TP-008 introduced prepared poll internals and stack pollitem buffers; sockets=32 optimized smoke reduced RTS allocation from 8,920,824 to 4,944,064 bytes (~44.6%) with similar/slightly better latency; limitation: rebuilding `Sockets` values still pays preparation cost.
- TP-009 reduced REQ probe work while preserving the 10ms safety cadence; `req-poll-idle` benchmark scenario was added, but pure event-gated REQ wakeups proved unreliable, so residual 10ms quantization remains a documented tradeoff.
- TP-010 fused receive-side expected `EAGAIN` errno into one minimal C wrapper; optimized EAGAIN smoke was roughly baseline/noisy but preserved public error/scheduler behavior; send-side saturated/HWM error-path optimization was deferred pending a benchmark fixture.
- TP-011 added a conservative large-send path only for multipart prefix frames >=64KiB using C-owned copied `zmq_msg_init_data`; multipart 4x1MiB smoke improved substantially versus baseline, while single/final frames stayed on `zmq_send` because broader use regressed/noised direct sends; true Haskell `ByteString` zero-copy remains deferred.
- TP-012 reduced EventLoop coordination latency by replacing public wait polling and shortening worker slices; benchmark improved from about p50 10.2ms / elapsed 511ms / 3.55MB allocated to p50 ~1.25ms / elapsed ~62ms / 1.07MB allocated; remaining tradeoffs include receiver poll slices and positive-timeout timer strategy.
- TP-013 replaced lifecycle registry scanning with exact pending counters and thresholded compaction; lifecycle smoke elapsed improved from 107.034ms to ~76-83ms for 400 sockets, with modest allocation/residency increases; compaction threshold tuning and interruption regression coverage remain future work.

### Step 1 final benchmark matrix

Small final smoke matrix chosen for Step 2 (optimized build, no CI-style gates):

1. Standard all-scenario smoke covering direct send/receive, multipart receive, poll, valid REQ poll, EventLoop, and lifecycle:
   `cabal run --enable-optimization=2 zmqx-overheads -- --scenario all --messages 5 --warmup 1 --sockets 2 --frames 2 --payload-bytes 64 +RTS -s`
2. Explicit idle REQ poll/probe smoke (not included in `all` because it is timeout-dominated):
   `cabal run --enable-optimization=2 zmqx-overheads -- --scenario req-poll-idle --messages 3 --warmup 1 --timeout-ms 10 --payload-bytes 64 +RTS -s`
3. Threshold-adjacent direct send/receive comparison for the large-send caveat:
   `cabal run --enable-optimization=2 zmqx-overheads -- --scenario direct --messages 5 --warmup 1 --payload-bytes 65536 +RTS -s`
4. Threshold-adjacent multipart send smoke that exercises the TP-011 multipart-prefix path:
   `cabal run --enable-optimization=2 zmqx-overheads -- --scenario multipart --messages 5 --warmup 1 --frames 4 --payload-bytes 65536 +RTS -s`

### Step 1 documentation update plan

- Update `docs/performance.md` with a final-verification section that names the smoke matrix above, explains what each command covers, and records that `req-poll-idle` is intentionally separate from `--scenario all`.
- Add a concise before/after evidence summary from TP-006 through TP-013 where the STATUS files provide comparable numbers, explicitly marking smoke/noisy areas instead of claiming hard regression gates.
- Preserve and expand caveats: local `inproc://` benchmarks are machine-dependent; tiny smoke counts verify wiring, not performance claims; RTS allocation excludes some foreign heap churn; EventLoop numbers are end-to-end; large-send direct and multipart results must be interpreted separately.
- Keep README/examples/quickstart edits minimal. README already links `docs/performance.md`, and `docs/examples.md` already lists the opt-in benchmark harness; plan is to verify those pointers and leave them unchanged unless Step 3 reveals stale wording. `docs/quickstart.md` has no performance benchmark section today and does not need one for discoverability.

### Step 1 unresolved future work

Already logged in `taskplane-tasks/CONTEXT.md` and to preserve as future work (not implement in TP-014): benchmark statistics/persisted baselines, narrower EventLoop instrumentation, receive zero-copy ownership, send zero-copy ownership, dynamic/prepared poll-set builder API, EventLoop command wakeup fairness, EventLoop timer strategy, lifecycle compaction threshold tuning, lifecycle compaction interruption coverage, REQ poll wakeup-source limitations, and send-side EAGAIN/HWM benchmark fixture. Step 4 will verify whether final docs introduce any new unresolved performance work; if not, no new CONTEXT item is needed beyond confirming this list.

### Step 2 benchmark results

Final smoke matrix passed on 2026-06-03 with libzmq 4.3.4 / GHC 9.14 / Linux x86_64:

- `all` smoke (`messages=5`, `warmup=1`, `sockets=2`, `frames=2`, `payload=64`): direct elapsed 0.011ms p50 0.765us; multipart 0.036ms p50 7.134us; poll 0.119ms p50 23.817us; req-poll 0.189ms p50 23.440us; event-loop 6.609ms p50 1330.439us; lifecycle 0.877ms for 10 sockets; RTS allocated 917,224 bytes.
- `req-poll-idle` smoke (`messages=3`, `timeout=10ms`): elapsed 30.553ms, p50 10171.278us, RTS allocated 335,760 bytes, matching the expected timeout-dominated 10ms cadence.
- `direct` threshold-adjacent send smoke (`payload=65536`, `messages=5`): elapsed 0.615ms, p50 121.022us, RTS allocated 1,102,808 bytes; this is the direct comparison path, not the TP-011 multipart-prefix optimization.
- `multipart` threshold-adjacent send smoke (`payload=65536`, `frames=4`, `messages=5`): elapsed 1.889ms, p50 377.973us, RTS allocated 2,314,544 bytes; this exercises the large multipart-prefix path.

### Step 2 test suite result

- `cabal test all` passed on 2026-06-03. Cabal built/ran 29 automated test suites, including the performance-series suites `test-receive-path-auto`, `test-poll-scaling-auto`, `test-req-poll-probe-auto`, `test-blocking-backpressure-auto`, `test-large-payload-send-auto`, `test-event-loop-latency-auto`, and `test-finalizer-scaling-auto`; all reported PASS.

### Step 2 build result

- `cabal build` passed on 2026-06-03 with `Up to date`.

### Step 2 outputs and limitations

- No benchmark, full-suite, or build command failed in Step 2.
- Benchmark results are smoke-quality because counts are intentionally tiny and Cabal executable startup/RTS reporting dominate total elapsed time. They are suitable for final wiring validation and documentation examples, not hard performance regression gates.
- The smoke matrix uses local `inproc://` endpoints and current lane metadata only (libzmq 4.3.4, GHC 9.14, Linux x86_64). Future comparisons should rerun like-for-like commands on stable hardware.

### Step 3 docs update

- `docs/performance.md` now includes the TP-014 final smoke matrix, final smoke sample output, focused `req-poll-idle`/threshold send results, a TP-006..TP-013 remediation evidence table, remaining-overheads/future-work summary, and expanded smoke-vs-regression caveats.
- `docs/examples.md` performance benchmark pointer now mentions the scenario list, final smoke matrix, and caveats in `docs/performance.md`. README already links Performance benchmarks, and `docs/quickstart.md` remains focused on API usage without adding benchmark-specific content.
- Remaining overheads are documented in `docs/performance.md` under the remediation evidence table and `Remaining overheads and future work`, including zero-copy ownership, dynamic poll-set builders, REQ cadence, EventLoop wakeups/timers, lifecycle compaction tuning, and send-side HWM/EAGAIN coverage.
- Benchmark caveats are documented in `docs/performance.md`: smoke counts are not performance conclusions, optimized like-for-like runs are needed for trends, RTS totals are contextual and miss some foreign heap work, EventLoop/lifecycle scenario semantics affect interpretation, and large send results must separate direct from multipart-prefix behavior.

### Step 4 delivery triage

- Updated `taskplane-tasks/CONTEXT.md` timestamp and the existing **Benchmark statistics and persistence** future-work item to note TP-014's final verification still uses smoke-quality STATUS/docs evidence, not machine-readable persisted baselines or regression gates.
- Verified `git diff --name-only` lists only docs/task files (`docs/examples.md`, `docs/performance.md`, `taskplane-tasks/CONTEXT.md`, TP-014 `STATUS.md`) plus review metadata; no `lib/`, `c/`, `bench/`, or `test/` source-code optimization changes were made by TP-014.
- Checked TP-014 `STATUS.md` evidence sections are present for Step 1 inventory, Step 2 benchmark/test/build results, Step 3 docs updates, and Step 4 delivery triage.
- Discoveries table now records final evidence limitations, user-facing pointer status, and absent optional `performance-overheads/` reports.
