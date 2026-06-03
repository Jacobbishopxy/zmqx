# TP-008: Reduce poll allocation with prepared poll sets — Status

**Current Step:** Step 5: Documentation & Delivery
**Status:** ✅ Complete
**Last Updated:** 2026-06-02
**Review Level:** 2
**Review Counter:** 7
**Iteration:** 1
**Size:** M

> **Hydration:** Checkboxes represent meaningful outcomes, not individual code changes. Workers expand steps when runtime discoveries warrant it.

---

### Step 0: Preflight
**Status:** ✅ Complete

- [x] Required files and paths exist
- [x] TP-006 poll benchmark available
- [x] Baseline poll benchmark captured

---

### Step 1: Poll representation plan
**Status:** ✅ Complete

- [x] Current poll allocations mapped
- [x] Lower-allocation design chosen
- [x] REQ integration plan recorded
- [x] Benchmark and correctness evidence defined

---

### Step 2: Implement lower-allocation poll internals
**Status:** ✅ Complete

- [x] Per-call poll churn reduced where safe
- [x] `Ready` behavior preserved
- [x] Timeout semantics preserved
- [x] Blocking FFI safety preserved

---

### Step 3: Add poll scaling regression coverage and benchmark evidence
**Status:** ✅ Complete

- [x] Poll scaling/correctness test added
- [x] Test registered in `test/test.cabal`
- [x] Existing poll/REQ suites pass
- [x] Poll benchmark evidence captured

---

### Step 4: Testing & Verification
**Status:** ✅ Complete

- [x] Targeted poll tests passing
- [x] Poll benchmark smoke with RTS allocation summary run
- [x] FULL test suite passing
- [x] Build passes
- [x] All failures fixed

---

### Step 5: Documentation & Delivery
**Status:** ✅ Complete

- [x] "Must Update" docs modified if needed
- [x] "Check If Affected" docs reviewed
- [x] Discoveries logged
- [x] Remaining poll limitations logged if needed

---

## Reviews

| # | Type | Step | Verdict | File |
|---|------|------|---------|------|
| R001 | plan | 1 | APPROVE | inline review_step |
| R002 | plan | 2 | APPROVE | inline review_step |
| R003 | code | 2 | UNAVAILABLE | inline review_step |
| R004 | plan | 3 | APPROVE | inline review_step |
| R005 | code | 3 | APPROVE | inline review_step |
| R006 | plan | 4 | APPROVE | inline review_step |
| R007 | code | 4 | APPROVE | inline review_step |

---

## Discoveries

| Discovery | Disposition | Location |
|-----------|-------------|----------|
| Optional `performance-overheads/` reports referenced by the prompt are absent in this worktree | Treated as unavailable context; source/docs/tests were sufficient | Preflight |
| Poll hot path allocation was dominated by per-call `StorableArray`/`Set` rebuilding rather than libzmq polling itself for many-socket smoke runs | Addressed by prepared `Sockets` internals and stack pollitem buffers; benchmark evidence recorded | `lib/Zmqx/Core/Poll.hs`, STATUS Notes |
| Rebuilding public `Sockets` values now carries the preparation cost; callers should keep reusing `Sockets` for repeated polls | Logged as remaining limitation/future work for dynamic large poll sets | `taskplane-tasks/CONTEXT.md` |

---

## Execution Log

| Timestamp | Action | Outcome |
|-----------|--------|---------|
| 2026-06-02 | Task staged | PROMPT.md and STATUS.md created |
| 2026-06-02 16:01 | Task started | Runtime V2 lane-runner execution |
| 2026-06-02 16:01 | Step 0 started | Preflight |
| 2026-06-03 | Preflight paths verified | Source, docs, test, bench, and task paths exist; optional `performance-overheads/` reports are absent in this worktree |
| 2026-06-03 | TP-006 poll benchmark smoke | `cabal run --enable-optimization=2 zmqx-overheads -- --scenario poll --messages 1 --warmup 0 --sockets 1 --payload-bytes 16 --timeout-ms 100` passed |
| 2026-06-03 | Baseline poll benchmark captured | sockets=1 alloc=4,422,984 bytes elapsed=9.367ms; sockets=32 alloc=8,920,824 bytes elapsed=72.790ms |
| 2026-06-03 | Step 1 started | Poll allocation and representation plan |
| 2026-06-03 | Step 1 plan review | APPROVE |
| 2026-06-03 | Step 2 started | Implement lower-allocation poll internals |
| 2026-06-03 | Step 2 plan review | APPROVE |
| 2026-06-03 | Step 2 poll internals refactored | `Sockets` now precomputes pollable/input-REQ arrays; poll calls use stack pollitem buffers and `IntSet` readiness; `cabal build` passed |
| 2026-06-03 | Ready behavior smoke | `cabal test test-items-poll-auto test-poll-out` passed |
| 2026-06-03 | Timeout behavior smoke | `cabal test test-recv-for` passed |
| 2026-06-03 | Blocking FFI/REQ smoke | `zmq_poll_ptr` keeps unsafe FFI only for timeout 0 and interruptible FFI otherwise; `cabal test test-req-poll` passed |
| 2026-06-03 | Step 2 code review | UNAVAILABLE; proceeding with targeted build/test evidence |
| 2026-06-03 | Step 3 started | Add poll scaling test and benchmark evidence |
| 2026-06-03 | Step 3 plan review | APPROVE |
| 2026-06-03 | Poll scaling test added | `test/PollScalingAuto.hs` covers idle, one-ready, many-ready, mixed POLLIN/POLLOUT, and mixed REQ/PULL; `cabal test test-poll-scaling-auto` passed |
| 2026-06-03 | Poll scaling test registered | `test/test.cabal` adds `test-poll-scaling-auto`; target built and passed |
| 2026-06-03 | Existing poll/REQ suites | `cabal test test-items-poll-auto test-poll-out test-req-poll` passed |
| 2026-06-03 | Post-change poll benchmark | sockets=1 alloc=4,203,096 bytes elapsed=8.016ms; sockets=32 alloc=4,944,064 bytes elapsed=70.281ms |
| 2026-06-03 | Step 3 code review | APPROVE |
| 2026-06-03 | Step 4 started | Testing and verification |
| 2026-06-03 | Step 4 plan review | APPROVE |
| 2026-06-03 | Step 4 targeted poll tests | `cabal test test-poll-scaling-auto test-items-poll-auto test-poll-out test-req-poll` passed |
| 2026-06-03 | Step 4 benchmark smoke | `poll` messages=100 warmup=10: sockets=1 alloc=683,336 bytes elapsed=0.583ms; sockets=32 alloc=895,752 bytes elapsed=6.834ms |
| 2026-06-03 | Full test suite | `cabal test all` passed |
| 2026-06-03 | Build verification | `cabal build` passed (up to date) |
| 2026-06-03 | Failure review | No introduced test/build failures remain |
| 2026-06-03 | Step 4 code review | APPROVE |
| 2026-06-03 | Step 5 started | Documentation and delivery review |
| 2026-06-03 | Must-update docs reviewed | `docs/performance.md` benchmark commands/options still match; no doc edit needed |
| 2026-06-03 | Check-if-affected docs reviewed | `taskplane-tasks/CONTEXT.md` reviewed for future-work placement; optional `performance-overheads/` reports are absent |
| 2026-06-03 | Discoveries logged | STATUS Discoveries table updated with missing optional reports, allocation finding, and `Sockets` reuse limitation |
| 2026-06-03 | Remaining limitation logged | Added dynamic poll-set builder/API future-work item to `taskplane-tasks/CONTEXT.md` |
| 2026-06-03 | Task complete | All steps complete; final commit amended with code, tests, reviews, docs, and STATUS |
| 2026-06-02 16:41 | Worker iter 1 | done in 2403s, tools: 142 |
| 2026-06-02 16:41 | Task complete | .DONE created |

---

## Blockers

*None*

---

## Notes

- Pre-change poll baseline (2026-06-03, optimized, `--messages 1000 --warmup 100 --payload-bytes 64 --timeout-ms 100 +RTS -s`): sockets=1 elapsed=9.367ms p50=8.836us p95=13.776us alloc=4,422,984 bytes; sockets=32 elapsed=72.790ms p50=71.797us p95=81.504us alloc=8,920,824 bytes.
- Post-change poll benchmark (same command/settings): sockets=1 elapsed=8.016ms p50=7.659us p95=12.043us alloc=4,203,096 bytes (~5.0% lower allocation); sockets=32 elapsed=70.281ms p50=68.692us p95=78.498us alloc=4,944,064 bytes (~44.6% lower allocation).
- Step 1 allocation map: every `poll_` loop calls `prepareSockets`, traversing the public `Sockets` list, allocating `Set SomeSocket` values for REQ bookkeeping, reversing/accumulating the non-REQ poll list, allocating a `Primitive.Array SocketToPoll`, allocating/filling a `StorableArray Int Zmq_pollitem`, then after `zmq_poll` scanning that array and allocating another `Set` for ostensibly ready sockets. Timeout `0` pays that cost once; positive and blocking polls pay it again after each no-ready retry/EINTR-like loop, and REQ input polls additionally allocate/probe `Set`s while slicing waits.
- Step 1 selected design: keep the public `Sockets`/`Ready` API, but make `Sockets` an internal prepared representation built at `pollIn`/`pollInAlso`/`pollOut`/`pollOutAlso` time. Store immutable arrays of pollable sockets and pollitem templates, plus a static array of input REQ sockets that must be probed. During `poll_`, reuse a stack-allocated pollitem buffer copied from the template instead of allocating a `StorableArray`, scan `revents` into an `IntSet` keyed by `Zmq_socket` pointer identity, and keep REQ probes as the only dynamic preparation. Rejected alternatives: exporting a new prepared-poll API (public API churn), `unsafePerformIO` cached mutable arrays in `Sockets` (lifetime/thread-safety risk), and changing blocking `zmq_poll` to unsafe FFI (interruptibility regression).
- Step 1 REQ plan: classify input REQ sockets statically at `Sockets` construction and never pass them to libzmq polling. Each wait loop still reads the REQ message buffer first, reports buffered replies immediately, and probes empty REQs with `receiveManyDontWait` under the existing `EFSM` catch so stale/correlated replies remain hidden until a valid reply can be buffered. When any REQ is ready, poll non-REQ sockets with timeout `0` before returning so mixed sets keep reporting simultaneous readiness; otherwise input REQs keep the existing 10ms probe slice for positive and indefinite waits.
- Step 1 evidence plan: add `test-poll-scaling-auto` covering idle timeout, one-ready, many-ready, POLLOUT/POLLIN mix, and a mixed REQ/non-REQ ready set; rerun `test-items-poll-auto`, `test-poll-out`, and `test-req-poll`; compare optimized `poll` benchmark smoke against the pre-change sockets=1 and sockets=32 allocation/latency baselines captured in Step 0; finish with `cabal test all` and `cabal build`.
| 2026-06-02 16:11 | Review R001 | plan Step 1: APPROVE |
| 2026-06-02 16:15 | Review R002 | plan Step 2: APPROVE |
| 2026-06-02 16:23 | Review R004 | plan Step 3: APPROVE |
| 2026-06-02 16:31 | Review R005 | code Step 3: APPROVE |
| 2026-06-02 16:33 | Review R006 | plan Step 4: APPROVE |
| 2026-06-02 16:38 | Review R007 | code Step 4: APPROVE |
