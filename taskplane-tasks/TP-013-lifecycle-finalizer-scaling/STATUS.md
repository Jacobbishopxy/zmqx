# TP-013: Improve lifecycle and finalizer registry scaling — Status

**Current Step:** Step 5: Documentation & Delivery
**Status:** ✅ Complete
**Last Updated:** 2026-06-03
**Review Level:** 2
**Review Counter:** 7
**Iteration:** 2
**Size:** M

> **Hydration:** Checkboxes represent meaningful outcomes, not individual code changes. Workers expand steps when runtime discoveries warrant it.

---

### Step 0: Preflight
**Status:** ✅ Complete

- [x] Required files and paths exist
- [x] TP-006 lifecycle benchmark available
- [x] Baseline lifecycle benchmark captured

---

### Step 1: Lifecycle registry design plan
**Status:** ✅ Complete

- [x] Lifecycle paths mapped
- [x] Highest-value scaling issue chosen
- [x] Scalable cleanup design recorded
- [x] Correctness tests defined

---

### Step 2: Implement scalable lifecycle bookkeeping
**Status:** ✅ Complete

- [x] Registry compaction/scanning overhead reduced
- [x] Idempotent close/finalizer safety preserved
- [x] `pendingSockets` semantics preserved or clarified
- [x] Context teardown behavior preserved

---

### Step 3: Add lifecycle scaling coverage and benchmark evidence
**Status:** ✅ Complete

- [x] Lifecycle scaling test added
- [x] Test registered in `test/test.cabal`
- [x] Existing lifecycle tests pass
- [x] Lifecycle benchmark evidence captured

---

### Step 4: Testing & Verification
**Status:** ✅ Complete

- [x] Targeted lifecycle tests passing
- [x] Lifecycle benchmark smoke with RTS summary run
- [x] FULL test suite passing
- [x] Build passes
- [x] All failures fixed

---

### Step 5: Documentation & Delivery
**Status:** ✅ Complete

- [x] "Must Update" docs modified if needed
- [x] "Check If Affected" docs reviewed
- [x] Discoveries logged
- [x] Remaining lifecycle tradeoffs logged if needed

---

## Reviews

| # | Type | Step | Verdict | File |
|---|------|------|---------|------|
| 1 | plan | 1 | UNAVAILABLE | n/a |
| 2 | plan | 2 | UNAVAILABLE | n/a |
| 3 | code | 2 | APPROVE | `.reviews/R003-code-step2.md` |
| 4 | plan | 3 | APPROVE | `.reviews/R004-plan-step3.md` |
| 5 | code | 3 | APPROVE | `.reviews/R005-code-step3.md` |
| 6 | plan | 4 | APPROVE | `.reviews/R006-plan-step4.md` |
| 7 | code | 4 | APPROVE | `.reviews/R007-code-step4.md` |

---

## Discoveries

| Discovery | Disposition | Location |
|-----------|-------------|----------|
| `performance-overheads/*.md` report files referenced by the prompt are absent in this worktree. | Recorded for delivery; no report edits made. | Step 5 docs review |
| Exact pending-count registry improved lifecycle smoke elapsed time but adds modest allocation/residency for registry state. | Accepted tradeoff for reduced scan frequency; future threshold tuning logged separately if needed. | Step 3/4 benchmark evidence |

---

## Execution Log

| Timestamp | Action | Outcome |
|-----------|--------|---------|
| 2026-06-02 | Task staged | PROMPT.md and STATUS.md created |
| 2026-06-02 17:23 | Task started | Runtime V2 lane-runner execution |
| 2026-06-02 17:23 | Step 0 started | Preflight |
| 2026-06-02 17:26 | Worker iter 1 | done in 184s, tools: 33 |
| 2026-06-03 | Step 1 plan review | review_step returned UNAVAILABLE; proceeding with recorded plan |
| 2026-06-03 | Step 2 plan review | review_step returned UNAVAILABLE before implementation |
| 2026-06-03 | Step 2 code review | review_step returned APPROVE for lifecycle registry implementation |
| 2026-06-03 | Step 3 plan review | review_step returned APPROVE before adding scaling coverage |
| 2026-06-03 | Step 3 code review | review_step returned APPROVE for scaling tests and benchmark evidence |
| 2026-06-03 | Step 4 plan review | review_step returned APPROVE before verification gate |
| 2026-06-03 | Step 4 code review | review_step returned APPROVE after verification gates |
| 2026-06-03 | Task completed | All steps complete; final commit ready for integration |
| 2026-06-02 18:00 | Worker iter 2 | done in 2045s, tools: 118 |
| 2026-06-02 18:00 | Task complete | .DONE created |

---

## Blockers

*None*

---

## Notes

### Step 0 baseline benchmark evidence

- TP-006 lifecycle benchmark availability confirmed with `cabal run zmqx-overheads -- --scenario lifecycle --messages 10 +RTS -s`: sockets=20, pending_before_cleanup=20, elapsed_ms=1.444, allocated=271,840 bytes.
- Baseline before lifecycle changes captured with `cabal run zmqx-overheads -- --scenario lifecycle --messages 200 +RTS -s`: sockets=400, pending_before_cleanup=400, elapsed_ms=107.034, throughput_msg_per_s=1868.573, allocated=554,000 bytes, maximum_residency=110,464 bytes.
- Exploratory baseline `--messages 1000` failed before code changes with `zmq_socket` EMFILE; retained `--messages 200` as the comparable smoke baseline.

### Step 1 lifecycle path map

- Socket open path: `openSocket` resolves the global `Context` or `openWith` provides an explicit `Context`; `openSocketIn` creates an `MVar` canary, creates the `Zmq_socket`, calls `makeSocketFinalizer (zmq_close zsocket) contextFinalizers canary#`, and conses the returned finalizer into the per-context `IORef [SocketFinalizer]` under `mask_` before applying socket options.
- Explicit close path today is effectively context/finalizer driven: public socket operations keep the socket/canary alive through `usingSocket`; direct deterministic close runs via `runSocketFinalizer` during context teardown or weak finalization rather than a public close API.
- GC weak finalizer path: `makeWeakPointer` attaches an idempotent close action to the socket canary; after `zmq_close`, the finalizer calls `compactSocketFinalizers`, which currently atomically removes the whole registry list, filters closed entries, and appends live entries back with any newer registrations.
- `pendingSockets` path: `pendingSockets` calls `compactSocketFinalizers` and then returns `length <$> readIORef contextFinalizers`, so diagnostics currently pay a full registry scan and count finalizers whose close action has not completed.
- Context teardown path: `run`/`withContext` cleanup runs under `uninterruptibleMask_`, calls `zmq_ctx_shutdown`, compacts the registry, reads finalizers, runs them in acquisition order (`reverse` of cons-list), clears the registry, then loops on `zmq_ctx_term` until success while preserving `EINTR`.

### Step 1 selected scaling issue

- Highest-value first fix: replace the plain per-context finalizer list with registry state that keeps an exact pending count and a stale-entry count. This addresses both repeated full-list scans from weak finalizers and `pendingSockets` scans in socket-churn diagnostics without changing public socket APIs.
- Expected complexity improvement: socket open remains O(1), each successful close/finalizer completion becomes O(1) count bookkeeping, `pendingSockets` becomes O(1) for the count, and full list scans happen only for thresholded compaction or teardown rather than on every weak finalizer/pending count.
- Teardown still needs one O(n) traversal of live registered finalizers to close sockets deterministically; that traversal is inherent to strict cleanup and is not the target for this TP.

### Step 1 scalable cleanup design

- Introduce an internal `SocketFinalizerRegistry` owned by each `Context`, backed by an `IORef` state record containing registered finalizers, exact pending count, and stale/closed-since-compact count. Keep `Context` abstract at the public `Zmqx` surface while updating internal users (`Context`, `Socket`, `EventLoop`) to carry the registry instead of a raw `IORef [SocketFinalizer]`.
- Registering a socket appends/conses its `SocketFinalizer` and increments the pending count under `mask_`. The weak finalizer still uses the canary key, still runs the idempotent close action, but after the close completes it decrements the registry pending count and increments the stale count instead of scanning the full registry every time.
- `pendingSockets` should return the registry pending count directly. This preserves the diagnostic meaning (“registered sockets whose close action has not completed”) while avoiding a count-time scan; the docs should clarify that compaction is internal/lazy rather than guaranteed by every diagnostic call.
- `compactSocketFinalizers` remains available for teardown and thresholded stale cleanup. It should keep the existing mask/restore/onException pattern so an async exception during scanning restores the removed snapshot and does not drop finalizers.
- Context teardown keeps the existing strict sequence: shutdown, compact stale entries once, traverse currently registered live finalizers in acquisition order, reset the registry, then term-loop preserving `EINTR` behavior.

### Step 1 correctness and evidence plan

- Explicit/GC close correctness: keep `test-finalizer-registry` cases for dead sockets draining after GC and mixed live/dead sockets preserving the live socket; these assert the pending count reaches 0/1 after weak finalizers run even though `pendingSockets` no longer scans.
- Interruption safety: keep the existing `pendingSockets` interruption test and add/adjust registry compaction code so async exceptions during a scan restore snapshots; the new O(1) pending path should make this safer, not weaker.
- Context shutdown/run guard: keep `test-run-guard` and context teardown behavior unchanged so shutdown still closes live sockets and `run` recovers after failures/user exceptions.
- Scaling coverage: add `test-finalizer-scaling-auto` that opens many short-lived inproc-free sockets under `withContext`, forces GC until `pendingSockets` drains, checks live sockets still count while referenced, and uses a portable `maxSockets` cap to avoid host FD limits.
- Benchmark evidence: compare `cabal run zmqx-overheads -- --scenario lifecycle --messages 200 +RTS -s` against the Step 0 baseline and record elapsed/allocation/pending fields in STATUS.

### Step 2 implementation notes

- Replaced the raw `IORef [SocketFinalizer]` registry with `SocketFinalizerRegistry` state carrying registered finalizers, exact pending count, and stale-entry count. Weak finalizers now close and update counters in O(1), with full compaction only when stale entries cross a threshold or during teardown.
- Verified the library still builds after the registry/type updates with `cabal build`.
- Verified idempotent weak-finalizer behavior with `cabal test test-finalizer-registry` after the registry counter changes.
- Clarified `pendingSockets` documentation: it still reports registered sockets whose close action has not completed, but reads the exact registry count directly instead of forcing compaction on every diagnostic call.
- Verified context/run teardown behavior still passes with `cabal test test-run-guard` after global/explicit contexts switched to `SocketFinalizerRegistry`.

### Step 3 scaling coverage notes

- Added `test/FinalizerScalingAuto.hs` with batched short-lived socket churn under `withContext`, explicit GC drain checks using `pendingSockets`, and a live-socket preservation scenario while dead sockets churn.
- Registered `test-finalizer-scaling-auto` in `test/test.cabal`.
- `cabal test test-finalizer-scaling-auto test-finalizer-registry test-run-guard` passed; this confirms the new scaling suite plus existing lifecycle suites after registration.
- Post-change lifecycle benchmark with the Step 0 comparison command `cabal run zmqx-overheads -- --scenario lifecycle --messages 200 +RTS -s`: sockets=400, pending_before_cleanup=400, elapsed_ms=83.056, throughput_msg_per_s=2408.017, allocated=613,616 bytes, maximum_residency=123,264 bytes. Compared with baseline elapsed_ms=107.034, this smoke run improved elapsed time by about 22%; allocation/residency rose modestly due to registry counter state.

### Step 4 verification evidence

- Targeted lifecycle gate passed with `cabal test test-finalizer-scaling-auto test-finalizer-registry test-run-guard`.
- Verification lifecycle benchmark smoke passed with `cabal run zmqx-overheads -- --scenario lifecycle --messages 200 +RTS -s`: sockets=400, pending_before_cleanup=400, elapsed_ms=76.812, throughput_msg_per_s=2603.760, allocated=613,632 bytes, maximum_residency=123,264 bytes.
- Full automated suite passed with `cabal test all`.
- Build gate passed with `cabal build` (up to date).
- No task-introduced failures remain after targeted tests, benchmark smoke, full suite, and build gate.

### Step 5 delivery notes

- Reviewed `docs/performance.md`; benchmark command names/options did not change, so no Must Update doc edit was needed.
- Reviewed Check If Affected targets: `taskplane-tasks/CONTEXT.md` is available for future-work notes; `performance-overheads/*.md` files are not present in this worktree, so there was no read-only report to update.
- Logged discoveries for absent `performance-overheads` reports and the elapsed-vs-allocation lifecycle benchmark tradeoff.
- Added `Lifecycle registry compaction threshold tuning` and `Lifecycle compaction interruption regression test` future work to `taskplane-tasks/CONTEXT.md`.
| 2026-06-02 17:40 | Review R003 | code Step 2: APPROVE |
| 2026-06-02 17:41 | Review R004 | plan Step 3: APPROVE |
| 2026-06-02 17:48 | Review R005 | code Step 3: APPROVE |
| 2026-06-02 17:49 | Review R006 | plan Step 4: APPROVE |
| 2026-06-02 17:56 | Review R007 | code Step 4: APPROVE |
