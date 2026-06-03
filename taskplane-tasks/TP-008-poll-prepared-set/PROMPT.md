# Task: TP-008 - Reduce poll allocation with prepared poll sets

**Created:** 2026-06-02
**Size:** M

## Review Level: 2 (Plan and Code)

**Assessment:** Optimizes `Zmqx.Core.Poll`, a shared path used by role timeout helpers and EventLoop. The public API should stay compatible, but readiness semantics are subtle enough to require plan and code review.
**Score:** 5/8 — Blast radius: 2, Pattern novelty: 2, Security: 0, Reversibility: 1

## Canonical Task Folder

```
taskplane-tasks/TP-008-poll-prepared-set/
├── PROMPT.md   ← This file (immutable above --- divider)
├── STATUS.md   ← Execution state (worker updates this)
├── .reviews/   ← Reviewer output (created by the orchestrator runtime)
└── .DONE       ← Created when complete
```

## Mission

Reduce allocation and CPU overhead in `pollFor`/`poll` by avoiding repeated rebuilds of poll arrays and ready `Set`s where practical. The parallel review found that every poll call partitions sockets, builds `Set`s, allocates a `StorableArray`, scans results, and returns a closure over a `Set`; this task should introduce a safer lower-allocation representation while preserving the existing public `Sockets`/`Ready` API and all REQ-specific correctness behavior.

## Dependencies

- **Task:** TP-006 (benchmark foundation must exist so poll changes can be measured)

## Context to Read First

**Tier 2 (area context):**
- `taskplane-tasks/CONTEXT.md`

**Relevant source and report files:**
- `performance-overheads/haskell-hotpaths.md` — poll hot-path findings, if present
- `performance-overheads/concurrency-lifecycle.md` — poll rebuild and REQ wakeup findings, if present
- `docs/performance.md` — benchmark commands from TP-006
- `lib/Zmqx/Core/Poll.hs` — poll set preparation, ready extraction, REQ probe handling
- `lib/Zmqx/Internal/Functions.hs` and `lib/Zmqx/Internal/Bindings/Types.hsc` — `zmq_poll` marshalling
- Role modules using timeout receives, especially `lib/Zmqx/Dealer.hs`, `lib/Zmqx/Req.hs`, and similar modules
- `test/ItemsPollAuto.hs`, `test/PollOut.hs`, `test/ReqPoll.hs` — existing poll correctness coverage

## Environment

- **Workspace:** `/home/xiey/Code/zmqx`
- **Services required:** None

## File Scope

- `lib/Zmqx/Core/Poll.hs`
- `lib/Zmqx/Internal/Functions.hs`
- `lib/Zmqx/Internal/Bindings/Types.hsc`
- `lib/Zmqx/Core/SomeSocket.hs`
- `lib/Zmqx/Core/Socket.hs`
- `test/PollScalingAuto.hs`
- `test/ItemsPollAuto.hs`
- `test/PollOut.hs`
- `test/ReqPoll.hs`
- `test/test.cabal`
- `bench/*`
- `docs/performance.md`
- `taskplane-tasks/CONTEXT.md`

## Steps

### Step 0: Preflight

- [ ] Required files and paths exist
- [ ] TP-006 poll benchmark runs for at least 1 and many sockets
- [ ] Baseline poll allocation/latency numbers are captured before changes

### Step 1: Poll representation plan

> **Plan-review checkpoint** — confirm the plan preserves public API compatibility and all existing readiness semantics.

- [ ] Map allocations in `prepareSockets`, `peekReadySockets`, and `poll_` for timeout `0`, positive, and blocking polls
- [ ] Choose a lower-allocation design, such as an internal prepared poll set, reusable array within one wait loop, index/bitmap readiness, or a combination that fits the current API
- [ ] Define how REQ sockets and buffered REQ replies remain integrated without regressing stale/correlated reply behavior
- [ ] Define benchmark and correctness evidence for idle, one-ready, many-ready, input/output, and mixed REQ/non-REQ poll sets

**Artifacts:**
- STATUS.md notes with selected design and rejected alternatives

### Step 2: Implement lower-allocation poll internals

- [ ] Refactor poll preparation/extraction to reduce per-call list/set/array churn on common paths
- [ ] Preserve `Ready (forall a. Socket a -> Bool)` behavior for existing callers
- [ ] Preserve timeout semantics, including timeout `0`, positive timeouts, and indefinite blocking
- [ ] Keep `zmq_poll` safe/interruptible behavior unchanged for blocking waits unless a reviewed alternative is required

**Artifacts:**
- `lib/Zmqx/Core/Poll.hs` (modified)
- `lib/Zmqx/Internal/Functions.hs` or bindings (modified only if marshalling changes)

### Step 3: Add poll scaling regression coverage and benchmark evidence

> **Code review checkpoint** — review the full poll diff plus tests/bench evidence.

- [ ] Add an automated poll scaling/correctness test with multiple sockets and mixed readiness
- [ ] Register the test in `test/test.cabal`
- [ ] Run existing poll suites and verify no stale REQ behavior regressed
- [ ] Run poll benchmark before/after or compare against TP-006 recorded baseline with identical settings

**Artifacts:**
- `test/PollScalingAuto.hs` (new)
- `test/test.cabal` (modified)
- `bench/*` (modified only if needed)
- `docs/performance.md` (modified only if command names/options change)

### Step 4: Testing & Verification

- [ ] Run targeted tests: `cabal test test-poll-scaling-auto test-items-poll-auto test-poll-out test-req-poll`
- [ ] Run poll benchmark smoke for N=1 and a larger N with RTS allocation summary
- [ ] Run FULL test suite: `cabal test all`
- [ ] Run build: `cabal build`
- [ ] Fix all failures introduced by this task

### Step 5: Documentation & Delivery

- [ ] "Must Update" docs modified if benchmark usage changed
- [ ] "Check If Affected" docs reviewed
- [ ] Discoveries logged in STATUS.md
- [ ] Any remaining poll scaling limitations logged to `taskplane-tasks/CONTEXT.md`

## Documentation Requirements

**Must Update:**
- `docs/performance.md` — only if benchmark command names/options change

**Check If Affected:**
- `taskplane-tasks/CONTEXT.md` — log deferred poll API or scaling improvements
- `performance-overheads/*.md` — read-only context; do not rewrite unless explicitly useful for delivery notes

## Completion Criteria

- [ ] Poll internals allocate less on the common benchmarked paths or a measured, reviewed reason is recorded
- [ ] Existing `Sockets`/`Ready` public API remains compatible
- [ ] Existing poll, timeout, output-readiness, and REQ stale-reply tests pass
- [ ] New poll scaling test passes
- [ ] Poll benchmark evidence is recorded in STATUS.md
- [ ] `cabal build` and `cabal test all` pass

## Git Commit Convention

Commits happen at **step boundaries**. All commits for this task MUST include the task ID:

- **Step completion:** `feat(TP-008): complete Step N — description`
- **Tests:** `test(TP-008): description`
- **Bug fixes:** `fix(TP-008): description`
- **Hydration:** `hydrate: TP-008 expand Step N checkboxes`

## Do NOT

- Break the public `Poll` API or role-module timeout helpers
- Remove REQ stale/correlated reply safeguards
- Replace blocking `zmq_poll` with an unsafe FFI call for long waits
- Optimize only the no-socket or trivial path while making multi-socket behavior worse
- Skip benchmark evidence

---

## Amendments (Added During Execution)

<!-- Workers add amendments here if issues discovered during execution. -->
