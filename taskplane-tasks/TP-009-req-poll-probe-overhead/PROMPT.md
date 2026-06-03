# Task: TP-009 - Reduce REQ poll probe overhead

**Created:** 2026-06-02
**Size:** M

## Review Level: 2 (Plan and Code)

**Assessment:** Refines REQ-specific poll behavior that exists to protect correctness around stale/correlated replies. It should improve idle CPU and latency without changing public semantics, so plan and code review are required.
**Score:** 4/8 — Blast radius: 1, Pattern novelty: 2, Security: 0, Reversibility: 1

## Canonical Task Folder

```
taskplane-tasks/TP-009-req-poll-probe-overhead/
├── PROMPT.md   ← This file (immutable above --- divider)
├── STATUS.md   ← Execution state (worker updates this)
├── .reviews/   ← Reviewer output (created by the orchestrator runtime)
└── .DONE       ← Created when complete
```

## Mission

Reduce the overhead of REQ input polling while preserving the existing stale-reply/correlated-reply safeguards. The current implementation caps waits to a 10ms probe slice and may repeatedly nonblocking-receive on every input REQ; this task should make the probe path less wasteful and, where possible, reduce latency quantization without breaking `Req.receivesFor`, `pollFor`, or buffered reply behavior.

## Dependencies

- **Task:** TP-008 (poll internals should be stabilized before refining REQ probe behavior)

## Context to Read First

**Tier 2 (area context):**
- `taskplane-tasks/CONTEXT.md`

**Relevant source and report files:**
- `performance-overheads/haskell-hotpaths.md` — REQ buffering/probing findings, if present
- `performance-overheads/concurrency-lifecycle.md` — REQ 10ms wakeup and probe findings, if present
- `docs/performance.md` — benchmark commands from TP-006/TP-008
- `lib/Zmqx/Core/Poll.hs` — `reqProbeSliceUs`, REQ partitioning, probing, and buffered readiness
- `lib/Zmqx/Req.hs` — REQ send/receive/timeout behavior
- `lib/Zmqx/Core/Socket.hs` — REQ `Extra` buffers and receive helpers
- `test/ReqPoll.hs`, `test/ItemsPollAuto.hs`, `test/PollOut.hs` — existing correctness tests

## Environment

- **Workspace:** `/home/xiey/Code/zmqx`
- **Services required:** None

## File Scope

- `lib/Zmqx/Core/Poll.hs`
- `lib/Zmqx/Req.hs`
- `lib/Zmqx/Core/Socket.hs`
- `test/ReqPollProbeAuto.hs`
- `test/ReqPoll.hs`
- `test/test.cabal`
- `bench/*`
- `docs/performance.md`
- `taskplane-tasks/CONTEXT.md`

## Steps

### Step 0: Preflight

- [ ] Required files and paths exist
- [ ] TP-008 is complete and poll targeted tests pass
- [ ] Baseline REQ poll benchmark captures idle polling and valid-reply latency before changes

### Step 1: REQ probe behavior plan

> **Plan-review checkpoint** — confirm the plan cannot reintroduce stale/correlated reply bugs.

- [ ] Map the current REQ buffer/probe lifecycle for `Req.receivesFor`, `pollFor`, and mixed poll sets
- [ ] Identify which overhead to reduce first: fixed 10ms wakeups, O(number of REQs) probes, repeated `MVar`/IORef traffic, or extra receive attempts
- [ ] Choose a design that preserves buffered replies and timeout semantics, such as adaptive probe cadence, event-gated probes, reduced repeated probing, or better integration with prepared poll data
- [ ] Define test cases for stale reply skip, valid reply readiness, timeout accuracy, and mixed REQ/non-REQ polls

**Artifacts:**
- STATUS.md notes with chosen design, preserved invariants, and any deferred alternatives

### Step 2: Implement lower-overhead REQ polling

- [ ] Reduce unnecessary probe wakeups or repeated probe work while preserving correctness
- [ ] Preserve `Req.receivesFor` behavior for timeout `0`, positive timeouts, and blocking receives
- [ ] Preserve `pollFor` behavior with only REQ sockets and with mixed REQ/non-REQ sockets
- [ ] Keep buffer ownership and `IORef` updates exception-safe

**Artifacts:**
- `lib/Zmqx/Core/Poll.hs` (modified)
- `lib/Zmqx/Req.hs` or `lib/Zmqx/Core/Socket.hs` (modified only if needed)

### Step 3: Add REQ probe regression coverage and benchmark evidence

> **Code review checkpoint** — review the REQ probe diff plus stale-reply tests and benchmark evidence.

- [ ] Add an automated test focused on REQ probe behavior, including idle timeout and valid-reply readiness
- [ ] Register the test in `test/test.cabal`
- [ ] Run existing `ReqPoll` stale-reply scenarios and confirm behavior is unchanged
- [ ] Run REQ poll benchmarks for idle CPU/probe count proxy and reply latency; record results in STATUS.md

**Artifacts:**
- `test/ReqPollProbeAuto.hs` (new)
- `test/test.cabal` (modified)
- `bench/*` (modified only if scenario gaps are discovered)
- `docs/performance.md` (modified only if command names/options change)

### Step 4: Testing & Verification

- [ ] Run targeted tests: `cabal test test-req-poll-probe-auto test-req-poll test-items-poll-auto test-poll-out`
- [ ] Run REQ poll benchmark smoke with RTS allocation summary
- [ ] Run FULL test suite: `cabal test all`
- [ ] Run build: `cabal build`
- [ ] Fix all failures introduced by this task

### Step 5: Documentation & Delivery

- [ ] "Must Update" docs modified if benchmark usage changed
- [ ] "Check If Affected" docs reviewed
- [ ] Discoveries logged in STATUS.md
- [ ] Remaining REQ poll tradeoffs logged to `taskplane-tasks/CONTEXT.md`

## Documentation Requirements

**Must Update:**
- `docs/performance.md` — only if benchmark command names/options change

**Check If Affected:**
- `taskplane-tasks/CONTEXT.md` — log deferred REQ probe design tradeoffs or future work
- `performance-overheads/*.md` — read-only context; do not rewrite unless explicitly useful for delivery notes

## Completion Criteria

- [ ] REQ poll path has reduced idle/probe overhead or a measured, reviewed reason is recorded
- [ ] Stale/correlated reply safeguards still pass existing and new tests
- [ ] Timeout semantics remain compatible
- [ ] REQ poll benchmark evidence is recorded in STATUS.md
- [ ] `cabal build` and `cabal test all` pass

## Git Commit Convention

Commits happen at **step boundaries**. All commits for this task MUST include the task ID:

- **Step completion:** `feat(TP-009): complete Step N — description`
- **Tests:** `test(TP-009): description`
- **Bug fixes:** `fix(TP-009): description`
- **Hydration:** `hydrate: TP-009 expand Step N checkboxes`

## Do NOT

- Remove or weaken REQ stale/correlated reply protection
- Change public REQ timeout semantics without explicit amendment
- Hide busy CPU behind longer sleeps without measuring latency impact
- Skip mixed REQ/non-REQ poll validation
- Modify unrelated send, receive, EventLoop, or lifecycle code

---

## Amendments (Added During Execution)

<!-- Workers add amendments here if issues discovered during execution. -->
