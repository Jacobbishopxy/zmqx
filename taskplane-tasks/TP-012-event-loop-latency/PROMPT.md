# Task: TP-012 - Reduce EventLoop coordination latency

**Created:** 2026-06-02
**Size:** M

## Review Level: 2 (Plan and Code)

**Assessment:** Optimizes the high-level `Zmqx.EventLoop` coordination path without changing direct socket APIs. It touches concurrent worker loops and shutdown behavior, so both plan and code review are required.
**Score:** 4/8 — Blast radius: 1, Pattern novelty: 2, Security: 0, Reversibility: 1

## Canonical Task Folder

```
taskplane-tasks/TP-012-event-loop-latency/
├── PROMPT.md   ← This file (immutable above --- divider)
├── STATUS.md   ← Execution state (worker updates this)
├── .reviews/   ← Reviewer output (created by the orchestrator runtime)
└── .DONE       ← Created when complete
```

## Mission

Reduce avoidable millisecond-scale latency in `Zmqx.EventLoop` while preserving shutdown safety, worker-owned socket semantics, bounded mailbox behavior, and callback behavior. The parallel review found public sends and receives that poll `MVar`s with `threadDelay 1000`, worker send retries that sleep 1ms, and receiver polling slices that can add tail latency. This task should replace polling sleeps with event-driven or better-bounded waits where practical and prove the change with EventLoop benchmarks/tests.

## Dependencies

- **Task:** TP-006 (benchmark foundation must exist so EventLoop latency changes can be measured)

## Context to Read First

**Tier 2 (area context):**
- `taskplane-tasks/CONTEXT.md`

**Relevant source and report files:**
- `performance-overheads/haskell-hotpaths.md` — EventLoop overhead findings, if present
- `performance-overheads/benchmark-plan.md` — EventLoop benchmark recommendations, if present
- `docs/performance.md` — benchmark commands from TP-006
- `lib/Zmqx/EventLoop.hs` — command queue, worker loops, mailbox/callback delivery, shutdown coordination
- `lib/Zmqx/Core/Poll.hs` — receiver polling behavior used by EventLoop
- `test/EventLoopSendAuto.hs`, `test/EventLoopReceiveAuto.hs`, `test/EventLoopTransceiverAuto.hs`, `test/EventLoopSafetyAuto.hs` — existing EventLoop correctness suites

## Environment

- **Workspace:** `/home/xiey/Code/zmqx`
- **Services required:** None

## File Scope

- `lib/Zmqx/EventLoop.hs`
- `lib/Zmqx/Core/Poll.hs`
- `test/EventLoopLatencyAuto.hs`
- `test/EventLoopSendAuto.hs`
- `test/EventLoopReceiveAuto.hs`
- `test/EventLoopTransceiverAuto.hs`
- `test/EventLoopSafetyAuto.hs`
- `test/test.cabal`
- `bench/*`
- `docs/performance.md`
- `docs/quickstart.md`
- `taskplane-tasks/CONTEXT.md`

## Steps

### Step 0: Preflight

- [ ] Required files and paths exist
- [ ] Existing EventLoop targeted tests pass before changes
- [ ] Baseline EventLoop send/receive benchmark captures latency and allocation before changes

### Step 1: EventLoop wait strategy plan

> **Plan-review checkpoint** — confirm the plan preserves shutdown behavior and does not introduce blocking deadlocks.

- [ ] Map command send wait, worker send retry, receiver poll loop, mailbox `recv`, callback delivery, and shutdown paths
- [ ] Identify which `threadDelay` polling loops can be replaced with blocking waits, timeout combinators, STM/MVar signaling, or existing socket readiness waits
- [ ] Define how worker failure, loop closure, context termination, and caller timeouts are still observed promptly
- [ ] Define benchmark/test evidence for low-latency send ack, mailbox receive, transceiver roundtrip, slow callbacks, and shutdown

**Artifacts:**
- STATUS.md notes with selected wait strategy and preserved invariants

### Step 2: Implement lower-latency coordination

- [ ] Replace avoidable 1ms polling sleeps in public command waits or mailbox waits with reviewed event-driven/bounded waits
- [ ] Reduce worker send retry sleep overhead where safe, while preserving backpressure and shutdown detection
- [ ] Preserve bounded mailbox drop policy and callback threading semantics
- [ ] Preserve startup validation and context mismatch behavior

**Artifacts:**
- `lib/Zmqx/EventLoop.hs` (modified)
- `lib/Zmqx/Core/Poll.hs` (modified only if shared poll behavior needs a narrow hook from TP-008/TP-009)

### Step 3: Add EventLoop latency regression coverage and benchmark evidence

> **Code review checkpoint** — review EventLoop diff plus concurrency tests and benchmark evidence.

- [ ] Add an automated EventLoop latency/safety test that avoids brittle wall-clock assertions where possible
- [ ] Register the test in `test/test.cabal`
- [ ] Run EventLoop benchmarks before/after or compare against TP-006 baseline with identical settings
- [ ] Record latency, allocation, and any tail-latency caveats in STATUS.md

**Artifacts:**
- `test/EventLoopLatencyAuto.hs` (new)
- `test/test.cabal` (modified)
- `bench/*` (modified only if needed)
- `docs/performance.md` (modified only if benchmark options change)

### Step 4: Testing & Verification

- [ ] Run targeted tests: `cabal test test-event-loop-latency-auto test-event-loop-send-auto test-event-loop-receive-auto test-event-loop-transceiver-auto test-event-loop-safety-auto`
- [ ] Run EventLoop benchmark smoke with RTS allocation summary
- [ ] Run FULL test suite: `cabal test all`
- [ ] Run build: `cabal build`
- [ ] Fix all failures introduced by this task

### Step 5: Documentation & Delivery

- [ ] "Must Update" docs modified if EventLoop performance caveats or benchmark usage changed
- [ ] "Check If Affected" docs reviewed
- [ ] Discoveries logged in STATUS.md
- [ ] Remaining EventLoop latency tradeoffs logged to `taskplane-tasks/CONTEXT.md`

## Documentation Requirements

**Must Update:**
- `docs/performance.md` — only if EventLoop benchmark command names/options or caveats change

**Check If Affected:**
- `docs/quickstart.md` — only if EventLoop usage guidance needs a caveat update
- `taskplane-tasks/CONTEXT.md` — log deferred EventLoop latency/fairness future work
- `performance-overheads/*.md` — read-only context; do not rewrite unless explicitly useful for delivery notes

## Completion Criteria

- [ ] At least one avoidable EventLoop polling-sleep path is removed or a measured, reviewed reason is recorded
- [ ] EventLoop shutdown, worker failure propagation, mailbox, callback, and transceiver behavior remain compatible
- [ ] New EventLoop latency/safety test passes
- [ ] EventLoop benchmark evidence is recorded in STATUS.md
- [ ] `cabal build` and `cabal test all` pass

## Git Commit Convention

Commits happen at **step boundaries**. All commits for this task MUST include the task ID:

- **Step completion:** `feat(TP-012): complete Step N — description`
- **Tests:** `test(TP-012): description`
- **Bug fixes:** `fix(TP-012): description`
- **Hydration:** `hydrate: TP-012 expand Step N checkboxes`

## Do NOT

- Regress EventLoop shutdown safety or worker failure visibility
- Replace bounded sleeps with uninterruptible waits that can deadlock shutdown
- Add brittle tests that depend on exact scheduler timing on slow machines
- Change direct socket APIs
- Skip EventLoop benchmark evidence

---

## Amendments (Added During Execution)

<!-- Workers add amendments here if issues discovered during execution. -->
