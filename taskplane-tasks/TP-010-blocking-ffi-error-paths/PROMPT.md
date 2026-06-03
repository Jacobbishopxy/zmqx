# Task: TP-010 - Optimize blocking FFI and expected-error paths

**Created:** 2026-06-02
**Size:** M

## Review Level: 2 (Plan and Code)

**Assessment:** Touches FFI wrappers and shared blocking send/receive paths, which affect most socket roles under backpressure or empty receives. The public API should remain stable, but scheduler/cancellation and error semantics require plan and code review.
**Score:** 5/8 — Blast radius: 2, Pattern novelty: 2, Security: 0, Reversibility: 1

## Canonical Task Folder

```
taskplane-tasks/TP-010-blocking-ffi-error-paths/
├── PROMPT.md   ← This file (immutable above --- divider)
├── STATUS.md   ← Execution state (worker updates this)
├── .reviews/   ← Reviewer output (created by the orchestrator runtime)
└── .DONE       ← Created when complete
```

## Mission

Reduce overhead in expected retry/error paths without weakening safe blocking behavior. The parallel review found that EAGAIN-heavy send/receive loops cross the FFI boundary repeatedly (`zmq_*`, `zmq_errno`, `ZMQ_FD`, `ZMQ_EVENTS`) and that normal public operations use exception machinery for expected `Left Error`s. This task should improve measured hot paths where practical while preserving async-exception friendliness, scheduler behavior, and current `Either Error` semantics.

## Dependencies

- **Task:** TP-007 (receive path refactor should be complete before changing shared blocking/error helpers)

## Context to Read First

**Tier 2 (area context):**
- `taskplane-tasks/CONTEXT.md`

**Relevant source and report files:**
- `performance-overheads/ffi-boundary.md` — EAGAIN/errno and safe/unsafe FFI findings, if present
- `performance-overheads/haskell-hotpaths.md` — blocking retry and exception-path findings, if present
- `docs/performance.md` — benchmark commands from TP-006
- `lib/Zmqx/Core/Socket.hs` — `blockUntilCanSend`, `blockUntilCanReceive`, `blockUntilEvent`, send/receive retry loops
- `lib/Zmqx/Internal/Functions.hs` — `zmq_errno`, send/receive wrappers, error conversion
- `lib/Zmqx/Internal/Bindings/Functions.hs` — safe/unsafe/interruptible imports
- `lib/Zmqx/Error.hs` — public error conversion helpers
- `c/` — add small C wrapper only if it reduces FFI crossings and remains portable
- `test/PollOut.hs`, `test/ReqRepAuto.hs`, `test/DealerRouterAuto.hs` — relevant correctness coverage

## Environment

- **Workspace:** `/home/xiey/Code/zmqx`
- **Services required:** None

## File Scope

- `lib/Zmqx/Core/Socket.hs`
- `lib/Zmqx/Internal/Functions.hs`
- `lib/Zmqx/Internal/Bindings/Functions.hs`
- `lib/Zmqx/Error.hs`
- `c/*`
- `test/BlockingBackpressureAuto.hs`
- `test/PollOut.hs`
- `test/test.cabal`
- `bench/*`
- `docs/performance.md`
- `taskplane-tasks/CONTEXT.md`

## Steps

### Step 0: Preflight

- [ ] Required files and paths exist
- [ ] TP-007 is complete and receive targeted tests pass
- [ ] Baseline EAGAIN/backpressure benchmark numbers are captured before changes

### Step 1: Blocking/error-path optimization plan

> **Plan-review checkpoint** — confirm any FFI or exception-path change preserves cancellation and `Either Error` behavior.

- [ ] Map current expected-error paths for `send*DontWait`, `receive*DontWait`, blocking send loops, and blocking receive loops
- [ ] Identify the most valuable measured target: fewer `zmq_errno` calls, fused C wrappers returning errno, fewer `ZMQ_EVENTS` probes, direct interruptible blocking call, or reduced exception wrapping on expected errors
- [ ] Define safety constraints for safe/unsafe/interruptible FFI and async exceptions
- [ ] Define correctness and benchmark evidence for empty receive, saturated send/HWM, `EINTR`, `ETERM`, and normal success paths

**Artifacts:**
- STATUS.md notes with chosen target and alternatives rejected as too risky or not measured

### Step 2: Implement the reviewed hot-path optimization

- [ ] Implement only the selected, measured optimization from Step 1
- [ ] Preserve public `Either Error` results and existing exception-to-error conversion boundaries
- [ ] Preserve safe scheduler behavior for operations that can block
- [ ] Keep C wrappers minimal and portable if C changes are required

**Artifacts:**
- `lib/Zmqx/Core/Socket.hs` (modified if retry/wait logic changes)
- `lib/Zmqx/Internal/Functions.hs` and bindings (modified if wrapper/error behavior changes)
- `lib/Zmqx/Error.hs` (modified only if public conversion boundaries are retained)
- `c/*` (modified only if a fused C wrapper is chosen)

### Step 3: Add backpressure/error-path regression coverage and benchmark evidence

> **Code review checkpoint** — review the FFI/error diff plus targeted tests and benchmark evidence.

- [ ] Add an automated test that exercises a backpressure or empty-receive path without relying on external services
- [ ] Register the test in `test/test.cabal`
- [ ] Run success-path and EAGAIN-heavy benchmarks before/after or compare against TP-006 baseline
- [ ] Update `docs/performance.md` only if benchmark commands/options change

**Artifacts:**
- `test/BlockingBackpressureAuto.hs` (new)
- `test/test.cabal` (modified)
- `bench/*` (modified only if needed)
- `docs/performance.md` (modified only if needed)

### Step 4: Testing & Verification

- [ ] Run targeted tests: `cabal test test-blocking-backpressure-auto test-poll-out` plus relevant Req/Dealer suites
- [ ] Run EAGAIN/backpressure benchmark smoke with RTS allocation summary
- [ ] Run FULL test suite: `cabal test all`
- [ ] Run build: `cabal build`
- [ ] Fix all failures introduced by this task

### Step 5: Documentation & Delivery

- [ ] "Must Update" docs modified if benchmark usage changed
- [ ] "Check If Affected" docs reviewed
- [ ] Discoveries logged in STATUS.md
- [ ] Any rejected high-risk FFI alternatives logged to `taskplane-tasks/CONTEXT.md`

## Documentation Requirements

**Must Update:**
- `docs/performance.md` — only if benchmark command names/options change

**Check If Affected:**
- `taskplane-tasks/CONTEXT.md` — log deferred FFI/error-path alternatives or risks
- `performance-overheads/*.md` — read-only context; do not rewrite unless explicitly useful for delivery notes

## Completion Criteria

- [ ] At least one measured expected-error/backpressure overhead is reduced, or a reviewed measurement shows no safe change is currently worthwhile
- [ ] Safe/unsafe/interruptible FFI usage remains correct for blocking operations
- [ ] Public error semantics remain compatible
- [ ] New backpressure/error-path test passes
- [ ] Benchmark evidence is recorded in STATUS.md
- [ ] `cabal build` and `cabal test all` pass

## Git Commit Convention

Commits happen at **step boundaries**. All commits for this task MUST include the task ID:

- **Step completion:** `feat(TP-010): complete Step N — description`
- **Tests:** `test(TP-010): description`
- **Bug fixes:** `fix(TP-010): description`
- **Hydration:** `hydrate: TP-010 expand Step N checkboxes`

## Do NOT

- Use `unsafe` FFI for calls that can block indefinitely
- Change public `Either Error` behavior without explicit amendment
- Swallow `EINTR`, `ETERM`, or context-termination semantics
- Add broad C wrapper surface beyond the measured optimization
- Skip EAGAIN/backpressure measurement

---

## Amendments (Added During Execution)

<!-- Workers add amendments here if issues discovered during execution. -->
