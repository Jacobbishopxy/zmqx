# Task: TP-011 - Optimize large-payload send path

**Created:** 2026-06-02
**Size:** L

## Review Level: 3 (Full)

**Assessment:** Introduces or refines FFI-backed send behavior for large frames, where memory ownership mistakes can corrupt data or crash callers. It spans shared socket sends, internal bindings/C helpers, tests, and benchmarks, so plan, code, and test review are required.
**Score:** 6/8 — Blast radius: 2, Pattern novelty: 2, Security: 1, Reversibility: 1

## Canonical Task Folder

```
taskplane-tasks/TP-011-send-large-payload-path/
├── PROMPT.md   ← This file (immutable above --- divider)
├── STATUS.md   ← Execution state (worker updates this)
├── .reviews/   ← Reviewer output (created by the orchestrator runtime)
└── .DONE       ← Created when complete
```

## Mission

Reduce large-payload send overhead while preserving `ByteString` lifetime safety and existing public socket semantics. The parallel review found that high-level sends use `zmq_send` from `ByteString` memory and likely pay a libzmq copy per frame, while internal `zmq_send_const`/`zmq_msg_init_data` helpers are not currently used by public hot paths. This task should design and implement a memory-safe large-frame path only where benchmarks show it helps and reviews approve the ownership model.

## Dependencies

- **Task:** TP-010 (blocking/error FFI paths should be stabilized before changing send internals)

## Context to Read First

**Tier 2 (area context):**
- `taskplane-tasks/CONTEXT.md`

**Relevant source and report files:**
- `performance-overheads/ffi-boundary.md` — send copy and zero-copy caveat findings, if present
- `performance-overheads/haskell-hotpaths.md` — send path and multipart findings, if present
- `docs/performance.md` — benchmark commands from TP-006/TP-010
- `lib/Zmqx/Core/Socket.hs` — `sendOne`, `sendMany`, `zhs_send_frame`, retry paths
- `lib/Zmqx/Internal/Functions.hs` — `zmq_send`, `zmq_send_const`, `zmq_msg_init_data`, current copy helpers
- `lib/Zmqx/Internal/Bindings/Functions.hs` and `lib/Zmqx/Internal/Bindings/Types.hsc` — FFI imports/types
- `c/utils.c`, `c/utils.h`, `c/zmq-wrapper.c`, `c/zmq-wrapper.h` — C callback/wrapper surface
- Role modules with sends: `Req`, `Rep`, `Dealer`, `Router`, `Pub`, `Push`, `Pair`
- Existing role tests for send/receive correctness

## Environment

- **Workspace:** `/home/xiey/Code/zmqx`
- **Services required:** None

## File Scope

- `lib/Zmqx/Core/Socket.hs`
- `lib/Zmqx/Internal/Functions.hs`
- `lib/Zmqx/Internal/Bindings/Functions.hs`
- `lib/Zmqx/Internal/Bindings/Types.hsc`
- `c/utils.c`
- `c/utils.h`
- `c/zmq-wrapper.c`
- `c/zmq-wrapper.h`
- `test/LargePayloadSendAuto.hs`
- `test/test.cabal`
- `bench/*`
- `docs/performance.md`
- `taskplane-tasks/CONTEXT.md`

## Steps

### Step 0: Preflight

- [ ] Required files and paths exist
- [ ] TP-010 is complete and targeted send/backpressure tests pass
- [ ] Baseline send benchmark captures small and large frame sizes before changes

### Step 1: Large-send ownership and API plan

> **Plan-review checkpoint** — this review must explicitly approve memory ownership, finalizer, and fallback behavior before implementation.

- [ ] Map current high-level send paths for single-frame and multipart sends across role modules
- [ ] Compare viable designs: keep `zmq_send`, use `zmq_send_const` for safe cases, use `zmq_msg_init_data` with retained `ForeignPtr`/finalizer ownership, or defer zero-copy and only reduce wrapper overhead
- [ ] Define a size threshold or opt-in/fallback policy if the optimized path should only apply to large payloads
- [ ] Define how errors, retries, multipart atomicity, cancellation, and `ByteString` lifetime are preserved
- [ ] Define correctness tests that would catch premature buffer release, multipart ordering errors, and large payload corruption

**Artifacts:**
- STATUS.md notes with ownership design, threshold/fallback policy, and rejected unsafe approaches

### Step 2: Implement reviewed large-send path

- [ ] Implement the approved send path with a conservative fallback to the existing behavior when safety or platform assumptions are not met
- [ ] Preserve current public API and role-module call sites unless a tiny internal helper signature change is required
- [ ] Preserve multipart send flags and first/rest frame ordering
- [ ] Keep finalizers/callbacks minimal, portable, and documented if C changes are required

**Artifacts:**
- `lib/Zmqx/Core/Socket.hs` (modified)
- `lib/Zmqx/Internal/Functions.hs` and bindings (modified)
- `c/*` (modified only if the ownership design requires it)

### Step 3: Add large-payload send tests and benchmark evidence

> **Code review checkpoint** — review the implementation for memory safety, fallback correctness, and benchmark interpretation.

- [ ] Add an automated test that sends and verifies large single-frame payloads and multipart payloads through at least two socket role pairs
- [ ] Register the test in `test/test.cabal`
- [ ] Run small and large payload send benchmarks before/after or compare against TP-006/TP-010 baseline
- [ ] Record whether the optimized path improves large payloads without regressing small payloads beyond noise

**Artifacts:**
- `test/LargePayloadSendAuto.hs` (new)
- `test/test.cabal` (modified)
- `bench/*` (modified only if needed)

### Step 4: Test-review and stress validation

> **Test-review checkpoint** — confirm tests are capable of catching lifetime/corruption regressions and that benchmark claims are supported.

- [ ] Run repeated large-payload sends with GC pressure enabled where practical
- [ ] Run multipart large-payload tests enough times to catch flaky lifetime/order bugs
- [ ] Run send benchmarks across small, medium, and large payload sizes with RTS allocation summary
- [ ] Update `docs/performance.md` if new benchmark options, thresholds, or caveats were introduced

**Artifacts:**
- `docs/performance.md` (modified only if needed)

### Step 5: Testing & Verification

- [ ] Run targeted tests: `cabal test test-large-payload-send-auto` plus relevant Req/Dealer/Pub/Push suites
- [ ] Run large-payload benchmark smoke with RTS allocation summary
- [ ] Run FULL test suite: `cabal test all`
- [ ] Run build: `cabal build`
- [ ] Fix all failures introduced by this task

### Step 6: Documentation & Delivery

- [ ] "Must Update" docs modified if benchmark usage or send caveats changed
- [ ] "Check If Affected" docs reviewed
- [ ] Discoveries logged in STATUS.md
- [ ] Any deferred zero-copy or platform-specific work logged to `taskplane-tasks/CONTEXT.md`

## Documentation Requirements

**Must Update:**
- `docs/performance.md` — only if benchmark commands, thresholds, or send-path caveats change

**Check If Affected:**
- `taskplane-tasks/CONTEXT.md` — log deferred zero-copy, platform, or FFI ownership future work
- `performance-overheads/*.md` — read-only context; do not rewrite unless explicitly useful for delivery notes

## Completion Criteria

- [ ] Large-payload send path is measurably improved or a reviewed safety/measurement reason documents why it remains unchanged
- [ ] `ByteString` lifetime and libzmq ownership are explicitly documented in code or STATUS.md
- [ ] Small payload performance is not meaningfully regressed
- [ ] New large-payload send tests pass repeatedly
- [ ] Benchmark evidence is recorded in STATUS.md
- [ ] `cabal build` and `cabal test all` pass

## Git Commit Convention

Commits happen at **step boundaries**. All commits for this task MUST include the task ID:

- **Step completion:** `feat(TP-011): complete Step N — description`
- **Tests:** `test(TP-011): description`
- **Bug fixes:** `fix(TP-011): description`
- **Hydration:** `hydrate: TP-011 expand Step N checkboxes`

## Do NOT

- Introduce unsafe zero-copy behavior without an approved ownership design
- Assume libzmq retains or releases Haskell memory without proof
- Regress small-message throughput without recording and justifying the tradeoff
- Change public API unless an amendment is added and reviewed
- Skip repeated large-payload tests and benchmark evidence

---

## Amendments (Added During Execution)

<!-- Workers add amendments here if issues discovered during execution. -->
