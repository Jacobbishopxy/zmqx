# Zmqx Fix-First Checklist

Ordered execution plan:

- [x] Audit `TODO.md` against the current codebase.
- [x] Confirm the repo is already a hybrid of Plan A hardening plus early Plan B work.
- [x] Finish Phase 1 correctness fixes before taking on the larger roadmap.

Working rules:

- [x] Fix correctness gaps in the current public API first.
- [x] Add tests that lock in each fix before moving on.
- [x] Return to `TODO.md` only after the current API behaves consistently.

## Phase 1: Fix Current Bugs and API Inconsistencies

### 1. Make `monitor` work with explicit contexts

Current state:

- [x] Bug identified: `monitor` currently depends on a global-context socket open path.
- [x] Bug identified: `Monitor.hs` still contains reachable `undefined`s.
- [x] `monitor` setup works under `withContext` without falling back to globals.
- [x] `monitor` works under both `run` and `withContext`.

Files:

- [x] `lib/Zmqx/Core/Monitor.hs`
- [x] `lib/Zmqx/Core/Socket.hs`
- [x] `test/ContextualOpen.hs`
- [x] `test/MonitorEvent.hs`

Implementation checklist:

- [x] Ensure sockets retain enough context information for internal helpers to stay in the correct context.
- [x] Remove hidden global usage from monitor setup.
- [x] Open the internal monitor PAIR socket in the same context as the monitored socket.
- [x] Replace every reachable `undefined` in `Monitor.hs`.
- [x] Parse monitor events safely instead of bottoming on unexpected data.
- [x] Fix monitor endpoint generation so it uses valid endpoint text.
- [x] Fix monitor socket naming logic if it is reversed or misleading.
- [x] Confirm monitor teardown does not leak sockets.

Acceptance criteria:

- [x] `monitor` succeeds under `run`.
- [x] `monitor` succeeds under `withContext`.
- [x] monitor event parsing returns typed values or a safe parse miss, never a runtime bottom.
- [x] there is a test that would have failed on the old global-only monitor path.

### 2. Fix REQ timeout polling semantics

Current state:

- [x] Likely correctness gap identified in REQ polling.
- [x] `Req.receivesFor` uses the REQ message buffer consistently after polling.
- [x] Deterministic stale-reply regression test exists.
- [x] `Req.receivesFor` fully honors its timeout contract.

Files:

- [x] `lib/Zmqx/Core/Poll.hs`
- [x] `lib/Zmqx/Req.hs`
- [x] `lib/Zmqx/Core/Socket.hs`
- [x] `test/ReqPoll.hs`

Implementation checklist:

- [x] Decide the exact timeout contract for `receivesFor` on REQ sockets.
- [x] Implement the missing REQ probe in polling.
- [x] Make sure probing does not lose or duplicate frames.
- [x] Reuse the REQ message buffer consistently.
- [x] Audit masking around any buffer writes or reads.
- [x] Verify behavior with relaxed/correlated REQ settings.

Acceptance criteria:

- [x] `Req.receivesFor timeout` returns `Right Nothing` when no valid reply arrives before timeout.
- [x] no blocking receive happens after polling has already decided the timeout expired.
- [x] buffered REQ replies are surfaced exactly once.

### 3. Repair the test harness so registered tests are actually runnable

Current state:

- [x] Problem identified: `test-recv-for` requires manual CLI arguments.
- [x] `test-recv-for` is now a real automated suite with no CLI dispatch.
- [x] No registered test suite currently requires mandatory CLI arguments.

Files:

- [x] `test/ReceivesFor.hs`
- [x] `test/test.cabal`

Implementation checklist:

- [x] Convert the registered `test-recv-for` suite into a real automated test.
- [x] Remove mandatory CLI dispatch from the automated suite.
- [x] Skip a separate manual executable unless a concrete exploratory need returns.
- [x] Remove infinite loops from automated tests.
- [x] Prefer deterministic local endpoints and bounded waits.

Acceptance criteria:

- [x] `cabal test test-recv-for` passes without extra arguments.
- [x] `cabal test all` no longer fails because a test expects user input.

### 4. Add real behavior tests for `withContext`

Current state:

- [x] Smoke coverage exists for `openWith`.
- [x] Explicit-context behavior is covered beyond socket creation for monitor setup.
- [x] Explicit-context behavior is covered beyond socket creation more broadly.

Files:

- [x] `test/ContextualOpen.hs`
- [x] `test/test.cabal`
- [x] `test/MonitorEvent.hs`

Implementation checklist:

- [x] Keep or replace the current socket-open smoke test.
- [x] Add an explicit-context test that exercises actual message flow.
- [x] Add an explicit-context test that exercises polling.
- [x] Add teardown coverage where sockets are left to finalizers and context shutdown still succeeds.
- [x] Cover at least one helper path that internally allocates resources, such as monitor setup.

Acceptance criteria:

- [x] explicit-context tests cover bind/connect/send/receive or equivalent real behavior.
- [x] explicit-context teardown is exercised automatically.

## Phase 2: Stabilize the Existing Hybrid Design

### 5. Compact or manage socket finalizer registries better

Current state:

- [x] Finalizer registry exists.
- [x] Registry reset/clear exists for `run` and `withContext`.
- [x] Finalizer registry is compacted or otherwise managed for long-lived runs.
- [x] Compaction restores live entries if interrupted mid-pass.

Files:

- [x] `lib/Zmqx/Core/Context.hs`
- [x] `lib/Zmqx/Core/SocketFinalizer.hs`
- [x] `test/FinalizerRegistry.hs`

Implementation checklist:

- [x] Decide whether compaction should be eager, lazy, or teardown-only.
- [x] Provide a helper to prune dead or already-run finalizers.
- [x] Keep registry mutation safe under concurrent socket creation.

Acceptance criteria:

- [x] long-lived runs do not accumulate stale finalizer entries without bound.
- [x] registry semantics remain idempotent and race-safe.

### 6. Add leaked-socket diagnostics before context termination

Current state:

- [x] Leak diagnostics helper exists.
- [x] `pendingSockets` is documented as advisory diagnostics, not an exact live-socket count.

Files:

- [x] `lib/Zmqx/Core/Context.hs`
- [x] `lib/Zmqx/Core/SocketFinalizer.hs`
- [x] `lib/Zmqx.hs`

Implementation checklist:

- [x] Define what counts as a leaked socket.
- [x] Decide whether diagnostics are always-on, debug-only, or opt-in.
- [x] Expose a helper that reports pending sockets or finalizers before terminate.
- [x] Ensure diagnostics do not keep sockets alive.

Acceptance criteria:

- [x] pending sockets can be inspected before shutdown.
- [x] diagnostics do not change socket lifetime behavior.

### 7. Decide whether `zmq_ctx_term` needs timeout or best-effort shutdown

Current state:

- [x] Teardown is bracketed and avoids the old double-terminate shape.
- [x] Timeout or best-effort shutdown policy is explicitly decided.
- [x] Shutdown docs distinguish strict teardown from delivery-preserving shutdown.

Files:

- [x] `lib/Zmqx/Core/Context.hs`
- [x] `README.md`
- [x] `docs/superpowers/specs/2026-04-01-shutdown-policy-design.md`

Implementation checklist:

- [x] Verify whether the current shutdown path is sufficient in practice.
- [x] Decide whether timeout belongs in options, a separate shutdown API, or debug tooling only.
- [x] Document the chosen semantics.

Acceptance criteria:

- [ ] a timeout or best-effort mode exists and is tested, or
- [x] the team explicitly decides not to add one on the main API path and documents why.

## Phase 3: Finish Plan A Items From `TODO.md`

### 8. Reconfirm `run` guard behavior with dedicated tests

Current state:

- [x] `RunAlreadyActive` guard exists.
- [x] `ContextNotInitialized` error exists.
- [x] Guard behavior is locked in by tests.

Files:

- [x] `test/RunGuard.hs`
- [x] `test/test.cabal`

Implementation checklist:

- [x] Add a test for nested `run`.
- [x] Add a test for concurrent `run`.
- [x] Assert `RunAlreadyActive` is thrown deterministically.
- [x] Add a test for using `open` outside `run`.

Acceptance criteria:

- [x] guard behavior is verified by automated tests.

### 9. Decide whether child-thread tracking belongs in Plan A

Current state:

- [x] Child-thread tracking policy is explicit.

Files:

- [x] `lib/Zmqx/Core/Context.hs`
- [x] `README.md`
- [x] `docs/superpowers/specs/2026-04-01-thread-lifetime-policy-design.md`

Implementation checklist:

- [x] Decide whether to add `forkZmqx` or `asyncZmqx`.
- [x] If no, document caller responsibility clearly.
- [x] Defer tracked child-thread helpers until there is stronger evidence they are worth the API cost.

Acceptance criteria:

- [x] thread lifetime semantics are explicit and documented.

## Phase 4: Continue Plan B Deliberately

### 10. Make the explicit-context surface complete

Current state:

- [x] `Context` exists.
- [x] `withContext` exists.
- [x] socket roles implement `ContextualOpen`.
- [x] No public helper silently falls back to globals in explicit-context mode.

Files:

- [x] `README.md`
- [x] `lib/Zmqx.hs`
- [x] `lib/Zmqx/*.hs`
- [x] `lib/Zmqx/Core/*`

Implementation checklist:

- [x] Audit all exported helpers for hidden global usage.
- [x] Ensure every helper that allocates internal sockets or resources has an explicit-context-safe path.
- [x] Make the README explicit about the remaining deliberate global entrypoints (`*.open`).

Acceptance criteria:

- [x] explicit-context mode is complete across the intended public surface.

### 11. Add `withSocket` or scoped socket helpers if they still earn their cost

Current state:

- [x] Decision made on scoped socket helpers.

Implementation checklist:

- [x] Decide whether `withSocket` adds real ergonomic value.
- [x] Prefer the current straight-line, context-scoped socket model for now.
- [x] Defer `withSocket` unless real usage shows repeated bracket boilerplate or socket-escape bugs.

Acceptance criteria:

- [x] no helper is added because it does not currently have a clear ergonomic win.

Files:

- [x] `README.md`
- [x] `docs/superpowers/specs/2026-04-01-with-socket-design.md`

### 12. Evaluate `ZmqxT` and `runZmqx` only after the explicit-context path is solid

Current state:

- [x] Decision made on `ZmqxT`.
- [x] `Zmqx.Monad` exists as a separate import path.
- [x] `runZmqx` reuses `withContext` rather than creating a second runtime path.
- [x] There is a clear ergonomic benefit for straight-line explicit-context code, not just architectural symmetry.

Files:

- [x] `lib/Zmqx/Monad.hs`
- [x] `zmqx.cabal`
- [x] `test/ZmqxMonad.hs`
- [x] `test/test.cabal`
- [x] `README.md`
- [x] `TODO.md`

Implementation checklist:

- [x] Do not start this before Phases 1 through 3 are stable.
- [x] Decide whether `ReaderT Context IO` materially improves ergonomics.
- [x] Define the migration story for current callers.
- [x] Keep the existing `run` compatibility path separate from `runZmqx`.

Acceptance criteria:

- [x] there is a clear ergonomic benefit, not just architectural symmetry.

### 13. Decide on region typing separately from the monad decision

Current state:

- [ ] Decision made on region typing.

Implementation checklist:

- [ ] Evaluate documented lifetime rules versus phantom-region typed sockets.
- [ ] Prefer documentation first unless escaping-socket bugs are common and costly.
- [ ] Introduce region typing only if it prevents real bugs seen in practice.

Acceptance criteria:

- [ ] the chosen lifetime model is explicit and documented.

### 14. Add transitional shims only after the target API is clear

Current state:

- [x] Some compatibility layering already exists.
- [ ] Compatibility surface is explicitly bounded.

Implementation checklist:

- [ ] Define the intended public end-state.
- [ ] Mark compatibility helpers clearly.
- [ ] Avoid duplicate entry points unless they ease a real migration.

Acceptance criteria:

- [ ] compatibility code has a clear purpose and does not obscure the preferred API.

## Phase 5: Documentation and Cleanup

### 15. Update `README.md` after each completed milestone

Checklist:

- [ ] Document the preferred API for new users.
- [ ] Document limitations of `run` versus `withContext`.
- [ ] Document any thread-lifetime caveats.
- [ ] Document any shutdown or leak diagnostics.
- [ ] Keep examples aligned with current recommended usage.

### 16. Keep `TODO.md` in sync as work lands

Checklist:

- [ ] Mark completed items explicitly.
- [ ] Split vague plan items into implementation-sized tasks when they become active work.
- [ ] Remove stale plan text once the preferred direction is decided.

## Suggested Execution Order

- [ ] 1. `monitor` explicit-context fix and monitor parser cleanup
- [ ] 2. REQ polling correctness
- [ ] 3. repair `test-recv-for`
- [ ] 4. strengthen `withContext` integration tests
- [ ] 5. finalizer registry compaction and leak diagnostics
- [ ] 6. decide shutdown timeout behavior
- [ ] 7. add `run` guard tests
- [ ] 8. decide child-thread tracking
- [ ] 9. complete the explicit-context API audit
- [ ] 10. evaluate optional ergonomic additions such as `withSocket`, `ZmqxT`, and region typing

## Definition Of Done For This Round

- [ ] no known correctness bug remains in the current public API
- [ ] no reachable runtime `undefined` remains
- [ ] automated tests cover both global and explicit-context modes
- [ ] `cabal test all` works without manual arguments
- [ ] `README.md` matches the actual supported behavior
