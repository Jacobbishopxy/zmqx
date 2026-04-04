# Zmqx Region Typing And Compatibility Boundary Design

**Date:** 2026-04-05

## Decision

Do not add phantom-region typed contexts or sockets at this stage.

The library will continue to prefer documented lifetime rules over a region-typed public API until
real escaping-socket bugs justify the added API cost.

The intended public end-state is:

- `Zmqx` remains the direct `IO` API
- `Zmqx.run` remains the guarded global-context entrypoint for `*.open`
- `Zmqx.withContext` plus `openWith` remains the explicit-context `IO` path
- `Zmqx.Monad` remains an additive ergonomic layer over `withContext`, not a replacement runtime
- compatibility helpers stay limited to preserving the current split rather than adding new
  duplicate entrypoints

## Rationale

Region typing would add compile-time protection against sockets escaping the lifetime of their
originating context, but that benefit is not currently tied to a demonstrated bug pattern in this
repository.

The public cost would be immediate and broad:

- context and socket types would gain extra type parameters
- more signatures would need to carry region variables through the public surface
- examples and migration paths would become heavier
- `Zmqx.Monad`, `ContextualOpen`, and compatibility layering would all become more complex at the
  same time

The rest of the roadmap has consistently preferred the smaller surface unless a stronger need is
proven:

- no `withSocket` helper was added
- no tracked `forkZmqx` / `asyncZmqx` helpers were added
- `Zmqx.Monad` was added as a narrow ergonomic layer over the existing runtime instead of opening a
  second lifecycle model

The same standard should apply here. Documentation-first lifetime rules are the cheaper and more
consistent choice unless real usage shows they are insufficient.

## Compatibility Boundary

The compatibility surface should be treated as intentionally bounded.

What stays:

- role modules continue to expose `*.open` for the existing global `run` path
- `Context`, `withContext`, and `openWith` remain the explicit-context foundation
- `Zmqx.Monad` continues to expose `ZmqxT`, `MonadZmqx`, `runZmqx`, `runZmqxT`, and monadic
  wrappers around the stable socket operations

What should not be added without a new demonstrated need:

- a second family of duplicate entrypoints that obscures which path is preferred
- compatibility shims that make the global and explicit-context styles harder to distinguish
- region-typed variants alongside the current first-class socket types

The preferred reading of the public API is:

- use `Zmqx.run` when the application wants one guarded global context and the existing `*.open`
  style
- use `Zmqx.withContext` when the caller needs explicit context ownership in direct `IO`
- use `Zmqx.Monad` when the caller wants explicit-context semantics with effect-layer ergonomics

## Consequences

This decision keeps the API split readable and keeps migration incremental, but it also leaves
socket lifetime discipline partly in documentation and caller behavior rather than enforcing it
entirely in the type system.

That trade-off is acceptable for now because:

- the current tests cover both the global and explicit-context paths
- the README already documents the lifetime and teardown model
- no concrete evidence in the current codebase shows that region typing would prevent a recurring
  class of bugs

## Deferred Work

Revisit region typing only if experience shows a repeated problem such as:

- sockets escaping their intended context lifetime in real code
- confusion between sockets from different contexts that type parameters would prevent
- repeated compatibility bugs that cannot be made clear through documentation alone

If that happens later, it should be designed as a distinct API revision rather than layered
piecemeal onto the current surface.

## Non-Goals

- adding `Context s` / `Socket s role` now
- introducing parallel region-typed and untyped public APIs
- changing `Zmqx.run` to delegate through `Zmqx.Monad`
- broadening compatibility helpers just for symmetry
