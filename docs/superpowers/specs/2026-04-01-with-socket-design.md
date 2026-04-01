# Zmqx Scoped Socket Helper Decision

**Date:** 2026-04-01

## Decision

Do not add `withSocket` or other scoped per-socket helpers at this stage.

The current API should continue to prefer:

- `Zmqx.run` for the single-context global path
- `Zmqx.withContext` plus `openWith` for explicit-context use
- straight-line socket usage within that enclosing context lifetime

## Rationale

The existing library design already leans away from per-socket bracketing:

- socket cleanup is handled by context-scoped finalizers and strict teardown
- the API style throughout the role modules is direct `open`/`openWith`, then ordinary socket
  operations
- Phase 3 explicitly documented caller responsibility for thread lifetime rather than moving Plan A
  toward a more managed runtime

Adding `withSocket` now would introduce another lifetime style without solving a demonstrated
problem. It would also risk muddying the preferred usage pattern by suggesting that per-socket
bracketing is the normal model, when the current design is explicitly context-scoped.

## When To Revisit

Revisit this only if real usage shows a repeated ergonomic problem such as:

- substantial bracket boilerplate around temporary sockets
- frequent socket-escape bugs that a scoped helper would materially reduce
- a clearer integration story with any future `ZmqxT` or region-typing decision

## Non-Goals

- adding a parallel socket-lifetime style just for architectural symmetry
- changing the current straight-line examples to favor nested per-socket bracketing
