# Zmqx Thread Lifetime Policy Design

**Date:** 2026-04-01

## Decision

Plan A will not add `forkZmqx` or `asyncZmqx` at this stage.

Instead, the thread-lifetime contract is:

- callers may spawn child threads around `Zmqx.run` or `Zmqx.withContext`
- callers are responsible for joining or cancelling those threads before the enclosing `run` or
  `withContext` exits if the threads use that context or its sockets
- the library does not currently track `forkIO` children or automatically join/cancel them during
  teardown

## Rationale

The current Phase 1 and Phase 2 work hardened socket/context lifetime and shutdown semantics.
Adding tracked child-thread helpers now would reopen that design and force new policy decisions:

- whether child exceptions should kill the parent action
- whether shutdown should cancel children or wait for them
- how tracked helpers should compose across `run` and `withContext`
- whether `async`-style ergonomics are worth the public API cost in Plan A

That is a larger design step than this checklist item requires. The immediate need is to make the
current contract explicit so callers do not assume the library owns thread shutdown for them.

## Consequences

This keeps Plan A small and consistent with the existing direct-`IO` API, but it also means that
mismanaged child threads remain a real source of blocked teardown:

- if a child thread still owns sockets when `run` or `withContext` exits, shutdown may block
- this is treated as a caller-side lifetime bug, not something the main API should silently hide
- `pendingSockets` remains useful as advisory diagnostics when investigating such cases in
  explicit-context mode

## Deferred Work

If real usage shows that caller-managed threads are too error-prone, revisit this as a separate
API design task. Candidate future directions:

- `forkZmqx` scoped to `run`
- `asyncZmqx` or a tracked worker helper for `withContext`
- explicit child cancellation/join policy before context teardown

## Non-Goals

- adding tracked thread helpers to Plan A right now
- implicitly cancelling arbitrary caller threads during teardown
- changing shutdown semantics to mask thread lifetime bugs
