# Zmqx Shutdown Policy Design

**Date:** 2026-04-01

## Decision

`Zmqx.run` and `Zmqx.withContext` keep strict socket/context shutdown semantics.

On teardown they should:

1. call `zmq_ctx_shutdown`
2. run registered socket finalizers
3. wait for `zmq_ctx_term` to complete

The main API should not add a default timeout or best-effort escape hatch at this stage.

## Rationale

The current API is correctness-first. Returning from `run` or `withContext` before the context is
actually terminated would weaken the lifetime guarantees that the rest of the library now depends
on. A blocked shutdown is treated as a real bug in socket or thread lifetime management, not as
something the default API should silently paper over.

This guarantee is about closing sockets and terminating the context. It is not a promise that
queued outbound messages will be drained before return. The library sets `ZMQ_BLOCKY = 0` on new
contexts, which means new sockets inherit `ZMQ_LINGER = 0`.

This also keeps the public contract simple:

- `run`/`withContext` either finish after full teardown or block while teardown is still required
- callers can use `pendingSockets` in explicit-context mode to inspect whether sockets are still
  pending before shutdown
- `pendingSockets` is an advisory diagnostic count, not a precise live-socket census

## Deferred Work

If real operational need emerges, an opt-in timeout or best-effort shutdown path can be added
later. That should be a separate API path so callers must explicitly choose weaker guarantees.

Possible future directions:

- explicit shutdown options on a dedicated helper
- diagnostic-only timeout reporting without changing termination semantics
- richer leak reporting before shutdown

## Non-Goals

- changing the default semantics of `run`
- changing the default semantics of `withContext`
- introducing timeout-based shutdown without a distinct opt-in contract
