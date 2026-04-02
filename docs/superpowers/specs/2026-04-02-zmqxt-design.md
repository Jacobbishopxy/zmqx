# ZmqxT And Monadic API Design

**Date:** 2026-04-02

## Decision

Add a separate monadic API under `Zmqx.Monad`.

This is an additive Plan B surface, not a replacement for the current explicit-`IO` API:

- `Zmqx` remains the direct API for `run`, `withContext`, `Context`, and `openWith`
- `Zmqx.Monad` provides `ZmqxT`, `runZmqx`, a minimal `MonadZmqx` class, and monadic wrappers
  around the stable public socket operations
- `runZmqx` is implemented in terms of `withContext`, so the monadic path shares the same context
  lifecycle, teardown policy, socket finalizer behavior, and diagnostics as the explicit-context
  API

## Why This Earns Its Cost

The explicit-context path is now stable and complete enough to serve as the single runtime
foundation. That makes it reasonable to add a second import path focused on ergonomics rather than
correctness fixes.

The ergonomic benefit is concrete:

- callers no longer need to thread `Context` manually through larger application flows
- monadic code can open sockets from ambient context rather than plumbing `ctx` through helper
  layers
- applications already using transformer stacks can integrate `zmqx` as an effect layer rather
  than dropping back to plain `IO` at every socket open

The design still keeps intent obvious at the import level:

- `Zmqx` means explicit lifetime and plain `IO`
- `Zmqx.Monad` means context carried by an effect layer

## API Shape

### Modules

- keep `Zmqx` unchanged as the explicit `IO` API
- add `Zmqx.Monad` as the monadic surface

This import split is intentional. Users should be able to tell which style they are adopting from
the module path alone.

### Core Types

`Zmqx.Monad` should introduce a reader-style transformer and a minimal class:

```haskell
newtype ZmqxT m a = ZmqxT { unZmqxT :: ReaderT Context m a }

class MonadIO m => MonadZmqx m where
  askContext :: m Context
```

Required instances:

- `MonadIO m => MonadZmqx (ZmqxT m)`
- standard transformer instances for `Functor`, `Applicative`, `Monad`, `MonadIO`,
  `MonadTrans`, and `MonadReader Context`

The class stays intentionally minimal. It only provides access to a `Context`; it does not make
socket operations into typeclass methods.

### Runtime Entry

```haskell
runZmqx :: Options () -> ZmqxT IO a -> IO a
runZmqxT :: Context -> ZmqxT m a -> m a
```

Semantics:

- `runZmqx` allocates a context via `withContext`
- the allocated `Context` is fed into `ZmqxT`
- all teardown behavior is inherited from `withContext`
- `runZmqxT` is the pure runner for an already-allocated `Context`

This keeps one runtime path in the library. `runZmqx` must not become a second independently
implemented context lifecycle.

`Zmqx.run` should remain separate from this path. The existing global-context API still exists for
`*.open` compatibility, so it cannot be mechanically redefined as a thin wrapper around
`runZmqx` without changing its semantics.

### Operation Surface

`Zmqx.Monad` should expose monadic versions of the existing public operations as ordinary free
functions with constraints.

Two broad categories:

1. Context-dependent operations require `MonadZmqx`

```haskell
open :: (MonadZmqx m, ContextualOpen socket) => Options socket -> m (Either Error socket)
```

2. Operations on an already-open socket usually only require `MonadIO`

```haskell
bind :: MonadIO m => socket -> Text -> m (Either Error ())
connect :: MonadIO m => socket -> Text -> m (Either Error ())
send :: (MonadIO m, CanSend socket) => socket -> ByteString -> m (Either Error ())
receive :: (MonadIO m, CanReceive socket) => socket -> m (Either Error ByteString)
```

This yields a full monadic interface without making the class itself large or hard to evolve.
In particular, the class should not grow just because existing socket values can already carry the
context they need internally.

## Compatibility And Migration

This design is additive.

Existing callers:

- can stay on `Zmqx`
- do not need source changes
- keep the current explicit `IO` and explicit-context APIs

New callers:

- can choose `Zmqx.Monad` if they want effect-layer ergonomics
- can still drop to `Zmqx.withContext` when they want direct control over explicit context
  lifetime

Application stacks:

- can use `ZmqxT IO` directly
- can stack their own transformers around `ZmqxT IO`
- can provide a custom `MonadZmqx` instance if their application monad can supply a `Context`

The migration story should stay incremental. The library should not try to push current `Zmqx`
users onto `Zmqx.Monad`.

## Error Semantics

Do not change error style as part of this design.

Monadic wrappers should continue returning `Either Error ...`, matching the current `IO` surface.
Changing both context-passing style and failure style at the same time would make migration and
debugging unnecessarily harder.

## Deferred Work

This decision does not include:

- region typing
- a `withSocket` helper
- tracked child-thread helpers
- a broad `MonadZmqx` class with socket operations as methods
- changing `Zmqx.run` to delegate through the monadic layer

Each of those should remain a separate roadmap item.

## Risks

Main costs and risks:

- public API size increases because the library now documents two styles
- a public `MonadZmqx` class is sticky and must remain minimal to stay maintainable
- docs and examples must keep the import-path split clear so users do not confuse the two modes
- tests must prove the monadic layer behaves the same as the explicit-context layer

## Implementation Direction

When implementation starts, keep it narrow:

1. add `Zmqx.Monad` with `ZmqxT`, `runZmqx`, `MonadZmqx`, and `open`
2. add monadic wrappers for the stable public operations already covered by Phase 1 through Phase 4
3. add cross-check tests that exercise the same scenario under `withContext` and `runZmqx`
4. update README examples to show both import paths clearly

## Acceptance Criteria

This design should only move forward if implementation can show:

- a clear call-site ergonomics improvement over manual `Context` threading
- no new runtime/lifecycle semantics relative to `withContext`
- an obvious import-path split between explicit `IO` and monadic styles
- a minimal `MonadZmqx` class that carries only `Context` access

## Non-Goals

- replacing the existing `Zmqx` API
- making `MonadZmqx` a large effect class
- introducing a second context runtime beside `withContext`
- using architectural symmetry alone as the justification for `ZmqxT`
