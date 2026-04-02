# Zmqx

Copied from:

- <https://github.com/awkward-squad/libzmq>
- <https://github.com/mitchellwrosen/zmq>

Support `base ^>=4.20.0.0`

## Usage

```cabal
# cabal.project

source-repository-package
  type: git
  location: git@github.com:Jacobbishopxy/zmqx.git
  tag: 35338d457593b9d9200c256edf93bfe2a370cf3a
```

## Quickstart

### `Zmqx.run` (global context)

Use `Zmqx.run` for typical applications: it creates a single global ØMQ context, and all `*.open` functions use that
context under the hood. `Zmqx.run` is guarded and must not be nested (it throws `RunAlreadyActive`).
If you spawn child threads that use `zmqx` sockets inside `run`, you must stop and join them
before `run` returns; the Plan A API does not track `forkIO` children for you.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (throwIO)
import Data.ByteString.Char8 qualified as BS
import Zmqx qualified
import Zmqx.Rep qualified
import Zmqx.Req qualified

unwrap :: Either Zmqx.Error a -> IO a
unwrap =
  either throwIO pure

main :: IO ()
main =
  Zmqx.run Zmqx.defaultOptions do
    rep <- unwrap (Zmqx.Rep.open (Zmqx.name "server"))
    req <- unwrap (Zmqx.Req.open (Zmqx.name "client"))

    let endpoint = "inproc://ping"
    unwrap (Zmqx.bind rep endpoint)
    unwrap (Zmqx.connect req endpoint)

    unwrap (Zmqx.send req "ping")
    msg <- unwrap (Zmqx.receive rep)
    putStrLn ("server got: " <> BS.unpack msg)
    unwrap (Zmqx.send rep "pong")
    reply <- unwrap (Zmqx.receive req)
    putStrLn ("client got: " <> BS.unpack reply)
```

### `Zmqx.withContext` (explicit context)

Use `Zmqx.withContext` when you want to avoid global state (e.g. embedding `zmqx` inside a larger app/library, or when you
need multiple isolated contexts). In this mode, open sockets via `openWith` (from `ContextualOpen`) instead of `*.open`.
As with `run`, any child threads that use the context or its sockets must be joined or cancelled by
the caller before `withContext` exits.

The explicit-context path is intended to be complete across the public helper surface: once you
have a socket opened with `openWith`, the usual helper operations (`bind`, `connect`, `send`,
`receive`, `pollFor`, `monitor`, and so on) stay on that socket's originating context rather than
silently falling back to the global `run` context. The remaining deliberate global entrypoints are
the `*.open` functions themselves.

There is no `withSocket` helper on the public API today. That is deliberate: the library currently
prefers straight-line, context-scoped socket lifetime over per-socket bracketing.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (throwIO)
import Data.ByteString.Char8 qualified as BS
import Zmqx qualified
import Zmqx.Rep
import Zmqx.Req

unwrap :: Either Zmqx.Error a -> IO a
unwrap =
  either throwIO pure

main :: IO ()
main =
  Zmqx.withContext Zmqx.defaultOptions \ctx -> do
    rep <- unwrap (Zmqx.openWith ctx (Zmqx.Rep.defaultOptions <> name "server"))
    req <- unwrap (Zmqx.openWith ctx (Zmqx.Req.defaultOptions <> name "client"))

    let endpoint = "inproc://ping"
    unwrap (Zmqx.bind rep endpoint)
    unwrap (Zmqx.connect req endpoint)

    unwrap (Zmqx.send req "ping")
    msg <- unwrap (Zmqx.receive rep)
    putStrLn ("server got: " <> BS.unpack msg)
    unwrap (Zmqx.send rep "pong")
    reply <- unwrap (Zmqx.receive req)
    putStrLn ("client got: " <> BS.unpack reply)
```

### `Zmqx.Monad` (monadic explicit context)

The `Zmqx` module remains the direct `IO` API. `Zmqx.Monad` is additive, not a replacement: it
provides a `MonadZmqx` / `ZmqxT` layer for callers that want straight-line explicit-context code
without manually threading `Context`, while the existing `run` and `withContext` entrypoints stay
available. `runZmqx` reuses `withContext`, so it shares the same teardown semantics rather than
introducing a second runtime path.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 qualified as BS
import Zmqx qualified
import Zmqx.Monad qualified as ZmqxM
import Zmqx.Pair qualified

unwrapM :: ZmqxM.ZmqxT IO (Either Zmqx.Error a) -> ZmqxM.ZmqxT IO a
unwrapM action = do
  result <- action
  either (liftIO . throwIO) pure result

main :: IO ()
main =
  ZmqxM.runZmqx Zmqx.defaultOptions $ do
    server <- unwrapM (ZmqxM.open (Zmqx.Pair.defaultOptions <> Zmqx.name "server"))
    client <- unwrapM (ZmqxM.open (Zmqx.Pair.defaultOptions <> Zmqx.name "client"))

    let endpoint = "inproc://ping"
    unwrapM (ZmqxM.bind server endpoint)
    unwrapM (ZmqxM.connect client endpoint)

    unwrapM (ZmqxM.send client "ping")
    msg <- unwrapM (ZmqxM.receive server)
    liftIO (putStrLn ("server got: " <> BS.unpack msg))
```

### Shutdown Semantics

`Zmqx.run` and `Zmqx.withContext` are strict about socket and context teardown. On exit they:

1. call `zmq_ctx_shutdown`
2. run registered socket finalizers
3. wait for `zmq_ctx_term` to complete

This is intentional. The default API is correctness-first and does not silently abandon stuck
cleanup. If shutdown blocks, treat that as a real lifetime bug to investigate. Use
`Zmqx.pendingSockets` with an explicit `Context` to inspect whether sockets are still pending
before teardown; treat that count as advisory diagnostics, not as an exact live-socket census.

The same rule applies to caller-managed child threads: `zmqx` does not currently provide
`forkZmqx` or `asyncZmqx` on the main API path. If a child thread still owns sockets when
`run` or `withContext` exits, teardown can legitimately block until that thread releases them.
Join or cancel such threads before returning from the enclosing context.

This does not promise delivery-preserving shutdown for queued outbound messages. Contexts are
created with `ZMQ_BLOCKY = 0`, so new sockets default to `ZMQ_LINGER = 0`.

There is no timeout or best-effort shutdown mode on the main API today. If one is added later,
it will be opt-in rather than changing the default semantics of `run` or `withContext`.

## Test & Build

```sh
# test
cabal test test-simple-dealer --flag debug

# build
cabal build --flag debug
```

## Example

- [Simple Req](./test/SimpleReq.hs) + [Simple Rep](./test/SimpleRep.hs)

- [Simple Dealer](./test/SimpleDealer.hs) + [Simple Router](./test/SimpleRouter.hs)

- [Simple Pub](./test/SimplePub.hs) + [Simple Sub](./test/SimpleSub.hs)

- [Task Ventilator (Push)](./test/TaskVentilator.hs) -> [Task Worker (Pull + Push)](./test/TaskWorker.hs) -> [Task Sink (Pull)](./test/TaskSink.hs)

- [Zmq items poll](./test/ItemsPoll.hs)

- [Simple Proxy (XPub + XSub)](./test/SimpleProxy.hs)

- [Multiple workers (Router + Dealer + Rep)](./test/MutWorker.hs)

- [Load-balance worker](./test/LBWorker.hs)

## Not implemented

- Timers API

  - zmq_timers_new/destroy create/destroy a timer scheduler object (handles multiple timers; call destroy with pointer-to-pointer).
  - zmq_timers_add registers a handler callback run every interval milliseconds; returns a timer id.
  - zmq_timers_cancel removes a timer by id.
  - zmq_timers_set_interval changes an existing timer’s interval.
  - zmq_timers_reset restarts a timer’s countdown.
  - zmq_timers_timeout reports milliseconds until the next timer should fire (or -1 if none) so you can integrate with your own event loop.
  - zmq_timers_execute runs due timers’ callbacks; you typically poll timeout and then call execute when appropriate.

- Utility helpers

  - zmq_stopwatch_start/intermediate/stop lightweight wall-clock stopwatch in microseconds; intermediate reads elapsed so far, stop reads and frees.
  - zmq_sleep simple whole-second sleep helper.
  - zmq_threadstart/threadclose starts a native thread running a zmq_thread_fn with a void* argument and later joins/frees it via threadclose; intended for quick utility threads
    without pulling in a threading lib.
