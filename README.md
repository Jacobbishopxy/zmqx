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

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
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

    _ <- forkIO do
      msg <- unwrap (Zmqx.receive rep)
      putStrLn ("server got: " <> BS.unpack msg)
      unwrap (Zmqx.send rep "pong")

    unwrap (Zmqx.send req "ping")
    reply <- unwrap (Zmqx.receive req)
    putStrLn ("client got: " <> BS.unpack reply)
```

### `Zmqx.withContext` (explicit context)

Use `Zmqx.withContext` when you want to avoid global state (e.g. embedding `zmqx` inside a larger app/library, or when you
need multiple isolated contexts). In this mode, open sockets via `openWith` (from `ContextualOpen`) instead of `*.open`.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
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

    _ <- forkIO do
      msg <- unwrap (Zmqx.receive rep)
      putStrLn ("server got: " <> BS.unpack msg)
      unwrap (Zmqx.send rep "pong")

    unwrap (Zmqx.send req "ping")
    reply <- unwrap (Zmqx.receive req)
    putStrLn ("client got: " <> BS.unpack reply)
```

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
