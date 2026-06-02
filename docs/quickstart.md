# Zmqx Quickstart

## Start Here

`zmqx` currently supports two user-facing styles plus an optional reactor helper:

- Direct API via `Zmqx`
- Monad-style API via `Zmqx.Monad`
- Optional event-loop reactor via `Zmqx.EventLoop`

All styles share the same socket/runtime behavior. The difference is how context lifetime and
socket ownership are expressed in your application code.

## Choosing An API Style

### Direct API

This means using the plain `IO` surface from `Zmqx`.

You can use it in two ways:

- `Zmqx.run` for the guarded global-context path
- `Zmqx.withContext` plus `openWith` when you want explicit context ownership in plain `IO`

Pros:

- simpler mental model
- less abstraction overhead
- explicit lifetime boundaries are visible in the call site
- easier to debug in small programs and one-off experiments

Cons:

- `withContext` requires manual `Context` plumbing
- larger applications can accumulate explicit-context boilerplate

Best fit:

- small programs
- examples and experiments
- callers who prefer explicit lifetime management in plain `IO`

### Monad Style API

This means using `Zmqx.Monad`, `ZmqxT`, `MonadZmqx`, `runZmqx`, and `runZmqxT`.

Pros:

- avoids manual `Context` threading through larger call chains
- fits transformer-based application code naturally
- keeps the same runtime semantics as `withContext`

Cons:

- adds an extra abstraction layer
- less direct for small scripts or debugging sessions
- should not be treated as a separate runtime or a replacement for `Zmqx`

Best fit:

- medium or large applications
- effect-stack based code
- callers who want explicit-context semantics with less plumbing

## Build And Test

```sh
# full automated test sweep
cabal test all

# targeted automated suite
cabal test test-req-poll

# optional demo/manual suite
cabal test test-simple-dealer --flag demo-tests --flag debug

# build
cabal build --flag debug
```

## Direct API: `Zmqx.run`

Use `Zmqx.run` for the typical one-context application shape. It creates one guarded global ØMQ
context, and the role-module `*.open` helpers use that context under the hood.

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

Use this when you want the shortest path and do not need to manage multiple contexts yourself.

## Direct API: `Zmqx.withContext`

Use `Zmqx.withContext` when you want explicit context ownership without leaving plain `IO`.

Once a socket is opened with `openWith`, the usual helper operations continue to use that socket's
originating context rather than silently falling back to the global `run` context.

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
  Zmqx.withContext Zmqx.defaultOptions \ctx -> do
    rep <- unwrap (Zmqx.openWith ctx (Zmqx.Rep.defaultOptions <> Zmqx.name "server"))
    req <- unwrap (Zmqx.openWith ctx (Zmqx.Req.defaultOptions <> Zmqx.name "client"))

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

Use this when you want direct control over context lifetime in plain `IO`.

## Monad Style: `Zmqx.Monad`

Use `Zmqx.Monad` when you want explicit-context semantics without threading `Context` manually.

`runZmqx` is implemented in terms of `withContext`, so this is an ergonomic layer over the same
runtime behavior rather than a second runtime path.

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

Use this when your application already benefits from an effect layer and you want less
context-plumbing noise.

## Optional Reactor: `Zmqx.EventLoop`

Use `Zmqx.EventLoop` when you want one worker thread to own registered sockets and expose named
sender, receiver, or transceiver endpoints. It is additive to the direct and monad APIs, not a
replacement runtime.

Supported MVP endpoint shapes are:

- `addSender` plus qualified `EventLoop.send` for single-frame outbound commands
- `addReceiver` with `EventLoop.Mailbox n` plus qualified `EventLoop.recv` for bounded multipart
  mailbox reads
- `addReceiver` with `EventLoop.Callback cb` for quick callbacks that run on the worker thread
- `addTransceiver` when one socket should both send single frames and receive multipart messages

Registered sockets are owned exclusively by the event-loop worker while the bracket is active. Bind
or connect them before starting the loop, then use `EventLoop.send`/`EventLoop.recv` instead of
touching the registered sockets directly.

### `Zmqx.run` plus `withEventLoop`

Use `withEventLoop` with sockets opened through the normal role-module `open` helpers inside
`Zmqx.run`.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (throwIO)
import Zmqx qualified
import Zmqx.EventLoop qualified as EventLoop
import Zmqx.Pull qualified
import Zmqx.Push qualified

unwrap :: Either Zmqx.Error a -> IO a
unwrap =
  either throwIO pure

main :: IO ()
main =
  Zmqx.run Zmqx.defaultOptions do
    pull <- unwrap (Zmqx.Pull.open (Zmqx.Pull.defaultOptions <> Zmqx.name "reactor-in"))
    push <- unwrap (Zmqx.Push.open (Zmqx.Push.defaultOptions <> Zmqx.name "reactor-out"))

    let endpoint = "inproc://event-loop"
    unwrap (Zmqx.bind pull endpoint)
    unwrap (Zmqx.connect push endpoint)

    let spec =
          Zmqx.addReceiver "inbox" pull (EventLoop.Mailbox 4)
            (Zmqx.addSender "outbox" push Zmqx.emptySpec)

    Zmqx.withEventLoop spec \loop -> do
      unwrap (EventLoop.send loop "outbox" "hello")
      received <- unwrap (EventLoop.recv loop "inbox" 1000)
      print received
```

### `Zmqx.withContext` plus `withEventLoopIn`

Use `withEventLoopIn` with sockets opened through `openWith` against the same explicit context.
This is the EventLoop form of the direct explicit-context style. With the same helper/import pattern
as above, plus `import Zmqx.Pair qualified`:

```haskell
Zmqx.withContext Zmqx.defaultOptions \ctx -> do
  pair <- unwrap (Zmqx.openWith ctx (Zmqx.Pair.defaultOptions <> Zmqx.name "reactor-pair"))
  peer <- unwrap (Zmqx.openWith ctx (Zmqx.Pair.defaultOptions <> Zmqx.name "reactor-peer"))

  let endpoint = "inproc://event-loop-explicit"
  unwrap (Zmqx.bind pair endpoint)
  unwrap (Zmqx.connect peer endpoint)

  let spec = Zmqx.addTransceiver "pair" pair (EventLoop.Mailbox 4) Zmqx.emptySpec
  Zmqx.withEventLoopIn ctx spec \loop -> do
    unwrap (Zmqx.sends peer ["from", "peer"])
    inbound <- unwrap (EventLoop.recv loop "pair" 1000)
    print inbound
    unwrap (EventLoop.send loop "pair" "reply")
```

For callbacks, replace `EventLoop.Mailbox n` with `EventLoop.Callback callback`; callbacks should
be quick and nonblocking because they run on the event-loop worker thread.

## Lifetime And Shutdown Notes

- `Zmqx.run` is guarded and must not be nested.
- `Zmqx.withContext` and `runZmqx` are explicit-context paths; they do not use the global `*.open`
  behavior.
- The main API does not currently provide `withSocket`, `forkZmqx`, or `asyncZmqx`.
- Caller-managed child threads must be joined or cancelled before `run` or `withContext` exits if
  they use that context or its sockets.
- Shutdown is strict: `zmq_ctx_shutdown`, registered socket finalizers, then `zmq_ctx_term`.
- `pendingSockets` is an advisory explicit-context diagnostic helper when investigating shutdown
  issues.
- The public API does not currently use phantom-region typed contexts or sockets such as
  `Context s` / `Socket s role`.

## Next Reading

- [Example index](./examples.md)
- [Current maintainer checklist](./checklist.md)
