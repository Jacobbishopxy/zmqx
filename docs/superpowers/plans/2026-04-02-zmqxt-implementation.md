# ZmqxT Monadic API Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add the `Zmqx.Monad` effect-layer API with `ZmqxT`, `MonadZmqx`, `runZmqx`, and monadic wrappers while preserving the existing `withContext` runtime semantics.

**Architecture:** Keep `withContext` as the single lifecycle implementation. Build `Zmqx.Monad` as an additive module that carries `Context` through `ReaderT`, opens sockets from ambient context via a minimal `MonadZmqx` class, and wraps the stable top-level `Zmqx` operations instead of reimplementing socket behavior.

**Tech Stack:** Haskell 2024, Cabal, `mtl`, `transformers`, existing `zmqx` context/socket modules, plain executable tests in `test/`.

---

## File Map

- Create: `lib/Zmqx/Monad.hs`
  Responsibility: `ZmqxT`, `MonadZmqx`, `runZmqx`, `runZmqxT`, and monadic wrappers around stable public operations.
- Modify: `zmqx.cabal`
  Responsibility: expose `Zmqx.Monad` and add the direct library dependencies needed for `ReaderT`/`MonadReader` support.
- Create: `test/ZmqxMonad.hs`
  Responsibility: integration coverage for `runZmqx`, `runZmqxT`, ambient-context `open`, monadic wrappers, and generalized use through a custom `MonadZmqx` stack.
- Modify: `test/test.cabal`
  Responsibility: register the new `test-zmqxt` suite.
- Modify: `README.md`
  Responsibility: document the separate `Zmqx` vs `Zmqx.Monad` import paths and add a monadic quickstart example.
- Modify: `docs/checklist.md`
  Responsibility: close Phase 4 item 12 with the actual implementation/files/tests.
- Modify: `TODO.md`
  Responsibility: remove the stale claim that `Zmqx.run` should delegate to `runZmqx`; keep the approved split between the global API and `Zmqx.Monad`.

### Task 1: Add The Failing Core Monad Test

**Files:**
- Create: `test/ZmqxMonad.hs`
- Modify: `test/test.cabal`
- Test: `test/ZmqxMonad.hs`

- [ ] **Step 1: Write the failing core test and register the suite**

Add this executable and suite before creating `Zmqx.Monad`:

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Reader qualified as Reader
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Unique (hashUnique, newUnique)
import Zmqx qualified
import Zmqx.Monad qualified as ZmqxM
import Zmqx.Pair qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

uniqueEndpoint :: String -> IO Text
uniqueEndpoint label = do
  unique <- newUnique
  pure ("inproc://zmqxt-" <> Text.pack label <> "-" <> Text.pack (show (hashUnique unique)))

unwrapM :: (MonadIO m) => m (Either Zmqx.Error a) -> m a
unwrapM action = do
  result <- action
  liftIO (either throwIO pure result)

newtype App a = App {unApp :: ReaderT Text (ZmqxM.ZmqxT IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance ZmqxM.MonadZmqx App where
  askContext = App (lift ZmqxM.askContext)

runApp :: Text -> App a -> ZmqxM.ZmqxT IO a
runApp label (App action) =
  Reader.runReaderT action label

pairRoundTrip :: (MonadIO m) => Zmqx.Pair.Pair -> Zmqx.Pair.Pair -> Text -> m ()
pairRoundTrip server client endpoint = do
  liftIO (unwrap (Zmqx.bind server endpoint))
  liftIO (unwrap (Zmqx.connect client endpoint))
  liftIO (unwrap (Zmqx.send client "ping"))
  frame <- liftIO (unwrap (Zmqx.receive server))
  liftIO (assert (frame == "ping") "PAIR round trip returned the wrong payload")

main :: IO ()
main = do
  ZmqxM.runZmqx Zmqx.defaultOptions do
    server <- unwrapM (ZmqxM.open (Zmqx.Pair.defaultOptions <> Zmqx.name "run-zmqxt-server"))
    client <- unwrapM (ZmqxM.open (Zmqx.Pair.defaultOptions <> Zmqx.name "run-zmqxt-client"))
    endpoint <- liftIO (uniqueEndpoint "run-zmqxt")
    pairRoundTrip server client endpoint

  Zmqx.withContext Zmqx.defaultOptions \ctx ->
    ZmqxM.runZmqxT ctx do
      server <- unwrapM (ZmqxM.open (Zmqx.Pair.defaultOptions <> Zmqx.name "run-zmqxtt-server"))
      client <- unwrapM (ZmqxM.open (Zmqx.Pair.defaultOptions <> Zmqx.name "run-zmqxtt-client"))
      endpoint <- liftIO (uniqueEndpoint "run-zmqxtt")
      pairRoundTrip server client endpoint

  ZmqxM.runZmqx Zmqx.defaultOptions (runApp "stack" do
    server <- unwrapM (ZmqxM.open (Zmqx.Pair.defaultOptions <> Zmqx.name "app-server"))
    client <- unwrapM (ZmqxM.open (Zmqx.Pair.defaultOptions <> Zmqx.name "app-client"))
    label <- App Reader.ask
    endpoint <- liftIO (uniqueEndpoint (Text.unpack label))
    pairRoundTrip server client endpoint)
```

Register the suite in `test/test.cabal`:

```cabal
test-suite test-zmqxt
  import:  test-properties
  type:    exitcode-stdio-1.0
  main-is: ZmqxMonad.hs
```

- [ ] **Step 2: Run the new test to confirm it fails**

Run:

```bash
cabal test test-zmqxt
```

Expected: FAIL during build with `Could not find module 'Zmqx.Monad'`.

- [ ] **Step 3: Commit the failing-test scaffold**

```bash
git add test/ZmqxMonad.hs test/test.cabal
git commit -m "test: add failing ZmqxT core coverage"
```

### Task 2: Implement The Core `Zmqx.Monad` Runtime And Ambient `open`

**Files:**
- Create: `lib/Zmqx/Monad.hs`
- Modify: `zmqx.cabal`
- Test: `test/ZmqxMonad.hs`

- [ ] **Step 1: Expose the new module and add direct library dependencies**

Modify `zmqx.cabal`:

```cabal
library
  exposed-modules:
    Zmqx
    Zmqx.Dealer
    Zmqx.Monad
    Zmqx.Options
    Zmqx.Pair
    Zmqx.Pub
    Zmqx.Pull
    Zmqx.Push
    Zmqx.Rep
    Zmqx.Req
    Zmqx.Router
    Zmqx.Sub
    Zmqx.XPub
    Zmqx.XSub

  build-depends:
    , array
    , bytestring
    , containers
    , mtl
    , primitive
    , text
    , transformers
```

- [ ] **Step 2: Implement the core module with `ZmqxT`, `MonadZmqx`, runners, and `open`**

Create `lib/Zmqx/Monad.hs`:

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Zmqx.Monad
  ( ZmqxT (..),
    MonadZmqx (..),
    runZmqx,
    runZmqxT,
    open,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Zmqx qualified
import Zmqx.Core.Context (Context, ContextualOpen)
import Zmqx.Core.Options (Options)
import Zmqx.Error (Error)

newtype ZmqxT m a = ZmqxT {unZmqxT :: ReaderT Context m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Context)

instance MonadTrans ZmqxT where
  lift =
    ZmqxT . lift

class MonadIO m => MonadZmqx m where
  askContext :: m Context

instance MonadIO m => MonadZmqx (ZmqxT m) where
  askContext =
    ask

runZmqxT :: Context -> ZmqxT m a -> m a
runZmqxT context (ZmqxT action) =
  runReaderT action context

runZmqx :: Options () -> ZmqxT IO a -> IO a
runZmqx options action =
  Zmqx.withContext options \context ->
    runZmqxT context action

open :: (MonadZmqx m, ContextualOpen socket) => Options socket -> m (Either Error socket)
open options = do
  context <- askContext
  liftIO (Zmqx.openWith context options)
```

- [ ] **Step 3: Run the core test to verify it passes**

Run:

```bash
cabal test test-zmqxt
```

Expected: PASS. The suite should prove:
- `runZmqx` opens sockets from ambient context
- `runZmqxT` runs against an existing explicit `Context`
- a custom `MonadZmqx` stack can use `open`

- [ ] **Step 4: Commit the core runtime**

```bash
git add zmqx.cabal lib/Zmqx/Monad.hs
git commit -m "feat: add core ZmqxT runtime"
```

### Task 3: Add Failing Coverage For Monadic Wrappers

**Files:**
- Modify: `test/ZmqxMonad.hs`
- Test: `test/ZmqxMonad.hs`

- [ ] **Step 1: Extend the test to require monadic wrappers and context-safe helpers**

Replace the old `pairRoundTrip` helper and add a monitor/poll assertion block:

```haskell
pairRoundTrip :: (ZmqxM.MonadZmqx m) => Zmqx.Pair.Pair -> Zmqx.Pair.Pair -> Text -> m ()
pairRoundTrip server client endpoint = do
  unwrapM (ZmqxM.bind server endpoint)
  unwrapM (ZmqxM.connect client endpoint)

  readyBefore <- unwrapM (ZmqxM.pollFor (Zmqx.pollIn server) 20)
  liftIO (assert (readyBefore == Nothing) "PAIR polled ready before send")

  unwrapM (ZmqxM.send client "ping")

  readyAfter <- unwrapM (ZmqxM.pollFor (Zmqx.pollIn server) 1000)
  case readyAfter of
    Nothing ->
      liftIO (throwIO (userError "PAIR pollFor timed out after send"))
    Just (Zmqx.Ready isReady) ->
      liftIO (assert (isReady server) "PAIR server was not marked ready")

  frame <- unwrapM (ZmqxM.receive server)
  liftIO (assert (frame == "ping") "PAIR round trip returned the wrong payload")

main :: IO ()
main = do
  ZmqxM.runZmqx Zmqx.defaultOptions do
    dealer <- unwrapM (ZmqxM.open (Zmqx.Dealer.defaultOptions <> Zmqx.name "monad-monitor"))
    _ <- unwrapM (ZmqxM.monitor dealer)

    server <- unwrapM (ZmqxM.open (Zmqx.Pair.defaultOptions <> Zmqx.name "run-zmqxt-server"))
    client <- unwrapM (ZmqxM.open (Zmqx.Pair.defaultOptions <> Zmqx.name "run-zmqxt-client"))
    endpoint <- liftIO (uniqueEndpoint "run-zmqxt")
    pairRoundTrip server client endpoint

  Zmqx.withContext Zmqx.defaultOptions \ctx ->
    ZmqxM.runZmqxT ctx do
      dealer <- unwrapM (ZmqxM.open (Zmqx.Dealer.defaultOptions <> Zmqx.name "monad-monitor-explicit"))
      _ <- unwrapM (ZmqxM.monitor dealer)

      server <- unwrapM (ZmqxM.open (Zmqx.Pair.defaultOptions <> Zmqx.name "run-zmqxtt-server"))
      client <- unwrapM (ZmqxM.open (Zmqx.Pair.defaultOptions <> Zmqx.name "run-zmqxtt-client"))
      endpoint <- liftIO (uniqueEndpoint "run-zmqxtt")
      pairRoundTrip server client endpoint

  ZmqxM.runZmqx Zmqx.defaultOptions (runApp "stack" do
    server <- unwrapM (ZmqxM.open (Zmqx.Pair.defaultOptions <> Zmqx.name "app-server"))
    client <- unwrapM (ZmqxM.open (Zmqx.Pair.defaultOptions <> Zmqx.name "app-client"))
    label <- App Reader.ask
    endpoint <- liftIO (uniqueEndpoint (Text.unpack label))
    pairRoundTrip server client endpoint)
```

Also add the missing import:

```haskell
import Zmqx.Dealer qualified
```

- [ ] **Step 2: Run the test again to confirm it fails on missing wrappers**

Run:

```bash
cabal test test-zmqxt
```

Expected: FAIL with missing identifiers from `Zmqx.Monad`, such as `bind`, `connect`, `send`, `receive`, `pollFor`, or `monitor`.

- [ ] **Step 3: Commit the expanded failing test**

```bash
git add test/ZmqxMonad.hs
git commit -m "test: require monadic socket wrappers"
```

### Task 4: Implement The Monadic Wrapper Surface

**Files:**
- Modify: `lib/Zmqx/Monad.hs`
- Test: `test/ZmqxMonad.hs`

- [ ] **Step 1: Expand the export list and add wrappers over the stable top-level API**

Update `lib/Zmqx/Monad.hs` so it exposes the pure poll builders plus monadic wrappers:

```haskell
module Zmqx.Monad
  ( ZmqxT (..),
    MonadZmqx (..),
    runZmqx,
    runZmqxT,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    send,
    sends,
    receive,
    receives,
    receivesFor,
    monitor,
    Sockets,
    Ready (..),
    PollEvent (..),
    pollIn,
    pollInAlso,
    pollOut,
    pollOutAlso,
    poll,
    pollFor,
    pollUntil,
  )
where
```

Add these definitions:

```haskell
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word64)
import Zmqx (PollEvent (..), Ready (..), Sockets, pollIn, pollInAlso, pollOut, pollOutAlso)
import Zmqx.Core.Socket qualified as Socket

bind :: (MonadIO m) => Socket.Socket a -> Text -> m (Either Error ())
bind =
  liftIO . Zmqx.bind

unbind :: (MonadIO m) => Socket.Socket a -> Text -> m ()
unbind socket =
  liftIO . Zmqx.unbind socket

connect :: (MonadIO m) => Socket.Socket a -> Text -> m (Either Error ())
connect =
  liftIO . Zmqx.connect

disconnect :: (MonadIO m) => Socket.Socket a -> Text -> m ()
disconnect socket =
  liftIO . Zmqx.disconnect socket

send :: (MonadIO m, Socket.CanSend socket) => socket -> ByteString -> m (Either Error ())
send =
  liftIO . Zmqx.send

sends :: (MonadIO m, Socket.CanSends socket) => socket -> [ByteString] -> m (Either Error ())
sends =
  liftIO . Zmqx.sends

receive :: (MonadIO m, Socket.CanReceive socket) => socket -> m (Either Error ByteString)
receive =
  liftIO . Zmqx.receive

receives :: (MonadIO m, Socket.CanReceives socket) => socket -> m (Either Error [ByteString])
receives =
  liftIO . Zmqx.receives

receivesFor :: (MonadIO m, Socket.CanReceivesFor socket) => socket -> Int -> m (Either Error (Maybe [ByteString]))
receivesFor socket =
  liftIO . Zmqx.receivesFor socket

monitor :: (MonadIO m) => Socket.Socket a -> m (Either Error (IO (Either Error Zmqx.Event)))
monitor =
  liftIO . Zmqx.monitor

poll :: (MonadIO m) => Sockets -> m (Either Error Ready)
poll =
  liftIO . Zmqx.poll

pollFor :: (MonadIO m) => Sockets -> Int -> m (Either Error (Maybe Ready))
pollFor sockets =
  liftIO . Zmqx.pollFor sockets

pollUntil :: (MonadIO m) => Sockets -> Word64 -> m (Either Error (Maybe Ready))
pollUntil sockets =
  liftIO . Zmqx.pollUntil sockets
```

Do not reimplement socket behavior; these wrappers should stay thin adapters over the top-level API.

- [ ] **Step 2: Run the focused test and the nearest existing regression suite**

Run:

```bash
cabal test test-zmqxt test-contextual-open
```

Expected: PASS. `test-zmqxt` should now prove that the monadic layer shares the explicit-context behavior for open, helper allocation, polling, and message flow.

- [ ] **Step 3: Commit the wrapper surface**

```bash
git add lib/Zmqx/Monad.hs test/ZmqxMonad.hs
git commit -m "feat: add monadic Zmqx wrappers"
```

### Task 5: Sync Documentation And Roadmap State

**Files:**
- Modify: `README.md`
- Modify: `docs/checklist.md`
- Modify: `TODO.md`

- [ ] **Step 1: Document the new import path in `README.md`**

Add a new quickstart section after `withContext`:

```markdown
### `Zmqx.Monad` (effect-layer context)

Use `Zmqx.Monad` when you want the same explicit-context runtime semantics without threading
`Context` manually through application code. `runZmqx` allocates a context through
`Zmqx.withContext`, while `open` reads the ambient context from `ZmqxT`.
```

Use this example:

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
  ZmqxM.runZmqx Zmqx.defaultOptions do
    server <- unwrapM (ZmqxM.open (Zmqx.Pair.defaultOptions <> Zmqx.name "server"))
    client <- unwrapM (ZmqxM.open (Zmqx.Pair.defaultOptions <> Zmqx.name "client"))

    let endpoint = "inproc://ping"
    unwrapM (ZmqxM.bind server endpoint)
    unwrapM (ZmqxM.connect client endpoint)

    unwrapM (ZmqxM.send client "ping")
    msg <- unwrapM (ZmqxM.receive server)
    liftIO (putStrLn ("server got: " <> BS.unpack msg))
```

Keep the prose explicit that:
- `Zmqx` remains the direct `IO` API
- `Zmqx.Monad` is additive, not a replacement
- `runZmqx` shares `withContext` teardown semantics

- [ ] **Step 2: Close checklist item 12 and sync the remaining roadmap text**

Update `docs/checklist.md`:

```markdown
### 12. Evaluate `ZmqxT` and `runZmqx` only after the explicit-context path is solid

Current state:

- [x] Decision made on `ZmqxT`.
- [x] `Zmqx.Monad` exists as a separate import path.
- [x] `runZmqx` reuses `withContext` rather than creating a second runtime path.

Implementation checklist:

- [x] Do not start this before Phases 1 through 3 are stable.
- [x] Decide whether `ReaderT Context IO` materially improves ergonomics.
- [x] Define the migration story for current callers.
- [x] Keep `run` separate from the monadic path when compatibility requires globals.

Acceptance criteria:

- [x] there is a clear ergonomic benefit, not just architectural symmetry.

Files:

- [x] `lib/Zmqx/Monad.hs`
- [x] `zmqx.cabal`
- [x] `test/ZmqxMonad.hs`
- [x] `test/test.cabal`
- [x] `README.md`
- [x] `TODO.md`
```

Update `TODO.md` so the Plan B bullets match the approved design:

```markdown
- Offer `runZmqx :: Options () -> ZmqxT IO a -> IO a` for straight-line syntax; keep the existing
  `Zmqx.run` global wrapper separate for compatibility rather than delegating it through the
  monadic path.
- Update public modules with a separate `Zmqx.Monad` surface built around `Context` /
  `MonadZmqx`, and keep transitional overlap explicit.
```

- [ ] **Step 3: Run the final verification sweep**

Run:

```bash
cabal test test-zmqxt test-contextual-open test-run-guard test-monitor-event
```

Expected: PASS.

- [ ] **Step 4: Commit the docs and roadmap sync**

```bash
git add README.md docs/checklist.md TODO.md
git commit -m "docs: document the ZmqxT import path"
```
