# ZmqxT Core Test Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Scaffold the failing `test-zmqxt` executable so we capture the desired `Zmqx.Monad` integration before the module exists.

**Architecture:** Add a dedicated test executable that exercises `runZmqx`, `runZmqxT`, and a custom `MonadZmqx` stack using PAIR sockets and unique `inproc://` endpoints, and register it in `test/test.cabal` so `cabal test test-zmqxt` builds and fails with the missing module.

**Tech Stack:** Haskell 2024, Cabal, `mtl`, the existing `zmqx` test helpers, `Zmqx.Pair` sockets.

---

### Task 1: Add failing core `ZmqxT` coverage

**Files:**
- Create: `test/ZmqxMonad.hs`
- Modify: `test/test.cabal`
- Test: `test/ZmqxMonad.hs`

- [ ] **Step 1: Write the failing test executable**

```haskell
#-# LANGUAGE GeneralizedNewtypeDeriving #-}
#-# LANGUAGE OverloadedStrings #-}

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

- [ ] **Step 2: Register the suite**

```cabal
test-suite test-zmqxt
  import:  test-properties
  type:    exitcode-stdio-1.0
  main-is: ZmqxMonad.hs
```

- [ ] **Step 3: Run the new test to confirm it fails**

Run: `cabal test test-zmqxt`
Expected: FAIL during build because `Zmqx.Monad` does not exist.

- [ ] **Step 4: Commit the failing-test scaffold**

```bash
git add test/ZmqxMonad.hs test/test.cabal
git commit -m "test: add failing ZmqxT core coverage"
```
