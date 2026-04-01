{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Unique (hashUnique, newUnique)
import Zmqx
import Zmqx.Dealer qualified
import Zmqx.Pair qualified
import Zmqx.Pub qualified
import Zmqx.Pull qualified
import Zmqx.Push qualified
import Zmqx.Rep qualified
import Zmqx.Req qualified
import Zmqx.Router qualified
import Zmqx.Sub qualified
import Zmqx.XPub qualified
import Zmqx.XSub qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

uniqueEndpoint :: String -> IO Text
uniqueEndpoint label = do
  unique <- newUnique
  pure ("inproc://contextual-open-" <> Text.pack label <> "-" <> Text.pack (show (hashUnique unique)))

main :: IO ()
main = do
  withContext Zmqx.defaultOptions \ctx -> do
    -- Assert each socket role implements ContextualOpen under one explicit context.
    dealer <- unwrap (openWith ctx (Zmqx.Dealer.defaultOptions <> name "dealer"))
    pairServer <- unwrap (openWith ctx (Zmqx.Pair.defaultOptions <> name "pair-server"))
    pairClient <- unwrap (openWith ctx (Zmqx.Pair.defaultOptions <> name "pair-client"))
    _ <- unwrap (openWith ctx (Zmqx.Pub.defaultOptions <> name "pub"))
    _ <- unwrap (openWith ctx (Zmqx.Pull.defaultOptions <> name "pull"))
    _ <- unwrap (openWith ctx (Zmqx.Push.defaultOptions <> name "push"))
    _ <- unwrap (openWith ctx (Zmqx.Rep.defaultOptions <> name "rep"))
    _ <- unwrap (openWith ctx (Zmqx.Req.defaultOptions <> name "req"))
    _ <- unwrap (openWith ctx (Zmqx.Router.defaultOptions <> name "router"))
    _ <- unwrap (openWith ctx (Zmqx.Sub.defaultOptions <> name "sub"))
    _ <- unwrap (openWith ctx (Zmqx.XPub.defaultOptions <> name "xpub"))
    _ <- unwrap (openWith ctx (Zmqx.XSub.defaultOptions <> name "xsub"))

    -- Internal helper allocation must stay inside the explicit context too.
    _ <- unwrap (monitor dealer)

    endpoint <- uniqueEndpoint "pair"
    unwrap (bind pairServer endpoint)
    unwrap (connect pairClient endpoint)
    threadDelay 100000

    pollFor (pollIn pairServer) 20 >>= \case
      Right Nothing -> pure ()
      Right (Just _) -> throwIO (userError "Explicit-context PAIR unexpectedly polled ready before send")
      Left err -> throwIO err

    unwrap (send pairClient "context-message")

    pollFor (pollIn pairServer) 1000 >>= \case
      Right (Just (Ready isReady)) ->
        assert (isReady pairServer) "Explicit-context PAIR was not marked ready after send"
      Right Nothing ->
        throwIO (userError "Explicit-context PAIR timed out in pollFor after send")
      Left err -> throwIO err

    frame <- unwrap (receive pairServer)
    assert (frame == "context-message") "Explicit-context PAIR received the wrong payload"

  run Zmqx.defaultOptions do
    dealer <- unwrap (Zmqx.Dealer.open (name "run-monitor"))
    _ <- unwrap (monitor dealer)
    pure ()
