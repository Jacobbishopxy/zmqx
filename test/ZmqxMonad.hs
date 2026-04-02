{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Reader qualified as Reader
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Unique (hashUnique, newUnique)
import Zmqx qualified
import Zmqx.Dealer qualified
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
  askContext = App (ReaderT (\_ -> ZmqxM.askContext))

runApp :: Text -> App a -> ZmqxM.ZmqxT IO a
runApp label (App action) =
  Reader.runReaderT action label

pairRoundTrip :: (MonadIO m) => Zmqx.Pair.Pair -> Zmqx.Pair.Pair -> Text -> m ()
pairRoundTrip server client endpoint = do
  unwrapM (ZmqxM.bind server endpoint)
  unwrapM (ZmqxM.connect client endpoint)

  readyBefore <- unwrapM (ZmqxM.pollFor (Zmqx.pollIn server) 20)
  liftIO (assert (isNothing readyBefore) "PAIR polled ready before send")

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

  Zmqx.withContext Zmqx.defaultOptions \ctx -> do
    (server, client, endpoint) <- ZmqxM.runZmqxT ctx do
      dealer <- unwrapM (ZmqxM.open (Zmqx.Dealer.defaultOptions <> Zmqx.name "monad-monitor-explicit"))
      _ <- unwrapM (ZmqxM.monitor dealer)

      server <- unwrapM (ZmqxM.open (Zmqx.Pair.defaultOptions <> Zmqx.name "run-zmqxtt-server"))
      client <- unwrapM (ZmqxM.open (Zmqx.Pair.defaultOptions <> Zmqx.name "run-zmqxtt-client"))
      endpoint <- liftIO (uniqueEndpoint "run-zmqxtt")
      pure (server, client, endpoint)

    pairRoundTrip server client endpoint

  ZmqxM.runZmqx Zmqx.defaultOptions (runApp "stack" do
    server <- unwrapM (ZmqxM.open (Zmqx.Pair.defaultOptions <> Zmqx.name "app-server"))
    client <- unwrapM (ZmqxM.open (Zmqx.Pair.defaultOptions <> Zmqx.name "app-client"))
    label <- App Reader.ask
    endpoint <- liftIO (uniqueEndpoint (Text.unpack label))
    pairRoundTrip server client endpoint)
