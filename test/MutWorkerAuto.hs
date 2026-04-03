{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Unique (hashUnique, newUnique)
import System.Timeout qualified as Timeout
import Zmqx
import Zmqx.Dealer qualified
import Zmqx.Rep qualified
import Zmqx.Req qualified
import Zmqx.Router qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

uniqueEndpoint :: String -> IO Text
uniqueEndpoint label = do
  unique <- newUnique
  pure ("inproc://mut-worker-auto-" <> Text.pack label <> "-" <> Text.pack (show (hashUnique unique)))

awaitConnection :: IO ()
awaitConnection =
  threadDelay 100000

expectFrames :: IO (Either Error (Maybe [ByteString])) -> String -> IO [ByteString]
expectFrames action message =
  action >>= \case
    Right (Just frames) -> pure frames
    Right Nothing -> throwIO (userError (message <> ": timed out"))
    Left err -> throwIO err

receiveWorkerRequest :: Rep -> Rep -> IO (String, ByteString, Rep)
receiveWorkerRequest worker1 worker2 =
  pollFor (pollIn worker1 & pollInAlso worker2) 1000 >>= \case
    Right (Just (Ready ready))
      | ready worker1 -> do
          request <- unwrap (receive worker1)
          pure ("mut-worker-1", request, worker1)
      | ready worker2 -> do
          request <- unwrap (receive worker2)
          pure ("mut-worker-2", request, worker2)
      | otherwise ->
          throwIO (userError "worker poll reported readiness without a ready worker")
    Right Nothing ->
      throwIO (userError "worker pool timed out waiting for a dispatched request")
    Left err ->
      throwIO err

testBody :: IO ()
testBody =
  withContext defaultOptions \ctx -> do
    frontendEndpoint <- uniqueEndpoint "frontend"
    backendEndpoint <- uniqueEndpoint "backend"
    clients <- unwrap (openWith ctx (Zmqx.Router.defaultOptions <> name "mut-clients"))
    workers <- unwrap (openWith ctx (Zmqx.Dealer.defaultOptions <> name "mut-workers"))
    client <- unwrap (openWith ctx (Zmqx.Req.defaultOptions <> name "mut-client"))
    worker1 <- unwrap (openWith ctx (Zmqx.Rep.defaultOptions <> name "mut-worker-1"))
    worker2 <- unwrap (openWith ctx (Zmqx.Rep.defaultOptions <> name "mut-worker-2"))

    unwrap (bind clients frontendEndpoint)
    unwrap (bind workers backendEndpoint)
    unwrap (connect client frontendEndpoint)
    unwrap (connect worker1 backendEndpoint)
    unwrap (connect worker2 backendEndpoint)
    awaitConnection

    handledBy <-
      traverse
        ( \request -> do
            unwrap (send client request)

            clientFrames <- expectFrames (receivesFor clients 1000) "queue proxy did not receive a client request"
            unwrap (sends workers clientFrames)

            (workerName, workerRequest, readyWorker) <- receiveWorkerRequest worker1 worker2
            assert (workerRequest == request) ("worker received the wrong request body: " <> show workerRequest)
            unwrap (send readyWorker ("handled:" <> request))

            workerFrames <- expectFrames (receivesFor workers 1000) "queue proxy did not receive a worker reply"
            unwrap (sends clients workerFrames)

            reply <- unwrap (receive client)
            assert (reply == "handled:" <> request) ("client received the wrong reply: " <> show reply)
            pure workerName
        )
        ["job-1", "job-2"]

    assert
      (sort handledBy == ["mut-worker-1", "mut-worker-2"])
      ("queue proxy did not reach both workers: " <> show handledBy)

main :: IO ()
main =
  Timeout.timeout 5000000 testBody >>= \case
    Just () -> pure ()
    Nothing -> throwIO (userError "MutWorkerAuto timed out")
