{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Unique (hashUnique, newUnique)
import System.Timeout qualified as Timeout
import Zmqx
import Zmqx.Req qualified
import Zmqx.Router qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

type WorkerInfo = (ByteString, ByteString)
type ClientRequest = (ByteString, ByteString, ByteString)

uniqueEndpoint :: String -> IO Text
uniqueEndpoint label = do
  unique <- newUnique
  pure ("inproc://lb-worker-auto-" <> Text.pack label <> "-" <> Text.pack (show (hashUnique unique)))

awaitConnection :: IO ()
awaitConnection =
  threadDelay 100000

expectFrames :: IO (Either Error (Maybe [ByteString])) -> String -> IO [ByteString]
expectFrames action message =
  action >>= \case
    Right (Just frames) -> pure frames
    Right Nothing -> throwIO (userError (message <> ": timed out"))
    Left err -> throwIO err

decodeReady :: [ByteString] -> IO WorkerInfo
decodeReady = \case
  [workerId, workerReqId, "", "READY"] ->
    pure (workerId, workerReqId)
  frames ->
    throwIO (userError ("unexpected worker READY frames: " <> show frames))

dispatchClientRequest :: Router -> Router -> ByteString -> ByteString -> WorkerInfo -> IO ClientRequest
dispatchClientRequest frontend backend expectedClientId expectedRequest (workerId, workerReqId) =
  expectFrames (receivesFor frontend 1000) "broker frontend did not receive a client request" >>= \case
    [clientId, clientReqId, "", request] -> do
      assert (clientId == expectedClientId) ("frontend received the wrong client identity: " <> show clientId)
      assert (request == expectedRequest) ("frontend received the wrong client request body: " <> show request)
      unwrap (sends backend [workerId, workerReqId, "", clientId, clientReqId, "", request])
      pure (clientId, clientReqId, request)
    frames ->
      throwIO (userError ("unexpected frontend frames: " <> show frames))

receiveWorkerRequest :: Req -> ClientRequest -> IO ()
receiveWorkerRequest worker (expectedClientId, expectedClientReqId, expectedRequest) =
  expectFrames (receivesFor worker 1000) "worker did not receive its routed request" >>= \case
    [clientId, clientReqId, "", request] -> do
      assert (clientId == expectedClientId) ("worker received the wrong client identity: " <> show clientId)
      assert (clientReqId == expectedClientReqId) ("worker received the wrong client request id: " <> show clientReqId)
      assert (request == expectedRequest) ("worker received the wrong request body: " <> show request)
    frames ->
      throwIO (userError ("worker received the wrong routed frames: " <> show frames))

forwardWorkerReply :: Router -> Router -> IO ()
forwardWorkerReply frontend backend =
  expectFrames (receivesFor backend 1000) "broker backend did not receive a worker reply" >>= \case
    [_, _, "", clientId, clientReqId, "", reply] ->
      unwrap (sends frontend [clientId, clientReqId, "", reply])
    frames ->
      throwIO (userError ("unexpected backend reply frames: " <> show frames))

testBody :: IO ()
testBody =
  withContext defaultOptions \ctx -> do
    frontendEndpoint <- uniqueEndpoint "frontend"
    backendEndpoint <- uniqueEndpoint "backend"
    frontend <- unwrap (openWith ctx (Zmqx.Router.defaultOptions <> name "lb-frontend"))
    backend <- unwrap (openWith ctx (Zmqx.Router.defaultOptions <> name "lb-backend"))
    worker1 <- unwrap (openWith ctx (Zmqx.Req.defaultOptions <> name "lb-worker-1"))
    worker2 <- unwrap (openWith ctx (Zmqx.Req.defaultOptions <> name "lb-worker-2"))
    client1 <- unwrap (openWith ctx (Zmqx.Req.defaultOptions <> name "lb-client-1"))
    client2 <- unwrap (openWith ctx (Zmqx.Req.defaultOptions <> name "lb-client-2"))

    setSocketOpt worker1 (Z_RoutingId "worker-1")
    setSocketOpt worker2 (Z_RoutingId "worker-2")
    setSocketOpt client1 (Z_RoutingId "client-1")
    setSocketOpt client2 (Z_RoutingId "client-2")

    unwrap (bind frontend frontendEndpoint)
    unwrap (bind backend backendEndpoint)
    unwrap (connect worker1 backendEndpoint)
    unwrap (connect worker2 backendEndpoint)
    unwrap (connect client1 frontendEndpoint)
    unwrap (connect client2 frontendEndpoint)
    awaitConnection

    unwrap (send worker1 "READY")
    unwrap (send worker2 "READY")

    workerQueue <-
      traverse
        (const (expectFrames (receivesFor backend 1000) "broker backend did not receive worker readiness" >>= decodeReady))
        [1 :: Int, 2]

    ((workerId1, workerReqId1), (workerId2, workerReqId2)) <-
      case workerQueue of
        [workerInfo1, workerInfo2] ->
          pure (workerInfo1, workerInfo2)
        _ ->
          throwIO (userError ("expected exactly two ready workers, got " <> show workerQueue))

    unwrap (send client1 "hello-1")
    clientRequest1 <- dispatchClientRequest frontend backend "client-1" "hello-1" (workerId1, workerReqId1)
    if workerId1 == "worker-1"
      then do
        receiveWorkerRequest worker1 clientRequest1
        let (clientId, clientReqId, _) = clientRequest1
        unwrap (sends worker1 [clientId, clientReqId, "", "handled:hello-1"])
      else do
        receiveWorkerRequest worker2 clientRequest1
        let (clientId, clientReqId, _) = clientRequest1
        unwrap (sends worker2 [clientId, clientReqId, "", "handled:hello-1"])

    unwrap (send client2 "hello-2")
    clientRequest2 <- dispatchClientRequest frontend backend "client-2" "hello-2" (workerId2, workerReqId2)
    if workerId2 == "worker-1"
      then do
        receiveWorkerRequest worker1 clientRequest2
        let (clientId, clientReqId, _) = clientRequest2
        unwrap (sends worker1 [clientId, clientReqId, "", "handled:hello-2"])
      else do
        receiveWorkerRequest worker2 clientRequest2
        let (clientId, clientReqId, _) = clientRequest2
        unwrap (sends worker2 [clientId, clientReqId, "", "handled:hello-2"])

    forwardWorkerReply frontend backend
    forwardWorkerReply frontend backend

    replies <- traverse unwrap [receive client1, receive client2]
    assert
      (sort replies == sort ["handled:hello-1", "handled:hello-2"])
      ("load-balancing broker returned the wrong replies: " <> show replies)

main :: IO ()
main =
  Timeout.timeout 5000000 testBody >>= \case
    Just () -> pure ()
    Nothing -> throwIO (userError "LBWorkerAuto timed out")
