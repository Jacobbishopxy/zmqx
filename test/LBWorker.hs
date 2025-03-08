-- file: LBWorker.hs
-- author: Jacob Xie
-- date: 2025/03/08 00:01:58 Saturday
-- brief:
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception (throwIO)
import Control.Monad (forM_, forever)
import Data.ByteString.Char8 qualified as ByteString
import Data.Function ((&))
import Data.Text qualified as Text
import Ki
import Text.Printf (printf)
import Zmqx
import Zmqx.Req
import Zmqx.Router

data BrokerConfig = BrokerConfig
  { frontendAddr :: Text.Text,
    backendAddr :: Text.Text,
    nbrClients :: Int,
    nbrWorkers :: Int,
    clientTask :: Text.Text -> Int -> IO (),
    workerTask :: Text.Text -> Int -> IO (),
    frontendHandler :: [ByteString.ByteString] -> IO (Maybe FrontendAction),
    backendHandler :: [ByteString.ByteString] -> IO (Maybe BackendAction),
    clientMsgBuilder :: ClientMsgBuilder,
    workerMsgBuilder :: WorkerMsgBuilder
  }

type ClientMsgBuilder =
  -- client ID
  ByteString.ByteString ->
  -- client request ID
  ByteString.ByteString ->
  -- reply body
  ByteString.ByteString ->
  [ByteString.ByteString]

type WorkerMsgBuilder =
  -- worker ID
  ByteString.ByteString ->
  -- worker request ID
  ByteString.ByteString ->
  -- client ID
  ByteString.ByteString ->
  -- client request ID
  ByteString.ByteString ->
  -- request body
  ByteString.ByteString ->
  [ByteString.ByteString]

data FrontendAction
  = ClientRequest
      ByteString.ByteString -- Client ID
      ByteString.ByteString -- Client Request ID
      ByteString.ByteString -- Request body

data BackendAction
  = WorkerReady
      ByteString.ByteString -- Worker ID
      ByteString.ByteString -- Worker Request ID
  | WorkerReply
      ByteString.ByteString -- Worker ID
      ByteString.ByteString -- Worker Request ID
      ByteString.ByteString -- Client ID
      ByteString.ByteString -- Client Request ID
      ByteString.ByteString -- Reply body

type WorkerInfo = (ByteString.ByteString, ByteString.ByteString)

----------------------------------------------------------------------------------------------------

defaultBrokerConfig :: BrokerConfig
defaultBrokerConfig =
  BrokerConfig
    { frontendAddr = "ipc://frontend.ipc",
      backendAddr = "ipc://frontend.ipc",
      nbrClients = 10,
      nbrWorkers = 3,
      clientTask = defaultClientTask,
      workerTask = defaultWorkerTask,
      frontendHandler = defaultFrontendHandler,
      backendHandler = defaultBackendHandler,
      clientMsgBuilder = defaultClientMsgBuilder,
      workerMsgBuilder = defaultWorkerMsgBuilder
    }

defaultFrontendHandler :: [ByteString.ByteString] -> IO (Maybe FrontendAction)
defaultFrontendHandler = \case
  [clientId, reqId, request] ->
    pure $ Just $ ClientRequest clientId reqId request
  _ -> pure Nothing

defaultBackendHandler :: [ByteString.ByteString] -> IO (Maybe BackendAction)
defaultBackendHandler = \case
  [workerId, reqId, "READY"] ->
    pure $ Just $ WorkerReady workerId reqId
  [workerId, reqId, clientId, clientReqId, reply] ->
    pure $ Just $ WorkerReply workerId reqId clientId clientReqId reply
  _ -> pure Nothing

defaultClientMsgBuilder :: ClientMsgBuilder
defaultClientMsgBuilder clientId clientReqId reply =
  [clientId, clientReqId, reply]

defaultWorkerMsgBuilder :: WorkerMsgBuilder
defaultWorkerMsgBuilder workerId workerReqId clientId clientReqId request =
  [workerId, workerReqId, clientId, clientReqId, request]

defaultClientTask :: Text.Text -> Int -> IO ()
defaultClientTask addr nbr = do
  client <- unwrap (Zmqx.Req.open $ Zmqx.name clientName)
  unwrap (Zmqx.connect client addr)
  unwrap (Zmqx.send client "Hello")
  reply <- unwrap (Zmqx.receive client)
  printf "Client %d: %s\n" nbr (ByteString.unpack reply)
  where
    clientName = Text.pack $ "client-" <> show nbr

defaultWorkerTask :: Text.Text -> Int -> IO ()
defaultWorkerTask addr nbr = do
  worker <- unwrap (Zmqx.Req.open $ Zmqx.name workerName)
  unwrap (Zmqx.connect worker addr)
  unwrap (Zmqx.send worker "READY")
  forever do
    unwrap (Zmqx.receives worker) >>= \case
      [clientId, reqId, request] -> do
        printf "Worker %d: %s\n" nbr (ByteString.unpack request)
        unwrap (Zmqx.Req.sends worker [clientId, reqId, "OK"])
      _ -> pure ()
  where
    workerName = Text.pack $ "worker-" <> show nbr

----------------------------------------------------------------------------------------------------

runBroker :: BrokerConfig -> IO ()
runBroker config@BrokerConfig {..} =
  Zmqx.run Zmqx.defaultOptions do
    frontend <- unwrap (Zmqx.Router.open $ Zmqx.name "frontend")
    backend <- unwrap (Zmqx.Router.open $ Zmqx.name "backend")

    unwrap (Zmqx.bind frontend frontendAddr)
    unwrap (Zmqx.bind backend backendAddr)

    Ki.scoped $ \scope -> do
      forM_ [1 .. nbrClients] $ \n -> Ki.fork scope $ clientTask frontendAddr n
      forM_ [1 .. nbrWorkers] $ \n -> Ki.fork scope $ workerTask backendAddr n
      brokerLoop config frontend backend nbrClients []

-- main broker loop
brokerLoop :: BrokerConfig -> Zmqx.Router -> Zmqx.Router -> Int -> [WorkerInfo] -> IO ()
brokerLoop config frontend backend remainingClients workerQueue = do
  let pollItems = Zmqx.the backend & if not (null workerQueue) then Zmqx.also frontend else id

  Zmqx.poll pollItems >>= \case
    Left _ -> pure ()
    Right ready -> do
      (newClients, newQueue) <- handlePollResults ready remainingClients workerQueue
      brokerLoop config frontend backend newClients newQueue
  where
    handlePollResults :: Zmqx.Ready -> Int -> [WorkerInfo] -> IO (Int, [WorkerInfo])
    handlePollResults ready clientsLeft queue = do
      (clientsAfterBackend, queueAfterBackend) <- handleBackend config frontend backend ready queue clientsLeft
      handleFrontend config frontend backend ready clientsAfterBackend queueAfterBackend

-- frontend client message handling
handleFrontend :: BrokerConfig -> Zmqx.Router -> Zmqx.Router -> Zmqx.Ready -> Int -> [WorkerInfo] -> IO (Int, [WorkerInfo])
handleFrontend BrokerConfig {..} frontend backend (Zmqx.Ready ready) clientsLeft queue
  | clientsLeft <= 0 || not (ready frontend) = pure (clientsLeft, queue)
  | otherwise = case queue of
      [] -> pure (clientsLeft, queue)
      (workerId, workerReqId) : rest -> do
        msg <- unwrap (Zmqx.receives frontend)
        frontendHandler msg >>= \case
          Just (ClientRequest clientId clientReqId request) -> do
            let workerMsg = workerMsgBuilder workerId workerReqId clientId clientReqId request
            unwrap (Zmqx.sends backend workerMsg)
            pure (clientsLeft, rest)
          Nothing -> pure (clientsLeft, queue)

-- backend client message handling
handleBackend :: BrokerConfig -> Zmqx.Router -> Zmqx.Router -> Zmqx.Ready -> [WorkerInfo] -> Int -> IO (Int, [WorkerInfo])
handleBackend BrokerConfig {..} frontend backend (Zmqx.Ready ready) queue clientsLeft
  | not (ready backend) = pure (clientsLeft, queue)
  | otherwise = do
      msg <- unwrap (Zmqx.receives backend)
      backendHandler msg >>= \case
        Just (WorkerReady workerId reqId) ->
          pure (clientsLeft, (workerId, reqId) : queue)
        Just (WorkerReply workerId reqId clientId clientReqId reply) -> do
          let replyMsg = clientMsgBuilder clientId clientReqId reply
          unwrap (Zmqx.sends frontend replyMsg)
          pure (clientsLeft - 1, (workerId, reqId) : queue)
        Nothing -> pure (clientsLeft, queue)

----------------------------------------------------------------------------------------------------

unwrap :: IO (Either Zmqx.Error a) -> IO a
unwrap action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value

----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  let customConfig =
        defaultBrokerConfig
          { frontendAddr = "tcp://127.0.0.1:5555",
            backendAddr = "tcp://127.0.0.1:5556",
            nbrClients = 5
          }

  runBroker customConfig
