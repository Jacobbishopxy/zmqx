{-# LANGUAGE RecordWildCards #-}

-- file: LBWorker.hs
-- author: Jacob Xie
-- date: 2025/03/08 00:01:58 Saturday
-- brief: A load-balancing worker system using ZeroMQ for message passing between clients and workers.

module Main where

import Common (AppMonad, Logger, createLogger, logErrorR, logInfoM, logInfoR)
import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throwIO)
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ask, liftIO, runReaderT)
import Data.ByteString.Char8 qualified as ByteString
import Data.Function ((&))
import Data.Text qualified as Text
import Ki
import Text.Printf (printf)
import Zmqx
import Zmqx.Req
import Zmqx.Router

data BrokerConfig = BrokerConfig
  { frontendAddr :: Text.Text, -- Address for client-facing communication
    backendAddr :: Text.Text, -- Address for worker-facing communication
    nbrClients :: Int, -- Number of clients to simulate
    nbrWorkers :: Int, -- Number of workers to simulate
    clientTask :: Text.Text -> Int -> AppMonad (), -- Task for each client
    workerTask :: Text.Text -> Int -> AppMonad (), -- Task for each worker
    decodeClientMsg :: [ByteString.ByteString] -> Maybe ClientMessage,
    decodeWorkerMsg :: [ByteString.ByteString] -> Maybe WorkerMessage,
    encodeClientMsg :: ClientMessage -> [ByteString.ByteString],
    encodeWorkerMsg :: WorkerMessage -> [ByteString.ByteString]
  }

data ClientMessage
  = ClientRequest
  { requestClientId :: ByteString.ByteString,
    requestClientReqId :: ByteString.ByteString,
    requestBody :: ByteString.ByteString
  }

data WorkerMessage
  = WorkerReady
      { workerId :: ByteString.ByteString,
        workerReqId :: ByteString.ByteString
      }
  | WorkerReply
      { workerId :: ByteString.ByteString,
        workerReqId :: ByteString.ByteString,
        replyClientId :: ByteString.ByteString,
        replyClientReqId :: ByteString.ByteString,
        replyBody :: ByteString.ByteString
      }

-- Worker information tuple: (Worker ID, Request ID)
type WorkerInfo = (ByteString.ByteString, ByteString.ByteString)

----------------------------------------------------------------------------------------------------
-- Message decoding and encoding functions for ZeroMQ communication.
-- Notice: Req + Router message pattern leads empty frame

decodeClientMessage :: [ByteString.ByteString] -> Maybe ClientMessage
decodeClientMessage = \case
  [clientId, clientReqId, "", requestBody] ->
    Just $ ClientRequest clientId clientReqId requestBody
  _ -> Nothing

encodeClientMessage :: ClientMessage -> [ByteString.ByteString]
encodeClientMessage ClientRequest {..} =
  [requestClientId, requestClientReqId, "", requestBody]

decodeWorkerMessage :: [ByteString.ByteString] -> Maybe WorkerMessage
decodeWorkerMessage = \case
  [workerId, workerReqId, "", "READY"] ->
    Just $ WorkerReady workerId workerReqId
  [workerId, workerReqId, "", clientId, clientReqId, "", replyBody] ->
    Just $ WorkerReply workerId workerReqId clientId clientReqId replyBody
  _ -> Nothing

encodeWorkerMessage :: WorkerMessage -> [ByteString.ByteString]
encodeWorkerMessage WorkerReady {..} =
  [workerId, workerReqId, "", "READY"]
encodeWorkerMessage WorkerReply {..} =
  [workerId, workerReqId, "", replyClientId, replyClientReqId, "", replyBody]

----------------------------------------------------------------------------------------------------

defaultBrokerConfig :: BrokerConfig
defaultBrokerConfig =
  BrokerConfig
    { frontendAddr = "ipc://frontend.ipc",
      backendAddr = "ipc://backend.ipc",
      nbrClients = 10,
      nbrWorkers = 3,
      clientTask = defaultClientTask,
      workerTask = defaultWorkerTask,
      decodeClientMsg = decodeClientMessage,
      decodeWorkerMsg = decodeWorkerMessage,
      encodeClientMsg = encodeClientMessage,
      encodeWorkerMsg = encodeWorkerMessage
    }

-- default task for a client. In a production env, clients are other server.
defaultClientTask :: Text.Text -> Int -> AppMonad ()
defaultClientTask addr nbr = do
  client <- zmqUnwrap (Zmqx.Req.open $ Zmqx.name clientName)
  liftIO $ Zmqx.setSocketOpt client (Zmqx.Z_RoutingId clientName')
  zmqUnwrap (Zmqx.connect client addr)

  logInfoR $ printf "Client %d: start..." nbr

  zmqUnwrap (Zmqx.send client "Hello")
  reply <- zmqUnwrap (Zmqx.receive client)

  logInfoR $ printf "Client %d received: %s" nbr (ByteString.unpack reply)
  where
    clientName = Text.pack $ "client-" <> show nbr
    clientName' = ByteString.pack $ "client-" <> show nbr

-- default task for a worker. In a production env, clients are other server.
defaultWorkerTask :: Text.Text -> Int -> AppMonad ()
defaultWorkerTask addr nbr = do
  logger <- ask
  worker <- zmqUnwrap (Zmqx.Req.open $ Zmqx.name workerName)
  liftIO $ Zmqx.setSocketOpt worker (Zmqx.Z_RoutingId workerName')
  zmqUnwrap (Zmqx.connect worker addr)

  logInfoR $ printf "Worker %d: start..." nbr

  zmqUnwrap (Zmqx.send worker "READY")
  logInfoR $ printf "Worker %d: sent READY" nbr

  forever do
    zmqUnwrap (Zmqx.receives worker) >>= \case
      [clientId, reqId, "", request] -> do
        -- received WorkerReply, then send "OK" to backend
        zmqUnwrap (Zmqx.sends worker [clientId, reqId, "", "OK"])
        logInfoM logger $ printf "Worker %d received then sent: %s" nbr (ByteString.unpack request)
      a -> do
        logInfoM logger $ printf "Unexpected worker msg!!! len: %d" (length a)
        pure ()
  where
    workerName = Text.pack $ "worker-" <> show nbr
    workerName' = ByteString.pack $ "worker-" <> show nbr

----------------------------------------------------------------------------------------------------
-- main function

runBroker :: BrokerConfig -> AppMonad ()
runBroker config@BrokerConfig {..} = do
  logger <- ask
  liftIO $ Zmqx.run Zmqx.defaultOptions do
    frontend <- zmqUnwrap' (Zmqx.Router.open $ Zmqx.name "frontend")
    backend <- zmqUnwrap' (Zmqx.Router.open $ Zmqx.name "backend")
    zmqUnwrap' (Zmqx.bind frontend frontendAddr)
    zmqUnwrap' (Zmqx.bind backend backendAddr)

    -- wait for connections to stabilize
    threadDelay 1_000_000

    -- start clients and workers in separate threads
    liftIO $ Ki.scoped $ \scope -> do
      -- mock clients
      forM_ [1 .. nbrClients] $ Ki.fork scope . flip runReaderT logger . clientTask frontendAddr
      -- mock workers
      forM_ [1 .. nbrWorkers] $ Ki.fork scope . flip runReaderT logger . workerTask backendAddr
      -- main event loop
      runReaderT (brokerLoop config frontend backend nbrClients []) logger

-- main broker loop
brokerLoop :: BrokerConfig -> Zmqx.Router -> Zmqx.Router -> Int -> [WorkerInfo] -> AppMonad ()
brokerLoop config frontend backend remainingClients workerQueue = do
  logger <- ask
  logInfoR $ formatWorkerQueue workerQueue
  -- ⭐️⭐️⭐️ determine which sockets to poll based on the worker queue
  let pollItems = Zmqx.the backend & if not (null workerQueue) then Zmqx.also frontend else id

  -- ⭐️ poll for messages
  liftIO $
    Zmqx.poll pollItems >>= \case
      Left _ -> pure ()
      Right ready -> do
        logInfoM logger $ printf "brokerLoop poll msg, remainingClients: %d" remainingClients
        -- ⭐️ handle the poll results and continue the loop
        (newClients, newQueue) <- runReaderT (handlePollResults ready remainingClients workerQueue) logger
        runReaderT (brokerLoop config frontend backend newClients newQueue) logger
  where
    -- ⭐️ handle poll results by processing backend and frontend messages
    handlePollResults :: Zmqx.Ready -> Int -> [WorkerInfo] -> AppMonad (Int, [WorkerInfo])
    handlePollResults ready clientsLeft queue = do
      (clientsAfterBackend, queueAfterBackend) <- handleBackend config frontend backend ready queue clientsLeft
      handleFrontend config frontend backend ready clientsAfterBackend queueAfterBackend

-- ⭐️⭐️ handle messages from the frontend (clients)
handleFrontend :: BrokerConfig -> Zmqx.Router -> Zmqx.Router -> Zmqx.Ready -> Int -> [WorkerInfo] -> AppMonad (Int, [WorkerInfo])
handleFrontend BrokerConfig {..} frontend backend (Zmqx.Ready ready) clientsLeft queue
  | clientsLeft <= 0 || not (ready frontend) = do
      logInfoR $ printf "handleFrontend: no client/ not ready"
      pure (clientsLeft, queue)
  | otherwise = do
      logInfoR $ printf "handleFrontend: pass"
      case queue of
        [] -> pure (clientsLeft, queue)
        (workerId, workerReqId) : rest ->
          decodeClientMsg <$> zmqUnwrap (Zmqx.receives frontend) >>= \case
            Just (ClientRequest clientId clientReqId request) -> do
              let workerMsg = encodeWorkerMessage (WorkerReply workerId workerReqId clientId clientReqId request)
              -- 📧 get next client request, route to last-used worker
              zmqUnwrap (Zmqx.sends backend workerMsg)
              logInfoR $ printf "handleFrontend: sent backend WorkerReply %s" (show workerMsg)

              -- TODO: try to delay the worker queue modification,
              -- if only one worker in the pool, it would be removed from the queue,
              -- so that the backend can not received the reply msg
              liftIO $ threadDelay 1_000_000
              -- remove the first worker from the queue
              pure (clientsLeft, rest)
            Nothing -> do
              logInfoR $ printf "handleFrontend: decode clientMsg failed"
              pure (clientsLeft, queue)

-- ⭐️⭐️ handle messages from the backend (workers)
handleBackend :: BrokerConfig -> Zmqx.Router -> Zmqx.Router -> Zmqx.Ready -> [WorkerInfo] -> Int -> AppMonad (Int, [WorkerInfo])
handleBackend BrokerConfig {..} frontend backend (Zmqx.Ready ready) queue clientsLeft
  | not (ready backend) = do
      logInfoR $ printf "handleBackend: not ready"
      pure (clientsLeft, queue)
  | otherwise = do
      logInfoR $ printf "handleBackend: ready"
      decodeWorkerMsg <$> zmqUnwrap (Zmqx.receives backend) >>= \case
        Just (WorkerReady workerId reqId) -> do
          logInfoR $ printf "handleBackend: WorkerReady"
          pure (clientsLeft, (workerId, reqId) : queue)
        Just (WorkerReply workerId reqId clientId clientReqId reply) -> do
          -- received message from a worker, and pack the reply message then send to the client
          let replyMsg = encodeClientMsg (ClientRequest clientId clientReqId reply)
          zmqUnwrap (Zmqx.sends frontend replyMsg)
          logInfoR $ printf "handleBackend: WorkerReply %s" (show replyMsg)
          pure (clientsLeft - 1, (workerId, reqId) : queue)
        Nothing -> do
          logInfoR $ printf "handleBackend: Nothing"
          pure (clientsLeft, queue)

----------------------------------------------------------------------------------------------------
-- Error handling with proper logging

-- Generalized unwrap with logging (now properly constrained)
logUnwrap ::
  (MonadIO m, MonadReader Logger m, Exception e) =>
  (String -> m ()) -> -- Logger function
  (e -> String) -> -- Error formatter
  IO (Either e a) -> -- IO action
  m a
logUnwrap loggerFn formatErr action = do
  result <- liftIO action
  case result of
    Left err -> do
      loggerFn $ formatErr err
      liftIO $ throwIO err
    Right x -> pure x

-- Specialized version for ZMQ errors using error logger
zmqUnwrap :: IO (Either Zmqx.Error a) -> AppMonad a
zmqUnwrap = logUnwrap logErrorR show

-- Generalized unwrap with logging for IO
logUnwrapIO :: (Exception e) => (String -> IO ()) -> (e -> String) -> IO (Either e a) -> IO a
logUnwrapIO loggerFn formatErr action = do
  result <- action
  case result of
    Left err -> do
      loggerFn $ formatErr err
      throwIO err
    Right x -> pure x

-- Specialized version for ZMQ errors using error logger in IO
zmqUnwrap' :: IO (Either Zmqx.Error a) -> IO a
zmqUnwrap' = logUnwrapIO printf show

-- Helper function to format workerQueue for printing
formatWorkerQueue :: [WorkerInfo] -> String
formatWorkerQueue queue =
  "Worker Queue: " ++ show ((\(i, reqId) -> (ByteString.unpack i, ByteString.unpack reqId)) <$> queue)

----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  logger <- createLogger
  let customConfig =
        defaultBrokerConfig
          { -- overwrite
            -- frontendAddr = "tcp://127.0.0.1:5555",
            -- backendAddr = "tcp://127.0.0.1:5556",
            -- nbrClients = 3,
            -- nbrWorkers = 1
            nbrWorkers = 3
          }

  runReaderT (runBroker customConfig) logger
