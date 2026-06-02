{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Minimal event-loop API for worker-owned sender sockets.
--
-- An 'EventLoopSpec' registers application-created sender sockets under stable
-- names. While 'withEventLoop' or 'withEventLoopIn' is running, those registered
-- sockets are owned exclusively by the event-loop worker thread; callers must
-- send through 'send' instead of using the sockets directly. Ownership returns
-- to the surrounding bracket only after the event loop exits.
module Zmqx.EventLoop
  ( EventLoop,
    EventLoopSpec,
    ReceiverMode (..),
    emptySpec,
    addSender,
    withEventLoop,
    withEventLoopIn,
    send,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, tryReadMVar, tryTakeMVar)
import Control.Concurrent.STM (TQueue, TVar, atomically, newTQueueIO, newTVarIO, readTQueue, readTVar, writeTQueue, writeTVar)
import Control.Exception (SomeException, bracket, finally, mask_, throwIO, try)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.IORef (readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Zmqx.Core.Context (Context (..), RunError (..), globalContextRef, globalSocketFinalizersRef)
import Zmqx.Core.Socket (Socket)
import Zmqx.Core.Socket qualified as Socket
import Zmqx.Error (Error (..))
import Zmqx.Internal (Zmq_error (EINVAL, ENOENT, ETERM))

-- | Opaque handle to a running event loop.
--
-- Registered sockets are owned exclusively by the event-loop worker while the
-- loop is running. Public callers should interact with them through command
-- helpers such as 'send', not by touching those sockets directly.
data EventLoop = EventLoop
  { eventLoopCommands :: !(TQueue Command),
    eventLoopAccepting :: !(TVar Bool),
    eventLoopWorkerDone :: !(MVar (Either SomeException ()))
  }

-- | Declarative event-loop configuration.
--
-- The MVP configuration supports registering named sender sockets. Each
-- registered socket is handed to the event-loop worker for exclusive use while
-- the loop runs. Receiver polling is intentionally reserved for later tasks.
data EventLoopSpec = EventLoopSpec
  { specSenders :: !(Map Text Sender),
    specReceiverMode :: !ReceiverMode
  }

-- | Receiver handling mode for future receiver/polling work.
data ReceiverMode
  = NoReceivers
  deriving stock (Eq, Show)

data Sender where
  Sender :: Socket.CanSend (Socket a) => !(Socket a) -> Sender

type SendReply = Either SomeException (Either Error ())

data Command
  = Send !Text !ByteString !(MVar SendReply)
  | Stop

-- | Empty event-loop specification with no registered sockets.
emptySpec :: EventLoopSpec
emptySpec =
  EventLoopSpec
    { specSenders = Map.empty,
      specReceiverMode = NoReceivers
    }

-- | Register a named sender socket.
--
-- The sender socket must belong to the context selected by 'withEventLoop' or
-- 'withEventLoopIn'. Once the loop starts, the worker owns the socket until the
-- bracketed action exits.
addSender :: Socket.CanSend (Socket a) => Text -> Socket a -> EventLoopSpec -> EventLoopSpec
addSender endpoint socket spec@EventLoopSpec {specSenders} =
  spec {specSenders = Map.insert endpoint (Sender socket) specSenders}

-- | Run an event loop using the active global context.
--
-- Use this with sockets opened through the normal @open@ helpers inside
-- 'Zmqx.run'. Registered sockets must belong to that active global context and
-- are worker-owned for the duration of the bracketed action.
withEventLoop :: EventLoopSpec -> (EventLoop -> IO a) -> IO a
withEventLoop spec action = do
  context <- getActiveGlobalContext
  withLoop context spec action

-- | Run an event loop using an explicit context.
--
-- Use this with sockets opened through @openWith@ against the same 'Context'.
-- Registered sockets are worker-owned for the duration of the bracketed action.
withEventLoopIn :: Context -> EventLoopSpec -> (EventLoop -> IO a) -> IO a
withEventLoopIn =
  withLoop

withLoop :: Context -> EventLoopSpec -> (EventLoop -> IO a) -> IO a
withLoop loopContext spec action = do
  validateSpecContext loopContext spec
  bracket (startEventLoop spec) stopEventLoop action

getActiveGlobalContext :: IO Context
getActiveGlobalContext =
  readIORef globalContextRef >>= \case
    Nothing -> throwIO ContextNotInitialized
    Just contextPtr ->
      pure
        Context
          { contextPtr,
            contextFinalizers = globalSocketFinalizersRef
          }

startEventLoop :: EventLoopSpec -> IO EventLoop
startEventLoop EventLoopSpec {specSenders} =
  mask_ do
    commands <- newTQueueIO
    accepting <- newTVarIO True
    workerDone <- newEmptyMVar
    _ <- forkIO do
      result <- try (workerLoop accepting specSenders commands)
      putMVar workerDone result
    pure
      EventLoop
        { eventLoopCommands = commands,
          eventLoopAccepting = accepting,
          eventLoopWorkerDone = workerDone
        }

stopEventLoop :: EventLoop -> IO ()
stopEventLoop EventLoop {eventLoopCommands, eventLoopAccepting, eventLoopWorkerDone} =
  mask_ do
    atomically do
      accepting <- readTVar eventLoopAccepting
      when accepting do
        writeTVar eventLoopAccepting False
        writeTQueue eventLoopCommands Stop
    readMVar eventLoopWorkerDone >>= \case
      Left exception -> throwIO exception
      Right () -> pure ()

workerLoop :: TVar Bool -> Map Text Sender -> TQueue Command -> IO ()
workerLoop accepting senders commands =
  loop `finally` atomically (writeTVar accepting False)
  where
    loop =
      atomically (readTQueue commands) >>= \case
        Send endpoint frame reply -> do
          result <- try do
            case Map.lookup endpoint senders of
              Nothing -> pure (Left (missingSenderError endpoint))
              Just sender -> sendWithSender sender frame
          case result of
            Left exception -> do
              atomically (writeTVar accepting False)
              putMVar reply (Left exception)
              throwIO exception
            Right sendResult -> do
              putMVar reply (Right sendResult)
              loop
        Stop -> pure ()

sendWithSender :: Sender -> ByteString -> IO (Either Error ())
sendWithSender (Sender socket) =
  Socket.send_ socket

validateSpecContext :: Context -> EventLoopSpec -> IO ()
validateSpecContext loopContext EventLoopSpec {specSenders} =
  traverse_ validateSender (Map.toList specSenders)
  where
    validateSender (endpoint, Sender socket) =
      when (Socket.context socket /= loopContext) do
        throwIO (contextMismatchError endpoint)

-- | Queue a single-frame send command for a registered sender.
--
-- Public calls never touch registered sender sockets directly. While the loop
-- is running this function writes a command to the worker and waits for the
-- worker's result. Missing sender keys and stopped loops are normal
-- user-visible failures and return 'Left' 'Error'.
send :: EventLoop -> Text -> ByteString -> IO (Either Error ())
send EventLoop {eventLoopCommands, eventLoopAccepting, eventLoopWorkerDone} endpoint frame = do
  reply <- newEmptyMVar
  queued <-
    atomically do
      accepting <- readTVar eventLoopAccepting
      if accepting
        then do
          writeTQueue eventLoopCommands (Send endpoint frame reply)
          pure True
        else pure False
  if queued
    then waitForSendReply eventLoopWorkerDone reply
    else stoppedResult eventLoopWorkerDone

waitForSendReply :: MVar (Either SomeException ()) -> MVar SendReply -> IO (Either Error ())
waitForSendReply workerDone reply =
  tryTakeMVar reply >>= \case
    Just result -> completeSendReply result
    Nothing -> do
      testDelayAfterEmptyReply
      tryReadMVar workerDone >>= \case
        Just workerResult ->
          tryTakeMVar reply >>= \case
            Just result -> completeSendReply result
            Nothing -> completeWorkerResult workerResult
        Nothing -> do
          threadDelay 1000
          waitForSendReply workerDone reply

completeSendReply :: SendReply -> IO (Either Error ())
completeSendReply = \case
  Left exception -> throwIO exception
  Right result -> pure result

completeWorkerResult :: Either SomeException () -> IO (Either Error ())
completeWorkerResult = \case
  Left exception -> throwIO exception
  Right () -> pure (Left stoppedLoopError)

testDelayAfterEmptyReply :: IO ()
testDelayAfterEmptyReply =
  lookupEnv eventLoopTestDelayAfterEmptyReplyEnv >>= \case
    Just raw
      | Just delay <- readMaybe raw -> threadDelay delay
    _ -> pure ()

-- Deliberately unexported regression-test hook for forcing a caller to remain
-- in the reply wait path after observing an initially empty command reply.
eventLoopTestDelayAfterEmptyReplyEnv :: String
eventLoopTestDelayAfterEmptyReplyEnv =
  "ZMQX_EVENT_LOOP_TEST_DELAY_AFTER_EMPTY_REPLY_US"

stoppedResult :: MVar (Either SomeException ()) -> IO (Either Error ())
stoppedResult workerDone =
  tryReadMVar workerDone >>= \case
    Just (Left exception) -> throwIO exception
    _ -> pure (Left stoppedLoopError)

missingSenderError :: Text -> Error
missingSenderError endpoint =
  Error
    { function = "Zmqx.EventLoop.send",
      errno = ENOENT,
      description = "event loop sender is not registered: " <> endpoint
    }

stoppedLoopError :: Error
stoppedLoopError =
  Error
    { function = "Zmqx.EventLoop.send",
      errno = ETERM,
      description = "event loop is stopped"
    }

contextMismatchError :: Text -> Error
contextMismatchError endpoint =
  Error
    { function = "Zmqx.EventLoop.withEventLoop",
      errno = EINVAL,
      description = "event loop sender belongs to a different context: " <> endpoint
    }
