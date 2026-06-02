{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Minimal event-loop API for worker-owned sockets.
--
-- An 'EventLoopSpec' registers application-created sender and receiver sockets
-- under stable names. While 'withEventLoop' or 'withEventLoopIn' is running,
-- those registered sockets are owned exclusively by the event-loop worker
-- thread; callers must send through 'send' and read mailbox receivers through
-- 'recv' instead of using the sockets directly. Ownership returns to the
-- surrounding bracket only after the event loop exits.
module Zmqx.EventLoop
  ( EventLoop,
    EventLoopSpec,
    ReceiverMode (..),
    emptySpec,
    addSender,
    addReceiver,
    withEventLoop,
    withEventLoopIn,
    send,
    recv,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, tryReadMVar, tryTakeMVar)
import Control.Concurrent.STM
  ( STM,
    TBQueue,
    TQueue,
    TVar,
    atomically,
    isEmptyTBQueue,
    isFullTBQueue,
    newTBQueueIO,
    newTQueueIO,
    newTVarIO,
    orElse,
    readTBQueue,
    readTQueue,
    readTVar,
    tryReadTQueue,
    retry,
    writeTBQueue,
    writeTQueue,
    writeTVar,
  )
import Control.Exception (SomeException, bracket, finally, mask_, throwIO, try)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.IORef (readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Zmqx.Core.Context (Context (..), RunError (..), globalContextRef, globalSocketFinalizersRef)
import Zmqx.Core.Poll qualified as Poll
import Zmqx.Core.Socket (Socket)
import Zmqx.Core.Socket qualified as Socket
import Zmqx.Error (Error (..))
import Zmqx.Internal (Zmq_error (EINVAL, ENOENT, ETERM))

-- | Opaque handle to a running event loop.
--
-- Registered sockets are owned exclusively by the event-loop worker while the
-- loop is running. Public callers should interact with them through command
-- helpers such as 'send' and mailbox reads through 'recv', not by touching
-- those sockets directly.
data EventLoop = EventLoop
  { eventLoopCommands :: !(TQueue Command),
    eventLoopAccepting :: !(TVar Bool),
    eventLoopWorkerDone :: !(MVar (Either SomeException ())),
    eventLoopReceivers :: !(Map Text ReceiverHandle)
  }

-- | Declarative event-loop configuration.
--
-- The configuration supports registering named sender sockets and named
-- receiver sockets. Each registered socket is handed to the event-loop worker
-- for exclusive use while the loop runs.
data EventLoopSpec = EventLoopSpec
  { specSenders :: !(Map Text Sender),
    specReceivers :: !(Map Text Receiver)
  }

-- | Receiver delivery mode.
--
-- 'Mailbox' creates a bounded mailbox for the receiver. Public 'recv' calls read
-- complete multipart messages from that mailbox and use a millisecond timeout.
-- The capacity must be positive. If the mailbox is full when a message arrives,
-- the event loop drops the newest message instead of blocking the worker.
--
-- 'Callback' executes the supplied callback on the event-loop worker thread for
-- each received multipart message. Callback receivers do not have a public
-- mailbox, so 'recv' on their endpoint returns a normal 'Left' 'Error'.
-- Callbacks must be quick and nonblocking; slow callbacks delay all other loop
-- work, and callback exceptions terminate the event-loop worker.
--
-- 'NoReceivers' is retained for source compatibility with the initial API
-- scaffold; when used with 'addReceiver' it registers the socket without a
-- public mailbox or callback delivery.
data ReceiverMode
  = NoReceivers
  | Mailbox !Int
  | Callback !([ByteString] -> IO ())

instance Eq ReceiverMode where
  NoReceivers == NoReceivers = True
  Mailbox left == Mailbox right = left == right
  Callback _ == Callback _ = True
  _ == _ = False

instance Show ReceiverMode where
  show = \case
    NoReceivers -> "NoReceivers"
    Mailbox capacity -> "Mailbox " <> show capacity
    Callback _ -> "Callback <function>"

data Sender where
  Sender :: Socket.CanSend (Socket a) => !(Socket a) -> Sender

data Receiver where
  Receiver ::
    (Socket.CanReceives (Socket a), Poll.CanPoll 'Poll.PollIn a) =>
    !(Socket a) ->
    !ReceiverMode ->
    Receiver

data ReceiverDelivery
  = NoReceiverDelivery
  | MailboxDelivery !(TBQueue [ByteString])
  | CallbackDelivery !([ByteString] -> IO ())

data ReceiverRuntime where
  ReceiverRuntime ::
    (Socket.CanReceives (Socket a), Poll.CanPoll 'Poll.PollIn a) =>
    !(Socket a) ->
    !ReceiverDelivery ->
    ReceiverRuntime

data ReceiverHandle = ReceiverHandle
  { receiverHandleMailbox :: !(Maybe (TBQueue [ByteString]))
  }

data WorkerReceivers = WorkerReceivers
  { workerReceiverMap :: !(Map Text ReceiverRuntime),
    workerReceiverPollSet :: !(Maybe Poll.Sockets)
  }

type SendReply = Either SomeException (Either Error ())

data Command
  = Send !Text !ByteString !(MVar SendReply)
  | Stop

data RecvOutcome
  = RecvMessage ![ByteString]
  | RecvTimeout
  | RecvStopped

-- | Empty event-loop specification with no registered sockets.
emptySpec :: EventLoopSpec
emptySpec =
  EventLoopSpec
    { specSenders = Map.empty,
      specReceivers = Map.empty
    }

-- | Register a named sender socket.
--
-- The sender socket must belong to the context selected by 'withEventLoop' or
-- 'withEventLoopIn'. Once the loop starts, the worker owns the socket until the
-- bracketed action exits.
addSender :: Socket.CanSend (Socket a) => Text -> Socket a -> EventLoopSpec -> EventLoopSpec
addSender endpoint socket spec@EventLoopSpec {specSenders} =
  spec {specSenders = Map.insert endpoint (Sender socket) specSenders}

-- | Register a named receiver socket with a delivery mode.
--
-- The receiver socket must belong to the context selected by 'withEventLoop' or
-- 'withEventLoopIn'. A 'Mailbox' receiver creates a bounded public mailbox read
-- through 'recv'; a 'Callback' receiver runs the callback on the event-loop
-- worker thread. Registered receivers are polled by the worker with
-- 'Zmqx.Core.Poll' so callers must not read from those sockets directly while
-- the loop is running.
addReceiver ::
  (Socket.CanReceives (Socket a), Poll.CanPoll 'Poll.PollIn a) =>
  Text ->
  Socket a ->
  ReceiverMode ->
  EventLoopSpec ->
  EventLoopSpec
addReceiver endpoint socket mode spec@EventLoopSpec {specReceivers} =
  spec {specReceivers = Map.insert endpoint (Receiver socket mode) specReceivers}

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
startEventLoop EventLoopSpec {specSenders, specReceivers} =
  mask_ do
    commands <- newTQueueIO
    accepting <- newTVarIO True
    workerDone <- newEmptyMVar
    (receiverHandles, workerReceivers) <- prepareReceivers specReceivers
    _ <- forkIO do
      result <- try (workerLoop accepting specSenders workerReceivers commands)
      putMVar workerDone result
    pure
      EventLoop
        { eventLoopCommands = commands,
          eventLoopAccepting = accepting,
          eventLoopWorkerDone = workerDone,
          eventLoopReceivers = receiverHandles
        }

prepareReceivers :: Map Text Receiver -> IO (Map Text ReceiverHandle, WorkerReceivers)
prepareReceivers receivers = do
  prepared <- Map.traverseWithKey prepareReceiver receivers
  let receiverHandles = fmap fst prepared
      receiverRuntimes = fmap snd prepared
  pure
    ( receiverHandles,
      WorkerReceivers
        { workerReceiverMap = receiverRuntimes,
          workerReceiverPollSet = receiverPollSet receiverRuntimes
        }
    )

prepareReceiver :: Text -> Receiver -> IO (ReceiverHandle, ReceiverRuntime)
prepareReceiver endpoint (Receiver socket mode) =
  case mode of
    NoReceivers ->
      pure (ReceiverHandle Nothing, ReceiverRuntime socket NoReceiverDelivery)
    Mailbox capacity -> do
      when (capacity <= 0) do
        throwIO (invalidMailboxCapacityError endpoint capacity)
      mailbox <- newTBQueueIO (fromIntegral capacity)
      pure (ReceiverHandle (Just mailbox), ReceiverRuntime socket (MailboxDelivery mailbox))
    Callback callback ->
      pure (ReceiverHandle Nothing, ReceiverRuntime socket (CallbackDelivery callback))

receiverPollSet :: Map Text ReceiverRuntime -> Maybe Poll.Sockets
receiverPollSet =
  Map.foldl' addReceiverToPollSet Nothing
  where
    addReceiverToPollSet :: Maybe Poll.Sockets -> ReceiverRuntime -> Maybe Poll.Sockets
    addReceiverToPollSet maybeSockets (ReceiverRuntime socket _) =
      case maybeSockets of
        Nothing -> Just (Poll.pollIn socket)
        Just sockets -> Just (Poll.pollInAlso socket sockets)

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

workerLoop :: TVar Bool -> Map Text Sender -> WorkerReceivers -> TQueue Command -> IO ()
workerLoop accepting senders workerReceivers commands =
  loop `finally` atomically (writeTVar accepting False)
  where
    loop =
      nextCommand workerReceivers commands >>= \case
        Just command ->
          handleCommand accepting senders command >>= \case
            True -> loop
            False -> pure ()
        Nothing -> do
          pollAndDeliverReceivers workerReceivers
          loop

nextCommand :: WorkerReceivers -> TQueue Command -> IO (Maybe Command)
nextCommand WorkerReceivers {workerReceiverPollSet} commands =
  case workerReceiverPollSet of
    Nothing -> Just <$> atomically (readTQueue commands)
    Just _ -> atomically (tryReadTQueue commands)

handleCommand :: TVar Bool -> Map Text Sender -> Command -> IO Bool
handleCommand accepting senders = \case
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
        pure True
  Stop -> pure False

pollAndDeliverReceivers :: WorkerReceivers -> IO ()
pollAndDeliverReceivers WorkerReceivers {workerReceiverMap, workerReceiverPollSet} =
  case workerReceiverPollSet of
    Nothing -> pure ()
    Just sockets ->
      Poll.pollFor sockets receiverPollSliceMs >>= \case
        Left err -> throwIO err
        Right Nothing -> pure ()
        Right (Just ready) -> traverse_ (deliverReadyReceiver ready) (Map.elems workerReceiverMap)

receiverPollSliceMs :: Int
receiverPollSliceMs =
  10

deliverReadyReceiver :: Poll.Ready -> ReceiverRuntime -> IO ()
deliverReadyReceiver (Poll.Ready isReady) receiver@(ReceiverRuntime socket _) =
  when (isReady socket) do
    receiveWithReceiver receiver >>= deliverReceiverMessage receiver

deliverReceiverMessage :: ReceiverRuntime -> [ByteString] -> IO ()
deliverReceiverMessage (ReceiverRuntime _ delivery) frames =
  case delivery of
    NoReceiverDelivery -> pure ()
    MailboxDelivery mailbox -> do
      _ <- atomically (tryWriteMailbox mailbox frames)
      pure ()
    CallbackDelivery callback -> callback frames

tryWriteMailbox :: TBQueue [ByteString] -> [ByteString] -> STM Bool
tryWriteMailbox mailbox frames =
  isFullTBQueue mailbox >>= \case
    True -> pure False
    False -> do
      writeTBQueue mailbox frames
      pure True

receiveWithReceiver :: ReceiverRuntime -> IO [ByteString]
receiveWithReceiver (ReceiverRuntime socket _) =
  Socket.receives_ socket >>= \case
    Left err -> throwIO err
    Right frames -> pure frames

sendWithSender :: Sender -> ByteString -> IO (Either Error ())
sendWithSender (Sender socket) =
  Socket.send_ socket

validateSpecContext :: Context -> EventLoopSpec -> IO ()
validateSpecContext loopContext EventLoopSpec {specSenders, specReceivers} = do
  traverse_ validateSender (Map.toList specSenders)
  traverse_ validateReceiver (Map.toList specReceivers)
  where
    validateSender (endpoint, Sender socket) =
      when (Socket.context socket /= loopContext) do
        throwIO (contextMismatchError "sender" endpoint)

    validateReceiver (endpoint, Receiver socket _) =
      when (Socket.context socket /= loopContext) do
        throwIO (contextMismatchError "receiver" endpoint)

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
    else stoppedResult "Zmqx.EventLoop.send" eventLoopWorkerDone

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
  Right () -> pure (Left (stoppedLoopError "Zmqx.EventLoop.send"))

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

-- | Read the next multipart message from a mailbox receiver.
--
-- The timeout is expressed in milliseconds. A negative timeout waits until a
-- message arrives or the loop stops; zero performs a nonblocking mailbox check;
-- positive values wait for at most that many milliseconds. Timeout returns
-- 'Right' 'Nothing'. Missing receiver endpoints, stopped loops, and receivers
-- registered without 'Mailbox' delivery return 'Left' 'Error'. Waiting mailbox
-- reads are released with the stopped-loop error when the event loop exits.
recv :: EventLoop -> Text -> Int -> IO (Either Error (Maybe [ByteString]))
recv EventLoop {eventLoopAccepting, eventLoopWorkerDone, eventLoopReceivers} endpoint timeoutMs =
  case Map.lookup endpoint eventLoopReceivers of
    Nothing -> pure (Left (missingReceiverError endpoint))
    Just ReceiverHandle {receiverHandleMailbox = Nothing} ->
      pure (Left (nonMailboxReceiverError endpoint))
    Just ReceiverHandle {receiverHandleMailbox = Just mailbox} -> do
      waitForMailbox eventLoopAccepting mailbox timeoutMs >>= \case
        RecvMessage frames -> pure (Right (Just frames))
        RecvTimeout -> pure (Right Nothing)
        RecvStopped -> stoppedResult "Zmqx.EventLoop.recv" eventLoopWorkerDone

waitForMailbox :: TVar Bool -> TBQueue [ByteString] -> Int -> IO RecvOutcome
waitForMailbox accepting mailbox timeoutMs
  | timeoutMs < 0 = atomically stoppedOrMessage
  | timeoutMs == 0 = pollMailboxOnce accepting mailbox
  | otherwise = do
      now <- getMonotonicTimeNSec
      loop (now + timeoutNs timeoutMs)
  where
    stoppedOrMessage =
      stoppedAlternative accepting `orElse` (RecvMessage <$> readTBQueue mailbox)

    loop deadline =
      pollMailboxOnce accepting mailbox >>= \case
        RecvTimeout -> do
          now <- getMonotonicTimeNSec
          if now >= deadline
            then pure RecvTimeout
            else do
              threadDelay (pollSleepUs now deadline)
              loop deadline
        outcome -> pure outcome

pollMailboxOnce :: TVar Bool -> TBQueue [ByteString] -> IO RecvOutcome
pollMailboxOnce accepting mailbox =
  atomically do
    stoppedAlternative accepting
      `orElse` do
        isEmpty <- isEmptyTBQueue mailbox
        if isEmpty
          then pure RecvTimeout
          else RecvMessage <$> readTBQueue mailbox

stoppedAlternative :: TVar Bool -> STM RecvOutcome
stoppedAlternative accepting = do
  isAccepting <- readTVar accepting
  if isAccepting
    then retry
    else pure RecvStopped

timeoutNs :: Int -> Word64
timeoutNs timeoutMs =
  fromIntegral timeoutMs * 1_000_000

pollSleepUs :: Word64 -> Word64 -> Int
pollSleepUs now deadline =
  let remainingUs = (deadline - now) `div` 1000
   in fromIntegral (min 1000 (max 1 remainingUs))

stoppedResult :: Text -> MVar (Either SomeException ()) -> IO (Either Error a)
stoppedResult functionName workerDone =
  tryReadMVar workerDone >>= \case
    Just (Left exception) -> throwIO exception
    _ -> pure (Left (stoppedLoopError functionName))

missingSenderError :: Text -> Error
missingSenderError endpoint =
  Error
    { function = "Zmqx.EventLoop.send",
      errno = ENOENT,
      description = "event loop sender is not registered: " <> endpoint
    }

missingReceiverError :: Text -> Error
missingReceiverError endpoint =
  Error
    { function = "Zmqx.EventLoop.recv",
      errno = ENOENT,
      description = "event loop receiver is not registered: " <> endpoint
    }

nonMailboxReceiverError :: Text -> Error
nonMailboxReceiverError endpoint =
  Error
    { function = "Zmqx.EventLoop.recv",
      errno = EINVAL,
      description = "event loop receiver does not use mailbox delivery: " <> endpoint
    }

stoppedLoopError :: Text -> Error
stoppedLoopError functionName =
  Error
    { function = functionName,
      errno = ETERM,
      description = "event loop is stopped"
    }

contextMismatchError :: Text -> Text -> Error
contextMismatchError role endpoint =
  Error
    { function = "Zmqx.EventLoop.withEventLoop",
      errno = EINVAL,
      description = "event loop " <> role <> " belongs to a different context: " <> endpoint
    }

invalidMailboxCapacityError :: Text -> Int -> Error
invalidMailboxCapacityError endpoint capacity =
  Error
    { function = "Zmqx.EventLoop.withEventLoop",
      errno = EINVAL,
      description =
        "event loop receiver mailbox capacity must be positive for "
          <> endpoint
          <> ": "
          <> showText capacity
    }

showText :: Show a => a -> Text
showText =
  Text.pack . show
