{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Minimal event-loop API for worker-owned sockets.
--
-- An 'EventLoopSpec' registers application-created sender, receiver, and
-- transceiver sockets under stable names. A transceiver, registered with
-- @addTransceiver@, is a single socket that participates in both halves of the
-- API: it must support multipart sends, multipart receives, and poll-in
-- readiness; public 'send'/'sends' commands write through the worker and inbound
-- multipart messages are delivered with the same 'ReceiverMode' machinery used
-- by receiver-only endpoints.
--
-- Endpoint names form one namespace across sender, receiver, and transceiver
-- registrations; duplicates are rejected before the worker thread starts.
-- Registered sockets must belong to the context selected by 'withEventLoop' or
-- 'withEventLoopIn'; context mismatches are rejected during bracket startup
-- before worker ownership begins. While 'withEventLoop' or 'withEventLoopIn' is
-- running, all registered sockets are owned exclusively by the event-loop worker
-- thread; callers must send through
-- 'send' and read mailbox receivers through 'recv' instead of using the sockets
-- directly. Ownership returns to the surrounding bracket only after the event
-- loop exits, and shutdown wakes pending public send/recv callers with either
-- the recorded worker exception or a stopped-loop error.
module Zmqx.EventLoop
  ( EventLoop,
    EventLoopSpec,
    ReceiverMode (..),
    emptySpec,
    addSender,
    addReceiver,
    addTransceiver,
    withEventLoop,
    withEventLoopIn,
    send,
    sends,
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
import Control.Exception (SomeException, bracket, mask_, throwIO, try)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.IORef (readIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
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
import Zmqx.Error (Error (..), catchingOkErrors)
import Zmqx.Internal (Zmq_error (EINVAL, ENOENT, ETERM))

-- | Opaque handle to a running event loop.
--
-- Registered sockets are owned exclusively by the event-loop worker while the
-- loop is running. Public callers should interact with them through command
-- helpers such as 'send' and mailbox reads through 'recv', not by touching
-- those sockets directly. After the bracket exits, public operations consult
-- loop state and the worker result only; they fail with a stopped-loop error or
-- rethrow the recorded worker failure without touching worker-owned sockets.
data EventLoop = EventLoop
  { eventLoopCommands :: !(TQueue Command),
    eventLoopAccepting :: !(TVar Bool),
    eventLoopWorkerDone :: !(MVar (Either SomeException ())),
    eventLoopReceivers :: !(Map Text ReceiverHandle)
  }

-- | Declarative event-loop configuration.
--
-- The configuration supports registering named sender, receiver, and
-- transceiver sockets. Endpoint names are a single namespace across all three
-- roles: if a name is registered more than once, whether in the same role or
-- across different roles, loop startup fails deterministically before any
-- worker thread takes ownership of sockets. Each registered socket is handed to
-- the event-loop worker for exclusive use while the loop runs.
data EventLoopSpec = EventLoopSpec
  { specSenders :: !(Map Text Sender),
    specReceivers :: !(Map Text Receiver),
    specTransceivers :: !(Map Text Transceiver),
    specDuplicateEndpoints :: !(Set Text)
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
  Sender :: Socket.CanSends (Socket a) => !(Socket a) -> Sender

data Receiver where
  Receiver ::
    (Socket.CanReceives (Socket a), Poll.CanPoll 'Poll.PollIn a) =>
    !(Socket a) ->
    !ReceiverMode ->
    Receiver

data Transceiver where
  Transceiver ::
    (Socket.CanSends (Socket a), Socket.CanReceives (Socket a), Poll.CanPoll 'Poll.PollIn a) =>
    !(Socket a) ->
    !ReceiverMode ->
    Transceiver

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
  = Send !Text !(NonEmpty ByteString) !(MVar SendReply)
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
      specReceivers = Map.empty,
      specTransceivers = Map.empty,
      specDuplicateEndpoints = Set.empty
    }

-- | Register a named sender socket.
--
-- The sender socket must belong to the context selected by 'withEventLoop' or
-- 'withEventLoopIn'. Once the loop starts, the worker owns the socket until the
-- bracketed action exits.
addSender :: Socket.CanSends (Socket a) => Text -> Socket a -> EventLoopSpec -> EventLoopSpec
addSender endpoint socket spec@EventLoopSpec {specSenders} =
  spec
    { specSenders = Map.insert endpoint (Sender socket) specSenders,
      specDuplicateEndpoints = recordDuplicateEndpoint endpoint spec
    }

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
  spec
    { specReceivers = Map.insert endpoint (Receiver socket mode) specReceivers,
      specDuplicateEndpoints = recordDuplicateEndpoint endpoint spec
    }

-- | Register a named transceiver socket with a delivery mode.
--
-- The transceiver socket must belong to the context selected by
-- 'withEventLoop' or 'withEventLoopIn'. Public 'send'/'sends' calls write
-- multipart messages through the worker, and inbound multipart messages use
-- the supplied 'ReceiverMode' just like receiver-only endpoints. Registered
-- transceivers are polled by the worker, so callers must not send to or receive
-- from those sockets directly while the loop is running.
addTransceiver ::
  (Socket.CanSends (Socket a), Socket.CanReceives (Socket a), Poll.CanPoll 'Poll.PollIn a) =>
  Text ->
  Socket a ->
  ReceiverMode ->
  EventLoopSpec ->
  EventLoopSpec
addTransceiver endpoint socket mode spec@EventLoopSpec {specTransceivers} =
  spec
    { specTransceivers = Map.insert endpoint (Transceiver socket mode) specTransceivers,
      specDuplicateEndpoints = recordDuplicateEndpoint endpoint spec
    }

-- | Run an event loop using the active global context.
--
-- Use this with sockets opened through the normal @open@ helpers inside
-- 'Zmqx.run'. Registered sockets must belong to that active global context;
-- mismatches are rejected before the worker thread starts. Sockets are
-- worker-owned for the duration of the bracketed action. This bracketed helper
-- is the public lifecycle boundary; loop startup and shutdown remain internal so
-- stopped loops cannot be reused as long-lived mutable objects.
withEventLoop :: EventLoopSpec -> (EventLoop -> IO a) -> IO a
withEventLoop spec action = do
  context <- getActiveGlobalContext
  withLoop context spec action

-- | Run an event loop using an explicit context.
--
-- Use this with sockets opened through @openWith@ against the same 'Context'.
-- Context mismatches are rejected before the worker thread starts. Registered
-- sockets are worker-owned for the duration of the bracketed action; as with
-- 'withEventLoop', the bracket is the public lifecycle boundary.
withEventLoopIn :: Context -> EventLoopSpec -> (EventLoop -> IO a) -> IO a
withEventLoopIn =
  withLoop

withLoop :: Context -> EventLoopSpec -> (EventLoop -> IO a) -> IO a
withLoop loopContext spec action = do
  validateDuplicateEndpoints spec
  validateSpecContext loopContext spec
  bracket (startEventLoop spec) stopEventLoop action

recordDuplicateEndpoint :: Text -> EventLoopSpec -> Set Text
recordDuplicateEndpoint endpoint EventLoopSpec {specSenders, specReceivers, specTransceivers, specDuplicateEndpoints} =
  if endpoint `Map.member` specSenders
    || endpoint `Map.member` specReceivers
    || endpoint `Map.member` specTransceivers
    then Set.insert endpoint specDuplicateEndpoints
    else specDuplicateEndpoints

validateDuplicateEndpoints :: EventLoopSpec -> IO ()
validateDuplicateEndpoints EventLoopSpec {specDuplicateEndpoints} =
  when (not (Set.null specDuplicateEndpoints)) do
    throwIO (duplicateEndpointNameError specDuplicateEndpoints)

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
startEventLoop EventLoopSpec {specSenders, specReceivers, specTransceivers} =
  mask_ do
    commands <- newTQueueIO
    accepting <- newTVarIO True
    workerDone <- newEmptyMVar
    (receiverHandles, workerReceivers) <- prepareReceivers (mergeReceivers specReceivers specTransceivers)
    _ <- forkIO do
      runWorker accepting workerDone (workerLoop accepting (mergeSenders specSenders specTransceivers) workerReceivers commands)
    pure
      EventLoop
        { eventLoopCommands = commands,
          eventLoopAccepting = accepting,
          eventLoopWorkerDone = workerDone,
          eventLoopReceivers = receiverHandles
        }

mergeSenders :: Map Text Sender -> Map Text Transceiver -> Map Text Sender
mergeSenders senders transceivers =
  Map.union senders (fmap transceiverSender transceivers)

transceiverSender :: Transceiver -> Sender
transceiverSender (Transceiver socket _) =
  Sender socket

mergeReceivers :: Map Text Receiver -> Map Text Transceiver -> Map Text Receiver
mergeReceivers receivers transceivers =
  Map.union receivers (fmap transceiverReceiver transceivers)

transceiverReceiver :: Transceiver -> Receiver
transceiverReceiver (Transceiver socket mode) =
  Receiver socket mode

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

-- Worker failure contract: send, receive, poll, and callback exceptions are
-- captured in 'eventLoopWorkerDone' before the worker closes the accepting
-- state. Public waiters wake from that state change and then surface the
-- recorded exception instead of remaining blocked during cleanup.
runWorker :: TVar Bool -> MVar (Either SomeException ()) -> IO () -> IO ()
runWorker accepting workerDone action = do
  result <- try action
  putMVar workerDone result
  atomically (writeTVar accepting False)

workerLoop :: TVar Bool -> Map Text Sender -> WorkerReceivers -> TQueue Command -> IO ()
workerLoop accepting senders workerReceivers commands =
  loop
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
  Send endpoint frames reply -> do
    result <- try do
      case Map.lookup endpoint senders of
        Nothing -> pure (Left (missingSenderError endpoint))
        Just sender -> sendWithSender accepting sender frames
    case result of
      Left exception -> do
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

sendWithSender :: TVar Bool -> Sender -> NonEmpty ByteString -> IO (Either Error ())
sendWithSender accepting (Sender socket) frames =
  case Socket.extra socket of
    Socket.DealerExtra -> sendWithShutdownAwareSocket accepting socket frames
    Socket.PairExtra -> sendWithShutdownAwareSocket accepting socket frames
    Socket.PushExtra -> sendWithShutdownAwareSocket accepting socket frames
    Socket.ReqExtra _ -> sendWithShutdownAwareSocket accepting socket frames
    Socket.RouterExtra -> sendWithShutdownAwareSocket accepting socket frames
    _ -> Socket.sends_ socket (NonEmpty.toList frames)

sendWithShutdownAwareSocket :: TVar Bool -> Socket a -> NonEmpty ByteString -> IO (Either Error ())
sendWithShutdownAwareSocket accepting socket frames =
  catchingOkErrors sendLoop >>= \case
    Left err -> pure (Left err)
    Right SendCompleted -> pure (Right ())
    Right SendStopped -> pure (Left (stoppedLoopError "Zmqx.EventLoop.send"))
  where
    sendLoop =
      atomically (readTVar accepting) >>= \case
        False -> pure SendStopped
        True ->
          Socket.sendManyDontWait socket frames >>= \case
            True -> pure SendCompleted
            False -> do
              threadDelay senderRetrySliceUs
              sendLoop

senderRetrySliceUs :: Int
senderRetrySliceUs =
  1000

data WorkerSendResult
  = SendCompleted
  | SendStopped

validateSpecContext :: Context -> EventLoopSpec -> IO ()
validateSpecContext loopContext EventLoopSpec {specSenders, specReceivers, specTransceivers} = do
  traverse_ validateSender (Map.toList specSenders)
  traverse_ validateReceiver (Map.toList specReceivers)
  traverse_ validateTransceiver (Map.toList specTransceivers)
  where
    validateSender (endpoint, Sender socket) =
      when (Socket.context socket /= loopContext) do
        throwIO (contextMismatchError "sender" endpoint)

    validateReceiver (endpoint, Receiver socket _) =
      when (Socket.context socket /= loopContext) do
        throwIO (contextMismatchError "receiver" endpoint)

    validateTransceiver (endpoint, Transceiver socket _) =
      when (Socket.context socket /= loopContext) do
        throwIO (contextMismatchError "transceiver" endpoint)

-- | Queue a single-frame send command for a registered sender.
--
-- This is a convenience wrapper around 'sends'.
send :: EventLoop -> Text -> ByteString -> IO (Either Error ())
send loop endpoint frame =
  sends loop endpoint [frame]

-- | Queue a multipart send command for a registered sender.
--
-- Public calls never touch registered sender or transceiver sockets directly.
-- While the loop is running this function writes a command to the worker and
-- waits for the worker's result. Sender roles whose underlying socket send may
-- block are retried by the worker in shutdown-aware slices, so bracket exit can
-- wake pending 'send'/'sends' callers with @ETERM@ instead of deadlocking.
-- Missing sender keys and stopped loops are normal user-visible failures and
-- return 'Left' 'Error'. An empty frame list is a no-op.
sends :: EventLoop -> Text -> [ByteString] -> IO (Either Error ())
sends loop endpoint = \case
  [] -> pure (Right ())
  frame : frames -> queueSend loop endpoint (frame :| frames)

queueSend :: EventLoop -> Text -> NonEmpty ByteString -> IO (Either Error ())
queueSend EventLoop {eventLoopCommands, eventLoopAccepting, eventLoopWorkerDone} endpoint frames = do
  reply <- newEmptyMVar
  queued <-
    atomically do
      accepting <- readTVar eventLoopAccepting
      if accepting
        then do
          writeTQueue eventLoopCommands (Send endpoint frames reply)
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
  readMVar workerDone >>= \case
    Left exception -> throwIO exception
    Right () -> pure (Left (stoppedLoopError functionName))

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

duplicateEndpointNameError :: Set Text -> Error
duplicateEndpointNameError endpoints =
  Error
    { function = "Zmqx.EventLoop.withEventLoop",
      errno = EINVAL,
      description =
        "event loop endpoint name registered more than once: "
          <> Text.intercalate ", " (Set.toAscList endpoints)
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
