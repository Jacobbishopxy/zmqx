{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-poly-kind-signatures #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Zmqx.Core.Poll
  ( CanPoll,
    PollEvent (..),
    Sockets,
    Ready (..),
    pollIn,
    pollInAlso,
    pollOut,
    pollOutAlso,
    poll,
    pollFor,
    pollUntil,
  )
where

import Control.Exception
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.IORef (readIORef, writeIORef)
import Data.Primitive.Array qualified as Primitive (Array)
import Data.Primitive.Array qualified as Primitive.Array
import Data.Word (Word64)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr, ptrToIntPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import GHC.Base (Symbol)
import GHC.Clock (getMonotonicTimeNSec)
import Zmqx.Core.IO (keepAlive)
import Zmqx.Core.Socket (Socket (..))
import Zmqx.Core.Socket qualified as Socket
import Zmqx.Core.SomeSocket (SomeSocket (..))
import Zmqx.Error (Error (..), catchingOkErrors, enrichError, throwOkError, unexpectedError)
import Zmqx.Internal
import Zmqx.Internal.Bindings qualified

data PollEvent
  = PollIn
  | PollOut

class CanPoll (event :: PollEvent) (a :: Symbol)

instance CanPoll 'PollIn "DEALER"

instance CanPoll 'PollOut "DEALER"

instance CanPoll 'PollIn "PAIR"

instance CanPoll 'PollOut "PAIR"

instance CanPoll 'PollIn "PULL"

instance CanPoll 'PollOut "PUSH"

instance CanPoll 'PollIn "REP"

instance CanPoll 'PollOut "REP"

instance CanPoll 'PollIn "REQ"

instance CanPoll 'PollOut "REQ"

instance CanPoll 'PollIn "ROUTER"

instance CanPoll 'PollOut "ROUTER"

instance CanPoll 'PollIn "SUB"

instance CanPoll 'PollIn "XPUB"

instance CanPoll 'PollOut "XPUB"

instance CanPoll 'PollIn "XSUB"

instance CanPoll 'PollOut "XSUB"

instance CanPoll 'PollOut "PUB"

data Sockets
  = Sockets
      { -- sockets (with events) in reverse order of how they were added (with postfix syntax), e.g. pollIn A & pollInAlso B
        -- & pollInAlso C = [C, B, A]. Kept so adding another socket is cheap; poll calls use the prepared arrays below.
        socketList :: ![SocketToPoll],
        -- number of sockets in the list
        socketCount :: !Int,
        -- Sockets passed to libzmq. Input REQ sockets are included as wakeup candidates, but input REQ revents are
        -- validated by a nonblocking receive probe before the socket is reported ready.
        pollableSockets :: !(Primitive.Array SocketToPoll),
        -- Immutable pollitem template corresponding to `pollableSockets`; copied into a reusable stack buffer before
        -- each zmq_poll call so `revents` starts clear without allocating a StorableArray per poll.
        pollitemTemplates :: !(Primitive.Array Zmq_pollitem),
        pollableCount :: !Int,
        -- Input REQ sockets are tracked separately so buffered replies can make a poll ready even after the reply was
        -- moved out of libzmq and into the REQ buffer by an earlier probe.
        inputREQSockets :: !(Primitive.Array SomeSocket),
        inputREQCount :: !Int
      }

-- | Build a polling set listening for incoming readiness on a single socket.
pollIn :: (CanPoll 'PollIn a) => Socket a -> Sockets
pollIn socket =
  makeSockets [socketToPoll PollIn socket] 1

-- | Append another input-ready socket to an existing polling set.
pollInAlso :: (CanPoll 'PollIn a) => Socket a -> Sockets -> Sockets
pollInAlso socket Sockets {socketList, socketCount} =
  makeSockets (socketToPoll PollIn socket : socketList) (socketCount + 1)

-- | Build a polling set listening for outgoing readiness on a single socket.
pollOut :: (CanPoll 'PollOut a) => Socket a -> Sockets
pollOut socket =
  makeSockets [socketToPoll PollOut socket] 1

-- | Append another output-ready socket to an existing polling set.
pollOutAlso :: (CanPoll 'PollOut a) => Socket a -> Sockets -> Sockets
pollOutAlso socket Sockets {socketList, socketCount} =
  makeSockets (socketToPoll PollOut socket : socketList) (socketCount + 1)

makeSockets :: [SocketToPoll] -> Int -> Sockets
makeSockets socketList socketCount =
  let (pollableSocketList, inputREQSocketList) = partitionSocketList socketList
      pollableCount = length pollableSocketList
      inputREQCount = length inputREQSocketList
   in Sockets
        { socketList,
          socketCount,
          pollableSockets = Primitive.Array.arrayFromListN pollableCount pollableSocketList,
          pollitemTemplates = Primitive.Array.arrayFromListN pollableCount (map someSocketToPollitem pollableSocketList),
          pollableCount,
          inputREQSockets = Primitive.Array.arrayFromListN inputREQCount inputREQSocketList,
          inputREQCount
        }

partitionSocketList :: [SocketToPoll] -> ([SocketToPoll], [SomeSocket])
partitionSocketList =
  go [] []
  where
    go !pollableAcc !inputREQAcc = \case
      [] -> (reverse pollableAcc, reverse inputREQAcc)
      socketToPoll'@SocketToPoll {pollSocket = someSocket} : remainingSockets ->
        let inputREQAcc' =
              if socketToPollRequiresInputREQProbe socketToPoll'
                then someSocket : inputREQAcc
                else inputREQAcc
         in go (socketToPoll' : pollableAcc) inputREQAcc' remainingSockets

data Ready
  = Ready (forall a. Socket a -> Bool)

makeReady :: IntSet -> Socket a -> Bool
makeReady sockets socket =
  socketReadyId socket `IntSet.member` sockets

socketReadyId :: Socket a -> Int
socketReadyId Socket {zsocket = Zmq_socket socketPtr} =
  fromIntegral (ptrToIntPtr socketPtr)

someSocketReadyId :: SomeSocket -> Int
someSocketReadyId (SomeSocket socket) =
  socketReadyId socket

data SocketToPoll = SocketToPoll
  { pollSocket :: !SomeSocket,
    pollEvent :: !PollEvent
  }

socketToPoll :: PollEvent -> Socket a -> SocketToPoll
socketToPoll pollEvent socket =
  SocketToPoll (SomeSocket socket) pollEvent

pollEventToEvents :: PollEvent -> Zmq_events
pollEventToEvents = \case
  PollIn -> ZMQ_POLLIN
  PollOut -> ZMQ_POLLOUT

pollEventIsInput :: PollEvent -> Bool
pollEventIsInput = \case
  PollIn -> True
  PollOut -> False

socketToPollRequiresInputREQProbe :: SocketToPoll -> Bool
socketToPollRequiresInputREQProbe SocketToPoll {pollSocket = SomeSocket socket, pollEvent} =
  pollEventIsInput pollEvent
    && case extra socket of
      Socket.ReqExtra _ -> True
      _ -> False

------------------------------------------------------------------------------------------------------------------------
-- Prepared poll buffers and REQ probing

someSocketToPollitem :: SocketToPoll -> Zmq_pollitem
someSocketToPollitem SocketToPoll {pollSocket = SomeSocket Socket {zsocket}, pollEvent} =
  Zmq_pollitem_socket zsocket (pollEventToEvents pollEvent)

withPollItems :: Sockets -> (Ptr Zmq_pollitem -> IO a) -> IO a
withPollItems Sockets {pollableCount} action =
  allocaArray pollableCount action

resetPollItems :: Sockets -> Ptr Zmq_pollitem -> IO ()
resetPollItems Sockets {pollitemTemplates, pollableCount} pollitemsPtr =
  loop 0
  where
    loop !index
      | index >= pollableCount = pure ()
      | otherwise = do
          pokeElemOff pollitemsPtr index (Primitive.Array.indexArray pollitemTemplates index)
          loop (index + 1)

getBufferedInputREQSocketIds :: Sockets -> IO IntSet
getBufferedInputREQSocketIds Sockets {inputREQSockets, inputREQCount} =
  loop IntSet.empty 0
  where
    loop !acc !index
      | index >= inputREQCount = pure acc
      | otherwise = do
          acc' <- getBufferedInputREQSocketId acc (Primitive.Array.indexArray inputREQSockets index)
          loop acc' (index + 1)

getBufferedInputREQSocketId :: IntSet -> SomeSocket -> IO IntSet
getBufferedInputREQSocketId acc someSocket@(SomeSocket socket) =
  case extra socket of
    Socket.ReqExtra messageBuffer ->
      readIORef messageBuffer >>= \case
        Just _ -> pure (IntSet.insert (someSocketReadyId someSocket) acc)
        Nothing -> pure acc
    _ -> pure acc

collectReadySocketIds :: Primitive.Array SocketToPoll -> Int -> Ptr Zmq_pollitem -> IntSet -> IO (IntSet, Bool)
collectReadySocketIds socketsToPoll socketsToPollCount pollitemsPtr initialReady =
  loop initialReady False 0
  where
    loop !acc !sawInvalidREQWakeup !index
      | index >= socketsToPollCount = pure (acc, sawInvalidREQWakeup)
      | otherwise = do
          pollitem <- peekElemOff pollitemsPtr index
          if Zmqx.Internal.Bindings.revents pollitem == 0
            then loop acc sawInvalidREQWakeup (index + 1)
            else do
              let socketToPoll' = Primitive.Array.indexArray socketsToPoll index
                  readySocket = pollSocket socketToPoll'
              if socketToPollRequiresInputREQProbe socketToPoll'
                then do
                  let readyId = someSocketReadyId readySocket
                      wasReady = readyId `IntSet.member` acc
                  acc' <- probeReadyInputREQSocket acc readySocket
                  let isReady = readyId `IntSet.member` acc'
                  loop acc' (sawInvalidREQWakeup || (not wasReady && not isReady)) (index + 1)
                else
                  loop (IntSet.insert (someSocketReadyId readySocket) acc) sawInvalidREQWakeup (index + 1)

probeReadyInputREQSockets :: Sockets -> IntSet -> IO IntSet
probeReadyInputREQSockets Sockets {inputREQSockets, inputREQCount} initialReady =
  loop initialReady 0
  where
    loop !acc !index
      | index >= inputREQCount = pure acc
      | otherwise = do
          acc' <- probeReadyInputREQSocket acc (Primitive.Array.indexArray inputREQSockets index)
          loop acc' (index + 1)

probeReadyInputREQSocket :: IntSet -> SomeSocket -> IO IntSet
probeReadyInputREQSocket acc someSocket@(SomeSocket socket) =
  case extra socket of
    Socket.ReqExtra messageBuffer ->
      readIORef messageBuffer >>= \case
        Just _ -> pure (IntSet.insert (someSocketReadyId someSocket) acc)
        Nothing ->
          mask_
            ( Socket.receiveManyDontWait socket >>= \case
                Nothing -> pure acc
                Just frames -> do
                  writeIORef messageBuffer (Just frames)
                  pure (IntSet.insert (someSocketReadyId someSocket) acc)
            )
            `catch` \case
              Error {errno = EFSM} -> pure acc
              err -> throwIO err
    _ -> pure acc

------------------------------------------------------------------------------------------------------------------------
-- Polling

poll :: Sockets -> IO (Either Error Ready)
poll sockets =
  poll_ sockets Nothing <&> \case
    Left err -> Left err
    -- This case should be impossible
    Right Nothing -> Right (Ready \_ -> False)
    Right (Just ready) -> Right ready

-- | milliseconds
pollFor :: Sockets -> Int -> IO (Either Error (Maybe Ready))
pollFor sockets timeout
  | timeout < 0 = poll_ sockets Nothing
  | timeout == 0 = poll_ sockets (Just 0)
  | otherwise = do
      now <- getMonotonicTimeNSec
      poll_ sockets (Just (now + (fromIntegral @Int @Word64 timeout * 1_000_000)))

-- | monotonic time as reported by 'getMonotonicTimeNSec'
pollUntil :: Sockets -> Word64 -> IO (Either Error (Maybe Ready))
pollUntil sockets deadline = do
  poll_ sockets (Just deadline)

poll_ :: Sockets -> Maybe Word64 -> IO (Either Error (Maybe Ready))
poll_ sockets@Sockets {pollableCount, inputREQCount} maybeDeadline =
  catchingOkErrors do
    if pollableCount == 0
      then loopWithoutPollItems
      else withPollItems sockets (loopWithPollItems hasInputREQs)
  where
    hasInputREQs :: Bool
    hasInputREQs = inputREQCount > 0

    reqProbeSliceMs :: Int64
    reqProbeSliceMs = 10

    pollTimeout :: Bool -> IntSet -> IO Int64
    pollTimeout forceProbeCadence reqReady
      | not (IntSet.null reqReady) = pure 0
      | otherwise =
          case maybeDeadline of
            Nothing ->
              pure
                if forceProbeCadence && hasInputREQs
                  then reqProbeSliceMs
                  else -1
            Just deadline -> do
              now <- getMonotonicTimeNSec
              pure
                if now >= deadline
                  then 0
                  else -- safe downcast: can't overflow Int64 after dividing by 1,000,000
                    let remainingMs = fromIntegral @Word64 @Int64 (((deadline - now - 1) `div` 1_000_000) + 1)
                     in if forceProbeCadence && hasInputREQs
                          then min remainingMs reqProbeSliceMs
                          else remainingMs

    retryOrTimeout :: IO (Maybe Ready) -> IO (Maybe Ready)
    retryOrTimeout retry =
      case maybeDeadline of
        Nothing -> retry
        Just deadline -> do
          now <- getMonotonicTimeNSec
          if now >= deadline
            then pure Nothing
            else retry

    loopWithoutPollItems :: IO (Maybe Ready)
    loopWithoutPollItems = do
      reqReady <- getBufferedInputREQSocketIds sockets
      if IntSet.null reqReady
        then pure Nothing
        else ready1 reqReady

    loopWithPollItems :: Bool -> Ptr Zmq_pollitem -> IO (Maybe Ready)
    loopWithPollItems forceProbeCadence pollitemsPtr = do
      bufferedREQReady <- getBufferedInputREQSocketIds sockets
      timeout <- pollTimeout forceProbeCadence bufferedREQReady
      keepAlive (pollableSockets sockets) do
        resetPollItems sockets pollitemsPtr
        _numOstensiblyReadySockets <- zhs_poll_ptr pollitemsPtr pollableCount timeout
        (readyAfterRevents, invalidREQWakeup) <-
          collectReadySocketIds (pollableSockets sockets) pollableCount pollitemsPtr bufferedREQReady
        readySockets <-
          if forceProbeCadence && hasInputREQs && IntSet.null readyAfterRevents
            then probeReadyInputREQSockets sockets readyAfterRevents
            else pure readyAfterRevents
        if IntSet.null readySockets
          then retryOrTimeout (loopWithPollItems (forceProbeCadence || invalidREQWakeup) pollitemsPtr)
          else ready1 readySockets

    ready1 :: IntSet -> IO (Maybe Ready)
    ready1 ss =
      pure (Just (Ready (makeReady ss)))

zhs_poll_ptr :: Ptr Zmq_pollitem -> Int -> Int64 -> IO Int
zhs_poll_ptr pollitems pollitemCount timeout = do
  zmq_poll_ptr pollitems pollitemCount timeout >>= \case
    Left errno ->
      let err = enrichError "zmq_poll" errno
       in case errno of
            EINTR -> throwOkError err
            EFAULT -> throwIO err
            ETERM -> throwOkError err
            _ -> unexpectedError err
    Right n -> pure n
