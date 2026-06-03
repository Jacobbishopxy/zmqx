{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.ByteString.Char8 qualified as ByteString
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Unique (hashUnique, newUnique)
import System.Timeout qualified as Timeout
import Zmqx qualified
import Zmqx.EventLoop qualified as EventLoop
import Zmqx.Pair qualified
import Zmqx.Pull qualified
import Zmqx.Push qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

uniqueEndpoint :: String -> IO Text
uniqueEndpoint label = do
  unique <- newUnique
  pure ("inproc://event-loop-latency-auto-" <> Text.pack label <> "-" <> Text.pack (show (hashUnique unique)))

awaitConnection :: IO ()
awaitConnection =
  threadDelay 100000

expectFrames :: String -> Maybe [ByteString] -> Maybe [ByteString] -> IO ()
expectFrames label actual expected =
  assert (actual == expected) (label <> ": expected " <> show expected <> ", got " <> show actual)

expectRightUnit :: String -> Either Zmqx.Error () -> IO ()
expectRightUnit label = \case
  Right () -> pure ()
  Left err -> throwIO (userError (label <> ": expected Right (), got " <> show err))

expectLeftErrno :: String -> Zmqx.Zmq_error -> Either Zmqx.Error a -> IO ()
expectLeftErrno label expectedErrno = \case
  Left err ->
    assert
      (Zmqx.errno err == expectedErrno)
      (label <> ": expected errno " <> show expectedErrno <> ", got " <> show (Zmqx.errno err))
  Right _ -> throwIO (userError (label <> ": expected Left Error"))

expectMVar :: String -> MVar a -> IO a
expectMVar label var =
  Timeout.timeout 2000000 (takeMVar var) >>= \case
    Nothing -> throwIO (userError (label <> ": timed out"))
    Just result -> pure result

expectCompletes :: String -> IO a -> IO a
expectCompletes label action =
  Timeout.timeout 2000000 action >>= \case
    Nothing -> throwIO (userError (label <> ": timed out"))
    Just result -> pure result

expectNoMVar :: String -> MVar a -> IO ()
expectNoMVar label var =
  Timeout.timeout 50000 (takeMVar var) >>= \case
    Nothing -> pure ()
    Just _ -> throwIO (userError (label <> ": completed before it should have"))

readyMailboxPositiveTimeoutTest :: IO ()
readyMailboxPositiveTimeoutTest =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "ready-mailbox"
    pull <- unwrap (Zmqx.Pull.open (Zmqx.Pull.defaultOptions <> Zmqx.name "event-loop-latency-ready-pull"))
    push <- unwrap (Zmqx.Push.open (Zmqx.Push.defaultOptions <> Zmqx.name "event-loop-latency-ready-push"))

    unwrap (Zmqx.bind pull endpoint)
    unwrap (Zmqx.connect push endpoint)
    awaitConnection

    let spec = Zmqx.addReceiver "mailbox" pull (EventLoop.Mailbox 4) Zmqx.emptySpec
    Zmqx.withEventLoop spec \loop -> do
      unwrap (Zmqx.sends push ["ready-frame"])
      received <- expectCompletes "ready positive-timeout mailbox recv" (EventLoop.recv loop "mailbox" 60000)
      case received of
        Right frames -> expectFrames "ready positive-timeout mailbox recv" frames (Just ["ready-frame"])
        Left err -> throwIO err

waitingMailboxPositiveTimeoutTest :: IO ()
waitingMailboxPositiveTimeoutTest =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "waiting-mailbox"
    pull <- unwrap (Zmqx.Pull.open (Zmqx.Pull.defaultOptions <> Zmqx.name "event-loop-latency-wait-pull"))
    push <- unwrap (Zmqx.Push.open (Zmqx.Push.defaultOptions <> Zmqx.name "event-loop-latency-wait-push"))
    recvDone <- newEmptyMVar

    unwrap (Zmqx.bind pull endpoint)
    unwrap (Zmqx.connect push endpoint)
    awaitConnection

    let spec = Zmqx.addReceiver "mailbox" pull (EventLoop.Mailbox 4) Zmqx.emptySpec
    Zmqx.withEventLoop spec \loop -> do
      _ <- forkIO do
        result <- EventLoop.recv loop "mailbox" 60000
        putMVar recvDone result
      threadDelay 10000
      unwrap (Zmqx.sends push ["wake-frame"])
      received <- expectMVar "waiting positive-timeout mailbox recv" recvDone
      case received of
        Right frames -> expectFrames "waiting positive-timeout mailbox recv" frames (Just ["wake-frame"])
        Left err -> throwIO err

longTimeoutRecvShutdownTest :: IO ()
longTimeoutRecvShutdownTest =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "shutdown-timeout"
    pull <- unwrap (Zmqx.Pull.open (Zmqx.Pull.defaultOptions <> Zmqx.name "event-loop-latency-shutdown-pull"))
    recvDone <- newEmptyMVar

    unwrap (Zmqx.bind pull endpoint)

    let spec = Zmqx.addReceiver "mailbox" pull (EventLoop.Mailbox 1) Zmqx.emptySpec
    Zmqx.withEventLoop spec \loop -> do
      _ <- forkIO do
        result <- EventLoop.recv loop "mailbox" 60000
        putMVar recvDone result
      threadDelay 10000

    result <- expectMVar "long positive-timeout recv shutdown" recvDone
    expectLeftErrno "long positive-timeout recv shutdown" Zmqx.ETERM result

transceiverSendAckUnderIdleReceiverTest :: IO ()
transceiverSendAckUnderIdleReceiverTest =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "transceiver-idle"
    loopPair <- unwrap (Zmqx.Pair.open (Zmqx.Pair.defaultOptions <> Zmqx.name "event-loop-latency-loop-pair"))
    peerPair <- unwrap (Zmqx.Pair.open (Zmqx.Pair.defaultOptions <> Zmqx.name "event-loop-latency-peer-pair"))

    unwrap (Zmqx.bind loopPair endpoint)
    unwrap (Zmqx.connect peerPair endpoint)
    awaitConnection

    let spec = Zmqx.addTransceiver "pair" loopPair (EventLoop.Mailbox 4) Zmqx.emptySpec
    Zmqx.withEventLoop spec \loop -> do
      for_ [1 :: Int .. 16] \index -> do
        let frame = ByteString.pack ("idle-send-" <> show index)
        sendResult <- expectCompletes "idle receiver transceiver send ack" (EventLoop.send loop "pair" frame)
        expectRightUnit "idle receiver transceiver send ack" sendResult
        received <- unwrap (Zmqx.receivesFor peerPair 1000)
        expectFrames "idle receiver transceiver peer receive" received (Just [frame])

slowCallbackOrdersDeliveryTest :: IO ()
slowCallbackOrdersDeliveryTest =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "slow-callback"
    pull <- unwrap (Zmqx.Pull.open (Zmqx.Pull.defaultOptions <> Zmqx.name "event-loop-latency-callback-pull"))
    push <- unwrap (Zmqx.Push.open (Zmqx.Push.defaultOptions <> Zmqx.name "event-loop-latency-callback-push"))
    firstStarted <- newEmptyMVar
    releaseFirst <- newEmptyMVar
    secondDelivered <- newEmptyMVar
    deliveryOrder <- newIORef ([] :: [ByteString])

    unwrap (Zmqx.bind pull endpoint)
    unwrap (Zmqx.connect push endpoint)
    awaitConnection

    let callback frames =
          case frames of
            ["first"] -> do
              putMVar firstStarted ()
              _ <- takeMVar releaseFirst
              modifyIORef' deliveryOrder (<> ["first"])
            ["second"] -> do
              modifyIORef' deliveryOrder (<> ["second"])
              putMVar secondDelivered ()
            other -> throwIO (userError ("unexpected callback frames: " <> show other))
        spec = Zmqx.addReceiver "callback" pull (EventLoop.Callback callback) Zmqx.emptySpec
    Zmqx.withEventLoop spec \_loop -> do
      unwrap (Zmqx.sends push ["first"])
      _ <- expectMVar "slow callback started" firstStarted
      unwrap (Zmqx.sends push ["second"])
      expectNoMVar "slow callback blocks later delivery" secondDelivered
      putMVar releaseFirst ()
      _ <- expectMVar "slow callback second delivery" secondDelivered
      order <- readIORef deliveryOrder
      assert (order == ["first", "second"]) ("callback delivery order was " <> show order)

main :: IO ()
main =
  Timeout.timeout 15000000 tests >>= \case
    Just () -> pure ()
    Nothing -> throwIO (userError "EventLoopLatencyAuto timed out")
  where
    tests = do
      readyMailboxPositiveTimeoutTest
      waitingMailboxPositiveTimeoutTest
      longTimeoutRecvShutdownTest
      transceiverSendAckUnderIdleReceiverTest
      slowCallbackOrdersDeliveryTest
