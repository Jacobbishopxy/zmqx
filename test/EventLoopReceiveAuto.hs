{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Unique (hashUnique, newUnique)
import System.Timeout qualified as Timeout
import Zmqx qualified
import Zmqx.EventLoop qualified as EventLoop
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
  pure ("inproc://event-loop-receive-auto-" <> Text.pack label <> "-" <> Text.pack (show (hashUnique unique)))

awaitConnection :: IO ()
awaitConnection =
  threadDelay 100000

expectFrames :: String -> Maybe [ByteString] -> Maybe [ByteString] -> IO ()
expectFrames label actual expected =
  assert (actual == expected) (label <> ": expected " <> show expected <> ", got " <> show actual)

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

globalContextMailboxTest :: IO ()
globalContextMailboxTest =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "global-mailbox"
    pull <- unwrap (Zmqx.Pull.open (Zmqx.Pull.defaultOptions <> Zmqx.name "event-loop-recv-global-pull"))
    push <- unwrap (Zmqx.Push.open (Zmqx.Push.defaultOptions <> Zmqx.name "event-loop-recv-global-push"))

    unwrap (Zmqx.bind pull endpoint)
    unwrap (Zmqx.connect push endpoint)
    awaitConnection

    let spec = Zmqx.addReceiver "global" pull (EventLoop.Mailbox 4) Zmqx.emptySpec
    Zmqx.withEventLoop spec \loop -> do
      unwrap (Zmqx.sends push ["global-frame-1", "global-frame-2"])
      received <- unwrap (EventLoop.recv loop "global" 1000)
      expectFrames "global-context mailbox receive" received (Just ["global-frame-1", "global-frame-2"])

explicitContextMailboxTest :: IO ()
explicitContextMailboxTest =
  Zmqx.withContext Zmqx.defaultOptions \context -> do
    endpoint <- uniqueEndpoint "explicit-mailbox"
    pull <- unwrap (Zmqx.openWith context (Zmqx.Pull.defaultOptions <> Zmqx.name "event-loop-recv-explicit-pull"))
    push <- unwrap (Zmqx.openWith context (Zmqx.Push.defaultOptions <> Zmqx.name "event-loop-recv-explicit-push"))

    unwrap (Zmqx.bind pull endpoint)
    unwrap (Zmqx.connect push endpoint)
    awaitConnection

    let spec = Zmqx.addReceiver "explicit" pull (EventLoop.Mailbox 4) Zmqx.emptySpec
    Zmqx.withEventLoopIn context spec \loop -> do
      unwrap (Zmqx.sends push ["explicit-frame-1", "explicit-frame-2"])
      received <- unwrap (EventLoop.recv loop "explicit" 1000)
      expectFrames "explicit-context mailbox receive" received (Just ["explicit-frame-1", "explicit-frame-2"])

callbackDeliveryTest :: IO ()
callbackDeliveryTest =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "callback"
    pull <- unwrap (Zmqx.Pull.open (Zmqx.Pull.defaultOptions <> Zmqx.name "event-loop-recv-callback-pull"))
    push <- unwrap (Zmqx.Push.open (Zmqx.Push.defaultOptions <> Zmqx.name "event-loop-recv-callback-push"))
    callbackResult <- newEmptyMVar

    unwrap (Zmqx.bind pull endpoint)
    unwrap (Zmqx.connect push endpoint)
    awaitConnection

    let spec = Zmqx.addReceiver "callback" pull (EventLoop.Callback (putMVar callbackResult)) Zmqx.emptySpec
    Zmqx.withEventLoop spec \_loop -> do
      unwrap (Zmqx.sends push ["callback-frame-1", "callback-frame-2"])
      frames <- expectMVar "callback delivery" callbackResult
      expectFrames "callback delivery" (Just frames) (Just ["callback-frame-1", "callback-frame-2"])

timeoutMissingAndNonMailboxTest :: IO ()
timeoutMissingAndNonMailboxTest =
  Zmqx.run Zmqx.defaultOptions do
    mailboxEndpoint <- uniqueEndpoint "timeout-mailbox"
    callbackEndpoint <- uniqueEndpoint "non-mailbox-callback"
    mailboxPull <- unwrap (Zmqx.Pull.open (Zmqx.Pull.defaultOptions <> Zmqx.name "event-loop-recv-timeout-pull"))
    callbackPull <- unwrap (Zmqx.Pull.open (Zmqx.Pull.defaultOptions <> Zmqx.name "event-loop-recv-non-mailbox-pull"))

    unwrap (Zmqx.bind mailboxPull mailboxEndpoint)
    unwrap (Zmqx.bind callbackPull callbackEndpoint)

    let spec =
          Zmqx.addReceiver
            "callback"
            callbackPull
            (EventLoop.Callback (\_ -> pure ()))
            (Zmqx.addReceiver "mailbox" mailboxPull (EventLoop.Mailbox 2) Zmqx.emptySpec)
    Zmqx.withEventLoop spec \loop -> do
      timeoutResult <- EventLoop.recv loop "mailbox" 20
      case timeoutResult of
        Right Nothing -> pure ()
        Right (Just frames) -> throwIO (userError ("timeout receive unexpectedly returned frames: " <> show frames))
        Left err -> throwIO err

      missingResult <- EventLoop.recv loop "missing" 0
      expectLeftErrno "missing receiver" Zmqx.ENOENT missingResult

      nonMailboxResult <- EventLoop.recv loop "callback" 0
      expectLeftErrno "callback receiver has no mailbox" Zmqx.EINVAL nonMailboxResult

stoppedRecvTest :: IO ()
stoppedRecvTest =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "stopped"
    pull <- unwrap (Zmqx.Pull.open (Zmqx.Pull.defaultOptions <> Zmqx.name "event-loop-recv-stopped-pull"))
    stoppedResult <- newEmptyMVar

    unwrap (Zmqx.bind pull endpoint)

    let spec = Zmqx.addReceiver "stopped" pull (EventLoop.Mailbox 1) Zmqx.emptySpec
    Zmqx.withEventLoop spec \loop -> do
      _ <- forkIO do
        result <- EventLoop.recv loop "stopped" (-1)
        putMVar stoppedResult result
      threadDelay 10000

    result <- expectMVar "stopped recv" stoppedResult
    expectLeftErrno "stopped recv" Zmqx.ETERM result

main :: IO ()
main =
  Timeout.timeout 10000000 tests >>= \case
    Just () -> pure ()
    Nothing -> throwIO (userError "EventLoopReceiveAuto timed out")
  where
    tests = do
      globalContextMailboxTest
      explicitContextMailboxTest
      callbackDeliveryTest
      timeoutMissingAndNonMailboxTest
      stoppedRecvTest
