{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (throwIO, try)
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
  pure ("inproc://event-loop-safety-auto-" <> Text.pack label <> "-" <> Text.pack (show (hashUnique unique)))

expectThrownErrno :: String -> Zmqx.Zmq_error -> Either Zmqx.Error a -> IO ()
expectThrownErrno label expectedErrno = \case
  Left err ->
    assert
      (Zmqx.errno err == expectedErrno)
      (label <> ": expected errno " <> show expectedErrno <> ", got " <> show (Zmqx.errno err))
  Right _ -> throwIO (userError (label <> ": expected thrown Error"))

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

duplicateEndpointNameTest :: IO ()
duplicateEndpointNameTest =
  Zmqx.run Zmqx.defaultOptions do
    firstPair <- unwrap (Zmqx.Pair.open (Zmqx.Pair.defaultOptions <> Zmqx.name "event-loop-safety-dup-first-pair"))
    secondPair <- unwrap (Zmqx.Pair.open (Zmqx.Pair.defaultOptions <> Zmqx.name "event-loop-safety-dup-second-pair"))
    pull <- unwrap (Zmqx.Pull.open (Zmqx.Pull.defaultOptions <> Zmqx.name "event-loop-safety-dup-pull"))
    push <- unwrap (Zmqx.Push.open (Zmqx.Push.defaultOptions <> Zmqx.name "event-loop-safety-dup-push"))

    sameRoleOutcome <-
      ( try do
          Zmqx.withEventLoop
            ( Zmqx.addTransceiver
                "duplicate"
                secondPair
                (EventLoop.Mailbox 1)
                (Zmqx.addTransceiver "duplicate" firstPair (EventLoop.Mailbox 1) Zmqx.emptySpec)
            )
            \_ -> pure ()
      )
        :: IO (Either Zmqx.Error ())
    expectThrownErrno "same-role duplicate endpoint" Zmqx.EINVAL sameRoleOutcome

    crossRoleOutcome <-
      ( try do
          Zmqx.withEventLoop
            ( Zmqx.addReceiver
                "shared"
                pull
                (EventLoop.Mailbox 1)
                (Zmqx.addSender "shared" push Zmqx.emptySpec)
            )
            \_ -> pure ()
      )
        :: IO (Either Zmqx.Error ())
    expectThrownErrno "cross-role duplicate endpoint" Zmqx.EINVAL crossRoleOutcome

contextMismatchTest :: IO ()
contextMismatchTest =
  Zmqx.withContext Zmqx.defaultOptions \socketContext -> do
    pair <- unwrap (Zmqx.openWith socketContext (Zmqx.Pair.defaultOptions <> Zmqx.name "event-loop-safety-context-pair"))
    Zmqx.withContext Zmqx.defaultOptions \loopContext -> do
      outcome <-
        ( try do
            Zmqx.withEventLoopIn
              loopContext
              (Zmqx.addTransceiver "mismatch" pair (EventLoop.Mailbox 1) Zmqx.emptySpec)
              \_ -> pure ()
        )
          :: IO (Either Zmqx.Error ())
      expectThrownErrno "transceiver context mismatch" Zmqx.EINVAL outcome

shutdownUnblocksPendingRecvTest :: IO ()
shutdownUnblocksPendingRecvTest =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "shutdown"
    pull <- unwrap (Zmqx.Pull.open (Zmqx.Pull.defaultOptions <> Zmqx.name "event-loop-safety-shutdown-pull"))
    stoppedResult <- newEmptyMVar

    unwrap (Zmqx.bind pull endpoint)

    let spec = Zmqx.addReceiver "pending" pull (EventLoop.Mailbox 1) Zmqx.emptySpec
    Zmqx.withEventLoop spec \loop -> do
      _ <- forkIO do
        result <- EventLoop.recv loop "pending" (-1)
        putMVar stoppedResult result
      threadDelay 10000

    result <- expectMVar "shutdown unblocks pending recv" stoppedResult
    expectLeftErrno "shutdown unblocks pending recv" Zmqx.ETERM result

blockedSendShutdownTest :: IO ()
blockedSendShutdownTest =
  Zmqx.run Zmqx.defaultOptions do
    push <- unwrap (Zmqx.Push.open (Zmqx.Push.defaultOptions <> Zmqx.name "event-loop-safety-blocked-push"))
    sendResult <- newEmptyMVar

    completed <-
      Timeout.timeout 2000000 do
        Zmqx.withEventLoop (Zmqx.addSender "blocked" push Zmqx.emptySpec) \loop -> do
          _ <- forkIO do
            result <- EventLoop.send loop "blocked" "never-delivered"
            putMVar sendResult result
          threadDelay 100000
    case completed of
      Nothing -> throwIO (userError "blocked send shutdown timed out")
      Just () -> pure ()

    result <- expectMVar "shutdown unblocks blocked send" sendResult
    expectLeftErrno "shutdown unblocks blocked send" Zmqx.ETERM result

main :: IO ()
main = do
  duplicateEndpointNameTest
  contextMismatchTest
  shutdownUnblocksPendingRecvTest
  blockedSendShutdownTest
