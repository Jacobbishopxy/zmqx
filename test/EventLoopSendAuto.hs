{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, throwIO, try)
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.Foldable (for_, traverse_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Unique (hashUnique, newUnique)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Timeout qualified as Timeout
import Zmqx qualified
import Zmqx.EventLoop qualified as EventLoop
import Zmqx.Pull qualified
import Zmqx.Push qualified
import Zmqx.Rep qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

expectFrames :: String -> Maybe [ByteString] -> Maybe [ByteString] -> IO ()
expectFrames label actual expected =
  assert (actual == expected) (label <> ": expected " <> show expected <> ", got " <> show actual)

expectLeft :: String -> Either Zmqx.Error a -> IO ()
expectLeft label = \case
  Left _ -> pure ()
  Right _ -> throwIO (userError (label <> ": expected Left Error"))

expectCompletes :: String -> IO a -> IO a
expectCompletes label action =
  Timeout.timeout 2000000 action >>= \case
    Nothing -> throwIO (userError (label <> ": timed out"))
    Just result -> pure result

type SendAttempt = Either Zmqx.Error (Either Zmqx.Error ())

expectRightUnit :: String -> SendAttempt -> IO ()
expectRightUnit label = \case
  Right (Right ()) -> pure ()
  other -> throwIO (userError (label <> ": expected Right (Right ()), got " <> show other))

expectMaybeRightUnit :: String -> Maybe SendAttempt -> IO ()
expectMaybeRightUnit label = \case
  Just attempt -> expectRightUnit label attempt
  Nothing -> throwIO (userError (label <> ": timed out"))

expectThrownAttempt :: String -> Maybe SendAttempt -> IO ()
expectThrownAttempt label = \case
  Just (Left _) -> pure ()
  Just other -> throwIO (userError (label <> ": expected thrown Error, got " <> show other))
  Nothing -> throwIO (userError (label <> ": timed out"))

expectRecordedFrames :: String -> Maybe (Either Zmqx.Error (Maybe [ByteString])) -> Maybe [ByteString] -> IO ()
expectRecordedFrames label recorded expected =
  case recorded of
    Just (Right actual) -> expectFrames label actual expected
    Just (Left err) -> throwIO (userError (label <> ": receive failed with " <> show err))
    Nothing -> throwIO (userError (label <> ": receive did not run"))

eventLoopReplyDelayEnv :: String
eventLoopReplyDelayEnv =
  "ZMQX_EVENT_LOOP_TEST_DELAY_AFTER_EMPTY_REPLY_US"

withReplyDelayHook :: IO a -> IO a
withReplyDelayHook action =
  bracket (lookupEnv eventLoopReplyDelayEnv) restoreEnv \_ -> do
    setEnv eventLoopReplyDelayEnv "200000"
    action
  where
    restoreEnv = \case
      Nothing -> unsetEnv eventLoopReplyDelayEnv
      Just value -> setEnv eventLoopReplyDelayEnv value

uniqueEndpoint :: String -> IO Text
uniqueEndpoint label = do
  unique <- newUnique
  pure ("inproc://event-loop-send-auto-" <> Text.pack label <> "-" <> Text.pack (show (hashUnique unique)))

globalContextSenderTest :: IO ()
globalContextSenderTest =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "global"
    pull <- unwrap (Zmqx.Pull.open (Zmqx.Pull.defaultOptions <> Zmqx.name "event-loop-global-pull"))
    push <- unwrap (Zmqx.Push.open (Zmqx.Push.defaultOptions <> Zmqx.name "event-loop-global-push"))

    unwrap (Zmqx.bind pull endpoint)
    unwrap (Zmqx.connect push endpoint)

    let spec = Zmqx.addSender "global" push Zmqx.emptySpec
    Zmqx.withEventLoop spec \loop -> do
      unwrap (EventLoop.send loop "global" "global-payload")
      received <- unwrap (Zmqx.receivesFor pull 1000)
      expectFrames "global-context event-loop send" received (Just ["global-payload"])

explicitContextSenderTest :: IO ()
explicitContextSenderTest =
  Zmqx.withContext Zmqx.defaultOptions \context -> do
    endpoint <- uniqueEndpoint "explicit"
    pull <- unwrap (Zmqx.openWith context (Zmqx.Pull.defaultOptions <> Zmqx.name "event-loop-explicit-pull"))
    push <- unwrap (Zmqx.openWith context (Zmqx.Push.defaultOptions <> Zmqx.name "event-loop-explicit-push"))

    unwrap (Zmqx.bind pull endpoint)
    unwrap (Zmqx.connect push endpoint)

    let spec = Zmqx.addSender "explicit" push Zmqx.emptySpec
    Zmqx.withEventLoopIn context spec \loop -> do
      unwrap (EventLoop.send loop "explicit" "explicit-payload")
      received <- unwrap (Zmqx.receivesFor pull 1000)
      expectFrames "explicit-context event-loop send" received (Just ["explicit-payload"])

missingAndStoppedTest :: IO ()
missingAndStoppedTest =
  Zmqx.run Zmqx.defaultOptions do
    push <- unwrap (Zmqx.Push.open (Zmqx.Push.defaultOptions <> Zmqx.name "event-loop-stopped-push"))
    loopRef <- newIORef Nothing

    let spec = Zmqx.addSender "registered" push Zmqx.emptySpec
    Zmqx.withEventLoop spec \loop -> do
      missing <- EventLoop.send loop "missing" "payload"
      expectLeft "missing event-loop sender" missing
      writeIORef loopRef (Just loop)

    readIORef loopRef >>= \case
      Nothing -> throwIO (userError "stopped event-loop handle was not captured")
      Just loop -> do
        stopped <- EventLoop.send loop "registered" "after-stop"
        expectLeft "stopped event loop" stopped

throwingSendDoesNotHangTest :: IO ()
throwingSendDoesNotHangTest =
  Zmqx.run Zmqx.defaultOptions do
    rep <- unwrap (Zmqx.Rep.open (Zmqx.Rep.defaultOptions <> Zmqx.name "event-loop-throwing-rep"))
    let spec = Zmqx.addSender "rep" rep Zmqx.emptySpec
    outcome <-
      expectCompletes "throwing event-loop send cleanup" $
        ( try do
            Zmqx.withEventLoop spec \loop -> do
              _ <- EventLoop.send loop "rep" "illegal-rep-send"
              pure ()
        )
          :: IO (Either Zmqx.Error ())
    case outcome of
      Left _ -> pure ()
      Right () -> throwIO (userError "throwing event-loop send unexpectedly succeeded")

queuedFailureRaceTest :: IO ()
queuedFailureRaceTest =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "queued-race"
    gateEndpoint <- uniqueEndpoint "queued-race-gate"
    pull <- unwrap (Zmqx.Pull.open (Zmqx.Pull.defaultOptions <> Zmqx.name "event-loop-race-pull"))
    gatePull <- unwrap (Zmqx.Pull.open (Zmqx.Pull.defaultOptions <> Zmqx.name "event-loop-race-gate-pull"))
    push <- unwrap (Zmqx.Push.open (Zmqx.Push.defaultOptions <> Zmqx.name "event-loop-race-push"))
    gatePush <- unwrap (Zmqx.Push.open (Zmqx.Push.defaultOptions <> Zmqx.name "event-loop-race-gate-push"))
    rep <- unwrap (Zmqx.Rep.open (Zmqx.Rep.defaultOptions <> Zmqx.name "event-loop-race-rep"))

    unwrap (Zmqx.bind pull endpoint)
    unwrap (Zmqx.connect push endpoint)
    unwrap (Zmqx.bind gatePush gateEndpoint)

    gateResultRef <- newIORef (Nothing :: Maybe SendAttempt)
    acceptedResultRef <- newIORef (Nothing :: Maybe SendAttempt)
    gateReceivedRef <- newIORef (Nothing :: Maybe (Either Zmqx.Error (Maybe [ByteString])))
    receivedRef <- newIORef (Nothing :: Maybe (Either Zmqx.Error (Maybe [ByteString])))
    queuedResultsRef <- newIORef (Nothing :: Maybe [Maybe SendAttempt])

    let spec =
          Zmqx.addSender
            "rep"
            rep
            (Zmqx.addSender "push" push (Zmqx.addSender "gate" gatePush Zmqx.emptySpec))
    outcome <-
      withReplyDelayHook $
        expectCompletes "queued event-loop send failure race cleanup" $
          ( try do
              Zmqx.withEventLoop spec \loop -> do
                gateDone <- newEmptyMVar
                acceptedDone <- newEmptyMVar
                _ <- forkIO do
                  result <- try (EventLoop.send loop "gate" "gate-release") :: IO SendAttempt
                  putMVar gateDone result

                threadDelay 10000
                _ <- forkIO do
                  result <- try (EventLoop.send loop "push" "accepted-during-failure") :: IO SendAttempt
                  putMVar acceptedDone result

                threadDelay 10000
                completions <- replicateM 16 newEmptyMVar
                for_ completions \done -> do
                  _ <- forkIO do
                    result <- try (EventLoop.send loop "rep" "queued-illegal-send") :: IO SendAttempt
                    putMVar done result
                  pure ()

                unwrap (Zmqx.connect gatePull gateEndpoint)

                gateResult <- Timeout.timeout 2000000 (takeMVar gateDone)
                writeIORef gateResultRef gateResult
                acceptedResult <- Timeout.timeout 2000000 (takeMVar acceptedDone)
                writeIORef acceptedResultRef acceptedResult

                gateReceived <- Zmqx.receivesFor gatePull 1000
                writeIORef gateReceivedRef (Just gateReceived)
                received <- Zmqx.receivesFor pull 1000
                writeIORef receivedRef (Just received)

                results <- traverse (Timeout.timeout 2000000 . takeMVar) completions
                writeIORef queuedResultsRef (Just results)
          )
            :: IO (Either Zmqx.Error ())
    case outcome of
      Left _ -> pure ()
      Right () -> throwIO (userError "queued event-loop failure unexpectedly completed without worker failure")

    gateResult <- readIORef gateResultRef
    expectMaybeRightUnit "gate push send during worker failure" gateResult
    acceptedResult <- readIORef acceptedResultRef
    expectMaybeRightUnit "accepted push send during worker failure" acceptedResult
    gateReceived <- readIORef gateReceivedRef
    expectRecordedFrames "gate send during worker failure" gateReceived (Just ["gate-release"])
    received <- readIORef receivedRef
    expectRecordedFrames "accepted send during worker failure" received (Just ["accepted-during-failure"])
    queuedResults <- readIORef queuedResultsRef
    case queuedResults of
      Nothing -> throwIO (userError "queued throwing sends did not run")
      Just results -> do
        assert (all isJust results) "queued throwing sends did not all complete"
        traverse_ (expectThrownAttempt "queued throwing send observes worker failure") results

main :: IO ()
main = do
  globalContextSenderTest
  explicitContextSenderTest
  missingAndStoppedTest
  throwingSendDoesNotHangTest
  queuedFailureRaceTest
