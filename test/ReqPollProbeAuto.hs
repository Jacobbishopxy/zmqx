{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Concurrent (MVar, forkIO, newEmptyMVar, threadDelay, putMVar, takeMVar)
import Control.Exception (SomeException, throwIO, try)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Unique (hashUnique, newUnique)
import System.Timeout qualified as Timeout
import Zmqx qualified
import Zmqx.Pull qualified
import Zmqx.Push qualified
import Zmqx.Rep qualified
import Zmqx.Req qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

uniqueEndpoint :: String -> IO Text
uniqueEndpoint label = do
  unique <- newUnique
  pure ("inproc://req-poll-probe-auto-" <> Text.pack label <> "-" <> Text.pack (show (hashUnique unique)))

awaitConnection :: IO ()
awaitConnection =
  threadDelay 100000

forkDelayedReply :: Zmqx.Rep.Rep -> ByteString -> Int -> IO (MVar (Either SomeException ()))
forkDelayedReply rep reply delayUs = do
  done <- newEmptyMVar
  _ <- forkIO do
    result <- try do
      threadDelay delayUs
      unwrap (Zmqx.send rep reply)
    putMVar done result
  pure done

assertThreadSucceeded :: MVar (Either SomeException ()) -> IO ()
assertThreadSucceeded done =
  takeMVar done >>= \case
    Right () -> pure ()
    Left err -> throwIO err

testIdleTimeout :: IO ()
testIdleTimeout = do
  endpoint <- uniqueEndpoint "idle"
  rep <- unwrap (Zmqx.Rep.open (Zmqx.name "req-poll-probe-idle-rep"))
  req <- unwrap (Zmqx.Req.open (Zmqx.name "req-poll-probe-idle-req"))
  unwrap (Zmqx.bind rep endpoint)
  unwrap (Zmqx.connect req endpoint)
  awaitConnection

  unwrap (Zmqx.send req "idle-request")
  request <- unwrap (Zmqx.receive rep)
  assert (request == BS.pack "idle-request") "REP did not receive idle timeout request"

  start <- getCurrentTime
  result <- Zmqx.pollFor (Zmqx.pollIn req) 50
  finish <- getCurrentTime
  case result of
    Right Nothing -> pure ()
    Right (Just (Zmqx.Ready ready)) ->
      assert (not (ready req)) "idle REQ poll unexpectedly reported the unanswered REQ ready"
    Left err -> throwIO err

  let elapsedMs = realToFrac (diffUTCTime finish start) * 1000.0 :: Double
  assert (elapsedMs >= 35.0) ("idle REQ poll returned too early: " <> show elapsedMs <> "ms")
  assert (elapsedMs < 500.0) ("idle REQ poll exceeded loose timeout bound: " <> show elapsedMs <> "ms")

testValidReplyReadiness :: IO ()
testValidReplyReadiness = do
  endpoint <- uniqueEndpoint "valid"
  rep <- unwrap (Zmqx.Rep.open (Zmqx.name "req-poll-probe-valid-rep"))
  req <- unwrap (Zmqx.Req.open (Zmqx.name "req-poll-probe-valid-req"))
  unwrap (Zmqx.bind rep endpoint)
  unwrap (Zmqx.connect req endpoint)
  awaitConnection

  unwrap (Zmqx.send req "valid-request")
  request <- unwrap (Zmqx.receive rep)
  assert (request == BS.pack "valid-request") "REP did not receive valid readiness request"

  replyDone <- forkDelayedReply rep "valid-reply" 20000
  Zmqx.pollFor (Zmqx.pollIn req) 1000 >>= \case
    Right (Just (Zmqx.Ready ready)) -> assert (ready req) "valid REQ reply did not mark the REQ ready"
    Right Nothing -> throwIO (userError "valid REQ reply did not wake pollFor before the deadline")
    Left err -> throwIO err

  reply <- unwrap (Zmqx.receives req)
  assert (reply == [BS.pack "valid-reply"]) ("REQ did not drain the buffered valid reply: " <> show reply)
  assertThreadSucceeded replyDone

  Zmqx.receivesFor req 0 >>= \case
    Right Nothing -> pure ()
    Right (Just duplicate) -> throwIO (userError ("REQ surfaced the buffered reply twice: " <> show duplicate))
    Left err -> throwIO err

testMixedReqAndPullPoll :: IO ()
testMixedReqAndPullPoll = do
  reqEndpoint <- uniqueEndpoint "mixed-req"
  pullEndpoint <- uniqueEndpoint "mixed-pull"
  rep <- unwrap (Zmqx.Rep.open (Zmqx.name "req-poll-probe-mixed-rep"))
  req <- unwrap (Zmqx.Req.open (Zmqx.name "req-poll-probe-mixed-req"))
  pull <- unwrap (Zmqx.Pull.open (Zmqx.name "req-poll-probe-mixed-pull"))
  push <- unwrap (Zmqx.Push.open (Zmqx.name "req-poll-probe-mixed-push"))
  unwrap (Zmqx.bind rep reqEndpoint)
  unwrap (Zmqx.connect req reqEndpoint)
  unwrap (Zmqx.bind pull pullEndpoint)
  unwrap (Zmqx.connect push pullEndpoint)
  awaitConnection

  unwrap (Zmqx.send req "mixed-request")
  request <- unwrap (Zmqx.receive rep)
  assert (request == BS.pack "mixed-request") "REP did not receive mixed poll request"

  let mixedPollSet = Zmqx.pollInAlso pull (Zmqx.pollIn req)
  unwrap (Zmqx.send push "mixed-pull-message")
  Zmqx.pollFor mixedPollSet 1000 >>= \case
    Right (Just (Zmqx.Ready ready)) -> do
      assert (ready pull) "mixed poll did not report the non-REQ socket ready"
      assert (not (ready req)) "mixed poll reported the unanswered REQ ready"
    Right Nothing -> throwIO (userError "mixed poll timed out before the non-REQ message was ready")
    Left err -> throwIO err
  pullMessage <- unwrap (Zmqx.receive pull)
  assert (pullMessage == BS.pack "mixed-pull-message") "PULL received the wrong mixed poll message"

  unwrap (Zmqx.send rep "mixed-reply")
  Zmqx.pollFor mixedPollSet 1000 >>= \case
    Right (Just (Zmqx.Ready ready)) -> assert (ready req) "mixed poll did not report the valid REQ reply ready"
    Right Nothing -> throwIO (userError "mixed poll timed out before the valid REQ reply was ready")
    Left err -> throwIO err
  reqReply <- unwrap (Zmqx.receives req)
  assert (reqReply == [BS.pack "mixed-reply"]) ("REQ received the wrong mixed reply: " <> show reqReply)

testBody :: IO ()
testBody =
  Zmqx.run Zmqx.defaultOptions do
    testIdleTimeout
    testValidReplyReadiness
    testMixedReqAndPullPoll

main :: IO ()
main =
  Timeout.timeout 5000000 testBody >>= \case
    Just () -> pure ()
    Nothing -> throwIO (userError "ReqPollProbeAuto timed out")
