module Main where

import Common (unwrap)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (throwIO)
import Data.ByteString.Char8 qualified as BS
import Zmqx qualified
import Zmqx.Rep qualified
import Zmqx.Req qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

main :: IO ()
main =
  Zmqx.run Zmqx.defaultOptions do
    -- Req.open enables ZMQ_REQ_RELAXED and ZMQ_REQ_CORRELATE by default. This
    -- scenario verifies that receivesFor respects that mode: stale replies are
    -- ignored, the valid reply is surfaced once, and the buffer is then empty.
    let endpoint = "inproc://req-poll-stale-reply"

    rep <- unwrap (Zmqx.Rep.open (Zmqx.name "rep"))
    req <- unwrap (Zmqx.Req.open (Zmqx.name "req"))

    unwrap (Zmqx.bind rep endpoint)
    unwrap (Zmqx.connect req endpoint)

    unwrap (Zmqx.send req "request-1")
    firstRequest <- unwrap (Zmqx.receive rep)
    assert (firstRequest == BS.pack "request-1") "REP did not receive the first request"

    unwrap (Zmqx.send req "request-2")
    unwrap (Zmqx.send rep "reply-1")

    staleReplyResult <- Zmqx.receivesFor req 50
    case staleReplyResult of
      Right Nothing -> pure ()
      Right (Just reply) ->
        throwIO (userError ("REQ unexpectedly accepted a stale reply: " <> show reply))
      Left err -> throwIO err

    secondRequest <- unwrap (Zmqx.receive rep)
    assert (secondRequest == BS.pack "request-2") "REP did not receive the second request"

    unwrap (Zmqx.send rep "reply-2")

    validReplyResult <- Zmqx.receivesFor req 1000
    case validReplyResult of
      Right (Just [reply]) ->
        assert (reply == BS.pack "reply-2") "REQ did not receive the expected reply to the second request"
      Right (Just reply) ->
        throwIO (userError ("REQ received an unexpected multipart reply: " <> show reply))
      Right Nothing ->
        throwIO (userError "REQ timed out waiting for the valid second reply")
      Left err -> throwIO err

    duplicateReplyResult <- Zmqx.receivesFor req 0
    case duplicateReplyResult of
      Right Nothing -> pure ()
      Right (Just reply) ->
        throwIO (userError ("REQ surfaced the same reply more than once: " <> show reply))
      Left err -> throwIO err

    let delayedEndpoint = "inproc://req-poll-stale-then-valid"

    rep2 <- unwrap (Zmqx.Rep.open (Zmqx.name "rep-delayed"))
    req2 <- unwrap (Zmqx.Req.open (Zmqx.name "req-delayed"))

    unwrap (Zmqx.bind rep2 delayedEndpoint)
    unwrap (Zmqx.connect req2 delayedEndpoint)

    unwrap (Zmqx.send req2 "request-a")
    firstDelayedRequest <- unwrap (Zmqx.receive rep2)
    assert (firstDelayedRequest == BS.pack "request-a") "REP did not receive the first delayed request"

    unwrap (Zmqx.send req2 "request-b")
    unwrap (Zmqx.send rep2 "reply-a")

    repThreadDone <- newEmptyMVar
    _ <- forkIO do
      secondDelayedRequest <- unwrap (Zmqx.receive rep2)
      if secondDelayedRequest == BS.pack "request-b"
        then do
          threadDelay 100000
          unwrap (Zmqx.send rep2 "reply-b")
          putMVar repThreadDone Nothing
        else
          putMVar repThreadDone (Just ("REP received the wrong second delayed request: " <> show secondDelayedRequest))

    delayedReplyResult <- Zmqx.receivesFor req2 1000
    case delayedReplyResult of
      Right (Just [reply]) ->
        assert (reply == BS.pack "reply-b") "REQ did not keep polling until the valid delayed reply arrived"
      Right (Just reply) ->
        throwIO (userError ("REQ received an unexpected delayed multipart reply: " <> show reply))
      Right Nothing ->
        throwIO (userError "REQ returned timeout even though the valid delayed reply arrived before the deadline")
      Left err -> throwIO err

    takeMVar repThreadDone >>= \case
      Nothing -> pure ()
      Just message -> throwIO (userError message)

    let directPollEndpoint = "inproc://req-poll-direct-probe"

    rep3 <- unwrap (Zmqx.Rep.open (Zmqx.name "rep-direct-poll"))
    req3 <- unwrap (Zmqx.Req.open (Zmqx.name "req-direct-poll"))

    unwrap (Zmqx.bind rep3 directPollEndpoint)
    unwrap (Zmqx.connect req3 directPollEndpoint)

    unwrap (Zmqx.send req3 "request-x")
    firstPollRequest <- unwrap (Zmqx.receive rep3)
    assert (firstPollRequest == BS.pack "request-x") "REP did not receive the first direct-poll request"

    unwrap (Zmqx.send req3 "request-y")
    unwrap (Zmqx.send rep3 "reply-x")

    directPollDone <- newEmptyMVar
    _ <- forkIO do
      secondPollRequest <- unwrap (Zmqx.receive rep3)
      if secondPollRequest == BS.pack "request-y"
        then do
          threadDelay 100000
          unwrap (Zmqx.send rep3 "reply-y")
          putMVar directPollDone Nothing
        else
          putMVar directPollDone (Just ("REP received the wrong second direct-poll request: " <> show secondPollRequest))

    directPollResult <- Zmqx.pollFor (Zmqx.pollIn req3) 1000
    case directPollResult of
      Right (Just (Zmqx.Ready isReady)) -> do
        assert (isReady req3) "REQ pollFor returned a Ready set that did not actually include the REQ socket"
        directReply <- unwrap (Zmqx.receives req3)
        case directReply of
          [reply] ->
            assert (reply == BS.pack "reply-y") "REQ pollFor surfaced the wrong buffered reply"
          reply ->
            throwIO (userError ("REQ pollFor surfaced an unexpected multipart reply: " <> show reply))
      Right Nothing ->
        throwIO (userError "REQ pollFor timed out even though the valid delayed reply arrived before the deadline")
      Left err -> throwIO err

    duplicateDirectReply <- Zmqx.receivesFor req3 0
    case duplicateDirectReply of
      Right Nothing -> pure ()
      Right (Just reply) ->
        throwIO (userError ("REQ pollFor surfaced the same buffered reply more than once: " <> show reply))
      Left err -> throwIO err

    takeMVar directPollDone >>= \case
      Nothing -> pure ()
      Just message -> throwIO (userError message)
