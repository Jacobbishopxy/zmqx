{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Data.ByteString.Char8 qualified as ByteString
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Unique (hashUnique, newUnique)
import System.Timeout qualified as Timeout
import Zmqx
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
  pure ("inproc://poll-scaling-auto-" <> Text.pack label <> "-" <> Text.pack (show (hashUnique unique)))

awaitConnection :: IO ()
awaitConnection =
  threadDelay 100000

openPullPush :: String -> IO (Pull, Push)
openPullPush label = do
  endpoint <- uniqueEndpoint label
  pull <- unwrap (Zmqx.Pull.open (name (Text.pack (label <> "-pull"))))
  push <- unwrap (Zmqx.Push.open (name (Text.pack (label <> "-push"))))
  unwrap (bind pull endpoint)
  unwrap (connect push endpoint)
  pure (pull, push)

pollPulls :: [Pull] -> Sockets
pollPulls = \case
  [] -> error "pollPulls requires at least one socket"
  firstPull : remainingPulls -> foldr pollInAlso (pollIn firstPull) remainingPulls

expectIdle :: Sockets -> IO ()
expectIdle sockets =
  pollFor sockets 20 >>= \case
    Right Nothing -> pure ()
    Right (Just _) -> throwIO (userError "poll set reported readiness while all PULL sockets were idle")
    Left err -> throwIO err

awaitReady :: String -> Sockets -> (Ready -> IO Bool) -> IO Ready
awaitReady label sockets accepts =
  loop (20 :: Int)
  where
    loop attempts
      | attempts <= 0 = throwIO (userError (label <> " did not reach the expected readiness state"))
      | otherwise =
          pollFor sockets 100 >>= \case
            Right Nothing -> loop (attempts - 1)
            Right (Just ready) -> do
              accepted <- accepts ready
              if accepted
                then pure ready
                else do
                  threadDelay 50000
                  loop (attempts - 1)
            Left err -> throwIO err

scalingInputReadinessTest :: IO ()
scalingInputReadinessTest = do
  endpoints <- traverse (openPullPush . ("scale-" <>) . show @Int) [1 .. 8]
  awaitConnection
  let pulls = map fst endpoints
      pushes = map snd endpoints
      sockets = pollPulls pulls
      selectedIndex = 3
      selectedPull = pulls !! selectedIndex
      selectedPush = pushes !! selectedIndex

  expectIdle sockets

  unwrap (send selectedPush "one-ready")
  Ready oneReady <- awaitReady "single ready PULL" sockets \(Ready ready) -> do
    let selectedIsReady = ready selectedPull
        othersAreIdle = and [not (ready pull) | (index, pull) <- zip [0 :: Int ..] pulls, index /= selectedIndex]
    pure (selectedIsReady && othersAreIdle)
  assert (oneReady selectedPull) "selected PULL was not ready after its PUSH sent a message"
  for_ (zip [0 :: Int ..] pulls) \(index, pull) ->
    if index == selectedIndex
      then pure ()
      else assert (not (oneReady pull)) ("unselected PULL was unexpectedly ready: " <> show index)
  frame <- unwrap (receive selectedPull)
  assert (frame == ByteString.pack "one-ready") ("single-ready PULL received the wrong frame: " <> show frame)

  for_ (zip [0 :: Int ..] pushes) \(index, push) ->
    unwrap (send push (ByteString.pack ("many-ready-" <> show index)))
  Ready manyReady <- awaitReady "many ready PULLs" sockets \(Ready ready) ->
    pure (all ready pulls)
  for_ pulls \pull ->
    assert (manyReady pull) "a PULL socket with a queued message was not marked ready"
  for_ (zip [0 :: Int ..] pulls) \(index, pull) -> do
    manyFrame <- unwrap (receive pull)
    assert (manyFrame == ByteString.pack ("many-ready-" <> show index)) ("many-ready PULL received the wrong frame: " <> show manyFrame)

mixedInputOutputReadinessTest :: IO ()
mixedInputOutputReadinessTest = do
  (pull, push) <- openPullPush "mixed-in-out"
  awaitConnection
  unwrap (send push "input-ready")
  Ready ready <- awaitReady "mixed POLLIN/POLLOUT" (pollOut push & pollInAlso pull) \(Ready isReady) ->
    pure (isReady push && isReady pull)
  assert (ready push) "PUSH socket was not marked POLLOUT-ready in a mixed poll set"
  assert (ready pull) "PULL socket was not marked POLLIN-ready in a mixed poll set"
  frame <- unwrap (receive pull)
  assert (frame == ByteString.pack "input-ready") ("mixed POLLIN/POLLOUT received the wrong frame: " <> show frame)

mixedReqAndNonReqReadinessTest :: IO ()
mixedReqAndNonReqReadinessTest = do
  reqRepEndpoint <- uniqueEndpoint "mixed-req-rep"
  rep <- unwrap (Zmqx.Rep.open (name "poll-scaling-rep"))
  req <- unwrap (Zmqx.Req.open (name "poll-scaling-req"))
  unwrap (bind rep reqRepEndpoint)
  unwrap (connect req reqRepEndpoint)

  (pull, push) <- openPullPush "mixed-req-pull"
  awaitConnection

  unwrap (send req "request")
  request <- unwrap (receive rep)
  assert (request == ByteString.pack "request") ("REP received the wrong request: " <> show request)
  unwrap (send rep "reply")
  unwrap (send push "pull-ready")

  Ready ready <- awaitReady "mixed REQ/PULL" (pollIn req & pollInAlso pull) \(Ready isReady) ->
    pure (isReady req && isReady pull)
  assert (ready req) "REQ socket with a valid buffered reply was not marked ready"
  assert (ready pull) "non-REQ PULL socket was not marked ready alongside a REQ"

  reply <- unwrap (receives req)
  assert (reply == [ByteString.pack "reply"]) ("REQ received the wrong reply: " <> show reply)
  pullFrame <- unwrap (receive pull)
  assert (pullFrame == ByteString.pack "pull-ready") ("PULL received the wrong frame next to REQ: " <> show pullFrame)

testBody :: IO ()
testBody =
  run defaultOptions do
    scalingInputReadinessTest
    mixedInputOutputReadinessTest
    mixedReqAndNonReqReadinessTest

main :: IO ()
main =
  Timeout.timeout 10000000 testBody >>= \case
    Just () -> pure ()
    Nothing -> throwIO (userError "PollScalingAuto timed out")
