{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Unique (hashUnique, newUnique)
import Zmqx qualified
import Zmqx.EventLoop qualified as EventLoop
import Zmqx.Pair qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

uniqueEndpoint :: String -> IO Text
uniqueEndpoint label = do
  unique <- newUnique
  pure ("inproc://event-loop-transceiver-auto-" <> Text.pack label <> "-" <> Text.pack (show (hashUnique unique)))

awaitConnection :: IO ()
awaitConnection =
  threadDelay 100000

expectFrames :: String -> Maybe [ByteString] -> Maybe [ByteString] -> IO ()
expectFrames label actual expected =
  assert (actual == expected) (label <> ": expected " <> show expected <> ", got " <> show actual)

globalContextTransceiverRoundTripTest :: IO ()
globalContextTransceiverRoundTripTest =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "global"
    loopPair <- unwrap (Zmqx.Pair.open (Zmqx.Pair.defaultOptions <> Zmqx.name "event-loop-transceiver-global-loop"))
    peerPair <- unwrap (Zmqx.Pair.open (Zmqx.Pair.defaultOptions <> Zmqx.name "event-loop-transceiver-global-peer"))

    unwrap (Zmqx.bind loopPair endpoint)
    unwrap (Zmqx.connect peerPair endpoint)
    awaitConnection

    let spec = Zmqx.addTransceiver "pair" loopPair (EventLoop.Mailbox 4) Zmqx.emptySpec
    Zmqx.withEventLoop spec \loop -> do
      unwrap (Zmqx.sends peerPair ["incoming-frame-1", "incoming-frame-2"])
      inbound <- unwrap (EventLoop.recv loop "pair" 1000)
      expectFrames "global transceiver inbound multipart" inbound (Just ["incoming-frame-1", "incoming-frame-2"])

      unwrap (EventLoop.send loop "pair" "outbound-frame")
      outbound <- unwrap (Zmqx.receivesFor peerPair 1000)
      expectFrames "global transceiver outbound send" outbound (Just ["outbound-frame"])

explicitContextTransceiverRoundTripTest :: IO ()
explicitContextTransceiverRoundTripTest =
  Zmqx.withContext Zmqx.defaultOptions \context -> do
    endpoint <- uniqueEndpoint "explicit"
    loopPair <- unwrap (Zmqx.openWith context (Zmqx.Pair.defaultOptions <> Zmqx.name "event-loop-transceiver-explicit-loop"))
    peerPair <- unwrap (Zmqx.openWith context (Zmqx.Pair.defaultOptions <> Zmqx.name "event-loop-transceiver-explicit-peer"))

    unwrap (Zmqx.bind loopPair endpoint)
    unwrap (Zmqx.connect peerPair endpoint)
    awaitConnection

    let spec = Zmqx.addTransceiver "pair" loopPair (EventLoop.Mailbox 4) Zmqx.emptySpec
    Zmqx.withEventLoopIn context spec \loop -> do
      unwrap (Zmqx.sends peerPair ["explicit-incoming-1", "explicit-incoming-2"])
      inbound <- unwrap (EventLoop.recv loop "pair" 1000)
      expectFrames "explicit transceiver inbound multipart" inbound (Just ["explicit-incoming-1", "explicit-incoming-2"])

      unwrap (EventLoop.send loop "pair" "explicit-outbound")
      outbound <- unwrap (Zmqx.receivesFor peerPair 1000)
      expectFrames "explicit transceiver outbound send" outbound (Just ["explicit-outbound"])

main :: IO ()
main = do
  globalContextTransceiverRoundTripTest
  explicitContextTransceiverRoundTripTest
