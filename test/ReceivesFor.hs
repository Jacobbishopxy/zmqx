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
import Zmqx.Dealer qualified
import Zmqx.Pair qualified
import Zmqx.Pub qualified
import Zmqx.Pull qualified
import Zmqx.Push qualified
import Zmqx.Rep qualified
import Zmqx.Req qualified
import Zmqx.Router qualified
import Zmqx.Sub qualified
import Zmqx.XPub qualified
import Zmqx.XSub qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

assertReceives :: Show a => (Eq a) => a -> a -> String -> IO ()
assertReceives actual expected message =
  assert (actual == expected) (message <> ": expected " <> show expected <> ", got " <> show actual)

uniqueEndpoint :: String -> IO Text
uniqueEndpoint label = do
  unique <- newUnique
  pure ("inproc://recv-for-" <> Text.pack label <> "-" <> Text.pack (show (hashUnique unique)))

assertTimeout :: IO (Either Zmqx.Error (Maybe [ByteString])) -> String -> IO ()
assertTimeout action message =
  action >>= \case
    Right Nothing -> pure ()
    Right (Just frames) -> throwIO (userError (message <> ": unexpectedly received " <> show frames))
    Left err -> throwIO err

expectFrames :: IO (Either Zmqx.Error (Maybe [ByteString])) -> String -> IO [ByteString]
expectFrames action message =
  action >>= \case
    Right (Just frames) -> pure frames
    Right Nothing -> throwIO (userError (message <> ": timed out"))
    Left err -> throwIO err

awaitConnection :: IO ()
awaitConnection =
  threadDelay 100000

testPair :: IO ()
testPair =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "pair"
    pair1 <- unwrap (Zmqx.Pair.open (Zmqx.name "pair1"))
    pair2 <- unwrap (Zmqx.Pair.open (Zmqx.name "pair2"))

    unwrap (Zmqx.bind pair1 endpoint)
    unwrap (Zmqx.connect pair2 endpoint)
    awaitConnection

    assertTimeout (Zmqx.receivesFor pair1 20) "PAIR should time out before any message is sent"

    unwrap (Zmqx.send pair2 "pair-message")
    frames <- expectFrames (Zmqx.receivesFor pair1 1000) "PAIR did not receive its message"
    assertReceives frames ["pair-message"] "PAIR received the wrong frames"

testPull :: IO ()
testPull =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "pull"
    pull <- unwrap (Zmqx.Pull.open (Zmqx.name "pull"))
    push <- unwrap (Zmqx.Push.open (Zmqx.name "push"))

    unwrap (Zmqx.bind pull endpoint)
    unwrap (Zmqx.connect push endpoint)
    awaitConnection

    assertTimeout (Zmqx.receivesFor pull 20) "PULL should time out before any message is sent"

    unwrap (Zmqx.send push "pull-message")
    frames <- expectFrames (Zmqx.receivesFor pull 1000) "PULL did not receive its message"
    assertReceives frames ["pull-message"] "PULL received the wrong frames"

testDealer :: IO ()
testDealer =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "dealer"
    dealer1 <- unwrap (Zmqx.Dealer.open (Zmqx.name "dealer1"))
    dealer2 <- unwrap (Zmqx.Dealer.open (Zmqx.name "dealer2"))

    unwrap (Zmqx.bind dealer1 endpoint)
    unwrap (Zmqx.connect dealer2 endpoint)
    awaitConnection

    assertTimeout (Zmqx.receivesFor dealer1 20) "DEALER should time out before any message is sent"

    unwrap (Zmqx.send dealer2 "dealer-message")
    frames <- expectFrames (Zmqx.receivesFor dealer1 1000) "DEALER did not receive its message"
    assertReceives frames ["dealer-message"] "DEALER received the wrong frames"

testRepReq :: IO ()
testRepReq =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "rep-req"
    rep <- unwrap (Zmqx.Rep.open (Zmqx.name "rep"))
    req <- unwrap (Zmqx.Req.open (Zmqx.name "req"))

    unwrap (Zmqx.bind rep endpoint)
    unwrap (Zmqx.connect req endpoint)
    awaitConnection

    assertTimeout (Zmqx.receivesFor rep 20) "REP should time out before the REQ sends a request"

    unwrap (Zmqx.send req "request-message")
    requestFrames <- expectFrames (Zmqx.receivesFor rep 1000) "REP did not receive the request"
    assertReceives requestFrames ["request-message"] "REP received the wrong request frames"

    unwrap (Zmqx.send rep "reply-message")
    replyFrames <- expectFrames (Zmqx.receivesFor req 1000) "REQ did not receive the reply"
    assertReceives replyFrames ["reply-message"] "REQ received the wrong reply frames"

testRouter :: IO ()
testRouter =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "router"
    router <- unwrap (Zmqx.Router.open (Zmqx.name "router"))
    dealer <- unwrap (Zmqx.Dealer.open (Zmqx.name "dealer"))

    unwrap (Zmqx.bind router endpoint)
    unwrap (Zmqx.connect dealer endpoint)
    awaitConnection

    assertTimeout (Zmqx.receivesFor router 20) "ROUTER should time out before any message is sent"

    unwrap (Zmqx.send dealer "router-message")
    frames <- expectFrames (Zmqx.receivesFor router 1000) "ROUTER did not receive its message"
    assert (length frames >= 2) ("ROUTER expected an identity frame and payload, got " <> show frames)
    assertReceives (last frames) "router-message" "ROUTER received the wrong payload frame"

testSub :: IO ()
testSub =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "sub"
    pub <- unwrap (Zmqx.Pub.open (Zmqx.name "pub"))
    sub <- unwrap (Zmqx.Sub.open (Zmqx.name "sub"))

    unwrap (Zmqx.bind pub endpoint)
    unwrap (Zmqx.connect sub endpoint)
    unwrap (Zmqx.Sub.subscribe sub "")
    awaitConnection
    awaitConnection

    assertTimeout (Zmqx.receivesFor sub 20) "SUB should time out before any publication is sent"

    unwrap (Zmqx.send pub "sub-message")
    frames <- expectFrames (Zmqx.receivesFor sub 1000) "SUB did not receive its publication"
    assertReceives frames ["sub-message"] "SUB received the wrong frames"

testXPub :: IO ()
testXPub =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "xpub"
    xpub <- unwrap (Zmqx.XPub.open (Zmqx.name "xpub"))
    xsub <- unwrap (Zmqx.XSub.open (Zmqx.name "xsub"))

    unwrap (Zmqx.bind xpub endpoint)
    unwrap (Zmqx.connect xsub endpoint)
    awaitConnection

    assertTimeout (Zmqx.receivesFor xpub 20) "XPUB should time out before any subscription is sent"

    unwrap (Zmqx.XSub.subscribe xsub "topic")
    frames <- expectFrames (Zmqx.receivesFor xpub 1000) "XPUB did not receive the subscription"
    assertReceives frames [Zmqx.Subscribe "topic"] "XPUB received the wrong subscription frame"

testXSub :: IO ()
testXSub =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "xsub"
    xpub <- unwrap (Zmqx.XPub.open (Zmqx.name "xpub"))
    xsub <- unwrap (Zmqx.XSub.open (Zmqx.name "xsub"))

    unwrap (Zmqx.bind xpub endpoint)
    unwrap (Zmqx.connect xsub endpoint)
    awaitConnection

    unwrap (Zmqx.XSub.subscribe xsub "")
    subscriptionFrames <- expectFrames (Zmqx.receivesFor xpub 1000) "XPUB did not observe the XSUB subscription"
    assertReceives subscriptionFrames [Zmqx.Subscribe ""] "XPUB observed the wrong XSUB subscription"

    assertTimeout (Zmqx.receivesFor xsub 20) "XSUB should time out before any publication is sent"

    unwrap (Zmqx.send xpub "xsub-message")
    frames <- expectFrames (Zmqx.receivesFor xsub 1000) "XSUB did not receive its publication"
    assertReceives frames ["xsub-message"] "XSUB received the wrong frames"

main :: IO ()
main = do
  mapM_
    id
    [ testPair,
      testPull,
      testDealer,
      testRepReq,
      testRouter,
      testSub,
      testXPub,
      testXSub
    ]
