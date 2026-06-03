{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Zmqx qualified
import Zmqx.Req qualified
import Zmqx.Router qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

expectFrames :: String -> Maybe [ByteString] -> Maybe [ByteString] -> IO ()
expectFrames label actual expected =
  assert (actual == expected) (label <> ": expected " <> show expected <> ", got " <> show actual)

replyToReq :: [ByteString] -> ByteString -> [ByteString]
replyToReq routed reply =
  case reverse routed of
    [] -> [reply]
    _request : envelope -> reverse (reply : envelope)

main :: IO ()
main =
  Zmqx.run Zmqx.defaultOptions do
    let endpoint = "inproc://blocking-backpressure-auto"
        request = "request-empty-probe"
        reply = "reply-after-empty-probe"

    router <- unwrap (Zmqx.Router.open Zmqx.Router.defaultOptions)
    req <- unwrap (Zmqx.Req.open Zmqx.Req.defaultOptions)

    unwrap (Zmqx.bind router endpoint)
    unwrap (Zmqx.connect req endpoint)
    threadDelay 100000

    unwrap (Zmqx.send req request)
    routed <- unwrap (Zmqx.receives router)
    assert (request `elem` routed) ("ROUTER did not receive request frame: " <> show routed)

    emptyReply <- unwrap (Zmqx.receivesFor req 0)
    expectFrames "REQ empty receive" emptyReply Nothing

    unwrap (Zmqx.sends router (replyToReq routed reply))
    received <- unwrap (Zmqx.receivesFor req 1000)
    expectFrames "REQ reply after empty probe" received (Just [reply])
