{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Zmqx qualified
import Zmqx.Rep qualified
import Zmqx.Req qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

expectFrames :: String -> Maybe [ByteString] -> Maybe [ByteString] -> IO ()
expectFrames label actual expected =
  assert (actual == expected) (label <> ": expected " <> show expected <> ", got " <> show actual)

main :: IO ()
main =
  Zmqx.run Zmqx.defaultOptions do
    let endpoint = "inproc://req-rep-auto"

    rep <- unwrap (Zmqx.Rep.open (Zmqx.name "rep-auto"))
    req <- unwrap (Zmqx.Req.open (Zmqx.name "req-auto"))

    unwrap (Zmqx.bind rep endpoint)
    unwrap (Zmqx.connect req endpoint)

    unwrap (Zmqx.send req "ping")
    request <- unwrap (Zmqx.receivesFor rep 1000)
    expectFrames "REP request" request (Just ["ping"])

    unwrap (Zmqx.send rep "pong")
    reply <- unwrap (Zmqx.receivesFor req 1000)
    expectFrames "REQ reply" reply (Just ["pong"])
