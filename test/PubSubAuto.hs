{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Zmqx qualified
import Zmqx.Pub qualified
import Zmqx.Sub qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

main :: IO ()
main =
  Zmqx.run Zmqx.defaultOptions do
    let endpoint = "inproc://pub-sub-auto"

    publisher <- unwrap (Zmqx.Pub.open (Zmqx.name "publisher-auto"))
    subscriber <- unwrap (Zmqx.Sub.open (Zmqx.name "subscriber-auto"))

    unwrap (Zmqx.bind publisher endpoint)
    unwrap (Zmqx.Sub.subscribe subscriber "10001 ")
    unwrap (Zmqx.connect subscriber endpoint)

    threadDelay 100000

    unwrap (Zmqx.send publisher "99999 10 20")
    missing <- unwrap (Zmqx.receivesFor subscriber 50)
    assert (missing == Nothing) ("SUB unexpectedly received a non-matching message: " <> show missing)

    unwrap (Zmqx.send publisher "10001 24 60")
    matching <- unwrap (Zmqx.receivesFor subscriber 1000)
    assert (matching == Just ["10001 24 60"]) ("SUB matching message mismatch: " <> show matching)
