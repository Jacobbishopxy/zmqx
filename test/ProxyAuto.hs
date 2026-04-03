{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Zmqx qualified
import Zmqx.Pub qualified
import Zmqx.Sub qualified
import Zmqx.XPub qualified
import Zmqx.XSub qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

main :: IO ()
main =
  Zmqx.run Zmqx.defaultOptions do
    let upstreamEndpoint = "inproc://proxy-auto-upstream"
        downstreamEndpoint = "inproc://proxy-auto-downstream"

    publisher <- unwrap (Zmqx.Pub.open (Zmqx.name "proxy-publisher-auto"))
    frontend <- unwrap (Zmqx.XSub.open (Zmqx.name "proxy-frontend-auto"))
    backend <- unwrap (Zmqx.XPub.open (Zmqx.name "proxy-backend-auto"))
    subscriber <- unwrap (Zmqx.Sub.open (Zmqx.name "proxy-subscriber-auto"))

    unwrap (Zmqx.bind publisher upstreamEndpoint)
    unwrap (Zmqx.connect frontend upstreamEndpoint)
    unwrap (Zmqx.bind backend downstreamEndpoint)
    unwrap (Zmqx.Sub.subscribe subscriber "topic ")
    unwrap (Zmqx.connect subscriber downstreamEndpoint)

    subscription <- unwrap (Zmqx.receivesFor backend 1000)
    case subscription of
      Just frames -> unwrap (Zmqx.XSub.sends frontend frames)
      Nothing -> throwIO (userError "PROXY backend did not receive a subscriber subscription")

    threadDelay 100000

    unwrap (Zmqx.send publisher "topic 42")
    publication <- unwrap (Zmqx.receivesFor frontend 1000)
    case publication of
      Just frames -> unwrap (Zmqx.XPub.sends backend frames)
      Nothing -> throwIO (userError "PROXY frontend did not receive an upstream publication")

    delivered <- unwrap (Zmqx.receivesFor subscriber 1000)
    assert (delivered == Just ["topic 42"]) ("PROXY subscriber delivery mismatch: " <> show delivered)
