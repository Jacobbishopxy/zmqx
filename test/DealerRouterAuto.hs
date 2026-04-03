{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Exception (throwIO)
import Data.List (uncons)
import Zmqx qualified
import Zmqx.Dealer qualified
import Zmqx.Router qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

main :: IO ()
main =
  Zmqx.run Zmqx.defaultOptions do
    let endpoint = "inproc://dealer-router-auto"
        payload = ["Hello", "Jacob", "42"]

    router <- unwrap (Zmqx.Router.open (Zmqx.name "router-auto"))
    dealer <- unwrap (Zmqx.Dealer.open (Zmqx.name "dealer-auto"))

    unwrap (Zmqx.bind router endpoint)
    unwrap (Zmqx.connect dealer endpoint)

    unwrap (Zmqx.sends dealer payload)
    routed <- unwrap (Zmqx.receivesFor router 1000)
    case routed >>= uncons of
      Just (routingId, frames) -> do
        assert (frames == payload) ("ROUTER payload mismatch: " <> show frames)
        unwrap (Zmqx.Router.sends router (routingId : ["World"]))
      Nothing ->
        throwIO (userError ("ROUTER did not receive a routed message: " <> show routed))

    reply <- unwrap (Zmqx.receivesFor dealer 1000)
    assert (reply == Just ["World"]) ("DEALER reply mismatch: " <> show reply)
