{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Zmqx qualified
import Zmqx.Dealer qualified
import Zmqx.Rep qualified
import Zmqx.Req qualified
import Zmqx.Router qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

lastMay :: [a] -> Maybe a
lastMay = \case
  [] -> Nothing
  xs -> Just (last xs)

main :: IO ()
main =
  Zmqx.run Zmqx.defaultOptions do
    let frontendEndpoint = "inproc://broker-auto-frontend"
        backendEndpoint = "inproc://broker-auto-backend"

    frontend <- unwrap (Zmqx.Router.open (Zmqx.name "broker-frontend-auto"))
    backend <- unwrap (Zmqx.Dealer.open (Zmqx.name "broker-backend-auto"))
    client <- unwrap (Zmqx.Req.open (Zmqx.name "broker-client-auto"))
    worker <- unwrap (Zmqx.Rep.open (Zmqx.name "broker-worker-auto"))

    unwrap (Zmqx.bind frontend frontendEndpoint)
    unwrap (Zmqx.bind backend backendEndpoint)
    unwrap (Zmqx.connect client frontendEndpoint)
    unwrap (Zmqx.connect worker backendEndpoint)

    threadDelay 100000

    unwrap (Zmqx.send client "Hello")
    clientFrames <- unwrap (Zmqx.receivesFor frontend 1000)
    case clientFrames of
      Just frames -> do
        assert (lastMay frames == Just "Hello") ("BROKER frontend request mismatch: " <> show frames)
        unwrap (Zmqx.Dealer.sends backend frames)
      Nothing ->
        throwIO (userError "BROKER frontend did not receive a client request")

    request <- unwrap (Zmqx.receive worker)
    assert (request == "Hello") ("BROKER worker request mismatch: " <> show request)

    unwrap (Zmqx.send worker "World")
    workerFrames <- unwrap (Zmqx.receivesFor backend 1000)
    case workerFrames of
      Just frames -> do
        assert (lastMay frames == Just "World") ("BROKER backend reply mismatch: " <> show frames)
        unwrap (Zmqx.Router.sends frontend frames)
      Nothing ->
        throwIO (userError "BROKER backend did not receive a worker reply")

    reply <- unwrap (Zmqx.receive client)
    assert (reply == "World") ("BROKER client reply mismatch: " <> show reply)
