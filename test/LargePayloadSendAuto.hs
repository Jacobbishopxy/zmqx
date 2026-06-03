{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate, throwIO)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (uncons)
import System.Mem (performGC)
import Zmqx qualified
import Zmqx.Dealer qualified
import Zmqx.Pair qualified
import Zmqx.Router qualified

largeFrameBytes :: Int
largeFrameBytes =
  1024 * 1024

multipartFrameBytes :: Int
multipartFrameBytes =
  256 * 1024

smallFrameBytes :: Int
smallFrameBytes =
  256

assert :: Bool -> String -> IO ()
assert condition message =
  unless condition (throwIO (userError message))

payload :: Int -> Int -> ByteString
payload seed size =
  ByteString.pack [fromIntegral ((seed + index) `mod` 251) | index <- [0 .. size - 1]]

payloads :: Int -> Int -> Int -> [ByteString]
payloads seed count size =
  [payload (seed + index * 17) size | index <- [0 .. count - 1]]

frameLengths :: Maybe [ByteString] -> Maybe [Int]
frameLengths =
  fmap (fmap ByteString.length)

expectFrames :: String -> Maybe [ByteString] -> [ByteString] -> IO ()
expectFrames label actual expected =
  assert
    (actual == Just expected)
    ( label
        <> ": expected frame lengths "
        <> show (fmap ByteString.length expected)
        <> ", got "
        <> show (frameLengths actual)
    )

pressureGC :: IO ()
pressureGC = do
  performGC
  let garbage = payloads 9000 16 (32 * 1024)
  _ <- evaluate (sum (fmap ByteString.length garbage))
  performGC

runPairCases :: IO ()
runPairCases = do
  let endpoint = "inproc://large-payload-send-auto-pair"
      single = payload 1 largeFrameBytes
      multipart = payloads 100 3 multipartFrameBytes
      small = payload 500 smallFrameBytes

  left <- unwrap (Zmqx.Pair.open (Zmqx.name "large-payload-pair-left"))
  right <- unwrap (Zmqx.Pair.open (Zmqx.name "large-payload-pair-right"))
  unwrap (Zmqx.bind left endpoint)
  unwrap (Zmqx.connect right endpoint)
  threadDelay 100000

  unwrap (Zmqx.send left single)
  pressureGC
  receivedSingle <- unwrap (Zmqx.receivesFor right 1000)
  expectFrames "PAIR large single" receivedSingle [single]

  unwrap (Zmqx.sends right multipart)
  pressureGC
  receivedMultipart <- unwrap (Zmqx.receivesFor left 1000)
  expectFrames "PAIR large multipart" receivedMultipart multipart

  unwrap (Zmqx.send left small)
  receivedSmall <- unwrap (Zmqx.receivesFor right 1000)
  expectFrames "PAIR small fallback" receivedSmall [small]

runDealerRouterCases :: IO ()
runDealerRouterCases = do
  let endpoint = "inproc://large-payload-send-auto-dealer-router"
      dealerSingle = payload 1000 largeFrameBytes
      dealerMultipart = payloads 2000 3 multipartFrameBytes
      routerMultipart = payloads 3000 3 multipartFrameBytes

  router <- unwrap (Zmqx.Router.open (Zmqx.name "large-payload-router"))
  dealer <- unwrap (Zmqx.Dealer.open (Zmqx.name "large-payload-dealer"))
  unwrap (Zmqx.bind router endpoint)
  unwrap (Zmqx.connect dealer endpoint)
  threadDelay 100000

  unwrap (Zmqx.send dealer dealerSingle)
  pressureGC
  routedSingle <- unwrap (Zmqx.receivesFor router 1000)
  routingId <- case routedSingle >>= uncons of
    Just (identity, frames) -> do
      expectFrames "DEALER->ROUTER large single" (Just frames) [dealerSingle]
      pure identity
    Nothing ->
      throwIO (userError ("ROUTER did not receive large single frame; got lengths " <> show (frameLengths routedSingle)))

  unwrap (Zmqx.Router.sends router (routingId : routerMultipart))
  pressureGC
  dealerReply <- unwrap (Zmqx.receivesFor dealer 1000)
  expectFrames "ROUTER->DEALER large multipart" dealerReply routerMultipart

  unwrap (Zmqx.sends dealer dealerMultipart)
  pressureGC
  routedMultipart <- unwrap (Zmqx.receivesFor router 1000)
  case routedMultipart >>= uncons of
    Just (_identity, frames) ->
      expectFrames "DEALER->ROUTER large multipart" (Just frames) dealerMultipart
    Nothing ->
      throwIO (userError ("ROUTER did not receive large multipart frames; got lengths " <> show (frameLengths routedMultipart)))

main :: IO ()
main =
  Zmqx.run Zmqx.defaultOptions do
    runPairCases
    runDealerRouterCases
