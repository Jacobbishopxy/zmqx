{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Unique (hashUnique, newUnique)
import System.Timeout qualified as Timeout
import Zmqx qualified
import Zmqx.Pair qualified
import Zmqx.Pull qualified
import Zmqx.Push qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

expectFrame :: String -> ByteString -> ByteString -> IO ()
expectFrame label actual expected =
  assert (actual == expected) (label <> ": expected " <> show expected <> ", got " <> show actual)

expectFrames :: String -> Maybe [ByteString] -> [ByteString] -> IO ()
expectFrames label actual expected =
  assert (actual == Just expected) (label <> ": expected " <> show (Just expected) <> ", got " <> show actual)

uniqueEndpoint :: String -> IO Text
uniqueEndpoint label = do
  unique <- newUnique
  pure ("inproc://receive-path-auto-" <> Text.pack label <> "-" <> Text.pack (show (hashUnique unique)))

testPairReceivePath :: IO ()
testPairReceivePath = do
  endpoint <- uniqueEndpoint "pair"
  left <- unwrap (Zmqx.Pair.open (Zmqx.name "receive-path-pair-left"))
  right <- unwrap (Zmqx.Pair.open (Zmqx.name "receive-path-pair-right"))

  unwrap (Zmqx.bind left endpoint)
  unwrap (Zmqx.connect right endpoint)

  unwrap (Zmqx.send left "pair-single")
  single <- unwrap (Zmqx.receive right)
  expectFrame "PAIR single-frame receive" single "pair-single"

  let frames = ["pair-frame-1", "pair-frame-2", "pair-frame-3"]
  unwrap (Zmqx.sends right frames)
  multipart <- unwrap (Zmqx.receivesFor left 1000)
  expectFrames "PAIR multipart receive" multipart frames

testPushPullReceivePath :: IO ()
testPushPullReceivePath = do
  endpoint <- uniqueEndpoint "push-pull"
  receiver <- unwrap (Zmqx.Pull.open (Zmqx.name "receive-path-pull"))
  sender <- unwrap (Zmqx.Push.open (Zmqx.name "receive-path-push"))

  unwrap (Zmqx.bind receiver endpoint)
  unwrap (Zmqx.connect sender endpoint)

  unwrap (Zmqx.send sender "push-pull-single")
  single <- unwrap (Zmqx.receive receiver)
  expectFrame "PUSH/PULL single-frame receive" single "push-pull-single"

  let frames = ["push-pull-frame-1", "push-pull-frame-2", "push-pull-frame-3"]
  unwrap (Zmqx.sends sender frames)
  multipart <- unwrap (Zmqx.receivesFor receiver 1000)
  expectFrames "PUSH/PULL multipart receive" multipart frames

testBody :: IO ()
testBody =
  Zmqx.run Zmqx.defaultOptions do
    testPairReceivePath
    testPushPullReceivePath

main :: IO ()
main =
  Timeout.timeout 5000000 testBody >>= \case
    Just () -> pure ()
    Nothing -> throwIO (userError "ReceivePathAuto timed out")
