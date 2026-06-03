{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (replicateM, replicateM_, void)
import GHC.Exts (keepAlive#)
import GHC.IO (IO (IO))
import Numeric.Natural (Natural)
import System.Mem (performMajorGC)
import Text.Printf (printf)
import Zmqx
import Zmqx.Pair qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

keepAlive :: a -> IO b -> IO b
keepAlive thing (IO action) =
  IO \s -> keepAlive# thing s action

waitUntil :: Int -> IO Bool -> IO Bool
waitUntil attempts action
  | attempts <= 0 = pure False
  | otherwise =
      action >>= \case
        True -> pure True
        False -> do
          threadDelay 50000
          waitUntil (attempts - 1) action

portableContextMaxSockets :: Natural
portableContextMaxSockets = 128

liveSocketCount :: Int
liveSocketCount = 8

churnBatchSize :: Int
churnBatchSize = 32

churnBatches :: Int
churnBatches = 12

openPairIn :: Context -> IO Zmqx.Pair.Pair
openPairIn context =
  unwrap (openWith context (Zmqx.Pair.defaultOptions <> name "finalizer-scaling-pair"))

openShortLivedPair :: Context -> IO ()
openShortLivedPair context =
  void (openPairIn context)

waitForPendingSockets :: Context -> Int -> IO Bool
waitForPendingSockets context expected =
  waitUntil 40 do
    performMajorGC
    (== expected) <$> pendingSockets context

testShortLivedSocketChurnDrains :: IO ()
testShortLivedSocketChurnDrains =
  withContext (Zmqx.defaultOptions <> maxSockets portableContextMaxSockets) \context -> do
    replicateM_ churnBatches do
      replicateM_ churnBatchSize (openShortLivedPair context)
      drained <- waitForPendingSockets context 0
      assert drained (printf "Expected short-lived socket batch to drain to 0 pending sockets after GC")
    finalPending <- pendingSockets context
    assert (finalPending == 0) (printf "Expected no pending sockets after churn; got %d" finalPending)

testLiveSocketsRemainPendingAcrossChurn :: IO ()
testLiveSocketsRemainPendingAcrossChurn =
  withContext (Zmqx.defaultOptions <> maxSockets portableContextMaxSockets) \context -> do
    liveSockets <- replicateM liveSocketCount (openPairIn context)
    let expectedLive = length liveSockets
    replicateM_ churnBatches do
      replicateM_ churnBatchSize (openShortLivedPair context)
      drainedToLive <- keepAlive liveSockets (waitForPendingSockets context expectedLive)
      assert drainedToLive (printf "Expected pending sockets to drain back to %d live sockets" expectedLive)
    remaining <- keepAlive liveSockets (pendingSockets context)
    assert (remaining == expectedLive) (printf "Expected %d live pending sockets; got %d" expectedLive remaining)

main :: IO ()
main = do
  testShortLivedSocketChurnDrains
  testLiveSocketsRemainPendingAcrossChurn
