{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Common (unwrap)
import Control.Concurrent (forkFinally, forkIO, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay, throwTo, tryReadMVar, yield)
import Control.Exception (AsyncException (ThreadKilled), throwIO)
import Control.Monad (replicateM, replicateM_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Unique (hashUnique, newUnique)
import GHC.Exts (keepAlive#)
import GHC.IO (IO (IO))
import Text.Printf (printf)
import System.Mem (performMajorGC)
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

uniqueEndpoint :: String -> IO Text
uniqueEndpoint label = do
  unique <- newUnique
  pure ("inproc://finalizer-registry-" <> Text.pack label <> "-" <> Text.pack (show (hashUnique unique)))

testDeadSocketsDrainAfterGc :: IO ()
testDeadSocketsDrainAfterGc =
  withContext Zmqx.defaultOptions \ctx -> do
    replicateM_ 32 $
      unwrap (openWith ctx (Zmqx.Pair.defaultOptions <> name "gc-pair"))

    beforeGcCount <- pendingSockets ctx
    assert (beforeGcCount > 0) "Expected the finalizer registry to contain sockets before GC"

    registryDrained <-
      waitUntil 40 do
        performMajorGC
        (== 0) <$> pendingSockets ctx

    assert registryDrained "Expected GC-driven socket finalizers to compact the registry back to empty"

testMixedLiveAndDeadSockets :: IO ()
testMixedLiveAndDeadSockets =
  withContext Zmqx.defaultOptions \ctx -> do
    liveSocket <- unwrap (openWith ctx (Zmqx.Pair.defaultOptions <> name "live-pair"))
    replicateM_ 32 $
      unwrap (openWith ctx (Zmqx.Pair.defaultOptions <> name "dead-pair"))

    beforeGcCount <- pendingSockets ctx
    assert (beforeGcCount > 1) "Expected live and dead sockets to both appear before GC"

    registryCompactedToLiveOnly <-
      waitUntil 40 do
        performMajorGC
        (== 1) <$> pendingSockets ctx

    assert registryCompactedToLiveOnly "Expected GC to compact dead sockets while preserving the live socket"
    endpoint <- uniqueEndpoint "live-pair"
    unwrap (bind liveSocket endpoint)

testPendingSocketsInterruptionSafety :: IO ()
testPendingSocketsInterruptionSafety =
  withContext (Zmqx.defaultOptions <> maxSockets 8192) \ctx -> do
    liveSockets <-
      replicateM 2048 $
        unwrap (openWith ctx (Zmqx.Pair.defaultOptions <> name "interrupt-live-pair"))
    let expectedLiveCount = length liveSockets

    let attemptInterruption :: Int -> IO (Maybe Int)
        attemptInterruption 0 = pure Nothing
        attemptInterruption attempts = do
          started <- newEmptyMVar
          done <- newEmptyMVar
          tid <-
            forkFinally
              (putMVar started () >> pendingSockets ctx)
              \_ -> putMVar done ()
          takeMVar started
          killer <-
            forkIO $
              let loop = do
                    tryReadMVar done >>= \case
                      Just () -> pure ()
                      Nothing -> do
                        throwTo tid ThreadKilled
                        yield
                        loop
               in loop
          takeMVar done
          killThread killer
          remaining <- pendingSockets ctx
          if remaining /= expectedLiveCount
            then pure (Just remaining)
            else attemptInterruption (attempts - 1)

    unexpectedRemaining <- keepAlive liveSockets (attemptInterruption 200)

    case unexpectedRemaining of
      Nothing -> pure ()
      Just remaining ->
        assert False (printf "pendingSockets preserved %d live finalizers; expected %d" remaining expectedLiveCount)
    assert (expectedLiveCount == 2048) "Expected all live sockets to remain referenced during the test"

main :: IO ()
main = do
  testDeadSocketsDrainAfterGc
  testMixedLiveAndDeadSockets
  testPendingSocketsInterruptionSafety
