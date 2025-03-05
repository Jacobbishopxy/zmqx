{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: TaskSink.hs
-- author: Jacob Xie
-- date: 2025/03/04 13:59:42 Tuesday
-- brief:

module Main where

import Common (endpoint2, unwrap)
import Data.Foldable (for_)
import GHC.Clock (getMonotonicTimeNSec)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Zmqx
import Zmqx.Pull

main :: IO ()
main = do
  Zmqx.run Zmqx.defaultOptions do
    receiver <- unwrap (Zmqx.Pull.open (Zmqx.name "receiver"))
    unwrap (Zmqx.bind receiver endpoint2)

    -- wait for start of batch
    _ <- unwrap (Zmqx.receive receiver)

    -- start clock
    startTime <- getMonotonicTimeNSec

    -- process 100 confirmations
    for_ [(0 :: Int) .. 99] \taskNbr -> do
      _ <- unwrap (Zmqx.receive receiver)
      putChar (if mod taskNbr 10 == 0 then ':' else '.')
      hFlush stdout

    -- calculate and report duration of batch
    stopTime <- getMonotonicTimeNSec
    printf "Total elapsed time: %d msec\n" ((stopTime - startTime) `div` 1_000_000)
