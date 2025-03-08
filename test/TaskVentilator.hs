-- file: TaskVentilator.hs
-- author: Jacob Xie
-- date: 2025/03/04 13:24:58 Tuesday
-- brief:

module Main where

import Common (endpoint1, endpoint2, unwrap)
import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as ByteString.Char8
import System.Random.Stateful (globalStdGen, uniformRM)
import Text.Printf (printf)
import Zmqx
import Zmqx.Push

-- Task ventilator
main :: IO ()
main = do
  Zmqx.run Zmqx.defaultOptions do
    sender <- unwrap (Zmqx.Push.open (Zmqx.name "sender"))
    unwrap (Zmqx.bind sender endpoint1)

    sink <- unwrap (Zmqx.Push.open (Zmqx.name "sink"))
    unwrap (Zmqx.connect sink endpoint2)

    putStrLn "Press Enter when the workers are ready"
    _ <- getLine
    putStrLn "Sending tasks to workers..."

    -- The first message is "0" and signals start of batch
    unwrap (Zmqx.send sink "0")

    -- Send 100 tasks
    workloads <-
      replicateM 100 do
        -- Random workload from 1 to 100msecs
        workload <- uniformRM (1 :: Int, 100) globalStdGen
        unwrap (Zmqx.send sender (ByteString.Char8.pack (printf "%d" workload)))
        pure workload
    printf "Total expected cost: %d msec\n" (sum workloads)
