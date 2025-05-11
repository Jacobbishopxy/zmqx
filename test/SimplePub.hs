-- file: SimplePub.hs
-- author: Jacob Xie
-- date: 2025/03/03 10:57:51 Monday
-- brief:

module Main where

import Common (endpoint, unwrap)
import Control.Concurrent
import Control.Monad (forever)
import Data.ByteString.Char8 qualified as ByteString.Char8
import System.Random.Stateful (globalStdGen, uniformRM)
import Text.Printf (printf)
import Zmqx
import Zmqx.Pub

main :: IO ()
main = do
  -- Illegal !!!

  publisher <- unwrap (Zmqx.Pub.open (Zmqx.name "publisher"))
  unwrap (Zmqx.bind publisher endpoint)

  tid <- forkIO $ do
    Zmqx.run Zmqx.defaultOptions do
      -- publisher <- unwrap (Zmqx.Pub.open (Zmqx.name "publisher"))
      -- unwrap (Zmqx.bind publisher endpoint)

      forever do
        -- Get values that will fool the boss
        zipcode <- uniformRM (0 :: Int, 99999) globalStdGen
        temperature <- uniformRM (-80 :: Int, 134) globalStdGen
        relhumidity <- uniformRM (10 :: Int, 59) globalStdGen

        -- Send message to all subscribers
        let update = ByteString.Char8.pack (printf "%05d %d %d" zipcode temperature relhumidity)
        unwrap (Zmqx.send publisher update)

  putStrLn $ "tid: " <> show tid

  threadDelay (10 * 1000000) -- sleep for 10 seconds (microseconds)
  return ()
