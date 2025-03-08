-- file: SimpleDealer.hs
-- author: Jacob Xie
-- date: 2025/02/28 11:07:36 Friday
-- brief:

module Main where

import Common (endpoint, unwrap)
import Control.Concurrent (threadDelay)
import Data.ByteString.Char8 (pack)
import Data.Foldable (for_)
import Text.Printf (printf)
import Zmqx
import Zmqx.Dealer

main :: IO ()
main =
  Zmqx.run
    Zmqx.defaultOptions
    do
      putStrLn "Connecting to hello world server..."
      dealer <- unwrap (Zmqx.Dealer.open (Zmqx.name "dealer"))
      _ <- Zmqx.setSocketOpt dealer (Zmqx.Z_RoutingId "xy-dealer")
      unwrap (Zmqx.connect dealer endpoint)

      for_ [(0 :: Int) .. 9] \num -> do
        printf "Sending Hello %d...\n" num
        unwrap (Zmqx.sends dealer ["Hello", "Jacob", pack (show num)])
        threadDelay 1_000_000 -- Do some work
