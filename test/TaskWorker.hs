{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: SimpleWorker.hs
-- author: Jacob Xie
-- date: 2025/03/04 13:25:28 Tuesday
-- brief:

module Main where

import Common (endpoint1, endpoint2, unwrap)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.ByteString.Char8 qualified as ByteString.Char8
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Zmqx
import Zmqx.Pull
import Zmqx.Push

main :: IO ()
main = do
  Zmqx.run Zmqx.defaultOptions do
    -- recv msg
    receiver <- unwrap (Zmqx.Pull.open (Zmqx.name "receiver"))
    unwrap (Zmqx.connect receiver endpoint1)

    -- send msg
    sender <- unwrap (Zmqx.Push.open (Zmqx.name "sender"))
    unwrap (Zmqx.connect sender endpoint2)

    -- process tasks forever
    forever do
      string <- unwrap (Zmqx.receive receiver)
      printf "%s." (ByteString.Char8.unpack string) -- Show progress
      hFlush stdout
      threadDelay (read (ByteString.Char8.unpack string) * 1_000) -- Do the work
      unwrap (Zmqx.send sender "")
