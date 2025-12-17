-- file: MutWorker.hs
-- author: Jacob Xie
-- date: 2025/03/07 21:03:58 Friday
-- brief:

module Main where

import Common (endpoint, endpointInproc, unwrap)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, replicateM_, when)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Function ((&))
import Ki
import Text.Printf (printf)
import Zmqx
import Zmqx.Dealer
import Zmqx.Rep
import Zmqx.Router

main :: IO ()
main = do
  Zmqx.run Zmqx.defaultOptions do
    clients <- unwrap (Zmqx.Router.open (Zmqx.name "clients"))
    unwrap (Zmqx.bind clients endpoint)

    workers <- unwrap (Zmqx.Dealer.open (Zmqx.name "workers"))
    unwrap (Zmqx.bind workers endpointInproc)

    -- Launch pool of worker threads
    Ki.scoped \scope -> do
      replicateM_ 5 do
        Ki.fork_ scope do
          -- Socket to talk to dispatcher
          receiver <- unwrap (Zmqx.Rep.open (Zmqx.name "receiver"))
          unwrap (Zmqx.connect receiver endpointInproc)

          forever do
            string <- unwrap (Zmqx.receive receiver)
            printf "Received request: [%s]\n" (ByteString.Char8.unpack string)
            -- Do some 'work'
            threadDelay 1_000_000
            unwrap (Zmqx.send receiver "World")

      -- Connect work threads to client threads via a queue proxy
      let items = Zmqx.the clients & Zmqx.also workers
      forever do
        Zmqx.Ready ready <- unwrap (Zmqx.poll items)
        when (ready clients) do
          message <- unwrap (Zmqx.receives clients)
          unwrap (Zmqx.sends workers message)
        when (ready workers) do
          message <- unwrap (Zmqx.receives workers)
          unwrap (Zmqx.sends clients message)
