-- file: SimpleRep.hs
-- author: Jacob Xie
-- date: 2025/02/27 15:19:31 Thursday
-- brief:

module Main where

import Common (endpoint, unwrap)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Zmqx
import Zmqx.Rep

main :: IO ()
main =
  Zmqx.run Zmqx.defaultOptions do
    -- Socket to talk to clients
    responder <- unwrap (Zmqx.Rep.open (Zmqx.name "responder"))
    unwrap (Zmqx.bind responder endpoint)

    forever do
      _ <- unwrap (Zmqx.receive responder)
      putStrLn "Received Hello"
      threadDelay 1_000_000 -- Do some work
      unwrap (Zmqx.send responder "World")
