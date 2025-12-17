-- file: SimpleBroker.hs
-- author: Jacob Xie
-- date: 2025/03/05 10:50:31 Wednesday
-- brief:

module Main where

import Common (endpoint1, endpoint2, unwrap)
import Control.Monad (forever, when)
import Data.Function ((&))
import Zmqx
import Zmqx.Dealer
import Zmqx.Router

main :: IO ()
main = do
  Zmqx.run Zmqx.defaultOptions do
    -- Prepare our sockets
    frontend <- unwrap (Zmqx.Router.open (Zmqx.name "frontend"))
    backend <- unwrap (Zmqx.Dealer.open (Zmqx.name "backend"))
    unwrap (Zmqx.bind frontend endpoint1)
    unwrap (Zmqx.bind backend endpoint2)

    -- Initialize poll set
    let items = Zmqx.the frontend & Zmqx.also backend
    -- Switch messages between sockets
    forever do
      Zmqx.Ready ready <- unwrap (Zmqx.poll items)
      when (ready frontend) do
        message <- unwrap (Zmqx.receives frontend)
        unwrap (Zmqx.Dealer.sends backend message)
      when (ready backend) do
        message <- unwrap (Zmqx.receives backend)
        unwrap (Zmqx.Router.sends frontend message)
