

-- file: SimpleRouter.hs
-- author: Jacob Xie
-- date: 2025/02/28 11:07:39 Friday
-- brief:

module Main where

import Common (endpoint, unwrap)
import Control.Monad (forever)
import Zmqx
import Zmqx.Router

main :: IO ()
main =
  Zmqx.run Zmqx.defaultOptions do
    -- Socket to talk to clients
    router <- unwrap (Zmqx.Router.open (Zmqx.name "router"))
    unwrap (Zmqx.bind router endpoint)

    forever do
      messages <- unwrap (Zmqx.receives router)
      print messages
