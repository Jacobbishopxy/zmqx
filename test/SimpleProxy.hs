{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: SimpleProxy.hs
-- author: Jacob Xie
-- date: 2025/03/05 10:58:44 Wednesday
-- brief:

module Main where

import Common (endpoint1, endpoint2, unwrap)
import Control.Monad (forever, when)
import Data.Function ((&))
import Zmqx
import Zmqx.XPub
import Zmqx.XSub

main :: IO ()
main = do
  Zmqx.run Zmqx.defaultOptions do
    -- This is where the weather server sits
    frontend <- unwrap (Zmqx.XSub.open (Zmqx.name "frontend"))
    unwrap (Zmqx.connect frontend endpoint1)

    -- This is our public endpoint for subscribers
    backend <- unwrap (Zmqx.XPub.open (Zmqx.name "backend"))
    unwrap (Zmqx.bind backend endpoint2)

    -- Run the proxy until the user interrupts us
    let items = Zmqx.the frontend & Zmqx.also backend
    forever do
      Zmqx.Ready ready <- unwrap (Zmqx.poll items)
      when (ready frontend) do
        message <- unwrap (Zmqx.receives frontend)
        unwrap (Zmqx.XPub.sends backend message)
      when (ready backend) do
        message <- unwrap (Zmqx.receives backend)
        unwrap (Zmqx.XSub.sends frontend message)
