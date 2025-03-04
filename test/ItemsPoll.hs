{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: ItemsPoll.hs
-- author: Jacob Xie
-- date: 2025/03/04 22:56:19 Tuesday
-- brief:

module Main where

import Common (endpoint1, endpoint2, unwrap)
import Control.Monad (forever, when)
import Data.Function ((&))
import Zmqx
import Zmqx.Pull
import Zmqx.Sub

main :: IO ()
main = do
  Zmqx.run Zmqx.defaultOptions do
    -- Connect to task ventilator
    receiver <- unwrap (Zmqx.Pull.open (Zmqx.name "receiver"))
    unwrap (Zmqx.connect receiver endpoint1)

    -- Connect to weather server
    subscriber <- unwrap (Zmqx.Sub.open (Zmqx.name "subscriber"))
    unwrap (Zmqx.connect subscriber endpoint2)
    unwrap (Zmqx.Sub.subscribe subscriber "10001 ")

    -- Process messages from both sockets
    forever do
      let items =
            Zmqx.the receiver
              & Zmqx.also subscriber
      Zmqx.Ready ready <- unwrap (Zmqx.poll items)
      when (ready receiver) do
        Zmqx.receive receiver >>= \case
          Left _ -> pure ()
          Right _ ->
            -- Process task
            pure ()
      when (ready subscriber) do
        Zmqx.receive subscriber >>= \case
          Left _ -> pure ()
          Right _ ->
            -- Process weather update
            pure ()
