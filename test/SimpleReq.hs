{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: SimpleReq.hs
-- author: Jacob Xie
-- date: 2025/02/27 15:21:51 Thursday
-- brief:

module Main where

import Common (endpoint, unwrap)
import Data.Foldable (for_)
import Text.Printf (printf)
import Zmqx
import Zmqx.Req

main :: IO ()
main =
  Zmqx.run
    Zmqx.defaultOptions
    do
      putStrLn "Connecting to hello world server..."
      requester <- unwrap (Zmqx.Req.open (Zmqx.name "requester"))
      unwrap (Zmqx.connect requester endpoint)

      for_ [(0 :: Int) .. 9] \requestNbr -> do
        printf "Sending Hello %d...\n" requestNbr
        unwrap (Zmqx.send requester "Hello")
        _ <- unwrap (Zmqx.receive requester)
        printf "Received World %d\n" requestNbr
