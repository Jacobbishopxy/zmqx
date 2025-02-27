{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: SimpleReq.hs
-- author: Jacob Xie
-- date: 2025/02/27 15:21:51 Thursday
-- brief:

module Main where

import Control.Exception (throwIO)
import Data.Foldable (for_)
import Text.Printf (printf)
import Zmqx
import Zmqx.Core.Req

main :: IO ()
main =
  Zmqx.run
    Zmqx.defaultOptions
    do
      putStrLn "Connecting to hello world server..."
      requester <- unwrap (Zmqx.Core.Req.open (Zmqx.name "requester"))
      unwrap (Zmqx.connect requester "tcp://localhost:5555")

      for_ [(0 :: Int) .. 9] \requestNbr -> do
        printf "Sending Hello %d...\n" requestNbr
        unwrap (Zmqx.send requester "Hello")
        _ <- unwrap (Zmqx.receive requester)
        printf "Received World %d\n" requestNbr

unwrap :: IO (Either Zmqx.Error a) -> IO a
unwrap action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value
