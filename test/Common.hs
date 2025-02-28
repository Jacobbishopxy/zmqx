{-# LANGUAGE OverloadedStrings #-}

-- file: Common.hs
-- author: Jacob Xie
-- date: 2025/02/28 08:51:15 Friday
-- brief:

module Common
  ( endpoint,
    unwrap,
  )
where

import Control.Exception (throwIO)
import Data.Text (Text)
import Zmqx

endpoint :: Text
endpoint = "tcp://127.0.0.1:5555"

----------------------------------------------------------------------------------------------------

unwrap :: IO (Either Zmqx.Error a) -> IO a
unwrap action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value
