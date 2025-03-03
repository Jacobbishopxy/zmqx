-- file: Version.hs
-- author: Jacob Xie
-- date: 2025/03/03 13:19:43 Monday
-- brief:

module Main where

import Text.Printf (printf)
import Zmqx

main :: IO ()
main = do
  let (major, minor, patch) = Zmqx.version
  printf "Current 0MQ version is %d.%d.%d\n" major minor patch
