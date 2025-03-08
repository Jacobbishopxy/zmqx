

-- file: SimpleSub.hs
-- author: Jacob Xie
-- date: 2025/03/03 10:57:56 Monday
-- brief:

module Main where

import Common (endpoint, unwrap)
import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Functor ((<&>))
import System.Environment (getArgs)
import Text.Printf (printf)
import Zmqx
import Zmqx.Sub

main :: IO ()
main =
  Zmqx.run Zmqx.defaultOptions do
    -- Socket to talk to server
    putStrLn "Collecting updates from weather server..."
    subscriber <- unwrap (Zmqx.Sub.open (Zmqx.name "subscriber"))
    unwrap (Zmqx.connect subscriber endpoint)

    -- Subscribe to zipcode, default is NYC, 10001
    ft <-
      getArgs <&> \case
        [] -> "10001 "
        ft : _ -> ft
    unwrap (Zmqx.Sub.subscribe subscriber (ByteString.Char8.pack ft))

    -- Process 100 updates
    temps <-
      replicateM 100 do
        string <- unwrap (Zmqx.receive subscriber)
        let parsedWords = map read (words (ByteString.Char8.unpack string))
        let temperature = case parsedWords of
              (_zipcode :: Int) : temp : _relhumidity : _ -> temp
              _ -> error "Unexpected format: could not parse temperature data"
        pure (realToFrac @Int @Double temperature)
    printf "Average temperature for zipcode '%s' was %dF\n" ft (floor (sum temps / 100) :: Int)
