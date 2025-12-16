-- file: PollOut.hs
-- brief: Verify polling for POLLOUT and POLLIN works through the public API.

module Main where

import Common (unwrap)
import Control.Exception (throwIO)
import Data.ByteString.Char8 qualified as ByteString
import Zmqx
import Zmqx.Pair qualified as Pair

main :: IO ()
main = do
  let endpoint = "inproc://poll-out"
  Zmqx.run Zmqx.defaultOptions $ do
    sender <- unwrap (Pair.open (Zmqx.name "poll-out-sender"))
    receiver <- unwrap (Pair.open (Zmqx.name "poll-out-receiver"))

    unwrap (Pair.bind receiver endpoint)
    unwrap (Pair.connect sender endpoint)

    awaitReadyOut sender

    unwrap (Zmqx.send sender (ByteString.pack "ping"))

    awaitReadyIn receiver
    _ <- unwrap (Zmqx.receive receiver)
    pure ()

awaitReadyOut :: Pair.Pair -> IO ()
awaitReadyOut socket =
  awaitReady "POLLOUT" socket (Zmqx.pollOut socket)

awaitReadyIn :: Pair.Pair -> IO ()
awaitReadyIn socket =
  awaitReady "POLLIN" socket (Zmqx.pollIn socket)

awaitReady :: String -> Socket a -> Sockets -> IO ()
awaitReady label socket sockets =
  loop (10 :: Int)
  where
    loop n
      | n <= 0 = throwIO (userError ("timed out waiting for " <> label))
      | otherwise =
          Zmqx.pollFor sockets 50 >>= \case
            Left err -> throwIO err
            Right Nothing -> loop (n - 1)
            Right (Just (Zmqx.Ready ready)) ->
              if ready socket
                then pure ()
                else loop (n - 1)
