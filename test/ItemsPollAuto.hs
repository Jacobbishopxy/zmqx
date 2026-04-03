{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Unique (hashUnique, newUnique)
import System.Timeout qualified as Timeout
import Zmqx
import Zmqx.Pub qualified
import Zmqx.Pull qualified
import Zmqx.Push qualified
import Zmqx.Sub qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

uniqueEndpoint :: String -> IO Text
uniqueEndpoint label = do
  unique <- newUnique
  pure ("inproc://items-poll-auto-" <> Text.pack label <> "-" <> Text.pack (show (hashUnique unique)))

awaitConnection :: IO ()
awaitConnection =
  threadDelay 100000

collectMessages :: Pull -> Sub -> Sockets -> Bool -> Bool -> IO ()
collectMessages receiver subscriber items gotTask gotWeather
  | gotTask && gotWeather =
      pure ()
  | otherwise =
      pollFor items 1000 >>= \case
        Right (Just (Ready ready)) -> do
          gotTask' <-
            if ready receiver
              then do
                frame <- unwrap (receive receiver)
                assert (frame == "task-1") ("PULL received the wrong task frame: " <> show frame)
                pure True
              else pure gotTask
          gotWeather' <-
            if ready subscriber
              then do
                frame <- unwrap (receive subscriber)
                assert (frame == "10001 weather") ("SUB received the wrong weather frame: " <> show frame)
                pure True
              else pure gotWeather
          collectMessages receiver subscriber items gotTask' gotWeather'
        Right Nothing ->
          throwIO (userError "mixed poll timed out before both sockets became ready")
        Left err ->
          throwIO err

testBody :: IO ()
testBody =
  run defaultOptions do
    pullEndpoint <- uniqueEndpoint "pull"
    pubEndpoint <- uniqueEndpoint "pub"
    receiver <- unwrap (Zmqx.Pull.open (name "items-poll-receiver"))
    taskSender <- unwrap (Zmqx.Push.open (name "items-poll-sender"))
    subscriber <- unwrap (Zmqx.Sub.open (name "items-poll-subscriber"))
    publisher <- unwrap (Zmqx.Pub.open (name "items-poll-publisher"))

    unwrap (bind receiver pullEndpoint)
    unwrap (connect taskSender pullEndpoint)
    unwrap (bind publisher pubEndpoint)
    unwrap (Zmqx.Sub.subscribe subscriber "10001 ")
    unwrap (connect subscriber pubEndpoint)
    awaitConnection

    let items = pollIn receiver & pollInAlso subscriber
    pollFor items 20 >>= \case
      Right Nothing -> pure ()
      Right (Just _) -> throwIO (userError "mixed poll reported readiness before any message was sent")
      Left err -> throwIO err

    unwrap (send taskSender "task-1")
    unwrap (send publisher "10001 weather")

    collectMessages receiver subscriber items False False

main :: IO ()
main =
  Timeout.timeout 5000000 testBody >>= \case
    Just () -> pure ()
    Nothing -> throwIO (userError "ItemsPollAuto timed out")
