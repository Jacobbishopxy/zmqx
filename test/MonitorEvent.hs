{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Int (Int32)
import Foreign.C.Types (CUShort (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (pokeByteOff)
import Zmqx

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

monitorFrame :: CUShort -> Int32 -> IO ByteString
monitorFrame typ value =
  allocaBytes 6 \ptr -> do
    pokeByteOff ptr 0 typ
    pokeByteOff ptr 2 value
    ByteString.packCStringLen (ptr, 6)

main :: IO ()
main = do
  listeningFrame <- monitorFrame 0x0008 42

  decodeMonitorEvent [listeningFrame, "tcp://127.0.0.1:5555"] >>= \case
    Just (Listening fd) ->
      assert (fd == 42) ("Expected Listening 42, got Listening " <> show fd)
    Just event ->
      throwIO (userError ("Expected Listening event, got " <> show event))
    Nothing ->
      throwIO (userError "Failed to decode a valid Listening monitor frame")

  decodeMonitorEvent [ByteString.pack [0x08, 0x00, 0x2A], "tcp://127.0.0.1:5555"] >>= \case
    Nothing -> pure ()
    Just event ->
      throwIO (userError ("Expected malformed monitor frame to be ignored, got " <> show event))
