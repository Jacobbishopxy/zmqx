{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (Exception, SomeException, fromException, throwIO, try)
import Zmqx
import Zmqx.Pair qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

expectException :: forall e a. (Exception e, Eq e, Show e) => e -> IO a -> IO ()
expectException expected action =
  try action >>= \case
    Left err ->
      case fromException err of
        Just actual ->
          assert (actual == expected) ("Expected " <> show expected <> ", got " <> show actual)
        Nothing ->
          throwIO err
    Right _ ->
      throwIO (userError ("Expected exception " <> show expected))

testNestedRunRejected :: IO ()
testNestedRunRejected =
  run Zmqx.defaultOptions do
    expectException RunAlreadyActive $
      run Zmqx.defaultOptions (pure ())

testConcurrentRunRejected :: IO ()
testConcurrentRunRejected = do
  started <- newEmptyMVar
  release <- newEmptyMVar
  finished <- newEmptyMVar

  _ <-
    forkIO $
      run Zmqx.defaultOptions do
        putMVar started ()
        takeMVar release
      >> putMVar finished ()

  takeMVar started
  expectException RunAlreadyActive $
    run Zmqx.defaultOptions (pure ())
  putMVar release ()
  takeMVar finished

testRunGuardRecoversAfterFailure :: IO ()
testRunGuardRecoversAfterFailure = do
  started <- newEmptyMVar
  release <- newEmptyMVar
  finished <- newEmptyMVar

  _ <-
    forkIO $
      run Zmqx.defaultOptions do
        putMVar started ()
        takeMVar release
      >> putMVar finished ()

  takeMVar started
  expectException RunAlreadyActive $
    run Zmqx.defaultOptions (pure ())
  putMVar release ()
  takeMVar finished
  run Zmqx.defaultOptions (pure ())

testRunGuardRecoversAfterUserException :: IO ()
testRunGuardRecoversAfterUserException = do
  result <- try @SomeException $
    run Zmqx.defaultOptions $
      throwIO (userError "boom")
  case result of
    Left _ -> pure ()
    Right _ -> throwIO (userError "Expected run to rethrow the user exception")
  run Zmqx.defaultOptions (pure ())

testOpenOutsideRunRejected :: IO ()
testOpenOutsideRunRejected =
  expectException ContextNotInitialized $
    Zmqx.Pair.open (Zmqx.Pair.defaultOptions <> name "outside-run")

main :: IO ()
main = do
  testNestedRunRejected
  testConcurrentRunRejected
  testRunGuardRecoversAfterFailure
  testRunGuardRecoversAfterUserException
  testOpenOutsideRunRejected
