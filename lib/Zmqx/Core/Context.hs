{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: Context.hs
-- author: Jacob Xie
-- date: 2025/02/27 11:01:10 Thursday
-- brief:

module Zmqx.Core.Context
  ( RunError (..),
    globalContextRef,
    globalSocketFinalizersRef,
    run,
  )
where

import Control.Concurrent.MVar
import Control.Exception
import Data.Foldable (for_)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Zmqx.Core.Options (Options)
import Zmqx.Core.Options qualified as Options
import Zmqx.Core.SocketFinalizer (SocketFinalizer, runSocketFinalizer)
import Zmqx.Error (enrichError, unexpectedError)
import Zmqx.Internal

data RunError
  = RunAlreadyActive
  | ContextNotInitialized
  deriving stock (Eq, Show)

instance Exception RunError where
  displayException = \case
    RunAlreadyActive -> "Zmqx.run was called while another run is active"
    ContextNotInitialized -> "Zmqx context not initialized; wrap usage in Zmqx.run"

globalContextRef :: IORef (Maybe Zmq_ctx)
globalContextRef =
  unsafePerformIO (newIORef Nothing)
{-# NOINLINE globalContextRef #-}

globalRunLock :: MVar ()
globalRunLock =
  unsafePerformIO (newMVar ())
{-# NOINLINE globalRunLock #-}

-- A context cannot terminate until all sockets are closed. So, whenever we open a socket, we register a weak
-- pointer to an action that closes that socket. Sockets thus either close "naturally" (via a finalizer), or during
-- context termination.
--
-- This design allows us to acquire sockets with straight-line syntax, rather than incur a syntactic indent due to
-- bracketing a resource acquire/release.
--
-- FIXME compact this when a finalizer runs, probably
globalSocketFinalizersRef :: IORef [SocketFinalizer]
globalSocketFinalizersRef =
  unsafePerformIO (newIORef [])
{-# NOINLINE globalSocketFinalizersRef #-}

-- | Run a main function.
--
-- This function must be called exactly once at a time, and must wrap all other calls to this library.
run :: Options () -> IO a -> IO a
run options action =
  withRunGuard do
    bracket (initializeContext options) cleanupContext \_ ->
      action
  where
    initializeContext opts =
      mask_ do
        atomicWriteIORef globalSocketFinalizersRef []
        context <- newContext opts
        atomicWriteIORef globalContextRef (Just context)
        pure context

    cleanupContext context =
      uninterruptibleMask_ $
        terminateContext context
          `finally` do
            atomicWriteIORef globalSocketFinalizersRef []
            atomicWriteIORef globalContextRef Nothing

withRunGuard :: IO a -> IO a
withRunGuard =
  bracket acquireRunLock releaseRunLock . const
  where
    acquireRunLock =
      mask_ do
        tryTakeMVar globalRunLock >>= \case
          Nothing -> throwIO RunAlreadyActive
          Just () -> pure ()
    releaseRunLock () =
      putMVar globalRunLock ()

newContext :: Options () -> IO Zmq_ctx
newContext options = do
  context <- zmq_ctx_new
  Options.setContextOption context ZMQ_BLOCKY 0
  Options.setContextOptions context options
  pure context

-- Terminate a context.
terminateContext :: Zmq_ctx -> IO ()
terminateContext context = do
  -- Shut down the context, causing any blocking operations on sockets to return ETERM
  zmq_ctx_shutdown context >>= \case
    Left errno ->
      let err = enrichError "zmq_ctx_shutdown" errno
       in case errno of
            EFAULT -> throwIO err
            _ -> unexpectedError err
    Right () -> pure ()

  -- Close all of the open sockets
  -- Why reverse: close in the order they were acquired :shrug:
  finalizers <- readIORef globalSocketFinalizersRef
  for_ (reverse finalizers) runSocketFinalizer
  atomicWriteIORef globalSocketFinalizersRef []

  -- Terminate the context
  let loop maybeErr =
        zmq_ctx_term context >>= \case
          Left errno ->
            let err = enrichError "zmq_ctx_term" errno
             in case errno of
                  EFAULT -> throwIO err
                  EINTR -> loop (Just err) -- remember that we were interrupted to throw after termination
                  _ -> unexpectedError err
          Right () -> for_ maybeErr throwIO
  loop Nothing
