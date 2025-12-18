{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: Context.hs
-- author: Jacob Xie
-- date: 2025/02/27 11:01:10 Thursday
-- brief:

module Zmqx.Core.Context
  ( RunError (..),
    Context (..),
    ContextualOpen (..),
    globalContextRef,
    globalSocketFinalizersRef,
    run,
    withContext,
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
import Zmqx.Error (Error, enrichError, unexpectedError)
import Zmqx.Internal

data RunError
  = RunAlreadyActive
  | ContextNotInitialized
  deriving stock (Eq, Show)

instance Exception RunError where
  displayException = \case
    RunAlreadyActive -> "Zmqx.run was called while another run is active"
    ContextNotInitialized -> "Zmqx context not initialized; wrap usage in Zmqx.run or withContext"

-- | A concrete ØMQ context handle for explicit lifetime management.
--
-- This is used by 'withContext' and the @openWith@ helpers to let callers manage
-- multiple contexts or avoid global state while keeping the existing API intact.
data Context = Context
  { contextPtr :: !Zmq_ctx,
    contextFinalizers :: !(IORef [SocketFinalizer])
  }
  deriving stock (Eq)

-- | Class of socket types that can open against an explicit 'Context'.
class ContextualOpen socket where
  openWith :: Context -> Options socket -> IO (Either Error socket)

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
        terminateContext globalSocketFinalizersRef context
          `finally` do
            atomicWriteIORef globalSocketFinalizersRef []
            atomicWriteIORef globalContextRef Nothing

-- | Run an action with an explicit context handle, without touching global state.
--
-- This is compatible with Plan A's 'run'—callers can either:
--
-- * Keep using 'run' (single global context), or
-- * Use 'withContext' plus @openWith@ to scope sockets to a specific context.
withContext :: Options () -> (Context -> IO a) -> IO a
withContext options action =
  bracket (initializeContext options) cleanupContext \(context, socketFinalizers) ->
    action
      Context
        { contextPtr = context,
          contextFinalizers = socketFinalizers
        }
  where
    initializeContext :: Options () -> IO (Zmq_ctx, IORef [SocketFinalizer])
    initializeContext opts = do
      socketFinalizers <- newIORef []
      context <- newContext opts
      pure (context, socketFinalizers)

    cleanupContext :: (Zmq_ctx, IORef [SocketFinalizer]) -> IO ()
    cleanupContext (context, socketFinalizers) =
      uninterruptibleMask_ $
        terminateContext socketFinalizers context

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
terminateContext :: IORef [SocketFinalizer] -> Zmq_ctx -> IO ()
terminateContext socketFinalizers context = do
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
  finalizers <- readIORef socketFinalizers
  for_ (reverse finalizers) runSocketFinalizer
  atomicWriteIORef socketFinalizers []

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
