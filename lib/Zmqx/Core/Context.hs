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
    pendingSockets,
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
import Zmqx.Core.SocketFinalizer (SocketFinalizer, compactSocketFinalizers, runSocketFinalizer)
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
globalSocketFinalizersRef :: IORef [SocketFinalizer]
globalSocketFinalizersRef =
  unsafePerformIO (newIORef [])
{-# NOINLINE globalSocketFinalizersRef #-}

-- | Run a main function.
--
-- This function must be called exactly once at a time, and must wrap all other calls to this library.
--
-- Shutdown is strict about socket and context teardown: cleanup waits for socket finalizers to
-- run and for @zmq_ctx_term@ to complete. This keeps the default API correctness-first. It does
-- not promise delivery-preserving shutdown for queued outbound messages: we set @ZMQ_BLOCKY = 0@
-- when creating the context, so new sockets default to @ZMQ_LINGER = 0@. If an opt-in timeout or
-- best-effort shutdown mode is added later, it should live on a separate API path rather than
-- weakening 'run''s current contract.
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
--
-- Like 'run', teardown is strict about socket and context termination. It does not promise that
-- queued outbound messages will be drained before return, because contexts are created with
-- @ZMQ_BLOCKY = 0@. Callers that need timeout or best-effort shutdown semantics should use a
-- dedicated opt-in API if one is added later, rather than relying on 'withContext' to abandon
-- cleanup work.
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

-- | Return the number of sockets that would still require teardown work for this context.
--
-- This is an opt-in diagnostic helper. It compacts dead or already-closed finalizers before
-- counting, so a non-zero result means there are still registered sockets whose close action
-- has not completed yet. Treat the result as advisory diagnostics, not as an exact live-socket
-- census. The helper itself does not keep sockets alive.
pendingSockets :: Context -> IO Int
pendingSockets Context {contextFinalizers} = do
  compactSocketFinalizers contextFinalizers
  length <$> readIORef contextFinalizers

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

-- Terminate a context strictly.
--
-- The current policy is to shut down the context, run all known socket finalizers, and then wait
-- until @zmq_ctx_term@ succeeds. Contexts are created with @ZMQ_BLOCKY = 0@, so this guarantees
-- strict socket/context teardown rather than delivery-preserving shutdown for queued outbound
-- messages. We do not currently expose timeout or best-effort termination on this path because
-- that would weaken the main API's lifetime guarantees.
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
  compactSocketFinalizers socketFinalizers
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
