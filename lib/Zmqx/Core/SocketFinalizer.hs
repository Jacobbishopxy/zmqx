{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Zmqx.Core.SocketFinalizer
  ( SocketFinalizer,
    makeSocketFinalizer,
    compactSocketFinalizers,
    runSocketFinalizer,
  )
where

import Control.Exception (mask, onException)
import Control.Monad (filterM)
import Data.Functor (void)
import Data.IORef
import GHC.Base (mkWeak#)
import GHC.Exts (TYPE, UnliftedRep)
import GHC.IO (IO (..), unIO)
import GHC.Weak (Weak (..))
import Zmqx.Internal (Zmq_error)

-- | A socket finalizer is a weak reference to an idempotent linger+close action.
--
-- Why idempotent: in the case that we explicitly close the socket first (during context teardown), its registered
-- finalizer will still run when the socket is GC'd, and we don't want to finalize more than once per socket.
data SocketFinalizer = SocketFinalizer
  { weakSocketFinalizer :: !(Weak ()),
    runSocketFinalizer :: !(IO ()),
    socketFinalizerClosed :: !(IORef Bool)
  }

makeSocketFinalizer ::
  forall (canary# :: TYPE UnliftedRep).
  -- zmq_close
  IO (Either Zmq_error ()) ->
  IORef [SocketFinalizer] ->
  canary# ->
  IO SocketFinalizer
makeSocketFinalizer close finalizersRef canary# = do
  (idempotentClose, closedRef) <- makeIdempotent (void close)
  weak <- makeWeakPointer canary# () (idempotentClose >> compactSocketFinalizers finalizersRef)
  pure
    SocketFinalizer
      { weakSocketFinalizer = weak,
        runSocketFinalizer = idempotentClose,
        socketFinalizerClosed = closedRef
      }

makeWeakPointer :: forall (key# :: TYPE UnliftedRep) value. key# -> value -> IO () -> IO (Weak value)
makeWeakPointer key# value finalizer =
  IO \s0 ->
    case mkWeak# key# value (unIO finalizer) s0 of
      (# s1, weak #) -> (# s1, Weak weak #)

compactSocketFinalizers :: IORef [SocketFinalizer] -> IO ()
compactSocketFinalizers finalizersRef =
  mask \restore -> do
    snapshot <- atomicModifyIORef' finalizersRef \finalizers -> ([], finalizers)
    let restoreSnapshot =
          atomicModifyIORef' finalizersRef \newerFinalizers -> (newerFinalizers ++ snapshot, ())
    liveFinalizers <-
      restore (filterM isSocketFinalizerAlive snapshot)
        `onException` restoreSnapshot
    atomicModifyIORef' finalizersRef \newerFinalizers -> (newerFinalizers ++ liveFinalizers, ())

isSocketFinalizerAlive :: SocketFinalizer -> IO Bool
isSocketFinalizerAlive SocketFinalizer {socketFinalizerClosed} =
  not <$> readIORef socketFinalizerClosed

makeIdempotent :: IO () -> IO (IO (), IORef Bool)
makeIdempotent action = do
  hasRunRef <- newIORef False
  closedRef <- newIORef False
  let runOnce = do
        shouldRun <-
          atomicModifyIORef' hasRunRef \hasRun ->
            if hasRun
              then (True, False)
              else (True, True)
        if shouldRun
          then do
            action
            writeIORef closedRef True
          else pure ()
  pure (runOnce, closedRef)
