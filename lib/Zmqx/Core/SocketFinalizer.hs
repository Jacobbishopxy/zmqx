{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Zmqx.Core.SocketFinalizer
  ( SocketFinalizer,
    SocketFinalizerRegistry,
    compactSocketFinalizers,
    makeSocketFinalizer,
    newSocketFinalizerRegistry,
    pendingSocketFinalizers,
    registeredSocketFinalizers,
    registerSocketFinalizer,
    resetSocketFinalizerRegistry,
    runSocketFinalizer,
  )
where

import Control.Exception (mask, onException)
import Control.Monad (filterM, when)
import Data.Functor (void)
import Data.IORef
import GHC.Base (mkWeak#)
import GHC.Exts (TYPE, UnliftedRep)
import GHC.IO (IO (..), unIO)
import GHC.Weak (Weak (..))
import Zmqx.Internal (Zmq_error)

-- | A per-context registry for socket finalizers.
--
-- The registry keeps an exact count of close actions that have not completed yet.
-- Closed entries are compacted lazily so weak finalizers and diagnostic counts do
-- not scan the whole registry on every socket churn event.
data SocketFinalizerRegistry = SocketFinalizerRegistry
  { socketFinalizerRegistryState :: !(IORef SocketFinalizerRegistryState)
  }
  deriving stock (Eq)

data SocketFinalizerRegistryState = SocketFinalizerRegistryState
  { socketFinalizerRegistryFinalizers :: ![SocketFinalizer],
    socketFinalizerRegistryPendingCount :: !Int,
    socketFinalizerRegistryStaleCount :: !Int
  }

emptySocketFinalizerRegistryState :: SocketFinalizerRegistryState
emptySocketFinalizerRegistryState =
  SocketFinalizerRegistryState
    { socketFinalizerRegistryFinalizers = [],
      socketFinalizerRegistryPendingCount = 0,
      socketFinalizerRegistryStaleCount = 0
    }

-- | A socket finalizer is a weak reference to an idempotent linger+close action.
--
-- Why idempotent: in the case that we explicitly close the socket first (during context teardown), its registered
-- finalizer will still run when the socket is GC'd, and we don't want to finalize more than once per socket.
data SocketFinalizer = SocketFinalizer
  { weakSocketFinalizer :: !(Weak ()),
    runSocketFinalizer :: !(IO ()),
    socketFinalizerClosed :: !(IORef Bool)
  }

newSocketFinalizerRegistry :: IO SocketFinalizerRegistry
newSocketFinalizerRegistry =
  SocketFinalizerRegistry <$> newIORef emptySocketFinalizerRegistryState

resetSocketFinalizerRegistry :: SocketFinalizerRegistry -> IO ()
resetSocketFinalizerRegistry SocketFinalizerRegistry {socketFinalizerRegistryState} =
  atomicWriteIORef socketFinalizerRegistryState emptySocketFinalizerRegistryState

registerSocketFinalizer :: SocketFinalizerRegistry -> SocketFinalizer -> IO ()
registerSocketFinalizer SocketFinalizerRegistry {socketFinalizerRegistryState} finalizer =
  atomicModifyIORef' socketFinalizerRegistryState \registryState ->
    ( registryState
        { socketFinalizerRegistryFinalizers = finalizer : socketFinalizerRegistryFinalizers registryState,
          socketFinalizerRegistryPendingCount = socketFinalizerRegistryPendingCount registryState + 1
        },
      ()
    )

pendingSocketFinalizers :: SocketFinalizerRegistry -> IO Int
pendingSocketFinalizers SocketFinalizerRegistry {socketFinalizerRegistryState} =
  socketFinalizerRegistryPendingCount <$> readIORef socketFinalizerRegistryState

registeredSocketFinalizers :: SocketFinalizerRegistry -> IO [SocketFinalizer]
registeredSocketFinalizers SocketFinalizerRegistry {socketFinalizerRegistryState} =
  socketFinalizerRegistryFinalizers <$> readIORef socketFinalizerRegistryState

makeSocketFinalizer ::
  forall (canary# :: TYPE UnliftedRep).
  -- zmq_close
  IO (Either Zmq_error ()) ->
  SocketFinalizerRegistry ->
  canary# ->
  IO SocketFinalizer
makeSocketFinalizer close registry canary# = do
  (idempotentClose, closedRef) <- makeIdempotent (void close) (markSocketFinalizerClosed registry)
  weak <- makeWeakPointer canary# () (idempotentClose >> compactSocketFinalizersWhenStale registry)
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

compactSocketFinalizersWhenStale :: SocketFinalizerRegistry -> IO ()
compactSocketFinalizersWhenStale registry@SocketFinalizerRegistry {socketFinalizerRegistryState} = do
  shouldCompact <- socketFinalizerRegistryShouldCompact <$> readIORef socketFinalizerRegistryState
  when shouldCompact (compactSocketFinalizers registry)

socketFinalizerRegistryCompactionThreshold :: Int
socketFinalizerRegistryCompactionThreshold = 64

socketFinalizerRegistryShouldCompact :: SocketFinalizerRegistryState -> Bool
socketFinalizerRegistryShouldCompact SocketFinalizerRegistryState {socketFinalizerRegistryPendingCount, socketFinalizerRegistryStaleCount} =
  socketFinalizerRegistryStaleCount >= socketFinalizerRegistryCompactionThreshold
    && socketFinalizerRegistryStaleCount >= socketFinalizerRegistryPendingCount

markSocketFinalizerClosed :: SocketFinalizerRegistry -> IO ()
markSocketFinalizerClosed SocketFinalizerRegistry {socketFinalizerRegistryState} =
  atomicModifyIORef' socketFinalizerRegistryState \registryState ->
    ( registryState
        { socketFinalizerRegistryPendingCount = max 0 (socketFinalizerRegistryPendingCount registryState - 1),
          socketFinalizerRegistryStaleCount = socketFinalizerRegistryStaleCount registryState + 1
        },
      ()
    )

compactSocketFinalizers :: SocketFinalizerRegistry -> IO ()
compactSocketFinalizers SocketFinalizerRegistry {socketFinalizerRegistryState} =
  mask \restore -> do
    snapshot <-
      atomicModifyIORef' socketFinalizerRegistryState \registryState ->
        ( registryState {socketFinalizerRegistryFinalizers = []},
          socketFinalizerRegistryFinalizers registryState
        )
    let restoreSnapshot =
          atomicModifyIORef' socketFinalizerRegistryState \newerRegistryState ->
            ( newerRegistryState
                { socketFinalizerRegistryFinalizers =
                    socketFinalizerRegistryFinalizers newerRegistryState ++ snapshot
                },
              ()
            )
    liveFinalizers <-
      restore (filterM isSocketFinalizerAlive snapshot)
        `onException` restoreSnapshot
    let removedCount = length snapshot - length liveFinalizers
    atomicModifyIORef' socketFinalizerRegistryState \newerRegistryState ->
      ( newerRegistryState
          { socketFinalizerRegistryFinalizers =
              socketFinalizerRegistryFinalizers newerRegistryState ++ liveFinalizers,
            socketFinalizerRegistryStaleCount =
              max 0 (socketFinalizerRegistryStaleCount newerRegistryState - removedCount)
          },
        ()
      )

isSocketFinalizerAlive :: SocketFinalizer -> IO Bool
isSocketFinalizerAlive SocketFinalizer {socketFinalizerClosed} =
  not <$> readIORef socketFinalizerClosed

makeIdempotent :: IO () -> IO () -> IO (IO (), IORef Bool)
makeIdempotent action onClose = do
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
            onClose
          else pure ()
  pure (runOnce, closedRef)
