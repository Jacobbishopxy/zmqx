{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}

module Zmqx.Monad
  ( ZmqxT (..),
    MonadZmqx (..),
    runZmqx,
    runZmqxT,
    open,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Zmqx qualified
import Zmqx.Core.Context (Context, ContextualOpen)
import Zmqx.Core.Options (Options)
import Zmqx.Error (Error)

newtype ZmqxT m a = ZmqxT {unZmqxT :: ReaderT Context m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Context)

type role ZmqxT representational nominal

instance MonadTrans ZmqxT where
  lift =
    ZmqxT . lift

class MonadIO m => MonadZmqx m where
  askContext :: m Context

instance MonadIO m => MonadZmqx (ZmqxT m) where
  askContext =
    ask

runZmqxT :: Context -> ZmqxT m a -> m a
runZmqxT context (ZmqxT action) =
  runReaderT action context

runZmqx :: Options () -> ZmqxT IO a -> IO a
runZmqx options action =
  Zmqx.withContext options \context ->
    runZmqxT context action

open :: (MonadZmqx m, ContextualOpen socket) => Options socket -> m (Either Error socket)
open options = do
  context <- askContext
  liftIO (Zmqx.openWith context options)
