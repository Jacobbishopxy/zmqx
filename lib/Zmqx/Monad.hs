{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Zmqx.Monad
  ( ZmqxT (..),
    MonadZmqx (..),
    runZmqx,
    runZmqxT,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    send,
    sends,
    receive,
    receives,
    receivesFor,
    monitor,
    Sockets,
    Ready (..),
    PollEvent (..),
    pollIn,
    pollInAlso,
    pollOut,
    pollOutAlso,
    poll,
    pollFor,
    pollUntil,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word64)
import Zmqx qualified
import Zmqx.Core.Context (Context, ContextualOpen)
import Zmqx.Core.Options (Options)
import Zmqx.Core.Poll (PollEvent (..), Ready (..), Sockets, pollIn, pollInAlso, pollOut, pollOutAlso)
import Zmqx.Core.Socket qualified as Socket
import Zmqx.Error (Error)

newtype ZmqxT m a = ZmqxT {unZmqxT :: ReaderT Context m a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Context)

type role ZmqxT representational nominal

instance Eq Ready where
  Ready _ == Ready _ = False

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

bind :: (MonadIO m) => Socket.Socket a -> Text -> m (Either Error ())
bind socket endpoint =
  liftIO (Zmqx.bind socket endpoint)

unbind :: (MonadIO m) => Socket.Socket a -> Text -> m ()
unbind socket =
  liftIO . Zmqx.unbind socket

connect :: (MonadIO m) => Socket.Socket a -> Text -> m (Either Error ())
connect socket endpoint =
  liftIO (Zmqx.connect socket endpoint)

disconnect :: (MonadIO m) => Socket.Socket a -> Text -> m ()
disconnect socket =
  liftIO . Zmqx.disconnect socket

send :: (MonadIO m, Socket.CanSend socket) => socket -> ByteString -> m (Either Error ())
send socket payload =
  liftIO (Zmqx.send socket payload)

sends :: (MonadIO m, Socket.CanSends socket) => socket -> [ByteString] -> m (Either Error ())
sends socket payloads =
  liftIO (Zmqx.sends socket payloads)

receive :: (MonadIO m, Socket.CanReceive socket) => socket -> m (Either Error ByteString)
receive =
  liftIO . Zmqx.receive

receives :: (MonadIO m, Socket.CanReceives socket) => socket -> m (Either Error [ByteString])
receives =
  liftIO . Zmqx.receives

receivesFor :: (MonadIO m, Socket.CanReceivesFor socket) => socket -> Int -> m (Either Error (Maybe [ByteString]))
receivesFor socket =
  liftIO . Zmqx.receivesFor socket

monitor :: (MonadIO m) => Socket.Socket a -> m (Either Error (IO (Either Error Zmqx.Event)))
monitor =
  liftIO . Zmqx.monitor

poll :: (MonadIO m) => Sockets -> m (Either Error Ready)
poll =
  liftIO . Zmqx.poll

pollFor :: (MonadIO m) => Sockets -> Int -> m (Either Error (Maybe Ready))
pollFor sockets =
  liftIO . Zmqx.pollFor sockets

pollUntil :: (MonadIO m) => Sockets -> Word64 -> m (Either Error (Maybe Ready))
pollUntil sockets =
  liftIO . Zmqx.pollUntil sockets
