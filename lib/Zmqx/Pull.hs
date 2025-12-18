{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zmqx.Pull
  ( Pull,
    defaultOptions,
    sendQueueSize,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    receive,
    receives,
    receivesFor,
  )
where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Numeric.Natural (Natural)
import Zmqx.Core.Context (Context, ContextualOpen (..))
import Zmqx.Core.Options (Options)
import Zmqx.Core.Options qualified as Options
import Zmqx.Core.Poll qualified as Poll
import Zmqx.Core.Socket (CanReceive, CanReceives, CanReceivesFor, Socket (..))
import Zmqx.Core.Socket qualified as Socket
import Zmqx.Error
import Zmqx.Internal

-- | A thread-safe __puller__ socket.
--
-- Valid peers: __pusher__
type Pull =
  Socket "PULL"

instance Options.CanSetSendQueueSize Pull

instance CanReceive Pull where
  receive_ = receive

instance CanReceives Pull where
  receives_ = receives

instance CanReceivesFor Pull where
  receivesFor_ = receivesFor

defaultOptions :: Options Pull
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options Pull
sendQueueSize =
  Options.sendQueueSize

-- Open a __puller__.
open :: Options Pull -> IO (Either Error Pull)
open options =
  catchingOkErrors do
    Socket.openSocket ZMQ_PULL options Socket.PullExtra

instance ContextualOpen Pull where
  -- Open a __puller__ with an explicit context.
  openWith :: Context -> Options Pull -> IO (Either Error Pull)
  openWith context options =
    catchingOkErrors do
      Socket.openSocketIn context ZMQ_PULL options Socket.PullExtra

-- | Bind a __puller__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Pull -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __puller__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Pull -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __puller__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Pull -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __puller__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Pull -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Receive a __message__ on a __puller__ from one peer (fair-queued).
--
-- /Alias/: 'Zmq.receive'
receive :: Pull -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors (Socket.receiveOne socket)

-- | Receive a __multiframe message__ on a __puller__ from one peer (fair-queued).
--
-- /Alias/: 'Zmq.receives'
receives :: Pull -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)

-- | Receive a __multiframe message__ on a __dealer__ from any peer (fair-queued) with a timeout.
--
-- The timeout is specified in milliseconds. If no message is available within the timeout,
-- returns `Right Nothing`. If a message is received, returns `Right (Just message)`.
-- If an error occurs, returns `Left error`.
receivesFor :: Pull -> Int -> IO (Either Error (Maybe [ByteString]))
receivesFor socket timeout =
  catchingOkErrors do
    Poll.pollFor (Poll.pollIn socket) timeout >>= \case
      Right Nothing -> pure Nothing
      Right (Just (Poll.Ready isReady)) ->
        if isReady socket
          then do
            frame :| frames <- Socket.receiveMany socket
            pure (Just (frame : frames))
          else pure Nothing
      Left err -> throwOkError err
