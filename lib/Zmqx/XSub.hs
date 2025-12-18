{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zmqx.XSub
  ( XSub,
    defaultOptions,
    open,
    openWith,
    bind,
    unbind,
    connect,
    disconnect,
    subscribe,
    unsubscribe,
    send,
    sends,
    receive,
    receives,
    receivesFor,
  )
where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Zmqx.Core.Context (Context)
import Zmqx.Core.Options (Options)
import Zmqx.Core.Options qualified as Options
import Zmqx.Core.Poll qualified as Poll
import Zmqx.Core.Socket (CanReceive, CanReceives, CanReceivesFor, CanSend, Socket (..))
import Zmqx.Core.Socket qualified as Socket
import Zmqx.Error (Error, catchingOkErrors, throwOkError)
import Zmqx.Internal
import Zmqx.Subscription (pattern Subscribe, pattern Unsubscribe)

-- | A thread-safe __xsubscriber__ socket.
--
-- Valid peers: __publisher__, __xpublisher__
type XSub =
  Socket "XSUB"

instance CanSend XSub where
  send_ = send

instance CanReceive XSub where
  receive_ = receive

instance CanReceives XSub where
  receives_ = receives

instance CanReceivesFor XSub where
  receivesFor_ = receivesFor

defaultOptions :: Options XSub
defaultOptions =
  Options.defaultOptions

-- | Open an __xsubscriber__.
open :: Options XSub -> IO (Either Error XSub)
open options =
  catchingOkErrors do
    Socket.openSocket
      ZMQ_XSUB
      ( Options.sockopt ZMQ_SNDHWM 0 -- don't drop subscriptions
          <> options
      )
      Socket.XSubExtra

-- | Open an __xsubscriber__ with an explicit context.
openWith :: Context -> Options XSub -> IO (Either Error XSub)
openWith context options =
  catchingOkErrors do
    Socket.openSocketIn
      context
      ZMQ_XSUB
      ( Options.sockopt ZMQ_SNDHWM 0 -- don't drop subscriptions
          <> options
      )
      Socket.XSubExtra

-- | Bind an __xsubscriber__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: XSub -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind an __xsubscriber__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: XSub -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect an __xsubscriber__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: XSub -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect an __xsubscriber__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: XSub -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Subscribe an __xsubscriber__ to a __topic__ (prefix matching).
--
-- To subscribe to all topics, subscribe to the empty string.
subscribe :: XSub -> ByteString -> IO (Either Error ())
subscribe socket prefix =
  send socket (Subscribe prefix)

-- | Unsubscribe an __xsubscriber__ from a previously-subscribed __topic__.
unsubscribe :: XSub -> ByteString -> IO (Either Error ())
unsubscribe socket prefix =
  send socket (Unsubscribe prefix)

-- | Send a __message__ on an __xsubscriber__ to all peers.
--
-- This operation never blocks. All peers with full messages queues will not receive the message.
--
-- /Alias/: 'Zmq.send'
send :: XSub -> ByteString -> IO (Either Error ())
send socket frame =
  catchingOkErrors do
    Socket.sendOneWontBlock socket frame False

-- | Send a __multiframe message__ on an __xsubscriber__ to all peers.
--
-- This operation never blocks. All peers with full messages queues will not receive the message.
sends :: XSub -> [ByteString] -> IO (Either Error ())
sends socket = \case
  [] -> pure (Right ())
  frame : frames ->
    catchingOkErrors do
      Socket.sendManyWontBlock socket (frame :| frames)

-- | Receive a __message__ on an __xsubscriber__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receive'
receive :: XSub -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors (Socket.receiveOne socket)

-- | Receive a __multiframe message__ on an __xsubscriber__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receives'
receives :: XSub -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)

-- | Receive a __multiframe message__ on a __dealer__ from any peer (fair-queued) with a timeout.
--
-- The timeout is specified in milliseconds. If no message is available within the timeout,
-- returns `Right Nothing`. If a message is received, returns `Right (Just message)`.
-- If an error occurs, returns `Left error`.
receivesFor :: XSub -> Int -> IO (Either Error (Maybe [ByteString]))
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
