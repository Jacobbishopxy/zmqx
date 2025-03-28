{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zmqx.Router
  ( Router,
    defaultOptions,
    sendQueueSize,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    sends,
    receives,
    receivesFor,
  )
where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Numeric.Natural (Natural)
import Zmqx.Core.Options (Options)
import Zmqx.Core.Options qualified as Options
import Zmqx.Core.Poll qualified as Poll
import Zmqx.Core.Socket (CanReceives, CanReceivesFor, CanSends, Socket (..))
import Zmqx.Core.Socket qualified as Socket
import Zmqx.Error (Error, catchingOkErrors, throwOkError)
import Zmqx.Internal

-- | A thread-safe __router__ socket.
--
-- Valid peers: __dealer__, __requester__, __router__
type Router =
  Socket "ROUTER"

instance Options.CanSetSendQueueSize Router

instance CanSends Router where
  sends_ = sends

instance CanReceives Router where
  receives_ = receives

instance CanReceivesFor Router where
  receivesFor_ = receivesFor

defaultOptions :: Options Router
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options Router
sendQueueSize =
  Options.sendQueueSize

-- | Open a __router__.
open :: Options Router -> IO (Either Error Router)
open options =
  catchingOkErrors do
    Socket.openSocket
      ZMQ_ROUTER
      (Options.sockopt ZMQ_ROUTER_MANDATORY 1 <> options)
      Socket.RouterExtra

-- | Bind a __router__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Router -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __router__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Router -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __router__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Router -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __router__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Router -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __multiframe message__ on a __router__ to a peer.
--
-- If the peer no longer exists, returns @EHOSTUNREACH@.
sends :: Router -> [ByteString] -> IO (Either Error ())
sends socket = \case
  [] -> pure (Right ())
  frame : frames -> do
    let message = frame :| frames
    catchingOkErrors do
      -- First try a non-blocking send, but if that doesn't work, try a blocking send. We'll get EAGAIN if the peer we
      -- are we are trying to send to has reached its high-water mark. In this case, waiting for the socket to become
      -- writable is not useful for a router - we want to block until we can send to *this* peer. So we do that with a
      -- safe FFI call to zmq_send without ZMQ_DONTWAIT. Note that this means while we're blocking in send, other
      -- threads can't receive on this router.
      Socket.sendManyDontWait socket message >>= \case
        True -> pure ()
        False -> Socket.sendMany socket message

-- | Receive a __multiframe message__ on a __router__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receives'
receives :: Router -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)

-- | Receive a __multiframe message__ on a __dealer__ from any peer (fair-queued) with a timeout.
--
-- The timeout is specified in milliseconds. If no message is available within the timeout,
-- returns `Right Nothing`. If a message is received, returns `Right (Just message)`.
-- If an error occurs, returns `Left error`.
receivesFor :: Router -> Int -> IO (Either Error (Maybe [ByteString]))
receivesFor socket timeout =
  catchingOkErrors do
    Poll.pollFor (Poll.the socket) timeout >>= \case
      Right Nothing -> pure Nothing
      Right (Just (Poll.Ready isReady)) ->
        if isReady socket
          then do
            frame :| frames <- Socket.receiveMany socket
            pure (Just (frame : frames))
          else pure Nothing
      Left err -> throwOkError err
