{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zmqx.Rep
  ( Rep,
    defaultOptions,
    sendQueueSize,
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
  )
where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Numeric.Natural (Natural)
import Zmqx.Core.Options (Options)
import Zmqx.Core.Options qualified as Options
import Zmqx.Core.Poll qualified as Poll
import Zmqx.Core.Socket (CanReceive, CanReceives, CanReceivesFor, CanSend, CanSends, Socket (..))
import Zmqx.Core.Socket qualified as Socket
import Zmqx.Error (Error, catchingOkErrors, throwOkError)
import Zmqx.Internal

-- | A __replier__ socket.
--
-- Valid peers: __dealer__, __requester__
type Rep =
  Socket "REP"

instance Options.CanSetSendQueueSize Rep

instance CanSend Rep where
  send_ = send

instance CanSends Rep where
  sends_ = sends

instance CanReceive Rep where
  receive_ = receive

instance CanReceives Rep where
  receives_ = receives

instance CanReceivesFor Rep where
  receivesFor_ = receivesFor

defaultOptions :: Options Rep
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options Rep
sendQueueSize =
  Options.sendQueueSize

-- | Open a __replier__.
open :: Options Rep -> IO (Either Error Rep)
open options =
  catchingOkErrors do
    Socket.openSocket ZMQ_REP options Socket.RepExtra

-- | Bind a __replier__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Rep -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __replier__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Rep -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __replier__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Rep -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __replier__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Rep -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __message__ on a __replier__ to the last peer received from.
--
-- If the last peer received from no longer exists, the message is discarded.
--
-- /Alias/: 'Zmq.send'
send :: Rep -> ByteString -> IO (Either Error ())
send socket frame =
  catchingOkErrors do
    Socket.sendOneWontBlock socket frame False

-- | Send a __multiframe message__ on a __replier__ to the last peer received from.
--
-- If the last peer received from no longer exists, the message is discarded.
sends :: Rep -> [ByteString] -> IO (Either Error ())
sends socket = \case
  [] -> pure (Right ())
  frame : frames ->
    catchingOkErrors do
      Socket.sendManyWontBlock socket (frame :| frames)

-- | Receive a __message__ on a __replier__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receive'
receive :: Rep -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors (Socket.receiveOne socket)

-- | Receive a __multiframe message__ on a __replier__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receives'
receives :: Rep -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)

-- | Receive a __multiframe message__ on a __dealer__ from any peer (fair-queued) with a timeout.
--
-- The timeout is specified in milliseconds. If no message is available within the timeout,
-- returns `Right Nothing`. If a message is received, returns `Right (Just message)`.
-- If an error occurs, returns `Left error`.
receivesFor :: Rep -> Int -> IO (Either Error (Maybe [ByteString]))
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
