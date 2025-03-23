{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zmqx.Dealer
  ( Dealer,
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

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Numeric.Natural (Natural)
import Zmqx.Core.Options (Options)
import Zmqx.Core.Options qualified as Options
import Zmqx.Core.Poll qualified as Poll
import Zmqx.Core.Socket (CanReceive, CanReceives, CanReceivesFor, CanSend, CanSends, Socket (..))
import Zmqx.Core.Socket qualified as Socket
import Zmqx.Error (Error (..), catchingOkErrors, throwOkError)
import Zmqx.Internal

-- | A thread-safe __dealer__ socket.
--
-- Valid peers: __dealer__, __replier__, __router__
type Dealer =
  Socket "DEALER"

instance Options.CanSetSendQueueSize Dealer

instance CanSend Dealer where
  send_ = send

instance CanSends Dealer where
  sends_ = sends

instance CanReceive Dealer where
  receive_ = receive

instance CanReceives Dealer where
  receives_ = receives

instance CanReceivesFor Dealer where
  receivesFor_ = receivesFor

defaultOptions :: Options Dealer
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options Dealer
sendQueueSize =
  Options.sendQueueSize

-- | Open a __dealer__.
open :: Options Dealer -> IO (Either Error Dealer)
open options =
  catchingOkErrors do
    Socket.openSocket ZMQ_DEALER options Socket.DealerExtra

-- | Bind a __dealer__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Dealer -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __dealer__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Dealer -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __dealer__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Dealer -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __dealer__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Dealer -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __message__ on a __dealer__ to one peer (round-robin).
--
-- This operation blocks until a peer can receive the message.
--
-- /Alias/: 'Zmq.send'
send :: Dealer -> ByteString -> IO (Either Error ())
send socket@Socket {zsocket} frame =
  catchingOkErrors loop
  where
    loop = do
      sent <- Socket.sendOneDontWait socket frame False
      when (not sent) do
        Socket.blockUntilCanSend zsocket
        loop

-- | Send a __multiframe message__ on a __dealer__ to one peer (round-robin).
--
-- This operation blocks until a peer can receive the message.
sends :: Dealer -> [ByteString] -> IO (Either Error ())
sends socket@Socket {zsocket} = \case
  [] -> pure (Right ())
  frame : frames -> do
    let loop = do
          sent <- Socket.sendManyDontWait socket (frame :| frames)
          when (not sent) do
            Socket.blockUntilCanSend zsocket
            loop
    catchingOkErrors loop

-- | Receive a __message__ on a __dealer__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receive'
receive :: Dealer -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors (Socket.receiveOne socket)

-- | Receive a __multiframe message__ on a __dealer__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receives'
receives :: Dealer -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)

-- | Receive a __multiframe message__ on a __dealer__ from any peer (fair-queued) with a timeout.
--
-- The timeout is specified in milliseconds. If no message is available within the timeout,
-- returns `Right Nothing`. If a message is received, returns `Right (Just message)`.
-- If an error occurs, returns `Left error`.
receivesFor :: Dealer -> Int -> IO (Either Error (Maybe [ByteString]))
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
