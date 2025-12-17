{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zmqx.Pair
  ( Pair,
    defaultOptions,
    sendQueueSize,
    open,
    open_,
    bind,
    unbind,
    connect,
    connect_,
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

-- | A thread-safe __pair__ socket.
--
-- Valid peers: __pair__
type Pair =
  Socket "PAIR"

instance Options.CanSetSendQueueSize Pair

instance CanSend Pair where
  send_ = send

instance CanSends Pair where
  sends_ = sends

instance CanReceive Pair where
  receive_ = receive

instance CanReceives Pair where
  receives_ = receives

instance CanReceivesFor Pair where
  receivesFor_ = receivesFor

defaultOptions :: Options Pair
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options Pair
sendQueueSize =
  Options.sendQueueSize

-- | Open a __pair__.
open :: Options Pair -> IO (Either Error Pair)
open options =
  catchingOkErrors (open_ options)

open_ :: Options Pair -> IO Pair
open_ options =
  Socket.openSocket ZMQ_PAIR options Socket.PairExtra

-- | Bind a __pair__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Pair -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __pair__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Pair -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __pair__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Pair -> Text -> IO (Either Error ())
connect =
  Socket.connect

connect_ :: Pair -> Text -> IO ()
connect_ =
  Socket.connect_

-- | Disconnect a __pair__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Pair -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __message__ on a __pair__ to the peer.
--
-- This operation blocks until the peer can receive the message.
--
-- /Alias/: 'Zmq.send'
send :: Pair -> ByteString -> IO (Either Error ())
send socket@Socket {zsocket} frame =
  catchingOkErrors loop
  where
    loop = do
      sent <- Socket.sendOneDontWait socket frame False
      when (not sent) do
        Socket.blockUntilCanSend zsocket
        loop

-- | Send a __multiframe message__ on a __pair__ to the peer.
--
-- This operation blocks until the peer can receive the message.
sends :: Pair -> [ByteString] -> IO (Either Error ())
sends socket@Socket {zsocket} = \case
  [] -> pure (Right ())
  frame : frames -> do
    let loop = do
          sent <- Socket.sendManyDontWait socket (frame :| frames)
          when (not sent) do
            Socket.blockUntilCanSend zsocket
            loop
    catchingOkErrors loop

-- | Receive a __message__ on a __pair__ from the peer.
--
-- /Alias/: 'Zmq.receive'
receive :: Pair -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors (Socket.receiveOne socket)

-- | Receive a __multiframe message__ on a __pair__ from the peer.
--
-- /Alias/: 'Zmq.receives'
receives :: Pair -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)

-- | Receive a __multiframe message__ on a __dealer__ from any peer (fair-queued) with a timeout.
--
-- The timeout is specified in milliseconds. If no message is available within the timeout,
-- returns `Right Nothing`. If a message is received, returns `Right (Just message)`.
-- If an error occurs, returns `Left error`.
receivesFor :: Pair -> Int -> IO (Either Error (Maybe [ByteString]))
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
