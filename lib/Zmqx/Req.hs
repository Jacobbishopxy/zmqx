{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zmqx.Req
  ( Req,
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
import Control.Exception (catch, throwIO)
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Numeric.Natural (Natural)
import System.Timeout qualified as Timeout
import Zmqx.Core.Context (Context, ContextualOpen (..))
import Zmqx.Core.Options (Options)
import Zmqx.Core.Options qualified as Options
import Zmqx.Core.Socket (CanReceive, CanReceives, CanReceivesFor, CanSend, CanSends, Socket (..))
import Zmqx.Core.Socket qualified as Socket
import Zmqx.Error (Error (..), catchingOkErrors)
import Zmqx.Internal

-- | A __requester__ socket.
--
-- Valid peers: __replier__, __router__
type Req =
  Socket "REQ"

instance Options.CanSetSendQueueSize Req

instance CanSend Req where
  send_ = send

instance CanSends Req where
  sends_ = sends

instance CanReceive Req where
  receive_ = receive

instance CanReceives Req where
  receives_ = receives

instance CanReceivesFor Req where
  receivesFor_ = receivesFor

defaultOptions :: Options Req
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options Req
sendQueueSize =
  Options.sendQueueSize

-- Open a __requester__.
open :: Options Req -> IO (Either Error Req)
open options =
  catchingOkErrors do
    messageBuffer <- newIORef Nothing
    Socket.openSocket
      ZMQ_REQ
      ( Options.sockopt ZMQ_REQ_CORRELATE 1
          <> Options.sockopt ZMQ_REQ_RELAXED 1
          <> options
      )
      (Socket.ReqExtra messageBuffer)

instance ContextualOpen Req where
  -- Open a __requester__ with an explicit context.
  openWith :: Context -> Options Req -> IO (Either Error Req)
  openWith context options =
    catchingOkErrors do
      messageBuffer <- newIORef Nothing
      Socket.openSocketIn
        context
        ZMQ_REQ
        ( Options.sockopt ZMQ_REQ_CORRELATE 1
            <> Options.sockopt ZMQ_REQ_RELAXED 1
            <> options
        )
        (Socket.ReqExtra messageBuffer)

-- | Bind a __requester__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Req -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __requester__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Req -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __requester__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Req -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __requester__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Req -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __message__ on a __requester__ to one peer (round-robin).
--
-- This operation blocks until a peer can receive the message.
--
-- /Alias/: 'Zmq.send'
send :: Req -> ByteString -> IO (Either Error ())
send socket@Socket {zsocket} frame = do
  catchingOkErrors do
    let loop =
          Socket.sendOneDontWait socket frame False >>= \case
            False -> do
              Socket.blockUntilCanSend zsocket
              loop
            True -> pure ()
    loop

-- | Send a __multiframe message__ on a __requester__ to one peer (round-robin).
--
-- This operation blocks until a peer can receive the message.
sends :: Req -> [ByteString] -> IO (Either Error ())
sends socket@Socket {zsocket} = \case
  [] -> pure (Right ())
  frame : frames ->
    catchingOkErrors do
      let loop = do
            sent <- Socket.sendManyDontWait socket (frame :| frames)
            when (not sent) do
              Socket.blockUntilCanSend zsocket
              loop
      loop

-- | Receive a __message__ on a __requester__ from the last peer sent to.
--
-- /Alias/: 'Zmq.receive'
receive :: Req -> IO (Either Error ByteString)
receive socket@Socket {extra = Socket.ReqExtra messageBuffer} =
  -- Remember: this socket isn't thread safe, so we don't have to be very careful with our readIORef/writeIORefs
  readIORef messageBuffer >>= \case
    Nothing -> catchingOkErrors (Socket.receiveOne socket)
    Just (frame :| _) -> do
      writeIORef messageBuffer Nothing
      pure (Right frame)

-- | Receive a __multiframe message__ on a __requester__ from the last peer sent to.
--
-- /Alias/: 'Zmq.receives'
receives :: Req -> IO (Either Error [ByteString])
receives socket@Socket {extra = Socket.ReqExtra messageBuffer} =
  -- Remember: this socket isn't thread safe, so we don't have to be very careful with our readIORef/writeIORefs
  readIORef messageBuffer >>= \case
    Nothing ->
      catchingOkErrors do
        frame :| frames <- Socket.receiveMany socket
        pure (frame : frames)
    Just (frame :| frames) -> do
      writeIORef messageBuffer Nothing
      pure (Right (frame : frames))

-- | Receive a __multiframe reply__ on a __requester__ from the last peer sent to, with a timeout.
--
-- The timeout is specified in milliseconds. If no valid reply is available within the timeout,
-- returns `Right Nothing`. If a reply is received, returns `Right (Just reply)`.
-- If an error occurs, returns `Left error`.
receivesFor :: Req -> Int -> IO (Either Error (Maybe [ByteString]))
receivesFor socket@Socket {extra = Socket.ReqExtra messageBuffer, zsocket} timeoutMs =
  catchingOkErrors do
    let drainBuffer =
          readIORef messageBuffer >>= \case
            Nothing -> pure Nothing
            Just (frame :| frames) -> do
              writeIORef messageBuffer Nothing
              pure (Just (frame : frames))
        receiveDontWait =
          ( Socket.receiveManyDontWait socket <&> \case
              Nothing -> Nothing
              Just (frame :| frames) -> Just (frame : frames)
          )
            `catch` \case
              Error {errno = EFSM} -> pure Nothing
              err -> throwIO err
        receiveLoop =
          receiveDontWait >>= \case
            Just frames -> pure frames
            Nothing -> do
              Socket.blockUntilCanReceive zsocket
              receiveLoop
    drainBuffer >>= \case
      Just frames -> pure (Just frames)
      Nothing
        | timeoutMs < 0 ->
            Just <$> receiveLoop
        | timeoutMs == 0 ->
            receiveDontWait
        | otherwise ->
            Timeout.timeout (timeoutMs * 1000) receiveLoop >>= \case
              Just frames -> pure (Just frames)
              Nothing ->
                drainBuffer >>= \case
                  Just frames -> pure (Just frames)
                  Nothing -> receiveDontWait
