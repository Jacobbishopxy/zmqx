{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Zmqx.Core.Monitor
  ( Event (..),
    decodeMonitorEvent,
    monitor,
  )
where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Bits ((.|.), shiftL)
import Data.Int (Int32)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Unique (hashUnique, newUnique)
import Data.Word (Word16, Word32, Word8)
import Foreign.C.Error (Errno (..))
import Zmqx.Core.Options qualified as Options
import Zmqx.Core.Socket qualified as Socket
import Zmqx.Error (Error (..), catchingOkErrors, enrichError, throwOkError, unexpectedError)
import Zmqx.Internal
import Zmqx.Pair qualified as Pair

generateMonitorEndpoint :: IO Text
generateMonitorEndpoint = do
  unique <- newUnique
  pure ("inproc://zmqx-monitor-" <> Text.pack (show (hashUnique unique)))

monitor :: Socket.Socket a -> IO (Either Error (IO (Either Error Event)))
monitor Socket.Socket {zsocket, context, name} = do
  endpoint <- generateMonitorEndpoint
  catchingOkErrors do
    zhs_socket_monitor zsocket endpoint ZMQ_EVENT_ALL
    pair <-
      Pair.openWith_
        context
        ( if Text.null name
            then Options.defaultOptions
            else Options.name (name <> " monitor")
        )
    Pair.connect_ pair endpoint
    let receiveEvent :: IO (Either Error Event)
        receiveEvent =
          Pair.receives pair >>= \case
            Left err -> pure (Left err)
            Right message ->
              decodeMonitorEvent message >>= \case
                Nothing -> receiveEvent
                Just event -> pure (Right event)
    pure receiveEvent

data Event
  = AcceptFailed {-# UNPACK #-} !Errno
  | Accepted {-# UNPACK #-} !Zmq_fd
  | BindFailed {-# UNPACK #-} !Errno
  | CloseFailed {-# UNPACK #-} !Errno
  | Closed {-# UNPACK #-} !Zmq_fd
  | ConnectDelayed
  | ConnectRetried {-# UNPACK #-} !Int
  | Connected {-# UNPACK #-} !Zmq_fd
  | Disconnected {-# UNPACK #-} !Zmq_fd
  | HandshakeFailedAuth {-# UNPACK #-} !Int
  | HandshakeFailedNoDetail
  | HandshakeFailedProtocol {-# UNPACK #-} !Zmq_protocol_error
  | HandshakeSucceeded
  | Listening {-# UNPACK #-} !Zmq_fd
  | MonitorStopped
  deriving stock (Eq)

instance Show Event where
  show event =
    case event of
      AcceptFailed errno -> "AcceptFailed " <> showErrno errno
      Accepted fd -> "Accepted " <> show fd
      BindFailed errno -> "BindFailed " <> showErrno errno
      CloseFailed errno -> "CloseFailed " <> showErrno errno
      Closed fd -> "Closed " <> show fd
      ConnectDelayed -> "ConnectDelayed"
      ConnectRetried n -> "ConnectRetried " <> show n
      Connected fd -> "Connected " <> show fd
      Disconnected fd -> "Disconnected " <> show fd
      HandshakeFailedAuth n -> "HandshakeFailedAuth " <> show n
      HandshakeFailedNoDetail -> "HandshakeFailedNoDetail"
      HandshakeFailedProtocol protocolError -> "HandshakeFailedProtocol " <> show protocolError
      HandshakeSucceeded -> "HandshakeSucceeded"
      Listening fd -> "Listening " <> show fd
      MonitorStopped -> "MonitorStopped"
    where
      showErrno (Errno errno) =
        show errno

decodeMonitorEvent :: [ByteString] -> IO (Maybe Event)
decodeMonitorEvent = \case
  [eventFrame, _endpoint] ->
    parseEventFrame eventFrame <&> \case
      Nothing -> Nothing
      Just (typ, value) ->
        case typ of
          ZMQ_EVENT_ACCEPTED -> Just (Accepted (parseFdEventValue value))
          ZMQ_EVENT_ACCEPT_FAILED -> Just (AcceptFailed (parseErrnoEventValue value))
          ZMQ_EVENT_BIND_FAILED -> Just (BindFailed (parseErrnoEventValue value))
          ZMQ_EVENT_CLOSED -> Just (Closed (parseFdEventValue value))
          ZMQ_EVENT_CLOSE_FAILED -> Just (CloseFailed (parseErrnoEventValue value))
          ZMQ_EVENT_CONNECTED -> Just (Connected (parseFdEventValue value))
          ZMQ_EVENT_CONNECT_DELAYED -> Just ConnectDelayed
          ZMQ_EVENT_CONNECT_RETRIED -> Just (ConnectRetried (parseIntEventValue value))
          ZMQ_EVENT_DISCONNECTED -> Just (Disconnected (parseFdEventValue value))
          ZMQ_EVENT_HANDSHAKE_FAILED_AUTH -> Just (HandshakeFailedAuth (parseIntEventValue value))
          ZMQ_EVENT_HANDSHAKE_FAILED_NO_DETAIL -> Just HandshakeFailedNoDetail
          ZMQ_EVENT_HANDSHAKE_FAILED_PROTOCOL -> Just (HandshakeFailedProtocol (parseProtocolErrorValue value))
          ZMQ_EVENT_HANDSHAKE_SUCCEEDED -> Just HandshakeSucceeded
          ZMQ_EVENT_LISTENING -> Just (Listening (parseFdEventValue value))
          ZMQ_EVENT_MONITOR_STOPPED -> Just MonitorStopped
          _ -> Nothing
  _ -> pure Nothing
  where
    parseEventFrame :: ByteString -> IO (Maybe (Zmq_socket_events, Int32))
    parseEventFrame bytes
      | ByteString.length bytes /= 6 = pure Nothing
      | otherwise =
          let typ =
                decodeWord16Host
                  (ByteString.index bytes 0)
                  (ByteString.index bytes 1)
              value =
                decodeInt32Host
                  (ByteString.index bytes 2)
                  (ByteString.index bytes 3)
                  (ByteString.index bytes 4)
                  (ByteString.index bytes 5)
           in pure (Just (Zmq_socket_events (fromIntegral typ), value))

    parseErrnoEventValue :: Int32 -> Errno
    parseErrnoEventValue =
      Errno . fromIntegral

    parseFdEventValue :: Int32 -> Zmq_fd
    parseFdEventValue =
      fromIntegral

    parseIntEventValue :: Int32 -> Int
    parseIntEventValue =
      fromIntegral

    parseProtocolErrorValue :: Int32 -> Zmq_protocol_error
    parseProtocolErrorValue =
      Zmq_protocol_error . fromIntegral

    -- libzmq monitor events are a native-endian 16-bit tag plus a native-endian
    -- 32-bit value. Decode them bytewise so the parser does not rely on
    -- unaligned reads at offset 2 on strict-alignment platforms.
    decodeWord16Host :: Word8 -> Word8 -> Word16
#if defined(WORDS_BIGENDIAN)
    decodeWord16Host b0 b1 =
      (fromIntegral b0 `shiftL` 8)
        .|. fromIntegral b1
#else
    decodeWord16Host b0 b1 =
      fromIntegral b0
        .|. (fromIntegral b1 `shiftL` 8)
#endif

    decodeInt32Host :: Word8 -> Word8 -> Word8 -> Word8 -> Int32
#if defined(WORDS_BIGENDIAN)
    decodeInt32Host b0 b1 b2 b3 =
      fromIntegral
        ( (fromIntegral b0 `shiftL` 24)
            .|. (fromIntegral b1 `shiftL` 16)
            .|. (fromIntegral b2 `shiftL` 8)
            .|. fromIntegral b3 ::
              Word32
        )
#else
    decodeInt32Host b0 b1 b2 b3 =
      fromIntegral
        ( fromIntegral b0
            .|. (fromIntegral b1 `shiftL` 8)
            .|. (fromIntegral b2 `shiftL` 16)
            .|. (fromIntegral b3 `shiftL` 24) ::
              Word32
        )
#endif

zhs_socket_monitor :: Zmq_socket -> Text -> Zmq_socket_events -> IO ()
zhs_socket_monitor socket endpoint events =
  zmq_socket_monitor socket endpoint events >>= \case
    Left errno ->
      let err = enrichError "zmq_socket_monitor" errno
       in case errno of
            EINVAL -> throwIO err
            ETERM -> throwOkError err
            EPROTONOSUPPORT -> throwIO err
            _ -> unexpectedError err
    Right () -> pure ()
