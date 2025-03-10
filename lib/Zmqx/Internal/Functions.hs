{-# LANGUAGE AllowAmbiguousTypes #-}

-- file: Functions.hs
-- author: Jacob Xie
-- date: 2025/02/27 10:17:37 Thursday
-- brief:

module Zmqx.Internal.Functions (module Zmqx.Internal.Functions) where

import Data.Array.MArray qualified as MArray
import Data.Array.Storable (StorableArray)
import Data.Array.Storable qualified as StorableArray
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal qualified as ByteString.Internal
import Data.ByteString.Unsafe qualified as ByteString.Unsafe
import Data.Coerce (coerce)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Text.Foreign qualified as Text
import Data.Text.Internal (Text (Text))
import Data.Void (Void)
import Data.Word (Word8)
import Foreign (FunPtr, Ptr, Storable (peek, poke, sizeOf), alloca, allocaBytes, castPtr, free, malloc, mallocBytes, nullPtr, withForeignPtr)
import Foreign.C (CChar (..), CInt (..), CLong (..), CSize (..), CUInt)
import Foreign.Marshal.Utils (copyBytes)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Zmqx.Internal.Bindings qualified
import Zmqx.Internal.Types
  ( Zmq_atomic_counter (..),
    Zmq_ctx (..),
    Zmq_ctx_option (..),
    Zmq_error (..),
    Zmq_msg (..),
    Zmq_msg_option (..),
    Zmq_reconnect_stop_option (..),
    Zmq_send_option (..),
    Zmq_socket (..),
    Zmq_socket_events (..),
    Zmq_socket_option (..),
    Zmq_socket_type (..),
    pattern EINVAL,
  )

------------------------------------------------------------------------------------------------------------------------
-- Error

-- Get errno. Not exported on purpose: we return Eithers intead.
zmq_errno :: IO Zmq_error
zmq_errno =
  coerce Zmqx.Internal.Bindings.zmq_errno

-- | Get the string of a ØMQ error number.
--
-- http://api.zeromq.org/master:zmq-strerror
zmq_strerror :: Zmq_error -> Text
zmq_strerror (Zmq_error errno) =
  Text.decodeUtf8 (unsafeDupablePerformIO (ByteString.Unsafe.unsafePackCString (Zmqx.Internal.Bindings.zmq_strerror errno)))

------------------------------------------------------------------------------------------------------------------------
-- Version

-- | The ØMQ library version.
--
-- http://api.zeromq.org/master:zmq-version
zmq_version :: (Int, Int, Int)
zmq_version =
  unsafeDupablePerformIO do
    alloca \px ->
      alloca \py ->
        alloca \pz -> do
          Zmqx.Internal.Bindings.zmq_version px py pz
          x <- peek px
          y <- peek py
          z <- peek pz
          pure
            ( fromIntegral @CInt @Int x,
              fromIntegral @CInt @Int y,
              fromIntegral @CInt @Int z
            )

------------------------------------------------------------------------------------------------------------------------
-- Context

-- | Get a ØMQ context option.
--
-- http://api.zeromq.org/master:zmq-ctx-get
zmq_ctx_get :: Zmq_ctx -> Zmq_ctx_option -> IO Int
zmq_ctx_get (Zmq_ctx context) (Zmq_ctx_option option) =
  -- We don't check errno here, because the only way this function can fail is if the user provides an invalid option.
  -- This should be impossible, as we have a Zmq_ctx_option type that only contains valid options. If the user unsafely
  -- uses its exposed constructor to call this function with an invalid option, ok... they'll get back a -1 and maybe
  -- have to call zmq_errno themselves.
  --
  -- This function's contract isn't great anyway: it claims to return -1 on error, but -1 is actually a valid return
  -- value for ZMQ_THREAD_PRIORITY, ZMQ_THREAD_SCHED_POLICY, and possibly others...
  fromIntegral @CInt @Int <$> Zmqx.Internal.Bindings.zmq_ctx_get context option

-- | Create a new ØMQ context.
--
-- http://api.zeromq.org/master:zmq-ctx-new
zmq_ctx_new :: IO Zmq_ctx
zmq_ctx_new =
  coerce Zmqx.Internal.Bindings.zmq_ctx_new

-- | Set a ØMQ context option.
--
-- http://api.zeromq.org/master:zmq-ctx-set
zmq_ctx_set :: Zmq_ctx -> Zmq_ctx_option -> Int -> IO (Either Zmq_error ())
zmq_ctx_set (Zmq_ctx context) (Zmq_ctx_option option) value =
  Zmqx.Internal.Bindings.zmq_ctx_set context option (fromIntegral @Int @CInt value) >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Shutdown a ØMQ context.
--
-- http://api.zeromq.org/master:zmq-ctx-shutdown
zmq_ctx_shutdown :: Zmq_ctx -> IO (Either Zmq_error ())
zmq_ctx_shutdown (Zmq_ctx context) =
  Zmqx.Internal.Bindings.zmq_ctx_shutdown context >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Terminate a ØMQ context.
--
-- http://api.zeromq.org/master:zmq-ctx-term
zmq_ctx_term :: Zmq_ctx -> IO (Either Zmq_error ())
zmq_ctx_term (Zmq_ctx context) =
  Zmqx.Internal.Bindings.zmq_ctx_term context >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

------------------------------------------------------------------------------------------------------------------------
-- Message

-- | Release a ØMQ message.
--
-- http://api.zeromq.org/master:zmq-msg-close
zmq_msg_close :: Zmq_msg -> IO (Either Zmq_error ())
zmq_msg_close (Zmq_msg message) =
  Zmqx.Internal.Bindings.zmq_msg_close message >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Copy the content of one ØMQ message to another.
--
-- http://api.zeromq.org/master:zmq-msg-copy
zmq_msg_copy :: Zmq_msg -> Zmq_msg -> IO (Either Zmq_error ())
zmq_msg_copy (Zmq_msg dst) (Zmq_msg src) =
  Zmqx.Internal.Bindings.zmq_msg_copy dst src >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Get a ØMQ message's content.
--
-- http://api.zeromq.org/master:zmq-msg-data
zmq_msg_data :: Zmq_msg -> IO ByteString
zmq_msg_data (Zmq_msg message) = do
  size <- Zmqx.Internal.Bindings.zmq_msg_size message
  bytes <- Zmqx.Internal.Bindings.zmq_msg_data message
  ByteString.packCStringLen (bytes, fromIntegral @CSize @Int size)

-- | Free a ØMQ message initialized by 'zmq_msg_init' or 'zmq_msg_init_data', or 'zmq_msg_init_size'.
zmq_msg_free :: Zmq_msg -> IO ()
zmq_msg_free (Zmq_msg message) =
  free message

-- | Get a ØMQ message metadata property.
--
-- http://api.zeromq.org/master:zmq-msg-gets
zmq_msg_gets :: Zmq_msg -> Text -> IO (Either Zmq_error Text)
zmq_msg_gets (Zmq_msg message) property = do
  value <- Text.withCString property (Zmqx.Internal.Bindings.zmq_msg_gets message)
  if value == nullPtr then Left <$> zmq_errno else Right <$> Text.fromPtr0 (castPtr @CChar @Word8 value)

-- | Get a ØMQ message option.
--
-- http://api.zeromq.org/master:zmq-msg-get
zmq_msg_get :: Zmq_msg -> Zmq_msg_option -> IO (Either Zmq_error Int)
zmq_msg_get (Zmq_msg message) (Zmq_msg_option option) =
  Zmqx.Internal.Bindings.zmq_msg_get message option >>= \case
    -1 -> Left <$> zmq_errno
    n -> pure (Right (fromIntegral @CInt @Int n))

-- | Initialise an empty ØMQ message.
--
-- The message must be freed with 'zmq_msg_free'.
--
-- http://api.zeromq.org/master:zmq-msg-init
zmq_msg_init :: IO Zmq_msg
zmq_msg_init = do
  message <- malloc
  _ <- Zmqx.Internal.Bindings.zmq_msg_init message -- always returns 0
  pure (Zmq_msg message)

-- | Initialise a ØMQ message from a buffer.
--
-- The message must be freed with 'zmq_msg_free'.
--
-- http://api.zeromq.org/master:zmq-msg-init-data
zmq_msg_init_data :: ByteString -> IO (Either Zmq_error Zmq_msg)
zmq_msg_init_data (ByteString.Internal.BS original len) = do
  message <- malloc
  copy <- mallocBytes len
  withForeignPtr original \original1 ->
    copyBytes copy original1 len
  Zmqx.Internal.Bindings.zmq_msg_init_data message copy (fromIntegral @Int @CSize len) free2 nullPtr >>= \case
    0 -> pure (Right (Zmq_msg message))
    _ -> do
      free message
      free copy
      Left <$> zmq_errno

foreign import capi unsafe "static utils.h &free2"
  free2 :: FunPtr (Ptr a -> Ptr b -> IO ())

-- | Initialize an empty ØMQ message of a specified size.
--
-- The message must be freed with 'zmq_msg_free'.
--
-- http://api.zeromq.org/master:zmq-msg-init-size
zmq_msg_init_size :: Int -> IO (Either Zmq_error Zmq_msg)
zmq_msg_init_size size = do
  message <- malloc
  Zmqx.Internal.Bindings.zmq_msg_init_size message (fromIntegral @Int @CSize size) >>= \case
    0 -> pure (Right (Zmq_msg message))
    _ -> do
      free message
      Left <$> zmq_errno

-- | Get whether there are more ØMQ message parts to receive.
--
-- http://api.zeromq.org/master:zmq-msg-more
zmq_msg_more :: Zmq_msg -> IO Bool
zmq_msg_more (Zmq_msg message) =
  (== 1) <$> Zmqx.Internal.Bindings.zmq_msg_more message

-- | Move the content of one ØMQ message to another.
--
-- http://api.zeromq.org/master:zmq-msg-move
zmq_msg_move :: Zmq_msg -> Zmq_msg -> IO (Either Zmq_error ())
zmq_msg_move (Zmq_msg dst) (Zmq_msg src) =
  Zmqx.Internal.Bindings.zmq_msg_copy dst src >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Receive a ØMQ message from a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-msg-recv
zmq_msg_recv :: Zmq_msg -> Zmq_socket -> IO (Either Zmq_error Int)
zmq_msg_recv (Zmq_msg message) (Zmq_socket socket) =
  Zmqx.Internal.Bindings.zmq_msg_recv message socket 0 >>= \case
    -1 -> Left <$> zmq_errno
    n -> pure (Right (fromIntegral @CInt @Int n))

-- | Receive a ØMQ message from a ØMQ socket (non-blocking).
--
-- http://api.zeromq.org/master:zmq-msg-recv
zmq_msg_recv_dontwait :: Zmq_msg -> Zmq_socket -> IO (Either Zmq_error Int)
zmq_msg_recv_dontwait (Zmq_msg message) (Zmq_socket socket) =
  Zmqx.Internal.Bindings.zmq_msg_recv__unsafe message socket Zmqx.Internal.Bindings.ZMQ_DONTWAIT >>= \case
    -1 -> Left <$> zmq_errno
    n -> pure (Right (fromIntegral @CInt @Int n))

-- | Send a ØMQ message on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-msg-send
zmq_msg_send :: Zmq_msg -> Zmq_socket -> Bool -> IO (Either Zmq_error Int)
zmq_msg_send (Zmq_msg message) (Zmq_socket socket) more =
  Zmqx.Internal.Bindings.zmq_msg_send message socket (if more then Zmqx.Internal.Bindings.ZMQ_SNDMORE else 0) >>= \case
    -1 -> Left <$> zmq_errno
    n -> pure (Right (fromIntegral @CInt @Int n))

-- | Send a ØMQ message on a ØMQ socket (non-blocking)
--
-- http://api.zeromq.org/master:zmq-msg-send
zmq_msg_send_dontwait :: Zmq_msg -> Zmq_socket -> Bool -> IO (Either Zmq_error Int)
zmq_msg_send_dontwait (Zmq_msg message) (Zmq_socket socket) more =
  Zmqx.Internal.Bindings.zmq_msg_send__unsafe message socket flags >>= \case
    -1 -> Left <$> zmq_errno
    n -> pure (Right (fromIntegral @CInt @Int n))
  where
    flags =
      if more
        then Zmqx.Internal.Bindings.ZMQ_DONTWAIT .|. Zmqx.Internal.Bindings.ZMQ_SNDMORE
        else Zmqx.Internal.Bindings.ZMQ_DONTWAIT

-- | Set a ØMQ message option.
--
-- http://api.zeromq.org/master:zmq-msg-set
zmq_msg_set :: Zmq_msg -> Zmq_msg_option -> Int -> IO (Either Zmq_error ())
zmq_msg_set (Zmq_msg message) (Zmq_msg_option option) value =
  Zmqx.Internal.Bindings.zmq_msg_set message option (fromIntegral @Int @CInt value) >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Get a ØMQ message's size, in bytes.
--
-- http://api.zeromq.org/master:zmq-msg-size
zmq_msg_size :: Zmq_msg -> IO Int
zmq_msg_size (Zmq_msg message) =
  fromIntegral @CSize @Int <$> Zmqx.Internal.Bindings.zmq_msg_size message

------------------------------------------------------------------------------------------------------------------------
-- Socket

-- | Accept incoming connections on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-bind
zmq_bind :: Zmq_socket -> Text -> IO (Either Zmq_error ())
zmq_bind (Zmq_socket socket) endpoint =
  Text.withCString endpoint (Zmqx.Internal.Bindings.zmq_bind socket) >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Close a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-close
zmq_close :: Zmq_socket -> IO (Either Zmq_error ())
zmq_close (Zmq_socket socket) =
  Zmqx.Internal.Bindings.zmq_close socket >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Create an outgoing connection from a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-connect
zmq_connect :: Zmq_socket -> Text -> IO (Either Zmq_error ())
zmq_connect (Zmq_socket socket) endpoint =
  Text.withCString endpoint (Zmqx.Internal.Bindings.zmq_connect socket) >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Disconnect a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-disconnect
zmq_disconnect :: Zmq_socket -> Text -> IO (Either Zmq_error ())
zmq_disconnect (Zmq_socket socket) endpoint =
  Text.withCString endpoint (Zmqx.Internal.Bindings.zmq_connect socket) >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Get a ØMQ socket option of type 'Int'.
--
-- http://api.zeromq.org/master:zmq-getsockopt
zmq_getsockopt_int :: Zmq_socket -> CInt -> IO (Either Zmq_error Int)
zmq_getsockopt_int (Zmq_socket socket) option =
  alloca \value ->
    alloca \size -> do
      poke size (sizeof @CInt)
      Zmqx.Internal.Bindings.zmq_getsockopt socket option value size >>= \case
        0 -> Right . fromIntegral @CInt @Int <$> peek value
        _ -> Left <$> zmq_errno

-- | Get a ØMQ socket option of type 'Word64'.
--
-- http://api.zeromq.org/master:zmq-getsockopt
zmq_getsockopt_word :: Zmq_socket -> CInt -> IO (Either Zmq_error Word)
zmq_getsockopt_word (Zmq_socket socket) option =
  alloca \value ->
    alloca \size -> do
      poke size (sizeof @CUInt)
      Zmqx.Internal.Bindings.zmq_getsockopt socket option value size >>= \case
        0 -> Right . fromIntegral @CUInt @Word <$> peek value
        _ -> Left <$> zmq_errno

-- | Monitor a ØMQ socket's events.
--
-- http://api.zeromq.org/master:zmq-socket-monitor
zmq_socket_monitor :: Zmq_socket -> Text -> Zmq_socket_events -> IO (Either Zmq_error ())
zmq_socket_monitor (Zmq_socket socket) endpoint (Zmq_socket_events events) =
  Text.withCString endpoint \cendpoint ->
    Zmqx.Internal.Bindings.zmq_socket_monitor socket cendpoint events >>= \case
      -1 -> Left <$> zmq_errno
      _ -> pure (Right ())

-- | Unmonitor a ØMQ socket's events.
--
-- http://api.zeromq.org/master:zmq-socket-monitor
zmq_socket_unmonitor :: Zmq_socket -> IO (Either Zmq_error ())
zmq_socket_unmonitor (Zmq_socket socket) =
  Zmqx.Internal.Bindings.zmq_socket_monitor socket nullPtr 0 >>= \case
    -1 -> Left <$> zmq_errno
    _ -> pure (Right ())

-- | Receive a message from a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-recv
zmq_recv :: Zmq_socket -> Ptr Word8 -> Int -> IO (Either Zmq_error Int)
zmq_recv (Zmq_socket socket) bytes len =
  Zmqx.Internal.Bindings.zmq_recv socket bytes (fromIntegral @Int @CSize len) 0 >>= \case
    -1 -> Left <$> zmq_errno
    n -> pure (Right (fromIntegral @CInt @Int n))

-- | Receive a message from a ØMQ socket (non-blocking)
--
-- http://api.zeromq.org/master:zmq-recv
zmq_recv_dontwait :: Zmq_socket -> Ptr Word8 -> Int -> IO (Either Zmq_error Int)
zmq_recv_dontwait (Zmq_socket socket) bytes len =
  Zmqx.Internal.Bindings.zmq_recv__unsafe socket bytes (fromIntegral @Int @CSize len) Zmqx.Internal.Bindings.ZMQ_DONTWAIT >>= \case
    -1 -> Left <$> zmq_errno
    n -> pure (Right (fromIntegral @CInt @Int n))

-- | Send a message on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-send
zmq_send :: Zmq_socket -> ByteString -> Zmq_send_option -> IO (Either Zmq_error Int)
zmq_send =
  sendwith Zmqx.Internal.Bindings.zmq_send

-- | Send a message on a ØMQ socket (unsafe FFI).
--
-- http://api.zeromq.org/master:zmq-send
zmq_send__unsafe :: Zmq_socket -> ByteString -> Zmq_send_option -> IO (Either Zmq_error Int)
zmq_send__unsafe =
  sendwith Zmqx.Internal.Bindings.zmq_send__unsafe

-- | Send a constant-memory message on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-send-const
zmq_send_const :: Zmq_socket -> ByteString -> Zmq_send_option -> IO (Either Zmq_error Int)
zmq_send_const =
  sendwith Zmqx.Internal.Bindings.zmq_send_const

-- | Send a constant-memory message on a ØMQ socket (unsafe FFI).
--
-- http://api.zeromq.org/master:zmq-send-const
zmq_send_const__unsafe :: Zmq_socket -> ByteString -> Zmq_send_option -> IO (Either Zmq_error Int)
zmq_send_const__unsafe =
  sendwith Zmqx.Internal.Bindings.zmq_send_const__unsafe

sendwith ::
  (forall socket. Ptr socket -> Ptr CChar -> CSize -> CInt -> IO CInt) ->
  Zmq_socket ->
  ByteString ->
  Zmq_send_option ->
  IO (Either Zmq_error Int)
sendwith send0 (Zmq_socket socket) message (Zmq_send_option option) =
  send >>= \case
    -1 -> Left <$> zmq_errno
    n -> pure (Right (fromIntegral @CInt @Int n))
  where
    send =
      ByteString.Unsafe.unsafeUseAsCStringLen message \(cmessage, len) ->
        send0
          socket
          cmessage
          (fromIntegral @Int @CSize len)
          option

-- | Set a ØMQ socket option.
--
-- http://api.zeromq.org/master:zmq-setsockopt
zmq_setsockopt :: Zmq_socket -> Zmq_socket_option a -> a -> IO (Either Zmq_error ())
zmq_setsockopt socket = \case
  ZMQ_LOOPBACK_FASTPATH -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_LOOPBACK_FASTPATH
  ZMQ_ZAP_ENFORCE_DOMAIN -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_ZAP_ENFORCE_DOMAIN
  ZMQ_XPUB_MANUAL_LAST_VALUE -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_XPUB_MANUAL_LAST_VALUE
  ZMQ_WSS_CERT_PEM -> zmq_setsockopt_text socket Zmqx.Internal.Bindings.ZMQ_WSS_CERT_PEM
  ZMQ_WSS_HOSTNAME -> zmq_setsockopt_text socket Zmqx.Internal.Bindings.ZMQ_WSS_HOSTNAME
  ZMQ_WSS_KEY_PEM -> zmq_setsockopt_text socket Zmqx.Internal.Bindings.ZMQ_WSS_KEY_PEM
  ZMQ_WSS_TRUST_PEM -> zmq_setsockopt_text socket Zmqx.Internal.Bindings.ZMQ_WSS_TRUST_PEM
  ZMQ_WSS_TRUST_SYSTEM -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_WSS_TRUST_SYSTEM
  ZMQ_RECONNECT_STOP -> zmq_setsockopt_reconnect_stop_option socket Zmqx.Internal.Bindings.ZMQ_RECONNECT_STOP
  ZMQ_SOCKS_USERNAME -> zmq_setsockopt_text socket Zmqx.Internal.Bindings.ZMQ_SOCKS_USERNAME
  ZMQ_SOCKS_PASSWORD -> zmq_setsockopt_text socket Zmqx.Internal.Bindings.ZMQ_SOCKS_PASSWORD
  ZMQ_PRIORITY -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_PRIORITY
  ZMQ_METADATA -> zmq_setsockopt_text socket Zmqx.Internal.Bindings.ZMQ_METADATA
  ZMQ_MULTICAST_LOOP -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_MULTICAST_LOOP
  ZMQ_ONLY_FIRST_SUBSCRIBE -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_ONLY_FIRST_SUBSCRIBE
  ZMQ_OUT_BATCH_SIZE -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_OUT_BATCH_SIZE
  ZMQ_AFFINITY -> zmq_setsockopt_storable socket Zmqx.Internal.Bindings.ZMQ_AFFINITY
  ZMQ_BACKLOG -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_BACKLOG
  ZMQ_BINDTODEVICE -> zmq_setsockopt_text socket Zmqx.Internal.Bindings.ZMQ_BINDTODEVICE
  ZMQ_CONFLATE -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_CONFLATE
  ZMQ_CONNECT_ROUTING_ID -> zmq_setsockopt_bytestring socket Zmqx.Internal.Bindings.ZMQ_CONNECT_ROUTING_ID
  ZMQ_CONNECT_TIMEOUT -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_CONNECT_TIMEOUT
  ZMQ_CURVE_PUBLICKEY -> zmq_setsockopt_bytestring32 socket Zmqx.Internal.Bindings.ZMQ_CURVE_PUBLICKEY
  ZMQ_CURVE_SECRETKEY -> zmq_setsockopt_bytestring32 socket Zmqx.Internal.Bindings.ZMQ_CURVE_SECRETKEY
  ZMQ_CURVE_SERVER -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_CURVE_SERVER
  ZMQ_CURVE_SERVERKEY -> zmq_setsockopt_bytestring32 socket Zmqx.Internal.Bindings.ZMQ_CURVE_SERVERKEY
  ZMQ_DISCONNECT_MSG -> zmq_setsockopt_bytestring socket Zmqx.Internal.Bindings.ZMQ_DISCONNECT_MSG
  ZMQ_GSSAPI_PLAINTEXT -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_GSSAPI_PLAINTEXT
  ZMQ_GSSAPI_PRINCIPAL -> zmq_setsockopt_text socket Zmqx.Internal.Bindings.ZMQ_GSSAPI_PRINCIPAL
  ZMQ_GSSAPI_PRINCIPAL_NAMETYPE -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_GSSAPI_PRINCIPAL_NAMETYPE
  ZMQ_GSSAPI_SERVER -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_GSSAPI_SERVER
  ZMQ_GSSAPI_SERVICE_PRINCIPAL -> zmq_setsockopt_text socket Zmqx.Internal.Bindings.ZMQ_GSSAPI_SERVICE_PRINCIPAL
  ZMQ_GSSAPI_SERVICE_PRINCIPAL_NAMETYPE -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_GSSAPI_SERVICE_PRINCIPAL_NAMETYPE
  ZMQ_HANDSHAKE_IVL -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_HANDSHAKE_IVL
  ZMQ_HEARTBEAT_IVL -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_HEARTBEAT_IVL
  ZMQ_HEARTBEAT_TIMEOUT -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_HEARTBEAT_TIMEOUT
  ZMQ_HEARTBEAT_TTL -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_HEARTBEAT_TTL
  ZMQ_HELLO_MSG -> zmq_setsockopt_bytestring socket Zmqx.Internal.Bindings.ZMQ_HELLO_MSG
  ZMQ_IMMEDIATE -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_IMMEDIATE
  ZMQ_IN_BATCH_SIZE -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_IN_BATCH_SIZE
  ZMQ_INVERT_MATCHING -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_INVERT_MATCHING
  ZMQ_IPV6' -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_IPV6
  ZMQ_LINGER -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_LINGER
  ZMQ_MAXMSGSIZE -> zmq_setsockopt_storable socket Zmqx.Internal.Bindings.ZMQ_MAXMSGSIZE
  ZMQ_MULTICAST_HOPS -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_MULTICAST_HOPS
  ZMQ_MULTICAST_MAXTPDU -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_MULTICAST_MAXTPDU
  ZMQ_PLAIN_PASSWORD -> zmq_setsockopt_text socket Zmqx.Internal.Bindings.ZMQ_PLAIN_PASSWORD
  ZMQ_PLAIN_SERVER -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_PLAIN_SERVER
  ZMQ_PLAIN_USERNAME -> zmq_setsockopt_text socket Zmqx.Internal.Bindings.ZMQ_PLAIN_USERNAME
  ZMQ_PROBE_ROUTER -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_PROBE_ROUTER
  ZMQ_RATE -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_RATE
  ZMQ_RCVBUF -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_RCVBUF
  ZMQ_RCVHWM -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_RCVHWM
  ZMQ_RCVTIMEO -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_RCVTIMEO
  ZMQ_RECONNECT_IVL -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_RECONNECT_IVL
  ZMQ_RECONNECT_IVL_MAX -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_RECONNECT_IVL_MAX
  ZMQ_RECOVERY_IVL -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_RECOVERY_IVL
  ZMQ_REQ_CORRELATE -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_REQ_CORRELATE
  ZMQ_REQ_RELAXED -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_REQ_RELAXED
  ZMQ_ROUTER_HANDOVER -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_ROUTER_HANDOVER
  ZMQ_ROUTER_MANDATORY -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_ROUTER_MANDATORY
  ZMQ_ROUTING_ID -> zmq_setsockopt_bytestring socket Zmqx.Internal.Bindings.ZMQ_ROUTING_ID
  ZMQ_SNDBUF -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_SNDBUF
  ZMQ_SNDHWM -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_SNDHWM
  ZMQ_SNDTIMEO -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_SNDTIMEO
  ZMQ_SOCKS_PROXY -> zmq_setsockopt_text socket Zmqx.Internal.Bindings.ZMQ_SOCKS_PROXY
  ZMQ_STREAM_NOTIFY -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_STREAM_NOTIFY
  ZMQ_SUBSCRIBE -> zmq_setsockopt_bytestring socket Zmqx.Internal.Bindings.ZMQ_SUBSCRIBE
  ZMQ_TCP_KEEPALIVE -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_TCP_KEEPALIVE
  ZMQ_TCP_KEEPALIVE_CNT -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_TCP_KEEPALIVE_CNT
  ZMQ_TCP_KEEPALIVE_IDLE -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_TCP_KEEPALIVE_IDLE
  ZMQ_TCP_KEEPALIVE_INTVL -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_TCP_KEEPALIVE_INTVL
  ZMQ_TCP_MAXRT -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_TCP_MAXRT
  ZMQ_TOS -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_TOS
  ZMQ_UNSUBSCRIBE -> zmq_setsockopt_bytestring socket Zmqx.Internal.Bindings.ZMQ_UNSUBSCRIBE
  ZMQ_USE_FD -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_USE_FD
  ZMQ_VMCI_BUFFER_MAX_SIZE -> zmq_setsockopt_storable socket Zmqx.Internal.Bindings.ZMQ_VMCI_BUFFER_MAX_SIZE
  ZMQ_VMCI_BUFFER_MIN_SIZE -> zmq_setsockopt_storable socket Zmqx.Internal.Bindings.ZMQ_VMCI_BUFFER_MIN_SIZE
  ZMQ_VMCI_BUFFER_SIZE -> zmq_setsockopt_storable socket Zmqx.Internal.Bindings.ZMQ_VMCI_BUFFER_SIZE
  ZMQ_VMCI_CONNECT_TIMEOUT -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_VMCI_CONNECT_TIMEOUT
  ZMQ_XPUB_MANUAL -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_XPUB_MANUAL
  ZMQ_XPUB_NODROP -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_XPUB_NODROP
  ZMQ_XPUB_VERBOSE -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_XPUB_VERBOSE
  ZMQ_XPUB_VERBOSER -> zmq_setsockopt_int32 socket Zmqx.Internal.Bindings.ZMQ_XPUB_VERBOSER
  ZMQ_XPUB_WELCOME_MSG -> zmq_setsockopt_bytestring socket Zmqx.Internal.Bindings.ZMQ_XPUB_WELCOME_MSG
  ZMQ_ZAP_DOMAIN -> zmq_setsockopt_text socket Zmqx.Internal.Bindings.ZMQ_ZAP_DOMAIN

zmq_setsockopt_bytestring :: Zmq_socket -> CInt -> ByteString -> IO (Either Zmq_error ())
zmq_setsockopt_bytestring (Zmq_socket socket) option bytes =
  ByteString.Unsafe.unsafeUseAsCStringLen bytes \(cstring, len) ->
    Zmqx.Internal.Bindings.zmq_setsockopt socket option cstring (fromIntegral @Int @CSize len) >>= \case
      0 -> pure (Right ())
      _ -> Left <$> zmq_errno

zmq_setsockopt_bytestring32 :: Zmq_socket -> CInt -> ByteString -> IO (Either Zmq_error ())
zmq_setsockopt_bytestring32 (Zmq_socket socket) option bytes =
  if ByteString.length bytes /= 32
    then pure (Left EINVAL)
    else ByteString.Unsafe.unsafeUseAsCStringLen bytes \(cstring, len) ->
      Zmqx.Internal.Bindings.zmq_setsockopt socket option cstring (fromIntegral @Int @CSize len) >>= \case
        0 -> pure (Right ())
        _ -> Left <$> zmq_errno

zmq_setsockopt_cint :: Zmq_socket -> CInt -> CInt -> IO (Either Zmq_error ())
zmq_setsockopt_cint (Zmq_socket socket) option value =
  alloca \ptr -> do
    poke ptr value
    Zmqx.Internal.Bindings.zmq_setsockopt socket option ptr (sizeof @CInt) >>= \case
      0 -> pure (Right ())
      _ -> Left <$> zmq_errno

zmq_setsockopt_int32 :: Zmq_socket -> CInt -> Int32 -> IO (Either Zmq_error ())
zmq_setsockopt_int32 socket option value =
  zmq_setsockopt_cint socket option (fromIntegral @Int32 @CInt value)

zmq_setsockopt_reconnect_stop_option :: Zmq_socket -> CInt -> Zmq_reconnect_stop_option -> IO (Either Zmq_error ())
zmq_setsockopt_reconnect_stop_option =
  coerce zmq_setsockopt_cint

-- TODO monomorphise
zmq_setsockopt_storable :: forall a. (Storable a) => Zmq_socket -> CInt -> a -> IO (Either Zmq_error ())
zmq_setsockopt_storable (Zmq_socket socket) option value =
  alloca \ptr -> do
    poke ptr value
    Zmqx.Internal.Bindings.zmq_setsockopt socket option ptr (sizeof @a) >>= \case
      0 -> pure (Right ())
      _ -> Left <$> zmq_errno

zmq_setsockopt_text :: Zmq_socket -> CInt -> Text -> IO (Either Zmq_error ())
zmq_setsockopt_text (Zmq_socket socket) option text@(Text _ _ len) =
  Text.withCString text \cstring ->
    Zmqx.Internal.Bindings.zmq_setsockopt socket option cstring (fromIntegral @Int @CSize (len + 1)) >>= \case
      0 -> pure (Right ())
      _ -> Left <$> zmq_errno

-- | Create a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-socket
zmq_socket :: Zmq_ctx -> Zmq_socket_type -> IO (Either Zmq_error Zmq_socket)
zmq_socket (Zmq_ctx context) (Zmq_socket_type typ) =
  Zmqx.Internal.Bindings.zmq_socket context typ >>= \socket ->
    if socket == nullPtr then Left <$> zmq_errno else pure (Right (Zmq_socket socket))

-- | Stop accepting connections on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-unbind
zmq_unbind :: Zmq_socket -> Text -> IO (Either Zmq_error ())
zmq_unbind (Zmq_socket socket) endpoint =
  Text.withCString endpoint (Zmqx.Internal.Bindings.zmq_unbind socket) >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

------------------------------------------------------------------------------------------------------------------------
-- Input/output multiplexing

-- TODO replace StorableArray with raw ForeignPtr to drop `array` dependency

-- | Input/output multiplexing.
--
-- http://api.zeromq.org/master:zmq-poll
zmq_poll :: StorableArray Int Zmqx.Internal.Bindings.Zmq_pollitem -> Int64 -> IO (Either Zmq_error Int)
zmq_poll pollitems timeout = do
  (lo, hi) <- MArray.getBounds pollitems
  let numPollitems = fromIntegral @Int @CInt (hi - lo + 1)
  StorableArray.withStorableArray pollitems \cpollitems ->
    poll cpollitems numPollitems (fromIntegral @Int64 @CLong timeout) >>= \case
      -1 -> Left <$> zmq_errno
      n -> pure (Right (fromIntegral @CInt @Int n))
  where
    poll =
      if timeout == 0
        then Zmqx.Internal.Bindings.zmq_poll__unsafe
        else Zmqx.Internal.Bindings.zmq_poll

-- | Start a built-in ØMQ proxy.
--
-- http://api.zeromq.org/master:zmq-proxy
zmq_proxy :: Zmq_socket -> Zmq_socket -> Maybe Zmq_socket -> IO (Either Zmq_error Void)
zmq_proxy (Zmq_socket frontend) (Zmq_socket backend) maybeCapture = do
  _ <- Zmqx.Internal.Bindings.zmq_proxy frontend backend capture
  Left <$> zmq_errno
  where
    capture :: Ptr ()
    capture =
      case maybeCapture of
        Nothing -> nullPtr
        Just (Zmq_socket p) -> p

-- | Start a built-in ØMQ proxy with control flow.
--
-- http://api.zeromq.org/master:zmq-proxy-steerable
zmq_proxy_steerable :: Zmq_socket -> Zmq_socket -> Maybe Zmq_socket -> Zmq_socket -> IO (Either Zmq_error ())
zmq_proxy_steerable (Zmq_socket frontend) (Zmq_socket backend) maybeCapture (Zmq_socket control) =
  Zmqx.Internal.Bindings.zmq_proxy_steerable frontend backend capture control >>= \case
    -1 -> Left <$> zmq_errno
    _ -> pure (Right ())
  where
    capture :: Ptr ()
    capture =
      case maybeCapture of
        Nothing -> nullPtr
        Just (Zmq_socket p) -> p

------------------------------------------------------------------------------------------------------------------------
-- Probe library capabilities

-- | Check whether a ØMQ capability is available.
--
-- http://api.zeromq.org/master:zmq-has
zmq_has :: Text -> IO Bool
zmq_has capability =
  (== 1) <$> Text.withCString capability Zmqx.Internal.Bindings.zmq_has

------------------------------------------------------------------------------------------------------------------------
-- Encryption

-- | Generate a Z85-encoded ØMQ CURVE keypair.
--
-- http://api.zeromq.org/master:zmq-curve-keypair
zmq_curve_keypair :: IO (Either Zmq_error (Text, Text))
zmq_curve_keypair =
  allocaBytes 41 \cpublicKey ->
    allocaBytes 41 \csecretKey -> do
      Zmqx.Internal.Bindings.zmq_curve_keypair cpublicKey csecretKey >>= \case
        -1 -> Left <$> zmq_errno
        _ -> do
          publicKey <- Text.fromPtr0 (castPtr @CChar @Word8 cpublicKey)
          secretKey <- Text.fromPtr0 (castPtr @CChar @Word8 csecretKey)
          pure (Right (publicKey, secretKey))

-- | Derive a Z85-encoded ØMQ CURVE public key from a Z85-encoded ØMQ CURVE private key.
--
-- http://api.zeromq.org/master:zmq-curve-public
zmq_curve_public :: Text -> Either Zmq_error Text
zmq_curve_public secretKey@(Text _ _ secretKeyLen) =
  -- zmq doesn't check this is exactly 40 bytes, so we do
  if secretKeyLen /= 40
    then Left EINVAL
    else unsafeDupablePerformIO do
      Text.withCString secretKey \csecretKey ->
        allocaBytes 41 \cpublicKey ->
          Zmqx.Internal.Bindings.zmq_curve_public cpublicKey csecretKey >>= \case
            -1 -> Left <$> zmq_errno
            _ -> Right <$> Text.fromPtr0 (castPtr @CChar @Word8 cpublicKey)

-- | Decode Z85 as bytes.
--
-- http://api.zeromq.org/master:zmq-z85-decode
zmq_z85_decode :: Text -> Either Zmq_error ByteString
zmq_z85_decode string@(Text _ _ len) =
  unsafeDupablePerformIO do
    Text.withCString string \cstring -> do
      let decodedLen = div (4 * len) 5
      allocaBytes decodedLen \buffer -> do
        result <- Zmqx.Internal.Bindings.zmq_z85_decode buffer cstring
        if result == nullPtr
          then Left <$> zmq_errno
          else Right <$> ByteString.packCStringLen (castPtr @Word8 @CChar buffer, decodedLen)

-- | Encode bytes in Z85.
--
-- http://api.zeromq.org/master:zmq-z85-encode
zmq_z85_encode :: ByteString -> Either Zmq_error Text
zmq_z85_encode bytes =
  unsafeDupablePerformIO do
    ByteString.Unsafe.unsafeUseAsCStringLen bytes \(cbytes, len) ->
      allocaBytes (div (len * 5) 4 + 1) \buffer -> do
        result <- Zmqx.Internal.Bindings.zmq_z85_encode buffer (castPtr @CChar @Word8 cbytes) (fromIntegral @Int @CSize len)
        if result == nullPtr
          then Left <$> zmq_errno
          else Right <$> Text.fromPtr0 (castPtr @CChar @Word8 buffer)

------------------------------------------------------------------------------------------------------------------------
-- Atomic counters

-- | Create a new atomic counter.
--
-- http://api.zeromq.org/master:zmq-atomic-counter-new
zmq_atomic_counter_new :: IO (Maybe Zmq_atomic_counter)
zmq_atomic_counter_new = do
  counter <- Zmqx.Internal.Bindings.zmq_atomic_counter_new
  pure if counter == nullPtr then Nothing else Just (Zmq_atomic_counter counter)

-- | Set the value of an atomic counter.
--
-- http://api.zeromq.org/master:zmq-atomic-counter-set
zmq_atomic_counter_set :: Zmq_atomic_counter -> Int -> IO ()
zmq_atomic_counter_set (Zmq_atomic_counter counter) n =
  Zmqx.Internal.Bindings.zmq_atomic_counter_set counter (fromIntegral @Int @CInt n)

-- | Increment an atomic counter.
--
-- http://api.zeromq.org/master:zmq-atomic-counter-inc
zmq_atomic_counter_inc :: Zmq_atomic_counter -> IO Int
zmq_atomic_counter_inc (Zmq_atomic_counter counter) =
  fromIntegral @CInt @Int <$> Zmqx.Internal.Bindings.zmq_atomic_counter_inc counter

-- | Decrement an atomic counter.
--
-- http://api.zeromq.org/master:zmq-atomic-counter-dec
zmq_atomic_counter_dec :: Zmq_atomic_counter -> IO Int
zmq_atomic_counter_dec (Zmq_atomic_counter counter) =
  fromIntegral @CInt @Int <$> Zmqx.Internal.Bindings.zmq_atomic_counter_dec counter

-- | Get the value of an atomic counter.
--
-- http://api.zeromq.org/master:zmq-atomic-counter-value
zmq_atomic_counter_value :: Zmq_atomic_counter -> IO Int
zmq_atomic_counter_value (Zmq_atomic_counter counter) =
  fromIntegral @CInt @Int <$> Zmqx.Internal.Bindings.zmq_atomic_counter_value counter

-- | Destroy an atomic counter.
--
-- http://api.zeromq.org/master:zmq-atomic-counter-destroy
zmq_atomic_counter_destroy :: Zmq_atomic_counter -> IO ()
zmq_atomic_counter_destroy =
  coerce Zmqx.Internal.Bindings.zmq_atomic_counter_destroy

------------------------------------------------------------------------------------------------------------------------
-- Misc. utils

sizeof :: forall a. (Storable a) => CSize
sizeof =
  fromIntegral @Int @CSize (sizeOf (undefined :: a))
