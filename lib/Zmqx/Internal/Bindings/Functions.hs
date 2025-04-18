-- file: Functions.hs
-- author: Jacob Xie
-- date: 2025/02/26 15:11:09 Wednesday
-- brief:

module Zmqx.Internal.Bindings.Functions (module Zmqx.Internal.Bindings.Functions) where

import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CChar (..), CInt (..), CLong (..), CSize (..))
import Foreign.Ptr (FunPtr, Ptr)
import Zmqx.Internal.Bindings.Types (Zmq_msg, Zmq_pollitem)

------------------------------------------------------------------------------------------------------------------------
-- Error

-- | Get the ØMQ error number for the calling thread.
--
-- http://api.zeromq.org/master:zmq-errno
foreign import capi unsafe "zmq.h zmq_errno"
  zmq_errno :: IO CInt

-- | Get the string of a ØMQ error number.
--
-- http://api.zeromq.org/master:zmq-strerror
foreign import capi unsafe "zmq.h zmq_strerror"
  zmq_strerror :: CInt -> Ptr CChar

------------------------------------------------------------------------------------------------------------------------
-- Version

-- | Get the ØMQ library version.
--
-- http://api.zeromq.org/master:zmq-version
foreign import capi unsafe "zmq.h zmq_version"
  zmq_version :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

------------------------------------------------------------------------------------------------------------------------
-- Context

-- | Get a ØMQ context option.
--
-- http://api.zeromq.org/master:zmq-ctx-get
foreign import capi unsafe "zmq.h zmq_ctx_get"
  zmq_ctx_get :: Ptr context -> CInt -> IO CInt

-- | Create a new ØMQ context.
--
-- http://api.zeromq.org/master:zmq-ctx-new
foreign import capi unsafe "zmq.h zmq_ctx_new"
  zmq_ctx_new :: IO (Ptr context)

-- | Set a ØMQ context option.
--
-- http://api.zeromq.org/master:zmq-ctx-set
foreign import capi unsafe "zmq.h zmq_ctx_set"
  zmq_ctx_set :: Ptr context -> CInt -> CInt -> IO CInt

-- | Shutdown a ØMQ context.
--
-- http://api.zeromq.org/master:zmq-ctx-shutdown
foreign import capi unsafe "zmq.h zmq_ctx_shutdown"
  zmq_ctx_shutdown :: Ptr context -> IO CInt

-- | Terminate a ØMQ context.
--
-- http://api.zeromq.org/master:zmq-ctx-term
foreign import capi interruptible "zmq.h zmq_ctx_term"
  zmq_ctx_term :: Ptr context -> IO CInt

------------------------------------------------------------------------------------------------------------------------
-- Message

-- | Release a ØMQ message.
--
-- http://api.zeromq.org/master:zmq-msg-close
foreign import capi unsafe "zmq.h zmq_msg_close"
  zmq_msg_close :: Ptr Zmq_msg -> IO CInt

-- | Copy the content of one ØMQ message to another.
--
-- http://api.zeromq.org/master:zmq-msg-copy
foreign import capi unsafe "zmq.h zmq_msg_copy"
  zmq_msg_copy :: Ptr Zmq_msg -> Ptr Zmq_msg -> IO CInt

-- | Get a ØMQ message's content.
--
-- http://api.zeromq.org/master:zmq-msg-data
foreign import capi unsafe "zmq.h zmq_msg_data"
  zmq_msg_data :: Ptr Zmq_msg -> IO (Ptr a)

-- | Get a ØMQ message option.
--
-- http://api.zeromq.org/master:zmq-msg-get
foreign import capi unsafe "zmq.h zmq_msg_get"
  zmq_msg_get :: Ptr Zmq_msg -> CInt -> IO CInt

-- | Get a ØMQ message metadata property.
--
-- http://api.zeromq.org/master:zmq-msg-gets
foreign import capi unsafe "zmq.h zmq_msg_gets"
  zmq_msg_gets :: Ptr Zmq_msg -> CString -> IO CString

-- | Initialise an empty ØMQ message.
--
-- http://api.zeromq.org/master:zmq-msg-init
foreign import capi unsafe "zmq.h zmq_msg_init"
  zmq_msg_init :: Ptr Zmq_msg -> IO CInt

-- | Initialise a ØMQ message from a buffer.
--
-- http://api.zeromq.org/master:zmq-msg-init-data
foreign import capi unsafe "zmq.h zmq_msg_init_data"
  zmq_msg_init_data :: Ptr Zmq_msg -> Ptr a -> CSize -> FunPtr (Ptr a -> Ptr b -> IO ()) -> Ptr b -> IO CInt

-- | Initialize an empty ØMQ message of a specified size.
--
-- http://api.zeromq.org/master:zmq-msg-init-size
foreign import capi unsafe "zmq.h zmq_msg_init_size"
  zmq_msg_init_size :: Ptr Zmq_msg -> CSize -> IO CInt

-- | Get whether there are more ØMQ message parts to receive.
--
-- http://api.zeromq.org/master:zmq-msg-more
foreign import capi unsafe "zmq.h zmq_msg_more"
  zmq_msg_more :: Ptr Zmq_msg -> IO CInt

-- | Move the content of one ØMQ message to another.
--
-- http://api.zeromq.org/master:zmq-msg-move
foreign import capi unsafe "zmq.h zmq_msg_move"
  zmq_msg_move :: Ptr Zmq_msg -> Ptr Zmq_msg -> IO CInt

-- | Receive a ØMQ message from a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-msg-recv
foreign import capi interruptible "zmq.h zmq_msg_recv"
  zmq_msg_recv :: Ptr Zmq_msg -> Ptr socket -> CInt -> IO CInt

-- | Receive a ØMQ message from a ØMQ socket (unsafe FFI).
--
-- http://api.zeromq.org/master:zmq-msg-recv
foreign import capi unsafe "zmq.h zmq_msg_recv"
  zmq_msg_recv__unsafe :: Ptr Zmq_msg -> Ptr socket -> CInt -> IO CInt

-- | Send a ØMQ message on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-msg-send
foreign import capi interruptible "zmq.h zmq_msg_send"
  zmq_msg_send :: Ptr Zmq_msg -> Ptr socket -> CInt -> IO CInt

-- | Send a ØMQ message on a ØMQ socket (unsafe FFI).
--
-- http://api.zeromq.org/master:zmq-msg-send
foreign import capi unsafe "zmq.h zmq_msg_send"
  zmq_msg_send__unsafe :: Ptr Zmq_msg -> Ptr socket -> CInt -> IO CInt

-- | Set a ØMQ message option.
--
-- http://api.zeromq.org/master:zmq-msg-set
foreign import capi unsafe "zmq.h zmq_msg_set"
  zmq_msg_set :: Ptr Zmq_msg -> CInt -> CInt -> IO CInt

-- | Get a ØMQ message's size, in bytes.
--
-- http://api.zeromq.org/master:zmq-msg-size
foreign import capi unsafe "zmq.h zmq_msg_size"
  zmq_msg_size :: Ptr Zmq_msg -> IO CSize

------------------------------------------------------------------------------------------------------------------------
-- Socket

-- | Accept incoming connections on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-bind
foreign import capi interruptible "zmq.h zmq_bind"
  -- N.B. use safe FFI because I've seen zmq_bind block, though I haven't found docs that say when/why
  zmq_bind :: Ptr socket -> Ptr CChar -> IO CInt

-- | Close a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-close
foreign import capi unsafe "zmq.h zmq_close"
  zmq_close :: Ptr socket -> IO CInt

-- | Create an outgoing connection from a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-connect
foreign import capi interruptible "zmq.h zmq_connect"
  -- N.B. use safe FFI because it's not totally clear if some transports or socket types block
  zmq_connect :: Ptr socket -> Ptr CChar -> IO CInt

-- | Disconnect a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-disconnect
foreign import capi unsafe "zmq.h zmq_disconnect"
  zmq_disconnect :: Ptr socket -> Ptr CChar -> IO CInt

-- | Get a ØMQ socket option.
--
-- http://api.zeromq.org/master:zmq-getsockopt
foreign import capi unsafe "zmq.h zmq_getsockopt"
  zmq_getsockopt :: Ptr socket -> CInt -> Ptr value -> Ptr CSize -> IO CInt

-- | Monitor a ØMQ socket's events.
--
-- http://api.zeromq.org/master:zmq-socket-monitor
foreign import capi interruptible "zmq.h zmq_socket_monitor"
  -- N.B. use safe FFI because it's unclear how much work this function does
  zmq_socket_monitor :: Ptr socket -> Ptr CChar -> CInt -> IO CInt

-- | Receive a message from a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-recv
foreign import capi interruptible "zmq.h zmq_recv"
  zmq_recv :: Ptr socket -> Ptr buffer -> CSize -> CInt -> IO CInt

-- | Receive a message from a ØMQ socket (unsafe FFI).
--
-- http://api.zeromq.org/master:zmq-recv
foreign import capi unsafe "zmq.h zmq_recv"
  zmq_recv__unsafe :: Ptr socket -> Ptr buffer -> CSize -> CInt -> IO CInt

-- | Send a message on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-send
foreign import capi interruptible "zmq.h zmq_send"
  zmq_send :: Ptr socket -> Ptr buffer -> CSize -> CInt -> IO CInt

-- | Send a message on a ØMQ socket (unsafe FFI).
--
-- http://api.zeromq.org/master:zmq-send
foreign import capi unsafe "zmq.h zmq_send"
  zmq_send__unsafe :: Ptr socket -> Ptr buffer -> CSize -> CInt -> IO CInt

-- | Send a constant-memory message on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-send-const
foreign import capi interruptible "zmq.h zmq_send_const"
  zmq_send_const :: Ptr socket -> Ptr buffer -> CSize -> CInt -> IO CInt

-- | Send a constant-memory message on a ØMQ socket (unsafe FFI).
--
-- http://api.zeromq.org/master:zmq-send-const
foreign import capi unsafe "zmq.h zmq_send_const"
  zmq_send_const__unsafe :: Ptr socket -> Ptr buffer -> CSize -> CInt -> IO CInt

-- | Set a ØMQ socket option.
--
-- http://api.zeromq.org/master:zmq-setsockopt
foreign import capi unsafe "zmq.h zmq_setsockopt"
  zmq_setsockopt :: Ptr socket -> CInt -> Ptr value -> CSize -> IO CInt

-- | Create a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-socket
foreign import capi unsafe "zmq.h zmq_socket"
  zmq_socket :: Ptr context -> CInt -> IO (Ptr socket)

-- | Stop accepting connections on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-unbind
foreign import capi unsafe "zmq.h zmq_unbind"
  zmq_unbind :: Ptr socket -> Ptr CChar -> IO CInt

------------------------------------------------------------------------------------------------------------------------
-- Input/output multiplexing

-- | Input/output multiplexing.
--
-- http://api.zeromq.org/master:zmq-poll
foreign import capi interruptible "zmq.h zmq_poll"
  zmq_poll :: Ptr Zmq_pollitem -> CInt -> CLong -> IO CInt

-- | Input/output multiplexing (unsafe FFI).
--
-- http://api.zeromq.org/master:zmq-poll
foreign import capi unsafe "zmq.h zmq_poll"
  zmq_poll__unsafe :: Ptr Zmq_pollitem -> CInt -> CLong -> IO CInt

------------------------------------------------------------------------------------------------------------------------
-- Message proxying

-- | Start a built-in ØMQ proxy.
--
-- http://api.zeromq.org/master:zmq-proxy
foreign import capi interruptible "zmq.h zmq_proxy"
  zmq_proxy :: Ptr frontend -> Ptr backend -> Ptr capture -> IO CInt

-- | Start a built-in ØMQ proxy with control flow.
--
-- http://api.zeromq.org/master:zmq-proxy-steerable
foreign import capi interruptible "zmq.h zmq_proxy_steerable"
  zmq_proxy_steerable :: Ptr frontend -> Ptr backend -> Ptr capture -> Ptr control -> IO CInt

------------------------------------------------------------------------------------------------------------------------
-- Probe library capabilities

-- | Check whether a ØMQ capability is available.
--
-- http://api.zeromq.org/master:zmq-has
foreign import capi unsafe "zmq.h zmq_has"
  zmq_has :: CString -> IO CInt

------------------------------------------------------------------------------------------------------------------------
-- Encryption

-- | Generate a Z85-encoded ØMQ CURVE keypair.
--
-- http://api.zeromq.org/master:zmq-curve-keypair
foreign import capi unsafe "zmq.h zmq_curve_keypair"
  zmq_curve_keypair :: Ptr CChar -> Ptr CChar -> IO CInt

-- | Derive a Z85-encoded ØMQ CURVE public key from a Z85-encoded ØMQ CURVE private key.
--
-- http://api.zeromq.org/master:zmq-curve-public
foreign import capi unsafe "zmq.h zmq_curve_public"
  zmq_curve_public :: Ptr CChar -> CString -> IO CInt

-- | Decode Z85 as bytes.
--
-- http://api.zeromq.org/master:zmq-z85-decode
foreign import capi unsafe "zmq.h zmq_z85_decode"
  zmq_z85_decode :: Ptr Word8 -> CString -> IO (Ptr Word8)

-- | Encode bytes in Z85.
--
-- http://api.zeromq.org/master:zmq-z85-encode
foreign import capi unsafe "zmq.h zmq_z85_encode"
  zmq_z85_encode :: Ptr CChar -> Ptr Word8 -> CSize -> IO CString

------------------------------------------------------------------------------------------------------------------------
-- Atomic counters

-- | Create a new atomic counter.
--
-- http://api.zeromq.org/master:zmq-atomic-counter-new
foreign import capi unsafe "zmq.h zmq_atomic_counter_new"
  zmq_atomic_counter_new :: IO (Ptr counter)

-- | Set the value of an atomic counter.
--
-- http://api.zeromq.org/master:zmq-atomic-counter-set
foreign import capi unsafe "zmq.h zmq_atomic_counter_set"
  zmq_atomic_counter_set :: Ptr counter -> CInt -> IO ()

-- | Increment an atomic counter.
--
-- http://api.zeromq.org/master:zmq-atomic-counter-inc
foreign import capi unsafe "zmq.h zmq_atomic_counter_inc"
  zmq_atomic_counter_inc :: Ptr counter -> IO CInt

-- | Decrement an atomic counter.
--
-- http://api.zeromq.org/master:zmq-atomic-counter-dec
foreign import capi unsafe "zmq.h zmq_atomic_counter_dec"
  zmq_atomic_counter_dec :: Ptr counter -> IO CInt

-- | Get the value of an atomic counter.
--
-- http://api.zeromq.org/master:zmq-atomic-counter-value
foreign import capi unsafe "zmq.h zmq_atomic_counter_value"
  zmq_atomic_counter_value :: Ptr counter -> IO CInt

-- | Destroy an atomic counter.
--
-- http://api.zeromq.org/master:zmq-atomic-counter-destroy
foreign import capi unsafe "zmq-wrapper.h zmq_atomic_counter_destroy_wrapper"
  zmq_atomic_counter_destroy :: Ptr counter -> IO ()
