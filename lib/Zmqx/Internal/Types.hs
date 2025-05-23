{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-role-annotations #-}

-- file: Types.hs
-- author: Jacob Xie
-- date: 2025/02/27 10:18:17 Thursday
-- brief:

module Zmqx.Internal.Types (module Zmqx.Internal.Types) where

import Control.Monad (guard)
import Data.Bits (Bits, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Int (Int32, Int64)
import Data.List qualified as List
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Word (Word64)
import Foreign.C.Types (CInt, CShort)
import Foreign.Ptr (Ptr, nullPtr)
import Zmqx.Internal.Bindings qualified

------------------------------------------------------------------------------------------------------------------------
-- Zmq_atomic_counter

-- | An atomic counter.
newtype Zmq_atomic_counter
  = Zmq_atomic_counter (Ptr ())
  deriving stock (Eq, Ord)

------------------------------------------------------------------------------------------------------------------------
-- Zmq_ctx

-- | A ØMQ context.
newtype Zmq_ctx
  = Zmq_ctx (Ptr ())
  deriving stock (Eq, Ord)

------------------------------------------------------------------------------------------------------------------------
-- Zmq_ctx_option

-- | A ØMQ context option.
newtype Zmq_ctx_option
  = Zmq_ctx_option CInt
  deriving stock (Eq, Ord)

instance Show Zmq_ctx_option where
  show = \case
    ZMQ_BLOCKY -> "ZMQ_BLOCKY"
    ZMQ_IO_THREADS -> "ZMQ_IO_THREADS"
    ZMQ_IPV6 -> "ZMQ_IPV6"
    ZMQ_MAX_MSGSZ -> "ZMQ_MAX_MSGSZ"
    ZMQ_MAX_SOCKETS -> "ZMQ_MAX_SOCKETS"
    ZMQ_MSG_T_SIZE -> "ZMQ_MSG_T_SIZE"
    ZMQ_SOCKET_LIMIT -> "ZMQ_SOCKET_LIMIT"
    ZMQ_THREAD_NAME_PREFIX -> "ZMQ_THREAD_NAME_PREFIX"
    ZMQ_THREAD_SCHED_POLICY -> "ZMQ_THREAD_SCHED_POLICY"

pattern ZMQ_BLOCKY :: Zmq_ctx_option
pattern ZMQ_BLOCKY = Zmq_ctx_option Zmqx.Internal.Bindings.ZMQ_BLOCKY

pattern ZMQ_IO_THREADS :: Zmq_ctx_option
pattern ZMQ_IO_THREADS = Zmq_ctx_option Zmqx.Internal.Bindings.ZMQ_IO_THREADS

pattern ZMQ_IPV6 :: Zmq_ctx_option
pattern ZMQ_IPV6 = Zmq_ctx_option Zmqx.Internal.Bindings.ZMQ_IPV6

pattern ZMQ_MAX_MSGSZ :: Zmq_ctx_option
pattern ZMQ_MAX_MSGSZ = Zmq_ctx_option Zmqx.Internal.Bindings.ZMQ_MAX_MSGSZ

pattern ZMQ_MAX_SOCKETS :: Zmq_ctx_option
pattern ZMQ_MAX_SOCKETS = Zmq_ctx_option Zmqx.Internal.Bindings.ZMQ_MAX_SOCKETS

pattern ZMQ_MSG_T_SIZE :: Zmq_ctx_option
pattern ZMQ_MSG_T_SIZE = Zmq_ctx_option Zmqx.Internal.Bindings.ZMQ_MSG_T_SIZE

pattern ZMQ_SOCKET_LIMIT :: Zmq_ctx_option
pattern ZMQ_SOCKET_LIMIT = Zmq_ctx_option Zmqx.Internal.Bindings.ZMQ_SOCKET_LIMIT

pattern ZMQ_THREAD_NAME_PREFIX :: Zmq_ctx_option
pattern ZMQ_THREAD_NAME_PREFIX = Zmq_ctx_option Zmqx.Internal.Bindings.ZMQ_THREAD_NAME_PREFIX

pattern ZMQ_THREAD_SCHED_POLICY :: Zmq_ctx_option
pattern ZMQ_THREAD_SCHED_POLICY = Zmq_ctx_option Zmqx.Internal.Bindings.ZMQ_THREAD_SCHED_POLICY

{-# COMPLETE
  ZMQ_BLOCKY,
  ZMQ_IO_THREADS,
  ZMQ_IPV6,
  ZMQ_MAX_MSGSZ,
  ZMQ_MAX_SOCKETS,
  ZMQ_MSG_T_SIZE,
  ZMQ_SOCKET_LIMIT,
  ZMQ_THREAD_NAME_PREFIX,
  ZMQ_THREAD_SCHED_POLICY
  #-}

------------------------------------------------------------------------------------------------------------------------
-- Zmq_error

-- | A ØMQ error.
newtype Zmq_error
  = Zmq_error CInt
  deriving stock (Eq, Ord)

instance Show Zmq_error where
  show = \case
    EADDRINUSE -> "EADDRINUSE"
    EADDRNOTAVAIL -> "EADDRNOTAVAIL"
    EAFNOSUPPORT -> "EAFNOSUPPORT"
    EAGAIN -> "EAGAIN"
    EBADF -> "EBADF"
    ECONNABORTED -> "ECONNABORTED"
    ECONNREFUSED -> "ECONNREFUSED"
    ECONNRESET -> "ECONNRESET"
    EFAULT -> "EFAULT"
    EFSM -> "EFSM"
    EHOSTUNREACH -> "EHOSTUNREACH"
    EINPROGRESS -> "EINPROGRESS"
    EINTR -> "EINTR"
    EINVAL -> "EINVAL"
    EMFILE -> "EMFILE"
    EMSGSIZE -> "EMSGSIZE"
    EMTHREAD -> "EMTHREAD"
    ENETDOWN -> "ENETDOWN"
    ENETRESET -> "ENETRESET"
    ENETUNREACH -> "ENETUNREACH"
    ENOBUFS -> "ENOBUFS"
    ENOCOMPATPROTO -> "ENOCOMPATPROTO"
    ENODEV -> "ENODEV"
    ENOENT -> "ENOENT"
    ENOMEM -> "ENOMEM"
    ENOTCONN -> "ENOTCONN"
    ENOTSOCK -> "ENOTSOCK"
    ENOTSUP -> "ENOTSUP"
    EPROTONOSUPPORT -> "EPROTONOSUPPORT"
    ETERM -> "ETERM"
    ETIMEDOUT -> "ETIMEDOUT"

pattern EADDRINUSE :: Zmq_error
pattern EADDRINUSE = Zmq_error Zmqx.Internal.Bindings.EADDRINUSE

pattern EADDRNOTAVAIL :: Zmq_error
pattern EADDRNOTAVAIL = Zmq_error Zmqx.Internal.Bindings.EADDRNOTAVAIL

pattern EAFNOSUPPORT :: Zmq_error
pattern EAFNOSUPPORT = Zmq_error Zmqx.Internal.Bindings.EAFNOSUPPORT

pattern EAGAIN :: Zmq_error
pattern EAGAIN = Zmq_error Zmqx.Internal.Bindings.EAGAIN

pattern EBADF :: Zmq_error
pattern EBADF = Zmq_error Zmqx.Internal.Bindings.EBADF

pattern ECONNABORTED :: Zmq_error
pattern ECONNABORTED = Zmq_error Zmqx.Internal.Bindings.ECONNABORTED

pattern ECONNREFUSED :: Zmq_error
pattern ECONNREFUSED = Zmq_error Zmqx.Internal.Bindings.ECONNREFUSED

pattern ECONNRESET :: Zmq_error
pattern ECONNRESET = Zmq_error Zmqx.Internal.Bindings.ECONNRESET

pattern EFAULT :: Zmq_error
pattern EFAULT = Zmq_error Zmqx.Internal.Bindings.EFAULT

pattern EFSM :: Zmq_error
pattern EFSM = Zmq_error Zmqx.Internal.Bindings.EFSM

pattern EHOSTUNREACH :: Zmq_error
pattern EHOSTUNREACH = Zmq_error Zmqx.Internal.Bindings.EHOSTUNREACH

pattern EINPROGRESS :: Zmq_error
pattern EINPROGRESS = Zmq_error Zmqx.Internal.Bindings.EINPROGRESS

pattern EINTR :: Zmq_error
pattern EINTR = Zmq_error Zmqx.Internal.Bindings.EINTR

pattern EINVAL :: Zmq_error
pattern EINVAL = Zmq_error Zmqx.Internal.Bindings.EINVAL

pattern EMFILE :: Zmq_error
pattern EMFILE = Zmq_error Zmqx.Internal.Bindings.EMFILE

pattern EMSGSIZE :: Zmq_error
pattern EMSGSIZE = Zmq_error Zmqx.Internal.Bindings.EMSGSIZE

pattern EMTHREAD :: Zmq_error
pattern EMTHREAD = Zmq_error Zmqx.Internal.Bindings.EMTHREAD

pattern ENETDOWN :: Zmq_error
pattern ENETDOWN = Zmq_error Zmqx.Internal.Bindings.ENETDOWN

pattern ENETRESET :: Zmq_error
pattern ENETRESET = Zmq_error Zmqx.Internal.Bindings.ENETRESET

pattern ENETUNREACH :: Zmq_error
pattern ENETUNREACH = Zmq_error Zmqx.Internal.Bindings.ENETUNREACH

pattern ENOBUFS :: Zmq_error
pattern ENOBUFS = Zmq_error Zmqx.Internal.Bindings.ENOBUFS

pattern ENOCOMPATPROTO :: Zmq_error
pattern ENOCOMPATPROTO = Zmq_error Zmqx.Internal.Bindings.ENOCOMPATPROTO

pattern ENODEV :: Zmq_error
pattern ENODEV = Zmq_error Zmqx.Internal.Bindings.ENODEV

pattern ENOENT :: Zmq_error
pattern ENOENT = Zmq_error Zmqx.Internal.Bindings.ENOENT

pattern ENOMEM :: Zmq_error
pattern ENOMEM = Zmq_error Zmqx.Internal.Bindings.ENOMEM

pattern ENOTCONN :: Zmq_error
pattern ENOTCONN = Zmq_error Zmqx.Internal.Bindings.ENOTCONN

pattern ENOTSOCK :: Zmq_error
pattern ENOTSOCK = Zmq_error Zmqx.Internal.Bindings.ENOTSOCK

pattern ENOTSUP :: Zmq_error
pattern ENOTSUP = Zmq_error Zmqx.Internal.Bindings.ENOTSUP

pattern EPROTONOSUPPORT :: Zmq_error
pattern EPROTONOSUPPORT = Zmq_error Zmqx.Internal.Bindings.EPROTONOSUPPORT

pattern ETERM :: Zmq_error
pattern ETERM = Zmq_error Zmqx.Internal.Bindings.ETERM

pattern ETIMEDOUT :: Zmq_error
pattern ETIMEDOUT = Zmq_error Zmqx.Internal.Bindings.ETIMEDOUT

{-# COMPLETE
  EADDRINUSE,
  EADDRNOTAVAIL,
  EAFNOSUPPORT,
  EAGAIN,
  EBADF,
  ECONNABORTED,
  ECONNREFUSED,
  ECONNRESET,
  EFAULT,
  EFSM,
  EHOSTUNREACH,
  EINPROGRESS,
  EINTR,
  EINVAL,
  EMFILE,
  EMSGSIZE,
  EMTHREAD,
  ENETDOWN,
  ENETRESET,
  ENETUNREACH,
  ENOBUFS,
  ENOCOMPATPROTO,
  ENODEV,
  ENOENT,
  ENOMEM,
  ENOTCONN,
  ENOTSOCK,
  ENOTSUP,
  EPROTONOSUPPORT,
  ETERM,
  ETIMEDOUT
  #-}

------------------------------------------------------------------------------------------------------------------------
-- Zmq_events

-- | A set of ØMQ events.
newtype Zmq_events
  = Zmq_events CShort
  deriving stock (Eq, Ord)
  deriving (Monoid, Semigroup) via (Bitfield CShort)

instance Show Zmq_events where
  show =
    showMonoid
      [ ("ZMQ_POLLIN", hasPollin),
        ("ZMQ_POLLOUT", hasPollout),
        ("ZMQ_POLLERR", hasPollerr),
        ("ZMQ_POLLPRI", hasPollpri)
      ]

pattern ZMQ_POLLIN :: Zmq_events
pattern ZMQ_POLLIN <-
  (hasPollin -> True)
  where
    ZMQ_POLLIN = Zmq_events Zmqx.Internal.Bindings.ZMQ_POLLIN

pattern ZMQ_POLLOUT :: Zmq_events
pattern ZMQ_POLLOUT <-
  (hasPollout -> True)
  where
    ZMQ_POLLOUT = Zmq_events Zmqx.Internal.Bindings.ZMQ_POLLOUT

pattern ZMQ_POLLERR :: Zmq_events
pattern ZMQ_POLLERR <-
  (hasPollerr -> True)
  where
    ZMQ_POLLERR = Zmq_events Zmqx.Internal.Bindings.ZMQ_POLLERR

pattern ZMQ_POLLPRI :: Zmq_events
pattern ZMQ_POLLPRI <-
  (hasPollpri -> True)
  where
    ZMQ_POLLPRI = Zmq_events Zmqx.Internal.Bindings.ZMQ_POLLPRI

hasPollin :: Zmq_events -> Bool
hasPollin (Zmq_events n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_POLLIN /= 0

hasPollout :: Zmq_events -> Bool
hasPollout (Zmq_events n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_POLLOUT /= 0

hasPollerr :: Zmq_events -> Bool
hasPollerr (Zmq_events n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_POLLERR /= 0

hasPollpri :: Zmq_events -> Bool
hasPollpri (Zmq_events n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_POLLPRI /= 0

------------------------------------------------------------------------------------------------------------------------
-- Zmq_msg

-- | A ØMQ message.
newtype Zmq_msg
  = Zmq_msg (Ptr Zmqx.Internal.Bindings.Zmq_msg)
  deriving stock (Eq, Ord)

------------------------------------------------------------------------------------------------------------------------
-- Zmq_msg_option

-- | A ØMQ message option.
newtype Zmq_msg_option
  = Zmq_msg_option CInt
  deriving stock (Eq, Ord)

instance Show Zmq_msg_option where
  show = \case
    ZMQ_MORE -> "ZMQ_MORE"
    ZMQ_SHARED -> "ZMQ_SHARED"

pattern ZMQ_MORE :: Zmq_msg_option
pattern ZMQ_MORE = Zmq_msg_option Zmqx.Internal.Bindings.ZMQ_MORE

pattern ZMQ_SHARED :: Zmq_msg_option
pattern ZMQ_SHARED = Zmq_msg_option Zmqx.Internal.Bindings.ZMQ_SHARED

{-# COMPLETE
  ZMQ_MORE,
  ZMQ_SHARED
  #-}

------------------------------------------------------------------------------------------------------------------------
-- Zmq_pollitem_fd

-- | A file descriptor ØMQ pollitem.
pattern Zmq_pollitem_fd :: Zmqx.Internal.Bindings.Zmq_fd -> Zmq_events -> Zmqx.Internal.Bindings.Zmq_pollitem
pattern Zmq_pollitem_fd fd events <-
  (asPollitemFd -> Just (fd, events))
  where
    Zmq_pollitem_fd fd (Zmq_events events) =
      Zmqx.Internal.Bindings.Zmq_pollitem
        { Zmqx.Internal.Bindings.socket = nullPtr,
          Zmqx.Internal.Bindings.fd = fd,
          Zmqx.Internal.Bindings.events = events,
          Zmqx.Internal.Bindings.revents = 0
        }

asPollitemFd :: Zmqx.Internal.Bindings.Zmq_pollitem -> Maybe (Zmqx.Internal.Bindings.Zmq_fd, Zmq_events)
asPollitemFd Zmqx.Internal.Bindings.Zmq_pollitem {Zmqx.Internal.Bindings.socket, Zmqx.Internal.Bindings.fd, Zmqx.Internal.Bindings.revents} =
  if socket == nullPtr
    then Just (fd, Zmq_events revents)
    else Nothing

------------------------------------------------------------------------------------------------------------------------
-- Zmq_pollitem_socket

-- | A socket ØMQ pollitem.
pattern Zmq_pollitem_socket :: Zmq_socket -> Zmq_events -> Zmqx.Internal.Bindings.Zmq_pollitem
pattern Zmq_pollitem_socket socket events <-
  (asPollitemSocket -> Just (socket, events))
  where
    Zmq_pollitem_socket (Zmq_socket socket) (Zmq_events events) =
      Zmqx.Internal.Bindings.Zmq_pollitem
        { Zmqx.Internal.Bindings.socket = socket,
          Zmqx.Internal.Bindings.fd = 0,
          Zmqx.Internal.Bindings.events = events,
          Zmqx.Internal.Bindings.revents = 0
        }

asPollitemSocket :: Zmqx.Internal.Bindings.Zmq_pollitem -> Maybe (Zmq_socket, Zmq_events)
asPollitemSocket Zmqx.Internal.Bindings.Zmq_pollitem {Zmqx.Internal.Bindings.socket, Zmqx.Internal.Bindings.revents} =
  if socket == nullPtr
    then Nothing
    else Just (Zmq_socket socket, Zmq_events revents)

------------------------------------------------------------------------------------------------------------------------
-- Zmq_protocol_error

-- | A ØMQ protocol error.
newtype Zmq_protocol_error
  = Zmq_protocol_error CInt
  deriving stock (Eq, Ord)

instance Show Zmq_protocol_error where
  show = \case
    ZMQ_PROTOCOL_ERROR_WS_UNSPECIFIED -> "ZMQ_PROTOCOL_ERROR_WS_UNSPECIFIED"
    ZMQ_PROTOCOL_ERROR_ZAP_BAD_REQUEST_ID -> "ZMQ_PROTOCOL_ERROR_ZAP_BAD_REQUEST_ID"
    ZMQ_PROTOCOL_ERROR_ZAP_BAD_VERSION -> "ZMQ_PROTOCOL_ERROR_ZAP_BAD_VERSION "
    ZMQ_PROTOCOL_ERROR_ZAP_INVALID_METADATA -> "ZMQ_PROTOCOL_ERROR_ZAP_INVALID_METADATA"
    ZMQ_PROTOCOL_ERROR_ZAP_INVALID_STATUS_CODE -> "ZMQ_PROTOCOL_ERROR_ZAP_INVALID_STATUS_CODE"
    ZMQ_PROTOCOL_ERROR_ZAP_MALFORMED_REPLY -> "ZMQ_PROTOCOL_ERROR_ZAP_MALFORMED_REPLY"
    ZMQ_PROTOCOL_ERROR_ZAP_UNSPECIFIED -> "ZMQ_PROTOCOL_ERROR_ZAP_UNSPECIFIED"
    ZMQ_PROTOCOL_ERROR_ZMTP_CRYPTOGRAPHIC -> "ZMQ_PROTOCOL_ERROR_ZMTP_CRYPTOGRAPHIC"
    ZMQ_PROTOCOL_ERROR_ZMTP_INVALID_METADATA -> "ZMQ_PROTOCOL_ERROR_ZMTP_INVALID_METADATA"
    ZMQ_PROTOCOL_ERROR_ZMTP_INVALID_SEQUENCE -> "ZMQ_PROTOCOL_ERROR_ZMTP_INVALID_SEQUENCE"
    ZMQ_PROTOCOL_ERROR_ZMTP_KEY_EXCHANGE -> "ZMQ_PROTOCOL_ERROR_ZMTP_KEY_EXCHANGE"
    ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_ERROR -> "ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_ERROR"
    ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_HELLO -> "ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_HELLO"
    ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_INITIATE -> "ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_INITIATE"
    ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_MESSAGE -> "ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_MESSAGE"
    ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_READY -> "ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_READY"
    ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_UNSPECIFIED -> "ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_UNSPECIFIED"
    ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_WELCOME -> "ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_WELCOME"
    ZMQ_PROTOCOL_ERROR_ZMTP_MECHANISM_MISMATCH -> "ZMQ_PROTOCOL_ERROR_ZMTP_MECHANISM_MISMATCH"
    ZMQ_PROTOCOL_ERROR_ZMTP_UNEXPECTED_COMMAND -> "ZMQ_PROTOCOL_ERROR_ZMTP_UNEXPECTED_COMMAND"
    ZMQ_PROTOCOL_ERROR_ZMTP_UNSPECIFIED -> "ZMQ_PROTOCOL_ERROR_ZMTP_UNSPECIFIED"
    Zmq_protocol_error n -> "Zmq_protocol_error " ++ show n

pattern ZMQ_PROTOCOL_ERROR_WS_UNSPECIFIED :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_WS_UNSPECIFIED = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_WS_UNSPECIFIED

pattern ZMQ_PROTOCOL_ERROR_ZAP_BAD_REQUEST_ID :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_ZAP_BAD_REQUEST_ID = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_ZAP_BAD_REQUEST_ID

pattern ZMQ_PROTOCOL_ERROR_ZAP_BAD_VERSION :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_ZAP_BAD_VERSION = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_ZAP_BAD_VERSION

pattern ZMQ_PROTOCOL_ERROR_ZAP_INVALID_METADATA :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_ZAP_INVALID_METADATA = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_ZAP_INVALID_METADATA

pattern ZMQ_PROTOCOL_ERROR_ZAP_INVALID_STATUS_CODE :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_ZAP_INVALID_STATUS_CODE = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_ZAP_INVALID_STATUS_CODE

pattern ZMQ_PROTOCOL_ERROR_ZAP_MALFORMED_REPLY :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_ZAP_MALFORMED_REPLY = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_ZAP_MALFORMED_REPLY

pattern ZMQ_PROTOCOL_ERROR_ZAP_UNSPECIFIED :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_ZAP_UNSPECIFIED = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_ZAP_UNSPECIFIED

pattern ZMQ_PROTOCOL_ERROR_ZMTP_CRYPTOGRAPHIC :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_ZMTP_CRYPTOGRAPHIC = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_ZMTP_CRYPTOGRAPHIC

pattern ZMQ_PROTOCOL_ERROR_ZMTP_INVALID_METADATA :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_ZMTP_INVALID_METADATA = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_ZMTP_INVALID_METADATA

pattern ZMQ_PROTOCOL_ERROR_ZMTP_INVALID_SEQUENCE :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_ZMTP_INVALID_SEQUENCE = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_ZMTP_INVALID_SEQUENCE

pattern ZMQ_PROTOCOL_ERROR_ZMTP_KEY_EXCHANGE :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_ZMTP_KEY_EXCHANGE = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_ZMTP_KEY_EXCHANGE

pattern ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_ERROR :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_ERROR = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_ERROR

pattern ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_HELLO :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_HELLO = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_HELLO

pattern ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_INITIATE :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_INITIATE = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_INITIATE

pattern ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_MESSAGE :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_MESSAGE = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_MESSAGE

pattern ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_READY :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_READY = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_READY

pattern ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_UNSPECIFIED :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_UNSPECIFIED = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_UNSPECIFIED

pattern ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_WELCOME :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_WELCOME = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_WELCOME

pattern ZMQ_PROTOCOL_ERROR_ZMTP_MECHANISM_MISMATCH :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_ZMTP_MECHANISM_MISMATCH = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_ZMTP_MECHANISM_MISMATCH

pattern ZMQ_PROTOCOL_ERROR_ZMTP_UNEXPECTED_COMMAND :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_ZMTP_UNEXPECTED_COMMAND = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_ZMTP_UNEXPECTED_COMMAND

pattern ZMQ_PROTOCOL_ERROR_ZMTP_UNSPECIFIED :: Zmq_protocol_error
pattern ZMQ_PROTOCOL_ERROR_ZMTP_UNSPECIFIED = Zmq_protocol_error Zmqx.Internal.Bindings.ZMQ_PROTOCOL_ERROR_ZMTP_UNSPECIFIED

-- COMPLETE pragma intentionally missing for forward-compatibility
-- Users' programs should be allowed to use _ pattern to mean "the errors that were invented after I wrote this"

------------------------------------------------------------------------------------------------------------------------
-- Zmq_reconnect_stop_option

-- | @ZMQ_RECONNECT_STOP@ option.
newtype Zmq_reconnect_stop_option
  = Zmq_reconnect_stop_option CInt
  deriving stock (Eq, Ord)
  deriving (Monoid, Semigroup) via (Bitfield CInt)

instance Show Zmq_reconnect_stop_option where
  show =
    showMonoid
      [ -- ("ZMQ_RECONNECT_STOP_AFTER_DISCONNECT", hasReconnectStopAfterDisconnect),
        ("ZMQ_RECONNECT_STOP_CONN_REFUSED", hasReconnectStopConnRefused),
        ("ZMQ_RECONNECT_STOP_HANDSHAKE_FAILED", hasReconnectStopHandshakeFailed)
      ]

-- | /Draft API/.
-- pattern ZMQ_RECONNECT_STOP_AFTER_DISCONNECT :: Zmq_reconnect_stop_option
-- pattern ZMQ_RECONNECT_STOP_AFTER_DISCONNECT <-
--   (hasReconnectStopAfterDisconnect -> True)
--   where
--     ZMQ_RECONNECT_STOP_AFTER_DISCONNECT = Zmq_reconnect_stop_option Zmqx.Internal.Bindings.ZMQ_RECONNECT_STOP_AFTER_DISCONNECT

-- | /Draft API/.
pattern ZMQ_RECONNECT_STOP_CONN_REFUSED :: Zmq_reconnect_stop_option
pattern ZMQ_RECONNECT_STOP_CONN_REFUSED <-
  (hasReconnectStopConnRefused -> True)
  where
    ZMQ_RECONNECT_STOP_CONN_REFUSED = Zmq_reconnect_stop_option Zmqx.Internal.Bindings.ZMQ_RECONNECT_STOP_CONN_REFUSED

-- | /Draft API/.
pattern ZMQ_RECONNECT_STOP_HANDSHAKE_FAILED :: Zmq_reconnect_stop_option
pattern ZMQ_RECONNECT_STOP_HANDSHAKE_FAILED <-
  (hasReconnectStopHandshakeFailed -> True)
  where
    ZMQ_RECONNECT_STOP_HANDSHAKE_FAILED = Zmq_reconnect_stop_option Zmqx.Internal.Bindings.ZMQ_RECONNECT_STOP_HANDSHAKE_FAILED

-- hasReconnectStopAfterDisconnect :: Zmq_reconnect_stop_option -> Bool
-- hasReconnectStopAfterDisconnect (Zmq_reconnect_stop_option n) =
--   n .&. Zmqx.Internal.Bindings.ZMQ_RECONNECT_STOP_AFTER_DISCONNECT /= 0

hasReconnectStopConnRefused :: Zmq_reconnect_stop_option -> Bool
hasReconnectStopConnRefused (Zmq_reconnect_stop_option n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_RECONNECT_STOP_CONN_REFUSED /= 0

hasReconnectStopHandshakeFailed :: Zmq_reconnect_stop_option -> Bool
hasReconnectStopHandshakeFailed (Zmq_reconnect_stop_option n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_RECONNECT_STOP_HANDSHAKE_FAILED /= 0

------------------------------------------------------------------------------------------------------------------------
-- Zmq_send_option

-- | A ØMQ send option.
newtype Zmq_send_option
  = Zmq_send_option CInt
  deriving stock (Eq, Ord)
  deriving (Monoid, Semigroup) via (Bitfield CInt)

instance Show Zmq_send_option where
  show =
    showMonoid
      [ ("ZMQ_DONTWAIT", hasDontwait),
        ("ZMQ_SNDMORE", hasSndmore)
      ]

pattern ZMQ_DONTWAIT :: Zmq_send_option
pattern ZMQ_DONTWAIT <-
  (hasDontwait -> True)
  where
    ZMQ_DONTWAIT = Zmq_send_option Zmqx.Internal.Bindings.ZMQ_DONTWAIT

pattern ZMQ_SNDMORE :: Zmq_send_option
pattern ZMQ_SNDMORE <-
  (hasSndmore -> True)
  where
    ZMQ_SNDMORE = Zmq_send_option Zmqx.Internal.Bindings.ZMQ_SNDMORE

hasDontwait :: Zmq_send_option -> Bool
hasDontwait (Zmq_send_option n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_DONTWAIT /= 0

hasSndmore :: Zmq_send_option -> Bool
hasSndmore (Zmq_send_option n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_SNDMORE /= 0

------------------------------------------------------------------------------------------------------------------------
-- Zmq_socket

-- | A ØMQ socket.
newtype Zmq_socket
  = Zmq_socket (Ptr ())
  deriving stock (Eq, Ord)

------------------------------------------------------------------------------------------------------------------------
-- Zmq_socket_event_type

-- | A ØMQ socket event type.
--
-- The @Monoid@ instance can be used to combine types together for the purpose of passing to
-- 'Libzmq.zmq_socket_monitor'.
--
-- FIXME rename to Zmq_socket_event_type
newtype Zmq_socket_events
  = Zmq_socket_events CInt
  deriving stock (Eq, Ord)
  deriving (Monoid, Semigroup) via (Bitfield CInt)

instance Show Zmq_socket_events where
  show =
    showMonoid
      [ ("ZMQ_EVENT_ACCEPTED", hasAccepted),
        ("ZMQ_EVENT_ACCEPT_FAILED", hasAcceptFailed),
        ("ZMQ_EVENT_ALL", hasAll),
        ("ZMQ_EVENT_BIND_FAILED", hasBindFailed),
        ("ZMQ_EVENT_CLOSED", hasClosed),
        ("ZMQ_EVENT_CLOSE_FAILED", hasCloseFailed),
        ("ZMQ_EVENT_CONNECTED", hasConnected),
        ("ZMQ_EVENT_CONNECT_DELAYED", hasConnectDelayed),
        ("ZMQ_EVENT_CONNECT_RETRIED", hasConnectRetried),
        ("ZMQ_EVENT_DISCONNECTED", hasDisconnected),
        ("ZMQ_EVENT_HANDSHAKE_FAILED_AUTH", hasHandshakeFailedAuth),
        ("ZMQ_EVENT_HANDSHAKE_FAILED_NO_DETAIL", hasHandshakeFailedNoDetail),
        ("ZMQ_EVENT_HANDSHAKE_FAILED_PROTOCOL", hasHandshakeFailedProtocol),
        ("ZMQ_EVENT_HANDSHAKE_SUCCEEDED", hasHandshakeSucceeded),
        ("ZMQ_EVENT_LISTENING", hasListening),
        ("ZMQ_EVENT_MONITOR_STOPPED", hasMonitorStopped)
      ]

pattern ZMQ_EVENT_ACCEPTED :: Zmq_socket_events
pattern ZMQ_EVENT_ACCEPTED = Zmq_socket_events Zmqx.Internal.Bindings.ZMQ_EVENT_ACCEPTED

pattern ZMQ_EVENT_ACCEPT_FAILED :: Zmq_socket_events
pattern ZMQ_EVENT_ACCEPT_FAILED = Zmq_socket_events Zmqx.Internal.Bindings.ZMQ_EVENT_ACCEPT_FAILED

pattern ZMQ_EVENT_ALL :: Zmq_socket_events
pattern ZMQ_EVENT_ALL = Zmq_socket_events Zmqx.Internal.Bindings.ZMQ_EVENT_ALL

pattern ZMQ_EVENT_BIND_FAILED :: Zmq_socket_events
pattern ZMQ_EVENT_BIND_FAILED = Zmq_socket_events Zmqx.Internal.Bindings.ZMQ_EVENT_BIND_FAILED

pattern ZMQ_EVENT_CLOSED :: Zmq_socket_events
pattern ZMQ_EVENT_CLOSED = Zmq_socket_events Zmqx.Internal.Bindings.ZMQ_EVENT_CLOSED

pattern ZMQ_EVENT_CLOSE_FAILED :: Zmq_socket_events
pattern ZMQ_EVENT_CLOSE_FAILED = Zmq_socket_events Zmqx.Internal.Bindings.ZMQ_EVENT_CLOSE_FAILED

pattern ZMQ_EVENT_CONNECTED :: Zmq_socket_events
pattern ZMQ_EVENT_CONNECTED = Zmq_socket_events Zmqx.Internal.Bindings.ZMQ_EVENT_CONNECTED

pattern ZMQ_EVENT_CONNECT_DELAYED :: Zmq_socket_events
pattern ZMQ_EVENT_CONNECT_DELAYED = Zmq_socket_events Zmqx.Internal.Bindings.ZMQ_EVENT_CONNECT_DELAYED

pattern ZMQ_EVENT_CONNECT_RETRIED :: Zmq_socket_events
pattern ZMQ_EVENT_CONNECT_RETRIED = Zmq_socket_events Zmqx.Internal.Bindings.ZMQ_EVENT_CONNECT_RETRIED

pattern ZMQ_EVENT_DISCONNECTED :: Zmq_socket_events
pattern ZMQ_EVENT_DISCONNECTED = Zmq_socket_events Zmqx.Internal.Bindings.ZMQ_EVENT_DISCONNECTED

pattern ZMQ_EVENT_HANDSHAKE_FAILED_AUTH :: Zmq_socket_events
pattern ZMQ_EVENT_HANDSHAKE_FAILED_AUTH = Zmq_socket_events Zmqx.Internal.Bindings.ZMQ_EVENT_HANDSHAKE_FAILED_AUTH

pattern ZMQ_EVENT_HANDSHAKE_FAILED_NO_DETAIL :: Zmq_socket_events
pattern ZMQ_EVENT_HANDSHAKE_FAILED_NO_DETAIL = Zmq_socket_events Zmqx.Internal.Bindings.ZMQ_EVENT_HANDSHAKE_FAILED_NO_DETAIL

pattern ZMQ_EVENT_HANDSHAKE_FAILED_PROTOCOL :: Zmq_socket_events
pattern ZMQ_EVENT_HANDSHAKE_FAILED_PROTOCOL = Zmq_socket_events Zmqx.Internal.Bindings.ZMQ_EVENT_HANDSHAKE_FAILED_PROTOCOL

pattern ZMQ_EVENT_HANDSHAKE_SUCCEEDED :: Zmq_socket_events
pattern ZMQ_EVENT_HANDSHAKE_SUCCEEDED = Zmq_socket_events Zmqx.Internal.Bindings.ZMQ_EVENT_HANDSHAKE_SUCCEEDED

pattern ZMQ_EVENT_LISTENING :: Zmq_socket_events
pattern ZMQ_EVENT_LISTENING = Zmq_socket_events Zmqx.Internal.Bindings.ZMQ_EVENT_LISTENING

pattern ZMQ_EVENT_MONITOR_STOPPED :: Zmq_socket_events
pattern ZMQ_EVENT_MONITOR_STOPPED = Zmq_socket_events Zmqx.Internal.Bindings.ZMQ_EVENT_MONITOR_STOPPED

-- COMPLETE pragma intentionally missing for forward-compatibility
-- Users' programs should be allowed to use _ pattern to mean "the events that were invented after I wrote this"

hasAccepted :: Zmq_socket_events -> Bool
hasAccepted (Zmq_socket_events n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_EVENT_ACCEPTED /= 0

hasAcceptFailed :: Zmq_socket_events -> Bool
hasAcceptFailed (Zmq_socket_events n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_EVENT_ACCEPT_FAILED /= 0

hasAll :: Zmq_socket_events -> Bool
hasAll (Zmq_socket_events n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_EVENT_ALL /= 0

hasBindFailed :: Zmq_socket_events -> Bool
hasBindFailed (Zmq_socket_events n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_EVENT_BIND_FAILED /= 0

hasClosed :: Zmq_socket_events -> Bool
hasClosed (Zmq_socket_events n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_EVENT_CLOSED /= 0

hasCloseFailed :: Zmq_socket_events -> Bool
hasCloseFailed (Zmq_socket_events n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_EVENT_CLOSE_FAILED /= 0

hasConnected :: Zmq_socket_events -> Bool
hasConnected (Zmq_socket_events n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_EVENT_CONNECTED /= 0

hasConnectDelayed :: Zmq_socket_events -> Bool
hasConnectDelayed (Zmq_socket_events n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_EVENT_CONNECT_DELAYED /= 0

hasConnectRetried :: Zmq_socket_events -> Bool
hasConnectRetried (Zmq_socket_events n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_EVENT_CONNECT_RETRIED /= 0

hasDisconnected :: Zmq_socket_events -> Bool
hasDisconnected (Zmq_socket_events n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_EVENT_DISCONNECTED /= 0

hasHandshakeFailedAuth :: Zmq_socket_events -> Bool
hasHandshakeFailedAuth (Zmq_socket_events n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_EVENT_HANDSHAKE_FAILED_AUTH /= 0

hasHandshakeFailedNoDetail :: Zmq_socket_events -> Bool
hasHandshakeFailedNoDetail (Zmq_socket_events n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_EVENT_HANDSHAKE_FAILED_NO_DETAIL /= 0

hasHandshakeFailedProtocol :: Zmq_socket_events -> Bool
hasHandshakeFailedProtocol (Zmq_socket_events n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_EVENT_HANDSHAKE_FAILED_PROTOCOL /= 0

hasHandshakeSucceeded :: Zmq_socket_events -> Bool
hasHandshakeSucceeded (Zmq_socket_events n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_EVENT_HANDSHAKE_SUCCEEDED /= 0

hasListening :: Zmq_socket_events -> Bool
hasListening (Zmq_socket_events n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_EVENT_LISTENING /= 0

hasMonitorStopped :: Zmq_socket_events -> Bool
hasMonitorStopped (Zmq_socket_events n) =
  n .&. Zmqx.Internal.Bindings.ZMQ_EVENT_MONITOR_STOPPED /= 0

------------------------------------------------------------------------------------------------------------------------
-- Zmq_socket_option

-- | A ØMQ socket option.
data Zmq_socket_option a where
  ZMQ_AFFINITY :: Zmq_socket_option Word64
  ZMQ_BACKLOG :: Zmq_socket_option Int32
  ZMQ_BINDTODEVICE :: Zmq_socket_option Text
  ZMQ_CONFLATE :: Zmq_socket_option Int32
  ZMQ_CONNECT_ROUTING_ID :: Zmq_socket_option ByteString
  ZMQ_CONNECT_TIMEOUT :: Zmq_socket_option Int32
  ZMQ_CURVE_PUBLICKEY :: Zmq_socket_option ByteString
  ZMQ_CURVE_SECRETKEY :: Zmq_socket_option ByteString
  ZMQ_CURVE_SERVER :: Zmq_socket_option Int32
  ZMQ_CURVE_SERVERKEY :: Zmq_socket_option ByteString
  -- | /Draft API/.
  ZMQ_DISCONNECT_MSG :: Zmq_socket_option ByteString
  ZMQ_GSSAPI_PLAINTEXT :: Zmq_socket_option Int32
  ZMQ_GSSAPI_PRINCIPAL :: Zmq_socket_option Text
  ZMQ_GSSAPI_PRINCIPAL_NAMETYPE :: Zmq_socket_option Int32
  ZMQ_GSSAPI_SERVER :: Zmq_socket_option Int32
  ZMQ_GSSAPI_SERVICE_PRINCIPAL :: Zmq_socket_option Text
  ZMQ_GSSAPI_SERVICE_PRINCIPAL_NAMETYPE :: Zmq_socket_option Int32
  ZMQ_HANDSHAKE_IVL :: Zmq_socket_option Int32
  ZMQ_HEARTBEAT_IVL :: Zmq_socket_option Int32
  ZMQ_HEARTBEAT_TIMEOUT :: Zmq_socket_option Int32
  ZMQ_HEARTBEAT_TTL :: Zmq_socket_option Int32
  -- | /Draft API/.
  ZMQ_HELLO_MSG :: Zmq_socket_option ByteString
  ZMQ_IMMEDIATE :: Zmq_socket_option Int32
  ZMQ_INVERT_MATCHING :: Zmq_socket_option Int32
  -- | /Draft API/.
  ZMQ_IN_BATCH_SIZE :: Zmq_socket_option Int32
  ZMQ_IPV6' :: Zmq_socket_option Int32
  ZMQ_LINGER :: Zmq_socket_option Int32
  -- | /Draft API/.
  ZMQ_LOOPBACK_FASTPATH :: Zmq_socket_option Int32
  ZMQ_MAXMSGSIZE :: Zmq_socket_option Int64
  -- | /Draft API/.
  ZMQ_METADATA :: Zmq_socket_option Text
  ZMQ_MULTICAST_HOPS :: Zmq_socket_option Int32
  -- | /Draft API/.
  ZMQ_MULTICAST_LOOP :: Zmq_socket_option Int32
  ZMQ_MULTICAST_MAXTPDU :: Zmq_socket_option Int32
  -- | /Draft API/.
  ZMQ_ONLY_FIRST_SUBSCRIBE :: Zmq_socket_option Int32
  -- | /Draft API/.
  ZMQ_OUT_BATCH_SIZE :: Zmq_socket_option Int32
  ZMQ_PLAIN_PASSWORD :: Zmq_socket_option Text
  ZMQ_PLAIN_SERVER :: Zmq_socket_option Int32
  ZMQ_PLAIN_USERNAME :: Zmq_socket_option Text
  -- | /Draft API/.
  ZMQ_PRIORITY :: Zmq_socket_option Int32
  ZMQ_PROBE_ROUTER :: Zmq_socket_option Int32
  ZMQ_RATE :: Zmq_socket_option Int32
  ZMQ_RCVBUF :: Zmq_socket_option Int32
  ZMQ_RCVHWM :: Zmq_socket_option Int32
  ZMQ_RCVTIMEO :: Zmq_socket_option Int32
  ZMQ_RECONNECT_IVL :: Zmq_socket_option Int32
  ZMQ_RECONNECT_IVL_MAX :: Zmq_socket_option Int32
  -- -- | /Draft API/.
  ZMQ_RECONNECT_STOP :: Zmq_socket_option Zmq_reconnect_stop_option
  ZMQ_RECOVERY_IVL :: Zmq_socket_option Int32
  ZMQ_REQ_CORRELATE :: Zmq_socket_option Int32
  ZMQ_REQ_RELAXED :: Zmq_socket_option Int32
  ZMQ_ROUTER_HANDOVER :: Zmq_socket_option Int32
  ZMQ_ROUTER_MANDATORY :: Zmq_socket_option Int32
  -- -- | /Draft API/.
  -- ZMQ_ROUTER_NOTIFY :: Zmq_socket_option ()
  ZMQ_ROUTING_ID :: Zmq_socket_option ByteString
  ZMQ_SNDBUF :: Zmq_socket_option Int32
  ZMQ_SNDHWM :: Zmq_socket_option Int32
  ZMQ_SNDTIMEO :: Zmq_socket_option Int32
  -- | /Draft API/.
  ZMQ_SOCKS_PASSWORD :: Zmq_socket_option Text
  ZMQ_SOCKS_PROXY :: Zmq_socket_option Text
  -- | /Draft API/.
  ZMQ_SOCKS_USERNAME :: Zmq_socket_option Text
  ZMQ_STREAM_NOTIFY :: Zmq_socket_option Int32
  ZMQ_SUBSCRIBE :: Zmq_socket_option ByteString
  ZMQ_TCP_KEEPALIVE :: Zmq_socket_option Int32
  ZMQ_TCP_KEEPALIVE_CNT :: Zmq_socket_option Int32
  ZMQ_TCP_KEEPALIVE_IDLE :: Zmq_socket_option Int32
  ZMQ_TCP_KEEPALIVE_INTVL :: Zmq_socket_option Int32
  ZMQ_TCP_MAXRT :: Zmq_socket_option Int32
  ZMQ_TOS :: Zmq_socket_option Int32
  ZMQ_UNSUBSCRIBE :: Zmq_socket_option ByteString
  ZMQ_USE_FD :: Zmq_socket_option Int32
  ZMQ_VMCI_BUFFER_MAX_SIZE :: Zmq_socket_option Word64
  ZMQ_VMCI_BUFFER_MIN_SIZE :: Zmq_socket_option Word64
  ZMQ_VMCI_BUFFER_SIZE :: Zmq_socket_option Word64
  ZMQ_VMCI_CONNECT_TIMEOUT :: Zmq_socket_option Int32
  -- | /Draft API/.
  ZMQ_WSS_CERT_PEM :: Zmq_socket_option Text
  -- | /Draft API/.
  ZMQ_WSS_HOSTNAME :: Zmq_socket_option Text
  -- | /Draft API/.
  ZMQ_WSS_KEY_PEM :: Zmq_socket_option Text
  -- | /Draft API/.
  ZMQ_WSS_TRUST_PEM :: Zmq_socket_option Text
  -- | /Draft API/.
  ZMQ_WSS_TRUST_SYSTEM :: Zmq_socket_option Int32
  ZMQ_XPUB_MANUAL :: Zmq_socket_option Int32
  -- | /Draft API/.
  ZMQ_XPUB_MANUAL_LAST_VALUE :: Zmq_socket_option Int32
  ZMQ_XPUB_NODROP :: Zmq_socket_option Int32
  ZMQ_XPUB_VERBOSE :: Zmq_socket_option Int32
  ZMQ_XPUB_VERBOSER :: Zmq_socket_option Int32
  ZMQ_XPUB_WELCOME_MSG :: Zmq_socket_option ByteString
  ZMQ_ZAP_DOMAIN :: Zmq_socket_option Text
  -- | /Draft API/.
  ZMQ_ZAP_ENFORCE_DOMAIN :: Zmq_socket_option Int32

deriving stock instance Eq (Zmq_socket_option a)

deriving stock instance Show (Zmq_socket_option a)

------------------------------------------------------------------------------------------------------------------------
-- Zmq_socket_type

-- | A ØMQ socket type.
newtype Zmq_socket_type
  = Zmq_socket_type CInt
  deriving stock (Eq, Ord)

instance Show Zmq_socket_type where
  show = \case
    ZMQ_CHANNEL -> "ZMQ_CHANNEL"
    ZMQ_CLIENT -> "ZMQ_CLIENT"
    ZMQ_DEALER -> "ZMQ_DEALER"
    ZMQ_DGRAM -> "ZMQ_DGRAM"
    ZMQ_DISH -> "ZMQ_DISH"
    ZMQ_GATHER -> "ZMQ_GATHER"
    ZMQ_PAIR -> "ZMQ_PAIR"
    ZMQ_PEER -> "ZMQ_PEER"
    ZMQ_PUB -> "ZMQ_PUB"
    ZMQ_PULL -> "ZMQ_PULL"
    ZMQ_PUSH -> "ZMQ_PUSH"
    ZMQ_RADIO -> "ZMQ_RADIO"
    ZMQ_REP -> "ZMQ_REP"
    ZMQ_REQ -> "ZMQ_REQ"
    ZMQ_ROUTER -> "ZMQ_ROUTER"
    ZMQ_SCATTER -> "ZMQ_SCATTER"
    ZMQ_SERVER -> "ZMQ_SERVER"
    ZMQ_STREAM -> "ZMQ_STREAM"
    ZMQ_SUB -> "ZMQ_SUB"
    ZMQ_XPUB -> "ZMQ_XPUB"
    ZMQ_XSUB -> "ZMQ_XSUB"

-- | /Draft API/.
pattern ZMQ_CHANNEL :: Zmq_socket_type
pattern ZMQ_CHANNEL = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_CHANNEL

-- | /Draft API/.
pattern ZMQ_CLIENT :: Zmq_socket_type
pattern ZMQ_CLIENT = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_CLIENT

pattern ZMQ_DEALER :: Zmq_socket_type
pattern ZMQ_DEALER = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_DEALER

-- | /Draft API/.
pattern ZMQ_DGRAM :: Zmq_socket_type
pattern ZMQ_DGRAM = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_DGRAM

-- | /Draft API/.
pattern ZMQ_DISH :: Zmq_socket_type
pattern ZMQ_DISH = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_DISH

-- | /Draft API/.
pattern ZMQ_GATHER :: Zmq_socket_type
pattern ZMQ_GATHER = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_GATHER

pattern ZMQ_PAIR :: Zmq_socket_type
pattern ZMQ_PAIR = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_PAIR

-- | /Draft API/.
pattern ZMQ_PEER :: Zmq_socket_type
pattern ZMQ_PEER = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_PEER

pattern ZMQ_PUB :: Zmq_socket_type
pattern ZMQ_PUB = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_PUB

pattern ZMQ_PULL :: Zmq_socket_type
pattern ZMQ_PULL = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_PULL

pattern ZMQ_PUSH :: Zmq_socket_type
pattern ZMQ_PUSH = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_PUSH

-- | /Draft API/.
pattern ZMQ_RADIO :: Zmq_socket_type
pattern ZMQ_RADIO = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_RADIO

pattern ZMQ_REP :: Zmq_socket_type
pattern ZMQ_REP = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_REP

pattern ZMQ_REQ :: Zmq_socket_type
pattern ZMQ_REQ = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_REQ

pattern ZMQ_ROUTER :: Zmq_socket_type
pattern ZMQ_ROUTER = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_ROUTER

-- | /Draft API/.
pattern ZMQ_SCATTER :: Zmq_socket_type
pattern ZMQ_SCATTER = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_SCATTER

-- | /Draft API/.
pattern ZMQ_SERVER :: Zmq_socket_type
pattern ZMQ_SERVER = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_SERVER

pattern ZMQ_STREAM :: Zmq_socket_type
pattern ZMQ_STREAM = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_STREAM

pattern ZMQ_SUB :: Zmq_socket_type
pattern ZMQ_SUB = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_SUB

pattern ZMQ_XPUB :: Zmq_socket_type
pattern ZMQ_XPUB = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_XPUB

pattern ZMQ_XSUB :: Zmq_socket_type
pattern ZMQ_XSUB = Zmq_socket_type Zmqx.Internal.Bindings.ZMQ_XSUB

{-# COMPLETE
  ZMQ_CHANNEL,
  ZMQ_CLIENT,
  ZMQ_DEALER,
  ZMQ_DGRAM,
  ZMQ_DISH,
  ZMQ_GATHER,
  ZMQ_PAIR,
  ZMQ_PEER,
  ZMQ_PUB,
  ZMQ_PULL,
  ZMQ_PUSH,
  ZMQ_RADIO,
  ZMQ_REP,
  ZMQ_REQ,
  ZMQ_ROUTER,
  ZMQ_SCATTER,
  ZMQ_SERVER,
  ZMQ_STREAM,
  ZMQ_SUB,
  ZMQ_XPUB,
  ZMQ_XSUB
  #-}

------------------------------------------------------------------------------------------------------------------------
-- Utils

newtype Bitfield a
  = Bitfield a

instance (Bits a, Num a) => Monoid (Bitfield a) where
  mempty = Bitfield 0
  mappend = (<>)

instance (Bits a) => Semigroup (Bitfield a) where
  Bitfield x <> Bitfield y = Bitfield (x .|. y)

showMonoid :: [(String, a -> Bool)] -> a -> String
showMonoid xs y =
  xs
    & mapMaybe (\(x, p) -> x <$ guard (p y))
    & List.intersperse "<>"
    & \case
      [] -> "mempty"
      events -> unwords events
