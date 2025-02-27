-- file: Zmqx.hs
-- author: Jacob Xie
-- date: 2025/02/12 08:57:42 Wednesday
-- brief:

module Zmqx
  ( -- * Main
    run,

    -- ** Main options
    Options.ioThreads,
    Options.maxSockets,

    -- * Socket
    Socket.Socket,
    monitor,

    -- ** Options
    Options.curveClient,
    Options.curveServer,
    Options.lossy,
    Options.name,
    Options.sendQueueSize,

    -- ** Peering
    Socket.bind,
    Socket.unbind,
    Socket.connect,
    Socket.disconnect,

    -- ** Messaging
    send,
    receive,
    receives,

    -- ** IO multiplexing
    Sockets,
    Ready (..),
    the,
    also,
    poll,
    pollFor,
    pollUntil,

    -- * Socket types
    Dealer,
    Pair,
    Pub,
    Pull,
    Push,
    Rep,
    Req,
    Router,
    Sub,
    XPub,
    XSub,

    -- * Subscription message
    pattern Subscribe,
    pattern Unsubscribe,

    -- * Encryption
    CurvePublicKey (..),
    CurveSecretKey (..),
    generateCurveSecretKey,
    deriveCurvePublicKey,

    -- * Options
    Options.Options,
    Options.defaultOptions,

    -- * Errors
    Error (..),
    Zmq_error (..),

    -- * Socket subclasses
    Socket.CanSend,
    Socket.CanReceive,
    Socket.CanReceives,
    CanPoll,

    -- ** Options
    Options.CanSetLossy,
    Options.CanSetSendQueueSize,

    -- * Version
    version,
  )
where

import Data.ByteString (ByteString)
import Zmqx.Core.Dealer (Dealer)
import Zmqx.Core.Error (Error (..))
import Zmqx.Core.Internal.Context
import Zmqx.Core.Internal.Curve
import Zmqx.Core.Internal.Monitor (monitor)
import Zmqx.Core.Internal.Options qualified as Options
import Zmqx.Core.Internal.Poll (CanPoll, Ready (..), Sockets, also, poll, pollFor, pollUntil, the)
import Zmqx.Core.Internal.Socket qualified as Socket
import Zmqx.Core.Pair (Pair)
import Zmqx.Core.Pub (Pub)
import Zmqx.Core.Pull (Pull)
import Zmqx.Core.Push (Push)
import Zmqx.Core.Rep (Rep)
import Zmqx.Core.Req (Req)
import Zmqx.Core.Router (Router)
import Zmqx.Core.Sub (Sub)
import Zmqx.Core.Subscription (pattern Subscribe, pattern Unsubscribe)
import Zmqx.Core.XPub (XPub)
import Zmqx.Core.XSub (XSub)
import Zmqx.Internal

send :: (Socket.CanSend socket) => socket -> ByteString -> IO (Either Error ())
send =
  Socket.send_

receive :: (Socket.CanReceive socket) => socket -> IO (Either Error ByteString)
receive =
  Socket.receive_

receives :: (Socket.CanReceives socket) => socket -> IO (Either Error [ByteString])
receives =
  Socket.receives_

version :: (Int, Int, Int)
version =
  zmq_version
