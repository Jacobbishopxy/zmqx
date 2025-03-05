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
    sends,
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

    -- * Opt
    Opt.SocketOpt (..),
    Opt.setSocketOpt,

    -- * Errors
    Error (..),
    Zmq_error (..),

    -- * Socket subclasses
    Socket.CanSend,
    Socket.CanSends,
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
import Zmqx.Core.Context
import Zmqx.Core.Curve
import Zmqx.Core.Monitor (monitor)
import Zmqx.Core.Options qualified as Options
import Zmqx.Core.Poll (CanPoll, Ready (..), Sockets, also, poll, pollFor, pollUntil, the)
import Zmqx.Core.Socket qualified as Socket
import Zmqx.Dealer (Dealer)
import Zmqx.Error (Error (..))
import Zmqx.Internal
import Zmqx.Options qualified as Opt
import Zmqx.Pair (Pair)
import Zmqx.Pub (Pub)
import Zmqx.Pull (Pull)
import Zmqx.Push (Push)
import Zmqx.Rep (Rep)
import Zmqx.Req (Req)
import Zmqx.Router (Router)
import Zmqx.Sub (Sub)
import Zmqx.Subscription (pattern Subscribe, pattern Unsubscribe)
import Zmqx.XPub (XPub)
import Zmqx.XSub (XSub)

send :: (Socket.CanSend socket) => socket -> ByteString -> IO (Either Error ())
send =
  Socket.send_

sends :: (Socket.CanSends socket) => socket -> [ByteString] -> IO (Either Error ())
sends =
  Socket.sends_

receive :: (Socket.CanReceive socket) => socket -> IO (Either Error ByteString)
receive =
  Socket.receive_

receives :: (Socket.CanReceives socket) => socket -> IO (Either Error [ByteString])
receives =
  Socket.receives_

version :: (Int, Int, Int)
version =
  zmq_version
