-- file: Options.hs
-- author: Jacob Xie
-- date: 2025/03/05 22:47:09 Wednesday
-- brief:

module Zmqx.Options
  ( SocketOpt (..),
    setSocketOpt,
  )
where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Zmqx.Core.Options
import Zmqx.Core.Socket
import Zmqx.Internal

data SocketOpt
  = RoutingId ByteString
  | SndTimeO Int32
  | RcvTimeO Int32
  | SndHWM Int32
  | RcvHWM Int32
  | Sbc ByteString
  | USbc ByteString

setSocketOpt :: Socket a -> SocketOpt -> IO ()
setSocketOpt Socket {zsocket} opt =
  case opt of
    RoutingId ri -> setSocketOptions zsocket (sockopt ZMQ_ROUTING_ID ri)
    SndTimeO t -> setSocketOptions zsocket (sockopt ZMQ_SNDTIMEO t)
    RcvTimeO t -> setSocketOptions zsocket (sockopt ZMQ_RCVTIMEO t)
    SndHWM h -> setSocketOptions zsocket (sockopt ZMQ_SNDHWM h)
    RcvHWM h -> setSocketOptions zsocket (sockopt ZMQ_RCVHWM h)
    Sbc t -> setSocketOptions zsocket (sockopt ZMQ_SUBSCRIBE t)
    USbc t -> setSocketOptions zsocket (sockopt ZMQ_UNSUBSCRIBE t)
