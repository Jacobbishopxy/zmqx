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
  = Z_RoutingId ByteString
  | Z_Rate Int32
  | Z_SndBuf Int32
  | Z_RcvBuf Int32
  | Z_SndTimeO Int32
  | Z_RcvTimeO Int32
  | Z_SndHWM Int32
  | Z_RcvHWM Int32
  | Z_Subscribe ByteString
  | Z_Unsubscribe ByteString

setSocketOpt :: Socket a -> SocketOpt -> IO ()
setSocketOpt Socket {zsocket} opt =
  case opt of
    Z_RoutingId ri -> setSocketOptions zsocket (sockopt ZMQ_ROUTING_ID ri)
    Z_Rate r -> setSocketOptions zsocket (sockopt ZMQ_RATE r)
    Z_SndBuf t -> setSocketOptions zsocket (sockopt ZMQ_SNDBUF t)
    Z_RcvBuf t -> setSocketOptions zsocket (sockopt ZMQ_RCVBUF t)
    Z_SndTimeO t -> setSocketOptions zsocket (sockopt ZMQ_SNDTIMEO t)
    Z_RcvTimeO t -> setSocketOptions zsocket (sockopt ZMQ_RCVTIMEO t)
    Z_SndHWM h -> setSocketOptions zsocket (sockopt ZMQ_SNDHWM h)
    Z_RcvHWM h -> setSocketOptions zsocket (sockopt ZMQ_RCVHWM h)
    Z_Subscribe t -> setSocketOptions zsocket (sockopt ZMQ_SUBSCRIBE t)
    Z_Unsubscribe t -> setSocketOptions zsocket (sockopt ZMQ_UNSUBSCRIBE t)
