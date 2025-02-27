module Zmqx.Core.Socket (Socket) where

import GHC.TypeLits (Symbol)

type role Socket nominal

data Socket (a :: Symbol)
