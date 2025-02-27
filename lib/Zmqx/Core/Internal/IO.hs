{-# LANGUAGE MagicHash #-}

module Zmqx.Core.Internal.IO
  ( keepAlive,
  )
where

import GHC.Exts (keepAlive#)
import GHC.IO (IO (IO))

keepAlive :: a -> IO b -> IO b
keepAlive thing (IO action) =
  IO \s -> keepAlive# thing s action
