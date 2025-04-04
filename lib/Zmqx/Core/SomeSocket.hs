module Zmqx.Core.SomeSocket
  ( SomeSocket (..),
  )
where

import Zmqx.Core.Socket (Socket (..))

data SomeSocket
  = forall a. SomeSocket (Socket a)

instance Eq SomeSocket where
  SomeSocket x == SomeSocket y = zsocket x == zsocket y
  SomeSocket x /= SomeSocket y = zsocket x /= zsocket y

instance Ord SomeSocket where
  compare (SomeSocket x) (SomeSocket y) = compare (zsocket x) (zsocket y)
  SomeSocket x < SomeSocket y = zsocket x < zsocket y
  SomeSocket x <= SomeSocket y = zsocket x <= zsocket y
  SomeSocket x > SomeSocket y = zsocket x > zsocket y
  SomeSocket x >= SomeSocket y = zsocket x >= zsocket y
