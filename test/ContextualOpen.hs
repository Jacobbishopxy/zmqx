-- file: ContextualOpen.hs
-- brief: Verify all sockets can be opened via ContextualOpen under an explicit context.

module Main where

import Common (unwrap)
import Zmqx
import Zmqx.Dealer
import Zmqx.Pair
import Zmqx.Pub
import Zmqx.Pull
import Zmqx.Push
import Zmqx.Rep
import Zmqx.Req
import Zmqx.Router
import Zmqx.Sub
import Zmqx.XPub
import Zmqx.XSub

main :: IO ()
main =
  -- One explicit context; assert each socket role implements ContextualOpen.
  withContext Zmqx.defaultOptions \ctx -> do
    _ <- unwrap (openWith ctx (Zmqx.Dealer.defaultOptions <> name "dealer"))
    _ <- unwrap (openWith ctx (Zmqx.Pair.defaultOptions <> name "pair") )
    _ <- unwrap (openWith ctx (Zmqx.Pub.defaultOptions <> name "pub") )
    _ <- unwrap (openWith ctx (Zmqx.Pull.defaultOptions <> name "pull") )
    _ <- unwrap (openWith ctx (Zmqx.Push.defaultOptions <> name "push") )
    _ <- unwrap (openWith ctx (Zmqx.Rep.defaultOptions <> name "rep") )
    _ <- unwrap (openWith ctx (Zmqx.Req.defaultOptions <> name "req") )
    _ <- unwrap (openWith ctx (Zmqx.Router.defaultOptions <> name "router") )
    _ <- unwrap (openWith ctx (Zmqx.Sub.defaultOptions <> name "sub") )
    _ <- unwrap (openWith ctx (Zmqx.XPub.defaultOptions <> name "xpub") )
    _ <- unwrap (openWith ctx (Zmqx.XSub.defaultOptions <> name "xsub") )
    pure ()
