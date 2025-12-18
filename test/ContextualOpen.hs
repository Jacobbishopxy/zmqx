-- file: ContextualOpen.hs
-- brief: Verify all sockets can be opened via ContextualOpen under an explicit context.

module Main where

import Common (unwrap)
import Zmqx
import qualified Zmqx.Dealer as Dealer
import qualified Zmqx.Pair as Pair
import qualified Zmqx.Pub as Pub
import qualified Zmqx.Pull as Pull
import qualified Zmqx.Push as Push
import qualified Zmqx.Rep as Rep
import qualified Zmqx.Req as Req
import qualified Zmqx.Router as Router
import qualified Zmqx.Sub as Sub
import qualified Zmqx.XPub as XPub
import qualified Zmqx.XSub as XSub

main :: IO ()
main =
  -- One explicit context; assert each socket role implements ContextualOpen.
  withContext defaultOptions \ctx -> do
    _ <- unwrap (openWith ctx (Dealer.defaultOptions <> name "dealer") :: IO (Either Error Dealer.Dealer))
    _ <- unwrap (openWith ctx (Pair.defaultOptions <> name "pair") :: IO (Either Error Pair.Pair))
    _ <- unwrap (openWith ctx (Pub.defaultOptions <> name "pub") :: IO (Either Error Pub.Pub))
    _ <- unwrap (openWith ctx (Pull.defaultOptions <> name "pull") :: IO (Either Error Pull.Pull))
    _ <- unwrap (openWith ctx (Push.defaultOptions <> name "push") :: IO (Either Error Push.Push))
    _ <- unwrap (openWith ctx (Rep.defaultOptions <> name "rep") :: IO (Either Error Rep.Rep))
    _ <- unwrap (openWith ctx (Req.defaultOptions <> name "req") :: IO (Either Error Req.Req))
    _ <- unwrap (openWith ctx (Router.defaultOptions <> name "router") :: IO (Either Error Router.Router))
    _ <- unwrap (openWith ctx (Sub.defaultOptions <> name "sub") :: IO (Either Error Sub.Sub))
    _ <- unwrap (openWith ctx (XPub.defaultOptions <> name "xpub") :: IO (Either Error XPub.XPub))
    _ <- unwrap (openWith ctx (XSub.defaultOptions <> name "xsub") :: IO (Either Error XSub.XSub))
    pure ()
