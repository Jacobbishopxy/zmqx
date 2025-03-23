-- file: ReceivesFor.hs
-- author: Jacob Xie
-- date: 2025/02/12
-- brief: Test for receivesFor function with various socket types

module Main where

import Common (unwrap)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, when)
import Data.ByteString.Char8 qualified as C
import Data.Functor ((<&>))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
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

-- unwrap :: IO (Either Zmqx.Error a) -> IO a
-- unwrap action =
--   action >>= \case
--     Left err -> throwIO err
--     Right value -> pure value

-- Test DEALER socket
testDealer :: IO ()
testDealer = do
  putStrLn "Testing receivesFor with DEALER socket"

  run Zmqx.defaultOptions do
    dealer <- unwrap $ Zmqx.Dealer.open (Zmqx.name "dealer")
    unwrap $ Zmqx.bind dealer "tcp://127.0.0.1:5555"

    -- Fork a thread to send messages
    void $ forkIO $ do
      threadDelay 1000000 -- 1s
      forever $ do
        client <- unwrap $ Zmqx.Dealer.open (Zmqx.name "dealer")
        unwrap $ Zmqx.connect client "tcp://127.0.0.1:5555"
        unwrap $ Zmqx.send client (C.pack "Hello from DEALER")
        threadDelay 3000000 -- 3s

    -- Receive loop with timeout
    forever $ do
      putStrLn "Waiting for messages..."
      result <- Zmqx.receivesFor dealer 2000 -- 2s timeout
      case result of
        Right (Just msgs) -> putStrLn $ "Received: " ++ show msgs
        Right Nothing -> putStrLn "Timeout occurred"
        Left err -> putStrLn $ "Error: " ++ show err
      threadDelay 500000 -- 500ms between receive attempts

-- Test PAIR socket
testPair :: IO ()
testPair = do
  putStrLn "Testing receivesFor with PAIR socket"

  run Zmqx.defaultOptions do
    pair1 <- unwrap $ Zmqx.Pair.open $ Zmqx.name "pair1"
    unwrap $ Zmqx.bind pair1 "inproc://pair-test"

    -- Fork a thread to send messages
    void $ forkIO $ do
      threadDelay 1000000 -- 1s
      pair2 <- unwrap $ Zmqx.Pair.open $ Zmqx.name "pair2"
      unwrap $ Zmqx.connect pair2 "inproc://pair-test"
      forever $ do
        unwrap $ Zmqx.send pair2 (C.pack "Hello from PAIR")
        threadDelay 3000000 -- 3s

    -- Receive loop with timeout
    forever $ do
      putStrLn "Waiting for messages..."
      result <- Zmqx.receivesFor pair1 2000 -- 2s timeout
      case result of
        Right (Just msgs) -> putStrLn $ "Received: " ++ show msgs
        Right Nothing -> putStrLn "Timeout occurred"
        Left err -> putStrLn $ "Error: " ++ show err
      threadDelay 500000 -- 500ms between receive attempts

-- Test PULL socket
testPull :: IO ()
testPull = do
  putStrLn "Testing receivesFor with PULL socket"

  run Zmqx.defaultOptions do
    pull <- unwrap $ Zmqx.Pull.open $ Zmqx.name "pull"
    unwrap $ Zmqx.bind pull "tcp://127.0.0.1:5556"

    -- Fork a thread to send messages
    void $ forkIO $ do
      threadDelay 1000000 -- 1s
      push <- unwrap $ Zmqx.Push.open $ Zmqx.name "push"
      unwrap $ Zmqx.connect push "tcp://127.0.0.1:5556"
      forever $ do
        unwrap $ Zmqx.send push (C.pack "Hello from PUSH to PULL")
        threadDelay 3000000 -- 3s

    -- Receive loop with timeout
    forever $ do
      putStrLn "Waiting for messages..."
      result <- Zmqx.receivesFor pull 2000 -- 2s timeout
      case result of
        Right (Just msgs) -> putStrLn $ "Received: " ++ show msgs
        Right Nothing -> putStrLn "Timeout occurred"
        Left err -> putStrLn $ "Error: " ++ show err
      threadDelay 500000 -- 500ms between receive attempts

-- Test REP socket
testRep :: IO ()
testRep = do
  putStrLn "Testing receivesFor with REP socket"

  run Zmqx.defaultOptions do
    rep <- unwrap $ Zmqx.Rep.open $ Zmqx.name "rep"
    unwrap $ Zmqx.bind rep "tcp://127.0.0.1:5557"

    -- Fork a thread to send messages
    void $ forkIO $ do
      threadDelay 1000000 -- 1s
      forever $ do
        req <- unwrap $ Zmqx.Req.open $ Zmqx.name "req"
        unwrap $ Zmqx.connect req "tcp://127.0.0.1:5557"
        unwrap $ Zmqx.send req (C.pack "Hello from REQ")
        -- Wait for reply
        result <- Zmqx.receivesFor req 1000
        case result of
          Right (Just _) -> putStrLn "REQ received reply"
          _ -> return ()
        threadDelay 3000000 -- 3s

    -- Receive loop with timeout
    forever $ do
      putStrLn "Waiting for messages..."
      result <- Zmqx.receivesFor rep 2000 -- 2s timeout
      case result of
        Right (Just msgs) -> do
          putStrLn $ "Received: " ++ show msgs
          unwrap $ Zmqx.send rep (C.pack "Reply from REP")
        Right Nothing -> putStrLn "Timeout occurred"
        Left err -> putStrLn $ "Error: " ++ show err
      threadDelay 500000 -- 500ms between receive attempts

-- Test REQ socket
testReq :: IO ()
testReq = do
  putStrLn "Testing receivesFor with REQ socket"

  run Zmqx.defaultOptions do
    -- Fork a thread to handle REP
    void $ forkIO $ do
      rep <- unwrap $ Zmqx.Rep.open $ Zmqx.name "rep"
      unwrap $ Zmqx.bind rep "tcp://127.0.0.1:5558"
      forever $ do
        result <- Zmqx.receivesFor rep 3000
        case result of
          Right (Just msgs) -> do
            putStrLn $ "REP received: " ++ show msgs
            unwrap $ Zmqx.send rep (C.pack "Reply from REP")
          _ -> return ()

    -- Give REP time to start
    threadDelay 1000000 -- 1s
    req <- unwrap $ Zmqx.Req.open $ Zmqx.name "req"
    unwrap $ Zmqx.connect req "tcp://127.0.0.1:5558"

    -- Send and receive loop with timeout
    forever $ do
      putStrLn "Sending request..."
      unwrap $ Zmqx.send req (C.pack "Hello from REQ")

      putStrLn "Waiting for reply..."
      result <- Zmqx.receivesFor req 2000 -- 2s timeout
      case result of
        Right (Just msgs) -> putStrLn $ "REQ received reply: " ++ show msgs
        Right Nothing -> putStrLn "Timeout occurred"
        Left err -> putStrLn $ "Error: " ++ show err

      threadDelay 3000000 -- 3s between request cycles

-- Test ROUTER socket
testRouter :: IO ()
testRouter = do
  putStrLn "Testing receivesFor with ROUTER socket"

  run Zmqx.defaultOptions do
    router <- unwrap $ Zmqx.Router.open $ Zmqx.name "router"
    unwrap $ Zmqx.bind router "tcp://127.0.0.1:5559"

    -- Fork a thread to send messages
    void $ forkIO $ do
      threadDelay 1000000 -- 1s
      dealer <- unwrap $ Zmqx.Dealer.open $ Zmqx.name "dealer"
      unwrap $ Zmqx.connect dealer "tcp://127.0.0.1:5559"
      forever $ do
        unwrap $ Zmqx.send dealer (C.pack "Hello from DEALER to ROUTER")
        threadDelay 3000000 -- 3s

    -- Receive loop with timeout
    forever $ do
      putStrLn "Waiting for messages..."
      result <- Zmqx.receivesFor router 2000 -- 2s timeout
      case result of
        Right (Just msgs) -> putStrLn $ "Received: " ++ show msgs
        Right Nothing -> putStrLn "Timeout occurred"
        Left err -> putStrLn $ "Error: " ++ show err
      threadDelay 500000 -- 500ms between receive attempts

-- Test SUB socket
testSub :: IO ()
testSub = do
  putStrLn "Testing receivesFor with SUB socket"

  run Zmqx.defaultOptions do
    -- Fork a thread to publish messages
    void $ forkIO $ do
      pub <- unwrap $ Zmqx.Pub.open $ Zmqx.name "pub"
      unwrap $ Zmqx.bind pub "tcp://127.0.0.1:5560"
      threadDelay 1000000 -- 1s to establish connection
      forever $ do
        unwrap $ Zmqx.send pub (C.pack "Hello from PUB")
        threadDelay 1000000 -- 1s between publishes

    -- Give PUB time to start
    threadDelay 2000000 -- 2s
    sub <- unwrap $ Zmqx.Sub.open $ Zmqx.name "sub"
    unwrap $ Zmqx.connect sub "tcp://127.0.0.1:5560"
    -- setSocketOpt sub (Subscribe "")
    unwrap $ Zmqx.Sub.subscribe sub (C.pack "")

    -- Receive loop with timeout
    forever $ do
      putStrLn "Waiting for messages..."
      result <- Zmqx.receivesFor sub 2000 -- 2s timeout
      case result of
        Right (Just msgs) -> putStrLn $ "Received: " ++ show msgs
        Right Nothing -> putStrLn "Timeout occurred"
        Left err -> putStrLn $ "Error: " ++ show err
      threadDelay 500000 -- 500ms between receive attempts

-- Test XPUB socket
testXPub :: IO ()
testXPub = do
  putStrLn "Testing receivesFor with XPUB socket"

  run Zmqx.defaultOptions do
    xpub <- unwrap $ Zmqx.XPub.open $ Zmqx.name "xpub"
    unwrap $ Zmqx.bind xpub "tcp://127.0.0.1:5561"

    -- Fork a thread to send subscription
    void $ forkIO $ do
      threadDelay 1000000 -- 1s
      forever $ do
        xsub <- unwrap $ Zmqx.XSub.open $ Zmqx.name "xsub"
        unwrap $ Zmqx.connect xsub "tcp://127.0.0.1:5561"
        unwrap $ Zmqx.send xsub (Subscribe "topic")
        threadDelay 3000000 -- 3s

    -- Receive loop with timeout
    forever $ do
      putStrLn "Waiting for subscriptions..."
      result <- Zmqx.receivesFor xpub 2000 -- 2s timeout
      case result of
        Right (Just msgs) -> putStrLn $ "Received subscription: " ++ show msgs
        Right Nothing -> putStrLn "Timeout occurred"
        Left err -> putStrLn $ "Error: " ++ show err
      threadDelay 500000 -- 500ms between receive attempts

-- Test XSUB socket
testXSub :: IO ()
testXSub = do
  putStrLn "Testing receivesFor with XSUB socket"

  run Zmqx.defaultOptions do
    -- Fork a thread to publish
    void $ forkIO $ do
      xpub <- unwrap $ Zmqx.XPub.open $ Zmqx.name "xpub"
      unwrap $ Zmqx.bind xpub "tcp://127.0.0.1:5562"
      threadDelay 1000000 -- 1s to establish connection
      forever $ do
        unwrap $ Zmqx.send xpub (C.pack "Hello from XPUB")
        threadDelay 1000000 -- 1s between publishes

    -- Give XPUB time to start
    threadDelay 2000000 -- 2s
    xsub <- unwrap $ Zmqx.XSub.open $ Zmqx.name "xsub"
    unwrap $ Zmqx.connect xsub "tcp://127.0.0.1:5562"

    -- Receive loop with timeout
    forever $ do
      putStrLn "Waiting for messages..."
      result <- Zmqx.receivesFor xsub 2000 -- 2s timeout
      case result of
        Right (Just msgs) -> putStrLn $ "Received: " ++ show msgs
        Right Nothing -> putStrLn "Timeout occurred"
        Left err -> putStrLn $ "Error: " ++ show err
      threadDelay 500000 -- 500ms between receive attempts

-- Main function that reads arguments to decide which test to run
main :: IO ()
main = do
  args <- getArgs

  when (null args) $ do
    hPutStrLn stderr "Usage: ReceivesFor <socket-type>"
    hPutStrLn stderr "Available socket types: DEALER, PAIR, PULL, REP, REQ, ROUTER, SUB, XPUB, XSUB"
    exitFailure

  socketType <-
    getArgs <&> \case
      [] -> error ""
      st : _ -> st

  case socketType of
    "DEALER" -> testDealer
    "PAIR" -> testPair
    "PULL" -> testPull
    "REP" -> testRep
    "REQ" -> testReq
    "ROUTER" -> testRouter
    "SUB" -> testSub
    "XPUB" -> testXPub
    "XSUB" -> testXSub
    _ -> do
      hPutStrLn stderr $ "Unknown socket type: " ++ socketType
      hPutStrLn stderr "Available socket types: DEALER, PAIR, PULL, REP, REQ, ROUTER, SUB, XPUB, XSUB"
      exitFailure
