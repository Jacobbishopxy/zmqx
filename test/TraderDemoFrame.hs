{-# LANGUAGE OverloadedStrings #-}

-- | OMS-like EventLoop demo inspired by Conflux's trader_demo_frame.cc.
--
-- Run the finite smoke scenario:
--
-- > cabal test test-trader-demo-frame --flag demo-tests
--
-- Or run the long-lived roles in separate terminals:
--
-- > cabal run test:test-trader-demo-frame --flag demo-tests -- broker
-- > cabal run test:test-trader-demo-frame --flag demo-tests -- trader
-- > cabal run test:test-trader-demo-frame --flag demo-tests -- client
module Main where

import Control.Concurrent (Chan, forkIO, newChan, readChan, threadDelay, writeChan)
import Control.Concurrent.MVar (MVar, modifyMVar, newEmptyMVar, newMVar, readMVar, tryPutMVar)
import Control.Exception (throwIO)
import Control.Monad (forever, void, when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.Timeout qualified as Timeout
import Text.Read (readMaybe)
import Zmqx qualified
import Zmqx.Dealer qualified as Dealer
import Zmqx.EventLoop qualified as EventLoop
import Zmqx.Pub qualified as Pub
import Zmqx.Router qualified as Router
import Zmqx.Sub qualified as Sub

addrFrt :: Text
addrFrt =
  "tcp://127.0.0.1:5651"

addrBck :: Text
addrBck =
  "tcp://127.0.0.1:5652"

addrPub :: Text
addrPub =
  "tcp://127.0.0.1:5653"

addrSub :: Text
addrSub =
  "tcp://127.0.0.1:5654"

data DemoEndpoints = DemoEndpoints
  { endpointFrontend :: !Text,
    endpointBackend :: !Text,
    endpointBroadcastOut :: !Text,
    endpointBroadcastIn :: !Text
  }

staticEndpoints :: DemoEndpoints
staticEndpoints =
  DemoEndpoints
    { endpointFrontend = addrFrt,
      endpointBackend = addrBck,
      endpointBroadcastOut = addrPub,
      endpointBroadcastIn = addrSub
    }

uniqueEndpoints :: IO DemoEndpoints
uniqueEndpoints = do
  nonce <- getMonotonicTimeNSec
  let base suffix =
        "inproc://trader-demo-frame-" <> suffix <> "-" <> Text.pack (show nonce)
  pure
    DemoEndpoints
      { endpointFrontend = base "front",
        endpointBackend = base "back",
        endpointBroadcastOut = base "pub",
        endpointBroadcastIn = base "sub"
      }

data SideType
  = Buy
  | Sell
  deriving stock (Eq, Read, Show)

data CommandMessageType
  = OrderAlgo
  deriving stock (Eq, Read, Show)

data BroadcastMessageType
  = OrderReport
  deriving stock (Eq, Read, Show)

data ExecStatus
  = Succeed
  | Rejected
  deriving stock (Eq, Read, Show)

data OrderStatus
  = PartialFill
  | Filled
  deriving stock (Eq, Read, Show)

data CancelType
  = NotCanceled
  | Canceled
  deriving stock (Eq, Read, Show)

data QOrderAlgo = QOrderAlgo
  { orderExternalId :: !String,
    orderAccount :: !String,
    orderBasketId :: !String,
    orderSymbol :: !String,
    orderSide :: !SideType,
    orderQty :: !Int,
    orderOrdType :: !Int,
    orderEffTime :: !String,
    orderExpTime :: !String,
    orderLimAction :: !Int,
    orderAftAction :: !Int,
    orderAlgoParam :: !String,
    orderText :: !String
  }
  deriving stock (Eq, Read, Show)

data MetaCommandInfo = MetaCommandInfo
  { commandSeriesId :: !Integer,
    commandMessageType :: !CommandMessageType,
    commandHashValue :: !Int
  }
  deriving stock (Eq, Read, Show)

data QAck = QAck
  { ackSourceId :: !String,
    ackOriginSeriesId :: !Integer,
    ackReceiveTime :: !Integer
  }
  deriving stock (Eq, Read, Show)

data QExecReport = QExecReport
  { execSourceId :: !String,
    execOriginSeriesId :: !Integer,
    execTime :: !Integer,
    execStatus :: !ExecStatus,
    execText :: !String
  }
  deriving stock (Eq, Read, Show)

data QOrderReport = QOrderReport
  { reportExternalId :: !String,
    reportAccount :: !String,
    reportSymbol :: !String,
    reportSide :: !SideType,
    reportTransactTime :: !Integer,
    reportOrderQty :: !Int,
    reportOrdType :: !Int,
    reportEffTime :: !String,
    reportExpTime :: !String,
    reportLimAction :: !Int,
    reportAftAction :: !Int,
    reportAlgoParam :: !String,
    reportExecId :: !String,
    reportCumQty :: !Int,
    reportLeavesQty :: !Int,
    reportOutstandingQty :: !Int,
    reportAvgPx :: !Double,
    reportOrdStatus :: !OrderStatus,
    reportCancelType :: !CancelType,
    reportBasketId :: !String,
    reportLastUpdateTime :: !Integer,
    reportText :: !String
  }
  deriving stock (Eq, Read, Show)

data MetaBroadcastInfo = MetaBroadcastInfo
  { broadcastSeriesId :: !Integer,
    broadcastMessageType :: !BroadcastMessageType,
    broadcastHashValue :: !Int
  }
  deriving stock (Eq, Read, Show)

data Routed = Routed
  { routedEndpoint :: !Text,
    routedFrames :: ![ByteString]
  }

data TraderState = TraderState
  { traderLatestOrder :: !QOrderAlgo,
    traderLatestSeries :: !Integer,
    traderBroadcastCount :: !Integer
  }

require :: Bool -> String -> IO ()
require condition message =
  when (not condition) (throwIO (userError message))

unwrap :: IO (Either Zmqx.Error a) -> IO a
unwrap action =
  action >>= either throwIO pure

frame :: Show a => a -> ByteString
frame =
  ByteString.Char8.pack . show

fromFrame :: Read a => ByteString -> IO a
fromFrame raw =
  case readMaybe (ByteString.Char8.unpack raw) of
    Nothing -> throwIO (userError ("could not decode frame: " <> ByteString.Char8.unpack raw))
    Just value -> pure value

textFrame :: Text -> ByteString
textFrame =
  ByteString.Char8.pack . Text.unpack

frameText :: ByteString -> Text
frameText =
  Text.pack . ByteString.Char8.unpack

frameString :: ByteString -> String
frameString =
  ByteString.Char8.unpack

mockHash :: ByteString -> Int
mockHash bytes =
  ByteString.Char8.length bytes * 131

makeOrderAlgo :: Integer -> QOrderAlgo
makeOrderAlgo seriesId =
  QOrderAlgo
    { orderExternalId = "EXT-FRAME-" <> show seriesId,
      orderAccount = "ACCT-001",
      orderBasketId = "BASKET-VWAP-0428",
      orderSymbol = "600000.SH",
      orderSide = Buy,
      orderQty = 12000,
      orderOrdType = 2,
      orderEffTime = "09:30:00",
      orderExpTime = "14:55:00",
      orderLimAction = 1,
      orderAftAction = 0,
      orderAlgoParam = "algo=VWAP;participation=0.12;min_slice=500",
      orderText = "frame demo order " <> show seriesId
    }

makeOrderMeta :: Integer -> QOrderAlgo -> MetaCommandInfo
makeOrderMeta seriesId order =
  MetaCommandInfo
    { commandSeriesId = seriesId,
      commandMessageType = OrderAlgo,
      commandHashValue = mockHash (frame order)
    }

makeAck :: Text -> Integer -> QAck
makeAck traderId originSeriesId =
  QAck
    { ackSourceId = Text.unpack traderId,
      ackOriginSeriesId = originSeriesId,
      ackReceiveTime = 1_777_333_010_000 + originSeriesId
    }

makeExecReport :: Integer -> QExecReport
makeExecReport originSeriesId =
  QExecReport
    { execSourceId = "sim-exchange-gateway",
      execOriginSeriesId = originSeriesId,
      execTime = 1_777_333_001_234,
      execStatus = Succeed,
      execText = "exchange accepted order"
    }

makeOrderReport :: QOrderAlgo -> QOrderReport
makeOrderReport order =
  QOrderReport
    { reportExternalId = orderExternalId order,
      reportAccount = orderAccount order,
      reportSymbol = orderSymbol order,
      reportSide = orderSide order,
      reportTransactTime = 1_777_333_001_500,
      reportOrderQty = orderQty order,
      reportOrdType = orderOrdType order,
      reportEffTime = orderEffTime order,
      reportExpTime = orderExpTime order,
      reportLimAction = orderLimAction order,
      reportAftAction = orderAftAction order,
      reportAlgoParam = orderAlgoParam order,
      reportExecId = "EXEC-00000042",
      reportCumQty = 3000,
      reportLeavesQty = 9000,
      reportOutstandingQty = 9000,
      reportAvgPx = 10.25,
      reportOrdStatus = PartialFill,
      reportCancelType = NotCanceled,
      reportBasketId = orderBasketId order,
      reportLastUpdateTime = 1_777_333_002_000,
      reportText = "partial fill from simulated venue"
    }

makeOrderReportMeta :: Integer -> QOrderReport -> MetaBroadcastInfo
makeOrderReportMeta seriesId report =
  MetaBroadcastInfo
    { broadcastSeriesId = seriesId,
      broadcastMessageType = OrderReport,
      broadcastHashValue = mockHash (frame report)
    }

makeClientCommandPacket :: Text -> Integer -> QOrderAlgo -> [ByteString]
makeClientCommandPacket traderId seriesId order =
  [ textFrame traderId,
    frame (makeOrderMeta seriesId order),
    frame order
  ]

initialTraderState :: TraderState
initialTraderState =
  TraderState
    { traderLatestOrder = makeOrderAlgo 0,
      traderLatestSeries = 0,
      traderBroadcastCount = 0
    }

waitSignal :: String -> MVar a -> IO a
waitSignal label signal =
  Timeout.timeout 3_000_000 (readMVar signal) >>= \case
    Nothing -> throwIO (userError (label <> " did not arrive before timeout"))
    Just value -> pure value

signalOnce :: MVar a -> a -> IO ()
signalOnce signal value =
  void (tryPutMVar signal value)

pumpSends :: EventLoop.EventLoop -> Chan Routed -> IO ()
pumpSends loop outgoing =
  forever do
    Routed endpoint frames <- readChan outgoing
    unwrap (EventLoop.sends loop endpoint frames)

brokerSpec :: Chan Routed -> Router.Router -> Router.Router -> Sub.Sub -> Pub.Pub -> Zmqx.EventLoopSpec
brokerSpec outgoing front back broadcastIn broadcastOut =
  Zmqx.addSender "broadcast-out" broadcastOut $
    Zmqx.addReceiver "broadcast-in" broadcastIn (EventLoop.Callback handleBroadcast) $
      Zmqx.addTransceiver "front" front (EventLoop.Callback handleFront) $
        Zmqx.addTransceiver "back" back (EventLoop.Callback handleBack) Zmqx.emptySpec
  where
    handleFront = \case
      clientId : targetTrader : payload -> do
        putStrLn ("broker: command client=" <> frameString clientId <> " target=" <> frameString targetTrader)
        writeChan outgoing (Routed "back" (targetTrader : clientId : payload))
      frames -> putStrLn ("broker: unexpected front packet " <> show (length frames))

    handleBack = \case
      traderId : clientId : payload -> do
        putStrLn ("broker: reply trader=" <> frameString traderId <> " target=" <> frameString clientId)
        writeChan outgoing (Routed "front" (clientId : traderId : payload))
      frames -> putStrLn ("broker: unexpected back packet " <> show (length frames))

    handleBroadcast frames@(topic : _) = do
      putStrLn ("broker: broadcast topic=" <> frameString topic)
      writeChan outgoing (Routed "broadcast-out" frames)
    handleBroadcast [] = putStrLn "broker: empty broadcast packet"

traderSpec :: MVar TraderState -> Chan Routed -> Dealer.Dealer -> Pub.Pub -> Zmqx.EventLoopSpec
traderSpec traderState outgoing traderDealer traderPub =
  Zmqx.addSender "trader-pub" traderPub $
    Zmqx.addTransceiver "trader-it" traderDealer (EventLoop.Callback handleOrder) Zmqx.emptySpec
  where
    traderId = "worker1"

    handleOrder = \case
      clientId : metaFrame : orderFrame : [] -> do
        meta <- fromFrame metaFrame
        order <- fromFrame orderFrame
        putStrLn $
          "trader: order series="
            <> show (commandSeriesId meta)
            <> " client="
            <> frameString clientId
            <> " symbol="
            <> orderSymbol order
            <> " qty="
            <> show (orderQty order)

        _ <-
          modifyMVar traderState \state ->
            pure
              ( state
                  { traderLatestOrder = order,
                    traderLatestSeries = commandSeriesId meta
                  },
                ()
              )

        writeChan outgoing (Routed "trader-it" [clientId, frame (makeAck traderId (commandSeriesId meta))])
        putStrLn ("trader: ack queued origin_series=" <> show (commandSeriesId meta))
      frames -> putStrLn ("trader: unexpected command packet " <> show (length frames))

publishTraderBroadcasts :: Int -> Maybe (MVar ()) -> Chan Routed -> MVar TraderState -> IO ()
publishTraderBroadcasts intervalUs maybeBroadcastSeen outgoing traderState =
  forever do
    threadDelay intervalUs
    (topic, reportMeta, report) <-
      modifyMVar traderState \state -> do
        let count = traderBroadcastCount state + 1
            order = traderLatestOrder state
            report = makeOrderReport order
            reportMeta = makeOrderReportMeta (traderLatestSeries state + 1000 + count) report
            topic = Text.pack ("order." <> orderAccount order <> "." <> orderSymbol order)
        pure
          ( state {traderBroadcastCount = count},
            (topic, reportMeta, report)
          )
    writeChan outgoing (Routed "trader-pub" [textFrame topic, frame reportMeta, frame report])
    maybe (pure ()) (`signalOnce` ()) maybeBroadcastSeen
    putStrLn ("trader: periodic broadcast queued topic=" <> Text.unpack topic)

clientSpec :: Maybe (MVar QAck) -> Maybe (MVar QOrderReport) -> Dealer.Dealer -> Sub.Sub -> Zmqx.EventLoopSpec
clientSpec maybeAckSeen maybeReportSeen clientDealer clientSub =
  Zmqx.addReceiver "client-sub" clientSub (EventLoop.Callback handleBroadcast) $
    Zmqx.addTransceiver "client-it" clientDealer (EventLoop.Callback handleAck) Zmqx.emptySpec
  where
    handleAck = \case
      traderId : ackFrame : [] -> do
        ack <- fromFrame ackFrame
        putStrLn $
          "client: ack from="
            <> frameString traderId
            <> " source="
            <> ackSourceId ack
            <> " origin_series="
            <> show (ackOriginSeriesId ack)
        maybe (pure ()) (`signalOnce` ack) maybeAckSeen
      frames -> putStrLn ("client: unexpected ack packet " <> show (length frames))

    handleBroadcast = \case
      topic : metaFrame : reportFrame : [] -> do
        meta <- fromFrame metaFrame
        report <- fromFrame reportFrame
        putStrLn $
          "client: broadcast topic="
            <> frameString topic
            <> " series="
            <> show (broadcastSeriesId meta)
            <> " exec_id="
            <> reportExecId report
            <> " cum_qty="
            <> show (reportCumQty report)
            <> " leaves_qty="
            <> show (reportLeavesQty report)
        maybe (pure ()) (`signalOnce` report) maybeReportSeen
      frames -> putStrLn ("client: unexpected broadcast packet " <> show (length frames))

openBroker :: DemoEndpoints -> IO (Router.Router, Router.Router, Sub.Sub, Pub.Pub)
openBroker DemoEndpoints {endpointFrontend, endpointBackend, endpointBroadcastOut, endpointBroadcastIn} = do
  front <- unwrap (Router.open (Router.defaultOptions <> Zmqx.name "broker-front"))
  back <- unwrap (Router.open (Router.defaultOptions <> Zmqx.name "broker-back"))
  broadcastIn <- unwrap (Sub.open (Sub.defaultOptions <> Zmqx.name "broker-broadcast-in"))
  broadcastOut <- unwrap (Pub.open (Pub.defaultOptions <> Zmqx.name "broker-broadcast-out"))
  unwrap (Router.bind front endpointFrontend)
  unwrap (Router.bind back endpointBackend)
  unwrap (Sub.subscribe broadcastIn "")
  unwrap (Sub.bind broadcastIn endpointBroadcastIn)
  unwrap (Pub.bind broadcastOut endpointBroadcastOut)
  pure (front, back, broadcastIn, broadcastOut)

openTrader :: DemoEndpoints -> IO (Dealer.Dealer, Pub.Pub)
openTrader DemoEndpoints {endpointBackend, endpointBroadcastIn} = do
  traderDealer <- unwrap (Dealer.open (Dealer.defaultOptions <> Zmqx.name "trader-it"))
  Zmqx.setSocketOpt traderDealer (Zmqx.Z_RoutingId "worker1")
  traderPub <- unwrap (Pub.open (Pub.defaultOptions <> Zmqx.name "trader-pub"))
  unwrap (Dealer.connect traderDealer endpointBackend)
  unwrap (Pub.connect traderPub endpointBroadcastIn)
  pure (traderDealer, traderPub)

openClient :: DemoEndpoints -> IO (Dealer.Dealer, Sub.Sub)
openClient DemoEndpoints {endpointFrontend, endpointBroadcastOut} = do
  clientDealer <- unwrap (Dealer.open (Dealer.defaultOptions <> Zmqx.name "client-it"))
  Zmqx.setSocketOpt clientDealer (Zmqx.Z_RoutingId "frame-client")
  clientSub <- unwrap (Sub.open (Sub.defaultOptions <> Zmqx.name "client-sub"))
  unwrap (Sub.subscribe clientSub "")
  unwrap (Dealer.connect clientDealer endpointFrontend)
  unwrap (Sub.connect clientSub endpointBroadcastOut)
  pure (clientDealer, clientSub)

withBroker :: DemoEndpoints -> (EventLoop.EventLoop -> IO a) -> IO a
withBroker endpoints action = do
  (front, back, broadcastIn, broadcastOut) <- openBroker endpoints
  outgoing <- newChan
  Zmqx.withEventLoop (brokerSpec outgoing front back broadcastIn broadcastOut) \loop -> do
    void (forkIO (pumpSends loop outgoing))
    action loop

withTrader :: Int -> Maybe (MVar ()) -> DemoEndpoints -> (EventLoop.EventLoop -> IO a) -> IO a
withTrader broadcastIntervalUs maybeBroadcastSeen endpoints action = do
  (traderDealer, traderPub) <- openTrader endpoints
  outgoing <- newChan
  traderState <- newMVar initialTraderState
  Zmqx.withEventLoop (traderSpec traderState outgoing traderDealer traderPub) \loop -> do
    void (forkIO (pumpSends loop outgoing))
    void (forkIO (publishTraderBroadcasts broadcastIntervalUs maybeBroadcastSeen outgoing traderState))
    action loop

withClient :: Maybe (MVar QAck) -> Maybe (MVar QOrderReport) -> DemoEndpoints -> (EventLoop.EventLoop -> IO a) -> IO a
withClient maybeAckSeen maybeReportSeen endpoints action = do
  (clientDealer, clientSub) <- openClient endpoints
  Zmqx.withEventLoop (clientSpec maybeAckSeen maybeReportSeen clientDealer clientSub) action

smokeTest :: IO ()
smokeTest =
  Zmqx.run Zmqx.defaultOptions do
    endpoints <- uniqueEndpoints
    ackSeen <- newEmptyMVar
    reportSeen <- newEmptyMVar
    broadcastQueued <- newEmptyMVar
    withBroker endpoints \_brokerLoop ->
      withTrader 300_000 (Just broadcastQueued) endpoints \_traderLoop ->
        withClient (Just ackSeen) (Just reportSeen) endpoints \clientLoop -> do
          -- Give DEALER/ROUTER and PUB/SUB handshakes time to settle. The
          -- trader publishes periodically even before the client sends an
          -- order, and the client subscription callback should observe that
          -- broadcast independently of its command path.
          threadDelay 1_000_000
          _ <- waitSignal "trader periodic broadcast queue" broadcastQueued
          report <- waitSignal "client periodic broadcast" reportSeen
          require (reportExecId report == "EXEC-00000042") "periodic broadcast exec_id mismatch"
          require (reportCumQty report == 3000) "periodic broadcast cum_qty mismatch"

          let seriesId = 1
              order = makeOrderAlgo seriesId
          unwrap (EventLoop.sends clientLoop "client-it" (makeClientCommandPacket "worker1" seriesId order))
          ack <- waitSignal "client ack" ackSeen
          require (ackOriginSeriesId ack == seriesId) "ack origin_series_id mismatch"
          putStrLn "trader_demo_frame smoke passed"

runBroker :: IO ()
runBroker =
  Zmqx.run Zmqx.defaultOptions do
    withBroker staticEndpoints \_loop -> do
      putStrLn "frame broker running"
      forever (threadDelay 60_000_000)

runTrader :: IO ()
runTrader =
  Zmqx.run Zmqx.defaultOptions do
    withTrader 5_000_000 Nothing staticEndpoints \_loop -> do
      putStrLn "frame trader worker1 running; periodic broadcasts every 5s"
      forever (threadDelay 60_000_000)

runClient :: IO ()
runClient =
  Zmqx.run Zmqx.defaultOptions do
    withClient Nothing Nothing staticEndpoints \clientLoop -> do
      threadDelay 1_000_000
      putStrLn "frame client running; subscription receiver is always active"
      void (forkIO (clientOrderLoop clientLoop 1))
      forever (threadDelay 60_000_000)

clientOrderLoop :: EventLoop.EventLoop -> Integer -> IO ()
clientOrderLoop clientLoop seriesId = do
  let order = makeOrderAlgo seriesId
  putStrLn ("client: sending order series=" <> show seriesId <> " symbol=" <> orderSymbol order)
  unwrap (EventLoop.sends clientLoop "client-it" (makeClientCommandPacket "worker1" seriesId order))
  threadDelay 3_000_000
  clientOrderLoop clientLoop (seriesId + 1)

printUsage :: IO ()
printUsage =
  putStrLn "Usage: trader-demo-frame <smoke|broker|trader|client>"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case args of
    [] -> smokeTest
    ["smoke"] -> smokeTest
    ["broker"] -> runBroker
    ["trader"] -> runTrader
    ["client"] -> runClient
    _ -> printUsage *> throwIO (userError "invalid mode")
