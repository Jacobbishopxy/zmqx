module Main where

import Bench.Helpers
  ( Summary (..),
    awaitConnection,
    benchmarkMetadata,
    latencySummary,
    payload,
    payloadFrames,
    renderSummary,
    timeAction,
    uniqueEndpoint,
    unwrap,
    unwrapMaybe,
    warmup,
  )
import Control.Exception (throwIO)
import Control.Monad (forM, forM_, replicateM_, unless)
import Data.List qualified as List
import Data.Time.Clock (NominalDiffTime)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Zmqx qualified
import Zmqx.EventLoop qualified as EventLoop
import Zmqx.Pair qualified
import Zmqx.Pull qualified
import Zmqx.Push qualified
import Zmqx.Rep qualified
import Zmqx.Req qualified
import Zmqx.Router qualified


data Scenario
  = ScenarioAll
  | ScenarioDirect
  | ScenarioMultipart
  | ScenarioPoll
  | ScenarioReqPoll
  | ScenarioReqPollIdle
  | ScenarioEventLoop
  | ScenarioLifecycle
  deriving stock (Eq, Show)

data Cli = Cli
  { cliScenario :: !Scenario,
    cliMessages :: !Int,
    cliPayloadBytes :: !Int,
    cliFrames :: !Int,
    cliSockets :: !Int,
    cliWarmup :: !Int,
    cliTimeoutMs :: !Int
  }
  deriving stock (Eq, Show)

data Command
  = ShowHelp
  | Run !Cli
  deriving stock (Eq, Show)

data PollEndpoint = PollEndpoint
  { pollPull :: !Zmqx.Pull.Pull,
    pollPush :: !Zmqx.Push.Push
  }

defaultCli :: Cli
defaultCli =
  Cli
    { cliScenario = ScenarioAll,
      cliMessages = 1000,
      cliPayloadBytes = 64,
      cliFrames = 3,
      cliSockets = 4,
      cliWarmup = 10,
      cliTimeoutMs = 1000
    }

main :: IO ()
main = do
  command <- parseArgs <$> getArgs
  case command of
    Left message -> do
      hPutStrLn stderr message
      hPutStrLn stderr "Run `cabal run zmqx-overheads -- --help` for usage."
      exitFailure
    Right ShowHelp -> putStr helpText
    Right (Run cli) -> runBenchmarks cli

parseArgs :: [String] -> Either String Command
parseArgs =
  parse defaultCli
  where
    parse cli = \case
      [] -> Right (Run cli)
      "--help" : _ -> Right ShowHelp
      "-h" : _ -> Right ShowHelp
      arg : args
        | Just value <- List.stripPrefix "--scenario=" arg -> do
            scenario <- parseScenario value
            parse cli {cliScenario = scenario} args
        | Just value <- List.stripPrefix "--messages=" arg -> do
            messages <- readPositive "--messages" value
            parse cli {cliMessages = messages} args
        | Just value <- List.stripPrefix "--payload-bytes=" arg -> do
            payloadBytes <- readNonNegative "--payload-bytes" value
            parse cli {cliPayloadBytes = payloadBytes} args
        | Just value <- List.stripPrefix "--frames=" arg -> do
            frames <- readPositive "--frames" value
            parse cli {cliFrames = frames} args
        | Just value <- List.stripPrefix "--sockets=" arg -> do
            sockets <- readPositive "--sockets" value
            parse cli {cliSockets = sockets} args
        | Just value <- List.stripPrefix "--warmup=" arg -> do
            warmupCount <- readNonNegative "--warmup" value
            parse cli {cliWarmup = warmupCount} args
        | Just value <- List.stripPrefix "--timeout-ms=" arg -> do
            timeoutMs <- readNonNegative "--timeout-ms" value
            parse cli {cliTimeoutMs = timeoutMs} args
        | otherwise ->
            case (arg, args) of
              ("--scenario", value : rest) -> do
                scenario <- parseScenario value
                parse cli {cliScenario = scenario} rest
              ("--messages", value : rest) -> do
                messages <- readPositive "--messages" value
                parse cli {cliMessages = messages} rest
              ("--payload-bytes", value : rest) -> do
                payloadBytes <- readNonNegative "--payload-bytes" value
                parse cli {cliPayloadBytes = payloadBytes} rest
              ("--frames", value : rest) -> do
                frames <- readPositive "--frames" value
                parse cli {cliFrames = frames} rest
              ("--sockets", value : rest) -> do
                sockets <- readPositive "--sockets" value
                parse cli {cliSockets = sockets} rest
              ("--warmup", value : rest) -> do
                warmupCount <- readNonNegative "--warmup" value
                parse cli {cliWarmup = warmupCount} rest
              ("--timeout-ms", value : rest) -> do
                timeoutMs <- readNonNegative "--timeout-ms" value
                parse cli {cliTimeoutMs = timeoutMs} rest
              (knownOption, [])
                | knownOption `elem` optionsRequiringValues -> Left (knownOption <> " requires a value")
              _ -> Left ("unknown argument: " <> arg)

optionsRequiringValues :: [String]
optionsRequiringValues =
  [ "--scenario",
    "--messages",
    "--payload-bytes",
    "--frames",
    "--sockets",
    "--warmup",
    "--timeout-ms"
  ]

parseScenario :: String -> Either String Scenario
parseScenario = \case
  "all" -> Right ScenarioAll
  "direct" -> Right ScenarioDirect
  "multipart" -> Right ScenarioMultipart
  "poll" -> Right ScenarioPoll
  "req-poll" -> Right ScenarioReqPoll
  "req-poll-idle" -> Right ScenarioReqPollIdle
  "event-loop" -> Right ScenarioEventLoop
  "lifecycle" -> Right ScenarioLifecycle
  other -> Left ("unknown scenario: " <> other)

readPositive :: String -> String -> Either String Int
readPositive option raw = do
  value <- readInt option raw
  if value > 0
    then Right value
    else Left (option <> " must be positive")

readNonNegative :: String -> String -> Either String Int
readNonNegative option raw = do
  value <- readInt option raw
  if value >= 0
    then Right value
    else Left (option <> " must be non-negative")

readInt :: String -> String -> Either String Int
readInt option raw =
  case reads raw of
    [(value, "")] -> Right value
    _ -> Left (option <> " expects an integer, got: " <> raw)

helpText :: String
helpText =
  unlines
    [ "zmqx-overheads: opt-in zmqx overhead benchmarks",
      "",
      "Usage:",
      "  cabal run zmqx-overheads -- [OPTIONS]",
      "",
      "Options:",
      "  --scenario NAME        all, direct, multipart, poll, req-poll, req-poll-idle, event-loop, lifecycle (default: all)",
      "  --messages N          Messages or lifecycle iterations per scenario (default: 1000)",
      "  --payload-bytes N     Payload bytes per frame (default: 64)",
      "  --frames N            Multipart frame count (default: 3)",
      "  --sockets N           Socket pairs for poll scaling (default: 4)",
      "  --warmup N            Warmup iterations before timing (default: 10)",
      "  --timeout-ms N        Poll/receive timeout in milliseconds (default: 1000)",
      "  -h, --help            Show this help text",
      "",
      "Examples:",
      "  cabal run zmqx-overheads -- --scenario direct --messages 100 --payload-bytes 64",
      "  cabal run zmqx-overheads -- --scenario poll --sockets 8 --messages 100",
      "  cabal run zmqx-overheads -- --scenario lifecycle --messages 1000 +RTS -s",
      "",
      "Output is one key=value summary per scenario. Use +RTS -s to capture allocation summaries."
    ]

runBenchmarks :: Cli -> IO ()
runBenchmarks cli = do
  metadata <- benchmarkMetadata
  summaries <- traverse (runScenario cli) (selectedScenarios (cliScenario cli))
  forM_ summaries (putStrLn . renderSummary metadata)

selectedScenarios :: Scenario -> [Scenario]
selectedScenarios = \case
  ScenarioAll ->
    [ ScenarioDirect,
      ScenarioMultipart,
      ScenarioPoll,
      ScenarioReqPoll,
      ScenarioEventLoop,
      ScenarioLifecycle
    ]
  scenario -> [scenario]

runScenario :: Cli -> Scenario -> IO Summary
runScenario cli = \case
  ScenarioAll -> throwIO (userError "ScenarioAll cannot be run directly")
  ScenarioDirect -> runDirect cli
  ScenarioMultipart -> runMultipart cli
  ScenarioPoll -> runPoll cli
  ScenarioReqPoll -> runReqPoll cli
  ScenarioReqPollIdle -> runReqPollIdle cli
  ScenarioEventLoop -> runEventLoop cli
  ScenarioLifecycle -> runLifecycle cli

measureLatencyLoop :: Int -> (Int -> IO ()) -> IO (NominalDiffTime, [NominalDiffTime])
measureLatencyLoop iterationCount action =
  timeAction (forM [0 .. iterationCount - 1] (\index -> fst <$> timeAction (action index)))

runDirect :: Cli -> IO Summary
runDirect Cli {..} =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "direct"
    server <- Zmqx.Pair.open Zmqx.Pair.defaultOptions >>= unwrap
    client <- Zmqx.Pair.open Zmqx.Pair.defaultOptions >>= unwrap
    Zmqx.bind server endpoint >>= unwrap
    Zmqx.connect client endpoint >>= unwrap
    awaitConnection

    let frame = payload cliPayloadBytes
        iteration _index = do
          Zmqx.send client frame >>= unwrap
          received <- Zmqx.receive server >>= unwrap
          unless (received == frame) (throwIO (userError "direct scenario received an unexpected payload"))

    warmup cliWarmup (iteration 0)
    (elapsed, latencies) <- measureLatencyLoop cliMessages iteration
    pure
      Summary
        { summaryScenario = "direct",
          summaryPayloadBytes = cliPayloadBytes,
          summaryFrames = 1,
          summarySockets = 2,
          summaryMessages = cliMessages,
          summaryElapsed = elapsed,
          summaryLatency = latencySummary latencies,
          summaryExtra = []
        }

runMultipart :: Cli -> IO Summary
runMultipart Cli {..} =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "multipart"
    server <- Zmqx.Pair.open Zmqx.Pair.defaultOptions >>= unwrap
    client <- Zmqx.Pair.open Zmqx.Pair.defaultOptions >>= unwrap
    Zmqx.bind server endpoint >>= unwrap
    Zmqx.connect client endpoint >>= unwrap
    awaitConnection

    let frames = payloadFrames cliFrames cliPayloadBytes
        iteration _index = do
          Zmqx.sends client frames >>= unwrap
          received <- Zmqx.receives server >>= unwrap
          unless (received == frames) (throwIO (userError "multipart scenario received unexpected frames"))

    warmup cliWarmup (iteration 0)
    (elapsed, latencies) <- measureLatencyLoop cliMessages iteration
    pure
      Summary
        { summaryScenario = "multipart",
          summaryPayloadBytes = cliPayloadBytes,
          summaryFrames = cliFrames,
          summarySockets = 2,
          summaryMessages = cliMessages,
          summaryElapsed = elapsed,
          summaryLatency = latencySummary latencies,
          summaryExtra = []
        }

runPoll :: Cli -> IO Summary
runPoll Cli {..} =
  Zmqx.run Zmqx.defaultOptions do
    endpoints <- forM [1 .. cliSockets] \index -> do
      endpoint <- uniqueEndpoint ("poll-" <> show index)
      pull <- Zmqx.Pull.open Zmqx.Pull.defaultOptions >>= unwrap
      push <- Zmqx.Push.open Zmqx.Push.defaultOptions >>= unwrap
      Zmqx.bind pull endpoint >>= unwrap
      Zmqx.connect push endpoint >>= unwrap
      pure PollEndpoint {pollPull = pull, pollPush = push}
    awaitConnection

    let frame = payload cliPayloadBytes
        pollSet = buildPollSet (map pollPull endpoints)
        endpointCount = length endpoints
        iteration index = do
          let selected = endpoints !! (index `mod` endpointCount)
          Zmqx.send (pollPush selected) frame >>= unwrap
          Zmqx.Ready isReady <- Zmqx.pollFor pollSet cliTimeoutMs >>= unwrapMaybe "poll scenario pollFor"
          unless (isReady (pollPull selected)) (throwIO (userError "poll scenario selected socket was not ready"))
          received <- Zmqx.receive (pollPull selected) >>= unwrap
          unless (received == frame) (throwIO (userError "poll scenario received an unexpected payload"))

    warmup cliWarmup (iteration 0)
    (elapsed, latencies) <- measureLatencyLoop cliMessages iteration
    pure
      Summary
        { summaryScenario = "poll",
          summaryPayloadBytes = cliPayloadBytes,
          summaryFrames = 1,
          summarySockets = cliSockets,
          summaryMessages = cliMessages,
          summaryElapsed = elapsed,
          summaryLatency = latencySummary latencies,
          summaryExtra = [("poll_timeout_ms", show cliTimeoutMs)]
        }

buildPollSet :: [Zmqx.Pull.Pull] -> Zmqx.Sockets
buildPollSet = \case
  [] -> error "buildPollSet requires at least one socket"
  firstPull : remainingPulls -> foldr Zmqx.pollInAlso (Zmqx.pollIn firstPull) remainingPulls

runReqPoll :: Cli -> IO Summary
runReqPoll Cli {..} =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "req-poll"
    rep <- Zmqx.Rep.open Zmqx.Rep.defaultOptions >>= unwrap
    req <- Zmqx.Req.open Zmqx.Req.defaultOptions >>= unwrap
    Zmqx.bind rep endpoint >>= unwrap
    Zmqx.connect req endpoint >>= unwrap
    awaitConnection

    let frame = payload cliPayloadBytes
        iteration _index = do
          Zmqx.send req frame >>= unwrap
          request <- Zmqx.receive rep >>= unwrap
          unless (request == frame) (throwIO (userError "REQ poll scenario REP received an unexpected request"))
          Zmqx.send rep frame >>= unwrap
          reply <- Zmqx.receivesFor req cliTimeoutMs >>= unwrapMaybe "REQ poll scenario receivesFor"
          unless (reply == [frame]) (throwIO (userError "REQ poll scenario received an unexpected reply"))

    warmup cliWarmup (iteration 0)
    (elapsed, latencies) <- measureLatencyLoop cliMessages iteration
    pure
      Summary
        { summaryScenario = "req-poll",
          summaryPayloadBytes = cliPayloadBytes,
          summaryFrames = 1,
          summarySockets = 2,
          summaryMessages = cliMessages,
          summaryElapsed = elapsed,
          summaryLatency = latencySummary latencies,
          summaryExtra = [("receive_timeout_ms", show cliTimeoutMs)]
        }

runReqPollIdle :: Cli -> IO Summary
runReqPollIdle Cli {..} =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "req-poll-idle"
    router <- Zmqx.Router.open Zmqx.Router.defaultOptions >>= unwrap
    req <- Zmqx.Req.open Zmqx.Req.defaultOptions >>= unwrap
    Zmqx.bind router endpoint >>= unwrap
    Zmqx.connect req endpoint >>= unwrap
    awaitConnection

    let frame = payload cliPayloadBytes
        iteration _index = do
          Zmqx.send req frame >>= unwrap
          routed <- Zmqx.receives router >>= unwrap
          unless (frame `elem` routed) (throwIO (userError "REQ idle poll scenario ROUTER received an unexpected request"))
          Zmqx.pollFor (Zmqx.pollIn req) cliTimeoutMs >>= \case
            Right Nothing -> pure ()
            Right (Just (Zmqx.Ready ready)) ->
              unless (not (ready req)) (throwIO (userError "REQ idle poll scenario unexpectedly reported the REQ ready"))
            Left err -> throwIO err

    warmup cliWarmup (iteration 0)
    (elapsed, latencies) <- measureLatencyLoop cliMessages iteration
    pure
      Summary
        { summaryScenario = "req-poll-idle",
          summaryPayloadBytes = cliPayloadBytes,
          summaryFrames = 1,
          summarySockets = 2,
          summaryMessages = cliMessages,
          summaryElapsed = elapsed,
          summaryLatency = latencySummary latencies,
          summaryExtra = [("poll_timeout_ms", show cliTimeoutMs)]
        }

runEventLoop :: Cli -> IO Summary
runEventLoop Cli {..} =
  Zmqx.run Zmqx.defaultOptions do
    endpoint <- uniqueEndpoint "event-loop"
    loopPair <- Zmqx.Pair.open Zmqx.Pair.defaultOptions >>= unwrap
    peerPair <- Zmqx.Pair.open Zmqx.Pair.defaultOptions >>= unwrap
    Zmqx.bind loopPair endpoint >>= unwrap
    Zmqx.connect peerPair endpoint >>= unwrap
    awaitConnection

    let frames = payloadFrames cliFrames cliPayloadBytes
        spec = Zmqx.addTransceiver "pair" loopPair (EventLoop.Mailbox (max 1 (min 1024 cliMessages))) Zmqx.emptySpec
        iteration loop _index = do
          Zmqx.sends peerPair frames >>= unwrap
          inbound <- EventLoop.recv loop "pair" cliTimeoutMs >>= unwrapMaybe "EventLoop inbound recv"
          unless (inbound == frames) (throwIO (userError "EventLoop scenario received unexpected inbound frames"))
          EventLoop.sends loop "pair" frames >>= unwrap
          outbound <- Zmqx.receivesFor peerPair cliTimeoutMs >>= unwrapMaybe "EventLoop outbound receive"
          unless (outbound == frames) (throwIO (userError "EventLoop scenario received unexpected outbound frames"))

    Zmqx.withEventLoop spec \loop -> do
      warmup cliWarmup (iteration loop 0)
      (elapsed, latencies) <- measureLatencyLoop cliMessages (iteration loop)
      pure
        Summary
          { summaryScenario = "event-loop",
            summaryPayloadBytes = cliPayloadBytes,
            summaryFrames = cliFrames,
            summarySockets = 2,
            summaryMessages = cliMessages,
            summaryElapsed = elapsed,
            summaryLatency = latencySummary latencies,
            summaryExtra = [("mailbox_capacity", show (max 1 (min 1024 cliMessages)))]
          }

runLifecycle :: Cli -> IO Summary
runLifecycle Cli {..} = do
  (elapsed, pendingBeforeCleanup) <- timeAction do
    Zmqx.withContext Zmqx.defaultOptions \context -> do
      replicateM_ cliMessages do
        _server <- openPairIn context
        _client <- openPairIn context
        pure ()
      Zmqx.pendingSockets context
  pure
    Summary
      { summaryScenario = "lifecycle",
        summaryPayloadBytes = 0,
        summaryFrames = 0,
        summarySockets = cliMessages * 2,
        summaryMessages = cliMessages,
        summaryElapsed = elapsed,
        summaryLatency = Nothing,
        summaryExtra = [("pending_before_cleanup", show pendingBeforeCleanup)]
      }

openPairIn :: Zmqx.Context -> IO Zmqx.Pair.Pair
openPairIn context =
  Zmqx.openWith context Zmqx.Pair.defaultOptions >>= unwrap
