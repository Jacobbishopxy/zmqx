module Bench.Helpers
  ( BenchmarkMetadata (..),
    LatencySummary (..),
    Summary (..),
    awaitConnection,
    benchmarkMetadata,
    elapsedMicroseconds,
    formatDouble,
    latencySummary,
    payload,
    payloadFrames,
    renderSummary,
    throughputPerSecond,
    timeAction,
    uniqueEndpoint,
    unwrap,
    unwrapMaybe,
    warmup,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (replicateM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.Unique (hashUnique, newUnique)
import Data.Version (showVersion)
import System.Info qualified as Info
import Text.Printf (printf)
import Zmqx qualified

awaitConnection :: IO ()
awaitConnection =
  threadDelay 100000

uniqueEndpoint :: String -> IO Text
uniqueEndpoint label = do
  unique <- newUnique
  pure ("inproc://zmqx-overheads-" <> Text.pack label <> "-" <> Text.pack (show (hashUnique unique)))

payload :: Int -> ByteString
payload payloadBytes =
  ByteString.replicate (max 0 payloadBytes) 97

payloadFrames :: Int -> Int -> [ByteString]
payloadFrames frameCount payloadBytes =
  replicate (max 1 frameCount) (payload payloadBytes)

unwrap :: Either Zmqx.Error a -> IO a
unwrap =
  either throwIO pure

unwrapMaybe :: String -> Either Zmqx.Error (Maybe a) -> IO a
unwrapMaybe label result =
  unwrap result >>= \case
    Nothing -> throwIO (userError (label <> " timed out"))
    Just value -> pure value

warmup :: Int -> IO () -> IO ()
warmup iterationCount action =
  replicateM_ (max 0 iterationCount) action

timeAction :: IO a -> IO (NominalDiffTime, a)
timeAction action = do
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  pure (diffUTCTime end start, result)

elapsedMicroseconds :: NominalDiffTime -> Double
elapsedMicroseconds elapsed =
  realToFrac elapsed * 1000000.0

throughputPerSecond :: Int -> NominalDiffTime -> Double
throughputPerSecond messages elapsed =
  let seconds = realToFrac elapsed :: Double
   in if seconds <= 0.0
        then 0.0
        else fromIntegral messages / seconds

data LatencySummary = LatencySummary
  { latencyP50Us :: !Double,
    latencyP95Us :: !Double,
    latencyMaxUs :: !Double
  }
  deriving stock (Eq, Show)

latencySummary :: [NominalDiffTime] -> Maybe LatencySummary
latencySummary samples =
  case List.sort (map elapsedMicroseconds samples) of
    [] -> Nothing
    sorted ->
      Just
        LatencySummary
          { latencyP50Us = percentile sorted 0.50,
            latencyP95Us = percentile sorted 0.95,
            latencyMaxUs = last sorted
          }

percentile :: [Double] -> Double -> Double
percentile sorted quantile =
  let sampleCount = length sorted
      index = min (sampleCount - 1) (floor (quantile * fromIntegral (sampleCount - 1)))
   in sorted !! index

data BenchmarkMetadata = BenchmarkMetadata
  { metadataLibzmqVersion :: !String,
    metadataCompiler :: !String,
    metadataOS :: !String,
    metadataArch :: !String
  }
  deriving stock (Eq, Show)

benchmarkMetadata :: IO BenchmarkMetadata
benchmarkMetadata = do
  let (majorVersion, minorVersion, patchVersion) = Zmqx.version
  pure
    BenchmarkMetadata
      { metadataLibzmqVersion = show majorVersion <> "." <> show minorVersion <> "." <> show patchVersion,
        metadataCompiler = Info.compilerName <> "-" <> showVersion Info.compilerVersion,
        metadataOS = Info.os,
        metadataArch = Info.arch
      }

data Summary = Summary
  { summaryScenario :: !String,
    summaryPayloadBytes :: !Int,
    summaryFrames :: !Int,
    summarySockets :: !Int,
    summaryMessages :: !Int,
    summaryElapsed :: !NominalDiffTime,
    summaryLatency :: !(Maybe LatencySummary),
    summaryExtra :: ![(String, String)]
  }
  deriving stock (Eq, Show)

renderSummary :: BenchmarkMetadata -> Summary -> String
renderSummary metadata Summary {..} =
  unwords (map renderField fields)
  where
    fields =
      [ ("scenario", summaryScenario),
        ("payload_bytes", show summaryPayloadBytes),
        ("frames", show summaryFrames),
        ("sockets", show summarySockets),
        ("messages", show summaryMessages),
        ("elapsed_ms", formatDouble (elapsedMicroseconds summaryElapsed / 1000.0)),
        ("throughput_msg_per_s", formatDouble (throughputPerSecond summaryMessages summaryElapsed)),
        ("latency_p50_us", maybe "NA" (formatDouble . latencyP50Us) summaryLatency),
        ("latency_p95_us", maybe "NA" (formatDouble . latencyP95Us) summaryLatency),
        ("latency_max_us", maybe "NA" (formatDouble . latencyMaxUs) summaryLatency),
        ("metadata_libzmq", metadataLibzmqVersion metadata),
        ("metadata_compiler", metadataCompiler metadata),
        ("metadata_os", metadataOS metadata),
        ("metadata_arch", metadataArch metadata),
        ("rts_allocation_guidance", "run_with_+RTS_-s")
      ]
        <> summaryExtra

renderField :: (String, String) -> String
renderField (key, value) =
  key <> "=" <> value

formatDouble :: Double -> String
formatDouble =
  printf "%.3f"
