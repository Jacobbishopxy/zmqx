-- file: Common.hs
-- author: Jacob Xie
-- date: 2025/02/28 08:51:15 Friday
-- brief:

module Common (module Common) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, ask, liftIO)
import Data.Text (Text)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.IO
import Zmqx

endpoint :: Text
endpoint = "tcp://127.0.0.1:5555"

endpoint1 :: Text
endpoint1 = "tcp://127.0.0.1:5556"

endpoint2 :: Text
endpoint2 = "tcp://127.0.0.1:5557"

endpointInproc :: Text
endpointInproc = "inproc://mem-inproc"

endpointIpc1 :: Text
endpointIpc1 = "ipc://frontend.ipc"

endpointIpc2 :: Text
endpointIpc2 = "ipc://backend.ipc"

----------------------------------------------------------------------------------------------------

unwrap :: IO (Either Zmqx.Error a) -> IO a
unwrap action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value

----------------------------------------------------------------------------------------------------

type AppMonad = ReaderT Logger IO

data Logger = Logger
  { logInfo :: String -> IO (),
    logError :: String -> IO ()
  }

createLogger :: IO Logger
createLogger = do
  now <- getCurrentTime
  let fileName = formatTime defaultTimeLocale "%Y-%m-%d.log" now
  handle <- openFile fileName AppendMode
  hSetBuffering handle LineBuffering
  let infoFunc msg = do
        timestamp <- formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getCurrentTime
        let output = "[" ++ timestamp ++ "] INFO: " ++ msg
        putStrLn output
        hPutStrLn handle output
        hFlush handle
      errorFunc msg = do
        timestamp <- formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getCurrentTime
        let output = "[" ++ timestamp ++ "] ERROR: " ++ msg
        hPutStrLn stderr output
        hPutStrLn handle output
        hFlush handle
  return $ Logger infoFunc errorFunc

logInfoM :: (MonadIO m) => Logger -> String -> m ()
logInfoM logger = liftIO . logInfo logger

logErrorM :: (MonadIO m) => Logger -> String -> m ()
logErrorM logger = liftIO . logError logger

logInfoR :: String -> AppMonad ()
logInfoR msg = ask >>= \logger -> liftIO $ logInfo logger msg

logErrorR :: String -> AppMonad ()
logErrorR msg = ask >>= \logger -> liftIO $ logError logger msg
