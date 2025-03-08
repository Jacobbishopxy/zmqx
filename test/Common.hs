-- file: Common.hs
-- author: Jacob Xie
-- date: 2025/02/28 08:51:15 Friday
-- brief:

module Common
  ( endpoint,
    endpoint1,
    endpoint2,
    endpointInproc,
    unwrap,
    endpointIpc1,
    endpointIpc2,
  )
where

import Control.Exception (throwIO)
import Data.Text (Text)
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
