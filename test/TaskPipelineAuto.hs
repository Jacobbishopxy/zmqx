{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common (unwrap)
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Data.Foldable (for_)
import Zmqx qualified
import Zmqx.Pull qualified
import Zmqx.Push qualified

assert :: Bool -> String -> IO ()
assert condition message =
  if condition
    then pure ()
    else throwIO (userError message)

main :: IO ()
main =
  Zmqx.run Zmqx.defaultOptions do
    let tasksEndpoint = "inproc://task-pipeline-auto-tasks"
        sinkEndpoint = "inproc://task-pipeline-auto-sink"
        workloads = ["1", "2", "3"]

    ventilator <- unwrap (Zmqx.Push.open (Zmqx.name "ventilator-auto"))
    workerReceiver <- unwrap (Zmqx.Pull.open (Zmqx.name "worker-receiver-auto"))
    workerSender <- unwrap (Zmqx.Push.open (Zmqx.name "worker-sender-auto"))
    sink <- unwrap (Zmqx.Pull.open (Zmqx.name "sink-auto"))

    unwrap (Zmqx.bind ventilator tasksEndpoint)
    unwrap (Zmqx.connect workerReceiver tasksEndpoint)
    unwrap (Zmqx.bind sink sinkEndpoint)
    unwrap (Zmqx.connect workerSender sinkEndpoint)

    threadDelay 100000

    for_ workloads (unwrap . Zmqx.send ventilator)

    for_ workloads \workload -> do
      received <- unwrap (Zmqx.receive workerReceiver)
      assert (received == workload) ("WORKER received the wrong workload: " <> show received)
      unwrap (Zmqx.send workerSender ("done:" <> workload))

    for_ workloads \workload -> do
      completed <- unwrap (Zmqx.receive sink)
      assert (completed == "done:" <> workload) ("SINK received the wrong completion: " <> show completed)
