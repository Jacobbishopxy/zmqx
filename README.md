# Zmqx

Copied from:

- <https://github.com/awkward-squad/libzmq>
- <https://github.com/mitchellwrosen/zmq>

Support `base ^>=4.20.0.0`

## Usage

```cabal
# cabal.project

source-repository-package
  type: git
  location: git@github.com:Jacobbishopxy/zmqx.git
  tag: 35338d457593b9d9200c256edf93bfe2a370cf3a
```

## Test & Build

```sh
# test
cabal test test-simple-dealer --flag debug

# build
cabal build --flag debug
```

## Example

- [Simple Req](./test/SimpleReq.hs) + [Simple Rep](./test/SimpleRep.hs)

- [Simple Dealer](./test/SimpleDealer.hs) + [Simple Router](./test/SimpleRouter.hs)

- [Simpl Pub](./test/SimplePub.hs) + [Simple Sub](./test/SimpleSub.hs)

- [Task Ventilator (Push)](./test/TaskVentilator.hs) -> [Task Worker (Pull + Push)](./test/TaskWorker.hs) -> [Task Sink (Pull)](./test/TaskSink.hs)

- [Zmq items poll](./test/ItemsPoll.hs)

- [Simple Proxy (XPub + XSub)](./test/SimpleProxy.hs)

- [Multiple workers (Router + Dealer + Rep)](./test/MutWorker.hs)

- [Load-balance worker](./test/LBWorker.hs)

## Not implemented

- Timers API

  - zmq_timers_new/destroy create/destroy a timer scheduler object (handles multiple timers; call destroy with pointer-to-pointer).
  - zmq_timers_add registers a handler callback run every interval milliseconds; returns a timer id.
  - zmq_timers_cancel removes a timer by id.
  - zmq_timers_set_interval changes an existing timer’s interval.
  - zmq_timers_reset restarts a timer’s countdown.
  - zmq_timers_timeout reports milliseconds until the next timer should fire (or -1 if none) so you can integrate with your own event loop.
  - zmq_timers_execute runs due timers’ callbacks; you typically poll timeout and then call execute when appropriate.

- Utility helpers

  - zmq_stopwatch_start/intermediate/stop lightweight wall-clock stopwatch in microseconds; intermediate reads elapsed so far, stop reads and frees.
  - zmq_sleep simple whole-second sleep helper.
  - zmq_threadstart/threadclose starts a native thread running a zmq_thread_fn with a void* argument and later joins/frees it via threadclose; intended for quick utility threads
    without pulling in a threading lib.
