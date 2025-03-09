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
  tag: 3725a3c5ed03389071a5eef827ca1d0a12db5039
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
