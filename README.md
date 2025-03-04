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
  tag: 2b649f32d1c5cfb947bd85c2aff504058612cb2e
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

- [Task Ventilator (PUSH)](./test/TaskVentilator.hs) -> [Task Worker (PULL + PUSH)](./test/TaskWorker.hs) -> [Task Sink (PULL)](./test/TaskSink.hs)
