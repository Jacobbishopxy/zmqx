# Zmqx Examples

## How To Read This Page

This repository keeps both:

- demo/manual example executables under `test/`
- default automated regression suites under `test/test.cabal`

Many of the `Simple*` and similar example executables are gated behind `--flag demo-tests`.

Use:

```sh
# default automated sweep
cabal test all

# targeted automated suite
cabal test test-req-poll

# demo/manual suite
cabal test test-simple-dealer --flag demo-tests --flag debug
```

## Demo / Manual Example Sources

### Req / Rep

- [test/SimpleReq.hs](../test/SimpleReq.hs)
- [test/SimpleRep.hs](../test/SimpleRep.hs)

### Dealer / Router

- [test/SimpleDealer.hs](../test/SimpleDealer.hs)
- [test/SimpleRouter.hs](../test/SimpleRouter.hs)

### Pub / Sub

- [test/SimplePub.hs](../test/SimplePub.hs)
- [test/SimpleSub.hs](../test/SimpleSub.hs)

### Push / Pull Pipeline

- [test/TaskVentilator.hs](../test/TaskVentilator.hs)
- [test/TaskWorker.hs](../test/TaskWorker.hs)
- [test/TaskSink.hs](../test/TaskSink.hs)

### Polling / Proxy / Broker

- [test/ItemsPoll.hs](../test/ItemsPoll.hs)
- [test/SimpleProxy.hs](../test/SimpleProxy.hs)
- [test/SimpleBroker.hs](../test/SimpleBroker.hs)

### Worker Topologies

- [test/MutWorker.hs](../test/MutWorker.hs)
- [test/LBWorker.hs](../test/LBWorker.hs)

## Automated Regression Suites

The default automated sweep is buildable without `--flag demo-tests` and includes suites such as:

### Core Behavior

- `test-req-poll`
- `test-recv-for`
- `test-run-guard`
- `test-contextual-open`
- `test-monitor-event`
- `test-finalizer-registry`
- `test-zmqxt`

### Automated Scenario Coverage

- `test-req-rep-auto`
- `test-dealer-router-auto`
- `test-pub-sub-auto`
- `test-broker-auto`
- `test-proxy-auto`
- `test-task-pipeline-auto`
- `test-items-poll-auto`
- `test-mut-worker-auto`
- `test-lb-worker-auto`

## Where To Start

- Want a first runnable walkthrough: [Quickstart](./quickstart.md)
- Want the smallest automated regression to inspect: `test-req-poll`
- Want to browse scenario-style example code: start with the `Simple*` files above
