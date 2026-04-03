# Demo Test Split Design

**Context**

`test/test.cabal` currently mixes two different kinds of executables:

- manual/demo topology programs such as `SimpleRep`, `SimpleRouter`, `SimpleProxy`, and `TaskVentilator`
- bounded automated regression suites such as `ReqPoll`, `MonitorEvent`, and `RunGuard`

That makes `cabal test all` an unreliable signal. Some demo-style suites depend on external peers, `tcp://127.0.0.1:*`, `getLine`, or indefinite loops, so they either fail under sandbox restrictions or pass without proving anything useful.

**Decision**

Keep the demo programs, but remove them from the default `cabal test all` surface. Replace their intended coverage with new self-contained automated suites.

**Scope**

Manual demos to make opt-in:

- `test-simple-req`
- `test-simple-rep`
- `test-simple-dealer`
- `test-simple-router`
- `test-simple-pub`
- `test-simple-sub`
- `test-task-ventilator`
- `test-task-worker`
- `test-task-sink`
- `test-simple-broker`
- `test-simple-proxy`

New automated coverage to add:

- `ReqRepAuto.hs`
- `DealerRouterAuto.hs`
- `PubSubAuto.hs`
- `BrokerAuto.hs`
- `ProxyAuto.hs`
- `TaskPipelineAuto.hs`

**Approach**

1. Add a `demo-tests` Cabal flag in `test/test.cabal`, default `False`.
2. Gate the demo suites with `if !flag(demo-tests) buildable: False`.
3. Add new automated suites that:
   - run in one process
   - use `inproc://` endpoints by default
   - avoid user input
   - avoid infinite loops
   - assert actual message flow instead of just printing
4. Keep the old demo source files unchanged so they remain usable as manual examples.

**Why This Split**

- preserves the example value of the old demos
- gives `cabal test all` a clean contract: bounded and self-contained
- avoids reworking demo files into awkward pseudo-tests
- keeps sandbox compatibility aligned with the rest of the modern regression suite

**Non-Goals**

- rewriting `MutWorker` or `LBWorker` unless they prove to be part of the default-suite problem
- changing the public library API
- deleting the old demo sources

**Acceptance Criteria**

- default `cabal test all` no longer includes the manual demo suites
- the old demo suites are still runnable explicitly with `-f demo-tests`
- each excluded topology has a real automated replacement
- the new automated replacements are bounded and sandbox-safe
