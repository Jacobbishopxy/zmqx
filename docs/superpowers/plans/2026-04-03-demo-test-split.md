# Demo Test Split Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove manual demo executables from the default `cabal test all` surface while adding real self-contained automated replacements for their intended coverage.

**Architecture:** Keep the old demo programs as opt-in Cabal test-suites behind a `demo-tests` flag, then add new bounded regression executables that run both ends of each topology in one process over `inproc://` endpoints. This preserves example value while making default test execution meaningful and sandbox-safe.

**Tech Stack:** Cabal, Haskell test executables, ZeroMQ inproc transport, existing `Zmqx.run`/socket-role APIs

---

### Task 1: Make Demo Suites Opt-In

**Files:**
- Modify: `test/test.cabal`
- Test: `cabal --config-file=/tmp/cabal-home/config test all`

- [ ] **Step 1: Add a `demo-tests` flag**

Add a manual flag with default `False` near the existing `debug` flag.

- [ ] **Step 2: Gate the old demo suites**

Mark these suites `buildable: False` unless `flag(demo-tests)` is enabled:

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

- [ ] **Step 3: Verify the default surface changed**

Run:

```bash
cabal --config-file=/tmp/cabal-home/config test all
```

Expected: the old demo suites are no longer configured or run by default.

### Task 2: Add Automated REQ/REP Coverage

**Files:**
- Create: `test/ReqRepAuto.hs`
- Modify: `test/test.cabal`
- Test: `cabal --config-file=/tmp/cabal-home/config test test-req-rep-auto`

- [ ] **Step 1: Write a failing automated REQ/REP suite**

Cover:

- REP bind + REQ connect over `inproc://`
- bounded request/reply exchange
- REP receives exactly what REQ sent
- REQ receives exactly what REP replied

- [ ] **Step 2: Register the suite**

Add:

- suite name: `test-req-rep-auto`
- `main-is: ReqRepAuto.hs`
- `build-depends: bytestring`

- [ ] **Step 3: Run the suite and verify behavior**

Run:

```bash
cabal --config-file=/tmp/cabal-home/config test test-req-rep-auto
```

Expected: PASS.

### Task 3: Add Automated DEALER/ROUTER Coverage

**Files:**
- Create: `test/DealerRouterAuto.hs`
- Modify: `test/test.cabal`
- Test: `cabal --config-file=/tmp/cabal-home/config test test-dealer-router-auto`

- [ ] **Step 1: Write a failing automated DEALER/ROUTER suite**

Cover:

- DEALER bind/connect path over `inproc://`
- ROUTER receives routing envelope and payload
- ROUTER replies to the same routing id
- DEALER receives the reply body

- [ ] **Step 2: Register the suite**

Add:

- suite name: `test-dealer-router-auto`
- `main-is: DealerRouterAuto.hs`
- `build-depends: bytestring`

- [ ] **Step 3: Run the suite**

Run:

```bash
cabal --config-file=/tmp/cabal-home/config test test-dealer-router-auto
```

Expected: PASS.

### Task 4: Add Automated PUB/SUB Coverage

**Files:**
- Create: `test/PubSubAuto.hs`
- Modify: `test/test.cabal`
- Test: `cabal --config-file=/tmp/cabal-home/config test test-pub-sub-auto`

- [ ] **Step 1: Write a failing automated PUB/SUB suite**

Cover:

- PUB bind + SUB connect over `inproc://`
- explicit subscription prefix
- bounded publication sequence
- SUB receives only matching messages

- [ ] **Step 2: Register the suite**

Add:

- suite name: `test-pub-sub-auto`
- `main-is: PubSubAuto.hs`
- `build-depends: bytestring`

- [ ] **Step 3: Run the suite**

Run:

```bash
cabal --config-file=/tmp/cabal-home/config test test-pub-sub-auto
```

Expected: PASS.

### Task 5: Add Automated Broker/Proxy Coverage

**Files:**
- Create: `test/BrokerAuto.hs`
- Create: `test/ProxyAuto.hs`
- Modify: `test/test.cabal`
- Test: `cabal --config-file=/tmp/cabal-home/config test test-broker-auto test-proxy-auto`

- [ ] **Step 1: Write a failing automated broker suite**

Cover:

- frontend `ROUTER` + backend `DEALER`
- one client request forwarded to one worker and back
- bounded loop with explicit completion

- [ ] **Step 2: Write a failing automated proxy suite**

Cover:

- `XSUB` + `XPUB`
- subscriber subscription forwarding
- one published message crossing the proxy to a subscriber

- [ ] **Step 3: Register both suites**

Add:

- `test-broker-auto`
- `test-proxy-auto`

- [ ] **Step 4: Run both suites**

Run:

```bash
cabal --config-file=/tmp/cabal-home/config test test-broker-auto test-proxy-auto
```

Expected: PASS.

### Task 6: Add Automated Task Pipeline Coverage

**Files:**
- Create: `test/TaskPipelineAuto.hs`
- Modify: `test/test.cabal`
- Test: `cabal --config-file=/tmp/cabal-home/config test test-task-pipeline-auto`

- [ ] **Step 1: Write a failing automated task pipeline suite**

Cover:

- PUSH ventilator -> PULL worker -> PUSH sink
- bounded fixed workload list
- no `getLine`
- no infinite loops
- sink receives all expected completions

- [ ] **Step 2: Register the suite**

Add:

- suite name: `test-task-pipeline-auto`
- `main-is: TaskPipelineAuto.hs`
- `build-depends: bytestring`

- [ ] **Step 3: Run the suite**

Run:

```bash
cabal --config-file=/tmp/cabal-home/config test test-task-pipeline-auto
```

Expected: PASS.

### Task 7: Re-run the Default Full Suite

**Files:**
- Modify: `test/test.cabal`
- Test: `cabal --config-file=/tmp/cabal-home/config test all`

- [ ] **Step 1: Run the full default suite**

Run:

```bash
cabal --config-file=/tmp/cabal-home/config test all
```

Expected: PASS without requiring `-f demo-tests`.

- [ ] **Step 2: Spot-check demo opt-in behavior**

Run:

```bash
cabal --config-file=/tmp/cabal-home/config test test-simple-req -f demo-tests
```

Expected: the manual suite is buildable again when explicitly requested.
