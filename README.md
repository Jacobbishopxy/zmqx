# Zmqx

Haskell bindings and higher-level ergonomics for `libzmq`, with both direct `IO` and additive
monad-style APIs.

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

## API Styles

### Direct API: `Zmqx`

Use the direct API when you want plain `IO` and explicit lifetime boundaries.

- Pros: simpler mental model, less abstraction overhead, easier debugging, explicit lifetime
  boundaries.
- Cons: explicit-context code can accumulate `Context` plumbing.
- Best fit: small programs, examples, focused experiments, callers who prefer plain `IO`.

This includes both `Zmqx.run` and `Zmqx.withContext`.

### Monad Style API: `Zmqx.Monad`

Use the monad-style API when you want explicit-context semantics with less manual context
threading.

- Pros: less context plumbing, natural fit for transformer stacks, same runtime semantics as
  `withContext`.
- Cons: extra abstraction layer, less direct for tiny scripts, should not be confused with a
  separate runtime.
- Best fit: medium or large applications and effect-stack based code.

`Zmqx.Monad` is additive, not a replacement for `Zmqx`.

### Optional Reactor API: `Zmqx.EventLoop`

Use the event loop when you want one worker thread to own registered sockets and expose named
sender, receiver mailbox, callback, or transceiver endpoints.

- Pros: centralizes socket ownership and polling for small reactor-style components.
- Cons: intentionally narrow MVP; registered sockets must not be touched directly while the loop
  runs, and callbacks execute on the worker thread.
- Best fit: applications that want a concise reactor around existing direct API sockets.

`Zmqx.EventLoop` works with both `Zmqx.run`/`withEventLoop` and
`Zmqx.withContext`/`withEventLoopIn`. It is optional and additive to the direct and monad-style
APIs, not a replacement runtime.

## Test & Build

```sh
cabal test all
cabal test test-req-poll
cabal test test-simple-dealer --flag demo-tests --flag debug
cabal build --flag debug
```

## Documentation

- [Quickstart](./docs/quickstart.md)
- [Examples](./docs/examples.md)
- [Performance benchmarks](./docs/performance.md)
- [Maintainer checklist](./docs/checklist.md)
