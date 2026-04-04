# TODO

- Remaining roadmap items

  - Revisit region typing only if real escaping-socket bugs or multi-context confusion justify the public API cost of `Context s` / `Socket s role`.
  - Keep the compatibility surface intentionally bounded: `Zmqx` stays the direct `IO` API, `Zmqx.Monad` stays additive, and new duplicate entrypoints should require a concrete migration need.
  - Keep `README.md`, `docs/checklist.md`, and this file aligned if that preferred surface changes.

- Completed direction

  - `run` is guarded, global-context initialization errors are typed, and teardown is strict and bracketed.
  - Socket finalizer registries are scoped per context/run, compacted safely, and exposed through `pendingSockets` diagnostics.
  - The explicit-context path is complete through `Context`, `withContext`, and `openWith`/`ContextualOpen`.
  - `Zmqx.Monad` and `runZmqx` exist as additive ergonomics on top of `withContext`, while `Zmqx.run` remains the compatibility path for global `*.open`.
  - The main API does not currently add `withSocket`, `forkZmqx`, `asyncZmqx`, or timeout/best-effort shutdown.
