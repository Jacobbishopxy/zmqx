## Plan Review: Step 1: Design and API foundation

### Verdict: REVISE

### Summary
The available Step 1 plan covers the right high-level artifacts, but it does not yet settle the key API-shape decision that this checkpoint is meant to confirm. The blocking gap is how the new `Zmqx.EventLoop.send` will be exposed from `Zmqx` without breaking the existing top-level socket `send` export.

### Issues Found
1. **[Severity: important]** — `Zmqx` already exports and defines `send` for sockets (`lib/Zmqx.hs:39`, `lib/Zmqx.hs:132`), while the task requires a public `Zmqx.EventLoop.send` and also says to re-export the EventLoop API from `Zmqx` without disrupting existing exports. The plan does not resolve this single-namespace conflict, so a straightforward re-export can either fail to compile or change/break existing `Zmqx.send` callers. Revise the plan to state the compatibility strategy before implementation, e.g. a deliberate overloaded `Zmqx.send` that preserves existing socket calls and supports EventLoop sends, or a task amendment/export policy that keeps the EventLoop-specific `send` in `Zmqx.EventLoop` while preserving the current `Zmqx.send`.

### Missing Items
- A concrete public exposure strategy for the new EventLoop `send` name versus the existing socket `send` name.
- Enough signature-level API shape to verify the foundation checkpoint: expected `withEventLoop`/`withEventLoopIn` action style, `send` return/error behavior, and what key/endpoint type `addSender` and `send` share.

### Suggestions
- Keep Step 1 focused on opaque public types and stable signatures; leave worker-thread internals for Step 2, but document the socket-ownership invariant in the new module from the start so the API cannot imply direct public socket access.
