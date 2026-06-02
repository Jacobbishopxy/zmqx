## Code Review: Step 2: Implement lifecycle and sender command routing

### Verdict: REVISE

### Summary
The command-queue lifecycle is broadly in place and `cabal build` succeeds; no `.pi`/`package.json` static-check commands were configured in this worktree. However, the worker can leave a public `send` caller blocked forever when the underlying socket send throws, which is a blocking lifecycle/command-routing bug.

### Issues Found
1. **[lib/Zmqx/EventLoop.hs:153] [important]** — A command that throws before `putMVar reply result` terminates the worker without ever replying, while the caller waits indefinitely at line 191. This can happen with supported `CanSend` sockets such as `Rep` when ZeroMQ reports a programmer/state error (direct `Zmqx.send` would throw, but the event-loop path hangs). Fix by catching exceptions per `Send` command and always completing the reply MVar (for example with `Either SomeException (Either Error ())` and rethrowing in the caller), and ensure worker failure is observed rather than silently stranding queued callers.

### Pattern Violations
- None.

### Test Gaps
- Add a regression in the Step 3 sender tests for a send command whose socket operation fails/throws, verifying the public `Zmqx.EventLoop.send` call does not hang and the loop finalizer remains joinable.

### Suggestions
- Consider separating “stop requested” from “worker is done” state so `stopEventLoop` can always join/report `eventLoopWorkerDone` even if the worker set the stopped flag itself.
