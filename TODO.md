# TODO

- Plan A: Keep Current API (run + globals)

  - Add a RunState guard (e.g. MVar/IORef) so nested/concurrent run fails fast instead of clobbering globals.
  - Swap bogusContext bottom for IORef (Maybe Zmq_ctx) and throw a typed “uninitialized” error when used outside run.
  - Restructure teardown with bracket/finally to avoid double-terminate; consider optional timeout or best-effort mode for stuck zmq_ctx_term.
  - Keep socket registry but make it per-run (reset/clear safely); expose a helper to detect leaked sockets before terminate.
  - Optional: forkZmqx/asyncZmqx helper to track child threads and join/cancel before context shutdown.

- Plan B: New API (Explicit Handle / Monad)

  - Keep Context and withContext :: Options () -> (Context -> IO a) -> IO a as the core explicit-context API, and expose a separate Zmqx.Monad surface built around Context / MonadZmqx / ZmqxT.
  - Provide monadic open helpers on the separate Zmqx.Monad API; sockets still reference their originating context handle rather than a global.
  - Offer runZmqx :: Options () -> ZmqxT IO a -> IO a for straight-line syntax; keep the existing Zmqx.run global wrapper separate for compatibility rather than delegating it through the monadic path.
  - Add optional region typing (Context s, Socket s role) to prevent sockets escaping the context; or keep first-class but document lifetime.
  - Update public modules to accept Context/MonadZmqx where needed, and add transitional shims for existing callers.
