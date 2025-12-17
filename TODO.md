# TODO

- Plan A: Keep Current API (run + globals)

  - Add a RunState guard (e.g. MVar/IORef) so nested/concurrent run fails fast instead of clobbering globals.
  - Swap bogusContext bottom for IORef (Maybe Zmq_ctx) and throw a typed “uninitialized” error when used outside run.
  - Restructure teardown with bracket/finally to avoid double-terminate; consider optional timeout or best-effort mode for stuck zmq_ctx_term.
  - Keep socket registry but make it per-run (reset/clear safely); expose a helper to detect leaked sockets before terminate.
  - Optional: forkZmqx/asyncZmqx helper to track child threads and join/cancel before context shutdown.

- Plan B: New API (Explicit Handle / Monad)

  - Introduce Context handle and withContext :: Options () -> (Context -> IO a) -> IO a; move globals into Env carried in a ReaderT (ZmqxT).
  - Provide withSocket/openSocket that use the explicit Context; sockets reference their context handle, not a global.
  - Offer runZmqx :: Options () -> ZmqxT IO a -> IO a for straight-line syntax; keep a thin Zmqx.run wrapper that delegates to runZmqx for compatibility.
  - Add optional region typing (Context s, Socket s role) to prevent sockets escaping the context; or keep first-class but document lifetime.
  - Update public modules to accept Context/MonadZmqx where needed, and add transitional shims for existing callers.
