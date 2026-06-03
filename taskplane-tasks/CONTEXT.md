# General — Context

**Last Updated:** 2026-06-02
**Status:** Active
**Next Task ID:** TP-006

---

## Current State

This is the default task area for zmqx. Tasks that don't belong
to a specific domain area are created here.

Taskplane is configured and ready for task execution. Use `/orch all` for
parallel batch execution or `/orch <path/to/PROMPT.md>` for a single task.

---

## Key Files

| Category | Path |
|----------|------|
| Tasks | `taskplane-tasks/` |
| Config | `.pi/taskplane-config.json` |

---

## Operational Rules

- One Taskplane task equals one final commit. Squash all step, review, hydration, checkpoint, `.DONE`, and task artifact changes for a `TP-###` into exactly one commit before integration.
- If a task cannot be represented as one coherent commit, split it into multiple TPs rather than committing multiple times for the same TP.

---

## Technical Debt / Future Work

_Items discovered during task execution are logged here by agents._

- [ ] **Benchmark statistics and persistence** — `zmqx-overheads` currently emits simple key=value summaries for manual smoke/trend runs; future work could add repeated samples, CSV/JSON output, and a persisted baseline workflow before using numbers as regression gates (discovered during TP-006).
- [ ] **EventLoop benchmark granularity** — the initial EventLoop scenario measures mailbox/transceiver round trips end-to-end, including worker scheduling and poll cadence; future optimization tasks may want narrower instrumentation around worker wakeups and callback/mailbox paths (discovered during TP-006).
- [ ] **Receive zero-copy ownership design** — TP-007 kept `ByteString.packCStringLen` receive copies because safely exposing libzmq message storage would require an owned-message/finalizer design that closes `zmq_msg_t` exactly once after the `ByteString` dies; revisit only with explicit lifetime tests and review.
- [ ] **Dynamic poll-set builder/API** — TP-008 moved poll preparation into `Sockets`, which improves repeated polling but shifts array/template rebuild cost to each `pollInAlso`/`pollOutAlso`; very large or frequently rebuilt dynamic poll sets may need an explicit builder or mutable prepared-poll API with lifecycle tests.
- [ ] **EventLoop command wakeup fairness** — TP-012 reduced the receiver poll slice to 1ms and fixed sub-millisecond poll timeout rounding, but commands still wait for the current `zmq_poll` slice rather than waking the worker through an explicit control socket/eventfd; revisit if sub-millisecond tail latency or idle receiver CPU usage needs tighter guarantees.
- [ ] **EventLoop mailbox timer strategy** — TP-012 uses a bracketed cancellable timer thread for positive mailbox timeouts because `registerDelay` requires a threaded runtime in the current Cabal configuration; a shared timer manager or threaded-runtime policy could reduce per-wait timer overhead if long concurrent mailbox waits become common.
