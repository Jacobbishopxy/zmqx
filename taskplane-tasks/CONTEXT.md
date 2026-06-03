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
