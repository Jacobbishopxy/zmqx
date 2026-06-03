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
- [ ] **Send zero-copy ownership design** — TP-011 kept large sends copy-backed through C `malloc`/`free2` and limited the optimized path to multipart prefix frames; true zero-copy over Haskell `ByteString` storage still needs a reviewed `ForeignPtr`/`StablePtr` lifecycle that is safe for libzmq callbacks on arbitrary threads and context shutdown before `zmq_send_const` or zero-copy `zmq_msg_init_data` can be used publicly (discovered during TP-011).
- [ ] **Dynamic poll-set builder/API** — TP-008 moved poll preparation into `Sockets`, which improves repeated polling but shifts array/template rebuild cost to each `pollInAlso`/`pollOutAlso`; very large or frequently rebuilt dynamic poll sets may need an explicit builder or mutable prepared-poll API with lifecycle tests.
- [ ] **EventLoop command wakeup fairness** — TP-012 reduced the receiver poll slice to 1ms and fixed sub-millisecond poll timeout rounding, but commands still wait for the current `zmq_poll` slice rather than waking the worker through an explicit control socket/eventfd; revisit if sub-millisecond tail latency or idle receiver CPU usage needs tighter guarantees.
- [ ] **EventLoop mailbox timer strategy** — TP-012 uses a bracketed cancellable timer thread for positive mailbox timeouts because `registerDelay` requires a threaded runtime in the current Cabal configuration; a shared timer manager or threaded-runtime policy could reduce per-wait timer overhead if long concurrent mailbox waits become common.
- [ ] **Lifecycle registry compaction threshold tuning** — TP-013 replaced per-finalizer/pending full scans with exact pending-count bookkeeping and thresholded stale compaction; future work could tune the fixed threshold/heuristic against larger churn workloads if registry memory growth or allocation becomes more important than elapsed teardown time.
- [ ] **Lifecycle compaction interruption regression test** — TP-013 kept the production compaction mask/restore/onException pattern, but `pendingSockets` now reads an O(1) count so the existing interruption-safety test no longer directly stresses compaction; future internal coverage could target interrupted compaction if a test-only hook is introduced.
- [ ] **REQ poll wakeup source limitations** — Input `REQ` sockets still need the 10ms direct-probe safety cadence because libzmq `zmq_poll` wakeups were not reliable enough for valid or stale-then-valid correlated replies; TP-009 reduced repeated direct probe work within that cadence, but eliminating latency quantization likely needs explicit probe instrumentation, a stronger readiness signal, or a deeper REQ state-machine design.
- [ ] **Send-side EAGAIN/HWM benchmark fixture** — TP-010 optimized receive-side EAGAIN errno handling only; changing `zmq_send__unsafe` through a fused errno wrapper was deferred until there is deterministic saturated-send/high-water-mark correctness and benchmark coverage for public send behavior.
