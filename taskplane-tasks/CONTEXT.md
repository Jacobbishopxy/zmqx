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
