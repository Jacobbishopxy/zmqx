## Plan Review: Step 3: Harden lifecycle and registry behavior

### Verdict: APPROVE

### Summary
The Step 3 plan targets the right lifecycle-hardening outcomes from the prompt: deterministic duplicate-name rejection, stop/exit wakeups for public waiters, safe surfacing of worker/callback failures, and preservation of the bracketed `withEventLoop` lifecycle. This also follows the Step 1 design notes and the Step 2 implementation review suggestions around keeping duplicate validation visible before worker startup.

### Issues Found
None.

### Missing Items
- None blocking.

### Suggestions
- When implementing duplicate-name rejection, make sure same-role repeated additions are still detectable rather than hidden by the current `Map.insert`/`Map.union` behavior before validation runs.
- Clarify in the implementation/Haddocks whether normal stop drains already-queued `send` commands or cancels them, as long as every waiter is woken with either its worker result, the recorded worker exception, or a stopped-loop error.
