## Code Review: Step 1: Design and API foundation

### Verdict: APPROVE

### Summary
The Step 1 implementation adds the exposed `Zmqx.EventLoop` module, the `stm` dependency, and the MVP API surface while preserving the existing top-level `Zmqx.send` export. No `.pi`/`package.json` static-check commands were configured; I ran `cabal build`, which reported the project was up to date.

### Issues Found
None.

### Pattern Violations
- None.

### Test Gaps
- None for this API-foundation step; sender behavior tests are scheduled for Step 3.

### Suggestions
- When Step 2 replaces the placeholder lifecycle/send behavior, ensure the current forward-looking ownership Haddocks stay accurate for both global and explicit context modes.
