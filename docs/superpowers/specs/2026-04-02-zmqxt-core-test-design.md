# ZmqxT Core Test Design

## Context
- Task 1 in the approved implementation plan needs a failing integration test that imports the not-yet-existent `Zmqx.Monad` API surface. The goal is to capture the intended runtime behavior so the future implementation can be guided by a real-world use case.
- The new suite must live in `test/test.cabal`, so running `cabal test test-zmqxt` builds the executable and fails with `Could not find module 'Zmqx.Monad'`.

## Goals
1. Add `test/ZmqxMonad.hs` that exercises `runZmqx`, `runZmqxT`, and a custom `MonadZmqx` stack while pinging a PAIR socket pair via unique `inproc://` endpoints.
2. Register `test-zmqxt` in `test/test.cabal` so the new executable can be built independently and flagged by the test runner.
3. Keep the test content exactly as the snippet assigned in the plan so upstream developers have a concrete artifact to extend once `Zmqx.Monad` exists.

## Approach
- Create the new executable from the provided code, including helper functions (`assert`, `uniqueEndpoint`, `unwrapM`, `pairRoundTrip`, `App`, `runApp`) and the three execution paths that exercise different runners.
- Add the suite stanza that imports `test-properties` so it shares dependency management with the other tests.
- Leave the suite failing for now; the job is just to capture the desired behavior before implementing `Zmqx.Monad`.

## Testing
- Run `cabal test test-zmqxt` and confirm the build fails with `Could not find module 'Zmqx.Monad'` during compilation.

## Questions/Assumptions
- I’m assuming the provided executable is the desired shape of the eventual test; no additional coverage is required before the module exists. Please let me know if that assumption is wrong.
