# Repository Guidelines

## Project Structure & Module Organization
- `zmqx.cabal` defines the library surface; `cabal.project` pins local builds.
- `lib/` contains the Haskell source. `Zmqx.hs` re-exports the public API; `Zmqx/Core/*` manages context, polling, monitoring, and socket lifecycles; `Zmqx/Internal/*` holds FFI bindings and constants; `Zmqx/*.hs` implements socket roles (Req, Rep, Dealer, Router, Pub/Sub, etc.).
- `c/` wraps `libzmq` with `zmq-wrapper.c` and helpers referenced by the cabal stanza.
- `test/` houses scenario-style executables defined in `test/test.cabal` (SimpleReq/Rep, Dealer/Router, Pub/Sub, proxies, workers). Add new cases here and register them in `test/test.cabal`.
- `dist-newstyle/` is build output; leave it untouched.

## Build, Test, and Development Commands
- Ensure `libzmq` (>=4,<5) is available via `pkg-config`; export `PKG_CONFIG_PATH` if it lives in a non-default prefix.
- Build the library: `cabal build [--flag debug]`. The `debug` flag enables extra tracing at the C boundary.
- Run tests: `cabal test test-simple-dealer --flag debug` (swap the suite name as needed) or `cabal test all` for a full sweep.
- REPL for quick experiments: `cabal repl zmqx` (library) or `cabal repl test:test-simple-req` (per-suite).

## Coding Style & Naming Conventions
- Haskell 2024 with `-Wall`/`-Weverything` (selectively relaxed); keep builds warning-free. Prefer explicit export lists and qualified imports for clarity.
- Follow existing 2-space indentation, leading module pragmas, and grouped exports by topic. Keep top-level type signatures and avoid redundant `deriving` when deriving strategies already cover it.
- Use `CamelCase` for modules/types, `lowerCamel` for values/functions, and match socket-specific modules (`Zmqx.Req`, `Zmqx.Router`, etc.) with their ZMQ role names.

## Testing Guidelines
- Tests are plain `main` executables (no framework). Mirror existing naming (`SimpleFoo.hs`) and add a `test-suite` block in `test/test.cabal` with dependencies and flags.
- Prefer lightweight, deterministic scenarios; gate any long-running or network-sensitive flows behind explicit flags.

## Commit & Pull Request Guidelines
- Commit messages follow the current history: emoji prefix + scope + concise action, e.g., `♻️ (Core/Context): replace IORef...`. Keep summaries in the imperative mood.
- PRs should describe behavior changes, list impacted commands, and note test runs (`cabal test ...`). Link related issues and add logs or screenshots when touching visible behavior.

## Security & Configuration Tips
- FFI depends on the system `libzmq`; verify versions before publishing. Avoid committing generated artifacts (`dist-newstyle/`, GHC interface files). Keep sensitive endpoints out of test defaults; prefer `inproc://` or `ipc://` endpoints for local runs.
