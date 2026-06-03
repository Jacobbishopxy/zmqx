# Performance benchmarks

This repository includes an opt-in benchmark package under `bench/`. It is not part of the default automated test sweep; run it explicitly when measuring zmqx overheads.

## Executable

```sh
cabal run zmqx-overheads -- --help
```

The executable emits one `key=value` summary line per scenario. All scenarios use local `inproc://` endpoints and require no external services. The benchmark package is wired through `cabal.project` as an opt-in target, so keep using `cabal test all` for correctness gates and invoke `zmqx-overheads` explicitly for measurement.

## Scenarios

| Scenario | Purpose | Main knobs |
| --- | --- | --- |
| `direct` | Direct single-frame `PAIR` send/receive overhead | `--messages`, `--payload-bytes`, `--warmup` |
| `multipart` | Multipart send and receive allocation/copy overhead | `--messages`, `--payload-bytes`, `--frames` |
| `poll` | Poll-set scaling across multiple `PULL` sockets | `--messages`, `--payload-bytes`, `--sockets`, `--timeout-ms` |
| `req-poll` | `REQ` valid-reply timeout/probe behavior through `receivesFor` | `--messages`, `--payload-bytes`, `--timeout-ms` |
| `req-poll-idle` | Idle `REQ` input `pollFor` timeout/probe behavior with a ROUTER peer that does not reply | `--messages`, `--payload-bytes`, `--timeout-ms` |
| `event-loop` | EventLoop mailbox/transceiver send/receive latency | `--messages`, `--payload-bytes`, `--frames`, `--timeout-ms` |
| `lifecycle` | Socket open/finalizer/context cleanup churn | `--messages` |

Use `--scenario all` (the default) to run the standard smoke matrix. The timeout-dominated `req-poll-idle` scenario is intentionally explicit-only so default/all runs do not spend `messages * timeout-ms` waiting for idle timeouts.

## Final verification smoke matrix

TP-014 used this small optimized matrix as the final performance-docs smoke sweep after the TP-006 through TP-013 remediation series. These commands validate wiring and representative paths; they are not CI performance gates.

Standard all-scenario smoke covering direct send/receive, multipart receive, poll, valid REQ poll, EventLoop, and lifecycle:

```sh
cabal run --enable-optimization=2 zmqx-overheads -- \
  --scenario all --messages 5 --warmup 1 --sockets 2 --frames 2 --payload-bytes 64 +RTS -s
```

Explicit idle REQ poll/probe smoke, separate from `all` because it is timeout-dominated:

```sh
cabal run --enable-optimization=2 zmqx-overheads -- \
  --scenario req-poll-idle --messages 3 --warmup 1 --timeout-ms 10 --payload-bytes 64 +RTS -s
```

Threshold-adjacent direct send/receive comparison for the large-send caveat:

```sh
cabal run --enable-optimization=2 zmqx-overheads -- \
  --scenario direct --messages 5 --warmup 1 --payload-bytes 65536 +RTS -s
```

Threshold-adjacent multipart send smoke that exercises the large multipart-prefix path:

```sh
cabal run --enable-optimization=2 zmqx-overheads -- \
  --scenario multipart --messages 5 --warmup 1 --frames 4 --payload-bytes 65536 +RTS -s
```

## Output fields

Common fields:

- `scenario` — benchmark scenario name.
- `payload_bytes` — bytes per frame.
- `frames` — frame count per message where applicable.
- `sockets` — sockets or socket pairs involved in the measurement.
- `messages` — timed messages or lifecycle iterations.
- `elapsed_ms` — total timed section duration.
- `throughput_msg_per_s` — messages per second over the timed section.
- `latency_p50_us`, `latency_p95_us`, `latency_max_us` — per-iteration latency summaries where practical; `NA` for scenarios that only time aggregate lifecycle work.
- `metadata_libzmq`, `metadata_compiler`, `metadata_os`, `metadata_arch` — environment metadata for comparing runs.
- `rts_allocation_guidance` — reminder to use `+RTS -s` for allocation summaries.

Scenario-specific fields currently include `poll_timeout_ms`, `receive_timeout_ms`, `mailbox_capacity`, and `pending_before_cleanup`.

## Final smoke sample

On 2026-06-03, the TP-014 final matrix passed on this lane worktree with libzmq `4.3.4`, GHC `9.14`, Linux x86_64.

Representative output from the standard all-scenario smoke:

```text
scenario=direct payload_bytes=64 frames=1 sockets=2 messages=5 elapsed_ms=0.011 throughput_msg_per_s=439560.440 latency_p50_us=0.765 latency_p95_us=1.980 latency_max_us=3.938 metadata_libzmq=4.3.4 metadata_compiler=ghc-9.14 metadata_os=linux metadata_arch=x86_64 rts_allocation_guidance=run_with_+RTS_-s
scenario=multipart payload_bytes=64 frames=2 sockets=2 messages=5 elapsed_ms=0.036 throughput_msg_per_s=140445.493 latency_p50_us=7.134 latency_p95_us=7.663 latency_max_us=12.453 metadata_libzmq=4.3.4 metadata_compiler=ghc-9.14 metadata_os=linux metadata_arch=x86_64 rts_allocation_guidance=run_with_+RTS_-s
scenario=poll payload_bytes=64 frames=1 sockets=2 messages=5 elapsed_ms=0.119 throughput_msg_per_s=42178.432 latency_p50_us=23.817 latency_p95_us=23.957 latency_max_us=24.360 metadata_libzmq=4.3.4 metadata_compiler=ghc-9.14 metadata_os=linux metadata_arch=x86_64 rts_allocation_guidance=run_with_+RTS_-s poll_timeout_ms=1000
scenario=req-poll payload_bytes=64 frames=1 sockets=2 messages=5 elapsed_ms=0.189 throughput_msg_per_s=26385.642 latency_p50_us=23.440 latency_p95_us=26.947 latency_max_us=92.338 metadata_libzmq=4.3.4 metadata_compiler=ghc-9.14 metadata_os=linux metadata_arch=x86_64 rts_allocation_guidance=run_with_+RTS_-s receive_timeout_ms=1000
scenario=event-loop payload_bytes=64 frames=2 sockets=2 messages=5 elapsed_ms=6.609 throughput_msg_per_s=756.524 latency_p50_us=1330.439 latency_p95_us=1337.706 latency_max_us=1360.523 metadata_libzmq=4.3.4 metadata_compiler=ghc-9.14 metadata_os=linux metadata_arch=x86_64 rts_allocation_guidance=run_with_+RTS_-s mailbox_capacity=5
scenario=lifecycle payload_bytes=0 frames=0 sockets=10 messages=5 elapsed_ms=0.877 throughput_msg_per_s=5702.763 latency_p50_us=NA latency_p95_us=NA latency_max_us=NA metadata_libzmq=4.3.4 metadata_compiler=ghc-9.14 metadata_os=linux metadata_arch=x86_64 rts_allocation_guidance=run_with_+RTS_-s pending_before_cleanup=10
917,224 bytes allocated in the heap
```

Focused final smoke results:

```text
req-poll-idle messages=3 timeout_ms=10: elapsed_ms=30.553 latency_p50_us=10171.278 allocated=335,760 bytes
direct payload_bytes=65536 messages=5: elapsed_ms=0.615 latency_p50_us=121.022 allocated=1,102,808 bytes
multipart payload_bytes=65536 frames=4 messages=5: elapsed_ms=1.889 latency_p50_us=377.973 allocated=2,314,544 bytes
```

These values are smoke-quality only because the iteration counts are intentionally tiny and Cabal executable startup/RTS reporting can dominate total elapsed time. They verify benchmark wiring, metadata, representative paths, and allocation-summary capture.

## Remediation evidence summary

The TP-006 through TP-013 series produced the following comparable evidence. Treat these as task-local smoke/trend measurements, not universal guarantees.

| Area | Evidence summary | Interpretation |
| --- | --- | --- |
| Benchmark foundation (TP-006) | Added opt-in `zmqx-overheads` scenarios and `docs/performance.md`; initial all-scenario smoke passed with RTS output. | Provides a repeatable local harness rather than a hard performance gate. |
| Receive frame storage (TP-007) | Direct receive smoke 2.036ms / 3,529,848 RTS bytes before and 2.102ms / 3,695,752 after; multipart 3.862ms / 4,724,624 before and 4.088ms / 5,269,648 after. | The change removed foreign `zmq_msg_t` heap allocation/free churn, which RTS allocation summaries do not measure; payload copies remain by design. |
| Prepared poll sets (TP-008) | Sockets=32 poll smoke allocation fell from 8,920,824 to 4,944,064 bytes (~44.6% lower) with elapsed 72.790ms to 70.281ms. | Reusing prepared `Sockets` improves repeated poll sets; dynamically rebuilding large sets still pays preparation cost. |
| REQ poll probe work (TP-009) | Valid `req-poll` 100-message smoke allocation was roughly stable (873,168 to 870,968 bytes); direct all-REQ probe work was reduced within the retained 10ms fallback cadence. | Pure libzmq wakeups were not reliable enough for input `REQ` readiness, so the 10ms safety cadence remains visible in idle latency. |
| Receive-side expected EAGAIN path (TP-010) | `req-poll-idle --timeout-ms 0` smoke was 10.864ms / 6,581,912 bytes before and final reruns around 10.477-10.850ms / 6,578,864-6,582,496 bytes after. | The minimal negative-errno wrapper removes one receive-side FFI crossing for expected `EAGAIN`, but smoke timings are noisy; send-side HWM/EAGAIN remains future work. |
| Large multipart send path (TP-011) | Multipart 4x1MiB smoke improved from 179.991ms to 93.035ms; direct 1MiB sends were noisy/regressed when the message path was applied broadly, so direct/final frames remain on `zmq_send`. | The optimized path is intentionally limited to multipart prefix frames >=64KiB. True zero-copy over Haskell storage is not implemented. |
| EventLoop coordination (TP-012) | EventLoop 50-message smoke improved from elapsed 511.708ms / p50 10211us / 3,554,392 bytes to about elapsed 62.5ms / p50 1244-1255us / 1,066,000 bytes. | Public waits and worker slices are much lower-latency, but measurements remain end-to-end and include scheduling, poll slices, and mailbox delivery. |
| Lifecycle/finalizer registry (TP-013) | Lifecycle 400-socket smoke improved from 107.034ms to 76-83ms; allocation/residency rose modestly from registry state. | Exact pending counts and thresholded compaction reduce scan overhead; threshold tuning and compaction-specific interruption coverage remain future work. |

## Large send-path benchmark notes

TP-011 adds an internal large-frame send path for multipart prefix frames only: frames at or above 64 KiB that are sent with `ZMQ_SNDMORE` use a copy-backed `zmq_msg_init_data`/`zmq_msg_send` path, while single-frame sends, final multipart frames, and smaller frames remain on `zmq_send`. This keeps the public API unchanged and avoids the single-frame regression observed when the message path was tried for all large frames.

When checking this path, run both `direct` and `multipart` scenarios with `+RTS -s` and include payload sizes around the threshold as well as small and large frames, for example:

```sh
cabal run --enable-optimization=2 zmqx-overheads -- \
  --scenario direct --messages 100 --warmup 5 --payload-bytes 65536 +RTS -s
cabal run --enable-optimization=2 zmqx-overheads -- \
  --scenario multipart --messages 100 --warmup 5 --frames 4 --payload-bytes 65536 +RTS -s
```

Treat multipart large-payload improvements and direct single-frame noise separately because only multipart prefix frames exercise the optimized path.

## Remaining overheads and future work

Known unresolved work is tracked in `taskplane-tasks/CONTEXT.md`. The major remaining themes are:

- statistical/persisted benchmark output before adopting performance regression gates;
- receive and send true zero-copy ownership designs with explicit finalizer/lifetime tests;
- dynamic/prepared poll-set builder APIs for very large frequently rebuilt poll sets;
- REQ poll wakeup-source limitations and the retained 10ms safety cadence;
- EventLoop command wakeup fairness, timer strategy, and narrower instrumentation;
- lifecycle compaction threshold tuning and compaction-specific interruption coverage;
- send-side saturated/HWM benchmark coverage before changing send-side expected-error wrappers.

## Smoke versus regression guidance

- Use tiny runs (`--messages` 1-100) as smoke checks during development. They should pass reliably but are too noisy for performance conclusions.
- Use optimized runs with larger counts, stable local hardware, and the same RTS settings for trend/regression comparisons.
- Treat `+RTS -s` allocation totals as contextual evidence, not CI gates. Compare like-for-like commands and metadata.
- Remember that RTS allocation summaries do not capture all foreign heap effects; TP-007's receive container change is the main example.
- For large send-path checks, include both direct and multipart scenarios at small, threshold-adjacent, medium, and large payload sizes; compare multipart-prefix behavior separately from direct single-frame sends.
- EventLoop timings include worker scheduling, receiver polling slices, and mailbox delivery; use larger message counts before comparing changes.
- The lifecycle scenario times context cleanup/finalizer churn in aggregate and therefore reports latency fields as `NA`.
