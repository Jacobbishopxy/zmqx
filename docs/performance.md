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
| `req-poll` | `REQ` timeout/probe behavior through `receivesFor` | `--messages`, `--payload-bytes`, `--timeout-ms` |
| `req-poll-idle` | Idle `REQ` input `pollFor` timeout/probe behavior with a ROUTER peer that does not reply | `--messages`, `--payload-bytes`, `--timeout-ms` |
| `event-loop` | EventLoop mailbox/transceiver send/receive latency | `--messages`, `--payload-bytes`, `--frames`, `--timeout-ms` |
| `lifecycle` | Socket open/finalizer/context cleanup churn | `--messages` |

Use `--scenario all` (the default) to run the standard smoke matrix. The timeout-dominated `req-poll-idle` scenario is intentionally explicit-only so default/all runs do not spend `messages * timeout-ms` waiting for idle timeouts.

## Example commands

Small smoke run:

```sh
cabal run zmqx-overheads -- --scenario all --messages 5 --warmup 1 --sockets 2 --frames 2 --payload-bytes 64
```

Optimized smoke run with RTS allocation summary:

```sh
cabal run --enable-optimization=2 zmqx-overheads -- \
  --scenario all --messages 5 --warmup 1 --sockets 2 --frames 2 --payload-bytes 64 +RTS -s
```

Larger trend/regression run for one scenario:

```sh
cabal run --enable-optimization=2 zmqx-overheads -- \
  --scenario poll --messages 10000 --warmup 100 --sockets 32 --payload-bytes 64 +RTS -s
```

REQ idle poll/probe smoke with a short timeout:

```sh
cabal run --enable-optimization=2 zmqx-overheads -- \
  --scenario req-poll-idle --messages 10 --warmup 1 --timeout-ms 10 --payload-bytes 64 +RTS -s
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

## Baseline smoke sample

On 2026-06-02, the following command passed on this lane worktree with libzmq `4.3.4`, GHC `9.14`, Linux x86_64:

```sh
cabal run --enable-optimization=2 zmqx-overheads -- \
  --scenario all --messages 5 --warmup 1 --sockets 2 --frames 2 --payload-bytes 64 +RTS -s
```

Representative output from that smoke run:

```text
scenario=direct payload_bytes=64 frames=1 sockets=2 messages=5 elapsed_ms=0.030 throughput_msg_per_s=168361.506 latency_p50_us=1.686 latency_p95_us=3.298 latency_max_us=5.038 metadata_libzmq=4.3.4 metadata_compiler=ghc-9.14 metadata_os=linux metadata_arch=x86_64 rts_allocation_guidance=run_with_+RTS_-s
scenario=multipart payload_bytes=64 frames=2 sockets=2 messages=5 elapsed_ms=0.049 throughput_msg_per_s=101173.614 latency_p50_us=6.580 latency_p95_us=14.566 latency_max_us=19.586 metadata_libzmq=4.3.4 metadata_compiler=ghc-9.14 metadata_os=linux metadata_arch=x86_64 rts_allocation_guidance=run_with_+RTS_-s
scenario=poll payload_bytes=64 frames=1 sockets=2 messages=5 elapsed_ms=0.182 throughput_msg_per_s=27541.078 latency_p50_us=30.103 latency_p95_us=41.903 latency_max_us=47.160 metadata_libzmq=4.3.4 metadata_compiler=ghc-9.14 metadata_os=linux metadata_arch=x86_64 rts_allocation_guidance=run_with_+RTS_-s poll_timeout_ms=1000
scenario=req-poll payload_bytes=64 frames=1 sockets=2 messages=5 elapsed_ms=0.192 throughput_msg_per_s=26052.929 latency_p50_us=24.433 latency_p95_us=29.410 latency_max_us=88.927 metadata_libzmq=4.3.4 metadata_compiler=ghc-9.14 metadata_os=linux metadata_arch=x86_64 rts_allocation_guidance=run_with_+RTS_-s receive_timeout_ms=1000
scenario=event-loop payload_bytes=64 frames=2 sockets=2 messages=5 elapsed_ms=51.196 throughput_msg_per_s=97.664 latency_p50_us=10236.341 latency_p95_us=10242.881 latency_max_us=10275.041 metadata_libzmq=4.3.4 metadata_compiler=ghc-9.14 metadata_os=linux metadata_arch=x86_64 rts_allocation_guidance=run_with_+RTS_-s mailbox_capacity=5
scenario=lifecycle payload_bytes=0 frames=0 sockets=10 messages=5 elapsed_ms=0.949 throughput_msg_per_s=5268.587 latency_p50_us=NA latency_p95_us=NA latency_max_us=NA metadata_libzmq=4.3.4 metadata_compiler=ghc-9.14 metadata_os=linux metadata_arch=x86_64 rts_allocation_guidance=run_with_+RTS_-s pending_before_cleanup=10
1,442,696 bytes allocated in the heap
```

These values are smoke-quality only because the iteration count is intentionally tiny. They verify wiring, metadata, and RTS allocation capture.

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

## Smoke versus regression guidance

- Use tiny runs (`--messages` 1-100) as smoke checks during development. They should pass reliably but are too noisy for performance conclusions.
- Use optimized runs with larger counts, stable local hardware, and the same RTS settings for trend/regression comparisons.
- Treat `+RTS -s` allocation totals as contextual evidence, not CI gates. Compare like-for-like commands and metadata.
- For large send-path checks, include both direct and multipart scenarios at small, threshold-adjacent, medium, and large payload sizes; compare multipart-prefix behavior separately from direct single-frame sends.
- EventLoop timings include worker scheduling, receiver polling slices, and mailbox delivery; use larger message counts before comparing changes.
- The lifecycle scenario times context cleanup/finalizer churn in aggregate and therefore reports latency fields as `NA`.
