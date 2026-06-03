# FFI/C-boundary performance overhead findings

Scope inspected: `c/`, `lib/Zmqx/Internal/*`, and Core/socket call sites. Findings are prioritized by likely impact on hot send/receive/poll paths.

## P1 — Receive path copies every payload and allocates/frees a `zmq_msg_t` per frame

- References: `lib/Zmqx/Core/Socket.hs` lines 529-593; `lib/Zmqx/Internal/Functions.hs` lines 163-167, 197-201, 241-243, 266-270; `lib/Zmqx/Internal/Bindings/Functions.hs` lines 78-91, 108-127, 138-145.
- Why overhead: each frame receive does `malloc` + `zmq_msg_init`, `zmq_msg_recv`, then `zmq_msg_size`, `zmq_msg_data`, `ByteString.packCStringLen` (payload copy), `zmq_msg_more`, `zmq_msg_close`, and `free`. Multi-frame receives recurse and repeat this per frame.
- Compared with direct libzmq: a direct C loop could stack-allocate/reuse `zmq_msg_t`, fuse recv/data/size/more handling, or receive into reusable buffers where appropriate.
- Confidence: high.
- Benchmark/profiling: inproc PUSH/PULL or DEALER/ROUTER, measure msgs/sec and allocations for 1-byte, 64-byte, 1 KiB, and 64 KiB frames; compare current receive path against a temporary C/Haskell FFI microbenchmark that reuses one `zmq_msg_t` and records copies/allocation via `+RTS -s`, eventlog, and `perf record`.

## P1 — Send path uses `zmq_send` from `ByteString`; zero-copy APIs are exposed but not used by socket call sites

- References: `lib/Zmqx/Core/Socket.hs` lines 203-275, 491-522; role call sites such as `lib/Zmqx/Dealer.hs` lines 111-132, `lib/Zmqx/Pub.hs` lines 125-148, `lib/Zmqx/Rep.hs` lines 110-123, `lib/Zmqx/Req.hs` lines 129-152; `lib/Zmqx/Internal/Functions.hs` lines 411-452.
- Why overhead: high-level sends pass a `ByteString` pointer with `unsafeUseAsCStringLen` to `zmq_send`/`zmq_send__unsafe`; libzmq can copy user memory into its message/queue. This is one FFI call per frame and one potential payload copy per frame.
- Zero-copy caveat: `zmq_send_const` and `zmq_msg_init_data` are exported (`lib/Zmqx/Internal/Functions.hs` lines 208-214, 425-437), but not used outside Internal; `zmq_msg_init_data` currently first `mallocBytes` + `copyBytes`, so it is not zero-copy from the original `ByteString`.
- Confidence: high.
- Benchmark/profiling: compare current `send`/`sends` against a prototype using `zmq_msg_init_data` with a retained `ForeignPtr` finalizer or `zmq_send_const` for large immutable buffers. Sweep message sizes; look for CPU cycles/byte and allocation reductions.

## P2 — Blocking semantics are implemented as nonblocking retry plus `ZMQ_FD`/`ZMQ_EVENTS`, adding extra FFI/syscalls under backpressure or empty receives

- References: send/receive loops in `lib/Zmqx/Core/Socket.hs` lines 282-340; readiness wait in lines 370-377; common role loops in `lib/Zmqx/Dealer.hs` lines 111-132 and `lib/Zmqx/Req.hs` lines 129-152.
- Why overhead: on EAGAIN, paths do a failed `zmq_msg_recv__unsafe`/`zmq_send__unsafe`, call `zmq_errno`, then fetch `ZMQ_FD`, wait in the RTS, fetch `ZMQ_EVENTS`, and retry. Direct blocking `zmq_msg_recv`/`zmq_send` would be fewer boundary crossings, though it would require safe/interruptible FFI and affects scheduler behavior.
- Confidence: medium-high.
- Benchmark/profiling: test no-message receive latency and saturated HWM send latency with current path vs direct `interruptible zmq_msg_recv/zmq_msg_send`; include multi-thread RTS (`-N`) to quantify scheduler impact.

## P2 — `poll` rebuilds and re-peeks polling structures on every call

- References: `lib/Zmqx/Core/Poll.hs` lines 143-194, 202-235, 289-350; `lib/Zmqx/Internal/Functions.hs` lines 618-635; `lib/Zmqx/Internal/Bindings/Types.hsc` lines 46-68.
- Why overhead: every poll builds lists/sets, allocates a `StorableArray`, maps sockets to `zmq_pollitem_t`, then after `zmq_poll` peeks each item into a Haskell record and builds a `Set`. For small timeouts or event-loop workloads, this Haskell marshalling can dominate the actual `zmq_poll` call.
- Extra REQ overhead: input REQ sockets are probed with `receiveManyDontWait`, which can do full message receives/copies just to determine readiness (`lib/Zmqx/Core/Poll.hs` lines 216-235, 323-350).
- Confidence: high for frequent polling; medium for long blocking polls.
- Benchmark/profiling: poll 1, 4, 16, 128 sockets with timeout 0 and 1ms; measure allocations and per-poll ns. Compare reusable `ForeignPtr`/pinned array of `zmq_pollitem_t` and a bitmap/list result instead of `Set`.

## P2 — Safe/interruptible FFI imports on potentially hot blocking operations carry RTS transition overhead

- References: `lib/Zmqx/Internal/Bindings/Functions.hs` lines 138-157 (`zmq_msg_recv/send`), 228-247 (`zmq_send`/`zmq_send_const`), 273-280 (`zmq_poll`), plus bind/connect/monitor lines 187-211.
- Why overhead: `interruptible` FFI is appropriate for calls that can block, but each call has materially higher overhead than `unsafe`. The code already uses unsafe variants for DONTWAIT and timeout-0 poll; any hot path that reaches `zhs_send_frame` uses interruptible `zmq_send` (`lib/Zmqx/Core/Socket.hs` lines 475-489).
- Confidence: medium.
- Benchmark/profiling: microbenchmark raw unsafe DONTWAIT, interruptible blocking, and retry+FD wait variants. Use `perf`/eventlog to separate Haskell RTS transition cost from libzmq wait time.

## P3 — Error handling adds extra FFI calls on expected EAGAIN/probe failures

- References: wrappers call `zmq_errno` on `-1` in `lib/Zmqx/Internal/Functions.hs` lines 257-270, 445-448, 623-630; high-level classifiers in `lib/Zmqx/Core/Socket.hs` lines 491-506 and 529-543; `enrichError` calls `zmq_strerror` in `lib/Zmqx/Error.hs` lines 34-36.
- Why overhead: every expected EAGAIN from nonblocking send/receive requires at least one additional FFI call to `zmq_errno`. Real errors also convert errno to text via `zmq_strerror` and UTF-8 decoding. Laziness avoids `strerror` for some EAGAIN branches, but not the extra errno boundary crossing.
- Confidence: high for EAGAIN-heavy workloads.
- Benchmark/profiling: measure tight `receiveOneDontWait` on an empty socket and saturated nonblocking sends; compare to a tiny C wrapper returning `{rc, errno}` in one FFI call.

## P3 — Monitor handling goes through normal multipart receive and copies/decodes frames even though only 6 bytes are needed

- References: `lib/Zmqx/Core/Monitor.hs` lines 34-45 and 97-186; receive implementation reused from `lib/Zmqx/Core/Socket.hs` lines 328-350 and 588-593.
- Why overhead: monitor events arrive as two frames; the endpoint frame is copied then ignored, and the 6-byte event frame is copied into a `ByteString` before bytewise decode. This is fine for normal monitor volume but expensive if monitoring high-churn sockets.
- Confidence: medium.
- Benchmark/profiling: stress connect/disconnect events and compare current `Pair.receives` monitor path with a specialized monitor receive that parses `zmq_msg_data` before copying and drops endpoint without packing.

## P4 — Low-impact C wrappers/callbacks

- References: `c/utils.c` lines 12-16 and `lib/Zmqx/Internal/Functions.hs` lines 208-222; `c/zmq-wrapper.c` lines 1-11 and `lib/Zmqx/Internal/Bindings/Functions.hs` lines 335-370.
- Why overhead: `free2` is a C callback used by `zmq_msg_init_data`, but that path already copies into malloc-owned memory and is not used by high-level sockets. The atomic counter destroy wrapper is one extra C function call, likely not hot.
- Confidence: high that these are not primary bottlenecks.
- Benchmark/profiling: only benchmark if atomic counters become hot or if `zmq_msg_init_data` is promoted to the send path; otherwise prioritize receive/send/poll first.

## Other note

- `lib/Zmqx/Internal/Functions.hs` lines 248-250 defines `zmq_msg_move` but calls `zmq_msg_copy`; if this wrapper becomes used, it would add copying and different lifecycle semantics vs `zmq_msg_move`. Currently grep found no high-level use.
