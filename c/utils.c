#include <stdlib.h>

// libzmq may invoke this callback from an arbitrary I/O thread after a successful zmq_msg_send.
// Keep it C-only and thread-safe: copy-backed large sends allocate with malloc, pass this callback to
// zmq_msg_init_data, and let libzmq call back when the queued frame buffer is no longer needed.
void free2 (void* data, void* hint) {
  free(data);
}
