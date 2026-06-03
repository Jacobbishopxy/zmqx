#include <zmq.h>

void zmq_atomic_counter_destroy_wrapper (void* counter);
int zmqx_msg_recv_errno (zmq_msg_t* msg, void* socket, int flags);
