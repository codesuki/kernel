#pragma once

#include "types.h"

// This probably cannot be an enum for long. Message types are probably pretty
// dynamic. Prefixing the types makes them extremely long and not doing it
// causes many shadowing issues. Naming struct types uppercase would solve it to
// some degree. Or adding _t which is reserved for POSIX. Not that it matters in
// a non-POSIX kernel.
enum message_type {
  message_type_ps2_byte,
  mouse_byte,
  network_data,
  dns_request,
  dns_response,
  message_type_dhcp_response,
};
typedef enum message_type message_type_t;

typedef struct message message;
struct message {
  message_type_t type;
  void* data;
  message* next;
};

// TODO: add a constructor/factory
// If head is not initialized to nullptr everything crashes.
struct message_queue {
  message** head;
};
typedef struct message_queue message_queue;

bool message_peek(message_queue* queue);
void message_send(message_queue* queue, message_type_t type, void* data);
void message_receive(message_queue* queue, message* dst);
void message_receive_timeout(message_queue* queue, message* dst, u64 timeout);
