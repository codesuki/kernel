#pragma once

// This probably cannot be an enum for long. Message types are probably pretty
// dynamic.
enum message_type {
  message_type_ps2_byte,
  mouse_byte,
  network_data,
  dns_request,
  dns_response,
};
typedef enum message_type message_type_t;

typedef struct message message;
struct message {
  message_type_t type;
  void* data;
  message* next;
};

bool message_peek(message* head);
void message_send(message** head, message_type_t type, void* data);
void message_receive(message** head, message* dst);
