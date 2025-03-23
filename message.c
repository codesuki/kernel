#include "message.h"

#include "memory.h"
#include "print.h"
#include "task.h"
#include "time.h"
#include "types.h"

const u8 message_type_count = 2;

// The interrupt handler should either start this task with high priority OR
// this task already exists and waits for the interrupt blocking. If this is
// late then we will see new data/lose old data. Hence it's important how often
// the OS calls this handler. If it's too slow I could imagine the mouse pointer
// jumping around.
//
// Many applications may wait for mouse/keyboard input, how is that handled? The
// OS/driver could read state from the input device and has a list of whom to
// notify. Maybe in our case we don't notify, yet, we just update globals and
// call display?
//
// The ps/2 controller sends one interrupt per data byte. The mouse sends 3
// bytes so there will be 3 interrupts. Just reading three bytes here does not
// guarantee they are from the mouse, could be a keyboard byte too. We rely on
// the IRQ to know which device the data comes from. Therefore we need to track
// how many bytes we read so far.
// ref: https://forum.osdev.org/viewtopic.php?t=36691&start=15
// ref: https://www.reddit.com/r/osdev/comments/ld8ril/comment/gm6rfx5/
//
// Actually we can see if a byte came from the keyboard or the mouse by checking
// the PS/2 controller output port by sending the 0xD0 command.
//
// Bit 4 is set if the data came from port 1 (kbd)
// Bit 5 is set if the data came from port 2 (mouse)
//
// The first byte we receive has overflow bits. If they are set we should
// probably ignore the corresponding packet or check if the mouse is still
// usable.

// Message passing
//
// Message
// Receiver (?) I don't think I need this. All receivers are registered.
// Type
// Data
//
// handle = Register(type) ?
// message = Wait(handle) ?
// Why did I randomly add handle here? Because there needs to be some connection
// between registering and waiting
//
// Send(type, data)
//

// message* message_first = nullptr;

// TODO: maybe it's time to abstract a queue..

// message_peek returns true if there are messages waiting.
// TODO: what's the naming here? queue, head, first?
bool message_peek(message* head) {
  // if length of queue > 0 return true
  if (head != nullptr) {
    return true;
  } else {
    return false;
  }
}

// Let's do point to point first.
// Let's replace 'wait_for_mouse_data'.
void message_send(message** head, message_type_t type, void* data) {
  // printf("message_send\n");
  // printf("message_send %d\n", message_peek(*head));
  //  alloc message or get from pool.
  //  add to queue
  message* msg = malloc(sizeof(*msg));
  msg->next = nullptr;
  msg->type = type;
  msg->data = data;

  if (*head == nullptr) {
    // TODO: here we probably need pointer to pointer.
    *head = msg;
    return;
  }

  // TODO: keep a pointer to tail for faster append.
  message* m = *head;
  for (; m->next != nullptr; m = m->next) {
  }
  m->next = msg;
}

// mssage_receive checks if there is a message and if not sets the task to a
// waiting state and calls the scheduler. The scheduler wakes it up in case
// there is data waiting, which will restart the loop.
void message_receive(message** head, message* dst) {
  while (true) {
    // TODO: there is a risk to dereference a null pointer here.
    if (*head != nullptr) {
      message* m = *head;
      *head = m->next;
      dst->type = m->type;
      dst->data = m->data;
      free(m);
      return;
    }
    task_current->state = blocked;
    switch_task(task_current, task_scheduler);
    // We resume here
    // Check if we got woken up because of a timeout.
    // TODO: is there a better way?
    // printf("message_receive: waking up\n");
    if (task_current->timed_out == true) {
      // printf("message_receive: timed out\n");
      task_current->timed_out = false;
      dst = nullptr;
      return;
    }
  }
}

void message_receive_timeout(message** head, message* dst, u64 timeout) {
  task_current->timeout = get_global_timer_value() + timeout * _1ms;
  message_receive(head, dst);
}

message* message_type_registry[2];

void message_register(message_type_t type, message* queue) {}
