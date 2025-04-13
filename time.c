#include "time.h"
#include "memory.h"
#include "print.h"
#include "types.h"

// TODO: read timer resolution from config.
// The main timer tick interval is 10000000 (1e+7) femto seconds, read from
// config. This equals 10 nanoseconds. 1000000 (1e+6) nanoseconds are 1
// millisecond.
const u64 _1ms = 100000;  // we need this many timer periods to have 1 ms
const u64 _1s = _1ms * 1000;

void set_timer0() {
  // main counter 0xf0
  u64* counter_value =
      (u64*)(physical2virtual((void*)HPET_BASE) + HPET_MAIN_COUNTER);
  // TODO: crash because of large number
  // printf("hpet: counter: %d", *counter_value);

  // comparator timer 0 0x108
  // printf("hpet: setting timer to %x %d\n", _1s, _1s);
  u64* comparator_0 = (u64*)(physical2virtual((void*)HPET_BASE) + 0x108);
  // TODO: how is wrapping handled? By GPE :D
  *comparator_0 = *counter_value + _1ms * 10;

  // TODO: this crashes when wrapping 64bit value. Probably formatting code
  // wrong.
  // printf("hpet: set timer to %x %d\n", *comparator_0, *comparator_0);
}

u64 get_global_timer_value() {
  return *(u64*)(physical2virtual((void*)HPET_BASE) + HPET_MAIN_COUNTER);
}

void setup_hpet() {
  // Timer 0: 100h – 107h, Timer 1: 120h – 127h, Timer 2: 140h – 147h
  u32* available_interrupts =
      (u32*)(physical2virtual((void*)HPET_BASE) + 0x104);
  printf("hpet: interrupts timer 0: %x\n", *available_interrupts);

  u32* timer_0 = (u32*)(physical2virtual((void*)HPET_BASE) +
			0x100);  // set bit 2 and maybe 9-13
  printf("hpet: configured interrupt: %x\n", ((*timer_0) >> 9) & 31);
  *timer_0 = (*timer_0) | (1 << 2) | (4 << 9);
  printf("hpet: configured interrupt: %x\n", ((*timer_0) >> 9) & 31);

  u32* timer_period = (u32*)(physical2virtual((void*)HPET_BASE) + 0x4);
  printf("hpet: femto: %d\n", *timer_period);

  // main counter 0xf0
  u64* counter_value =
      (u64*)(physical2virtual((void*)HPET_BASE) + HPET_MAIN_COUNTER);
  printf("hpet: counter: %d\n", *counter_value);

  // comparator timer 0 0x108
  printf("hpet: setting timer to %x %d\n", _1s, _1s);
  u64* comparator_0 = (u64*)(physical2virtual((void*)HPET_BASE) + 0x108);
  *comparator_0 = _1s;
  printf("hpet: set timer to %x %d\n", *comparator_0, *comparator_0);

  // bit 0 is enable flag
  u32* c = (u32*)(physical2virtual((void*)HPET_BASE) + HPET_CONFIG_REG);
  printf("hpet: config %x\n", *c);
  *c = (*c) | 0x1;
  printf("hpet: config %x\n", *c);
  printf("hpet: counter: %d\n", *counter_value);
}
