#pragma once

#include "types.h"

#define HPET_CONFIG_REG 0x10
#define HPET_BASE 0xfed00000
#define HPET_MAIN_COUNTER 0xf0

void set_timer0();
u64 get_global_timer_value();
void setup_hpet();

// TODO: read timer resolution from config.
// The main timer tick interval is 10000000 (1e+7) femto seconds, read from
// config. This equals 10 nanoseconds. 1000000 (1e+6) nanoseconds are 1
// millisecond.
extern const u64 _1ms;// we need this many timer periods to have 1 ms
extern const u64 _1s;
