#pragma once

#include "types.h"

#define RTL8139_MAC 0x0
#define RTL8139_MAR 0x8
#define RTL8139_RBSTART 0x30
#define RTL8139_CMD 0x37
#define RTL8139_IMR 0x3c
#define RTL8139_ISR 0x3e

#define RTL8139_CONFIG_1 0x52

#define PCI_CONFIG_ADDRESS 0xcf8
#define PCI_CONFIG_DATA 0xcfc

typedef union {
  u32 raw;
  struct {
    u32 offset : 8;
    u32 function : 3;
    u32 device : 5;
    u32 bus : 8;
    u32 reserved : 7;
    u32 enabled : 1;
  } bits;
} pci_config_address_t;

typedef union {
  u32 raw;
  struct {
    u16 vendor_id;
    u16 device_id;
  } __attribute__((packed)) fields;
} pci_config_register_0_t;

typedef union {
  u32 raw;
  struct {
    union {
      u16 command;
      struct {
	u16 io_space : 1;
	u16 memory_space : 1;
	u16 bus_master : 1;
	u16 reserved : 13;
      } command_bits;
    };
    union {
      u16 status;
      struct {
	u16 reserved : 16;
      } status_bits;
    };
  } __attribute__((packed)) fields;
} pci_config_register_1_t;

typedef union {
  u32 raw;
  struct {
    u8 revision_id;
    u8 prog_if;
    u8 subclass;
    u8 class;
  } __attribute__((packed)) fields;
} pci_config_register_2_t;

typedef union {
  u32 raw;
  struct {
    u8 cache_line_size;
    u8 latency_timer;
    u8 header_type;
    u8 bist;
  } __attribute__((packed)) fields;
} pci_config_register_3_t;

typedef union {
  u32 raw;
  struct {
    u32 is_io_space : 1;
    u32 type : 2;
    u32 prefetchable : 1;
    u32 address : 28;
  } __attribute__((packed)) memory_space;
  struct {
    u32 is_io_space : 1;
    u32 reserved : 1;
    u32 address : 30;
  } __attribute__((packed)) io_space;
} pci_config_register_4_t;

typedef union {
  u32 raw;
  struct {
    u32 is_io_space : 1;
    u32 type : 2;
    u32 prefetchable : 1;
    u32 address : 28;
  } __attribute__((packed)) memory_space;
  struct {
    u32 is_io_space : 1;
    u32 reserved : 1;
    u32 io_size : 6;
    u32 address : 24;
  } __attribute__((packed)) io_space;
} pci_config_register_4_rtl8139_t;

typedef union {
  u32 raw;
  struct {
    u8 interrupt_line;
    u8 interrupt_pin;
    u8 min_grant;
    u8 max_latency;
  } __attribute__((packed)) fields;
} pci_config_register_f_t;

void pci_enumerate();
void handle_network_interrupt();
void net_transmit(void* data, u32 length);

extern u8 mac[6];
