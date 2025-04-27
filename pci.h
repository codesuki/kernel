#pragma once

#include "types.h"

#define PCI_CONFIG_ADDRESS 0xcf8
#define PCI_CONFIG_DATA 0xcfc

#define PCI_VENDOR_INTEL 0x8086
#define PCI_VENDOR_BOCHS 0x1234
#define PCI_VENDOR_REALTEK 0x10ec

#define PCI_DEVICE_RTL8139 0x8139

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
} pci_config_address;

typedef union {
  u32 raw;
  struct {
    u16 vendor_id;
    u16 device_id;
  } __attribute__((packed)) fields;
} pci_config_register_0;

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
} pci_config_register_1;

typedef union {
  u32 raw;
  struct {
    u8 revision_id;
    u8 prog_if;
    u8 subclass;
    u8 class;
  } __attribute__((packed)) fields;
} pci_config_register_2;

typedef union {
  u32 raw;
  struct {
    u8 cache_line_size;
    u8 latency_timer;
    u8 header_type;
    u8 bist;
  } __attribute__((packed)) fields;
} pci_config_register_3;

typedef union {
  u32 raw;
  struct {
    u32 is_io_space : 1;
    u32 type : 2;  // 0 = 32 bit, 1 = reserved, 2 = 64 bit (uses two BARs)
    u32 prefetchable : 1;
    u32 address : 28;
  } __attribute__((packed)) memory_space;
  struct {
    u32 is_io_space : 1;
    u32 reserved : 1;
    u32 address : 30;
  } __attribute__((packed)) io_space;
} pci_config_register_4;

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
} pci_config_register_4_rtl8139;

typedef union {
  u32 raw;
  struct {
    u8 interrupt_line;
    u8 interrupt_pin;
    u8 min_grant;
    u8 max_latency;
  } __attribute__((packed)) fields;
} pci_config_register_f;

void pci_enumerate();
