#pragma once

#include "pci.h"
#include "types.h"

#define RTL8139_MAC 0x0
#define RTL8139_MAR 0x8
#define RTL8139_RBSTART 0x30
#define RTL8139_CMD 0x37
#define RTL8139_IMR 0x3c
#define RTL8139_ISR 0x3e

#define RTL8139_CONFIG_1 0x52

void rtl8139_configure(pci_config_address a);
void handle_network_interrupt();
void net_transmit(void* data, u32 length);

extern u8 mac[6];
