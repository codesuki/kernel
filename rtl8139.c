#include "rtl8139.h"

#include "io.h"
#include "lib.h"
#include "memory.h"
#include "message.h"
#include "print.h"
#include "task.h"
#include "types.h"

#define RX_BUFFER_SIZE 8192
// 00 = 8k + 16 byte
// wrap bit = 1
//
// When set to 1: The RTL8139D(L) will keep moving the rest of the packet data
// into the memory immediately after the end of the Rx buffer, if this packet
// has not been completely moved into the Rx buffer and the transfer has arrived
// at the end of the Rx buffer. The software driver must reserve at least 1.5K
// bytes buffer to accept the remainder of the packet. We assume that the
// remainder of the packet is X bytes. The next packet will be moved into the
// memory from the X byte offset at the top of the Rx buffer.
u8* network_rx_buffer = nullptr;
//[RX_BUFFER_SIZE + 16 + 1500]
//    __attribute__((aligned(4))) = {0};
u16 network_rx_buffer_index = {0};

u8* network_tx_buffer = nullptr;
u8 network_current_tx_descriptor = 0;

u16 num_packets = 0;

// Now it's starting to get super dirty.
u32 base = 0;

u8 mac[6] = {0};

// Is this the network driver? No. The network driver would fetch data from the
// card and pass it up the network stack.
// This is huge. Maybe I should start with the mouse driver?
void handle_network_interrupt() {
  /*
    packet header from network card
    ref:
    https://www.cs.usfca.edu/~cruse/cs326f04/RTL8139_ProgrammersGuide.pdf

    Bit R/W Symbol Description

    15 R MAR Multicast Address Received: Set to 1 indicates that a multicast
    packet is received.

    14 R PAM Physical Address Matched: Set to 1 indicates that the
    destination address of this packet matches the value written in ID
    registers.

    13 R BAR Broadcast Address Received: Set to 1 indicates that a broadcast
    packet is received. BAR, MAR bit will not be set simultaneously. 12-6 -
    - Reserved

    5 R ISE Invalid Symbol Error: (100BASE-TX only) An invalid symbol was
    encountered during the reception of this packet if this bit set to 1.

    4 R RUNT Runt Packet Received: Set to 1 indicates that the received
    packet length is smaller than 64 bytes ( i.e. media header + data + CRC
    < 64 bytes )

    3 R LONG Long Packet: Set to 1 indicates that the size of the received
    packet exceeds 4k bytes.

    2 R CRC CRC Error: When set, indicates that a CRC error occurred on the
    received packet.

    1 R FAE Frame Alignment Error: When set, indicates that a frame
    alignment error occurred on this received packet.

    0 R ROK Receive OK: When set, indicates that a good packet is received.
   */

  /*
    The receive path of RTL8139(A/B) is designed as a ring buffer. This ring
    buffer is in a physical continuous memory. Data coming from line is
    first stored in a Receive FIFO in the chip, and then move to the receive
    buffer when the early receive threshold is met. The register CBA keeps
    the current address of data moved to buffer. CAPR is the read pointer
    which keeps the address of data that driver had read. The status of
    receiving a packet is stored in front of the packet(packet header).
   */

  u16 isr = inw(base + 0x3e);
  // printf("isr: %x\n", isr);
  // although the docs say we only need to read, we actually need to write
  // to reset
  //   printf("resetting\n");
  outw(base + 0x3e, isr);

  if (isr & 0x1) {
    // printf("isr: Rx OK\n");
  } else if (isr & 0x4) {
    // printf("isr: Tx OK");
    goto eth_return;
  } else {
    goto eth_return;
  }

#define NET_BIT_BUFFER_EMPTY 0x1
  while (true) {
    u8 cmd = inb(base + 0x37);
    if (cmd & NET_BIT_BUFFER_EMPTY) {  // Buffer Empty = 1
      printf("network: buffer empty\n");
      break;
    }

    u16 capr = inw(base + 0x38);
    // printf("capr: %x\n", capr);

    u16 cbr = inw(base + 0x3a);
    // printf("cbr: %x\n", cbr);

    // RX buffer content
    // packet header | packet length | ethernet frame
    // 2 bytes         | 2 bytes        |

    // ethernet frame
    // MAC dest | MAC src | Tag (optional) | EtherType / length | Payload |
    // CRC/FCS 6 bytes   | 6 bytes | 4 bytes          | 2 bytes | 42–1500  |
    // 4 bytes

    // ether type
    // 0x0800	Internet Protocol version 4 (IPv4)
    // 0x86DD	Internet Protocol Version 6 (IPv6)
    // 0x0806      ARP

    // we get 0x45 = 0b0100 0101 in big endian / network byte order
    // I assume we get protocol ipv4 so the left part is the 4 and right is
    // 5 which is the header size of a header without options.

    // Note: network byte order

    // TODO: print 16 bytes before this to see what the card puts there. Since
    // we emulate it's probably empty.

    u16* packet_status = (u16*)(network_rx_buffer + network_rx_buffer_index);
    printf("network: packet status = %d\n", *packet_status);
    //  printf("network interrupt: num: %d, status: %x, buffer index: %x\n",
    //	     num_packets++, *packet_status, network_rx_buffer_index);
    printf("network_rx_buffer: %x networ_rx_buffer_index: %d\n",
	   network_rx_buffer, network_rx_buffer_index);

    // TODO: signal this error to network service.
    // CRC, RUNT, LONG, FAE, BAD SYMBOL errors
    if (*packet_status & (1 << 1) || *packet_status & (1 << 2) ||
	*packet_status & (1 << 3) || *packet_status & (1 << 4) ||
	*packet_status & (1 << 5)) {
      printf("network packet 3.error\n");
      break;
    }

    if ((*packet_status & 0b1) != 0b1) {
      printf("network: ROK not set in packet status\n");
      break;
    }

    u16* length = (u16*)(network_rx_buffer + network_rx_buffer_index +
			 2);  // first two bytes are the rx header

    //  printf("length: %d\n", *length);

    // TODO: pull length and etheretype from this.
    // first two bytes are the rx header followed by 2 bytes for length
    void* ef = network_rx_buffer + network_rx_buffer_index + 4;

    // Copy packet and send it to network service.
    // The network card appends a 4 byte CRC at the end which we ignore.
    void* buffer = malloc(*length - 4);
    memcpy(ef, buffer, *length - 4);
    message_send(&service_network->queue, network_data, buffer);

    //     printf("network: idx before: %x\n", network_rx_buffer_index);

    // length seems to not include the header and size which are 2 bytes each
    network_rx_buffer_index += *length + 4;

    // wrap
    if (network_rx_buffer_index > RX_BUFFER_SIZE) {
      // the controller expects us to not reset it to 0 for some reason.
      network_rx_buffer_index -= RX_BUFFER_SIZE;
    }

    // it seems we need to align on dword boundaries.
    // this means the first 2 bits should be 0, because 1 2 or 3 would not
    // be 4 which is dword. just cutting them off would mean we inside are
    // the packet we just read. is this bad? the programming guide adds 3 to
    // make sure we are outside.

    // according to qemu code there is a 4byte checksum
    // https://github.com/qemu/qemu/blob/master/hw/net/rtl8139.c#L1161-L1165

    // ~ is the complement meaning all 1s except the first 2 bits.
    network_rx_buffer_index = (network_rx_buffer_index + 3) & ~3;
    // Result: network_rx_buffer_index == cbr in the first run. Later cbr
    // grows much faster. After looping until BUFFER_EMPTY == 1, CRB always
    // matches with network_rx_buffer_index. The - 0x10 adjustment was also
    // necessary. Otherwise BUFFER_EMPTY never gets set.
    // ref: https://github.com/qemu/qemu/blob/master/hw/net/rtl8139.c#L1384

    // printf("new buffer index: %x %d\n", network_rx_buffer_index,
    //	     network_rx_buffer_index);

    // the doc says to subtract 0x10 to avoid overflow. also, given we use
    // network_rx_buffer_index which is now bigger, won't this break us
    // reading packets? for some reason this is added back
    // https://github.com/qemu/qemu/blob/master/hw/net/rtl8139.c#L2522 so if
    // if we don't subtract the numbers don't match. who knows why this is
    // done.
    // Thinking and researching more about this I came to the conclusion that
    // the network card must somehow reserve 16 byte for some kind of header.
    outw(base + 0x38, network_rx_buffer_index - 0x10);
  }
eth_return:
  return;
}

// TODO: can this be async?
// We probably get an interrupt that tells us TX was done.
// BUG:
// race:
// task 1 calls net_transmit, and calls the first outl,
// network_current_tx_descriptor is 1 task 1 gets pre-empted task 2 calls
// net_transmit, and calls the first outl for a different packet
//
// And many more possibilities.
// Packets need to be queued up and one process puts them onto the network card,
// or we need a lock.
// While I can solve it with the message queue maybe now I can implement a lock?
//
// acquire
// blocks until acquired
// so what is a lock actually that we can wake on it changing?
// simplest we could just sleep 100ms or and try again.
//
// release
//
// most brute force solution is to disable interrupts
void net_transmit(void* data, u32 length) {
  memcpy(data, network_tx_buffer, length);
  //__asm__("cli");
  // set address to descriptor
  // set size
  // set 0 to own
  // printf("network: tx: using descriptor %d\n",
  // network_current_tx_descriptor);
  outl(base + 0x20 + network_current_tx_descriptor * 4,
       (u32)virtual2physical(network_tx_buffer));
  // u32 a = inl(base + 0x20);
  // printf("TX addr: %x\n", a);
  // bit 0-12 = size, bit 13 = own
  outl(base + 0x10 + network_current_tx_descriptor * 4, length);
  // wait for TOK
  while (inl(base + 0x10 + network_current_tx_descriptor * 4) &
	 (1 << 15) == 0) {
  }

  network_current_tx_descriptor = ++network_current_tx_descriptor % 4;
  //__asm__("sti");
}

// PCI vendor
// - device
// 0x8086 Intel
// - 0x1237 440FX - 82441FX PMC [Natoma]
// - 0x7000 82371SB PIIX3 ISA [Natoma/Triton II]
// 0x1234 Bochs
// - 0x1111 Bochs Graphic Adapter
// 0x10ec Realtek
// - 0x8139 RTL-8100/8101L/8139 PCI Fast Ethernet Adapter

// PCI class
// - subclass
// 0x2 Network controller
// - 0x0 Ethernet controller
// 0x3 Display controller
// - 0x0 VGA compatible
// 0x6 Bridge
// - 0x0 Host Bridge
// - 0x1 ISA Bridge
//

void pci_enumerate() {
  pci_config_address_t a = {0};
  a.bits.enabled = 1;
  a.bits.bus = 0;
  a.bits.device = 0;

  // Root bus is apparently always 0 so we could scan from there.
  for (int bus = 0; bus < 256; bus++) {
    if (bus > 1) {
      break;
    }
    // printf("pci: scanning bus %d\n", bus);
    a.bits.bus = bus;
    for (int device = 0; device < 32; device++) {
      a.bits.device = device;
      a.bits.offset = 0;
      outl(PCI_CONFIG_ADDRESS, a.raw);
      pci_config_register_0_t h1;
      h1.raw = inl(PCI_CONFIG_DATA);

      if (h1.fields.vendor_id == 0xffff) {
	continue;
      }
      //    printf("pci: %d\n", device);
      //    printf("pci: vendor: %x, device: %x\n", h1.fields.vendor_id,
      //	     h1.fields.device_id);

      a.bits.offset = 0x8;
      outl(PCI_CONFIG_ADDRESS, a.raw);
      pci_config_register_2_t h2;
      h2.raw = inl(PCI_CONFIG_DATA);
      //      printf("pci: class: %x, subclass: %x\n", h2.fields.class,
      //	     h2.fields.subclass);

      a.bits.offset = 0xc;
      outl(PCI_CONFIG_ADDRESS, a.raw);
      pci_config_register_3_t h3;
      h3.raw = inl(PCI_CONFIG_DATA);
      //    printf("pci header: %x\n", h3.fields.header_type);

      if (h1.fields.vendor_id == 0x10ec && h1.fields.device_id == 0x8139) {
	// found ethernet controller
	a.bits.offset = 0x4;
	outl(PCI_CONFIG_ADDRESS, a.raw);
	pci_config_register_1_t h1;
	h1.raw = inl(PCI_CONFIG_DATA);
	/* printf("pci status: %x, command: %x, bus master: %d\n", */
	/*        h1.fields.status, h1.fields.command, */
	/*        h1.fields.command_bits.bus_master); */

	h1.fields.command_bits.bus_master = 1;
	outl(PCI_CONFIG_ADDRESS, a.raw);
	outl(PCI_CONFIG_DATA, h1.raw);

	outl(PCI_CONFIG_ADDRESS, a.raw);
	h1.raw = inl(PCI_CONFIG_DATA);
	/* printf("pci status: %x, command: %x, bus master: %d\n", */
	/*        h1.fields.status, h1.fields.command, */
	/*        h1.fields.command_bits.bus_master); */

	a.bits.offset = 0x10;
	outl(PCI_CONFIG_ADDRESS, a.raw);
	pci_config_register_4_rtl8139_t h4;
	h4.raw = inl(PCI_CONFIG_DATA);
	base = h4.raw & 0xFFFFFFFC;
	//	printf("pci header: address 1: %x\n", base);
	if (h4.io_space.is_io_space) {
	  // printf("pci header: io space: address: %x, base: %x\n",
	  //	 h4.io_space.address, base);
	  //	  outb(base + 0x52, 0x0);  // start?

	  mac[0] = inb(base + 0x00);
	  mac[1] = inb(base + 0x01);
	  mac[2] = inb(base + 0x02);
	  mac[3] = inb(base + 0x03);
	  mac[4] = inb(base + 0x04);
	  mac[5] = inb(base + 0x05);

	  printf("ethernet: mac: %x:%x:%x:%x:%x:%x\n", mac[0], mac[1], mac[2],
		 mac[3], mac[4], mac[5]);

	  //	  u8 config_1 = inb(base + RTL8139_CONFIG_1);
	  //	  printf("ethernet: config 1: %x\n", config_1);

	  //	  u8 config_3 = inb(base + 0x59);
	  //	  printf("ethernet: config 3: %x\n", config_3);

	  //	  u8 config_4 = inb(base + 0x5A);
	  //	  printf("ethernet: config 4: %x\n", config_4);

	  u8 cmd = inb(base + RTL8139_CMD);
	  // here reset is 1 as written on osdev. qemu bug.
	  printf("ethernet: cmd: %x\n", cmd);

	  // soft reset
	  outb(base + RTL8139_CMD, 0x10);
	  while ((inb(base + RTL8139_CMD) & 0x10) != 0) {
	  }
	  printf("ethernet: reset successful \n");

	  // 0x30 is a 4 byte receive buffer start address register.
	  // Must be in 32bit range and 4kb aligned.
	  memory* rx_buffer = memory_remove();
	  outl(base + 0x30, (u32)rx_buffer->address);
	  network_rx_buffer = physical2virtual((void*)rx_buffer->address);
	  printf("ethernet: rx_buffer physical=%x virtual=%x",
		 rx_buffer->address, network_rx_buffer);
	  memory* tx_buffer = memory_remove();
	  network_tx_buffer = physical2virtual((void*)tx_buffer->address);
	  printf("ethernet: tx_buffer physical=%x virtual=%x",
		 tx_buffer->address, network_tx_buffer);

	  // Interrupt Mask Register
	  // 0x3c 16 bit
	  // bit 0: rx OK
	  // bit 2: tx OK
	  // note: it is important to read / write the right size, i.e.
	  // outb/outw/outl. using the wrong one results in no action.
	  outw(base + 0x3c, 0x5);

	  // Receive Configuration Register
	  // 0x44
	  // Bit 1: Accept all packets
	  // Bit 2: Accept physical match packets
	  // Bit 3: Accept multicast packets
	  // Bit 4: Accept broadcast packets
	  // Bit 11, 12: decide receive buffer length.
	  // 00 = 8k + 16 byte
	  // 01 = 16k + 16 byte
	  // 10 = 32K + 16 byte
	  // 11 = 64K + 16 byte

	  // 0xf is promiscuous mode, 0xe is normal.
	  outl(base + 0x44,
	       0xe | (1 << 7));  // wrap bit and rx flags

	  outb(base + 0x37, 0x0C);  // Enable RX and TX in command register

	  // everything until here works.
	} else {
	  printf("pci header: memory space: %d, type: %d, address: %x\n",
		 h4.memory_space.is_io_space, h4.memory_space.type,
		 h4.memory_space.address);
	}

	a.bits.offset = 0x14;
	outl(PCI_CONFIG_ADDRESS, a.raw);
	h4.raw = inl(PCI_CONFIG_DATA);
	printf("pci header: address 2: %x, addr: %x\n", h4.raw,
	       h4.memory_space.address);

	a.bits.offset = 0x3c;
	outl(PCI_CONFIG_ADDRESS, a.raw);
	pci_config_register_f_t hf;
	hf.raw = inl(PCI_CONFIG_DATA);
	printf("pci header: interrupt line: %d, interrupt pin: %d\n",
	       hf.fields.interrupt_line, hf.fields.interrupt_pin);
      }
    }
  }
}
