#include "network.h"

#include "lib.h"
#include "memory.h"
#include "message.h"
#include "print.h"
#include "rtl8139.h"
#include "task.h"
#include "time.h"
#include "types.h"

u8 broadcast_mac[6] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff};
u8 ip[4] = {0};
u8 broadcast_ip[4] = {0xff, 0xff, 0xff, 0xff};

// How are IPs stored in memory?
// ip global
// 000000000041f0de: 0xc0 0xa8 0x0b 0x08
// ip_2 uint32 generated by this code
// ip[0] = 0xc0, ip[1] = 0xa8, ip[2] = 0x0b, ip[3] = 0x08
// ip_2 = (ip[3] << 24) | (ip[2] << 16) | (ip[1] << 8) | ip[0];//
// 000000000041f0e4: 0xc0 0xa8 0x0b 0x08
// x /4xb 0x41f0e4
// 000000000041f0e4: 0xc0 0xa8 0x0b 0x08
// (qemu) x /4xw 0x41f0e4
// 000000000041f0e4: 0x080ba8c0 0x010ba8c0 0x00ffffff 0x010ba8c0

// note: I was confused. The UDP checksum was wrong, because I mistakenly put
// the address fields down as u16. Because I noticed that the only thing
// that made it fail were the addresses I thought it had something to do with
// endianess. I wondered why the above resulted in the same memory layout, but
// taking a step back it became obvious. I laid out the little endian u32
// value ip_2 such that as a little endian value it would be laid out like the
// address in big endian. Not sure why I was confused :)

u8 dhcp_identifier[4];
u8 dhcp_subnet_mask[4];
u8 dhcp_router[4];
u8 dhcp_router_mac[6];
u8 dhcp_dns[4];
u32 dhcp_offer_xid;

u16 ntohs(u16 netshort) {
  return (netshort >> 8) | (netshort << 8);
}

u16 htons(u16 hostshort) {
  return (hostshort >> 8) | (hostshort << 8);
}

u32 htonl(u32 hostlong) {
  return ((hostlong & 0x000000ff) << 24) | ((hostlong & 0x0000ff00) << 8) |
	 ((hostlong & 0x00ff0000) >> 8) | ((hostlong & 0xff000000) >> 24);
}

u32 htonl_bytes(u8 bytes[4]) {
  return (bytes[3] << 24) | (bytes[2] << 16) | (bytes[1] << 8) | bytes[0];
}

// source: https://datatracker.ietf.org/doc/html/rfc1071#section-4.1
u16 ipv4_checksum(u16* addr, u8 count) {
  /* Compute Internet Checksum for "count" bytes
   *         beginning at location "addr".
   */
  u32 sum = 0;

  while (count > 1) {
    /*  This is the inner loop */
    sum += *addr++;
    count -= 2;
  }

  /*  Add left-over byte, if any */
  if (count > 0) {
    sum += *(u8*)addr;
  }

  /*  Fold 32-bit sum to 16 bits */
  while (sum >> 16) {
    sum = (sum & 0xffff) + (sum >> 16);
  }

  return ~sum;
}

// Merge
#define NET_ETHERTYPE_IPV4 0x0800
#define NET_ETHERTYPE_ARP 0x0806

#define NET_ARP_OP_REQUEST 1
#define NET_ARP_OP_REPLY 2

#define NET_PORT_DNS 53
#define NET_PORT_DHCP_SERVER 67
#define NET_PORT_DHCP_CLIENT 68

// From ipv4 header
#define NET_PROTOCOL_ICMP 0x1
#define NET_PROTOCOL_UDP 0x11

#define DHCP_OP_REQUEST 1
#define DHCP_OP_REPLY 2

#define DHCP_HARDWARE_TYPE_ETHERNET 1
#define DHCP_HARDWARE_LENGTH_ETHERNET 6

#define DHCP_OPTION_PAD 0
#define DHCP_OPTION_END 255
#define DHCP_OPTION_SUBNET_MASK 1
#define DHCP_OPTION_ROUTER 3
#define DHCP_OPTION_DNS 6
#define DHCP_OPTION_MESSAGE_TYPE 53
#define DHCP_OPTION_LENGTH_MESSAGE_TYPE 1
#define DHCP_OPTION_MESSAGE_TYPE_DISCOVER 1
#define DHCP_OPTION_MESSAGE_TYPE_OFFER 2
#define DHCP_OPTION_MESSAGE_TYPE_REQUEST 3
#define DHCP_OPTION_MESSAGE_TYPE_DECLINE 4
#define DHCP_OPTION_MESSAGE_TYPE_ACK 5
#define DHCP_OPTION_MESSAGE_TYPE_NACK 6
#define DHCP_OPTION_IP_REQUEST 50
#define DHCP_OPTION_IP_REQUEST_LENGTH 4
#define DHCP_OPTION_LEASE_SECONDS 51
#define DHCP_OPTION_SERVER 54
#define DHCP_OPTION_LENGTH_SERVER 4

struct net_device {
  u8 mac[6];
  u8 ip[4];
};

// needs source and dest port
// needs src dest address and protocol ipv4
// all of this is probably on the connection? is there a connection in udp?
// is it on the 'socket'? Is it a request? Request is from to and what kind of
// protocol to use? from / to also decided if it's ipv4 or ipv6?
// I like connection better but udp has no connections.
struct net_request {
  // ipv6 would be longer.
  u8 source_mac[6];
  u8 source_address[4];
  u32 source_port;
  u8 destination_mac[6];
  u8 destination_address[4];
  u32 destination_port;
  u32 ether_type;
  u8 protocol;
};
typedef struct net_request net_request;

struct buffer {
  u8 packet[1518];
  u32 bytes_written_body;
  u32 bytes_written_header;
};
typedef struct buffer buffer;

// Ethernet header 18 bytes + 1500 bytes content.
// ref: https://en.wikipedia.org/wiki/Ethernet_frame
// Ethernet header 18 bytes.
// IP header is min 20 bytes and max 60 bytes.
// UDP header is 8 bytes.
// TCP header is min 20 bytes and max 60 bytes.
// We can reserve 138 bytes.
const u32 buffer_header_offset = 138;

// need to write content
// then need to write all headers backwards
// difficult to know size of ip header
// individual write_header functions?
// ipv4 header size depends on how many options there are, we can precompute
// given we know the options.
void buffer_write_body(buffer* buffer, void* bytes, usize len) {
  memcpy(bytes,
	 buffer->packet + buffer_header_offset + buffer->bytes_written_body,
	 len);
  buffer->bytes_written_body += len;
}

void buffer_write_byte_body(buffer* buffer, u8 byte) {
  buffer_write_body(buffer, &byte, sizeof(byte));
}

void buffer_write_header(buffer* buffer, void* bytes, usize len) {
  buffer->bytes_written_header += len;
  memcpy(bytes,
	 buffer->packet + buffer_header_offset - buffer->bytes_written_header,
	 len);
}

void* buffer_packet(buffer* buffer) {
  return buffer->packet + buffer_header_offset - buffer->bytes_written_header;
}

void* buffer_body(buffer* buffer) {
  return buffer->packet + buffer_header_offset;
}

u32 buffer_length(buffer* buffer) {
  return buffer->bytes_written_body + buffer->bytes_written_header;
}

void net_write_ethernet_frame(net_request* req, buffer* buffer) {
  ethernet_frame frame = {0};
  memcpy(req->source_mac, frame.source_mac, 6);
  memcpy(req->destination_mac, frame.destination_mac, 6);
  if (req->ether_type != 0) {
    frame.ethertype = htons(req->ether_type);
  } else {
    frame.ethertype = htons(NET_ETHERTYPE_IPV4);
  }
  buffer_write_header(buffer, &frame, sizeof(frame));
}

void net_write_ipv4_header(net_request* req, buffer* buffer) {
  ipv4_header header = {0};
  header.version = 4;
  header.ihl = 5;  // no options
  header.ttl = 100;
  if (req->protocol != 0) {
    header.protocol = req->protocol;
  } else {
    header.protocol = NET_PROTOCOL_UDP;
  }
  memcpy(req->source_address, &header.source_address, 4);
  memcpy(req->destination_address, &header.destination_address, 4);

  // For one moment I thought we could just access the udp header here and get
  // the length, but it's not feasible for this code to know about other
  // headers.
  header.length = htons(header.ihl * 4 + buffer->bytes_written_header +
			buffer->bytes_written_body);

  header.checksum = ipv4_checksum((u16*)&header, header.ihl * 4);
  buffer_write_header(buffer, &header, sizeof(header));
}

// Checksum is the 16-bit one's complement of the one's complement sum of a
// pseudo header of information from the IP header, the UDP header, and the
// data, padded with zero octets at the end (if necessary) to make a multiple of
// two octets.
// ref: https://datatracker.ietf.org/doc/html/rfc768
// ref: http://www.faqs.org/rfcs/rfc768.html

u16 udp_checksum(net_request* req, udp_header* udph, u16* addr, usize length) {
  udp_pseudo_ip_header ps = {0};
  ps.source_address = htonl_bytes(req->source_address);
  ps.destination_address = htonl_bytes(req->destination_address);
  ps.protocol = NET_PROTOCOL_UDP;
  ps.udp_length = length;

  /* Compute Internet Checksum for "count" bytes
   *         beginning at location "addr".
   */
  u32 sum = 0;

  u16* first = (u16*)(&ps);
  for (u32 i = 0; i < sizeof(ps) / 2; i++) {
    sum += *first++;
  }

  first = (u16*)(udph);

  for (u32 i = 0; i < sizeof(*udph) / 2; i++) {
    sum += *first++;
  }

  u16 count = ntohs(length);

  while (count > 1) {
    /*  This is the inner loop */
    sum += *addr++;
    count -= 2;
  }

  /*  Add left-over byte, if any */
  if (count > 0) {
    sum += *(u8*)addr;
  }

  /*  Fold 32-bit sum to 16 bits */
  while (sum >> 16) {
    sum = (sum & 0xffff) + (sum >> 16);
  }

  return ~sum;
}

void net_write_udp_header(struct net_request* req, buffer* buffer) {
  udp_header header = {0};
  header.source_port = htons(req->source_port);            // htons(68);
  header.destination_port = htons(req->destination_port);  // htons(67);
  header.length = htons(sizeof(header) + buffer->bytes_written_body);

  // Tricky. We need the header.
  // Accessor for content would be good.
  header.checksum =
      udp_checksum(req, &header, (u16*)(buffer->packet + buffer_header_offset),
		   header.length);

  buffer_write_header(buffer, &header, sizeof(header));
}

// TODO: take a net_device as first parameter for access to mac and ip
// Should this move the FP or return how many bytes written?
void net_write_dhcp_discover_message(net_request* req,
				     u32 xid,
				     buffer* buffer) {
  dhcp_header msg = {0};

  msg.op = DHCP_OP_REQUEST;
  msg.htype = DHCP_HARDWARE_TYPE_ETHERNET;
  msg.hlen = DHCP_HARDWARE_LENGTH_ETHERNET;
  msg.hops = 0x00;
  // Used to match return messages
  msg.xid = xid;
  msg.secs = 0;
  msg.flags = 0;  // Set broadcast bit?
  // MAC
  // From the commented out code, this is what wireshark shows.
  // 52:54:00:12:34:56
  // I assume a byte by byte copy will result in the same.
  // Yes, worked. Confirmed via wireshark.
  memcpy(req->source_mac, msg.chaddr, 6);
  // msg.chaddr[0] = (mac[3] << 24) | (mac[2] << 16) | (mac[1] << 8) | mac[0];
  // msg.chaddr[1] = (mac[5] << 8) | mac[4];

  //  TODO: make this a constant? It's the options header.
  //  ref: https://datatracker.ietf.org/doc/html/rfc2131#section-3
  msg.magic[0] = 0x63;
  msg.magic[1] = 0x82;
  msg.magic[2] = 0x53;
  msg.magic[3] = 0x63;

  buffer_write_body(buffer, &msg, sizeof(msg));

  // net_write_dhcp_option(option, buffer)
  buffer_write_byte_body(buffer, DHCP_OPTION_MESSAGE_TYPE);
  buffer_write_byte_body(buffer, DHCP_OPTION_LENGTH_MESSAGE_TYPE);
  buffer_write_byte_body(buffer, DHCP_OPTION_MESSAGE_TYPE_DISCOVER);
  buffer_write_byte_body(buffer, DHCP_OPTION_END);
}

void send_dhcp_discover() {
  printf("dhcp: sending DHCPDISCOVER\n");
  u32 xid = (u32)get_global_timer_value();

  net_request req = {
      .source_mac = {mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]},
      .source_address = {0},
      .source_port = NET_PORT_DHCP_CLIENT,
      .destination_mac = {broadcast_mac[0], broadcast_mac[1], broadcast_mac[2],
			  broadcast_mac[3], broadcast_mac[4], broadcast_mac[5]},
      .destination_address = {broadcast_ip[0], broadcast_ip[1], broadcast_ip[2],
			      broadcast_ip[3]},
      .destination_port = NET_PORT_DHCP_SERVER,
  };

  buffer buffer = {0};
  net_write_dhcp_discover_message(&req, xid, &buffer);
  net_write_udp_header(&req, &buffer);
  net_write_ipv4_header(&req, &buffer);
  net_write_ethernet_frame(&req, &buffer);

  net_transmit(buffer_packet(&buffer), buffer_length(&buffer));
}

void net_write_dhcp_request_message(net_request* req,
				    dhcp_message* dhcp_req,
				    void* buffer) {
  // TODO: duplication started again because now we are just building these
  // packets from the dhcp_request.
  dhcp_header msg = {0};
  msg.op = DHCP_OP_REQUEST;
  msg.htype = DHCP_HARDWARE_TYPE_ETHERNET;
  msg.hlen = DHCP_HARDWARE_LENGTH_ETHERNET;
  msg.hops = 0x00;
  // TODO: there is some state per request.
  msg.xid = dhcp_req->xid;
  msg.secs = 0;
  msg.flags = 0;
  memcpy(req->source_mac, msg.chaddr, 6);

  memcpy(dhcp_req->router, &msg.siaddr, 4);
  // msg.siaddr = (dhcp_router[3] << 24) | (dhcp_router[2] << 16) |
  //	       (dhcp_router[1] << 8) |
  //	       dhcp_router[0];  // htonl_bytes(dhcp_router);
  msg.magic[0] = 0x63;
  msg.magic[1] = 0x82;
  msg.magic[2] = 0x53;
  msg.magic[3] = 0x63;

  buffer_write_body(buffer, &msg, sizeof(msg));

  buffer_write_byte_body(buffer, DHCP_OPTION_MESSAGE_TYPE);
  buffer_write_byte_body(buffer, DHCP_OPTION_LENGTH_MESSAGE_TYPE);
  buffer_write_byte_body(buffer, DHCP_OPTION_MESSAGE_TYPE_REQUEST);
  buffer_write_byte_body(buffer, DHCP_OPTION_IP_REQUEST);
  buffer_write_byte_body(buffer, DHCP_OPTION_IP_REQUEST_LENGTH);
  buffer_write_body(buffer, dhcp_req->address, 4);
  //  buffer_write_byte_body(buffer, ip[0]);
  //  buffer_write_byte_body(buffer, ip[1]);
  //  buffer_write_byte_body(buffer, ip[2]);
  //  buffer_write_byte_body(buffer, ip[3]);
  buffer_write_byte_body(buffer, DHCP_OPTION_SERVER);
  buffer_write_byte_body(buffer, DHCP_OPTION_LENGTH_SERVER);
  buffer_write_body(buffer, dhcp_req->dhcp, 4);
  // buffer_write_byte_body(buffer, dhcp_router[0]);
  // buffer_write_byte_body(buffer, dhcp_router[1]);
  // buffer_write_byte_body(buffer, dhcp_router[2]);
  // buffer_write_byte_body(buffer, dhcp_router[3]);
  buffer_write_byte_body(buffer, 0xff);
}

void send_dhcp_request(dhcp_message* dhcp_req) {
  printf("dhcp: sending DHCPREQUEST\n");
  net_request req = {
      // TODO: adding this gets tiring, I need the device, it's always the same.
      .source_mac = {mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]},
      .source_address = {0},
      .source_port = NET_PORT_DHCP_CLIENT,
      .destination_mac = {broadcast_mac[0], broadcast_mac[1], broadcast_mac[2],
			  broadcast_mac[3], broadcast_mac[4], broadcast_mac[5]},
      .destination_address = {broadcast_ip[0], broadcast_ip[1], broadcast_ip[2],
			      broadcast_ip[3]},
      .destination_port = NET_PORT_DHCP_SERVER,
  };

  buffer buffer = {0};
  net_write_dhcp_request_message(&req, dhcp_req, &buffer);
  net_write_udp_header(&req, &buffer);
  net_write_ipv4_header(&req, &buffer);
  net_write_ethernet_frame(&req, &buffer);

  net_transmit(buffer_packet(&buffer), buffer_length(&buffer));
}

#define DNS_FLAG_QUERY 0
#define DNS_FLAG_RESPONSE 1
#define DNS_NULL_LABEL 0

// Inject hostname
usize net_write_dns_query(u16 id, buffer* buffer) {
  dns_header header = {0};

  // transaction id
  header.id = htons(id);
  // number of questions
  header.qdcount = htons(1);
  header.qr = DNS_FLAG_QUERY;
  // recursion desired
  header.rd = 1;

  buffer_write_body(buffer, &header, sizeof(header));

  buffer_write_byte_body(buffer, 6);
  buffer_write_byte_body(buffer, 'g');
  buffer_write_byte_body(buffer, 'o');
  buffer_write_byte_body(buffer, 'o');
  buffer_write_byte_body(buffer, 'g');
  buffer_write_byte_body(buffer, 'l');
  buffer_write_byte_body(buffer, 'e');
  buffer_write_byte_body(buffer, 3);
  buffer_write_byte_body(buffer, 'c');
  buffer_write_byte_body(buffer, 'o');
  buffer_write_byte_body(buffer, 'm');
  buffer_write_byte_body(buffer, DNS_NULL_LABEL);
  // Type A
  buffer_write_byte_body(buffer, 0);  // type byte
  buffer_write_byte_body(buffer, 1);  // type byte
  // Class Internet
  buffer_write_byte_body(buffer, 0);  // class byte
  buffer_write_byte_body(buffer, 1);  // class byte
}

#define NET_PORT_DNS 53

void send_dns_request(u16 id) {
  // How do we usually know the mac address?
  // Is the mac address the one from the router always?
  net_request req = {
      .source_mac = {mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]},
      .source_address = {ip[0], ip[1], ip[2], ip[3]},
      .source_port = NET_PORT_DNS,
      .destination_mac = {dhcp_router_mac[0], dhcp_router_mac[1],
			  dhcp_router_mac[2], dhcp_router_mac[3],
			  dhcp_router_mac[4], dhcp_router_mac[5]},
      .destination_address = {dhcp_dns[0], dhcp_dns[1], dhcp_dns[2],
			      dhcp_dns[3]},
      .destination_port = NET_PORT_DNS,
  };

  buffer buffer = {0};
  net_write_dns_query(id, &buffer);
  net_write_udp_header(&req, &buffer);
  net_write_ipv4_header(&req, &buffer);
  net_write_ethernet_frame(&req, &buffer);

  net_transmit(buffer_packet(&buffer), buffer_length(&buffer));
}

void net_write_arp_response(net_request* req, buffer* buffer) {
  arp_message msg = {0};
  msg.htype = htons(1);  // ethernet
  msg.hlen = 6;
  msg.ptype = htons(NET_ETHERTYPE_IPV4);
  msg.plen = 4;
  msg.oper = htons(2);  // reply
  memcpy(req->source_mac, &msg.sender_mac_1, 6);
  memcpy(req->source_address, &msg.sender_protocol_address_1, 4);
  memcpy(req->destination_mac, &msg.target_mac_1, 6);
  memcpy(req->destination_address, &msg.target_protocol_address_1, 4);
  buffer_write_body(buffer, &msg, sizeof(msg));
}

void send_arp_response(u8* sender_mac, u8* sender_ip) {
  net_request req = {
      .source_mac = {mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]},
      .source_address = {ip[0], ip[1], ip[2], ip[3]},
      .source_port = NET_PORT_DHCP_CLIENT,
      .destination_mac = {sender_mac[0], sender_mac[1], sender_mac[2],
			  sender_mac[3], sender_mac[4], sender_mac[5]},
      .destination_address = {sender_ip[0], sender_ip[1], sender_ip[2],
			      sender_ip[3]},
      .destination_port = NET_PORT_DHCP_SERVER,
      .ether_type = NET_ETHERTYPE_ARP,
  };
  buffer buffer = {0};
  net_write_arp_response(&req, &buffer);
  net_write_ethernet_frame(&req, &buffer);

  //  Can this be async?
  net_transmit(buffer_packet(&buffer), buffer_length(&buffer));

  //  TODO: free packet because it's on the network card buffer now.
}

// TODO: split into header and message
void net_write_icmp_echo(buffer* buffer) {
  u16 packet[4] = {0};
  icmp_header* h = (icmp_header*)packet;
  h->type = 8;
  h->code = 0;

  icmp_echo_message* m = (icmp_echo_message*)(h + 1);
  m->identifier = 0x1;
  m->sequence_number = 0;

  h->checksum =
      ipv4_checksum(packet, sizeof(icmp_header) + sizeof(icmp_echo_message));
  buffer_write_body(buffer, packet,
		    sizeof(icmp_header) + sizeof(icmp_echo_message));
}

struct dns_request {
  u8 name[63];
  message** sender;
};

// TODO: add ttl
struct dns_response {
  u16 id;
  u8 name[63];
  u8 address[4];
};

struct net_address {
  u8 address[4];
};

// TODO: make this work async by doing the dns query.
// Imagine:
// send_echo()
// - build package with hostname
// - send
// -- do we know this hostname?
// --- no: resolve and block
// --- yes: fetch from cache
// -- fill IP
// Question: how to do timeouts? The blocking command needs a timeout.
struct net_address net_resolve_hostname(u8 hostname[63]) {
  // What to do here?
  // Let's assume there is no cache. We always have to resolve the addresses.
  //

  // how do we wait for it's response?
  // we want to block here until the response is here.
  message msg = {0};

  message_send(&service_dns->queue, dns_request, nullptr);

  printf("resolve_hostname: waiting for response\n");
  message_receive(&service_dhcp->queue, &msg);

  struct dns_response* resp = msg.data;
  printf("resolve_hostname: %d.%d.%d.%d\n", resp->address[0], resp->address[1],
	 resp->address[2], resp->address[3]);

  struct net_address addr = {.address[0] = resp->address[0],
			     .address[1] = resp->address[1],
			     .address[2] = resp->address[2],
			     .address[3] = resp->address[3]};

  return addr;

  // alloc object that holds address.
  // put into data
  //  msg.data =
  //  message_send(message **head, message_type_t type, void *data);

  // what's the difference between asking synchronously vs asynchronously?
  // synchronous implies no task change?

  // Let's assume there is a dns server and it's listening for messages.
}

// TODO: where to? take hostname
// wait for answer.
// This is almost user level code. I bet ping is implemented like this.
// This could be my first program.
void send_echo() {
  if (ip[0] == 0) {
    printf("send_echo: network stack down\n");
    return;
  }
  // TODO: resolve via dns query first.
  // so we need to block until it's resolved. Interesting.
  struct net_address google_address = net_resolve_hostname(nullptr);
  net_request req = {
      .source_mac = {mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]},
      .source_address = {ip[0], ip[1], ip[2], ip[3]},

      .destination_mac = {dhcp_router_mac[0], dhcp_router_mac[1],
			  dhcp_router_mac[2], dhcp_router_mac[3],
			  dhcp_router_mac[4], dhcp_router_mac[5]},
      .destination_address = {google_address.address[0],
			      google_address.address[1],
			      google_address.address[2],
			      google_address.address[3]},
      .protocol = NET_PROTOCOL_ICMP,
  };

  buffer buffer = {0};
  net_write_icmp_echo(&buffer);
  net_write_ipv4_header(&req, &buffer);
  net_write_ethernet_frame(&req, &buffer);

  // if (memcmp(ef, buffer_packet(&buffer), buffer_length(&buffer)) == false) {
  //   printf("[net_write] did not match\n");
  //   __asm__("cli");
  //   __asm__("hlt");
  // }

  net_transmit(buffer_packet(&buffer), buffer_length(&buffer));
}

// net_handle_arp handles the address resolution protocol.
// ARP is used to find a MAC address that corresponds to an IP address.
// ARP can be used to test if an IP address is free to use by sending a probing
// packet.
// ref: https://datatracker.ietf.org/doc/html/rfc826
void net_handle_arp(arp_message* msg) {
  // printf("arp: htype: %x, ptype: %x, hlen: %x, plen: %x, oper: %x\n",
  //	 ntohs(msg->htype), ntohs(msg->ptype), msg->hlen, msg->plen,
  //	 ntohs(msg->oper));

  // TODO: memcmp so I don't have to copy?
  if (ntohs(msg->oper) == NET_ARP_OP_REQUEST) {
    // check if our IP matches
    u8* target_ip = (u8*)&msg->target_protocol_address_1;
    // printf("arp: target IP: %d.%d.%d.%d\n", target_ip[0], target_ip[1],
    //	   target_ip[2], target_ip[3]);

    if (memcmp(target_ip, ip, 4)) {
      // Can this be typed somehow?
      u8* sender_mac = (u8*)&msg->sender_mac_1;
      u8* sender_ip = (u8*)&msg->sender_protocol_address_1;
      send_arp_response(sender_mac, sender_ip);
    }
  }
}

// I want to save the domain -> ip mapping.
// Maybe I need a map after all.
void* net_dns_parse_question(void* buffer) {
  u8* p = buffer;
  u8 domain[5][63] = {0};
  u8 label = 0;
  while (*p != 0) {
    u8 length = *p;
    p++;
    for (u8 i = 0; i < length; i++) {
      domain[label][i] = *p;
      p++;
    }

    printf("label: %s\n", domain[label]);
    label++;
  }
  // skip 0-TERMINATOR QTYPE QCLASS
  return p + 1 + 2 + 2;
}

// This gets trickier beacause of message compression. Need access to the whole
// message.
// TODO: this is super brittle, refactor out the label reading etc.
// TODO: return answer structs. domain name is limited to 63.
// Also, it would probably be good to have some kind of reader.
// ReadyByte, ReadyByte, ReadInt and it converts to LE
void* net_dns_parse_answer(void* message,
			   void* answers,
			   struct dns_response* response) {
  u8* p = answers;
  u8 domain[5][63] = {0};
  u8 label = 0;
  while (*p != 0) {
    // first 2 bits signal offset
    if ((*p & 0b11000000) == 0b11000000) {
      u16 offset = ((*p & ~0b11000000) >> 8) | *(p + 1);
      p += 2;

      u8* offsetptr = message + offset;
      while (*offsetptr != 0) {
	u8 length = *offsetptr;
	offsetptr++;
	for (u8 i = 0; i < length; i++) {
	  domain[label][i] = *offsetptr;
	  offsetptr++;
	}
	printf("answer offset label: %s\n", domain[label]);
	label++;
      }
    } else {
      u8 length = *p;
      p++;
      for (u8 i = 0; i < length; i++) {
	domain[label][i] = *p;
	p++;
      }
      printf("answer label: %s\n", domain[label]);
      label++;
    }
  }

  // skip type, class, ttl
  p += 2 + 2 + 4;

  //  u16* rdlength = p;
  p += 2;

  u8 address[4] = {0};
  for (u8 i = 0; i < 4; i++) {
    address[i] = *p;
    response->address[i] = *p;
    p++;
  }

  printf("address: %d.%d.%d.%d\n", address[0], address[1], address[2],
	 address[3]);

  return p;
}

void net_handle_dns(dns_header* h) {
  printf("dns: id: %x, opcode: %x, qr: %x acount: %x\n", ntohs(h->id),
	 h->opcode, h->qr, ntohs(h->ancount));

  if (h->qr != DNS_FLAG_RESPONSE) {
    return;
  }

  // read questions (are repeated in response messages)
  void* p = h + 1;
  for (int i = 0; i < ntohs(h->qdcount); i++) {
    p = net_dns_parse_question(p);
  }
  // read answers
  struct dns_response* resp = malloc(sizeof(*resp));
  resp->id = ntohs(h->id);
  for (int i = 0; i < ntohs(h->ancount); i++) {
    p = net_dns_parse_answer(h, p, resp);
  }
  message_send(&service_dns->queue, dns_response, resp);
}

enum dhcp_state dhcp_state_current = dhcp_discover;

void net_handle_dhcp(ethernet_frame* ef, dhcp_header* h) {
  printf("dhcp: op: %x htype: %x hlen: %x hops: %x xid: %x\n", h->op, h->htype,
	 h->hlen, h->hops, h->xid);

  if (h->op != DHCP_OP_REPLY) {
    // This actually happens because other clients broadcast this.
    printf("dhcp: received a dhcp request\n");
    return;
  }

  dhcp_message* msg = malloc(sizeof(*msg));
  msg->xid = h->xid;
  // TODO: should this only be copied in case of OFFER?
  memcpy(h->yiaddr, msg->address, 4);
  memcpy(ef->source_mac, msg->router_mac, 6);

  u8* o = (u8*)(h + 1);  // end of dhcp message for options

  // TODO: double check that we don't go over length of message.

  //  u8 dhcp_type = 0;

  // TODO: what about having a reader? I thought about this before for DNS.
  //  bool done = false;
  while (*o != 0xff) {
    switch (*o++) {
      case DHCP_OPTION_SUBNET_MASK:  // subnet mask
	if (*o++ != 4) {
	  // panic
	}
	memcpy(o, msg->subnet_mask, 4);
	//	memcpy(o, dhcp_subnet_mask, 4);
	o += 4;
	break;
      case DHCP_OPTION_ROUTER:  // TODO: can be multiples. how to parse?
	if (*o++ != 4) {
	  // panic
	}
	memcpy(o, msg->router, 4);
	//	memcpy(o, dhcp_router, 4);
	o += 4;
	break;
      case DHCP_OPTION_DNS:
	if (*o++ != 4) {
	  // panic
	}
	memcpy(o, msg->dns, 4);
	//	memcpy(o, dhcp_dns, 4);
	o += 4;
	break;
      case DHCP_OPTION_MESSAGE_TYPE:
	if (*o++ != 1) {
	  // panic
	}
	memcpy(o, &msg->type, 1);
	o++;
	//	dhcp_type = *o++;
	break;
      case DHCP_OPTION_SERVER:
	// what about ipv6? seems it's a slightly different protocol
	if (*o++ != DHCP_OPTION_LENGTH_SERVER) {
	  // panic
	}
	// I am on the fence about using DHCP_OPTION_LENGTH_SERVER. On one hand
	// it's nice not to have a magic number, on the other we also have to
	// define the types in the struct so we know the size here.
	memcpy(o, msg->dhcp, DHCP_OPTION_LENGTH_SERVER);
	//	memcpy(o, dhcp_identifier, 4);
	o += DHCP_OPTION_LENGTH_SERVER;
	break;
      case DHCP_OPTION_LEASE_SECONDS:  // lease time (2 days, so low priority to
				       // implement)
	if (*o++ != 4) {
	  // panic
	}
	// TODO: convert?
	memcpy(o, &msg->lease_seconds, 4);
	o += 4;
	break;
      case 0x0:
	// padding
	break;
      case 0xff:
	//	done = true;
	break;
    }
  }

  message_send(&service_dhcp->queue, message_type_dhcp_response, msg);

  // if (dhcp_state_current == dhcp_offer &&
  //     dhcp_type != DHCP_OPTION_MESSAGE_TYPE_OFFER) {
  //   printf("dhcp: we are waiting for an offer, but was %x\n", dhcp_type);
  //   return;
  // }

  // if (dhcp_state_current == dhcp_ack &&
  //     dhcp_type != DHCP_OPTION_MESSAGE_TYPE_ACK) {
  //   printf("dhcp: we are waiting for an ack, but was %x\n", dhcp_type);
  //   return;
  // }

  // if (dhcp_type == DHCP_OPTION_MESSAGE_TYPE_OFFER) {
  //   printf("dhcp: received DHCPOFFER\n");
  //   printf("dhcp: new state: request\n");
  //   dhcp_state_current = dhcp_request;

  //   memcpy(m->yiaddr, ip, 4);
  //   /* for (int i = 0; i < 4; i++) { */
  //   /*   ip[i] = m->yiaddr[i]; */
  //   /* } */
  //   printf("dhcp: assigned IP %d.%d.%d.%d\n", ip[0], ip[1], ip[2], ip[3]);
  //   printf("dhcp: router IP %d.%d.%d.%d\n", dhcp_router[0], dhcp_router[1],
  //	   dhcp_router[2], dhcp_router[3]);
  //   printf("dhcp: dns IP %d.%d.%d.%d\n", dhcp_dns[0], dhcp_dns[1],
  //   dhcp_dns[2],
  //	   dhcp_dns[3]);
  //   printf("dhcp: subnet mask %d.%d.%d.%d\n", dhcp_subnet_mask[0],
  //	   dhcp_subnet_mask[1], dhcp_subnet_mask[2], dhcp_subnet_mask[3]);

  //   dhcp_offer_xid = m->xid;

  // } else if (dhcp_type == DHCP_OPTION_MESSAGE_TYPE_ACK) {
  //   printf("dhcp: received DHCPACK\n");
  //   dhcp_state_current = dhcp_done;
  //   printf("dhcp: new state: done %d\n", dhcp_state_current);

  //   printf("dhcp: target_mac: %x:%x:%x:%x:%x:%x\n", ef->destination_mac[0],
  //	   ef->destination_mac[1], ef->destination_mac[2],
  //	   ef->destination_mac[3], ef->destination_mac[4],
  //	   ef->destination_mac[5]);
  //   printf("dhcp: source_mac: %x:%x:%x:%x:%x:%x\n", ef->source_mac[0],
  //	   ef->source_mac[1], ef->source_mac[2], ef->source_mac[3],
  //	   ef->source_mac[4], ef->source_mac[5]);
  //   memcpy(ef->source_mac, dhcp_router_mac, 6);
  // }
}

void net_handle_udp(ethernet_frame* frame, udp_header* header) {
  printf("udp: src port: %d dst port: %d\n", ntohs(header->source_port),
	 ntohs(header->destination_port));

  if (ntohs(header->destination_port) == NET_PORT_DNS) {  // DNS
    net_handle_dns((dns_header*)(header + 1));
  } else if (ntohs(header->source_port) == 67 &&
	     ntohs(header->destination_port) == 68) {  // DHCP
    net_handle_dhcp(frame, (dhcp_header*)(header + 1));
  }
}

void net_handle_ipv4(ethernet_frame* frame, ipv4_header* header) {
  printf("ipv4: version: %x ihl: %x\n", header->version, header->ihl);

  // printf("ipv4: protocol: %x\n", iph->protocol);

  if (header->protocol == NET_PROTOCOL_ICMP) {
  } else if (header->protocol == NET_PROTOCOL_UDP) {
    net_handle_udp(frame, (udp_header*)(header + 1));
  }
}

void network_service() {
  // Why do I call this service? I imagine the OS always keeps the network stack
  // in shape. Does DHCP stuff, etc. Can the kernel do this directly? Well it
  // needs to do many network calls so it blocks.. Hence modeling it as a task
  // or service seems to make sense?

  // Reply to ARP messages?

  // -> dhcp discover
  // <- dhcp offer
  // -> dhcp request
  while (true) {
    message msg;
    message_receive(&service_network->queue, &msg);

    ethernet_frame* frame = (ethernet_frame*)msg.data;

    u16 ether_type = ntohs(frame->ethertype);

    if (ether_type == NET_ETHERTYPE_ARP) {
      net_handle_arp((arp_message*)(frame + 1));
    } else if (ether_type == NET_ETHERTYPE_IPV4) {
      net_handle_ipv4(frame, (ipv4_header*)(frame + 1));
    }

    free(msg.data);
  }
}

// I want to model DHCP in case I miss a package or something I should retry.
// But now we suddenly need timers and a better msg queue.
// I imagine I have another service, a dhcp service that is a state machine.
// could I do this here in the network_service directly?
// We could do it super primitive. On every network message check how long we
// waited.

u64 dhcp_last_request_ts = {0};
// RACE: dhcp_state_current
// The receiver task changes it because it handles dhcp respondes.
// This task handles it.
// Maybe this task should be the only handler and the other task just parses
// messages and puts them on a queue for the state machine.
// For now we can simplify once more and make the dhcp_service global and put
// onto its queue.
//
// How to handle race conditions?
// Easiest, only access in the same task/thread.
// Lock.
//
// Thought: wouldn't it be easier to code this in a req/response style?
// Send discover and wait for response or timeout, then proceed or retry.
// Interestingly this is similar to having to block until dns resolution.
enum dhcp_state dhcp_state_last = dhcp_discover;
void dhcp_service() {
  // we can do it similar to the dns service.
  // instead of doing state changes in the network packet decoding step we can
  // just decode them and send messages that arrive here. this will process the
  // state machine and we get rid of the globals. YES!
  dhcp_message* resp = nullptr;
  while (true) {
    switch (dhcp_state_current) {
      case dhcp_discover:
	printf("dhcp: new state: offer\n");
	dhcp_state_current = dhcp_offer;
	send_dhcp_discover();
	break;
      case dhcp_offer:
	// timeout while waiting for an answer, let's retry
	if (resp == nullptr) {
	  printf("dhcp: retrying discover\n");
	  dhcp_state_current = dhcp_discover;
	  continue;
	}
	// compare xid, are we even waiting for this?
	if (resp->type != DHCP_OPTION_MESSAGE_TYPE_OFFER) {
	  printf("dhcp: received %d while waiting for offer\n", resp->type);
	  break;
	}

	printf("dhcp: new state: request\n");
	dhcp_state_current = dhcp_request;

	// Inject parameters from offer
	send_dhcp_request(resp);

	break;
      case dhcp_request:
	// timeout while waiting for an answer, let's retry
	// TODO: to retry request we would need to hold on to the offer message.
	if (resp == nullptr) {
	  printf("dhcp: retrying discover\n");
	  dhcp_state_current = dhcp_discover;
	  continue;
	}

	// compare xid, are we even waiting for this?
	if (resp->type != DHCP_OPTION_MESSAGE_TYPE_ACK) {
	  break;
	}

	// TODO: should this be done?
	printf("dhcp: new state: ack\n");
	dhcp_state_current = dhcp_ack;

	memcpy(resp->address, ip, 4);
	memcpy(resp->dns, dhcp_dns, 4);
	memcpy(resp->router_mac, dhcp_router_mac, 6);

	//	printf("send echo\n");
	//	send_echo();
	break;
      case dhcp_ack:
	dhcp_state_current = dhcp_done;
	// Should trigger after timeout below.
	printf("dhcp_service: new state: done\n");
	// Anything to do here?
	break;
      case dhcp_done:
	break;
    }

    // resp was allocated by the dhcp parser and put on the message queue. Let's
    // clean it up before we receive another message. Warning: in case we
    // somehow exit this function before this line we will leak memory.
    if (resp != nullptr) {
      free(resp);
      resp = nullptr;
    }

    message msg = {0};
    // TODO: when we set the timeout too low we stagger discovery requests
    // because of the simple logic above.
    message_receive_timeout(&service_dhcp->queue, &msg, 1000);
    resp = msg.data;

    //    printf("dhcp_service: received %d %d\n", resp->xid, resp->type);
  }
}

// we can have an array of tx ids circular. max N requests.
// we can attach the receiver pointer

message_queue* dns_requests[10];
u8 dns_request_idx = 0;
// -1 because we count from 0
u8 dns_request_count = 10 - 1;

// TODO: since everything is request response, arp, dns, dhcp.. can this be
// abstracted and just the state machines modeled?
void dns_service() {
  while (true) {
    message msg;
    message_receive(&service_dns->queue, &msg);

    switch (msg.type) {
      case dns_request:
	// dns_request could have the hostname on it
	// it should also carry something so we can send the answer back.
	// what is that?
	// is it a pointer to a queue?
	// if I use the tasks queue then other tasks than dns_service may send
	// something there. how do I differentiate?
	// For now let's assume the sender only waits for one reply.

	// inject hostname
	u8 id = dns_request_idx;
	dns_request_idx = (dns_request_idx + 1) % dns_request_count;

	// This should be pulled out of the message
	dns_requests[id] = &service_dhcp->queue;

	// note that we are waiting for a response.
	// printf("dns_service: sending dns request with id %d\n", id);
	send_dns_request(id);
	break;
      case dns_response:
	// We got a response from the network stack.
	struct dns_response* resp = msg.data;
	// printf("dns_service: response for id %d address %d.%d.%d.%d\n",
	//        resp->id, resp->address[0], resp->address[1],
	//        resp->address[2], resp->address[3]);
	message_queue* queue = dns_requests[resp->id];
	if (queue != nullptr) {
	  message_send(queue, dns_response, resp);
	}

	// Check tx id if we waited for it. There will always be something
	// in the tx array, so we don't know if it was the same question, we
	// may reply with the wrong IP. Important: if the array is full we
	// forget requests and the caller is waiting for a reply. Needs a
	// timeout, or we have an unlimited queue, or both. Find if someone
	// asked for it and answer.
	break;
    }
  }
}
