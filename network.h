#pragma once

#include "types.h"

struct ethernet_frame {
  u8 destination_mac[6];
  u8 source_mac[6];
  u16 ethertype;  // network byte order
  // 14
  //  payload
  //  crc
} __attribute__((packed));
typedef struct ethernet_frame ethernet_frame;

struct ipv4_header {
  u8 ihl : 4;
  u8 version : 4;
  u8 ecn : 2;   // congestion notification
  u8 dscp : 6;  // type of service. so routers can see what to prioritize
  // 15
  // minimum is 20 with is just the ipv4 header.
  u16 length;                // network byte order
  u16 identification;        // network byte order
  u16 fragment_offset : 13;  // network byte order
  u16 flags : 3;             // network byte order
  // 21
  u8 ttl;
  // 22
  // ref: https://en.wikipedia.org/wiki/List_of_IP_protocol_numbers
  u8 protocol;
  // 23
  u16 checksum;  // network byte order?
  // 25
  u32 source_address;  // network byte order
  // 29
  u32 destination_address;  // network byte order
  // 33
} __attribute__((packed));
typedef struct ipv4_header ipv4_header;

struct udp_header {
  u16 source_port;  // network byte order
  // 35
  u16 destination_port;  // network byte order
  // 37
  u16 length;    // network byte order
  u16 checksum;  // network byte order
} __attribute__((packed));
typedef struct udp_header udp_header;

// ref: http://www.faqs.org/rfcs/rfc768.html
struct udp_pseudo_ip_header {
  u32 source_address;       // network byte order
  u32 destination_address;  // network byte order
  u8 zero;
  u8 protocol;
  u16 udp_length;  // network byte order
} __attribute__((packed));
typedef struct udp_pseudo_ip_header udp_pseudo_ip_header;

struct dhcp_header {
  u8 op;
  u8 htype;
  u8 hlen;
  u8 hops;
  u32 xid;        // network byte order
  u16 secs;       // network byte order
  u16 flags;      // network byte order
  u32 ciaddr;     // network byte order
  u8 yiaddr[4];   // network byte order
  u32 siaddr;     // network byte order
  u32 giaddr;     // network byte order
  u32 chaddr[4];  // network byte order
  u8 reserved[192];
  u8 magic[4];
} __attribute__((packed));
typedef struct dhcp_header dhcp_header;

struct dns_header {
  u16 id;
  u8 rd : 1;  // recursion desired
  u8 tc : 1;
  u8 aa : 1;
  // standard query (0), inverse query (1), server status request (2)
  u8 opcode : 4;
  u8 qr : 1;  // query (0), response (1)

  // response code: no error (0), format error (1), server failure (2), name
  // error (3), not implemented (4), refused (5)
  u8 rcode : 4;
  u8 z : 3;
  u8 ra : 1;  // recusion available

  u16 qdcount;
  u16 ancount;
  u16 nscount;
  u16 arcount;
} __attribute__((packed));
typedef struct dns_header dns_header;

struct dns_question {
  u16 qname;  // this is actually flex. so could be pretty long.
  u16 qtype;
  u16 qclass;
} __attribute__((packed));
typedef struct dns_question dns_question;

struct dns_resource {
  u16 name;
  u16 type;
  u16 class;
  u16 ttl;
  u16 rdlength;
  u16 rdata;
} __attribute__((packed));
typedef struct dns_resource dns_resource;

struct arp_message {
  u16 htype;  // 1 = ethernet, 0x0800 IPv4
  u16 ptype;
  u8 hlen;
  u8 plen;  // Protocol length in octets. IPv4 = 4
  u16 oper;
  u16 sender_mac_1;
  u16 sender_mac_2;
  u16 sender_mac_3;
  u16 sender_protocol_address_1;
  u16 sender_protocol_address_2;
  u16 target_mac_1;
  u16 target_mac_2;
  u16 target_mac_3;
  u16 target_protocol_address_1;
  u16 target_protocol_address_2;
} __attribute__((packed));
typedef struct arp_message arp_message;

struct icmp_header {
  u8 type;  // 8 = echo, 0 = echo reply
  u8 code;  // 0 = echo(?)
  u16 checksum;
};
typedef struct icmp_header icmp_header;

struct icmp_echo_message {
  u16 identifier;
  u16 sequence_number;
};
typedef struct icmp_echo_message icmp_echo_message;

enum dhcp_state {
  dhcp_discover,
  dhcp_offer,
  dhcp_request,
  dhcp_ack,
  dhcp_done
};
struct dhcp_message {
  u32 xid;
  u8 type;
  u32 lease_seconds;
  u8 address[4];
  u8 subnet_mask[4];
  u8 dns[4];
  u8 router[4];
  u8 dhcp[4];
  // this should come from some ARP cache. Then I can remove it from here.
  u8 router_mac[6];
};
typedef struct dhcp_message dhcp_message;

void network_service();
void dhcp_service();
void dns_service();

void send_echo();
