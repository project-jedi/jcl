
type
   Pether_arp = ^ether_arp;
   ether_arp = record
        ea_hdr : arphdr;
        arp_sha : array[0..(ETH_ALEN)-1] of u_int8_t;
        arp_spa : array[0..3] of u_int8_t;
        arp_tha : array[0..(ETH_ALEN)-1] of u_int8_t;
        arp_tpa : array[0..3] of u_int8_t;
     end;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

type
  TEthernetAddress = packed array[0..ETH_ALEN-1] of u_char;
  PEthernetAddress = ^TEthernetAddress;

procedure ETHER_MAP_IP_MULTICAST(const ipaddr: in_addr; enaddr: PEthernetAddress);

