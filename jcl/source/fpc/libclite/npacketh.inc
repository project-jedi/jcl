
type
   Psockaddr_ll = ^sockaddr_ll;
   sockaddr_ll = record
        sll_family : word;
        sll_protocol : word;
        sll_ifindex : longint;
        sll_hatype : word;
        sll_pkttype : byte;
        sll_halen : byte;
        sll_addr : array[0..7] of byte;
     end;


const
   PACKET_HOST = 0;
   PACKET_BROADCAST = 1;
   PACKET_MULTICAST = 2;
   PACKET_OTHERHOST = 3;
   PACKET_OUTGOING = 4;
   PACKET_LOOPBACK = 5;
   PACKET_FASTROUTE = 6;
   PACKET_ADD_MEMBERSHIP = 1;
   PACKET_DROP_MEMBERSHIP = 2;
   PACKET_RECV_OUTPUT = 3;
   PACKET_RX_RING = 5;
   PACKET_STATISTICS = 6;
type
   Ppacket_mreq = ^packet_mreq;
   packet_mreq = record
        mr_ifindex : longint;
        mr_type : word;
        mr_alen : word;
        mr_address : array[0..7] of byte;
     end;


const
   PACKET_MR_MULTICAST = 0;
   PACKET_MR_PROMISC = 1;
   PACKET_MR_ALLMULTI = 2;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
