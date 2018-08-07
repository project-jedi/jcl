
const
   IP_TOS = 1;
   IP_TTL = 2;
   IP_HDRINCL = 3;
   IP_OPTIONS = 4;
   IP_ROUTER_ALERT = 5;
   IP_RECVOPTS = 6;
   IP_RETOPTS = 7;
   IP_PKTINFO = 8;
   IP_PKTOPTIONS = 9;
   IP_PMTUDISC = 10;
   IP_MTU_DISCOVER = 10;
   IP_RECVERR = 11;
   IP_RECVTTL = 12;
   IP_RECVTOS = 13;
   IP_MULTICAST_IF = 32;
   IP_MULTICAST_TTL = 33;
   IP_MULTICAST_LOOP = 34;
   IP_ADD_MEMBERSHIP = 35;
   IP_DROP_MEMBERSHIP = 36;
   IP_RECVRETOPTS = IP_RETOPTS;
   IP_PMTUDISC_DONT = 0;
   IP_PMTUDISC_WANT = 1;
   IP_PMTUDISC_DO = 2;
   SOL_IP = 0;
   IP_DEFAULT_MULTICAST_TTL = 1;
   IP_DEFAULT_MULTICAST_LOOP = 1;
   IP_MAX_MEMBERSHIPS = 20;

type
   Pip_opts = ^ip_opts;
   ip_opts = record
        ip_dst : in_addr;
        ip_opts : array[0..39] of char;
     end;

   Pip_mreq = ^ip_mreq;
   ip_mreq = record
        imr_multiaddr : in_addr;
        imr_interface : in_addr;
     end;

   Pip_mreqn = ^ip_mreqn;
   ip_mreqn = record
        imr_multiaddr : in_addr;
        imr_address : in_addr;
        imr_ifindex : longint;
     end;

   Pin_pktinfo = ^in_pktinfo;
   in_pktinfo = record
        ipi_ifindex : longint;
        ipi_spec_dst : in_addr;
        ipi_addr : in_addr;
     end;


const
   IPV6_ADDRFORM = 1;
   IPV6_PKTINFO = 2;
   IPV6_HOPOPTS = 3;
   IPV6_DSTOPTS = 4;
   IPV6_RTHDR = 5;
   IPV6_PKTOPTIONS = 6;
   IPV6_CHECKSUM = 7;
   IPV6_HOPLIMIT = 8;
   IPV6_NEXTHOP = 9;
   IPV6_AUTHHDR = 10;
   IPV6_UNICAST_HOPS = 16;
   IPV6_MULTICAST_IF = 17;
   IPV6_MULTICAST_HOPS = 18;
   IPV6_MULTICAST_LOOP = 19;
   IPV6_JOIN_GROUP = 20;
   IPV6_LEAVE_GROUP = 21;
   IPV6_ROUTER_ALERT = 22;
   IPV6_MTU_DISCOVER = 23;
   IPV6_MTU = 24;
   IPV6_RECVERR = 25;
//   SCM_SRCRT = IPV6_RXSRCRT;
   IPV6_RXHOPOPTS = IPV6_HOPOPTS;
   IPV6_RXDSTOPTS = IPV6_DSTOPTS;
   IPV6_ADD_MEMBERSHIP = IPV6_JOIN_GROUP;
   IPV6_DROP_MEMBERSHIP = IPV6_LEAVE_GROUP;
   IPV6_PMTUDISC_DONT = 0;
   IPV6_PMTUDISC_WANT = 1;
   IPV6_PMTUDISC_DO = 2;
   SOL_IPV6 = 41;
   SOL_ICMPV6 = 58;
   IPV6_RTHDR_LOOSE = 0;
   IPV6_RTHDR_STRICT = 1;
   IPV6_RTHDR_TYPE_0 = 0;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
