
const
   SOL_NETROM = 259;
   NETROM_T1 = 1;
   NETROM_T2 = 2;
   NETROM_N2 = 3;
   NETROM_PACLEN = 5;
   NETROM_T4 = 6;
   NETROM_IDLE = 7;
   NETROM_KILL = 99;
   NETROM_NEIGH = 0;
   NETROM_NODE = 1;

type
   Pnr_route_struct = ^nr_route_struct;
   nr_route_struct = record
        _type : longint;
        callsign : ax25_address;
        device : array[0..15] of char;
        quality : dword;
        mnemonic : array[0..6] of char;
        neighbour : ax25_address;
        obs_count : dword;
        ndigis : dword;
        digipeaters : array[0..(AX25_MAX_DIGIS)-1] of ax25_address;
     end;


const
   SIOCNRGETPARMS = SIOCPROTOPRIVATE + 0;
   SIOCNRSETPARMS = SIOCPROTOPRIVATE + 1;
   SIOCNRDECOBS = SIOCPROTOPRIVATE + 2;
   SIOCNRRTCTL = SIOCPROTOPRIVATE + 3;
   SIOCNRCTLCON = SIOCPROTOPRIVATE + 4;

type
   Pnr_parms_struct = ^nr_parms_struct;
   nr_parms_struct = record
        quality : dword;
        obs_count : dword;
        ttl : dword;
        timeout : dword;
        ack_delay : dword;
        busy_delay : dword;
        tries : dword;
        window : dword;
        paclen : dword;
     end;

   Pnr_ctl_struct = ^nr_ctl_struct;
   nr_ctl_struct = record
        index : byte;
        id : byte;
        cmd : dword;
        arg : dword;
     end;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
