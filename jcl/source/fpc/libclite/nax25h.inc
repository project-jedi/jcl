
const
   SOL_AX25 = 257;
   AX25_WINDOW = 1;
   AX25_T1 = 2;
   AX25_T2 = 5;
   AX25_T3 = 4;
   AX25_N2 = 3;
   AX25_BACKOFF = 6;
   AX25_EXTSEQ = 7;
   AX25_PIDINCL = 8;
   AX25_IDLE = 9;
   AX25_PACLEN = 10;
   AX25_IPMAXQUEUE = 11;
   AX25_IAMDIGI = 12;
   AX25_KILL = 99;
   SIOCAX25GETUID = SIOCPROTOPRIVATE;
   SIOCAX25ADDUID = SIOCPROTOPRIVATE + 1;
   SIOCAX25DELUID = SIOCPROTOPRIVATE + 2;
   SIOCAX25NOUID = SIOCPROTOPRIVATE + 3;
   SIOCAX25BPQADDR = SIOCPROTOPRIVATE + 4;
   SIOCAX25GETPARMS = SIOCPROTOPRIVATE + 5;
   SIOCAX25SETPARMS = SIOCPROTOPRIVATE + 6;
   SIOCAX25OPTRT = SIOCPROTOPRIVATE + 7;
   SIOCAX25CTLCON = SIOCPROTOPRIVATE + 8;
   SIOCAX25GETINFO = SIOCPROTOPRIVATE + 9;
   SIOCAX25ADDFWD = SIOCPROTOPRIVATE + 10;
   SIOCAX25DELFWD = SIOCPROTOPRIVATE + 11;
   AX25_NOUID_DEFAULT = 0;
   AX25_NOUID_BLOCK = 1;
   AX25_SET_RT_IPMODE = 2;
   AX25_DIGI_INBAND = $01;
   AX25_DIGI_XBAND = $02;
   AX25_MAX_DIGIS = 8;

type

   Pax25_address = ^ax25_address;
   ax25_address = record
        ax25_call : array[0..6] of char;
     end;
   Psockaddr_ax25 = ^sockaddr_ax25;
   sockaddr_ax25 = record
        sax25_family : sa_family_t;
        sax25_call : ax25_address;
        sax25_ndigis : longint;
     end;

   Pfull_sockaddr_ax25 = ^full_sockaddr_ax25;
   full_sockaddr_ax25 = record
        fsa_ax25 : sockaddr_ax25;
        fsa_digipeater : array[0..(AX25_MAX_DIGIS)-1] of ax25_address;
     end;


type
   Pax25_routes_struct = ^ax25_routes_struct;
   ax25_routes_struct = record
        port_addr : ax25_address;
        dest_addr : ax25_address;
        digi_count : byte;
        digi_addr : array[0..(AX25_MAX_DIGIS)-1] of ax25_address;
     end;

   Pax25_ctl_struct = ^ax25_ctl_struct;
   ax25_ctl_struct = record
        port_addr : ax25_address;
        source_addr : ax25_address;
        dest_addr : ax25_address;
        cmd : dword;
        arg : dword;
        digi_count : byte;
        digi_addr : array[0..(AX25_MAX_DIGIS)-1] of ax25_address;
     end;

   Pax25_info_struct = ^ax25_info_struct;
   ax25_info_struct = record
        n2 : dword;
        n2count : dword;
        t1 : dword;
        t1timer : dword;
        t2 : dword;
        t2timer : dword;
        t3 : dword;
        t3timer : dword;
        idle : dword;
        idletimer : dword;
        state : dword;
        rcv_q : dword;
        snd_q : dword;
     end;

   Pax25_fwd_struct = ^ax25_fwd_struct;
   ax25_fwd_struct = record
        port_from : ax25_address;
        port_to : ax25_address;
     end;

   Pax25_route_opt_struct = ^ax25_route_opt_struct;
   ax25_route_opt_struct = record
        port_addr : ax25_address;
        dest_addr : ax25_address;
        cmd : longint;
        arg : longint;
     end;

   Pax25_bpqaddr_struct = ^ax25_bpqaddr_struct;
   ax25_bpqaddr_struct = record
        dev : array[0..15] of char;
        addr : ax25_address;
     end;


const
   AX25_VALUES_IPDEFMODE = 0;
   AX25_VALUES_AXDEFMODE = 1;
   AX25_VALUES_NETROM = 2;
   AX25_VALUES_TEXT = 3;
   AX25_VALUES_BACKOFF = 4;
   AX25_VALUES_CONMODE = 5;
   AX25_VALUES_WINDOW = 6;
   AX25_VALUES_EWINDOW = 7;
   AX25_VALUES_T1 = 8;
   AX25_VALUES_T2 = 9;
   AX25_VALUES_T3 = 10;
   AX25_VALUES_N2 = 11;
   AX25_VALUES_DIGI = 12;
   AX25_VALUES_IDLE = 13;
   AX25_VALUES_PACLEN = 14;
   AX25_VALUES_IPMAXQUEUE = 15;
   AX25_MAX_VALUES = 20;
type
   Pax25_parms_struct = ^ax25_parms_struct;
   ax25_parms_struct = record
        port_addr : ax25_address;
        values : array[0..(AX25_MAX_VALUES)-1] of word;
     end;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
