
const

   SOL_ROSE = 260;
   ROSE_MTU = 251;
   ROSE_MAX_DIGIS = 6;
   ROSE_DEFER = 1;
   ROSE_T1 = 2;
   ROSE_T2 = 3;
   ROSE_T3 = 4;
   ROSE_IDLE = 5;
   ROSE_QBITINCL = 6;
   ROSE_HOLDBACK = 7;
   SIOCRSGCAUSE = SIOCPROTOPRIVATE + 0;
   SIOCRSSCAUSE = SIOCPROTOPRIVATE + 1;
   SIOCRSL2CALL = SIOCPROTOPRIVATE + 2;
   SIOCRSSL2CALL = SIOCPROTOPRIVATE + 2;
   SIOCRSACCEPT = SIOCPROTOPRIVATE + 3;
   SIOCRSCLRRT = SIOCPROTOPRIVATE + 4;
   SIOCRSGL2CALL = SIOCPROTOPRIVATE + 5;
   SIOCRSGFACILITIES = SIOCPROTOPRIVATE + 6;
   ROSE_DTE_ORIGINATED = $00;
   ROSE_NUMBER_BUSY = $01;
   ROSE_INVALID_FACILITY = $03;
   ROSE_NETWORK_CONGESTION = $05;
   ROSE_OUT_OF_ORDER = $09;
   ROSE_ACCESS_BARRED = $0B;
   ROSE_NOT_OBTAINABLE = $0D;
   ROSE_REMOTE_PROCEDURE = $11;
   ROSE_LOCAL_PROCEDURE = $13;
   ROSE_SHIP_ABSENT = $39;
type

   Prose_address = ^rose_address;
   rose_address = record
        rose_addr : array[0..4] of char;
     end;
   Psockaddr_rose = ^sockaddr_rose;
   sockaddr_rose = record
        srose_family : sa_family_t;
        srose_addr : rose_address;
        srose_call : ax25_address;
        srose_ndigis : longint;
        srose_digi : ax25_address;
     end;

   Pfull_sockaddr_rose = ^full_sockaddr_rose;
   full_sockaddr_rose = record
        srose_family : sa_family_t;
        srose_addr : rose_address;
        srose_call : ax25_address;
        srose_ndigis : dword;
        srose_digis : array[0..(ROSE_MAX_DIGIS)-1] of ax25_address;
     end;

   Prose_route_struct = ^rose_route_struct;
   rose_route_struct = record
        address : rose_address;
        mask : word;
        neighbour : ax25_address;
        device : array[0..15] of char;
        ndigis : byte;
        digipeaters : array[0..(AX25_MAX_DIGIS)-1] of ax25_address;
     end;

   Prose_cause_struct = ^rose_cause_struct;
   rose_cause_struct = record
        cause : byte;
        diagnostic : byte;
     end;

   Prose_facilities_struct = ^rose_facilities_struct;
   rose_facilities_struct = record
        source_addr : rose_address;
        dest_addr : rose_address;
        source_call : ax25_address;
        dest_call : ax25_address;
        source_ndigis : byte;
        dest_ndigis : byte;
        source_digis : array[0..(ROSE_MAX_DIGIS)-1] of ax25_address;
        dest_digis : array[0..(ROSE_MAX_DIGIS)-1] of ax25_address;
        rand : dword;
        fail_addr : rose_address;
        fail_call : ax25_address;
     end;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
