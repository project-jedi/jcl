
const
   IF_NAMESIZE = 16;

type
   Pif_nameindex = ^_if_nameindex;
   _if_nameindex = record
        if_index : dword;
        if_name : Pchar;
     end;
   P_if_nameindex = ^_if_nameindex;

   Const
     IFF_UP = $1;
     IFF_BROADCAST = $2;
     IFF_DEBUG = $4;
     IFF_LOOPBACK = $8;
     IFF_POINTOPOINT = $10;
     IFF_NOTRAILERS = $20;
     IFF_RUNNING = $40;
     IFF_NOARP = $80;
     IFF_PROMISC = $100;
     IFF_ALLMULTI = $200;
     IFF_MASTER = $400;
     IFF_SLAVE = $800;
     IFF_MULTICAST = $1000;
     IFF_PORTSEL = $2000;
     IFF_AUTOMEDIA = $4000;

type
   Pifaddr = ^ifaddr;
   ifaddr = record
        ifa_addr : sockaddr;
        ifa_ifu : record
            case longint of
               0 : ( ifu_broadaddr : sockaddr );
               1 : ( ifu_dstaddr : sockaddr );
            end;
        ifa_ifp : Pointer; // Piface;
        ifa_next : Pifaddr;
     end;

   Pifmap = ^ifmap;
   ifmap = record
        mem_start : dword;
        mem_end : dword;
        base_addr : word;
        irq : byte;
        dma : byte;
        port : byte;
     end;


const
   IFHWADDRLEN = 6;
   IFNAMSIZ = IF_NAMESIZE;

type
   Pifreq = ^ifreq;
   ifreq = record
        ifr_ifrn : record
            case longint of
               0 : ( ifrn_name : array[0..(IFNAMSIZ)-1] of char );
            end;
        ifr_ifru : record
            case longint of
               0 : ( ifru_addr : sockaddr );
               1 : ( ifru_dstaddr : sockaddr );
               2 : ( ifru_broadaddr : sockaddr );
               3 : ( ifru_netmask : sockaddr );
               4 : ( ifru_hwaddr : sockaddr );
               5 : ( ifru_flags : smallint );
               6 : ( ifru_ivalue : longint );
               7 : ( ifru_mtu : longint );
               8 : ( ifru_map : ifmap );
               9 : ( ifru_slave : array[0..(IFNAMSIZ)-1] of char );
               10 : ( ifru_newname : array[0..(IFNAMSIZ)-1] of char );
               11 : ( ifru_data : __caddr_t );
            end;
     end;

   Pifconf = ^ifconf;
   ifconf = record
        ifc_len : longint;
        ifc_ifcu : record
            case longint of
               0 : ( ifcu_buf : __caddr_t );
               1 : ( ifcu_req : Pifreq );
            end;
     end;


function if_nametoindex(__ifname:Pchar):dword;cdecl;external clib name 'if_nametoindex';
function if_indextoname(__ifindex:dword; __ifname:Pchar):Pchar;cdecl;external clib name 'if_indextoname';
function if_nameindex:Pif_nameindex;cdecl;external clib name 'if_nameindex';
procedure if_freenameindex(__ptr:Pif_nameindex);cdecl;external clib name 'if_freenameindex';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TIfNameIndex = _if_nameindex;
  PIfNameIndex = ^TIfNameIndex;

  TIfAddr = ifaddr;
  TIFreq = ifreq;
