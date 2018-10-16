
const
   RIPVERSION = 1;

type
   Pnetinfo = ^netinfo;
   netinfo = record
        rip_dst : sockaddr;
        rip_metric : longint;
     end;

   Prip = ^rip;
   rip = record
        rip_cmd : u_char;
        rip_vers : u_char;
        rip_res1 : array[0..1] of u_char;
        ripun : record
            case longint of
               0 : ( ru_nets : array[0..0] of netinfo );
               1 : ( ru_tracefile : array[0..0] of char );
            end;
     end;


const
   RIPCMD_REQUEST = 1;
   RIPCMD_RESPONSE = 2;
   RIPCMD_TRACEON = 3;
   RIPCMD_TRACEOFF = 4;
   RIPCMD_MAX = 5;

   ripcmds : array [0..RIPCMD_MAX-1] of pchar = ( ''#0, 'REQUEST', 'RESPONSE', 'TRACEON', 'TRACEOFF' );

const
   HOPCNT_INFINITY = 16;
   MAXPACKETSIZE = 512;
   TIMER_RATE = 30;
   SUPPLY_INTERVAL = 30;
   MIN_WAITTIME = 2;
   MAX_WAITTIME = 5;
   EXPIRE_TIME = 180;
   GARBAGE_TIME = 240;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
