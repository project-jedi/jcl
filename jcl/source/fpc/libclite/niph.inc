
Type
   Ptimestamp = ^timestamp;
   timestamp = record
        len : u_int8_t;
        ptr : u_int8_t;
        flag0 : dword;
        data : array[0..8] of u_int32_t;
     end;

   Piphdr = ^iphdr;
   iphdr = record
        flag0 : byte;
        tos : u_int8_t;
        tot_len : u_int16_t;
        id : u_int16_t;
        frag_off : u_int16_t;
        ttl : u_int8_t;
        protocol : u_int8_t;
        check : u_int16_t;
        saddr : u_int32_t;
        daddr : u_int32_t;
     end;

   Pip = ^ip;
   ip = record
        flag0 : byte;
        ip_tos : u_int8_t;
        ip_len : u_short;
        ip_id : u_short;
        ip_off : u_short;
        ip_ttl : u_int8_t;
        ip_p : u_int8_t;
        ip_sum : u_short;
        ip_src : in_addr;
        ip_dst : in_addr;
     end;

const
   IP_RF = $8000;
   IP_DF = $4000;
   IP_MF = $2000;
   IP_OFFMASK = $1fff;

type
   Pip_timestamp = ^ip_timestamp;
   ip_timestamp = record
        ipt_code : u_int8_t;
        ipt_len : u_int8_t;
        ipt_ptr : u_int8_t;
        flag0 : dword;
        data : array[0..8] of u_int32_t;
     end;

const
   IPVERSION = 4;
   IP_MAXPACKET = 65535;
   IPTOS_TOS_MASK = $1E;

function IPTOS_TOS(tos : longint) : longint;


const
   IPTOS_LOWDELAY = $10;
   IPTOS_THROUGHPUT = $08;
   IPTOS_RELIABILITY = $04;
   IPTOS_LOWCOST = $02;
   IPTOS_MINCOST = IPTOS_LOWCOST;
   IPTOS_PREC_MASK = $e0;

function IPTOS_PREC(tos : longint) : longint;


const
   IPTOS_PREC_NETCONTROL = $e0;
   IPTOS_PREC_INTERNETCONTROL = $c0;
   IPTOS_PREC_CRITIC_ECP = $a0;
   IPTOS_PREC_FLASHOVERRIDE = $80;
   IPTOS_PREC_FLASH = $60;
   IPTOS_PREC_IMMEDIATE = $40;
   IPTOS_PREC_PRIORITY = $20;
   IPTOS_PREC_ROUTINE = $00;
   IPOPT_COPY = $80;
   IPOPT_CLASS_MASK = $60;
   IPOPT_NUMBER_MASK = $1f;

function IPOPT_COPIED(o : longint) : longint;

function IPOPT_CLASS(o : longint) : longint;

function IPOPT_NUMBER(o : longint) : longint;


const
   IPOPT_CONTROL = $00;
   IPOPT_RESERVED1 = $20;
   IPOPT_DEBMEAS = $40;
   IPOPT_MEASUREMENT = IPOPT_DEBMEAS;
   IPOPT_RESERVED2 = $60;
   IPOPT_EOL = 0;
   IPOPT_END = IPOPT_EOL;
   IPOPT_NOP = 1;
   IPOPT_NOOP = IPOPT_NOP;
   IPOPT_RR = 7;
   IPOPT_TS = 68;
   IPOPT_TIMESTAMP = IPOPT_TS;
   IPOPT_SECURITY = 130;
   IPOPT_SEC = IPOPT_SECURITY;
   IPOPT_LSRR = 131;
   IPOPT_SATID = 136;
   IPOPT_SID = IPOPT_SATID;
   IPOPT_SSRR = 137;
   IPOPT_RA = 148;
   IPOPT_OPTVAL = 0;
   IPOPT_OLEN = 1;
   IPOPT_OFFSET = 2;
   IPOPT_MINOFF = 4;
   MAX_IPOPTLEN = 40;
   IPOPT_TS_TSONLY = 0;
   IPOPT_TS_TSANDADDR = 1;
   IPOPT_TS_PRESPEC = 3;
   IPOPT_SECUR_UNCLASS = $0000;
   IPOPT_SECUR_CONFID = $f135;
   IPOPT_SECUR_EFTO = $789a;
   IPOPT_SECUR_MMMM = $bc4d;
   IPOPT_SECUR_RESTR = $af13;
   IPOPT_SECUR_SECRET = $d788;
   IPOPT_SECUR_TOPSECRET = $6bc5;
   MAXTTL = 255;
   IPDEFTTL = 64;
   IPFRAGTTL = 60;
   IPTTLDEC = 1;
   IP_MSS = 576;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
