

const
   PPP_MTU = 1500;
   PPP_MAXMRU = 65000;
   PPP_VERSION = '2.2.0';
   PPP_MAGIC = $5002;
   PROTO_IPX = $002b;
   PROTO_DNA_RT = $0027;
   SC_COMP_PROT = $00000001;
   SC_COMP_AC = $00000002;
   SC_COMP_TCP = $00000004;
   SC_NO_TCP_CCID = $00000008;
   SC_REJ_COMP_AC = $00000010;
   SC_REJ_COMP_TCP = $00000020;
   SC_CCP_OPEN = $00000040;
   SC_CCP_UP = $00000080;
   SC_ENABLE_IP = $00000100;
   SC_COMP_RUN = $00001000;
   SC_DECOMP_RUN = $00002000;
   SC_DEBUG = $00010000;
   SC_LOG_INPKT = $00020000;
   SC_LOG_OUTPKT = $00040000;
   SC_LOG_RAWIN = $00080000;
   SC_LOG_FLUSH = $00100000;
   SC_MASK = $0fE0ffff;
   SC_ESCAPED = $80000000;
   SC_FLUSH = $40000000;
   SC_VJ_RESET = $20000000;
   SC_XMIT_BUSY = $10000000;
   SC_RCV_ODDP = $08000000;
   SC_RCV_EVNP = $04000000;
   SC_RCV_B7_1 = $02000000;
   SC_RCV_B7_0 = $01000000;
   SC_DC_FERROR = $00800000;
   SC_DC_ERROR = $00400000;

type
   Pnpioctl = ^npioctl;
   npioctl = record
        protocol : longint;
        mode : NPmode;
     end;

   Pppp_option_data = ^ppp_option_data;
   ppp_option_data = record
        ptr : Pu_int8_t;
        length : u_int32_t;
        transmit : longint;
     end;

   Pifpppstatsreq = ^ifpppstatsreq;
   ifpppstatsreq = record
        b : ifreq;
        stats : ppp_stats;
     end;

   Pifpppcstatsreq = ^ifpppcstatsreq;
   ifpppcstatsreq = record
        b : ifreq;
        stats : ppp_comp_stats;
     end;

function PPPIOCGFLAGS: Cardinal;
function PPPIOCSFLAGS: Cardinal;
function PPPIOCGASYNCMAP: Cardinal;
function PPPIOCSASYNCMAP: Cardinal;
function PPPIOCGUNIT: Cardinal;
function PPPIOCGRASYNCMAP: Cardinal;
function PPPIOCSRASYNCMAP: Cardinal;
function PPPIOCGMRU: Cardinal;
function PPPIOCSMRU: Cardinal;
function PPPIOCSMAXCID: Cardinal;
function PPPIOCGXASYNCMAP: Cardinal;
function PPPIOCSXASYNCMAP: Cardinal;
function PPPIOCXFERUNIT: Cardinal;
function PPPIOCSCOMPRESS: Cardinal;
function PPPIOCGNPMODE: Cardinal;
function PPPIOCSNPMODE: Cardinal;
function PPPIOCGDEBUG: Cardinal;
function PPPIOCSDEBUG: Cardinal;
function PPPIOCGIDLE: Cardinal;

    const
       SIOCGPPPSTATS = SIOCDEVPRIVATE + 0;
       SIOCGPPPVER = SIOCDEVPRIVATE + 1;
       SIOCGPPPCSTATS = SIOCDEVPRIVATE + 2;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
