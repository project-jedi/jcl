
type
  SOCKLEN_T = __socklen_t;
  PSOCKLEN_T = ^SOCKLEN_T;

const

  INVALID_SOCKET = -1;
  SOCKET_ERROR = -1;
  INADDR_ANY = 0;
  INADDR_BROADCAST = -1;
  INADDR_NONE = $FFFFFFFF;

Type
   __socket_type = (
  SOCK_STREAM := 1,
  SOCK_DGRAM := 2,
  SOCK_RAW := 3,
  SOCK_RDM := 4,
  SOCK_SEQPACKET := 5,
  SOCK_PACKET := 10
  );

Const
  PF_UNSPEC = 0;
  PF_LOCAL = 1;
  PF_UNIX = PF_LOCAL;
  PF_FILE = PF_LOCAL;
  PF_INET = 2;
  PF_AX25 = 3;
  PF_IPX = 4;
  PF_APPLETALK = 5;
  PF_NETROM = 6;
  PF_BRIDGE = 7;
  PF_ATMPVC = 8;
  PF_X25 = 9;
  PF_INET6 = 10;
  PF_ROSE = 11;
  PF_DECnet = 12;
  PF_NETBEUI = 13;
  PF_SECURITY = 14;
  PF_KEY = 15;
  PF_NETLINK = 16;
  PF_ROUTE = PF_NETLINK;
  PF_PACKET = 17;
  PF_ASH = 18;
  PF_ECONET = 19;
  PF_ATMSVC = 20;
  PF_SNA = 22;
  PF_IRDA = 23;
  PF_PPPOX = 24;
  PF_WANPIPE = 25;
  PF_BLUETOOTH = 31;
  PF_MAX = 32;
  AF_UNSPEC = PF_UNSPEC;
  AF_LOCAL = PF_LOCAL;
  AF_UNIX = PF_UNIX;
  AF_FILE = PF_FILE;
  AF_INET = PF_INET;
  AF_AX25 = PF_AX25;
  AF_IPX = PF_IPX;
  AF_APPLETALK = PF_APPLETALK;
  AF_NETROM = PF_NETROM;
  AF_BRIDGE = PF_BRIDGE;
  AF_ATMPVC = PF_ATMPVC;
  AF_X25 = PF_X25;
  AF_INET6 = PF_INET6;
  AF_ROSE = PF_ROSE;
  AF_DECnet = PF_DECnet;
  AF_NETBEUI = PF_NETBEUI;
  AF_SECURITY = PF_SECURITY;
  AF_KEY = PF_KEY;
  AF_NETLINK = PF_NETLINK;
  AF_ROUTE = PF_ROUTE;
  AF_PACKET = PF_PACKET;
  AF_ASH = PF_ASH;
  AF_ECONET = PF_ECONET;
  AF_ATMSVC = PF_ATMSVC;
  AF_SNA = PF_SNA;
  AF_IRDA = PF_IRDA;
  AF_PPPOX = PF_PPPOX;
  AF_WANPIPE = PF_WANPIPE;
  AF_BLUETOOTH = PF_BLUETOOTH;
  AF_MAX = PF_MAX;
  SOL_RAW = 255;
  SOL_DECNET = 261;
  SOL_X25 = 262;
  SOL_PACKET = 263;
  SOL_ATM = 264;
  SOL_AAL = 265;
  SOL_IRDA = 266;
  SOMAXCONN = 128;


Type
   __ss_aligntype = __uint32_t;

const
   _SS_SIZE = 128;
   _SS_PADSIZE = _SS_SIZE - (2 * SizeOf(__ss_aligntype));

type
 {  // replaced by structure down below.
    // Why borland decided to change it is a mystery to me :/
   sockaddr = record
     sa_family : sa_family_t ;
     sa_data : array [0..13] of char;
   end;
 }
   // redefined as osockaddr.
   osockaddr = record
     sa_family : sa_family_t ;
     sa_data : array [0..13] of char;
   end;

   Psockaddr_storage = ^sockaddr_storage;
   sockaddr_storage = record
      __ss__family : sa_family_t;
      __ss_align   : __ss_aligntype;
      __ss_padding : array[0..(_SS_PADSIZE)-1] of char;
   end;


Const
  MSG_OOB = $01;
  MSG_PEEK = $02;
  MSG_DONTROUTE = $04;
  MSG_TRYHARD = MSG_DONTROUTE;
  MSG_CTRUNC = $08;
  MSG_PROXY = $10;
  MSG_TRUNC = $20;
  MSG_DONTWAIT = $40;
  MSG_EOR = $80;
  MSG_WAITALL = $100;
  MSG_FIN = $200;
  MSG_SYN = $400;
  MSG_CONFIRM = $800;
  MSG_RST = $1000;
  MSG_ERRQUEUE = $2000;
  MSG_NOSIGNAL = $4000;
  MSG_MORE = $8000;

type
  Pmsghdr = ^msghdr;
  msghdr = record
     msg_name : pointer;
     msg_namelen : socklen_t;
     msg_iov : Piovec;
     msg_iovlen : longint;
     msg_control : pointer;
     msg_controllen : socklen_t;
     msg_flags : longint;
  end;

  Pcmsghdr = ^cmsghdr;
  cmsghdr = record
    cmsg_len : size_t;
    cmsg_level : longint;
    cmsg_type : longint;
  end;

function CMSG_DATA(cmsg : pointer) : Pbyte;
function CMSG_NXTHDR(mhdr: Pmsghdr; cmsg: Pcmsghdr): Pcmsghdr;
function CMSG_FIRSTHDR(mhdr: Pmsghdr): Pcmsghdr;
function CMSG_ALIGN(len: size_t): size_t;
function CMSG_SPACE(len: size_t): size_t;
function CMSG_LEN(len: size_t): size_t;


function __cmsg_nxthdr(__mhdr:Pmsghdr; __cmsg:Pcmsghdr):Pcmsghdr;cdecl;external clib name '__cmsg_nxthdr';

const
  SCM_RIGHTS    = $01;
  SCM_CREDENTIALS = $02;
  SCM_CONNECT   = $03;



type
  Pucred = ^ucred;
  ucred = record
    pid : pid_t;
    uid : uid_t;
    gid : gid_t;
  end;

  Plinger = ^linger;
  linger = record
    l_onoff : longint;
    l_linger : longint;
  end;






{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

type
  SunB = record
    s_b1,
    s_b2,
    s_b3,
    s_b4: u_char;
  end;

  SunW = record
    s_w1,
    s_w2: u_short;
  end;

  in_addr =  record
    case Integer of
      0: (S_un_b: SunB);
      1: (S_un_w: SunW);
      2: (S_addr: u_long);
  end;
  TInAddr = in_addr;
  PInAddr = ^TInAddr;

  sockaddr = {packed} record
    case Integer of
      0: (sa_family: sa_family_t;
          sa_data: packed array[0..13] of Byte);
      1: (sin_family: sa_family_t;
          sin_port: u_short;
          sin_addr: TInAddr;
          sin_zero: packed array[0..7] of Byte);
  end;
  TSockAddr = sockaddr;
  PSockAddr = ^TSockAddr;

  TMessageHeader = msghdr;
  PMessageHeader = ^TMessageHeader;

  TCMessageHeader = cmsghdr;
  PCMessageHeader = ^TCMessageHeader;

  sockaddr_in = sockaddr;
  Psockaddr_in = ^sockaddr;
  TSockAddrIn = sockaddr_in;
  PSockAddrIn = ^TSockAddrIn;

  TSocket = longint;

function SA_LEN(const buf): Cardinal; // Untyped buffer; this is *unsafe*.
function __libc_sa_len(__af: sa_family_t): Integer; cdecl;external clib name '__libc_sa_len';

{ Borland compatibility }

Type
  PSocketLength = ^socklen_t;
  