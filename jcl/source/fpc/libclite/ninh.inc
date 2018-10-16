

Const
  IPPROTO_IP = 0;
  IPPROTO_HOPOPTS = 0;
  IPPROTO_ICMP = 1;
  IPPROTO_IGMP = 2;
  IPPROTO_IPIP = 4;
  IPPROTO_TCP = 6;
  IPPROTO_EGP = 8;
  IPPROTO_PUP = 12;
  IPPROTO_UDP = 17;
  IPPROTO_IDP = 22;
  IPPROTO_TP = 29;
  IPPROTO_IPV6 = 41;
  IPPROTO_ROUTING = 43;
  IPPROTO_FRAGMENT = 44;
  IPPROTO_RSVP = 46;
  IPPROTO_GRE = 47;
  IPPROTO_ESP = 50;
  IPPROTO_AH = 51;
  IPPROTO_ICMPV6 = 58;
  IPPROTO_NONE = 59;
  IPPROTO_DSTOPTS = 60;
  IPPROTO_MTP = 92;
  IPPROTO_ENCAP = 98;
  IPPROTO_PIM = 103;
  IPPROTO_COMP = 108;
  IPPROTO_RAW = 255;
  IPPROTO_MAX = 256;


type
  Pin_port_t = ^in_port_t;
  in_port_t = uint16_t;

Const
  IPPORT_ECHO = 7;
  IPPORT_DISCARD = 9;
  IPPORT_SYSTAT = 11;
  IPPORT_DAYTIME = 13;
  IPPORT_NETSTAT = 15;
  IPPORT_FTP = 21;
  IPPORT_TELNET = 23;
  IPPORT_SMTP = 25;
  IPPORT_TIMESERVER = 37;
  IPPORT_NAMESERVER = 42;
  IPPORT_WHOIS = 43;
  IPPORT_MTP = 57;
  IPPORT_TFTP = 69;
  IPPORT_RJE = 77;
  IPPORT_FINGER = 79;
  IPPORT_TTYLINK = 87;
  IPPORT_SUPDUP = 95;
  IPPORT_EXECSERVER = 512;
  IPPORT_LOGINSERVER = 513;
  IPPORT_CMDSERVER = 514;
  IPPORT_EFSSERVER = 520;
  IPPORT_BIFFUDP = 512;
  IPPORT_WHOSERVER = 513;
  IPPORT_ROUTESERVER = 520;
  IPPORT_RESERVED = 1024;
  IPPORT_USERRESERVED = 5000;

type

   Pin_addr_t = ^in_addr_t;
   in_addr_t = uint32_t;
   Pin_addr = ^in_addr;
{
   in_addr = record
     s_addr : in_addr_t;
   end;
}


const
   IN_CLASSA_NET = $ff000000;
   IN_CLASSA_NSHIFT = 24;
   IN_CLASSA_HOST = $ffffffff and ( not (IN_CLASSA_NET));
   IN_CLASSA_MAX = 128;

const
   IN_CLASSB_NET = $ffff0000;
   IN_CLASSB_NSHIFT = 16;
   IN_CLASSB_HOST = $ffffffff and ( not (IN_CLASSB_NET));
   IN_CLASSB_MAX = 65536;


const
   IN_CLASSC_NET = $ffffff00;
   IN_CLASSC_NSHIFT = 8;
   IN_CLASSC_HOST = $ffffffff and ( not (IN_CLASSC_NET));

Function IN_CLASSA(a: in_addr_t): Boolean;
Function IN_CLASSB(a: in_addr_t): Boolean;
Function IN_CLASSC(a: in_addr_t): Boolean;
Function IN_CLASSD(a: in_addr_t): Boolean;
Function IN_MULTICAST(a: in_addr_t): Boolean;
Function IN_EXPERIMENTAL(a: in_addr_t): Boolean;
Function IN_BADCLASS(a: in_addr_t): Boolean;


const
  IN_LOOPBACKNET = 127;
  INADDR_LOOPBACK = in_addr_t($7f000001);
  INADDR_UNSPEC_GROUP = in_addr_t($e0000000);
  INADDR_ALLHOSTS_GROUP = in_addr_t($e0000001);
  INADDR_ALLRTRS_GROUP = in_addr_t($0000002);
  INADDR_MAX_LOCAL_GROUP = in_addr_t($e00000ff);

type
   Pin6_addr = ^in6_addr;
   in6_addr = record
     case longint of
      0 : ( in6_u : record
           case longint of
            0 : ( u6_addr8 : array[0..15] of uint8_t );
            1 : ( u6_addr16 : array[0..7] of uint16_t );
            2 : ( u6_addr32 : array[0..3] of uint32_t );
          end;);
    1 : (case Integer of
          0: (s6_addr: packed array [0..16-1] of __uint8_t);
          1: (s6_addr16: packed array [0..8-1] of uint16_t);
          2: (s6_addr32: packed array [0..4-1] of uint32_t);
        );
   end;


Const
  IN6ADDR_ANY_INIT : in6_addr = ( in6_u : (U6_addr8 : (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 )) );
  IN6ADDR_LOOPBACK_INIT : in6_addr = ( in6_u : (U6_addr8 : ( 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 )));

const
  INET_ADDRSTRLEN = 16;
  INET6_ADDRSTRLEN = 46;

Type
  Psockaddr_in6 = ^sockaddr_in6;
  sockaddr_in6 = record
    sin6_family: sa_family_t;
    sin6_port : in_port_t;
    sin6_flowinfo : uint32_t;
    sin6_addr : in6_addr;
    sin6_scope_id : uint32_t;
  end;

  Pipv6_mreq = ^ipv6_mreq;
  ipv6_mreq = record
    ipv6mr_multiaddr : in6_addr;
    ipv6mr_interface : dword;
  end;

type
  in6_pktinfo = {packed} record
    ipi6_addr: in6_addr;         { src/dst IPv6 address }
    ipi6_ifindex: Cardinal;      { send/recv interface index }
  end;

Function IN6_IS_ADDR_UNSPECIFIED(const a: in6_addr): Boolean;
Function IN6_IS_ADDR_LOOPBACK(const a: in6_addr): Boolean;
Function IN6_IS_ADDR_MULTICAST(const a: in6_addr): Boolean;
Function IN6_IS_ADDR_LINKLOCAL(const a: in6_addr): Boolean;
Function IN6_IS_ADDR_SITELOCAL(const a: in6_addr): Boolean;
Function IN6_IS_ADDR_V4MAPPED(const a: in6_addr): Boolean;
Function IN6_IS_ADDR_V4COMPAT(const a: in6_addr): Boolean;
Function IN6_ARE_ADDR_EQUAL(const a, b: in6_addr): Boolean;
Function IN6_IS_ADDR_MC_NODELOCAL(const a: in6_addr): Boolean;
Function IN6_IS_ADDR_MC_LINKLOCAL(const a: in6_addr): Boolean;
Function IN6_IS_ADDR_MC_SITELOCAL(const a: in6_addr): Boolean;
Function IN6_IS_ADDR_MC_ORGLOCAL(const a: in6_addr): Boolean;
Function IN6_IS_ADDR_MC_GLOBAL(const a: in6_addr): Boolean;

function ntohl(__netlong:uint32_t):uint32_t;cdecl;external clib name 'ntohl';
function ntohs(__netshort:uint16_t):uint16_t;cdecl;external clib name 'ntohs';
function htonl(__hostlong:uint32_t):uint32_t;cdecl;external clib name 'htonl';
function htons(__hostshort:uint16_t):uint16_t;cdecl;external clib name 'htons';

function bindresvport6(__sockfd:longint; __sock_in:Psockaddr_in6):longint;cdecl;external clib name 'bindresvport6';
function bindresvport(__sockfd:longint; __sock_in:Psockaddr_in):longint;cdecl;external clib name 'bindresvport';

