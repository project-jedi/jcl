
Const
  SHUT_RD = 0;
  SHUT_WR = 1;
  SHUT_RDWR = 2;

function socket(__domain:longint; __type:longint; __protocol:longint):longint;cdecl;external clib name 'socket';
function socketpair(__domain:longint; __type:longint; __protocol:longint; __fds: plongint):longint;cdecl;external clib name 'socketpair';
function bind(__fd:longint; __addr:Psockaddr; __len:socklen_t):longint;cdecl;external clib name 'bind';
function getsockname(__fd:longint; __addr:Psockaddr; __len:Psocklen_t):longint;cdecl;external clib name 'getsockname';
function connect(__fd:longint; __addr:Psockaddr; __len:socklen_t):longint;cdecl;external clib name 'connect';
function getpeername(__fd:longint; __addr:Psockaddr; __len:Psocklen_t):longint;cdecl;external clib name 'getpeername';
function send(__fd:longint; __buf:pointer; __n:size_t; __flags:longint):ssize_t;cdecl;external clib name 'send';
function recv(__fd:longint; __buf:pointer; __n:size_t; __flags:longint):ssize_t;cdecl;external clib name 'recv';
function sendto(__fd:longint; __buf:pointer; __n:size_t; __flags:longint; __addr:Psockaddr;  __addr_len:socklen_t):ssize_t;cdecl;external clib name 'sendto';
function recvfrom(__fd:longint; __buf:pointer; __n:size_t; __flags:longint; __addr:Psockaddr;   __addr_len:Psocklen_t):ssize_t;cdecl;external clib name 'recvfrom';
function sendmsg(__fd:longint; __message:Pmsghdr; __flags:longint):ssize_t;cdecl;external clib name 'sendmsg';
function recvmsg(__fd:longint; __message:Pmsghdr; __flags:longint):ssize_t;cdecl;external clib name 'recvmsg';
function getsockopt(__fd:longint; __level:longint; __optname:longint; __optval:pointer; __optlen:Psocklen_t):longint;cdecl;external clib name 'getsockopt';
function setsockopt(__fd:longint; __level:longint; __optname:longint; __optval:pointer; __optlen:socklen_t):longint;cdecl;external clib name 'setsockopt';
function listen(__fd:longint; __n:longint):longint;cdecl;external clib name 'listen';
function accept(__fd:longint; __addr:Psockaddr; __addr_len:Psocklen_t):longint;cdecl;external clib name 'accept';
function shutdown(__fd:longint; __how:longint):longint;cdecl;external clib name 'shutdown';
function isfdtype(__fd:longint; __fdtype:longint):longint;cdecl;external clib name 'isfdtype';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

type
  TSocketPair = packed array[0..1] of TSocket;

function socket(__domain: Integer; __type: __socket_type; __protocol: Integer): TSocket; cdecl;external clib name 'socket';
function socketpair(__domain, __type, __protocol: Integer; var __fds: TSocketPair): Integer; cdecl;external clib name 'socketpair';
function socketpair(__domain: Integer; __type: __socket_type; __protocol: Integer; var __fds: TSocketPair): Integer; cdecl;external clib name 'socketpair';

function bind(__fd:longint; var __addr: sockaddr; __len:socklen_t):longint;cdecl;external clib name 'bind';
function getsockname(__fd:longint; var __addr: sockaddr; var __len: socklen_t):longint;cdecl;external clib name 'getsockname';
function connect(__fd:longint; const __addr: sockaddr; __len:socklen_t):longint;cdecl;external clib name 'connect';
function getpeername(__fd:longint; var __addr: sockaddr; var __len: socklen_t):longint;cdecl;external clib name 'getpeername';
function sendto(__fd:longint; __buf:pointer; __n:size_t; __flags:longint; var __addr: sockaddr;  __addr_len:socklen_t):ssize_t;cdecl;external clib name 'sendto';
function recvfrom(__fd:longint; __buf:pointer; __n:size_t; __flags:longint; var __addr: sockaddr;   __addr_len:Psocklen_t):ssize_t;cdecl;external clib name 'recvfrom';
function sendmsg(__fd:longint; const __message: msghdr; __flags:longint):ssize_t;cdecl;external clib name 'sendmsg';
function recvmsg(__fd:longint; var __message: msghdr; __flags:longint):ssize_t;cdecl;external clib name 'recvmsg';
function getsockopt(__fd:longint; __level:longint; __optname:longint; __optval:pointer; var __optlen: socklen_t):longint;cdecl;external clib name 'getsockopt';
function accept(__fd:longint; var __addr: sockaddr; var __addr_len:socklen_t):longint;cdecl;external clib name 'accept';
function send(__fd:longint; const __buf; __n:size_t; __flags:longint):ssize_t;cdecl;external clib name 'send';
function recv(__fd:longint; var __buf; __n:size_t; __flags:longint):ssize_t;cdecl;external clib name 'recv';
function sendto(__fd:longint; const __buf; __n:size_t; __flags:longint; __addr:Psockaddr;  __addr_len:socklen_t):ssize_t;cdecl;external clib name 'sendto';
function recvfrom(__fd:longint; var __buf; __n:size_t; __flags:longint; __addr:Psockaddr;   __addr_len:Psocklen_t):ssize_t;cdecl;external clib name 'recvfrom';
