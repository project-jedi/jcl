

function inet_addr(__cp:Pchar):in_addr_t;cdecl;external clib name 'inet_addr';
function inet_lnaof(__in:in_addr):in_addr_t;cdecl;external clib name 'inet_lnaof';
function inet_makeaddr(__net:in_addr_t; __host:in_addr_t): in_addr_t;cdecl;external clib name 'inet_makeaddr';
function inet_netof(__in:in_addr):in_addr_t;cdecl;external clib name 'inet_netof';
function inet_network(__cp:Pchar):in_addr_t;cdecl;external clib name 'inet_network';
function inet_ntoa(__in:in_addr):Pchar;cdecl;external clib name 'inet_ntoa';
function inet_pton(__af:longint; __cp:Pchar; __buf:pointer):longint;cdecl;external clib name 'inet_pton';
function inet_ntop(__af:longint; __cp:pointer; __buf:Pchar; __len:socklen_t):Pchar;cdecl;external clib name 'inet_ntop';
function inet_aton(__cp:Pchar; __inp:Pin_addr):in_addr_t;cdecl;external clib name 'inet_aton';
function inet_neta(__net:in_addr_t; __buf:Pchar; __len:size_t):Pchar;cdecl;external clib name 'inet_neta';
function inet_net_ntop(__af:longint; __cp:pointer; __bits:longint; __buf:Pchar; __len:size_t):Pchar;cdecl;external clib name 'inet_net_ntop';
function inet_net_pton(__af:longint; __cp:Pchar; __buf:pointer; __len:size_t):longint;cdecl;external clib name 'inet_net_pton';
function inet_nsap_addr(__cp:Pchar; __buf:Pbyte; __len:longint):dword;cdecl;external clib name 'inet_nsap_addr';
function inet_nsap_ntoa(__len:longint; __cp:Pbyte; __buf:Pchar):Pchar;cdecl;external clib name 'inet_nsap_ntoa';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function inet_aton(__cp:Pchar; var __inp:in_addr):in_addr_t;cdecl;external clib name 'inet_aton';
