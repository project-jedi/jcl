
const
   _NETINET_ETHER_H = 1;

function ether_ntoa(__addr:Pether_addr):Pchar;cdecl;external clib name 'ether_ntoa';
function ether_ntoa_r(__addr:Pether_addr; __buf:Pchar):Pchar;cdecl;external clib name 'ether_ntoa_r';
function ether_aton(__asc:Pchar):Pether_addr;cdecl;external clib name 'ether_aton';
function ether_aton_r(__asc:Pchar; __addr:Pether_addr):Pether_addr;cdecl;external clib name 'ether_aton_r';
function ether_ntohost(__hostname:Pchar; __addr:Pether_addr):longint;cdecl;external clib name 'ether_ntohost';
function ether_hostton(__hostname:Pchar; __addr:Pether_addr):longint;cdecl;external clib name 'ether_hostton';
function ether_line(__line:Pchar; __addr:Pether_addr; __hostname:Pchar):longint;cdecl;external clib name 'ether_line';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
