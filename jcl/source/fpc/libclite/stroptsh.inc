
function isastream(__fildes:longint):longint;cdecl;external clib name 'isastream';
function getmsg(__fildes:longint; __ctlptr:Pstrbuf; __dataptr:Pstrbuf; __flagsp:Plongint):longint;cdecl;external clib name 'getmsg';
function getpmsg(__fildes:longint; __ctlptr:Pstrbuf; __dataptr:Pstrbuf; __bandp:Plongint; __flagsp:Plongint):longint;cdecl;external clib name 'getpmsg';
function putmsg(__fildes:longint; __ctlptr:Pstrbuf; __dataptr:Pstrbuf; __flags:longint):longint;cdecl;external clib name 'putmsg';
function putpmsg(__fildes:longint; __ctlptr:Pstrbuf; __dataptr:Pstrbuf; __band:longint; __flags:longint):longint;cdecl;external clib name 'putpmsg';
function fattach(__fildes:longint; __path:Pchar):longint;cdecl;external clib name 'fattach';
function fdetach(__path:Pchar):longint;cdecl;external clib name 'fdetach';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
