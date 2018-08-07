
function ioperm(__from:dword; __num:dword; __turn_on:longint):longint;cdecl;external clib name 'ioperm';
function iopl(__level:longint):longint;cdecl;external clib name 'iopl';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
