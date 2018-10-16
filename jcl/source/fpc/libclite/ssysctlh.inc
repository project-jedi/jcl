

function sysctl(__name:Plongint; __nlen:longint; __oldval:pointer; __oldlenp:Psize_t; __newval:pointer;
           __newlen:size_t):longint;cdecl;external clib name 'sysctl';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
