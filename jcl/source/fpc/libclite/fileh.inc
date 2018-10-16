
const
   L_SET = 0;
   L_INCR = 1;
   L_XTND = 2;


function __flock(__fd:longint; __operation:longint):longint;cdecl;external clib name 'flock';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
