
Const
  UL_GETFSIZE = 1;
  UL_SETFSIZE = 2;
  __UL_GETMAXBRK = 3;
  __UL_GETOPENMAX = 4;


function ulimit(__cmd:longint; args:array of const):longint;cdecl;external clib name 'ulimit';
function ulimit(__cmd:longint):longint;cdecl;varargs;external clib name 'ulimit';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
