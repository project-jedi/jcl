
type
   mcheck_status =  (
     MCHECK_DISABLED := -(1),
     MCHECK_OK := 0,
     MCHECK_FREE := 1,
     MCHECK_HEAD := 2,
     MCHECK_TAIL := 3
   );

type
  TMemCheckAbortProc = procedure(MCheckStatus: mcheck_status); cdecl; // Used anonymously in header file


function mcheck(__abortfunc: TMemCheckAbortProc):longint;cdecl;external clib name 'mcheck';
function mcheck_pedantic(__abortfunc:TMemCheckAbortProc):longint;cdecl;external clib name 'mcheck_pedantic';
procedure mcheck_check_all;cdecl;external clib name 'mcheck_check_all';
function mprobe(__ptr:pointer):mcheck_status;cdecl;external clib name 'mprobe';
procedure mtrace;cdecl;external clib name 'mtrace';
procedure muntrace;cdecl;external clib name 'muntrace';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
