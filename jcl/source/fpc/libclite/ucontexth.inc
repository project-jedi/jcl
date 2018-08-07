
type
  TMakeContextProc = procedure; // Used anonymously in header file.

function getcontext(__ucp:Pucontext_t):longint;cdecl;external clib name 'getcontext';
function setcontext(__ucp:Pucontext_t):longint;cdecl;external clib name 'setcontext';
function swapcontext(__oucp:Pucontext_t; __ucp:Pucontext_t):longint;cdecl;external clib name 'swapcontext';
procedure makecontext(__ucp:Pucontext_t; __func:TMakeContextProc; __argc:longint; args:array of const);cdecl;external clib name 'makecontext';
procedure makecontext(__ucp:Pucontext_t; __func:TMakeContextProc; __argc:longint);cdecl;varargs;external clib name 'makecontext';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function getcontext(var __ucp:ucontext_t):longint;cdecl;external clib name 'getcontext';
function setcontext(const __ucp: ucontext_t):longint;cdecl;external clib name 'setcontext';
function swapcontext(var __oucp:ucontext_t; const __ucp: ucontext_t):longint;cdecl;external clib name 'swapcontext';
procedure makecontext(var __ucp:ucontext_t; __func:TMakeContextProc; __argc:longint; args:array of const);cdecl;external clib name 'makecontext';
procedure makecontext(var __ucp:ucontext_t; __func:TMakeContextProc; __argc:longint);cdecl;external clib name 'makecontext';
