
type
  TMakeContextProc = procedure; // Used anonymously in header file.

function getcontext(__ucp:Pucontext_t):longint;cdecl;external clib name 'getcontext'; overload;
function setcontext(__ucp:Pucontext_t):longint;cdecl;external clib name 'setcontext'; overload;
function swapcontext(__oucp:Pucontext_t; __ucp:Pucontext_t):longint;cdecl;external clib name 'swapcontext'; overload;
procedure makecontext(__ucp:Pucontext_t; __func:TMakeContextProc; __argc:longint; args:array of const);cdecl;external clib name 'makecontext'; overload;
procedure makecontext(__ucp:Pucontext_t; __func:TMakeContextProc; __argc:longint);cdecl;varargs;external clib name 'makecontext'; overload;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function getcontext(var __ucp:ucontext_t):longint;cdecl;external clib name 'getcontext'; overload;
function setcontext(const __ucp: ucontext_t):longint;cdecl;external clib name 'setcontext'; overload;
function swapcontext(var __oucp:ucontext_t; const __ucp: ucontext_t):longint;cdecl;external clib name 'swapcontext'; overload;
procedure makecontext(var __ucp:ucontext_t; __func:TMakeContextProc; __argc:longint; args:array of const);cdecl;external clib name 'makecontext'; overload;
procedure makecontext(var __ucp:ucontext_t; __func:TMakeContextProc; __argc:longint);cdecl;external clib name 'makecontext'; overload;
