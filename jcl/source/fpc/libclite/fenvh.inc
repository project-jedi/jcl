
function feclearexcept(__excepts:longint):longint;cdecl;external clib name 'feclearexcept';
function fegetexceptflag(__flagp:Pfexcept_t; __excepts:longint):longint;cdecl;external clib name 'fegetexceptflag';
function feraiseexcept(__excepts:longint):longint;cdecl;external clib name 'feraiseexcept';
function fesetexceptflag(__flagp:Pfexcept_t; __excepts:longint):longint;cdecl;external clib name 'fesetexceptflag';
function fetestexcept(__excepts:longint):longint;cdecl;external clib name 'fetestexcept';
function fegetround:longint;cdecl;external clib name 'fegetround';
function fesetround(__rounding_direction:longint):longint;cdecl;external clib name 'fesetround';
function fegetenv(__envp:Pfenv_t):longint;cdecl;external clib name 'fegetenv';
function feholdexcept(__envp:Pfenv_t):longint;cdecl;external clib name 'feholdexcept';
function fesetenv(__envp:Pfenv_t):longint;cdecl;external clib name 'fesetenv';
function feupdateenv(__envp:Pfenv_t):longint;cdecl;external clib name 'feupdateenv';
function feenableexcept(__excepts:longint):longint;cdecl;external clib name 'feenableexcept';
function fedisableexcept(__excepts:longint):longint;cdecl;external clib name 'fedisableexcept';
function fegetexcept:longint;cdecl;external clib name 'fegetexcept';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function fegetexceptflag(var __flagp:fexcept_t; __excepts:longint):longint;cdecl;external clib name 'fegetexceptflag';
function fesetexceptflag(const __flagp:fexcept_t; __excepts:longint):longint;cdecl;external clib name 'fesetexceptflag';
function fegetenv(var __envp:fenv_t):longint;cdecl;external clib name 'fegetenv';
function feholdexcept(var __envp:fenv_t):longint;cdecl;external clib name 'feholdexcept';
function fesetenv(const __envp:fenv_t):longint;cdecl;external clib name 'fesetenv';
function feupdateenv(var __envp:fenv_t):longint;cdecl;external clib name 'feupdateenv';
