type
  __RLIMIT_RESOURCE_T = __RLIMIT_RESOURCE;
  __RUSAGE_T = RUSAGE;
  __RUSAGE_WHO_T = __rusage_who;
  __priority_which_t = __priority_which;

function getrlimit(__resource:__rlimit_resource_t; __rlimits:Prlimit):longint;cdecl;external clib name 'getrlimit';
function getrlimit64(__resource:__rlimit_resource_t; __rlimits:Prlimit64):longint;cdecl;external clib name 'getrlimit64';
function setrlimit(__resource:__rlimit_resource_t; __rlimits:Prlimit):longint;cdecl;external clib name 'setrlimit';
function setrlimit64(__resource:__rlimit_resource_t; __rlimits:Prlimit64):longint;cdecl;external clib name 'setrlimit64';

function getrusage(__who:__rusage_who_t; __usage:Prusage):longint;cdecl;external clib name 'getrusage';
function getpriority(__which:__priority_which_t; __who:id_t):longint;cdecl;external clib name 'getpriority';
function setpriority(__which:__priority_which_t; __who:id_t; __prio:longint):longint;cdecl;external clib name 'setpriority';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type

Function setrlimit (__resource: __rlimit_resource_t; const __rlimits: TRLimit): longint; cdecl; external clib name 'setrlimit';
