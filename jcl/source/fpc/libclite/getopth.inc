
var
  optarg : Pchar;cvar;external;
  optind : longint;cvar;external;
  opterr : longint;cvar;external;
  optopt : longint;cvar;external;

type
   Poption = ^option;
   option = record
     name : Pchar;
     has_arg : longint;
     flag : Plongint;
     val : longint;
   end;


const
   no_argument = 0;
   required_argument = 1;
   optional_argument = 2;


function getopt(___argc:longint; ___argv:PPchar; __shortopts:Pchar):longint;cdecl;external clib name 'getopt';
function getopt_long(___argc:longint; ___argv:PPchar; __shortopts:Pchar; __longopts:Poption; __longind:Plongint):longint;cdecl;external clib name 'getopt_long';
function getopt_long_only(___argc:longint; ___argv:PPchar; __shortopts:Pchar; __longopts:Poption; __longind:Plongint):longint;cdecl;external clib name 'getopt_long_only';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function getopt_long(___argc:longint; ___argv:PPchar; __shortopts:Pchar; const __longopts:option; var __longind:longint):longint;cdecl;external clib name 'getopt_long';
function getopt_long_only(___argc:longint; ___argv:PPchar; __shortopts:Pchar; const __longopts:option; var __longind:longint):longint;cdecl;external clib name 'getopt_long_only';
