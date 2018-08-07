
const
  FSETLOCKING_QUERY    = 0;
  FSETLOCKING_INTERNAL = 1;
  FSETLOCKING_BYCALLER = 2;

function __fbufsize(__fp:PFILE):size_t;cdecl;external clib name '__fbufsize';
function __freading(__fp:PFILE):longint;cdecl;external clib name '__freading';
function __fwriting(__fp:PFILE):longint;cdecl;external clib name '__fwriting';
function __freadable(__fp:PFILE):longint;cdecl;external clib name '__freadable';
function __fwritable(__fp:PFILE):longint;cdecl;external clib name '__fwritable';
function __flbf(__fp:PFILE):longint;cdecl;external clib name '__flbf';
procedure __fpurge(__fp:PFILE);cdecl;external clib name '__fpurge';
function __fpending(__fp:PFILE):size_t;cdecl;external clib name '__fpending';
procedure _flushlbf;cdecl;external clib name '_flushlbf';
function __fsetlocking(__fp:PFILE; __type:longint):longint;cdecl;external clib name '__fsetlocking';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
