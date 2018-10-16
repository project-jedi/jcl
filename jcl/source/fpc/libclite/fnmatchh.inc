

    const
       FNM_PATHNAME = 1 shl 0;
       FNM_NOESCAPE = 1 shl 1;
       FNM_PERIOD = 1 shl 2;

       FNM_FILE_NAME = FNM_PATHNAME;
       FNM_LEADING_DIR = 1 shl 3;
       FNM_CASEFOLD = 1 shl 4;
       FNM_EXTMATCH = 1 shl 5;

    const
       FNM_NOMATCH = 1;

       FNM_NOSYS = -(1);

function fnmatch(__pattern:Pchar; __name:Pchar; __flags:longint):longint;cdecl;external clib name 'fnmatch';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
