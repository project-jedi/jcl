
 Const
     WRDE_DOOFFS = 1 shl 0;
     WRDE_APPEND = 1 shl 1;
     WRDE_NOCMD = 1 shl 2;
     WRDE_REUSE = 1 shl 3;
     WRDE_SHOWERR = 1 shl 4;
     WRDE_UNDEF = 1 shl 5;
     __WRDE_FLAGS = ((((WRDE_DOOFFS or WRDE_APPEND) or WRDE_NOCMD) or WRDE_REUSE) or WRDE_SHOWERR) or WRDE_UNDEF;

Type
   Pwordexp_t = ^wordexp_t;
   wordexp_t = record
        we_wordc : size_t;
        we_wordv : ^Pchar;
        we_offs : size_t;
     end;

Const
     WRDE_NOSYS = -(1);
     WRDE_NOSPACE = 1;
     WRDE_BADCHAR = 2;
     WRDE_BADVAL = 3;
     WRDE_CMDSUB = 4;
     WRDE_SYNTAX = 5;

function wordexp(__words:Pchar; __pwordexp:Pwordexp_t; __flags:longint):longint;cdecl;external clib name 'wordexp';
procedure wordfree(__wordexp:Pwordexp_t);cdecl;external clib name 'wordfree';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TWordExp = wordexp_t;
  PWordExp = ^TWordExp;

function wordexp(__words:Pchar; var __pwordexp: wordexp_t; __flags:longint):longint;cdecl;external clib name 'wordexp';
procedure wordfree(var __wordexp: wordexp_t);cdecl;external clib name 'wordfree';

