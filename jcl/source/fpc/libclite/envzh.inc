
function envz_entry(__envz:Pchar; __envz_len:size_t; __name:Pchar):Pchar;cdecl;external clib name 'envz_entry';
function envz_get(__envz:Pchar; __envz_len:size_t; __name:Pchar):Pchar;cdecl;external clib name 'envz_get';
function envz_add(__envz:PPchar; __envz_len:Psize_t; __name:Pchar; __value:Pchar):error_t;cdecl;external clib name 'envz_add';
function envz_merge(__envz:PPchar; __envz_len:Psize_t; __envz2:Pchar; __envz2_len:size_t; __override:longint):error_t;cdecl;external clib name 'envz_merge';
procedure envz_remove(__envz:PPchar; __envz_len:Psize_t; __name:Pchar);cdecl;external clib name 'envz_remove';
procedure envz_strip(__envz:PPchar; __envz_len:Psize_t);cdecl;external clib name 'envz_strip';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function envz_add(var __envz:Pchar; var __envz_len: size_t; __name:Pchar; __value:Pchar):error_t;cdecl;external clib name 'envz_add';
function envz_merge(var __envz: Pchar; var __envz_len: size_t; __envz2:Pchar; __envz2_len:size_t; __override:longint):error_t;cdecl;external clib name 'envz_merge';
procedure envz_remove(var __envz:Pchar; var __envz_len: size_t; __name:Pchar);cdecl;external clib name 'envz_remove';
procedure envz_strip(var __envz:Pchar; var __envz_len: size_t);cdecl;external clib name 'envz_strip';
