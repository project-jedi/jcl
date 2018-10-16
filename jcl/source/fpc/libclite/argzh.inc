

type

   Perror_t = ^error_t;

function argz_create(__argv:PPchar; __argz:PPchar; __len:Psize_t):error_t;cdecl;external clib name 'argz_create';
function argz_create_sep(__string:Pchar; __sep:longint; __argz:PPchar; __len:Psize_t):error_t;cdecl;external clib name 'argz_create_sep';
function argz_count(__argz:Pchar; __len:size_t):size_t;cdecl;external clib name 'argz_count';
procedure argz_extract(__argz:Pchar; __len:size_t; __argv:PPchar);cdecl;external clib name 'argz_extract';
procedure argz_stringify(__argz:Pchar; __len:size_t; __sep:longint);cdecl;external clib name 'argz_stringify';
function argz_append(__argz:PPchar; __argz_len:Psize_t; __buf:Pchar; __buf_len:size_t):error_t;cdecl;external clib name 'argz_append';
function argz_add(__argz:PPchar; __argz_len:Psize_t; __str:Pchar):error_t;cdecl;external clib name 'argz_add';
function argz_add_sep(__argz:PPchar; __argz_len:Psize_t; __string:Pchar; __delim:longint):error_t;cdecl;external clib name 'argz_add_sep';
procedure argz_delete(__argz:PPchar; __argz_len:Psize_t; __entry:Pchar);cdecl;external clib name 'argz_delete';
function argz_insert(__argz:PPchar; __argz_len:Psize_t; __before:Pchar; __entry:Pchar):error_t;cdecl;external clib name 'argz_insert';
function argz_replace(__argz:PPchar; __argz_len:Psize_t; __str:Pchar; __with:Pchar; __replace_count:Pdword):error_t;cdecl;external clib name 'argz_replace';
function argz_next(__argz:Pchar; __argz_len:size_t; __entry:Pchar):Pchar;cdecl;external clib name 'argz_next';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function argz_create(__argv:PPchar; var __argz: Pchar; var __len: size_t):error_t;cdecl;external clib name 'argz_create';
function argz_create_sep(__string:Pchar; __sep:longint; var __argz: Pchar; var __len: size_t):error_t;cdecl;external clib name 'argz_create_sep';
function argz_append(var __argz: Pchar; var __argz_len: size_t; __buf:Pchar; __buf_len:size_t):error_t;cdecl;external clib name 'argz_append';
function argz_add(var __argz: Pchar; var __argz_len: size_t; __str:Pchar):error_t;cdecl;external clib name 'argz_add';
function argz_add_sep(var __argz: Pchar; var __argz_len: size_t; __string:Pchar; __delim:longint):error_t;cdecl;external clib name 'argz_add_sep';
procedure argz_delete(var __argz: Pchar; var __argz_len: size_t; __entry:Pchar);cdecl;external clib name 'argz_delete';
function argz_insert(var __argz: Pchar; var __argz_len: size_t; __before:Pchar; __entry:Pchar):error_t;cdecl;external clib name 'argz_insert';
function argz_replace(var __argz: Pchar; var __argz_len: size_t; __str:Pchar; __with:Pchar; __replace_count:Pdword):error_t;cdecl;external clib name 'argz_replace';
