
type

   Piconv_t = ^iconv_t;
   iconv_t = pointer;

function iconv_open(__tocode:Pchar; __fromcode:Pchar):iconv_t;cdecl;external clib name 'iconv_open';
function iconv(__cd:iconv_t; __inbuf:PPchar; __inbytesleft:Psize_t; __outbuf:PPchar; __outbytesleft:Psize_t):size_t;cdecl;external clib name 'iconv';
function iconv_close(__cd:iconv_t):longint;cdecl;external clib name 'iconv_close';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

function iconv(__cd:iconv_t; var __inbuf:Pchar; var __inbytesleft:size_t; var __outbuf:Pchar; var __outbytesleft: size_t):size_t;cdecl;external clib name 'iconv';
