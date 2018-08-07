
function ustat(__dev:__dev_t; __ubuf:Pustat):longint;cdecl;external clib name 'ustat';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function ustat(__dev:__dev_t; var __ubuf:_ustat):longint;cdecl;external clib name 'ustat';
