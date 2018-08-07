
const
   NL_SETD = 1;
   NL_CAT_LOCALE = 1;

type

   Pnl_catd = ^nl_catd;
   nl_catd = pointer;

   Pnl_item = ^nl_item;
   nl_item = longint;

function catopen(__cat_name:Pchar; __flag:longint):nl_catd;cdecl;external clib name 'catopen';
function catgets(__catalog:nl_catd; __set:longint; __number:longint; __string:Pchar):Pchar;cdecl;external clib name 'catgets';
function catclose(__catalog:nl_catd):longint;cdecl;external clib name 'catclose';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
