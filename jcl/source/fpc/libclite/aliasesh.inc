
type
 Paliasent = ^aliasent;
 aliasent = record
   alias_name : Pchar;
   alias_members_len : size_t;
   alias_members : ^Pchar;
   alias_local : longint;
 end;
 PPaliasent = ^Paliasent;

procedure setaliasent;cdecl;external clib name 'setaliasent';
procedure endaliasent;cdecl;external clib name 'endaliasent';
function getaliasent:Paliasent;cdecl;external clib name 'getaliasent';
function getaliasent_r(__result_buf:Paliasent; __buffer:Pchar; __buflen:size_t; __result:PPaliasent):longint;cdecl;external clib name 'getaliasent_r';
function getaliasbyname(__name:Pchar):Paliasent;cdecl;external clib name 'getaliasbyname';
function getaliasbyname_r(__name:Pchar; __result_buf:Paliasent; __buffer:Pchar; __buflen:size_t; __result:PPaliasent):longint;cdecl;external clib name 'getaliasbyname_r';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TAliasEntry = aliasent;
  PAliasEntry = ^TAliasEntry;

function getaliasent_r(var __result_buf:aliasent; __buffer:Pchar; __buflen:size_t; var __result:Paliasent):longint;cdecl;external clib name 'getaliasent_r';
function getaliasbyname_r(__name:Pchar; var __result_buf:aliasent; __buffer:Pchar; __buflen:size_t; var __result:Paliasent):longint;cdecl;external clib name 'getaliasbyname_r';
