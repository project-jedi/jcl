
type
   Pqelem = ^qelem;
   qelem = record
        q_forw : Pqelem;
        q_back : Pqelem;
        q_data : array[0..0] of char;
     end;

procedure insque(__elem:pointer; __prev:pointer);cdecl;external clib name 'insque';
procedure remque(__elem:pointer);cdecl;external clib name 'remque';

type
//   __compar_fn_t = function (_para1:pointer; _para2:pointer):longint;cdecl;

   Pcomparison_fn_t = ^comparison_fn_t;
//   comparison_fn_t = __compar_fn_t;

type
  hsearch_ACTION = (FIND, ENTER);
     PACTION = ^ACTION;
   ACTION =  Longint;

{   Pentry = ^entry;
   entry = record
        key : Pchar;
        data : pointer;
     end;
}
   Phsearch_ENTRY = ^hsearch_ENTRY;
   hsearch_ENTRY = record
       {undefined structure}
     end;
   PPhsearch_ENTRY = ^Phsearch_ENTRY;


function hsearch(__item:hsearch_ENTRY; __action:hsearch_ACTION):Phsearch_ENTRY;cdecl;external clib name 'hsearch';
function hcreate(__nel:size_t):longint;cdecl;external clib name 'hcreate';
procedure hdestroy;cdecl;external clib name 'hdestroy';

type
   Phsearch_data = ^hsearch_data;
   hsearch_data = record
     table : Phsearch_ENTRY;
     size : dword;
     filled : dword;
   end;


function hsearch_r(__item:hsearch_ENTRY; __action:hsearch_ACTION; __retval:PPhsearch_ENTRY; __htab:Phsearch_data):longint;cdecl;external clib name 'hsearch_r';
function hcreate_r(__nel:size_t; __htab:Phsearch_data):longint;cdecl;external clib name 'hcreate_r';
procedure hdestroy_r(__htab:Phsearch_data);cdecl;external clib name 'hdestroy_r';

type
  PSearch_VISIT = ^tsearch_VISIT;
  tsearch_VISIT = (preorder, postorder, endorder, leaf);

function tsearch(__key:pointer; __rootp:Ppointer; __compar:__compar_fn_t):pointer;cdecl;external clib name 'tsearch';
function tfind(__key:pointer; __rootp:Ppointer; __compar:__compar_fn_t):pointer;cdecl;external clib name 'tfind';
function tdelete(__key:pointer; __rootp:Ppointer; __compar:__compar_fn_t):pointer;cdecl;external clib name 'tdelete';

type
   __action_fn_t = procedure (__nodep:pointer; __value:tsearch_VISIT; __level:longint);cdecl;

procedure twalk(__root:pointer; __action:__action_fn_t);cdecl;external clib name 'twalk';

type
   __free_fn_t = procedure (__nodep:pointer);cdecl;

procedure tdestroy(__root:pointer; __freefct:__free_fn_t);cdecl;external clib name 'tdestroy';
function lfind(__key:pointer; __base:pointer; __nmemb:Psize_t; __size:size_t; __compar:__compar_fn_t):pointer;cdecl;external clib name 'lfind';
function lsearch(__key:pointer; __base:pointer; __nmemb:Psize_t; __size:size_t; __compar:__compar_fn_t):pointer;cdecl;external clib name 'lsearch';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  PQElement = ^TQElement;
  TQElement = qelem;

  THSearchEntry = hsearch_ENTRY;
  PHSearchEntry = ^THSearchEntry;

  THSearchEntryOpaque = hsearch_ENTRY;
  PHSearchEntryOpaque = ^THSearchEntryOpaque;

function hsearch_r(__item:hsearch_ENTRY; __action:hsearch_ACTION; var __retval:Phsearch_ENTRY; __htab:Phsearch_data):longint;cdecl;external clib name 'hsearch_r';

function tsearch(__key:pointer; var __rootp:pointer; __compar:__compar_fn_t):pointer;cdecl;external clib name 'tsearch';
function tfind(__key:pointer; var __rootp:pointer; __compar:__compar_fn_t):pointer;cdecl;external clib name 'tfind';
function tdelete(__key:pointer; var __rootp:pointer; __compar:__compar_fn_t):pointer;cdecl;external clib name 'tdelete';

