
type
   Pgroup = ^group;
   group = record
        gr_name : Pchar;
        gr_passwd : Pchar;
        gr_gid : __gid_t;
        gr_mem : ^Pchar;
     end;
   PPgroup = ^Pgroup;

procedure setgrent;cdecl;external clib name 'setgrent';
procedure endgrent;cdecl;external clib name 'endgrent';
function getgrent:Pgroup;cdecl;external clib name 'getgrent';
function fgetgrent(__stream:PFILE):Pgroup;cdecl;external clib name 'fgetgrent';
function putgrent(__p:Pgroup; __f:PFILE):longint;cdecl;external clib name 'putgrent';
function getgrgid(__gid:__gid_t):Pgroup;cdecl;external clib name 'getgrgid';
function getgrnam(__name:Pchar):Pgroup;cdecl;external clib name 'getgrnam';

const
   NSS_BUFLEN_GROUP = 1024;

function getgrent_r(__resultbuf:Pgroup; __buffer:Pchar; __buflen:size_t; __result:PPgroup):longint;cdecl;external clib name 'getgrent_r';
function getgrgid_r(__gid:__gid_t; __resultbuf:Pgroup; __buffer:Pchar; __buflen:size_t; __result:PPgroup):longint;cdecl;external clib name 'getgrgid_r';
function getgrnam_r(__name:Pchar; __resultbuf:Pgroup; __buffer:Pchar; __buflen:size_t; __result:PPgroup):longint;cdecl;external clib name 'getgrnam_r';
function fgetgrent_r(__stream:PFILE; __resultbuf:Pgroup; __buffer:Pchar; __buflen:size_t; __result:PPgroup):longint;cdecl;external clib name 'fgetgrent_r';
function setgroups(__n:size_t; __groups:P__gid_t):longint;cdecl;external clib name 'setgroups';
function getgrouplist(__user:Pchar; __group:__gid_t; __groups:P__gid_t; __ngroups:Plongint):longint;cdecl;external clib name 'getgrouplist';
function initgroups(__user:Pchar; __group:__gid_t):longint;cdecl;external clib name 'initgroups';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TGroup = group;

function putgrent(const __p: group; __f:PFILE):longint;cdecl;external clib name 'putgrent';
function getgrent_r(var __resultbuf: group; __buffer:Pchar; __buflen:size_t; var __result: Pgroup):longint;cdecl;external clib name 'getgrent_r';
function getgrgid_r(__gid:__gid_t; var __resultbuf:group; __buffer:Pchar; __buflen:size_t; var __result:Pgroup):longint;cdecl;external clib name 'getgrgid_r';
function getgrnam_r(__name:Pchar; var __resultbuf:group; __buffer:Pchar; __buflen:size_t; var __result:Pgroup):longint;cdecl;external clib name 'getgrnam_r';
function fgetgrent_r(__stream:PFILE; var __resultbuf:group; __buffer:Pchar; __buflen:size_t; var __result:Pgroup):longint;cdecl;external clib name 'fgetgrent_r';
function getgrouplist(__user:Pchar; __group:__gid_t; var __groups: __gid_t; var __ngroups:longint):longint;cdecl;external clib name 'getgrouplist';

