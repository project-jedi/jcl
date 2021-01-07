
type
   Ppasswd = ^passwd;
   passwd = record
        pw_name : Pchar;
        pw_passwd : Pchar;
        pw_uid : __uid_t;
        pw_gid : __gid_t;
        pw_gecos : Pchar;
        pw_dir : Pchar;
        pw_shell : Pchar;
     end;
   PPpasswd = ^Ppasswd;

procedure setpwent;cdecl;external clib name 'setpwent';
procedure endpwent;cdecl;external clib name 'endpwent';
function getpwent:Ppasswd;cdecl;external clib name 'getpwent';
function fgetpwent(__stream:PFILE):Ppasswd;cdecl;external clib name 'fgetpwent';
function putpwent(__p:Ppasswd; __f:PFILE):longint;cdecl;external clib name 'putpwent'; overload;
function getpwuid(__uid:__uid_t):Ppasswd;cdecl;external clib name 'getpwuid';
function getpwnam(__name:Pchar):Ppasswd;cdecl;external clib name 'getpwnam';

const
   NSS_BUFLEN_PASSWD = 1024;

function getpwent_r(__resultbuf:Ppasswd; __buffer:Pchar; __buflen:size_t; __result:PPpasswd):longint;cdecl;external clib name 'getpwent_r'; overload;
function getpwuid_r(__uid:__uid_t; __resultbuf:Ppasswd; __buffer:Pchar; __buflen:size_t; __result:PPpasswd):longint;cdecl;external clib name 'getpwuid_r'; overload;
function getpwnam_r(__name:Pchar; __resultbuf:Ppasswd; __buffer:Pchar; __buflen:size_t; __result:PPpasswd):longint;cdecl;external clib name 'getpwnam_r'; overload;
function fgetpwent_r(__stream:PFILE; __resultbuf:Ppasswd; __buffer:Pchar; __buflen:size_t; __result:PPpasswd):longint;cdecl;external clib name 'fgetpwent_r'; overload;
function getpw(__uid:__uid_t; __buffer:Pchar):longint;cdecl;external clib name 'getpw';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TPasswordRecord = passwd;
  PPasswordRecord = ^TPasswordRecord;

function putpwent(const __p:passwd; __f:PFILE):longint;cdecl;external clib name 'putpwent'; overload;
function getpwent_r(var __resultbuf:passwd; __buffer:Pchar; __buflen:size_t; var __result:Ppasswd):longint;cdecl;external clib name 'getpwent_r'; overload;
function getpwuid_r(__uid:__uid_t; var __resultbuf:passwd; __buffer:Pchar; __buflen:size_t; var __result: Ppasswd):longint;cdecl;external clib name 'getpwuid_r'; overload;
function getpwnam_r(__name:Pchar; var __resultbuf:passwd; __buffer:Pchar; __buflen:size_t; var __result:Ppasswd):longint;cdecl;external clib name 'getpwnam_r'; overload;
function fgetpwent_r(__stream:PFILE; var __resultbuf:passwd; __buffer:Pchar; __buflen:size_t; var __result:Ppasswd):longint;cdecl;external clib name 'fgetpwent_r'; overload;
