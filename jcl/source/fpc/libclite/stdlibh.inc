
type

   Pdiv_t = ^div_t;
   div_t = record
        quot : longint;
        rem : longint;
     end;

   Pldiv_t = ^ldiv_t;
   ldiv_t = record
        quot : longint;
        rem : longint;
     end;

   Plldiv_t = ^lldiv_t;
   lldiv_t = record
        quot : int64;
        rem : int64;
     end;

const
   RAND_MAX = 2147483647;
   EXIT_FAILURE = 1;
   EXIT_SUCCESS = 0;

function MB_CUR_MAX : size_t;cdecl; external clib name '__ctype_get_mb_cur_max';
function __ctype_get_mb_cur_max:size_t;cdecl;external clib name '__ctype_get_mb_cur_max';
function atof(__nptr:Pchar):double;cdecl;external clib name 'atof';
function atoi(__nptr:Pchar):longint;cdecl;external clib name 'atoi';
function atol(__nptr:Pchar):longint;cdecl;external clib name 'atol';
function atoll(__nptr:Pchar):int64;cdecl;external clib name 'atoll';
function strtod(__nptr:Pchar; __endptr:PPchar):double;cdecl;external clib name 'strtod';
function strtof(__nptr:Pchar; __endptr:PPchar):double;cdecl;external clib name 'strtof';
function strtold(NumPtr: PChar; EndPtr: PPChar): Extended; cdecl; external clib name 'strtold';
function strtol(__nptr:Pchar; __endptr:PPchar; __base:longint):longint;cdecl;external clib name 'strtol';
function strtoul(__nptr:Pchar; __endptr:PPchar; __base:longint):dword;cdecl;external clib name 'strtoul';
function strtoq(__nptr:Pchar; __endptr:PPchar; __base:longint):int64;cdecl;external clib name 'strtoq';
function strtouq(__nptr:Pchar; __endptr:PPchar; __base:longint):qword;cdecl;external clib name 'strtouq';
function strtoll(__nptr:Pchar; __endptr:PPchar; __base:longint):int64;cdecl;external clib name 'strtoll';
function strtoull(__nptr:Pchar; __endptr:PPchar; __base:longint):qword;cdecl;external clib name 'strtoull';
function __strtol_l(__nptr:Pchar; __endptr:PPchar; __base:longint; __loc:__locale_t):longint;cdecl;external clib name '__strtol_l';
function __strtoul_l(__nptr:Pchar; __endptr:PPchar; __base:longint; __loc:__locale_t):dword;cdecl;external clib name '__strtoul_l';
function __strtoll_l(__nptr:Pchar; __endptr:PPchar; __base:longint; __loc:__locale_t):int64;cdecl;external clib name '__strtoll_l';
function __strtoull_l(__nptr:Pchar; __endptr:PPchar; __base:longint; __loc:__locale_t):qword;cdecl;external clib name '__strtoull_l';
function __strtod_l(__nptr:Pchar; __endptr:PPchar; __loc:__locale_t):double;cdecl;external clib name '__strtod_l';
function __strtof_l(__nptr:Pchar; __endptr:PPchar; __loc:__locale_t):double;cdecl;external clib name '__strtof_l';
function __strtold_l(__nptr: PChar; __endptr: PPChar;__loc: __locale_t): Extended; cdecl;external clib name '__strtold_l';
function __strtod_internal(__nptr:Pchar; __endptr:PPchar; __group:longint):double;cdecl;external clib name '__strtod_internal';
function __strtof_internal(__nptr:Pchar; __endptr:PPchar; __group:longint):double;cdecl;external clib name '__strtof_internal';
function __strtold_internal(__nptr: PChar; __endptr: PPChar;  __group: longint): Extended; cdecl;external clib name '__strtold_internal';
function __strtol_internal(__nptr:Pchar; __endptr:PPchar; __base:longint; __group:longint):longint;cdecl;external clib name '__strtol_internal';
function __strtoul_internal(__nptr:Pchar; __endptr:PPchar; __base:longint; __group:longint):dword;cdecl;external clib name '__strtoul_internal';
function __strtoll_internal(__nptr:Pchar; __endptr:PPchar; __base:longint; __group:longint):int64;cdecl;external clib name '__strtoll_internal';
function __strtoull_internal(__nptr:Pchar; __endptr:PPchar; __base:longint; __group:longint):qword;cdecl;external clib name '__strtoull_internal';
function a64l(__s:Pchar):longint;cdecl;external clib name 'a64l';
function l64a(N: longint): PChar; cdecl;external clib name 'l64a';

function __random:longint;cdecl;external clib name 'random';
procedure srandom(__seed:dword);cdecl;external clib name 'srandom';
function initstate(__seed:dword; __statebuf:Pchar; __statelen:size_t):Pchar;cdecl;external clib name 'initstate';
function setstate(__statebuf:Pchar):Pchar;cdecl;external clib name 'setstate';

Type
  Prandom_data = ^random_data;
  random_data = record
       fptr : Pint32_t;
       rptr : Pint32_t;
       state : Pint32_t;
       rand_type : longint;
       rand_deg : longint;
       rand_sep : longint;
       end_ptr : Pint32_t;
    end;


function random_r(__buf:Prandom_data; __result:Pint32_t):longint;cdecl;external clib name 'random_r';
function srandom_r(__seed:dword; __buf:Prandom_data):longint;cdecl;external clib name 'srandom_r';
function initstate_r(__seed:dword; __statebuf:Pchar; __statelen:size_t; __buf:Prandom_data):longint;cdecl;external clib name 'initstate_r';
function setstate_r(__statebuf:Pchar; __buf:Prandom_data):longint;cdecl;external clib name 'setstate_r';
function rand:longint;cdecl;external clib name 'rand';
procedure srand(__seed:dword);cdecl;external clib name 'srand';
function rand_r(__seed:Pdword):longint;cdecl;external clib name 'rand_r';
function drand48:double;cdecl;external clib name 'drand48';

Type
  TRandWordArray = Array[0..2] of word;

function erand48(var __xsubi: trandwordarray):double;cdecl;external clib name 'erand48';
function lrand48:longint;cdecl;external clib name 'lrand48';
function nrand48(var __xsubi: trandwordarray):longint;cdecl;external clib name 'nrand48';
function mrand48:longint;cdecl;external clib name 'mrand48';
function jrand48(var __xsubi:trandwordarray):longint;cdecl;external clib name 'jrand48';
procedure srand48(__seedval:longint);cdecl;external clib name 'srand48';
function seed48(var __seed16v:trandwordarray):Pword;cdecl;external clib name 'seed48';
procedure lcong48(var __param:trandwordarray);cdecl;external clib name 'lcong48';

type
  Pdrand48_data = ^drand48_data;
  drand48_data = record
       __x : array[0..2] of word;
       __old_x : array[0..2] of word;
       __c : word;
       __init : word;
       __a : qword;
    end;

function drand48_r(__buffer:Pdrand48_data; __result:Pdouble):longint;cdecl;external clib name 'drand48_r';
function erand48_r(var __xsubi: TRandWordArray; __buffer:Pdrand48_data; __result:Pdouble):longint;cdecl;external clib name 'erand48_r';
function lrand48_r(__buffer:Pdrand48_data; __result:Plongint):longint;cdecl;external clib name 'lrand48_r';
function nrand48_r(var __xsubi: TRandWordArray; __buffer:Pdrand48_data; __result:Plongint):longint;cdecl;external clib name 'nrand48_r';
function mrand48_r(__buffer:Pdrand48_data; __result:Plongint):longint;cdecl;external clib name 'mrand48_r';
function jrand48_r(var __xsubi: TRandWordArray; __buffer:Pdrand48_data; __result:Plongint):longint;cdecl;external clib name 'jrand48_r';
function srand48_r(__seedval:longint; __buffer:Pdrand48_data):longint;cdecl;external clib name 'srand48_r';
function seed48_r(var __seed16v: TRandWordArray; __buffer:Pdrand48_data):longint;cdecl;external clib name 'seed48_r';
function lcong48_r(__param: pword; __buffer:Pdrand48_data):longint;cdecl;external clib name 'lcong48_r';

procedure __abort; cdecl; external clib name 'abort';

type
  TAtExitProc = procedure; cdecl;
  TOnExitProc = procedure(status: longint; arg: Pointer); cdecl;

function atexit(__func:TAtExitProc):longint;cdecl;external clib name 'atexit';
function on_exit(__func:TOnExitProc; __arg:pointer):longint;cdecl;external clib name 'on_exit';
procedure __exit(Status: longint); cdecl;external clib name 'exit';
//procedure _Exit (__status : longint);cdecl; external clib name '_exit';

function getenv(__name:Pchar):Pchar;cdecl;external clib name 'getenv';
function __secure_getenv(__name:Pchar):Pchar;cdecl;external clib name '__secure_getenv';
function putenv(__string:Pchar):longint;cdecl;external clib name 'putenv';
function setenv(__name:Pchar; __value:Pchar; __replace:longint):longint;cdecl;external clib name 'setenv';
function unsetenv(__name:Pchar):longint;cdecl;external clib name 'unsetenv';
function clearenv:longint;cdecl;external clib name 'clearenv';
function mktemp(__template:Pchar):Pchar;cdecl;external clib name 'mktemp';
function mkstemp(__template:Pchar):longint;cdecl;external clib name 'mkstemp';
function mkstemp64(__template:Pchar):longint;cdecl;external clib name 'mkstemp64';
function mkdtemp(__template:Pchar):Pchar;cdecl;external clib name 'mkdtemp';
// **************** detect whether system symbol is hidden. Should be as of 1.9.4
// **************** End of detect
function __system(__command:Pchar):longint;cdecl;external clib name 'system';
function system(__command:Pchar):longint;cdecl;external clib name 'system';
function canonicalize_file_name(__name:Pchar):Pchar;cdecl;external clib name 'canonicalize_file_name';
function realpath(__name:Pchar; __resolved:Pchar):Pchar;cdecl;external clib name 'realpath';

Type
  __compar_fn_t = function (_para1:pointer; _para2:pointer):longint;cdecl;
  comparison_fn_t = __compar_fn_t;

function bsearch(__key:pointer; __base:pointer; __nmemb:size_t; __size:size_t; __compar:__compar_fn_t):pointer;cdecl;external clib name 'bsearch';
procedure qsort(__base:pointer; __nmemb:size_t; __size:size_t; __compar:__compar_fn_t);cdecl;external clib name 'qsort';

function __abs(X: longint): longint; cdecl;external clib name 'abs';
function labs(X: Longint): Longint; cdecl;external clib name 'labs';
function llabs(X: Int64): Int64; cdecl;external clib name 'llabs';
function __div(Numer: longint; Denom: longint): div_t; cdecl;external clib name 'div';
function ldiv(Numer: Longint; Denom: Longint): ldiv_t; cdecl;external clib name 'ldiv';
function lldiv(Numer: Int64; Denom: Int64): lldiv_t; cdecl;external clib name 'lldiv';

function ecvt(__value:double; __ndigit:longint; __decpt:Plongint; __sign:Plongint):Pchar;cdecl;external clib name 'ecvt';
function fcvt(__value:double; __ndigit:longint; __decpt:Plongint; __sign:Plongint):Pchar;cdecl;external clib name 'fcvt';
function gcvt(__value:double; __ndigit:longint; __buf:Pchar):Pchar;cdecl;external clib name 'gcvt';
function qecvt(__value: extended; __ndigit: longint; __decpt: Plongint;  __sign: plongint): PChar; cdecl;external clib name 'qecvt';
function qfcvt(__value: extended; __ndigit: longint; __decpt: plongint;  __sign: plongint): PChar; cdecl;external clib name 'qfcvt';
function qgcvt(__value: extended; __ndigit: longint;  __buf: PChar): PChar; cdecl;external clib name 'qgcvt';

function ecvt_r(__value:double; __ndigit:longint; __decpt:Plongint; __sign:Plongint; __buf:Pchar; __len:size_t):longint;cdecl;external clib name 'ecvt_r';
function fcvt_r(__value:double; __ndigit:longint; __decpt:Plongint; __sign:Plongint; __buf:Pchar; __len:size_t):longint;cdecl;external clib name 'fcvt_r';
function qecvt_r(Value: Extended; __ndigit: longint; __decpt: Plongint; __sign: plongint; Buf: PChar; Len: size_t): longint; cdecl;external clib name 'qecvt_r';
function qfcvt_r(Value: Extended; __ndigit: longint; __decpt: plongint; __sign: plongint; Buf: PChar; Len: size_t): longint; cdecl;external clib name 'qfcvt_r';

function mblen(__s:Pchar; __n:size_t):longint;cdecl;external clib name 'mblen';
function mbtowc(__pwc:Pwchar_t; __s:Pchar; __n:size_t):longint;cdecl;external clib name 'mbtowc';
function wctomb(__s:Pchar; __wchar:wchar_t):longint;cdecl;external clib name 'wctomb';
function mbstowcs(__pwcs:Pwchar_t; __s:Pchar; __n:size_t):size_t;cdecl;external clib name 'mbstowcs';
function wcstombs(__s:Pchar; __pwcs:Pwchar_t; __n:size_t):size_t;cdecl;external clib name 'wcstombs';
function rpmatch(__response:Pchar):longint;cdecl;external clib name 'rpmatch';
function getsubopt(__optionp:PPchar; __tokens:PPchar; __valuep:PPchar):longint;cdecl;external clib name 'getsubopt';
procedure setkey(__key:Pchar);cdecl;external clib name 'setkey';
function posix_openpt(__oflag:longint):longint;cdecl;external clib name 'posix_openpt';
function grantpt(__fd:longint):longint;cdecl;external clib name 'grantpt';
function unlockpt(__fd:longint):longint;cdecl;external clib name 'unlockpt';
function ptsname(__fd:longint):Pchar;cdecl;external clib name 'ptsname';
function ptsname_r(__fd:longint; __buf:Pchar; __buflen:size_t):longint;cdecl;external clib name 'ptsname_r';
function getpt:longint;cdecl;external clib name 'getpt';
function getloadavg(__loadavg:array of double; __nelem:longint):longint;cdecl;external clib name 'getloadavg';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TRandomData = random_data;
  PRandomData = ^TRandomData;

  TDrand48Data = drand48_data;
  PDrand48Data = ^TDrand48Data;

function erand48(__xsubi: pword):double;cdecl;external clib name 'erand48';
function nrand48(__xsubi: pword):longint;cdecl;external clib name 'nrand48';
function jrand48(__xsubi: pword):longint;cdecl;external clib name 'jrand48';
function seed48(__seed16v: pword):Pword;cdecl;external clib name 'seed48';
procedure lcong48(__param: pword);cdecl;external clib name 'lcong48';

function erand48_r(__xsubi:pword; __buffer:Pdrand48_data; __result:Pdouble):longint;cdecl;external clib name 'erand48_r';
function nrand48_r(__xsubi:pword; __buffer:Pdrand48_data; __result:Plongint):longint;cdecl;external clib name 'nrand48_r';
function jrand48_r(__xsubi:pword; __buffer:Pdrand48_data; __result:Plongint):longint;cdecl;external clib name 'jrand48_r';
function seed48_r(__seed16v:pword; __buffer:Pdrand48_data):longint;cdecl;external clib name 'seed48_r';

