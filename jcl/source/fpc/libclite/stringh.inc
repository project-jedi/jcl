

function memcpy(__dest:pointer; __src:pointer; __n:size_t):pointer;cdecl;external clib name 'memcpy';
function memmove(__dest:pointer; __src:pointer; __n:size_t):pointer;cdecl;external clib name 'memmove';
function memccpy(__dest:pointer; __src:pointer; __c:longint; __n:size_t):pointer;cdecl;external clib name 'memccpy';
function memset(__s:pointer; __c:longint; __n:size_t):pointer;cdecl;external clib name 'memset';
function memcmp(__s1:pointer; __s2:pointer; __n:size_t):longint;cdecl;external clib name 'memcmp';
function memchr(__s:pointer; __c:longint; __n:size_t):pointer;cdecl;external clib name 'memchr';
function rawmemchr(__s:pointer; __c:longint):pointer;cdecl;external clib name 'rawmemchr';
function memrchr(__s:pointer; __c:longint; __n:size_t):pointer;cdecl;external clib name 'memrchr';
function strcpy(__dest:Pchar; __src:Pchar):Pchar;cdecl;external clib name 'strcpy';
function strncpy(__dest:Pchar; __src:Pchar; __n:size_t):Pchar;cdecl;external clib name 'strncpy';
function __strcat(__dest:Pchar; __src:Pchar):Pchar;cdecl;external clib name 'strcat';
function strncat(__dest:Pchar; __src:Pchar; __n:size_t):Pchar;cdecl;external clib name 'strncat';
function strcmp(__s1:Pchar; __s2:Pchar):longint;cdecl;external clib name 'strcmp';
function strncmp(__s1:Pchar; __s2:Pchar; __n:size_t):longint;cdecl;external clib name 'strncmp';
function strcoll(__s1:Pchar; __s2:Pchar):longint;cdecl;external clib name 'strcoll';
function strxfrm(__dest:Pchar; __src:Pchar; __n:size_t):size_t;cdecl;external clib name 'strxfrm';
function __strcoll_l(__s1:Pchar; __s2:Pchar; __l:__locale_t):longint;cdecl;external clib name '__strcoll_l';
function __strxfrm_l(__dest:Pchar; __src:Pchar; __n:size_t; __l:__locale_t):size_t;cdecl;external clib name '__strxfrm_l';
function strdup(const S: PChar): PChar; cdecl;external clib name 'strdup';
function strndup(const S: PChar; N: size_t): PChar; cdecl; external clib name 'strndup';
function strrchr(__s:Pchar; __c:longint):Pchar;cdecl;external clib name 'strrchr';
function strchrnul(__s:Pchar; __c:longint):Pchar;cdecl;external clib name 'strchrnul';
function strcspn(__s:Pchar; __reject:Pchar):size_t;cdecl;external clib name 'strcspn';
function strspn(__s:Pchar; __accept:Pchar):size_t;cdecl;external clib name 'strspn';
function strpbrk(__s:Pchar; __accept:Pchar):Pchar;cdecl;external clib name 'strpbrk';
function strstr(__haystack:Pchar; __needle:Pchar):Pchar;cdecl;external clib name 'strstr';
function strcasestr(__haystack:Pchar; __needle:Pchar):Pchar;cdecl;external clib name 'strcasestr';
function strtok(__s:Pchar; __delim:Pchar):Pchar;cdecl;external clib name 'strtok';
function __strtok_r(__s:Pchar; __delim:Pchar; __save_ptr:PPchar):Pchar;cdecl;external clib name '__strtok_r';
function strtok_r(__s:Pchar; __delim:Pchar; __save_ptr:PPchar):Pchar;cdecl;external clib name 'strtok_r';
function memmem(__haystack:pointer; __haystacklen:size_t; __needle:pointer; __needlelen:size_t):pointer;cdecl;external clib name 'memmem';
function __mempcpy(__dest:pointer; __src:pointer; __n:size_t):pointer;cdecl;external clib name '__mempcpy';
function mempcpy(__dest:pointer; __src:pointer; __n:size_t):pointer;cdecl;external clib name 'mempcpy';
function __strlen(__s:Pchar):size_t;cdecl;external clib name 'strlen';
function strnlen(__string:Pchar; __maxlen:size_t):size_t;cdecl;external clib name 'strnlen';
function strerror(__errnum:longint):Pchar;cdecl;external clib name 'strerror';
function strerror_r(__errnum:longint; __buf:Pchar; __buflen:size_t):Pchar;cdecl;external clib name 'strerror_r';
procedure __bzero(__s:pointer; __n:size_t);cdecl;external clib name '__bzero';
procedure bcopy(__src:pointer; __dest:pointer; __n:size_t);cdecl;external clib name 'bcopy';
procedure bzero(__s:pointer; __n:size_t);cdecl;external clib name 'bzero';
function bcmp(__s1:pointer; __s2:pointer; __n:size_t):longint;cdecl;external clib name 'bcmp';
function __index(__s:Pchar; __c:longint):Pchar;cdecl;external clib name 'index';
function rindex(__s:Pchar; __c:longint):Pchar;cdecl;external clib name 'rindex';
function ffs(I: Integer): Integer; cdecl; external clib name 'ffs';
function ffsl(L: Integer): Integer; cdecl; external clib name 'ffsl';
function ffsll(LL: Int64): Integer; cdecl; external clib name 'ffsll';
function strcasecmp(__s1:Pchar; __s2:Pchar):longint;cdecl;external clib name 'strcasecmp';
function strncasecmp(__s1:Pchar; __s2:Pchar; __n:size_t):longint;cdecl;external clib name 'strncasecmp';
function __strcasecmp_l(__s1:Pchar; __s2:Pchar; __loc:__locale_t):longint;cdecl;external clib name '__strcasecmp_l';
function __strncasecmp_l(__s1:Pchar; __s2:Pchar; __n:size_t; __loc:__locale_t):longint;cdecl;external clib name '__strncasecmp_l';
function strsep(__stringp:PPchar; __delim:Pchar):Pchar;cdecl;external clib name 'strsep';
function strverscmp(__s1:Pchar; __s2:Pchar):longint;cdecl;external clib name 'strverscmp';
function strsignal(__sig:longint):Pchar;cdecl;external clib name 'strsignal';
function __stpcpy(__dest:Pchar; __src:Pchar):Pchar;cdecl;external clib name '__stpcpy';
function stpcpy(__dest:Pchar; __src:Pchar):Pchar;cdecl;external clib name 'stpcpy';
function __stpncpy(__dest:Pchar; __src:Pchar; __n:size_t):Pchar;cdecl;external clib name '__stpncpy';
function stpncpy(__dest:Pchar; __src:Pchar; __n:size_t):Pchar;cdecl;external clib name 'stpncpy';
function strfry(__string:Pchar):Pchar;cdecl;external clib name 'strfry';
function memfrob(__s:pointer; __n:size_t):pointer;cdecl;external clib name 'memfrob';
function basename(__filename:Pchar):Pchar;cdecl;external clib name 'basename';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
