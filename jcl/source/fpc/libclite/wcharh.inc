
type
  Pmbstate_t = ^mbstate_t;
  P__FILE = PFILE;
  __GNUC_VA_LIST = pointer;

function wcscpy(__dest:Pwchar_t; __src:Pwchar_t):Pwchar_t;cdecl;external clib name 'wcscpy';
function wcsncpy(__dest:Pwchar_t; __src:Pwchar_t; __n:size_t):Pwchar_t;cdecl;external clib name 'wcsncpy';
function wcscat(__dest:Pwchar_t; __src:Pwchar_t):Pwchar_t;cdecl;external clib name 'wcscat';
function wcsncat(__dest:Pwchar_t; __src:Pwchar_t; __n:size_t):Pwchar_t;cdecl;external clib name 'wcsncat';
function wcscmp(__s1:Pwchar_t; __s2:Pwchar_t):longint;cdecl;external clib name 'wcscmp';
function wcsncmp(__s1:Pwchar_t; __s2:Pwchar_t; __n:size_t):longint;cdecl;external clib name 'wcsncmp';
function wcscasecmp(__s1:Pwchar_t; __s2:Pwchar_t):longint;cdecl;external clib name 'wcscasecmp';
function wcsncasecmp(__s1:Pwchar_t; __s2:Pwchar_t; __n:size_t):longint;cdecl;external clib name 'wcsncasecmp';
function __wcscasecmp_l(__s1:Pwchar_t; __s2:Pwchar_t; __loc:__locale_t):longint;cdecl;external clib name '__wcscasecmp_l';
function __wcsncasecmp_l(__s1:Pwchar_t; __s2:Pwchar_t; __n:size_t; __loc:__locale_t):longint;cdecl;external clib name '__wcsncasecmp_l';
function wcscoll(__s1:Pwchar_t; __s2:Pwchar_t):longint;cdecl;external clib name 'wcscoll';
function wcsxfrm(__s1:Pwchar_t; __s2:Pwchar_t; __n:size_t):size_t;cdecl;external clib name 'wcsxfrm';
function __wcscoll_l(__s1:Pwchar_t; __s2:Pwchar_t; __loc:__locale_t):longint;cdecl;external clib name '__wcscoll_l';
function __wcsxfrm_l(__s1:Pwchar_t; __s2:Pwchar_t; __n:size_t; __loc:__locale_t):size_t;cdecl;external clib name '__wcsxfrm_l';
function wcsdup(__s:Pwchar_t):Pwchar_t;cdecl;external clib name 'wcsdup';
function wcschr(__wcs:Pwchar_t; __wc:wchar_t):Pwchar_t;cdecl;external clib name 'wcschr';
function wcsrchr(__wcs:Pwchar_t; __wc:wchar_t):Pwchar_t;cdecl;external clib name 'wcsrchr';
function wcschrnul(__s:Pwchar_t; __wc:wchar_t):Pwchar_t;cdecl;external clib name 'wcschrnul';
function wcscspn(__wcs:Pwchar_t; __reject:Pwchar_t):size_t;cdecl;external clib name 'wcscspn';
function wcsspn(__wcs:Pwchar_t; __accept:Pwchar_t):size_t;cdecl;external clib name 'wcsspn';
function wcspbrk(__wcs:Pwchar_t; __accept:Pwchar_t):Pwchar_t;cdecl;external clib name 'wcspbrk';
function wcsstr(__haystack:Pwchar_t; __needle:Pwchar_t):Pwchar_t;cdecl;external clib name 'wcsstr';
function wcswcs(__haystack:Pwchar_t; __needle:Pwchar_t):Pwchar_t;cdecl;external clib name 'wcswcs';
function wcstok(__s:Pwchar_t; __delim:Pwchar_t; __ptr:PPwchar_t):Pwchar_t;cdecl;external clib name 'wcstok';
function wcslen(__s:Pwchar_t):size_t;cdecl;external clib name 'wcslen';
function wcsnlen(__s:Pwchar_t; __maxlen:size_t):size_t;cdecl;external clib name 'wcsnlen';
function wmemchr(__s:Pwchar_t; __c:wchar_t; __n:size_t):Pwchar_t;cdecl;external clib name 'wmemchr';
function wmemcmp(__s1:Pwchar_t; __s2:Pwchar_t; __n:size_t):longint;cdecl;external clib name 'wmemcmp';
function wmemcpy(__s1:Pwchar_t; __s2:Pwchar_t; __n:size_t):Pwchar_t;cdecl;external clib name 'wmemcpy';
function wmemmove(__s1:Pwchar_t; __s2:Pwchar_t; __n:size_t):Pwchar_t;cdecl;external clib name 'wmemmove';
function wmemset(__s:Pwchar_t; __c:wchar_t; __n:size_t):Pwchar_t;cdecl;external clib name 'wmemset';
function wmempcpy(__s1:Pwchar_t; __s2:Pwchar_t; __n:size_t):Pwchar_t;cdecl;external clib name 'wmempcpy';
function btowc(__c:longint):wint_t;cdecl;external clib name 'btowc';
function wctob(__c:wint_t):longint;cdecl;external clib name 'wctob';
function mbsinit(__ps:Pmbstate_t):longint;cdecl;external clib name 'mbsinit';
function mbrtowc(__pwc:Pwchar_t; __s:Pchar; __n:size_t; __p:Pmbstate_t):size_t;cdecl;external clib name 'mbrtowc';
function wcrtomb(__s:Pchar; __wc:wchar_t; __ps:Pmbstate_t):size_t;cdecl;external clib name 'wcrtomb';
function __mbrlen(__s:Pchar; __n:size_t; __ps:Pmbstate_t):size_t;cdecl;external clib name '__mbrlen';
function mbrlen(__s:Pchar; __n:size_t; __ps:Pmbstate_t):size_t;cdecl;external clib name 'mbrlen';
function mbsrtowcs(__dst:Pwchar_t; __src:PPchar; __len:size_t; __ps:Pmbstate_t):size_t;cdecl;external clib name 'mbsrtowcs';
function wcsrtombs(__dst:Pchar; __src:PPwchar_t; __len:size_t; __ps:Pmbstate_t):size_t;cdecl;external clib name 'wcsrtombs';
function mbsnrtowcs(__dst:Pwchar_t; __src:PPchar; __nmc:size_t; __len:size_t; __ps:Pmbstate_t):size_t;cdecl;external clib name 'mbsnrtowcs';
function wcsnrtombs(__dst:Pchar; __src:PPwchar_t; __nwc:size_t; __len:size_t; __ps:Pmbstate_t):size_t;cdecl;external clib name 'wcsnrtombs';
function wcwidth(__c:wchar_t):longint;cdecl;external clib name 'wcwidth';
function wcswidth(__s:Pwchar_t; __n:size_t):longint;cdecl;external clib name 'wcswidth';
function wcstod(__nptr:Pwchar_t; __endptr:PPwchar_t):double;cdecl;external clib name 'wcstod';
function wcstof(__nptr:Pwchar_t; __endptr:PPwchar_t):double;cdecl;external clib name 'wcstof';
function wcstold(__nptr:Pwchar_t; __endptr:PPwchar_t):extended;cdecl;external clib name 'wcstold';
function wcstol(__nptr:Pwchar_t; __endptr:PPwchar_t; __base:longint):longint;cdecl;external clib name 'wcstol';
function wcstoul(__nptr:Pwchar_t; __endptr:PPwchar_t; __base:longint):dword;cdecl;external clib name 'wcstoul';
function wcstoq(__nptr:Pwchar_t; __endptr:PPwchar_t; __base:longint):int64;cdecl;external clib name 'wcstoq';
function wcstouq(__nptr:Pwchar_t; __endptr:PPwchar_t; __base:longint):qword;cdecl;external clib name 'wcstouq';
function wcstoll(__nptr:Pwchar_t; __endptr:PPwchar_t; __base:longint):int64;cdecl;external clib name 'wcstoll';
function wcstoull(__nptr:Pwchar_t; __endptr:PPwchar_t; __base:longint):qword;cdecl;external clib name 'wcstoull';

function __wcstol_l(__nptr:Pwchar_t; __endptr:PPwchar_t; __base:longint; __loc:__locale_t):longint;cdecl;external clib name '__wcstol_l';
function __wcstoul_l(__nptr:Pwchar_t; __endptr:PPwchar_t; __base:longint; __loc:__locale_t):dword;cdecl;external clib name '__wcstoul_l';
function __wcstoll_l(__nptr:Pwchar_t; __endptr:PPwchar_t; __base:longint; __loc:__locale_t):int64;cdecl;external clib name '__wcstoll_l';
function __wcstoull_l(__nptr:Pwchar_t; __endptr:PPwchar_t; __base:longint; __loc:__locale_t):qword;cdecl;external clib name '__wcstoull_l';
function __wcstod_l(__nptr:Pwchar_t; __endptr:PPwchar_t; __loc:__locale_t):double;cdecl;external clib name '__wcstod_l';
function __wcstof_l(__nptr:Pwchar_t; __endptr:PPwchar_t; __loc:__locale_t):double;cdecl;external clib name '__wcstof_l';
function __wcstold_l(__nptr:Pwchar_t; __endptr:PPwchar_t; __loc:__locale_t):extended;cdecl;external clib name '__wcstold_l';

function __wcstod_internal(__nptr:Pwchar_t; __endptr:PPwchar_t; __group:longint):double;cdecl;external clib name '__wcstod_internal';
function __wcstof_internal(__nptr:Pwchar_t; __endptr:PPwchar_t; __group:longint):double;cdecl;external clib name '__wcstof_internal';
function __wcstold_internal(__nptr:Pwchar_t; __endptr:PPwchar_t; __group:longint):extended;cdecl;external clib name '__wcstold_internal';
function __wcstol_internal(__nptr:Pwchar_t; __endptr:PPwchar_t; __base:longint; __group:longint):longint;cdecl;external clib name '__wcstol_internal';
function __wcstoul_internal(__npt:Pwchar_t; __endptr:PPwchar_t; __base:longint; __group:longint):dword;cdecl;external clib name '__wcstoul_internal';
function __wcstoll_internal(__nptr:Pwchar_t; __endptr:PPwchar_t; __base:longint; __group:longint):int64;cdecl;external clib name '__wcstoll_internal';
function __wcstoull_internal(__nptr:Pwchar_t; __endptr:PPwchar_t; __base:longint; __group:longint):qword;cdecl;external clib name '__wcstoull_internal';

function wcpcpy(__dest:Pwchar_t; __src:Pwchar_t):Pwchar_t;cdecl;external clib name 'wcpcpy';
function wcpncpy(__dest:Pwchar_t; __src:Pwchar_t; __n:size_t):Pwchar_t;cdecl;external clib name 'wcpncpy';
function fwide(__fp:P__FILE; __mode:longint):longint;cdecl;external clib name 'fwide';
function fwprintf(__stream:P__FILE; __format:Pwchar_t; args:array of const):longint;cdecl;external clib name 'fwprintf';
function fwprintf(__stream:P__FILE; __format:Pwchar_t):longint;cdecl;varargs;external clib name 'fwprintf';
function wprintf(__format:Pwchar_t; args:array of const):longint;cdecl;external clib name 'wprintf';
function wprintf(__format:Pwchar_t):longint;cdecl;varargs;external clib name 'wprintf';
function swprintf(__s:Pwchar_t; __n:size_t; __format:Pwchar_t; args:array of const):longint;cdecl;external clib name 'swprintf';
function swprintf(__s:Pwchar_t; __n:size_t; __format:Pwchar_t):longint;cdecl;varargs;external clib name 'swprintf';
function vfwprintf(__s:P__FILE; __format:Pwchar_t; __arg:__gnuc_va_list):longint;cdecl;external clib name 'vfwprintf';
function vwprintf(__format:Pwchar_t; __arg:__gnuc_va_list):longint;cdecl;external clib name 'vwprintf';
function vswprintf(__s:Pwchar_t; __n:size_t; __format:Pwchar_t; __arg:__gnuc_va_list):longint;cdecl;external clib name 'vswprintf';
function fwscanf(__stream:P__FILE; __format:Pwchar_t; args:array of const):longint;cdecl;external clib name 'fwscanf';
function fwscanf(__stream:P__FILE; __format:Pwchar_t):longint;cdecl;varargs;external clib name 'fwscanf';
function wscanf(__format:Pwchar_t; args:array of const):longint;cdecl;external clib name 'wscanf';
function wscanf(__format:Pwchar_t):longint;cdecl;varargs;external clib name 'wscanf';
function swscanf(__s:Pwchar_t; __format:Pwchar_t; args:array of const):longint;cdecl;external clib name 'swscanf';
function swscanf(__s:Pwchar_t; __format:Pwchar_t):longint;cdecl;varargs;external clib name 'swscanf';
function vfwscanf(__s:P__FILE; __format:Pwchar_t; __arg:__gnuc_va_list):longint;cdecl;external clib name 'vfwscanf';
function vwscanf(__format:Pwchar_t; __arg:__gnuc_va_list):longint;cdecl;external clib name 'vwscanf';
function vswscanf(__s:Pwchar_t; __format:Pwchar_t; __arg:__gnuc_va_list):longint;cdecl;external clib name 'vswscanf';
function fgetwc(__stream:P__FILE):wint_t;cdecl;external clib name 'fgetwc';
function getwc(__stream:P__FILE):wint_t;cdecl;external clib name 'getwc';
function getwchar:wint_t;cdecl;external clib name 'getwchar';
function fputwc(__wc:wchar_t; __stream:P__FILE):wint_t;cdecl;external clib name 'fputwc';
function putwc(__wc:wchar_t; __stream:P__FILE):wint_t;cdecl;external clib name 'putwc';
function putwchar(__wc:wchar_t):wint_t;cdecl;external clib name 'putwchar';
function fgetws(__ws:Pwchar_t; __n:longint; __stream:P__FILE):Pwchar_t;cdecl;external clib name 'fgetws';
function fputws(__ws:Pwchar_t; __stream:P__FILE):longint;cdecl;external clib name 'fputws';
function ungetwc(__wc:wint_t; __stream:P__FILE):wint_t;cdecl;external clib name 'ungetwc';
function getwc_unlocked(__stream:P__FILE):wint_t;cdecl;external clib name 'getwc_unlocked';
function getwchar_unlocked:wint_t;cdecl;external clib name 'getwchar_unlocked';
function fgetwc_unlocked(__stream:P__FILE):wint_t;cdecl;external clib name 'fgetwc_unlocked';
function fputwc_unlocked(__wc:wchar_t; __stream:P__FILE):wint_t;cdecl;external clib name 'fputwc_unlocked';
function putwc_unlocked(__wc:wchar_t; __stream:P__FILE):wint_t;cdecl;external clib name 'putwc_unlocked';
function putwchar_unlocked(__wc:wchar_t):wint_t;cdecl;external clib name 'putwchar_unlocked';
function fgetws_unlocked(__ws:Pwchar_t; __n:longint; __stream:P__FILE):Pwchar_t;cdecl;external clib name 'fgetws_unlocked';
function fputws_unlocked(__ws:Pwchar_t; __stream:P__FILE):longint;cdecl;external clib name 'fputws_unlocked';
function wcsftime(__s:Pwchar_t; __maxsize:size_t; __format:Pwchar_t; __tp:Ptm):size_t;cdecl;external clib name 'wcsftime';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
