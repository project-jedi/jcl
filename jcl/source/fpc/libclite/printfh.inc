
type
   Pprintf_info = ^printf_info;
   printf_info = record
        prec : longint;
        width : longint;
        spec : wchar_t;
        flag0 : dword;
        pad : wchar_t;
     end;

  printf_function = function(__stream: PIOFile; const __info: printf_info; __args: PPointer): Integer; cdecl;
  printf_arginfo_function = function(const __info: printf_info; __n: size_t; var __argtypes: Integer): Integer; cdecl;

function register_printf_function(__spec:longint; __func:printf_function; __arginfo:printf_arginfo_function):longint;cdecl;external clib name 'register_printf_function';
function parse_printf_format(__fmt:Pchar; __n:size_t; __argtypes:Plongint):size_t;cdecl;external clib name 'parse_printf_format';

Const
  PA_INT = 0;
  PA_CHAR = 1;
  PA_WCHAR = 2;
  PA_STRING = 3;
  PA_WSTRING = 4;
  PA_POINTER = 5;
  PA_FLOAT = 6;
  PA_DOUBLE = 7;
  PA_LAST = 8;

  PA_FLAG_MASK = $ff00;
  PA_FLAG_LONG_LONG = 1 shl 8;
  PA_FLAG_LONG_DOUBLE = PA_FLAG_LONG_LONG;
  PA_FLAG_LONG = 1 shl 9;
  PA_FLAG_SHORT = 1 shl 10;
  PA_FLAG_PTR = 1 shl 11;

function printf_size(__fp:PFILE; __info:Pprintf_info; __args:Ppointer):longint;cdecl;external clib name 'printf_size';
function printf_size_info(__info:Pprintf_info; __n:size_t; __argtypes:Plongint):longint;cdecl;external clib name 'printf_size_info';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TPrintfInfo = printf_info;
  PPrintfInfo = ^TPrintfInfo;
