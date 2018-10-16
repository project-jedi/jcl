
type
   Pfpos_t = ^fpos_t;
   fpos_t = _G_fpos_t;

   Pfpos64_t = ^fpos64_t;
   fpos64_t = _G_fpos64_t;

   PFILE = P_IO_FILE;

const
   _IOFBF = 0;
   _IOLBF = 1;
   _IONBF = 2;

   BUFSIZ = _IO_BUFSIZ;


const
   P_tmpdir = '/tmp';

function remove(__filename:Pchar):longint;cdecl;external clib name 'remove';
function __rename(__old:Pchar; __new:Pchar):longint;cdecl;external clib name 'rename';

function tmpfile:PFILE;cdecl;external clib name 'tmpfile';
function tmpfile64:PFILE;cdecl;external clib name 'tmpfile64';

function tmpnam(__s:Pchar):Pchar;cdecl;external clib name 'tmpnam';
function tmpnam_r(__s:Pchar):Pchar;cdecl;external clib name 'tmpnam_r';
function tempnam(dir,pfx: PChar): PChar; cdecl; external clib name 'tempnam';

function fclose(__stream:PFILE):longint;cdecl;external clib name 'fclose';
function fflush(__stream:PFILE):longint;cdecl;external clib name 'fflush';
function fflush_unlocked(__stream:PFILE):longint;cdecl;external clib name 'fflush_unlocked';
function fcloseall:longint;cdecl;external clib name 'fcloseall';

function fopen(__filename:Pchar; __modes:Pchar):PFILE;cdecl;external clib name 'fopen';
function freopen(__filename:Pchar; __modes:Pchar; __stream:PFILE):PFILE;cdecl;external clib name 'freopen';

function fopen64(__filename:Pchar; __modes:Pchar):PFILE;cdecl;external clib name 'fopen64';
function freopen64(__filename:Pchar; __modes:Pchar; __stream:PFILE):PFILE;cdecl;external clib name 'freopen64';

function fdopen(__fd:longint; __modes:Pchar):PFILE;cdecl;external clib name 'fdopen';

function fopencookie(__magic_cookie:pointer; __modes:Pchar; __io_funcs:_IO_cookie_io_functions_t):PFILE;cdecl;external clib name 'fopencookie';
function fmemopen(__s:pointer; __len:size_t; __modes:Pchar):PFILE;cdecl;external clib name 'fmemopen';
function open_memstream(__bufloc:PPchar; __sizeloc:Psize_t):PFILE;cdecl;external clib name 'open_memstream';

procedure setbuf(__stream:PFILE; __buf:Pchar);cdecl;external clib name 'setbuf';
function setvbuf(__stream:PFILE; __buf:Pchar; __modes:longint; __n:size_t):longint;cdecl;external clib name 'setvbuf';

procedure setbuffer(__stream:PFILE; __buf:Pchar; __size:size_t);cdecl;external clib name 'setbuffer';
procedure setlinebuf(__stream:PFILE);cdecl;external clib name 'setlinebuf';

function fprintf(__stream:PFILE; __format:Pchar; args:array of const):longint;cdecl;external clib name 'fprintf';
function fprintf(__stream:PFILE; __format:Pchar):longint;cdecl;varargs;external clib name 'fprintf';
function printf(__format:Pchar; args:array of const):longint;cdecl;external clib name 'printf';
function printf(__format:Pchar):longint;cdecl;varargs;external clib name 'printf';
function sprintf(__s:Pchar; __format:Pchar; args:array of const):longint;cdecl;external clib name 'sprintf';
function sprintf(__s:Pchar; __format:Pchar):longint;varargs;cdecl;external clib name 'sprintf';
function vfprintf(__s:PFILE; __format:Pchar; __arg:_G_va_list):longint;cdecl;external clib name 'vfprintf';
function vprintf(__format:Pchar; __arg:_G_va_list):longint;cdecl;external clib name 'vprintf';
function vsprintf(__s:Pchar; __format:Pchar; __arg:_G_va_list):longint;cdecl;external clib name 'vsprintf';

function fscanf(__stream:PFILE; __format:Pchar; args:array of const):longint;cdecl;external clib name 'fscanf';
function fscanf(__stream:PFILE; __format:Pchar):longint;varargs;cdecl;external clib name 'fscanf';
function scanf(__format:Pchar; args:array of const):longint;cdecl;external clib name 'scanf';
function scanf(__format:Pchar):longint;cdecl;varargs;external clib name 'scanf';
function sscanf(__s:Pchar; __format:Pchar; args:array of const):longint;cdecl;external clib name 'sscanf';
function sscanf(__s:Pchar; __format:Pchar):longint;cdecl;varargs;external clib name 'sscanf';

function fgetc(__stream:PFILE):longint;cdecl;external clib name 'fgetc';
function getc(__stream:PFILE):longint;cdecl;external clib name 'getc';
function getchar:longint;cdecl;external clib name 'getchar';


function getc_unlocked(__stream:PFILE):longint;cdecl;external clib name 'getc_unlocked';
function getchar_unlocked:longint;cdecl;external clib name 'getchar_unlocked';

function fputc(__c:longint; __stream:PFILE):longint;cdecl;external clib name 'fputc';
function putc(__c:longint; __stream:PFILE):longint;cdecl;external clib name 'putc';
function putchar(__c:longint):longint;cdecl;external clib name 'putchar';

function fputc_unlocked(__c:longint; __stream:PFILE):longint;cdecl;external clib name 'fputc_unlocked';
function putchar_unlocked(__c:longint):longint;cdecl;external clib name 'putchar_unlocked';

function getw(__stream:PFILE):longint;cdecl;external clib name 'getw';
function putw(__w:longint; __stream:PFILE):longint;cdecl;external clib name 'putw';

function fgets(__s:Pchar; __n:longint; __stream:PFILE):Pchar;cdecl;external clib name 'fgets';
function fgets_unlocked(__s:Pchar; __n:longint; __stream:PFILE):Pchar;cdecl;external clib name 'fgets_unlocked';

function gets(__s:Pchar):Pchar;cdecl;external clib name 'gets';

function __getdelim(__lineptr:PPchar; __n:Psize_t; __delimiter:longint; __stream:PFILE):_IO_ssize_t;cdecl;external clib name '__getdelim';
function getdelim(__lineptr:PPchar; __n:Psize_t; __delimiter:longint; __stream:PFILE):_IO_ssize_t;cdecl;external clib name 'getdelim';
function getline(__lineptr:PPchar; __n:Psize_t; __stream:PFILE):_IO_ssize_t;cdecl;external clib name 'getline';

function fputs(__s:Pchar; __stream:PFILE):longint;cdecl;external clib name 'fputs';
function fputs_unlocked(__s:Pchar; __stream:PFILE):longint;cdecl;external clib name 'fputs_unlocked';

function puts(__s:Pchar):longint;cdecl;external clib name 'puts';
function ungetc(__c:longint; __stream:PFILE):longint;cdecl;external clib name 'ungetc';
function fread(__ptr:pointer; __size:size_t; __n:size_t; __stream:PFILE):size_t;cdecl;external clib name 'fread';
function fwrite(__ptr:pointer; __size:size_t; __n:size_t; __s:PFILE):size_t;cdecl;external clib name 'fwrite';

function fread_unlocked(__ptr:pointer; __size:size_t; __n:size_t; __stream:PFILE):size_t;cdecl;external clib name 'fread_unlocked';
function fwrite_unlocked(__ptr:pointer; __size:size_t; __n:size_t; __stream:PFILE):size_t;cdecl;external clib name 'fwrite_unlocked';

function fseek(__stream:PFILE; __off:longint; __whence:longint):longint;cdecl;external clib name 'fseek';
function ftell(__stream:PFILE):longint;cdecl;external clib name 'ftell';
procedure rewind(__stream:PFILE);cdecl;external clib name 'rewind';

function fseeko(__stream:PFILE; __off:__off_t; __whence:longint):longint;cdecl;external clib name 'fseeko';
function ftello(__stream:PFILE):__off_t;cdecl;external clib name 'ftello';
function fgetpos(__stream:PFILE; __pos:Pfpos_t):longint;cdecl;external clib name 'fgetpos';
function fsetpos(__stream:PFILE; __pos:Pfpos_t):longint;cdecl;external clib name 'fsetpos';

function fseeko64(__stream:PFILE; __off:__off64_t; __whence:longint):longint;cdecl;external clib name 'fseeko64';
function ftello64(__stream:PFILE):__off64_t;cdecl;external clib name 'ftello64';
function fgetpos64(__stream:PFILE; __pos:Pfpos64_t):longint;cdecl;external clib name 'fgetpos64';
function fsetpos64(__stream:PFILE; __pos:Pfpos64_t):longint;cdecl;external clib name 'fsetpos64';

procedure clearerr(__stream:PFILE);cdecl;external clib name 'clearerr';
function feof(__stream:PFILE):longint;cdecl;external clib name 'feof';
function ferror(__stream:PFILE):longint;cdecl;external clib name 'ferror';

procedure clearerr_unlocked(__stream:PFILE);cdecl;external clib name 'clearerr_unlocked';
function feof_unlocked(__stream:PFILE):longint;cdecl;external clib name 'feof_unlocked';
function ferror_unlocked(__stream:PFILE):longint;cdecl;external clib name 'ferror_unlocked';

procedure perror(__s:Pchar);cdecl;external clib name 'perror';

function fileno_unlocked(__stream:PFILE):longint;cdecl;external clib name 'fileno_unlocked';

function popen(__command:PChar; __type:Pchar):PFILE;cdecl;external clib name 'popen';
function pclose(__stream:PFILE):longint;cdecl;external clib name 'pclose';
function ctermid(__s:Pchar):Pchar;cdecl;external clib name 'ctermid';
function cuserid(__s:Pchar):Pchar;cdecl;external clib name 'cuserid';

type
   Pobstack = ^obstack;
   obstack = record
       {undefined structure}
   end;

function obstack_printf(__obstack:Pobstack; __format:Pchar; args:array of const):longint;cdecl;external clib name 'obstack_printf';
function obstack_printf(__obstack:Pobstack; __format:Pchar):longint;varargs;cdecl;external clib name 'obstack_printf';
function obstack_vprintf(__obstack:Pobstack; __format:Pchar; __args:_G_va_list):longint;cdecl;external clib name 'obstack_vprintf';

procedure flockfile(__stream:PFILE);cdecl;external clib name 'flockfile';
function ftrylockfile(__stream:PFILE):longint;cdecl;external clib name 'ftrylockfile';
procedure funlockfile(__stream:PFILE);cdecl;external clib name 'funlockfile';



{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
