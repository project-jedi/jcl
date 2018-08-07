
Type
   _IO_pos_t = _G_fpos_t;
   _IO_fpos_t = _G_fpos_t;
   _IO_fpos64_t = _G_fpos64_t;
   _IO_size_t = _G_size_t;
   _IO_ssize_t = _G_ssize_t;
   _IO_off_t = _G_off_t;
   _IO_off64_t = _G_off64_t;
   _IO_pid_t = _G_pid_t;
   _IO_uid_t = _G_uid_t;
   _IO_iconv_t = _G_iconv_t;

   P_IO_pos_t = ^_IO_pos_t;
   P_IO_fpos_t = ^_IO_fpos_t;
   P_IO_fpos64_t = ^_IO_fpos64_t;
   P_IO_size_t = ^_IO_size_t;
   P_IO_ssize_t = ^_IO_ssize_t;
   P_IO_off_t = ^_IO_off_t;
   P_IO_off64_t = ^_IO_off64_t;
   P_IO_pid_t = ^_IO_pid_t;
   P_IO_uid_t = ^_IO_uid_t;
   P_IO_iconv_t = ^_IO_iconv_t;

Const
   _IO_BUFSIZ = _G_BUFSIZ;

Type
   _IO_va_list = _G_va_list;
   _IO_wint_t = _G_wint_t;


const
   _IO_UNIFIED_JUMPTABLES = 1;
   _IO_USE_DTOA = 1;

   __EOF = -(1);

const
  _IOS_INPUT = 1;
  _IOS_OUTPUT = 2;
  _IOS_ATEND = 4;
  _IOS_APPEND = 8;
  _IOS_TRUNC = 16;
  _IOS_NOCREATE = 32;
  _IOS_NOREPLACE = 64;
  _IOS_BIN = 128;
  _IO_MAGIC = $FBAD0000;
  _OLD_STDIO_MAGIC = $FABC0000;
  _IO_MAGIC_MASK = $FFFF0000;
  _IO_USER_BUF = 1;
  _IO_UNBUFFERED = 2;
  _IO_NO_READS = 4;
  _IO_NO_WRITES = 8;
  _IO_EOF_SEEN = $10;
  _IO_ERR_SEEN = $20;
  _IO_DELETE_DONT_CLOSE = $40;
  _IO_LINKED = $80;
  _IO_IN_BACKUP = $100;
  _IO_LINE_BUF = $200;
  _IO_TIED_PUT_GET = $400;
  _IO_CURRENTLY_PUTTING = $800;
  _IO_IS_APPENDING = $1000;
  _IO_IS_FILEBUF = $2000;
  _IO_BAD_SEEN = $4000;
  _IO_USER_LOCK = $8000;
  _IO_SKIPWS = 01;
  _IO_LEFT = 02;
  _IO_RIGHT = 04;
  _IO_INTERNAL = 010;
  _IO_DEC = 020;
  _IO_OCT = 040;
  _IO_HEX = 0100;
  _IO_SHOWBASE = 0200;
  _IO_SHOWPOINT = 0400;
  _IO_UPPERCASE = 01000;
  _IO_SHOWPOS = 02000;
  _IO_SCIENTIFIC = 04000;
  _IO_FIXED = 010000;
  _IO_UNITBUF = 020000;
  _IO_STDIO = 040000;
  _IO_DONT_CLOSE = 0100000;
  _IO_BOOLALPHA = 0200000;

type
  P_IO_jump_t = ^_IO_jump_t;
  _IO_jump_t = record
  end;

  P_IO_lock_t = ^_IO_lock_t;
  _IO_lock_t = pointer;
  P_IO_lock = P_IO_lock_t;
type
  __codecvt_result =  Longint;

Const
  __codecvt_ok = 0;
  __codecvt_partial = 1;
  __codecvt_error = 2;
    __codecvt_noconv = 3;

type
  P_IO_codecvt = ^_IO_codecvt;

  // The following function declarations are used anonymously in the header file.
  TCodeCvtDestrProc = procedure(p1: P_IO_codecvt); cdecl;
  TCodeCvtDoOutFunc = function(p1: P_IO_codecvt; p2: PMultiByteState; p3, p4: Pwchar_t; p5: PPwchar_t; p6, p7: PChar; p8: PPChar): __codecvt_result; cdecl;
  TCodeCvtDoUnshiftFunc = function(p1: P_IO_codecvt; p2: PMultiByteState; p3: PChar; p4: PChar; p5: PPChar): __codecvt_result; cdecl;
  TCodeCvtDoInFunc = function(p1: P_IO_codecvt; p2: PMultiByteState; p3: PChar; p4: PChar; p5: PPChar; p6: Pwchar_t; p7: Pwchar_t; p8: PPwchar_t): __codecvt_result; cdecl;
  TCodeCvtDoEncodingFunc = function(p1: P_IO_codecvt): Integer; cdecl;
  TCodeCvtDoAlwaysNoConvFunc = function(p1: P_IO_codecvt): Integer; cdecl;
  TCodeCvtDoLengthFunc = function(p1: P_IO_codecvt; p2: PMultiByteState; p3: PChar; p4: PChar; p5: _IO_size_t): Integer; cdecl;
  TCodeCvtDoMaxLengthFunc = function(p1: P_IO_codecvt): Integer; cdecl;

  _IO_codecvt = record
       __codecvt_destr : TCodeCvtDestrProc;
       __codecvt_do_out : TCodeCvtDoOutFunc;
       __codecvt_do_unshift : TCodeCvtDoUnshiftFunc;
       __codecvt_do_in : TCodeCvtDoInFunc;
       __codecvt_do_encoding : TCodeCvtDoEncodingFunc;
       __codecvt_do_always_noconv : TCodeCvtDoAlwaysNoConvFunc;
       __codecvt_do_length : TCodeCvtDoLengthFunc;
       __codecvt_do_max_length : TCodeCvtDoMaxLengthFunc;
       __cd_in : _IO_iconv_t;
       __cd_out : _IO_iconv_t;
    end;

  P_IO_wide_data = ^_IO_wide_data;
  _IO_wide_data = record
       _IO_read_ptr : Pwchar_t;
       _IO_read_end : Pwchar_t;
       _IO_read_base : Pwchar_t;
       _IO_write_base : Pwchar_t;
       _IO_write_ptr : Pwchar_t;
       _IO_write_end : Pwchar_t;
       _IO_buf_base : Pwchar_t;
       _IO_buf_end : Pwchar_t;
       _IO_save_base : Pwchar_t;
       _IO_backup_base : Pwchar_t;
       _IO_save_end : Pwchar_t;
       _IO_state : __mbstate_t;
       _IO_last_state : __mbstate_t;
       _codecvt : _IO_codecvt;
       _shortbuf : array[0..0] of wchar_t;
       _wide_vtable : P_IO_jump_t;
    end;

  P_IO_FILE = ^_IO_FILE;
  PIOMarker = ^_IO_marker;
  _IO_marker = {packed} record
    _next: PIOMarker;
    _sbuf: P_IO_File;
    _pos: Integer;
  end;

  _IO_FILE = {packed} record
    _flags: Longint;
    _IO_read_ptr: PChar;
    _IO_read_end: PChar;
    _IO_read_base: PChar;
    _IO_write_base: PChar;
    _IO_write_ptr: PChar;
    _IO_write_end: PChar;
    _IO_buf_base: PChar;
    _IO_buf_end: PChar;
    _IO_save_base: PChar;
    _IO_backup_base: PChar;
    _IO_save_end: PChar;
    _markers: PIOMarker;
    _chain: P_IO_File;
    _fileno: Integer;
    _blksize: Integer;
    _old_offset: _IO_off_t;
    _cur_column: word;
    _vtable_offset: shortint;
    _shortbuf: packed array[0..1-1] of Char;
    _lock: P_IO_Lock;
    _offset: _IO_off64_t;
    _codecvt: ^_IO_codecvt;
    _wide_data: ^_IO_wide_data;
    _mode: longint;
    _unused2: packed array[0..(15*SizeOf(Integer) - 2*SizeOf(Pointer))-1] of Byte;
  end;

   P_IO_FILE_complete = ^_IO_FILE_complete;
   _IO_FILE_complete = record
     _file : _IO_FILE;
     _offset : _IO_off64_t;
     _codecvt : P_IO_codecvt;
     _wide_data : P_IO_wide_data;
     __pad1 : pointer;
     __pad2 : pointer;
     _mode : longint;
   end;

   P_IO_FILE_plus = ^_IO_FILE_plus;
   _IO_FILE_plus = record
       {undefined structure}
     end;

var
  _IO_2_1_stdin_ : _IO_FILE_plus;cvar;external;
  _IO_2_1_stdout_ : _IO_FILE_plus;cvar;external;
  _IO_2_1_stderr_ : _IO_FILE_plus;cvar;external;

  _IO_stdin : P_IO_FILE;cvar;external;
  _IO_stdout : P_IO_FILE;cvar;external;
  _IO_stderr : P_IO_FILE;cvar;external;

type
  __io_read_fn = function(__cookie: Pointer; __buf: PChar; __nbytes: size_t): __ssize_t; cdecl;
  __io_write_fn = function(__cookie: Pointer; __buf: PChar; __n: size_t): __ssize_t; cdecl;
  __io_seek_fn = function(__cookie: Pointer; __pos: P_IO_off64_t; __w: Integer): Integer; cdecl;
  __io_close_fn = function(__cookie: Pointer): Integer; cdecl;


  Pcookie_read_function_t = ^cookie_read_function_t;
  cookie_read_function_t = __io_read_fn;

  Pcookie_write_function_t = ^cookie_write_function_t;
  cookie_write_function_t = __io_write_fn;

  Pcookie_seek_function_t = ^cookie_seek_function_t;
  cookie_seek_function_t = __io_seek_fn;

  Pcookie_close_function_t = ^cookie_close_function_t;
  cookie_close_function_t = __io_close_fn;

  P_IO_cookie_io_functions_t = ^_IO_cookie_io_functions_t;
  _IO_cookie_io_functions_t = record
    read : __io_read_fn;
    write : __io_write_fn;
    seek : __io_seek_fn;
    close : __io_close_fn;
  end;

  Pcookie_io_functions_t = ^cookie_io_functions_t;
  cookie_io_functions_t = _IO_cookie_io_functions_t;
  P_IO_cookie_file = ^_IO_cookie_file;
  _IO_cookie_file =  record
    _file: _IO_FILE;
    vtable: Pointer;
    cookie: Pointer;
    io_functions: _IO_cookie_io_functions_t;
  end;


function __underflow(_para1:P_IO_FILE):longint;cdecl;external clib name '__underflow';
function __uflow(_para1:P_IO_FILE):longint;cdecl;external clib name '__uflow';
function __overflow(_para1:P_IO_FILE; _para2:longint):longint;cdecl;external clib name '__overflow';
function __wunderflow(_para1:P_IO_FILE):_IO_wint_t;cdecl;external clib name '__wunderflow';
function __wuflow(_para1:P_IO_FILE):_IO_wint_t;cdecl;external clib name '__wuflow';
function __woverflow(_para1:P_IO_FILE; _para2:_IO_wint_t):_IO_wint_t;cdecl;external clib name '__woverflow';

function _IO_getc_unlocked(_fp: P_IO_File): Longint;
function _IO_peekc_unlocked(_fp: P_IO_File): Longint;
function _IO_putc_unlocked(_ch: Char; _fp: P_IO_File): Longint;

function _IO_getwc_unlocked(_fp: P_IO_File): Longint;
function _IO_putwc_unlocked(_wch: wchar_t; _fp: P_IO_File): Longint;

function _IO_feof_unlocked(_fp: P_IO_File): Longint;
function _IO_ferror_unlocked(_fp: P_IO_File): Longint;

function _IO_getc(__fp:P_IO_FILE):longint;cdecl;external clib name '_IO_getc';
function _IO_putc(__c:longint; __fp:P_IO_FILE):longint;cdecl;external clib name '_IO_putc';
function _IO_feof(__fp:P_IO_FILE):longint;cdecl;external clib name '_IO_feof';
function _IO_ferror(__fp:P_IO_FILE):longint;cdecl;external clib name '_IO_ferror';
function _IO_peekc_locked(__fp:P_IO_FILE):longint;cdecl;external clib name '_IO_peekc_locked';

function _IO_PENDING_OUTPUT_COUNT(_fp : P_IO_File) : longint;

procedure _IO_flockfile(_para1:P_IO_FILE);cdecl;external clib name '_IO_flockfile';
procedure _IO_funlockfile(_para1:P_IO_FILE);cdecl;external clib name '_IO_funlockfile';
function _IO_ftrylockfile(_para1:P_IO_FILE):longint;cdecl;external clib name '_IO_ftrylockfile';

function _IO_vfscanf(_para1:P_IO_FILE; _para2:Pchar; _para3:_IO_va_list; _para4:Plongint):longint;cdecl;external clib name '_IO_vfscanf';
function _IO_vfprintf(_para1:P_IO_FILE; _para2:Pchar; _para3:_IO_va_list):longint;cdecl;external clib name '_IO_vfprintf';
function _IO_padn(_para1:P_IO_FILE; _para2:longint; _para3:_IO_ssize_t):_IO_ssize_t;cdecl;external clib name '_IO_padn';
function _IO_sgetn(_para1:P_IO_FILE; _para2:pointer; _para3:_IO_size_t):_IO_size_t;cdecl;external clib name '_IO_sgetn';
function _IO_seekoff(_para1:P_IO_FILE; _para2:_IO_off64_t; _para3:longint; _para4:longint):_IO_off64_t;cdecl;external clib name '_IO_seekoff';
function _IO_seekpos(_para1:P_IO_FILE; _para2:_IO_off64_t; _para3:longint):_IO_off64_t;cdecl;external clib name '_IO_seekpos';
procedure _IO_free_backup_area(_para1:P_IO_FILE);cdecl;external clib name '_IO_free_backup_area';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

 Type
  TIOLock = _IO_lock_t;
  PIOLock = ^TIOLock;

  TIOCodeVect = _IO_codecvt;
  PIOCodeVect = ^TIOCodeVect;

  TCodeCvtDoOutProc =   TCodeCvtDoOutFunc;
  TCodeCvtDoUnshiftProc =   TCodeCvtDoUnshiftFunc;
  TCodeCvtDoInProc =   TCodeCvtDoInFunc;
  TCodeCvtDoEncodingProc =   TCodeCvtDoEncodingFunc;
  TCodeCvtDoAlwaysNoConvProc =   TCodeCvtDoAlwaysNoConvFunc;
  TCodeCvtDoLengthProc =   TCodeCvtDoLengthFunc;
  TCodeCvtDoMaxLengthProc =   TCodeCvtDoMaxLengthFunc;

  TIOFile = _IO_FILE;
  PIOFile = ^TIOFile;

  TIOCookieFunctions = _IO_cookie_io_functions_t;
  PIOCookieFunctions = ^TIOCookieFunctions;

  TIOCookieFile = _IO_cookie_file;
  PIOCookieFile = ^TIOCookieFile;

