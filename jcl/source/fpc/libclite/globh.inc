Const
  GLOB_ERR = 1 shl 0;
  GLOB_MARK = 1 shl 1;
  GLOB_NOSORT = 1 shl 2;
  GLOB_DOOFFS = 1 shl 3;
  GLOB_NOCHECK = 1 shl 4;
  GLOB_APPEND = 1 shl 5;
  GLOB_NOESCAPE = 1 shl 6;
  GLOB_PERIOD = 1 shl 7;
  GLOB_MAGCHAR = 1 shl 8;
  GLOB_ALTDIRFUNC = 1 shl 9;
  GLOB_BRACE = 1 shl 10;
  GLOB_NOMAGIC = 1 shl 11;
  GLOB_TILDE = 1 shl 12;
  GLOB_ONLYDIR = 1 shl 13;
  GLOB_TILDE_CHECK = 1 shl 14;
  __GLOB_FLAGS = ((((((((((((GLOB_ERR or GLOB_MARK) or GLOB_NOSORT) or GLOB_DOOFFS) or GLOB_NOESCAPE) or GLOB_NOCHECK) or GLOB_APPEND) or GLOB_PERIOD) or GLOB_ALTDIRFUNC) or GLOB_BRACE) or GLOB_NOMAGIC) or GLOB_TILDE) or GLOB_ONLYDIR) or GLOB_TILDE_CHECK;
  GLOB_NOSPACE = 1;
  GLOB_ABORTED = 2;
  GLOB_NOMATCH = 3;
  GLOB_NOSYS = 4;

  GLOB_ABEND = GLOB_ABORTED;

Type
  TGlobCloseDirProc = procedure(Param: Pointer); cdecl;
  TGlobReadDirFunc = function(Param: Pointer): PDirEnt; cdecl;
  TGlobOpenDirFunc = function(Param: PChar): __ptr_t; cdecl;
  TGlobStatFunc = function(Param1: PChar; Param2: PStatBuf): longint; cdecl;

   Pglob_t = ^glob_t;
   glob_t = record
     gl_pathc : size_t;
     gl_pathv : ^Pchar;
     gl_offs : size_t;
     gl_flags : longint;
     gl_closedir : TGlobClosedirProc;
     gl_readdir : TGlobReaddirFunc;
     gl_opendir : TGlobOpendirFunc;
     gl_lstat : TGlobStatFunc;
     gl_stat : TGlobStatFunc;
   end;

  TGlobReadDir64Func = function(Param: Pointer): PDirEnt64; cdecl;
  TGlobStat64Func = function(Param1: PChar; Param2: PStatBuf64): longint; cdecl;

  Pglob64_t = ^glob64_t;
  glob64_t = record
    gl_pathc: size_t;
    gl_pathv: PPChar;
    gl_offs: size_t;
    gl_flags: longint;
    gl_closedir: TGlobCloseDirProc;
    gl_readdir: TGlobReaddir64Func;
    gl_opendir: TGlobOpenDirFunc;
    gl_lstat: TGlobStat64Func;
    gl_stat: TGlobStat64Func;
  end;

  TGlobErrFunc = function(PathName: PChar; ErrNo: longint): longint; cdecl;

function glob(__pattern:Pchar; __flags:longint; __errfunc:TGlobErrFunc; __pglob:Pglob_t):longint;cdecl;external clib name 'glob';
procedure globfree(__pglob:Pglob_t);cdecl;external clib name 'globfree';

function glob64(__pattern: PChar; __flags: longint; __errfunc: TGlobErrFunc; __pglob: Pglob64_t): longint; cdecl;external clib name 'glob64';
procedure globfree64(__pglob: Pglob64_t); cdecl;external clib name 'globfree64';

function glob_pattern_p(__pattern: PChar; __quote: longint): longint; cdecl;external clib name 'glob_pattern_p';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TGlobData = glob_t;
  PGlobData = ^TGlobData;

  TGlob64Data = glob_t;
  PGlob64Data = ^TGlob64Data;

  TGlobReadDirProc = TGlobReadDirFunc;
  TGlobOpenDirProc = TGlobOpenDirFunc;
  TGlobStatProc = TGlobStatFunc;

  TGlobReadDir64Proc = TGlobReadDir64Func;
  TGlobStat64Proc = TGlobStat64Func;
