

Const
     DT_UNKNOWN = 0;
     DT_FIFO = 1;
     DT_CHR = 2;
     DT_DIR = 4;
     DT_BLK = 6;
     DT_REG = 8;
     DT_LNK = 10;
     DT_SOCK = 12;
     DT_WHT = 14;

function IFTODT(mode : __mode_t) : longint;
function DTTOIF(dirtype : longint) : __mode_t;

type
  DIR = record end;
   __dirstream = DIR;
  PDIR = ^DIR;

function opendir(__name:Pchar):PDIR;cdecl;external clib name 'opendir';
function closedir(__dirp:PDIR):longint;cdecl;external clib name 'closedir';

function readdir(__dirp:PDIR):Pdirent;cdecl;external clib name 'readdir';
function readdir64(__dirp:PDIR):Pdirent64;cdecl;external clib name 'readdir64';

function readdir_r(__dirp:PDIR; __entry:Pdirent; __result:PPdirent):longint;cdecl;external clib name 'readdir_r';
function readdir64_r(__dirp:PDIR; __entry:Pdirent64; __result:PPdirent64):longint;cdecl;external clib name 'readdir64_r';

procedure rewinddir(__dirp:PDIR);cdecl;external clib name 'rewinddir';
procedure seekdir(__dirp:PDIR; __pos:longint);cdecl;external clib name 'seekdir';
function telldir(__dirp:PDIR):longint;cdecl;external clib name 'telldir';

function dirfd(__dirp:PDIR):longint;cdecl;external clib name 'dirfd';

const
   MAXNAMLEN = 255;

Type
  TSelectorFunc   = function(const p1: PDirEnt): Integer; cdecl;
  TSelectorFunc64 = function(const p1: PDirEnt64): Integer; cdecl;
  TCompareFunc    = function(const p1, p2: Pointer): Integer; cdecl;

function scandir(__dir:Pchar; __namelist:PPPdirent; __selector:TSelectorfunc; __cmp:TComparefunc):longint;cdecl;external clib name 'scandir';
function scandir64(__dir:Pchar; __namelist:PPPdirent64; __selector:TSelectorFunc64; __cmp:TCompareFunc):longint;cdecl;external clib name 'scandir64';

function getdirentries(__fd:longint; __buf:Pchar; __nbytes:size_t; __basep:P__off_t):__ssize_t;cdecl;external clib name 'getdirentries';
function getdirentries64(__fd:longint; __buf:Pchar; __nbytes:size_t; __basep:P__off64_t):__ssize_t;cdecl;external clib name 'getdirentries64';

function alphasort(const e1: Pointer; const e2: Pointer): Integer; cdecl; external clib name 'alphasort';
function alphasort64(const e1: Pointer; const e2: Pointer): Integer; cdecl; external clib name 'alphasort64';
function versionsort(const e1: Pointer; const e2: Pointer): Integer; cdecl; external clib name 'versionsort';
function versionsort64(const e1: Pointer; const e2: Pointer): Integer; cdecl; external clib name 'versionsort64';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TDirectoryStream = DIR;
  PDirectoryStream = ^TDirectoryStream;

  TSelectorProc   = TSelectorFunc;
  TSelectorProc64 = TSelectorFunc64;
  TCompareProc    = TCompareFunc;

function scandir(__dir:Pchar; var __namelist:PPdirent; __selector:TSelectorfunc; __cmp:TComparefunc):longint;cdecl;external clib name 'scandir';
function scandir64(__dir:Pchar; var __namelist:PPdirent64; __selector:TSelectorFunc64; __cmp:TCompareFunc):longint;cdecl;external clib name 'scandir64';

function getdirentries(__fd:longint; __buf:Pchar; __nbytes:size_t; var __basep:__off_t):__ssize_t;cdecl;external clib name 'getdirentries';
function getdirentries64(__fd:longint; __buf:Pchar; __nbytes:size_t; var __basep: __off64_t):__ssize_t;cdecl;external clib name 'getdirentries64';

