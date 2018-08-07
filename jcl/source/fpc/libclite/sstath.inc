
const
   S_IFMT   = __S_IFMT;
   S_IFDIR  = __S_IFDIR;
   S_IFCHR  = __S_IFCHR;
   S_IFBLK  = __S_IFBLK;
   S_IFREG  = __S_IFREG;
   S_IFIFO  = __S_IFIFO;
   S_IFLNK  = __S_IFLNK;
   S_IFSOCK = __S_IFSOCK;


function __S_ISTYPE(mode,mask : __mode_t) : boolean;
function S_ISDIR(mode : __mode_t) : boolean;
function S_ISCHR(mode : __mode_t) : boolean;
function S_ISBLK(mode : __mode_t) : boolean;
function S_ISREG(mode : __mode_t) : boolean;
function S_ISFIFO(mode : __mode_t) : boolean;
function S_ISLNK(mode : __mode_t) : boolean;
function S_ISSOCK(mode : __mode_t) : boolean;

const
  S_ISUID = __S_ISUID;
  S_ISGID = __S_ISGID;
  S_ISVTX = __S_ISVTX;

  S_IRUSR = __S_IREAD;
  S_IWUSR = __S_IWRITE;
  S_IXUSR = __S_IEXEC;
  S_IRWXU = (__S_IREAD or __S_IWRITE) or __S_IEXEC;

  S_IREAD = S_IRUSR;
  S_IWRITE = S_IWUSR;
  S_IEXEC = S_IXUSR;

  S_IRGRP = S_IRUSR shr 3;
  S_IWGRP = S_IWUSR shr 3;
  S_IXGRP = S_IXUSR shr 3;
  S_IRWXG = S_IRWXU shr 3;
  S_IROTH = S_IRGRP shr 3;
  S_IWOTH = S_IWGRP shr 3;
  S_IXOTH = S_IXGRP shr 3;
  S_IRWXO = S_IRWXG shr 3;

const
  ACCESSPERMS = (S_IRWXU or S_IRWXG) or S_IRWXO;
  ALLPERMS = ((((S_ISUID or S_ISGID) or S_ISVTX) or S_IRWXU) or S_IRWXG) or S_IRWXO;
  DEFFILEMODE = ((((S_IRUSR or S_IWUSR) or S_IRGRP) or S_IWGRP) or S_IROTH) or S_IWOTH;
  S_BLKSIZE = 512;

function stat(__file:Pchar; __buf:Pstat):longint;
function fstat(__fd:longint; __buf:Pstat):longint;

function stat64(__file:Pchar; __buf:Pstat64):longint;
function fstat64(__fd:longint; __buf:Pstat64):longint;

function lstat(__file:Pchar; __buf:Pstat):longint;
function lstat64(__file:Pchar; __buf:Pstat64):longint;

function chmod(__file:Pchar; __mode:__mode_t):longint;cdecl;external clib name 'chmod';
function fchmod(__fd:longint; __mode:__mode_t):longint;cdecl;external clib name 'fchmod';
function umask(__mask:__mode_t):__mode_t;cdecl;external clib name 'umask';
function getumask:__mode_t;cdecl;external clib name 'getumask';

function __mkdir(__path:Pchar; __mode:__mode_t):longint;cdecl;external clib name 'mkdir';
function mknod(__path:Pchar; __mode:__mode_t; __dev:__dev_t):longint;cdecl;external clib name 'mknod';
function mkfifo(__path:Pchar; __mode:__mode_t):longint;cdecl;external clib name 'mkfifo';

function __fxstat(__ver:longint; __fildes:longint; __stat_buf:Pstat):longint;cdecl;external clib name '__fxstat';
function __xstat(__ver:longint; __filename:Pchar; __stat_buf:Pstat):longint;cdecl;external clib name '__xstat';
function __lxstat(__ver:longint; __filename:Pchar; __stat_buf:Pstat):longint;cdecl;external clib name '__lxstat';

function __fxstat64(__ver:longint; __fildes:longint; __stat_buf:Pstat64):longint;cdecl;external clib name '__fxstat64';
function __xstat64(__ver:longint; __filename:Pchar; __stat_buf:Pstat64):longint;cdecl;external clib name '__xstat64';
function __lxstat64(__ver:longint; __filename:Pchar; __stat_buf:Pstat64):longint;cdecl;external clib name '__lxstat64';

function __xmknod(__ver:longint; __path:Pchar; __mode:__mode_t; __dev:P__dev_t):longint;cdecl;external clib name '__xmknod';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function stat(__file:Pchar; var __buf:_stat):longint;
function fstat(__fd:longint; var __buf:_stat):longint;

function stat64(__file:Pchar; var __buf: _stat64):longint;
function fstat64(__fd:longint; var __buf: _stat64):longint;

function lstat(__file:Pchar; var __buf:_stat):longint;
function lstat64(__file:Pchar; var __buf:_stat64):longint;

function __fxstat(__ver:longint; __fildes:longint; var __stat_buf: _stat):longint;cdecl;external clib name '__fxstat';
function __xstat(__ver:longint; __filename:Pchar; var __stat_buf: _stat):longint;cdecl;external clib name '__xstat';
function __lxstat(__ver:longint; __filename:Pchar; var __stat_buf: _stat):longint;cdecl;external clib name '__lxstat';

function __fxstat64(__ver:longint; __fildes:longint; var __stat_buf: _stat64):longint;cdecl;external clib name '__fxstat64';
function __xstat64(__ver:longint; __filename:Pchar; var __stat_buf: _stat64):longint;cdecl;external clib name '__xstat64';
function __lxstat64(__ver:longint; __filename:Pchar; var __stat_buf: _stat64):longint;cdecl;external clib name '__lxstat64';

function __xmknod(__ver:longint; __path:Pchar; __mode:__mode_t; var __dev: __dev_t):longint;cdecl;external clib name '__xmknod';
