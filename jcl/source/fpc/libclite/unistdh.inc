

const
   _POSIX_VERSION = 199506;
   _POSIX2_C_VERSION = 199209;
   _POSIX2_VERSION = 199209;
   _POSIX2_C_BIND = 1;
   _POSIX2_C_DEV = 1;
   _POSIX2_SW_DEV = 1;
   _POSIX2_LOCALEDEF = 1;

const
   _XOPEN_VERSION = 500;

const
   _XOPEN_XCU_VERSION = 4;
   _XOPEN_XPG2 = 1;
   _XOPEN_XPG3 = 1;
   _XOPEN_XPG4 = 1;
   _XOPEN_UNIX = 1;
   _XOPEN_CRYPT = 1;
   _XOPEN_ENH_I18N = 1;
   _XOPEN_LEGACY = 1;

const
   STDIN_FILENO = 0;
   STDOUT_FILENO = 1;
   STDERR_FILENO = 2;

type

   Pintptr_t = ^intptr_t;


function access(__name:Pchar; __type:longint):longint;cdecl;external clib name 'access';

function euidaccess(__name:Pchar; __type:longint):longint;cdecl;external clib name 'euidaccess';

function lseek(__fd:longint; __offset:__off_t; __whence:longint):__off_t;cdecl;external clib name 'lseek';
function lseek64(__fd:longint; __offset:__off64_t; __whence:longint):__off64_t;cdecl;external clib name 'lseek64';

{ // Removed, conflicts with system routines
function close(__fd:longint):longint;cdecl;external clib name 'close';
function read(__fd:longint; __buf:pointer; __nbytes:size_t):ssize_t;cdecl;external clib name 'read';
function write(__fd:longint; __buf:pointer; __n:size_t):ssize_t;cdecl;external clib name 'write';
}

function pread(__fd:longint; __buf:pointer; __nbytes:size_t; __offset:__off_t):ssize_t;cdecl;external clib name 'pread';
function pwrite(__fd:longint; __buf:pointer; __n:size_t; __offset:__off_t):ssize_t;cdecl;external clib name 'pwrite';

function pread64(__fd:longint; __buf:pointer; __nbytes:size_t; __offset:__off64_t):ssize_t;cdecl;external clib name 'pread64';
function pwrite64(__fd:longint; __buf:pointer; __n:size_t; __offset:__off64_t):ssize_t;cdecl;external clib name 'pwrite64';

Type
  TPipes = Array[0..1] of longint;
  PPipes = ^TPipes;

function pipe(var __pipedes: TPipes):longint;cdecl;external clib name 'pipe';
function pipe(__pipedes: Plongint):longint;cdecl;external clib name 'pipe';
function alarm(__seconds:dword):dword;cdecl;external clib name 'alarm';
function __sleep(__seconds:dword):dword;cdecl;external clib name 'sleep';

function ualarm(__value:__useconds_t; __interval:__useconds_t):__useconds_t;cdecl;external clib name 'ualarm';
function usleep(__useconds:__useconds_t):longint;cdecl;external clib name 'usleep';

function pause:longint;cdecl;external clib name 'pause';
function chown(__file:Pchar; __owner:__uid_t; __group:__gid_t):longint;cdecl;external clib name 'chown';

function fchown(__fd:longint; __owner:__uid_t; __group:__gid_t):longint;cdecl;external clib name 'fchown';
function lchown(__file:Pchar; __owner:__uid_t; __group:__gid_t):longint;cdecl;external clib name 'lchown';

function __chdir(__path:Pchar):longint;cdecl;external clib name 'chdir';
function fchdir(__fd:longint):longint;cdecl;external clib name 'fchdir';

function getcwd(__buf:Pchar; __size:size_t):Pchar;cdecl;external clib name 'getcwd';
function get_current_dir_name:Pchar;cdecl;external clib name 'get_current_dir_name';
function getwd(__buf:Pchar):Pchar;cdecl;external clib name 'getwd';

function dup(__fd:longint):longint;cdecl;external clib name 'dup';
function dup2(__fd:longint; __fd2:longint):longint;cdecl;external clib name 'dup2';

  var
     __environ : ^Pchar;cvar;external;
     environ : ^Pchar;cvar;external;

function execve(__path:Pchar; __argv:PPchar; __envp:PPchar):longint;cdecl;external clib name 'execve';
function fexecve(__fd:longint; __argv:PPchar; __envp:PPchar):longint;cdecl;external clib name 'fexecve';

function execv(__path:Pchar; __argv:PPchar):longint;cdecl;external clib name 'execv';
function execle(__path:Pchar; __arg:Pchar; args:array of const):longint;cdecl;varargs;external clib name 'execle';
function execle(__path:Pchar; __arg:Pchar):longint;cdecl;external clib name 'execle';
function execl(__path:Pchar; __arg:Pchar; args:array of const):longint;cdecl;external clib name 'execl';
function execl(__path:Pchar; __arg:Pchar):longint;cdecl;varargs;external clib name 'execl';
function execvp(__file:Pchar; __argv:PPchar):longint;cdecl;external clib name 'execvp';
function execlp(__file:Pchar; __arg:Pchar; args:array of const):longint;cdecl;external clib name 'execlp';
function execlp(__file:Pchar; __arg:Pchar):longint;cdecl;varargs;external clib name 'execlp';

function nice(__inc:longint):longint;cdecl;external clib name 'nice';
procedure _exit (__status : longint); cdecl; external clib name '_exit';

function pathconf(__path:Pchar; __name:longint):longint;cdecl;external clib name 'pathconf';
function fpathconf(__fd:longint; __name:longint):longint;cdecl;external clib name 'fpathconf';

function sysconf (__name : longint) : longint; cdecl; external clib name 'sysconf';
function confstr(__name:longint; __buf:Pchar; __len:size_t):size_t;cdecl;external clib name 'confstr';

function getpid:__pid_t;cdecl;external clib name 'getpid';
function getppid:__pid_t;cdecl;external clib name 'getppid';
function getpgrp:__pid_t;cdecl;external clib name 'getpgrp';

function __getpgid(__pid:__pid_t):__pid_t;cdecl;external clib name '__getpgid';

function getpgid(__pid:__pid_t):__pid_t;cdecl;external clib name 'getpgid';

function setpgid(__pid:__pid_t; __pgid:__pid_t):longint;cdecl;external clib name 'setpgid';
function setpgrp:longint;cdecl;external clib name 'setpgrp';

function setsid:__pid_t;cdecl;external clib name 'setsid';
function getsid(__pid:__pid_t):__pid_t;cdecl;external clib name 'getsid';
function getuid:__uid_t;cdecl;external clib name 'getuid';
function geteuid:__uid_t;cdecl;external clib name 'geteuid';
function getgid:__gid_t;cdecl;external clib name 'getgid';
function getegid:__gid_t;cdecl;external clib name 'getegid';
function getgroups(__size:longint; __list:array of __gid_t):longint;cdecl;external clib name 'getgroups';
function group_member(__gid:__gid_t):longint;cdecl;external clib name 'group_member';
function setuid(__uid:__uid_t):longint;cdecl;external clib name 'setuid';
function setreuid(__ruid:__uid_t; __euid:__uid_t):longint;cdecl;external clib name 'setreuid';
function seteuid(__uid:__uid_t):longint;cdecl;external clib name 'seteuid';
function setgid(__gid:__gid_t):longint;cdecl;external clib name 'setgid';

function setregid(__rgid:__gid_t; __egid:__gid_t):longint;cdecl;external clib name 'setregid';
function setegid(__gid:__gid_t):longint;cdecl;external clib name 'setegid';
function fork:__pid_t;cdecl;external clib name 'fork';
function vfork:__pid_t;cdecl;external clib name 'vfork';
function ttyname(__fd:longint):Pchar;cdecl;external clib name 'ttyname';
function ttyname_r(__fd:longint; __buf:Pchar; __buflen:size_t):longint;cdecl;external clib name 'ttyname_r';
function isatty(__fd:longint):longint;cdecl;external clib name 'isatty';
function link(__from:Pchar; __to:Pchar):longint;cdecl;external clib name 'link';
function symlink(__from:Pchar; __to:Pchar):longint;cdecl;external clib name 'symlink';
function readlink(__path:Pchar; __buf:Pchar; __len:size_t):longint;cdecl;external clib name 'readlink';
function unlink(__name:Pchar):longint;cdecl;external clib name 'unlink';
function __rmdir(__path:Pchar):longint;cdecl;external clib name 'rmdir';
function tcgetpgrp(__fd:longint):__pid_t;cdecl;external clib name 'tcgetpgrp';
function tcsetpgrp(__fd:longint; __pgrp_id:__pid_t):longint;cdecl;external clib name 'tcsetpgrp';
function getlogin:Pchar;cdecl;external clib name 'getlogin';
function getlogin_r(__name:Pchar; __name_len:size_t):longint;cdecl;external clib name 'getlogin_r';
function setlogin(__name:Pchar):longint;cdecl;external clib name 'setlogin';
function gethostname(__name:Pchar; __len:size_t):longint;cdecl;external clib name 'gethostname';
function sethostname(__name:Pchar; __len:size_t):longint;cdecl;external clib name 'sethostname';
function sethostid(__id:longint):longint;cdecl;external clib name 'sethostid';
function getdomainname(__name:Pchar; __len:size_t):longint;cdecl;external clib name 'getdomainname';
function setdomainname(__name:Pchar; __len:size_t):longint;cdecl;external clib name 'setdomainname';
function vhangup:longint;cdecl;external clib name 'vhangup';
function revoke(__file:Pchar):longint;cdecl;external clib name 'revoke';
function profil(__sample_buffer:Pword; __size:size_t; __offset:size_t; __scale:dword):longint;cdecl;external clib name 'profil';
function acct(__name:Pchar):longint;cdecl;external clib name 'acct';
function getusershell:Pchar;cdecl;external clib name 'getusershell';
procedure endusershell;cdecl;external clib name 'endusershell';
procedure setusershell;cdecl;external clib name 'setusershell';
function daemon(__nochdir:longint; __noclose:longint):longint;cdecl;external clib name 'daemon';
function chroot(__path:Pchar):longint;cdecl;external clib name 'chroot';
function getpass(__prompt:Pchar):Pchar;cdecl;external clib name 'getpass';
function fsync(__fd:longint):longint;cdecl;external clib name 'fsync';
function gethostid:longint;cdecl;external clib name 'gethostid';
procedure sync;cdecl;external clib name 'sync';
function getpagesize : longint;cdecl; external clib name 'getpagesize';
function __truncate(__file:Pchar; __length:__off_t):longint;cdecl;external clib name 'truncate';
function truncate64(__file:Pchar; __length:__off64_t):longint;cdecl;external clib name 'truncate64';
function ftruncate(__fd:longint; __length:__off_t):longint;cdecl;external clib name 'ftruncate';
function ftruncate64(__fd:longint; __length:__off64_t):longint;cdecl;external clib name 'ftruncate64';
function getdtablesize:longint;cdecl;external clib name 'getdtablesize';
function brk(__addr:pointer):longint;cdecl;external clib name 'brk';
function sbrk(__delta:intptr_t):pointer;cdecl;external clib name 'sbrk';

function syscall(__sysno:longint; args:array of const):longint;cdecl;external clib name 'syscall';
function syscall(__sysno:longint):longint;cdecl;varargs;external clib name 'syscall';


function crypt(__key:Pchar; __salt:Pchar):Pchar;cdecl;external clib name 'crypt';
procedure encrypt(__block:Pchar; __edflag:longint);cdecl;external clib name 'encrypt';
procedure swab(__from:pointer; __to:pointer; __n:ssize_t);cdecl;external clib name 'swab';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TPipeDescriptors = {packed} record
    ReadDes: Integer;
    WriteDes: Integer;
  end;
  PPipeDescriptors = ^TPipeDescriptors;

function pipe(var PipeDes: TPipeDescriptors): Integer; cdecl; external clib name 'pipe';
function __read(Handle: Integer; var Buffer; Count: size_t): ssize_t; cdecl;external clib name 'read';
function __write(Handle: Integer; const Buffer; Count: size_t): ssize_t; cdecl;external clib name 'write';
function __close(Handle: Integer): Integer; cdecl;external clib name 'close';




