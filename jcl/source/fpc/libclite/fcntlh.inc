
const
   R_OK = 4;
   W_OK = 2;
   X_OK = 1;
   F_OK = 0;

const
   SEEK_SET = 0;
   SEEK_CUR = 1;
   SEEK_END = 2;

function fcntl(__fd:longint; __cmd:longint; args:array of const):longint;cdecl;external clib name 'fcntl';
function fcntl(__fd:longint; __cmd:longint):longint;cdecl;varargs;external clib name 'fcntl';

function open(__file:Pchar; __oflag:longint; args:array of const):longint;cdecl;external clib name 'open';
function open(__file:Pchar; __oflag:longint):longint;cdecl;varargs;external clib name 'open';

function open64(__file:Pchar; __oflag:longint; args:array of const):longint;cdecl;external clib name 'open64';
function open64(__file:Pchar; __oflag:longint):longint;cdecl;varargs;external clib name 'open64';

function creat(__file:Pchar; __mode:__mode_t):longint;cdecl;external clib name 'creat';
function creat64(__file:Pchar; __mode:__mode_t):longint;cdecl;external clib name 'creat64';

const
  F_LOCK = 1;
  F_TLOCK = 2;
  F_TEST = 3;

function lockf(__fd:longint; __cmd:longint; __len:__off_t):longint;cdecl;external clib name 'lockf';
function lockf64(__fd:longint; __cmd:longint; __len:__off64_t):longint;cdecl;external clib name 'lockf64';

function posix_fadvise(__fd:longint; __offset:__off_t; __len:size_t; __advise:longint):longint;cdecl;external clib name 'posix_fadvise';
function posix_fadvise64(__fd:longint; __offset:__off64_t; __len:size_t; __advise:longint):longint;cdecl;external clib name 'posix_fadvise64';

function posix_fallocate(__fd:longint; __offset:__off_t; __len:size_t):longint;cdecl;external clib name 'posix_fallocate';
function posix_fallocate64(__fd:longint; __offset:__off64_t; __len:size_t):longint;cdecl;external clib name 'posix_fallocate64';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function fcntl(Handle: Integer; Command: Integer; var Lock: TFlock): Integer; cdecl; external clib name 'fcntl';
function fcntl(Handle: Integer; Command: Integer; Arg: Longint): Integer; cdecl;external clib name 'fcntl';

