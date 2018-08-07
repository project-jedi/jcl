
function SHMLBA : longint;cdecl; external clib name '__getpagesize';
function __getpagesize:longint;cdecl;external clib name '__getpagesize';
function shmctl(__shmid:longint; __cmd:longint; __buf:Pshmid_ds):longint;cdecl;external clib name 'shmctl';
function shmget(__key:key_t; __size:size_t; __shmflg:longint):longint;cdecl;external clib name 'shmget';
function shmat(__shmid:longint; __shmaddr:pointer; __shmflg:longint):pointer;cdecl;external clib name 'shmat';
function shmdt(__shmaddr:pointer):longint;cdecl;external clib name 'shmdt';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function shmctl(__shmid:longint; __cmd:longint; var __buf:shmid_ds):longint;cdecl;external clib name 'shmctl';
