

Const
  MAP_FAILED = pointer(-1);

Const
  rtlib = 'rt';

function mmap(__addr:pointer; __len:size_t; __prot:longint; __flags:longint; __fd:longint;
           __offset:__off_t):pointer;cdecl;external clib name 'mmap';
function mmap64(__addr:pointer; __len:size_t; __prot:longint; __flags:longint; __fd:longint;
           __offset:__off64_t):pointer;cdecl;external clib name 'mmap64';
function munmap(__addr:pointer; __len:size_t):longint;cdecl;external clib name 'munmap';
function mprotect(__addr:pointer; __len:size_t; __prot:longint):longint;cdecl;external clib name 'mprotect';
function msync(__addr:pointer; __len:size_t; __flags:longint):longint;cdecl;external clib name 'msync';
function madvise(__addr:pointer; __len:size_t; __advice:longint):longint;cdecl;external clib name 'madvise';
function posix_madvise(__addr:pointer; __len:size_t; __advice:longint):longint;cdecl;external clib name 'posix_madvise';
function mlock(__addr:pointer; __len:size_t):longint;cdecl;external clib name 'mlock';
function munlock(__addr:pointer; __len:size_t):longint;cdecl;external clib name 'munlock';
function mlockall(__flags:longint):longint;cdecl;external clib name 'mlockall';
function munlockall:longint;cdecl;external clib name 'munlockall';
function mremap(__addr:pointer; __old_len:size_t; __new_len:size_t; __may_move:longint):pointer;cdecl;external clib name 'mremap';
function mincore(__start:pointer; __len:size_t; __vec:Pbyte):longint;cdecl;external clib name 'mincore';
function shm_open(__name:Pchar; __oflag:longint; __mode:mode_t):longint;cdecl;external rtlib name 'shm_open';
function shm_unlink(__name:Pchar):longint;cdecl;external rtlib name 'shm_unlink';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
