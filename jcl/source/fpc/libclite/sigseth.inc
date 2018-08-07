
type

   P__sig_atomic_t = ^__sig_atomic_t;
   __sig_atomic_t = longint;
Const
  _SIGSET_NWORDS = 1024 div (8 * (sizeof(dword)));

type

   P__sigset_t = ^__sigset_t;
   __sigset_t = record
        __val : array[0..(_SIGSET_NWORDS)-1] of dword;
     end;
  sigset_t = __sigset_t;
  Psigset_t = ^sigset_t;

function __sigismember(_para1:P__sigset_t; _para2:longint):longint;cdecl;external clib name '__sigismember';
function __sigaddset(_para1:P__sigset_t; _para2:longint):longint;cdecl;external clib name '__sigaddset';
function __sigdelset(_para1:P__sigset_t; _para2:longint):longint;cdecl;external clib name '__sigdelset';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TSigset = __sigset_t;
  PSigset = ^TSigset;
  sig_atomic_t =  __sig_atomic_t;


