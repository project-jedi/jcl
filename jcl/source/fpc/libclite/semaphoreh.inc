
type
   Psem_t = ^sem_t;
   sem_t = record
        __sem_lock : _pthread_fastlock;
        __sem_value : longint;
        __sem_waiting : _pthread_descr;
     end;
              

Const
  SEM_FAILED    = Psem_t(nil);
  SEM_VALUE_MAX = ((not 0) shr 1);

function sem_init(__sem:Psem_t; __pshared:longint; __value:dword):longint;cdecl;external threadslib name 'sem_init';
function sem_destroy(__sem:Psem_t):longint;cdecl;external threadslib name 'sem_destroy';
function sem_open(__name:Pchar; __oflag:longint; args:array of const):Psem_t;cdecl;external threadslib name 'sem_open';
function sem_open(__name:Pchar; __oflag:longint):Psem_t;cdecl;varargs;external threadslib name 'sem_open';
function sem_close(__sem:Psem_t):longint;cdecl;external threadslib name 'sem_close';
function sem_unlink(__name:Pchar):longint;cdecl;external threadslib name 'sem_unlink';
function sem_wait(__sem:Psem_t):longint;cdecl;external threadslib name 'sem_wait';
function sem_timedwait(__sem:Psem_t; __abstime:Ptimespec):longint;cdecl;external threadslib name 'sem_timedwait';
function sem_trywait(__sem:Psem_t):longint;cdecl;external threadslib name 'sem_trywait';
function sem_post(__sem:Psem_t):longint;cdecl;external threadslib name 'sem_post';
function sem_getvalue(__sem:Psem_t; __sval:Plongint):longint;cdecl;external threadslib name 'sem_getvalue';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TSemaphore = sem_t;
  PSemaphore = ^TSemaphore;

   __sem_lock_t = record { Not in header file - anonymous , should equal _pthread_fastlock} 
     status: Longint;
     spinlock: Integer;
   end;

  TPCharArray = System.TPCharArray;
  PPCharArray = System.PPCharArray;

function sem_init(var __sem: sem_t; __pshared:longint; __value:dword):longint;cdecl;external threadslib name 'sem_init';
function sem_destroy(var __sem: sem_t):longint;cdecl;external threadslib name 'sem_destroy';
function sem_close(var __sem: sem_t):longint;cdecl;external threadslib name 'sem_close';
function sem_wait(var __sem: sem_t):longint;cdecl;external threadslib name 'sem_wait';
function sem_timedwait(var __sem: sem_t; __abstime:Ptimespec):longint;cdecl;external threadslib name 'sem_timedwait';
function sem_trywait(var __sem: sem_t):longint;cdecl;external threadslib name 'sem_trywait';
function sem_post(var __sem: sem_t):longint;cdecl;external threadslib name 'sem_post';
function sem_getvalue(var __sem: sem_t; __sval:Plongint):longint;cdecl;external threadslib name 'sem_getvalue';

      
