
const
   SEM_UNDO = $1000;
   _GETPID = 11; // renamed, conflict with getpid call.
   GETVAL = 12;
   GETALL = 13;
   GETNCNT = 14;
   GETZCNT = 15;
   SETVAL = 16;
   SETALL = 17;

type
   Psemid_ds = ^semid_ds;
   semid_ds = record
        sem_perm : ipc_perm;
        sem_otime : __time_t;
        __unused1 : dword;
        sem_ctime : __time_t;
        __unused2 : dword;
        sem_nsems : dword;
        __unused3 : dword;
        __unused4 : dword;
     end;


const
   SEM_STAT = 18;
   SEM_INFO = 19;

type
   Pseminfo = ^seminfo;
   seminfo = record
        semmap : longint;
        semmni : longint;
        semmns : longint;
        semmnu : longint;
        semmsl : longint;
        semopm : longint;
        semume : longint;
        semusz : longint;
        semvmx : longint;
        semaem : longint;
     end;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TSemaphoreIdDescriptor = semid_ds;
  PSemaphoreIdDescriptor = ^TSemaphoreIdDescriptor;

  TSemaphoreInfo = seminfo;
  PSemaphoreInfo = ^TSemaphoreInfo;
