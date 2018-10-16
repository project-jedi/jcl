
type
   Psembuf = ^sembuf;
   sembuf = record
        sem_num : word;
        sem_op : smallint;
        sem_flg : smallint;
     end;


function semctl(__semid:longint; __semnum:longint; __cmd:longint; args:array of const):longint;cdecl;external clib name 'semctl';
function semctl(__semid:longint; __semnum:longint; __cmd:longint):longint;varargs;cdecl;external clib name 'semctl';
function semget(__key:key_t; __nsems:longint; __semflg:longint):longint;cdecl;external clib name 'semget';
function semop(__semid:longint; __sops:psembuf; __nsops:size_t):longint;cdecl;external clib name 'semop';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TSemaphoreBuffer = sembuf;
  PSemaphoreBuffer = ^TSemaphoreBuffer;

function semop(__semid:longint; var __sops:sembuf; __nsops:size_t):longint;cdecl;external clib name 'semop';
