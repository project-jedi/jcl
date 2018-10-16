
Type
   Pmsgbuf = ^msgbuf;
   msgbuf = record
        mtype : longint;
        mtext : array[0..0] of char;
     end;


function msgctl(__msqid:longint; __cmd:longint; __buf:Pmsqid_ds):longint;cdecl;external clib name 'msgctl';
function msgget(__key:key_t; __msgflg:longint):longint;cdecl;external clib name 'msgget';
function msgrcv(__msqid:longint; __msgp:pointer; __msgsz:size_t; __msgtyp:longint; __msgflg:longint):longint;cdecl;external clib name 'msgrcv';
function msgsnd(__msqid:longint; __msgp:pointer; __msgsz:size_t; __msgflg:longint):longint;cdecl;external clib name 'msgsnd';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function msgctl(__msqid:longint; __cmd:longint; const __buf:msqid_ds):longint;cdecl;external clib name 'msgctl';
function msgrcv(__msqid:longint; var __msgp; __msgsz:size_t; __msgtyp:longint; __msgflg:longint):longint;cdecl;external clib name 'msgrcv';
function msgsnd(__msqid:longint; const __msgp; __msgsz:size_t; __msgflg:longint):longint;cdecl;external clib name 'msgsnd';
