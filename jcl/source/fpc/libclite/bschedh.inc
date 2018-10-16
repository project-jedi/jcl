
const
   SCHED_OTHER = 0;
   SCHED_FIFO = 1;
   SCHED_RR = 2;

const
   CSIGNAL = $000000ff;
   CLONE_VM = $00000100;
   CLONE_FS = $00000200;
   CLONE_FILES = $00000400;
   CLONE_SIGHAND = $00000800;
   CLONE_PID = $00001000;
   CLONE_PTRACE = $00002000;
   CLONE_VFORK = $00004000;

type
   Psched_param = ^sched_param;
   sched_param = record
     __sched_priority : longint;
   end;

  TCloneFunc = function(Arg: Pointer): Integer; cdecl; // For translation


function clone(__fn:tclonefunc; __child_stack:pointer; __flags:longint; __arg:pointer):longint;cdecl;external clib name 'clone';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
   __sched_param = sched_param;
   P__sched_param = ^__sched_param;
   TSchedParam = __sched_param;
   PSchedParam = ^TSchedParam;

   TCloneProc = TCloneFunc;



