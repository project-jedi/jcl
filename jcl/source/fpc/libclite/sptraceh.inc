
type

   __ptrace_request =(
     PTRACE_TRACEME := 0,
     PTRACE_PEEKTEXT := 1,
     PTRACE_PEEKDATA := 2,
     PTRACE_PEEKUSER := 3,
     PTRACE_POKETEXT := 4,
     PTRACE_POKEDATA := 5,
     PTRACE_POKEUSER := 6,
     PTRACE_CONT := 7,
     PTRACE_KILL := 8,
     PTRACE_SINGLESTEP := 9,
     PTRACE_GETREGS := 12,
     PTRACE_SETREGS := 13,
     PTRACE_GETFPREGS := 14,
     PTRACE_SETFPREGS := 15,
     PTRACE_ATTACH := 16,
     PTRACE_DETACH := 17,
     PTRACE_GETFPXREGS := 18,
     PTRACE_SETFPXREGS := 19,
     PTRACE_SYSCALL := 24
  );

Const
   PT_TRACE_ME = PTRACE_TRACEME;
   PT_READ_I = PTRACE_PEEKTEXT;
   PT_READ_D = PTRACE_PEEKDATA;
   PT_READ_U = PTRACE_PEEKUSER;
   PT_WRITE_I = PTRACE_POKETEXT;
   PT_WRITE_D = PTRACE_POKEDATA;
   PT_WRITE_U = PTRACE_POKEUSER;
   PT_CONTINUE = PTRACE_CONT;
   PT_KILL = PTRACE_KILL;
   PT_STEP = PTRACE_SINGLESTEP;
   PT_GETREGS = PTRACE_GETREGS;
   PT_SETREGS = PTRACE_SETREGS;
   PT_GETFPREGS = PTRACE_GETFPREGS;
   PT_SETFPREGS = PTRACE_SETFPREGS;
   PT_ATTACH = PTRACE_ATTACH;
   PT_DETACH = PTRACE_DETACH;
   PT_GETFPXREGS = PTRACE_GETFPXREGS;
   PT_SETFPXREGS = PTRACE_SETFPXREGS;
   PT_SYSCALL = PTRACE_SYSCALL;

function ptrace(__request:__ptrace_request; args:array of const):longint;cdecl;external clib name 'ptrace';
function ptrace(__request:__ptrace_request):longint;cdecl;varargs;external clib name 'ptrace';
function ptrace(__request: __ptrace_request; PID: pid_t; Address: Pointer; Data: Longint): longint; cdecl; external clib name 'ptrace';
function ptrace(__request: __ptrace_request; PID: pid_t; Address: Pointer; Data: Longint; Addr2: Pointer): longint; cdecl; external clib name 'ptrace';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
