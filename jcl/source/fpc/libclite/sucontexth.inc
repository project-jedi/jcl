
Type
  Pgreg_t = ^greg_t;
  greg_t = longint;

const
  NGREG = 19;

Type
  Pgregset_t = ^gregset_t;
  gregset_t = greg_t;

Const
  REG_GS = 0;
  REG_FS = 1;
  REG_ES = 2;
  REG_DS = 3;
  REG_EDI = 4;
  REG_ESI = 5;
  REG_EBP = 6;
  REG_ESP = 7;
  REG_EBX = 8;
  REG_EDX = 9;
  REG_ECX = 10;
  REG_EAX = 11;
  REG_TRAPNO = 12;
  REG_ERR = 13;
  REG_EIP = 14;
  REG_CS = 15;
  REG_EFL = 16;
  REG_UESP = 17;
  REG_SS = 18;

type
   P_libc_fpreg = ^_libc_fpreg;
   _libc_fpreg = record
        significand : array[0..3] of word;
        exponent : word;
     end;

   P_libc_fpstate = ^_libc_fpstate;
   _libc_fpstate = record
        cw : dword;
        sw : dword;
        tag : dword;
        ipoff : dword;
        cssel : dword;
        dataoff : dword;
        datasel : dword;
        _st : array[0..7] of _libc_fpreg;
        status : dword;
     end;


   Pfpregset_t = ^fpregset_t;
   fpregset_t = _libc_fpstate;

   Pmcontext_t = ^mcontext_t;
   mcontext_t = record
        gregs : gregset_t;
        fpregs : fpregset_t;
        oldmask : dword;
        cr2 : dword;
     end;

   Pucontext = ^ucontext;
   ucontext = record
        uc_flags : dword;
        uc_link : Pucontext;
        uc_stack : stack_t;
        uc_mcontext : mcontext_t;
        uc_sigmask : __sigset_t;
        __fpregs_mem : _libc_fpstate;
     end;
   ucontext_t = ucontext;
   Pucontext_t = ^ucontext_t;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TUserContext = ucontext_t;
  PUserContext = ^TUserContext;


