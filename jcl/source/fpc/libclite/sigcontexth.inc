
type
   P_fpreg = ^_fpreg;
   _fpreg = record
        significand : array[0..3] of word;
        exponent : word;
     end;

   P_fpxreg = ^_fpxreg;
   _fpxreg = record
        significand : array[0..3] of word;
        exponent : word;
        padding : array[0..2] of word;
     end;

   P_xmmreg = ^_xmmreg;
   _xmmreg = record
        element : array[0..3] of dword;
     end;

   P_fpstate = ^_fpstate;
   _fpstate = record
        cw : dword;
        sw : dword;
        tag : dword;
        ipoff : dword;
        cssel : dword;
        dataoff : dword;
        datasel : dword;
        _st : array[0..7] of _fpreg;
        status : word;
        magic : word;
        _fxsr_env : array[0..5] of dword;
        mxcsr : dword;
        reserved : dword;
        _fxsr_st : array[0..7] of _fpxreg;
        _xmm : array[0..7] of _xmmreg;
        padding : array[0..55] of dword;
     end;


const
   X86_FXSR_MAGIC = $0000;

type
   Psigcontext = ^sigcontext;
   sigcontext = record
        gs : word;
        __gsh : word;
        fs : word;
        __fsh : word;
        es : word;
        __esh : word;
        ds : word;
        __dsh : word;
        edi : dword;
        esi : dword;
        ebp : dword;
        esp : dword;
        ebx : dword;
        edx : dword;
        ecx : dword;
        eax : dword;
        trapno : dword;
        err : dword;
        eip : dword;
        cs : word;
        __csh : word;
        eflags : dword;
        esp_at_signal : dword;
        ss : word;
        __ssh : word;
        fpstate : P_fpstate;
        oldmask : dword;
        cr2 : dword;
     end;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}
  TFPreg = _fpreg;
  PFPreg = ^TFPreg;

  TFPstate = _fpstate;
  PFPstate = ^TFPstate;

  TSigContext = sigcontext;

