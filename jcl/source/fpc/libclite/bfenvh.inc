
Const
  FE_INVALID = $01;
  __FE_DENORM = $02;
  FE_DIVBYZERO = $04;
  FE_OVERFLOW = $08;
  FE_UNDERFLOW = $10;
  FE_INEXACT = $20;

  FE_ALL_EXCEPT = (((FE_INEXACT or FE_DIVBYZERO) or FE_UNDERFLOW) or FE_OVERFLOW) or FE_INVALID;

  FE_TONEAREST = 0;
  FE_DOWNWARD = $400;
  FE_UPWARD = $800;
  FE_TOWARDZERO = $c00;

type
   Pfexcept_t = ^fexcept_t;
   fexcept_t = word;

   Pfenv_t = ^fenv_t;
   fenv_t = record
        __control_word : word;
        __unused1 : word;
        __status_word : word;
        __unused2 : word;
        __tags : word;
        __unused3 : word;
        __eip : dword;
        __cs_selector : word;
        flag0 : word;
        __data_offset : dword;
        __data_selector : word;
        __unused5 : word;
     end;
const
   bm_fenv_t___opcode = $7FF;
   bp_fenv_t___opcode = 0;
   bm_fenv_t___unused4 = $F800;
   bp_fenv_t___unused4 = 11;

const
  FE_DFL_ENV = Pfenv_t(-1);
  FE_NOMASK_ENV = Pfenv_t(-2);

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TExceptionFlags = fexcept_t;
  PExceptionFlags = ^TExceptionFlags;

  TFloatingPointEnv = fenv_t;
  PFloatingPointEnv = ^TFloatingPointEnv;
