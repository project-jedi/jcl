
type
   P_sigstack = ^_sigstack;
   _sigstack = record  // Renamed to avoid clash with sigstack function.
     ss_sp : pointer;
     ss_onstack : longint;
   end;

Const
  SS_ONSTACK = 1;
  SS_DISABLE = 2;

  MINSIGSTKSZ = 2048;
  SIGSTKSZ = 8192;

Type
  Psigaltstack = ^_sigaltstack;
  _sigaltstack = record  // Renamed to avoid clash with sigaltstack function.
    ss_sp : pointer;
    ss_flags : longint;
    ss_size : size_t;
  end;
  stack_t = _sigaltstack;
  Pstack_t = ^stack_t;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TSigStack = _sigstack;
  PSigStack = ^TSigStack;

  TStack = stack_t;
  PStack = ^TStack;


