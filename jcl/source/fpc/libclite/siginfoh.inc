const
  __SI_MAX_SIZE = 128;
  __SI_PAD_SIZE = (__SI_MAX_SIZE div sizeof (Integer)) - 3;

type

   Psigval = ^sigval;
   sigval = record
       case longint of
          0 : ( sival_int : longint );
          1 : ( sival_ptr : pointer );
       end;
   sigval_t = sigval;
   Psigval_t = ^sigval_t;


  // Borland compatibility types moved here, needed for siginfo
  _si_sigchld = record
     si_pid: __pid_t;
     si_uid: __uid_t;
     si_status: Integer;
     si_utime: __clock_t;
     si_stime: __clock_t;
  end;
                        
  _si_kill =  record
    si_pid: __pid_t;
    si_uid: __uid_t;
  end;
  _si_pad = packed array[0..__SI_PAD_SIZE-1] of Integer;
  _si_sigfault = record
    si_addr: Pointer; 
  end;
  _si_sigpoll = record
    si_band: Longint; 
    si_fd: Integer;
  end;
  _si_timer = record
    _timer1: Cardinal;
    _timer2: Cardinal;
  end;
  _si_rt =  record
    si_pid: __pid_t;
    si_uid: __uid_t;
    si_sigval: sigval_t;
  end;




type

   Psiginfo = ^siginfo;
   siginfo = record
        si_signo : longint;
        si_errno : longint;
        si_code : longint;
        Case integer of 
          0: (_pad: _si_pad);
          1: (_kill: _si_kill);
          2: (_timer: _si_timer);
          3: (_rt: _si_rt);
          4: (_sigchld: _si_sigchld);
          5: (_sigfault: _si_sigfault);
          6: (_sigpoll: _si_sigpoll);
     end;
   siginfo_t = siginfo;
   Psiginfo_t = ^siginfo_t;
   Tsiginfo_t = siginfo_t;

Const

  SI_ASYNCNL = -(6);
  SI_SIGIO = (-(6))+1;
  SI_ASYNCIO = (-(6))+2;
  SI_MESGQ = (-(6))+3;
  SI_TIMER = (-(6))+4;
  SI_QUEUE = (-(6))+5;
  SI_USER = (-(6))+6;
  SI_KERNEL = $80;

  ILL_ILLOPC = 1;
  ILL_ILLOPN = 2;
  ILL_ILLADR = 3;
  ILL_ILLTRP = 4;
  ILL_PRVOPC = 5;
  ILL_PRVREG = 6;
  ILL_COPROC = 7;
  ILL_BADSTK = 8;

  FPE_INTDIV = 1;
  FPE_INTOVF = 2;
  FPE_FLTDIV = 3;
  FPE_FLTOVF = 4;
  FPE_FLTUND = 5;
  FPE_FLTRES = 6;
  FPE_FLTINV = 7;
  FPE_FLTSUB = 8;

  SEGV_MAPERR = 1;
  SEGV_ACCERR = 2;

  BUS_ADRALN = 1;
  BUS_ADRERR = 2;
  BUS_OBJERR = 3;

  TRAP_BRKPT = 1;
  TRAP_TRACE = 2;

  CLD_EXITED = 1;
  CLD_KILLED = 2;
  CLD_DUMPED = 3;
  CLD_TRAPPED = 4;
  CLD_STOPPED = 5;
  CLD_CONTINUED = 6;

  POLL_IN = 1;
  POLL_OUT = 2;
  POLL_MSG = 3;
  POLL_ERR = 4;
  POLL_PRI = 5;
  POLL_HUP = 6;

__SIGEV_MAX_SIZE = 64;
__SIGEV_PAD_SIZE = (__SIGEV_MAX_SIZE div SizeOf(Integer)) - 3;

type
  // Glue structures.
  _se_pad = packed array[0..__SIGEV_PAD_SIZE-1] of Integer;
  TSignalEventStartProc = procedure(Param: sigval_t); cdecl;

  _se_sigev_thread = {packed} record
    _function: TSignalEventStartProc;  { Function to start.  }
    _attribute: Pointer;               { Really pthread_attr_t.  }
  end;

  Psigevent = ^sigevent;
  sigevent = record
  sigev_value : sigval_t;
  sigev_signo : longint;
  sigev_notify : longint;
  _sigev_un : record
    case longint of
      0 : ( _pad : array[0..(__SIGEV_PAD_SIZE)-1] of longint );
      1 : ( _sigev_thread :_se_sigev_thread;);
    end;
  end;
  sigevent_t = sigevent;
  Psigevent_t = ^sigevent_t;

Const
  SIGEV_SIGNAL = 0;
  SIGEV_NONE = 1;
  SIGEV_THREAD = 2;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TSigval = sigval_t;

  TSigInfo = siginfo;

  TSigEvent = sigevent;

                  