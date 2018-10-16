
type
   P_sigaction = ^_sigaction;
   _sigaction = record // Renamed, avoid conflict with sigaction function
     case integer of
       1: (sa_handler : __sighandler_t;
           sa_mask : __sigset_t;
           sa_flags : longint;
           sa_restorer : procedure ;cdecl;
          );
       // Kylix compatibility
       2: (__sigaction_handler: __sighandler_t);
   end;

const
   SA_NOCLDSTOP = 1;
   SA_NOCLDWAIT = 2;
   SA_SIGINFO = 4;

const
   SA_ONSTACK = $08000000;
   SA_RESTART = $10000000;
   SA_NODEFER = $40000000;
   SA_RESETHAND = $80000000;

   SA_INTERRUPT = $20000000;
   SA_NOMASK = SA_NODEFER;
   SA_ONESHOT = SA_RESETHAND;
   SA_STACK = SA_ONSTACK;

const
   SIG_BLOCK = 0;
   SIG_UNBLOCK = 1;
   SIG_SETMASK = 2;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TSigAction = _sigaction;
  PSigAction = ^TSigAction;
  TRestoreHandler = procedure; cdecl;
   __sigaction = _sigaction;
  TSigActionHandler = procedure(Signal: Integer); cdecl; 
    

