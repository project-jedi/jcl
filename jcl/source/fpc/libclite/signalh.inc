
function sysv_signal(__sig:longint; __handler:__sighandler_t):__sighandler_t;cdecl;external clib name 'sysv_signal';
function signal(__sig:longint; __handler:__sighandler_t):__sighandler_t;cdecl;external clib name 'signal';
function bsd_signal(__sig:longint; __handler:__sighandler_t):__sighandler_t;cdecl;external clib name 'bsd_signal';
function kill(__pid:__pid_t; __sig:longint):longint;cdecl;external clib name 'kill';
function killpg(__pgrp:__pid_t; __sig:longint):longint;cdecl;external clib name 'killpg';
function __raise(__sig:longint):longint;cdecl;external clib name 'raise';
function ssignal(__sig:longint; __handler:__sighandler_t):__sighandler_t;cdecl;external clib name 'ssignal';
function gsignal(__sig:longint):longint;cdecl;external clib name 'gsignal';
procedure psignal(__sig:longint; __s:Pchar);cdecl;external clib name 'psignal';
function __sigpause(__sig_or_mask:longint; __is_sig:longint):longint;cdecl;external clib name '__sigpause';
function sigpause(__mask:longint):longint;cdecl;external clib name 'sigpause';
function sigblock(__mask:longint):longint;cdecl;external clib name 'sigblock';
function sigsetmask(__mask:longint):longint;cdecl;external clib name 'sigsetmask';
function siggetmask:longint;cdecl;external clib name 'siggetmask';

const
   NSIG = _NSIG;

Type
   Psighandler_t = ^sighandler_t;
   sighandler_t = __sighandler_t;

   Psig_t = ^sig_t;
   sig_t = __sighandler_t;


var
  _sys_siglist : array[0..(_NSIG)-1] of Pchar;cvar;external;
  sys_siglist : array[0..(_NSIG)-1] of Pchar;cvar;external;

Type
  P_sigvec = ^_sigvec;
  _sigvec = record
    sv_handler : __sighandler_t;
    sv_mask : longint;
    sv_flags : longint;
  end;


const
   SV_ONSTACK = 1 shl 0;
   SV_INTERRUPT = 1 shl 1;
   SV_RESETHAND = 1 shl 2;

function sigvec(__sig:longint; __vec:p_sigvec; __ovec:p_sigvec):longint;cdecl;external clib name 'sigvec';
function sigreturn(__scp:Psigcontext):longint;cdecl;external clib name 'sigreturn';
function siginterrupt(__sig:longint; __interrupt:longint):longint;cdecl;external clib name 'siginterrupt';
function sigstack(__ss:Psigstack; __oss:Psigstack):longint;cdecl;external clib name 'sigstack';
function sigaltstack(__ss:Psigaltstack; __oss:Psigaltstack):longint;cdecl;external clib name 'sigaltstack';
function sighold(__sig:longint):longint;cdecl;external clib name 'sighold';
function sigrelse(__sig:longint):longint;cdecl;external clib name 'sigrelse';
function sigignore(__sig:longint):longint;cdecl;external clib name 'sigignore';
function sigset(__sig:longint; __disp:__sighandler_t):__sighandler_t;cdecl;external clib name 'sigset';
function sigaction(__sig: longint; Action: PSigAction; OldAction: PSigAction): Integer; cdecl;external clib name 'sigaction';

function __libc_current_sigrtmin:longint;cdecl;external clib name '__libc_current_sigrtmin';
function __libc_current_sigrtmax:longint;cdecl;external clib name '__libc_current_sigrtmax';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TSigVec = _sigvec;
  PSigVec = ^TSigVec;


function sigemptyset(var SigSet : TSigSet):longint;cdecl;external clib name 'sigemptyset';
function sigfillset(var SigSet : TSigSet):longint;cdecl;external clib name 'sigfillset';
function sigaddset(var SigSet : TSigSet; SigNum : Longint):longint;cdecl;external clib name 'sigaddset';
function sigdelset(var SigSet : TSigSet; SigNum : Longint):longint;cdecl;external clib name 'sigdelset';
function sigismember(const SigSet : TSigSet; SigNum : Longint):longint;cdecl;external clib name 'sigismember';
function sigisemptyset(const SigSet : TSigSet):longint;cdecl;external clib name 'sigisemptyset';
function sigandset(var SigSet : TSigSet; const Left : TSigSet; const Right : TSigSet):longint;cdecl;external clib name 'sigandset';
function sigorset(var SigSet : TSigSet; const Left : TSigSet; const Right : TSigSet):longint;cdecl;external clib name 'sigorset';
function sigprocmask(__how:longint; var SigSet : TSigSet; var oldset: Tsigset):longint;cdecl;external clib name 'sigprocmask';
function sigsuspend(SigSet : TSigSet):longint;cdecl;external clib name 'sigsuspend';
function sigaction(__sig:longint; Const act: _sigaction; Var oldact: _sigaction):longint;cdecl;external clib name 'sigaction';
function sigpending(SigSet : TSigSet):longint;cdecl;external clib name 'sigpending';
function sigwait(SigSet : TSigSet; Var __sig:longint):longint;cdecl;external clib name 'sigwait';
function sigwaitinfo(SigSet : TSigSet; var __info: tsiginfo_t):longint;cdecl;external clib name 'sigwaitinfo';
function sigtimedwait(SigSet : TSigSet; var __info: tsiginfo_t; var __timeout: ttimespec):longint;cdecl;external clib name 'sigtimedwait';
function sigqueue(__pid:__pid_t; __sig:longint; __val:sigval):longint;cdecl;external clib name 'sigqueue';

function sigvec(__sig:longint; const vec: tsigvec; __ovec:p_sigvec):longint;cdecl;external clib name 'sigvec';
function sigvec(__sig:longint; const vec: tsigvec; var __ovec: tsigvec):longint;cdecl;external clib name 'sigvec';


