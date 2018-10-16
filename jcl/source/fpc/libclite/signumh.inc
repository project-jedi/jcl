
Const
  SIG_ERR  = (-1);
  SIG_DFL  = (0);
  SIG_IGN  = (1);
  SIG_HOLD = (2);


const
   SIGHUP = 1;
   SIGINT = 2;
   SIGQUIT = 3;
   SIGILL = 4;
   SIGTRAP = 5;
   SIGABRT = 6;
   SIGIOT = 6;
   SIGBUS = 7;
   SIGFPE = 8;
   SIGKILL = 9;
   SIGUSR1 = 10;
   SIGSEGV = 11;
   SIGUSR2 = 12;
   SIGPIPE = 13;
   SIGALRM = 14;
   SIGTERM = 15;
   SIGSTKFLT = 16;
   SIGCHLD = 17;
   SIGCLD = SIGCHLD;
   SIGCONT = 18;
   SIGSTOP = 19;
   SIGTSTP = 20;
   SIGTTIN = 21;
   SIGTTOU = 22;
   SIGURG = 23;
   SIGXCPU = 24;
   SIGXFSZ = 25;
   SIGVTALRM = 26;
   SIGPROF = 27;
   SIGWINCH = 28;
   SIGIO = 29;
   SIGPOLL = SIGIO;
   SIGPWR = 30;
   SIGSYS = 31;
   SIGUNUSED = 31;
   _NSIG = 64;

const
   __SIGRTMIN = 32;
   __SIGRTMAX = _NSIG - 1;

function SIGRTMIN : longint;cdecl; external clib name '__libc_current_sigrtmin';
function SIGRTMAX : longint;cdecl; external clib name '__libc_current_sigrtmax';
