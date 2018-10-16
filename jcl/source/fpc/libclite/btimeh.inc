
const
   CLOCKS_PER_SEC = 1000000;

function __sysconf(_para1:longint):longint;cdecl;external clib name '__sysconf';
function CLK_TCK : __clock_t;

const
   CLOCK_REALTIME = 0;
   CLOCK_PROCESS_CPUTIME_ID = 2;
   CLOCK_THREAD_CPUTIME_ID = 3;
   TIMER_ABSTIME = 1;

const
   _STRUCT_TIMEVAL = 1;

type
   Ptimeval = ^timeval;
   timeval = record
        tv_sec : __time_t;
        tv_usec : __suseconds_t;
     end;



{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TTimeVal = timeval;


