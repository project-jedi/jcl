
type
   __rlimit_resource =  Longint;

Const
     RLIMIT_CPU = 0;
     RLIMIT_FSIZE = 1;
     RLIMIT_DATA = 2;
     RLIMIT_STACK = 3;
     RLIMIT_CORE = 4;
     RLIMIT_RSS = 5;
     RLIMIT_NOFILE = 7;
     RLIMIT_OFILE = RLIMIT_NOFILE;
     RLIMIT_AS = 9;
     RLIMIT_NPROC = 6;
     RLIMIT_MEMLOCK = 8;
     RLIMIT_LOCKS = 10;
     RLIMIT_NLIMITS = 11;
     RLIM_NLIMITS = RLIMIT_NLIMITS;

  RLIM_INFINITY = LongInt(-1);
  RLIM64_INFINITY = Int64(-1);

  RLIM_SAVED_MAX = RLIM_INFINITY;
  RLIM_SAVED_CUR = RLIM_INFINITY;

type
  Prlim_t = ^rlim_t;
  rlim_t = __rlim_t;

  Prlim64_t = ^rlim64_t;
  rlim64_t = __rlim64_t;

  Prlimit = ^rlimit;
  rlimit = record
    rlim_cur : rlim_t;
    rlim_max : rlim_t;
  end;

  Prlimit64 = ^rlimit64;
  rlimit64 = record
    rlim_cur : rlim64_t;
    rlim_max : rlim64_t;
  end;

type
  __rusage_who =  Longint;

Const
  RUSAGE_SELF = 0;
  RUSAGE_CHILDREN = -(1);
  RUSAGE_BOTH = -(2);

type
  Prusage = ^rusage;
  rusage = record
       ru_utime : timeval;
       ru_stime : timeval;
       ru_maxrss : longint;
       ru_ixrss : longint;
       ru_idrss : longint;
       ru_isrss : longint;
       ru_minflt : longint;
       ru_majflt : longint;
       ru_nswap : longint;
       ru_inblock : longint;
       ru_oublock : longint;
       ru_msgsnd : longint;
       ru_msgrcv : longint;
       ru_nsignals : longint;
       ru_nvcsw : longint;
       ru_nivcsw : longint;
    end;


const
  PRIO_MIN = -(20);
  PRIO_MAX = 20;

type
  __priority_which =  Longint;

Const
   PRIO_PROCESS = 0;
   PRIO_PGRP = 1;
   PRIO_USER = 2;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TRLimit = rlimit;
  TRLimit64 = rlimit64;
  TRUsage = rusage;

