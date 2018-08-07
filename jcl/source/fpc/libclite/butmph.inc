
const
   UT_LINESIZE = 32;
   UT_NAMESIZE = 32;
   UT_HOSTSIZE = 256;

type
   Plastlog = ^lastlog;
   lastlog = record
        ll_time : __time_t;
        ll_line : array[0..(UT_LINESIZE)-1] of char;
        ll_host : array[0..(UT_HOSTSIZE)-1] of char;
     end;

   Pexit_status = ^exit_status;
   exit_status = record
        e_termination : smallint;
        e_exit : smallint;
     end;

   Putmp = ^utmp;
   utmp = record
        ut_type : smallint;
        ut_pid : pid_t;
        ut_line : array[0..(UT_LINESIZE)-1] of char;
        ut_id : array[0..3] of char;
        ut_user : array[0..(UT_NAMESIZE)-1] of char;
        ut_host : array[0..(UT_HOSTSIZE)-1] of char;
        ut_exit : exit_status;
        ut_session : longint;
        ut_tv : timeval;
        ut_addr_v6 : array[0..3] of int32_t;
        __unused : array[0..19] of char;
     end;
   PPutmp = ^Putmp;

const
   EMPTY = 0;
   RUN_LVL = 1;
   BOOT_TIME = 2;
   NEW_TIME = 3;
   OLD_TIME = 4;
   INIT_PROCESS = 5;
   LOGIN_PROCESS = 6;
   USER_PROCESS = 7;
   DEAD_PROCESS = 8;
   ACCOUNTING = 9;
   UT_UNKNOWN = EMPTY;
   _HAVE_UT_TYPE = 1;
   _HAVE_UT_PID = 1;
   _HAVE_UT_ID = 1;
   _HAVE_UT_TV = 1;
   _HAVE_UT_HOST = 1;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TUserTmp = utmp;
  PUserTmp = ^TUserTmp;
