
const
   _PATH_UTMPX = _PATH_UTMP;
   _PATH_WTMPX = _PATH_WTMP;

const
   __UT_LINESIZE = 32;
   __UT_NAMESIZE = 32;
   __UT_HOSTSIZE = 256;

type
   P__exit_status = ^__exit_status;
   __exit_status = record
     e_termination : smallint;
     e_exit : smallint;
  end;

  Putmpx = ^utmpx;
  utmpx = record
    ut_type : smallint;
    ut_pid : __pid_t;
    ut_line : array[0..(__UT_LINESIZE)-1] of char;
    ut_id : array[0..3] of char;
    ut_user : array[0..(__UT_NAMESIZE)-1] of char;
    ut_host : array[0..(__UT_HOSTSIZE)-1] of char;
    ut_exit : __exit_status;
    ut_session : longint;
    ut_tv : timeval;
    ut_addr_v6 : array[0..3] of __int32_t;
    __unused : array[0..19] of char;
  end;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TUserTmpX = utmpx;
  PUserTmpX = ^TUserTmpX;


