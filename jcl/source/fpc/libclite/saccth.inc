
const
   ACCT_COMM = 16;

type

  Pcomp_t = ^comp_t;
  comp_t = u_int16_t;

  Pacct = ^_acct;
  _acct = record
    ac_flag : char;
    ac_uid : u_int16_t;
    ac_gid : u_int16_t;
    ac_tty : u_int16_t;
    ac_btime : u_int32_t;
    ac_utime : comp_t;
    ac_stime : comp_t;
    ac_etime : comp_t;
    ac_mem : comp_t;
    ac_io : comp_t;
    ac_rw : comp_t;
    ac_minflt : comp_t;
    ac_majflt : comp_t;
    ac_swaps : comp_t;
    ac_exitcode : u_int32_t;
    ac_comm : array[0..(ACCT_COMM + 1)-1] of char;
    ac_pad : array[0..9] of char;
  end;
  P_acct = ^_acct;

Const
  AFORK = $01;
  ASU   = $02;
  ACORE = $08;
  AXSIG = $10;

  AHZ   = 100;

// Already defined in unistdh.inc
//function acct(__filename:Pchar):longint;cdecl;external clib name 'acct';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TAccountingRecord = _acct;


