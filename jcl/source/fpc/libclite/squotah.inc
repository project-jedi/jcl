
function dbtob(num: cardinal): cardinal;
function btodb(num: cardinal): cardinal;
function fs_to_dq_blocks(num, blksize: cardinal): quad_t;


const
  MAX_IQ_TIME = 604800;
  MAX_DQ_TIME = 604800;
  MAXQUOTAS = 2;
  USRQUOTA = 0;
  GRPQUOTA = 1;

const
  INITQFNAMES: array[0..2] of PChar = ('user','group','undefined');

  QUOTAFILENAME = 'quota';
  QUOTAGROUP = 'staff';

  NR_DQHASH = 43;
  NR_DQUOTS = 256;
  SUBCMDMASK = $00ff;
  SUBCMDSHIFT = 8;

function QCMD(cmd, _type: cardinal): cardinal;


const
  Q_QUOTAON = $0100;
  Q_QUOTAOFF = $0200;
  Q_GETQUOTA = $0300;
  Q_SETQUOTA = $0400;
  Q_SETUSE = $0500;
  Q_SYNC = $0600;
  Q_SETQLIM = $0700;
  Q_GETSTATS = $0800;
  Q_RSQUASH = $1000;

type
  Pdqblk = ^dqblk;
  dqblk = record
    dqb_bhardlimit : u_int32_t;
    dqb_bsoftlimit : u_int32_t;
    dqb_curblocks : u_int32_t;
    dqb_ihardlimit : u_int32_t;
    dqb_isoftlimit : u_int32_t;
    dqb_curinodes : u_int32_t;
    dqb_btime : time_t;
    dqb_itime : time_t;
  end;

function dqoff(UID: loff_t): quad_t;


type
  Pdqstats = ^dqstats;
  dqstats = record
       lookups : u_int32_t;
       drops : u_int32_t;
       reads : u_int32_t;
       writes : u_int32_t;
       cache_hits : u_int32_t;
       allocated_dquots : u_int32_t;
       free_dquots : u_int32_t;
       syncs : u_int32_t;
    end;


function quotactl(__cmd:longint; __special:Pchar; __id:longint; __addr:caddr_t):longint;cdecl;external clib name 'quotactl';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
