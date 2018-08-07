
const
   _PATH_LOG = '/dev/log';
   LOG_EMERG = 0;
   LOG_ALERT = 1;
   LOG_CRIT = 2;
   LOG_ERR = 3;
   LOG_WARNING = 4;
   LOG_NOTICE = 5;
   LOG_INFO = 6;
   LOG_DEBUG = 7;
   LOG_PRIMASK = $07;

function LOG_PRI(p : longint) : longint;
function LOG_MAKEPRI(fac,pri : longint) : longint;

const
   INTERNAL_NOPRI = $10;
   INTERNAL_MARK  = 24 shl 3;

type

   P_code = ^_code;
   _code = record
        c_name : Pchar;
        c_val : longint;
     end;
   CODE = _code;
   PCODE = ^CODE;

Const
  prioritynames: array[0..12] of _code =
  (
    (c_name: 'alert';   c_val: LOG_ALERT),
    (c_name: 'crit';    c_val: LOG_CRIT),
    (c_name: 'debug';   c_val: LOG_DEBUG),
    (c_name: 'emerg';   c_val: LOG_EMERG),
    (c_name: 'err';     c_val: LOG_ERR),
    (c_name: 'error';   c_val: LOG_ERR),
    (c_name: 'info';    c_val: LOG_INFO),
    (c_name: 'none';    c_val: INTERNAL_NOPRI),
    (c_name: 'notice';  c_val: LOG_NOTICE),
    (c_name: 'panic';   c_val: LOG_EMERG),
    (c_name: 'warn';    c_val: LOG_WARNING),
    (c_name: 'warning'; c_val: LOG_WARNING),
    (c_name: nil;       c_val: -1)
  );

const
   LOG_KERN = 0 shl 3;
   LOG_USER = 1 shl 3;
   LOG_MAIL = 2 shl 3;
   LOG_DAEMON = 3 shl 3;
   LOG_AUTH = 4 shl 3;
   LOG_SYSLOG = 5 shl 3;
   LOG_LPR = 6 shl 3;
   LOG_NEWS = 7 shl 3;
   LOG_UUCP = 8 shl 3;
   LOG_CRON = 9 shl 3;
   LOG_AUTHPRIV = 10 shl 3;
   LOG_FTP = 11 shl 3;
   LOG_LOCAL0 = 16 shl 3;
   LOG_LOCAL1 = 17 shl 3;
   LOG_LOCAL2 = 18 shl 3;
   LOG_LOCAL3 = 19 shl 3;
   LOG_LOCAL4 = 20 shl 3;
   LOG_LOCAL5 = 21 shl 3;
   LOG_LOCAL6 = 22 shl 3;
   LOG_LOCAL7 = 23 shl 3;
   LOG_NFACILITIES = 24;
   LOG_FACMASK = $03f8;

const
  facilitynames: array[0..22] of _code =
  (
    (c_name: 'auth';     c_val: LOG_AUTH),
    (c_name: 'authpriv'; c_val: LOG_AUTHPRIV),
    (c_name: 'cron';      c_val: LOG_CRON),
    (c_name: 'daemon';   c_val: LOG_DAEMON),
    (c_name: 'ftp';      c_val: LOG_FTP),
    (c_name: 'kern';     c_val: LOG_KERN),
    (c_name: 'lpr';      c_val: LOG_LPR),
    (c_name: 'mail';     c_val: LOG_MAIL),
    (c_name: 'mark';     c_val: INTERNAL_MARK),
    (c_name: 'news';     c_val: LOG_NEWS),
    (c_name: 'security'; c_val: LOG_AUTH),
    (c_name: 'syslog';   c_val: LOG_SYSLOG),
    (c_name: 'user';     c_val: LOG_USER),
    (c_name: 'uucp';     c_val: LOG_UUCP),
    (c_name: 'local0';   c_val: LOG_LOCAL0),
    (c_name: 'local1';   c_val: LOG_LOCAL1),
    (c_name: 'local2';   c_val: LOG_LOCAL2),
    (c_name: 'local3';   c_val: LOG_LOCAL3),
    (c_name: 'local4';   c_val: LOG_LOCAL4),
    (c_name: 'local5';   c_val: LOG_LOCAL5),
    (c_name: 'local6';   c_val: LOG_LOCAL6),
    (c_name: 'local7';   c_val: LOG_LOCAL7),
    (c_name: nil;        c_val: -1)
  );


function LOG_FAC(p : longint) : longint;
function LOG_MASK(pri : longint) : longint;
function LOG_UPTO(pri : longint) : longint;


const
   LOG_PID = $01;
   LOG_CONS = $02;
   LOG_ODELAY = $04;
   LOG_NDELAY = $08;
   LOG_NOWAIT = $10;
   LOG_PERROR = $20;

procedure closelog;cdecl;external clib name 'closelog';
procedure openlog(__ident:Pchar; __option:longint; __facility:longint);cdecl;external clib name 'openlog';
function setlogmask(__mask:longint):longint;cdecl;external clib name 'setlogmask';
procedure syslog(__pri: Integer; __fmt: PChar; arg : array of const); cdecl; external clib name 'syslog';
procedure vsyslog(__pri: Integer; Fmt: PChar; Arg: Pointer);cdecl; external clib name 'vsyslog';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TSysLogCode = _code;
