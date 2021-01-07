type

   Pclock_t = ^clock_t;
   clock_t = __clock_t;

   Ptime_t = ^time_t;
   time_t = __time_t;

   Pclockid_t = ^clockid_t;
   clockid_t = __clockid_t;

   Ptimer_t = ^timer_t;
   timer_t = __timer_t;

type
   Ptm = ^tm;
   tm = record
        tm_sec : longint;
        tm_min : longint;
        tm_hour : longint;
        tm_mday : longint;
        tm_mon : longint;
        tm_year : longint;
        tm_wday : longint;
        tm_yday : longint;
        tm_isdst : longint;
        case boolean of 
         false : (tm_gmtoff : longint;tm_zone : Pchar);
         true  : (__tm_gmtoff : longint;__tm_zone : Pchar);
	end;

   Pitimerspec = ^itimerspec;
   itimerspec = record
        it_interval : timespec;
        it_value : timespec;
     end;



//var
//  getdate_err : longint;cvar;external;

function clock:clock_t;cdecl;external clib name 'clock';
function __time(__timer:Ptime_t):time_t;cdecl;external clib name 'time'; overload;
function difftime(__time1:time_t; __time0:time_t):double;cdecl;external clib name 'difftime';
function mktime(__tp:Ptm):time_t;cdecl;external clib name 'mktime'; overload;
function strftime(__s:Pchar; __maxsize:size_t; __format:Pchar; __tp:Ptm):size_t;cdecl;external clib name 'strftime'; overload;
function strptime(__s:Pchar; __fmt:Pchar; __tp:Ptm):Pchar;cdecl;external clib name 'strptime'; overload;
function gmtime(__timer:Ptime_t):Ptm;cdecl;external clib name 'gmtime'; overload;
function localtime(__timer:Ptime_t):Ptm;cdecl;external clib name 'localtime'; overload;
function gmtime_r(__timer:Ptime_t; __tp:Ptm):Ptm;cdecl;external clib name 'gmtime_r'; overload;
function localtime_r(__timer:Ptime_t; __tp:Ptm):Ptm;cdecl;external clib name 'localtime_r'; overload;
function asctime(__tp:Ptm):Pchar;cdecl;external clib name 'asctime'; overload;
function ctime(__timer:Ptime_t):Pchar;cdecl;external clib name 'ctime'; overload;
function asctime_r(__tp:Ptm; __buf:Pchar):Pchar;cdecl;external clib name 'asctime_r'; overload;
function ctime_r(__timer:Ptime_t; __buf:Pchar):Pchar;cdecl;external clib name 'ctime_r'; overload;
procedure tzset;cdecl;external clib name 'tzset';
function stime(__when:Ptime_t):longint;cdecl;external clib name 'stime'; overload;
function timelocal(__tp:Ptm):time_t;cdecl;external clib name 'timelocal'; overload;
function dysize(__year:longint):longint;cdecl;external clib name 'dysize';
function nanosleep(__requested_time:Ptimespec; __remaining:Ptimespec):longint;cdecl;external clib name 'nanosleep'; overload;
function clock_getres(__clock_id:clockid_t; __res:Ptimespec):longint;cdecl;external clib name 'clock_getres'; overload;
function clock_gettime(__clock_id:clockid_t; __tp:Ptimespec):longint;cdecl;external clib name 'clock_gettime'; overload;
function clock_settime(__clock_id:clockid_t; __tp:Ptimespec):longint;cdecl;external clib name 'clock_settime'; overload;
function clock_nanosleep(__clock_id:clockid_t; __flags:longint; __req:Ptimespec; __rem:Ptimespec):longint;cdecl;external clib name 'clock_nanosleep'; overload;
function clock_getcpuclockid(__pid:pid_t; __clock_id:Pclockid_t):longint;cdecl;external clib name 'clock_getcpuclockid'; overload;
function timer_create(__clock_id:clockid_t; __evp:Psigevent; __timerid:Ptimer_t):longint;cdecl;external clib name 'timer_create'; overload;
function timer_delete(__timerid:timer_t):longint;cdecl;external clib name 'timer_delete';
function timer_settime(__timerid:timer_t; __flags:longint; __value:Pitimerspec; __ovalue:Pitimerspec):longint;cdecl;external clib name 'timer_settime'; overload;
function timer_gettime(__timerid:timer_t; __value:Pitimerspec):longint;cdecl;external clib name 'timer_gettime'; overload;
function timer_getoverrun(__timerid:timer_t):longint;cdecl;external clib name 'timer_getoverrun';
function getdate(__string:Pchar):Ptm;cdecl;external clib name 'getdate';
function getdate_r(__string:Pchar; __resbufp:Ptm):longint;cdecl;external clib name 'getdate_r';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TTime_T = Time_t;
  TCLOCKID_T = CLOCKID_T;
  TUnixTime = tm;
  PUnixTime = ^TUnixTime;

  TITimerSpec = itimerspec;

function __time(var __timer : ttime_t):time_t;cdecl;external clib name 'time'; overload;
function mktime(var __tp:tm):time_t;cdecl;external clib name 'mktime'; overload;
function strftime(__s:Pchar; __maxsize:size_t; __format:Pchar; var __tp:tm):size_t;cdecl;external clib name 'strftime'; overload;
function strptime(__s:Pchar; __fmt:Pchar; var __tp: tm):Pchar;cdecl;external clib name 'strptime'; overload;
function gmtime(var __timer : ttime_t):Ptm;cdecl;external clib name 'gmtime'; overload;
function localtime(var __timer : ttime_t):Ptm;cdecl;external clib name 'localtime'; overload;
function gmtime_r(var __timer : ttime_t; var __tp:tm):Ptm;cdecl;external clib name 'gmtime_r'; overload;
function localtime_r(var __timer : ttime_t; var __tp: tm):Ptm;cdecl;external clib name 'localtime_r'; overload;
function asctime(var __tp: tm):Pchar;cdecl;external clib name 'asctime'; overload;
function ctime(var __timer : ttime_t):Pchar;cdecl;external clib name 'ctime'; overload;
function asctime_r(var __tp:tm; __buf:Pchar):Pchar;cdecl;external clib name 'asctime_r'; overload;
function ctime_r(var __timer : ttime_t; __buf:Pchar):Pchar;cdecl;external clib name 'ctime_r'; overload;

function stime(var __when : ttime_t):longint;cdecl;external clib name 'stime'; overload;
function timegm(var __tp:tm):time_t;cdecl;external clib name 'timegm'; overload;
function timelocal(var __tp: tm):time_t;cdecl;external clib name 'timelocal'; overload;
function nanosleep(var __requested_time : ttimespec; var __remaining : ttimespec):longint;cdecl;external clib name 'nanosleep'; overload;
function clock_getres(var __clock_id:clockid_t; __res : ttimespec):longint;cdecl;external clib name 'clock_getres'; overload;
function clock_gettime(var __clock_id:clockid_t; __tp : ttimespec):longint;cdecl;external clib name 'clock_gettime'; overload;
function clock_settime(var __clock_id:clockid_t; __tp : ttimespec):longint;cdecl;external clib name 'clock_settime'; overload;
function clock_nanosleep(var __clock_id:clockid_t; __flags:longint; __req : ttimespec; var __rem : ttimespec):longint;cdecl;external clib name 'clock_nanosleep'; overload;
function clock_getcpuclockid(__pid:pid_t; var __clock_id: tclockid_t):longint;cdecl;external clib name 'clock_getcpuclockid'; overload;
function timer_create(__clock_id:clockid_t; var __ev :tsigevent; var __timerid:timer_t):longint;cdecl;external clib name 'timer_create'; overload;
function timer_settime(__timerid:timer_t; __flags:longint; const __value: titimerspec; var __ovalue:titimerspec):longint;cdecl;external clib name 'timer_settime'; overload;
function timer_gettime(__timerid:timer_t; var __value: titimerspec):longint;cdecl;external clib name 'timer_gettime'; overload;

