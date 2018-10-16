
// Macros, translated.

procedure TIMEVAL_TO_TIMESPEC(const tv: TTimeVal; var ts: TTimeSpec);
procedure TIMESPEC_TO_TIMEVAL(var tv: TTimeVal; const ts: TTimeSpec);

var
  tz_dsttime : longint;cvar;public;

Type
  timezone = record
    tz_minuteswest: Integer;
    tz_dsttime: Integer;
  end;
  ptimezone = ^timezone;

  P__timezone_ptr_t = ^__timezone_ptr_t;
  __timezone_ptr_t = ^timezone;

function gettimeofday(__tv:Ptimeval; __tz:__timezone_ptr_t):longint;cdecl;external clib name 'gettimeofday';
function settimeofday(__tv:Ptimeval; __tz:Ptimezone):longint;cdecl;external clib name 'settimeofday';
function adjtime(__delta:Ptimeval; __olddelta:Ptimeval):longint;cdecl;external clib name 'adjtime';

Type
  __itimer_which = (
   ITIMER_REAL := 0,
   ITIMER_VIRTUAL := 1,
   ITIMER_PROF := 2
  );

  Pitimerval = ^itimerval;
  itimerval = record
    it_interval : timeval;
    it_value : timeval;
  end;

  P__itimer_which_t = ^__itimer_which_t;
  __itimer_which_t = __itimer_which;



function getitimer(__which:__itimer_which_t; __value:Pitimerval):longint;cdecl;external clib name 'getitimer';
function setitimer(__which:__itimer_which_t; __new:Pitimerval; __old:Pitimerval):longint;cdecl;external clib name 'setitimer';
function utimes(__file:Pchar; __tvp: Ptimeval):longint;cdecl;external clib name 'utimes';

// Macros, translated.

function timerisset(const Value: TTimeVal): Boolean;
procedure timerclear(var Value: TTimeVal);
function __timercmp(const a, b: TTimeVal): Integer;
function timeradd(const a, b: TTimeVal): TTimeVal;
function timersub(const a, b: TTimeVal): TTimeVal;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

type
  TTimeZone = timezone;

  TAccessModificationTimes = record
    AccessTime: TTimeVal;
    ModificationTime: TTimeVal;
  end;
  PAccessModificationTimes = ^TAccessModificationTimes;

  TIntervalTimerValue = itimerval;
  PIntervalTimerValue = ^TIntervalTimerValue;


function utimes(__file: PChar; const AccessModTimes: TAccessModificationTimes):longint;cdecl;external clib name 'utimes';
function gettimeofday(var __tv:timeval; __tz:__timezone_ptr_t):longint;cdecl;external clib name 'gettimeofday';
function gettimeofday(var __tv:timeval; var _tz:timezone):longint;cdecl;external clib name 'gettimeofday';
function settimeofday(const __tv: timeval; __tz:Ptimezone):longint;cdecl;external clib name 'settimeofday';
function settimeofday(const __tv: timeval; const __tz:timezone):longint;cdecl;external clib name 'settimeofday';
function adjtime(const __delta: timeval; var __olddelta:timeval):longint;cdecl;external clib name 'adjtime';
function adjtime(const __delta: timeval;  __olddelta:ptimeval):longint;cdecl;external clib name 'adjtime';


