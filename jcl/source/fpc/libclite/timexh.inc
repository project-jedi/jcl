
type
   Pntptimeval = ^ntptimeval;
   ntptimeval = record
        time : timeval;
        maxerror : longint;
        esterror : longint;
     end;

   Ptimex = ^timex;
   timex = record
        modes : dword;
        offset : longint;
        freq : longint;
        maxerror : longint;
        esterror : longint;
        status : longint;
        constant : longint;
        precision : longint;
        tolerance : longint;
        time : timeval;
        tick : longint;
        ppsfreq : longint;
        jitter : longint;
        shift : longint;
        stabil : longint;
        jitcnt : longint;
        calcnt : longint;
        errcnt : longint;
        stbcnt : longint;
        bitfield11 : longint;
        bitfield12 : longint;
        bitfield13 : longint;
        bitfield14 : longint;
        bitfield21 : longint;
        bitfield22 : longint;
        bitfield23 : longint;
        bitfield24 : longint;
        bitfield31 : longint;
        bitfield32 : longint;
        bitfield33 : longint;
        bitfield34 : longint;
     end;


const
   ADJ_OFFSET = $0001;
   ADJ_FREQUENCY = $0002;
   ADJ_MAXERROR = $0004;
   ADJ_ESTERROR = $0008;
   ADJ_STATUS = $0010;
   ADJ_TIMECONST = $0020;
   ADJ_TICK = $4000;
   ADJ_OFFSET_SINGLESHOT = $8001;
   MOD_OFFSET = ADJ_OFFSET;
   MOD_FREQUENCY = ADJ_FREQUENCY;
   MOD_MAXERROR = ADJ_MAXERROR;
   MOD_ESTERROR = ADJ_ESTERROR;
   MOD_STATUS = ADJ_STATUS;
   MOD_TIMECONST = ADJ_TIMECONST;
   MOD_CLKB = ADJ_TICK;
   MOD_CLKA = ADJ_OFFSET_SINGLESHOT;
   STA_PLL = $0001;
   STA_PPSFREQ = $0002;
   STA_PPSTIME = $0004;
   STA_FLL = $0008;
   STA_INS = $0010;
   STA_DEL = $0020;
   STA_UNSYNC = $0040;
   STA_FREQHOLD = $0080;
   STA_PPSSIGNAL = $0100;
   STA_PPSJITTER = $0200;
   STA_PPSWANDER = $0400;
   STA_PPSERROR = $0800;
   STA_CLOCKERR = $1000;
   STA_RONLY = (((STA_PPSSIGNAL or STA_PPSJITTER) or STA_PPSWANDER) or STA_PPSERROR) or STA_CLOCKERR;
   TIME_OK = 0;
   TIME_INS = 1;
   TIME_DEL = 2;
   TIME_OOP = 3;
   TIME_WAIT = 4;
   TIME_ERROR = 5;
   TIME_BAD = TIME_ERROR;
   MAXTC = 6;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type

function __adjtimex(var __ntx:timex):longint;cdecl;external clib name '__adjtimex';
function adjtimex(var __ntx:timex):longint;cdecl;external clib name 'adjtimex';
function ntp_gettime(var __ntv:ntptimeval):longint;cdecl;external clib name 'ntp_gettime';
function ntp_adjtime(var __tntx:timex):longint;cdecl;external clib name 'ntp_adjtime';
