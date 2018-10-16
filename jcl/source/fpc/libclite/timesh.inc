
type
   Ptms = ^tms;
   tms = record
        tms_utime : clock_t;
        tms_stime : clock_t;
        tms_cutime : clock_t;
        tms_cstime : clock_t;
     end;

function times(__buffer:Ptms):clock_t;cdecl;external clib name 'times';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TTimes = tms;
  PTimes = ^TTimes;

function times(var __buffer:tms):clock_t;cdecl;external clib name 'times';

