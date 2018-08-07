
type
   Putimbuf = ^utimbuf;
   utimbuf = record
     actime : __time_t;
     modtime : __time_t;
   end;


function utime(__file:Pchar; __file_times:Putimbuf):longint;cdecl;external clib name 'utime';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

type
  TUTimeBuffer = utimbuf;
  PUTimeBuffer = ^TUTimeBuffer;

function utime(__file:Pchar; const __file_times: utimbuf):longint;cdecl;external clib name 'utime';
