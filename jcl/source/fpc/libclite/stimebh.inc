
type
   Ptimeb = ^timeb;
   timeb = record
     time : time_t;
     millitm : word;
     timezone : smallint;
     dstflag : smallint;
   end;


function ftime(__timebuf:Ptimeb):longint;cdecl;external clib name 'ftime';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function ftime(var __timebuf:timeb):longint;cdecl;external clib name 'ftime';
