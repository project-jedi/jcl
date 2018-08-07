
type
   Psgttyb = ^sgttyb;
   sgttyb = record
       {undefined structure}
     end;


function gtty(__fd:longint; __params:Psgttyb):longint;cdecl;external clib name 'gtty';
function stty(__fd:longint; __params:Psgttyb):longint;cdecl;external clib name 'stty';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TSgTTyB = sgttyb;
