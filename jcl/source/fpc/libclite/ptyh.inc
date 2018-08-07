
function openpty(__amaster:Plongint; __aslave:Plongint; __name:Pchar; __termp:Ptermios; __winp:Pwinsize):longint;cdecl;external clib name 'openpty';
function forkpty(__amaster:Plongint; __name:Pchar; __termp:Ptermios; __winp:Pwinsize):longint;cdecl;external clib name 'forkpty';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
