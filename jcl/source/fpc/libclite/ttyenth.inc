
const
   _PATH_TTYS = '/etc/ttys';
   _TTYS_OFF = 'off';
   _TTYS_ON = 'on';
   _TTYS_SECURE = 'secure';
   _TTYS_WINDOW = 'window';

type
   Pttyent = ^ttyent;
   ttyent = record
        ty_name : Pchar;
        ty_getty : Pchar;
        ty_type : Pchar;
        ty_status : longint;
        ty_window : Pchar;
        ty_comment : Pchar;
     end;


const
   TTY_ON = $01;
   TTY_SECURE = $02;

function getttyent:Pttyent;cdecl;external clib name 'getttyent';
function getttynam(__tty:Pchar):Pttyent;cdecl;external clib name 'getttynam';
function setttyent:longint;cdecl;external clib name 'setttyent';
function endttyent:longint;cdecl;external clib name 'endttyent';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TTtyEnt = ttyent;

