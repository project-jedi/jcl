

const
   UTMP_FILE = _PATH_UTMP;
   UTMP_FILENAME = _PATH_UTMP;
   WTMP_FILE = _PATH_WTMP;
   WTMP_FILENAME = _PATH_WTMP;

function login_tty(__fd:longint):longint;cdecl;external clib name 'login_tty';
procedure login(__entry:Putmp);cdecl;external clib name 'login'; overload;
function logout(__ut_line:Pchar):longint;cdecl;external clib name 'logout';
procedure logwtmp(__ut_line:Pchar; __ut_name:Pchar; __ut_host:Pchar);cdecl;external clib name 'logwtmp';
procedure updwtmp(__wtmp_file:Pchar; __utmp:Putmp);cdecl;external clib name 'updwtmp'; overload;
function utmpname(__file:Pchar):longint;cdecl;external clib name 'utmpname';
function getutent:Putmp;cdecl;external clib name 'getutent';
procedure setutent;cdecl;external clib name 'setutent';
procedure endutent;cdecl;external clib name 'endutent';
function getutid(__id:Putmp):Putmp;cdecl;external clib name 'getutid'; overload;
function getutline(__line:Putmp):Putmp;cdecl;external clib name 'getutline'; overload;
function pututline(__utmp_ptr:Putmp):Putmp;cdecl;external clib name 'pututline'; overload;
function getutent_r(__buffer:Putmp; __result:PPutmp):longint;cdecl;external clib name 'getutent_r'; overload;
function getutid_r(__id:Putmp; __buffer:Putmp; __result:PPutmp):longint;cdecl;external clib name 'getutid_r'; overload;
function getutline_r(__line:Putmp; __buffer:Putmp; __result:PPutmp):longint;cdecl;external clib name 'getutline_r'; overload;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
procedure login(const __entry: utmp);cdecl;external clib name 'login'; overload;
procedure updwtmp(__wtmp_file:Pchar; const __utmp:utmp);cdecl;external clib name 'updwtmp'; overload;
function getutid(const __id: utmp):Putmp;cdecl;external clib name 'getutid'; overload;
function getutline(const __line:utmp):Putmp;cdecl;external clib name 'getutline'; overload;
function pututline(const __utmp_ptr: utmp):Putmp;cdecl;external clib name 'pututline'; overload;
function getutent_r(var __buffer:Putmp; var __result: Putmp):longint;cdecl;external clib name 'getutent_r'; overload;
function getutid_r(const __id:utmp; var __buffer: utmp; var __result:Putmp):longint;cdecl;external clib name 'getutid_r'; overload;
function getutline_r(const __line:utmp; var __buffer: utmp; var __result:Putmp):longint;cdecl;external clib name 'getutline_r'; overload;
