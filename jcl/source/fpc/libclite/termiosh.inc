
function CCEQ(val,c : cc_t) : boolean;

function cfgetospeed(__termios_p:Ptermios):speed_t;cdecl;external clib name 'cfgetospeed';
function cfgetispeed(__termios_p:Ptermios):speed_t;cdecl;external clib name 'cfgetispeed';
function cfsetospeed(__termios_p:Ptermios; __speed:speed_t):longint;cdecl;external clib name 'cfsetospeed';
function cfsetispeed(__termios_p:Ptermios; __speed:speed_t):longint;cdecl;external clib name 'cfsetispeed';
function cfsetspeed(__termios_p:Ptermios; __speed:speed_t):longint;cdecl;external clib name 'cfsetspeed';
function tcgetattr(__fd:longint; __termios_p:Ptermios):longint;cdecl;external clib name 'tcgetattr';
function tcsetattr(__fd:longint; __optional_actions:longint; __termios_p:Ptermios):longint;cdecl;external clib name 'tcsetattr';
procedure cfmakeraw(__termios_p:Ptermios);cdecl;external clib name 'cfmakeraw';
function tcsendbreak(__fd:longint; __duration:longint):longint;cdecl;external clib name 'tcsendbreak';
function tcdrain(__fd:longint):longint;cdecl;external clib name 'tcdrain';
function tcflush(__fd:longint; __queue_selector:longint):longint;cdecl;external clib name 'tcflush';
function tcflow(__fd:longint; __action:longint):longint;cdecl;external clib name 'tcflow';
function tcgetsid(__fd:longint):__pid_t;cdecl;external clib name 'tcgetsid';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

function cfgetospeed(const __termios_p: termios):speed_t;cdecl;external clib name 'cfgetospeed';
function cfgetispeed(const __termios_p: termios):speed_t;cdecl;external clib name 'cfgetispeed';
function cfsetospeed(var __termios_p: termios; __speed:speed_t):longint;cdecl;external clib name 'cfsetospeed';
function cfsetispeed(var __termios_p: termios; __speed:speed_t):longint;cdecl;external clib name 'cfsetispeed';
function cfsetspeed(var __termios_p: termios; __speed:speed_t):longint;cdecl;external clib name 'cfsetspeed';
function tcgetattr(__fd:longint; var __termios_p: termios):longint;cdecl;external clib name 'tcgetattr';
function tcsetattr(__fd:longint; __optional_actions:longint; const __termios_p: termios):longint;cdecl;external clib name 'tcsetattr';
procedure cfmakeraw(var __termios_p: termios);cdecl;external clib name 'cfmakeraw';
