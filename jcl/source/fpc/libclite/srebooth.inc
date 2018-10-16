

const
   RB_AUTOBOOT = $01234567;
   RB_HALT_SYSTEM = $cdef0123;
   RB_ENABLE_CAD = $89abcdef;
   RB_DISABLE_CAD = 0;
   RB_POWER_OFF = $4321fedc;

function reboot(__howto:longint):longint;cdecl;external clib name 'reboot';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
