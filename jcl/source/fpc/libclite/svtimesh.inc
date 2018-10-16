
const
   VTIMES_UNITS_PER_SECOND = 60;

type
   Pvtimes = ^_vtimes;
   _vtimes = record
        vm_utime : longint;
        vm_stime : longint;
        vm_idsrss : dword;
        vm_ixrss : dword;
        vm_maxrss : longint;
        vm_majflt : longint;
        vm_minflt : longint;
        vm_nswap : longint;
        vm_inblk : longint;
        vm_oublk : longint;
     end;
    P_vtimes = ^_vtimes;

function vtimes(__current:Pvtimes; __child:Pvtimes):longint;cdecl;external clib name 'vtimes';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  Tvtimes = _vtimes;

function vtimes(var __current: _vtimes; var __child: _vtimes):longint;cdecl;external clib name 'vtimes';
function vtimes(var __current: _vtimes; __child: pvtimes):longint;cdecl;external clib name 'vtimes';
function vtimes( __current: pvtimes; var __child: _vtimes):longint;cdecl;external clib name 'vtimes';
