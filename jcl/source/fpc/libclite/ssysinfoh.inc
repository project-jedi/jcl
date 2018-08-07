
Const
  SI_LOAD_SHIFT   = 16;

type
  PSysInfo = ^_sysInfo;
  _sysinfo = {packed} record // Renamed due to identifier conflict with sysinfo function
    uptime: Integer;                    { Seconds since boot }
    loads: packed array[0..2] of LongWord;{ 1, 5, and 15 minute load averages }
    totalram: LongWord;                 { Total usable main memory size }
    freeram: LongWord;                  { Available memory size }
    sharedram: LongWord;                { Amount of shared memory }
    bufferram: LongWord;                { Memory used by buffers }
    totalswap: LongWord;                { Total swap space size }
    freeswap: LongWord;                 { swap space still available }
    procs: Word;                        { Number of current processes }
    _f: packed array[0..21] of Char;    { Pads structure to 64 bytes }
  end;
  P_sysInfo = ^_sysInfo;


function sysinfo(__info:Psysinfo):longint;cdecl;external clib name 'sysinfo';
function get_nprocs_conf:longint;cdecl;external clib name 'get_nprocs_conf';
function get_nprocs:longint;cdecl;external clib name 'get_nprocs';
function get_phys_pages:longint;cdecl;external clib name 'get_phys_pages';
function get_avphys_pages:longint;cdecl;external clib name 'get_avphys_pages';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TSysInfo = _sysinfo;
