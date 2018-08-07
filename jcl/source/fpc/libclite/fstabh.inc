
const
   _PATH_FSTAB = '/etc/fstab';
   _FSTAB = '/etc/fstab';
   FSTAB_RW = 'rw';
   FSTAB_RQ = 'rq';
   FSTAB_RO = 'ro';
   FSTAB_SW = 'sw';
   FSTAB_XX = 'xx';

type
   Pfstab = ^fstab;
   fstab = record
        fs_spec : Pchar;
        fs_file : Pchar;
        fs_vfstype : Pchar;
        fs_mntops : Pchar;
        fs_type : Pchar;
        fs_freq : longint;
        fs_passno : longint;
   end;


function getfsent:Pfstab;cdecl;external clib name 'getfsent';
function getfsspec(__name:Pchar):Pfstab;cdecl;external clib name 'getfsspec';
function getfsfile(__name:Pchar):Pfstab;cdecl;external clib name 'getfsfile';
function setfsent:longint;cdecl;external clib name 'setfsent';
procedure endfsent;cdecl;external clib name 'endfsent';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TFSTab = fstab;

