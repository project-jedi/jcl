
type
   Pdirent = ^dirent;
   dirent = record
        d_ino : __ino_t;
        d_off : __off_t;
        d_reclen : word;
        d_type : byte;
        d_name : array[0..255] of char;
     end;

type
   Pdirent64 = ^dirent64;
   dirent64 = record
        d_ino : __ino64_t;
        d_off : __off64_t;
        d_reclen : word;
        d_type : byte;
        d_name : array[0..255] of char;
     end;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TDirEnt = dirent;
  PPDirEnt = ^PDirEnt;
  PPPDirEnt = ^PPDirEnt;

  TDirEnt64 = dirent64;
  PPDirEnt64 = ^PDirEnt64;
  PPPDirEnt64 = ^PPDirEnt64;



