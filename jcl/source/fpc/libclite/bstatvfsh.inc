
type
  Pstatvfs = ^_statvfs;
  _statvfs = record
       f_bsize : dword;
       f_frsize : dword;
       f_blocks : __fsblkcnt_t;
       f_bfree : __fsblkcnt_t;
       f_bavail : __fsblkcnt_t;
       f_files : __fsfilcnt_t;
       f_ffree : __fsfilcnt_t;
       f_favail : __fsfilcnt_t;
       f_fsid : dword;
       __f_unused : longint;
       f_flag : dword;
       f_namemax : dword;
       __f_spare : array[0..5] of longint;
    end;
  P_statvfs = ^_statvfs;

  Pstatvfs64 = ^_statvfs64;
  _statvfs64 = record
       f_bsize : dword;
       f_frsize : dword;
       f_blocks : __fsblkcnt64_t;
       f_bfree : __fsblkcnt64_t;
       f_bavail : __fsblkcnt64_t;
       f_files : __fsfilcnt64_t;
       f_ffree : __fsfilcnt64_t;
       f_favail : __fsfilcnt64_t;
       f_fsid : dword;
       __f_unused : longint;
       f_flag : dword;
       f_namemax : dword;
       __f_spare : array[0..5] of longint;
    end;
  P_statvfs64 = ^_statvfs64;

Const
  ST_RDONLY = 1;
  ST_NOSUID = 2;
  ST_NODEV = 4;
  ST_NOEXEC = 8;
  ST_SYNCHRONOUS = 16;
  ST_MANDLOCK = 64;
  ST_WRITE = 128;
  ST_APPEND = 256;
  ST_IMMUTABLE = 512;
  ST_NOATIME = 1024;
  ST_NODIRATIME = 1025;



{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
 TStatVFs = _statvfs;
  TStatVFs64 = _statvfs64;

