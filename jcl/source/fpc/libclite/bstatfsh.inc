
type
   Pstatfs = ^_statfs;
   _statfs = record
        f_type : longint;
        f_bsize : longint;
        f_blocks : __fsblkcnt_t;
        f_bfree : __fsblkcnt_t;
        f_bavail : __fsblkcnt_t;
        f_files : __fsfilcnt_t;
        f_ffree : __fsfilcnt_t;
        f_fsid : __fsid_t;
        f_namelen : longint;
        f_spare : array[0..5] of longint;
     end;
   P_statfs = ^_statfs;

   Pstatfs64 = ^_statfs64;
   _statfs64 = record
        f_type : longint;
        f_bsize : longint;
        f_blocks : __fsblkcnt64_t;
        f_bfree : __fsblkcnt64_t;
        f_bavail : __fsblkcnt64_t;
        f_files : __fsfilcnt64_t;
        f_ffree : __fsfilcnt64_t;
        f_fsid : __fsid_t;
        f_namelen : longint;
        f_spare : array[0..5] of longint;
     end;
   P_statfs64 = ^_statfs64;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TStatFs = _statfs;
  TStatFs64 = _statfs64;


