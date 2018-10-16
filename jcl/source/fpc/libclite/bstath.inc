
const
   _STAT_VER_LINUX_OLD = 1;
   _STAT_VER_KERNEL = 1;
   _STAT_VER_SVR4 = 2;
   _STAT_VER_LINUX = 3;
   _STAT_VER = _STAT_VER_LINUX;

   _MKNOD_VER_LINUX = 1;
   _MKNOD_VER_SVR4 = 2;
   _MKNOD_VER = _MKNOD_VER_LINUX;

type
   P_stat = ^_stat;
   PStat = ^_stat;
   _stat = packed record
        st_dev : __dev_t;
        __pad1 : word;
        __align_pad1 : word;
        st_ino : __ino_t;
        st_mode : __mode_t;
        st_nlink : __nlink_t;
        st_uid : __uid_t;
        st_gid : __gid_t;
        st_rdev : __dev_t;
        __pad2 : word;
        __align_pad2 : word;
        st_size : __off_t;
        st_blksize : __blksize_t;
        st_blocks : __blkcnt_t;
        st_atime : __time_t;
        __unused1 : dword;
        st_mtime : __time_t;
        __unused2 : dword;
        st_ctime : __time_t;
        __unused3 : dword;
        __unused4 : dword;
        __unused5 : dword;
     end;

   P_stat64 = ^_stat64;
   Pstat64 = ^_stat64;
   _stat64 = record
        st_dev : __dev_t;
        __pad1 : dword;
        __st_ino : __ino_t;
        st_mode : __mode_t;
        st_nlink : __nlink_t;
        st_uid : __uid_t;
        st_gid : __gid_t;
        st_rdev : __dev_t;
        __pad2 : dword;
        st_size : __off64_t;
        st_blksize : __blksize_t;
        st_blocks : __blkcnt64_t;
        st_atime : __time_t;
        __unused1 : dword;
        st_mtime : __time_t;
        __unused2 : dword;
        st_ctime : __time_t;
        __unused3 : dword;
        st_ino : __ino64_t;
     end;

const
  __S_IFMT        = $F000;
  __S_IFDIR       = $4000;
  __S_IFCHR       = $2000;
  __S_IFBLK       = $6000;
  __S_IFREG       = $8000;
  __S_IFIFO       = $1000;
  __S_IFLNK       = $A000;
  __S_IFSOCK      = $C000;

  __S_ISUID       = $800;
  __S_ISGID       = $400;
  __S_ISVTX       = $200;
  __S_IREAD       = $100;
  __S_IWRITE      = $80;
  __S_IEXEC       = $40;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TStatBuf = _stat;
  PStatBuf = ^TStatBuf;

  TStatBuf64 = _stat64;
  PStatBuf64 = ^TStatBuf64;


