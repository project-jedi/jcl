{ ---------------------------------------------------------------------
    'h2pas -S -c -D -l libc -i' from bits/types.h sys/types.h
  ---------------------------------------------------------------------}

type
   __u_char = byte;
   __u_short = word;
   __u_int = dword;
   __u_long = dword;
   __u_quad_t = qword;
   __quad_t = int64;

   __int8_t = char;
   __uint8_t = byte;
   __int16_t = smallint;
   __uint16_t = word;
   __int32_t = longint;
   __uint32_t = dword;
   __int64_t = Int64;
   __uint64_t = Qword;

   __qaddr_t = __quad_t;
   __dev_t = __u_quad_t;
   __uid_t = __u_int;
   __gid_t = __u_int;
   __ino_t = __u_long;
   __mode_t = __u_int;
   __nlink_t = __u_int;
   __off_t = longint;
   __loff_t = __quad_t;
   __pid_t = longint;
   __ssize_t = longint;
   __rlim_t = __u_long;
   __rlim64_t = __u_quad_t;
   __id_t = __u_int;
   __fsid_t = record
        __val : array[0..1] of longint;
     end;
   __daddr_t = longint;
   __caddr_t = char;
   __time_t = longint;
   __useconds_t = dword;
   __suseconds_t = longint;
   __swblk_t = longint;
   __clock_t = longint;
   __clockid_t = longint;
   __timer_t = longint;
   __fd_mask = dWord;

const
  __FD_SETSIZE = 1024;
  __NFDBITS       = 8 * sizeof(__fd_mask);

type
  __fd_set = record
     fds_bits: packed array[0..(__FD_SETSIZE div __NFDBITS)-1] of __fd_mask;
  end;
  TFdSet = __fd_set;
  PFdSet = ^TFdSet;

 
type
  __key_t = longint;
  __ipc_pid_t = word;
  __blksize_t = longint;
  __blkcnt_t = longint;
  __blkcnt64_t = __quad_t;
  __fsblkcnt_t = __u_long;
  __fsblkcnt64_t = __u_quad_t;
  __fsfilcnt_t = __u_long;
  __fsfilcnt64_t = __u_quad_t;
  __ino64_t = __u_quad_t;
  __off64_t = __loff_t;
  __t_scalar_t = longint;
  __t_uscalar_t = dword;
  __intptr_t = longint;
  __socklen_t = dword;
  TFileDescriptor = integer;

Const
  stdin   = 0;
  stdout  = 1;
  stderr  = 2;
           
  
{ ---------------------------------------------------------------------
    'h2pas -S -c -D -l libc -i' from sys/types.h
  ---------------------------------------------------------------------}

Type

  u_char = __u_char;
  u_short = __u_short;
  u_int = __u_int;
  u_long = __u_long;
  quad_t = __quad_t;
  u_quad_t = __u_quad_t;
  fsid_t = __fsid_t;
  loff_t = __loff_t;
  ino_t = __ino_t;
  ino64_t = __ino64_t;
  dev_t = __dev_t;
  gid_t = __gid_t;
  mode_t = __mode_t;
  nlink_t = __nlink_t;
  uid_t = __uid_t;
  off_t = __off_t;
  off64_t = __off64_t;
  pid_t = __pid_t;
  id_t = __id_t;
  ssize_t = __ssize_t;
  daddr_t = __daddr_t;
  caddr_t = __caddr_t;
  key_t = __key_t;
  useconds_t = __useconds_t;
  suseconds_t = __suseconds_t;
  ulong = dword;
  ushort = word;
  uint = dword;
  int8_t = char;
  int16_t = smallint;
  int32_t = longint;
  u_int8_t = byte;
  u_int16_t = word;
  u_int32_t = dword;
  register_t = longint;
  blksize_t = __blksize_t;
  blkcnt_t = __blkcnt_t;
  fsblkcnt_t = __fsblkcnt_t;
  fsfilcnt_t = __fsfilcnt_t;
  blkcnt64_t = __blkcnt64_t;
  fsblkcnt64_t = __fsblkcnt64_t;
  fsfilcnt64_t = __fsfilcnt64_t;

  P__key_t = ^__key_t;
  P__ipc_pid_t = ^__ipc_pid_t;
  P__blksize_t = ^__blksize_t;
  P__blkcnt_t = ^__blkcnt_t;
  P__blkcnt64_t = ^__blkcnt64_t;
  P__fsblkcnt_t = ^__fsblkcnt_t;
  P__fsblkcnt64_t = ^__fsblkcnt64_t;
  P__fsfilcnt_t = ^__fsfilcnt_t;
  P__fsfilcnt64_t = ^__fsfilcnt64_t;
  P__ino64_t = ^__ino64_t;
  P__off64_t = ^__off64_t;
  P__t_scalar_t = ^__t_scalar_t;
  P__t_uscalar_t = ^__t_uscalar_t;
  P__intptr_t = ^__intptr_t;
  P__socklen_t = ^__socklen_t;


  Pu_char = ^u_char;
  Pu_short = ^u_short;
  Pu_int = ^u_int;
  Pu_long = ^u_long;
  Pquad_t = ^quad_t;
  Pu_quad_t = ^u_quad_t;
  Pfsid_t = ^fsid_t;
  Ploff_t = ^loff_t;
  Pino_t = ^ino_t;
  Pino64_t = ^ino64_t;
  Pdev_t = ^dev_t;
  Pgid_t = ^gid_t;
  Pmode_t = ^mode_t;
  Pnlink_t = ^nlink_t;
  Puid_t = ^uid_t;
  Poff_t = ^off_t;
  Poff64_t = ^off64_t;
  Ppid_t = ^pid_t;
  Pssize_t = ^ssize_t;
  Pdaddr_t = ^daddr_t;
  Pcaddr_t = ^caddr_t;
  Pkey_t = ^key_t;
  Puseconds_t = ^useconds_t;
  Psuseconds_t = ^suseconds_t;
  Pulong = ^ulong;
  Pushort = ^ushort;
  Puint = ^uint;
  Pint8_t = ^int8_t;
  Pint16_t = ^int16_t;
  Pint32_t = ^int32_t;
  Pu_int8_t = ^u_int8_t;
  Pu_int16_t = ^u_int16_t;
  Pu_int32_t = ^u_int32_t;
  Pregister_t = ^register_t;
  Pblksize_t = ^blksize_t;
  Pblkcnt_t = ^blkcnt_t;
  Pfsblkcnt_t = ^fsblkcnt_t;
  Pfsfilcnt_t = ^fsfilcnt_t;
  Pblkcnt64_t = ^blkcnt64_t;
  Pfsblkcnt64_t = ^fsblkcnt64_t;
  Pfsfilcnt64_t = ^fsfilcnt64_t;

  P__qaddr_t = ^__qaddr_t;
  P__dev_t = ^__dev_t;
  P__uid_t = ^__uid_t;
  P__gid_t = ^__gid_t;
  P__ino_t = ^__ino_t;
  P__mode_t = ^__mode_t;
  P__nlink_t = ^__nlink_t;
  P__off_t = ^__off_t;
  P__loff_t = ^__loff_t;
  P__pid_t = ^__pid_t;
  P__ssize_t = ^__ssize_t;
  P__rlim_t = ^__rlim_t;
  P__rlim64_t = ^__rlim64_t;
  P__id_t = ^__id_t;
  P__fsid_t = ^__fsid_t;
  P__daddr_t = ^__daddr_t;
  P__caddr_t = ^__caddr_t;
  P__time_t = ^__time_t;
  P__useconds_t = ^__useconds_t;
  P__suseconds_t = ^__suseconds_t;
  P__swblk_t = ^__swblk_t;
  P__clock_t = ^__clock_t;
  P__clockid_t = ^__clockid_t;
  P__timer_t = ^__timer_t;


{ Macros }
Function __FDELT(d: longint): Integer;
Function __FDMASK(d: longint): __fd_mask;
 
{ Borland compatibility }
Type

  TFsID = __fsid_t;
  PFsID = ^TFsID;
 
