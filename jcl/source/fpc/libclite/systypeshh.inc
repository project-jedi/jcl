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
  ino_t = __ino64_t;
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
  blkcnt_t = __blkcnt64_t;
  fsblkcnt_t = __fsblkcnt64_t;
  fsfilcnt_t = __fsfilcnt64_t;
  blkcnt64_t = __blkcnt64_t;
  fsblkcnt64_t = __fsblkcnt64_t;
  fsfilcnt64_t = __fsfilcnt64_t;
  u_int64_t = QWord;
    