

const
  BLOCK_SIZE = 1024;
  BLOCK_SIZE_BITS = 10;

  MS_RDONLY = 1;
  MS_NOSUID = 2;
  MS_NODEV = 4;
  MS_NOEXEC = 8;
  MS_SYNCHRONOUS = 16;
  MS_REMOUNT = 32;
  MS_MANDLOCK = 64;
  S_WRITE = 128;
  S_APPEND = 256;
  S_IMMUTABLE = 512;
  MS_NOATIME = 1024;
  MS_NODIRATIME = 2048;
  MS_BIND = 4096;

  MS_RMT_MASK = MS_RDONLY or MS_MANDLOCK;
  MS_MGC_VAL = $c0ed0000;
  MS_MGC_MSK = $ffff0000;

  MNT_FORCE = 1;

function mount(__special_file:Pchar; __dir:Pchar; __fstype:Pchar; __rwflag:dword; __data:pointer):longint;cdecl;external clib name 'mount';
function umount(__special_file:Pchar):longint;cdecl;external clib name 'umount';
function umount2(__special_file:Pchar; __flags:longint):longint;cdecl;external clib name 'umount2';

