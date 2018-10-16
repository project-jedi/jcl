
const
   O_ACCMODE  = &00003;
   O_RDONLY   = &00000;
   O_WRONLY   = &00001;
   O_RDWR     = &00002;
   O_CREAT    = &00100;
   O_EXCL     = &00200;
   O_NOCTTY   = &00400;
   O_TRUNC    = &01000;
   O_APPEND   = &02000;
   O_NONBLOCK = &04000;
   O_NDELAY   = O_NONBLOCK;
   O_SYNC     = &010000;
   O_FSYNC    = O_SYNC;
   O_ASYNC    = &020000;

   O_DIRECT    = &0040000;
   O_DIRECTORY = &0200000;
   O_NOFOLLOW  = &0400000;

   O_DSYNC = O_SYNC;
   O_RSYNC = O_SYNC;

   O_LARGEFILE = &0100000;

   F_DUPFD   = 0;
   F_GETFD   = 1;
   F_SETFD   = 2;
   F_GETFL   = 3;
   F_SETFL   = 4;

   F_GETLK   = 5;
   F_SETLK   = 6;
   F_SETLKW  = 7;

   F_GETLK64  = 12;
   F_SETLK64  = 13;
   F_SETLKW64 = 14;

   F_SETOWN = 8;
   F_GETOWN = 9;

   F_SETSIG = 10;
   F_GETSIG = 11;

   F_SETLEASE = 1024;
   F_GETLEASE = 1025;
   F_NOTIFY = 1026;

   FD_CLOEXEC = 1;
   F_RDLCK = 0;
   F_WRLCK = 1;
   F_UNLCK = 2;
   F_EXLCK = 4;
   F_SHLCK = 8;

   LOCK_SH = 1;
   LOCK_EX = 2;
   LOCK_NB = 4;
   LOCK_UN = 8;

   LOCK_MAND = 32;
   LOCK_READ = 64;
   LOCK_WRITE = 128;
   LOCK_RW = 192;

   DN_ACCESS = $00000001;
   DN_MODIFY = $00000002;
   DN_CREATE = $00000004;
   DN_DELETE = $00000008;
   DN_RENAME = $00000010;
   DN_ATTRIB = $00000020;
   DN_MULTISHOT = $80000000;

type
   Pflock = ^flock;
   flock = record
        l_type : smallint;
        l_whence : smallint;
        l_start : __off_t;
        l_len : __off_t;
        l_pid : __pid_t;
     end;

   Pflock64 = ^flock64;
   flock64 = record
        l_type : smallint;
        l_whence : smallint;
        l_start : __off64_t;
        l_len : __off64_t;
        l_pid : __pid_t;
     end;

const
   FAPPEND = O_APPEND;
   FFSYNC = O_FSYNC;
   FASYNC = O_ASYNC;
   FNONBLOCK = O_NONBLOCK;
   FNDELAY = O_NDELAY;

   POSIX_FADV_NORMAL = 0;
   POSIX_FADV_RANDOM = 1;
   POSIX_FADV_SEQUENTIAL = 2;
   POSIX_FADV_WILLNEED = 3;
   POSIX_FADV_DONTNEED = 4;
   POSIX_FADV_NOREUSE = 5;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TFlock = flock;

  TFlock64 = Flock64;


