
const
   PROT_READ = $1;
   PROT_WRITE = $2;
   PROT_EXEC = $4;
   PROT_NONE = $0;
   MAP_SHARED = $01;
   MAP_PRIVATE = $02;

   MAP_TYPE = $0f;

   MAP_FIXED = $10;

   MAP_FILE = 0;
   MAP_ANONYMOUS = $20;
   MAP_ANON = MAP_ANONYMOUS;

   MAP_GROWSDOWN = $0100;
   MAP_DENYWRITE = $0800;
   MAP_EXECUTABLE = $1000;
   MAP_LOCKED = $2000;
   MAP_NORESERVE = $4000;

   MS_ASYNC = 1;
   MS_SYNC = 4;
   MS_INVALIDATE = 2;
   MCL_CURRENT = 1;
   MCL_FUTURE = 2;

   MREMAP_MAYMOVE = 1;

   MADV_NORMAL = 0;
   MADV_RANDOM = 1;
   MADV_SEQUENTIAL = 2;
   MADV_WILLNEED = 3;
   MADV_DONTNEED = 4;

const
   POSIX_MADV_NORMAL = 0;
   POSIX_MADV_RANDOM = 1;
   POSIX_MADV_SEQUENTIAL = 2;
   POSIX_MADV_WILLNEED = 3;
   POSIX_MADV_DONTNEED = 4;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
