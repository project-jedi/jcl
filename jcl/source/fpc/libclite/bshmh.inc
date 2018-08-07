
const
   SHM_R = 0400;
   SHM_W = 0200;
   SHM_RDONLY = 010000;
   SHM_RND = 020000;
   SHM_REMAP = 040000;
   SHM_LOCK = 11;
   SHM_UNLOCK = 12;

type
   Pshmatt_t = ^shmatt_t;
   shmatt_t = dword;

   Pshmid_ds = ^shmid_ds;
   shmid_ds = record
        shm_perm : ipc_perm;
        shm_segsz : size_t;
        shm_atime : __time_t;
        __unused1 : dword;
        shm_dtime : __time_t;
        __unused2 : dword;
        shm_ctime : __time_t;
        __unused3 : dword;
        shm_cpid : __pid_t;
        shm_lpid : __pid_t;
        shm_nattch : shmatt_t;
        __unused4 : dword;
        __unused5 : dword;
     end;

const
   SHM_STAT = 13;
   SHM_INFO = 14;
   SHM_DEST = 01000;
   SHM_LOCKED = 02000;

type
   Pshminfo = ^shminfo;
   shminfo = record
        shmmax : dword;
        shmmin : dword;
        shmmni : dword;
        shmseg : dword;
        shmall : dword;
        __unused1 : dword;
        __unused2 : dword;
        __unused3 : dword;
        __unused4 : dword;
     end;

   Pshm_info = ^_shm_info;
   _shm_info = record
        used_ids : longint;
        shm_tot : dword;
        shm_rss : dword;
        shm_swp : dword;
        swap_attempts : dword;
        swap_successes : dword;
     end;
   P_shm_info = ^_shm_info;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TSharedMemIdDescriptor = shmid_ds;
  PSharedMemIdDescriptor = ^TSharedMemIdDescriptor;

  TSharedMemInfo = shminfo;
  PSharedMemInfo = ^TSharedMemInfo;

  TTotalSharedMemInfo = _shm_info;
  PTotalSharedMemInfo = ^TTotalSharedMemInfo;
