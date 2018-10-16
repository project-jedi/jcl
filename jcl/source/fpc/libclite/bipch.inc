
const
   IPC_CREAT = 01000;
   IPC_EXCL = 02000;
   IPC_NOWAIT = 04000;
   IPC_RMID = 0;
   IPC_SET = 1;
   IPC_STAT = 2;

   IPC_INFO = 3;

   IPC_PRIVATE = __key_t(0);

type
   Pipc_perm = ^ipc_perm;
   ipc_perm = record
        __key : __key_t;
        uid : __uid_t;
        gid : __gid_t;
        cuid : __uid_t;
        cgid : __gid_t;
        mode : word;
        __pad1 : word;
        __seq : word;
        __pad2 : word;
        __unused1 : dword;
        __unused2 : dword;
     end;



{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TIpcPermission = ipc_perm;
  PIpcPermission = ^TIpcPermission;


