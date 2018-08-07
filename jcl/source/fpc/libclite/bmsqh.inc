
const
   MSG_NOERROR = $1000;
   MSG_EXCEPT = $2000;

type
   Pmsgqnum_t = ^msgqnum_t;
   msgqnum_t = dword;

   Pmsglen_t = ^msglen_t;
   msglen_t = dword;
   Pmsqid_ds = ^msqid_ds;
   msqid_ds = record
        msg_perm : ipc_perm;
        msg_stime : __time_t;
        __unused1 : dword;
        msg_rtime : __time_t;
        __unused2 : dword;
        msg_ctime : __time_t;
        __unused3 : dword;
        __msg_cbytes : dword;
        msg_qnum : msgqnum_t;
        msg_qbytes : msglen_t;
        msg_lspid : __pid_t;
        msg_lrpid : __pid_t;
        __unused4 : dword;
        __unused5 : dword;
     end;


const
   MSG_STAT = 11;
   MSG_INFO = 12;

type
   Pmsginfo = ^msginfo;
   msginfo = record
        msgpool : longint;
        msgmap : longint;
        msgmax : longint;
        msgmnb : longint;
        msgmni : longint;
        msgssz : longint;
        msgtql : longint;
        msgseg : word;
     end;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TMsgQueueIdDesc = msqid_ds;
  PMsgQueueIdDesc = ^TMsgQueueIdDesc;
