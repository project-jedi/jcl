
const
   __SID = ord('S') shl 8;
   I_NREAD = __SID or 1;
   I_PUSH = __SID or 2;
   I_POP = __SID or 3;
   I_LOOK = __SID or 4;
   I_FLUSH = __SID or 5;
   I_SRDOPT = __SID or 6;
   I_GRDOPT = __SID or 7;
   I_STR = __SID or 8;
   I_SETSIG = __SID or 9;
   I_GETSIG = __SID or 10;
   I_FIND = __SID or 11;
   I_LINK = __SID or 12;
   I_UNLINK = __SID or 13;
   I_PEEK = __SID or 15;
   I_FDINSERT = __SID or 16;
   I_SENDFD = __SID or 17;
   I_RECVFD = __SID or 14;
   I_SWROPT = __SID or 19;
   I_GWROPT = __SID or 20;
   I_LIST = __SID or 21;
   I_PLINK = __SID or 22;
   I_PUNLINK = __SID or 23;
   I_FLUSHBAND = __SID or 28;
   I_CKBAND = __SID or 29;
   I_GETBAND = __SID or 30;
   I_ATMARK = __SID or 31;
   I_SETCLTIME = __SID or 32;
   I_GETCLTIME = __SID or 33;
   I_CANPUT = __SID or 34;
   FMNAMESZ = 8;
   FLUSHR = $01;
   FLUSHW = $02;
   FLUSHRW = $03;

   FLUSHBAND = $04;

   S_INPUT = $0001;
   S_HIPRI = $0002;
   S_OUTPUT = $0004;
   S_MSG = $0008;
   S_ERROR = $0010;
   S_HANGUP = $0020;
   S_RDNORM = $0040;
   S_WRNORM = S_OUTPUT;
   S_RDBAND = $0080;
   S_WRBAND = $0100;
   S_BANDURG = $0200;
   RS_HIPRI = $01;
   RNORM = $0000;
   RMSGD = $0001;
   RMSGN = $0002;
   RPROTDAT = $0004;
   RPROTDIS = $0008;
   RPROTNORM = $0010;

   RPROTMASK = $001C;
   SNDZERO = $001;
   SNDPIPE = $002;

   ANYMARK = $01;
   LASTMARK = $02;

   MUXID_ALL = -(1);

   MSG_HIPRI = $01;
   MSG_ANY = $02;
   MSG_BAND = $04;
   MORECTL = 1;
   MOREDATA = 2;

type
   Pbandinfo = ^bandinfo;
   bandinfo = record
        bi_pri : byte;
        bi_flag : longint;
     end;

   Pstrbuf = ^strbuf;
   strbuf = record
        maxlen : longint;
        len : longint;
        buf : Pchar;
     end;

   t_uscalar_t = __t_uscalar_t; // moved here from stropts.h

   Pstrpeek = ^strpeek;
   strpeek = record
        ctlbuf : strbuf;
        databuf : strbuf;
        flags : t_uscalar_t;
     end;

   Pstrfdinsert = ^strfdinsert;
   strfdinsert = record
        ctlbuf : strbuf;
        databuf : strbuf;
        flags : t_uscalar_t;
        fildes : longint;
        offset : longint;
     end;

   Pstrioctl = ^strioctl;
   strioctl = record
        ic_cmd : longint;
        ic_timout : longint;
        ic_len : longint;
        ic_dp : Pchar;
     end;

   Pstrrecvfd = ^strrecvfd;
   strrecvfd = record
        fd : longint;
        uid : uid_t;
        gid : gid_t;
        __fill : array[0..7] of char;
     end;

   Pstr_mlist = ^str_mlist;
   str_mlist = record
        l_name : array[0..(FMNAMESZ + 1)-1] of char;
     end;

   Pstr_list = ^str_list;
   str_list = record
        sl_nmods : longint;
        sl_modlist : Pstr_mlist;
     end;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  Tstrbuf = strbuf;


