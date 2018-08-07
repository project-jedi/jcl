
type
   Psg_iovec = ^sg_iovec;
   sg_iovec = record
        iov_base : pointer;
        iov_len : size_t;
     end;
   sg_iovec_t = sg_iovec;
   Psg_iovec_t = ^sg_iovec_t;

   Psg_io_hdr = ^sg_io_hdr;
   sg_io_hdr = record
        interface_id : longint;
        dxfer_direction : longint;
        cmd_len : byte;
        mx_sb_len : byte;
        iovec_count : word;
        dxfer_len : dword;
        dxferp : pointer;
        cmdp : Pbyte;
        sbp : Pbyte;
        timeout : dword;
        flags : dword;
        pack_id : longint;
        usr_ptr : pointer;
        status : byte;
        masked_status : byte;
        msg_status : byte;
        sb_len_wr : byte;
        host_status : word;
        driver_status : word;
        resid : longint;
        duration : dword;
        info : dword;
     end;
   sg_io_hdr_t = sg_io_hdr;
   Psg_io_hdr_t = ^sg_io_hdr_t;

const
   SG_DXFER_NONE = -(1);
   SG_DXFER_TO_DEV = -(2);
   SG_DXFER_FROM_DEV = -(3);
   SG_DXFER_TO_FROM_DEV = -(4);
   SG_FLAG_DIRECT_IO = 1;
   SG_FLAG_LUN_INHIBIT = 2;
   SG_FLAG_NO_DXFER = $10000;
   SG_INFO_OK_MASK = $1;
   SG_INFO_OK = $0;
   SG_INFO_CHECK = $1;
   SG_INFO_DIRECT_IO_MASK = $6;
   SG_INFO_INDIRECT_IO = $0;
   SG_INFO_DIRECT_IO = $2;
   SG_INFO_MIXED_IO = $4;
type
   Psg_scsi_id = ^sg_scsi_id;
   sg_scsi_id = record
        host_no : longint;
        channel : longint;
        scsi_id : longint;
        lun : longint;
        scsi_type : longint;
        h_cmd_per_lun : smallint;
        d_queue_depth : smallint;
        unused : array[0..1] of longint;
     end;


   Psg_req_info = ^sg_req_info;
   sg_req_info = record
        req_state : char;
        orphan : char;
        sg_io_owned : char;
        problem : char;
        pack_id : longint;
        usr_ptr : pointer;
        duration : dword;
        unused : longint;
     end;
   sg_req_info_t = sg_req_info;
   Psg_req_info_t = ^sg_req_info_t;

const
   SG_EMULATED_HOST = $2203;
   SG_SET_TRANSFORM = $2204;
   SG_GET_TRANSFORM = $2205;
   SG_SET_RESERVED_SIZE = $2275;
   SG_GET_RESERVED_SIZE = $2272;
   SG_GET_SCSI_ID = $2276;
   SG_SET_FORCE_LOW_DMA = $2279;
   SG_GET_LOW_DMA = $227a;
   SG_SET_FORCE_PACK_ID = $227b;
   SG_GET_PACK_ID = $227c;
   SG_GET_NUM_WAITING = $227d;
   SG_GET_SG_TABLESIZE = $227F;
   SG_GET_VERSION_NUM = $2282;
   SG_SCSI_RESET = $2284;
   SG_SCSI_RESET_NOTHING = 0;
   SG_SCSI_RESET_DEVICE = 1;
   SG_SCSI_RESET_BUS = 2;
   SG_SCSI_RESET_HOST = 3;
   SG_IO = $2285;
   SG_GET_REQUEST_TABLE = $2286;
   SG_SET_KEEP_ORPHAN = $2287;
   SG_GET_KEEP_ORPHAN = $2288;
   SG_SCATTER_SZ = 8 * 4096;
   SG_DEFAULT_RETRIES = 1;
   SG_DEF_FORCE_LOW_DMA = 0;
   SG_DEF_FORCE_PACK_ID = 0;
   SG_DEF_KEEP_ORPHAN = 0;
   SG_DEF_RESERVED_SIZE = SG_SCATTER_SZ;
   SG_MAX_QUEUE = 16;
   SG_BIG_BUFF = SG_DEF_RESERVED_SIZE;

const
   SG_MAX_SENSE = 16;
type
   Psg_header = ^sg_header;
   sg_header = record
        pack_len : longint;
        reply_len : longint;
        pack_id : longint;
        result : longint;
        flag0 : cardinal;
        sense_buffer : array[0..(SG_MAX_SENSE)-1] of byte;
     end;

const
   SG_SET_TIMEOUT = $2201;
   SG_GET_TIMEOUT = $2202;
   SG_GET_COMMAND_Q = $2270;
   SG_SET_COMMAND_Q = $2271;
   SG_SET_DEBUG = $227e;
   SG_NEXT_CMD_LEN = $2283;
   SG_DEFAULT_TIMEOUT = 60 * HZ;
   SG_DEF_COMMAND_Q = 0;
   SG_DEF_UNDERRUN_FLAG = 0;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
