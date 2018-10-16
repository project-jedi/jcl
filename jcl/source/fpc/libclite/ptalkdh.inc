

const
   NAME_SIZE = 12;
   TTY_SIZE = 16;
type

   PCTL_MSG = ^CTL_MSG;
   CTL_MSG = record
        vers : u_char;
        _type : u_char;
        answer : u_char;
        pad : u_char;
        id_num : u_int32_t;
        addr : osockaddr;
        ctl_addr : osockaddr;
        pid : int32_t;
        l_name : array[0..(NAME_SIZE)-1] of char;
        r_name : array[0..(NAME_SIZE)-1] of char;
        r_tty : array[0..(TTY_SIZE)-1] of char;
     end;

   PCTL_RESPONSE = ^CTL_RESPONSE;
   CTL_RESPONSE = record
        vers : u_char;
        _type : u_char;
        answer : u_char;
        pad : u_char;
        id_num : u_int32_t;
        addr : osockaddr;
     end;

const
   TALK_VERSION = 1;
   LEAVE_INVITE = 0;
   LOOK_UP = 1;
   __DELETE = 2;
   ANNOUNCE = 3;
   SUCCESS = 0;
   NOT_HERE = 1;
   FAILED = 2;
   MACHINE_UNKNOWN = 3;
   PERMISSION_DENIED = 4;
   UNKNOWN_REQUEST = 5;
   BADVERSION = 6;
   BADADDR = 7;
   BADCTLADDR = 8;
   MAX_LIFE = 60;
   RING_WAIT = 30;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
