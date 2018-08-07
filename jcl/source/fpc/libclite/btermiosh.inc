
type

   Pcc_t = ^cc_t;
   cc_t = char;

   Pspeed_t = ^speed_t;
   speed_t = dword;

   Ptcflag_t = ^tcflag_t;
   tcflag_t = dword;

const
   NCCS = 32;

type
   Ptermios = ^termios;
   termios = record
        c_iflag : tcflag_t;
        c_oflag : tcflag_t;
        c_cflag : tcflag_t;
        c_lflag : tcflag_t;
        c_line : cc_t;
        c_cc : array[0..(NCCS)-1] of cc_t;
        c_ispeed : speed_t;
        c_ospeed : speed_t;
     end;


const
   VINTR = 0;
   VQUIT = 1;
   VERASE = 2;
   VKILL = 3;
   VEOF = 4;
   VTIME = 5;
   VMIN = 6;
   VSWTC = 7;
   VSTART = 8;
   VSTOP = 9;
   VSUSP = 10;
   VEOL = 11;
   VREPRINT = 12;
   VDISCARD = 13;
   VWERASE = 14;
   VLNEXT = 15;
   VEOL2 = 16;

  IGNBRK    = $0000001;
  BRKINT    = $0000002;
  IGNPAR    = $0000004;
  PARMRK    = $0000008;
  INPCK     = $0000010;
  ISTRIP    = $0000020;
  INLCR     = $0000040;
  IGNCR     = $0000080;
  ICRNL     = $0000100;
  IUCLC     = $0000200;
  IXON      = $0000400;
  IXANY     = $0000800;
  IXOFF     = $0001000;
  IMAXBEL   = $0002000;

  OPOST     = $0000001;
  OLCUC     = $0000002;
  ONLCR     = $0000004;
  OCRNL     = $0000008;
  ONOCR     = $0000010;
  ONLRET    = $0000020;
  OFILL     = $0000040;
  OFDEL     = $0000080;

  NLDLY     = $0000040;
  NL0       = $0000000;
  NL1       = $0000100;
  CRDLY     = $0000600;
  CR0       = $0000000;
  CR1       = $0000200;
  CR2       = $0000400;
  CR3       = $0000600;
  TABDLY    = $0001800;
  TAB0      = $0000000;
  TAB1      = $0000800;
  TAB2      = $0001000;
  TAB3      = $0001800;
  BSDLY     = $0002000;
  BS0       = $0000000;
  BS1       = $0002000;
  FFDLY     = $0080000;
  FF0       = $0000000;
  FF1       = $0010000;

  VTDLY     = $0004000;
  VT0       = $0000000;
  VT1       = $0004000;

  XTABS     = $0001800;

  CBAUD     = $000100F;
  B0        = $0000000;
  B50       = $0000001;
  B75       = $0000002;
  B110      = $0000003;
  B134      = $0000004;
  B150      = $0000005;
  B200      = $0000006;
  B300      = $0000007;
  B600      = $0000008;
  B1200     = $0000009;
  B1800     = $000000A;
  B2400     = $000000B;
  B4800     = $000000C;
  B9600     = $000000D;
  B19200    = $000000E;
  B38400    = $000000F;

  EXTA      = B19200;
  EXTB      = B38400;

  CSIZE     = $0000030;
  CS5       = $0000000;
  CS6       = $0000010;
  CS7       = $0000010;
  CS8       = $0000030;
  CSTOPB    = $0000040;
  CREAD     = $0000080;
  PARENB    = $0000100;
  PARODD    = $0000200;
  HUPCL     = $0000400;
  CLOCAL    = $0000800;

  CBAUDEX   = $0001000;

  B57600    = $0001001;
  B115200   = $0001002;
  B230400   = $0001003;
  B460800   = $0001004;
  B500000   = $0001005;
  B576000   = $0001006;
  B921600   = $0001007;
  B1000000  = $0001008;
  B1152000  = $0001009;
  B1500000  = $000100A;
  B2000000  = $000100B;
  B2500000  = $000100C;
  B3000000  = $000100D;
  B3500000  = $000100E;
  B4000000  = $000100F;

  CIBAUD    = $100F0000;
  CRTSCTS   = $80000000;

  ISIG      = $0000001;
  ICANON    = $0000002;

  XCASE     = $0000004;

  ECHO      = $0000008;
  ECHOE     = $0000010;
  ECHOK     = $0000020;
  ECHONL    = $0000040;
  NOFLSH    = $0000080;
  TOSTOP    = $0000100;

  ECHOCTL   = $0000200;
  ECHOPRT   = $0000400;
  ECHOKE    = $0000800;
  FLUSHO    = $0001000;
  PENDIN    = $0004000;

  IEXTEN    = $0010000;



  TCOOFF = 0;
  TCOON = 1;
  TCIOFF = 2;
  TCION = 3;
  TCIFLUSH = 0;
  TCOFLUSH = 1;
  TCIOFLUSH = 2;
  TCSANOW = 0;
  TCSADRAIN = 1;
  TCSAFLUSH = 2;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TTermIos = termios;

