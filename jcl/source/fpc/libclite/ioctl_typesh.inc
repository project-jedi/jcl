
type
   Pwinsize = ^winsize;
   winsize = record
        ws_row : word;
        ws_col : word;
        ws_xpixel : word;
        ws_ypixel : word;
     end;


const
   NCC = 8;
type
   Ptermio = ^termio;
   termio = record
        c_iflag : word;
        c_oflag : word;
        c_cflag : word;
        c_lflag : word;
        c_line : byte;
        c_cc : array[0..(NCC)-1] of byte;
     end;


const
   TIOCM_LE = $001;
   TIOCM_DTR = $002;
   TIOCM_RTS = $004;
   TIOCM_ST = $008;
   TIOCM_SR = $010;
   TIOCM_CTS = $020;
   TIOCM_CAR = $040;
   TIOCM_RNG = $080;
   TIOCM_DSR = $100;
   TIOCM_CD = TIOCM_CAR;
   TIOCM_RI = TIOCM_RNG;
   N_TTY = 0;
   N_SLIP = 1;
   N_MOUSE = 2;
   N_PPP = 3;
   N_STRIP = 4;
   N_AX25 = 5;
   N_X25 = 6;
   N_6PACK = 7;
   N_MASC = 8;
   N_R3964 = 9;
   N_PROFIBUS_FDL = 10;
   N_IRDA = 11;
   N_SMSBLOCK = 12;
   N_HDLC = 13;
   N_SYNC_PPP = 14;
   N_HCI = 15;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TWinSize = winsize;

