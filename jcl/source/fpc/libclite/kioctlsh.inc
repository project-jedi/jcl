
{$ifndef __ARCH_I386_IOCTLS_H__}
{$define __ARCH_I386_IOCTLS_H__}
{$include <asm/ioctl.h>}

const
   TCGETS = $5401;
   TCSETS = $5402;
   TCSETSW = $5403;
   TCSETSF = $5404;
   TCGETA = $5405;
   TCSETA = $5406;
   TCSETAW = $5407;
   TCSETAF = $5408;
   TCSBRK = $5409;
   TCXONC = $540A;
   TCFLSH = $540B;
   TIOCEXCL = $540C;
   TIOCNXCL = $540D;
   TIOCSCTTY = $540E;
   TIOCGPGRP = $540F;
   TIOCSPGRP = $5410;
   TIOCOUTQ = $5411;
   TIOCSTI = $5412;
   TIOCGWINSZ = $5413;
   TIOCSWINSZ = $5414;
   TIOCMGET = $5415;
   TIOCMBIS = $5416;
   TIOCMBIC = $5417;
   TIOCMSET = $5418;
   TIOCGSOFTCAR = $5419;
   TIOCSSOFTCAR = $541A;
   FIONREAD = $541B;
   TIOCINQ = FIONREAD;
   TIOCLINUX = $541C;
   TIOCCONS = $541D;
   TIOCGSERIAL = $541E;
   TIOCSSERIAL = $541F;
   TIOCPKT = $5420;
   FIONBIO = $5421;
   TIOCNOTTY = $5422;
   TIOCSETD = $5423;
   TIOCGETD = $5424;
   TCSBRKP = $5425;
   TIOCTTYGSTRUCT = $5426;
   TIOCSBRK = $5427;
   TIOCCBRK = $5428;
   TIOCGSID = $5429;
function TIOCGPTN : longint;
    { return type might be wrong }

function TIOCSPTLCK : longint;
    { return type might be wrong }

function TIOCGDEV : longint;
    { return type might be wrong }


const
   FIONCLEX = $5450;
   FIOCLEX = $5451;
   FIOASYNC = $5452;
   TIOCSERCONFIG = $5453;
   TIOCSERGWILD = $5454;
   TIOCSERSWILD = $5455;
   TIOCGLCKTRMIOS = $5456;
   TIOCSLCKTRMIOS = $5457;
   TIOCSERGSTRUCT = $5458;
   TIOCSERGETLSR = $5459;
   TIOCSERGETMULTI = $545A;
   TIOCSERSETMULTI = $545B;
   TIOCMIWAIT = $545C;
   TIOCGICOUNT = $545D;
   TIOCGHAYESESP = $545E;
   TIOCSHAYESESP = $545F;
   TIOCPKT_DATA = 0;
   TIOCPKT_FLUSHREAD = 1;
   TIOCPKT_FLUSHWRITE = 2;
   TIOCPKT_STOP = 4;
   TIOCPKT_START = 8;
   TIOCPKT_NOSTOP = 16;
   TIOCPKT_DOSTOP = 32;
   TIOCSER_TEMT = $01;
{$endif}
function TIOCGPTN : longint;
    { return type might be wrong }
  begin
     TIOCGPTN:=_IOR('T',$30,dword);
  end;

function TIOCSPTLCK : longint;
    { return type might be wrong }
  begin
     TIOCSPTLCK:=_IOW('T',$31,longint);
  end;

function TIOCGDEV : longint;
    { return type might be wrong }
  begin
     TIOCGDEV:=_IOR('T',$32,dword);
  end;



{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
