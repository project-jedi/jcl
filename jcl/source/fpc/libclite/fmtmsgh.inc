
Const
  MM_HARD = $001;
  MM_SOFT = $002;
  MM_FIRM = $004;
  MM_APPL = $008;
  MM_UTIL = $010;
  MM_OPSYS = $020;
  MM_RECOVER = $040;
  MM_NRECOV = $080;
  MM_PRINT = $100;
  MM_CONSOLE = $200;

  MM_NOSEV = 0;
  MM_HALT = 1;
  MM_ERROR = 2;
  MM_WARNING = 3;
  MM_INFO = 4;

  MM_NULLLBL       = PChar(nil);
  MM_NULLSEV       = 0;
  MM_NULLMC        = Longint(0);
  MM_NULLTXT       = PChar(0);
  MM_NULLACT       = PChar(0);
  MM_NULLTAG       = PChar(0);

  MM_NOTOK = -(1);
  MM_OK = 0;
  MM_NOMSG = 1;
  MM_NOCON = 4;


function fmtmsg(__classification:longint; __label:Pchar; __severity:longint; __text:Pchar; __action:Pchar;  __tag:Pchar):longint;cdecl;external clib name 'fmtmsg';
function addseverity(__severity:longint; __string:Pchar):longint;cdecl;external clib name 'addseverity';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
