

function LOG_PRI(p : longint) : longint;

begin
   LOG_PRI:=(p and LOG_PRIMASK);
end;

function LOG_MAKEPRI(fac,pri : longint) : longint;

begin
   LOG_MAKEPRI:=(fac shl 3) or pri;
end;

function LOG_FAC(p : longint) : longint;

begin
   LOG_FAC:=(p and LOG_FACMASK) shr 3;
end;

function LOG_MASK(pri : longint) : longint;

begin
   LOG_MASK:=1 shl pri;
end;


function LOG_UPTO(pri : longint) : longint;
begin
   LOG_UPTO:=(1 shl (pri + 1)) - 1;
end;
