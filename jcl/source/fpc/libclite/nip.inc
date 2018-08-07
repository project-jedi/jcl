function IPTOS_TOS(tos : longint) : longint;
begin
   IPTOS_TOS:=tos and IPTOS_TOS_MASK;
end;

function IPTOS_PREC(tos : longint) : longint;
begin
   IPTOS_PREC:=tos and IPTOS_PREC_MASK;
end;

function IPOPT_COPIED(o : longint) : longint;
begin
   IPOPT_COPIED:=o and IPOPT_COPY;
end;

function IPOPT_CLASS(o : longint) : longint;
begin
   IPOPT_CLASS:=o and IPOPT_CLASS_MASK;
end;

function IPOPT_NUMBER(o : longint) : longint;
begin
   IPOPT_NUMBER:=o and IPOPT_NUMBER_MASK;
end;

