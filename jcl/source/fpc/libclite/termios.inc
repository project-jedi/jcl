function CCEQ(val,c : cc_t) : boolean ;
begin
   CCEQ:=((c=val) and (val<>_POSIX_VDISABLE));
end;

