Function __FDELT(d: longint): Integer;

begin
  Result:=d div __NFDBITS;
end;


Function __FDMASK(d: longint): __fd_mask;
begin
  Result:=1 shl (d mod __NFDBITS);
end;
