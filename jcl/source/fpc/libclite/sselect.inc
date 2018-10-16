
Function FD_ISSET(fd: longint; const fdset: _fd_set): Boolean;

begin
  Result:=(fdset.fds_bits[__FDELT(fd)] and __FDMASK(fd))<>0;
end;


Procedure FD_SET(fd: longint; var fdset: _fd_set);

begin
  fdset.fds_bits[__FDELT(fd)]:=fdset.fds_bits[__FDELT(fd)] or __FDMASK(fd);
end;


Procedure FD_CLR(fd: longint; var fdset: _fd_set);

begin
  fdset.fds_bits[__FDELT(fd)]:=fdset.fds_bits[__FDELT(fd)] and (not __FDMASK(fd));
end;


Procedure FD_ZERO(var fdset: _fd_set);

var
  I: Integer;

begin
  with fdset do
    for I:=Low(fds_bits) to High(fds_bits) do
      fds_bits[I]:=0;
end;

