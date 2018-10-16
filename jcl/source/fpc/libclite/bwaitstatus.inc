
Function WEXITSTATUS(Status: longint): longint;
begin
  Result:=(Status and $FF00) shr 8;
end;


Function WTERMSIG(Status: longint): longint;
begin
  Result:=(Status and $7F);
end;


Function WSTOPSIG(Status: longint): longint;
begin
  Result:=WEXITSTATUS(Status);
end;


Function WIFEXITED(Status: longint): Boolean;
begin
  Result:=(WTERMSIG(Status)=0);
end;


Function WIFSIGNALED(Status: longint): Boolean;
begin
  Result:=(not WIFSTOPPED(Status)) and (not WIFEXITED(Status));
end;


Function WIFSTOPPED(Status: longint): Boolean;
begin
  Result:=((Status and $FF)=$7F);
end;


Function WCOREDUMP(Status: longint): Boolean;
begin
  Result:=((Status and WCOREFLAG)<>0);
end;


Function W_EXITCODE(ReturnCode, Signal: longint): longint;
begin
  Result:=(ReturnCode shl 8) or Signal;
end;


Function W_STOPCODE(Signal: longint): longint;
begin
  Result:=(Signal shl 8) or $7F;
end;
