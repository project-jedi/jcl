function WEXITSTATUS(Status: Integer): Integer;
begin
  Result := (Status and $FF00) shr 8;
end;

function WTERMSIG(Status: Integer): Integer;
begin
  Result := (Status and $7F);
end;

function WSTOPSIG(Status: Integer): Integer;
begin
  Result := WEXITSTATUS(Status);
end;

function WIFEXITED(Status: Integer): Boolean;
begin
  Result := (WTERMSIG(Status) = 0);
end;

function WIFSIGNALED(Status: Integer): Boolean;
begin
  Result := (not WIFSTOPPED(Status)) and (not WIFEXITED(Status));
end;

function WIFSTOPPED(Status: Integer): Boolean;
begin
  Result := ((Status and $FF) = $7F);
end;

function WCOREDUMP(Status: Integer): Boolean;
begin
  Result := ((Status and WCOREFLAG) <> 0);
end;

function W_EXITCODE(ReturnCode, Signal: Integer): Integer;
begin
  Result := (ReturnCode shl 8) or Signal;
end;

function W_STOPCODE(Signal: Integer): Integer;
begin
  Result := (Signal shl 8) or $7F;
end;
