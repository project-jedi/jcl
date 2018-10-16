
{ Translation of macro }

{$ifndef LIBC_OLDERRNO}
function errno : error_t;

begin
  Result:=__errno_location()^;
end;

procedure seterrno (value : error_t);

begin
  __errno_location()^:=Value;
end;
{$endif}
