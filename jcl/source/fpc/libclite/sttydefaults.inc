
function CTRL(x: Char): Char;
begin
  Result:=Char(Ord(x) and $1F);
end;
