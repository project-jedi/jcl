
function SUN_LEN(ptr: PSockAddr_un): Cardinal;

begin
  Result:=SizeOf(ptr^.sun_family)+__strlen(ptr^.sun_path);
end;
