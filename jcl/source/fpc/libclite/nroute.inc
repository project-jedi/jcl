
Function RT_ADDRCLASS(flags: u_int32_t): u_int32_t;

begin
  Result:=flags shr 23;
end;


Function RT_TOS(tos: Integer): Integer;

begin
  Result:=tos and IPTOS_TOS_MASK;
end;

Function RT_LOCALADDR(flags: u_int32_t): Boolean;

begin
  Result:=(flags and RTF_ADDRCLASSMASK)=(RTF_LOCAL or RTF_INTERFACE);
end;
