
Function ICMP_ADVLEN(const p: icmp): cardinal;

var
  L :  Longint;

begin
  L:=p.icmp_dun.id_ip.idi_ip.flag0 and $F;
  Result:=(8+(L shl 2) + 8);
end;

Function ICMP_INFOTYPE(_type: cardinal): Boolean;

begin
  Result := (_type=ICMP_ECHOREPLY) or
            (_type=ICMP_ECHO) or
            (_type=ICMP_ROUTERADVERT) or
            (_type=ICMP_ROUTERSOLICIT) or
            (_type=ICMP_TSTAMP) or
            (_type=ICMP_TSTAMPREPLY) or
            (_type=ICMP_IREQ) or
            (_type=ICMP_IREQREPLY) or
            (_type=ICMP_MASKREQ) or
            (_type=ICMP_MASKREPLY);
end;
