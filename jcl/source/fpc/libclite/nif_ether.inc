
procedure ETHER_MAP_IP_MULTICAST(const ipaddr: in_addr; enaddr: PEthernetAddress);
begin
  enaddr^[0] := $01;
  enaddr^[1] := $00;
  enaddr^[2] := $5e;
  enaddr^[3] := ipaddr.S_un_b.s_b2 and $7f;
  enaddr^[4] := ipaddr.S_un_b.s_b3;
  enaddr^[5] := ipaddr.S_un_b.s_b4;
end;


