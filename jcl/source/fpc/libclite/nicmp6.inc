function ICMP6_FILTER_WILLPASS(__type: Integer; const filterp: TICMP6_Filter): Boolean;
begin
  Result := (filterp.data[__type shr 5] and (1 shl (__type and 31))) = 0;
end;

function ICMP6_FILTER_WILLBLOCK(__type: Integer; const filterp: TICMP6_Filter): Boolean;
begin
  Result := (filterp.data[__type shr 5] and (1 shl (__type and 31))) <> 0;
end;

procedure ICMP6_FILTER_SETPASS(__type: Integer; var filterp: TICMP6_Filter);
begin
  filterp.data[__type shr 5] := filterp.data[__type shr 5] and not
                                      (1 shl (__type and 31));
end;

procedure ICMP6_FILTER_SETBLOCK(__type: Integer; var filterp: TICMP6_Filter);
begin
  filterp.data[__type shr 5] := filterp.data[__type shr 5] or
                                      (1 shl (__type and 31));
end;

procedure ICMP6_FILTER_SETPASSALL(var filterp: TICMP6_Filter);
begin
  FillChar(filterp, SizeOf(filterp), 0);
end;

procedure ICMP6_FILTER_SETBLOCKALL(var filterp: TICMP6_Filter);
begin
  FillChar(filterp, SizeOf(filterp), $FF);
end;
