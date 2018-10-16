Function IN_CLASSA(a: in_addr_t): Boolean;

begin
  Result:=((a and $80000000)=0);
end;


Function IN_CLASSB(a: in_addr_t): Boolean;

begin
  Result:=((a and $c0000000)=$80000000);
end;

Function IN_CLASSC(a: in_addr_t): Boolean;
begin
  Result:=((a and $e0000000)=$c0000000);
end;

Function IN_CLASSD(a: in_addr_t): Boolean;
begin
  Result:=((a and $f0000000)=$e0000000);
end;

Function IN_MULTICAST(a: in_addr_t): Boolean;
begin
  Result:=IN_CLASSD(a);
end;

Function IN_EXPERIMENTAL(a: in_addr_t): Boolean;
begin
  Result:=((a and $e0000000)=$e0000000);
end;

Function IN_BADCLASS(a: in_addr_t): Boolean;
begin
  Result:=((a and $f0000000)=$f0000000);
end;


Function IN6_IS_ADDR_UNSPECIFIED(const a: in6_addr): Boolean;
begin
  Result:=(a.s6_addr32[0]=0) and
            (a.s6_addr32[1]=0) and
            (a.s6_addr32[2]=0) and
            (a.s6_addr32[3]=0);
end;

Function IN6_IS_ADDR_LOOPBACK(const a: in6_addr): Boolean;
begin
  Result:=(a.s6_addr32[0]=0) and
            (a.s6_addr32[1]=0) and
            (a.s6_addr32[2]=0) and
            (a.s6_addr32[3]=htonl(1));
end;

Function IN6_IS_ADDR_MULTICAST(const a: in6_addr): Boolean;
begin
  Result:=(a.s6_addr[0]=$ff);
end;

Function IN6_IS_ADDR_LINKLOCAL(const a: in6_addr): Boolean;
begin
  Result:=((a.s6_addr32[0] and htonl($ffc00000))=htonl($fe800000));
end;

Function IN6_IS_ADDR_SITELOCAL(const a: in6_addr): Boolean;
begin
  Result:=((a.s6_addr32[0] and htonl($ffc00000))=htonl($fec00000))
end;

Function IN6_IS_ADDR_V4MAPPED(const a: in6_addr): Boolean;
begin
  Result:=(a.s6_addr32[0]=0) and
            (a.s6_addr32[1]=0) and
            (a.s6_addr32[2]=htonl($ffff));
end;

Function IN6_IS_ADDR_V4COMPAT(const a: in6_addr): Boolean;
begin
  Result:=(a.s6_addr32[0]=0) and
            (a.s6_addr32[1]=0) and
            (a.s6_addr32[2]=0) and
            (ntohl(a.s6_addr32[3]) > 1);
end;

Function IN6_ARE_ADDR_EQUAL(const a, b: in6_addr): Boolean;
begin
  Result:=(a.s6_addr32[0]=b.s6_addr32[0]) and
            (a.s6_addr32[1]=b.s6_addr32[1]) and
            (a.s6_addr32[2]=b.s6_addr32[2]) and
            (a.s6_addr32[3]=b.s6_addr32[3]);
end;

Function IN6_IS_ADDR_MC_NODELOCAL(const a: in6_addr): Boolean;
begin
  Result:=IN6_IS_ADDR_MULTICAST(a) and
            ((a.s6_addr[1] and $f)=$1);
end;

Function IN6_IS_ADDR_MC_LINKLOCAL(const a: in6_addr): Boolean;
begin
  Result:=IN6_IS_ADDR_MULTICAST(a) and
            ((a.s6_addr[1] and $f)=$2);
end;

Function IN6_IS_ADDR_MC_SITELOCAL(const a: in6_addr): Boolean;
begin
  Result:=IN6_IS_ADDR_MULTICAST(a) and
            ((a.s6_addr[1] and $f)=$5);
end;

Function IN6_IS_ADDR_MC_ORGLOCAL(const a: in6_addr): Boolean;
begin
  Result:=IN6_IS_ADDR_MULTICAST(a) and
            ((a.s6_addr[1] and $f)=$8);
end;

Function IN6_IS_ADDR_MC_GLOBAL(const a: in6_addr): Boolean;
begin
  Result:=IN6_IS_ADDR_MULTICAST(a) and
            ((a.s6_addr[1] and $f)=$e);
end;

