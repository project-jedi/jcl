
function SA_LEN(const Buf): Cardinal; // Untyped buffer; this is *unsafe*.

begin
  Result:=__libc_sa_len(PSockAddr(@Buf)^.sa_family);
end;


function CMSG_DATA(cmsg: Pointer): PByte;

begin
  Result:=PByte(Cardinal(cmsg) + SizeOf(Pcmsghdr));
end;


function CMSG_NXTHDR(mhdr: Pmsghdr; cmsg: Pcmsghdr): Pcmsghdr;

begin
   Result:=__cmsg_nxthdr(mhdr, cmsg);
end;


function CMSG_FIRSTHDR(mhdr: Pmsghdr): Pcmsghdr;

begin
  if mhdr^.msg_controllen >= SizeOf(cmsghdr) then
    Result:=mhdr^.msg_control
  else
    Result:=nil;
end;


function CMSG_ALIGN(len: size_t): size_t;

begin
  Result:=(len+SizeOf(size_t)-1) and (not(SizeOf(size_t)-1));
end;


function CMSG_SPACE(len: size_t): size_t;

begin
  Result:=CMSG_ALIGN(len)+CMSG_ALIGN(SizeOf(cmsghdr));
end;


function CMSG_LEN(len: size_t): size_t;

begin
  Result:=CMSG_ALIGN(SizeOf(cmsghdr))+len;
end;
