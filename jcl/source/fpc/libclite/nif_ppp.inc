

Function PPPIOCGFLAGS: cardinal;

begin
  Result:=_IOR(ord('t'),90,SizeOf(longint));
end;


Function PPPIOCSFLAGS: cardinal;

begin
  Result:=_IOW(ord('t'),89,SizeOf(longint));
end;


Function PPPIOCGASYNCMAP: cardinal;

begin
  Result:=_IOR(ord('t'),88,SizeOf(longint));
end;


Function PPPIOCSASYNCMAP: cardinal;

begin
  Result:=_IOW(ord('t'),87,SizeOf(longint));
end;


Function PPPIOCGUNIT: cardinal;

begin
  Result:=_IOR(ord('t'),86,SizeOf(longint));
end;


Function PPPIOCGRASYNCMAP: cardinal;

begin
  Result:=_IOR(ord('t'),85,SizeOf(longint));
end;


Function PPPIOCSRASYNCMAP: cardinal;

begin
  Result:=_IOW(ord('t'),84,SizeOf(longint));
end;


Function PPPIOCGMRU: cardinal;

begin
  Result:=_IOR(ord('t'),83,SizeOf(longint));
end;


Function PPPIOCSMRU: cardinal;

begin
  Result:=_IOW(ord('t'),82,SizeOf(longint));
end;


Function PPPIOCSMAXCID: cardinal;

begin
  Result:=_IOW(ord('t'),81,SizeOf(longint));
end;


Function PPPIOCGXASYNCMAP: cardinal;

begin
  Result:=_IOR(ord('t'),80,SizeOf(ext_accm));
end;


Function PPPIOCSXASYNCMAP: cardinal;

begin
  Result:=_IOW(ord('t'),79,SizeOf(ext_accm));
end;


Function PPPIOCXFERUNIT: cardinal;

begin
  Result:=_IO(ord('t'),78);
end;


Function PPPIOCSCOMPRESS: cardinal;

begin
  Result:=_IOW(ord('t'),77,SizeOf(ppp_option_data));
end;


Function PPPIOCGNPMODE: cardinal;

begin
  Result:=_IOWR(ord('t'),76,SizeOf(npioctl));
end;


Function PPPIOCSNPMODE: cardinal;

begin
  Result:=_IOW(ord('t'),75,SizeOf(npioctl));
end;


Function PPPIOCGDEBUG: cardinal;

begin
  Result:=_IOR(ord('t'),65,SizeOf(longint));
end;


Function PPPIOCSDEBUG: cardinal;

begin
  Result:=_IOW(ord('t'),64,SizeOf(longint));
end;


Function PPPIOCGIDLE: cardinal;

begin
  Result:=_IOR(ord('t'),63,SizeOf(ppp_idle));
end;
