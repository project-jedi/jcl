{ ---------------------------------------------------------------------
    Macros from sys/time.h
  ---------------------------------------------------------------------}


Procedure TIMEVAL_TO_TIMESPEC(const tv: TTimeVal; var ts: TTimeSpec);
begin
  ts.tv_sec:=tv.tv_sec;
  ts.tv_nsec:=tv.tv_usec*1000;
end;


Procedure TIMESPEC_TO_TIMEVAL(var tv: TTimeVal; const ts: TTimeSpec);
begin
  tv.tv_sec:=ts.tv_sec;
  tv.tv_usec:=ts.tv_nsec div 1000;
end;


Function timerisset(const Value: TTimeVal): Boolean;
begin
  Result:=(Value.tv_sec<>0) or (Value.tv_usec<>0);
end;


Procedure timerclear(var Value: TTimeVal);
begin
  Value.tv_sec:=0;
  Value.tv_usec:=0;
end;


Function __timercmp(const a, b: TTimeVal): Integer;

begin
  if a.tv_sec=b.tv_sec then
    begin
    if a.tv_usec>b.tv_usec then
      Result:=1
    else if a.tv_usec<b.tv_usec then
      Result:=-1
    else
      Result:=0;
    end
  else
    begin
    if a.tv_sec>b.tv_sec then
      Result:=1
    else
      Result:=-1;
    end;
end;


Function timeradd(const a, b: TTimeVal): TTimeVal;

begin
  Result.tv_sec:=a.tv_sec+b.tv_sec;
  Result.tv_usec:=a.tv_usec+b.tv_usec;
  if Result.tv_usec>=1000000 then
    begin
    Inc(Result.tv_sec);
    Dec(Result.tv_usec, 1000000);
    end;
end;


Function timersub(const a, b: TTimeVal): TTimeVal;

begin
  Result.tv_sec:=a.tv_sec-b.tv_sec;
  Result.tv_usec:=a.tv_usec-b.tv_usec;
  if Result.tv_usec<0 then
    begin
    Dec(Result.tv_sec);
    Inc(Result.tv_usec,1000000);
    end;
end;

