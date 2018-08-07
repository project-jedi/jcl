Function InitializeCriticalSection(var lpCriticalSection: TRTLCriticalSection): Integer;

var
  Attr : pthread_mutexattr_t;

begin
  Result:=pthread_mutexattr_init(Attr);
  if Result=0 then 
    Try
      Result:=pthread_mutexattr_settype(Attr,PTHREAD_MUTEX_RECURSIVE);
      if Result=0 then 
        Result:=pthread_mutex_init(lpCriticalSection, Attr);
    Finally
      pthread_mutexattr_destroy(Attr);
    end;
end;

function TryEnterCriticalSection(var lpCriticalSection: TRTLCriticalSection): Boolean;
begin
  Result:=EBUSY<>pthread_mutex_trylock(lpCriticalSection);
end;

                              