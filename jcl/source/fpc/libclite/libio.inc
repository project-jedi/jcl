{ ---------------------------------------------------------------------
    Macros from libio.h
  ---------------------------------------------------------------------}

Function _IO_getc_unlocked(_fp: P_IO_FILE): longint;
begin
  if _fp^._IO_read_ptr>=_fp^._IO_read_end then
    Result:=__uflow(_fp)
  else
    begin
    Result:=PByte(_fp^._IO_read_ptr)^;
    Inc(_fp^._IO_read_ptr);
    end;
end;


Function _IO_peekc_unlocked(_fp: P_IO_FILE): longint;
begin
  if (_fp^._IO_read_ptr>=_fp^._IO_read_end) and (__underflow(_fp) = __EOF) then
    Result:=__EOF
  else
    Result:=PByte(_fp^._IO_read_ptr)^;
end;


Function _IO_putc_unlocked(_ch: Char; _fp: P_IO_FILE): longint;
begin
  if _fp^._IO_write_ptr>=_fp^._IO_write_end then
    Result:=__overflow(_fp, Byte(_ch))
  else
    begin
    Result:=Byte(_ch);
    _fp^._IO_write_ptr^:=_ch;
    Inc(_fp^._IO_write_ptr);
    end;
end;


Function _IO_getwc_unlocked(_fp: P_IO_FILE): longint;
begin
  if Cardinal(_fp^._wide_data^._IO_read_ptr)>=Cardinal(_fp^._wide_data^._IO_read_end) then
    Result:=__wuflow(_fp)
  else
    begin
//!! MVC    Result:=_fp^._wide_data^._IO_read_ptr^;
    Inc(_fp^._wide_data^._IO_read_ptr);
    end;
end;


Function _IO_putwc_unlocked(_wch: wchar_t; _fp: P_IO_FILE): longint;
begin
{ //!! MVC
  if Cardinal(_fp^._wide_data^._IO_write_ptr)>=Cardinal(_fp^._wide_data^._IO_write_end) then
    Result:=__woverflow(_fp, _wch)
  else
    begin
    Result:=_wch;
    _fp^._wide_data^._IO_write_ptr^:=_wch;
    Inc(_fp^._wide_data^._IO_write_ptr);
    end;
}
end;


Function _IO_feof_unlocked(_fp: P_IO_FILE): longint;
begin
  Result:=Ord((_fp^._flags and _IO_EOF_SEEN)<>0);
end;


Function _IO_ferror_unlocked(_fp: P_IO_FILE): longint;
begin
  Result:=Ord((_fp^._flags and _IO_ERR_SEEN)<>0);
end;


Function _IO_PENDING_OUTPUT_COUNT(_fp: P_IO_FILE): longint;
begin
  Result:=(_fp^._IO_write_ptr)-(_fp^._IO_write_base);
end;


