function MB_CUR_MAX : longint;
    { return type might be wrong }
  begin
     MB_CUR_MAX:=__ctype_get_mb_cur_max;
  end;

