{ ---------------------------------------------------------------------
    macros from sys/raw.h
  ---------------------------------------------------------------------}

function RAW_SETBIND : cardinal;

  begin
     RAW_SETBIND:=_IO($ac,0);
  end;

function RAW_GETBIND : cardinal;

  begin
     RAW_GETBIND:=_IO($ac,1);
  end;
