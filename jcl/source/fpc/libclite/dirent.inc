{ ---------------------------------------------------------------------
    Macros from dirent.h
  ---------------------------------------------------------------------}

function IFTODT(mode : __mode_t) : longint;
begin
   IFTODT:=(mode and $F000) shr 12;
end;

function DTTOIF(dirtype : longint) : __mode_t;
begin
   DTTOIF:=dirtype shl 12;
end;

