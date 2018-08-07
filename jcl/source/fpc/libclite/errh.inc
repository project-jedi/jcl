
procedure warn(__format: Pchar; args: array of const); cdecl; external clib name 'warn';
procedure vwarn(__format: Pchar; arg: Pointer); cdecl;external clib name 'vwarn';
procedure warnx(__format: Pchar; args: array of const); cdecl; external clib name 'warnx';
procedure vwarnx(__format: Pchar; arg: pointer); cdecl; external clib name 'vwarnx';
procedure err(__status: Integer; __format: Pchar; args : array of const); cdecl; external clib name 'err';
procedure verr(__status: Integer; __format: Pchar; Arg: Pointer); cdecl;  external clib name 'verr';
procedure errx(__status: Integer; __format: Pchar); cdecl;  external clib name 'errx';
procedure verrx(__status: Integer; __format: Pchar; Arg: Pointer); cdecl; external clib name 'verrx';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
