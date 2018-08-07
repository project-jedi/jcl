
procedure error(status: longint; errnum: longint; format: Pchar; args: array of const); cdecl; external clib name 'error';
procedure error_at_line(status: longint; errnum: longint; fname: Pchar; lineno: Cardinal; format: Pchar; args : array of const); cdecl; external clib name 'error_at_line';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
