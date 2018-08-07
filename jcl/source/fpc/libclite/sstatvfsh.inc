

function statvfs(__file:Pchar; __buf:Pstatvfs):longint;cdecl;external clib name 'statvfs';
function statvfs64(__file:Pchar; __buf:Pstatvfs64):longint;cdecl;external clib name 'statvfs64';
function fstatvfs(__fildes:longint; __buf:Pstatvfs):longint;cdecl;external clib name 'fstatvfs';
function fstatvfs64(__fildes:longint; __buf:Pstatvfs64):longint;cdecl;external clib name 'fstatvfs64';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function statvfs(__file:Pchar; var __buf: _statvfs):longint;cdecl;external clib name 'statvfs';
function statvfs64(__file:Pchar; var __buf: _statvfs64):longint;cdecl;external clib name 'statvfs64';
function fstatvfs(__fildes:longint; var __buf: _statvfs):longint;cdecl;external clib name 'fstatvfs';
function fstatvfs64(__fildes:longint; var __buf: _statvfs64):longint;cdecl;external clib name 'fstatvfs64';
