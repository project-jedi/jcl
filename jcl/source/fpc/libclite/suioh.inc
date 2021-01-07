
function readv(__fd:longint; __vector:Piovec; __count:longint):ssize_t;cdecl;external clib name 'readv'; overload;
function writev(__fd:longint; __vector:Piovec; __count:longint):ssize_t;cdecl;external clib name 'writev'; overload;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function readv(__fd:longint; var __vector: iovec; __count:longint):ssize_t;cdecl;external clib name 'readv'; overload;
function writev(__fd:longint; var __vector: iovec; __count:longint):ssize_t;cdecl;external clib name 'writev'; overload;
