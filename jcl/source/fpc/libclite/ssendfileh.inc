
function sendfile(__out_fd:longint; __in_fd:longint; offset:Poff_t; __count:size_t):ssize_t;cdecl;external clib name 'sendfile';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function sendfile(__out_fd:longint; __in_fd:longint; var offset: off_t; __count:size_t):ssize_t;cdecl;external clib name 'sendfile';
