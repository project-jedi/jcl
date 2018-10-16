

function pthread_sigmask(__how:longint; __newmask:P__sigset_t; __oldmask:P__sigset_t):longint;cdecl;external threadslib name 'pthread_sigmask';
function pthread_kill(__thread:pthread_t; __signo:longint):longint;cdecl;external threadslib name 'pthread_kill';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
