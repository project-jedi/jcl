
function WEXITSTATUS(Status: longint): longint;
function WTERMSIG(Status: longint): longint;
function WSTOPSIG(Status: longint): longint;
function WIFEXITED(Status: longint): Boolean;
function WIFSIGNALED(Status: longint): Boolean;
function WIFSTOPPED(Status: longint): Boolean;
function WCOREDUMP(Status: longint): Boolean;
function W_EXITCODE(ReturnCode, Signal: longint): longint;
function W_STOPCODE(Signal: longint): longint;

const
  WCOREFLAG          = $80;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
