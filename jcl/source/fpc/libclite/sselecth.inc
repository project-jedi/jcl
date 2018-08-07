
Type
  Pfd_set = ^_fd_set;
  _fd_set = __fd_set;

const
  FD_SETSIZE = __FD_SETSIZE;

Type
  Pfd_mask = ^fd_mask;
  fd_mask = __fd_mask;

const
  NFDBITS = __NFDBITS;
  
procedure FD_ZERO(var fdset: _fd_set);
procedure FD_SET(fd: longint; var fdset: _fd_Set);
procedure FD_CLR(fd: longint; var fdset: _fd_set);
function FD_ISSET(fd: longint; const fdset: _fd_set): Boolean;

function select(__nfds:longint; __readfds:Pfd_set; __writefds:Pfd_set; __exceptfds:Pfd_set; __timeout:Ptimeval):longint;cdecl;external clib name 'select';
function pselect(__nfds:longint; __readfds:Pfd_set; __writefds:Pfd_set; __exceptfds:Pfd_set; __timeout:Ptimespec;
               __sigmask:P__sigset_t):longint;cdecl;external clib name 'pselect';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function select(__nfds:longint; __readfds: Pfd_set; __writefds: Pfd_set; __exceptfds: Pfd_set; var __timeout:timeval):longint;cdecl;external clib name 'select';
function select(__nfds:longint; var __readfds,__writefds, __exceptfds: _fd_set; var __timeout:timeval):longint;cdecl;external clib name 'select';
function pselect(__nfds:longint; __readfds:Pfd_set; __writefds:Pfd_set; __exceptfds:Pfd_set; var __timeout:timespec; const __sigmask: __sigset_t):longint;cdecl;external clib name 'pselect';
function pselect(__nfds:longint; Var __readfds, __writefds, __exceptfds: _fd_set; var __timeout:timespec; const __sigmask: __sigset_t):longint;cdecl;external clib name 'pselect';
