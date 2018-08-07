
type
   Pnfds_t = ^nfds_t;
   nfds_t = dword;

   Ppollfd = ^pollfd;
   pollfd = record
      fd : longint;
      events : smallint;
      revents : smallint;
   end;


function poll(__fds: Ppollfd; __nfds:nfds_t; __timeout:longint):longint;cdecl;external clib name 'poll';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TPollFD = pollfd;

