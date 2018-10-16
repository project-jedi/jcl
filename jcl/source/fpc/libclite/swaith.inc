

type
  Pidtype_t = ^idtype_t;
  idtype_t =  (P_ALL,P_PID,P_PGID);

{ // used to be three constants. Why borland decided to change this is a total mystery...
       Const
         P_ALL = 0;
         P_PID = 1;
         P_PGID = 2;
}

const
  WAIT_ANY = -(1);
  WAIT_MYPGRP = 0;

function wait(__stat_loc:PLongint):__pid_t;cdecl;external clib name 'wait';
function waitpid(__pid:__pid_t; __stat_loc:Plongint; __options:longint):__pid_t;cdecl;external clib name 'waitpid';
function waitid(__idtype:idtype_t; __id:__id_t; __infop:Psiginfo_t; __options:longint):longint;cdecl;external clib name 'waitid';
function wait3(__stat_loc: Plongint; __options:longint; __usage:Prusage):__pid_t;cdecl;external clib name 'wait3';
function wait4(__pid:__pid_t; __stat_loc: Plongint; __options:longint; __usage:Prusage):__pid_t;cdecl;external clib name 'wait4';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
function wait(var __stat_loc: Longint):__pid_t;cdecl;external clib name 'wait';
function waitpid(__pid:__pid_t; var __stat_loc:longint; __options:longint):__pid_t;cdecl;external clib name 'waitpid';
function waitid(__idtype:idtype_t; __id:__id_t; var __infop: siginfo_t; __options:longint):longint;cdecl;external clib name 'waitid';
function wait3(var __stat_loc: longint; __options:longint; var __usage:rusage):__pid_t;cdecl;external clib name 'wait3';
function wait4(__pid:__pid_t; var __stat_loc: longint; __options:longint; var __usage: rusage):__pid_t;cdecl;external clib name 'wait4';
