
type

   Pposix_spawnattr_t = ^posix_spawnattr_t;
   posix_spawnattr_t = record
        __flags : smallint;
        __pgrp : pid_t;
        __sd : sigset_t;
        __ss : sigset_t;
        __sp : sched_param;
        __policy : longint;
        __pad : array[0..15] of longint;
     end;

    __spawn_action = record end; // Used anonymously in header file; opaque structure
    P__spawn_action = ^__spawn_action;



   Pposix_spawn_file_actions_t = ^posix_spawn_file_actions_t;
   posix_spawn_file_actions_t = record
        __allocated : longint;
        __used : longint;
        __actions : P__spawn_action;
        __pad : array[0..15] of longint;
     end;

const
   POSIX_SPAWN_RESETIDS = $01;
   POSIX_SPAWN_SETPGROUP = $02;
   POSIX_SPAWN_SETSIGDEF = $04;
   POSIX_SPAWN_SETSIGMASK = $08;
   POSIX_SPAWN_SETSCHEDPARAM = $10;
   POSIX_SPAWN_SETSCHEDULER = $20;

function posix_spawn(__pid:Ppid_t; __path:Pchar; __file_actions:Pposix_spawn_file_actions_t; __attrp:Pposix_spawnattr_t; argv:ppchar;envp:ppchar):longint;cdecl;external clib name 'posix_spawn';
function posix_spawnp(__pid:Ppid_t; __file:Pchar; __file_actions:Pposix_spawn_file_actions_t; __attrp:Pposix_spawnattr_t; argv:PPchar;envp:ppchar):longint;cdecl;external clib name 'posix_spawnp';
function posix_spawnattr_init(__attr:Pposix_spawnattr_t):longint;cdecl;external clib name 'posix_spawnattr_init';
function posix_spawnattr_destroy(__attr:Pposix_spawnattr_t):longint;cdecl;external clib name 'posix_spawnattr_destroy';
function posix_spawnattr_getsigdefault(__attr:Pposix_spawnattr_t; __sigdefault:Psigset_t):longint;cdecl;external clib name 'posix_spawnattr_getsigdefault';
function posix_spawnattr_setsigdefault(__attr:Pposix_spawnattr_t; __sigdefault:Psigset_t):longint;cdecl;external clib name 'posix_spawnattr_setsigdefault';
function posix_spawnattr_getsigmask(__attr:Pposix_spawnattr_t; __sigmask:Psigset_t):longint;cdecl;external clib name 'posix_spawnattr_getsigmask';
function posix_spawnattr_setsigmask(__attr:Pposix_spawnattr_t; __sigmask:Psigset_t):longint;cdecl;external clib name 'posix_spawnattr_setsigmask';
function posix_spawnattr_getflags(__attr:Pposix_spawnattr_t; __flags:Psmallint):longint;cdecl;external clib name 'posix_spawnattr_getflags';
function posix_spawnattr_setflags(_attr:Pposix_spawnattr_t; __flags:smallint):longint;cdecl;external clib name 'posix_spawnattr_setflags';
function posix_spawnattr_getpgroup(__attr:Pposix_spawnattr_t; __pgroup:Ppid_t):longint;cdecl;external clib name 'posix_spawnattr_getpgroup';
function posix_spawnattr_setpgroup(__attr:Pposix_spawnattr_t; __pgroup:pid_t):longint;cdecl;external clib name 'posix_spawnattr_setpgroup';
function posix_spawnattr_getschedpolicy(__attr:Pposix_spawnattr_t; __schedpolicy:Plongint):longint;cdecl;external clib name 'posix_spawnattr_getschedpolicy';
function posix_spawnattr_setschedpolicy(__attr:Pposix_spawnattr_t; __schedpolicy:longint):longint;cdecl;external clib name 'posix_spawnattr_setschedpolicy';
function posix_spawnattr_getschedparam(__attr:Pposix_spawnattr_t; __schedparam:Psched_param):longint;cdecl;external clib name 'posix_spawnattr_getschedparam';
function posix_spawnattr_setschedparam(__attr:Pposix_spawnattr_t; __schedparam:Psched_param):longint;cdecl;external clib name 'posix_spawnattr_setschedparam';
function posix_spawn_file_actions_init(__file_actions:Pposix_spawn_file_actions_t):longint;cdecl;external clib name 'posix_spawn_file_actions_init';
function posix_spawn_file_actions_destroy(__file_actions:Pposix_spawn_file_actions_t):longint;cdecl;external clib name 'posix_spawn_file_actions_destroy';
function posix_spawn_file_actions_addopen(__file_actions:Pposix_spawn_file_actions_t; __fd:longint; __path:Pchar; __oflag:longint; __mode:mode_t):longint;cdecl;external clib name 'posix_spawn_file_actions_addopen';
function posix_spawn_file_actions_addclose(__file_actions:Pposix_spawn_file_actions_t; __fd:longint):longint;cdecl;external clib name 'posix_spawn_file_actions_addclose';
function posix_spawn_file_actions_adddup2(__file_actions:Pposix_spawn_file_actions_t; __fd:longint; __newfd:longint):longint;cdecl;external clib name 'posix_spawn_file_actions_adddup2';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
   TSpawnAction = __spawn_action;
   PSpawnAction = ^TSpawnAction;

function posix_spawn(__pid:Ppid_t; __path:Pchar; var __file_actions: posix_spawn_file_actions_t; var __attrp:posix_spawnattr_t; argv:ppchar;envp:ppchar):longint;cdecl;external clib name 'posix_spawn';
function posix_spawnp(__pid:Ppid_t; __file:Pchar; var __file_actions: posix_spawn_file_actions_t; var __attrp: posix_spawnattr_t; argv:PPchar;envp:ppchar):longint;cdecl;external clib name 'posix_spawnp';
function posix_spawnattr_init(var __attr: posix_spawnattr_t):longint;cdecl;external clib name 'posix_spawnattr_init';
function posix_spawnattr_destroy(var __attr: posix_spawnattr_t):longint;cdecl;external clib name 'posix_spawnattr_destroy';
function posix_spawnattr_getsigdefault(var __attr: posix_spawnattr_t; var __sigdefault: sigset_t):longint;cdecl;external clib name 'posix_spawnattr_getsigdefault';
function posix_spawnattr_setsigdefault(var __attr: posix_spawnattr_t; var __sigdefault: sigset_t):longint;cdecl;external clib name 'posix_spawnattr_setsigdefault';
function posix_spawnattr_getsigmask(var __attr: posix_spawnattr_t; var __sigmask:sigset_t):longint;cdecl;external clib name 'posix_spawnattr_getsigmask';
function posix_spawnattr_setsigmask(var __attr: posix_spawnattr_t; var __sigmask:sigset_t):longint;cdecl;external clib name 'posix_spawnattr_setsigmask';
function posix_spawnattr_getflags(var __attr: posix_spawnattr_t; var __flags: smallint):longint;cdecl;external clib name 'posix_spawnattr_getflags';
function posix_spawnattr_setflags(var __attr: posix_spawnattr_t; __flags:smallint):longint;cdecl;external clib name 'posix_spawnattr_setflags';
function posix_spawnattr_getpgroup(var __attr: posix_spawnattr_t; var __pgroup:pid_t):longint;cdecl;external clib name 'posix_spawnattr_getpgroup';
function posix_spawnattr_setpgroup(var __attr: posix_spawnattr_t; __pgroup:pid_t):longint;cdecl;external clib name 'posix_spawnattr_setpgroup';
function posix_spawnattr_getschedpolicy(var __attr: posix_spawnattr_t; var __schedpolicy:longint):longint;cdecl;external clib name 'posix_spawnattr_getschedpolicy';
function posix_spawnattr_setschedpolicy(var __attr: posix_spawnattr_t; __schedpolicy:longint):longint;cdecl;external clib name 'posix_spawnattr_setschedpolicy';
function posix_spawnattr_getschedparam(var __attr: posix_spawnattr_t; var __schedparam:sched_param):longint;cdecl;external clib name 'posix_spawnattr_getschedparam';
function posix_spawnattr_setschedparam(var __attr: posix_spawnattr_t; var __schedparam:sched_param):longint;cdecl;external clib name 'posix_spawnattr_setschedparam';
function posix_spawn_file_actions_init(var __file_actions: posix_spawn_file_actions_t):longint;cdecl;external clib name 'posix_spawn_file_actions_init';
function posix_spawn_file_actions_destroy(var __file_actions: posix_spawn_file_actions_t):longint;cdecl;external clib name 'posix_spawn_file_actions_destroy';
function posix_spawn_file_actions_addopen(var __file_actions: posix_spawn_file_actions_t; __fd:longint; __path:Pchar; __oflag:longint; __mode:mode_t):longint;cdecl;external clib name 'posix_spawn_file_actions_addopen';
function posix_spawn_file_actions_addclose(var __file_actions: posix_spawn_file_actions_t; __fd:longint):longint;cdecl;external clib name 'posix_spawn_file_actions_addclose';
function posix_spawn_file_actions_adddup2(var __file_actions: posix_spawn_file_actions_t; __fd:longint; __newfd:longint):longint;cdecl;external clib name 'posix_spawn_file_actions_adddup2';
