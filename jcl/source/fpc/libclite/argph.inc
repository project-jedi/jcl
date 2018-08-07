
type
   Pargp_option = ^argp_option;
   argp_option = record
        name : Pchar;
        key : longint;
        arg : Pchar;
        flags : longint;
        doc : Pchar;
        group : longint;
     end;


const
   OPTION_ARG_OPTIONAL = $1;
   OPTION_HIDDEN = $2;
   OPTION_ALIAS = $4;
   OPTION_DOC = $8;
   OPTION_NO_USAGE = $10;

   ARGP_ERR_UNKNOWN = E2BIG;
   ARGP_KEY_ARG = 0;
   ARGP_KEY_ARGS = $1000006;
   ARGP_KEY_END = $1000001;
   ARGP_KEY_NO_ARGS = $1000002;
   ARGP_KEY_INIT = $1000003;
   ARGP_KEY_FINI = $1000007;
   ARGP_KEY_SUCCESS = $1000004;
   ARGP_KEY_ERROR = $1000005;

   ARGP_KEY_HELP_PRE_DOC = $2000001;
   ARGP_KEY_HELP_POST_DOC = $2000002;
   ARGP_KEY_HELP_HEADER = $2000003;
   ARGP_KEY_HELP_EXTRA = $2000004;
   ARGP_KEY_HELP_DUP_ARGS_NOTE = $2000005;
   ARGP_KEY_HELP_ARGS_DOC = $2000006;

   ARGP_PARSE_ARGV0 = $01;
   ARGP_NO_ERRS = $02;
   ARGP_NO_ARGS = $04;
   ARGP_IN_ORDER = $08;
   ARGP_NO_HELP = $10;
   ARGP_NO_EXIT = $20;
   ARGP_LONG_ONLY = $40;
   ARGP_SILENT = (ARGP_NO_EXIT or ARGP_NO_ERRS) or ARGP_NO_HELP;

type
  Pargp_state = ^argp_state;
  Pargp_child = ^argp_child;

  argp_parser_t = function (key:longint; arg:Pchar; state:Pargp_state):error_t;cdecl;

   Pargp = ^argp;
   argp = record
        options : Pargp_option;
        parser : argp_parser_t;
        args_doc : Pchar;
        doc : Pchar;
        children : Pargp_child;
        help_filter : function (__key:longint; __text:Pchar; __input:pointer):Pchar;
        argp_domain : Pchar;
     end;


   argp_child = record
        argp : Pargp;
        flags : longint;
        header : Pchar;
        group : longint;
     end;

   argp_state = record
        root_argp : Pargp;
        argc : longint;
        argv : ^Pchar;
        next : longint;
        flags : dword;
        arg_num : dword;
        quoted : longint;
        input : pointer;
        child_inputs : ^pointer;
        hook : pointer;
        name : Pchar;
        err_stream : PFILE;
        out_stream : PFILE;
        pstate : pointer;
     end;

type
  TArgPProgramVersionHook = procedure(__stream: PFILE; __state: pargp_state); cdecl;



function argp_parse(__argp:Pargp; __argc:longint; __argv:PPchar; __flags:dword; __arg_index:Plongint;
           __input:pointer):error_t;cdecl;external clib name 'argp_parse';
function __argp_parse(__argp:Pargp; __argc:longint; __argv:PPchar; __flags:dword; __arg_index:Plongint;
           __input:pointer):error_t;cdecl;external clib name '__argp_parse';
var
  argp_program_version : Pchar;cvar;external;
  argp_program_version_hook : TArgPProgramVersionHook;cvar;external;
  argp_program_bug_address : Pchar;cvar;external;
  argp_err_exit_status : error_t;cvar;external;

const
   ARGP_HELP_USAGE = $01;
   ARGP_HELP_SHORT_USAGE = $02;
   ARGP_HELP_SEE = $04;
   ARGP_HELP_LONG = $08;
   ARGP_HELP_PRE_DOC = $10;
   ARGP_HELP_POST_DOC = $20;
   ARGP_HELP_DOC = ARGP_HELP_PRE_DOC or ARGP_HELP_POST_DOC;
   ARGP_HELP_BUG_ADDR = $40;
   ARGP_HELP_LONG_ONLY = $80;
   ARGP_HELP_EXIT_ERR = $100;
   ARGP_HELP_EXIT_OK = $200;
   ARGP_HELP_STD_ERR = ARGP_HELP_SEE or ARGP_HELP_EXIT_ERR;
   ARGP_HELP_STD_USAGE = (ARGP_HELP_SHORT_USAGE or ARGP_HELP_SEE) or ARGP_HELP_EXIT_ERR;
   ARGP_HELP_STD_HELP = (((ARGP_HELP_SHORT_USAGE or ARGP_HELP_LONG) or ARGP_HELP_EXIT_OK) or ARGP_HELP_DOC) or ARGP_HELP_BUG_ADDR;

procedure argp_help(__argp:Pargp; __stream:PFILE; __flags:dword; __name:Pchar);cdecl;external clib name 'argp_help';
procedure __argp_help(__argp:Pargp; __stream:PFILE; __flags:dword; __name:Pchar);cdecl;external clib name '__argp_help';
procedure argp_state_help(__state:Pargp_state; __stream:PFILE; __flags:dword);cdecl;external clib name 'argp_state_help';
procedure __argp_state_help(__state:Pargp_state; __stream:PFILE; __flags:dword);cdecl;external clib name '__argp_state_help';
procedure argp_usage(__state:Pargp_state);cdecl;external clib name 'argp_usage';
procedure __argp_usage(__state:Pargp_state);cdecl;external clib name '__argp_usage';
procedure argp_error(__state:Pargp_state; __fmt:Pchar; args:array of const);cdecl;external clib name 'argp_error';
procedure argp_error(__state:Pargp_state; __fmt:Pchar);cdecl;varargs;external clib name 'argp_error';
procedure __argp_error(__state:Pargp_state; __fmt:Pchar; args:array of const);cdecl;external clib name '__argp_error';
procedure __argp_error(__state:Pargp_state; __fmt:Pchar);cdecl;varargs;external clib name '__argp_error';
procedure argp_failure(__state:Pargp_state; __status:longint; __errnum:longint; __fmt:Pchar; args:array of const);cdecl;external clib name 'argp_failure';
procedure argp_failure(__state:Pargp_state; __status:longint; __errnum:longint; __fmt:Pchar);cdecl;varargs;external clib name 'argp_failure';
procedure __argp_failure(__state:Pargp_state; __status:longint; __errnum:longint; __fmt:Pchar; args:array of const);cdecl;external clib name '__argp_failure';
procedure __argp_failure(__state:Pargp_state; __status:longint; __errnum:longint; __fmt:Pchar);cdecl;varargs;external clib name '__argp_failure';
function _option_is_short(__opt:Pargp_option):longint;cdecl;external clib name '_option_is_short';
function __option_is_short(__opt:Pargp_option):longint;cdecl;external clib name '__option_is_short';
function _option_is_end(__opt:Pargp_option):longint;cdecl;external clib name '_option_is_end';
function __option_is_end(__opt:Pargp_option):longint;cdecl;external clib name '__option_is_end';
function _argp_input(__argp:Pargp; __state:Pargp_state):pointer;cdecl;external clib name '_argp_input';
function __argp_input(__argp:Pargp; __state:Pargp_state):pointer;cdecl;external clib name '__argp_input';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TArgP = argp;

  TArgPOption = argp_option;
  PArgPOption = ^TArgPOption;

  TArgPState = argp_state;
  PArgPState = ^TArgPState;
  TArgPChild = argp_child;
  PArgPChild = ^TArgPChild;

