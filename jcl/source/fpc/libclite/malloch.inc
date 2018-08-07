
var
  __malloc_initialized : longint;cvar;external;

function malloc(__size:size_t):pointer;cdecl;external clib name 'malloc';
function calloc(__nmemb:size_t; __size:size_t):pointer;cdecl;external clib name 'calloc';
function realloc(__ptr:pointer; __size:size_t):pointer;cdecl;external clib name 'realloc';
procedure free(__ptr:pointer);cdecl;external clib name 'free';
procedure cfree(__ptr:pointer);cdecl;external clib name 'cfree';
function memalign(__alignment:size_t; __size:size_t):pointer;cdecl;external clib name 'memalign';
function valloc(__size:size_t):pointer;cdecl;external clib name 'valloc';
function pvalloc(__size:size_t):pointer;cdecl;external clib name 'pvalloc';

var
  __morecore : function (__size:ptrdiff_t):pointer;cvar;external;

function __default_morecore(__size:ptrdiff_t):pointer;cdecl;external clib name '__default_morecore';

type
  Pmallinfo = ^_mallinfo;
  _mallinfo = record
    arena : longint;
    ordblks : longint;
    smblks : longint;
    hblks : longint;
    hblkhd : longint;
    usmblks : longint;
    fsmblks : longint;
    uordblks : longint;
    fordblks : longint;
    keepcost : longint;
  end;

// not sure about this one ? Returns struct value??
function mallinfo:_mallinfo;cdecl;external clib name 'mallinfo';

const
  M_MXFAST  = 1;
  M_NLBLKS  = 2;
  M_GRAIN   = 3;
  M_KEEP    = 4;

  M_TRIM_THRESHOLD    = -1;
  M_TOP_PAD           = -2;
  M_MMAP_THRESHOLD    = -3;
  M_MMAP_MAX          = -4;
  M_CHECK_ACTION      = -5;


function mallopt(__param:longint; __val:longint):longint;cdecl;external clib name 'mallopt';
function malloc_trim(__pad:size_t):longint;cdecl;external clib name 'malloc_trim';
function malloc_usable_size(__ptr:pointer):size_t;cdecl;external clib name 'malloc_usable_size';
procedure malloc_stats;cdecl;external clib name 'malloc_stats';
function malloc_get_state:pointer;cdecl;external clib name 'malloc_get_state';
function malloc_set_state(__ptr:pointer):longint;cdecl;external clib name 'malloc_set_state';

var
  __malloc_initialize_hook : procedure ;cvar;external;
  __free_hook : procedure (__ptr:pointer; _para2:pointer);cvar;external;
  __malloc_hook : function (__size:size_t; _para2:pointer):pointer;cvar;external;
  __realloc_hook : function (__ptr:pointer; __size:size_t; _para3:pointer):pointer;cvar;external;
  __memalign_hook : function (__alignment:size_t; __size:size_t; _para3:pointer):pointer;cvar;external;
  __after_morecore_hook : procedure ;cvar;external;

procedure __malloc_check_init;cdecl;external clib name '__malloc_check_init';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TMallocInfo = _mallinfo;
  PMallocInfo = ^TMallocInfo;


