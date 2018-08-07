
type

   Ps_reg_t = ^s_reg_t;
   s_reg_t = longint;

   Pactive_reg_t = ^active_reg_t;
   active_reg_t = dword;

   Preg_syntax_t = ^reg_syntax_t;
   reg_syntax_t = dword;

Const
   RE_BACKSLASH_ESCAPE_IN_LISTS = dword(1);

const
   RE_BK_PLUS_QM = RE_BACKSLASH_ESCAPE_IN_LISTS shl 1;
   RE_CHAR_CLASSES = RE_BK_PLUS_QM shl 1;
   RE_CONTEXT_INDEP_ANCHORS = RE_CHAR_CLASSES shl 1;
   RE_CONTEXT_INDEP_OPS = RE_CONTEXT_INDEP_ANCHORS shl 1;
   RE_CONTEXT_INVALID_OPS = RE_CONTEXT_INDEP_OPS shl 1;
   RE_DOT_NEWLINE = RE_CONTEXT_INVALID_OPS shl 1;
   RE_DOT_NOT_NULL = RE_DOT_NEWLINE shl 1;
   RE_HAT_LISTS_NOT_NEWLINE = RE_DOT_NOT_NULL shl 1;
   RE_INTERVALS = RE_HAT_LISTS_NOT_NEWLINE shl 1;
   RE_LIMITED_OPS = RE_INTERVALS shl 1;
   RE_NEWLINE_ALT = RE_LIMITED_OPS shl 1;
   RE_NO_BK_BRACES = RE_NEWLINE_ALT shl 1;
   RE_NO_BK_PARENS = RE_NO_BK_BRACES shl 1;
   RE_NO_BK_REFS = RE_NO_BK_PARENS shl 1;
   RE_NO_BK_VBAR = RE_NO_BK_REFS shl 1;
   RE_NO_EMPTY_RANGES = RE_NO_BK_VBAR shl 1;
   RE_UNMATCHED_RIGHT_PAREN_ORD = RE_NO_EMPTY_RANGES shl 1;
   RE_NO_POSIX_BACKTRACKING = RE_UNMATCHED_RIGHT_PAREN_ORD shl 1;
   RE_NO_GNU_OPS = RE_NO_POSIX_BACKTRACKING shl 1;
   RE_DEBUG = RE_NO_GNU_OPS shl 1;
   RE_INVALID_INTERVAL_ORD = RE_DEBUG shl 1;

var
  re_syntax_options : reg_syntax_t;cvar;external;

const
   _RE_SYNTAX_POSIX_COMMON = (((RE_CHAR_CLASSES or RE_DOT_NEWLINE) or RE_DOT_NOT_NULL) or RE_INTERVALS) or RE_NO_EMPTY_RANGES;
   RE_SYNTAX_POSIX_EXTENDED = ((((((_RE_SYNTAX_POSIX_COMMON or RE_CONTEXT_INDEP_ANCHORS) or RE_CONTEXT_INDEP_OPS) or RE_NO_BK_BRACES) or RE_NO_BK_PARENS) or RE_NO_BK_VBAR) or RE_CONTEXT_INVALID_OPS) or RE_UNMATCHED_RIGHT_PAREN_ORD;
   RE_SYNTAX_POSIX_BASIC = _RE_SYNTAX_POSIX_COMMON or RE_BK_PLUS_QM;
   RE_SYNTAX_POSIX_MINIMAL_BASIC = _RE_SYNTAX_POSIX_COMMON or RE_LIMITED_OPS;
   RE_SYNTAX_POSIX_MINIMAL_EXTENDED = ((((((_RE_SYNTAX_POSIX_COMMON or RE_CONTEXT_INDEP_ANCHORS) or RE_CONTEXT_INVALID_OPS) or RE_NO_BK_BRACES) or RE_NO_BK_PARENS) or RE_NO_BK_REFS) or RE_NO_BK_VBAR) or RE_UNMATCHED_RIGHT_PAREN_ORD;
   RE_SYNTAX_EMACS = 0;
   RE_SYNTAX_AWK = ((((((((RE_BACKSLASH_ESCAPE_IN_LISTS or RE_DOT_NOT_NULL) or RE_NO_BK_PARENS) or RE_NO_BK_REFS) or RE_NO_BK_VBAR) or RE_NO_EMPTY_RANGES) or RE_DOT_NEWLINE) or RE_CONTEXT_INDEP_ANCHORS) or RE_UNMATCHED_RIGHT_PAREN_ORD) or RE_NO_GNU_OPS;
   RE_SYNTAX_GNU_AWK = ((RE_SYNTAX_POSIX_EXTENDED or RE_BACKSLASH_ESCAPE_IN_LISTS) or RE_DEBUG) and ( not ((RE_DOT_NOT_NULL or RE_INTERVALS) or RE_CONTEXT_INDEP_OPS));
   RE_SYNTAX_POSIX_AWK = ((RE_SYNTAX_POSIX_EXTENDED or RE_BACKSLASH_ESCAPE_IN_LISTS) or RE_INTERVALS) or RE_NO_GNU_OPS;
   RE_SYNTAX_GREP = (((RE_BK_PLUS_QM or RE_CHAR_CLASSES) or RE_HAT_LISTS_NOT_NEWLINE) or RE_INTERVALS) or RE_NEWLINE_ALT;
   RE_SYNTAX_EGREP = (((((RE_CHAR_CLASSES or RE_CONTEXT_INDEP_ANCHORS) or RE_CONTEXT_INDEP_OPS) or RE_HAT_LISTS_NOT_NEWLINE) or RE_NEWLINE_ALT) or RE_NO_BK_PARENS) or RE_NO_BK_VBAR;
   RE_SYNTAX_POSIX_EGREP = ((RE_SYNTAX_EGREP or RE_INTERVALS) or RE_NO_BK_BRACES) or RE_INVALID_INTERVAL_ORD;
   RE_SYNTAX_ED = RE_SYNTAX_POSIX_BASIC;
   RE_SYNTAX_SED = RE_SYNTAX_POSIX_BASIC;

const
   // Already in posix2_limh.inc:
   //   RE_DUP_MAX = $7fff;
   REG_EXTENDED = 1;
   REG_ICASE = REG_EXTENDED shl 1;
   REG_NEWLINE = REG_ICASE shl 1;
   REG_NOSUB = REG_NEWLINE shl 1;
   REG_NOTBOL = 1;
   REG_NOTEOL = 1 shl 1;

type
  RE_TRANSLATE_TYPE = PChar;

const
  REGS_UNALLOCATED = 0;
  REGS_REALLOCATE = 1;
  REGS_FIXED = 2;

type
   Preg_errcode_t = ^reg_errcode_t;
   reg_errcode_t =(
    REG_ENOSYS := -(1),
    REG_NOERROR := 0,
    REG_NOMATCH := 1,
    REG_BADPAT := 2,
    REG_ECOLLATE := 3,
    REG_ECTYPE := 4,
    REG_EESCAPE := 5,
    REG_ESUBREG := 6,
    REG_EBRACK := 7,
    REG_EPAREN := 8,
    REG_EBRACE := 9,
    REG_BADBR := 10,
    REG_ERANGE := 11,
    REG_ESPACE := 12,
    REG_BADRPT := 13,
    REG_EEND := 14,
    REG_ESIZE := 15,
    REG_ERPAREN := 16);

type
  Pre_pattern_buffer = ^re_pattern_buffer;
  re_pattern_buffer = record
    buffer: PByte;
    allocated: LongWord;
    used: LongWord;
    syntax: reg_syntax_t;
    fastmap: PChar;
    translate: RE_TRANSLATE_TYPE;
    re_nsub: size_t;
    __bitfield: Cardinal; (* In this case 8 bits use 32 bits *)
  end;
  regex_t = re_pattern_buffer;
  pregex_t = ^regex_t;

  Pregoff_t = ^regoff_t;
  regoff_t = longint;
  Pre_registers = ^re_registers;
  re_registers = record
     num_regs : dword;
     start : Pregoff_t;
     end_ : Pregoff_t;
  end;

const
  RE_NREGS = 30;

type
  Pregmatch_t = ^regmatch_t;
  regmatch_t = record
    rm_so : regoff_t;
    rm_eo : regoff_t;
  end;

function re_set_syntax(syntax:reg_syntax_t):reg_syntax_t;cdecl;external clib name 're_set_syntax';
function re_compile_pattern(pattern:Pchar; length:size_t; buffer:Pre_pattern_buffer):Pchar;cdecl;external clib name 're_compile_pattern';
function re_compile_fastmap(buffer:Pre_pattern_buffer):longint;cdecl;external clib name 're_compile_fastmap';
function re_search(buffer:Pre_pattern_buffer; _string:Pchar; length:longint; start:longint; range:longint;
           regs:Pre_registers):longint;cdecl;external clib name 're_search';
function re_search_2(buffer:Pre_pattern_buffer; string1:Pchar; length1:longint; string2:Pchar; length2:longint;
           start:longint; range:longint; regs:Pre_registers; stop:longint):longint;cdecl;external clib name 're_search_2';
function re_match(buffer:Pre_pattern_buffer; _string:Pchar; length:longint; start:longint; regs:Pre_registers):longint;cdecl;external clib name 're_match';
function re_match_2(buffer:Pre_pattern_buffer; string1:Pchar; length1:longint; string2:Pchar; length2:longint;
           start:longint; regs:Pre_registers; stop:longint):longint;cdecl;external clib name 're_match_2';
procedure re_set_registers(buffer:Pre_pattern_buffer; regs:Pre_registers; num_regs:dword; starts:Pregoff_t; ends:Pregoff_t);cdecl;external clib name 're_set_registers';
function regcomp(__preg:Pregex_t; __pattern:Pchar; __cflags:longint):longint;cdecl;external clib name 'regcomp';
function regexec(__preg:Pregex_t; __string:Pchar; __nmatch:size_t; __pmatch:array of regmatch_t; __eflags:longint):longint;cdecl;external clib name 'regexec';
function regerror(__errcode:longint; __preg:Pregex_t; __errbuf:Pchar; __errbuf_size:size_t):size_t;cdecl;external clib name 'regerror';
procedure regfree(__preg:Pregex_t);cdecl;external clib name 'regfree';


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TRePatternBuffer = re_pattern_buffer;
  PRePatternBuffer = ^TRePatternBuffer;

  TRegOff = regoff_t;
  PRegOff = ^TRegOff;

  TReRegisters = re_registers;
  PReRegisters = ^TReRegisters;

  TRegEx = regex_t;
  PRegEx = ^TRegEx;

  TRegMatch = regmatch_t;
  PRegMatch = ^TRegMatch;

function re_compile_pattern(pattern:Pchar; length:size_t; var buffer: re_pattern_buffer):Pchar;cdecl;external clib name 're_compile_pattern';
function re_compile_fastmap(var buffer: re_pattern_buffer):longint;cdecl;external clib name 're_compile_fastmap';
function re_search(var buffer:re_pattern_buffer; _string:Pchar; length:longint; start:longint; range:longint; regs:Pre_registers):longint;cdecl;external clib name 're_search';
function re_search(var buffer:re_pattern_buffer; _string:Pchar; length:longint; start:longint; range:longint; var regs:re_registers):longint;cdecl;external clib name 're_search';
function re_search_2(var buffer:re_pattern_buffer; string1:Pchar; length1:longint; string2:Pchar; length2:longint; start:longint; range:longint; regs:Pre_registers; stop:longint):longint;cdecl;external clib name 're_search_2';
function re_search_2(var buffer:re_pattern_buffer; string1:Pchar; length1:longint; string2:Pchar; length2:longint; start:longint; range:longint; var regs:re_registers; stop:longint):longint;cdecl;external clib name 're_search_2';
function re_match(var buffer:re_pattern_buffer; _string:Pchar; length:longint; start:longint; regs:Pre_registers):longint;cdecl;external clib name 're_match';
function re_match(var buffer:re_pattern_buffer; _string:Pchar; length:longint; start:longint; var regs:re_registers):longint;cdecl;external clib name 're_match';
function re_match_2(var buffer:re_pattern_buffer; string1:Pchar; length1:longint; string2:Pchar; length2:longint; start:longint; regs:Pre_registers; stop:longint):longint;cdecl;external clib name 're_match_2';
function re_match_2(var buffer:re_pattern_buffer; string1:Pchar; length1:longint; string2:Pchar; length2:longint; start:longint; var regs:re_registers; stop:longint):longint;cdecl;external clib name 're_match_2';
procedure re_set_registers(var buffer:re_pattern_buffer; var regs:re_registers; num_regs:dword; var starts:regoff_t; var ends:regoff_t);cdecl;external clib name 're_set_registers';
function regcomp(var __preg:regex_t; __pattern:Pchar; __cflags:longint):longint;cdecl;external clib name 'regcomp';
function regexec(var __preg:regex_t; __string:Pchar; __nmatch:size_t; __pmatch: Pregmatch_t; __eflags:longint):longint;cdecl;external clib name 'regexec';
function regerror(__errcode:longint; const __preg:regex_t; __errbuf:Pchar; __errbuf_size:size_t):size_t;cdecl;external clib name 'regerror';
procedure regfree(var __preg:regex_t);cdecl;external clib name 'regfree';

