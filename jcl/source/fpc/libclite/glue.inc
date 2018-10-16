{ ---------------------------------------------------------------------
    glue.inc : Glue between libc and Pascal types
  ---------------------------------------------------------------------}

Type

  __ptr_t = Pointer;
  P__ptr_t = ^__ptr_t;
  ptrdiff_t = Integer;
  __long_double_t = Extended;
  P__long_double_t = ^__long_double_t;
  size_t = Cardinal;
  Psize_t = ^size_t;
  UInt64 = 0..High(Int64); // Must be unsigned.
  wchar_t = widechar;
  Pwchar_t = ^wchar_t;
  PPwchar_t = ^Pwchar_t;

{ ---------------------------------------------------------------------
    Declarations moved forward because C headers are a mess.
  ---------------------------------------------------------------------}

Type
  __sighandler_t = procedure(SigNum: Integer); cdecl;

  timespec = record
    tv_sec: Longint;
    tv_nsec: Longint;
  end;

  wint_t = Cardinal;
  __mbstate_t = record
    count: Integer;
    case { __value } Integer of
      0: (__wch: wint_t);
      1: (__wchb: packed array[0..4 - 1] of Char);
    end;
  mbstate_t = __mbstate_t;
  P__mbstate_t = ^__mbstate_t;

{ ---------------------------------------------------------------------
    Borland compatibility types for forward declarations.
  ---------------------------------------------------------------------}

  TSignalHandler = __sighandler_t;
  TTimeSpec = timespec;
  PTimeSpec = ^TTimeSpec;
  TMultiByteState = __mbstate_t;
  PMultiByteState = ^TMultiByteState;


