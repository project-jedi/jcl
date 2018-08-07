
// function __UNKNOWN_10646_CHAR : wchar_t;

Const
//  __UNKNOWN_10646_CHAR = wchar_t($fffd);

  __GCONV_OK = 0;
  __GCONV_NOCONV = 1;
  __GCONV_NODB = 2;
  __GCONV_NOMEM = 3;
  __GCONV_EMPTY_INPUT = 4;
  __GCONV_FULL_OUTPUT = 5;
  __GCONV_ILLEGAL_INPUT = 6;
  __GCONV_INCOMPLETE_INPUT = 7;
  __GCONV_ILLEGAL_DESCRIPTOR = 8;
  __GCONV_INTERNAL_ERROR = 9;

  __GCONV_IS_LAST = $0001;
  __GCONV_IGNORE_ERRORS = $0002;

type
   // Forward definitions.
   P__gconv_step = ^__gconv_step;
   P__gconv_step_data = ^__gconv_step_data;
   P__gconv_loaded_object = pointer; // ^__gconv_loaded_object;
   P__gconv_trans_data = ^__gconv_trans_data;


   __gconv_fct = function (_para1:P__gconv_step; _para2:P__gconv_step_data; _para3:PPbyte; _para4:Pbyte; _para5:PPbyte;
                _para6:Psize_t; _para7:longint; _para8:longint):longint;cdecl;

   __gconv_init_fct = function (_para1:P__gconv_step):longint;cdecl;

   __gconv_end_fct = procedure (_para1:P__gconv_step);cdecl;

   __gconv_trans_fct = function (_para1:P__gconv_step; _para2:P__gconv_step_data; _para3:pointer; _para4:Pbyte; _para5:PPbyte;
                _para6:Pbyte; _para7:PPbyte; _para8:Psize_t):longint;cdecl;

   __gconv_trans_context_fct = function (_para1:pointer; _para2:Pbyte; _para3:Pbyte; _para4:Pbyte; _para5:Pbyte):longint;cdecl;

   __gconv_trans_query_fct = function (_para1:Pchar; _para2:PPPchar; _para3:Psize_t):longint;cdecl;

   __gconv_trans_init_fct = function (_para1:Ppointer; _para2:Pchar):longint;cdecl;

   __gconv_trans_end_fct = procedure (_para1:pointer);cdecl;

   __gconv_trans_data = record
        __trans_fct : __gconv_trans_fct;
        __trans_context_fct : __gconv_trans_context_fct;
        __trans_end_fct : __gconv_trans_end_fct;
        __data : pointer;
        __next : P__gconv_trans_data;
     end;

   __gconv_step = record
        __shlib_handle : P__gconv_loaded_object;
        __modname : Pchar;
        __counter : longint;
        __from_name : Pchar;
        __to_name : Pchar;
        __fct : __gconv_fct;
        __init_fct : __gconv_init_fct;
        __end_fct : __gconv_end_fct;
        __min_needed_from : longint;
        __max_needed_from : longint;
        __min_needed_to : longint;
        __max_needed_to : longint;
        __stateful : longint;
        __data : pointer;
     end;

   __gconv_step_data = record
        __outbuf : Pbyte;
        __outbufend : Pbyte;
        __flags : longint;
        __invocation_counter : longint;
        __internal_use : longint;
        __statep : P__mbstate_t;
        __state : __mbstate_t;
        __trans : P__gconv_trans_data;
     end;


   P__gconv_info = ^__gconv_info;
   __gconv_info = record
        __nsteps : size_t;
        __steps : P__gconv_step;
     end;
   __gconv_t = P__gconv_info;
   P__gconv_t = ^__gconv_t;


{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TGConvStep = __gconv_step;
  PGConvStep = ^__gconv_step;

  TGConvStepData = __gconv_step_data;
  PGConvStepData = ^TGConvStepData;

  TGConvTransData = __gconv_trans_data;
  PGConvTransData = ^TGConvTransData;

  TGConvInfo = __gconv_info;
  PGConvInfo = ^TGConvInfo;

