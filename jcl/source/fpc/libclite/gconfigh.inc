
type
   Pwint_t = ^wint_t;

   _G_size_t = size_t;

   P_G_fpos_t = ^_G_fpos_t;
   _G_fpos_t = record
        __pos : __off_t;
        __state : __mbstate_t;
     end;

   P_G_fpos64_t = ^_G_fpos64_t;
   _G_fpos64_t = record
        __pos : __off64_t;
        __state : __mbstate_t;
     end;

   _G_ssize_t = __ssize_t;
   _G_off_t = __off_t;
   _G_off64_t = __off64_t;
   _G_pid_t = __pid_t;
   _G_uid_t = __uid_t;
   _G_wchar_t = wchar_t;
   _G_wint_t = wint_t;
   _G_stat64 = _stat64;
type

   P_G_iconv_t = ^_G_iconv_t;
   _G_iconv_t = record
       case longint of
          0 : ( __cd : __gconv_info );
          1 : ( __combined : record
               __cd : __gconv_info;
               __data : __gconv_step_data;
            end );
       end;

const
   _G_HAVE_BOOL = 1;
   _G_HAVE_ATEXIT = 1;
   _G_HAVE_SYS_CDEFS = 1;
   _G_HAVE_SYS_WAIT = 1;
   _G_NEED_STDARG_H = 1;
   _G_HAVE_PRINTF_FP = 1;
   _G_HAVE_MMAP = 1;
   _G_HAVE_LONG_DOUBLE_IO = 1;
   _G_HAVE_IO_FILE_OPEN = 1;
   _G_HAVE_IO_GETLINE_INFO = 1;
   _G_IO_IO_FILE_VERSION = $20001;

   _G_BUFSIZ = 8192;
   _G_NAMES_HAVE_UNDERSCORE = 0;
   _G_VTABLE_LABEL_HAS_LENGTH = 1;
   _G_USING_THUNKS = 1;
   _G_VTABLE_LABEL_PREFIX = '__vt_';
   _G_VTABLE_LABEL_PREFIX_ID = '__vt_';

Type
   _G_va_list = Pointer;

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

// Type
