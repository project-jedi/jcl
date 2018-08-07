

type
  Paiocb = ^aiocb;
  aiocb = record
       aio_fildes : longint;
       aio_lio_opcode : longint;
       aio_reqprio : longint;
       aio_buf : pointer;
       aio_nbytes : size_t;
       aio_sigevent : sigevent;
       __next_prio : Paiocb;
       __abs_prio : longint;
       __policy : longint;
       __error_code : longint;
       __return_value : __ssize_t;
       aio_offset : __off_t;
       __pad : array[0..((sizeof(__off64_t)) - (sizeof(__off_t)))-1] of char;
       __unused : array[0..31] of char;
    end;
  PPaiocb = ^Paiocb;

  Paiocb64 = ^aiocb64;
  aiocb64 = record
       aio_fildes : longint;
       aio_lio_opcode : longint;
       aio_reqprio : longint;
       aio_buf : pointer;
       aio_nbytes : size_t;
       aio_sigevent : sigevent;
       __next_prio : Paiocb;
       __abs_prio : longint;
       __policy : longint;
       __error_code : longint;
       __return_value : __ssize_t;
       aio_offset : __off64_t;
       __unused : array[0..31] of char;
    end;
  PPaiocb64 = ^Paiocb64;

  Paioinit = ^aioinit;
  aioinit = record
       aio_threads : longint;
       aio_num : longint;
       aio_locks : longint;
       aio_usedba : longint;
       aio_debug : longint;
       aio_numusers : longint;
       aio_idle_time : longint;
       aio_reserved : longint;
    end;

Const
  AIO_CANCELED = 0;
  AIO_NOTCANCELED = 1;
  AIO_ALLDONE = 2;

Const
  LIO_READ = 0;
  LIO_WRITE = 1;
  LIO_NOP = 2;

Const
  LIO_WAIT = 0;
  LIO_NOWAIT = 1;


procedure aio_init(__init:Paioinit);cdecl;external clib name 'aio_init';
function aio_read(__aiocbp:Paiocb):longint;cdecl;external clib name 'aio_read';
function aio_write(__aiocbp:Paiocb):longint;cdecl;external clib name 'aio_write';
function lio_listio(__mode:longint; __list:PPaiocb; __nent:longint; __sig:Psigevent):longint;cdecl;external clib name 'lio_listio';
function aio_error(__aiocbp:Paiocb):longint;cdecl;external clib name 'aio_error';
function aio_return(__aiocbp:Paiocb):__ssize_t;cdecl;external clib name 'aio_return';
function aio_cancel(__fildes:longint; __aiocbp:Paiocb):longint;cdecl;external clib name 'aio_cancel';
function aio_suspend(__list:array of Paiocb; __nent:longint; __timeout:Ptimespec):longint;cdecl;external clib name 'aio_suspend';
function aio_fsync(__operation:longint; __aiocbp:Paiocb):longint;cdecl;external clib name 'aio_fsync';

function aio_read64(__aiocbp:Paiocb64):longint;cdecl;external clib name 'aio_read64';
function aio_write64(__aiocbp:Paiocb64):longint;cdecl;external clib name 'aio_write64';
function lio_listio64(__mode:longint; __list:PPaiocb64; __nent:longint; __sig:Psigevent):longint;cdecl;external clib name 'lio_listio64';
function aio_error64(__aiocbp:Paiocb64):longint;cdecl;external clib name 'aio_error64';
function aio_return64(__aiocbp:Paiocb64):__ssize_t;cdecl;external clib name 'aio_return64';
function aio_cancel64(__fildes:longint; __aiocbp:Paiocb64):longint;cdecl;external clib name 'aio_cancel64';
function aio_suspend64(__list:array of Paiocb64; __nent:longint; __timeout:Ptimespec):longint;cdecl;external clib name 'aio_suspend64';
function aio_fsync64(__operation:longint; __aiocbp:Paiocb64):longint;cdecl;external clib name 'aio_fsync64';

{ ---------------------------------------------------------------------
    Borland compatibility types
  ---------------------------------------------------------------------}

Type
  TPAsyncIoCB = aiocb;
  PAsyncIoCB = ^TPAsyncIoCB;
  PPAsyncIoCB = ^PAsyncIoCB;

  PAsyncIoCB64 = ^TAsyncIoCB64;
  TAsyncIoCB64 = aiocb64;
  PPAsyncIoCB64 = ^PAsyncIoCB64;

procedure aio_init(const __init:aioinit);cdecl;external clib name 'aio_init';
function aio_read(var __aiocbp:aiocb):longint;cdecl;external clib name 'aio_read';
function aio_write(var __aiocbp:aiocb):longint;cdecl;external clib name 'aio_write';
function lio_listio(__mode:longint; __list:PPaiocb; __nent:longint; var __sig:sigevent):longint;cdecl;external clib name 'lio_listio';
function aio_error(const __aiocbp:aiocb):longint;cdecl;external clib name 'aio_error';
function aio_return(var __aiocbp:aiocb):__ssize_t;cdecl;external clib name 'aio_return';
function aio_cancel(__fildes:longint; var __aiocbp:aiocb):longint;cdecl;external clib name 'aio_cancel';
function aio_suspend(__list: PPaiocb; __nent:longint; const __timeout:timespec):longint;cdecl;external clib name 'aio_suspend';
function aio_fsync(__operation:longint;var __aiocbp:aiocb):longint;cdecl;external clib name 'aio_fsync';

function aio_read64(const __aiocbp:aiocb64):longint;cdecl;external clib name 'aio_read64';
function aio_write64(var __aiocbp: aiocb64):longint;cdecl;external clib name 'aio_write64';
function lio_listio64(__mode:longint; __list:PPaiocb64; __nent:longint;var __sig:sigevent):longint;cdecl;external clib name 'lio_listio64';
function aio_error64(const __aiocbp:aiocb64):longint;cdecl;external clib name 'aio_error64';
function aio_return64(var __aiocbp:aiocb64):__ssize_t;cdecl;external clib name 'aio_return64';
function aio_cancel64(__fildes:longint; var __aiocbp:aiocb64):longint;cdecl;external clib name 'aio_cancel64';
function aio_suspend64(__list:PPaiocb64; __nent:longint; const __timeout:timespec):longint;cdecl;external clib name 'aio_suspend64';
function aio_fsync64(__operation:longint; var __aiocbp: aiocb64):longint;cdecl;external clib name 'aio_fsync64';
