{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

{******************************************************************************}
{                                                                              }
{  Delphi zlib 1.2.1 API Interface Unit                                        }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS" basis,  }
{  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License    }
{  for the specific language governing rights and limitations under the        }
{  License.                                                                    }
{                                                                              }
{  The Original Code is: zlib.h, released 2003-11-17.                          }
{  The Initial Developer of the Original Code are Jean-loup Gailly             }
{  (jloup att gzip dott org) and Mark Adler                                    }
{  (madler att alumni dott cal­tech dott edu).                                 }
{  Portions created by Jean-loup Gailly and Mark Adler are Copyright (C)       }
{  1995-2003 Jean-loup Gailly and Mark Adler. All Rights Reserved.             }
{                                                                              }
{  The Original Pascal code is: zlibh.pas.                                     }
{  The Initial Developer of the Original Pascal code is Peter J. Haas.         }
{  Portions created by Peter J. Haas are Copyright (C) 2002-2004               }
{  Peter J. Haas. All Rights Reserved.                                         }
{                                                                              }
{  Obtained through:                                                           }
{    Joint Endeavour of Delphi Innovators (Project JEDI)                       }
{                                                                              }
{  You may retrieve the latest version of the original file at the homepage    }
{  of the Project ZLib, located at http://www.gzip.org/zlib/                   }
{                                                                              }
{  You may retrieve the latest version of this file at the homepage of         }
{  JEDI+ (jediplus att pjh2 dott de), located at http://jediplus.pjh2.de/      }
{                                                                              }
{------------------------------------------------------------------------------}
{                                                                              }
{  NOTE: As of 2004-05-15, Peter J. Haas has stopped maintaining code he       }
{        donated to the JCL. He is not to be held responsible for              }
{        modifications applied after this date.                                }
{        Peter J. Haas no longer wants to be associated with Project JEDI.     }
{                                                                              }
{------------------------------------------------------------------------------}
{                                                                              }
{  Contributor(s):                                                             }
{    Matthias Thoma (mthoma), ma dott thoma att gmx dott de                    }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the GNU Lesser General Public License (the  "LGPL License"), in which case  }
{  the provisions of the LGPL License are applicable instead of those above.   }
{  If you wish to allow use of your version of this file only under the terms  }
{  of the LGPL License and not to allow others to use your version of this     }
{  file under the MPL, indicate your decision by deleting the provisions       }
{  above and replace them with the notice and other provisions required by     }
{  the LGPL License. If you do not delete the provisions above, a recipient    }
{  may use your version of this file under either the MPL or the LGPL License. }
{                                                                              }
{  For more information about the LGPL:                                        }
{  http://www.gnu.org/copyleft/lesser.html                                     }
{                                                                              }
{******************************************************************************}

// Last modified: $Date$
// For history see end of file

{$ALIGN ON}{$BOOLEVAL OFF}{$LONGSTRINGS ON}{$IOCHECKS ON}{$WRITEABLECONST OFF}
{$OVERFLOWCHECKS OFF}{$RANGECHECKS OFF}{$TYPEDADDRESS ON}{$MINENUMSIZE 1}

{$I jedi.inc}

{$IFDEF SUPPORTS_UNSAFE_WARNINGS}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF SUPPORTS_UNSAFE_WARNINGS}

unit zlibh;




{$IFDEF SUPPORTS_WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF SUPPORTS_WEAKPACKAGEUNIT}

interface

uses
  Windows;

{$HPPEMIT '#define ZEXPORT __fastcall'}
{$HPPEMIT '#define ZEXPORTVA __cdecl'}
{$HPPEMIT ''}
{$HPPEMIT '#include <zutil.h>'  // zutil.h include zlib.h }

{ zlib.h -- interface of the 'zlib' general purpose compression library
  version 1.2.1, November 17th, 2003

  Copyright (C) 1995-2003 Jean-loup Gailly and Mark Adler

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Jean-loup Gailly          Mark Adler
  jloup att gzip dott org   madler att alumni dott caltech dott edu


  The data format used by the zlib library is described by RFCs (Request for
  Comments) 1950 to 1952 in the files http://www.ietf.org/rfc/rfc1950.txt
  (zlib format), rfc1951.txt (deflate format) and rfc1952.txt (gzip format). }

// converted from zconf.h
const
  {$EXTERNALSYM SEEK_SET}
  SEEK_SET = 0;  // Seek from beginning of file.
  {$EXTERNALSYM SEEK_CUR}
  SEEK_CUR = 1;  // Seek from current position.
  {$EXTERNALSYM SEEK_END}
  SEEK_END = 2;  // Set file pointer to EOF plus "offset"

  {$EXTERNALSYM DEF_MEM_LEVEL}
  DEF_MEM_LEVEL = 8;

  {$EXTERNALSYM MAX_WBITS}
  MAX_WBITS = 15;  // 32K LZ77 window

  {$EXTERNALSYM DEF_WBITS}
  DEF_WBITS = MAX_WBITS;

type
  {$EXTERNALSYM z_off_t}
  z_off_t = LongInt;
  TZOff = z_off_t;

type
  {$EXTERNALSYM PCRCTable}
  PCRCTable = ^TCRCTable;
  {$EXTERNALSYM TCRCTable}
  TCRCTable = array [0..255] of ULong;

const
  {$EXTERNALSYM ZLIB_VERSION}
  ZLIB_VERSION = '1.2.1';
  {$EXTERNALSYM ZLIB_VERNUM}
  ZLIB_VERNUM = $1210;

{ The 'zlib' compression library provides in-memory compression and
  decompression functions, including integrity checks of the uncompressed
  data.  This version of the library supports only one compression method
  (deflation) but other algorithms will be added later and will have the same
  stream interface.

  Compression can be done in a single step if the buffers are large
  enough (for example if an input file is mmap'ed), or can be done by
  repeated calls of the compression function.  In the latter case, the
  application must provide more input and/or consume the output
  (providing more output space) before each call.

     The compressed data format used by the in-memory functions is the zlib
  format, which is a zlib wrapper documented in RFC 1950, wrapped around a
  deflate stream, which is itself documented in RFC 1951.

     The library also supports reading and writing files in gzip (.gz) format
  with an interface similar to that of stdio using the functions that start
  with "gz".  The gzip format is different from the zlib format.  gzip is a
  gzip wrapper, documented in RFC 1952, wrapped around a deflate stream.

     The zlib format was designed to be compact and fast for use in memory
  and on communications channels.  The gzip format was designed for single-
  file compression on file systems, has a larger header than zlib to maintain
  directory information, and uses a different, slower check method than zlib.

     This library does not provide any functions to write gzip files in memory.
  However such functions could be easily written using zlib's deflate function,
  the documentation in the gzip RFC, and the examples in gzio.c.

  The library does not install any signal handler. The decoder checks
  the consistency of the compressed data, so the library should never
  crash even in case of corrupted input. }

type
  TAllocFunc = function(opaque: Pointer; items, size: UInt): Pointer;
  TFreeFunc = procedure(opaque, address: Pointer);

  PInternalState = ^TInternalState;
  {$EXTERNALSYM internal_state}
  internal_state = packed record end;
  TInternalState = internal_state;

  PZStreamRec = ^TZStreamRec;
  {$EXTERNALSYM z_stream_s}
  z_stream_s = packed record
    next_in: PChar;         // next input byte
    avail_in: UInt;         // number of bytes available at next_in
    total_in: ULong;        // total nb of input bytes read so far

    next_out: PChar;        // next output byte should be put there
    avail_out: UInt;        // remaining free space at next_out
    total_out: ULong;       // total nb of bytes output so far

    msg: PChar;             // last error message, NULL if no error
    state: PInternalState;  // not visible by applications

    zalloc: TAllocFunc;     // used to allocate the internal state
    zfree: TFreeFunc;       // used to free the internal state
    opaque: Pointer;        // private data object passed to zalloc and zfree

    data_type: Integer;     // best guess about the data type: ascii or binary
    adler: ULong;           // adler32 value of the uncompressed data
    reserved: ULong;        // reserved for future use
  end;

  TZStreamRec = z_stream_s;
  {$EXTERNALSYM z_stream}
  z_stream = z_stream_s;
  {$EXTERNALSYM z_streamp}
  z_streamp = PZStreamRec;


{ The application must update next_in and avail_in when avail_in has
  dropped to zero. It must update next_out and avail_out when avail_out
  has dropped to zero. The application must initialize zalloc, zfree and
  opaque before calling the init function. All other fields are set by the
  compression library and must not be updated by the application.

  The opaque value provided by the application will be passed as the first
  parameter for calls of zalloc and zfree. This can be useful for custom
  memory management. The compression library attaches no meaning to the
  opaque value.

  zalloc must return Z_NULL if there is not enough memory for the object.
  If zlib is used in a multi-threaded application, zalloc and zfree must be
  thread safe.

  On 16-bit systems, the functions zalloc and zfree must be able to allocate
  exactly 65536 bytes, but will not be required to allocate more than this
  if the symbol MAXSEG_64K is defined (see zconf.h). WARNING: On MSDOS,
  pointers returned by zalloc for objects of exactly 65536 bytes *must*
  have their offset normalized to zero. The default allocation function
  provided by this library ensures this (see zutil.c). To reduce memory
  requirements and avoid any allocation of 64K objects, at the expense of
  compression ratio, compile the library with -DMAX_WBITS=14 (see zconf.h).

  The fields total_in and total_out can be used for statistics or
  progress reports. After compression, total_in holds the total size of
  the uncompressed data and may be saved for use in the decompressor
  (particularly if the decompressor wants to decompress everything in
  a single step). }

//                        constants
const
  // Allowed flush values; see deflate() and inflate() below for details
  {$EXTERNALSYM Z_NO_FLUSH}
  Z_NO_FLUSH      = 0;
  {$EXTERNALSYM Z_PARTIAL_FLUSH}
  Z_PARTIAL_FLUSH = 1;  // will be removed, use Z_SYNC_FLUSH instead
  {$EXTERNALSYM Z_SYNC_FLUSH}
  Z_SYNC_FLUSH    = 2;
  {$EXTERNALSYM Z_FULL_FLUSH}
  Z_FULL_FLUSH    = 3;
  {$EXTERNALSYM Z_FINISH}
  Z_FINISH        = 4;
  {$EXTERNALSYM Z_BLOCK}
  Z_BLOCK         = 5;

  // Return codes for the compression/decompression functions. Negative
  // values are errors, positive values are used for special but normal events.
  {$EXTERNALSYM Z_OK}
  Z_OK            =  0;
  {$EXTERNALSYM Z_STREAM_END}
  Z_STREAM_END    =  1;
  {$EXTERNALSYM Z_NEED_DICT}
  Z_NEED_DICT     =  2;
  {$EXTERNALSYM Z_ERRNO}
  Z_ERRNO         = -1;
  {$EXTERNALSYM Z_STREAM_ERROR}
  Z_STREAM_ERROR  = -2;
  {$EXTERNALSYM Z_DATA_ERROR}
  Z_DATA_ERROR    = -3;
  {$EXTERNALSYM Z_MEM_ERROR}
  Z_MEM_ERROR     = -4;
  {$EXTERNALSYM Z_BUF_ERROR}
  Z_BUF_ERROR     = -5;
  {$EXTERNALSYM Z_VERSION_ERROR}
  Z_VERSION_ERROR = -6;

  // compression levels
  {$EXTERNALSYM Z_NO_COMPRESSION}
  Z_NO_COMPRESSION      =  0;
  {$EXTERNALSYM Z_BEST_SPEED}
  Z_BEST_SPEED          =  1;
  {$EXTERNALSYM Z_BEST_COMPRESSION}
  Z_BEST_COMPRESSION    =  9;
  {$EXTERNALSYM Z_DEFAULT_COMPRESSION}
  Z_DEFAULT_COMPRESSION = -1;

  // compression strategy; see deflateInit2() below for details
  {$EXTERNALSYM Z_FILTERED}
  Z_FILTERED         = 1;
  {$EXTERNALSYM Z_HUFFMAN_ONLY}
  Z_HUFFMAN_ONLY     = 2;
  {$EXTERNALSYM Z_RLE}
  Z_RLE              = 3;
  {$EXTERNALSYM Z_DEFAULT_STRATEGY}
  Z_DEFAULT_STRATEGY = 0;

  // Possible values of the data_type field (though see inflate())
  {$EXTERNALSYM Z_BINARY}
  Z_BINARY  = 0;
  {$EXTERNALSYM Z_ASCII}
  Z_ASCII   = 1;
  {$EXTERNALSYM Z_UNKNOWN}
  Z_UNKNOWN = 2;

  // The deflate compression method (the only one supported in this version)
  {$EXTERNALSYM Z_DEFLATED}
  Z_DEFLATED = 8;

  // for initializing zalloc, zfree, opaque
  {$EXTERNALSYM Z_NULL}
  Z_NULL = nil;


//                         basic functions

{ zlibVersion
  The application can compare zlibVersion and ZLIB_VERSION for consistency.
  If the first character differs, the library code actually used is
  not compatible with the zlib.h header file used by the application.
  This check is automatically made by deflateInit and inflateInit. }
{$EXTERNALSYM zlibVersion}
function zlibVersion: PChar;


{ deflateInit
  Initializes the internal stream state for compression. The fields
  zalloc, zfree and opaque must be initialized before by the caller.
  If zalloc and zfree are set to Z_NULL, deflateInit updates them to
  use default allocation functions.

  The compression level must be Z_DEFAULT_COMPRESSION, or between 0 and 9:
  1 gives best speed, 9 gives best compression, 0 gives no compression at
  all (the input data is simply copied a block at a time).
  Z_DEFAULT_COMPRESSION requests a default compromise between speed and
  compression (currently equivalent to level 6).

  deflateInit returns Z_OK if success, Z_MEM_ERROR if there was not
  enough memory, Z_STREAM_ERROR if level is not a valid compression level,
  Z_VERSION_ERROR if the zlib library version (zlib_version) is incompatible
  with the version assumed by the caller (ZLIB_VERSION).
  msg is set to null if there is no error message.  deflateInit does not
  perform any compression: this will be done by deflate(). }
{$EXTERNALSYM deflateInit}
function deflateInit(var strm: TZStreamRec; level: Integer): Integer;  // macro


{ deflate
  deflate compresses as much data as possible, and stops when the input
  buffer becomes empty or the output buffer becomes full. It may introduce some
  output latency (reading input without producing any output) except when
  forced to flush.

  The detailed semantics are as follows. deflate performs one or both of the
  following actions:

  - Compress more input starting at next_in and update next_in and avail_in
    accordingly. If not all input can be processed (because there is not
    enough room in the output buffer), next_in and avail_in are updated and
    processing will resume at this point for the next call of deflate().

  - Provide more output starting at next_out and update next_out and avail_out
    accordingly. This action is forced if the parameter flush is non zero.
    Forcing flush frequently degrades the compression ratio, so this parameter
    should be set only when necessary (in interactive applications).
    Some output may be provided even if flush is not set.

  Before the call of deflate(), the application should ensure that at least
  one of the actions is possible, by providing more input and/or consuming
  more output, and updating avail_in or avail_out accordingly; avail_out
  should never be zero before the call. The application can consume the
  compressed output when it wants, for example when the output buffer is full
  (avail_out == 0), or after each call of deflate(). If deflate returns Z_OK
  and with zero avail_out, it must be called again after making room in the
  output buffer because there might be more output pending.

  If the parameter flush is set to Z_SYNC_FLUSH, all pending output is flushed
  to the output buffer and the output is aligned on a byte boundary, so that
  the decompressor can get all input data available so far. (In particular
  avail_in is zero after the call if enough output space has been provided
  before the call.)  Flushing may degrade compression for some compression
  algorithms and so it should be used only when necessary.

  If flush is set to Z_FULL_FLUSH, all output is flushed as with Z_SYNC_FLUSH,
  and the compression state is reset so that decompression can restart from
  this point if previous compressed data has been damaged or if random access
  is desired. Using Z_FULL_FLUSH too often can seriously degrade the
  compression.

  If deflate returns with avail_out == 0, this function must be called again
  with the same value of the flush parameter and more output space (updated
  avail_out), until the flush is complete (deflate returns with non-zero
  avail_out). In the case of a Z_FULL_FLUSH or Z_SYNC_FLUSH, make sure that
  avail_out is greater than six to avoid repeated flush markers due to
  avail_out == 0 on return.

  If the parameter flush is set to Z_FINISH, pending input is processed,
  pending output is flushed and deflate returns with Z_STREAM_END if there
  was enough output space; if deflate returns with Z_OK, this function must be
  called again with Z_FINISH and more output space (updated avail_out) but no
  more input data, until it returns with Z_STREAM_END or an error. After
  deflate has returned Z_STREAM_END, the only possible operations on the
  stream are deflateReset or deflateEnd.

  Z_FINISH can be used immediately after deflateInit if all the compression
  is to be done in a single step. In this case, avail_out must be at least
  the value returned by deflateBound (see below). If deflate does not return
  Z_STREAM_END, then it must be called again as described above.

  deflate() sets strm->adler to the adler32 checksum of all input read
  so far (that is, total_in bytes).

  deflate() may update data_type if it can make a good guess about
  the input data type (Z_ASCII or Z_BINARY). In doubt, the data is considered
  binary. This field is only for information purposes and does not affect
  the compression algorithm in any manner.

  deflate() returns Z_OK if some progress has been made (more input
  processed or more output produced), Z_STREAM_END if all input has been
  consumed and all output has been produced (only when flush is set to
  Z_FINISH), Z_STREAM_ERROR if the stream state was inconsistent (for example
  if next_in or next_out was NULL), Z_BUF_ERROR if no progress is possible
  (for example avail_in or avail_out was zero). Note that Z_BUF_ERROR is not
  fatal, and deflate() can be called again with more input and more output
  space to continue compressing. }
{$EXTERNALSYM deflate}
function deflate(var strm: TZStreamRec; flush: Integer): Integer;

{ deflateEnd
  All dynamically allocated data structures for this stream are freed.
  This function discards any unprocessed input and does not flush any
  pending output.

  deflateEnd returns Z_OK if success, Z_STREAM_ERROR if the
  stream state was inconsistent, Z_DATA_ERROR if the stream was freed
  prematurely (some input or output was discarded). In the error case,
  msg may be set but then points to a static string (which must not be
  deallocated). }
{$EXTERNALSYM deflateEnd}
function deflateEnd(var strm: TZStreamRec): Integer;

{ inflateInit
  Initializes the internal stream state for decompression. The fields
  next_in, avail_in, zalloc, zfree and opaque must be initialized before by
  the caller. If next_in is not Z_NULL and avail_in is large enough (the exact
  value depends on the compression method), inflateInit determines the
  compression method from the zlib header and allocates all data structures
  accordingly; otherwise the allocation will be deferred to the first call of
  inflate.  If zalloc and zfree are set to Z_NULL, inflateInit updates them to
  use default allocation functions.

    inflateInit returns Z_OK if success, Z_MEM_ERROR if there was not enough
  memory, Z_VERSION_ERROR if the zlib library version is incompatible with the
  version assumed by the caller.  msg is set to null if there is no error
  message. inflateInit does not perform any decompression apart from reading
  the zlib header if present: this will be done by inflate().  (So next_in and
  avail_in may be modified, but next_out and avail_out are unchanged.) }
{$EXTERNALSYM inflateInit}
function inflateInit(var strm: TZStreamRec): Integer;  // macro


{ inflate
  inflate decompresses as much data as possible, and stops when the input
  buffer becomes empty or the output buffer becomes full. It may introduce
  some output latency (reading input without producing any output) except when
  forced to flush.

  The detailed semantics are as follows. inflate performs one or both of the
  following actions:

  - Decompress more input starting at next_in and update next_in and avail_in
    accordingly. If not all input can be processed (because there is not
    enough room in the output buffer), next_in is updated and processing
    will resume at this point for the next call of inflate().

  - Provide more output starting at next_out and update next_out and avail_out
    accordingly.  inflate() provides as much output as possible, until there
    is no more input data or no more space in the output buffer (see below
    about the flush parameter).

  Before the call of inflate(), the application should ensure that at least
  one of the actions is possible, by providing more input and/or consuming
  more output, and updating the next_* and avail_* values accordingly.
  The application can consume the uncompressed output when it wants, for
  example when the output buffer is full (avail_out == 0), or after each
  call of inflate(). If inflate returns Z_OK and with zero avail_out, it
  must be called again after making room in the output buffer because there
  might be more output pending.

  The flush parameter of inflate() can be Z_NO_FLUSH, Z_SYNC_FLUSH,
  Z_FINISH, or Z_BLOCK. Z_SYNC_FLUSH requests that inflate() flush as much
  output as possible to the output buffer. Z_BLOCK requests that inflate() stop
  if and when it get to the next deflate block boundary. When decoding the zlib
  or gzip format, this will cause inflate() to return immediately after the
  header and before the first block. When doing a raw inflate, inflate() will
  go ahead and process the first block, and will return when it gets to the end
  of that block, or when it runs out of data.

  The Z_BLOCK option assists in appending to or combining deflate streams.
  Also to assist in this, on return inflate() will set strm->data_type to the
  number of unused bits in the last byte taken from strm->next_in, plus 64
  if inflate() is currently decoding the last block in the deflate stream,
  plus 128 if inflate() returned immediately after decoding an end-of-block
  code or decoding the complete header up to just before the first byte of the
  deflate stream. The end-of-block will not be indicated until all of the
  uncompressed data from that block has been written to strm->next_out.  The
  number of unused bits may in general be greater than seven, except when
  bit 7 of data_type is set, in which case the number of unused bits will be
  less than eight.

  inflate() should normally be called until it returns Z_STREAM_END or an
  error. However if all decompression is to be performed in a single step
  (a single call of inflate), the parameter flush should be set to
  Z_FINISH. In this case all pending input is processed and all pending
  output is flushed; avail_out must be large enough to hold all the
  uncompressed data. (The size of the uncompressed data may have been saved
  by the compressor for this purpose.) The next operation on this stream must
  be inflateEnd to deallocate the decompression state. The use of Z_FINISH
  is never required, but can be used to inform inflate that a faster approach
  may be used for the single inflate() call.

  In this implementation, inflate() always flushes as much output as
  possible to the output buffer, and always uses the faster approach on the
  first call. So the only effect of the flush parameter in this implementation
  is on the return value of inflate(), as noted below, or when it returns early
  because Z_BLOCK is used.

  If a preset dictionary is needed after this call (see inflateSetDictionary
  below), inflate sets strm-adler to the adler32 checksum of the dictionary
  chosen by the compressor and returns Z_NEED_DICT; otherwise it sets
  strm->adler to the adler32 checksum of all output produced so far (that is,
  total_out bytes) and returns Z_OK, Z_STREAM_END or an error code as described
  below. At the end of the stream, inflate() checks that its computed adler32
  checksum is equal to that saved by the compressor and returns Z_STREAM_END
  only if the checksum is correct.

  inflate() will decompress and check either zlib-wrapped or gzip-wrapped
  deflate data.  The header type is detected automatically.  Any information
  contained in the gzip header is not retained, so applications that need that
  information should instead use raw inflate, see inflateInit2() below, or
  inflateBack() and perform their own processing of the gzip header and
  trailer.

  inflate() returns Z_OK if some progress has been made (more input processed
  or more output produced), Z_STREAM_END if the end of the compressed data has
  been reached and all uncompressed output has been produced, Z_NEED_DICT if a
  preset dictionary is needed at this point, Z_DATA_ERROR if the input data was
  corrupted (input stream not conforming to the zlib format or incorrect check
  value), Z_STREAM_ERROR if the stream structure was inconsistent (for example
  if next_in or next_out was NULL), Z_MEM_ERROR if there was not enough memory,
  Z_BUF_ERROR if no progress is possible or if there was not enough room in the
  output buffer when Z_FINISH is used. Note that Z_BUF_ERROR is not fatal, and
  inflate() can be called again with more input and more output space to
  continue decompressing. If Z_DATA_ERROR is returned, the application may then
  call inflateSync() to look for a good compression block if a partial recovery
  of the data is desired. }
{$EXTERNALSYM inflate}
function inflate(var strm: TZStreamRec; flush: Integer): Integer;

{ inflateEnd
  All dynamically allocated data structures for this stream are freed.
  This function discards any unprocessed input and does not flush any
  pending output.

  inflateEnd returns Z_OK if success, Z_STREAM_ERROR if the stream state
  was inconsistent. In the error case, msg may be set but then points to a
  static string (which must not be deallocated). }
{$EXTERNALSYM inflateEnd}
function inflateEnd(var strm: TZStreamRec): Integer;

//                         Advanced functions

// The following functions are needed only in some special applications.

{ deflateInit2
  This is another version of deflateInit with more compression options. The
  fields next_in, zalloc, zfree and opaque must be initialized before by
  the caller.

  The method parameter is the compression method. It must be Z_DEFLATED in
  this version of the library.

  The windowBits parameter is the base two logarithm of the window size
  (the size of the history buffer).  It should be in the range 8..15 for this
  version of the library. Larger values of this parameter result in better
  compression at the expense of memory usage. The default value is 15 if
  deflateInit is used instead.

  windowBits can also be -8..-15 for raw deflate. In this case, -windowBits
  determines the window size. deflate() will then generate raw deflate data
  with no zlib header or trailer, and will not compute an adler32 check value.

  windowBits can also be greater than 15 for optional gzip encoding. Add
  16 to windowBits to write a simple gzip header and trailer around the
  compressed data instead of a zlib wrapper. The gzip header will have no
  file name, no extra data, no comment, no modification time (set to zero),
  no header crc, and the operating system will be set to 255 (unknown).

  The memLevel parameter specifies how much memory should be allocated
  for the internal compression state. memLevel=1 uses minimum memory but
  is slow and reduces compression ratio; memLevel=9 uses maximum memory
  for optimal speed. The default value is 8. See zconf.h for total memory
  usage as a function of windowBits and memLevel.

  The strategy parameter is used to tune the compression algorithm. Use the
  value Z_DEFAULT_STRATEGY for normal data, Z_FILTERED for data produced by a
  filter (or predictor), Z_HUFFMAN_ONLY to force Huffman encoding only (no
  string match), or Z_RLE to limit match distances to one (run-length
  encoding). Filtered data consists mostly of small values with a somewhat
  random distribution. In this case, the compression algorithm is tuned to
  compress them better. The effect of Z_FILTERED is to force more Huffman
  coding and less string matching; it is somewhat intermediate between
  Z_DEFAULT and Z_HUFFMAN_ONLY. Z_RLE is designed to be almost as fast as
  Z_HUFFMAN_ONLY, but give better compression for PNG image data. The strategy
  parameter only affects the compression ratio but not the correctness of the
  compressed output even if it is not set appropriately.

  deflateInit2 returns Z_OK if success, Z_MEM_ERROR if there was not enough
  memory, Z_STREAM_ERROR if a parameter is invalid (such as an invalid
  method). msg is set to null if there is no error message.  deflateInit2 does
  not perform any compression: this will be done by deflate(). }
{$EXTERNALSYM deflateInit2}
function deflateInit2(var strm: TZStreamRec; level, method,
                      windowBits, memLevel, strategy: Integer): Integer;  // macro


{ deflateSetDictionary
  Initializes the compression dictionary from the given byte sequence
  without producing any compressed output. This function must be called
  immediately after deflateInit, deflateInit2 or deflateReset, before any
  call of deflate. The compressor and decompressor must use exactly the same
  dictionary (see inflateSetDictionary).

  The dictionary should consist of strings (byte sequences) that are likely
  to be encountered later in the data to be compressed, with the most commonly
  used strings preferably put towards the end of the dictionary. Using a
  dictionary is most useful when the data to be compressed is short and can be
  predicted with good accuracy; the data can then be compressed better than
  with the default empty dictionary.

  Depending on the size of the compression data structures selected by
  deflateInit or deflateInit2, a part of the dictionary may in effect be
  discarded, for example if the dictionary is larger than the window size in
  deflate or deflate2. Thus the strings most likely to be useful should be
  put at the end of the dictionary, not at the front.

  Upon return of this function, strm->adler is set to the Adler32 value
  of the dictionary; the decompressor may later use this value to determine
  which dictionary has been used by the compressor. (The Adler32 value
  applies to the whole dictionary even if only a subset of the dictionary is
  actually used by the compressor.) If a raw deflate was requested, then the
  adler32 value is not computed and strm->adler is not set.

  deflateSetDictionary returns Z_OK if success, or Z_STREAM_ERROR if a
  parameter is invalid (such as NULL dictionary) or the stream state is
  inconsistent (for example if deflate has already been called for this stream
  or if the compression method is bsort). deflateSetDictionary does not
  perform any compression: this will be done by deflate(). }
{$EXTERNALSYM deflateSetDictionary}
function deflateSetDictionary(var strm: TZStreamRec;
  const dictionary; dictLength: UInt): Integer;

{ deflateCopy
  Sets the destination stream as a complete copy of the source stream.

  This function can be useful when several compression strategies will be
  tried, for example when there are several ways of pre-processing the input
  data with a filter. The streams that will be discarded should then be freed
  by calling deflateEnd.  Note that deflateCopy duplicates the internal
  compression state which can be quite large, so this strategy is slow and
  can consume lots of memory.

  deflateCopy returns Z_OK if success, Z_MEM_ERROR if there was not
  enough memory, Z_STREAM_ERROR if the source stream state was inconsistent
  (such as zalloc being NULL). msg is left unchanged in both source and
  destination. }
{$EXTERNALSYM deflateCopy}
function deflateCopy(var dest, source: TZStreamRec): Integer;

{ deflateReset
  This function is equivalent to deflateEnd followed by deflateInit,
  but does not free and reallocate all the internal compression state.
  The stream will keep the same compression level and any other attributes
  that may have been set by deflateInit2.

  deflateReset returns Z_OK if success, or Z_STREAM_ERROR if the source
  stream state was inconsistent (such as zalloc or state being NULL). }
{$EXTERNALSYM deflateReset}
function deflateReset(var strm: TZStreamRec): Integer;

{ deflateParams
  Dynamically update the compression level and compression strategy.  The
  interpretation of level and strategy is as in deflateInit2.  This can be
  used to switch between compression and straight copy of the input data, or
  to switch to a different kind of input data requiring a different
  strategy. If the compression level is changed, the input available so far
  is compressed with the old level (and may be flushed); the new level will
  take effect only at the next call of deflate().

  Before the call of deflateParams, the stream state must be set as for
  a call of deflate(), since the currently available input may have to
  be compressed and flushed. In particular, strm->avail_out must be non-zero.

  deflateParams returns Z_OK if success, Z_STREAM_ERROR if the source
  stream state was inconsistent or if a parameter was invalid, Z_BUF_ERROR
  if strm->avail_out was zero. }
{$EXTERNALSYM deflateParams}
function deflateParams(var strm: TZStreamRec; level, strategy: Integer): Integer;

{ deflateBound
  deflateBound() returns an upper bound on the compressed size after
  deflation of sourceLen bytes.  It must be called after deflateInit()
  or deflateInit2().  This would be used to allocate an output buffer
  for deflation in a single pass, and so would be called before deflate(). }
{$EXTERNALSYM deflateBound}
function deflateBound(var strm: TZStreamRec; sourceLen: ULong): ULong;

{ deflatePrime
  deflatePrime() inserts bits in the deflate output stream.  The intent
  is that this function is used to start off the deflate output with the
  bits leftover from a previous deflate stream when appending to it.  As such,
  this function can only be used for raw deflate, and must be used before the
  first deflate() call after a deflateInit2() or deflateReset().  bits must be
  less than or equal to 16, and that many of the least significant bits of
  value will be inserted in the output.

  deflatePrime returns Z_OK if success, or Z_STREAM_ERROR if the source
  stream state was inconsistent. }
{$EXTERNALSYM deflatePrime}
function deflatePrime(var strm: TZStreamRec; bits, value: Integer): Integer;

{ inflateInit2
  This is another version of inflateInit with an extra parameter. The
  fields next_in, avail_in, zalloc, zfree and opaque must be initialized
  before by the caller.

  The windowBits parameter is the base two logarithm of the maximum window
  size (the size of the history buffer).  It should be in the range 8..15 for
  this version of the library. The default value is 15 if inflateInit is used
  instead. windowBits must be greater than or equal to the windowBits value
  provided to deflateInit2() while compressing, or it must be equal to 15 if
  deflateInit2() was not used. If a compressed stream with a larger window
  size is given as input, inflate() will return with the error code
  Z_DATA_ERROR instead of trying to allocate a larger window.

  windowBits can also be -8..-15 for raw inflate. In this case, -windowBits
  determines the window size. inflate() will then process raw deflate data,
  not looking for a zlib or gzip header, not generating a check value, and not
  looking for any check values for comparison at the end of the stream. This
  is for use with other formats that use the deflate compressed data format
  such as zip.  Those formats provide their own check values. If a custom
  format is developed using the raw deflate format for compressed data, it is
  recommended that a check value such as an adler32 or a crc32 be applied to
  the uncompressed data as is done in the zlib, gzip, and zip formats.  For
  most applications, the zlib format should be used as is. Note that comments
  above on the use in deflateInit2() applies to the magnitude of windowBits.

  windowBits can also be greater than 15 for optional gzip decoding. Add
  32 to windowBits to enable zlib and gzip decoding with automatic header
  detection, or add 16 to decode only the gzip format (the zlib format will
  return a Z_DATA_ERROR).

  inflateInit2 returns Z_OK if success, Z_MEM_ERROR if there was not enough
  memory, Z_STREAM_ERROR if a parameter is invalid (such as a negative
  memLevel). msg is set to null if there is no error message.  inflateInit2
  does not perform any decompression apart from reading the zlib header if
  present: this will be done by inflate(). (So next_in and avail_in may be
  modified, but next_out and avail_out are unchanged.) }
{$EXTERNALSYM inflateInit2}
function inflateInit2(var strm: TZStreamRec;
                      windowBits: Integer): Integer;  // macro


{ inflateSetDictionary
  Initializes the decompression dictionary from the given uncompressed byte
  sequence. This function must be called immediately after a call of inflate
  if this call returned Z_NEED_DICT. The dictionary chosen by the compressor
  can be determined from the Adler32 value returned by this call of
  inflate. The compressor and decompressor must use exactly the same
  dictionary (see deflateSetDictionary).

  inflateSetDictionary returns Z_OK if success, Z_STREAM_ERROR if a
  parameter is invalid (such as NULL dictionary) or the stream state is
  inconsistent, Z_DATA_ERROR if the given dictionary doesn't match the
  expected one (incorrect Adler32 value). inflateSetDictionary does not
  perform any decompression: this will be done by subsequent calls of
  inflate(). }
{$EXTERNALSYM inflateSetDictionary}
function inflateSetDictionary(var strm: TZStreamRec;
  const dictionary; dictLength: Integer): Integer;

{ inflateSync
  Skips invalid compressed data until a full flush point (see above the
  description of deflate with Z_FULL_FLUSH) can be found, or until all
  available input is skipped. No output is provided.

  inflateSync returns Z_OK if a full flush point has been found, Z_BUF_ERROR
  if no more input was provided, Z_DATA_ERROR if no flush point has been found,
  or Z_STREAM_ERROR if the stream structure was inconsistent. In the success
  case, the application may save the current current value of total_in which
  indicates where valid compressed data was found. In the error case, the
  application may repeatedly call inflateSync, providing more input each time,
  until success or end of the input data. }
{$EXTERNALSYM inflateSync}
function inflateSync(var strm: TZStreamRec): Integer;

{ inflateCopy
  Sets the destination stream as a complete copy of the source stream.

  This function can be useful when randomly accessing a large stream.  The
  first pass through the stream can periodically record the inflate state,
  allowing restarting inflate at those points when randomly accessing the
  stream.

  inflateCopy returns Z_OK if success, Z_MEM_ERROR if there was not
  enough memory, Z_STREAM_ERROR if the source stream state was inconsistent
  (such as zalloc being NULL). msg is left unchanged in both source and
  destination. }
{$EXTERNALSYM inflateCopy}
function inflateCopy(var dest, source: TZStreamRec): Integer;

{ inflateReset
  This function is equivalent to inflateEnd followed by inflateInit,
  but does not free and reallocate all the internal decompression state.
  The stream will keep attributes that may have been set by inflateInit2.

  inflateReset returns Z_OK if success, or Z_STREAM_ERROR if the source
  stream state was inconsistent (such as zalloc or state being NULL). }
{$EXTERNALSYM inflateReset}
function inflateReset(var strm: TZStreamRec): Integer;

{ inflateBackInit
  Initialize the internal stream state for decompression using inflateBack()
  calls.  The fields zalloc, zfree and opaque in strm must be initialized
  before the call.  If zalloc and zfree are Z_NULL, then the default library-
  derived memory allocation routines are used.  windowBits is the base two
  logarithm of the window size, in the range 8..15.  window is a caller
  supplied buffer of that size.  Except for special applications where it is
  assured that deflate was used with small window sizes, windowBits must be 15
  and a 32K byte window must be supplied to be able to decompress general
  deflate streams.

  See inflateBack() for the usage of these routines.

  inflateBackInit will return Z_OK on success, Z_STREAM_ERROR if any of
  the paramaters are invalid, Z_MEM_ERROR if the internal state could not
  be allocated, or Z_VERSION_ERROR if the version of the library does not
  match the version of the header file. }
{$EXTERNALSYM inflateBackInit}
function inflateBackInit(var strm: TZStreamRec; windowBits: Integer;
                         window: PByte): Integer;  // macro

type
  {$EXTERNALSYM in_func}
  in_func = function(p1: Pointer; var p2: PByte): Cardinal;
  TInFunc = in_func;
  {$EXTERNALSYM out_func}
  out_func = function(p1: Pointer; p2: PByte; p3: Cardinal): Integer;
  TOutFunc = out_func;

{ inflateBack
  inflateBack() does a raw inflate with a single call using a call-back
  interface for input and output.  This is more efficient than inflate() for
  file i/o applications in that it avoids copying between the output and the
  sliding window by simply making the window itself the output buffer.  This
  function trusts the application to not change the output buffer passed by
  the output function, at least until inflateBack() returns.

  inflateBackInit() must be called first to allocate the internal state
  and to initialize the state with the user-provided window buffer.
  inflateBack() may then be used multiple times to inflate a complete, raw
  deflate stream with each call.  inflateBackEnd() is then called to free
  the allocated state.

  A raw deflate stream is one with no zlib or gzip header or trailer.
  This routine would normally be used in a utility that reads zip or gzip
  files and writes out uncompressed files.  The utility would decode the
  header and process the trailer on its own, hence this routine expects
  only the raw deflate stream to decompress.  This is different from the
  normal behavior of inflate(), which expects either a zlib or gzip header and
  trailer around the deflate stream.

  inflateBack() uses two subroutines supplied by the caller that are then
  called by inflateBack() for input and output.  inflateBack() calls those
  routines until it reads a complete deflate stream and writes out all of the
  uncompressed data, or until it encounters an error.  The function's
  parameters and return types are defined above in the in_func and out_func
  typedefs.  inflateBack() will call in(in_desc, &buf) which should return the
  number of bytes of provided input, and a pointer to that input in buf.  If
  there is no input available, in() must return zero--buf is ignored in that
  case--and inflateBack() will return a buffer error.  inflateBack() will call
  out(out_desc, buf, len) to write the uncompressed data buf[0..len-1].  out()
  should return zero on success, or non-zero on failure.  If out() returns
  non-zero, inflateBack() will return with an error.  Neither in() nor out()
  are permitted to change the contents of the window provided to
  inflateBackInit(), which is also the buffer that out() uses to write from.
  The length written by out() will be at most the window size.  Any non-zero
  amount of input may be provided by in().

  For convenience, inflateBack() can be provided input on the first call by
  setting strm->next_in and strm->avail_in.  If that input is exhausted, then
  in() will be called.  Therefore strm->next_in must be initialized before
  calling inflateBack().  If strm->next_in is Z_NULL, then in() will be called
  immediately for input.  If strm->next_in is not Z_NULL, then strm->avail_in
  must also be initialized, and then if strm->avail_in is not zero, input will
  initially be taken from strm->next_in[0 .. strm->avail_in - 1].

  The in_desc and out_desc parameters of inflateBack() is passed as the
  first parameter of in() and out() respectively when they are called.  These
  descriptors can be optionally used to pass any information that the caller-
  supplied in() and out() functions need to do their job.

  On return, inflateBack() will set strm->next_in and strm->avail_in to
  pass back any unused input that was provided by the last in() call.  The
  return values of inflateBack() can be Z_STREAM_END on success, Z_BUF_ERROR
  if in() or out() returned an error, Z_DATA_ERROR if there was a format
  error in the deflate stream (in which case strm->msg is set to indicate the
  nature of the error), or Z_STREAM_ERROR if the stream was not properly
  initialized.  In the case of Z_BUF_ERROR, an input or output error can be
  distinguished using strm->next_in which will be Z_NULL only if in() returned
  an error.  If strm->next is not Z_NULL, then the Z_BUF_ERROR was due to
  out() returning non-zero.  (in() will always be called before out(), so
  strm->next_in is assured to be defined if out() returns non-zero.)  Note
  that inflateBack() cannot return Z_OK. }
{$EXTERNALSYM inflateBack}
function inflateBack(var strm: TZStreamRec;
  in_func: TInFunc; in_desc: Pointer;
  out_func: TOutFunc; out_desc: Pointer): Integer;

{ inflateBackEnd
  All memory allocated by inflateBackInit() is freed.

  inflateBackEnd() returns Z_OK on success, or Z_STREAM_ERROR if the stream
  state was inconsistent. }
{$EXTERNALSYM inflateBackEnd}
function inflateBackEnd(var strm: TZStreamRec): Integer;

{ zlibCompileFlags
  Return flags indicating compile-time options.

   Type sizes, two bits each, 00 = 16 bits, 01 = 32, 10 = 64, 11 = other:
    1.0: size of uInt
    3.2: size of uLong
    5.4: size of voidpf (pointer)
    7.6: size of z_off_t

   Compiler, assembler, and debug options:
    8: DEBUG
    9: ASMV or ASMINF -- use ASM code
    10: ZLIB_WINAPI -- exported functions use the WINAPI calling convention
    11: 0 (reserved)

   One-time table building (smaller code, but not thread-safe if true):
    12: BUILDFIXED -- build static block decoding tables when needed
    13: DYNAMIC_CRC_TABLE -- build CRC calculation tables when needed
    14,15: 0 (reserved)

   Library content (indicates missing functionality):
    16: NO_GZCOMPRESS -- gz* functions cannot compress (to avoid linking
                         deflate code when not needed)
    17: NO_GZIP -- deflate can't write gzip streams, and inflate can't detect
                   and decode gzip streams (to avoid linking crc code)
    18-19: 0 (reserved)

   Operation variations (changes in library functionality):
    20: PKZIP_BUG_WORKAROUND -- slightly more permissive inflate
    21: FASTEST -- deflate algorithm with only one, lowest compression level
    22,23: 0 (reserved)

   The sprintf variant used by gzprintf (zero is best):
    24: 0 = vs*, 1 = s* -- 1 means limited to 20 arguments after the format
    25: 0 = *nprintf, 1 = *printf -- 1 means gzprintf() not secure!
    26: 0 = returns value, 1 = void -- 1 means inferred string length returned

   Remainder:
    27-31: 0 (reserved) }
{$EXTERNALSYM zlibCompileFlags}
function zlibCompileFlags: ULong;

//                         utility functions

{ The following utility functions are implemented on top of the
  basic stream-oriented functions. To simplify the interface, some
  default options are assumed (compression level and memory usage,
  standard memory allocation functions). The source code of these
  utility functions can easily be modified if you need special options. }


{ compress
  Compresses the source buffer into the destination buffer.  sourceLen is
  the byte length of the source buffer. Upon entry, destLen is the total
  size of the destination buffer, which must be at least the value returned
  by compressBound(sourceLen). Upon exit, destLen is the actual size of the
  compressed buffer.

  This function can be used to compress a whole file at once if the
  input file is mmap'ed.

  compress returns Z_OK if success, Z_MEM_ERROR if there was not
  enough memory, Z_BUF_ERROR if there was not enough room in the output
  buffer. }
{$EXTERNALSYM compress}
function compress(out dest; var destLen: ULong;
  const source; sourceLen: ULong): Integer;

{ compress2
  Compresses the source buffer into the destination buffer. The level
  parameter has the same meaning as in deflateInit.  sourceLen is the byte
  length of the source buffer. Upon entry, destLen is the total size of the
  destination buffer, which must be at least the value returned by
  compressBound(sourceLen). Upon exit, destLen is the actual size of the
  compressed buffer.

  compress2 returns Z_OK if success, Z_MEM_ERROR if there was not enough
  memory, Z_BUF_ERROR if there was not enough room in the output buffer,
  Z_STREAM_ERROR if the level parameter is invalid. }
{$EXTERNALSYM compress2}
function compress2(out dest; var destLen: ULong;
  const source; sourceLen: ULong; level: Integer): Integer;

{ compressBound
  compressBound() returns an upper bound on the compressed size after
  compress() or compress2() on sourceLen bytes.  It would be used before
  a compress() or compress2() call to allocate the destination buffer. }
{$EXTERNALSYM compressBound}
function compressBound(sourceLen: ULong): ULong;

{ uncompress
  Decompresses the source buffer into the destination buffer.  sourceLen is
  the byte length of the source buffer. Upon entry, destLen is the total
  size of the destination buffer, which must be large enough to hold the
  entire uncompressed data. (The size of the uncompressed data must have
  been saved previously by the compressor and transmitted to the decompressor
  by some mechanism outside the scope of this compression library.)
  Upon exit, destLen is the actual size of the compressed buffer.

  This function can be used to decompress a whole file at once if the
  input file is mmap'ed.

  uncompress returns Z_OK if success, Z_MEM_ERROR if there was not
  enough memory, Z_BUF_ERROR if there was not enough room in the output
  buffer, or Z_DATA_ERROR if the input data was corrupted or incomplete. }
{$EXTERNALSYM uncompress}
function uncompress(out dest; var destLen: ULong;
  const source; sourceLen: ULong): Integer;



//                         checksum functions

{ These functions are not related to compression but are exported
  anyway because they might be useful in applications using the
  compression library. }


{ adler32
  Update a running Adler-32 checksum with the bytes buf[0..len-1] and
  return the updated checksum. If buf is NULL, this function returns
  the required initial value for the checksum.
  An Adler-32 checksum is almost as reliable as a CRC32 but can be computed
  much faster. Usage example: }

//   uLong adler = adler32(0L, Z_NULL, 0);
//
//   while (read_buffer(buffer, length) != EOF) {
//     adler = adler32(adler, buffer, length);
//   }
//   if (adler != original_adler) error();
{$EXTERNALSYM adler32}
function adler32(adler: ULong; buf: Pointer; len: UInt): ULong;

{ crc32
  Update a running crc with the bytes buf[0..len-1] and return the updated
  crc. If buf is NULL, this function returns the required initial value
  for the crc. Pre- and post-conditioning (one's complement) is performed
  within this function so it shouldn't be done by the application.
  Usage example: }

//   uLong crc = crc32(0L, Z_NULL, 0);
//
//   while (read_buffer(buffer, length) != EOF) {
//     crc = crc32(crc, buffer, length);
//   }
//   if (crc != original_crc) error();
{$EXTERNALSYM crc32}
function crc32(crc: ULong; buf: Pointer; len: UInt): ULong;

//                         various hacks, don't look :)

{ deflateInit and inflateInit are macros to allow checking the zlib version
  and the compiler's view of z_stream: }
{$EXTERNALSYM deflateInit_}
function deflateInit_(var strm: TZStreamRec;
  level: Integer; const version: PChar; stream_size: Integer): Integer;

{$EXTERNALSYM inflateInit_}
function inflateInit_(var strm: TZStreamRec;
  const version: PChar; stream_size: Integer): Integer;

{$EXTERNALSYM deflateInit2_}
function deflateInit2_(var strm: TZStreamRec;
  level, method, windowBits, memLevel, strategy: Integer;
  const version: PChar; stream_size: Integer): Integer;

{$EXTERNALSYM inflateInit2_}
function inflateInit2_(var strm: TZStreamRec;
  windowBits: Integer; const version: PChar; stream_size: Integer): Integer;

{$EXTERNALSYM inflateBackInit_}
function inflateBackInit_(var strm: TZStreamRec; windowBits: Integer;
  window: PByte; version: PChar; stream_size: Integer): Integer;

{ exported to allow conversion of error code to string for compress() and
  uncompress() }
{$EXTERNALSYM zError}
function zError(err: Integer): PChar;

{ Returns true if inflate is currently at the end of a block generated
  by Z_SYNC_FLUSH or Z_FULL_FLUSH. This function is used by one PPP
  implementation to provide an additional safety check. PPP uses Z_SYNC_FLUSH
  but removes the length bytes of the resulting empty stored block. When
  decompressing, PPP checks that at the end of input packet, inflate is
  waiting for these length bytes. }
{$EXTERNALSYM inflateSyncPoint}
function inflateSyncPoint(var z: TZStreamRec): Integer;

{ This function can be used by asm versions of crc32() }
{$EXTERNALSYM get_crc_table}
function get_crc_table: PCRCTable;

implementation

{$L obj\deflate.obj}
{$L obj\inflate.obj}
{$L obj\compress.obj}
{$L obj\uncompr.obj}
{$L obj\adler32.obj}
{$L obj\crc32.obj}


{$L obj\inftrees.obj}
{$L obj\trees.obj}
{$L obj\inffast.obj}
{$L obj\zutil.obj}
{$L obj\infback.obj}



// **************************  zutil.c  *****************************
function zlibVersion;      external ;
function zError;           external ;
function zlibCompileFlags; external ;


procedure _memset(P: Pointer; B: Byte; count: Integer); cdecl;
begin
  FillChar(P^, count, B);
end;

procedure _memcpy(dest, source: Pointer; count: Integer); cdecl;
begin
  Move(source^, dest^, count);
end;

function _malloc(size: Cardinal): Pointer; cdecl;
begin
  GetMem(Result, size);
end;

procedure _free(ptr: Pointer); cdecl;
begin
  FreeMem(ptr);
end;


// **************************  deflate.c  ***************************
function deflateInit_;         external ;
function deflateInit2_;        external ;
function deflateSetDictionary; external ;
function deflateReset;         external ;
function deflateParams;        external ;
function deflateBound;         external ;
function deflatePrime;         external ;
function deflate;              external ;
function deflateEnd;           external ;
function deflateCopy;          external ;

// **************************  inflate.c  ***************************
function inflateReset;         external ;
function inflateEnd;           external ;
function inflateInit2_;        external ;
function inflateInit_;         external ;
function inflate;              external ;
function inflateSetDictionary; external ;
function inflateSync;          external ;
function inflateSyncPoint;     external ;
function inflateCopy;          external ;

// **************************  infback.c  ***************************
function inflateBackInit_; external ;
function inflateBack;      external ;
function inflateBackEnd;   external ;

// **************************  compress.c  **************************
function compress2;     external ;
function compress;      external ;
function compressBound; external ;

// **************************  uncompr.c  ***************************
function uncompress; external ;


// **************************  adler32.c  ***************************
function adler32; external ;

// **************************  crc32.c  *****************************
function get_crc_table; external ;
function crc32;         external ;

// **************************  zlib.h (Macros)  *********************

function deflateInit(var strm: TZStreamRec; level: Integer): Integer;
begin
  Result := deflateInit_(strm, level, ZLIB_VERSION, SizeOf(TZStreamRec));
end;

function inflateInit(var strm: TZStreamRec): Integer;
begin
  Result := inflateInit_(strm, ZLIB_VERSION, SizeOf(TZStreamRec));
end;

function deflateInit2(var strm: TZStreamRec;
  level, method, windowBits, memLevel, strategy: Integer): Integer;
begin
  Result := deflateInit2_(strm, level, method, windowBits, memLevel, strategy,
    ZLIB_VERSION, SizeOf(TZStreamRec));
end;

function inflateInit2(var strm: TZStreamRec; windowBits: Integer): Integer;
begin
  Result := inflateInit2_(strm, windowBits, ZLIB_VERSION, SizeOf(TZStreamRec));
end;

function inflateBackInit(var strm: TZStreamRec; windowBits: Integer;
  window: PByte): Integer;
begin
  Result := inflateBackInit_(strm, windowBits, window, ZLIB_VERSION, SizeOf(strm));
end;

// ****************************************************************************

//  History:

//   Revision 1.10  2004/06/14 13:05:19  marquardt
//   style cleaning ENDIF, Tabs
//
//   Revision 1.9  2004/06/06 01:57:03  rrossmair
//   check-in in preparation of build #1558 release
//
//   Revision 1.8  2004/05/31 22:38:51  rrossmair
//   added PJH disclaimer; resolved $IFDEF JCL
//
//   Revision 1.7  2004/05/09 00:03:51  peterjhaas
//   - old history in reverse order like CVS log
//   - change interface adler32 and crc32 to avoid FPC compatibility problems
//
//   Revision 1.6  2004/05/08 08:44:18  rrossmair
//   introduced & applied symbol HAS_UNIT_LIBC
//
//   Revision 1.5  2004/05/01 03:04:37  peterjhaas
//   - move jedi.in after setting compiler options
//   - add symbols for calling conventions
//
//   Revision 1.4  2004/04/30 17:59:43  peterjhaas
//   - add calling convention for .hpp file
//   - change NONBORLAND to BORLAND
//
//   Revision 1.3  2004/04/28 15:48:45  peterjhaas
//   - add include of zutil.h for C compilers
//
//   Revision 1.2  2004/04/20 01:34:06  rrossmair
//   "uses LibC" changed to "uses Libc" (unit names are case-sensitive on Unix)
//
//   Revision 1.1  2004/04/08 00:29:22  peterjhaas
//   Zlib header conversion prototype
//
//   2004-03-19, Peter J. Haas
//    - compiler symbol to hide platform specific comments
//
//   2004-03-15, Peter J. Haas
//    - move directive comments to directives
//
//   2004-01-23, Peter J. Haas
//    - add new functions from version 1.2.1
//    - generate different units for Delphi 3, Delphi 4 ... and Kylix
//
//   2003-11-14, Peter J. Haas
//    - add demo
//
//   2003-04-14, Peter J. Haas
//    - First public version
//
//   2002-04-07, Peter J. Haas
//    - First public pre release
//
//   2002-04-04, Matthias Thoma (mthoma)
//    - Global change: define Kylix had to be change to Linux
//
//   2002-04-04, Peter J. Haas
//    - first internal version 1.1.4
//

end.
