{******************************************************************************}
{                                                                              }
{  Project JEDI Code Library (JCL)                                             }
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
{  The Original Code is: zlibs.pas, gzips.pas, tar.pas.                        }
{  The Initial Developer of the Original Code is Peter J. Haas. Portions       }
{  created by Peter J. Haas are Copyright (C) 2002-2003 Peter J. Haas. All     }
{  Rights Reserved.                                                            }
{                                                                              }
{  The Original Code Version 2.0 is: JclZlib.pas.                              }
{  The Initial Developer of the Original Code V2.0 is Peter J. Haas. Portions  }
{  created by Peter J. Haas are Copyright (C) 2004 Peter J. Haas. All Rights   }
{  Reserved.                                                                   }
{                                                                              }
{  You may retrieve the latest version of the Original Code at the homepage    }
{  of JEDI+ (jediplus att pjh2 dott de), located at http://jediplus.pjh2.de/   }
{                                                                              }
{  You may retrieve the latest version of this file at the homepage of         }
{  JEDI, located at http://www.delphi-jedi.org/                                }
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
{    Peter J. Haas (peterjhaas)                                                }
{    Robert Rossmair (rrossmair)                                               }
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
{                                                                              }
{  Additional information:                                                     }
{    RFC 1952: GZIP file format specification version 4.3, 1996, Peter Deutsch }
{    ftp://ftp.uu.net/graphics/png/documents/zlib/zdoc-index.html              }
{                                                                              }
{    The gzip file format, additional informations, Jean-loup Gailly           }
{    http://www.gzip.org/format.txt                                            }
{                                                                              }
{    gzip format                                                               }
{    http://www.onicos.com/staff/iz/formats/gzip.html                          }
{                                                                              }
{******************************************************************************}

// Last modified: $Id$
// For history see end of file

{$I jcl.inc}

unit JclZlib;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Types,
  {$ENDIF UNIX}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  SysUtils, Classes,
  JclBase, JclDateTime,
  zlibh;

const
  JclZLibStreamDefaultBufferSize = 32 * 1024;

const
  {$IFDEF MSWINDOWS}
  JclZLibDefaultLineSeparator = #$0D#$0A;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  JclZLibDefaultLineSeparator = #$0A;
  {$ENDIF UNIX}

const
  WindowsPathDelimiter = '\';
  UnixPathDelimiter = '/';
  {$IFNDEF RTL140_UP}
  {$IFDEF MSWINDOWS}
  PathDelim = WindowsPathDelimiter;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  PathDelim = UnixPathDelimiter;
  {$ENDIF UNIX}
  {$ENDIF ~RTL140_UP}

//--------------------------------------------------------------------------------------------------
// zlib format support
//--------------------------------------------------------------------------------------------------

type
  TJclZLibStream = class(TStream)
  protected
    FStream: TStream;
    FBufferSize: Integer;
    FBuffer: Pointer;
    FZLibStream: TZStreamRec;
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(const Stream: TStream; const BufferSize: Integer);
    destructor Destroy; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  TJclZLibReader = class(TJclZLibStream)
  protected
    FEndOfStream: Boolean;
    procedure ReadNextBlock;
    procedure FinishZLibStream;
  public
    constructor Create(const Stream: TStream;
      const BufferSize: Integer = JclZLibStreamDefaultBufferSize;
      const WindowBits: Integer = DEF_WBITS);

    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    procedure Reset;
    procedure SyncZLibStream;

    property EndOfStream: Boolean read FEndOfStream;
  end;

  TJclZLibWriter = class(TJclZLibStream)
  protected
    procedure WriteNextBlock;
    procedure FlushZLibStream(const Flush: Integer);
  public
    constructor Create(const Stream: TStream;
      const BufferSize: Integer = JclZLibStreamDefaultBufferSize;
      const Level: Integer = Z_DEFAULT_COMPRESSION;
      const Strategy: Integer = Z_DEFAULT_STRATEGY;
      const WindowBits: Integer = DEF_WBITS);

    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;

    procedure Reset;
  end;

  EJclZLibError = class(EJclError);

// zlib error texts
function GetZLibErrorText(const ErrorCode: Integer): PResStringRec;

function ZLibCompressMem(const Src: Pointer; SrcLen: Integer;
  out Dst: Pointer; out DstLen: Integer; out DstCapacity: Integer;
  const Level: Integer = Z_DEFAULT_COMPRESSION): Boolean;

// Flush:
//   Z_SYNC_FLUSH:  DstCapacity can be 0
//   Z_FINISH:      decompress with faster routine in a single step
//                  DstCapacity must be >= uncompressed size
function ZLibDecompressMem(const Src: Pointer; SrcLen: Integer;
  out Dst: Pointer; out DstLen: Integer; var DstCapacity: Integer;
  const Flush: Integer = Z_SYNC_FLUSH): Boolean;

//--------------------------------------------------------------------------------------------------
// gzip format support
//--------------------------------------------------------------------------------------------------

type
  TJclGZipStream = class(TStream)
  protected
    FStream: TStream;
    FCRC32: LongWord;
    FUncompressedSize: LongWord;
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(const Stream: TStream);
    destructor Destroy; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  TJclGZipReader = class(TJclGZipStream)
  private
    FZLibReader: TJclZLibReader;
    FTextMode: Boolean;
    FFilename: string;
    FComment: string;
    FTimeStamp: TJclUnixTime32;
    FLevel: Integer;
    FOperatingSystem: Byte;
    FMultipartNumber: Word;
    FExtraField: Pointer;
    FExtraFieldSize: Integer;
    FEndOfStream: Boolean;
  public
    constructor Create(const Stream: TStream;
      const BufferSize: Integer = JclZLibStreamDefaultBufferSize;
      const LineSeparator: string = JclZLibDefaultLineSeparator);
    
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;

    property TextMode: Boolean read FTextMode;
    property Filename: string read FFilename;
    property Comment: string read FComment;
    property TimeStamp: TJclUnixTime32 read FTimeStamp;
    property Level: Integer read FLevel;
    property OperatingSystem: Byte read FOperatingSystem;
    property MultipartNumber: Word read FMultipartNumber;  // 0 = first part
    property ExtraField: Pointer read FExtraField;
    property ExtraFieldSize: Integer read FExtraFieldSize;

    property EndOfStream: Boolean read FEndOfStream;
  end;

  TJclGZipWriter = class(TJclGZipStream)
  private
    FTextMode: Boolean;
    FZLibWriter: TJclZLibWriter;
  public
    constructor Create(const Stream: TStream;
      const BufferSize: Integer = JclZLibStreamDefaultBufferSize;
      const Level: Integer = Z_DEFAULT_COMPRESSION;
      const Strategie: Integer = Z_DEFAULT_STRATEGY;
      const Filename: string = '';
      const TimeStamp: TJclUnixTime32 = 0;
      const Comment: string = '';
      const TextMode: Boolean = False;
      const ExtraField: Pointer = nil;
      const ExtraFieldSize: Integer = 0);

    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  EJclGZipError = class(EJclError);

// gzip file extension
const
  JclGZipDefaultFileExtension = '.gz';

// if DstFilename = '' -> DstFilename := SrcFilename + JclGZipDefaultFileExtension
procedure GZipCompressFile(const SrcFilename: string; DstFilename: string;
  const Level: Integer = Z_DEFAULT_COMPRESSION);
procedure GZipDecompressFile(const SrcFilename: string; DstFilename: string);

//--------------------------------------------------------------------------------------------------
// tar archive support
//--------------------------------------------------------------------------------------------------

const
  TarBlockSize = 512;

type
  TTarArchiveFormat = (
    tafDefaultFormat,    // format to be decided later
    tafV7Format,         // old V7 tar format
    tafOldGnuFormat,     // GNU format as per before tar 1.12
    tafPosixFormat,      // restricted, pure POSIX format
    tafGnuFormat);       // POSIX format with GNU extensions

type
  PSparse = ^TSparse;
  TSparse = packed record                 // offset
    Offset: array [0..11] of AnsiChar;    //  $00
    NumBytes: array [0..11] of AnsiChar;  //  $0C
  end;                                    //  $18

  PTarHeader = ^TTarHeader;
  TTarHeader = packed record       // offset
  case Integer of
    0: (Buffer: array [0..TarBlockSize - 1] of Byte);
    1: (
      // Old UNIX TAR format
      Name: array [0..99] of AnsiChar;          //  $000     Char + #0   / mit 0 gefüllt
      Mode: array [0..7] of AnsiChar;           //  $064     Octal + ' '#0   9 + 3 bits              20 34 30 37 35 35 20 00
      UID: array [0..7] of AnsiChar;            //  $06C     Octal + ' '#0   ignore on DOS           20 20 31 37 35 36 20 00
      GID: array [0..7] of AnsiChar;            //  $074     Octal + ' '#0   ignore on DOS           20 20 20 31 34 34 20 00
      Size: array [0..11] of AnsiChar;          //  $07C     Octal + ' '     size in bytes           20 20 20 20 20 20 20 20 20 20 30 20
      MTime: array [0..11] of AnsiChar;         //  $088     Octal + ' '     last modify Unix        20 36 37 32 32 34 34 36 31 30 37 20
      Chksum: array [0..7] of AnsiChar;         //  $094     Octal + ' '#0   >= 17 bit, init 0, add  20 20 37 35 37 32 00 20
      TypeFlag: AnsiChar;                       //  $09C     Octal           + ' '#0 ??              35
      Linkname: array [0..99] of AnsiChar;      //  $09D     Char + #0
      // Extension of POSIX P1003.1
      Magic: array [0..5] of AnsiChar;          //  $101     Char + #0                               75 73 74 61 72 20
      Version: array [0..1] of AnsiChar;        //  $107     Octal + ' '                             20 00
      UName: array [0..31] of AnsiChar;         //  $109     Char + #0                               72 63 64 00 ...
      GName: array [0..31] of AnsiChar;         //  $129     Char + #0                               75 73 65 72 73 00 ...
      DevMajor: array [0..7] of AnsiChar;       //  $149     Octal + ' '#0
      DevMinor: array [0..7] of AnsiChar;       //  $151     Octal + ' '#0
    case TTarArchiveFormat of
      tafV7Format: (
        FillV7: array [0..166] of AnsiChar);    //  $159
      tafPosixFormat: (
        Prefix: array [0..154] of AnsiChar;     //  $159         Prefix for name
        FillPosix: array [0..11] of AnsiChar);  //  $1F4
      tafOldGnuFormat: (
        ATime: array [0..11] of AnsiChar;       //  $159
        CTime: array [0..11] of AnsiChar;       //  $165
        Offset: array [0..11] of AnsiChar;      //  $171
        Longnames: array [0..3] of AnsiChar;    //  $17D
        Pad: AnsiChar;                          //  $181
        Sparses: array [0..3] of TSparse;       //  $182
        IsExtended: AnsiChar;                   //  $1E2
        RealSize: array [0..11] of AnsiChar;    //  $1E3
        FillGnu: array [0..16] of AnsiChar));   //  $1EF
  end;                                          //  $200

// ModeFlag Flags
type
  TTarMode = (
    tmOtherExec,   // execute/search by other
    tmOtherWrite,  // write by other
    tmOtherRead,   // read by other
    tmGroupExec,   // execute/search by group
    tmGroupWrite,  // write by group
    tmGroupRead,   // read by group
    tmOwnerExec,   // execute/search by owner
    tmOwnerWrite,  // write by owner
    tmOwnerRead,   // read by owner
    tmSaveText,    // reserved
    tmSetGID,      // set GID on execution
    tmSetUID);     // set UID on execution
  TTarModes = set of TTarMode;

// TypeFlag
type
  TTarTypeFlag = AnsiChar;

const                     //                     V7  Posix
  ttfRegFile      = '0';  // regular file         x    x
  ttfARegFile     = #0;   // regular file         x    x
  ttfLink         = '1';  // link                 x    x
  ttfSymbolicLink = '2';  // symbolic link        x
  ttfCharacter    = '3';  // character special         x
  ttfBlock        = '4';  // block special             x
  ttfDirectory    = '5';  // directory                 x
  ttfFIFO         = '6';  // FIFO special              x
  ttfContiguous   = '7';  // contiguous file

  // GNU extensions
  ttfGnuDumpDir   = 'D';
  ttfGnuLongLink  = 'K';  // next file have a long link name
  ttfGnuLongName  = 'L';  // next file have a long name
  ttfGnuMultiVol  = 'M';  // file began on another volume
  ttfGnuNames     = 'N';  // long filename
  ttfGnuSparse    = 'S';  // sparse files
  ttfGnuVolHeader = 'V';  // Volume label (must be the first file)

const
  TarOldGnuMagic = 'ustar  '#0;  // old GNU  Magic + Version
  TarPosixMagic  = 'ustar'#0;    // Posix or GNU
  TarGnuVersion  = '00';

  // other version for GNU-Magic:  'GNUtar '#0

type
  TJclTarFileType = (tftUnknown, tftEof, tftFile, tftDirectory);
  
  TJclTarFileSize = Int64;
  

  TJclTarReader = class(TObject)
  private
    function GetFileDateTime: TDateTime;
  protected
    FTarStream: TStream;
    FHeader: TTarHeader;
    FArchiveFormat: TTarArchiveFormat;
    FFileType: TJclTarFileType;
    FFilename: string;
    FFileSize: TJclTarFileSize;
    FFileTime: TJclUnixTime32;
    function ReadHeader: Boolean;  // False if Eof
    procedure ScanHeader;
  public
    constructor Create(const TarStream: TStream);
    procedure CopyToStream(const FileStream: TStream; CanSeek: Boolean = False);
    procedure CopyToFile(const FilePath: string);
    procedure SkipFile;
    procedure SkipFileSeek;
    property FileType: TJclTarFileType read FFileType;
    property Filename: string read FFilename;
    property FileSize: TJclTarFileSize read FFileSize;
    property FileTime: TJclUnixTime32 read FFileTime;
    property FileDateTime: TDateTime read GetFileDateTime;
  end;

  TJclTarWriter = class(TObject)
  protected
    FTarStream: TStream;
    procedure AddEof;
  public
    constructor Create(const TarStream: TStream);
    destructor Destroy; override;
    procedure AddFile(FileRoot, Filename: string);
    procedure AddStream(const Stream: TStream; Filename: string;
      FileSize: TJclTarFileSize; FileTime: TJclUnixTime32);
    procedure AddDirectory(DirName: string);
  end;

  EJclTarError = class(EJclError);

procedure TarAllFiles(const TarFilename, FileRoot: string);
procedure TarFileList(const TarFilename, FileRoot: string; List: TStrings);
procedure TarFileArray(const TarFilename, FileRoot: string; const Filenames: array of string);
procedure TarGZipAllFiles(const TgzFilename, FileRoot: string);
procedure TarGZipFileList(const TgzFilename, FileRoot: string; List: TStrings);
procedure TarGZipFileArray(const TgzFilename, FileRoot: string; const Filenames: array of string);

procedure UnTarAllFiles(const TarFilename: string; DstDir: string);
procedure UnGZipTarAllFiles(const TgzFilename: string; DstDir: string);

procedure GetFileList(RootDir: string; List: TStrings);

implementation

uses
  JclResources, JclFileUtils;

//==================================================================================================
// zlib format support
//==================================================================================================

function GetZLibErrorText(const ErrorCode: Integer): PResStringRec;
const
  ErrorTexts: array [-6..2] of {$IFDEF FPC} AnsiString {$ELSE} PResStringRec {$ENDIF} =
   (
    {$IFNDEF FPC}@{$ENDIF}RsZLibVersionError,
    {$IFNDEF FPC}@{$ENDIF}RsZLibBufError,
    {$IFNDEF FPC}@{$ENDIF}RsZLibMemError,
    {$IFNDEF FPC}@{$ENDIF}RsZLibDataError,
    {$IFNDEF FPC}@{$ENDIF}RsZLibStreamError,
    {$IFNDEF FPC}@{$ENDIF}RsZLibErrNo,
    {$IFNDEF FPC}@{$ENDIF}RsZLibOK,
    {$IFNDEF FPC}@{$ENDIF}RsZLibStreamEnd,
    {$IFNDEF FPC}@{$ENDIF}RsZLibNeedDict
   );
begin
  case ErrorCode of
    Low(ErrorTexts)..High(ErrorTexts):
      Result := {$IFDEF FPC}@{$ENDIF}ErrorTexts[ErrorCode];
  else
    Result := @RsZLibUnknownError;
  end;
end;

//--------------------------------------------------------------------------------------------------

// if error then raise exception
// but not for Z_OK, Z_STREAM_END and Z_NEED_DICT
procedure Check(const ErrorCode: Integer);
begin
  if ErrorCode < Z_OK then
    raise EJclZLibError.CreateRes(GetZLibErrorText(ErrorCode));
end;

//--------------------------------------------------------------------------------------------------

constructor TJclZLibStream.Create(const Stream: TStream; const BufferSize: Integer);
begin
  inherited Create;

  FStream := Stream;
  // at least 1 byte buffer
  if BufferSize <= 0 then
    FBufferSize := JclZLibStreamDefaultBufferSize
  else
    FBufferSize := BufferSize;
  GetMem(FBuffer, FBufferSize);
end;

//--------------------------------------------------------------------------------------------------

destructor TJclZLibStream.Destroy;
begin
  FreeMem(FBuffer, FBufferSize);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclZLibStream.SetSize(NewSize: Longint);
begin
  raise EJclZLibError.CreateRes(@RsZLibNoSetSize);
end;

//--------------------------------------------------------------------------------------------------

function TJclZLibStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  raise EJclZLibError.CreateRes(@RsZLibNoSeek);
end;

//--------------------------------------------------------------------------------------------------
// TJclZLibReader
//--------------------------------------------------------------------------------------------------

constructor TJclZLibReader.Create(const Stream: TStream;
  const BufferSize: Integer = JclZLibStreamDefaultBufferSize;
  const WindowBits: Integer = DEF_WBITS);
begin
  inherited Create(Stream, BufferSize);
  FEndOfStream := False;
  ReadNextBlock;
  Check(inflateInit2(FZLibStream, WindowBits));
end;

//--------------------------------------------------------------------------------------------------

destructor TJclZLibReader.Destroy;
begin
  try  // Stream.Seek can raise any Exception
    if not FEndOfStream then
      FinishZLibStream;
  finally
    inherited Destroy;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclZLibReader.ReadNextBlock;
begin
  if (FZLibStream.avail_in = 0) and (not FEndOfStream) then
  begin
    FZLibStream.avail_in := FStream.Read(FBuffer^, FBufferSize);
    FZLibStream.next_in := FBuffer;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclZLibReader.SyncZLibStream;
var
  Err: Integer;
  Buf: array [0..255] of Byte;
begin
  // Skips leaving compressed data
  while not FEndOfStream do
  begin
    FZLibStream.next_out := @Buf;
    FZLibStream.avail_out := SizeOf(Buf);
    ReadNextBlock;
    if FZLibStream.avail_in = 0 then
      Exit;  // End of source file
    Err := inflate(FZLibStream, Z_NO_FLUSH);
    FEndOfStream := Err = Z_STREAM_END;  // End of zlib stream
    Check(Err);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclZLibReader.FinishZLibStream;
begin
  Check(inflateEnd(FZLibStream));
  // set stream position to end of zlib stream
  if FZLibStream.avail_in > 0 then
    FStream.Seek(-FZLibStream.avail_in, soFromCurrent);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclZLibReader.Reset;
begin
  SyncZLibStream;
  FEndOfStream := False;
  FZLibStream.total_in := 0;
  FZLibStream.total_out := 0;
  Check(inflateReset(FZLibStream));
end;

//--------------------------------------------------------------------------------------------------

function TJclZLibReader.Read(var Buffer; Count: Longint): Longint;
var
  Err: Integer;
begin
  Result := 0;
  if FEndOfStream then
    Exit;
  FZLibStream.next_out := @Buffer;
  FZLibStream.avail_out := Count;
  while FZLibStream.avail_out <> 0 do
  begin
    ReadNextBlock;
    if FZLibStream.avail_in = 0 then
      Exit;  // End of source file
    Err := inflate(FZLibStream, Z_NO_FLUSH);
    FEndOfStream := Err = Z_STREAM_END;  // End of zlib stream
    Result := Count - LongInt(FZLibStream.avail_out);
    if FEndOfStream then
    begin
      FinishZLibStream;
      Exit;
    end;
    Check(Err);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclZLibReader.Write(const Buffer; Count: Longint): Longint;
begin
  raise EJclZLibError.CreateRes(@RsZLibNoWrite);
end;

//--------------------------------------------------------------------------------------------------

function TJclZLibReader.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if (Offset = 0) and (Origin = soFromCurrent) then  // GetPosition
    Result := FZLibStream.total_out
  else
    Result := inherited Seek(Offset, Origin);
end;

//--------------------------------------------------------------------------------------------------
// TJclZLibWriter
//--------------------------------------------------------------------------------------------------

constructor TJclZLibWriter.Create(const Stream: TStream;
  const BufferSize: Integer = JclZLibStreamDefaultBufferSize;
  const Level: Integer = Z_DEFAULT_COMPRESSION;
  const Strategy: Integer = Z_DEFAULT_STRATEGY;
  const WindowBits: Integer = DEF_WBITS);
begin
  inherited Create(Stream, BufferSize);
  FZLibStream.next_out := FBuffer;
  FZLibStream.avail_out := FBufferSize;
  Check(deflateInit2(FZLibStream, Level, Z_DEFLATED, WindowBits, DEF_MEM_LEVEL, Strategy));
end;

//--------------------------------------------------------------------------------------------------

destructor TJclZLibWriter.Destroy;
begin
  FlushZLibStream(Z_FINISH);
  WriteNextBlock;
  Check(deflateEnd(FZLibStream));
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclZLibWriter.WriteNextBlock;
var
  Len: LongInt;
begin
  Len := FBufferSize - LongInt(FZLibStream.avail_out);
  if Len > 0 then
    FStream.WriteBuffer(FBuffer^, Len);
  FZLibStream.next_out := FBuffer;
  FZLibStream.avail_out := FBufferSize;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclZLibWriter.FlushZLibStream(const Flush: Integer);
var
  Err: Integer;
begin
  FZLibStream.next_in := nil;
  FZLibStream.avail_in := 0;
  repeat
    if FZLibStream.avail_out = 0 then
      WriteNextBlock;
    Err := deflate(FZLibStream, Flush);
  until Err <> Z_OK;
  Check(Err);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclZLibWriter.Reset;
begin
  FlushZLibStream(Z_FINISH);
  FZLibStream.total_in := 0;
  FZLibStream.total_out := 0;
  Check(deflateReset(FZLibStream));
end;

//--------------------------------------------------------------------------------------------------

function TJclZLibWriter.Read(var Buffer; Count: Longint): Longint;
begin
  raise EJclZLibError.CreateRes(@RsZLibNoRead);
end;

//--------------------------------------------------------------------------------------------------

function TJclZLibWriter.Write(const Buffer; Count: Longint): Longint;
var
  Err: Integer;
begin
  Result := 0;
  Err := Z_OK;
  FZLibStream.next_in := @Buffer;
  FZLibStream.avail_in := Count;
  while FZLibStream.avail_in <> 0 do
  begin
    if FZLibStream.avail_out = 0 then
      WriteNextBlock;
    Err := deflate(FZLibStream, Z_NO_FLUSH);
    Result := Count - LongInt(FZLibStream.avail_in);
    if Err <> Z_OK then Break;
  end;
  Check(Err);
end;

//--------------------------------------------------------------------------------------------------

function TJclZLibWriter.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if (Offset = 0) and (Origin = soFromCurrent) then  // GetPosition
    Result := FZLibStream.total_in
  else
    Result := inherited Seek(Offset, Origin);
end;

//--------------------------------------------------------------------------------------------------

function ZLibCompressMem(const Src: Pointer; SrcLen: Integer;
  out Dst: Pointer; out DstLen: Integer; out DstCapacity: Integer;
  const Level: Integer = Z_DEFAULT_COMPRESSION): Boolean;
var
  ZLibStream: TZStreamRec;
  Err: Integer;

  // calculate DstCapacity, at least 100.1% * SrcLen + 12
  procedure CalcDstCapacity(const Estimated: Double);
  var
    i: Integer;
  begin
    i := Round(Estimated * 1.002) + 12;
    i := (i + 15) and not 15;
    if DstCapacity < i then
      DstCapacity := i
    else
      DstCapacity := DstCapacity + 16;
  end;

begin
  Result := False;
  Dst := nil;
  DstLen := 0;
  DstCapacity := 0;
  if (SrcLen = 0) or (not Assigned(Src)) then
    Exit;
    
  CalcDstCapacity(SrcLen);
  GetMem(Dst, DstCapacity);
  try
    FillChar(ZLibStream, SizeOf(ZLibStream), 0);

    ZLibStream.next_in := Src;
    ZLibStream.avail_in := SrcLen;
    ZLibStream.total_in := 0;

    ZLibStream.next_out := Dst;
    ZLibStream.avail_out := DstCapacity;
    ZLibStream.total_out := 0;

    Check(deflateInit(ZLibStream, Level));
    try
      repeat
        Err := deflate(ZLibStream, Z_FINISH);

        if Err = Z_OK then  // there was not enough output space
        begin
          CalcDstCapacity(SrcLen * (ZLibStream.total_out / ZLibStream.total_in));
          ReAllocMem(Dst, DstCapacity);

          ZLibStream.next_out := Pointer(LongWord(Dst) + ZLibStream.total_out);
          ZLibStream.avail_out := LongWord(DstCapacity) - ZLibStream.total_out;
        end;
      until Err <> Z_OK;
      Check(Err);
    finally
      Check(deflateEnd(ZLibStream));
    end;
    DstLen := ZLibStream.total_out;
    Result := True;
  except
    FreeMem(Dst);
    Dst := nil;
    DstLen := 0;
    DstCapacity := 0;
    raise;
  end;
end;

//--------------------------------------------------------------------------------------------------

function ZLibDecompressMem(const Src: Pointer; SrcLen: Integer;
  out Dst: Pointer; out DstLen: Integer; var DstCapacity: Integer;
  const Flush: Integer = Z_SYNC_FLUSH): Boolean;
var
  ZLibStream: TZStreamRec;
  Err: Integer;

  // calculate DstCapacity, 120% of Estimated
  procedure CalcDstCapacity(const Estimated: Double);
  var
    i: Integer;
  begin
    i := Round(Estimated * 1.2);
    i := (i + 15) and not 15;
    if DstCapacity < i then
      DstCapacity := i
    else
      DstCapacity := DstCapacity + 16;
  end;

begin
  Result := False;
  DstLen := 0;
  Dst := nil;
  if (SrcLen = 0) or (not Assigned(Src)) then
    Exit;

  if DstCapacity <= 0 then
    CalcDstCapacity(SrcLen * 2);

  GetMem(Dst, DstCapacity);
  try
    FillChar(ZLibStream, SizeOf(ZLibStream), 0);

    ZLibStream.next_in := Src;
    ZLibStream.avail_in := SrcLen;
    ZLibStream.total_in := 0;

    ZLibStream.next_out := Dst;
    ZLibStream.avail_out := DstCapacity;
    ZLibStream.total_out := 0;

    Check(inflateInit(ZLibStream));
    try
      repeat
        Err := inflate(ZLibStream, Flush);

        if Err = Z_OK then  // there was not enough output space
        begin
          CalcDstCapacity(SrcLen * (ZLibStream.total_out / ZLibStream.total_in));
          ReAllocMem(Dst, DstCapacity);

          ZLibStream.next_out := Pointer(LongWord(Dst) + ZLibStream.total_out);
          ZLibStream.avail_out := LongWord(DstCapacity) - ZLibStream.total_out;
        end;
      until Err <> Z_OK;
      Check(Err);
    finally
      Check(inflateEnd(ZLibStream));
    end;
    DstLen := ZLibStream.total_out;
    Result := True;
  except
    FreeMem(Dst);
    Dst := nil;
    DstLen := 0;
    DstCapacity := 0;
    raise;
  end;
end;

//==================================================================================================
// gzip format support
//==================================================================================================

constructor TJclGZipStream.Create(const Stream: TStream);
begin
  inherited Create;
  FStream := Stream;
  FCRC32 := crc32(0, nil, 0);  // get crc32 initial value
  FUncompressedSize := 0;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclGZipStream.Destroy;
begin
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclGZipStream.SetSize(NewSize: Longint);
begin
  raise EJclGZipError.CreateRes(@RsGzipNoSetSize);
end;

//--------------------------------------------------------------------------------------------------

function TJclGZipStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  raise EJclGZipError.CreateRes(@RsGzipNoSeek);
end;

//--------------------------------------------------------------------------------------------------
// TJclGZipReader
//--------------------------------------------------------------------------------------------------

const
  gzipMagic = $8B1F;

  gzipFlag_ASCII_FLAG   = $01;  // bit 0 set: file probably ascii text
  gzipFlag_CONTINUATION = $02;  // bit 1 set: continuation of multi-part gzip file
  gzipFlag_EXTRA_FIELD  = $04;  // bit 2 set: extra field present
  gzipFlag_ORIG_NAME    = $08;  // bit 3 set: original file name present
  gzipFlag_COMMENT      = $10;  // bit 4 set: file comment present
  gzipFlag_ENCRYPTED    = $20;  // bit 5 set: file is encrypted
  gzipFlag_RESERVED     = $C0;  // bits 5..7: reserved

constructor TJclGZipReader.Create(const Stream: TStream;
  const BufferSize: Integer = JclZLibStreamDefaultBufferSize;
  const LineSeparator: string = JclZLibDefaultLineSeparator);
var
  b: Byte;
  c: AnsiChar;
  w: Word;
  Flags: Byte;
  EncryptionHeader: array [0..11] of Byte;  // placeholder
begin
  inherited Create(Stream);
                                        
  // check ID
  Stream.ReadBuffer(w, SizeOf(w));
  if w <> gzipMagic then
    raise EJclGZipError.CreateRes(@RsGzipNoGZipStream);

  // check compression mode
  Stream.ReadBuffer(b, SizeOf(b));
  if not (b in [Z_DEFLATED]) then
    raise EJclGZipError.CreateRes(@RsGzipNoDeflate);

  // read flags
  Stream.ReadBuffer(Flags, SizeOf(Flags));
  if (Flags and gzipFlag_CONTINUATION) <> 0 then
    raise EJclGZipError.CreateRes(@RsGzipMultipartNotSupported);
  if (Flags and gzipFlag_ENCRYPTED) <> 0 then
    raise EJclGZipError.CreateRes(@RsGzipEncryptedNotSupported);
  if (Flags and gzipFlag_RESERVED) <> 0 then
    raise EJclGZipError.CreateRes(@RsGzipUnknownFlags);

  // get TextMode
  FTextMode := (Flags and gzipFlag_ASCII_FLAG) <> 0;

  // read Timestamp
  Stream.ReadBuffer(FTimeStamp, SizeOf(FTimeStamp));

  // read compression level (extra flags)
  Stream.ReadBuffer(b, SizeOf(b));
  case b of
    2: FLevel := Z_BEST_COMPRESSION;
    4: FLevel := Z_BEST_SPEED;
  else
    FLevel := Z_DEFAULT_COMPRESSION;
  end;

  // read operating system
  Stream.ReadBuffer(FOperatingSystem, SizeOf(FOperatingSystem));

  // read multi-part number (second part = 1)
  if (Flags and gzipFlag_CONTINUATION) <> 0 then
    Stream.ReadBuffer(FMultipartNumber, SizeOf(FMultipartNumber));

  // read ExtraField
  if (Flags and gzipFlag_EXTRA_FIELD) <> 0 then
  begin
    Stream.ReadBuffer(w, SizeOf(w));
    FExtraFieldSize := w;
    GetMem(FExtraField, FExtraFieldSize);
    Stream.ReadBuffer(FExtraField^, FExtraFieldSize);
  end;

  // read filename
  if (Flags and gzipFlag_ORIG_NAME) <> 0 then
  begin
    repeat
      Stream.ReadBuffer(c, SizeOf(c));
      if c <> #0 then
        FFilename := FFilename + c;
    until c = #0;
  end;

  // read comment
  if (Flags and gzipFlag_COMMENT) <> 0 then
  begin
    repeat
      Stream.ReadBuffer(c, SizeOf(c));
      case c of
        #0: ;
        #$0A: FComment := FComment + LineSeparator;  // replace newline
      else
        FComment := FComment + c;
      end;
    until c = #0;
  end;

  // read encryption header
  if (Flags and gzipFlag_ENCRYPTED) <> 0 then
    Stream.ReadBuffer(EncryptionHeader, SizeOf(EncryptionHeader));

  // windowBits is passed < 0 to tell that there is no zlib header
  FZLibReader := TJclZLibReader.Create(Stream, BufferSize, -MAX_WBITS);
end;

//--------------------------------------------------------------------------------------------------

destructor TJclGZipReader.Destroy;
begin
  FZLibReader.Free;
  if Assigned(FExtraField) then
    FreeMem(FExtraField);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TJclGZipReader.Read(var Buffer; Count: Longint): Longint;
var
  xCRC32: LongWord;
  xUncompressedSize: LongWord;
begin
  Result := 0;
  if FEndOfStream then
    Exit;
  // read bytes from stream
  Result := FZLibReader.Read(Buffer, Count);
  // calculate CRC and Size
  FCRC32 := crc32(FCRC32, @Buffer, Result);
  FUncompressedSize := FUncompressedSize + LongWord(Result);
  // check end
  FEndOfStream := FZLibReader.EndOfStream;
  if FEndOfStream then
  begin
    FStream.ReadBuffer(xCRC32, SizeOf(xCRC32));
    FStream.ReadBuffer(xUncompressedSize, SizeOf(xUncompressedSize));
    if FCRC32 <> xCRC32 then
      raise EJclGZipError.CreateRes(@RsGzipCRCError);
    if FUncompressedSize <> xUncompressedSize then
      raise EJclGZipError.CreateRes(@RsGzipSizeError);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclGZipReader.Write(const Buffer; Count: Longint): Longint;
begin
  raise EJclGZipError.CreateRes(@RsGzipNoWrite);
end;

//--------------------------------------------------------------------------------------------------

function ConvertNewLineToLF(const Value: string): string;
var
  SrcPtr: PChar;
  DstIdx: Integer;

  procedure AddChar(c: Char);
  begin
    Result[DstIdx] := c;
    Inc(DstIdx);
  end;

begin
  SetLength(Result, Length(Value));
  DstIdx := 1;
  SrcPtr := Pointer(Value);
  while SrcPtr^ <> #0 do
  begin
    case SrcPtr^ of
      #$0D:
        begin
          Inc(SrcPtr);
          if SrcPtr^ = #$0A then
            Inc(SrcPtr);
          AddChar(#$0A);
        end;
      #$0A:
        begin
          Inc(SrcPtr);
          if SrcPtr^ = #$0D then
            Inc(SrcPtr);
          AddChar(#$0A);
        end;
    else
      AddChar(SrcPtr^);
      Inc(SrcPtr);
    end;
  end;
  SetLength(Result, DstIdx - 1);
end;

//--------------------------------------------------------------------------------------------------
// TJclGZipWriter
//--------------------------------------------------------------------------------------------------

constructor TJclGZipWriter.Create(const Stream: TStream;
  const BufferSize: Integer = JclZLibStreamDefaultBufferSize;
  const Level: Integer = Z_DEFAULT_COMPRESSION;
  const Strategie: Integer = Z_DEFAULT_STRATEGY;
  const Filename: string = '';
  const TimeStamp: TJclUnixTime32 = 0;
  const Comment: string = '';
  const TextMode: Boolean = False;
  const ExtraField: Pointer = nil;
  const ExtraFieldSize: Integer = 0);
var
  b: Byte;
  w: Word;
  s: string;
  Flags: Byte;
begin
  inherited Create(Stream);
  FTextMode := TextMode;

  // write ID
  w := gzipMagic;
  Stream.WriteBuffer(w, SizeOf(w));

  // write compression mode
  b := Z_DEFLATED;
  Stream.WriteBuffer(b, SizeOf(b));

  // write flags
  Flags := 0;
  if TextMode then
    Flags := Flags or gzipFlag_ASCII_FLAG;
  if Assigned(ExtraField) and (ExtraFieldSize > 0) then
    Flags := Flags or gzipFlag_EXTRA_FIELD;
  if Length(Filename) > 0 then
    Flags := Flags or gzipFlag_ORIG_NAME;
  if Length(Comment) > 0 then
    Flags := Flags or gzipFlag_COMMENT;
  Stream.WriteBuffer(Flags, SizeOf(Flags));

  // write Timestamp
  Stream.WriteBuffer(TimeStamp, SizeOf(TimeStamp));

  // write compression level
  case Level of
    Z_BEST_COMPRESSION:
      b := 2;
    Z_BEST_SPEED:
      b := 4;
  else
    b := 0;
  end;
  Stream.WriteBuffer(b, SizeOf(b));

  // write operating system
  b := 14;  // VFAT file system (Win95, NT)
  Stream.WriteBuffer(b, SizeOf(b));

  // write ExtraField
  if (Flags and gzipFlag_EXTRA_FIELD) <> 0 then
  begin
    w := ExtraFieldSize;
    Stream.WriteBuffer(w, SizeOf(w));
    Stream.WriteBuffer(ExtraField^, w);
  end;

  // write filename
  if (Flags and gzipFlag_ORIG_NAME) <> 0 then
  begin
    s := ExtractFileName(Filename);
    {$IFDEF MSWINDOWS}
    // convert to lower case, because caseinsensitive filenames
    s := AnsiLowerCase(s);
    {$ENDIF MSWINDOWS}
    Stream.WriteBuffer(Pointer(s)^, StrLen(Pointer(s)) + 1);
  end;

  // write comment
  if (Flags and gzipFlag_COMMENT) <> 0 then
  begin
    s := ConvertNewLineToLF(Comment);
    Stream.WriteBuffer(Pointer(s)^, Length(s) + 1);
  end;

  // windowBits is passed < 0 to suppress zlib header
  FZLibWriter := TJclZLibWriter.Create(Stream, BufferSize, Level,
                                       Z_DEFAULT_STRATEGY, -MAX_WBITS);
end;

//--------------------------------------------------------------------------------------------------

destructor TJclGZipWriter.Destroy;
begin
  FZLibWriter.Free;
  FStream.WriteBuffer(FCRC32, SizeOf(FCRC32));
  FStream.WriteBuffer(FUncompressedSize, SizeOf(FUncompressedSize));
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TJclGZipWriter.Read(var Buffer; Count: Longint): Longint;
begin
  raise EJclGZipError.CreateRes(@RsGzipNoRead);
end;

//--------------------------------------------------------------------------------------------------

function TJclGZipWriter.Write(const Buffer; Count: Longint): Longint;
var
  s: string;
  p: Pointer;
begin
  if FTextMode then
  begin
    SetString(s, PChar(@Buffer), Count);
    s := ConvertNewLineToLF(s);
    p := Pointer(s);
    Count := Length(s);
  end
  else
    p := @Buffer;
  Result := FZLibWriter.Write(p^, Count);
  // calculate CRC and Size
  FCRC32 := crc32(FCRC32, p, Result);
  FUncompressedSize := FUncompressedSize + LongWord(Result);
end;

//==================================================================================================
// gzip file support
//==================================================================================================

const
  MaxBufferSize = 1024 * 1024;  // 1 MByte
  BufferBlockSize = 32 * 1024;

//--------------------------------------------------------------------------------------------------

procedure GZipCompressFile(const SrcFilename: string; DstFilename: string;
  const Level: Integer = Z_DEFAULT_COMPRESSION);
var
  Src, Dst: TFileStream;
  DstPath: string;
  gzip: TJclGZipWriter;
  {$IFDEF MSWINDOWS}
  FileInfo: TByHandleFileInformation;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  FileInfo: TStatBuf;
  TimeInfo: TUTimeBuffer;
  {$ENDIF UNIX}
  GZipTime: TJclUnixTime32;
  Buffer: Pointer;
  BufferSize: Integer;
  BlockSize: Integer;
begin
  Src := TFileStream.Create(SrcFilename, fmOpenRead or fmShareDenyWrite);
  try
    // Get file time stamp and attributes
    {$IFDEF MSWINDOWS}
    if not GetFileInformationByHandle(Src.Handle, FileInfo) then
      RaiseLastOSError;
    GZipTime := FileTimeToUnixTime(FileInfo.ftLastWriteTime);
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    if stat(PChar(SrcFileName), FileInfo) <> 0 then
      RaiseLastOSError;
    GZipTime := FileInfo.st_mtime;
    {$ENDIF UNIX}
    // Allocate Buffer
    BufferSize := Src.Size;
    if BufferSize > MaxBufferSize then  // Limit the buffer size
      BufferSize := MaxBufferSize;
    DstPath := ExtractFilePath(DstFilename);
    DstFilename := ExtractFilename(DstFilename);

    if DstPath = '' then
    begin
      DstPath := ExtractFilePath(SrcFilename);  // path from SrcFilename
    end;

    if DstFilename = '' then
    begin
      DstFilename := ExtractFilename(SrcFilename) + JclGZipDefaultFileExtension;
    end;

    DstFilename := DstPath + DstFilename;

    Dst := TFileStream.Create(DstFilename, fmCreate);
    try
      gzip := TJclGZipWriter.Create(Dst, JclZLibStreamDefaultBufferSize,
        Level, Z_DEFAULT_STRATEGY, SrcFilename, GZipTime, '', False, nil, 0);
      try
        if BufferSize > 0 then
        begin
          GetMem(Buffer, BufferSize);
          try
            repeat
              BlockSize := Src.Read(Buffer^, BufferSize);
              gzip.WriteBuffer(Buffer^, BlockSize);
            until BlockSize <> BufferSize;
          finally
            FreeMem(Buffer, BufferSize);
          end;
        end;
      finally
        gzip.Free;
      end;
      // set file time stamp (without error check)
      {$IFDEF MSWINDOWS}
      with FileInfo do
        SetFileTime(Dst.Handle, @ftCreationTime, @ftLastAccessTime, @ftLastWriteTime);
      {$ENDIF MSWINDOWS}
    finally
      Dst.Free;
    end;
    {$IFDEF UNIX}
    // set file time stamp (without error check)
    TimeInfo.actime := FileInfo.st_atime;   // Access time
    TimeInfo.modtime := FileInfo.st_mtime;  // Modification time
    utime(PChar(DstFilename), @TimeInfo);
    // set file attributes (without error check)
    chmod(PChar(DstFilename), FileInfo.st_mode and $00000FFF);
    chown(PChar(DstFilename), FileInfo.st_uid, FileInfo.st_gid);
    {$ENDIF UNIX}
    {$IFDEF MSWINDOWS}
    // set file attributes (without error check)
    SetFileAttributes(PChar(DstFilename), FileInfo.dwFileAttributes);
    {$ENDIF MSWINDOWS}
  finally
    Src.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure GZipDecompressFile(const SrcFilename: string; DstFilename: string);
var
  Src, Dst: TFileStream;
  DstPath: string;
  gzip: TJclGZipReader;
  {$IFDEF MSWINDOWS}
  FileInfo: TByHandleFileInformation;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  FileInfo: TStatBuf;
  TimeInfo: TUTimeBuffer;
  {$ENDIF UNIX}
  Buffer: Pointer;
  BufferSize: Integer;
  BlockSize: Integer;
begin
  Src := TFileStream.Create(SrcFilename, fmOpenRead or fmShareDenyWrite);
  try
    // Get file time stamp and attributes
    {$IFDEF MSWINDOWS}
    if not GetFileInformationByHandle(Src.Handle, FileInfo) then
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    if stat(PChar(SrcFileName), FileInfo) <> 0 then
    {$ENDIF UNIX}
      RaiseLastOSError;
    // Allocate Buffer
    BufferSize := Src.Size;
    if BufferSize > (MaxBufferSize div 2) then
      BufferSize := MaxBufferSize  // Limit the buffer size
    else
      BufferSize := ((BufferSize * 2 + BufferBlockSize - 1) div BufferBlockSize) * BufferBlockSize;

    gzip := TJclGZipReader.Create(Src, JclZLibStreamDefaultBufferSize);
    try
      DstPath := ExtractFilePath(DstFilename);
      DstFilename := ExtractFilename(DstFilename);

      if DstPath = '' then
        DstPath := ExtractFilePath(SrcFilename);  // path from SrcFilename

      if DstFilename = '' then  // no override destination filename
      begin
        if gzip.Filename = '' then  // no gzip name field
          // use the filename from SrcFilename without the last ext. part
          DstFilename := ChangeFileExt(ExtractFilename(SrcFilename), '')
        else
          DstFilename := gzip.Filename;  // use the gzip name field
      end;

      DstFilename := DstPath + DstFilename;

      Dst := TFileStream.Create(DstFilename, fmCreate);
      try
        GetMem(Buffer, BufferSize);
        try
          while not gzip.EndOfStream do
          begin
            BlockSize := gzip.Read(Buffer^, BufferSize);
            Dst.WriteBuffer(Buffer^, BlockSize);
            if BlockSize <> BufferSize then
              Break;
          end;
        finally
          FreeMem(Buffer, BufferSize);
        end;
        {$IFDEF MSWINDOWS}
        // set file time stamp (without error check)
        if gzip.TimeStamp <> 0 then
          FileInfo.ftLastWriteTime := UnixTimeToFileTime(gzip.TimeStamp);
        with FileInfo do
          SetFileTime(Dst.Handle, @ftCreationTime, @ftLastAccessTime, @ftLastWriteTime);
        {$ENDIF MSWINDOWS}
      finally
        Dst.Free;
      end;
      {$IFDEF UNIX}
      // set file time stamp (without error check)
      TimeInfo.actime := FileInfo.st_atime;     // Access time
      if gzip.TimeStamp = 0 then                // Modification time
        TimeInfo.modtime := FileInfo.st_mtime
      else
        TimeInfo.modtime := gzip.TimeStamp;
      utime(PChar(DstFilename), @TimeInfo);
      // set file attributes (without error check)
      chmod(PChar(DstFilename), FileInfo.st_mode and $00000FFF);
      chown(PChar(DstFilename), FileInfo.st_uid, FileInfo.st_gid);
      {$ENDIF UNIX}
      {$IFDEF MSWINDOWS}
      // set file attributes (without error check)
      SetFileAttributes(PChar(DstFilename), FileInfo.dwFileAttributes);
      {$ENDIF MSWINDOWS}
    finally
      gzip.Free;
    end;
  finally
    Src.Free;
  end;
end;

//==================================================================================================
// tar archive support
//==================================================================================================

function OctalToInt(const Value: array of AnsiChar; MaxValue: TJclTarFileSize): TJclTarFileSize;
var
  I: Integer;
  V: string;
  C: AnsiChar;
begin
  I := Low(Value);
  while (I <= High(Value)) and (Value[I] <> #0) do
    Inc(I);
  SetString(V, PChar(@Value), I);
  V := Trim(V);
  // convert
  Result := 0;
  for I := 1 to Length(V) do
  begin
    C := V[I];
    case C of
      '0'..'7':
        Result := (Result shl 3) or (Ord(C) - Ord('0'));
    else
      {$IFDEF DELPHI3}
      raise EConvertError.CreateFmt(RsTarOctalToIntInvalidCharacters, [V]);
      {$ELSE DELPHI3}
      raise EConvertError.CreateResFmt(@RsTarOctalToIntInvalidCharacters, [V]);
      {$ENDIF DELPHI3}
    end;
  end;
  // check range
  if Result > MaxValue then
    {$IFDEF DELPHI3}
    raise EConvertError.CreateFmt(RsTarOctalToIntOutOfRange, [V]);
    {$ELSE DELPHI3}
    raise EConvertError.CreateResFmt(@RsTarOctalToIntOutOfRange, [V]);
    {$ENDIF DELPHI3}
end;

//--------------------------------------------------------------------------------------------------

function CalculateTarChecksum(Header: TTarHeader): Integer;
var
  I: Integer;
begin
  Result := ($09C - $094) * Ord(' ');
  for I := 0 to $093 do
    Result := Result + Header.Buffer[i];
  for I := $09C to $1FF do
    Result := Result + Header.Buffer[i];
end;

//--------------------------------------------------------------------------------------------------

constructor TJclTarReader.Create(const TarStream: TStream);
begin
  inherited Create;
  FTarStream := TarStream;
  if ReadHeader then
    ScanHeader;
end;

//--------------------------------------------------------------------------------------------------

function TJclTarReader.ReadHeader: Boolean;
var
  I: Integer;
  IsNullBlock: Boolean;
begin
  Result := FTarStream.Read(FHeader, SizeOf(FHeader)) = SizeOf(FHeader);
  if Result then
  begin
    // check for 0 block
    IsNullBlock := True;
    for I := Low(FHeader.Buffer) to High(FHeader.Buffer) do
    begin
      if FHeader.Buffer[I] <> 0 then
      begin
        IsNullBlock := False;
        Break;
      end;
    end;
    if IsNullBlock then
      Result := ReadHeader;
  end
  else
  begin
    FArchiveFormat := Low(FArchiveFormat);
    FFileType := tftEof;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTarReader.ScanHeader;
var
  {$IFNDEF UNIX}
  I: Integer;
  {$ENDIF ~UNIX}
  Prefix: string;
begin
  if FFileType = tftEof then
    Exit;
  // get archive format
  if StrLComp(FHeader.Magic, TarOldGnuMagic, SizeOf(TarOldGnuMagic)) = 0 then
    FArchiveFormat := tafOldGnuFormat
  else
  if StrLComp(FHeader.Magic, TarPosixMagic, SizeOf(TarPosixMagic)) = 0 then
    FArchiveFormat := tafPosixFormat  // or tafGnuFormat
  else
    FArchiveFormat := tafV7Format;
  // get file type
  case FHeader.TypeFlag of
    ttfRegFile, ttfARegFile:
      FFileType := tftFile;
    ttfDirectory:
      FFileType := tftDirectory;
  else
    FFileType := tftUnknown;
  end;
  // get file name
  FFilename := FHeader.Name;
  case FArchiveFormat of
    tafPosixFormat, tafGnuFormat:
      begin
        Prefix := FHeader.Prefix;
        if Prefix <> '' then
          FFilename := Prefix + UnixPathDelimiter + FFilename; 
      end;
  end;
  {$IFNDEF UNIX}
  // correct path delimiter 
  for I := 1 to Length(FFilename) do
    if FFilename[I] = UnixPathDelimiter then
      FFilename[I] := PathDelim;
  {$ENDIF ~UNIX}
  // get file size
  FFileSize := OctalToInt(FHeader.Size, High(FFileSize));
  // get file date
  FFileTime := OctalToInt(FHeader.MTime, High(FFileTime));
  if OctalToInt(FHeader.Chksum, High(Integer)) <> CalculateTarChecksum(FHeader) then
    raise EJclTarError.CreateRes(@RsTarChecksumError);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTarReader.CopyToStream(const FileStream: TStream;
  CanSeek: Boolean = False);
var
  Buffer: array [0..TarBlockSize - 1] of Byte;
  Blocks, I: Integer;
  ReadedBytes: Integer;
  RestBytes: Integer;
begin
  Blocks := (FFileSize + (TarBlockSize - 1)) div TarBlockSize;
  if not Assigned(FileStream) and CanSeek then
    FTarStream.Seek(Blocks * TarBlockSize, soFromCurrent)
  else
  begin
    RestBytes := FFileSize;
    for I := 0 to Blocks - 1 do
    begin
      ReadedBytes := FTarStream.Read(Buffer, TarBlockSize);
      // schreiben
      if Assigned(FileStream) then
      begin
        if ReadedBytes > RestBytes then
          ReadedBytes := RestBytes;
        FileStream.WriteBuffer(Buffer, ReadedBytes);
        RestBytes := RestBytes - ReadedBytes;
      end;
    end;
  end;
  if ReadHeader then
    ScanHeader;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTarReader.CopyToFile(const FilePath: string);
var
  Filename: string;
  FileDir: string;
  FileStream: TFileStream;
  UnixFileTime: TJclUnixTime32;
  {$IFDEF MSWINDOWS}
  FileTime: TFileTime;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  TimeInfo: TUTimeBuffer;
  {$ENDIF UNIX}
begin
  Filename := {$IFDEF XPLATFORM_RTL} IncludeTrailingPathDelimiter {$ELSE} IncludeTrailingBackslash{$ENDIF}(FilePath) + FFilename;
  FileDir := {$IFDEF XPLATFORM_RTL} ExcludeTrailingPathDelimiter {$ELSE} ExcludeTrailingBackslash{$ENDIF}(ExtractFilePath(Filename));
  if FileDir <> '' then
    ForceDirectories(FileDir);
  FileStream := TFileStream.Create(Filename, fmCreate);
  try
    UnixFileTime := FFileTime;
    CopyToStream(FileStream, False);
    {$IFDEF MSWINDOWS}
    FileTime := UnixTimeToFileTime(UnixFileTime);
    SetFileTime(FileStream.Handle, nil, nil, @FileTime);  // without error handling
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    TimeInfo.actime := UnixFileTime;
    TimeInfo.modtime := UnixFileTime;
    utime(PChar(FileName), @TimeInfo);  // without error handling
    {$ENDIF UNIX}
  finally
    FileStream.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTarReader.SkipFile;
begin
  CopyToStream(nil, False);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTarReader.SkipFileSeek;
begin
  CopyToStream(nil, True);
end;

//--------------------------------------------------------------------------------------------------

function TJclTarReader.GetFileDateTime: TDateTime;
begin
  Result := UnixTimeToDateTime(FFileTime);
end;

//--------------------------------------------------------------------------------------------------

procedure UnTarAllFiles(const TarFilename: string; DstDir: string);
var
  TarFile: TFileStream;
  TarReader: TJclTarReader;
  DstFilename: string;
begin
  if DstDir = '' then
    DstDir := ChangeFileExt(TarFilename, '')
  else
    DstDir := {$IFDEF XPLATFORM_RTL} ExcludeTrailingPathDelimiter {$ELSE} ExcludeTrailingBackslash{$ENDIF}(DstDir);
  if DstDir = TarFilename then
    DstDir := DstDir + '.dir';
  DstDir := DstDir + PathDelim;           

  TarFile := TFileStream.Create(TarFilename, fmOpenRead or fmShareDenyWrite);
  try
    TarReader := TJclTarReader.Create(TarFile);
    try
      while TarReader.FileType <> tftEof do
      begin
        DstFilename := DstDir + TarReader.Filename;
        case TarReader.FileType of
          tftFile:
            TarReader.CopyToFile(DstDir);
          tftDirectory:
            begin
              ForceDirectories(DstFilename);
              TarReader.SkipFile;
            end;
        else
          TarReader.SkipFile;
        end;
      end;
    finally
      TarReader.Free;
    end;
  finally
    TarFile.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------
// TJclTarWriter
//--------------------------------------------------------------------------------------------------

constructor TJclTarWriter.Create(const TarStream: TStream);
begin
  inherited Create;
  FTarStream := TarStream;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclTarWriter.Destroy;
begin
  AddEof;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTarWriter.AddFile(FileRoot, Filename: string);
var
  FileStream: TFileStream;
  {$IFDEF MSWINDOWS}
  FileInfo: TByHandleFileInformation;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  FileInfo: TStatBuf;
  {$ENDIF UNIX}
  FileUnixTime: TJclUnixTime32;
begin
  FileRoot := {$IFDEF XPLATFORM_RTL} IncludeTrailingPathDelimiter {$ELSE} IncludeTrailingBackslash{$ENDIF}(FileRoot);
  FileStream := TFileStream.Create(FileRoot + Filename, fmOpenRead or fmShareDenyWrite);
  try
    FillChar(FileInfo, SizeOf(FileInfo), 0);
    {$IFDEF MSWINDOWS}
    if GetFileInformationByHandle(FileStream.Handle, FileInfo) then
      FileUnixTime := FileTimeToUnixTime(FileInfo.ftLastWriteTime)
    else
      FileUnixTime := 0;
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    if fstat(FileStream.Handle, FileInfo) = 0 then
      FileUnixTime := FileInfo.st_mtime
    else
      FileUnixTime := 0;
    {$ENDIF UNIX}
    AddStream(FileStream, Filename, FileStream.Size, FileUnixTime);
  finally
    FileStream.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure SetOctal(var Field: array of AnsiChar; Value: TJclTarFileSize);
var
  V: AnsiString;
  Delta: Integer;
  I: Integer;
begin
  V := '';
  repeat
    V := Chr((Value and $07) + Ord('0')) + V;
    Value := Value shr 3;
  until Value = 0;
  Delta := Length(Field) - Length(V);
  if Delta < 0 then
    raise EJclTarError.CreateRes(@RsTarSetOctalOutOfRange)
  else
  if Delta > 1 then
  begin
    // fill with 0
    for I := 2 to Delta do
      V := '0' + V;
  end;
  for I := 1 to Length(V) do
    Field[I - 1] := V[i];
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTarWriter.AddStream(const Stream: TStream; Filename: string;
  FileSize: TJclTarFileSize; FileTime: TJclUnixTime32);
var
  I: Integer;
  Header: TTarHeader;
  Blocks: Integer;
  RestBytes: Integer;
begin
  {$IFNDEF UNIX}
  // path delimiter -> UNIX 
  for I := 1 to Length(Filename) do
    if Filename[I] = PathDelim then
      Filename[I] := UnixPathDelimiter;
  {$ENDIF ~UNIX}
  FillChar(Header, SizeOf(Header), 0);
  StrPLCopy(Header.Name, Filename, Length(Header.Name) - 1);
  StrCopy(Header.Mode, '0000777');
  StrCopy(Header.UID, '0000000');
  StrCopy(Header.GID, '0000000');
  SetOctal(Header.Size, FileSize);
  SetOctal(Header.MTime, FileTime);
  Header.TypeFlag := '0';
  SetOctal(Header.Chksum, CalculateTarChecksum(Header));
  FTarStream.WriteBuffer(Header, SizeOf(Header));
  Blocks := FileSize div TarBlockSize;
  RestBytes := FileSize mod TarBlockSize;
  for I := 0 to Blocks - 1 do
  begin
    Stream.ReadBuffer(Header.Buffer, SizeOf(Header.Buffer));
    FTarStream.WriteBuffer(Header.Buffer, SizeOf(Header.Buffer));
  end;
  if RestBytes > 0 then
  begin
    FillChar(Header.Buffer, SizeOf(Header.Buffer), 0);
    Stream.ReadBuffer(Header.Buffer, RestBytes);
    FTarStream.WriteBuffer(Header.Buffer, SizeOf(Header.Buffer));
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTarWriter.AddDirectory(DirName: string);
var
  {$IFNDEF UNIX}
  I: Integer;
  {$ENDIF ~UNIX}
  Header: TTarHeader;
begin
  DirName := {$IFDEF XPLATFORM_RTL} IncludeTrailingPathDelimiter {$ELSE} IncludeTrailingBackslash{$ENDIF}(DirName);
  {$IFNDEF UNIX}
  // path delimiter -> UNIX
  for I := 1 to Length(DirName) do
    if DirName[I] = PathDelim then
      DirName[I] := UnixPathDelimiter;
  {$ENDIF ~UNIX}
  FillChar(Header, SizeOf(Header), 0);
  StrPLCopy(Header.Name, DirName, Length(Header.Name) - 1);
  StrCopy(Header.Mode, '0000777');
  StrCopy(Header.UID, '0000000');
  StrCopy(Header.GID, '0000000');
  SetOctal(Header.Size, 0);
  SetOctal(Header.MTime, 0);
  Header.TypeFlag := '5';
  SetOctal(Header.Chksum, CalculateTarChecksum(Header));
  FTarStream.WriteBuffer(Header, SizeOf(Header));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTarWriter.AddEof;
var
  Buffer: array [0..TarBlockSize - 1] of Byte;
begin
  FillChar(Buffer, SizeOf(Buffer), 0);
  FTarStream.WriteBuffer(Buffer, SizeOf(Buffer));
end;

//--------------------------------------------------------------------------------------------------

procedure UnGZipTarAllFiles(const TgzFilename: string; DstDir: string);
var
  TgzFile: TFileStream;
  GZipReader: TJclGZipReader;
  TarReader: TJclTarReader;
  DstFilename: string;
begin
  if DstDir = '' then
    DstDir := ChangeFileExt(TgzFilename, '')
  else
    DstDir := {$IFDEF XPLATFORM_RTL} ExcludeTrailingPathDelimiter {$ELSE} ExcludeTrailingBackslash{$ENDIF}(DstDir);
  if DstDir = TgzFilename then
    DstDir := DstDir + '.dir';
  DstDir := DstDir + PathDelim;

  TgzFile := TFileStream.Create(TgzFilename, fmOpenRead or fmShareDenyWrite);
  try
    GZipReader := TJclGZipReader.Create(TgzFile, JclZLibStreamDefaultBufferSize);
    try
      TarReader := TJclTarReader.Create(GZipReader);
      try
        while TarReader.FileType <> tftEof do
        begin
          DstFilename := DstDir + TarReader.Filename;
          case TarReader.FileType of
            tftFile:
              TarReader.CopyToFile(DstDir);
            tftDirectory:
              begin
                ForceDirectories(DstFilename);
                TarReader.SkipFile;
              end;
          else
            TarReader.SkipFile;
          end;
        end;
      finally
        TarReader.Free;
      end;
    finally
      GZipReader.Free;
    end;
  finally
    TgzFile.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure GetFileList(RootDir: string; List: TStrings);

  procedure ScanDir(const Dir: string);
  var
    Info: TSearchRec;
  begin
    if Dir <> '' then
      List.Add(Dir);
    if FindFirst(RootDir + Dir + '*.*', -1, Info) = 0 then
    try
      repeat
        if (Info.Attr and faDirectory) <> 0 then
        begin
          if (Info.Name <> '.') and (Info.Name <> '..') then
            ScanDir(Dir + Info.Name + PathDelim);
        end
        else
          List.Add(Dir + Info.Name);
      until FindNext(Info) <> 0;
    finally
      SysUtils.FindClose(Info);
    end;
  end;

begin
  RootDir := {$IFDEF XPLATFORM_RTL} IncludeTrailingPathDelimiter {$ELSE} IncludeTrailingBackslash{$ENDIF}(RootDir);
  List.BeginUpdate;
  try
    ScanDir('');
  finally
    List.EndUpdate;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure ArrayToList(const Filenames: array of string; List: TStrings);
var
  I: Integer;
begin
  List.BeginUpdate;
  try
    for I := Low(Filenames) to High(Filenames) do
      List.Add(Filenames[I]);
  finally
    List.EndUpdate;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TarFileList(const TarFilename, FileRoot: string; List: TStrings);
var
  TarFile: TFileStream;
  TarWriter: TJclTarWriter;
  I: Integer;
  Filename: string;
begin
  TarFile := TFileStream.Create(TarFilename, fmCreate);
  try
    TarWriter := TJclTarWriter.Create(TarFile);
    try
      for I := 0 to List.Count - 1 do
      begin
        Filename := List[I];
        if Filename <> '' then
          if Filename[Length(Filename)] = PathDelim then
            TarWriter.AddDirectory(Filename)
          else
            TarWriter.AddFile(FileRoot, Filename);
      end;
    finally
      TarWriter.Free;
    end;
  finally
    TarFile.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TarAllFiles(const TarFilename, FileRoot: string);
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    GetFileList(FileRoot, List);
    TarFileList(TarFilename, FileRoot, List);
  finally
    List.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TarFileArray(const TarFilename, FileRoot: string; const Filenames: array of string);
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    ArrayToList(Filenames, List);
    TarFileList(TarFilename, FileRoot, List);
  finally
    List.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TarGZipFileList(const TgzFilename, FileRoot: string; List: TStrings);
var
  TgzFile: TFileStream;
  GZipWriter: TJclGZipWriter;
  TarWriter: TJclTarWriter;
  I: Integer;
  Filename: string;
begin
  TgzFile := TFileStream.Create(TgzFilename, fmCreate);
  try
    {$IFDEF DELPHI3}
      GZipWriter := TJclGZipWriter.CreateDef2(TgzFile,
        JclZLibStreamDefaultBufferSize, Z_BEST_COMPRESSION);
    {$ELSE DELPHI3}
      GZipWriter := TJclGZipWriter.Create(TgzFile,
        JclZLibStreamDefaultBufferSize, Z_BEST_COMPRESSION);
    {$ENDIF DELPHI3}
    try
      TarWriter := TJclTarWriter.Create(GZipWriter);
      try
        for I := 0 to List.Count - 1 do
        begin
          Filename := List[I];
          if Filename <> '' then
          begin
            if Filename[Length(Filename)] = PathDelim then
              TarWriter.AddDirectory(Filename)
            else
              TarWriter.AddFile(FileRoot, Filename);
          end;
        end;
      finally
        TarWriter.Free;
      end;
    finally
      GZipWriter.Free;
    end;
  finally
    TgzFile.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TarGZipAllFiles(const TgzFilename, FileRoot: string);
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    GetFileList(FileRoot, List);
    TarGZipFileList(TgzFilename, FileRoot, List);
  finally
    List.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TarGZipFileArray(const TgzFilename, FileRoot: string; const Filenames: array of string);
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    ArrayToList(Filenames, List);
    TarGZipFileList(TgzFilename, FileRoot, List);
  finally
    List.Free;
  end;
end;

//  History:                                                                  

//  $Log$
//  Revision 1.15  2004/11/11 23:43:40  assarbad
//  - zlib.h converted from scratch (assarbad) - modifications by Robert (rrossmair)
//  - Makefile.mak and JclZlib.pas modifications from Robert (rrossmair)
//
//  Revision 1.14  2004/08/01 05:52:12  marquardt
//  move constructors/destructors
//
//  Revision 1.13  2004/07/31 06:21:01  marquardt
//  fixing TStringLists, adding BeginUpdate/EndUpdate, finalization improved
//
//  Revision 1.12  2004/07/28 18:00:52  marquardt
//  various style cleanings, some minor fixes
//
//  Revision 1.11  2004/06/16 07:30:28  marquardt
//  added tilde to all IFNDEF ENDIFs, inherited qualified
//
//  Revision 1.10  2004/06/14 13:05:18  marquardt
//  style cleaning ENDIF, Tabs
//
//  Revision 1.9  2004/06/14 11:05:51  marquardt
//  symbols added to all ENDIFs and some other minor style changes like removing IFOPT
//
//  Revision 1.8  2004/05/31 22:11:35  rrossmair
//  added Log CVS key word.
//
//
//  Revision 1.9  2004/05/31 22:04:22  rrossmair
//  added PJH disclaimer; some formatting. Not longer generated file.
//
//  Revision 1.8  2004/05/09 00:18:21  peterjhaas
//  - old history in reverse order like CVS log
//  - change interface crc32 to avoid FPC compatibility problems
//  - change GetZlibErrorText implementation for FPC compatibility
//  - change OctalToInt implementation FPC compatibility
//  - change Char to AnsiChar in fixed structures
//
//  Revision 1.7  2004/05/08 08:44:18  rrossmair
//  introduced & applied symbol HAS_UNIT_LIBC
//
//  Revision 1.6  2004/05/05 05:50:40  rrossmair
//  fixed typo: '}' instead of ')'
//
//  Revision 1.5  2004/05/05 05:24:58  rrossmair
//  fixed typo
//
//  Revision 1.4  2004/05/05 04:06:06  rrossmair
//  changes for FPC-compatibility (tested under Win32 only)
//
//  Revision 1.3  2004/05/05 00:36:16  mthoma
//  Updated headers: Added donors as contributors, adjusted the initial authors, added cvs names when they were not obvious. Changed $data to $date where necessary,
//
//  Revision 1.2  2004/04/29 01:53:55  peterjhaas
//  - add Prototype directive to avoid log problems
//
//  Revision 1.1  2004/04/18 00:40:02  peterjhaas
//  add prototypes for standalone library / JCL
//  be careful with any modification to avoid breaks
//                                                                     
//  2004-03-20
//   - Bugfix: TJclGZipReader.Create: read multi-part number
//
//  2004-03-17 Version 2.0
//   - JCL version
//
//  2003-04-22 Version 1.0.1
//   - Interface GZipCompressFile and GZipDecompressFile changed
//   - GZipCompressFile, GZipDecompressFile for Linux
//
//  2003-04-19 Version 1.0.1
//   - Interface ZLibCompressMem and ZLibDecompressMem changed
//   - Bugfix: ZLibDecompressMem
//
//  2003-04-14 Version 1.0
//   - First public version
//
//  2002-04-07 Version 0.9
//   - First public pre release
//

end.
