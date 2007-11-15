{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclCompression.pas.                                                         }
{                                                                                                  }
{ The Initial Developer of the Original Code is Matthias Thoma.                                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Alternatively, the contents of this file may be used under the terms of  the GNU Lesser General  }
{ Public License (the  "LGPL License"), in which case  the provisions of the LGPL License are      }
{ applicable instead of those above. If you wish to allow use of your version of this file only    }
{ under the terms of the LGPL License and not to allow others to use your version of this file     }
{ under the MPL, indicate your decision by deleting the provisions above and replace them with the }
{ notice and other provisions required by the LGPL License. If you do not delete the provisions    }
{ above, a recipient may use your version of this file under either the MPL or the LGPL License.   }
{                                                                                                  }
{ For more information about the LGPL:                                                             }
{ http://www.gnu.org/copyleft/lesser.html                                                          }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit is still in alpha state. It is likely that it will change a lot. Suggestions are       }
{ welcome.                                                                                         }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclCompression;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
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
  zlibh, bzip2,
  JclBase, JclStreams;

{**************************************************************************************************}
{
                                       TJclCompressionStream
                                               -  -
                         -----------------------  --------------------------
                         -                                                 -
                   TJclCompressStream                            TJclDecompressStream
                         -                                                 -
            ---------------------------------                ---------------------------------
            -               -               -                -                 -             -
            -               -               -                -                 -             -
    TJclZLibCompressStream  -   TBZIP2CompressStram   TJclZLibDecompressStream -   TBZIP2DeCompressStream
                            -                                                  -
                            -                                          TGZDecompressStream
                      TGZCompressStream

                                                                               }
{**************************************************************************************************}

type
  TJclCompressionStream = class(TJclStream)
  private
    FOnProgress: TNotifyEvent;
    FBuffer: Pointer;
    FBufferSize: Cardinal;
    FStream: TStream;
  protected
    function SetBufferSize(Size: Cardinal): Cardinal; virtual;
    procedure Progress(Sender: TObject); dynamic;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure Reset; virtual;
  end;

  TJclCompressStream = class(TJclCompressionStream)
  public
    function Flush: Integer; dynamic; abstract;
    constructor Create(Destination: TStream);
  end;

  TJclDecompressStream = class(TJclCompressionStream)
  private
    FOwnsStream: Boolean;
  public
    constructor Create(Source: TStream; AOwnsStream: Boolean = False);
    destructor Destroy; override;
  end;

  // ZIP Support
  TJclCompressionLevel = Integer;

  TJclZLibCompressStream = class(TJclCompressStream)
  private
    FWindowBits: Integer;
    FMemLevel: Integer;
    FMethod: Integer;
    FStrategy: Integer;
    FDeflateInitialized: Boolean;
    FCompressionLevel: Integer;
  protected
    ZLibRecord: TZStreamRec;
    procedure SetCompressionLevel(Value: Integer);
    procedure SetStrategy(Value: Integer);
    procedure SetMemLevel(Value: Integer);
    procedure SetMethod(Value: Integer);
    procedure SetWindowBits(Value: Integer);
  public
    constructor Create(Destination: TStream; CompressionLevel: TJclCompressionLevel = -1);
    destructor Destroy; override;
    function Flush: Integer; override;
    procedure Reset; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property WindowBits: Integer read FWindowBits write SetWindowBits;
    property MemLevel: Integer read FMemLevel write SetMemLevel;
    property Method: Integer read FMethod write SetMethod;
    property Strategy: Integer read FStrategy write SetStrategy;
    property CompressionLevel: Integer read FCompressionLevel write SetCompressionLevel;
  end;

  TJclZLibDecompressStream = class(TJclDecompressStream)
  private
    FWindowBits: Integer;
    FInflateInitialized: Boolean;
  protected
    ZLibRecord: TZStreamRec;
    procedure SetWindowBits(Value: Integer);
  public
    constructor Create(Source: TStream; WindowBits: Integer = DEF_WBITS; AOwnsStream: Boolean = False);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property WindowBits: Integer read FWindowBits write SetWindowBits;
  end;

  // GZIP Support

//=== { GZIP helpers } =======================================================

type
  TJclGZIPHeader = packed record
    ID1: Byte;
    ID2: Byte;
    CompressionMethod: Byte;
    Flags: Byte;
    ModifiedTime: Cardinal;
    ExtraFlags: Byte;
    OS: Byte;
  end;

  TJclGZIPFooter = packed record
    DataCRC32: Cardinal;
    DataSize: Cardinal;
  end;

const
  // ID1 and ID2 fields
  JCL_GZIP_ID1 = $1F; // value for the ID1 field
  JCL_GZIP_ID2 = $8B; // value for the ID2 field

  // Compression Model field
  JCL_GZIP_CM_DEFLATE = 8; // Zlib classic

  // Flags field : extra fields for the header
  JCL_GZIP_FLAG_TEXT = $01; // file is probably ASCII text
  JCL_GZIP_FLAG_CRC  = $02; // a CRC16 for the header is present
  JCL_GZIP_FLAG_EXTRA = $04; // extra fields present
  JCL_GZIP_FLAG_NAME = $08; // original file name is present
  JCL_GZIP_FLAG_COMMENT = $10; // comment is present

  // ExtraFlags field : compression level
  JCL_GZIP_EFLAG_MAX = 2; // compressor used maximum compression
  JCL_GZIP_EFLAG_FAST = 4; // compressor used fastest compression

  // OS field : file system
  JCL_GZIP_OS_FAT = 0; // FAT filesystem (MS-DOS, OS/2, NT/Win32)
  JCL_GZIP_OS_AMIGA = 1; // Amiga
  JCL_GZIP_OS_VMS = 2; // VMS (or OpenVMS)
  JCL_GZIP_OS_UNIX = 3; // Unix
  JCL_GZIP_OS_VM = 4; // VM/CMS
  JCL_GZIP_OS_ATARI = 5; // Atari TOS
  JCL_GZIP_OS_HPFS = 6; // HPFS filesystem (OS/2, NT)
  JCL_GZIP_OS_MAC = 7; // Macintosh
  JCL_GZIP_OS_Z = 8; // Z-System
  JCL_GZIP_OS_CPM = 9; // CP/M
  JCL_GZIP_OS_TOPS = 10; // TOPS-20
  JCL_GZIP_OS_NTFS = 11; // NTFS filesystem (NT)
  JCL_GZIP_OS_QDOS = 12; // QDOS
  JCL_GZIP_OS_ACORN = 13; // Acorn RISCOS
  JCL_GZIP_OS_UNKNOWN = 255; // unknown

type
  TJclGZIPSubFieldHeader = packed record
    SI1: Byte;
    SI2: Byte;
    Len: Word;
  end;

// constants to identify sub fields in the extra field
// source: http://www.gzip.org/format.txt
const
  JCL_GZIP_X_AC1 = $41; // AC Acorn RISC OS/BBC MOS file type information
  JCL_GZIP_X_AC2 = $43;
  JCL_GZIP_X_Ap1 = $41; // Ap Apollo file type information
  JCL_GZIP_X_Ap2 = $70;
  JCL_GZIP_X_cp1 = $63; // cp file compressed by cpio
  JCL_GZIP_X_cp2 = $70;
  JCL_GZIP_X_GS1 = $1D; // GS gzsig
  JCL_GZIP_X_GS2 = $53;
  JCL_GZIP_X_KN1 = $4B; // KN KeyNote assertion (RFC 2704)
  JCL_GZIP_X_KN2 = $4E;
  JCL_GZIP_X_Mc1 = $4D; // Mc Macintosh info (Type and Creator values)
  JCL_GZIP_X_Mc2 = $63;
  JCL_GZIP_X_RO1 = $52; // RO Acorn Risc OS file type information
  JCL_GZIP_X_RO2 = $4F;

type
  TJclGZIPFlag = (gfDataIsText, gfHeaderCRC16, gfExtraField, gfOriginalFileName, gfComment);
  TJclGZIPFlags = set of TJclGZIPFlag;
  TJclGZIPFatSystem = (gfsFat, gfsAmiga, gfsVMS, gfsUnix, gfsVM, gfsAtari, gfsHPFS,
    gfsMac, gfsZ, gfsCPM, gfsTOPS, gfsNTFS, gfsQDOS, gfsAcorn, gfsOther, gfsUnknown);

  // Format is described in RFC 1952, http://www.faqs.org/rfcs/rfc1952.html
  TJclGZIPCompressionStream = class(TJclCompressStream)
  private
    FFlags: TJclGZIPFlags;
    FUnixTime: Cardinal;
    FAutoSetTime: Boolean;
    FCompressionLevel: TJclCompressionLevel;
    FFatSystem: TJclGZIPFatSystem;
    FExtraField: string;
    FOriginalFileName: string;
    FComment: string;
    FZLibStream: TJclZlibCompressStream;
    FOriginalSize: Cardinal;
    FDataCRC32: Cardinal;
    FHeaderWritten: Boolean;
    FFooterWritten: Boolean; // flag so we only write the footer once! (NEW 2007)

    procedure WriteHeader;
    function GetDosTime: TDateTime;
    function GetUnixTime: Cardinal;
    procedure SetDosTime(const Value: TDateTime);
    procedure SetUnixTime(Value: Cardinal);
    procedure ZLibStreamProgress(Sender: TObject);
  public
    constructor Create(Destination: TStream; CompressionLevel: TJclCompressionLevel = -1);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure Reset; override;

    // IMPORTANT: In order to get a valid GZip file, Flush MUST be called after
    // the last call to Write.
    function Flush: Integer; override;

    property Flags: TJclGZIPFlags read FFlags write FFlags;
    property DosTime: TDateTime read GetDosTime write SetDosTime;
    property UnixTime: Cardinal read GetUnixTime write SetUnixTime;
    property AutoSetTime: Boolean read FAutoSetTime write FAutoSetTime;
    property FatSystem: TJclGZIPFatSystem read FFatSystem write FFatSystem;
    property ExtraField: string read FExtraField write FExtraField;
    // Note: In order for most decompressors to work, the original file name
    // must be given or they would display an empty file name in their list.
    // This does not affect the decompression stream below as it simply reads
    // the value and does not work with it
    property OriginalFileName: string read FOriginalFileName write FOriginalFileName;
    property Comment: string read FComment write FComment;
  end;

  TJclGZIPDecompressionStream = class(TJclDecompressStream)
  private
    FHeader: TJclGZIPHeader;
    FFooter: TJclGZIPFooter;
    FCompressedDataStream: TJclDelegatedStream;
    FZLibStream: TJclZLibDecompressStream;
    FOriginalFileName: string;
    FComment: string;
    FExtraField: string;
    FComputedHeaderCRC16: Word;
    FStoredHeaderCRC16: Word;
    FComputedDataCRC32: Cardinal;
    FCompressedDataSize: Int64;
    FDataSize: Int64;
    FDataStarted: Boolean;
    FDataEnded: Boolean;
    FAutoCheckDataCRC32: Boolean;
    function GetCompressedDataSize: Int64;
    function GetComputedDataCRC32: Cardinal;
    function GetDosTime: TDateTime;
    function GetFatSystem: TJclGZIPFatSystem;
    function GetFlags: TJclGZIPFlags;
    function GetOriginalDataSize: Cardinal;
    function GetStoredDataCRC32: Cardinal;
    function ReadCompressedData(Sender: TObject; var Buffer; Count: Longint): Longint;
    procedure ZLibStreamProgress(Sender: TObject);
  public
    constructor Create(Source: TStream; CheckHeaderCRC: Boolean = True; AOwnsStream: Boolean = False);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;

    property ComputedHeaderCRC16: Word read FComputedHeaderCRC16;
    property StoredHeaderCRC16: Word read FStoredHeaderCRC16;
    property ExtraField: string read FExtraField;
    property OriginalFileName: string read FOriginalFileName;
    property Comment: string read FComment;
    property Flags: TJclGZIPFlags read GetFlags;
    property CompressionLevel: Byte read FHeader.ExtraFlags;
    property FatSystem: TJclGZIPFatSystem read GetFatSystem;
    property UnixTime: Cardinal read FHeader.ModifiedTime;
    property DosTime: TDateTime read GetDosTime;
    property ComputedDataCRC32: Cardinal read GetComputedDataCRC32;
    property StoredDataCRC32: Cardinal read GetStoredDataCRC32;
    property AutoCheckDataCRC32: Boolean read FAutoCheckDataCRC32 write FAutoCheckDataCRC32;
    property CompressedDataSize: Int64 read GetCompressedDataSize;
    property OriginalDataSize: Cardinal read GetOriginalDataSize;
  end;

  // RAR Support
  TJclRARCompressionStream = class(TJclCompressionStream)
  end;

  TJclRARDecompressionStream = class(TJclDecompressStream)
  end;

  // TAR Support
  TJclTARCompressionStream = class(TJclCompressionStream)
  end;

  TJclTARDecompressionStream = class(TJclDecompressStream)
  end;

  // BZIP2 Support
  TJclBZIP2CompressionStream = class(TJclCompressStream)
  private
    FDeflateInitialized: Boolean;
    FCompressionLevel: Integer;
  protected
    BZLibRecord: bz_stream;
    procedure SetCompressionLevel(const Value: Integer);
  public
    function Flush: Integer; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Write(const Buffer; Count: Longint): Longint; override;

    constructor Create(Destination: TStream; CompressionLevel: TJclCompressionLevel = -1);
    destructor Destroy; override;

    property CompressionLevel: Integer read FCompressionLevel write SetCompressionLevel;
  end;

  TJclBZIP2DecompressionStream = class(TJclDecompressStream)
  private
    FInflateInitialized: Boolean;
  protected
    BZLibRecord: bz_stream;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    constructor Create(Source: TStream; AOwnsStream: Boolean = False); overload;
    destructor Destroy; override;
  end;

  EJclCompressionError = class(EJclError);

  // callback type used in helper functions below:
  TJclCompressStreamProgressCallback = procedure(FileSize, Position: Int64; UserData: Pointer) of object;

{helper functions - one liners by wpostma}
function GZipFile(SourceFile, DestinationFile: string; CompressionLevel: Integer = Z_DEFAULT_COMPRESSION;
  ProgressCallback: TJclCompressStreamProgressCallback = nil; UserData: Pointer = nil): Boolean;
function UnGZipFile(SourceFile, DestinationFile: string;
  ProgressCallback: TJclCompressStreamProgressCallback = nil; UserData: Pointer = nil): Boolean;

function BZip2File(SourceFile, DestinationFile: string; CompressionLevel: Integer = 5;
  ProgressCallback: TJclCompressStreamProgressCallback = nil; UserData: Pointer = nil): Boolean;
function UnBZip2File(SourceFile, DestinationFile: string;
  ProgressCallback: TJclCompressStreamProgressCallback = nil; UserData: Pointer = nil): Boolean;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common'
    );
  {$ENDIF UNITVERSIONING}

implementation

uses
  JclDateTime, JclFileUtils, JclResources;

const
  JclDefaultBufferSize = 131072; // 128k

//=== { TJclCompressionStream } ==============================================

constructor TJclCompressionStream.Create(Stream: TStream);
begin
  inherited Create;
  FBuffer := nil;
  SetBufferSize(JclDefaultBufferSize);
end;

destructor TJclCompressionStream.Destroy;
begin
  SetBufferSize(0);
  inherited Destroy;
end;

function TJclCompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise EJclCompressionError.CreateRes(@RsCompressionReadNotSupported);
end;

function TJclCompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EJclCompressionError.CreateRes(@RsCompressionWriteNotSupported);
end;

function TJclCompressionStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  raise EJclCompressionError.CreateRes(@RsCompressionSeekNotSupported);
end;

procedure TJclCompressionStream.Reset;
begin
  raise EJclCompressionError.CreateRes(@RsCompressionResetNotSupported);
end;

function TJclCompressionStream.SetBufferSize(Size: Cardinal): Cardinal;
begin
  if FBuffer <> nil then
    FreeMem(FBuffer, FBufferSize);

  FBufferSize := Size;

  if FBufferSize > 0 then
    GetMem(FBuffer, FBufferSize)
  else
    FBuffer := nil;

  Result := FBufferSize;
end;

procedure TJclCompressionStream.Progress(Sender: TObject);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender);
end;

//=== { TJclCompressStream } =================================================

constructor TJclCompressStream.Create(Destination: TStream);
begin
  inherited Create(Destination);
  FStream := Destination;
end;

//=== { TJclDecompressStream } ===============================================

constructor TJclDecompressStream.Create(Source: TStream; AOwnsStream: Boolean);
begin
  inherited Create(Source);
  FStream := Source;
  FOwnsStream := AOwnsStream;
end;

destructor TJclDecompressStream.Destroy;
begin
  if FOwnsStream then
    FStream.Free;
  inherited Destroy;
end;

//=== { TJclZLibCompressionStream } ==========================================

{ Error checking helper }

function ZLibCheck(const ErrCode: Integer): Integer;
begin
  case ErrCode of
    0..High(ErrCode):
      Result := ErrCode; // no error
    Z_ERRNO:
      raise EJclCompressionError.CreateRes(@RsCompressionZLibZErrNo);
    Z_STREAM_ERROR:
      raise EJclCompressionError.CreateRes(@RsCompressionZLibZStreamError);
    Z_DATA_ERROR:
      raise EJclCompressionError.CreateRes(@RsCompressionZLibZDataError);
    Z_MEM_ERROR:
      raise EJclCompressionError.CreateRes(@RsCompressionZLibZMemError);
    Z_BUF_ERROR:
      raise EJclCompressionError.CreateRes(@RsCompressionZLibZBufError);
    Z_VERSION_ERROR:
      raise EJclCompressionError.CreateRes(@RsCompressionZLibZVersionError);
  else
    raise EJclCompressionError.CreateResFmt(@RsCompressionZLibError, [ErrCode]);
  end;
end;

constructor TJclZLibCompressStream.Create(Destination: TStream; CompressionLevel: TJclCompressionLevel);
begin
  inherited Create(Destination);

  Assert(FBuffer <> nil);
  Assert(FBufferSize > 0);

  // Initialize ZLib StreamRecord
  with ZLibRecord do
  begin
    zalloc := nil; // Use build-in memory allocation functionality
    zfree := nil;
    next_in := nil;
    avail_in := 0;
    next_out := FBuffer;
    avail_out := FBufferSize;
  end;

  FWindowBits := DEF_WBITS;
  FMemLevel := DEF_MEM_LEVEL;
  FMethod := Z_DEFLATED;
  FStrategy := Z_DEFAULT_STRATEGY;
  FCompressionLevel := CompressionLevel;
  FDeflateInitialized := False;
end;

destructor TJclZLibCompressStream.Destroy;
begin
  Flush;
  if FDeflateInitialized then
  begin
    ZLibRecord.next_in := nil;
    ZLibRecord.avail_in := 0;
    ZLibRecord.avail_out := 0;
    ZLibRecord.next_out := nil;

    ZLibCheck(deflateEnd(ZLibRecord));
  end;

  inherited Destroy;
end;

function TJclZLibCompressStream.Write(const Buffer; Count: Longint): Longint;
begin
  if not FDeflateInitialized then
  begin
    ZLibCheck(deflateInit2(ZLibRecord, FCompressionLevel, FMethod, FWindowBits, FMemLevel, FStrategy));
    FDeflateInitialized := True;
  end;

  ZLibRecord.next_in := @Buffer;
  ZLibRecord.avail_in := Count;

  while ZLibRecord.avail_in > 0 do
  begin
    ZLibCheck(deflate(ZLibRecord, Z_NO_FLUSH));

    if ZLibRecord.avail_out = 0 then // Output buffer empty. Write to stream and go on...
    begin
      FStream.WriteBuffer(FBuffer^, FBufferSize);
      Progress(Self);
      ZLibRecord.next_out := FBuffer;
      ZLibRecord.avail_out := FBufferSize;
    end;
  end;

  Result := Count;
end;

function TJclZLibCompressStream.Flush: Integer;
begin
  Result := 0;

  if FDeflateInitialized then
  begin
    ZLibRecord.next_in := nil;
    ZLibRecord.avail_in := 0;

    while (ZLibCheck(deflate(ZLibRecord, Z_FINISH)) <> Z_STREAM_END) and
      (ZLibRecord.avail_out = 0) do
    begin
      FStream.WriteBuffer(FBuffer^, FBufferSize);
      Progress(Self);

      ZLibRecord.next_out := FBuffer;
      ZLibRecord.avail_out := FBufferSize;
      Inc(Result, FBufferSize);
    end;

    if ZLibRecord.avail_out < FBufferSize then
    begin
      FStream.WriteBuffer(FBuffer^, FBufferSize - ZLibRecord.avail_out);
      Progress(Self);
      Inc(Result, FBufferSize - ZLibRecord.avail_out);
      ZLibRecord.next_out := FBuffer;
      ZLibRecord.avail_out := FBufferSize;
    end;
  end;
end;

function TJclZLibCompressStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (Offset = 0) and (Origin = soCurrent) then
    Result := ZLibRecord.total_in
  else
  if (Offset = 0) and (Origin = soBeginning) and (ZLibRecord.total_in = 0) then
    Result := 0
  else
    Result := inherited Seek(Offset, Origin);
end;

procedure TJclZLibCompressStream.SetWindowBits(Value: Integer);
begin
  FWindowBits := Value;
end;

procedure TJclZLibCompressStream.SetMethod(Value: Integer);
begin
  FMethod := Value;
end;

procedure TJclZLibCompressStream.SetStrategy(Value: Integer);
begin
  FStrategy := Value;
  if FDeflateInitialized then
    ZLibCheck(deflateParams(ZLibRecord, FCompressionLevel, FStrategy));
end;

procedure TJclZLibCompressStream.SetMemLevel(Value: Integer);
begin
  FMemLevel := Value;
end;

procedure TJclZLibCompressStream.SetCompressionLevel(Value: Integer);
begin
  FCompressionLevel := Value;
  if FDeflateInitialized then
    ZLibCheck(deflateParams(ZLibRecord, FCompressionLevel, FStrategy));
end;

procedure TJclZLibCompressStream.Reset;
begin
  if FDeflateInitialized then
  begin
    Flush;
    ZLibCheck(deflateReset(ZLibRecord));
  end;
end;

//=== {  TJclZLibDecompressionStream } =======================================

constructor TJclZLibDecompressStream.Create(Source: TStream; WindowBits: Integer; AOwnsStream: Boolean);
begin
  inherited Create(Source, AOwnsStream);

  // Initialize ZLib StreamRecord
  with ZLibRecord do
  begin
    zalloc := nil; // Use build-in memory allocation functionality
    zfree := nil;
    next_in := nil;
    avail_in := 0;
    next_out := FBuffer;
    avail_out := FBufferSize;
  end;

  FInflateInitialized := False;
  FWindowBits := WindowBits;
end;

destructor TJclZLibDecompressStream.Destroy;
begin
  if FInflateInitialized then
  begin
    FStream.Seek(-ZLibRecord.avail_in, soFromCurrent);
    ZLibCheck(inflateEnd(ZLibRecord));
  end;

  inherited Destroy;
end;

function TJclZLibDecompressStream.Read(var Buffer; Count: Longint): Longint;
var
  Res: Integer;
begin
  if not FInflateInitialized then
  begin
    ZLibCheck(InflateInit2(ZLibRecord, FWindowBits));
    FInflateInitialized := True;
  end;

  ZLibRecord.next_out := @Buffer;
  ZLibRecord.avail_out := Count;

  while ZLibRecord.avail_out > 0 do // as long as we have data
  begin
    if ZLibRecord.avail_in = 0 then
    begin
      ZLibRecord.avail_in := FStream.Read(FBuffer^, FBufferSize);

      if ZLibRecord.avail_in = 0 then
      begin
        Result := Count - Longint(ZLibRecord.avail_out);
        Exit;
      end;

      ZLibRecord.next_in := FBuffer;
    end;

    if ZLibRecord.avail_in > 0 then
    begin
      Res := inflate(ZLibRecord, Z_NO_FLUSH);
      ZLibCheck(Res);
      Progress(Self);
    end;
  end;

  Result := Count;
end;

function TJclZLibDecompressStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (Offset = 0) and (Origin = soCurrent) then
    Result := ZLibRecord.total_out
  else
    Result := inherited Seek(Offset, Origin);
end;

procedure TJclZLibDecompressStream.SetWindowBits(Value: Integer);
begin
  FWindowBits := Value;
end;

//=== { TJclGZIPCompressionStream } ==========================================

constructor TJclGZIPCompressionStream.Create(Destination: TStream; CompressionLevel: TJclCompressionLevel);
begin
  inherited Create(Destination);

  FFlags := [gfHeaderCRC16, gfExtraField, gfOriginalFileName, gfComment];
  FAutoSetTime := True;
  FFatSystem := gfsUnknown;
  FCompressionLevel := CompressionLevel;
  FDataCRC32 := crc32(0, nil, 0);
end;

destructor TJclGZIPCompressionStream.Destroy;
begin
  // BUGFIX: CRC32 and Uncompressed Size missing from GZIP output
  // unless you called Flush manually. This is not correct Stream behaviour.
  // Flush should be optional!
  Flush;
  FZLibStream.Free;
  inherited Destroy;
end;

function TJclGZIPCompressionStream.Flush: Integer;
var
  AFooter: TJclGZIPFooter;
begin
  if Assigned(FZLibStream) then
    Result := FZLibStream.Flush
  else
    Result := 0;

  if FFooterWritten then
    Exit;
  FFooterWritten := True;

  // Write footer, CRC32 followed by ISIZE
  AFooter.DataCRC32 := FDataCRC32;
  AFooter.DataSize := FOriginalSize;

  Inc(Result, FStream.Write(AFooter, SizeOf(AFooter)));
end;

function TJclGZIPCompressionStream.GetDosTime: TDateTime;
begin
  if AutoSetTime then
    Result := Now
  else
    Result := UnixTimeToDateTime(FUnixTime);
end;

function TJclGZIPCompressionStream.GetUnixTime: Cardinal;
begin
  if AutoSetTime then
    Result := DateTimeToUnixTime(Now)
  else
    Result := FUnixTime;
end;

procedure TJclGZIPCompressionStream.Reset;
begin
  if Assigned(FZLibStream) then
    FZLibStream.Reset;

  FDataCRC32 := crc32(0, nil, 0);
  FOriginalSize := 0;
end;

procedure TJclGZIPCompressionStream.SetDosTime(const Value: TDateTime);
begin
  AutoSetTime := False;
  FUnixTime := DateTimeToUnixTime(Value);
end;

procedure TJclGZIPCompressionStream.SetUnixTime(Value: Cardinal);
begin
  AutoSetTime := False;
  FUnixTime := Value;
end;

function TJclGZIPCompressionStream.Write(const Buffer; Count: Integer): Longint;
begin
  if not FHeaderWritten then
  begin
    WriteHeader;
    FHeaderWritten := True;
  end;

  if not Assigned(FZLibStream) then
  begin
    FZLibStream := TJclZlibCompressStream.Create(FStream, FCompressionLevel);
    FZLibStream.WindowBits := -DEF_WBITS; // negative value for raw mode
    FZLibStream.OnProgress := ZLibStreamProgress;
  end;

  Result := FZLibStream.Write(Buffer, Count);
  FDataCRC32 := crc32(FDataCRC32, PBytef(@Buffer), Result);
  Inc(FOriginalSize, Result);
end;

procedure TJclGZIPCompressionStream.WriteHeader;
const
  FatSystemToByte: array[TJclGZIPFatSystem] of Byte =
    (JCL_GZIP_OS_FAT, JCL_GZIP_OS_AMIGA, JCL_GZIP_OS_VMS, JCL_GZIP_OS_UNIX,
    JCL_GZIP_OS_VM, JCL_GZIP_OS_ATARI, JCL_GZIP_OS_HPFS, JCL_GZIP_OS_MAC,
    JCL_GZIP_OS_Z, JCL_GZIP_OS_CPM, JCL_GZIP_OS_TOPS, JCL_GZIP_OS_NTFS,
    JCL_GZIP_OS_QDOS, JCL_GZIP_OS_ACORN, JCL_GZIP_OS_UNKNOWN, JCL_GZIP_OS_UNKNOWN);
var
  AHeader: TJclGZIPHeader;
  ExtraFieldLength, HeaderCRC16: Word;
  HeaderCRC: Cardinal;

  procedure StreamWriteBuffer(const Buffer; Count: Longint);
  begin
    FStream.WriteBuffer(Buffer, Count);
    if gfHeaderCRC16 in Flags then
      HeaderCRC := crc32(HeaderCRC, @Byte(Buffer), Count);
  end;

  function CheckCString(const Buffer: string): Boolean;
  var
    Index: Integer;
  begin
    Result := False;
    for Index := 1 to Length(Buffer) do
      if Buffer[Index] = #0 then
        Exit;
    Result := True;
  end;

begin
  if gfHeaderCRC16 in Flags then
    HeaderCRC := crc32(0, nil, 0);

  AHeader.ID1 := JCL_GZIP_ID1;
  AHeader.ID2 := JCL_GZIP_ID2;
  AHeader.CompressionMethod := JCL_GZIP_CM_DEFLATE;
  AHeader.Flags := 0;
  if gfDataIsText in Flags then
    AHeader.Flags := AHeader.Flags or JCL_GZIP_FLAG_TEXT;
  if gfHeaderCRC16 in Flags then
    AHeader.Flags := AHeader.Flags or JCL_GZIP_FLAG_CRC;
  if (gfExtraField in Flags) and (ExtraField <> '') then
    AHeader.Flags := AHeader.Flags or JCL_GZIP_FLAG_EXTRA;
  if (gfOriginalFileName in Flags) and (OriginalFileName <> '') then
    AHeader.Flags := AHeader.Flags or JCL_GZIP_FLAG_NAME;
  if (gfComment in Flags) and (Comment <> '') then
    AHeader.Flags := AHeader.Flags or JCL_GZIP_FLAG_COMMENT;

  if AutoSetTime then
    AHeader.ModifiedTime := DateTimeToUnixTime(Now)
  else
    AHeader.ModifiedTime := FUnixTime;

  case FCompressionLevel of
    Z_BEST_COMPRESSION:
      AHeader.ExtraFlags := JCL_GZIP_EFLAG_MAX;
    Z_BEST_SPEED:
      AHeader.ExtraFlags := JCL_GZIP_EFLAG_FAST;
  else
    AHeader.ExtraFlags := 0;
  end;

  AHeader.OS := FatSystemToByte[FatSystem];

  StreamWriteBuffer(AHeader, SizeOf(AHeader));

  if (gfExtraField in Flags) and (ExtraField <> '') then
  begin
    if Length(ExtraField) > High(Word) then
      raise EJclCompressionError.CreateRes(@RsCompressionGZIPExtraFieldTooLong);
    ExtraFieldLength := Length(ExtraField);
    StreamWriteBuffer(ExtraFieldLength, SizeOf(ExtraFieldLength));
    StreamWriteBuffer(ExtraField[1], Length(ExtraField));
  end;

  if (gfOriginalFileName in Flags) and (OriginalFileName <> '') then
  begin
    if not CheckCString(OriginalFileName) then
      raise EJclCompressionError.CreateRes(@RsCompressionGZIPBadString);
    StreamWriteBuffer(OriginalFileName[1], Length(OriginalFileName) + 1);
  end;

  if (gfComment in Flags) and (Comment <> '') then
  begin
    if not CheckCString(Comment) then
      raise EJclCompressionError.CreateRes(@RsCompressionGZIPBadString);
    StreamWriteBuffer(Comment[1], Length(Comment) + 1);
  end;

  if (gfHeaderCRC16 in Flags) then
  begin
    HeaderCRC16 := HeaderCRC and $FFFF;
    FStream.WriteBuffer(HeaderCRC16, SizeOf(HeaderCRC16));
  end;
end;

procedure TJclGZIPCompressionStream.ZLibStreamProgress(Sender: TObject);
begin
  Progress(Self);
end;

//=== { TJclGZIPDecompressionStream } ========================================

constructor TJclGZIPDecompressionStream.Create(Source: TStream; CheckHeaderCRC: Boolean; AOwnsStream: Boolean);
var
  HeaderCRC: Cardinal;
  ComputeHeaderCRC: Boolean;
  ExtraFieldLength: Word;

  procedure ReadBuffer(var Buffer; SizeOfBuffer: Longint);
  begin
    Source.ReadBuffer(Buffer, SizeOfBuffer);
    if ComputeHeaderCRC then
      HeaderCRC := crc32(HeaderCRC, @Byte(Buffer), SizeOfBuffer);
  end;

  function ReadCString: string;
  var
    Dummy: Char;
  begin
    repeat
      Source.ReadBuffer(Dummy, SizeOf(Dummy));
      FOriginalFileName := FOriginalFileName + Dummy;
    until Dummy = #0;
    SetLength(FOriginalFileName, Length(FOriginalFileName) - 1);
  end;

begin
  inherited Create(Source, AOwnsStream);

  FAutoCheckDataCRC32 := True;
  FComputedDataCRC32 := crc32(0, nil, 0);
  HeaderCRC := crc32(0, nil, 0);

  ComputeHeaderCRC := CheckHeaderCRC;
  ReadBuffer(FHeader, SizeOf(FHeader));
  if (FHeader.ID1 <> JCL_GZIP_ID1) or (FHeader.ID2 <> JCL_GZIP_ID2) then
    raise EJclCompressionError.CreateResFmt(@RsCompressionGZipInvalidID, [FHeader.ID1, FHeader.ID2]);
  if (FHeader.CompressionMethod <> JCL_GZIP_CM_DEFLATE) then
    raise EJclCompressionError.CreateResFmt(@RsCompressionGZipUnsupportedCM, [FHeader.CompressionMethod]);

  if (FHeader.Flags and JCL_GZIP_FLAG_EXTRA) <> 0 then
  begin
    ReadBuffer(ExtraFieldLength, SizeOf(ExtraFieldLength));
    SetLength(FExtraField, ExtraFieldLength);
    ReadBuffer(FExtraField[1], ExtraFieldLength);
  end;

  if (FHeader.Flags and JCL_GZIP_FLAG_NAME) <> 0 then
    FOriginalFileName := ReadCString;
  if (FHeader.Flags and JCL_GZIP_FLAG_COMMENT) <> 0 then
    FComment := ReadCString;

  if CheckHeaderCRC then
  begin
    ComputeHeaderCRC := False;
    FComputedHeaderCRC16 := HeaderCRC and $FFFF;
  end;

  if (FHeader.Flags and JCL_GZIP_FLAG_CRC) <> 0 then
  begin
    Source.ReadBuffer(FStoredHeaderCRC16, SizeOf(FStoredHeaderCRC16));
    if CheckHeaderCRC and (FComputedHeaderCRC16 <> FStoredHeaderCRC16) then
      raise EJclCompressionError.CreateRes(@RsCompressionGZipHeaderCRC);
  end;
end;

destructor TJclGZIPDecompressionStream.Destroy;
begin
  FZLibStream.Free;
  FCompressedDataStream.Free;
  inherited Destroy;
end;

function TJclGZIPDecompressionStream.GetCompressedDataSize: Int64;
begin
  if not FDataStarted then
    Result := FStream.Size - FStream.Position - SizeOf(FFooter)
  else
  if FDataEnded then
    Result := FCompressedDataSize
  else
    raise EJclCompressionError.CreateRes(@RsCompressionGZipDecompressing);
end;

function TJclGZIPDecompressionStream.GetComputedDataCRC32: Cardinal;
begin
  if FDataEnded then
    Result := FComputedDataCRC32
  else
    raise EJclCompressionError.CreateRes(@RsCompressionGZipNotDecompressed);
end;

function TJclGZIPDecompressionStream.GetDosTime: TDateTime;
begin
  Result := UnixTimeToDateTime(FHeader.ModifiedTime);
end;

function TJclGZIPDecompressionStream.GetFatSystem: TJclGZIPFatSystem;
const
  ByteToFatSystem: array[JCL_GZIP_OS_FAT..JCL_GZIP_OS_ACORN] of TJclGZIPFatSystem =
    (gfsFat, gfsAmiga, gfsVMS, gfsUnix, gfsVM, gfsAtari, gfsHPFS, gfsMac, gfsZ,
    gfsCPM, gfsTOPS, gfsNTFS, gfsQDOS, gfsAcorn);
begin
  case FHeader.OS of
    JCL_GZIP_OS_FAT..JCL_GZIP_OS_ACORN:
      Result := ByteToFatSystem[FHeader.OS];
    JCL_GZIP_OS_UNKNOWN:
      Result := gfsUnknown;
  else
    Result := gfsOther;
  end;
end;

function TJclGZIPDecompressionStream.GetFlags: TJclGZIPFlags;
begin
  Result := [];
  if (FHeader.Flags and JCL_GZIP_FLAG_TEXT) <> 0 then
    Result := Result + [gfDataIsText];
  if (FHeader.Flags and JCL_GZIP_FLAG_CRC) <> 0 then
    Result := Result + [gfHeaderCRC16];
  if (FHeader.Flags and JCL_GZIP_FLAG_EXTRA) <> 0 then
    Result := Result + [gfExtraField];
  if (FHeader.Flags and JCL_GZIP_FLAG_NAME) <> 0 then
    Result := Result + [gfOriginalFileName];
  if (FHeader.Flags and JCL_GZIP_FLAG_COMMENT) <> 0 then
    Result := Result + [gfComment];
end;

function TJclGZIPDecompressionStream.GetOriginalDataSize: Cardinal;
var
  StartPos: {$IFDEF COMPILER5} Longint; {$ELSE} Int64; {$ENDIF}
  AFooter: TJclGZIPFooter;
begin
  if not FDataStarted then
  begin
    StartPos := FStream.Position;
    try
      FStream.Seek(-SizeOf(AFooter), soFromEnd);
      FStream.ReadBuffer(AFooter, SizeOf(AFooter));
      Result := AFooter.DataSize;
    finally
      FStream.Seek(StartPos, {$IFDEF COMPILER5} soFromBeginning {$ELSE} soBeginning {$ENDIF});
    end;
  end
  else
  if FDataEnded then
    Result := FFooter.DataSize
  else
    raise EJclCompressionError.CreateRes(@RsCompressionGZipDecompressing);
end;

function TJclGZIPDecompressionStream.GetStoredDataCRC32: Cardinal;
var
  StartPos: {$IFDEF COMPILER5} Longint; {$ELSE} Int64; {$ENDIF}
  AFooter: TJclGZIPFooter;
begin
  if not FDataStarted then
  begin
    StartPos := FStream.Position;
    try
      FStream.Seek(-SizeOf(AFooter), soFromEnd);
      FStream.ReadBuffer(AFooter, SizeOf(AFooter));
      Result := AFooter.DataCRC32;
    finally
      FStream.Seek(StartPos, {$IFDEF COMPILER5} soFromBeginning {$ELSE} soBeginning {$ENDIF});
    end;
  end
  else
  if FDataEnded then
    Result := FFooter.DataCRC32
  else
    raise EJclCompressionError.CreateRes(@RsCompressionGZipDecompressing);
end;

function TJclGZIPDecompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  if not Assigned(FZLibStream) then
  begin
    FCompressedDataStream := TJclDelegatedStream.Create;
    FCompressedDataStream.OnRead := ReadCompressedData;
    FZLibStream := TJclZLibDecompressStream.Create(FCompressedDataStream, -DEF_WBITS);
    FZLibStream.OnProgress := ZLibStreamProgress;
  end;
  Result := FZLibStream.Read(Buffer, Count);
  Inc(FDataSize, Result);
  FComputedDataCRC32 := crc32(FComputedDataCRC32, @Byte(Buffer), Result);
  if Result < Count then
  begin
    if not FDataEnded then
      // the decompressed stream is stopping before the compressed stream
      raise EJclCompressionError(RsCompressionGZipInternalError);
    if AutoCheckDataCRC32 and (FComputedDataCRC32 <> FFooter.DataCRC32) then
      raise EJclCompressionError(RsCompressionGZipDataCRCFailed);
  end;
end;

function TJclGZIPDecompressionStream.ReadCompressedData(Sender: TObject; var Buffer;
  Count: Longint): Longint;
var
  BufferAddr: PChar;
  FooterAddr: PChar;
begin
  if (Count = 0) or FDataEnded then
  begin
    Result := 0;
    Exit;
  end
  else
  if not FDataStarted then
  begin
    FDataStarted := True;
    // prolog
    if FStream.Read(FFooter, SizeOf(FFooter)) < SizeOf(FFooter) then
      raise EJclCompressionError.CreateRes(@RsCompressionGZipDataTruncated);
  end;

  BufferAddr := @Char(Buffer);
  Move(FFooter, Buffer, SizeOf(FFooter));
  Result := FStream.Read(BufferAddr[SizeOf(FFooter)], Count - SizeOf(FFooter))
    + FStream.Read(FFooter, SizeOf(FFooter));

  if Result < Count then
  begin
    FDataEnded := True;
    // epilog
    FooterAddr := @FFooter;
    if (Count - Result) < SizeOf(FFooter) then
    begin
      // the "real" footer is splitted in the data and the footer
      // shift the valid bytes of the footer to their place
      Move(FFooter, FooterAddr[Count - Result], SizeOf(FFooter) - Count + Result);
      // the missing bytes of the footer are located after the data
      Move(BufferAddr[Result], FFooter, Count - Result);
    end
    else
      // the "real" footer is located in the data
      Move(BufferAddr[Result], FFooter, SizeOf(FFooter));
  end;
  Inc(FCompressedDataSize, Result);
end;

procedure TJclGZIPDecompressionStream.ZLibStreamProgress(Sender: TObject);
begin
  Progress(Self);
end;

//=== { TJclBZLibCompressionStream } =========================================

{ Error checking helper }

function BZIP2LibCheck(const ErrCode: Integer): Integer;
begin
  case ErrCode of
    0..High(ErrCode):
      Result := ErrCode; // no error
    BZ_SEQUENCE_ERROR:
      raise EJclCompressionError.CreateRes(@RsCompressionBZIP2SequenceError);
    BZ_PARAM_ERROR:
      raise EJclCompressionError.CreateRes(@RsCompressionBZIP2ParameterError);
    BZ_MEM_ERROR:
      raise EJclCompressionError.CreateRes(@RsCompressionBZIP2MemoryError);
    BZ_DATA_ERROR:
      raise EJclCompressionError.CreateRes(@RsCompressionBZIP2DataError);
    BZ_DATA_ERROR_MAGIC:
      raise EJclCompressionError.CreateRes(@RsCompressionBZIP2HeaderError);
    BZ_IO_ERROR:
      raise EJclCompressionError.CreateRes(@RsCompressionBZIP2IOError);
    BZ_UNEXPECTED_EOF:
      raise EJclCompressionError.CreateRes(@RsCompressionBZIP2EOFError);
    BZ_OUTBUFF_FULL:
      raise EJclCompressionError.CreateRes(@RsCompressionBZIP2OutBuffError);
    BZ_CONFIG_ERROR:
      raise EJclCompressionError.CreateRes(@RsCompressionBZIP2ConfigError);
  else
    raise EJclCompressionError.CreateResFmt(@RsCompressionBZIP2Error, [ErrCode]);
  end;
end;

constructor TJclBZIP2CompressionStream.Create(Destination: TStream; CompressionLevel: TJclCompressionLevel);
begin
  inherited Create(Destination);

  LoadBZip2;

  Assert(FBuffer <> nil);
  Assert(FBufferSize > 0);

  // Initialize ZLib StreamRecord
  BZLibRecord.bzalloc := nil; // Use build-in memory allocation functionality
  BZLibRecord.bzfree := nil;
  BZLibRecord.next_in := nil;
  BZLibRecord.avail_in := 0;
  BZLibRecord.next_out := FBuffer;
  BZLibRecord.avail_out := FBufferSize;

  FDeflateInitialized := False;

  FCompressionLevel := 9;
end;

destructor TJclBZIP2CompressionStream.Destroy;
begin
  Flush;
  if FDeflateInitialized then
    BZIP2LibCheck(BZ2_bzCompressEnd(BZLibRecord));

  inherited Destroy;
end;

function TJclBZIP2CompressionStream.Flush: Integer;
begin
  Result := 0;

  if FDeflateInitialized then
  begin
    BZLibRecord.next_in := nil;
    BZLibRecord.avail_in := 0;

    while (BZIP2LibCheck(BZ2_bzCompress(BZLibRecord, BZ_FINISH)) <> BZ_STREAM_END) and (BZLibRecord.avail_out = 0) do
    begin
      FStream.WriteBuffer(FBuffer^, FBufferSize);
      Progress(Self);

      BZLibRecord.next_out := FBuffer;
      BZLibRecord.avail_out := FBufferSize;
      Inc(Result, FBufferSize);
    end;

    if BZLibRecord.avail_out < FBufferSize then
    begin
      FStream.WriteBuffer(FBuffer^, FBufferSize - BZLibRecord.avail_out);
      Progress(Self);
      Inc(Result, FBufferSize - BZLibRecord.avail_out);
      BZLibRecord.next_out := FBuffer;
      BZLibRecord.avail_out := FBufferSize;
    end;
  end;
end;

function TJclBZIP2CompressionStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (Offset = 0) and (Origin = soCurrent) then
    Result := (BZLibRecord.total_in_hi32 shl 32) or BZLibRecord.total_in_lo32
  else
  if (Offset = 0) and (Origin = soBeginning) and (BZLibRecord.total_in_lo32 = 0) then
    Result := 0
  else
    Result := inherited Seek(Offset, Origin);
end;

procedure TJclBZIP2CompressionStream.SetCompressionLevel(const Value: Integer);
begin
  if not FDeflateInitialized then
    FCompressionLevel := Value
  else
    raise EJclCompressionError.CreateRes(@RsCompressionBZIP2SequenceError);
end;

function TJclBZIP2CompressionStream.Write(const Buffer; Count: Longint): Longint;
begin
  if not FDeflateInitialized then
  begin
    BZIP2LibCheck(BZ2_bzCompressInit(BZLibRecord, FCompressionLevel, 0, 0));
    FDeflateInitialized := True;
  end;

  BZLibRecord.next_in := @Buffer;
  BZLibRecord.avail_in := Count;

  while BZLibRecord.avail_in > 0 do
  begin
    BZIP2LibCheck(BZ2_bzCompress(BZLibRecord, BZ_RUN));

    if BZLibRecord.avail_out = 0 then   // Output buffer empty. Write to stream and go on...
    begin
      FStream.WriteBuffer(FBuffer^, FBufferSize);
      Progress(Self);
      BZLibRecord.next_out := FBuffer;
      BZLibRecord.avail_out := FBufferSize;
    end;
  end;

  Result := Count;
end;

//=== { TJclZLibDecompressionStream } ========================================

constructor TJclBZIP2DecompressionStream.Create(Source: TStream; AOwnsStream: Boolean);
begin
  inherited Create(Source, AOwnsStream);

  LoadBZip2;

  // Initialize ZLib StreamRecord
  BZLibRecord.bzalloc := nil; // Use build-in memory allocation functionality
  BZLibRecord.bzfree := nil;
  BZLibRecord.opaque := nil;
  BZLibRecord.next_in := nil;
  BZLibRecord.state := nil;
  BZLibRecord.avail_in := 0;
  BZLibRecord.next_out := FBuffer;
  BZLibRecord.avail_out := FBufferSize;

  FInflateInitialized := False;
end;

destructor TJclBZIP2DecompressionStream.Destroy;
begin
  if FInflateInitialized then
  begin
    FStream.Seek(-BZLibRecord.avail_in, soFromCurrent);
    BZIP2LibCheck(BZ2_bzDecompressEnd(BZLibRecord));
  end;

  inherited Destroy;
end;

function TJclBZIP2DecompressionStream.Read(var Buffer; Count: Longint): Longint;
begin
  if not FInflateInitialized then
  begin
    BZIP2LibCheck(BZ2_bzDecompressInit(BZLibRecord, 0, 0));
    FInflateInitialized := True;
  end;

  BZLibRecord.next_out := @Buffer;
  BZLibRecord.avail_out := Count;
  Result := 0;

  while Result < Count do     // as long as we need data
  begin
    if BZLibRecord.avail_in = 0 then // no more compressed data
    begin
      BZLibRecord.avail_in := FStream.Read(FBuffer^, FBufferSize);
      if BZLibRecord.avail_in = 0 then
        Exit;

      BZLibRecord.next_in := FBuffer;
    end;

    if BZLibRecord.avail_in > 0 then
    begin
      BZIP2LibCheck(BZ2_bzDecompress(BZLibRecord));
      Result := Count;
      Dec(Result, BZLibRecord.avail_out);
    end;
  end;

  Result := Count;
end;

function TJclBZIP2DecompressionStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (Offset = 0) and (Origin = soCurrent) then
    Result := (BZLibRecord.total_out_hi32 shl 32) or BZLibRecord.total_out_lo32
  else
    Result := inherited Seek(Offset, Origin);
end;

procedure InternalCompress(SourceStream: TStream; CompressStream: TJclCompressStream;
  ProgressCallback: TJclCompressStreamProgressCallback; UserData: Pointer);
var
  SourceStreamSize, SourceStreamPosition: Int64;
  Buffer: Pointer;
  ReadBytes: Integer;
  EofFlag: Boolean;
begin
  SourceStreamSize := SourceStream.Size; // source file size
  SourceStreamPosition := 0;

  GetMem(Buffer, JclDefaultBufferSize + 2);
  try
    //    ZLibStream.CopyFrom(SourceStream, 0 ); // One line way to do it! may not
    //                                     // be reliable idea to do this! also,
    //                                       //no progress callbacks!
    EofFlag := False;
    while not EofFlag do
    begin
      if Assigned(ProgressCallback) then
        ProgressCallback(SourceStreamSize, SourceStreamPosition, UserData);

      ReadBytes := SourceStream.Read(Buffer^, JclDefaultBufferSize);
      SourceStreamPosition := SourceStreamPosition + ReadBytes;

      CompressStream.WriteBuffer(Buffer^, ReadBytes);

      // short block indicates end of zlib stream
      EofFlag := ReadBytes < JclDefaultBufferSize;
    end;
    //CompressStream.Flush; (called by the destructor of compression streams
  finally
    FreeMem(Buffer);
  end;
  if Assigned(ProgressCallback) then
    ProgressCallback(SourceStreamSize, SourceStreamPosition, UserData);
end;

procedure InternalDecompress(SourceStream, DestStream: TStream;
  DecompressStream: TJclDecompressStream;
  ProgressCallback: TJclCompressStreamProgressCallback; UserData: Pointer);
var
  SourceStreamSize: Int64;
  Buffer: Pointer;
  ReadBytes: Integer;
  EofFlag: Boolean;
begin
  SourceStreamSize := SourceStream.Size; // source file size

  GetMem(Buffer, JclDefaultBufferSize + 2);
  try
    //    ZLibStream.CopyFrom(SourceStream, 0 ); // One line way to do it! may not
    //                                     // be reliable idea to do this! also,
    //                                       //no progress callbacks!
    EofFlag := False;
    while not EofFlag do
    begin
      if Assigned(ProgressCallback) then
        ProgressCallback(SourceStreamSize, SourceStream.Position, UserData);

      ReadBytes := DecompressStream.Read(Buffer^, JclDefaultBufferSize);

      DestStream.WriteBuffer(Buffer^, ReadBytes);

      // short block indicates end of zlib stream
      EofFlag := ReadBytes < JclDefaultBufferSize;
    end;
  finally
    FreeMem(Buffer);
  end;
  if Assigned(ProgressCallback) then
    ProgressCallback(SourceStreamSize, SourceStream.Position, UserData);
end;

{ Compress to a .gz file - one liner - NEW MARCH 2007  }

function GZipFile(SourceFile, DestinationFile: string; CompressionLevel: Integer;
  ProgressCallback: TJclCompressStreamProgressCallback; UserData: Pointer): Boolean;
var
  GZipStream: TJclGZIPCompressionStream;
  DestStream: TFileStream;
  SourceStream: TFileStream;
  GZipStreamDateTime: TDateTime;
begin
  Result := False;
  if not FileExists(SourceFile) then // can't copy what doesn't exist!
    Exit;

  GetFileLastWrite(SourceFile, GZipStreamDateTime);

  {destination and source streams first and second}
  SourceStream := TFileStream.Create(SourceFile, fmOpenRead or fmShareDenyWrite);
  try
    DestStream := TFileStream.Create(DestinationFile, fmCreate); // see SysUtils
    try
      {   create compressionstream third, and copy from source,
          through zlib compress layer,
          out through file stream}
      GZipStream := TJclGZIPCompressionStream.Create(DestStream, CompressionLevel);
      try
        GZipStream.DosTime := GZipStreamDateTime;
        InternalCompress(SourceStream, GZipStream, ProgressCallback, UserData);
      finally
        GZipStream.Free;
      end;
    finally
      DestStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
  Result := FileExists(DestinationFile);
end;

{ Decompress a .gz file }

function UnGZipFile(SourceFile, DestinationFile: string;
  ProgressCallback: TJclCompressStreamProgressCallback; UserData: Pointer): Boolean;
var
  GZipStream: TJclGZIPDecompressionStream;
  DestStream: TFileStream;
  SourceStream: TFileStream;
  GZipStreamDateTime: TDateTime;
begin
  Result := False;
  if not FileExists(SourceFile) then // can't copy what doesn't exist!
    exit;

  {destination and source streams first and second}
  SourceStream := TFileStream.Create(SourceFile, {mode} fmOpenRead or fmShareDenyWrite);
  try
    DestStream := TFileStream.Create(DestinationFile, {mode} fmCreate); // see SysUtils
    try
      {   create decompressionstream third, and copy from source,
          through zlib decompress layer, out through file stream
      }
      GZipStream := TJclGZIPDecompressionStream.Create(SourceStream);
      try
        InternalDecompress(SourceStream, DestStream, GZipStream, ProgressCallback, UserData);
        GZipStreamDateTime := GZipStream.DosTime;
      finally
        GZipStream.Free;
      end;
    finally
      DestStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
  Result := FileExists(DestinationFile);
  if Result and (GZipStreamDateTime <> 0) then
    // preserve datetime when unpacking! (see JclFileUtils)
    SetFileLastWrite(DestinationFile, GZipStreamDateTime);
end;

{ Compress to a .bz2 file - one liner }

function BZip2File(SourceFile, DestinationFile: string; CompressionLevel: Integer;
  ProgressCallback: TJclCompressStreamProgressCallback; UserData: Pointer): Boolean;
var
  BZip2Stream: TJclBZIP2CompressionStream;
  DestStream:  TFileStream;
  SourceStream: TFileStream;
begin
  Result := False;
  if not FileExists(SourceFile) then // can't copy what doesn't exist!
    Exit;

  {destination and source streams first and second}
  SourceStream := TFileStream.Create(SourceFile, fmOpenRead or fmShareDenyWrite);
  try
    DestStream := TFileStream.Create(DestinationFile, fmCreate); // see SysUtils
    try
      {   create compressionstream third, and copy from source,
          through zlib compress layer,
          out through file stream}
      BZip2Stream := TJclBZIP2CompressionStream.Create(DestStream, CompressionLevel);
      try
        InternalCompress(SourceStream, BZip2Stream, ProgressCallback, UserData);
      finally
        BZip2Stream.Free;
      end;
    finally
      DestStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
  Result := FileExists(DestinationFile);
end;

{ Decompress a .bzip2 file }

function UnBZip2File(SourceFile, DestinationFile: string;
  ProgressCallback: TJclCompressStreamProgressCallback; UserData: Pointer): Boolean;
var
  BZip2Stream: TJclBZIP2DecompressionStream;
  DestStream:  TFileStream;
  SourceStream: TFileStream;
begin
  Result := False;
  if not FileExists(SourceFile) then // can't copy what doesn't exist!
    exit;

  {destination and source streams first and second}
  SourceStream := TFileStream.Create(SourceFile, {mode} fmOpenRead or fmShareDenyWrite);
  try
    DestStream := TFileStream.Create(DestinationFile, {mode} fmCreate); // see SysUtils
    try
      {   create decompressionstream third, and copy from source,
          through zlib decompress layer, out through file stream
      }
      BZip2Stream := TJclBZIP2DecompressionStream.Create(SourceStream);
      try
        InternalDecompress(SourceStream, DestStream, BZip2Stream, ProgressCallback, UserData);
      finally
        BZip2Stream.Free;
      end;
    finally
      DestStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
  Result := FileExists(DestinationFile);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
