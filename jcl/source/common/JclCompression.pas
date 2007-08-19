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

// Last modified: $Date$

unit JclCompression;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows, Sevenzip,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Types,
  {$ENDIF UNIX}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  SysUtils, Classes, Contnrs,
  zlibh, bzip2,
  JclBase, JclStreams;

{**************************************************************************************************
  Class hierarchy

  TJclCompressionStream
   |
   |-- TJclCompressStream
   |    |
   |    |-- TJclZLibCompressStream     handled by zlib http://www.zlib.net/
   |    |-- TJclBZIP2CompressStream    handled by bzip2 http://www.bzip.net/
   |    |-- TJclGZIPCompressStream     handled by zlib http://www.zlib.net/ + JCL
   |
   |-- TJclDecompressStream
        |
        |-- TJclZLibDecompressStream   handled by zlib http://www.zlib.net/
        |-- TBZIP2DecompressStream     handled by bzip2 http://www.bzip.net/
        |-- TGZIPDecompressStream      handled by zlib http://www.zlib.net/ + JCL

  TJclCompressionArchive
   |
   |-- TJclCompressArchive
   |    |
   |    |-- TJclSevenzipCompressArchive
   |         |
   |         |-- TJclZipCompressArchive     handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclBZ2CompressArchive     handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJcl7zCompressArchive      handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclTarCompressArchive     handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclGZipCompressArchive    handled by sevenzip http://sevenzip.sourceforge.net/
   |
   |-- TJclDecompressArchive
   |    |
   |    |-- TJclSevenZipDecompressArchive
   |         |
   |         |-- TJclZipDecompressArchive   handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclBZ2DecompressArchive   handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclRarDecompressArchive   handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclArjDecompressArchive   handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclZDecompressArchive     handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclLzhDecompressArchive   handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJcl7zDecompressArchive    handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclNsisDecompressArchive  handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclIsoDecompressArchive   handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclCabDecompressArchive   handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclChmDecompressArchive   handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclSplitDecompressArchive handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclRpmDecompressArchive   handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclDebDecompressArchive   handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclCpioDecompressArchive  handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclTarDecompressArchive   handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclGZipDecompressArchive  handled by sevenzip http://sevenzip.sourceforge.net/
   |
   |-- TJclUpdateArchive
        |
        |-- TJclSevenzipUpdateArchive
             |
             |-- TJclZipUpdateArchive       handled by sevenzip http://sevenzip.sourceforge.net/
             |-- TJclBZ2UpdateArchive       handled by sevenzip http://sevenzip.sourceforge.net/
             |-- TJcl7zUpdateArchive        handled by sevenzip http://sevenzip.sourceforge.net/
             |-- TJclTarUpdateArchive       handled by sevenzip http://sevenzip.sourceforge.net/
             |-- TJclGZipUpdateArchive      handled by sevenzip http://sevenzip.sourceforge.net/
  
**************************************************************************************************}

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
  public
    constructor Create(Source: TStream);
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
    constructor Create(Source: TStream; WindowBits: Integer = DEF_WBITS);
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
  JCL_GZIP_FLAG_TEXT    = $01; // file is probably ASCII text
  JCL_GZIP_FLAG_CRC     = $02; // a CRC16 for the header is present
  JCL_GZIP_FLAG_EXTRA   = $04; // extra fields present
  JCL_GZIP_FLAG_NAME    = $08; // original file name is present
  JCL_GZIP_FLAG_COMMENT = $10; // comment is present

  // ExtraFlags field : compression level
  JCL_GZIP_EFLAG_MAX  = 2; // compressor used maximum compression
  JCL_GZIP_EFLAG_FAST = 4; // compressor used fastest compression

  // OS field : file system
  JCL_GZIP_OS_FAT     = 0; // FAT filesystem (MS-DOS, OS/2, NT/Win32)
  JCL_GZIP_OS_AMIGA   = 1; // Amiga
  JCL_GZIP_OS_VMS     = 2; // VMS (or OpenVMS)
  JCL_GZIP_OS_UNIX    = 3; // Unix
  JCL_GZIP_OS_VM      = 4; // VM/CMS
  JCL_GZIP_OS_ATARI   = 5; // Atari TOS
  JCL_GZIP_OS_HPFS    = 6; // HPFS filesystem (OS/2, NT)
  JCL_GZIP_OS_MAC     = 7; // Macintosh
  JCL_GZIP_OS_Z       = 8; // Z-System
  JCL_GZIP_OS_CPM     = 9; // CP/M
  JCL_GZIP_OS_TOPS    = 10; // TOPS-20
  JCL_GZIP_OS_NTFS    = 11; // NTFS filesystem (NT)
  JCL_GZIP_OS_QDOS    = 12; // QDOS
  JCL_GZIP_OS_ACORN   = 13; // Acorn RISCOS
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
    FOriginalFileName: TFileName;
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
    property OriginalFileName: TFileName read FOriginalFileName write FOriginalFileName;
    property Comment: string read FComment write FComment;
  end;

  TJclGZIPDecompressionStream = class(TJclDecompressStream)
  private
    FHeader: TJclGZIPHeader;
    FFooter: TJclGZIPFooter;
    FCompressedDataStream: TJclDelegatedStream;
    FZLibStream: TJclZLibDecompressStream;
    FOriginalFileName: TFileName;
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
    constructor Create(Source: TStream; CheckHeaderCRC: Boolean = True);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;

    property ComputedHeaderCRC16: Word read FComputedHeaderCRC16;
    property StoredHeaderCRC16: Word read FStoredHeaderCRC16;
    property ExtraField: string read FExtraField;
    property OriginalFileName: TFileName read FOriginalFileName;
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

    constructor Create(Source: TStream); overload;
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
procedure GZipStream(SourceStream, DestinationStream: TStream; CompressionLevel: Integer = Z_DEFAULT_COMPRESSION;
  ProgressCallback: TJclCompressStreamProgressCallback = nil; UserData: Pointer = nil);
procedure UnGZipStream(SourceStream, DestinationStream: TStream;
  ProgressCallback: TJclCompressStreamProgressCallback = nil; UserData: Pointer = nil);

function BZip2File(SourceFile, DestinationFile: string; CompressionLevel: Integer = 5;
  ProgressCallback: TJclCompressStreamProgressCallback = nil; UserData: Pointer = nil): Boolean;
function UnBZip2File(SourceFile, DestinationFile: string;
  ProgressCallback: TJclCompressStreamProgressCallback = nil; UserData: Pointer = nil): Boolean;
procedure BZip2Stream(SourceStream, DestinationStream: TStream; CompressionLevel: Integer = 5;
  ProgressCallback: TJclCompressStreamProgressCallback = nil; UserData: Pointer = nil);
procedure UnBZip2Stream(SourceStream, DestinationStream: TStream;
  ProgressCallback: TJclCompressStreamProgressCallback = nil; UserData: Pointer = nil);

// archive ancestor classes
{$IFDEF MSWINDOWS}
type
  TJclCompressionVolumeEvent = procedure(Sender: TObject; Index: Integer;
    var AFileName: TFileName; var AStream: TStream; var AOwnsStream: Boolean) of object;
  TJclCompressionVolumeMaxSizeEvent = procedure(Sender: TObject; Index: Integer;
    var AVolumeMaxSize: Int64) of object;
  TJclCompressionProgressEvent = procedure(Sender: TObject; const Value, MaxValue: Int64) of object;

  TJclCompressionItemProperty = (ipPackedName, ipPackedSize, ipFileSize,
    ipFileName, ipAttributes, ipCreationTime, ipLastAccessTime, ipLastWriteTime,
    ipComment, ipHostOS, ipHostFS, ipUser, ipGroup, ipCRC, ipStream, ipMethod);
  TJclCompressionItemProperties = set of TJclCompressionItemProperty;

  TJclCompressionItemKind = (ikFile, ikDirectory);

  TJclCompressionOperationSuccess = (osNoOperation, osOK, osUnsupportedMethod,
    osDataError, osCRCError, osUnknownError);

  TJclCompressionArchive = class;

  TJclCompressionItem = class
  private
    FArchive: TJclCompressionArchive;
    // source or destination
    FFileName: TFileName;
    FStream: TStream;
    FOwnsStream: Boolean;
    // miscellaneous
    FValidProperties: TJclCompressionItemProperties;
    FModifiedProperties: TJclCompressionItemProperties;
    FPackedIndex: Integer;
    FSelected: Boolean;
    FOperationSuccess: TJclCompressionOperationSuccess;
    // file properties
    FPackedName: WideString;
    FPackedSize: Int64;
    FFileSize: Int64;
    FAttributes: Cardinal;
    FCreationTime: TFileTime;
    FLastAccessTime: TFileTime;
    FLastWriteTime: TFileTime;
    FComment: WideString;
    FHostOS: WideString;
    FHostFS: WideString;
    FUser: WideString;
    FGroup: WideString;
    FCRC: Cardinal;
    FMethod: WideString;
  protected
    // property checkers
    procedure CheckGetProperty(AProperty: TJclCompressionItemProperty); virtual; abstract;
    procedure CheckSetProperty(AProperty: TJclCompressionItemProperty); virtual; abstract;
    // property getters
    function GetAttributes: Cardinal;
    function GetComment: WideString;
    function GetCRC: Cardinal;
    function GetCreationTime: TFileTime;
    function GetFileName: TFileName;
    function GetFileSize: Int64;
    function GetGroup: WideString;
    function GetHostFS: WideString;
    function GetHostOS: WideString;
    function GetItemKind: TJclCompressionItemKind;
    function GetLastAccessTime: TFileTime;
    function GetLastWriteTime: TFileTime;
    function GetMethod: WideString;
    function GetPackedName: WideString;
    function GetPackedSize: Int64;
    function GetStream: TStream; virtual; abstract;
    function GetUser: WideString;
    // property setters
    procedure SetAttributes(const Value: Cardinal);
    procedure SetComment(const Value: WideString);
    procedure SetCRC(const Value: Cardinal);
    procedure SetCreationTime(const Value: TFileTime);
    procedure SetFileName(const Value: TFileName);
    procedure SetFileSize(const Value: Int64);
    procedure SetGroup(const Value: WideString);
    procedure SetHostFS(const Value: WideString);
    procedure SetHostOS(const Value: WideString);
    procedure SetLastAccessTime(const Value: TFileTime);
    procedure SetLastWriteTime(const Value: TFileTime);
    procedure SetMethod(const Value: WideString);
    procedure SetPackedName(const Value: WideString);
    procedure SetPackedSize(const Value: Int64);
    procedure SetStream(const Value: TStream);
    procedure SetUser(const Value: WideString);
  public
    constructor Create(AArchive: TJclCompressionArchive);
    destructor Destroy; override;
    // release stream if owned and created from file name
    procedure ReleaseStream;
    // properties in archive
    property Attributes: Cardinal read GetAttributes write SetAttributes;
    property Comment: WideString read GetComment write SetComment;
    property CRC: Cardinal read GetCRC write SetCRC;
    property CreationTime: TFileTime read GetCreationTime write SetCreationTime;
    property FileSize: Int64 read GetFileSize write SetFileSize;
    property Group: WideString read GetGroup write SetGroup;
    property HostOS: WideString read GetHostOS write SetHostOS;
    property HostFS: WideString read GetHostFS write SetHostFS;
    property Kind: TJclCompressionItemKind read GetItemKind;
    property LastAccessTime: TFileTime read GetLastAccessTime write SetLastAccessTime;
    property LastWriteTime: TFileTime read GetLastWriteTime write SetLastWriteTime;
    property Method: WideString read GetMethod write SetMethod;
    property PackedName: WideString read GetPackedName write SetPackedName;
    property PackedSize: Int64 read GetPackedSize write SetPackedSize;
    property User: WideString read GetUser write SetUser;
    // source or destination
    property FileName: TFileName read GetFileName write SetFileName;
    property OwnsStream: Boolean read FOwnsStream write FOwnsStream;
    property Stream: TStream read GetStream write SetStream;
    // miscellaneous
    property Archive: TJclCompressionArchive read FArchive;
    property OperationSuccess: TJclCompressionOperationSuccess read FOperationSuccess
      write FOperationSuccess;
    property ValidProperties: TJclCompressionItemProperties read FValidProperties;
    property ModifiedProperties: TJclCompressionItemProperties read FModifiedProperties
      write FModifiedProperties;
    property PackedIndex: Integer read FPackedIndex;
    property Selected: Boolean read FSelected write FSelected;
  end;

  TJclCompressionItemClass = class of TJclCompressionItem;

  TJclCompressionVolume = class
  protected
    FFileName: TFileName;
    FStream: TStream;
    FOwnsStream: Boolean;
    FVolumeMaxSize: Int64;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean; AFileName: TFileName;
      AVolumeMaxSize: Int64);
    destructor Destroy; override;
    property FileName: TFileName read FFileName;
    property Stream: TStream read FStream;
    property OwnsStream: Boolean read FOwnsStream;
    property VolumeMaxSize: Int64 read FVolumeMaxSize;
  end;

  TJclCompressionArchive = class
  private
    FOnProgress: TJclCompressionProgressEvent;
    FOnVolume: TJclCompressionVolumeEvent;
    FOnVolumeMaxSize: TJclCompressionVolumeMaxSizeEvent;
    FPassword: WideString;
    FVolumeIndex: Integer;
    FVolumeIndexOffset: Integer;
    FVolumeMaxSize: Int64;
    FVolumeNameMask: string;
    function GetItemCount: Integer;
    function GetItem(Index: Integer): TJclCompressionItem;
    function GetVolumeCount: Integer;
    function GetVolume(Index: Integer): TJclCompressionVolume;
  protected
    FVolumes: TObjectList;
    FItems: TObjectList;

    procedure CreateCompressionObject; virtual;
    procedure FreeCompressionObject; virtual;

    function InternalOpenVolume(const FileName: TFileName): TStream; virtual; abstract;

    procedure DoProgress(const Value, MaxValue: Int64);
    function NeedVolume(Index: Integer): TStream;
    function NeedVolumeMaxSize(Index: Integer): Int64;
    function GetItemClass: TJclCompressionItemClass; virtual; abstract;
  public
    constructor Create(Volume0: TStream; AVolumeMaxSize: Int64 = 0;
      AOwnVolume: Boolean = False); overload;
    constructor Create(const VolumeName: string; AVolumeMaxSize: Int64 = 0;
      VolumeMask: Boolean = False); overload;
      // if VolumeMask is true then VolumeName represents a mask to get volume file names
      // "myfile%d.zip" "myfile.zip.%.3d" ...
    destructor Destroy; override;

    function AddVolume(const VolumeName: string;
      AVolumeMaxSize: Int64 = 0): Integer; overload; virtual;
    function AddVolume(VolumeStream: TStream; AVolumeMaxSize: Int64 = 0;
      AOwnsStream: Boolean = False): Integer; overload; virtual;

    // miscellaneous
    procedure ClearVolumes;
    procedure ClearItems;

    procedure CheckOperationSuccess;
    procedure ClearOperationSuccess;
    procedure SelectAll;
    procedure UnselectAll;

    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TJclCompressionItem read GetItem;

    property VolumeCount: Integer read GetVolumeCount;
    property Volumes[Index: Integer]: TJclCompressionVolume read GetVolume;
    property VolumeMaxSize: Int64 read FVolumeMaxSize;
    property VolumeNameMask: string read FVolumeNameMask;
    property VolumeIndexOffset: Integer read FVolumeIndexOffset write FVolumeIndexOffset;

    property OnProgress: TJclCompressionProgressEvent read FOnProgress write FOnProgress;

    // volume events
    property OnVolume: TJclCompressionVolumeEvent read FOnVolume write FOnVolume;
    property OnVolumeMaxSize: TJclCompressionVolumeMaxSizeEvent read FOnVolumeMaxSize
      write FOnVolumeMaxSize;
    property Password: WideString read FPassword write FPassword;
  end;

  TJclCompressItem = class(TJclCompressionItem)
  protected
    procedure CheckGetProperty(AProperty: TJclCompressionItemProperty); override;
    procedure CheckSetProperty(AProperty: TJclCompressionItemProperty); override;
    function GetStream: TStream; override;
  end;

  TJclCompressArchive = class(TJclCompressionArchive)
  private
    FBaseRelName: WideString;
    FBaseDirName: string;
    FAddFilesInDir: Boolean;
    function InternalGetRelItemName(const ItemName: TFileName): WideString;
    procedure InternalAddFile(const Directory: string; const FileInfo: TSearchRec);
    procedure InternalAddDirectory(const Directory: string);
  protected
    FCompressing: Boolean;
    procedure CheckNotCompressing;

    function InternalOpenVolume(const FileName: TFileName): TStream; override;
  public
    function AddDirectory(const PackedName: WideString;
      const DirName: string = ''; RecurseIntoDir: Boolean = False;
      AddFilesInDir: Boolean = False): Integer; overload; virtual;
    function AddFile(const PackedName: WideString;
      const FileName: string): Integer; overload; virtual;
    function AddFile(const PackedName: WideString; AStream: TStream;
      AOwnsStream: Boolean = False): Integer; overload; virtual;
    procedure Compress; virtual; abstract;
  end;

  TJclDecompressItem = class(TJclCompressionItem)
  protected
    procedure CheckGetProperty(AProperty: TJclCompressionItemProperty); override;
    procedure CheckSetProperty(AProperty: TJclCompressionItemProperty); override;
    function GetStream: TStream; override;
    function ValidateExtraction(Index: Integer): Boolean; virtual;
  end;

  // return False not to extract this file
  // assign your own FileName, Stream or AOwnsStream to override default one
  TJclCompressionExtractEvent = function (Sender: TObject; Index: Integer;
    var FileName: TFileName; var Stream: TStream; var AOwnsStream: Boolean): Boolean of object;

  TJclDecompressArchive = class(TJclCompressionArchive)
  private
    FOnExtract: TJclCompressionExtractEvent;
    FAutoCreateSubDir: Boolean;
  protected
    FDecompressing: Boolean;
    FListing: Boolean;
    FDestinationDir: string;
    FExtractingAllIndex: Integer;
    procedure CheckNotDecompressing;
    procedure CheckListing;

    function InternalOpenVolume(const FileName: TFileName): TStream; override;
    function ValidateExtraction(Index: Integer; out FileName: TFileName; var AStream: TStream;
      var AOwnsStream: Boolean): Boolean; virtual;
  public
    procedure ListFiles; virtual; abstract;
    procedure ExtractSelected(const ADestinationDir: string = '';
      AAutoCreateSubDir: Boolean = True); virtual; abstract;
    procedure ExtractAll(const ADestinationDir: string = '';
      AAutoCreateSubDir: Boolean = True); virtual; abstract;

    property OnExtract: TJclCompressionExtractEvent read FOnExtract write FOnExtract;
    property DestinationDir: string read FDestinationDir;
    property AutoCreateSubDir: Boolean read FAutoCreateSubDir;
  end;

// sevenzip classes for compression
type
  TJclSevenzipCompressArchive = class(TJclCompressArchive)
  private
    FOutArchive: IOutArchive;
    FCompressionLevel: Cardinal;
    procedure SetCompressionLevel(Value: Cardinal);
  protected
    FProgressMax: Int64;
    procedure CreateCompressionObject; override;
    procedure FreeCompressionObject; override;
    function GetCLSID: TGUID; virtual; abstract;
    procedure SetCompressionProperties; virtual;
    function GetItemClass: TJclCompressionItemClass; override;
  public
    procedure Compress; override;

    property CompressionLevel: Cardinal read FCompressionLevel write SetCompressionLevel; // X9
  end;

  // file formats

  TJclZipMethod = (zmDeflate, zmDeflate64, zmCopy, zmBZip2);

  TJclZipCompressArchive = class(TJclSevenzipCompressArchive)
  private
    FDefaultMethod: TJclZipMethod;
    procedure SetDefaultMethod(Value: TJclZipMethod);
  protected
    procedure CreateCompressionObject; override;
    function GetCLSID: TGUID; override;
    procedure SetCompressionProperties; override;
  public
    property DefaultMethod: TJclZipMethod read FDefaultMethod write SetDefaultMethod;
  end;

  TJclBZ2CompressArchive = class(TJclSevenzipCompressArchive)
  protected
    procedure CreateCompressionObject; override;
    function GetCLSID: TGUID; override;
  end;

  TJcl7zCompressArchive = class(TJclSevenzipCompressArchive)
  protected
    procedure CreateCompressionObject; override;
    function GetCLSID: TGUID; override;
  end;

  TJclTarCompressArchive = class(TJclSevenzipCompressArchive)
  protected
    procedure CreateCompressionObject; override;
    function GetCLSID: TGUID; override;
    procedure SetCompressionProperties; override;
  end;

  TJclGZipCompressArchive = class(TJclSevenzipCompressArchive)
  protected
    procedure CreateCompressionObject; override;
    function GetCLSID: TGUID; override;
  end;

// sevenzip classes for decompression
type
  TJclSevenzipDecompressArchive = class(TJclDecompressArchive)
  private
    FInArchive: IInArchive;
    FOpened: Boolean;
    FListed: Boolean;
    FInStream: IInStream;
  protected
    FProgressMax: Int64;
    procedure OpenArchive;
    procedure CreateCompressionObject; override;
    procedure FreeCompressionObject; override;
    function GetCLSID: TGUID; virtual; abstract;
    function GetItemClass: TJclCompressionItemClass; override;
  public
    procedure ListFiles; override;
    procedure ExtractSelected(const ADestinationDir: string = '';
      AAutoCreateSubDir: Boolean = True); override;
    procedure ExtractAll(const ADestinationDir: string = '';
      AAutoCreateSubDir: Boolean = True); override;
  end;

  // file formats

  TJclZipDecompressArchive = class(TJclSevenzipDecompressArchive)
  protected
    function GetCLSID: TGUID; override;
  end;

  TJclBZ2DecompressArchive = class(TJclSevenzipDecompressArchive)
  protected
    function GetCLSID: TGUID; override;
  end;

  TJclRarDecompressArchive = class(TJclSevenzipDecompressArchive)
  protected
    function GetCLSID: TGUID; override;
  end;

  TJclArjDecompressArchive = class(TJclSevenzipDecompressArchive)
  protected
    function GetCLSID: TGUID; override;
  end;

  TJclZDecompressArchive = class(TJclSevenzipDecompressArchive)
  protected
    function GetCLSID: TGUID; override;
  end;

  TJclLzhDecompressArchive = class(TJclSevenzipDecompressArchive)
  protected
    function GetCLSID: TGUID; override;
  end;

  TJcl7zDecompressArchive = class(TJclSevenzipDecompressArchive)
  protected
    function GetCLSID: TGUID; override;
  end;

  TJclNsisDecompressArchive = class(TJclSevenzipDecompressArchive)
  protected
    function GetCLSID: TGUID; override;
  end;

  TJclIsoDecompressArchive = class(TJclSevenzipDecompressArchive)
  protected
    function GetCLSID: TGUID; override;
  end;

  TJclCabDecompressArchive = class(TJclSevenzipDecompressArchive)
  protected
    function GetCLSID: TGUID; override;
  end;

  TJclChmDecompressArchive = class(TJclSevenzipDecompressArchive)
  protected
    function GetCLSID: TGUID; override;
  end;

  TJclSplitDecompressArchive = class(TJclSevenzipDecompressArchive)
  protected
    function GetCLSID: TGUID; override;
  end;

  TJclRpmDecompressArchive = class(TJclSevenzipDecompressArchive)
  protected
    function GetCLSID: TGUID; override;
  end;

  TJclDebDecompressArchive = class(TJclSevenzipDecompressArchive)
  protected
    function GetCLSID: TGUID; override;
  end;

  TJclCpioDecompressArchive = class(TJclSevenzipDecompressArchive)
  protected
    function GetCLSID: TGUID; override;
  end;

  TJclTarDecompressArchive = class(TJclSevenzipDecompressArchive)
  protected
    function GetCLSID: TGUID; override;
  end;

  TJclGZipDecompressArchive = class(TJclSevenzipDecompressArchive)
  protected
    function GetCLSID: TGUID; override;
  end;
{$ENDIF MSWINDOWS}

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
  {$IFDEF MSWINDOWS}
  ActiveX,
  {$ENDIF MSWINDOWS}
  {$IFDEF COMPILER5}
  ComObj, // GUIDToString
  {$ENDIF COMPILER5}
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

constructor TJclDecompressStream.Create(Source: TStream);
begin
  inherited Create(Source);
  FStream := Source;
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

constructor TJclZLibDecompressStream.Create(Source: TStream; WindowBits: Integer = DEF_WBITS);
begin
  inherited Create(Source);

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

constructor TJclGZIPDecompressionStream.Create(Source: TStream; CheckHeaderCRC: Boolean);
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
  inherited Create(Source);

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
  BZLibRecord.bzalloc   := nil; // Use build-in memory allocation functionality
  BZLibRecord.bzfree    := nil;
  BZLibRecord.next_in   := nil;
  BZLibRecord.avail_in  := 0;
  BZLibRecord.next_out  := FBuffer;
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

//=== { TJclBZip2DecompressionStream } =======================================

constructor TJclBZIP2DecompressionStream.Create(Source: TStream);
begin
  inherited Create(Source);

  LoadBZip2;

  // Initialize ZLib StreamRecord
  BZLibRecord.bzalloc   := nil; // Use build-in memory allocation functionality
  BZLibRecord.bzfree    := nil;
  BZLibRecord.opaque    := nil;
  BZLibRecord.next_in   := nil;
  BZLibRecord.state     := nil;
  BZLibRecord.avail_in  := 0;
  BZLibRecord.next_out  := FBuffer;
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
    end
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

procedure GZipStream(SourceStream, DestinationStream: TStream; CompressionLevel: Integer = Z_DEFAULT_COMPRESSION;
  ProgressCallback: TJclCompressStreamProgressCallback = nil; UserData: Pointer = nil);
var
  GZStream: TJclGZIPCompressionStream;
begin
  GZStream := TJclGZIPCompressionStream.Create(DestinationStream, CompressionLevel);
  try
    InternalCompress(SourceStream, GZStream, ProgressCallback, UserData);
  finally
    GZStream.Free;
  end;
end;

procedure UnGZipStream(SourceStream, DestinationStream: TStream;
  ProgressCallback: TJclCompressStreamProgressCallback = nil; UserData: Pointer = nil);
var
  GZipStream: TJclGZIPDecompressionStream;
begin
  GZipStream := TJclGZIPDecompressionStream.Create(SourceStream);
  try
    InternalDecompress(SourceStream, DestinationStream, GZipStream, ProgressCallback, UserData);
  finally
    GZipStream.Free;
  end;
end;

{ Compress to a .bz2 file - one liner }

function BZip2File(SourceFile, DestinationFile: string; CompressionLevel: Integer;
  ProgressCallback: TJclCompressStreamProgressCallback; UserData: Pointer): Boolean;
var
  BZip2Stream: TJclBZIP2CompressionStream;
  DestStream: TFileStream;
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
  DestStream: TFileStream;
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
        InternalDecompress(SourceStream, DestStream,  BZip2Stream, ProgressCallback, UserData);
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

procedure BZip2Stream(SourceStream, DestinationStream: TStream; CompressionLevel: Integer = 5;
  ProgressCallback: TJclCompressStreamProgressCallback = nil; UserData: Pointer = nil);
var
  BZ2Stream: TJclBZIP2CompressionStream;
begin
  BZ2Stream := TJclBZIP2CompressionStream.Create(DestinationStream, CompressionLevel);
  try
    InternalCompress(SourceStream, BZ2Stream, ProgressCallback, UserData);
  finally
    BZ2Stream.Free;
  end;
end;

procedure UnBZip2Stream(SourceStream, DestinationStream: TStream;
  ProgressCallback: TJclCompressStreamProgressCallback = nil; UserData: Pointer = nil);
var
  BZip2Stream: TJclBZIP2DecompressionStream;
begin
  BZip2Stream := TJclBZIP2DecompressionStream.Create(SourceStream);
  try
    InternalDecompress(SourceStream, DestinationStream,  BZip2Stream, ProgressCallback, UserData);
  finally
    BZip2Stream.Free;
  end;
end;

//=== { TJclCompressionItem } ================================================
{$IFDEF MSWINDOWS}
constructor TJclCompressionItem.Create(AArchive: TJclCompressionArchive);
begin
  inherited Create;
  FArchive := AArchive;
end;

destructor TJclCompressionItem.Destroy;
begin
  ReleaseStream;
  
  inherited Destroy;
end;

function TJclCompressionItem.GetAttributes: Cardinal;
begin
  CheckGetProperty(ipAttributes);
  Result := FAttributes;
end;

function TJclCompressionItem.GetComment: WideString;
begin
  CheckGetProperty(ipComment);
  Result := FComment;
end;

function TJclCompressionItem.GetCRC: Cardinal;
begin
  CheckGetProperty(ipCRC);
  Result := FCRC;
end;

function TJclCompressionItem.GetCreationTime: TFileTime;
begin
  CheckGetProperty(ipCreationTime);
  Result := FCreationTime;
end;

function TJclCompressionItem.GetFileName: TFileName;
begin
  CheckGetProperty(ipFileName);
  Result := FFileName;
end;

function TJclCompressionItem.GetFileSize: Int64;
begin
  CheckGetProperty(ipFileSize);
  Result := FFileSize;
end;

function TJclCompressionItem.GetGroup: WideString;
begin
  CheckGetProperty(ipGroup);
  Result := FGroup;
end;

function TJclCompressionItem.GetHostFS: WideString;
begin
  CheckGetProperty(ipHostFS);
  Result := FHostFS;
end;

function TJclCompressionItem.GetHostOS: WideString;
begin
  CheckGetProperty(ipHostOS);
  Result := FHostOS;
end;

function TJclCompressionItem.GetItemKind: TJclCompressionItemKind;
begin
  if (Attributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
    Result := ikDirectory
  else
    Result := ikFile;
end;

function TJclCompressionItem.GetLastAccessTime: TFileTime;
begin
  CheckGetProperty(ipLastAccessTime);
  Result := FLastAccessTime;
end;

function TJclCompressionItem.GetLastWriteTime: TFileTime;
begin
  CheckGetProperty(ipLastWriteTime);
  Result := FLastWriteTime;
end;

function TJclCompressionItem.GetMethod: WideString;
begin
  CheckGetProperty(ipMethod);
  Result := FMethod;
end;

function TJclCompressionItem.GetPackedName: WideString;
begin
  CheckGetProperty(ipPackedName);
  Result := FPackedName;
end;

function TJclCompressionItem.GetPackedSize: Int64;
begin
  CheckGetProperty(ipPackedSize);
  Result := FPackedSize;
end;

function TJclCompressionItem.GetUser: WideString;
begin
  CheckGetProperty(ipUser);
  Result := FUser;
end;

procedure TJclCompressionItem.ReleaseStream;
begin
  if OwnsStream or (FileName <> '') then
    FreeAndNil(FStream);
end;

procedure TJclCompressionItem.SetAttributes(const Value: Cardinal);
begin
  CheckSetProperty(ipAttributes);
  FAttributes := Value;
  Include(FModifiedProperties, ipAttributes);
  Include(FValidProperties, ipAttributes);
end;

procedure TJclCompressionItem.SetComment(const Value: WideString);
begin
  CheckSetProperty(ipComment);
  FComment := Value;
  Include(FModifiedProperties, ipComment);
  Include(FValidProperties, ipComment);
end;

procedure TJclCompressionItem.SetCRC(const Value: Cardinal);
begin
  CheckSetProperty(ipCRC);
  FCRC := Value;
  Include(FModifiedProperties, ipCRC);
  Include(FValidProperties, ipCRC);
end;

procedure TJclCompressionItem.SetCreationTime(const Value: TFileTime);
begin
  CheckSetProperty(ipCreationTime);
  FCreationTime := Value;
  Include(FModifiedProperties, ipCreationTime);
  Include(FValidProperties, ipCreationTime);
end;

procedure TJclCompressionItem.SetFileName(const Value: TFileName);
var
  AFindData: TWin32FindData;
begin
  CheckSetProperty(ipFileName);
  FFileName := Value;
  Include(FModifiedProperties, ipFileName);
  Include(FValidProperties, ipFileName);

  if (Value <> '') and (FArchive is TJclCompressionArchive)
    and GetFileAttributesEx(PAnsiChar(Value), GetFileExInfoStandard, @AFindData) then
  begin
    FileSize := (Int64(AFindData.nFileSizeHigh) shl 32) or AFindData.nFileSizeLow;
    Attributes := AFindData.dwFileAttributes;
    CreationTime := AFindData.ftCreationTime;
    LastAccessTime := AFindData.ftLastAccessTime;
    LastWriteTime := AFindData.ftLastWriteTime;
    // TODO: user name and group (using file handle and GetSecurityInfo)
    {$IFDEF MSWINDOWS}
    HostOS := RsCompression7zWindows;
    {$ENDIF MSWINDOWS}
    {$IFDEF UNIX}
    HostOS := RsCompression7zUnix;
    {$ENDIF UNIX}
  end;
end;

procedure TJclCompressionItem.SetFileSize(const Value: Int64);
begin
  CheckSetProperty(ipFileSize);
  FFileSize := Value;
  Include(FModifiedProperties, ipFileSize);
  Include(FValidProperties, ipFileSize);
end;

procedure TJclCompressionItem.SetGroup(const Value: WideString);
begin
  CheckSetProperty(ipGroup);
  FGroup := Value;
  Include(FModifiedProperties, ipGroup);
  Include(FValidProperties, ipGroup);
end;

procedure TJclCompressionItem.SetHostFS(const Value: WideString);
begin
  CheckSetProperty(ipHostFS);
  FHostFS := Value;
  Include(FModifiedProperties, ipHostFS);
  Include(FValidProperties, ipHostFS);
end;

procedure TJclCompressionItem.SetHostOS(const Value: WideString);
begin
  CheckSetProperty(ipHostOS);
  FHostOS := Value;
  Include(FModifiedProperties, ipHostOS);
  Include(FValidProperties, ipHostOS);
end;

procedure TJclCompressionItem.SetLastAccessTime(const Value: TFileTime);
begin
  CheckSetProperty(ipLastAccessTime);
  FLastAccessTime := Value;
  Include(FModifiedProperties, ipLastAccessTime);
  Include(FValidProperties, ipLastAccessTime);
end;

procedure TJclCompressionItem.SetLastWriteTime(const Value: TFileTime);
begin
  CheckSetProperty(ipLastWriteTime);
  FLastWriteTime := Value;
  Include(FModifiedProperties, ipLastWriteTime);
  Include(FValidProperties, ipLastWriteTime);
end;

procedure TJclCompressionItem.SetMethod(const Value: WideString);
begin
  CheckSetProperty(ipMethod);
  FMethod := Value;
  Include(FModifiedProperties, ipMethod);
  Include(FValidProperties, ipMethod);
end;

procedure TJclCompressionItem.SetPackedName(const Value: WideString);
begin
  CheckSetProperty(ipPackedName);
  FPackedName := Value;
  Include(FModifiedProperties, ipPackedName);
  Include(FValidProperties, ipPackedName);
end;

procedure TJclCompressionItem.SetPackedSize(const Value: Int64);
begin
  CheckSetProperty(ipPackedSize);
  FPackedSize := Value;
  Include(FModifiedProperties, ipPackedSize);
  Include(FValidProperties, ipPackedSize);
end;

procedure TJclCompressionItem.SetStream(const Value: TStream);
begin
  CheckSetProperty(ipStream);
  ReleaseStream;
  FStream := Value;
  Include(FModifiedProperties, ipStream);
  Include(FValidProperties, ipStream);
end;

procedure TJclCompressionItem.SetUser(const Value: WideString);
begin
  CheckSetProperty(ipUser);
  FUser := Value;
  Include(FModifiedProperties, ipUser);
  Include(FValidProperties, ipUser);
end;

//=== { TJclCompressionVolume } ==============================================

constructor TJclCompressionVolume.Create(AStream: TStream; AOwnsStream: Boolean;
  AFileName: TFileName; AVolumeMaxSize: Int64);
begin
  inherited Create;
  FStream := AStream;
  FOwnsStream := AOwnsStream;
  FFileName := AFileName;
  FVolumeMaxSize := AVolumeMaxSize;
end;

destructor TJclCompressionVolume.Destroy;
begin
  if OwnsStream then
    FStream.Free;
  inherited Destroy;
end;

//=== { TJclCompressionArchive } =============================================

constructor TJclCompressionArchive.Create(Volume0: TStream;
  AVolumeMaxSize: Int64 = 0; AOwnVolume: Boolean = False);
begin
  inherited Create;
  FVolumeIndex := -1;
  FVolumeIndexOffset := 1;
  FVolumeMaxSize := AVolumeMaxSize;
  FItems := TObjectList.Create(True);
  FVolumes := TObjectList.Create(True);
  if Assigned(Volume0) then
    AddVolume(Volume0, AVolumeMaxSize, AOwnVolume);
  CreateCompressionObject;
end;

constructor TJclCompressionArchive.Create(const VolumeName: string;
  AVolumeMaxSize: Int64 = 0; VolumeMask: Boolean = False);
begin
  inherited Create;
  FVolumeIndex := -1;
  FVolumeIndexOffset := 1;
  FVolumeMaxSize := AVolumeMaxSize;
  FItems := TObjectList.Create(True);
  FVolumes := TObjectList.Create(True);
  if VolumeMask then
    FVolumeNameMask := VolumeName
  else
    AddVolume(VolumeName, AVolumeMaxSize);
  CreateCompressionObject;
end;

destructor TJclCompressionArchive.Destroy;
begin
  FItems.Free;
  FVolumes.Free;

  FreeCompressionObject;
  inherited Destroy;
end;

function TJclCompressionArchive.AddVolume(VolumeStream: TStream;
  AVolumeMaxSize: Int64; AOwnsStream: Boolean): Integer;
begin
  Result := FVolumes.Add(TJclCompressionVolume.Create(VolumeStream, AOwnsStream, '', AVolumeMaxSize));
end;

function TJclCompressionArchive.AddVolume(const VolumeName: string;
  AVolumeMaxSize: Int64): Integer;
begin
  Result := FVolumes.Add(TJclCompressionVolume.Create(nil, True, VolumeName, AVolumeMaxSize));
end;

procedure TJclCompressionArchive.CheckOperationSuccess;
var
  Index: Integer;
begin
  for Index := 0 to FItems.Count - 1 do
  begin
    case TJclCompressionItem(FItems.Items[Index]).OperationSuccess of
      osNoOperation: ;
      osOK: ;
      osUnsupportedMethod:
        raise EJclCompressionError.CreateRes(@RsCompressionUnsupportedMethod);
      osDataError:
        raise EJclCompressionError.CreateRes(@RsCompressionDataError);
      osCRCError:
        raise EJclCompressionError.CreateRes(@RsCompressionCRCError);
    else
      raise EJclCompressionError.CreateRes(@RsCompressionUnknownError);
    end;
  end;
end;

procedure TJclCompressionArchive.ClearItems;
begin
  FItems.Clear;
end;

procedure TJclCompressionArchive.ClearOperationSuccess;
var
  Index: Integer;
begin
  for Index := 0 to FItems.Count - 1 do
    TJclCompressionItem(FItems.Items[Index]).OperationSuccess := osNoOperation;
end;

procedure TJclCompressionArchive.ClearVolumes;
begin
  FVolumes.Clear;
end;

procedure TJclCompressionArchive.CreateCompressionObject;
begin
  // override to customize
end;

procedure TJclCompressionArchive.DoProgress(const Value, MaxValue: Int64);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Value, MaxValue);
end;

procedure TJclCompressionArchive.FreeCompressionObject;
begin
  // override to customize
end;

function TJclCompressionArchive.GetItem(Index: Integer): TJclCompressionItem;
begin
  Result := TJclCompressionItem(FItems.Items[Index]);
end;

function TJclCompressionArchive.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TJclCompressionArchive.GetVolume(Index: Integer): TJclCompressionVolume;
begin
  Result := TJclCompressionVolume(FVolumes.Items[Index]);
end;

function TJclCompressionArchive.GetVolumeCount: Integer;
begin
  Result := FVolumes.Count;
end;

function TJclCompressionArchive.NeedVolume(Index: Integer): TStream;
var
  AVolume: TJclCompressionVolume;
  AOwnsStream: Boolean;
  AFileName: TFileName;
begin
  Result := nil;

  if Index <> FVolumeIndex then
  begin
    AOwnsStream := VolumeNameMask <> '';
    AVolume := nil;
    AFileName := Format(VolumeNameMask, [Index + VolumeIndexOffset]);
    if (Index >= 0) and (Index < FVolumes.Count) then
    begin
      AVolume := TJclCompressionVolume(FVolumes.Items[Index]);
      Result := AVolume.Stream;
      AOwnsStream := AVolume.OwnsStream;
      AFileName := AVolume.FileName;
    end;

    if Assigned(FOnVolume) then
      FOnVolume(Self, Index, AFileName, Result, AOwnsStream);

    if Assigned(AVolume) then
    begin
      if not Assigned(Result) then
        Result := InternalOpenVolume(AFileName);
      AVolume.FFileName := AFileName;
      AVolume.FStream := Result;
      AVolume.FOwnsStream := AOwnsStream;
    end
    else
    begin
      while FVolumes.Count < Index do
        FVolumes.Add(TJclCompressionVolume.Create(nil, True, Format(VolumeNameMask, [Index + VolumeIndexOffset]), FVolumeMaxSize));
      if not Assigned(Result) then
        Result := InternalOpenVolume(AFileName);
      if Assigned(Result) then
      begin
        if Index < FVolumes.Count then
        begin
          AVolume := TJclCompressionVolume(FVolumes.Items[Index]);
          AVolume.FFileName := AFileName;
          AVolume.FStream := Result;
          AVolume.FOwnsStream := AOwnsStream;
          AVolume.FVolumeMaxSize := FVolumeMaxSize;
        end
        else
          FVolumes.Add(TJclCompressionVolume.Create(Result, AOwnsStream, AFileName, FVolumeMaxSize));
      end;
    end;
    FVolumeIndex := Index;
  end
  else if (Index >= 0) and (Index < FVolumes.Count) then
  begin
    AVolume := TJclCompressionVolume(FVolumes.Items[Index]);
    Result := AVolume.Stream;
    if Assigned(Result) then
      StreamSeek(Result, 0, soBeginning);
  end
  else
    FVolumeIndex := Index;
end;

function TJclCompressionArchive.NeedVolumeMaxSize(Index: Integer): Int64;
var
  AVolume: TJclCompressionVolume;
begin
  if (Index <> FVolumeIndex) then
  begin
    AVolume := nil;
    if (Index >= 0) and (Index < FVolumes.Count) then
    begin
      AVolume := TJclCompressionVolume(FVolumes.Items[Index]);
      FVolumeMaxSize := AVolume.VolumeMaxSize;
    end;
    if Assigned(FOnVolumeMaxSize) then
      FOnVolumeMaxSize(Self, Index, FVolumeMaxSize);
    if Assigned(AVolume) then
      AVolume.FVolumeMaxSize := FVolumeMaxSize
    else
    begin
      while FVolumes.Count < Index do
        FVolumes.Add(TJclCompressionVolume.Create(nil, True, Format(VolumeNameMask, [Index + VolumeIndexOffset]), FVolumeMaxSize));
      if Index < FVolumes.Count then
      begin
        AVolume := TJclCompressionVolume(FVolumes.Items[Index]);
        AVolume.FFileName := Format(VolumeNameMask, [Index + VolumeIndexOffset]);
        AVolume.FStream := nil;
        AVolume.FOwnsStream := True;
        AVolume.FVolumeMaxSize := FVolumeMaxSize;
      end
      else
        FVolumes.Add(TJclCompressionVolume.Create(nil, True, Format(VolumeNameMask, [Index + VolumeIndexOffset]), FVolumeMaxSize));
    end;
  end;
  Result := FVolumeMaxSize;
end;

procedure TJclCompressionArchive.SelectAll;
var
  Index: Integer;
begin
  for Index := 0 to FItems.Count - 1 do
    TJclCompressionItem(FItems.Items[Index]).Selected := True;
end;

procedure TJclCompressionArchive.UnselectAll;
var
  Index: Integer;
begin
  for Index := 0 to FItems.Count - 1 do
    TJclCompressionItem(FItems.Items[Index]).Selected := False;
end;

//=== { TJclCompressItem } ===================================================

procedure TJclCompressItem.CheckGetProperty(
  AProperty: TJclCompressionItemProperty);
begin
  // always valid
end;

procedure TJclCompressItem.CheckSetProperty(
  AProperty: TJclCompressionItemProperty);
begin
  if AProperty in [ipMethod] then
    raise EJclCompressionError.CreateRes(@RsCompressionWriteNotSupported);
  (Archive as TJclCompressArchive).CheckNotCompressing;
end;

function TJclCompressItem.GetStream: TStream;
begin
  if not Assigned(FStream) and (FileName <> '') then
  begin
    FStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    FOwnsStream := True;
  end;
  Result := FStream;
end;

//=== { TJclCompressArchive } ================================================

procedure TJclCompressArchive.CheckNotCompressing;
begin
  if FCompressing then
    raise EJclCompressionError.CreateRes(@RsCompressionCompressingError);
end;

function TJclCompressArchive.AddDirectory(const PackedName: WideString;
  const DirName: string; RecurseIntoDir: Boolean; AddFilesInDir: Boolean): Integer;
var
  AItem: TJclCompressionItem;
begin
  CheckNotCompressing;

  if DirName <> '' then
  begin
    FBaseRelName := PackedName;
    FBaseDirName := PathRemoveSeparator(DirName);
    FAddFilesInDir := AddFilesInDir;

    if RecurseIntoDir then
    begin
      Result := FItems.Count;
      EnumDirectories(DirName, InternalAddDirectory, True, '', nil);
      Exit;
    end;
  end;

  AItem := GetItemClass.Create(Self);
  try
    AItem.PackedName := PackedName;
    AItem.FileName := DirName;
  except
    AItem.Destroy;
    raise;
  end;

  Result := FItems.Add(AItem);

  if (DirName <> '') and AddFilesInDir then
    EnumFiles(PathAddSeparator(DirName) + '*', InternalAddFile, faDirectory);
end;

function TJclCompressArchive.AddFile(const PackedName: WideString;
  const FileName: string): Integer;
var
  AItem: TJclCompressionItem;
begin
  CheckNotCompressing;

  AItem := GetItemClass.Create(Self);
  try
    AItem.PackedName := PackedName;
    AItem.FileName := FileName;
  except
    AItem.Destroy;
    raise;
  end;

  Result := FItems.Add(AItem);
end;

function TJclCompressArchive.AddFile(const PackedName: WideString;
  AStream: TStream; AOwnsStream: Boolean): Integer;
var
  AItem: TJclCompressionItem;
begin
  CheckNotCompressing;

  AItem := GetItemClass.Create(Self);
  try
    AItem.PackedName := PackedName;
    AItem.Stream := AStream;
    AItem.OwnsStream := AOwnsStream;
  except
    AItem.Destroy;
    raise;
  end;

  Result := FItems.Add(AItem);
end;

procedure TJclCompressArchive.InternalAddDirectory(const Directory: string);
begin
  AddDirectory(InternalGetRelItemName(Directory), Directory, False, FAddFilesInDir);
end;

procedure TJclCompressArchive.InternalAddFile(const Directory: string;
  const FileInfo: TSearchRec);
var
  AFileName: string;
  AItem: TJclCompressionItem;
begin
  AFileName := PathAddSeparator(Directory) + FileInfo.Name;

  AItem := GetItemClass.Create(Self);
  try
    AItem.PackedName := InternalGetRelItemName(AFileName);
    AItem.FileName := AFileName;
  except
    AItem.Destroy;
    raise;
  end;

  FItems.Add(AItem);
end;

function TJclCompressArchive.InternalGetRelItemName(
  const ItemName: TFileName): WideString;
var
  RelPath: string;
begin
  RelPath := PathGetRelativePath(FBaseDirName, ItemName);
  if (RelPath <> '.') and (RelPath <> '') then
    Result := FBaseRelName + DirDelimiter + RelPath
  else
    Result := FBaseRelName;
end;

function TJclCompressArchive.InternalOpenVolume(const FileName: TFileName): TStream;
begin
  Result := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
end;

//=== { TJclDecompressItem } =================================================

procedure TJclDecompressItem.CheckGetProperty(
  AProperty: TJclCompressionItemProperty);
begin
  // TODO
end;

procedure TJclDecompressItem.CheckSetProperty(
  AProperty: TJclCompressionItemProperty);
begin
  (Archive as TJclDecompressArchive).CheckListing;
end;

function TJclDecompressItem.GetStream: TStream;
begin
  if not Assigned(FStream) and (FileName <> '') then
  begin
    FStream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
    FOwnsStream := True;
  end;
  Result := FStream;
end;

function TJclDecompressItem.ValidateExtraction(Index: Integer): Boolean;
begin
  Result := (FArchive as TJclDecompressArchive).ValidateExtraction(Index,
    FFileName, FStream, FOwnsStream);
end;

//=== { TJclDecompressArchive } ==============================================

procedure TJclDecompressArchive.CheckListing;
begin
  if not FListing then
    raise EJclCompressionError.CreateRes(@RsCompressionUnavailableProperty);
end;

procedure TJclDecompressArchive.CheckNotDecompressing;
begin
  if FDecompressing then
    raise EJclCompressionError.CreateRes(@RsCompressionDecompressingError);
end;

function TJclDecompressArchive.InternalOpenVolume(const FileName: TFileName): TStream;
begin
  if FileExists(FileName) then
    Result := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite)
  else
    Result := nil;
end;

function TJclDecompressArchive.ValidateExtraction(Index: Integer;
  out FileName: TFileName; var AStream: TStream; var AOwnsStream: Boolean): Boolean;
begin
  if FExtractingAllIndex <> -1 then
    // extracting all
    FExtractingAllIndex := Index;

  FileName := PathGetRelativePath(FDestinationDir, Items[Index].PackedName);
  Result := True;

  if Assigned(FOnExtract) then
    Result := FOnExtract(Self, Index, FileName, AStream, AOwnsStream);

  if Result and not Assigned(AStream) and AutoCreateSubDir then
  begin
    if (Items[Index].Attributes and faDirectory) <> 0 then
      ForceDirectories(FileName)
    else
      ForceDirectories(ExtractFilePath(FileName));
  end;
end;

//=== { TJclSevenzipOutStream } ==============================================

type
  TJclSevenzipOutStream = class(TInterfacedObject, ISequentialOutStream,
    IOutStream, IUnknown)
  private
    FArchive: TJclCompressionArchive;
    FFileIndex: Integer;
    FStream: TStream;
    FOwnsStream: Boolean;
    procedure NeedStream;
    procedure ReleaseStream;
  public
    constructor Create(AArchive: TJclCompressionArchive; AFileIndex: Integer); overload;
    constructor Create(AStream: TStream; AOwnsStream: Boolean); overload;
    destructor Destroy; override;
    // ISequentialOutStream
    function Write(Data: Pointer; Size: Cardinal; ProcessedSize: PCardinal): HRESULT; stdcall;
    // IOutStream
    function Seek(Offset: Int64; SeekOrigin: Cardinal; NewPosition: PInt64): HRESULT; stdcall;
    function SetSize(NewSize: Int64): HRESULT; stdcall;
  end;

constructor TJclSevenzipOutStream.Create(AArchive: TJclCompressionArchive; AFileIndex: Integer);
begin
  inherited Create;

  FArchive := AArchive;
  FFileIndex := AFileIndex;
  FStream := nil;
  FOwnsStream := False;
end;

constructor TJclSevenzipOutStream.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create;

  FArchive := nil;
  FFileIndex := -1;
  FStream := AStream;
  FOwnsStream := AOwnsStream;
end;

destructor TJclSevenzipOutStream.Destroy;
begin
  ReleaseStream;
    
  inherited Destroy;
end;

procedure TJclSevenzipOutStream.NeedStream;
begin
  if Assigned(FArchive) then
    FStream := FArchive.Items[FFileIndex].Stream;

  if not Assigned(FStream) then
    raise EJclCompressionError.CreateRes(@RsCompression7zUnassignedStream);
end;

procedure TJclSevenzipOutStream.ReleaseStream;
begin
  if Assigned(FArchive) then
    FArchive.Items[FFileIndex].ReleaseStream
  else if FOwnsStream then
    FStream.Free;
end;

function TJclSevenzipOutStream.Seek(Offset: Int64; SeekOrigin: Cardinal;
  NewPosition: PInt64): HRESULT;
var
  NewPos: Integer;
begin
  NeedStream;

  Result := S_OK;
  // STREAM_SEEK_SET	= 0 = soFromBeginning
  // STREAM_SEEK_CUR	= 1 = soFromCurrent
  // STREAM_SEEK_END  = 2 = soFromEnd
  NewPos := StreamSeek(FStream, Offset, TSeekOrigin(SeekOrigin));
  if Assigned(NewPosition) then
    NewPosition^ := NewPos;
end;

function TJclSevenzipOutStream.SetSize(NewSize: Int64): HRESULT;
begin
  NeedStream;

  Result := S_OK;
  FStream.Size := NewSize;
end;

function TJclSevenzipOutStream.Write(Data: Pointer; Size: Cardinal;
  ProcessedSize: PCardinal): HRESULT;
var
  Processed: Cardinal;
begin
  NeedStream;

  Result := S_OK;
  Processed := FStream.Write(Data^, Size);
  if Assigned(ProcessedSize) then
    ProcessedSize^ := Processed;
end;

//=== { TJclSevenzipInStream } ===============================================

type
  TJclSevenzipInStream = class(TInterfacedObject, ISequentialInStream,
    IInStream, IStreamGetSize, IUnknown)
  private
    FArchive: TJclCompressionArchive;
    FFileIndex: Integer;
    FStream: TStream;
    FOwnsStream: Boolean;
    procedure NeedStream;
    procedure ReleaseStream;
  public
    constructor Create(AArchive: TJclCompressionArchive; AFileIndex: Integer); overload;
    constructor Create(AStream: TStream; AOwnsStream: Boolean); overload;
    destructor Destroy; override;
    // ISequentialInStream
    function Read(Data: Pointer; Size: Cardinal; ProcessedSize: PCardinal): HRESULT; stdcall;
    // IInStream
    function Seek(Offset: Int64; SeekOrigin: Cardinal; NewPosition: PInt64): HRESULT; stdcall;
    // IStreamGetSize
    function GetSize(Size: PInt64): HRESULT; stdcall;
  end;

constructor TJclSevenzipInStream.Create(AArchive: TJclCompressionArchive; AFileIndex: Integer);
begin
  inherited Create;

  FArchive := AArchive;
  FFileIndex := AFileIndex;
  FStream := nil;
  FOwnsStream := False;
end;

constructor TJclSevenzipInStream.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create;

  FArchive := nil;
  FFileIndex := -1;
  FStream := AStream;
  FOwnsStream := AOwnsStream;
end;

destructor TJclSevenzipInStream.Destroy;
begin
  ReleaseStream;
  inherited Destroy;
end;

function TJclSevenzipInStream.GetSize(Size: PInt64): HRESULT;
begin
  NeedStream;

  if Assigned(Size) then
    Size^ := FStream.Size;
  Result := S_OK;
end;

procedure TJclSevenzipInStream.NeedStream;
begin
  if Assigned(FArchive) and not Assigned(FStream) then
    FStream := FArchive.Items[FFileIndex].Stream;

  if not Assigned(FStream) then
    raise EJclCompressionError.CreateRes(@RsCompression7zUnassignedStream);
end;

function TJclSevenzipInStream.Read(Data: Pointer; Size: Cardinal;
  ProcessedSize: PCardinal): HRESULT;
var
  Processed: Cardinal;
begin
  NeedStream;

  Processed := FStream.Read(Data^, Size);
  if Assigned(ProcessedSize) then
    ProcessedSize^ := Processed;
  Result := S_OK;
end;

procedure TJclSevenzipInStream.ReleaseStream;
begin
  if Assigned(FArchive) then
    FArchive.Items[FFileIndex].ReleaseStream
  else if FOwnsStream then
    FStream.Free;
end;

function TJclSevenzipInStream.Seek(Offset: Int64; SeekOrigin: Cardinal;
  NewPosition: PInt64): HRESULT;
var
  NewPos: Int64;
begin
  NeedStream;
  
  // STREAM_SEEK_SET	= 0 = soFromBeginning
	// STREAM_SEEK_CUR	= 1 = soFromCurrent
	// STREAM_SEEK_END  = 2 = soFromEnd
  NewPos := StreamSeek(FStream, Offset, TSeekOrigin(SeekOrigin));
  if Assigned(NewPosition) then
    NewPosition^ := NewPos;
  Result := S_OK;
end;

// sevenzip helper functions

procedure SevenzipCheck(Value: HRESULT);
begin
  if Value <> S_OK then
    raise EJclCompressionError.CreateResFmt(@RsCompression7zReturnError, [Value, SysErrorMessage(Value)]);
end;

function Get7zWideStringProp(const AArchive: IInArchive; FileIndex: Integer;
  PropID: Cardinal): WideString;
var
  Value: TPropVariant;
begin
  ZeroMemory(@Value, SizeOf(Value));
  SevenzipCheck(AArchive.GetProperty(FileIndex, PropID, Value));
  case Value.vt of
    VT_EMPTY, VT_NULL:
      Result := '';
    VT_LPSTR:
      Result := Value.pszVal;
    VT_LPWSTR:
      Result := Value.pwszVal;
    VT_BSTR:
      begin
        Result := Value.bstrVal;
        SysFreeString(Value.bstrVal);
      end;
  else
    raise EJclCompressionError.CreateResFmt(@RsCompression7zUnknownValueType, [Value.vt, PropID]);
  end;
end;

function Get7zCardinalProp(const AArchive: IInArchive; FileIndex: Integer;
  PropID: Cardinal): Cardinal;
var
  Value: TPropVariant;
begin
  ZeroMemory(@Value, SizeOf(Value));
  SevenzipCheck(AArchive.GetProperty(FileIndex, PropID, Value));
  case Value.vt of
    VT_EMPTY, VT_NULL:
      Result := 0;
    VT_I1:
      Result := Value.iVal;
    VT_I2:
      Result := Value.iVal;
    VT_INT, VT_I4:
      Result := Value.lVal;
    VT_I8:
      Result := Value.hVal.QuadPart;
    VT_UI1:
      Result := Value.bVal;
    VT_UI2:
      Result := Value.uiVal;
    VT_UINT, VT_UI4:
      Result := Value.ulVal;
    VT_UI8:
      Result := Value.uhVal.QuadPart;
  else
    raise EJclCompressionError.CreateResFmt(@RsCompression7zUnknownValueType, [Value.vt, PropID]);
  end;
end;

function Get7zInt64Prop(const AArchive: IInArchive; FileIndex: Integer;
  PropID: Cardinal): Int64;
var
  Value: TPropVariant;
begin
  ZeroMemory(@Value, SizeOf(Value));
  SevenzipCheck(AArchive.GetProperty(FileIndex, PropID, Value));
  case Value.vt of
    VT_EMPTY, VT_NULL:
      Result := 0;
    VT_I1:
      Result := Value.iVal;
    VT_I2:
      Result := Value.iVal;
    VT_INT, VT_I4:
      Result := Value.lVal;
    VT_I8:
      Result := Value.hVal.QuadPart;
    VT_UI1:
      Result := Value.bVal;
    VT_UI2:
      Result := Value.uiVal;
    VT_UINT, VT_UI4:
      Result := Value.ulVal;
    VT_UI8:
      Result := Value.uhVal.QuadPart;
  else
    raise EJclCompressionError.CreateResFmt(@RsCompression7zUnknownValueType, [Value.vt, PropID]);
  end;
end;

function Get7zFileTimeProp(const AArchive: IInArchive; FileIndex: Integer;
  PropID: Cardinal): TFileTime;
var
  Value: TPropVariant;
begin
  ZeroMemory(@Value, SizeOf(Value));
  SevenzipCheck(AArchive.GetProperty(FileIndex, PropID, Value));
  case Value.vt of
    VT_EMPTY, VT_NULL:
      begin
        Result.dwLowDateTime := 0;
        Result.dwHighDateTime := 0;
      end;
    VT_FILETIME:
      Result := Value.filetime;
  else
    raise EJclCompressionError.CreateResFmt(@RsCompression7zUnknownValueType, [Value.vt, PropID]);
  end;
end;

procedure Load7zFileAttribute(AInArchive: IInArchive; FileIndex: Integer;
  AItem: TJclCompressionItem);
begin
  AItem.PackedName := Get7zWideStringProp(AInArchive, FileIndex, kpidPath);
  AItem.FileName := '';
  AItem.Stream := nil;
  AItem.OwnsStream := False;
  AItem.Attributes := Get7zCardinalProp(AInArchive, FileIndex, kpidAttributes);
  AItem.FileSize := Get7zInt64Prop(AInArchive, FileIndex, kpidSize);
  AItem.PackedSize := Get7zInt64Prop(AInArchive, FileIndex, kpidPackedSize);
  AItem.CreationTime := Get7zFileTimeProp(AInArchive, FileIndex, kpidCreationTime);
  AItem.LastAccessTime := Get7zFileTimeProp(AInArchive, FileIndex, kpidLastAccessTime);
  AItem.LastWriteTime := Get7zFileTimeProp(AInArchive, FileIndex, kpidLastWriteTime);
  AItem.Comment := Get7zWideStringProp(AInArchive, FileIndex, kpidComment);
  AItem.HostOS := Get7zWideStringProp(AInArchive, FileIndex, kpidHostOS);
  AItem.HostFS := Get7zWideStringProp(AInArchive, FileIndex, kpidFileSystem);
  AItem.User := Get7zWideStringProp(AInArchive, FileIndex, kpidUser);
  AItem.Group := Get7zWideStringProp(AInArchive, FileIndex, kpidGroup);
  AItem.CRC := Get7zCardinalProp(AInArchive, FileIndex, kpidCRC);
  AItem.Method := Get7zWideStringProp(AInArchive, FileIndex, kpidMethod);

  // reset modified flags
  AItem.ModifiedProperties := [];
end;

//=== { TJclSevenzipOutputCallback } =========================================

type
  TJclSevenzipUpdateCallback = class(TInterfacedObject, IUnknown, IProgress,
    IArchiveUpdateCallback, IArchiveUpdateCallback2, ICryptoGetTextPassword2)
  private
    FCompressionArchive: TJclSevenzipCompressArchive;
    FLastStream: Cardinal;
  public
    constructor Create(ACompressionArchive: TJclSevenzipCompressArchive);
    // IProgress
    function SetCompleted(CompleteValue: PInt64): HRESULT; stdcall;
    function SetTotal(Total: Int64): HRESULT; stdcall;
    // IArchiveUpdateCallback
    function GetProperty(Index: Cardinal; PropID: Cardinal; out Value: tagPROPVARIANT): HRESULT; stdcall;
    function GetStream(Index: Cardinal; out InStream: ISequentialInStream): HRESULT; stdcall;
    function GetUpdateItemInfo(Index: Cardinal; NewData: PInteger;
      NewProperties: PInteger; IndexInArchive: PCardinal): HRESULT; stdcall;
    function SetOperationResult(OperationResult: Integer): HRESULT; stdcall;
    // IArchiveUpdateCallback2
    function GetVolumeSize(Index: Cardinal; Size: PInt64): HRESULT; stdcall;
    function GetVolumeStream(Index: Cardinal;
      out VolumeStream: ISequentialOutStream): HRESULT; stdcall;
    // ICryptoGetTextPassword2
    function CryptoGetTextPassword2(PasswordIsDefined: PInteger;
      Password: PBStr): HRESULT; stdcall;
  end;

constructor TJclSevenzipUpdateCallback.Create(
  ACompressionArchive: TJclSevenzipCompressArchive);
begin
  inherited Create;
  FCompressionArchive := ACompressionArchive;
end;

function TJclSevenzipUpdateCallback.CryptoGetTextPassword2(
  PasswordIsDefined: PInteger; Password: PBStr): HRESULT;
begin
  if Assigned(PasswordIsDefined) then
  begin
    if FCompressionArchive.Password <> '' then
      PasswordIsDefined^ := Integer($FFFFFFFF)
    else
      PasswordIsDefined^ := 0;
  end;
  if Assigned(Password) then
    Password^ := SysAllocString(PWideChar(FCompressionArchive.Password));
  Result := S_OK;
end;

function TJclSevenzipUpdateCallback.GetProperty(Index, PropID: Cardinal;
  out Value: tagPROPVARIANT): HRESULT;
var
  AItem: TJclCompressionItem;
begin
  Result := S_OK;
  AItem := FCompressionArchive.Items[Index];
  case PropID of
    kpidNoProperty:
      Value.vt := VT_NULL;
    //kpidHandlerItemIndex: (seems unused)
    kpidPath:
      begin
        Value.vt := VT_BSTR;
        Value.bstrVal := SysAllocString(PWideChar(AItem.PackedName));
      end;
    //kpidName: (read only)
{    kpidExtension:
      begin
        Value.vt := VT_BSTR;
        Value.bstrVal := SysAllocString(PWideChar(WideString(ExtractFileExt(FCompressionStream.FileNames[Index]))));
      end;}
    kpidIsFolder:
      begin
        Value.vt := VT_BOOL;
        Value.bool := AItem.Kind = ikDirectory;
      end;
    kpidSize:
      begin
        Value.vt := VT_UI8;
        Value.uhVal.QuadPart := AItem.FileSize;
      end;
    //kpidPackedSize:
    kpidAttributes:
      begin
        Value.vt := VT_UI4;
        Value.ulVal := AItem.Attributes;
      end;
    kpidCreationTime:
      begin
        Value.vt := VT_FILETIME;
        Value.filetime := AItem.CreationTime;
      end;
    kpidLastAccessTime:
      begin
        Value.vt := VT_FILETIME;
        Value.filetime := AItem.LastAccessTime;
      end;
    kpidLastWriteTime:
      begin
        Value.vt := VT_FILETIME;
        Value.filetime := AItem.LastWriteTime;
      end;
    kpidSolid:
      begin
        Value.vt := VT_BOOL;
        Value.bool := True;
      end;
    {kpidCommented:
    kpidEncrypted:
    kpidSplitBefore:
    kpidSplitAfter:
    kpidDictionarySize:
    kpidCRC:
    kpidType:}
    kpidIsAnti:
      begin
        Value.vt := VT_BOOL;
        Value.bool := False;
      end;
    {kpidMethod:
    kpidHostOS:
    kpidFileSystem:
    kpidUser:
    kpidGroup:
    kpidBlock:
    kpidComment:
    kpidPosition:
    kpidPrefix:}
  else
    Value.vt := VT_EMPTY;
    Result := S_FALSE;
  end;
end;

function TJclSevenzipUpdateCallback.GetStream(Index: Cardinal;
  out InStream: ISequentialInStream): HRESULT;
begin
  FLastStream := Index;
  InStream := TJclSevenzipInStream.Create(FCompressionArchive, Index);
  Result := S_OK;
end;

function TJclSevenzipUpdateCallback.GetUpdateItemInfo(Index: Cardinal; NewData,
  NewProperties: PInteger; IndexInArchive: PCardinal): HRESULT;
var
  CompressionItem: TJclCompressionItem;
begin
  CompressionItem := FCompressionArchive.Items[Index];

  if Assigned(NewData) then
  begin
    if ([ipFileName, ipStream] * CompressionItem.ModifiedProperties) <> [] then
      NewData^ := 1
    else
      NewData^ := 0;
  end;

  if Assigned(NewProperties) then
  begin
    if (CompressionItem.ModifiedProperties - [ipFileName, ipStream]) <> [] then
      NewProperties^ := 1
    else
      NewProperties^ := 0;
  end;

  // TODO
  if Assigned(IndexInArchive) then
    IndexInArchive^ := $FFFFFFFF; //Index;
  Result := S_OK;
end;

function TJclSevenzipUpdateCallback.GetVolumeSize(Index: Cardinal;
  Size: PInt64): HRESULT;
begin
  // the JCL has its own spliting engine
  if Assigned(Size) then
    Size^ := 0;
  Result := S_FALSE;
end;

function TJclSevenzipUpdateCallback.GetVolumeStream(Index: Cardinal;
  out VolumeStream: ISequentialOutStream): HRESULT;
begin
  VolumeStream := nil;
  Result := S_FALSE;
end;

function TJclSevenzipUpdateCallback.SetCompleted(
  CompleteValue: PInt64): HRESULT;
begin
  if Assigned(CompleteValue) then
    FCompressionArchive.DoProgress(CompleteValue^, FCompressionArchive.FProgressMax);
  Result := S_OK;
end;

function TJclSevenzipUpdateCallback.SetOperationResult(
  OperationResult: Integer): HRESULT;
begin
  case OperationResult of
    kOK:
      FCompressionArchive.Items[FLastStream].OperationSuccess := osOK;
    kUnSupportedMethod:
      FCompressionArchive.Items[FLastStream].OperationSuccess := osUnsupportedMethod;
    kDataError:
      FCompressionArchive.Items[FLastStream].OperationSuccess := osDataError;
    kCRCError:
      FCompressionArchive.Items[FLastStream].OperationSuccess := osCRCError;
  else
    FCompressionArchive.Items[FLastStream].OperationSuccess := osUnknownError;
  end;

  Result := S_OK;
end;

function TJclSevenzipUpdateCallback.SetTotal(Total: Int64): HRESULT;
begin
  FCompressionArchive.FProgressMax := Total;
  Result := S_OK;
end;

//=== { TJclSevenzipCompressArchive } ========================================

procedure TJclSevenzipCompressArchive.CreateCompressionObject;
var
  SevenzipCLSID, InterfaceID: TGUID;
begin
  SevenzipCLSID := GetCLSID;
  InterfaceID := Sevenzip.IOutArchive;
  if (Sevenzip.CreateObject(@SevenzipCLSID, @InterfaceID, FOutArchive) <> ERROR_SUCCESS)
    or not Assigned(FOutArchive) then
    raise EJclCompressionError.CreateResFmt(@RsCompression7zOutArchiveError, [GUIDToString(SevenzipCLSID)]);
end;

procedure TJclSevenzipCompressArchive.FreeCompressionObject;
begin
  FOutArchive := nil;
end;

function TJclSevenzipCompressArchive.GetItemClass: TJclCompressionItemClass;
begin
  Result := TJclCompressItem;
end;

procedure TJclSevenzipCompressArchive.SetCompressionLevel(Value: Cardinal);
begin
  CheckNotCompressing;

  FCompressionLevel := Value;
end;

procedure TJclSevenzipCompressArchive.SetCompressionProperties;
var
  PropertiesSetter: ISetProperties;
  PropNames: array [0..0] of PWideChar;
  PropValues: array [0..0] of TPropVariant;
begin
  if Supports(FOutArchive, ISetProperties, PropertiesSetter) and Assigned(PropertiesSetter) then
  begin
    PropNames[0] := 'X';
    PropValues[0].vt := VT_UI4;
    PropValues[0].ulVal := CompressionLevel;

    SevenzipCheck(PropertiesSetter.SetProperties(@PropNames[0], @PropValues[0], Length(PropNames)));
  end
  else
    raise EJclCompressionError.CreateRes(@RsCompression7zNoProperties);
end;

procedure TJclSevenzipCompressArchive.Compress;
var
  OutStream: IOutStream;
  UpdateCallback: IArchiveUpdateCallback;
  SplitStream: TJclDynamicSplitStream;
begin
  CheckNotCompressing;

  FCompressing := True;
  try
    SplitStream := TJclDynamicSplitStream.Create;
    SplitStream.OnVolume := NeedVolume;
    SplitStream.OnVolumeMaxSize := NeedVolumeMaxSize;
    OutStream := TJclSevenzipOutStream.Create(SplitStream, True);
    UpdateCallback := TJclSevenzipUpdateCallback.Create(Self);

    SetCompressionProperties;

    SevenzipCheck(FOutArchive.UpdateItems(OutStream, ItemCount, UpdateCallback));
  finally
    FCompressing := False;
  end;
end;

//=== { TJcl7zCompressArchive } ==============================================

procedure TJcl7zCompressArchive.CreateCompressionObject;
begin
  inherited CreateCompressionObject;
  FCompressionLevel := 9;
end;

function TJcl7zCompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormat7z;
end;

//=== { TJclZipCompressArchive } =============================================

procedure TJclZipCompressArchive.CreateCompressionObject;
begin
  inherited CreateCompressionObject;
  FCompressionLevel := 9;
end;

function TJclZipCompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatZip;
end;

procedure TJclZipCompressArchive.SetCompressionProperties;
var
  PropertiesSetter: ISetProperties;
  PropNames: array [0..1] of PWideChar;
  PropValues: array [0..1] of TPropVariant;
const
  CompressionMethodNames: array [TJclZipMethod] of WideString =
    ( kDeflateMethodName {zmDeflate}, kDeflate64MethodName {zmDeflate64},
      kCopyMethod {zmCopy}, kBZip2MethodName {zmBZip2} );
begin
  if Supports(FOutArchive, ISetProperties, PropertiesSetter) and Assigned(PropertiesSetter) then
  begin
    PropNames[0] := 'X';
    PropValues[0].vt := VT_UI4;
    PropValues[0].ulVal := CompressionLevel;
    PropNames[1] := 'M';
    PropValues[1].vt := VT_BSTR;
    PropValues[1].bstrVal := SysAllocString(PWideChar(CompressionMethodNames[DefaultMethod]));

    SevenzipCheck(PropertiesSetter.SetProperties(@PropNames[0], @PropValues[0], Length(PropNames)));
  end
  else
    raise EJclCompressionError.CreateRes(@RsCompression7zNoProperties);
end;

procedure TJclZipCompressArchive.SetDefaultMethod(Value: TJclZipMethod);
begin
  CheckNotCompressing;

  FDefaultMethod := Value;
end;

//=== { TJclBZ2CompressArchive } =============================================

procedure TJclBZ2CompressArchive.CreateCompressionObject;
begin
  inherited CreateCompressionObject;
  FCompressionLevel := 9;
end;

function TJclBZ2CompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatBZ2;
end;

//=== { TJclTarCompressArchive } =============================================

procedure TJclTarCompressArchive.CreateCompressionObject;
begin
  inherited CreateCompressionObject;
  FCompressionLevel := 0;
end;

function TJclTarCompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatTar;
end;

procedure TJclTarCompressArchive.SetCompressionProperties;
begin
  // no properties
end;

//=== { TJclGZipCompressArchive } ============================================

procedure TJclGZipCompressArchive.CreateCompressionObject;
begin
  inherited CreateCompressionObject;
  FCompressionLevel := 9;
end;

function TJclGZipCompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatGZip;
end;

//=== { TJclSevenzipOpenCallback } ===========================================

type
  TJclSevenzipOpenCallback = class(TInterfacedObject, IArchiveOpenCallback,
    ICryptoGetTextPassword, IUnknown)
  private
    FDecompressionArchive: TJclSevenzipDecompressArchive;
  public
    constructor Create(ADecompressionArchive: TJclSevenzipDecompressArchive);
    // IArchiveOpenCallback
    function SetCompleted(Files: PInt64; Bytes: PInt64): HRESULT; stdcall;
    function SetTotal(Files: PInt64; Bytes: PInt64): HRESULT; stdcall;
    // ICryptoGetTextPassword
    function CryptoGetTextPassword(password: PBStr): HRESULT; stdcall;
  end;

constructor TJclSevenzipOpenCallback.Create(
  ADecompressionArchive: TJclSevenzipDecompressArchive);
begin
  inherited Create;
  FDecompressionArchive := ADecompressionArchive;
end;

function TJclSevenzipOpenCallback.CryptoGetTextPassword(
  password: PBStr): HRESULT;
begin
  if Assigned(password) then
    password^ := SysAllocString(PWideChar(FDecompressionArchive.Password));
  Result := S_OK;
end;

function TJclSevenzipOpenCallback.SetCompleted(Files, Bytes: PInt64): HRESULT;
begin
  if Assigned(Files) then
    FDecompressionArchive.DoProgress(Files^, FDecompressionArchive.FProgressMax);
  Result := S_OK;
end;

function TJclSevenzipOpenCallback.SetTotal(Files, Bytes: PInt64): HRESULT;
begin
  if Assigned(Files) then
    FDecompressionArchive.FProgressMax := Files^;
  Result := S_OK;
end;

//=== { TJclSevenzipExtractCallback } ========================================

type
  TJclSevenzipExtractCallback = class(TInterfacedObject, IUnknown, IProgress,
    IArchiveExtractCallback, ICryptoGetTextPassword)
  private
    FDecompressionArchive: TJclSevenzipDecompressArchive;
    FLastStream: Cardinal;
  public
    constructor Create(ADecompressionArchive: TJclSevenzipDecompressArchive);
    // IArchiveExtractCallback
    function GetStream(Index: Cardinal; out OutStream: ISequentialOutStream;
      askExtractMode: Cardinal): HRESULT; stdcall;
    function PrepareOperation(askExtractMode: Cardinal): HRESULT; stdcall;
    function SetOperationResult(resultEOperationResult: Integer): HRESULT; stdcall;
    // IProgress
    function SetCompleted(CompleteValue: PInt64): HRESULT; stdcall;
    function SetTotal(Total: Int64): HRESULT; stdcall;
    // ICryptoGetTextPassword
    function CryptoGetTextPassword(password: PBStr): HRESULT; stdcall;
  end;

constructor TJclSevenzipExtractCallback.Create(
  ADecompressionArchive: TJclSevenzipDecompressArchive);
begin
  inherited Create;
  FDecompressionArchive := ADecompressionArchive;
end;

function TJclSevenzipExtractCallback.CryptoGetTextPassword(
  password: PBStr): HRESULT;
begin
  if Assigned(password) then
    password^ := SysAllocString(PWideChar(FDecompressionArchive.Password));
  Result := S_OK;
end;

function TJclSevenzipExtractCallback.GetStream(Index: Cardinal;
  out OutStream: ISequentialOutStream; askExtractMode: Cardinal): HRESULT;
begin
  FLastStream := Index;
  if (FDecompressionArchive.Items[Index] as TJclDecompressItem).ValidateExtraction(Index) then
  begin
    OutStream := TJclSevenzipOutStream.Create(FDecompressionArchive, Index);
    Result := S_OK;
  end
  else
  begin
    OutStream := nil;
    Result := S_FALSE;
  end;
end;

function TJclSevenzipExtractCallback.PrepareOperation(
  askExtractMode: Cardinal): HRESULT;
begin
  Result := S_OK;
end;

function TJclSevenzipExtractCallback.SetCompleted(
  CompleteValue: PInt64): HRESULT;
begin
  if Assigned(CompleteValue) then
    FDecompressionArchive.DoProgress(CompleteValue^, FDecompressionArchive.FProgressMax);
  Result := S_OK;
end;

function TJclSevenzipExtractCallback.SetOperationResult(
  resultEOperationResult: Integer): HRESULT;
begin
  case resultEOperationResult of
    kOK:
      FDecompressionArchive.Items[FLastStream].OperationSuccess := osOK;
    kUnSupportedMethod:
      FDecompressionArchive.Items[FLastStream].OperationSuccess := osUnsupportedMethod;
    kDataError:
      FDecompressionArchive.Items[FLastStream].OperationSuccess := osDataError;
    kCRCError:
      FDecompressionArchive.Items[FLastStream].OperationSuccess := osCRCError;
  else
    FDecompressionArchive.Items[FLastStream].OperationSuccess := osUnknownError;
  end;

  Result := S_OK;
end;

function TJclSevenzipExtractCallback.SetTotal(Total: Int64): HRESULT;
begin
  FDecompressionArchive.FProgressMax := Total;
  Result := S_OK;
end;

//=== { TJclSevenzipDecompressArchive } ======================================

procedure TJclSevenzipDecompressArchive.CreateCompressionObject;
var
  SevenzipCLSID, InterfaceID: TGUID;
begin
  SevenzipCLSID := GetCLSID;
  InterfaceID := Sevenzip.IInArchive;
  if (Sevenzip.CreateObject(@SevenzipCLSID, @InterfaceID, FInArchive) <> ERROR_SUCCESS)
    or not Assigned(FInArchive) then
    raise EJclCompressionError.CreateResFmt(@RsCompression7zInArchiveError, [GUIDToString(SevenzipCLSID)]);
  FExtractingAllIndex := -1;
end;

procedure TJclSevenzipDecompressArchive.ExtractAll(const ADestinationDir: string;
  AAutoCreateSubDir: Boolean);
var
  AExtractCallback: IArchiveExtractCallback;
begin
  FDestinationDir := ADestinationDir;
  FAutoCreateSubDir := AAutoCreateSubDir;
  
  if FDestinationDir <> '' then
    FDestinationDir := PathAddSeparator(FDestinationDir);

  FDecompressing := True;
  FExtractingAllIndex := 0;
  AExtractCallback := TJclSevenzipExtractCallback.Create(Self);
  try
    OpenArchive;

    SevenzipCheck(FInArchive.Extract(nil, $FFFFFFFF, 0, AExtractCallback));
    CheckOperationSuccess;
  finally
    FDestinationDir := '';
    FDecompressing := False;
    FExtractingAllIndex := -1;
    AExtractCallback := nil;
  end;
end;

procedure TJclSevenzipDecompressArchive.ExtractSelected(const ADestinationDir: string;
  AAutoCreateSubDir: Boolean);
var
  AExtractCallback: IArchiveExtractCallback;
  Indices: array of Cardinal;
  NbIndices: Cardinal;
  Index: Integer;
begin
  FDestinationDir := ADestinationDir;
  FAutoCreateSubDir := AAutoCreateSubDir;

  if FDestinationDir <> '' then
    FDestinationDir := PathAddSeparator(FDestinationDir);

  FDecompressing := True;
  AExtractCallback := TJclSevenzipExtractCallback.Create(Self);
  try
    OpenArchive;

    NbIndices := 0;
    for Index := 0 to ItemCount - 1 do
      if Items[Index].Selected then
        Inc(NbIndices);

    SetLength(Indices, NbIndices);
    NbIndices := 0;
    for Index := 0 to ItemCount - 1 do
      if Items[Index].Selected then
    begin
      Indices[NbIndices] := Index;
      Inc(NbIndices);
    end;

    SevenzipCheck(FInArchive.Extract(@Indices[0], Length(Indices), 0, AExtractCallback));
    CheckOperationSuccess;
  finally
    FDestinationDir := '';
    FDecompressing := False;
    AExtractCallback := nil;
  end;
end;

procedure TJclSevenzipDecompressArchive.FreeCompressionObject;
begin
  FInArchive := nil;
  FInStream := nil;
end;

function TJclSevenzipDecompressArchive.GetItemClass: TJclCompressionItemClass;
begin
  Result := TJclDecompressItem;
end;

procedure TJclSevenzipDecompressArchive.ListFiles;
var
  NumberOfItems: Cardinal;
  Index: Integer;
  AItem: TJclCompressionItem;
begin
  CheckNotDecompressing;

  FListing := True;
  try
    ClearItems;
    OpenArchive;

    SevenzipCheck(FInArchive.GetNumberOfItems(@NumberOfItems));
    if NumberOfItems > 0 then
    begin
      for Index := 0 to NumberOfItems - 1 do
      begin
        AItem := GetItemClass.Create(Self);
        Load7zFileAttribute(FInArchive, Index, AItem);
        FItems.Add(AItem);
      end;
    end;

    FListed := True;
  finally
    FListing := False;
  end;
end;

procedure TJclSevenzipDecompressArchive.OpenArchive;
var
  SplitStream: TJclDynamicSplitStream;
  OpenCallback: IArchiveOpenCallback;
  MaxCheckStartPosition: Int64;
begin
  if not FOpened then
  begin
    if (FVolumeMaxSize <> 0) or (FVolumes.Count <> 0) then
    begin
      SplitStream := TJclDynamicSplitStream.Create;
      SplitStream.OnVolume := NeedVolume;
      SplitStream.OnVolumeMaxSize := NeedVolumeMaxSize;
      FInStream := TJclSevenzipInStream.Create(SplitStream, True);
    end
    else
      FInStream := TJclSevenzipInStream.Create(NeedVolume(0), False);
    OpenCallback := TJclSevenzipOpenCallback.Create(Self);

    MaxCheckStartPosition := 1 shl 22;
    SevenzipCheck(FInArchive.Open(FInStream, @MaxCheckStartPosition, OpenCallback));

    FOpened := True;
  end;
end;

//=== { TJclZipDecompressArchive } ===========================================

function TJclZipDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatZip;
end;

//=== { TJclBZ2DecompressArchive } ===========================================

function TJclBZ2DecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatBZ2;
end;

//=== { TJclRarDecompressArchive } ===========================================

function TJclRarDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatRar;
end;

//=== { TJclArjDecompressArchive } ===========================================

function TJclArjDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatArj;
end;

//=== { TJclZDecompressArchive } =============================================

function TJclZDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatZ;
end;

//=== { TJclLzhDecompressArchive } ===========================================

function TJclLzhDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatLzh;
end;

//=== { TJcl7zDecompressArchive } ============================================

function TJcl7zDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormat7z;
end;

//=== { TJclNsisDecompressArchive } ==========================================

function TJclNsisDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatNsis;
end;

//=== { TJclIsoDecompressArchive } ===========================================

function TJclIsoDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatIso;
end;

//=== { TJclCabDecompressArchive } ===========================================

function TJclCabDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatCab;
end;

//=== { TJclChmDecompressArchive } ===========================================

function TJclChmDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatChm;
end;

//=== { TJclSplitDecompressArchive } =========================================

function TJclSplitDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatSplit;
end;

//=== { TJclRpmDecompressArchive } ===========================================

function TJclRpmDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatRpm;
end;

//=== { TJclDebDecompressArchive } ===========================================

function TJclDebDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatDeb;
end;

//=== { TJclCpioDecompressArchive } ==========================================

function TJclCpioDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatCpio;
end;

//=== { TJclTarDecompressArchive } ===========================================

function TJclTarDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatTar;
end;

//=== { TJclGZipDecompressArchive } ==========================================

function TJclGZipDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatGZip;
end;
{$ENDIF MSWINDOWS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.

