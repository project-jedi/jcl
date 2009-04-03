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
{   Jan Goyvaerts (jgsoft)                                                                         }
{   Uwe Schuster (uschuster)                                                                       }
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
  Windows, Sevenzip,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Types,
  {$ENDIF UNIX}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  {$IFNDEF SUPPORTS_UNICODE}
  JclWideStrings,
  {$ENDIF ~SUPPORTS_UNICODE}
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
   |         |-- TJclZipDecompressArchive      handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclBZ2DecompressArchive      handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclRarDecompressArchive      handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclArjDecompressArchive      handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclZDecompressArchive        handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclLzhDecompressArchive      handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJcl7zDecompressArchive       handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclCabDecompressArchive      handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclNsisDecompressArchive     handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclLzmaDecompressArchive     handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclPeDecompressArchive       handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclElfDecompressArchive      handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclMachoDecompressArchive    handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclUdfDecompressArchive      handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclXarDecompressArchive      handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclMubDecompressArchive      handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclHfsDecompressArchive      handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclDmgDecompressArchive      handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclCompoundDecompressArchive handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclWimDecompressArchive      handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclIsoDecompressArchive      handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclChmDecompressArchive      handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclSplitDecompressArchive    handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclRpmDecompressArchive      handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclDebDecompressArchive      handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclCpioDecompressArchive     handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclTarDecompressArchive      handled by sevenzip http://sevenzip.sourceforge.net/
   |         |-- TJclGZipDecompressArchive     handled by sevenzip http://sevenzip.sourceforge.net/
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
    class function StreamName: string; virtual;
    class function StreamExtensions: string; virtual;

    constructor Create(Stream: TStream);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure Reset; virtual;
  end;

  TJclCompressionStreamClass = class of TJclCompressionStream;

  TJclCompressStream = class(TJclCompressionStream)
  public
    function Flush: Integer; dynamic; abstract;
    constructor Create(Destination: TStream);
  end;

  TJclCompressStreamClass = class of TJclCompressStream;

  TJclDecompressStream = class(TJclCompressionStream)
  private
    FOwnsStream: Boolean;
  public
    constructor Create(Source: TStream; AOwnsStream: Boolean = False);
    destructor Destroy; override;
  end;

  TJclDecompressStreamClass = class of TJclDecompressStream;

  TJclCompressionStreamFormats = class
  private
    FCompressFormats: TList;
    FDecompressFormats: TList;
  protected
    function GetCompressFormatCount: Integer;
    function GetCompressFormat(Index: Integer): TJclCompressStreamClass;
    function GetDecompressFormatCount: Integer;
    function GetDecompressFormat(Index: Integer): TJclDecompressStreamClass;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterFormat(AClass: TJclCompressionStreamClass);
    procedure UnregisterFormat(AClass: TJclCompressionStreamClass);

    function FindCompressFormat(const AFileName: TFileName): TJclCompressStreamClass;
    function FindDecompressFormat(const AFileName: TFileName): TJclDecompressStreamClass;

    property CompressFormatCount: Integer read GetCompressFormatCount;
    property CompressFormats[Index: Integer]: TJclCompressStreamClass read GetCompressFormat;
    property DecompressFormatCount: Integer read GetDecompressFormatCount;
    property DecompressFormats[Index: Integer]: TJclDecompressStreamClass read GetDecompressFormat;
  end;

// retreive a singleton list containing registered stream classes
function GetStreamFormats: TJclCompressionStreamFormats;

// ZIP Support
type
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
    // stream description
    class function StreamName: string; override;
    class function StreamExtensions: string; override;

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
    // stream description
    class function StreamName: string; override;
    class function StreamExtensions: string; override;

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
    FZLibStream: TJclZLibCompressStream;
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
    // stream description
    class function StreamName: string; override;
    class function StreamExtensions: string; override;

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
    // stream description
    class function StreamName: string; override;
    class function StreamExtensions: string; override;

    constructor Create(Source: TStream; CheckHeaderCRC: Boolean = True; AOwnsStream: Boolean = False);
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
    // stream description
    class function StreamName: string; override;
    class function StreamExtensions: string; override;

    constructor Create(Destination: TStream; CompressionLevel: TJclCompressionLevel = -1);
    destructor Destroy; override;

    function Flush: Integer; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Write(const Buffer; Count: Longint): Longint; override;

    property CompressionLevel: Integer read FCompressionLevel write SetCompressionLevel;
  end;

  TJclBZIP2DecompressionStream = class(TJclDecompressStream)
  private
    FInflateInitialized: Boolean;
  protected
    BZLibRecord: bz_stream;
  public
    // stream description
    class function StreamName: string; override;
    class function StreamExtensions: string; override;

    constructor Create(Source: TStream; AOwnsStream: Boolean = False); overload;
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  EJclCompressionError = class(EJclError);

  // callback type used in helper functions below:
  TJclCompressStreamProgressCallback = procedure(FileSize, Position: Int64; UserData: Pointer) of object;

{helper functions - one liners by wpostma}
function GZipFile(SourceFile, DestinationFile: TFileName; CompressionLevel: Integer = Z_DEFAULT_COMPRESSION;
  ProgressCallback: TJclCompressStreamProgressCallback = nil; UserData: Pointer = nil): Boolean;
function UnGZipFile(SourceFile, DestinationFile: TFileName;
  ProgressCallback: TJclCompressStreamProgressCallback = nil; UserData: Pointer = nil): Boolean;
procedure GZipStream(SourceStream, DestinationStream: TStream; CompressionLevel: Integer = Z_DEFAULT_COMPRESSION;
  ProgressCallback: TJclCompressStreamProgressCallback = nil; UserData: Pointer = nil);
procedure UnGZipStream(SourceStream, DestinationStream: TStream;
  ProgressCallback: TJclCompressStreamProgressCallback = nil; UserData: Pointer = nil);

function BZip2File(SourceFile, DestinationFile: TFileName; CompressionLevel: Integer = 5;
  ProgressCallback: TJclCompressStreamProgressCallback = nil; UserData: Pointer = nil): Boolean;
function UnBZip2File(SourceFile, DestinationFile: TFileName;
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

  TJclCompressionItemProperty = (ipPackedName, ipPackedSize, ipPackedExtension,
    ipFileSize, ipFileName, ipAttributes, ipCreationTime, ipLastAccessTime,
    ipLastWriteTime, ipComment, ipHostOS, ipHostFS, ipUser, ipGroup, ipCRC,
    ipStream, ipMethod);
  TJclCompressionItemProperties = set of TJclCompressionItemProperty;

  TJclCompressionItemKind = (ikFile, ikDirectory);

  TJclCompressionOperationSuccess = (osNoOperation, osOK, osUnsupportedMethod,
    osDataError, osCRCError, osUnknownError);

  TJclCompressionDuplicateCheck = (dcNone, dcExisting, dcAll);
  TJclCompressionDuplicateAction = (daOverwrite, daError, daSkip);

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
    FPackedIndex: Cardinal;
    FSelected: Boolean;
    FOperationSuccess: TJclCompressionOperationSuccess;
    // file properties
    FPackedName: WideString;
    FPackedSize: Int64;
    FFileSize: Int64;
    FAttributes: Cardinal;
    FPackedExtension: WideString;
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
    function ValidateExtraction(Index: Integer): Boolean; virtual;
    function DeleteOutputFile: Boolean;
    function UpdateFileTimes: Boolean;
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
    function GetPackedExtension: WideString;
    function GetPackedName: WideString;
    function GetPackedSize: Int64;
    function GetStream: TStream;
    function GetUser: WideString;
    // property setters
    procedure SetAttributes(Value: Cardinal);
    procedure SetComment(const Value: WideString);
    procedure SetCRC(Value: Cardinal);
    procedure SetCreationTime(const Value: TFileTime);
    procedure SetFileName(const Value: TFileName);
    procedure SetFileSize(const Value: Int64);
    procedure SetGroup(const Value: WideString);
    procedure SetHostFS(const Value: WideString);
    procedure SetHostOS(const Value: WideString);
    procedure SetLastAccessTime(const Value: TFileTime);
    procedure SetLastWriteTime(const Value: TFileTime);
    procedure SetMethod(const Value: WideString);
    procedure SetPackedExtension(const Value: WideString);
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
    property PackedExtension: WideString read GetPackedExtension write SetPackedExtension;
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
    property PackedIndex: Cardinal read FPackedIndex;
    property Selected: Boolean read FSelected write FSelected;
  end;

  TJclCompressionItemClass = class of TJclCompressionItem;

  TJclCompressionVolume = class
  protected
    FFileName: TFileName;
    FTmpFileName: TFileName;
    FStream: TStream;
    FTmpStream: TStream;
    FOwnsStream: Boolean;
    FOwnsTmpStream: Boolean;
    FVolumeMaxSize: Int64;
  public
    constructor Create(AStream, ATmpStream: TStream; AOwnsStream, AOwnsTmpStream: Boolean;
      AFileName, ATmpFileName: TFileName; AVolumeMaxSize: Int64);
    destructor Destroy; override;
    procedure ReleaseStreams;
    property FileName: TFileName read FFileName;
    property TmpFileName: TFileName read FTmpFileName;
    property Stream: TStream read FStream;
    property TmpStream: TStream read FTmpStream;
    property OwnsStream: Boolean read FOwnsStream;
    property OwnsTmpStream: Boolean read FOwnsTmpStream;
    property VolumeMaxSize: Int64 read FVolumeMaxSize;
  end;

  TJclStreamAccess = (saCreate, saReadOnly, saReadOnlyDenyNone, saWriteOnly, saReadWrite);

  { TJclCompressionArchive is not ref-counted }
  TJclCompressionArchive = class(TObject, IInterface)
  private
    FOnProgress: TJclCompressionProgressEvent;
    FOnVolume: TJclCompressionVolumeEvent;
    FOnVolumeMaxSize: TJclCompressionVolumeMaxSizeEvent;
    FPassword: WideString;
    FVolumeIndex: Integer;
    FVolumeIndexOffset: Integer;
    FVolumeMaxSize: Int64;
    FVolumeFileNameMask: TFileName;
    FProgressMax: Int64;
    function GetItemCount: Integer;
    function GetItem(Index: Integer): TJclCompressionItem;
    function GetVolumeCount: Integer;
    function GetVolume(Index: Integer): TJclCompressionVolume;
  protected
    FVolumes: TObjectList;
    FItems: TObjectList;

    procedure CreateCompressionObject; virtual;
    procedure FreeCompressionObject; virtual;

    function InternalOpenStream(const FileName: TFileName): TStream;
    function TranslateItemPath(const ItemPath, OldBase, NewBase: WideString): WideString;

    procedure DoProgress(const Value, MaxValue: Int64);
    function NeedStream(Index: Integer): TStream;
    function NeedStreamMaxSize(Index: Integer): Int64;
    procedure ReleaseVolumes;
    function GetItemClass: TJclCompressionItemClass; virtual; abstract;
  public
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    class function MultipleItemContainer: Boolean; virtual;
    class function VolumeAccess: TJclStreamAccess; virtual;
    class function ItemAccess: TJclStreamAccess; virtual;
    class function ArchiveExtensions: string; virtual;
    class function ArchiveName: string; virtual;

    constructor Create(Volume0: TStream; AVolumeMaxSize: Int64 = 0;
      AOwnVolume: Boolean = False); overload; virtual;
    constructor Create(const VolumeFileName: TFileName; AVolumeMaxSize: Int64 = 0;
      VolumeMask: Boolean = False); overload; virtual;
      // if VolumeMask is true then VolumeFileName represents a mask to get volume file names
      // "myfile%d.zip" "myfile.zip.%.3d" ...
    destructor Destroy; override;

    function AddVolume(const VolumeFileName: TFileName;
      AVolumeMaxSize: Int64 = 0): Integer; overload; virtual;
    function AddVolume(const VolumeFileName, TmpVolumeFileName: TFileName;
      AVolumeMaxSize: Int64 = 0): Integer; overload; virtual;
    function AddVolume(VolumeStream: TStream; AVolumeMaxSize: Int64 = 0;
      AOwnsStream: Boolean = False): Integer; overload; virtual;
    function AddVolume(VolumeStream, TmpVolumeStream: TStream; AVolumeMaxSize: Int64 = 0;
      AOwnsStream: Boolean = False; AOwnsTmpStream: Boolean = False): Integer; overload; virtual;

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
    property VolumeFileNameMask: TFileName read FVolumeFileNameMask;
    property VolumeIndexOffset: Integer read FVolumeIndexOffset write FVolumeIndexOffset;

    property OnProgress: TJclCompressionProgressEvent read FOnProgress write FOnProgress;

    // volume events
    property OnVolume: TJclCompressionVolumeEvent read FOnVolume write FOnVolume;
    property OnVolumeMaxSize: TJclCompressionVolumeMaxSizeEvent read FOnVolumeMaxSize
      write FOnVolumeMaxSize;
    property Password: WideString read FPassword write FPassword;
  end;

  TJclCompressionArchiveClass = class of TJclCompressionArchive;

  IJclArchiveNumberOfThreads = interface
    ['{9CFAB801-E68E-4A51-AC49-277B297F1141}']
    function GetNumberOfThreads: Cardinal;
    procedure SetNumberOfThreads(Value: Cardinal);
    property NumberOfThreads: Cardinal read GetNumberOfThreads write SetNumberOfThreads;
  end;

  IJclArchiveCompressionLevel = interface
    ['{A6A2F55F-2860-4E44-BC20-8C5D3E322AB6}']
    function GetCompressionLevel: Cardinal;
    function GetCompressionLevelMax: Cardinal;
    function GetCompressionLevelMin: Cardinal;
    procedure SetCompressionLevel(Value: Cardinal);
    property CompressionLevel: Cardinal read GetCompressionLevel write SetCompressionLevel;
    property CompressionLevelMax: Cardinal read GetCompressionLevelMax;
    property CompressionLevelMin: Cardinal read GetCompressionLevelMin;
  end;

  TJclCompressionMethod = (cmCopy, cmDeflate, cmDeflate64, cmBZip2, cmLZMA, cmLZMA2, cmPPMd);
  TJclCompressionMethods = set of TJclCompressionMethod;

  IJclArchiveCompressionMethod = interface
    ['{2818F8E8-7D5F-4C8C-865E-9BA4512BB766}']
    function GetCompressionMethod: TJclCompressionMethod;
    function GetSupportedCompressionMethods: TJclCompressionMethods;
    procedure SetCompressionMethod(Value: TJclCompressionMethod);
    property CompressionMethod: TJclCompressionMethod read GetCompressionMethod write SetCompressionMethod;
    property SupportedCompressionMethods: TJclCompressionMethods read GetSupportedCompressionMethods;
  end;

  TJclEncryptionMethod = (emNone, emAES128, emAES192, emAES256, emZipCrypto);
  TJclEncryptionMethods = set of TJclEncryptionMethod;

  IJclArchiveEncryptionMethod = interface
    ['{643485B6-66A1-41C9-A13B-0A8453E9D0C9}']
    function GetEncryptionMethod: TJclEncryptionMethod;
    function GetSupportedEncryptionMethods: TJclEncryptionMethods;
    procedure SetEncryptionMethod(Value: TJclEncryptionMethod);
    property EncryptionMethod: TJclEncryptionMethod read GetEncryptionMethod write SetEncryptionMethod;
    property SupportedEncryptionMethods: TJclEncryptionMethods read GetSupportedEncryptionMethods;
  end;

  IJclArchiveDictionarySize = interface
    ['{D3949834-9F3B-49BC-8403-FE3CE5FDCF35}']
    function GetDictionarySize: Cardinal;
    procedure SetDictionarySize(Value: Cardinal);
    property DictionarySize: Cardinal read GetDictionarySize write SetDictionarySize;
  end;

  IJclArchiveNumberOfPasses = interface
    ['{C61B2814-50CE-4C3C-84A5-BACF8A57E3BC}']
    function GetNumberOfPasses: Cardinal;
    procedure SetNumberOfPasses(Value: Cardinal);
    property NumberOfPasses: Cardinal read GetNumberOfPasses write SetNumberOfPasses;
  end;

  IJclArchiveRemoveSfxBlock = interface
    ['{852D050D-734E-4610-902A-8FB845DB32A9}']
    function GetRemoveSfxBlock: Boolean;
    procedure SetRemoveSfxBlock(Value: Boolean);
    property RemoveSfxBlock: Boolean read GetRemoveSfxBlock write SetRemoveSfxBlock;
  end;

  IJclArchiveCompressHeader = interface
    ['{22C62A3B-A58E-4F88-9D3F-08586B542639}']
    function GetCompressHeader: Boolean;
    function GetCompressHeaderFull: Boolean;
    procedure SetCompressHeader(Value: Boolean);
    procedure SetCompressHeaderFull(Value: Boolean);
    property CompressHeader: Boolean read GetCompressHeader write SetCompressHeader;
    property CompressHeaderFull: Boolean read GetCompressHeaderFull write SetCompressHeaderFull;
  end;

  IJclArchiveEncryptHeader = interface
    ['{7DBA20A8-48A1-4CA2-B9AC-41C219A09A4A}']
    function GetEncryptHeader: Boolean;
    procedure SetEncryptHeader(Value: Boolean);
    property EncryptHeader: Boolean read GetEncryptHeader write SetEncryptHeader;
  end;

  IJclArchiveSaveCreationDateTime = interface
    ['{8B212BF9-C13F-4582-A4FA-A40E538EFF65}']
    function GetSaveCreationDateTime: Boolean;
    procedure SetSaveCreationDateTime(Value: Boolean);
    property SaveCreationDateTime: Boolean read GetSaveCreationDateTime write SetSaveCreationDateTime;
  end;

  IJclArchiveSaveLastAccessDateTime = interface
    ['{1A4B2906-9DD2-4584-B7A3-3639DA92AFC5}']
    function GetSaveLastAccessDateTime: Boolean;
    procedure SetSaveLastAccessDateTime(Value: Boolean);
    property SaveLastAccessDateTime: Boolean read GetSaveLastAccessDateTime write SetSaveLastAccessDateTime;
  end;

  IJclArchiveSaveLastWriteDateTime = interface
    ['{0C1729DC-35E8-43D4-8ECA-54F20CDFF87A}']
    function GetSaveLastWriteDateTime: Boolean;
    procedure SetSaveLastWriteDateTime(Value: Boolean);
    property SaveLastWriteDateTime: Boolean read GetSaveLastWriteDateTime write SetSaveLastWriteDateTime;
  end;

  IJclArchiveAlgorithm = interface
    ['{53965F1F-24CC-4548-B9E8-5AE2EB7F142D}']
    function GetAlgorithm: Cardinal;
    function GetSupportedAlgorithms: TDynCardinalArray;
    procedure SetAlgorithm(Value: Cardinal);
    property Algorithm: Cardinal read GetAlgorithm write SetAlgorithm;
    property SupportedAlgorithms: TDynCardinalArray read GetSupportedAlgorithms;
  end;

  IJclArchiveSolid = interface
    ['{6902C54C-1577-422C-B18B-E27953A28661}']
    function GetSolidBlockSize: Int64;
    function GetSolidExtension: Boolean;
    procedure SetSolidBlockSize(const Value: Int64);
    procedure SetSolidExtension(Value: Boolean);
    property SolidBlockSize: Int64 read GetSolidBlockSize write SetSolidBlockSize;
    property SolidExtension: Boolean read GetSolidExtension write SetSolidExtension;
  end;

  TJclCompressItem = class(TJclCompressionItem)
  protected
    procedure CheckGetProperty(AProperty: TJclCompressionItemProperty); override;
    procedure CheckSetProperty(AProperty: TJclCompressionItemProperty); override;
  end;

  TJclCompressArchive = class(TJclCompressionArchive, IInterface)
  private
    FBaseRelName: WideString;
    FBaseDirName: string;
    FAddFilesInDir: Boolean;
    FDuplicateAction: TJclCompressionDuplicateAction;
    FDuplicateCheck: TJclCompressionDuplicateCheck;
    procedure InternalAddFile(const Directory: string; const FileInfo: TSearchRec);
    procedure InternalAddDirectory(const Directory: string);
  protected
    FCompressing: Boolean;
    FPackedNames: {$IFDEF SUPPORTS_UNICODE}TStringList{$ELSE}TWStringList{$ENDIF};
    procedure CheckNotCompressing;
    function AddFileCheckDuplicate(NewItem: TJclCompressionItem): Integer;
  public
    class function VolumeAccess: TJclStreamAccess; override;
    class function ItemAccess: TJclStreamAccess; override;

    function AddDirectory(const PackedName: WideString;
      const DirName: string = ''; RecurseIntoDir: Boolean = False;
      AddFilesInDir: Boolean = False): Integer; overload; virtual;
    function AddFile(const PackedName: WideString;
      const FileName: TFileName): Integer; overload; virtual;
    function AddFile(const PackedName: WideString; AStream: TStream;
      AOwnsStream: Boolean = False): Integer; overload; virtual;
    procedure Compress; virtual;

    property DuplicateCheck: TJclCompressionDuplicateCheck read FDuplicateCheck write FDuplicateCheck;
    property DuplicateAction: TJclCompressionDuplicateAction read FDuplicateAction write FDuplicateAction;
  end;

  TJclCompressArchiveClass = class of TJclCompressArchive;

  TJclDecompressItem = class(TJclCompressionItem)
  protected
    procedure CheckGetProperty(AProperty: TJclCompressionItemProperty); override;
    procedure CheckSetProperty(AProperty: TJclCompressionItemProperty); override;
    function ValidateExtraction(Index: Integer): Boolean; override;
  end;

  // return False not to extract this file
  // assign your own FileName, Stream or AOwnsStream to override default one
  TJclCompressionExtractEvent = function (Sender: TObject; Index: Integer;
    var FileName: TFileName; var Stream: TStream; var AOwnsStream: Boolean): Boolean of object;

  TJclDecompressArchive = class(TJclCompressionArchive, IInterface)
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

    function ValidateExtraction(Index: Integer; var FileName: TFileName; var AStream: TStream;
      var AOwnsStream: Boolean): Boolean; virtual;
  public
    class function VolumeAccess: TJclStreamAccess; override;
    class function ItemAccess: TJclStreamAccess; override;

    procedure ListFiles; virtual; abstract;
    procedure ExtractSelected(const ADestinationDir: string = '';
      AAutoCreateSubDir: Boolean = True); virtual;
    procedure ExtractAll(const ADestinationDir: string = '';
      AAutoCreateSubDir: Boolean = True); virtual;

    property OnExtract: TJclCompressionExtractEvent read FOnExtract write FOnExtract;
    property DestinationDir: string read FDestinationDir;
    property AutoCreateSubDir: Boolean read FAutoCreateSubDir;
  end;

  TJclDecompressArchiveClass = class of TJclDecompressArchive;

  TJclUpdateItem = class(TJclCompressionItem)
  protected
    procedure CheckGetProperty(AProperty: TJclCompressionItemProperty); override;
    procedure CheckSetProperty(AProperty: TJclCompressionItemProperty); override;
    function ValidateExtraction(Index: Integer): Boolean; override;
  end;

  TJclUpdateArchive = class(TJclCompressArchive, IInterface)
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

    function ValidateExtraction(Index: Integer; var FileName: TFileName; var AStream: TStream;
      var AOwnsStream: Boolean): Boolean; virtual;
  public
    constructor Create(Volume0: TStream; AVolumeMaxSize: Int64 = 0;
      AOwnVolume: Boolean = False); overload; override;
    constructor Create(const VolumeFileName: TFileName; AVolumeMaxSize: Int64 = 0;
      VolumeMask: Boolean = False); overload; override;
    class function VolumeAccess: TJclStreamAccess; override;
    class function ItemAccess: TJclStreamAccess; override;

    procedure ListFiles; virtual; abstract;
    procedure ExtractSelected(const ADestinationDir: string = '';
      AAutoCreateSubDir: Boolean = True); virtual;
    procedure ExtractAll(const ADestinationDir: string = '';
      AAutoCreateSubDir: Boolean = True); virtual;
    procedure DeleteItem(Index: Integer); virtual; abstract;
    procedure RemoveItem(const PackedName: WideString); virtual; abstract;

    property OnExtract: TJclCompressionExtractEvent read FOnExtract write FOnExtract;
    property DestinationDir: string read FDestinationDir;
    property AutoCreateSubDir: Boolean read FAutoCreateSubDir;
  end;

  // ancestor class for all archives that update files in-place (not creating a copy of the volumes)
  TJclInPlaceUpdateArchive = class(TJclUpdateArchive, IInterface)
  end;

  // called when tmp volumes will replace volumes after out-of-place update
  TJclCompressionReplaceEvent = function (Sender: TObject; const SrcFileName, DestFileName: TFileName;
    var SrcStream, DestStream: TStream; var OwnsSrcStream, OwnsDestStream: Boolean): Boolean;

  // ancestor class for all archives that update files out-of-place (by creating a copy of the volumes)
  TJclOutOfPlaceUpdateArchive = class(TJclUpdateArchive, IInterface)
  private
    FReplaceVolumes: Boolean;
    FTmpVolumeIndex: Integer;
    FOnReplace: TJclCompressionReplaceEvent;
    FOnTmpVolume: TJclCompressionVolumeEvent;
  protected
    function NeedTmpStream(Index: Integer): TStream;
    function InternalOpenTmpStream(const FileName: TFileName): TStream;
  public
    class function TmpVolumeAccess: TJclStreamAccess; virtual;

    constructor Create(Volume0: TStream; AVolumeMaxSize: Int64 = 0;
      AOwnVolume: Boolean = False); overload; override;
    constructor Create(const VolumeFileName: TFileName; AVolumeMaxSize: Int64 = 0;
      VolumeMask: Boolean = False); overload; override;

    procedure Compress; override;

    property ReplaceVolumes: Boolean read FReplaceVolumes write FReplaceVolumes;
    property OnReplace: TJclCompressionReplaceEvent read FOnReplace write FOnReplace;
    property OnTmpVolume: TJclCompressionVolumeEvent read FOnTmpVolume write FOnTmpVolume;
  end;

  TJclUpdateArchiveClass = class of TJclUpdateArchive;

// registered archive formats
type
  TJclCompressionArchiveFormats = class
  private
    FCompressFormats: TList;
    FDecompressFormats: TList;
    FUpdateFormats: TList;
  protected
    function GetCompressFormatCount: Integer;
    function GetCompressFormat(Index: Integer): TJclCompressArchiveClass;
    function GetDecompressFormatCount: Integer;
    function GetDecompressFormat(Index: Integer): TJclDecompressArchiveClass;
    function GetUpdateFormatCount: Integer;
    function GetUpdateFormat(Index: Integer): TJclUpdateArchiveClass;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterFormat(AClass: TJclCompressionArchiveClass);
    procedure UnregisterFormat(AClass: TJclCompressionArchiveClass);

    function FindCompressFormat(const AFileName: TFileName): TJclCompressArchiveClass;
    function FindDecompressFormat(const AFileName: TFileName): TJclDecompressArchiveClass;
    function FindUpdateFormat(const AFileName: TFileName): TJclUpdateArchiveClass;

    property CompressFormatCount: Integer read GetCompressFormatCount;
    property CompressFormats[Index: Integer]: TJclCompressArchiveClass read GetCompressFormat;
    property DecompressFormatCount: Integer read GetDecompressFormatCount;
    property DecompressFormats[Index: Integer]: TJclDecompressArchiveClass read GetDecompressFormat;
    property UpdateFormatCount: Integer read GetUpdateFormatCount;
    property UpdateFormats[Index: Integer]: TJclUpdateArchiveClass read GetUpdateFormat;
  end;

// retreive a singleton list containing archive formats
function GetArchiveFormats: TJclCompressionArchiveFormats;

// sevenzip classes for compression
type
  TJclSevenzipCompressArchive = class(TJclCompressArchive, IInterface)
  private
    FOutArchive: IOutArchive;
  protected
    procedure CreateCompressionObject; override;
    procedure FreeCompressionObject; override;
    function GetCLSID: TGUID; virtual; abstract;
    function GetItemClass: TJclCompressionItemClass; override;
  public
    procedure Compress; override;
  end;

  // file formats

  TJclZipCompressArchive = class(TJclSevenzipCompressArchive, IJclArchiveCompressionLevel, IJclArchiveCompressionMethod,
    IJclArchiveEncryptionMethod, IJclArchiveDictionarySize, IJclArchiveNumberOfPasses, IJclArchiveNumberOfThreads,
    IJclArchiveAlgorithm, IInterface)
  private
    FNumberOfThreads: Cardinal;
    FEncryptionMethod: TJclEncryptionMethod;
    FDictionarySize: Cardinal;
    FCompressionLevel: Cardinal;
    FCompressionMethod: TJclCompressionMethod;
    FNumberOfPasses: Cardinal;
    FAlgorithm: Cardinal;
  protected
    function GetCLSID: TGUID; override;
    procedure CreateCompressionObject; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
    { IJclArchiveNumberOfThreads }
    function GetNumberOfThreads: Cardinal;
    procedure SetNumberOfThreads(Value: Cardinal);
    { IJclArchiveEncryptionMethod }
    function GetEncryptionMethod: TJclEncryptionMethod;
    function GetSupportedEncryptionMethods: TJclEncryptionMethods;
    procedure SetEncryptionMethod(Value: TJclEncryptionMethod);
    { IJclArchiveDictionarySize }
    function GetDictionarySize: Cardinal;
    procedure SetDictionarySize(Value: Cardinal);
    { IJclArchiveCompressionLevel }
    function GetCompressionLevel: Cardinal;
    function GetCompressionLevelMax: Cardinal;
    function GetCompressionLevelMin: Cardinal;
    procedure SetCompressionLevel(Value: Cardinal);
    { IJclArchiveCompressionMethod }
    function GetCompressionMethod: TJclCompressionMethod;
    function GetSupportedCompressionMethods: TJclCompressionMethods;
    procedure SetCompressionMethod(Value: TJclCompressionMethod);
    { IJclArchiveNumberOfPasses }
    function GetNumberOfPasses: Cardinal;
    procedure SetNumberOfPasses(Value: Cardinal);
    { IJclArchiveAlgoritm }
    function GetAlgorithm: Cardinal;
    function GetSupportedAlgorithms: TDynCardinalArray;
    procedure SetAlgorithm(Value: Cardinal);
  end;

  TJclBZ2CompressArchive = class(TJclSevenzipCompressArchive, IJclArchiveCompressionLevel, IJclArchiveDictionarySize,
    IJclArchiveNumberOfPasses, IJclArchiveNumberOfThreads, IInterface)
  private
    FNumberOfThreads: Cardinal;
    FDictionarySize: Cardinal;
    FCompressionLevel: Cardinal;
    FNumberOfPasses: Cardinal;
  protected
    function GetCLSID: TGUID; override;
    procedure CreateCompressionObject; override;
  public
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
    { IJclArchiveNumberOfThreads }
    function GetNumberOfThreads: Cardinal;
    procedure SetNumberOfThreads(Value: Cardinal);
    { IJclArchiveDictionarySize }
    function GetDictionarySize: Cardinal;
    procedure SetDictionarySize(Value: Cardinal);
    { IJclArchiveCompressionLevel }
    function GetCompressionLevel: Cardinal;
    function GetCompressionLevelMax: Cardinal;
    function GetCompressionLevelMin: Cardinal;
    procedure SetCompressionLevel(Value: Cardinal);
    { IJclArchiveNumberOfPasses }
    function GetNumberOfPasses: Cardinal;
    procedure SetNumberOfPasses(Value: Cardinal);
  end;

  TJcl7zCompressArchive = class(TJclSevenzipCompressArchive, IJclArchiveCompressionLevel, IJclArchiveDictionarySize,
    IJclArchiveNumberOfThreads, IJclArchiveRemoveSfxBlock, IJclArchiveCompressHeader, IJclArchiveEncryptHeader,
    IJclArchiveSaveCreationDateTime, IJclArchiveSaveLastAccessDateTime, IJclArchiveSaveLastWriteDateTime,
    IJclArchiveSolid, IInterface)
  private
    FNumberOfThreads: Cardinal;
    FEncryptHeader: Boolean;
    FRemoveSfxBlock: Boolean;
    FDictionarySize: Cardinal;
    FCompressionLevel: Cardinal;
    FCompressHeader: Boolean;
    FCompressHeaderFull: Boolean;
    FSaveLastAccessDateTime: Boolean;
    FSaveCreationDateTime: Boolean;
    FSaveLastWriteDateTime: Boolean;
    FSolidBlockSize: Int64;
    FSolidExtension: Boolean;
  protected
    function GetCLSID: TGUID; override;
    procedure CreateCompressionObject; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
    { IJclArchiveNumberOfThreads }
    function GetNumberOfThreads: Cardinal;
    procedure SetNumberOfThreads(Value: Cardinal);
    { IJclArchiveEncryptHeader }
    function GetEncryptHeader: Boolean;
    procedure SetEncryptHeader(Value: Boolean);
    { IJclArchiveRemoveSfxBlock }
    function GetRemoveSfxBlock: Boolean;
    procedure SetRemoveSfxBlock(Value: Boolean);
    { IJclArchiveDictionarySize }
    function GetDictionarySize: Cardinal;
    procedure SetDictionarySize(Value: Cardinal);
    { IJclArchiveCompressionLevel }
    function GetCompressionLevel: Cardinal;
    function GetCompressionLevelMax: Cardinal;
    function GetCompressionLevelMin: Cardinal;
    procedure SetCompressionLevel(Value: Cardinal);
    { IJclArchiveCompressHeader }
    function GetCompressHeader: Boolean;
    function GetCompressHeaderFull: Boolean;
    procedure SetCompressHeader(Value: Boolean);
    procedure SetCompressHeaderFull(Value: Boolean);
    { IJclArchiveSaveLastAccessDateTime }
    function GetSaveLastAccessDateTime: Boolean;
    procedure SetSaveLastAccessDateTime(Value: Boolean);
    { IJclArchiveSaveCreationDateTime }
    function GetSaveCreationDateTime: Boolean;
    procedure SetSaveCreationDateTime(Value: Boolean);
    { IJclArchiveSaveLastWriteDateTime }
    function GetSaveLastWriteDateTime: Boolean;
    procedure SetSaveLastWriteDateTime(Value: Boolean);
    { IJclArchiveSolid }
    function GetSolidBlockSize: Int64;
    function GetSolidExtension: Boolean;
    procedure SetSolidBlockSize(const Value: Int64);
    procedure SetSolidExtension(Value: Boolean);
  end;

  TJclTarCompressArchive = class(TJclSevenzipCompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclGZipCompressArchive = class(TJclSevenzipCompressArchive, IJclArchiveCompressionLevel, IJclArchiveNumberOfPasses,
    IJclArchiveAlgorithm, IInterface)
  private
    FCompressionLevel: Cardinal;
    FNumberOfPasses: Cardinal;
    FAlgorithm: Cardinal;
  protected
    function GetCLSID: TGUID; override;
    procedure CreateCompressionObject; override;
  public
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
    { IJclArchiveCompressionLevel }
    function GetCompressionLevel: Cardinal;
    function GetCompressionLevelMax: Cardinal;
    function GetCompressionLevelMin: Cardinal;
    procedure SetCompressionLevel(Value: Cardinal);
    { IJclArchiveNumberOfPasses }
    function GetNumberOfPasses: Cardinal;
    procedure SetNumberOfPasses(Value: Cardinal);
    { IJclArchiveAlgorithm }
    function GetAlgorithm: Cardinal;
    function GetSupportedAlgorithms: TDynCardinalArray;
    procedure SetAlgorithm(Value: Cardinal);
  end;

// sevenzip classes for decompression
type
  TJclSevenzipDecompressArchive = class(TJclDecompressArchive, IInterface)
  private
    FInArchive: IInArchive;
    FOpened: Boolean;
  protected
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

  TJclZipDecompressArchive = class(TJclSevenzipDecompressArchive, IJclArchiveNumberOfThreads, IInterface)
  private
    FNumberOfThreads: Cardinal;
  protected
    function GetCLSID: TGUID; override;
    procedure CreateCompressionObject; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
    { IJclArchiveNumberOfThreads }
    function GetNumberOfThreads: Cardinal;
    procedure SetNumberOfThreads(Value: Cardinal);
  end;

  TJclBZ2DecompressArchive = class(TJclSevenzipDecompressArchive, IJclArchiveNumberOfThreads, IInterface)
  private
    FNumberOfThreads: Cardinal;
  protected
    function GetCLSID: TGUID; override;
    procedure CreateCompressionObject; override;
  public
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
    { IJclArchiveNumberOfThreads }
    function GetNumberOfThreads: Cardinal;
    procedure SetNumberOfThreads(Value: Cardinal);
  end;

  TJclRarDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclArjDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclZDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclLzhDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJcl7zDecompressArchive = class(TJclSevenzipDecompressArchive, IJclArchiveNumberOfThreads, IInterface)
  private
    FNumberOfThreads: Cardinal;
  protected
    function GetCLSID: TGUID; override;
    procedure CreateCompressionObject; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
    { IJclArchiveNumberOfThreads }
    function GetNumberOfThreads: Cardinal;
    procedure SetNumberOfThreads(Value: Cardinal);
  end;

  TJclCabDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclNsisDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclLzmaDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclPeDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclElfDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclMachoDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclUdfDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclXarDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclMubDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclHfsDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclDmgDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclCompoundDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclWimDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclIsoDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  {TJclBkfDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;}

  TJclChmDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclSplitDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclRpmDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclDebDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclCpioDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclTarDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclGZipDecompressArchive = class(TJclSevenzipDecompressArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

//sevenzip classes for updates (read and write)
type
  TJclSevenzipUpdateArchive = class(TJclOutOfPlaceUpdateArchive, IInterface)
  private
    FInArchive: IInArchive;
    FOutArchive: IOutArchive;
    FOpened: Boolean;
  protected
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
    procedure Compress; override;
    procedure DeleteItem(Index: Integer); override;
    procedure RemoveItem(const PackedName: WideString); override;
  end;

  TJclZipUpdateArchive = class(TJclSevenzipUpdateArchive, IJclArchiveCompressionLevel, IJclArchiveCompressionMethod,
    IJclArchiveEncryptionMethod, IJclArchiveDictionarySize, IJclArchiveNumberOfPasses, IJclArchiveNumberOfThreads,
    IJclArchiveAlgorithm, IInterface)
  private
    FNumberOfThreads: Cardinal;
    FEncryptionMethod: TJclEncryptionMethod;
    FDictionarySize: Cardinal;
    FCompressionLevel: Cardinal;
    FCompressionMethod: TJclCompressionMethod;
    FNumberOfPasses: Cardinal;
    FAlgorithm: Cardinal;
  protected
    function GetCLSID: TGUID; override;
    procedure CreateCompressionObject; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
    { IJclArchiveNumberOfThreads }
    function GetNumberOfThreads: Cardinal;
    procedure SetNumberOfThreads(Value: Cardinal);
    { IJclArchiveEncryptionMethod }
    function GetEncryptionMethod: TJclEncryptionMethod;
    function GetSupportedEncryptionMethods: TJclEncryptionMethods;
    procedure SetEncryptionMethod(Value: TJclEncryptionMethod);
    { IJclArchiveDictionarySize }
    function GetDictionarySize: Cardinal;
    procedure SetDictionarySize(Value: Cardinal);
    { IJclArchiveCompressionLevel }
    function GetCompressionLevel: Cardinal;
    function GetCompressionLevelMax: Cardinal;
    function GetCompressionLevelMin: Cardinal;
    procedure SetCompressionLevel(Value: Cardinal);
    { IJclArchiveCompressionMethod }
    function GetCompressionMethod: TJclCompressionMethod;
    function GetSupportedCompressionMethods: TJclCompressionMethods;
    procedure SetCompressionMethod(Value: TJclCompressionMethod);
    { IJclArchiveNumberOfPasses }
    function GetNumberOfPasses: Cardinal;
    procedure SetNumberOfPasses(Value: Cardinal);
    { IJclArchiveAlgoritm }
    function GetAlgorithm: Cardinal;
    function GetSupportedAlgorithms: TDynCardinalArray;
    procedure SetAlgorithm(Value: Cardinal);
  end;

  TJclBZ2UpdateArchive = class(TJclSevenzipUpdateArchive, IJclArchiveCompressionLevel, IJclArchiveDictionarySize,
    IJclArchiveNumberOfPasses, IJclArchiveNumberOfThreads, IInterface)
  private
    FNumberOfThreads: Cardinal;
    FDictionarySize: Cardinal;
    FCompressionLevel: Cardinal;
    FNumberOfPasses: Cardinal;
  protected
    function GetCLSID: TGUID; override;
    procedure CreateCompressionObject; override;
  public
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
    { IJclArchiveNumberOfThreads }
    function GetNumberOfThreads: Cardinal;
    procedure SetNumberOfThreads(Value: Cardinal);
    { IJclArchiveDictionarySize }
    function GetDictionarySize: Cardinal;
    procedure SetDictionarySize(Value: Cardinal);
    { IJclArchiveCompressionLevel }
    function GetCompressionLevel: Cardinal;
    function GetCompressionLevelMax: Cardinal;
    function GetCompressionLevelMin: Cardinal;
    procedure SetCompressionLevel(Value: Cardinal);
    { IJclArchiveNumberOfPasses }
    function GetNumberOfPasses: Cardinal;
    procedure SetNumberOfPasses(Value: Cardinal);
  end;

  TJcl7zUpdateArchive = class(TJclSevenzipUpdateArchive, IJclArchiveCompressionLevel, IJclArchiveDictionarySize,
    IJclArchiveNumberOfThreads, IJclArchiveRemoveSfxBlock, IJclArchiveCompressHeader, IJclArchiveEncryptHeader,
    IJclArchiveSaveCreationDateTime, IJclArchiveSaveLastAccessDateTime, IJclArchiveSaveLastWriteDateTime, IInterface)
  private
    FNumberOfThreads: Cardinal;
    FEncryptHeader: Boolean;
    FRemoveSfxBlock: Boolean;
    FDictionarySize: Cardinal;
    FCompressionLevel: Cardinal;
    FCompressHeader: Boolean;
    FCompressHeaderFull: Boolean;
    FSaveLastAccessDateTime: Boolean;
    FSaveCreationDateTime: Boolean;
    FSaveLastWriteDateTime: Boolean;
  protected
    function GetCLSID: TGUID; override;
    procedure CreateCompressionObject; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
    { IJclArchiveNumberOfThreads }
    function GetNumberOfThreads: Cardinal;
    procedure SetNumberOfThreads(Value: Cardinal);
    { IJclArchiveEncryptHeader }
    function GetEncryptHeader: Boolean;
    procedure SetEncryptHeader(Value: Boolean);
    { IJclArchiveRemoveSfxBlock }
    function GetRemoveSfxBlock: Boolean;
    procedure SetRemoveSfxBlock(Value: Boolean);
    { IJclArchiveDictionarySize }
    function GetDictionarySize: Cardinal;
    procedure SetDictionarySize(Value: Cardinal);
    { IJclArchiveCompressionLevel }
    function GetCompressionLevel: Cardinal;
    function GetCompressionLevelMax: Cardinal;
    function GetCompressionLevelMin: Cardinal;
    procedure SetCompressionLevel(Value: Cardinal);
    { IJclArchiveCompressHeader }
    function GetCompressHeader: Boolean;
    function GetCompressHeaderFull: Boolean;
    procedure SetCompressHeader(Value: Boolean);
    procedure SetCompressHeaderFull(Value: Boolean);
    { IJclArchiveSaveLastAccessDateTime }
    function GetSaveLastAccessDateTime: Boolean;
    procedure SetSaveLastAccessDateTime(Value: Boolean);
    { IJclArchiveSaveCreationDateTime }
    function GetSaveCreationDateTime: Boolean;
    procedure SetSaveCreationDateTime(Value: Boolean);
    { IJclArchiveSaveLastWriteDateTime }
    function GetSaveLastWriteDateTime: Boolean;
    procedure SetSaveLastWriteDateTime(Value: Boolean);
  end;

  TJclTarUpdateArchive = class(TJclSevenzipUpdateArchive, IInterface)
  protected
    function GetCLSID: TGUID; override;
  public
    class function MultipleItemContainer: Boolean; override;
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
  end;

  TJclGZipUpdateArchive = class(TJclSevenzipUpdateArchive, IJclArchiveCompressionLevel, IJclArchiveNumberOfPasses,
    IJclArchiveAlgorithm, IInterface)
  private
    FCompressionLevel: Cardinal;
    FNumberOfPasses: Cardinal;
    FAlgorithm: Cardinal;
  protected
    function GetCLSID: TGUID; override;
    procedure CreateCompressionObject; override;
  public
    class function ArchiveExtensions: string; override;
    class function ArchiveName: string; override;
    { IJclArchiveCompressionLevel }
    function GetCompressionLevel: Cardinal;
    function GetCompressionLevelMax: Cardinal;
    function GetCompressionLevelMin: Cardinal;
    procedure SetCompressionLevel(Value: Cardinal);
    { IJclArchiveNumberOfPasses }
    function GetNumberOfPasses: Cardinal;
    procedure SetNumberOfPasses(Value: Cardinal);
    { IJclArchiveAlgorithm }
    function GetAlgorithm: Cardinal;
    function GetSupportedAlgorithms: TDynCardinalArray;
    procedure SetAlgorithm(Value: Cardinal);
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
  JclUnicode, // WideSameText // TODO: should we use WideSameText from JclUnicode also for D6 and higher?
  {$ENDIF COMPILER5}
  JclDateTime, JclFileUtils, JclResources, JclStrings, JclSysUtils;

const
  JclDefaultBufferSize = 131072; // 128k

var
  // using TObject prevents default linking of TJclCompressionStreamFormats
  // and TJclCompressionArchiveFormats and all classes
  GlobalStreamFormats: TObject;
  GlobalArchiveFormats: TObject;

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

class function TJclCompressionStream.StreamExtensions: string;
begin
  Result := '';
end;

class function TJclCompressionStream.StreamName: string;
begin
  Result := '';
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

//=== { TJclCompressionStreamFormats } =======================================

constructor TJclCompressionStreamFormats.Create;
begin
  inherited Create;
  FCompressFormats := TList.Create;
  FDecompressFormats := TList.Create;
  RegisterFormat(TJclZLibCompressStream);
  RegisterFormat(TJclZLibDecompressStream);
  RegisterFormat(TJclGZIPCompressionStream);
  RegisterFormat(TJclGZIPDecompressionStream);
  RegisterFormat(TJclBZIP2CompressionStream);
  RegisterFormat(TJclBZIP2DecompressionStream);
end;

destructor TJclCompressionStreamFormats.Destroy;
begin
  FCompressFormats.Free;
  FDecompressFormats.Free;
  inherited Destroy;
end;

function TJclCompressionStreamFormats.FindCompressFormat(const AFileName: TFileName): TJclCompressStreamClass;
var
  IndexFormat, IndexFilter: Integer;
  Filters: TStrings;
  AFormat: TJclCompressStreamClass;
begin
  Result := nil;
  Filters := TStringList.Create;
  try
    for IndexFormat := 0 to CompressFormatCount - 1 do
    begin
      AFormat := CompressFormats[IndexFormat];
      StrTokenToStrings(AFormat.StreamExtensions, DirSeparator, Filters);
      for IndexFilter := 0 to Filters.Count - 1 do
        if StrMatches(Filters.Strings[IndexFilter], StrLower(AFileName)) then
      begin
        Result := AFormat;
        Break;
      end;
      if Result <> nil then
        Break;
    end;
  finally
    Filters.Free;
  end;
end;

function TJclCompressionStreamFormats.FindDecompressFormat(const AFileName: TFileName): TJclDecompressStreamClass;
var
  IndexFormat, IndexFilter: Integer;
  Filters: TStrings;
  AFormat: TJclDecompressStreamClass;
begin
  Result := nil;
  Filters := TStringList.Create;
  try
    for IndexFormat := 0 to DecompressFormatCount - 1 do
    begin
      AFormat := DecompressFormats[IndexFormat];
      StrTokenToStrings(AFormat.StreamExtensions, DirSeparator, Filters);
      for IndexFilter := 0 to Filters.Count - 1 do
        if StrMatches(Filters.Strings[IndexFilter], AFileName) then
      begin
        Result := AFormat;
        Break;
      end;
      if Result <> nil then
        Break;
    end;
  finally
    Filters.Free;
  end;
end;

function TJclCompressionStreamFormats.GetCompressFormat(Index: Integer): TJclCompressStreamClass;
begin
  Result := TJclCompressStreamClass(FCompressFormats.Items[Index]);
end;

function TJclCompressionStreamFormats.GetCompressFormatCount: Integer;
begin
  Result := FCompressFormats.Count;
end;

function TJclCompressionStreamFormats.GetDecompressFormat(Index: Integer): TJclDecompressStreamClass;
begin
  Result := TJclDecompressStreamClass(FDecompressFormats.Items[Index]);
end;

function TJclCompressionStreamFormats.GetDecompressFormatCount: Integer;
begin
  Result := FDecompressFormats.Count;
end;

procedure TJclCompressionStreamFormats.RegisterFormat(AClass: TJclCompressionStreamClass);
begin
  if AClass.InheritsFrom(TJclCompressStream) then
    FCompressFormats.Add(AClass)
  else
  if AClass.InheritsFrom(TJclDecompressStream) then
    FDecompressFormats.Add(AClass);
end;

procedure TJclCompressionStreamFormats.UnregisterFormat(AClass: TJclCompressionStreamClass);
begin
  if AClass.InheritsFrom(TJclCompressStream) then
    FCompressFormats.Remove(AClass)
  else
  if AClass.InheritsFrom(TJclDecompressStream) then
    FDecompressFormats.Remove(AClass);
end;

function GetStreamFormats: TJclCompressionStreamFormats;
begin
  if not Assigned(GlobalStreamFormats) then
    GlobalStreamFormats := TJclCompressionStreamFormats.Create;
  Result := TJclCompressionStreamFormats(GlobalStreamFormats);
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

  LoadZLib;

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

class function TJclZLibCompressStream.StreamExtensions: string;
begin
  Result := LoadResString(@RsCompressionZExtensions);
end;

class function TJclZLibCompressStream.StreamName: string;
begin
  Result := LoadResString(@RsCompressionZName);
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

  LoadZLib;

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

      // Suggestion by ZENsan (mantis 4546)
      if Res = Z_STREAM_END then
      begin
        Result := Count - Longint(ZLibRecord.avail_out);
        Exit;
      end;
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

class function TJclZLibDecompressStream.StreamExtensions: string;
begin
  Result := LoadResString(@RsCompressionZExtensions);
end;

class function TJclZLibDecompressStream.StreamName: string;
begin
  Result := LoadResString(@RsCompressionZName);
end;

//=== { TJclGZIPCompressionStream } ==========================================

constructor TJclGZIPCompressionStream.Create(Destination: TStream; CompressionLevel: TJclCompressionLevel);
begin
  inherited Create(Destination);

  LoadZLib;

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

class function TJclGZIPCompressionStream.StreamExtensions: string;
begin
  Result := LoadResString(@RsCompressionGZipExtensions);
end;

class function TJclGZIPCompressionStream.StreamName: string;
begin
  Result := LoadResString(@RsCompressionGZipName);
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
    FZLibStream := TJclZLibCompressStream.Create(FStream, FCompressionLevel);
    FZLibStream.WindowBits := -DEF_WBITS; // negative value for raw mode
    FZLibStream.OnProgress := ZLibStreamProgress;
  end;

  Result := FZLibStream.Write(Buffer, Count);
  FDataCRC32 := crc32(FDataCRC32, PBytef(@Buffer), Result);
  Inc(FOriginalSize, Result);
end;

procedure TJclGZIPCompressionStream.WriteHeader;
const
  FatSystemToByte: array [TJclGZIPFatSystem] of Byte =
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

  LoadZLib;

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
  ByteToFatSystem: array [JCL_GZIP_OS_FAT..JCL_GZIP_OS_ACORN] of TJclGZIPFatSystem =
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
  BufferAddr: PAnsiChar;
  FooterAddr: PAnsiChar;
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

  BufferAddr := @Byte(Buffer);
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

class function TJclGZIPDecompressionStream.StreamExtensions: string;
begin
  Result := LoadResString(@RsCompressionGZipExtensions);
end;

class function TJclGZIPDecompressionStream.StreamName: string;
begin
  Result := LoadResString(@RsCompressionGZipName);
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

class function TJclBZIP2CompressionStream.StreamExtensions: string;
begin
  Result := LoadResString(@RsCompressionBZip2Extensions);
end;

class function TJclBZIP2CompressionStream.StreamName: string;
begin
  Result := LoadResString(@RsCompressionBZip2Name);
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

constructor TJclBZIP2DecompressionStream.Create(Source: TStream; AOwnsStream: Boolean);
begin
  inherited Create(Source, AOwnsStream);

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

class function TJclBZIP2DecompressionStream.StreamExtensions: string;
begin
  Result := LoadResString(@RsCompressionBZip2Extensions);
end;

class function TJclBZIP2DecompressionStream.StreamName: string;
begin
  Result := LoadResString(@RsCompressionBZip2Name);
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

function GZipFile(SourceFile, DestinationFile: TFileName; CompressionLevel: Integer;
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

function UnGZipFile(SourceFile, DestinationFile: TFileName;
  ProgressCallback: TJclCompressStreamProgressCallback; UserData: Pointer): Boolean;
var
  GZipStream: TJclGZIPDecompressionStream;
  DestStream: TFileStream;
  SourceStream: TFileStream;
  GZipStreamDateTime: TDateTime;
begin
  Result := False;
  if not FileExists(SourceFile) then // can't copy what doesn't exist!
    Exit;

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

function BZip2File(SourceFile, DestinationFile: TFileName; CompressionLevel: Integer;
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

function UnBZip2File(SourceFile, DestinationFile: TFileName;
  ProgressCallback: TJclCompressStreamProgressCallback; UserData: Pointer): Boolean;
var
  BZip2Stream: TJclBZIP2DecompressionStream;
  DestStream: TFileStream;
  SourceStream: TFileStream;
begin
  Result := False;
  if not FileExists(SourceFile) then // can't copy what doesn't exist!
    Exit;

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

{$IFDEF MSWINDOWS}

function OpenFileStream(const FileName: TFileName; StreamAccess: TJclStreamAccess): TStream;
begin
  Result := nil;
  case StreamAccess of
    saCreate:
      Result := TFileStream.Create(FileName, fmCreate);
    saReadOnly:
      if FileExists(FileName) then
        Result := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    saReadOnlyDenyNone:
      if FileExists(FileName) then
        Result := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    saWriteOnly:
      if FileExists(FileName) then
        Result := TFileStream.Create(FileName, fmOpenWrite)
      else
      if FileName <> '' then
        Result := TFileStream.Create(FileName, fmCreate);
    saReadWrite:
      if FileExists(FileName) then
        Result := TFileStream.Create(FileName, fmOpenReadWrite)
      else
      if FileName <> '' then
        Result := TFileStream.Create(FileName, fmCreate);
  end;
end;

//=== { TJclCompressionItem } ================================================

constructor TJclCompressionItem.Create(AArchive: TJclCompressionArchive);
begin
  inherited Create;
  FArchive := AArchive;
  FPackedIndex := $FFFFFFFF;
end;

function TJclCompressionItem.DeleteOutputFile: Boolean;
begin
  Result := (FFileName <> '') and FileExists(FFileName) and FileDelete(FFileName);
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

function TJclCompressionItem.GetPackedExtension: WideString;
var
  Index: Integer;
begin
  CheckGetProperty(ipPackedExtension);
  if FPackedName = '' then
    Result := FPackedExtension
  else
  begin
    Result := '';

    // Unicode version of ExtractFileExt
    for Index := Length(FPackedName) downto 1 do
    begin
      case FPackedName[Index] of
        '.':
          begin
            Result := Copy(FPackedName, Index, Length(FPackedName) - Index + 1);
            Break;
          end;
        DirSeparator,
        DirDelimiter:
          // no extension
          Break;
      end;
    end;
  end;
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

function TJclCompressionItem.GetStream: TStream;
begin
  if not Assigned(FStream) and (FileName <> '') then
    FStream := OpenFileStream(FileName, Archive.ItemAccess);

  Result := FStream;
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

procedure TJclCompressionItem.SetAttributes(Value: Cardinal);
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

procedure TJclCompressionItem.SetCRC(Value: Cardinal);
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
  if Value <> '' then
  begin
    Include(FModifiedProperties, ipFileName);
    Include(FValidProperties, ipFileName);
  end
  else
  begin
    Exclude(FModifiedProperties, ipFileName);
    Exclude(FValidProperties, ipFileName);
  end;

  if (Value <> '') and (FArchive is TJclCompressionArchive)
    and GetFileAttributesEx(PChar(Value), GetFileExInfoStandard, @AFindData) then
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

procedure TJclCompressionItem.SetPackedExtension(const Value: WideString);
begin
  CheckSetProperty(ipPackedExtension);
  if (Value <> '') and (Value[1] <> '.') then
    // force heading '.'
    FPackedExtension := '.' + Value
  else
    FPackedExtension := Value;
  Include(FModifiedProperties, ipPackedExtension);
  Include(FValidProperties, ipPackedExtension);
end;

procedure TJclCompressionItem.SetPackedName(const Value: WideString);
var
  PackedNamesIndex: Integer;
begin
  if FPackedName <> Value then
  begin
    CheckSetProperty(ipPackedName);
    if FArchive is TJclCompressArchive then
      with FArchive as TJclCompressArchive do
      begin
        if (FPackedNames <> nil) and FPackedNames.Find(FPackedName, PackedNamesIndex) then
        begin
          FPackedNames.Delete(PackedNamesIndex);
          try
            FPackedNames.Add(Value);
          except
            raise EJclCompressionError(Format(RsCompressionDuplicate, [Value]));
          end;
        end;
      end;
    FPackedName := Value;
    Include(FModifiedProperties, ipPackedName);
    Include(FValidProperties, ipPackedName);
  end;
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

function TJclCompressionItem.UpdateFileTimes: Boolean;
const
  FILE_WRITE_ATTRIBUTES = $00000100;
var
  FileHandle: HFILE;
  ACreationTime, ALastAccessTime, ALastWriteTime: PFileTime;
begin
  ReleaseStream;
  Result := FFileName <> '';
  if Result then
  begin
    FileHandle := CreateFile(PChar(FFileName), FILE_WRITE_ATTRIBUTES, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
    try
      // creation time should be the oldest
      if ipCreationTime in FValidProperties then
        ACreationTime := @FCreationTime
      else
      if ipLastWriteTime in FValidProperties then
        ACreationTime := @FLastWriteTime
      else
      if ipLastAccessTime in FValidProperties then
        ACreationTime := @FLastAccessTime
      else
        ACreationTime := nil;

      // last access time may default to now if not set
      if ipLastAccessTime in FValidProperties then
        ALastAccessTime := @FLastAccessTime
      else
        ALastAccessTime := nil;

      // last write time may, if not set, be the creation time or last access time
      if ipLastWriteTime in FValidProperties then
        ALastWriteTime := @FLastWriteTime
      else
      if ipCreationTime in FValidProperties then
        ALastWriteTime := @FCreationTime
      else
      if ipLastAccessTime in FValidProperties then
        ALastWriteTime := @FLastAccessTime
      else
        ALastWriteTime := nil;

      Result := (FileHandle <> INVALID_HANDLE_VALUE) and SetFileTime(FileHandle, ACreationTime, ALastAccessTime,
        ALastWriteTime);
    finally
      CloseHandle(FileHandle);
    end;
  end;
end;

function TJclCompressionItem.ValidateExtraction(Index: Integer): Boolean;
begin
  Result := False;
end;

//=== { TJclCompressionArchiveFormats } ======================================

constructor TJclCompressionArchiveFormats.Create;
begin
  inherited Create;
  FCompressFormats := TList.Create;
  FDecompressFormats := TList.Create;
  FUpdateFormats := TList.Create;
  RegisterFormat(TJclZipCompressArchive);
  RegisterFormat(TJclBZ2CompressArchive);
  RegisterFormat(TJcl7zCompressArchive);
  RegisterFormat(TJclTarCompressArchive);
  RegisterFormat(TJclGZipCompressArchive);
  RegisterFormat(TJclZipDecompressArchive);
  RegisterFormat(TJclBZ2DecompressArchive);
  RegisterFormat(TJclRarDecompressArchive);
  RegisterFormat(TJclArjDecompressArchive);
  RegisterFormat(TJclZDecompressArchive);
  RegisterFormat(TJclLzhDecompressArchive);
  RegisterFormat(TJcl7zDecompressArchive);
  RegisterFormat(TJclCabDecompressArchive);
  RegisterFormat(TJclNsisDecompressArchive);
  RegisterFormat(TJclLzmaDecompressArchive);
  RegisterFormat(TJclPeDecompressArchive);
  RegisterFormat(TJclElfDecompressArchive);
  RegisterFormat(TJclMachoDecompressArchive);
  RegisterFormat(TJclUdfDecompressArchive);
  RegisterFormat(TJclXarDecompressArchive);
  RegisterFormat(TJclMubDecompressArchive);
  RegisterFormat(TJclHfsDecompressArchive);
  RegisterFormat(TJclDmgDecompressArchive);
  RegisterFormat(TJclCompoundDecompressArchive);
  RegisterFormat(TJclWimDecompressArchive);
  RegisterFormat(TJclIsoDecompressArchive);
  RegisterFormat(TJclChmDecompressArchive);
  RegisterFormat(TJclSplitDecompressArchive);
  RegisterFormat(TJclRpmDecompressArchive);
  RegisterFormat(TJclDebDecompressArchive);
  RegisterFormat(TJclCpioDecompressArchive);
  RegisterFormat(TJclTarDecompressArchive);
  RegisterFormat(TJclGZipDecompressArchive);
  RegisterFormat(TJclZipUpdateArchive);
  RegisterFormat(TJclBZ2UpdateArchive);
  RegisterFormat(TJcl7zUpdateArchive);
  RegisterFormat(TJclTarUpdateArchive);
  RegisterFormat(TJclGZipUpdateArchive);
end;

destructor TJclCompressionArchiveFormats.Destroy;
begin
  FCompressFormats.Free;
  FDecompressFormats.Free;
  FUpdateFormats.Free;
  inherited Destroy;
end;

function TJclCompressionArchiveFormats.FindCompressFormat(const AFileName: TFileName): TJclCompressArchiveClass;
var
  IndexFormat, IndexFilter: Integer;
  Filters: TStrings;
  AFormat: TJclCompressArchiveClass;
begin
  Result := nil;
  Filters := TStringList.Create;
  try
    for IndexFormat := 0 to CompressFormatCount - 1 do
    begin
      AFormat := CompressFormats[IndexFormat];
      StrTokenToStrings(AFormat.ArchiveExtensions, DirSeparator, Filters);
      for IndexFilter := 0 to Filters.Count - 1 do
        if StrMatches(Filters.Strings[IndexFilter], AFileName) then
      begin
        Result := AFormat;
        Break;
      end;
      if Result <> nil then
        Break;
    end;
  finally
    Filters.Free;
  end;
end;

function TJclCompressionArchiveFormats.FindDecompressFormat(const AFileName: TFileName): TJclDecompressArchiveClass;
var
  IndexFormat, IndexFilter: Integer;
  Filters: TStrings;
  AFormat: TJclDecompressArchiveClass;
begin
  Result := nil;
  Filters := TStringList.Create;
  try
    for IndexFormat := 0 to DecompressFormatCount - 1 do
    begin
      AFormat := DecompressFormats[IndexFormat];
      StrTokenToStrings(AFormat.ArchiveExtensions, DirSeparator, Filters);
      for IndexFilter := 0 to Filters.Count - 1 do
        if StrMatches(Filters.Strings[IndexFilter], AFileName) then
      begin
        Result := AFormat;
        Break;
      end;
      if Result <> nil then
        Break;
    end;
  finally
    Filters.Free;
  end;
end;

function TJclCompressionArchiveFormats.FindUpdateFormat(const AFileName: TFileName): TJclUpdateArchiveClass;
var
  IndexFormat, IndexFilter: Integer;
  Filters: TStrings;
  AFormat: TJclUpdateArchiveClass;
begin
  Result := nil;
  Filters := TStringList.Create;
  try
    for IndexFormat := 0 to UpdateFormatCount - 1 do
    begin
      AFormat := UpdateFormats[IndexFormat];
      StrTokenToStrings(AFormat.ArchiveExtensions, DirSeparator, Filters);
      for IndexFilter := 0 to Filters.Count - 1 do
        if StrMatches(Filters.Strings[IndexFilter], AFileName) then
      begin
        Result := AFormat;
        Break;
      end;
      if Result <> nil then
        Break;
    end;
  finally
    Filters.Free;
  end;
end;

function TJclCompressionArchiveFormats.GetCompressFormat(Index: Integer): TJclCompressArchiveClass;
begin
  Result := TJclCompressArchiveClass(FCompressFormats.Items[Index]);
end;

function TJclCompressionArchiveFormats.GetCompressFormatCount: Integer;
begin
  Result := FCompressFormats.Count;
end;

function TJclCompressionArchiveFormats.GetDecompressFormat(Index: Integer): TJclDecompressArchiveClass;
begin
  Result := TJclDecompressArchiveClass(FDecompressFormats.Items[Index]);
end;

function TJclCompressionArchiveFormats.GetDecompressFormatCount: Integer;
begin
  Result := FDecompressFormats.Count;
end;

function TJclCompressionArchiveFormats.GetUpdateFormat(Index: Integer): TJclUpdateArchiveClass;
begin
  Result := TJclUpdateArchiveClass(FUpdateFormats.Items[Index]);
end;

function TJclCompressionArchiveFormats.GetUpdateFormatCount: Integer;
begin
  Result := FUpdateFormats.Count;
end;

procedure TJclCompressionArchiveFormats.RegisterFormat(AClass: TJclCompressionArchiveClass);
begin
  if AClass.InheritsFrom(TJclUpdateArchive) then
    FUpdateFormats.Add(AClass)
  else
  if AClass.InheritsFrom(TJclDecompressArchive) then
    FDecompressFormats.Add(AClass)
  else
  if AClass.InheritsFrom(TJclCompressArchive) then
    FCompressFormats.Add(AClass);
end;

procedure TJclCompressionArchiveFormats.UnregisterFormat(AClass: TJclCompressionArchiveClass);
begin
  if AClass.InheritsFrom(TJclUpdateArchive) then
    FUpdateFormats.Remove(AClass)
  else
  if AClass.InheritsFrom(TJclDecompressArchive) then
    FDecompressFormats.Remove(AClass)
  else
  if AClass.InheritsFrom(TJclCompressArchive) then
    FCompressFormats.Remove(AClass);
end;

function GetArchiveFormats: TJclCompressionArchiveFormats;
begin
  if not Assigned(GlobalArchiveFormats) then
    GlobalArchiveFormats := TJclCompressionArchiveFormats.Create;
  Result := TJclCompressionArchiveFormats(GlobalArchiveFormats);
end;

//=== { TJclCompressionVolume } ==============================================

constructor TJclCompressionVolume.Create(AStream, ATmpStream: TStream; AOwnsStream, AOwnsTmpStream: Boolean;
  AFileName, ATmpFileName: TFileName; AVolumeMaxSize: Int64);
begin
  inherited Create;
  FStream := AStream;
  FTmpStream := ATmpStream;
  FOwnsStream := AOwnsStream;
  FOwnsTmpStream := AOwnsTmpStream;
  FFileName := AFileName;
  FTmpFileName := ATmpFileName;
  FVolumeMaxSize := AVolumeMaxSize;
end;

destructor TJclCompressionVolume.Destroy;
begin
  ReleaseStreams;
  inherited Destroy;
end;

procedure TJclCompressionVolume.ReleaseStreams;
begin
  if OwnsStream then
    FreeAndNil(FStream);
  if OwnsTmpStream then
    FreeAndNil(FTmpStream);
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

constructor TJclCompressionArchive.Create(const VolumeFileName: TFileName;
  AVolumeMaxSize: Int64 = 0; VolumeMask: Boolean = False);
begin
  inherited Create;
  FVolumeIndex := -1;
  FVolumeIndexOffset := 1;
  FVolumeMaxSize := AVolumeMaxSize;
  FItems := TObjectList.Create(True);
  FVolumes := TObjectList.Create(True);
  if VolumeMask then
    FVolumeFileNameMask := VolumeFileName
  else
    AddVolume(VolumeFileName, AVolumeMaxSize);
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
  Result := FVolumes.Add(TJclCompressionVolume.Create(VolumeStream, nil, AOwnsStream, True, '', '', AVolumeMaxSize));
end;

function TJclCompressionArchive.AddVolume(VolumeStream, TmpVolumeStream: TStream;
  AVolumeMaxSize: Int64; AOwnsStream, AOwnsTmpStream: Boolean): Integer;
begin
  Result := FVolumes.Add(TJclCompressionVolume.Create(VolumeStream, TmpVolumeStream, AOwnsStream, AOwnsTmpStream, '', '', AVolumeMaxSize));
end;

function TJclCompressionArchive.AddVolume(const VolumeFileName: TFileName;
  AVolumeMaxSize: Int64): Integer;
begin
  Result := FVolumes.Add(TJclCompressionVolume.Create(nil, nil, True, True, VolumeFileName, '', AVolumeMaxSize));
end;

function TJclCompressionArchive.AddVolume(const VolumeFileName, TmpVolumeFileName: TFileName;
  AVolumeMaxSize: Int64): Integer;
begin
  Result := FVolumes.Add(TJclCompressionVolume.Create(nil, nil, True, True, VolumeFileName, TmpVolumeFileName, AVolumeMaxSize));
end;

class function TJclCompressionArchive.ArchiveExtensions: string;
begin
  Result := '';
end;

class function TJclCompressionArchive.ArchiveName: string;
begin
  Result := '';
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

function TJclCompressionArchive.InternalOpenStream(
  const FileName: TFileName): TStream;
begin
  Result := OpenFileStream(FileName, VolumeAccess);
end;

class function TJclCompressionArchive.ItemAccess: TJclStreamAccess;
begin
  Result := saReadOnly;
end;

class function TJclCompressionArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

function TJclCompressionArchive.NeedStream(Index: Integer): TStream;
var
  AVolume: TJclCompressionVolume;
  AOwnsStream: Boolean;
  AFileName: TFileName;
begin
  Result := nil;

  if Index <> FVolumeIndex then
  begin
    AOwnsStream := VolumeFileNameMask <> '';
    AVolume := nil;
    AFileName := Format(VolumeFileNameMask, [Index + VolumeIndexOffset]);
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
        Result := InternalOpenStream(AFileName);
      AVolume.FFileName := AFileName;
      AVolume.FStream := Result;
      AVolume.FOwnsStream := AOwnsStream;
    end
    else
    begin
      while FVolumes.Count < Index do
        FVolumes.Add(TJclCompressionVolume.Create(nil, nil, True, True, Format(VolumeFileNameMask, [Index + VolumeIndexOffset]), '', FVolumeMaxSize));
      if not Assigned(Result) then
        Result := InternalOpenStream(AFileName);
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
          FVolumes.Add(TJclCompressionVolume.Create(Result, nil, AOwnsStream, True, AFileName, '', FVolumeMaxSize));
      end;
    end;
    FVolumeIndex := Index;
  end
  else
  if (Index >= 0) and (Index < FVolumes.Count) then
  begin
    AVolume := TJclCompressionVolume(FVolumes.Items[Index]);
    Result := AVolume.Stream;
    if Assigned(Result) then
      StreamSeek(Result, 0, soBeginning);
  end
  else
    FVolumeIndex := Index;
end;

function TJclCompressionArchive.NeedStreamMaxSize(Index: Integer): Int64;
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
        FVolumes.Add(TJclCompressionVolume.Create(nil, nil, True, True, Format(VolumeFileNameMask, [Index + VolumeIndexOffset]), '', FVolumeMaxSize));
      if Index < FVolumes.Count then
      begin
        AVolume := TJclCompressionVolume(FVolumes.Items[Index]);
        AVolume.FFileName := Format(VolumeFileNameMask, [Index + VolumeIndexOffset]);
        AVolume.FStream := nil;
        AVolume.FOwnsStream := True;
        AVolume.FVolumeMaxSize := FVolumeMaxSize;
      end
      else
        FVolumes.Add(TJclCompressionVolume.Create(nil, nil, True, True, Format(VolumeFileNameMask, [Index + VolumeIndexOffset]), '', FVolumeMaxSize));
    end;
  end;
  Result := FVolumeMaxSize;
end;

procedure TJclCompressionArchive.ReleaseVolumes;
var
  Index: Integer;
begin
  for Index := 0 to FVolumes.Count - 1 do
    TJclCompressionVolume(FVolumes.Items[Index]).ReleaseStreams;
end;

procedure TJclCompressionArchive.SelectAll;
var
  Index: Integer;
begin
  for Index := 0 to FItems.Count - 1 do
    TJclCompressionItem(FItems.Items[Index]).Selected := True;
end;

function TJclCompressionArchive.TranslateItemPath(const ItemPath, OldBase,
  NewBase: WideString): WideString;
begin
  Result := PathCanonicalize(PathAddSeparator(NewBase) + PathGetRelativePath(OldBase, ItemPath));
end;

procedure TJclCompressionArchive.UnselectAll;
var
  Index: Integer;
begin
  for Index := 0 to FItems.Count - 1 do
    TJclCompressionItem(FItems.Items[Index]).Selected := False;
end;

class function TJclCompressionArchive.VolumeAccess: TJclStreamAccess;
begin
  Result := saReadOnly;
end;

function TJclCompressionArchive._AddRef: Integer;
begin
  Result := -1;
end;

function TJclCompressionArchive._Release: Integer;
begin
  Result := -1;
end;

function TJclCompressionArchive.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
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

//=== { TJclCompressArchive } ================================================

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

  Result := AddFileCheckDuplicate(AItem);

  if (DirName <> '') and AddFilesInDir then
    EnumFiles(PathAddSeparator(DirName) + '*', InternalAddFile, faDirectory);
end;

function TJclCompressArchive.AddFile(const PackedName: WideString;
  const FileName: TFileName): Integer;
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

  Result := AddFileCheckDuplicate(AItem);
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
    AItem.FileSize := AStream.Size - AStream.Position;
  except
    AItem.Destroy;
    raise;
  end;

  Result := AddFileCheckDuplicate(AItem);
end;

function TJclCompressArchive.AddFileCheckDuplicate(NewItem: TJclCompressionItem): Integer;
var
  I, PackedNamesIndex: Integer;
  S: string;
begin
  if FDuplicateCheck = dcNone then
    Result := FItems.Add(NewItem)
  else
  begin
    if FPackedNames = nil then
    begin
      FPackedNames := {$IFDEF SUPPORTS_UNICODE}TStringList{$ELSE}TWStringList{$ENDIF}.Create;
      FPackedNames.Sorted := True;
      {$IFDEF UNIX}
      FPackedNames.CaseSensitive := True;
      {$ELSE ~UNIX}
      FPackedNames.CaseSensitive := False;
      {$ENDIF ~UNIX}
      FPackedNames.Duplicates := dupIgnore;
      for I := ItemCount - 1 downto 0 do
        FPackedNames.AddObject(Items[I].PackedName, Items[I]);
      FPackedNames.Duplicates := dupError;
    end;
    if DuplicateCheck = dcAll then
    begin
      try
        PackedNamesIndex := -1;
        FPackedNames.AddObject(NewItem.PackedName, NewItem);
        Result := FItems.Add(NewItem);
      except
        Result := -1;
      end;
    end
    else
    if FPackedNames.Find(NewItem.PackedName, PackedNamesIndex) then
      Result := -1
    else
      Result := FItems.Add(NewItem);
    if Result < 0 then
    begin
      case DuplicateAction of
        daOverwrite:
          begin
            if PackedNamesIndex < 0 then
              PackedNamesIndex := FPackedNames.IndexOf(NewItem.PackedName);
            FItems.Remove(FPackedNames.Objects[PackedNamesIndex]);
            Result := FItems.Add(NewItem);
            if DuplicateCheck = dcAll then
              FPackedNames.Objects[PackedNamesIndex] := NewItem
            else
              FPackedNames.Delete(PackedNamesIndex);
          end;
        daError:
          begin
            S := Format(RsCompressionDuplicate, [NewItem.PackedName]);
            NewItem.Free;
            raise EJclCompressionError.Create(S);
          end;
        daSkip:
          begin
            NewItem.Free;
            Result := -1;
          end;
      end
    end;
  end;
end;

procedure TJclCompressArchive.CheckNotCompressing;
begin
  if FCompressing then
    raise EJclCompressionError.CreateRes(@RsCompressionCompressingError);
end;

procedure TJclCompressArchive.Compress;
begin
  ReleaseVolumes;
end;

procedure TJclCompressArchive.InternalAddDirectory(const Directory: string);
begin
  AddDirectory(TranslateItemPath(Directory, FBaseDirName, FBaseRelName), Directory, False, FAddFilesInDir);
end;

procedure TJclCompressArchive.InternalAddFile(const Directory: string;
  const FileInfo: TSearchRec);
var
  AFileName: TFileName;
  AItem: TJclCompressionItem;
begin
  AFileName := PathAddSeparator(Directory) + FileInfo.Name;

  AItem := GetItemClass.Create(Self);
  try
    AItem.PackedName := TranslateItemPath(AFileName, FBaseDirName, FBaseRelName);
    AItem.FileName := AFileName;
  except
    AItem.Destroy;
    raise;
  end;

  AddFileCheckDuplicate(AItem);
end;

class function TJclCompressArchive.ItemAccess: TJclStreamAccess;
begin
  Result := saReadOnly;
end;

class function TJclCompressArchive.VolumeAccess: TJclStreamAccess;
begin
  Result := saWriteOnly;
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
  (Archive as TJclDecompressArchive).CheckNotDecompressing;
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

procedure TJclDecompressArchive.ExtractAll(const ADestinationDir: string;
  AAutoCreateSubDir: Boolean);
begin
  ReleaseVolumes;
end;

procedure TJclDecompressArchive.ExtractSelected(const ADestinationDir: string;
  AAutoCreateSubDir: Boolean);
begin
  ReleaseVolumes;
end;

class function TJclDecompressArchive.ItemAccess: TJclStreamAccess;
begin
  Result := saCreate;
end;

function TJclDecompressArchive.ValidateExtraction(Index: Integer;
  var FileName: TFileName; var AStream: TStream; var AOwnsStream: Boolean): Boolean;
var
  AItem: TJclCompressionItem;
  PackedName: TFileName;
begin
  if FExtractingAllIndex <> -1 then
    // extracting all
    FExtractingAllIndex := Index;

  AItem := Items[Index];

  if (FileName = '') and not Assigned(AStream) then
  begin
    PackedName := AItem.PackedName;

    if PackedName = '' then
      PackedName := ChangeFileExt(ExtractFileName(Volumes[0].FileName), AItem.PackedExtension);

    FileName := PathGetRelativePath(FDestinationDir, PackedName);
  end;
  Result := True;

  if Assigned(FOnExtract) then
    Result := FOnExtract(Self, Index, FileName, AStream, AOwnsStream);

  if Result and not Assigned(AStream) and AutoCreateSubDir then
  begin
    if (AItem.Attributes and faDirectory) <> 0 then
      ForceDirectories(FileName)
    else
      ForceDirectories(ExtractFilePath(FileName));
  end;
end;

class function TJclDecompressArchive.VolumeAccess: TJclStreamAccess;
begin
  Result := saReadOnly;
end;

//=== { TJclUpdateItem } =====================================================

procedure TJclUpdateItem.CheckGetProperty(
  AProperty: TJclCompressionItemProperty);
begin
  // TODO
end;

procedure TJclUpdateItem.CheckSetProperty(
  AProperty: TJclCompressionItemProperty);
begin
  (Archive as TJclCompressArchive).CheckNotCompressing;
end;

function TJclUpdateItem.ValidateExtraction(Index: Integer): Boolean;
begin
  Result := (Archive as TJclUpdateArchive).ValidateExtraction(Index, FFileName,
    FStream, FOwnsStream);
end;

//=== { TJclUpdateArchive } ==================================================

procedure TJclUpdateArchive.CheckListing;
begin
  if not FListing then
    raise EJclCompressionError.CreateRes(@RsCompressionUnavailableProperty);
end;

procedure TJclUpdateArchive.CheckNotDecompressing;
begin
  if FDecompressing then
    raise EJclCompressionError.CreateRes(@RsCompressionDecompressingError);
end;

constructor TJclUpdateArchive.Create(Volume0: TStream; AVolumeMaxSize: Int64; AOwnVolume: Boolean);
begin
  inherited Create(Volume0, AVolumeMaxSize, AOwnVolume);
  FDuplicateCheck := dcExisting;
end;

constructor TJclUpdateArchive.Create(const VolumeFileName: TFileName; AVolumeMaxSize: Int64; VolumeMask: Boolean);
begin
  inherited Create(VolumeFileName, AVolumeMaxSize, VolumeMask);
  FDuplicateCheck := dcExisting;
end;

procedure TJclUpdateArchive.ExtractAll(const ADestinationDir: string;
  AAutoCreateSubDir: Boolean);
begin
  ReleaseVolumes;
end;

procedure TJclUpdateArchive.ExtractSelected(const ADestinationDir: string;
  AAutoCreateSubDir: Boolean);
begin
  ReleaseVolumes;
end;

class function TJclUpdateArchive.ItemAccess: TJclStreamAccess;
begin
  Result := saReadWrite;
end;

function TJclUpdateArchive.ValidateExtraction(Index: Integer;
  var FileName: TFileName; var AStream: TStream;
  var AOwnsStream: Boolean): Boolean;
var
  AItem: TJclCompressionItem;
  PackedName: TFileName;
begin
  if FExtractingAllIndex <> -1 then
    // extracting all
    FExtractingAllIndex := Index;

  AItem := Items[Index];

  if FileName = '' then
  begin
    PackedName := AItem.PackedName;

    if PackedName = '' then
      PackedName := ChangeFileExt(ExtractFileName(Volumes[0].FileName), AItem.PackedExtension);

    FileName := PathGetRelativePath(FDestinationDir, PackedName);
  end;
  Result := True;

  if Assigned(FOnExtract) then
    Result := FOnExtract(Self, Index, FileName, AStream, AOwnsStream);

  if Result and not Assigned(AStream) and AutoCreateSubDir then
  begin
    if (AItem.Attributes and faDirectory) <> 0 then
      ForceDirectories(FileName)
    else
      ForceDirectories(ExtractFilePath(FileName));
  end;
end;

class function TJclUpdateArchive.VolumeAccess: TJclStreamAccess;
begin
  Result := saReadOnly;
end;

//=== { TJclOutOfPlaceUpdateArchive } ========================================

procedure TJclOutOfPlaceUpdateArchive.Compress;
var
  Index: Integer;
  AVolume: TJclCompressionVolume;
  SrcFileName, DestFileName: TFileName;
  SrcStream, DestStream: TStream;
  OwnsSrcStream, OwnsDestStream, AllHandled, Handled: Boolean;
  CopiedSize: Int64;
begin
  // release volume streams and other finalization
  inherited Compress;

  if ReplaceVolumes then
  begin
    AllHandled := True;

    // replace streams by tmp streams
    for Index := 0 to FVolumes.Count - 1 do
    begin
      AVolume := TJclCompressionVolume(FVolumes.Items[Index]);

      SrcFileName := AVolume.TmpFileName;
      DestFileName := AVolume.FileName;
      SrcStream := AVolume.TmpStream;
      DestStream := AVolume.Stream;
      OwnsSrcStream := AVolume.OwnsTmpStream;
      OwnsDestStream := AVolume.OwnsStream;

      Handled := Assigned(FOnReplace) and FOnReplace(Self, SrcFileName, DestFileName, SrcStream, DestStream, OwnsSrcStream, OwnsDestStream);

      if not Handled then
      begin
        if (SrcFileName <> '') and (DestFileName <> '') and
           (OwnsSrcStream or not Assigned(SrcStream)) and
           (OwnsDestStream or not Assigned(DestStream)) then
        begin
          // close references before moving files
          if OwnsSrcStream then
            FreeAndNil(SrcStream);
          if OwnsDestStream then
            FreeAndNil(DestStream);
          Handled := FileMove(SrcFileName, DestFileName, True);
        end
        else
        if (SrcFileName = '') and (DestFileName = '') and Assigned(SrcStream) and Assigned(DestStream) then
        begin
          // in-memory moves
          StreamSeek(SrcStream, 0, soBeginning);
          StreamSeek(DestStream, 0, soBeginning);
          CopiedSize := StreamCopy(SrcStream, DestStream);
          // reset size
          DestStream.Size := CopiedSize;
        end;
        // identity
        // else
        //   Handled := False;
      end;

      // update volume information
      AVolume.FTmpStream := SrcStream;
      AVolume.FStream := DestStream;
      AVolume.FOwnsTmpStream := OwnsSrcStream;
      AVolume.FOwnsStream := OwnsDestStream;
      AVolume.FTmpFileName := SrcFileName;
      AVolume.FFileName := DestFileName;

      AllHandled := AllHandled and Handled;
    end;
    if not AllHandled then
      raise EJclCompressionError.CreateRes(@RsCompressionReplaceError);
  end;
end;

constructor TJclOutOfPlaceUpdateArchive.Create(Volume0: TStream;
  AVolumeMaxSize: Int64; AOwnVolume: Boolean);
begin
  inherited Create(Volume0, AVolumeMaxSize, AOwnVolume);
  FReplaceVolumes := True;
  FTmpVolumeIndex := -1;
end;

constructor TJclOutOfPlaceUpdateArchive.Create(const VolumeFileName: TFileName;
  AVolumeMaxSize: Int64; VolumeMask: Boolean);
begin
  inherited Create(VolumeFileName, AVolumeMaxSize, VolumeMask);
  FReplaceVolumes := True;
  FTmpVolumeIndex := -1;
end;

function TJclOutOfPlaceUpdateArchive.InternalOpenTmpStream(
  const FileName: TFileName): TStream;
begin
  Result := OpenFileStream(FileName, TmpVolumeAccess);
end;

function TJclOutOfPlaceUpdateArchive.NeedTmpStream(Index: Integer): TStream;
var
  AVolume: TJclCompressionVolume;
  AOwnsStream: Boolean;
  AFileName: TFileName;
begin
  Result := nil;

  if Index <> FTmpVolumeIndex then
  begin
    AOwnsStream := VolumeFileNameMask <> '';
    AVolume := nil;
    AFileName := FindUnusedFileName(Format(VolumeFileNameMask, [Index + VolumeIndexOffset]), '.tmp');
    if (Index >= 0) and (Index < FVolumes.Count) then
    begin
      AVolume := TJclCompressionVolume(FVolumes.Items[Index]);
      Result := AVolume.TmpStream;
      AOwnsStream := AVolume.OwnsTmpStream;
      AFileName := AVolume.TmpFileName;
      if AFileName = '' then
        AFileName := FindUnusedFileName(AVolume.FileName, '.tmp');
    end;

    if Assigned(FOnTmpVolume) then
      FOnTmpVolume(Self, Index, AFileName, Result, AOwnsStream);

    if Assigned(AVolume) then
    begin
      if not Assigned(Result) then
        Result := InternalOpenTmpStream(AFileName);
      AVolume.FTmpFileName := AFileName;
      AVolume.FTmpStream := Result;
      AVolume.FOwnsTmpStream := AOwnsStream;
    end
    else
    begin
      while FVolumes.Count < Index do
        FVolumes.Add(TJclCompressionVolume.Create(nil, nil, True, True, Format(VolumeFileNameMask, [Index + VolumeIndexOffset]), '', FVolumeMaxSize));
      if not Assigned(Result) then
        Result := InternalOpenTmpStream(AFileName);
      if Assigned(Result) then
      begin
        if Index < FVolumes.Count then
        begin
          AVolume := TJclCompressionVolume(FVolumes.Items[Index]);
          AVolume.FTmpFileName := AFileName;
          AVolume.FTmpStream := Result;
          AVolume.FOwnsTmpStream := AOwnsStream;
          AVolume.FVolumeMaxSize := FVolumeMaxSize;
        end
        else
          FVolumes.Add(TJclCompressionVolume.Create(nil, Result, True, AOwnsStream, '', AFileName, FVolumeMaxSize));
      end;
    end;
    FTmpVolumeIndex := Index;
  end
  else
  if (Index >= 0) and (Index < FVolumes.Count) then
  begin
    AVolume := TJclCompressionVolume(FVolumes.Items[Index]);
    Result := AVolume.TmpStream;
    if Assigned(Result) then
      StreamSeek(Result, 0, soBeginning);
  end
  else
    FTmpVolumeIndex := Index;
end;

class function TJclOutOfPlaceUpdateArchive.TmpVolumeAccess: TJclStreamAccess;
begin
  Result := saWriteOnly;
end;

//=== { TJclSevenzipOutStream } ==============================================

type
  TJclSevenzipOutStream = class(TInterfacedObject, ISequentialOutStream,
    IOutStream, IUnknown)
  private
    FArchive: TJclCompressionArchive;
    FItemIndex: Integer;
    FStream: TStream;
    FOwnsStream: Boolean;
    FTruncateOnRelease: Boolean;
    FMaximumPosition: Int64;
    procedure NeedStream;
    procedure ReleaseStream;
  public
    constructor Create(AArchive: TJclCompressionArchive; AItemIndex: Integer); overload;
    constructor Create(AStream: TStream; AOwnsStream: Boolean; ATruncateOnRelease: Boolean); overload;
    destructor Destroy; override;
    // ISequentialOutStream
    function Write(Data: Pointer; Size: Cardinal; ProcessedSize: PCardinal): HRESULT; stdcall;
    // IOutStream
    function Seek(Offset: Int64; SeekOrigin: Cardinal; NewPosition: PInt64): HRESULT; stdcall;
    function SetSize(NewSize: Int64): HRESULT; stdcall;
  end;

constructor TJclSevenzipOutStream.Create(AArchive: TJclCompressionArchive; AItemIndex: Integer);
begin
  inherited Create;

  FArchive := AArchive;
  FItemIndex := AItemIndex;
  FStream := nil;
  FOwnsStream := False;
  FMaximumPosition := 0;
  FTruncateOnRelease := False;
end;

constructor TJclSevenzipOutStream.Create(AStream: TStream; AOwnsStream: Boolean; ATruncateOnRelease: Boolean);
begin
  inherited Create;

  FArchive := nil;
  FItemIndex := -1;
  FStream := AStream;
  FOwnsStream := AOwnsStream;
  FMaximumPosition := 0;
  FTruncateOnRelease := ATruncateOnRelease;
end;

destructor TJclSevenzipOutStream.Destroy;
begin
  ReleaseStream;

  inherited Destroy;
end;

procedure TJclSevenzipOutStream.NeedStream;
begin
  if Assigned(FArchive) and not Assigned(FStream) then
    FStream := FArchive.Items[FItemIndex].Stream;

  if not Assigned(FStream) then
    raise EJclCompressionError.CreateRes(@RsCompression7zUnassignedStream);
end;

procedure TJclSevenzipOutStream.ReleaseStream;
begin
  // truncate to the maximum position that was written
  if FTruncateOnRelease then
    FStream.Size := FMaximumPosition;

  if Assigned(FArchive) then
    FArchive.Items[FItemIndex].ReleaseStream
  else
  if FOwnsStream then
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
  if FTruncateOnRelease and (FMaximumPosition < NewSize) then
    FMaximumPosition := NewSize;
end;

function TJclSevenzipOutStream.Write(Data: Pointer; Size: Cardinal;
  ProcessedSize: PCardinal): HRESULT;
var
  Processed: Cardinal;
  APosition: Int64;
begin
  NeedStream;

  Result := S_OK;
  Processed := FStream.Write(Data^, Size);
  if Assigned(ProcessedSize) then
    ProcessedSize^ := Processed;
  if FTruncateOnRelease then
  begin
    APosition := FStream.Position;
    if FMaximumPosition < APosition then
      FMaximumPosition := APosition;
  end;
end;

//=== { TJclSevenzipInStream } ===============================================

type
  TJclSevenzipInStream = class(TInterfacedObject, ISequentialInStream,
    IInStream, IStreamGetSize, IUnknown)
  private
    FArchive: TJclCompressionArchive;
    FItemIndex: Integer;
    FStream: TStream;
    FOwnsStream: Boolean;
    procedure NeedStream;
    procedure ReleaseStream;
  public
    constructor Create(AArchive: TJclCompressionArchive; AItemIndex: Integer); overload;
    constructor Create(AStream: TStream; AOwnsStream: Boolean); overload;
    destructor Destroy; override;
    // ISequentialInStream
    function Read(Data: Pointer; Size: Cardinal; ProcessedSize: PCardinal): HRESULT; stdcall;
    // IInStream
    function Seek(Offset: Int64; SeekOrigin: Cardinal; NewPosition: PInt64): HRESULT; stdcall;
    // IStreamGetSize
    function GetSize(Size: PInt64): HRESULT; stdcall;
  end;

constructor TJclSevenzipInStream.Create(AArchive: TJclCompressionArchive; AItemIndex: Integer);
begin
  inherited Create;

  FArchive := AArchive;
  FItemIndex := AItemIndex;
  FStream := nil;
  FOwnsStream := False;
end;

constructor TJclSevenzipInStream.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create;

  FArchive := nil;
  FItemIndex := -1;
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
    FStream := FArchive.Items[FItemIndex].Stream;

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
    FArchive.Items[FItemIndex].ReleaseStream
  else
  if FOwnsStream then
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

type
  TWideStringSetter = procedure (const Value: WideString) of object;
  TCardinalSetter = procedure (Value: Cardinal) of object;
  TInt64Setter = procedure (const Value: Int64) of object;
  TFileTimeSetter = procedure (const Value: TFileTime) of object;

function Get7zWideStringProp(const AArchive: IInArchive; ItemIndex: Integer;
  PropID: Cardinal; const Setter: TWideStringSetter): Boolean;
var
  Value: TPropVariant;
begin
  ZeroMemory(@Value, SizeOf(Value));
  SevenzipCheck(AArchive.GetProperty(ItemIndex, PropID, Value));
  case Value.vt of
    VT_EMPTY, VT_NULL:
      Result := False;
    VT_LPSTR:
      begin
        Result := True;
        Setter(WideString(AnsiString(Value.pszVal)));
      end;
    VT_LPWSTR:
      begin
        Result := True;
        Setter(Value.pwszVal);
      end;
    VT_BSTR:
      begin
        Result := True;
        Setter(Value.bstrVal);
        SysFreeString(Value.bstrVal);
      end;
    VT_I1:
      begin
        Result := True;
        Setter(IntToStr(Value.iVal));
      end;
    VT_I2:
      begin
        Result := True;
        Setter(IntToStr(Value.iVal));
      end;
    VT_INT, VT_I4:
      begin
        Result := True;
        Setter(IntToStr(Value.lVal));
      end;
    VT_I8:
      begin
        Result := True;
        Setter(IntToStr(Value.hVal.QuadPart));
      end;
    VT_UI1:
      begin
        Result := True;
        Setter(IntToStr(Value.bVal));
      end;
    VT_UI2:
      begin
        Result := True;
        Setter(IntToStr(Value.uiVal));
      end;
    VT_UINT, VT_UI4:
      begin
        Result := True;
        Setter(IntToStr(Value.ulVal));
      end;
    VT_UI8:
      begin
        Result := True;
        Setter(IntToStr(Value.uhVal.QuadPart));
      end;
  else
    raise EJclCompressionError.CreateResFmt(@RsCompression7zUnknownValueType, [Value.vt, PropID]);
  end;
end;

function Get7zCardinalProp(const AArchive: IInArchive; ItemIndex: Integer;
  PropID: Cardinal; const Setter: TCardinalSetter): Boolean;
var
  Value: TPropVariant;
begin
  ZeroMemory(@Value, SizeOf(Value));
  SevenzipCheck(AArchive.GetProperty(ItemIndex, PropID, Value));
  case Value.vt of
    VT_EMPTY, VT_NULL:
      Result := False;
    VT_I1, VT_I2, VT_INT, VT_I4, VT_I8,
    VT_UI1, VT_UI2, VT_UINT, VT_UI4, VT_UI8:
      begin
        Result := True;
        case Value.vt of
          VT_I1:
            Setter(Value.iVal);
          VT_I2:
            Setter(Value.iVal);
          VT_INT, VT_I4:
            Setter(Value.lVal);
          VT_I8:
            Setter(Value.hVal.QuadPart);
          VT_UI1:
            Setter(Value.bVal);
          VT_UI2:
            Setter(Value.uiVal);
          VT_UINT, VT_UI4:
            Setter(Value.ulVal);
          VT_UI8:
            Setter(Value.uhVal.QuadPart);
        end;
      end;
  else
    raise EJclCompressionError.CreateResFmt(@RsCompression7zUnknownValueType, [Value.vt, PropID]);
  end;
end;

function Get7zInt64Prop(const AArchive: IInArchive; ItemIndex: Integer;
  PropID: Cardinal; const Setter: TInt64Setter): Boolean;
var
  Value: TPropVariant;
begin
  ZeroMemory(@Value, SizeOf(Value));
  SevenzipCheck(AArchive.GetProperty(ItemIndex, PropID, Value));
  case Value.vt of
    VT_EMPTY, VT_NULL:
      Result := False;
    VT_I1, VT_I2, VT_INT, VT_I4, VT_I8,
    VT_UI1, VT_UI2, VT_UINT, VT_UI4, VT_UI8:
      begin
        Result := True;
        case Value.vt of
          VT_I1:
            Setter(Value.iVal);
          VT_I2:
            Setter(Value.iVal);
          VT_INT, VT_I4:
            Setter(Value.lVal);
          VT_I8:
            Setter(Value.hVal.QuadPart);
          VT_UI1:
            Setter(Value.bVal);
          VT_UI2:
            Setter(Value.uiVal);
          VT_UINT, VT_UI4:
            Setter(Value.ulVal);
          VT_UI8:
            Setter(Value.uhVal.QuadPart);
        end;
      end;
  else
    raise EJclCompressionError.CreateResFmt(@RsCompression7zUnknownValueType, [Value.vt, PropID]);
  end;
end;

function Get7zFileTimeProp(const AArchive: IInArchive; ItemIndex: Integer;
  PropID: Cardinal; const Setter: TFileTimeSetter): Boolean;
var
  Value: TPropVariant;
begin
  ZeroMemory(@Value, SizeOf(Value));
  SevenzipCheck(AArchive.GetProperty(ItemIndex, PropID, Value));
  case Value.vt of
    VT_EMPTY, VT_NULL:
      Result := False;
    VT_FILETIME:
      begin
        Result := True;
        Setter(Value.filetime);
      end;
  else
    raise EJclCompressionError.CreateResFmt(@RsCompression7zUnknownValueType, [Value.vt, PropID]);
  end;
end;

// TODO: Are changes for UTF-8 filenames (>= 4.58 beta) necessary?
procedure Load7zFileAttribute(AInArchive: IInArchive; ItemIndex: Integer;
  AItem: TJclCompressionItem);
begin
  AItem.FValidProperties := [];
  AItem.FPackedIndex := ItemIndex;
  AItem.FileName := '';
  AItem.Stream := nil;
  AItem.OwnsStream := False;

  // sometimes, items have neither names nor extension although other properties may succeed
  Get7zWideStringProp(AInArchive, ItemIndex, kpidPath, AItem.SetPackedName);
  Get7zWideStringProp(AInArchive, ItemIndex, kpidExtension, AItem.SetPackedExtension);
  Get7zCardinalProp(AInArchive, ItemIndex, kpidAttrib, AItem.SetAttributes);
  Get7zInt64Prop(AInArchive, ItemIndex, kpidSize, AItem.SetFileSize);
  Get7zInt64Prop(AInArchive, ItemIndex, kpidPackSize, AItem.SetPackedSize);
  Get7zFileTimeProp(AInArchive, ItemIndex, kpidCTime, AItem.SetCreationTime);
  Get7zFileTimeProp(AInArchive, ItemIndex, kpidATime, AItem.SetLastAccessTime);
  Get7zFileTimeProp(AInArchive, ItemIndex, kpidMTime, AItem.SetLastWriteTime);
  Get7zWideStringProp(AInArchive, ItemIndex, kpidComment, AItem.SetComment);
  Get7zWideStringProp(AInArchive, ItemIndex, kpidHostOS, AItem.SetHostOS);
  Get7zWideStringProp(AInArchive, ItemIndex, kpidFileSystem, AItem.SetHostFS);
  Get7zWideStringProp(AInArchive, ItemIndex, kpidUser, AItem.SetUser);
  Get7zWideStringProp(AInArchive, ItemIndex, kpidGroup, AItem.SetGroup);
  Get7zCardinalProp(AInArchive, ItemIndex, kpidCRC, AItem.SetCRC);
  Get7zWideStringProp(AInArchive, ItemIndex, kpidMethod, AItem.SetMethod);

  // reset modified flags
  AItem.ModifiedProperties := [];
end;

procedure GetSevenzipArchiveCompressionProperties(AJclArchive: IInterface; ASevenzipArchive: IInterface);
begin
  // TODO properties from ASevenzipArchive to AJclArchive
end;

procedure SetSevenzipArchiveCompressionProperties(AJclArchive: IInterface; ASevenzipArchive: IInterface);
var
  PropertySetter: Sevenzip.ISetProperties;
  InArchive, OutArchive: Boolean;
  Unused: IInterface;
  MultiThreadStrategy: IJclArchiveNumberOfThreads;
  CompressionLevel: IJclArchiveCompressionLevel;
  EncryptionMethod: IJclArchiveEncryptionMethod;
  DictionarySize: IJclArchiveDictionarySize;
  NumberOfPasses: IJclArchiveNumberOfPasses;
  RemoveSfxBlock: IJclArchiveRemoveSfxBlock;
  CompressHeader: IJclArchiveCompressHeader;
  EncryptHeader: IJclArchiveEncryptHeader;
  SaveCreationDateTime: IJclArchiveSaveCreationDateTime;
  SaveLastAccessDateTime: IJclArchiveSaveLastAccessDateTime;
  SaveLastWriteDateTime: IJclArchiveSaveLastWriteDateTime;
  Algorithm: IJclArchiveAlgorithm;
  Solid: IJclArchiveSolid;
  PropNames: array of PWideChar;
  PropValues: array of TPropVariant;

  procedure AddProperty(const Name: PWideChar; const Value: TPropVariant);
  begin
    SetLength(PropNames, Length(PropNames)+1);
    PropNames[High(PropNames)] := Name;
    SetLength(PropValues, Length(PropValues)+1);
    PropValues[High(PropValues)] := Value;
  end;

  procedure AddCardinalProperty(const Name: PWideChar; Value: Cardinal);
  var
    PropValue: TPropVariant;
  begin
    PropValue.vt := VT_UI4;
    PropValue.ulVal := Value;
    AddProperty(Name, PropValue);
  end;

  procedure AddWideStringProperty(const Name: PWideChar; const Value: WideString);
  var
    PropValue: TPropVariant;
  begin
    PropValue.vt := VT_BSTR;
    PropValue.bstrVal := SysAllocString(PWideChar(Value));
    AddProperty(Name, PropValue);
  end;

  procedure AddBooleanProperty(const Name: PWideChar; Value: Boolean);
  var
    PropValue: TPropVariant;
  const
    BooleanValues: array [False..True] of WideString = ( 'OFF', 'ON' );
  begin
    PropValue.vt := VT_BSTR;
      PropValue.bstrVal := SysAllocString(PWideChar(BooleanValues[Value]));
    AddProperty(Name, PropValue);
  end;
const
  EncryptionMethodName: array [TJclEncryptionMethod] of WideString =
    ( '' {emNone},
      kAES128MethodName {emAES128},
      kAES192MethodName {emAES192},
      kAES256MethodName {emAES256},
      kZipCryptoMethodName {emZipCrypto} );
  CompressionMethodNames: array [TJclCompressionMethod] of WideString =
    ( kCopyMethodName {cmCopy},
      kDeflateMethodName {cmDeflate},
      kDeflate64MethodName {cmDeflate64},
      kBZip2MethodName {cmBZip2},
      kLZMAMethodName {cmLZMA},
      kLZMA2MethodName {cmLZMA2},
      kPPMdMethodName {cmPPMd} );
begin
  if Supports(ASevenzipArchive, Sevenzip.ISetProperties, PropertySetter) and Assigned(PropertySetter) then
  begin
    InArchive := Supports(ASevenzipArchive, Sevenzip.IInArchive, Unused);
    OutArchive := Supports(ASevenzipArchive, Sevenzip.IOutArchive, Unused);
    if (InArchive or OutArchive) and Supports(AJclArchive, IJclArchiveNumberOfThreads, MultiThreadStrategy)
      and Assigned(MultiThreadStrategy) and (MultiThreadStrategy.NumberOfThreads > 1) then
      AddCardinalProperty('MT', MultiThreadStrategy.NumberOfThreads);

    if OutArchive then
    begin
      if Supports(AJclArchive, IJclArchiveCompressionLevel, CompressionLevel) and Assigned(CompressionLevel) then
        AddCardinalProperty('X', CompressionLevel.CompressionLevel);

      if Supports(AJclArchive, IJclArchiveEncryptionMethod, EncryptionMethod) and Assigned(EncryptionMethod)
        and (EncryptionMethod.EncryptionMethod <> emNone) then
        AddWideStringProperty('EM', EncryptionMethodName[EncryptionMethod.EncryptionMethod]);

      if Supports(AJclArchive, IJclArchiveDictionarySize, DictionarySize) and Assigned(DictionarySize) then
        AddWideStringProperty('D', IntToStr(DictionarySize.DictionarySize) + 'B');

      if Supports(AJclArchive, IJclArchiveNumberOfPasses, NumberOfPasses) and Assigned(NumberOfPasses) then
        AddCardinalProperty('PASS', NumberOfPasses.NumberOfPasses);

      if Supports(AJclArchive, IJclArchiveRemoveSfxBlock, RemoveSfxBlock) and Assigned(RemoveSfxBlock) then
        AddBooleanProperty('RSFX', RemoveSfxBlock.RemoveSfxBlock);

      if Supports(AJclArchive, IJclArchiveCompressHeader, CompressHeader) and Assigned(CompressHeader) then
      begin
        AddBooleanProperty('HC', CompressHeader.CompressHeader);
        if CompressHeader.CompressHeaderFull then
          AddBooleanProperty('HCF', CompressHeader.CompressHeaderFull);
      end;

      if Supports(AJclArchive, IJclArchiveEncryptHeader, EncryptHeader) and Assigned(EncryptHeader) then
        AddBooleanProperty('HE', EncryptHeader.EncryptHeader);

      if Supports(AJclArchive, IJclArchiveSaveCreationDateTime, SaveCreationDateTime)
        and Assigned(SaveCreationDateTime) then
        AddBooleanProperty('TC', SaveCreationDateTime.SaveCreationDateTime);

      if Supports(AJclArchive, IJclArchiveSaveLastAccessDateTime, SaveLastAccessDateTime)
        and Assigned(SaveLastAccessDateTime) then
        AddBooleanProperty('TA', SaveLastAccessDateTime.SaveLastAccessDateTime);

      if Supports(AJclArchive, IJclArchiveSaveLastWriteDateTime, SaveLastWriteDateTime)
        and Assigned(SaveLastWriteDateTime) then
        AddBooleanProperty('TM', SaveLastWriteDateTime.SaveLastWriteDateTime);

      if Supports(AJclArchive, IJclArchiveAlgorithm, Algorithm) and Assigned(Algorithm) then
        AddCardinalProperty('A', Algorithm.Algorithm);

      if Supports(AJclArchive, IJclArchiveSolid, Solid) and Assigned(Solid) then
      begin
        if Solid.SolidExtension then
          AddWideStringProperty('S', 'E');
        if Solid.SolidBlockSize > 0 then
          AddWideStringProperty('S', IntToStr(Solid.SolidBlockSize) + 'B')
        else
          AddWideStringProperty('S', IntToStr(Solid.SolidBlockSize) + 'F');
      end;
    end;
    if Length(PropNames) > 0 then
      SevenZipCheck(PropertySetter.SetProperties(@PropNames[0], @PropValues[0], Length(PropNames)));
  end;
end;

//=== { TJclSevenzipOutputCallback } =========================================

type
  TJclSevenzipUpdateCallback = class(TInterfacedObject, IUnknown, IProgress,
    IArchiveUpdateCallback, IArchiveUpdateCallback2, ICryptoGetTextPassword2)
  private
    FArchive: TJclCompressionArchive;
    FLastStream: Cardinal;
  public
    constructor Create(AArchive: TJclCompressionArchive);
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
  AArchive: TJclCompressionArchive);
begin
  inherited Create;
  FArchive := AArchive;
end;

function TJclSevenzipUpdateCallback.CryptoGetTextPassword2(
  PasswordIsDefined: PInteger; Password: PBStr): HRESULT;
begin
  if Assigned(PasswordIsDefined) then
  begin
    if FArchive.Password <> '' then
      PasswordIsDefined^ := Integer($FFFFFFFF)
    else
      PasswordIsDefined^ := 0;
  end;
  if Assigned(Password) then
    Password^ := SysAllocString(PWideChar(FArchive.Password));
  Result := S_OK;
end;

function TJclSevenzipUpdateCallback.GetProperty(Index, PropID: Cardinal;
  out Value: tagPROPVARIANT): HRESULT;
var
  AItem: TJclCompressionItem;
begin
  Result := S_OK;
  AItem := FArchive.Items[Index];
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
    kpidIsDir:
      begin
        Value.vt := VT_BOOL;
        Value.bool := AItem.Kind = ikDirectory;
      end;
    kpidSize:
      begin
        Value.vt := VT_UI8;
        Value.uhVal.QuadPart := AItem.FileSize;
      end;
    //kpidPackSize:
    kpidAttrib:
      begin
        Value.vt := VT_UI4;
        Value.ulVal := AItem.Attributes;
      end;
    kpidCTime:
      begin
        Value.vt := VT_FILETIME;
        Value.filetime := AItem.CreationTime;
      end;
    kpidATime:
      begin
        Value.vt := VT_FILETIME;
        Value.filetime := AItem.LastAccessTime;
      end;
    kpidMTime:
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
    kpidTimeType:
      begin
        Value.vt := VT_UI4;
        Value.ulVal := kWindows;
      end;
  else
    Value.vt := VT_EMPTY;
    Result := S_FALSE;
  end;
end;

function TJclSevenzipUpdateCallback.GetStream(Index: Cardinal;
  out InStream: ISequentialInStream): HRESULT;
begin
  FLastStream := Index;
  InStream := TJclSevenzipInStream.Create(FArchive, Index);
  Result := S_OK;
end;

function TJclSevenzipUpdateCallback.GetUpdateItemInfo(Index: Cardinal; NewData,
  NewProperties: PInteger; IndexInArchive: PCardinal): HRESULT;
var
  CompressionItem: TJclCompressionItem;
begin
  CompressionItem := FArchive.Items[Index];

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
    IndexInArchive^ := CompressionItem.PackedIndex;
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
    FArchive.DoProgress(CompleteValue^, FArchive.FProgressMax);
  Result := S_OK;
end;

function TJclSevenzipUpdateCallback.SetOperationResult(
  OperationResult: Integer): HRESULT;
begin
  case OperationResult of
    kOK:
      FArchive.Items[FLastStream].OperationSuccess := osOK;
    kUnSupportedMethod:
      FArchive.Items[FLastStream].OperationSuccess := osUnsupportedMethod;
    kDataError:
      FArchive.Items[FLastStream].OperationSuccess := osDataError;
    kCRCError:
      FArchive.Items[FLastStream].OperationSuccess := osCRCError;
  else
    FArchive.Items[FLastStream].OperationSuccess := osUnknownError;
  end;

  Result := S_OK;
end;

function TJclSevenzipUpdateCallback.SetTotal(Total: Int64): HRESULT;
begin
  FArchive.FProgressMax := Total;
  Result := S_OK;
end;

//=== { TJclSevenzipCompressArchive } ========================================

procedure TJclSevenzipCompressArchive.CreateCompressionObject;
var
  SevenzipCLSID, InterfaceID: TGUID;
begin
  SevenzipCLSID := GetCLSID;
  InterfaceID := Sevenzip.IOutArchive;
  if (not Is7ZipLoaded) and (not Load7Zip) then
    raise EJclCompressionError.CreateRes(@RsCompression7zLoadError);
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

procedure TJclSevenzipCompressArchive.Compress;
var
  OutStream: IOutStream;
  UpdateCallback: IArchiveUpdateCallback;
  SplitStream: TJclDynamicSplitStream;
begin
  CheckNotCompressing;

  FCompressing := True;
  try
    SplitStream := TJclDynamicSplitStream.Create(False);
    SplitStream.OnVolume := NeedStream;
    SplitStream.OnVolumeMaxSize := NeedStreamMaxSize;
    OutStream := TJclSevenzipOutStream.Create(SplitStream, True, False);
    UpdateCallback := TJclSevenzipUpdateCallback.Create(Self);

    SetSevenzipArchiveCompressionProperties(Self, FOutArchive);

    SevenzipCheck(FOutArchive.UpdateItems(OutStream, ItemCount, UpdateCallback));
  finally
    FCompressing := False;
    // release volumes and other finalizations
    inherited Compress;
  end;
end;

//=== { TJcl7zCompressArchive } ==============================================

class function TJcl7zCompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompression7zExtensions);
end;

class function TJcl7zCompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompression7zName);
end;

procedure TJcl7zCompressArchive.CreateCompressionObject;
begin
  inherited CreateCompressionObject;
  FNumberOfThreads := 1;
  FEncryptHeader := False;
  FRemoveSfxBlock := False;
  FDictionarySize := kLzmaDicSizeX5;
  FCompressionLevel := 6;
  FCompressHeader := False;
  FCompressHeaderFull := False;
  FSaveLastAccessDateTime := True;
  FSaveCreationDateTime := True;
  FSaveLastWriteDateTime := True;
  FSolidBlockSize := High(Cardinal);
  FSolidExtension := False;
end;

function TJcl7zCompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormat7z;
end;

function TJcl7zCompressArchive.GetCompressHeader: Boolean;
begin
  Result := FCompressHeader;
end;

function TJcl7zCompressArchive.GetCompressHeaderFull: Boolean;
begin
  Result := FCompressHeaderFull;
end;

function TJcl7zCompressArchive.GetCompressionLevel: Cardinal;
begin
  Result := FCompressionLevel;
end;

function TJcl7zCompressArchive.GetCompressionLevelMax: Cardinal;
begin
  Result := 9;
end;

function TJcl7zCompressArchive.GetCompressionLevelMin: Cardinal;
begin
  Result := 0;
end;

function TJcl7zCompressArchive.GetDictionarySize: Cardinal;
begin
  Result := FDictionarySize;
end;

function TJcl7zCompressArchive.GetEncryptHeader: Boolean;
begin
  Result := FEncryptHeader;
end;

function TJcl7zCompressArchive.GetNumberOfThreads: Cardinal;
begin
  Result := FNumberOfThreads;
end;

function TJcl7zCompressArchive.GetRemoveSfxBlock: Boolean;
begin
  Result := FRemoveSfxBlock;
end;

function TJcl7zCompressArchive.GetSaveCreationDateTime: Boolean;
begin
  Result := FSaveCreationDateTime;
end;

function TJcl7zCompressArchive.GetSaveLastAccessDateTime: Boolean;
begin
  Result := FSaveLastAccessDateTime;
end;

function TJcl7zCompressArchive.GetSaveLastWriteDateTime: Boolean;
begin
  Result := FSaveLastWriteDateTime;
end;

function TJcl7zCompressArchive.GetSolidBlockSize: Int64;
begin
  Result := FSolidBlockSize;
end;

function TJcl7zCompressArchive.GetSolidExtension: Boolean;
begin
  Result := FSolidExtension;
end;

class function TJcl7zCompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

procedure TJcl7zCompressArchive.SetCompressHeader(Value: Boolean);
begin
  CheckNotCompressing;
  FCompressHeader := Value;
end;

procedure TJcl7zCompressArchive.SetCompressHeaderFull(Value: Boolean);
begin
  CheckNotCompressing;
  FCompressHeaderFull := Value;
end;

procedure TJcl7zCompressArchive.SetCompressionLevel(Value: Cardinal);
begin
  CheckNotCompressing;
  if Value <= 9 then
  begin
    FCompressionLevel := Value;
    if Value >= 9 then
      FDictionarySize := kLzmaDicSizeX9
    else
    if Value >= 7 then
      FDictionarySize := kLzmaDicSizeX7
    else
    if Value >= 5 then
      FDictionarySize := kLzmaDicSizeX5
    else
    if Value >= 3 then
      FDictionarySize := kLzmaDicSizeX3
    else
      FDictionarySize := kLzmaDicSizeX1;
  end
  else
    raise EJclCompressionError.CreateRes(@RsCompressionUnavailableProperty);
end;

procedure TJcl7zCompressArchive.SetDictionarySize(Value: Cardinal);
begin
  CheckNotCompressing;
  FDictionarySize := Value;
end;

procedure TJcl7zCompressArchive.SetEncryptHeader(Value: Boolean);
begin
  CheckNotCompressing;
  FEncryptHeader := Value;
end;

procedure TJcl7zCompressArchive.SetNumberOfThreads(Value: Cardinal);
begin
  CheckNotCompressing;
  FNumberOfThreads := Value;
end;

procedure TJcl7zCompressArchive.SetRemoveSfxBlock(Value: Boolean);
begin
  CheckNotCompressing;
  FRemoveSfxBlock := Value;
end;

procedure TJcl7zCompressArchive.SetSaveCreationDateTime(Value: Boolean);
begin
  CheckNotCompressing;
  FSaveCreationDateTime := Value;
end;

procedure TJcl7zCompressArchive.SetSaveLastAccessDateTime(Value: Boolean);
begin
  CheckNotCompressing;
  FSaveLastAccessDateTime := Value;
end;

procedure TJcl7zCompressArchive.SetSaveLastWriteDateTime(Value: Boolean);
begin
  CheckNotCompressing;
  FSaveLastWriteDateTime := Value;
end;

procedure TJcl7zCompressArchive.SetSolidBlockSize(const Value: Int64);
begin
  CheckNotCompressing;
  FSolidBlockSize := Value;
end;

procedure TJcl7zCompressArchive.SetSolidExtension(Value: Boolean);
begin
  CheckNotCompressing;
  FSolidExtension := Value;
end;

//=== { TJclZipCompressArchive } =============================================

class function TJclZipCompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionZipExtensions);
end;

class function TJclZipCompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionZipName);
end;

procedure TJclZipCompressArchive.CreateCompressionObject;
begin
  inherited CreateCompressionObject;
  FNumberOfThreads := 1;
  FEncryptionMethod := emZipCrypto;
  FDictionarySize := kBZip2DicSizeX5;
  FCompressionLevel := 7;
  FCompressionMethod := cmDeflate;
  FNumberOfPasses := kDeflateNumPassesX7;
  FAlgorithm := kLzAlgoX5;
end;

function TJclZipCompressArchive.GetAlgorithm: Cardinal;
begin
  Result := FAlgorithm;
end;

function TJclZipCompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatZip;
end;

function TJclZipCompressArchive.GetCompressionLevel: Cardinal;
begin
  Result := FCompressionLevel;
end;

function TJclZipCompressArchive.GetCompressionLevelMax: Cardinal;
begin
  Result := 9;
end;

function TJclZipCompressArchive.GetCompressionLevelMin: Cardinal;
begin
  Result := 0;
end;

function TJclZipCompressArchive.GetCompressionMethod: TJclCompressionMethod;
begin
  Result := FCompressionMethod;
end;

function TJclZipCompressArchive.GetDictionarySize: Cardinal;
begin
  Result := FDictionarySize;
end;

function TJclZipCompressArchive.GetEncryptionMethod: TJclEncryptionMethod;
begin
  Result := FEncryptionMethod;
end;

function TJclZipCompressArchive.GetNumberOfPasses: Cardinal;
begin
  Result := FNumberOfPasses;
end;

function TJclZipCompressArchive.GetNumberOfThreads: Cardinal;
begin
  Result := FNumberOfThreads;
end;

function TJclZipCompressArchive.GetSupportedAlgorithms: TDynCardinalArray;
begin
  SetLength(Result, 2);
  Result[0] := 0;
  Result[1] := 1;
end;

function TJclZipCompressArchive.GetSupportedCompressionMethods: TJclCompressionMethods;
begin
  Result := [cmCopy,cmDeflate,cmDeflate64,cmBZip2,cmLZMA];
end;

function TJclZipCompressArchive.GetSupportedEncryptionMethods: TJclEncryptionMethods;
begin
  Result := [emNone,emAES128,emAES192,emAES256,emZipCrypto];
end;

class function TJclZipCompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

procedure TJclZipCompressArchive.SetAlgorithm(Value: Cardinal);
begin
  CheckNotCompressing;
  if (Value = 0) or (Value = 1) then
    FAlgorithm := Value
  else
    raise EJclCompressionError.CreateRes(@RsCompressionUnavailableProperty);
end;

procedure TJclZipCompressArchive.SetCompressionLevel(Value: Cardinal);
begin
  CheckNotCompressing;
  if Value <= 9 then
  begin
    FCompressionLevel := Value;
    case FCompressionMethod of
      cmDeflate, cmDeflate64:
        begin
          if Value >= 9 then
            FNumberOfPasses := kDeflateNumPassesX9
          else
          if Value >= 7 then
            FNumberOfPasses := kDeflateNumPassesX7
          else
            FNumberOfPasses := kDeflateNumPassesX1;
          if Value >= 5 then
            FAlgorithm := kLzAlgoX5
          else
            FAlgorithm := kLzAlgoX1;
        end;
      cmBZip2:
        begin
          if Value >= 9 then
            FNumberOfPasses := kBZip2NumPassesX9
          else
          if Value >= 7 then
            FNumberOfPasses := kBZip2NumPassesX7
          else
            FNumberOfPasses := kBZip2NumPassesX1;
          if Value >= 5 then
            FDictionarySize := kBZip2DicSizeX5
          else
          if Value >= 3 then
            FDictionarySize := kBZip2DicSizeX3
          else
            FDictionarySize := kBZip2DicSizeX1;
        end;
      cmLZMA:
        begin
          if Value >= 9 then
            FDictionarySize := kLzmaDicSizeX9
          else
          if Value >= 7 then
            FDictionarySize := kLzmaDicSizeX7
          else
          if Value >= 5 then
            FDictionarySize := kLzmaDicSizeX5
          else
          if Value >= 3 then
            FDictionarySize := kLzmaDicSizeX3
          else
            FDictionarySize := kLzmaDicSizeX1;
          if Value >= 5 then
            FAlgorithm := kLzAlgoX5
          else
            FAlgorithm := kLzAlgoX1;
        end;
    end;
  end
  else
    raise EJclCompressionError.CreateRes(@RsCompressionUnavailableProperty);
end;

procedure TJclZipCompressArchive.SetCompressionMethod(Value: TJclCompressionMethod);
begin
  CheckNotCompressing;
  if Value in GetSupportedCompressionMethods then
    FCompressionMethod := Value
  else
    raise EJclCompressionError.CreateRes(@RsCompressionUnavailableProperty);
end;

procedure TJclZipCompressArchive.SetDictionarySize(Value: Cardinal);
begin
  CheckNotCompressing;
  FDictionarySize := Value;
end;

procedure TJclZipCompressArchive.SetEncryptionMethod(Value: TJclEncryptionMethod);
begin
  CheckNotCompressing;
  if Value in GetSupportedEncryptionMethods then
    FEncryptionMethod := Value
  else
    raise EJclCompressionError.CreateRes(@RsCompressionUnavailableProperty);
end;

procedure TJclZipCompressArchive.SetNumberOfPasses(Value: Cardinal);
begin
  CheckNotCompressing;
  FNumberOfPasses := Value;
end;

procedure TJclZipCompressArchive.SetNumberOfThreads(Value: Cardinal);
begin
  CheckNotCompressing;
  FNumberOfThreads := Value;
end;

//=== { TJclBZ2CompressArchive } =============================================

class function TJclBZ2CompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionBZip2Extensions);
end;

class function TJclBZ2CompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionBZip2Name);
end;

procedure TJclBZ2CompressArchive.CreateCompressionObject;
begin
  inherited CreateCompressionObject;
  FNumberOfThreads := 1;
  FDictionarySize := kBZip2DicSizeX5;
  FCompressionLevel := 7;
  FNumberOfPasses := kBZip2NumPassesX7;
end;

function TJclBZ2CompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatBZ2;
end;

function TJclBZ2CompressArchive.GetCompressionLevel: Cardinal;
begin
  Result := FCompressionLevel;
end;

function TJclBZ2CompressArchive.GetCompressionLevelMax: Cardinal;
begin
  Result := 9;
end;

function TJclBZ2CompressArchive.GetCompressionLevelMin: Cardinal;
begin
  Result := 0;
end;

function TJclBZ2CompressArchive.GetDictionarySize: Cardinal;
begin
  Result := FDictionarySize;
end;

function TJclBZ2CompressArchive.GetNumberOfPasses: Cardinal;
begin
  Result := FNumberOfPasses;
end;

function TJclBZ2CompressArchive.GetNumberOfThreads: Cardinal;
begin
  Result := FNumberOfThreads;
end;

procedure TJclBZ2CompressArchive.SetCompressionLevel(Value: Cardinal);
begin
  CheckNotCompressing;
  if Value <= 9 then
  begin
    FCompressionLevel := Value;
    if Value >= 9 then
      FNumberOfPasses := kBZip2NumPassesX9
    else
    if Value >= 7 then
      FNumberOfPasses := kBZip2NumPassesX7
    else
      FNumberOfPasses := kBZip2NumPassesX1;
    if Value >= 5 then
      FDictionarySize := kBZip2DicSizeX5
    else
    if Value >= 3 then
      FDictionarySize := kBZip2DicSizeX3
    else
      FDictionarySize := kBZip2DicSizeX1;
  end
  else
    raise EJclCompressionError.CreateRes(@RsCompressionUnavailableProperty);
end;

procedure TJclBZ2CompressArchive.SetDictionarySize(Value: Cardinal);
begin
  CheckNotCompressing;
  FDictionarySize := Value;
end;

procedure TJclBZ2CompressArchive.SetNumberOfPasses(Value: Cardinal);
begin
  CheckNotCompressing;
  FNumberOfPasses := Value;
end;

procedure TJclBZ2CompressArchive.SetNumberOfThreads(Value: Cardinal);
begin
  CheckNotCompressing;
  FNumberOfThreads := Value;
end;

//=== { TJclTarCompressArchive } =============================================

class function TJclTarCompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionTarExtensions);
end;

class function TJclTarCompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionTarName);
end;

function TJclTarCompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatTar;
end;

class function TJclTarCompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclGZipCompressArchive } ============================================

class function TJclGZipCompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionGZipExtensions);
end;

class function TJclGZipCompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionGZipName);
end;

procedure TJclGZipCompressArchive.CreateCompressionObject;
begin
  inherited CreateCompressionObject;
  FCompressionLevel := 7;
  FNumberOfPasses := kDeflateNumPassesX7;
  FAlgorithm := kLzAlgoX5;
end;

function TJclGZipCompressArchive.GetAlgorithm: Cardinal;
begin
  Result := FAlgorithm;
end;

function TJclGZipCompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatGZip;
end;

function TJclGZipCompressArchive.GetCompressionLevel: Cardinal;
begin
  Result := FCompressionLevel;
end;

function TJclGZipCompressArchive.GetCompressionLevelMax: Cardinal;
begin
  Result := 9;
end;

function TJclGZipCompressArchive.GetCompressionLevelMin: Cardinal;
begin
  Result := 0;
end;

function TJclGZipCompressArchive.GetNumberOfPasses: Cardinal;
begin
  Result := FNumberOfPasses;
end;

function TJclGZipCompressArchive.GetSupportedAlgorithms: TDynCardinalArray;
begin
  SetLength(Result,2);
  Result[0] := 0;
  Result[1] := 1;
end;

procedure TJclGZipCompressArchive.SetAlgorithm(Value: Cardinal);
begin
  CheckNotCompressing;
  FAlgorithm := Value;
end;

procedure TJclGZipCompressArchive.SetCompressionLevel(Value: Cardinal);
begin
  CheckNotCompressing;
  if Value <= 9 then
  begin
    if Value >= 9 then
      FNumberOfPasses := kDeflateNumPassesX9
    else
    if Value >= 7 then
      FNumberOfPasses := kDeflateNumPassesX7
    else
      FNumberOfPasses := kDeflateNumPassesX1;
    if Value >= 5 then
      FAlgorithm := kLzAlgoX5
    else
      FAlgorithm := kLzAlgoX1;
  end
  else
    raise EJclCompressionError.CreateRes(@RsCompressionUnavailableProperty);
end;

procedure TJclGZipCompressArchive.SetNumberOfPasses(Value: Cardinal);
begin
  CheckNotCompressing;
  FNumberOfPasses := Value;
end;

//=== { TJclSevenzipOpenCallback } ===========================================

type
  TJclSevenzipOpenCallback = class(TInterfacedObject, IArchiveOpenCallback,
    ICryptoGetTextPassword, IUnknown)
  private
    FArchive: TJclCompressionArchive;
  public
    constructor Create(AArchive: TJclCompressionArchive);
    // IArchiveOpenCallback
    function SetCompleted(Files: PInt64; Bytes: PInt64): HRESULT; stdcall;
    function SetTotal(Files: PInt64; Bytes: PInt64): HRESULT; stdcall;
    // ICryptoGetTextPassword
    function CryptoGetTextPassword(password: PBStr): HRESULT; stdcall;
  end;

constructor TJclSevenzipOpenCallback.Create(
  AArchive: TJclCompressionArchive);
begin
  inherited Create;
  FArchive := AArchive;
end;

function TJclSevenzipOpenCallback.CryptoGetTextPassword(
  password: PBStr): HRESULT;
begin
  if Assigned(password) then
    password^ := SysAllocString(PWideChar(FArchive.Password));
  Result := S_OK;
end;

function TJclSevenzipOpenCallback.SetCompleted(Files, Bytes: PInt64): HRESULT;
begin
  if Assigned(Files) then
    FArchive.DoProgress(Files^, FArchive.FProgressMax);
  Result := S_OK;
end;

function TJclSevenzipOpenCallback.SetTotal(Files, Bytes: PInt64): HRESULT;
begin
  if Assigned(Files) then
    FArchive.FProgressMax := Files^;
  Result := S_OK;
end;

//=== { TJclSevenzipExtractCallback } ========================================

type
  TJclSevenzipExtractCallback = class(TInterfacedObject, IUnknown, IProgress,
    IArchiveExtractCallback, ICryptoGetTextPassword)
  private
    FArchive: TJclCompressionArchive;
    FLastStream: Cardinal;
  public
    constructor Create(AArchive: TJclCompressionArchive);
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
  AArchive: TJclCompressionArchive);
begin
  inherited Create;
  FArchive := AArchive;
end;

function TJclSevenzipExtractCallback.CryptoGetTextPassword(
  password: PBStr): HRESULT;
begin
  if Assigned(password) then
    password^ := SysAllocString(PWideChar(FArchive.Password));
  Result := S_OK;
end;

function TJclSevenzipExtractCallback.GetStream(Index: Cardinal;
  out OutStream: ISequentialOutStream; askExtractMode: Cardinal): HRESULT;
begin
  FLastStream := Index;

  Assert(askExtractMode in [kExtract, kTest, kSkip]);

  if askExtractMode in [kTest, kSkip] then
  begin
    OutStream := nil;
    Result := S_OK;
  end
  else
  if FArchive.Items[Index].ValidateExtraction(Index) then
  begin
    OutStream := TJclSevenzipOutStream.Create(FArchive, Index);
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
    FArchive.DoProgress(CompleteValue^, FArchive.FProgressMax);
  Result := S_OK;
end;

function TJclSevenzipExtractCallback.SetOperationResult(
  resultEOperationResult: Integer): HRESULT;
var
  LastItem: TJclCompressionItem;
begin
  LastItem := FArchive.Items[FLastStream];
  case resultEOperationResult of
    kOK:
      begin
        LastItem.OperationSuccess := osOK;
        LastItem.UpdateFileTimes;
      end;
    kUnSupportedMethod:
      begin
        LastItem.OperationSuccess := osUnsupportedMethod;
        LastItem.DeleteOutputFile;
      end;
    kDataError:
      begin
        LastItem.OperationSuccess := osDataError;
        LastItem.DeleteOutputFile;
      end;
    kCRCError:
      begin
        LastItem.OperationSuccess := osCRCError;
        LastItem.DeleteOutputFile;
      end
  else
    LastItem.OperationSuccess := osUnknownError;
    LastItem.DeleteOutputFile;
  end;

  Result := S_OK;
end;

function TJclSevenzipExtractCallback.SetTotal(Total: Int64): HRESULT;
begin
  FArchive.FProgressMax := Total;
  Result := S_OK;
end;

//=== { TJclSevenzipDecompressArchive } ======================================

procedure TJclSevenzipDecompressArchive.CreateCompressionObject;
var
  SevenzipCLSID, InterfaceID: TGUID;
begin
  SevenzipCLSID := GetCLSID;
  InterfaceID := Sevenzip.IInArchive;
  if (not Is7ZipLoaded) and (not Load7Zip) then
    raise EJclCompressionError.CreateRes(@RsCompression7zLoadError);
  if (Sevenzip.CreateObject(@SevenzipCLSID, @InterfaceID, FInArchive) <> ERROR_SUCCESS)
    or not Assigned(FInArchive) then
    raise EJclCompressionError.CreateResFmt(@RsCompression7zInArchiveError, [GUIDToString(SevenzipCLSID)]);
  FExtractingAllIndex := -1;
end;

procedure TJclSevenzipDecompressArchive.ExtractAll(const ADestinationDir: string;
  AAutoCreateSubDir: Boolean);
var
  AExtractCallback: IArchiveExtractCallback;
  Indices: array of Cardinal;
  NbIndices: Cardinal;
  Index: Integer;
begin
  CheckNotDecompressing;

  FDestinationDir := ADestinationDir;
  FAutoCreateSubDir := AAutoCreateSubDir;

  if FDestinationDir <> '' then
    FDestinationDir := PathAddSeparator(FDestinationDir);

  FDecompressing := True;
  FExtractingAllIndex := 0;
  AExtractCallback := TJclSevenzipExtractCallback.Create(Self);
  try
    OpenArchive;

    // seems buggy: first param "indices" is dereferenced without
    // liveness checks inside Sevenzip code
    //SevenzipCheck(FInArchive.Extract(nil, $FFFFFFFF, 0, AExtractCallback));

    NbIndices := ItemCount;
    SetLength(Indices, NbIndices);
    for Index := 0 to NbIndices - 1 do
    begin
      Items[Index].Selected := True;
      Indices[Index] := Index;
    end;
    SevenzipCheck(FInArchive.Extract(@Indices[0], NbIndices, 0, AExtractCallback));

    CheckOperationSuccess;
  finally
    FDestinationDir := '';
    FDecompressing := False;
    FExtractingAllIndex := -1;
    AExtractCallback := nil;
    // release volumes and other finalizations
    inherited ExtractAll(ADestinationDir, AAutoCreateSubDir);
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
  CheckNotDecompressing;

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
    // release volumes and other finalizations
    inherited ExtractSelected(ADestinationDir, AAutoCreateSubDir);
  end;
end;

procedure TJclSevenzipDecompressArchive.FreeCompressionObject;
begin
  FInArchive := nil;
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
  finally
    FListing := False;
  end;
end;

procedure TJclSevenzipDecompressArchive.OpenArchive;
var
  SplitStream: TJclDynamicSplitStream;
  OpenCallback: IArchiveOpenCallback;
  MaxCheckStartPosition: Int64;
  AInStream: IInStream;
begin
  if not FOpened then
  begin
    if (FVolumeMaxSize <> 0) or (FVolumes.Count <> 0) then
    begin
      SplitStream := TJclDynamicSplitStream.Create(False);
      SplitStream.OnVolume := NeedStream;
      SplitStream.OnVolumeMaxSize := NeedStreamMaxSize;
      AInStream := TJclSevenzipInStream.Create(SplitStream, True);
    end
    else
      AInStream := TJclSevenzipInStream.Create(NeedStream(0), False);
    OpenCallback := TJclSevenzipOpenCallback.Create(Self);

    SetSevenzipArchiveCompressionProperties(Self, FInArchive);

    MaxCheckStartPosition := 1 shl 22;
    SevenzipCheck(FInArchive.Open(AInStream, @MaxCheckStartPosition, OpenCallback));

    GetSevenzipArchiveCompressionProperties(Self, FInArchive);

    FOpened := True;
  end;
end;

//=== { TJclZipDecompressArchive } ===========================================

class function TJclZipDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionZipExtensions);
end;

class function TJclZipDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionZipName);
end;

procedure TJclZipDecompressArchive.CreateCompressionObject;
begin
  inherited CreateCompressionObject;
  FNumberOfThreads := 1;
end;

function TJclZipDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatZip;
end;

function TJclZipDecompressArchive.GetNumberOfThreads: Cardinal;
begin
  Result := FNumberOfThreads;
end;

class function TJclZipDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

procedure TJclZipDecompressArchive.SetNumberOfThreads(Value: Cardinal);
begin
  CheckNotDecompressing;
  FNumberOfThreads := Value;
end;

//=== { TJclBZ2DecompressArchive } ===========================================

class function TJclBZ2DecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionBZip2Extensions);
end;

class function TJclBZ2DecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionBZip2Name);
end;

procedure TJclBZ2DecompressArchive.CreateCompressionObject;
begin
  inherited CreateCompressionObject;
  FNumberOfThreads := 1;
end;

function TJclBZ2DecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatBZ2;
end;

function TJclBZ2DecompressArchive.GetNumberOfThreads: Cardinal;
begin
  Result := FNumberOfThreads;
end;

procedure TJclBZ2DecompressArchive.SetNumberOfThreads(Value: Cardinal);
begin
  CheckNotDecompressing;
  FNumberOfThreads := Value;
end;

//=== { TJclRarDecompressArchive } ===========================================

class function TJclRarDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionRarExtensions);
end;

class function TJclRarDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionRarName);
end;

function TJclRarDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatRar;
end;

class function TJclRarDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclArjDecompressArchive } ===========================================

class function TJclArjDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionArjExtensions);
end;

class function TJclArjDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionArjName);
end;

function TJclArjDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatArj;
end;

class function TJclArjDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclZDecompressArchive } =============================================

class function TJclZDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionZExtensions);
end;

class function TJclZDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionZName);
end;

function TJclZDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatZ;
end;

class function TJclZDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclLzhDecompressArchive } ===========================================

class function TJclLzhDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionLzhExtensions);
end;

class function TJclLzhDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionLzhName);
end;

function TJclLzhDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatLzh;
end;

class function TJclLzhDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJcl7zDecompressArchive } ============================================

class function TJcl7zDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompression7zExtensions);
end;

class function TJcl7zDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompression7zName);
end;

procedure TJcl7zDecompressArchive.CreateCompressionObject;
begin
  inherited CreateCompressionObject;
  FNumberOfThreads := 1;
end;

function TJcl7zDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormat7z;
end;

function TJcl7zDecompressArchive.GetNumberOfThreads: Cardinal;
begin
  Result := FNumberOfThreads;
end;

class function TJcl7zDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

procedure TJcl7zDecompressArchive.SetNumberOfThreads(Value: Cardinal);
begin
  CheckNotDecompressing;
  FNumberOfThreads := Value;
end;

//=== { TJclCabDecompressArchive } ===========================================

class function TJclCabDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionCabExtensions);
end;

class function TJclCabDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionCabName);
end;

function TJclCabDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatCab;
end;

class function TJclCabDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclNsisDecompressArchive } ==========================================

class function TJclNsisDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionNsisExtensions);
end;

class function TJclNsisDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionNsisName);
end;

function TJclNsisDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatNsis;
end;

class function TJclNsisDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclLzmaDecompressArchive } ==========================================

class function TJclLzmaDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionLzmaExtensions);
end;

class function TJclLzmaDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionLzmaName);
end;

function TJclLzmaDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatLzma;
end;

class function TJclLzmaDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclPeDecompressArchive } ============================================

class function TJclPeDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionPeExtensions);
end;

class function TJclPeDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionPeName);
end;

function TJclPeDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatPe;
end;

class function TJclPeDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclElfDecompressArchive } ===========================================

class function TJclElfDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionElfExtensions);
end;

class function TJclElfDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionElfName);
end;

function TJclElfDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatElf;
end;

class function TJclElfDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclMachoDecompressArchive } =========================================

class function TJclMachoDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionMachoExtensions);
end;

class function TJclMachoDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionMachoName);
end;

function TJclMachoDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatMacho;
end;

class function TJclMachoDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclUdfDecompressArchive } ==========================================

class function TJclUdfDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionUdfExtensions);
end;

class function TJclUdfDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionUdfName);
end;

function TJclUdfDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatUdf;
end;

class function TJclUdfDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclXarDecompressArchive } ===========================================

class function TJclXarDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionXarExtensions);
end;

class function TJclXarDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionXarName);
end;

function TJclXarDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatXar;
end;

class function TJclXarDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclMubDecompressArchive } ===========================================

class function TJclMubDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionMubExtensions);
end;

class function TJclMubDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionMubName);
end;

function TJclMubDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatMub;
end;

class function TJclMubDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclHfsDecompressArchive } ===========================================

class function TJclHfsDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionHfsExtensions);
end;

class function TJclHfsDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionHfsName);
end;

function TJclHfsDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatHfs;
end;

class function TJclHfsDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclDmgDecompressArchive } ===========================================

class function TJclDmgDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionDmgExtensions);
end;

class function TJclDmgDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionDmgName);
end;

function TJclDmgDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatDmg;
end;

class function TJclDmgDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclCompoundDecompressArchive } ======================================

class function TJclCompoundDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionCompoundExtensions);
end;

class function TJclCompoundDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionCompoundName);
end;

function TJclCompoundDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatCompound;
end;

class function TJclCompoundDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclWimDecompressArchive } ===========================================

class function TJclWimDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionWimExtensions);
end;

class function TJclWimDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionWimName);
end;

function TJclWimDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatWim;
end;

class function TJclWimDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclIsoDecompressArchive } ===========================================

class function TJclIsoDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionIsoExtensions);
end;

class function TJclIsoDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionIsoName);
end;

function TJclIsoDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatIso;
end;

class function TJclIsoDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclChmDecompressArchive } ===========================================

class function TJclChmDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionChmExtensions);
end;

class function TJclChmDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionChmName);
end;

function TJclChmDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatChm;
end;

class function TJclChmDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclSplitDecompressArchive } =========================================

class function TJclSplitDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionSplitExtensions);
end;

class function TJclSplitDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionSplitName);
end;

function TJclSplitDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatSplit;
end;

//=== { TJclRpmDecompressArchive } ===========================================

class function TJclRpmDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionRpmExtensions);
end;

class function TJclRpmDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionRpmName);
end;

function TJclRpmDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatRpm;
end;

class function TJclRpmDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclDebDecompressArchive } ===========================================

class function TJclDebDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionDebExtensions);
end;

class function TJclDebDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionDebName);
end;

function TJclDebDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatDeb;
end;

class function TJclDebDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclCpioDecompressArchive } ==========================================

class function TJclCpioDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionCpioExtensions);
end;

class function TJclCpioDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionCpioName);
end;

function TJclCpioDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatCpio;
end;

class function TJclCpioDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclTarDecompressArchive } ===========================================

class function TJclTarDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionTarExtensions);
end;

class function TJclTarDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionTarName);
end;

function TJclTarDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatTar;
end;

class function TJclTarDecompressArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclGZipDecompressArchive } ==========================================

class function TJclGZipDecompressArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionGZipExtensions);
end;

class function TJclGZipDecompressArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionGZipName);
end;

function TJclGZipDecompressArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatGZip;
end;

//=== { TJclSevenzipUpdateArchive } ==========================================

procedure TJclSevenzipUpdateArchive.Compress;
var
  OutStream: IOutStream;
  UpdateCallback: IArchiveUpdateCallback;
  SplitStream: TJclDynamicSplitStream;
begin
  CheckNotCompressing;
  CheckNotDecompressing;

  FCompressing := True;
  try
    SplitStream := TJclDynamicSplitStream.Create(True);
    SplitStream.OnVolume := NeedTmpStream;
    SplitStream.OnVolumeMaxSize := NeedStreamMaxSize;
    OutStream := TJclSevenzipOutStream.Create(SplitStream, True, True);
    UpdateCallback := TJclSevenzipUpdateCallback.Create(Self);

    SetSevenzipArchiveCompressionProperties(Self, FOutArchive);

    SevenzipCheck(FOutArchive.UpdateItems(OutStream, ItemCount, UpdateCallback));
  finally
    FCompressing := False;
    // release reference to volume streams
    OutStream := nil;
    // replace streams by tmp streams
    inherited Compress;
  end;
end;

procedure TJclSevenzipUpdateArchive.CreateCompressionObject;
var
  SevenzipCLSID, InterfaceID: TGUID;
begin
  SevenzipCLSID := GetCLSID;
  InterfaceID := Sevenzip.IInArchive;
  if (not Is7ZipLoaded) and (not Load7Zip) then
    raise EJclCompressionError.CreateRes(@RsCompression7zLoadError);
  if (Sevenzip.CreateObject(@SevenzipCLSID, @InterfaceID, FInArchive) <> ERROR_SUCCESS)
    or not Assigned(FInArchive) then
    raise EJclCompressionError.CreateResFmt(@RsCompression7zInArchiveError, [GUIDToString(SevenzipCLSID)]);
  FExtractingAllIndex := -1;
  InterfaceID := Sevenzip.IOutArchive;
  if not Supports(FInArchive, InterfaceID, FOutArchive) then
    raise EJclCompressionError.CreateResFmt(@RsCompression7zOutArchiveError, [GUIDToString(SevenzipCLSID)]);
end;

procedure TJclSevenzipUpdateArchive.DeleteItem(Index: Integer);
var
  I, BaseLength: Integer;
  IsDirectory: Boolean;
  AItem: TJclCompressionItem;
  DirectoryName: WideString;
begin
  AItem := Items[Index];
  IsDirectory := (AItem.Attributes and faDirectory) <> 0;
  DirectoryName := AItem.PackedName + DirDelimiter;

  FItems.Delete(Index);

  if IsDirectory then
  begin
    BaseLength := Length(DirectoryName);

    for I := ItemCount - 1 downto 0 do
      if WideSameText(DirectoryName, Copy(Items[I].PackedName, 1, BaseLength)) then
        FItems.Delete(I);
  end;
end;

procedure TJclSevenzipUpdateArchive.ExtractAll(const ADestinationDir: string;
  AAutoCreateSubDir: Boolean);
var
  AExtractCallback: IArchiveExtractCallback;
  Indices: array of Cardinal;
  NbIndices: Cardinal;
  Index: Integer;
begin
  CheckNotDecompressing;
  CheckNotCompressing;
  
  FDestinationDir := ADestinationDir;
  FAutoCreateSubDir := AAutoCreateSubDir;
  
  if FDestinationDir <> '' then
    FDestinationDir := PathAddSeparator(FDestinationDir);

  FDecompressing := True;
  FExtractingAllIndex := 0;
  AExtractCallback := TJclSevenzipExtractCallback.Create(Self);
  try
    OpenArchive;

    // seems buggy: first param "indices" is dereferenced without
    // liveness checks inside Sevenzip code
    //SevenzipCheck(FInArchive.Extract(nil, $FFFFFFFF, 0, AExtractCallback));

    NbIndices := ItemCount;
    SetLength(Indices, NbIndices);
    for Index := 0 to NbIndices - 1 do
    begin
      Items[Index].Selected := True;
      Indices[Index] := Index;
    end;
    SevenzipCheck(FInArchive.Extract(@Indices[0], NbIndices, 0, AExtractCallback));

    CheckOperationSuccess;
  finally
    FDestinationDir := '';
    FDecompressing := False;
    FExtractingAllIndex := -1;
    AExtractCallback := nil;
    // release volumes and other finalizations
    inherited ExtractAll(ADestinationDir, AAutoCreateSubDir);
  end;
end;

procedure TJclSevenzipUpdateArchive.ExtractSelected(
  const ADestinationDir: string; AAutoCreateSubDir: Boolean);
var
  AExtractCallback: IArchiveExtractCallback;
  Indices: array of Cardinal;
  NbIndices: Cardinal;
  Index: Integer;
begin
  CheckNotDecompressing;
  CheckNotCompressing;

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
    // release volumes and other finalizations
    inherited ExtractSelected(ADestinationDir, AAutoCreateSubDir);
  end;
end;

procedure TJclSevenzipUpdateArchive.FreeCompressionObject;
begin
  FInArchive := nil;
  FOutArchive := nil;
end;

function TJclSevenzipUpdateArchive.GetItemClass: TJclCompressionItemClass;
begin
  Result := TJclUpdateItem;
end;

procedure TJclSevenzipUpdateArchive.ListFiles;
var
  NumberOfItems: Cardinal;
  Index: Integer;
  AItem: TJclCompressionItem;
begin
  CheckNotDecompressing;
  CheckNotCompressing;

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
  finally
    FListing := False;
  end;
end;

procedure TJclSevenzipUpdateArchive.OpenArchive;
var
  OpenCallback: IArchiveOpenCallback;
  MaxCheckStartPosition: Int64;
  AInStream: IInStream;
  SplitStream: TJclDynamicSplitStream;
begin
  if not FOpened then
  begin
    SplitStream := TJclDynamicSplitStream.Create(True);
    SplitStream.OnVolume := NeedStream;
    SplitStream.OnVolumeMaxSize := NeedStreamMaxSize;

    AInStream := TJclSevenzipInStream.Create(SplitStream, True);
    OpenCallback := TJclSevenzipOpenCallback.Create(Self);

    SetSevenzipArchiveCompressionProperties(Self, FInArchive);

    MaxCheckStartPosition := 1 shl 22;
    SevenzipCheck(FInArchive.Open(AInStream, @MaxCheckStartPosition, OpenCallback));

    GetSevenzipArchiveCompressionProperties(Self, FInArchive);

    FOpened := True;
  end;
end;

procedure TJclSevenzipUpdateArchive.RemoveItem(const PackedName: WideString);
var
  Index, BaseLength, PackedNamesIndex: Integer;
  IsDirectory: Boolean;
  AItem: TJclCompressionItem;
  DirectoryName: WideString;
begin
  IsDirectory := False;
  for Index := 0 to ItemCount - 1 do
  begin
    AItem := Items[Index];
    if WideSameText(AItem.PackedName, PackedName) then
    begin
      DirectoryName := AItem.PackedName;
      if (AItem.Attributes and faDirectory) <> 0 then
        IsDirectory := True;
      FItems.Delete(Index);
      if (FPackedNames <> nil) and FPackedNames.Find(PackedName, PackedNamesIndex) then
        FPackedNames.Delete(PackedNamesIndex);
      Break;
    end;
  end;

  if IsDirectory then
  begin
    DirectoryName := PackedName + DirDelimiter;
    BaseLength := Length(DirectoryName);

    for Index := ItemCount - 1 downto 0 do
      if WideSameText(DirectoryName, Copy(Items[Index].PackedName, 1, BaseLength)) then
      begin
        if (FPackedNames <> nil) and FPackedNames.Find(Items[Index].PackedName, PackedNamesIndex) then
          FPackedNames.Delete(PackedNamesIndex);
        FItems.Delete(Index);
      end;
  end;
end;

//=== { TJclZipUpdateArchive } ===============================================

class function TJclZipUpdateArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionZipExtensions);
end;

class function TJclZipUpdateArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionZipName);
end;

procedure TJclZipUpdateArchive.CreateCompressionObject;
begin
  inherited CreateCompressionObject;
  FNumberOfThreads := 1;
  FEncryptionMethod := emZipCrypto;
  FDictionarySize := kBZip2DicSizeX5;
  FCompressionLevel := 7;
  FCompressionMethod := cmDeflate;
  FNumberOfPasses := kDeflateNumPassesX7;
  FAlgorithm := kLzAlgoX5;
end;

function TJclZipUpdateArchive.GetAlgorithm: Cardinal;
begin
  Result := FAlgorithm;
end;

function TJclZipUpdateArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatZip;
end;

function TJclZipUpdateArchive.GetCompressionLevel: Cardinal;
begin
  Result := FCompressionLevel;
end;

function TJclZipUpdateArchive.GetCompressionLevelMax: Cardinal;
begin
  Result := 9;
end;

function TJclZipUpdateArchive.GetCompressionLevelMin: Cardinal;
begin
  Result := 0;
end;

function TJclZipUpdateArchive.GetCompressionMethod: TJclCompressionMethod;
begin
  Result := FCompressionMethod;
end;

function TJclZipUpdateArchive.GetDictionarySize: Cardinal;
begin
  Result := FDictionarySize;
end;

function TJclZipUpdateArchive.GetEncryptionMethod: TJclEncryptionMethod;
begin
  Result := FEncryptionMethod;
end;

function TJclZipUpdateArchive.GetNumberOfPasses: Cardinal;
begin
  Result := FNumberOfPasses;
end;

function TJclZipUpdateArchive.GetNumberOfThreads: Cardinal;
begin
  Result := FNumberOfThreads;
end;

function TJclZipUpdateArchive.GetSupportedAlgorithms: TDynCardinalArray;
begin
  SetLength(Result,2);
  Result[0] := 0;
  Result[1] := 1;
end;

function TJclZipUpdateArchive.GetSupportedCompressionMethods: TJclCompressionMethods;
begin
  Result := [cmCopy,cmDeflate,cmDeflate64,cmBZip2,cmLZMA];
end;

function TJclZipUpdateArchive.GetSupportedEncryptionMethods: TJclEncryptionMethods;
begin
  Result := [emNone,emAES128,emAES192,emAES256,emZipCrypto];
end;

class function TJclZipUpdateArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

procedure TJclZipUpdateArchive.SetAlgorithm(Value: Cardinal);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  if (Value = 0) or (Value = 1) then
    FAlgorithm := Value
  else
    raise EJclCompressionError.CreateRes(@RsCompressionUnavailableProperty);
end;

procedure TJclZipUpdateArchive.SetCompressionLevel(Value: Cardinal);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  if Value <= 9 then
  begin
    FCompressionLevel := Value;
    case FCompressionMethod of
      cmDeflate, cmDeflate64:
        begin
          if Value >= 9 then
            FNumberOfPasses := kDeflateNumPassesX9
          else
          if Value >= 7 then
            FNumberOfPasses := kDeflateNumPassesX7
          else
            FNumberOfPasses := kDeflateNumPassesX1;
          if Value >= 5 then
            FAlgorithm := kLzAlgoX5
          else
            FAlgorithm := kLzAlgoX1;
        end;
      cmBZip2:
        begin
          if Value >= 9 then
            FNumberOfPasses := kBZip2NumPassesX9
          else
          if Value >= 7 then
            FNumberOfPasses := kBZip2NumPassesX7
          else
            FNumberOfPasses := kBZip2NumPassesX1;
          if Value >= 5 then
            FDictionarySize := kBZip2DicSizeX5
          else
          if Value >= 3 then
            FDictionarySize := kBZip2DicSizeX3
          else
            FDictionarySize := kBZip2DicSizeX1;
        end;
      cmLZMA:
        begin
          if Value >= 9 then
            FDictionarySize := kLzmaDicSizeX9
          else
          if Value >= 7 then
            FDictionarySize := kLzmaDicSizeX7
          else
          if Value >= 5 then
            FDictionarySize := kLzmaDicSizeX5
          else
          if Value >= 3 then
            FDictionarySize := kLzmaDicSizeX3
          else
            FDictionarySize := kLzmaDicSizeX1;
          if Value >= 5 then
            FAlgorithm := kLzAlgoX5
          else
            FAlgorithm := kLzAlgoX1;
        end;
    end;
  end
  else
    raise EJclCompressionError.CreateRes(@RsCompressionUnavailableProperty);
end;

procedure TJclZipUpdateArchive.SetCompressionMethod(Value: TJclCompressionMethod);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  if Value in GetSupportedCompressionMethods then
    FCompressionMethod := Value
  else
    raise EJclCompressionError.CreateRes(@RsCompressionUnavailableProperty);
end;

procedure TJclZipUpdateArchive.SetDictionarySize(Value: Cardinal);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  FDictionarySize := Value;
end;

procedure TJclZipUpdateArchive.SetEncryptionMethod(Value: TJclEncryptionMethod);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  if Value in GetSupportedEncryptionMethods then
    FEncryptionMethod := Value
  else
    raise EJclCompressionError.CreateRes(@RsCompressionUnavailableProperty);
end;

procedure TJclZipUpdateArchive.SetNumberOfPasses(Value: Cardinal);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  FNumberOfPasses := Value;
end;

procedure TJclZipUpdateArchive.SetNumberOfThreads(Value: Cardinal);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  FNumberOfThreads := Value;
end;

//=== { TJclBZ2UpdateArchive } ===============================================

class function TJclBZ2UpdateArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionBZip2Extensions);
end;

class function TJclBZ2UpdateArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionBZip2Name);
end;

procedure TJclBZ2UpdateArchive.CreateCompressionObject;
begin
  inherited CreateCompressionObject;
  FNumberOfThreads := 1;
  FDictionarySize := kBZip2DicSizeX5;
  FCompressionLevel := 7;
  FNumberOfPasses := kBZip2NumPassesX7;
end;

function TJclBZ2UpdateArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatBZ2;
end;

function TJclBZ2UpdateArchive.GetCompressionLevel: Cardinal;
begin
  Result := FCompressionLevel;
end;

function TJclBZ2UpdateArchive.GetCompressionLevelMax: Cardinal;
begin
  Result := 9;
end;

function TJclBZ2UpdateArchive.GetCompressionLevelMin: Cardinal;
begin
  Result := 0;
end;

function TJclBZ2UpdateArchive.GetDictionarySize: Cardinal;
begin
  Result := FDictionarySize;
end;

function TJclBZ2UpdateArchive.GetNumberOfPasses: Cardinal;
begin
  Result := FNumberOfPasses;
end;

function TJclBZ2UpdateArchive.GetNumberOfThreads: Cardinal;
begin
  Result := FNumberOfThreads;
end;

procedure TJclBZ2UpdateArchive.SetCompressionLevel(Value: Cardinal);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  if Value <= 9 then
  begin
    FCompressionLevel := Value;
    if Value >= 9 then
      FNumberOfPasses := kBZip2NumPassesX9
    else
    if Value >= 7 then
      FNumberOfPasses := kBZip2NumPassesX7
    else
      FNumberOfPasses := kBZip2NumPassesX1;
    if Value >= 5 then
      FDictionarySize := kBZip2DicSizeX5
    else
    if Value >= 3 then
      FDictionarySize := kBZip2DicSizeX3
    else
      FDictionarySize := kBZip2DicSizeX1;
  end
  else
    raise EJclCompressionError.CreateRes(@RsCompressionUnavailableProperty);
end;

procedure TJclBZ2UpdateArchive.SetDictionarySize(Value: Cardinal);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  FDictionarySize := Value;
end;

procedure TJclBZ2UpdateArchive.SetNumberOfPasses(Value: Cardinal);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  FNumberOfPasses := Value;
end;

procedure TJclBZ2UpdateArchive.SetNumberOfThreads(Value: Cardinal);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  FNumberOfThreads := Value;
end;

//=== { TJcl7zUpdateArchive } ================================================

class function TJcl7zUpdateArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompression7zExtensions);
end;

class function TJcl7zUpdateArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompression7zName);
end;

procedure TJcl7zUpdateArchive.CreateCompressionObject;
begin
  inherited CreateCompressionObject;
  FNumberOfThreads := 1;
  FEncryptHeader := False;
  FRemoveSfxBlock := False;
  FDictionarySize := kLzmaDicSizeX5;
  FCompressionLevel := 6;
  FCompressHeader := False;
  FCompressHeaderFull := False;
  FSaveLastAccessDateTime := True;
  FSaveCreationDateTime := True;
  FSaveLastWriteDateTime := True;
end;

function TJcl7zUpdateArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormat7z;
end;

function TJcl7zUpdateArchive.GetCompressHeader: Boolean;
begin
  Result := FCompressHeader;
end;

function TJcl7zUpdateArchive.GetCompressHeaderFull: Boolean;
begin
  Result := FCompressHeaderFull;
end;

function TJcl7zUpdateArchive.GetCompressionLevel: Cardinal;
begin
  Result := FCompressionLevel;
end;

function TJcl7zUpdateArchive.GetCompressionLevelMax: Cardinal;
begin
  Result := 9;
end;

function TJcl7zUpdateArchive.GetCompressionLevelMin: Cardinal;
begin
  Result := 0;
end;

function TJcl7zUpdateArchive.GetDictionarySize: Cardinal;
begin
  Result := FDictionarySize;
end;

function TJcl7zUpdateArchive.GetEncryptHeader: Boolean;
begin
  Result := FEncryptHeader;
end;

function TJcl7zUpdateArchive.GetNumberOfThreads: Cardinal;
begin
  Result := FNumberOfThreads;
end;

function TJcl7zUpdateArchive.GetRemoveSfxBlock: Boolean;
begin
  Result := FRemoveSfxBlock;
end;

function TJcl7zUpdateArchive.GetSaveCreationDateTime: Boolean;
begin
  Result := FSaveCreationDateTime;
end;

function TJcl7zUpdateArchive.GetSaveLastAccessDateTime: Boolean;
begin
  Result := FSaveLastAccessDateTime;
end;

function TJcl7zUpdateArchive.GetSaveLastWriteDateTime: Boolean;
begin
  Result := FSaveLastWriteDateTime;
end;

class function TJcl7zUpdateArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

procedure TJcl7zUpdateArchive.SetCompressHeader(Value: Boolean);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  FCompressHeader := Value;
end;

procedure TJcl7zUpdateArchive.SetCompressHeaderFull(Value: Boolean);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  FCompressHeaderFull := Value;
end;

procedure TJcl7zUpdateArchive.SetCompressionLevel(Value: Cardinal);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  if Value <= 9 then
  begin
    FCompressionLevel := Value;
    if Value >= 9 then
      FDictionarySize := kLzmaDicSizeX9
    else
    if Value >= 7 then
      FDictionarySize := kLzmaDicSizeX7
    else
    if Value >= 5 then
      FDictionarySize := kLzmaDicSizeX5
    else
    if Value >= 3 then
      FDictionarySize := kLzmaDicSizeX3
    else
      FDictionarySize := kLzmaDicSizeX1;
  end
  else
    raise EJclCompressionError.CreateRes(@RsCompressionUnavailableProperty);
end;

procedure TJcl7zUpdateArchive.SetDictionarySize(Value: Cardinal);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  FDictionarySize := Value;
end;

procedure TJcl7zUpdateArchive.SetEncryptHeader(Value: Boolean);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  FEncryptHeader := Value;
end;

procedure TJcl7zUpdateArchive.SetNumberOfThreads(Value: Cardinal);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  FNumberOfThreads := Value;
end;

procedure TJcl7zUpdateArchive.SetRemoveSfxBlock(Value: Boolean);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  FRemoveSfxBlock := Value;
end;

procedure TJcl7zUpdateArchive.SetSaveCreationDateTime(Value: Boolean);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  FSaveCreationDateTime := Value;
end;

procedure TJcl7zUpdateArchive.SetSaveLastAccessDateTime(Value: Boolean);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  FSaveLastAccessDateTime := Value;
end;

procedure TJcl7zUpdateArchive.SetSaveLastWriteDateTime(Value: Boolean);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  FSaveLastWriteDateTime := Value;
end;

//=== { TJclTarUpdateArchive } ===============================================

class function TJclTarUpdateArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionTarExtensions);
end;

class function TJclTarUpdateArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionTarName);
end;

function TJclTarUpdateArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatTar;
end;

class function TJclTarUpdateArchive.MultipleItemContainer: Boolean;
begin
  Result := True;
end;

//=== { TJclGZipUpdateArchive } ==============================================

class function TJclGZipUpdateArchive.ArchiveExtensions: string;
begin
  Result := LoadResString(@RsCompressionGZipExtensions);
end;

class function TJclGZipUpdateArchive.ArchiveName: string;
begin
  Result := LoadResString(@RsCompressionGZipName);
end;

procedure TJclGZipUpdateArchive.CreateCompressionObject;
begin
  inherited CreateCompressionObject;
  FCompressionLevel := 7;
  FNumberOfPasses := kDeflateNumPassesX7;
  FAlgorithm := kLzAlgoX5;
end;

function TJclGZipUpdateArchive.GetAlgorithm: Cardinal;
begin
  Result := FAlgorithm;
end;

function TJclGZipUpdateArchive.GetCLSID: TGUID;
begin
  Result := CLSID_CFormatGZip;
end;

function TJclGZipUpdateArchive.GetCompressionLevel: Cardinal;
begin
  Result := FCompressionLevel;
end;

function TJclGZipUpdateArchive.GetCompressionLevelMax: Cardinal;
begin
  Result := 9;
end;

function TJclGZipUpdateArchive.GetCompressionLevelMin: Cardinal;
begin
  Result := 0;
end;

function TJclGZipUpdateArchive.GetNumberOfPasses: Cardinal;
begin
  Result := FNumberOfPasses;
end;

function TJclGZipUpdateArchive.GetSupportedAlgorithms: TDynCardinalArray;
begin
  SetLength(Result,2);
  Result[0] := 0;
  Result[1] := 1;
end;

procedure TJclGZipUpdateArchive.SetAlgorithm(Value: Cardinal);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  FAlgorithm := Value;
end;

procedure TJclGZipUpdateArchive.SetCompressionLevel(Value: Cardinal);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  if Value <= 9 then
  begin
    if Value >= 9 then
      FNumberOfPasses := kDeflateNumPassesX9
    else
    if Value >= 7 then
      FNumberOfPasses := kDeflateNumPassesX7
    else
      FNumberOfPasses := kDeflateNumPassesX1;
    if Value >= 5 then
      FAlgorithm := kLzAlgoX5
    else
      FAlgorithm := kLzAlgoX1;
  end
  else
    raise EJclCompressionError.CreateRes(@RsCompressionUnavailableProperty);
end;

procedure TJclGZipUpdateArchive.SetNumberOfPasses(Value: Cardinal);
begin
  CheckNotCompressing;
  CheckNotDecompressing;
  FNumberOfPasses := Value;
end;

{$ENDIF MSWINDOWS}

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

  FreeAndNil(GlobalStreamFormats);
  FreeAndNil(GlobalArchiveFormats);

end.

