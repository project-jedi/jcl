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
{ The Original Code is JclTD32.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Borland TD32 symbolic debugging information support routines and classes.                        }
{                                                                                                  }
{ Unit owner: Flier Lu                                                                             }
{ Last modified: March 17, 2002                                                                    }
{                                                                                                  }
{**************************************************************************************************}

unit JclTD32;

interface

{$I jcl.inc}

{$WEAKPACKAGEUNIT ON}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils,
  {$IFDEF DELPHI5_UP}
  Contnrs,
  {$ENDIF DELPHI5_UP}
  JclBase, JclFileUtils, JclPeImage;

{ TODO -cDOC : Original code: "Flier Lu" <flier_lu@yahoo.com.cn> }

//--------------------------------------------------------------------------------------------------
// TD32 constants and structures
//--------------------------------------------------------------------------------------------------

{*******************************************************************************

  [-----------------------------------------------------------------------]
  [	        Symbol and Type OMF Format Borland Executable Files       ]
  [-----------------------------------------------------------------------]

  Introduction

  This section describes the format used to embed debugging information  into
  the executable file.

  Debug Information Format

  The format encompasses a block of  data which goes at  the end of the  .EXE
  file,  i.e.,  after   the   header   plus   load   image,   overlays,   and
  Windows/Presentation Manager  resource  compiler  information.   The  lower
  portion of the file is unaffected by the additional data.

  The last eight bytes of the file contain a signature and a long file offset
  from the end of the file (lfoBase).  The signature is FBxx, where xx is the
  version number.  The  long  offset  indicates  the  position  in  the  file
  (relative to the end of the file)  of the base address.  For the LX  format
  executables, the base address  is determined by  looking at the  executable
  header.

  The signatures have the following meanings:

    FB09       	The signature for a Borland 32 bit symbol file.

  The value

          lfaBase=length of the file - lfoBase

  gives the base address of the start of the Symbol and Type OMF  information
  relative to  the beginning  of the  file.  All  other file  offsets in  the
  Symbol and Type OMF are relative to  the lfaBase.  At the base address  the
  signature is repeated, followed by the long displacement to the  subsection
  directory (lfoDir).  All subsections start on a long word boundary and  are
  designed to maintain  natural alignment internally  in each subsection  and
  within the subsection directory.

  Subsection Directory

  The subsection directory has the format

       Directory header

       Directory entry 0

       Directory entry 1

        .
        .
        .

       Directory entry n

  There is no requirement for a particular subsection of a particular module to exist.

  The following is the layout of the FB09 debug information in the image:

  FB09 Header

    sstModule [1]
    .
    .
    .
    sstModule [n]

    sstAlignSym	[1]
    sstSrcModule	[1]
    .
    .
    .
    sstAlignSym	[n]
    sstSrcModule	[n]

    sstGlobalSym
    sstGlobalTypes
    sstNames

    SubSection Directory

  FB09 Trailer

*******************************************************************************}

const
  Borland32BitSymbolFileSignatureForDelphi = $39304246; // 'FB09'
  Borland32BitSymbolFileSignatureForBCB    = $41304246; // 'FB0A'

{ Signature structure }
type
  PJclTD32FileSignature = ^TJclTD32FileSignature;
  TJclTD32FileSignature = packed record
    Signature: DWORD;
    Offset: DWORD;
  end;

{ Subsection Types }
const
  SUBSECTION_TYPE_MODULE         = $120;
  SUBSECTION_TYPE_TYPES          = $121;
  SUBSECTION_TYPE_SYMBOLS        = $124;
  SUBSECTION_TYPE_ALIGN_SYMBOLS  = $125;
  SUBSECTION_TYPE_SOURCE_MODULE  = $127;
  SUBSECTION_TYPE_GLOBAL_SYMBOLS = $129;
  SUBSECTION_TYPE_GLOBAL_TYPES   = $12B;
  SUBSECTION_TYPE_NAMES          = $130;

{ Subsection directory header structure }
type
  { The directory header structure is followed by the directory entries
    which specify the subsection type, module index, file offset, and size.
    The subsection directory gives the location (LFO) and size of each subsection,
    as well as its type and module number if applicable. }
  PDirectoryEntry = ^TDirectoryEntry;
  TDirectoryEntry = packed record
    SubsectionType: Word; // Subdirectory type
    ModuleIndex: Word;    // Module index
    Offset: DWORD;        // Offset from the base offset lfoBase
    Size: DWORD;          // Number of bytes in subsection
  end;

  { The subsection directory is prefixed with a directory header structure
    indicating size and number of subsection directory entries that follow. }
  PDirectoryHeader = ^TDirectoryHeader;
  TDirectoryHeader = packed record
    Size: Word;           // Length of this structure
    DirEntrySize: Word;   // Length of each directory entry
    DirEntryCount: DWORD; // Number of directory entries
    lfoNextDir: DWORD;    // Offset from lfoBase of next directory.
    Flags: DWORD;         // Flags describing directory and subsection tables.
    DirEntries: array[0..0] of TDirectoryEntry;
  end;


{*******************************************************************************

  SUBSECTION_TYPE_MODULE	0x120

  This describes the basic information about an object module including  code
  segments, module name,  and the  number of  segments for  the modules  that
  follow.  Directory entries for  sstModules  precede  all  other  subsection
  directory entries.

*******************************************************************************}
type
  PSegmentInfo = ^TSegmentInfo;
  TSegmentInfo = packed record
    Segment: Word; // Segment that this structure describes
    Flags: Word;   // Attributes for the logical segment.
                   // The following attributes are defined:
				         //   0x0000	Data segment
				         //   0x0001	Code segment
    Offset: DWORD; // Offset in segment where the code starts
    Size: DWORD;   // Count of the number of bytes of code in the segment
  end;
  PSegmentInfoArray = ^TSegmentInfoArray;
  TSegmentInfoArray = array[0..32767] of TSegmentInfo;

  PModuleInfo = ^TModuleInfo;
  TModuleInfo = packed record
    OverlayNumber: Word;  // Overlay number
    LibraryIndex: Word;   // Index into sstLibraries subsection
			                    // if this module was linked from a library
    SegmentCount: Word;   // Count of the number of code segments
                          // this module contributes to
    DebuggingStyle: Word; // Debugging style  for this  module.
    NameIndex: DWORD;     // Name index of module.
    TimeStamp: DWORD;     // Time stamp from the OBJ file.
    Reserved: array[0..2] of DWORD; // Set to 0.
    Segments: array[0..0] of TSegmentInfo;
                          // Detailed information about each segment
                          // that code is contributed to.
                          // This is an array of cSeg count segment
                          // information descriptor structures.
  end;

{*******************************************************************************

  SUBSECTION_TYPE_SOURCE_MODULE	   0x0127

  This table describes the source line number to addressing mapping
  information for a module. The table permits the description of a module
  containing multiple source files with each source file contributing code to
  one or more code segments. The base addresses of the tables described
  below are all relative to the beginning of the sstSrcModule table.


  Module header

  Information for source file 1

    Information for segment 1
         .
         .
         .
    Information for segment n

         .
         .
         .

  Information for source file n

    Information for segment 1
         .
         .
         .
    Information for segment n

*******************************************************************************}
type
  { The line number to address mapping information is contained in a table with
    the following format: }
  PLineMappingEntry = ^TLineMappingEntry;
  TLineMappingEntry = packed record
    SegmentIndex: Word;  // Segment index for this table
    PairCount: Word;     // Count of the number of source line pairs to follow
    Offsets: array[0..0] of DWORD;
                     // An array of 32-bit offsets for the offset
                     // within the code segment ofthe start of ine contained
                     // in the parallel array linenumber.
    (*
    { This is an array of 16-bit line numbers of the lines in the source file
      that cause code to be emitted to the code segment.
      This array is parallel to the offset array.
      If cPair is not even, then a zero word is emitted to
      maintain natural alignment in the sstSrcModule table. }
    LineNumbers: array[0..PairCount - 1] of Word;
    *)
  end;

  TOffsetPair = packed record
    StartOffset: DWORD;
    EndOffset: DWORD;
  end;
  POffsetPairArray = ^TOffsetPairArray;
  TOffsetPairArray = array[0..32767] of TOffsetPair;

  { The file table describes the code segments that receive code from this
    source file. Source file entries have the following format: }
  PSourceFileEntry = ^TSourceFileEntry;
  TSourceFileEntry = packed record
    SegmentCount: Word; // Number of segments that receive code from this source file.
    NameIndex: DWORD;   // Name index of Source file name.

    BaseSrcLines: array[0..0] of DWORD;
                        // An array of offsets for the line/address mapping
			// tables for each of the segments that receive code
			// from this source file.
    (*
    { An array  of two  32-bit offsets  per segment  that
			receives code from this  module.  The first  offset
			is the offset within the segment of the first  byte
			of code from this module.  The second offset is the
			ending address of the  code from this module.   The
			order of these pairs corresponds to the ordering of
			the segments in the  seg  array.   Zeros  in  these
			entries means that the information is not known and
			the file and line tables described below need to be
			examined to determine if an address of interest  is
			contained within the code from this module. }
    SegmentAddress: array[0..SegmentCount - 1] of TOffsetPair;

    Name: ShortString; // Count of the number of bytes in source file name
    *)
  end;

  { The module header structure describes the source file and code segment
    organization of the module. Each module header has the following format: }
  PSourceModuleInfo = ^TSourceModuleInfo;
  TSourceModuleInfo = packed record
    FileCount: Word;    // The number of source file scontributing code to segments
    SegmentCount: Word; // The number of code segments receiving code from this module

    BaseSrcFiles: array[0..0] of DWORD;
    (*
    // This is an array of base offsets from the beginning of the sstSrcModule table
    BaseSrcFiles: array[0..FileCount - 1] of DWORD;

    { An array  of two  32-bit offsets  per segment  that
			receives code from this  module.  The first  offset
			is the offset within the segment of the first  byte
			of code from this module.  The second offset is the
			ending address of the  code from this module.   The
			order of these pairs corresponds to the ordering of
			the segments in the  seg  array.   Zeros  in  these
			entries means that the information is not known and
			the file and line tables described below need to be
			examined to determine if an address of interest  is
			contained within the code from this module. }
    SegmentAddress: array[0..SegmentCount - 1] of TOffsetPair;

    { An array of segment indices that receive code  from
			this module.  If the  number  of  segments  is  not
			even, a pad word  is inserted  to maintain  natural
			alignment. }
    SegmentIndexes: array[0..SegmentCount - 1] of Word;
    *)
  end;

{*******************************************************************************

  SUBSECTION_TYPE_GLOBAL_TYPES	0x12b

  This subsection contains the packed  type records for the executable  file.
  The first long word of the subsection  contains the number of types in  the
  table.  This count is  followed by a count-sized  array of long offsets  to
  the  corresponding  type  record.   As  the  sstGlobalTypes  subsection  is
  written, each  type record  is forced  to start  on a  long word  boundary.
  However, the length of the  type string is NOT  adjusted by the pad  count.
  The remainder of the subsection contains  the type records.

*******************************************************************************}
type
  PGlobalTypeInfo = ^TGlobalTypeInfo;
  TGlobalTypeInfo = packed record
    Count: DWORD; // count of the number of types

    // offset of each type string from the beginning of table
    Offsets: array[0..0] of DWORD;
  end;

{ Symbol type defines }
const
  SYMBOL_TYPE_COMPILE        = $0001; // Compile flags symbol
  SYMBOL_TYPE_REGISTER       = $0002; // Register variable
  SYMBOL_TYPE_CONST          = $0003; // Constant symbol
  SYMBOL_TYPE_UDT            = $0004; // User-defined Type
  SYMBOL_TYPE_SSEARCH        = $0005; // Start search
  SYMBOL_TYPE_END            = $0006; // End block, procedure, with, or thunk
  SYMBOL_TYPE_SKIP           = $0007; // Skip - Reserve symbol space
  SYMBOL_TYPE_CVRESERVE      = $0008; // Reserved for Code View internal use
  SYMBOL_TYPE_OBJNAME        = $0009; // Specify name of object file

  SYMBOL_TYPE_BPREL16        = $0100; // BP relative 16:16
  SYMBOL_TYPE_LDATA16        = $0101; // Local data 16:16
  SYMBOL_TYPE_GDATA16        = $0102; // Global data 16:16
  SYMBOL_TYPE_PUB16          = $0103; // Public symbol 16:16
  SYMBOL_TYPE_LPROC16        = $0104; // Local procedure start 16:16
  SYMBOL_TYPE_GPROC16        = $0105; // Global procedure start 16:16
  SYMBOL_TYPE_THUNK16        = $0106; // Thunk start 16:16
  SYMBOL_TYPE_BLOCK16        = $0107; // Block start 16:16
  SYMBOL_TYPE_WITH16         = $0108; // With start 16:16
  SYMBOL_TYPE_LABEL16        = $0109; // Code label 16:16
  SYMBOL_TYPE_CEXMODEL16     = $010A; // Change execution model 16:16
  SYMBOL_TYPE_VFTPATH16      = $010B; // Virtual function table path descriptor 16:16

  SYMBOL_TYPE_BPREL32        = $0200; // BP relative 16:32
  SYMBOL_TYPE_LDATA32        = $0201; // Local data 16:32
  SYMBOL_TYPE_GDATA32        = $0202; // Global data 16:32
  SYMBOL_TYPE_PUB32          = $0203; // Public symbol 16:32
  SYMBOL_TYPE_LPROC32        = $0204; // Local procedure start 16:32
  SYMBOL_TYPE_GPROC32        = $0205; // Global procedure start 16:32
  SYMBOL_TYPE_THUNK32        = $0206; // Thunk start 16:32
  SYMBOL_TYPE_BLOCK32        = $0207; // Block start 16:32
  SYMBOL_TYPE_WITH32         = $0208; // With start 16:32
  SYMBOL_TYPE_LABEL32        = $0209; // Label 16:32
  SYMBOL_TYPE_CEXMODEL32     = $020A; // Change execution model 16:32
  SYMBOL_TYPE_VFTPATH32      = $020B; // Virtual function table path descriptor 16:32

{*******************************************************************************

  Global and Local Procedure Start 16:32

  SYMBOL_TYPE_LPROC32	0x0204
  SYMBOL_TYPE_GPROC32   0x0205

    The symbol records define local (file static) and global procedure
  definition. For C/C++, functions that are declared static to a module are
  emitted as Local Procedure symbols. Functions not specifically declared
  static are emitted as Global Procedures.
    For each SYMBOL_TYPE_GPROC32 emitted, an SYMBOL_TYPE_GPROCREF symbol
  must be fabricated and emitted to the SUBSECTION_TYPE_GLOBAL_SYMBOLS section.

*******************************************************************************}
type
  TSymbolProcInfo = packed record
    pParent: DWORD;
    pEnd: DWORD;
    pNext: DWORD;
    Size: DWORD;        // Length in bytes of this procedure
    DebugStart: DWORD;  // Offset in bytes from the start of the procedure to
                        // the point where the stack frame has been set up.
    DebugEnd: DWORD;    // Offset in bytes from the start of the procedure to
                        // the point where the  procedure is  ready to  return
                        // and has calculated its return value, if any.
                        // Frame and register variables an still be viewed.
    Offset: DWORD;      // Offset portion of  the segmented address of
                        // the start of the procedure in the code segment
    Segment: Word;      // Segment portion of the segmented address of
                        // the start of the procedure in the code segment
    ProcType: DWORD;    // Type of the procedure type record
    NearFar: Byte;      // Type of return the procedure makes:
                        //   0       near
                        //   4       far
    Reserved: Byte;
    NameIndex: DWORD;   // Name index of procedure
  end;

{ Symbol Information Records }
type
  PSymbolInfo = ^TSymbolInfo;
  TSymbolInfo = packed record
    Size: Word;
    SymbolType: Word;
    case Word of
      SYMBOL_TYPE_LPROC32,
      SYMBOL_TYPE_GPROC32:   ( Proc: TSymbolProcInfo; );
  end;

  PSymbolInfos = ^TSymbolInfos;
  TSymbolInfos = packed record
    Signature: DWORD;
    Symbols: array[0..0] of TSymbolInfo;
  end;

{$IFDEF SUPPORTS_EXTSYM}

{$EXTERNALSYM Borland32BitSymbolFileSignatureForDelphi}
{$EXTERNALSYM Borland32BitSymbolFileSignatureForBCB}

{$EXTERNALSYM SUBSECTION_TYPE_MODULE}
{$EXTERNALSYM SUBSECTION_TYPE_TYPES}
{$EXTERNALSYM SUBSECTION_TYPE_SYMBOLS}
{$EXTERNALSYM SUBSECTION_TYPE_ALIGN_SYMBOLS}
{$EXTERNALSYM SUBSECTION_TYPE_SOURCE_MODULE}
{$EXTERNALSYM SUBSECTION_TYPE_GLOBAL_SYMBOLS}
{$EXTERNALSYM SUBSECTION_TYPE_GLOBAL_TYPES}
{$EXTERNALSYM SUBSECTION_TYPE_NAMES}

{$EXTERNALSYM SYMBOL_TYPE_COMPILE}
{$EXTERNALSYM SYMBOL_TYPE_REGISTER}
{$EXTERNALSYM SYMBOL_TYPE_CONST}
{$EXTERNALSYM SYMBOL_TYPE_UDT}
{$EXTERNALSYM SYMBOL_TYPE_SSEARCH}
{$EXTERNALSYM SYMBOL_TYPE_END}
{$EXTERNALSYM SYMBOL_TYPE_SKIP}
{$EXTERNALSYM SYMBOL_TYPE_CVRESERVE}
{$EXTERNALSYM SYMBOL_TYPE_OBJNAME}

{$EXTERNALSYM SYMBOL_TYPE_BPREL16}
{$EXTERNALSYM SYMBOL_TYPE_LDATA16}
{$EXTERNALSYM SYMBOL_TYPE_GDATA16}
{$EXTERNALSYM SYMBOL_TYPE_PUB16}
{$EXTERNALSYM SYMBOL_TYPE_LPROC16}
{$EXTERNALSYM SYMBOL_TYPE_GPROC16}
{$EXTERNALSYM SYMBOL_TYPE_THUNK16}
{$EXTERNALSYM SYMBOL_TYPE_BLOCK16}
{$EXTERNALSYM SYMBOL_TYPE_WITH16}
{$EXTERNALSYM SYMBOL_TYPE_LABEL16}
{$EXTERNALSYM SYMBOL_TYPE_CEXMODEL16}
{$EXTERNALSYM SYMBOL_TYPE_VFTPATH16}

{$EXTERNALSYM SYMBOL_TYPE_BPREL32}
{$EXTERNALSYM SYMBOL_TYPE_LDATA32}
{$EXTERNALSYM SYMBOL_TYPE_GDATA32}
{$EXTERNALSYM SYMBOL_TYPE_PUB32}
{$EXTERNALSYM SYMBOL_TYPE_LPROC32}
{$EXTERNALSYM SYMBOL_TYPE_GPROC32}
{$EXTERNALSYM SYMBOL_TYPE_THUNK32}
{$EXTERNALSYM SYMBOL_TYPE_BLOCK32}
{$EXTERNALSYM SYMBOL_TYPE_WITH32}
{$EXTERNALSYM SYMBOL_TYPE_LABEL32}
{$EXTERNALSYM SYMBOL_TYPE_CEXMODEL32}
{$EXTERNALSYM SYMBOL_TYPE_VFTPATH32}

{$ENDIF SUPPORTS_EXTSYM}

//--------------------------------------------------------------------------------------------------
// TD32 information related classes
//--------------------------------------------------------------------------------------------------

type
  TJclModuleInfo = class (TObject)
  private
    FNameIndex: DWORD;
    FSegments: PSegmentInfoArray;
    FSegmentCount: Integer;
    function GetSegment(const Idx: Integer): TSegmentInfo;
  protected
    constructor Create(const pModInfo: PModuleInfo);
  public
    property NameIndex: DWORD read FNameIndex;
    property SegmentCount: Integer read FSegmentCount;//GetSegmentCount;
    property Segment[const Idx: Integer]: TSegmentInfo read GetSegment; default;
  end;

  TJclLineInfo = class (TObject)
  private
    FLineNo: DWORD;
    FOffset: DWORD;
  protected
    constructor Create(ALineNo, AOffset: DWORD);
  public
    property LineNo: DWORD read FLineNo;
    property Offset: DWORD read FOffset;
  end;

  TJclSourceModuleInfo = class (TObject)
  private
    FLines: TObjectList;
    FSegments: POffsetPairArray;
    FSegmentCount: Integer;
    FNameIndex: DWORD;
    function GetLine(const Idx: Integer): TJclLineInfo;
    function GetLineCount: Integer;
    function GetSegment(const Idx: Integer): TOffsetPair;
  protected
    constructor Create(const pSrcFile: PSourceFileEntry; Base: DWORD);
  public
    destructor Destroy; override;
    function FindLine(const AAddr: DWORD; var ALine: TJclLineInfo): Boolean;
    property NameIndex: DWORD read FNameIndex;
    property LineCount: Integer read GetLineCount;
    property Line[const Idx: Integer]: TJclLineInfo read GetLine; default;
    property SegmentCount: Integer read FSegmentCount;//GetSegmentCount;
    property Segment[const Idx: Integer]: TOffsetPair read GetSegment;
  end;

  TJclSymbolInfo = class (TObject)
  private
    FSymbolType: Word;
  protected
    constructor Create(const pSymInfo: PSymbolInfo); virtual;
    property SymbolType: Word read FSymbolType;
  end;

  TJclProcSymbolInfo = class (TJclSymbolInfo)
  private
    FNameIndex: DWORD;
    FOffset: DWORD;
    FSize: DWORD;
  protected
    constructor Create(const pSymInfo: PSymbolInfo); override;
  public
    property NameIndex: DWORD read FNameIndex;
    property Offset: DWORD read FOffset;
    property Size: DWORD read FSize;
  end;

  TJclLocalProcSymbolInfo = class (TJclProcSymbolInfo);
  TJclGlobalProcSymbolInfo = class (TJclProcSymbolInfo);

//--------------------------------------------------------------------------------------------------
// TD32 parser
//--------------------------------------------------------------------------------------------------

  TJclTD32InfoParser = class (TObject)
  private
    FBase: Pointer;
    FData: TCustomMemoryStream;
    FNames: TList;
    FModules: TObjectList;
    FSourceModules: TObjectList;
    FSymbols: TObjectList;
    FValidData: Boolean;
    function GetName(const Idx: Integer): string;
    function GetNameCount: Integer;
    function GetSymbol(const Idx: Integer): TJclSymbolInfo;
    function GetSymbolCount: Integer;
    function GetModule(const Idx: Integer): TJclModuleInfo;
    function GetModuleCount: Integer;
    function GetSourceModule(const Idx: Integer): TJclSourceModuleInfo;
    function GetSourceModuleCount: Integer;
  protected
    procedure Analyse;
    procedure AnalyseNames(const pSubsection: Pointer; const Size: DWORD); virtual;
    procedure AnalyseAlignSymbols(const pSymbols: PSymbolInfos; const Size: DWORD); virtual;
    procedure AnalyseModules(const pModInfo: PModuleInfo; const Size: DWORD); virtual;
    procedure AnalyseSourceModules(const pSrcModInfo: PSourceModuleInfo; const Size: DWORD); virtual;
    procedure AnalyseUnknownSubSection(const pSubsection: Pointer; const Size: DWORD); virtual;
    function LfaToVa(Lfa: DWORD): Pointer;
  public
    constructor Create(const ATD32Data: TCustomMemoryStream); // Data mustn't be freed before the class is destroyed
    destructor Destroy; override;
    function FindModule(const AAddr: DWORD; var AMod: TJclModuleInfo): Boolean;
    function FindSourceModule(const AAddr: DWORD; var ASrcMod: TJclSourceModuleInfo): Boolean;
    function FindProc(const AAddr: DWORD; var AProc: TJclProcSymbolInfo): Boolean;
    class function IsTD32Sign(const Sign: TJclTD32FileSignature): Boolean;
    class function IsTD32DebugInfoValid(const DebugData: Pointer; const DebugDataSize: LongWord): Boolean;
    property Data: TCustomMemoryStream read FData;
    property Names[const Idx: Integer]: string read GetName;
    property NameCount: Integer read GetNameCount;
    property Symbols[const Idx: Integer]: TJclSymbolInfo read GetSymbol;
    property SymbolCount: Integer read GetSymbolCount;
    property Modules[const Idx: Integer]: TJclModuleInfo read GetModule;
    property ModuleCount: Integer read GetModuleCount;
    property SourceModules[const Idx: Integer]: TJclSourceModuleInfo read GetSourceModule;
    property SourceModuleCount: Integer read GetSourceModuleCount;
    property ValidData: Boolean read FValidData;
  end;

//--------------------------------------------------------------------------------------------------
// TD32 scanner with source location methods
//--------------------------------------------------------------------------------------------------

  TJclTD32InfoScanner = class (TJclTD32InfoParser)
  public
    function LineNumberFromAddr(AAddr: DWORD; var Offset: Integer): Integer; overload;
    function LineNumberFromAddr(AAddr: DWORD): Integer; overload;
    function ProcNameFromAddr(AAddr: DWORD): string; overload;
    function ProcNameFromAddr(AAddr: DWORD; var Offset: Integer): string; overload;
    function ModuleNameFromAddr(AAddr: DWORD): string;
    function SourceNameFromAddr(AAddr: DWORD): string;
  end;

//--------------------------------------------------------------------------------------------------
// PE Image with TD32 information and source location support 
//--------------------------------------------------------------------------------------------------

  TJclPeBorTD32Image = class (TJclPeBorImage)
  private
    FIsTD32DebugPresent: Boolean;
    FTD32DebugData: TCustomMemoryStream;
    FTD32Scanner: TJclTD32InfoScanner;
  protected
    procedure AfterOpen; override;
    procedure Clear; override;
    procedure ClearDebugData;
    procedure CheckDebugData;
    function IsDebugInfoInImage(var DataStream: TCustomMemoryStream): Boolean;
    function IsDebugInfoInTds(var DataStream: TCustomMemoryStream): Boolean;
  public
    property IsTD32DebugPresent: Boolean read FIsTD32DebugPresent;
    property TD32DebugData: TCustomMemoryStream read FTD32DebugData;
    property TD32Scanner: TJclTD32InfoScanner read FTD32Scanner;
  end;

implementation

uses
  Math,
  JclResources, JclSysUtils;

//--------------------------------------------------------------------------------------------------

const
  TurboDebuggerSymbolExt = '.tds';

//==================================================================================================
// TJclModuleInfo
//==================================================================================================

constructor TJclModuleInfo.Create(const pModInfo: PModuleInfo);
begin
  Assert(Assigned(pModInfo));
  inherited Create;
  FNameIndex := pModInfo.NameIndex;
  FSegments := @pModInfo.Segments[0];
  FSegmentCount := pModInfo.SegmentCount;
end;

//--------------------------------------------------------------------------------------------------

function TJclModuleInfo.GetSegment(const Idx: Integer): TSegmentInfo;
begin
  Assert((0 <= Idx) and (Idx < FSegmentCount));
  Result := FSegments[Idx];
end;

//==================================================================================================
// TJclLineInfo
//==================================================================================================

constructor TJclLineInfo.Create(ALineNo, AOffset: DWORD);
begin
  inherited Create;
  FLineNo := ALineNo;
  FOffset := AOffset;
end;

//==================================================================================================
// TJclSourceModuleInfo
//==================================================================================================

constructor TJclSourceModuleInfo.Create(const pSrcFile: PSourceFileEntry; Base: DWORD);
type
  PArrayOfWord = ^TArrayOfWord;
  TArrayOfWord = array[0..0] of Word;
var
  I, J: Integer;
  pLineEntry: PLineMappingEntry;
begin
  Assert(Assigned(pSrcFile));
  inherited Create;
  FNameIndex := pSrcFile.NameIndex;
  FLines := TObjectList.Create;
  for I := 0 to pSrcFile.SegmentCount - 1 do
  begin
    pLineEntry := PLineMappingEntry(Base + pSrcFile.BaseSrcLines[I]);
    for J := 0 to pLineEntry.PairCount - 1 do
      FLines.Add(TJclLineInfo.Create(
        PArrayOfWord(@pLineEntry.Offsets[pLineEntry.PairCount])^[J],
        pLineEntry.Offsets[J]));
  end;

  FSegments := @pSrcFile.BaseSrcLines[pSrcFile.SegmentCount];
  FSegmentCount := pSrcFile.SegmentCount;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclSourceModuleInfo.Destroy;
begin
  FreeAndNil(FLines);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TJclSourceModuleInfo.GetLine(const Idx: Integer): TJclLineInfo;
begin
  Result := TJclLineInfo(FLines.Items[Idx]);
end;

//--------------------------------------------------------------------------------------------------

function TJclSourceModuleInfo.GetLineCount: Integer;
begin
  Result := FLines.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclSourceModuleInfo.GetSegment(const Idx: Integer): TOffsetPair;
begin
  Assert((0 <= Idx) and (Idx < FSegmentCount));
  Result := FSegments[Idx];
end;

//--------------------------------------------------------------------------------------------------

function TJclSourceModuleInfo.FindLine(const AAddr: DWORD; var ALine: TJclLineInfo): Boolean;
var
  I: Integer;
begin
  for I := 0 to LineCount - 1 do
    with Line[I] do
    begin
      if AAddr = Offset then
      begin
        Result := True;
        ALine  := Line[I];
        Exit;
      end
      else
      if (I > 1) and (Line[I - 1].Offset < AAddr) and (AAddr < Offset) then
      begin
        Result := True;
        ALine  := Line[I-1];
        Exit;
      end;
    end;
  Result := False;
  ALine  := nil;
end;

//==================================================================================================
// TJclSymbolInfo
//==================================================================================================

constructor TJclSymbolInfo.Create(const pSymInfo: PSymbolInfo);
begin
  Assert(Assigned(pSymInfo));
  inherited Create;
  FSymbolType := pSymInfo.SymbolType;
end;

//==================================================================================================
// TJclProcSymbolInfo
//==================================================================================================

constructor TJclProcSymbolInfo.Create(const pSymInfo: PSymbolInfo);
begin
  Assert(Assigned(pSymInfo));
  inherited Create(pSymInfo);
  with pSymInfo^ do
  begin
    FNameIndex := Proc.NameIndex;
    FOffset := Proc.Offset;
    FSize  := Proc.Size;
  end;
end;

//==================================================================================================
// TJclTD32InfoParser
//==================================================================================================

procedure TJclTD32InfoParser.Analyse;
var
  I: Integer;
  pDirHeader: PDirectoryHeader;
  pSubsection: Pointer;
begin
  pDirHeader := PDirectoryHeader(LfaToVa(PJclTD32FileSignature(LfaToVa(0)).Offset));
  while True do
  begin
    Assert(pDirHeader.DirEntrySize = SizeOf(TDirectoryEntry));
    for I := 0 to pDirHeader.DirEntryCount - 1 do
    with pDirHeader.DirEntries[I] do
    begin
      pSubsection := LfaToVa(Offset);
      case SubsectionType of
        SUBSECTION_TYPE_MODULE:
          AnalyseModules(pSubsection, Size);
        SUBSECTION_TYPE_ALIGN_SYMBOLS:
          AnalyseAlignSymbols(pSubsection, Size);
        SUBSECTION_TYPE_SOURCE_MODULE:
          AnalyseSourceModules(pSubsection, Size);
        SUBSECTION_TYPE_NAMES:
          AnalyseNames(pSubsection, Size);
      else
        AnalyseUnknownSubSection(pSubsection, Size);
      end;
    end;
    if pDirHeader.lfoNextDir <> 0 then
      pDirHeader := PDirectoryHeader(LfaToVa(pDirHeader.lfoNextDir))
    else
      Break;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTD32InfoParser.AnalyseNames(const pSubsection: Pointer; const Size: DWORD);
var
  I, Count, Len: Integer;
  pszName: PChar;
begin
  Count := PDWORD(pSubsection)^;
  pszName := PChar(DWORD(pSubsection) + SizeOf(DWORD));
  for I := 0 to Count - 1 do
  begin
    // Get the length of the name
    Len := Ord(pszName^);
    Inc(pszName);
    // Get the name
    FNames.Add(pszName);
    // skip the length of name and a NULL at the end
    Inc(pszName, Len + 1);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTD32InfoParser.AnalyseAlignSymbols(const pSymbols: PSymbolInfos; const Size: DWORD);
var
  Offset: DWORD;
  pInfo: PSymbolInfo;
  Symbol: TJclSymbolInfo;
begin
  Offset := DWORD(@pSymbols.Symbols[0]) - DWORD(pSymbols);
  while Offset < Size do
  begin
    pInfo := PSymbolInfo(DWORD(pSymbols) + Offset);
    case pInfo.SymbolType of
      SYMBOL_TYPE_LPROC32:
        Symbol := TJclLocalProcSymbolInfo.Create(pInfo);
      SYMBOL_TYPE_GPROC32:
        Symbol := TJclGlobalProcSymbolInfo.Create(pInfo);
    else
      Symbol := nil;
    end;
    if Assigned(Symbol) then
      FSymbols.Add(Symbol);
    Inc(Offset, pInfo.Size + SizeOf(pInfo.Size));
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTD32InfoParser.AnalyseModules(const pModInfo: PModuleInfo; const Size: DWORD);
begin
  FModules.Add(TJclModuleInfo.Create(pModInfo));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTD32InfoParser.AnalyseSourceModules(const pSrcModInfo: PSourceModuleInfo; const Size: DWORD);
var
  I: Integer;
  pSrcFile: PSourceFileEntry;
begin
  for I := 0 to pSrcModInfo.FileCount - 1 do
  begin
    pSrcFile := PSourceFileEntry(DWORD(pSrcModInfo) + pSrcModInfo.BaseSrcFiles[I]);
    if pSrcFile.NameIndex > 0 then
      FSourceModules.Add(TJclSourceModuleInfo.Create(pSrcFile, DWORD(pSrcModInfo)));
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTD32InfoParser.AnalyseUnknownSubSection(const pSubsection: Pointer; const Size: DWORD);
begin
  // do nothing
end;

//--------------------------------------------------------------------------------------------------

constructor TJclTD32InfoParser.Create(const ATD32Data: TCustomMemoryStream);
begin
  Assert(Assigned(ATD32Data));
  inherited Create;
  FNames := TList.Create;
  FModules := TObjectList.Create;
  FSourceModules := TObjectList.Create;
  FSymbols := TObjectList.Create;
  FNames.Add(nil);
  FData := ATD32Data;
  FBase := FData.Memory;
  FValidData := IsTD32DebugInfoValid(FBase, FData.Size);
  if FValidData then
    Analyse;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclTD32InfoParser.Destroy;
begin
  FreeAndNil(FSymbols);
  FreeAndNil(FSourceModules);
  FreeAndNil(FModules);
  FreeAndNil(FNames);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TJclTD32InfoParser.GetModule(const Idx: Integer): TJclModuleInfo;
begin
  Result := TJclModuleInfo(FModules.Items[Idx]);
end;

//--------------------------------------------------------------------------------------------------

function TJclTD32InfoParser.GetModuleCount: Integer;
begin
  Result := FModules.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclTD32InfoParser.GetName(const Idx: Integer): string;
begin
  Result := PChar(FNames.Items[Idx]);
end;

//--------------------------------------------------------------------------------------------------

function TJclTD32InfoParser.GetNameCount: Integer;
begin
  Result := FNames.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclTD32InfoParser.GetSourceModule(const Idx: Integer): TJclSourceModuleInfo;
begin
  Result := TJclSourceModuleInfo(FSourceModules.Items[Idx]);
end;

//--------------------------------------------------------------------------------------------------

function TJclTD32InfoParser.GetSourceModuleCount: Integer;
begin
  Result := FSourceModules.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclTD32InfoParser.GetSymbol(const Idx: Integer): TJclSymbolInfo;
begin
  Result := TJclSymbolInfo(FSymbols.Items[Idx]);
end;

//--------------------------------------------------------------------------------------------------

function TJclTD32InfoParser.GetSymbolCount: Integer;
begin
  Result := FSymbols.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclTD32InfoParser.FindModule(
  const AAddr: DWORD; var AMod: TJclModuleInfo): Boolean;
var
  I, J, Offset: Integer;
begin
  if ValidData then
    for I := 0 to ModuleCount - 1 do
    with Modules[I] do
      for J := 0 to SegmentCount - 1 do
      begin
        Offset := AAddr - FSegments[J].Offset;
        if (0 <= Offset) and (Offset <= Integer(Segment[J].Size)) then
        begin
          Result := True;
          AMod   := Modules[I];
          Exit;
        end;
      end;

  Result := False;
  AMod   := nil;
end;

//--------------------------------------------------------------------------------------------------

function TJclTD32InfoParser.FindSourceModule(
  const AAddr: DWORD; var ASrcMod: TJclSourceModuleInfo): Boolean;
var
  I, J: Integer;
begin
  if ValidData then
    for I := 0 to SourceModuleCount - 1 do
    with SourceModules[I] do
      for J := 0 to SegmentCount - 1 do
      with Segment[J] do
        if (StartOffset <= AAddr) and (AAddr < EndOffset) then
        begin
          Result  := True;
          ASrcMod := SourceModules[I];
          Exit;
        end;

  Result  := False;
  ASrcMod := nil;
end;

//--------------------------------------------------------------------------------------------------

function TJclTD32InfoParser.FindProc(const AAddr: DWORD; var AProc: TJclProcSymbolInfo): Boolean;
var
  I: Integer;
begin
  if ValidData then
    for I := 0 to SymbolCount - 1 do
      if Symbols[I].InheritsFrom(TJclProcSymbolInfo) then
      with Symbols[I] as TJclProcSymbolInfo do
        if (Offset <= AAddr) and (AAddr < Offset + Size) then
        begin
          Result := True;
          AProc  := TJclProcSymbolInfo(Symbols[I]);
          Exit;
        end;

  Result := False;
  AProc  := nil;
end;

//--------------------------------------------------------------------------------------------------

class function TJclTD32InfoParser.IsTD32DebugInfoValid(
  const DebugData: Pointer; const DebugDataSize: LongWord): Boolean;
var
  Sign: TJclTD32FileSignature;
  EndOfDebugData: LongWord;
begin
  Assert(not IsBadReadPtr(DebugData, DebugDataSize));
  Result := False;
  EndOfDebugData := LongWord(DebugData) + DebugDataSize;
  if DebugDataSize > SizeOf(Sign) then
  begin
    Sign := PJclTD32FileSignature(EndOfDebugData - SizeOf(Sign))^;
    if IsTD32Sign(Sign) and (Sign.Offset <= DebugDataSize) then
    begin
      Sign := PJclTD32FileSignature(EndOfDebugData - Sign.Offset)^;
      Result := IsTD32Sign(Sign);
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

class function TJclTD32InfoParser.IsTD32Sign(const Sign: TJclTD32FileSignature): Boolean;
begin
  Result := (Sign.Signature = Borland32BitSymbolFileSignatureForDelphi) or
    (Sign.Signature = Borland32BitSymbolFileSignatureForBCB);
end;

//--------------------------------------------------------------------------------------------------

function TJclTD32InfoParser.LfaToVa(Lfa: DWORD): Pointer;
begin
  Result := Pointer(DWORD(FBase) + Lfa)
end;

//==================================================================================================
// TJclTD32InfoScanner
//==================================================================================================

function TJclTD32InfoScanner.LineNumberFromAddr(AAddr: DWORD): Integer;
var
  Dummy: Integer;
begin
  Result := LineNumberFromAddr(AAddr, Dummy);
end;

//--------------------------------------------------------------------------------------------------

function TJclTD32InfoScanner.LineNumberFromAddr(AAddr: DWORD; var Offset: Integer): Integer;
var
  ASrcMod: TJclSourceModuleInfo;
  ALine: TJclLineInfo;
begin
  if FindSourceModule(AAddr, ASrcMod) and ASrcMod.FindLine(AAddr, ALine) then
  begin
    Result := ALine.LineNo;
    Offset := AAddr - ALine.Offset;
  end
  else
  begin
    Result := 0;
    Offset := 0;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclTD32InfoScanner.ModuleNameFromAddr(AAddr: DWORD): string;
var
  AMod: TJclModuleInfo;
begin
  if FindModule(AAddr, AMod) then
    Result := Names[AMod.NameIndex]
  else
    Result := '';
end;

//--------------------------------------------------------------------------------------------------

function TJclTD32InfoScanner.ProcNameFromAddr(AAddr: DWORD): string;
var
  Dummy: Integer;
begin
  Result := ProcNameFromAddr(AAddr, Dummy);
end;

//--------------------------------------------------------------------------------------------------

function TJclTD32InfoScanner.ProcNameFromAddr(AAddr: DWORD; var Offset: Integer): string;
var
  AProc: TJclProcSymbolInfo;

  function FormatProcName(const ProcName: string): string;
  var
    pchSecondAt, P: PChar;
  begin
    Result := ProcName;
    if (Length(ProcName) > 0) and (ProcName[1] = '@') then
    begin
      pchSecondAt := StrScan(PChar(Copy(ProcName, 2, Length(ProcName) - 1)), '@');
      if pchSecondAt <> nil then
      begin
        Inc(pchSecondAt);
        Result := pchSecondAt;
        P := PChar(Result);
        while P^ <> #0 do
        begin
          if (pchSecondAt^ = '@') and ((pchSecondAt - 1)^ <> '@') then
            P^ := '.';
          Inc(P);
          Inc(pchSecondAt);
        end;
      end;
    end;
  end;

begin
  if FindProc(AAddr, AProc) then
  begin
    Result := FormatProcName(Names[AProc.NameIndex]);
    Offset := AAddr - AProc.Offset;
  end
  else
  begin
    Result := '';
    Offset := 0;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclTD32InfoScanner.SourceNameFromAddr(AAddr: DWORD): string;
var
  ASrcMod: TJclSourceModuleInfo;
begin
  if FindSourceModule(AAddr, ASrcMod) then
    Result := Names[ASrcMod.NameIndex];
end;

//==================================================================================================
// TJclPeBorTD32Image
//==================================================================================================

procedure TJclPeBorTD32Image.AfterOpen;
begin
  inherited;
  CheckDebugData;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPeBorTD32Image.CheckDebugData;
begin
  FIsTD32DebugPresent := IsDebugInfoInImage(FTD32DebugData);
  if not FIsTD32DebugPresent then
    FIsTD32DebugPresent := IsDebugInfoInTds(FTD32DebugData);
  if FIsTD32DebugPresent then
  begin
    FTD32Scanner := TJclTD32InfoScanner.Create(FTD32DebugData);
    if not FTD32Scanner.ValidData then
    begin
      ClearDebugData;
      if not NoExceptions then
        raise EJclError.CreateResRecFmt(@RsHasNotTD32Info, [FileName]);
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPeBorTD32Image.Clear;
begin
  ClearDebugData;
  inherited;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclPeBorTD32Image.ClearDebugData;
begin
  FIsTD32DebugPresent := False;
  FreeAndNil(FTD32Scanner);
  FreeAndNil(FTD32DebugData);
end;

//--------------------------------------------------------------------------------------------------

function TJclPeBorTD32Image.IsDebugInfoInImage(var DataStream: TCustomMemoryStream): Boolean;
var
  DebugDir: TImageDebugDirectory;
  BugDataStart: Pointer;
  DebugDataSize: Integer;
begin
  Result := False;
  DataStream := nil;
  if IsBorlandImage and (DebugList.Count = 1) then
  begin
    DebugDir := DebugList[0];
    if DebugDir._Type = IMAGE_DEBUG_TYPE_UNKNOWN then
    begin
      BugDataStart := RvaToVa(DebugDir.AddressOfRawData);
      DebugDataSize := DebugDir.SizeOfData;
      Result := TJclTD32InfoParser.IsTD32DebugInfoValid(BugDataStart, DebugDataSize);
      if Result then
        DataStream := TJclReferenceMemoryStream.Create(BugDataStart, DebugDataSize);
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclPeBorTD32Image.IsDebugInfoInTds(var DataStream: TCustomMemoryStream): Boolean;
var
  TdsFileName: TFileName;
  TempStream: TCustomMemoryStream;
begin
  Result := False;
  DataStream := nil;
  TdsFileName := ChangeFileExt(FileName, TurboDebuggerSymbolExt);
  if FileExists(TdsFileName) then
  begin
    TempStream := TJclFileMappingStream.Create(TdsFileName);
    try
      Result := TJclTD32InfoParser.IsTD32DebugInfoValid(TempStream.Memory, TempStream.Size);
      if Result then
        DataStream := TempStream
      else
        TempStream.Free;
    except
      TempStream.Free;
      raise;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

end.
