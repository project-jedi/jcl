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
{ The Original Code is JclClr.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Microsoft .Net framework Clr information support routines and classes.                           }
{                                                                                                  }
{ Unit owner: Flier Lu                                                                             }
{ Last modified: March 18, 2002                                                                    }
{                                                                                                  }
{**************************************************************************************************}

unit JclClr;

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
  JclBase, JclSysUtils, JclFileUtils, JclPeImage;

{ TODO -cDOC : Original code: "Flier Lu" <flier_lu@yahoo.com.cn> }

const
  MAX_CLASS_NAME = 1024;
  MAX_PATH_NAME  = 260;

  MetadataHeaderSignature = $424A5342; // 'BSJB'

type
  TJclClrToken = DWORD;

//==================================================================================================
// Flag	Value	Description
//==================================================================================================
const  COMIMAGE_FLAGS_ILONLY	          = $00000001;	// Always 1 (see Section 23.1).
  COMIMAGE_FLAGS_32BITREQUIRED	  = $00000002;	// Image may only be loaded into a 32-bit process, for instance if there are 32-bit vtablefixups, or casts from native integers to int32. CLI implementations that have 64 bit native integers shall refuse loading binaries with this flag set.
  COMIMAGE_FLAGS_STRONGNAMESIGNED = $00000008;	// Image has a strong name signature.
  COMIMAGE_FLAGS_TRACKDEBUGDATA	  = $00010000;	// Always 0 (see Section 23.1).type  TJclClrImageFlag = (cifILOnly, cif32BitRequired, cifStrongNameSinged, cifTrackDebugData);
  TJclClrImageFlags = set of TJclClrImageFlag;

//==================================================================================================
// Assembly attr bits, used by DefineAssembly.
//==================================================================================================
const
  afPublicKey                  = $0001; // The assembly ref holds the full (unhashed) public key.
  afCompatibilityMask          = $0070;
  afSideBySideCompatible       = $0000; // The assembly is side by side compatible.
  afNonSideBySideAppDomain     = $0010; // The assembly cannot execute with other versions if
                                        // they are executing in the same application domain.
  afNonSideBySideProcess       = $0020; // The assembly cannot execute with other versions if
                                        // they are executing in the same process.
  afNonSideBySideMachine       = $0030; // The assembly cannot execute with other versions if
                                        // they are executing on the same machine.
	afEnableJITcompileTracking   = $8000; // From "DebuggableAttribute".
	afDisableJITcompileOptimizer = $4000; // From "DebuggableAttribute".

type
  TJclClrAssemblyFlag = (cafPublicKey,
                         cafCompatibilityMask,
                         cafSideBySideCompatible,
                         cafNonSideBySideAppDomain,
                         cafNonSideBySideProcess,
                         cafNonSideBySideMachine,
                         cafEnableJITcompileTracking,
                         cafDisableJITcompileOptimizer);
  TJclClrAssemblyFlags = set of TJclClrAssemblyFlag;

//==================================================================================================
// Non VOS v-table entries.  Define an array of these pointed to by
// IMAGE_COR20_HEADER.VTableFixups.  Each entry describes a contiguous array of
// v-table slots.  The slots start out initialized to the meta data token value
// for the method they need to call.  At image load time, the Clr Loader will
// turn each entry into a pointer to machine code for the CPU and can be
// called directly.
//==================================================================================================
const
  COR_VTABLE_32BIT             = $01;          // V-table slots are 32-bits in size.
  COR_VTABLE_64BIT             = $02;          // V-table slots are 64-bits in size.
  COR_VTABLE_FROM_UNMANAGED    = $04;          // If set, transition from unmanaged.
  COR_VTABLE_CALL_MOST_DERIVED = $10;          // Call most derived method described by

type
  TJclClrVTableKind = (vtk32Bit, vtk64Bit, vtkFromUnmanaged, vtkCallMostDerived);
  TJclClrVTableKinds = set of TJclClrVTableKind;

type
  _IMAGE_COR_VTABLEFIXUP = packed record
    RVA: DWORD;     // Offset of v-table array in image.
    Count,          // How many entries at location.
    Kind: Word;     // COR_VTABLE_xxx type of entries.
  end;
  IMAGE_COR_VTABLEFIXUP = _IMAGE_COR_VTABLEFIXUP;
  TImageCorVTableFixup = _IMAGE_COR_VTABLEFIXUP;
  PImageCorVTableFixup = ^TImageCorVTableFixup;
  TImageCorVTableFixupArray = array[0..MaxWord-1] of TImageCorVTableFixup;
  PImageCorVTableFixupArray = ^TImageCorVTableFixupArray;

//==================================================================================================
// TypeDef/ExportedType attr bits, used by DefineTypeDef.
//==================================================================================================
const
  // Use this mask to retrieve the type visibility information.
  tdVisibilityMask        =   $00000007;
  tdNotPublic             =   $00000000;     // Class is not public scope.
  tdPublic                =   $00000001;     // Class is public scope.
  tdNestedPublic          =   $00000002;     // Class is nested with public visibility.
  tdNestedPrivate         =   $00000003;     // Class is nested with private visibility.
  tdNestedFamily          =   $00000004;     // Class is nested with family visibility.
  tdNestedAssembly        =   $00000005;     // Class is nested with assembly visibility.
  tdNestedFamANDAssem     =   $00000006;     // Class is nested with family and assembly visibility.
  tdNestedFamORAssem      =   $00000007;     // Class is nested with family or assembly visibility.

  // Use this mask to retrieve class layout information
  tdLayoutMask            =   $00000018;
  tdAutoLayout            =   $00000000;     // Class fields are auto-laid out
  tdSequentialLayout      =   $00000008;     // Class fields are laid out sequentially
  tdExplicitLayout        =   $00000010;     // Layout is supplied explicitly
  // end layout mask

  // Use this mask to retrieve class semantics information.
  tdClassSemanticsMask    =   $00000020;
  tdClass                 =   $00000000;     // Type is a class.
  tdInterface             =   $00000020;     // Type is an interface.
  // end semantics mask

  // Special semantics in addition to class semantics.
  tdAbstract              =   $00000080;     // Class is abstract
  tdSealed                =   $00000100;     // Class is concrete and may not be extended
  tdSpecialName           =   $00000400;     // Class name is special.  Name describes how.

  // Implementation attributes.
  tdImport                =   $00001000;     // Class / interface is imported
  tdSerializable          =   $00002000;     // The class is Serializable.

  // Use tdStringFormatMask to retrieve string information for native interop
  tdStringFormatMask      =   $00030000;
  tdAnsiClass             =   $00000000;     // LPTSTR is interpreted as ANSI in this class
  tdUnicodeClass          =   $00010000;     // LPTSTR is interpreted as UNICODE
  tdAutoClass             =   $00020000;     // LPTSTR is interpreted automatically
  // end string format mask

  tdBeforeFieldInit       =   $00100000;     // Initialize the class any time before first static field access.

  // Flags reserved for runtime use.
  tdReservedMask          =   $00040800;
  tdRTSpecialName         =   $00000800;     // Runtime should check name encoding.
  tdHasSecurity           =   $00040000;     // Class has security associate with it.

//==================================================================================================
// Flags for Params
//==================================================================================================
const
  pdIn                        =   $0001;     // Param is [In]
  pdOut                       =   $0002;     // Param is [out]
  pdOptional                  =   $0010;     // Param is optional

  // Reserved flags for Runtime use only.
  pdReservedMask              =   $f000;
  pdHasDefault                =   $1000;     // Param has default value.
  pdHasFieldMarshal           =   $2000;     // Param has FieldMarshal.

  pdUnused                    =   $cfe0;

type
  PClrStreamHeader = ^TClrStreamHeader;
  TClrStreamHeader = packed record
    Offset,      // Memory offset to start of this stream from start of the metadata root
    Size: DWORD; // Size of this stream in bytes, shall be a multiple of 4.
    // Name of the stream as null terminated variable length
    // array of ASCII characters, padded with \0 characters
    Name: array[0..MaxWord] of Char;
  end;

  PClrTableStreamHeader = ^TClrTableStreamHeader;
  TClrTableStreamHeader = packed record
    Reserved: DWORD; // Reserved, always 0
    MajorVersion,    // Major version of table schemata, always 1
    MinorVersion,    // Minor version of table schemata, always 0
    HeapSizes,       // Bit vector for heap sizes.
    Reserved2: Byte; // Reserved, always 1
    Valid,           // Bit vector of present tables, let n be the number of bits that are 1.
    Sorted: Int64;   // Bit vector of sorted tables.
    // Array of n four byte unsigned integers indicating the number of rows
    // for each present table.
    Rows: array[0..MaxWord] of DWORD;
    //Rows: array[0..n-1] of DWORD;
    //Tables: array
  end;

  PClrMetadataHeader = ^TClrMetadataHeader;
  TClrMetadataHeader = packed record
    Signature: DWORD;   // Magic signature for physical metadata : $424A5342.
    MajorVersion,       // Major version, 1
    MinorVersion: Word; // Minor version, 0
    Reserved,           // Reserved, always 0
    Length: DWORD;      // Length of version string in bytes, say m.
    Version: array[0..0] of Char;
    // UTF8-encoded version string of length m
    // Padding to next 4 byte boundary, say x.
    {
    Version: array[0..((m+3) and (not $3))-1] of Char;
    Flags,              // Reserved, always 0
    Streams: Word;      // Number of streams, say n.
    // Array of n StreamHdr structures.
    StreamHeaders: array[0..n-1] of TClrStreamHeader;
    }
  end;

type
  TJclClrTableKind = (
    ttModule,               //  $00
    ttTypeRef,              //  $01
    ttTypeDef,              //  $02
    ttUnknown03,            //  $03
    ttFieldDef,             //  $04
    ttUnknown05,            //  $05
    ttMethodDef,            //  $06
    ttUnknown07,            //  $07
    ttParamDef,             //  $08
    ttInterfaceImpl,        //  $09
    ttMemberRef,            //  $0a
    ttConstant,             //  $0b
    ttCustomAttribute,      //  $0c
    ttFieldMarshal,         //  $0d
    ttDeclSecurity,         //  $0e
    ttClassLayout,          //  $0f
    ttFieldLayout,          //  $10
    ttSignature,            //  $11
    ttEventMap,             //  $12
    ttUnknown13,            //  $13
    ttEvent,                //  $14
    ttPropertyMap,          //  $15
    ttUnknown16,            //  $16
    ttProperty,             //  $17
    ttMethodSemantics,      //  $18
    ttMethodImpl,           //  $19
    ttModuleRef,            //  $1a
    ttTypeSpec,             //  $1b
    ttImplMap,              //  $1c
    ttFieldRVA,             //  $1d
    ttUnknown1e,            //  $1e
    ttUnknown1f,            //  $1f
    ttAssembly,             //  $20
    ttAssemblyProcessor,    //  $21
    ttAssemblyOS,           //  $22
    ttAssemblyRef,          //  $23
    ttAssemblyRefProcessor, //  $24
    ttAssemblyRefOS,        //  $25
    ttFile,                 //  $26
    ttExportedType,         //  $27
    ttManifestResource,     //  $28
    ttNestedClass,          //  $29
    ttUnknown2a,            //  $2a
    ttUnknown2b);           //  $2b

type
  TJclClrHeaderEx = class;
  TJclPeMetadata = class;

  TJclClrStreamClass = class of TJclClrStream;
  TJclClrStream = class
  private
    FMetadata: TJclPeMetadata;
    FHeader: PClrStreamHeader;
    function GetName: string;
    function GetOffset: DWORD;
    function GetSize: DWORD;
    function GetData: Pointer;
  protected
    constructor Create(const AMetadata: TJclPeMetadata;
                       const AHeader: PClrStreamHeader); virtual;
  public
    property Metadata: TJclPeMetadata read FMetadata;
    property Header: PClrStreamHeader read FHeader;

    property Name: string read GetName;
    property Offset: DWORD read GetOffset;
    property Size: DWORD read GetSize;
    property Data: Pointer read GetData;
  end;

  TJclClrStringsStream = class(TJclClrStream)
  private
    FStrings: TStrings;

    function GetString(const Idx: Integer): WideString;
    function GetStringCount: Integer;
  protected
    constructor Create(const AMetadata: TJclPeMetadata;
                       const AHeader: PClrStreamHeader); override;
  public
    destructor Destroy; override;

    function At(const Offset: DWORD): WideString;

    property Strings[const Idx: Integer]: WideString read GetString; default;
    property StringCount: Integer read GetStringCount;
  end;

  TJclClrGuidStream = class(TJclClrStream)
  private
    FGuids: array of TGUID;
    function GetGuid(const Idx: Integer): TGUID;
    function GetGuidCount: Integer;
  protected
    constructor Create(const AMetadata: TJclPeMetadata;
                       const AHeader: PClrStreamHeader); override;
  public
    property Guids[const Idx: Integer]: TGUID read GetGuid; default;
    property GuidCount: Integer read GetGuidCount;
  end;

  TJclClrBlobRecord = class(TJClreferenceMemoryStream)
  private
    FPtr: PByteArray;
    FOffset: DWORD;
  protected
    constructor Create(const AStream: TJclClrStream; const APtr: PByteArray);
  public
    property Ptr: PByteArray read FPtr;
    property Offset: DWORD read FOffset;
  end;

  TJclClrBlobStream = class(TJclClrStream)
  private
    FBlobs: TObjectList;
    function GetBlob(const Idx: Integer): TJclClrBlobRecord;
    function GetBlobCount: Integer;
  protected
    constructor Create(const AMetadata: TJclPeMetadata;
                       const AHeader: PClrStreamHeader); override;
  public
    destructor Destroy; override;

    function At(const Offset: DWORD): TJclClrBlobRecord;

    property Blobs[const Idx: Integer]: TJclClrBlobRecord read GetBlob; default;
    property BlobCount: Integer read GetBlobCount;
  end;

  TJclClrUserStringStream = class(TJclClrBlobStream)
  private
    function GetString(const Idx: Integer): WideString;
    function GetStringCount: Integer;
  public
    function At(const Offset: DWORD): WideString;

    property Strings[const Idx: Integer]: WideString read GetString; default;
    property StringCount: Integer read GetStringCount;
  end;

  TJclClrTableStream = class;

  TJclClrHeapKind = (hkString, hkGuid, hkBlob);
  TJclClrComboIndex = (ciResolutionScope);

  TJclClrTable = class;

  TJclClrTableRowClass = class of TJclClrTableRow;
  TJclClrTableRow = class
  private
    FTable: TJclClrTable;
    FIndex: Integer;
  protected
    constructor Create(const ATable: TJclClrTable); virtual;

    procedure Update; virtual;
  public
    property Table: TJclClrTable read FTable;
    property Index: Integer read FIndex;
  end;

  TJclClrTableClass = class of TJclClrTable;
  TJclClrTable = class
  private
    FStream: TJclClrTableStream;
    FData,
    FPtr: PChar;
    FRows: TObjectList;
    FRowCount: Integer;
    FSize: DWORD;
    function GetOffset: DWORD;
    function GetRow(const Idx: Integer): TJclClrTableRow;
    function GetRowCount: Integer;
  protected
    constructor Create(const AStream: TJclClrTableStream;
      const Ptr: Pointer; const ARowCount: Integer); virtual;
    procedure Load; virtual;
    procedure SetSize(const Value: Integer);

    procedure Update; virtual;

    function AddRow(const ARow: TJclClrTableRow): Integer;
    function RealRowCount: Integer;

    procedure Reset;

    function ReadIndex(const HeapKind: TJclClrHeapKind): DWORD; overload;
    function ReadIndex(const TableKinds: array of TJclClrTableKind): DWORD; overload;
    function IsWideIndex(const HeapKind: TJclClrHeapKind): Boolean; overload;
    function IsWideIndex(const TableKinds: array of TJclClrTableKind): Boolean; overload;

    function GetCodedIndexTag(const CodedIndex, TagWidth: DWORD;
      const WideIndex: Boolean): DWORD;
    function GetCodedIndexValue(const CodedIndex, TagWidth: DWORD;
      const WideIndex: Boolean): DWORD;

    function ReadByte: Byte;
    function ReaDWORD: Word;
    function ReadDWORD: DWORD;

    class function TableRowClass: TJclClrTableRowClass; virtual;

    property Rows[const Idx: Integer]: TJclClrTableRow read GetRow; default;
  public
    destructor Destroy; override;

    property Stream: TJclClrTableStream read FStream;

    property Data: PChar read FData;
    property Size: DWORD read FSize;
    property Offset: DWORD read GetOffset;
    property RowCount: Integer read GetRowCount;
  end;

  TJclClrTableModuleRow = class(TJclClrTableRow)
  private
    FGeneration: Word;
    FNameOffset,
    FMvidIdx,
    FEncIdIdx,
    FEncBaseIdIdx: DWORD;
    function GetMvid: TGUID;
    function GetName: WideString;
    function GetEncBaseId: TGUID;
    function GetEncId: TGUID;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    function HasEncId: Boolean;
    function HasEncBaseId: Boolean;

    property Generation: Word read FGeneration;
    property NameOffset: DWORD read FNameOffset;
    property MvidIdx: DWORD read FMvidIdx;
    property EncIdIdx: DWORD read FEncIdIdx;
    property EncBaseIdIdx: DWORD read FEncBaseIdIdx;

    property Name: WideString read GetName;
    property Mvid: TGUID read GetMvid;
    property EncId: TGUID read GetEncId;
    property EncBaseId: TGUID read GetEncBaseId;
  end;

  TJclClrTableModule = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableModuleRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableModuleRow read GetRow; default;
  end;

  TJclClrTableModuleRefRow = class(TJclClrTableRow)
  private
    FNameOffset: DWORD;
    function GetName: WideString;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property NameOffset: DWORD read FNameOffset;

    property Name: WideString read GetName;
  end;

  TJclClrTableModuleRef = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableModuleRefRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableModuleRefRow read GetRow; default;
  end;

  TJclClrTableAssemblyRow = class(TJclClrTableRow)
  private
    FCultureOffset,
    FPublicKeyOffset,
    FHashAlgId,
    FNameOffset: DWORD;
    FMajorVersion,
    FBuildNumber,
    FRevisionNumber,
    FMinorVersion: Word;
    FFlagMask: DWORD;
    function GetCulture: WideString;
    function GetName: WideString;
    function GetPublicKey: TJclClrBlobRecord;
    function GetVersion: string;
    function GetFlags: TJclClrAssemblyFlags;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    class function AssemblyFlags(const Flags: TJclClrAssemblyFlags): DWORD; overload;
    class function AssemblyFlags(const Flags: DWORD): TJclClrAssemblyFlags; overload;

    property HashAlgId: DWORD read FHashAlgId;
    property MajorVersion: Word read FMajorVersion;
    property MinorVersion: Word read FMinorVersion;
    property BuildNumber: Word read FBuildNumber;
    property RevisionNumber: Word read FRevisionNumber;
    property FlagMask: DWORD read FFlagMask;
    property PublicKeyOffset: DWORD read FPublicKeyOffset;
    property NameOffset: DWORD read FNameOffset;
    property CultureOffset: DWORD read FCultureOffset;

    property PublicKey: TJclClrBlobRecord read GetPublicKey;
    property Name: WideString read GetName;
    property Culture: WideString read GetCulture;
    property Version: string read GetVersion;
    property Flags: TJclClrAssemblyFlags read GetFlags;
  end;

  TJclClrTableAssembly = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableAssemblyRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableAssemblyRow read GetRow; default;
  end;

  TJclClrTableAssemblyOSRow = class(TJclClrTableRow)
  private
    FPlatformID,
    FMajorVersion,
    FMinorVersion: DWORD;
    function GetVersion: string;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property PlatformID: DWORD read FPlatformID;
    property MajorVersion: DWORD read FMajorVersion;
    property MinorVersion: DWORD read FMinorVersion;

    property Version: string read GetVersion;
  end;

  TJclClrTableAssemblyOS = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableAssemblyOSRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableAssemblyOSRow read GetRow; default;
  end;

  TJclClrTableAssemblyProcessorRow = class(TJclClrTableRow)
  private
    FProcessor: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property Processor: DWORD read FProcessor;
  end;

  TJclClrTableAssemblyProcessor = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableAssemblyProcessorRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableAssemblyProcessorRow read GetRow; default;
  end;

  TJclClrTableAssemblyRefRow = class(TJclClrTableRow)
  private
    FCultureOffset,
    FNameOffset,
    FPublicKeyOrTokenOffset,
    FHashValueOffsetOffset: DWORD;
    FMajorVersion,
    FRevisionNumber,
    FBuildNumber,
    FMinorVersion: Word;
    FFlagMask: DWORD;
    function GetCulture: WideString;
    function GetHashValue: TJclClrBlobRecord;
    function GetName: WideString;
    function GetPublicKeyOrToken: TJclClrBlobRecord;
    function GetVersion: string;
    function GetFlags: TJclClrAssemblyFlags;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property MajorVersion: Word read FMajorVersion;
    property MinorVersion: Word read FMinorVersion;
    property BuildNumber: Word read FBuildNumber;
    property RevisionNumber: Word read FRevisionNumber;
    property FlagMask: DWORD read FFlagMask;
    property PublicKeyOrTokenOffset: DWORD read FPublicKeyOrTokenOffset;
    property NameOffset: DWORD read FNameOffset;
    property CultureOffset: DWORD read FCultureOffset;
    property HashValueOffsetOffset: DWORD read FHashValueOffsetOffset;

    property PublicKeyOrToken: TJclClrBlobRecord read GetPublicKeyOrToken;
    property Name: WideString read GetName;
    property Culture: WideString read GetCulture;
    property Version: string read GetVersion;
    property HashValue: TJclClrBlobRecord read GetHashValue;
    property Flags: TJclClrAssemblyFlags read GetFlags;
  end;

  TJclClrTableAssemblyRef = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableAssemblyRefRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableAssemblyRefRow read GetRow; default;
  end;

  TJclClrTableAssemblyRefOSRow = class(TJclClrTableAssemblyOSRow)
  private
    FAssemblyRefIdx: DWORD;
    function GetAssemblyRef: TJclClrTableAssemblyRefRow;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property AssemblyRefIdx: DWORD read FAssemblyRefIdx;

    property AssemblyRef: TJclClrTableAssemblyRefRow read GetAssemblyRef;
  end;

  TJclClrTableAssemblyRefOS = class(TJclClrTableAssemblyOS)
  private
    function GetRow(const Idx: Integer): TJclClrTableAssemblyRefOSRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableAssemblyRefOSRow read GetRow; default;
  end;

  TJclClrTableAssemblyRefProcessorRow = class(TJclClrTableAssemblyProcessorRow)
  private
    FAssemblyRefIdx: DWORD;
    function GetAssemblyRef: TJclClrTableAssemblyRefRow;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property AssemblyRefIdx: DWORD read FAssemblyRefIdx;

    property AssemblyRef: TJclClrTableAssemblyRefRow read GetAssemblyRef;
  end;

  TJclClrTableAssemblyRefProcessor = class(TJclClrTableAssemblyProcessor)
  private
    function GetRow(const Idx: Integer): TJclClrTableAssemblyRefProcessorRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableAssemblyRefProcessorRow read GetRow; default;
  end;

  TJclClrTableClassLayoutRow = class(TJclClrTableRow)
  private
    FClassSize: DWORD;
    FParentIdx: DWORD;
    FPackingSize: Word;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property PackingSize: Word read FPackingSize;
    property ClassSize: DWORD read FClassSize;
    property ParentIdx: DWORD read FParentIdx;
  end;

  TJclClrTableClassLayout = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableClassLayoutRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableClassLayoutRow read GetRow; default;
  end;

  TJclClrTableConstantRow = class(TJclClrTableRow)
  private
    FKind: Byte;
    FParentIdx: DWORD;
    FValueOffset: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property Kind: Byte read FKind;
    property ParentIdx: DWORD read FParentIdx;
    property ValueOffset: DWORD read FValueOffset;
  end;

  TJclClrTableConstant = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableConstantRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableConstantRow read GetRow; default;
  end;

  TJclClrTableCustomAttributeRow = class(TJclClrTableRow)
  private
    FParentIdx: DWORD;
    FTypeIdx: DWORD;
    FValueOffset: DWORD;
    function GetValue: TJclClrBlobRecord;
    function GetParent: TJclClrTableRow;
    function GetMethod: TJclClrTableRow;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property ParentIdx: DWORD read FParentIdx;
    property TypeIdx: DWORD read FTypeIdx;
    property ValueOffset: DWORD read FValueOffset;

    property Parent: TJclClrTableRow read GetParent;
    property Method: TJclClrTableRow read GetMethod;
    property Value: TJclClrBlobRecord read GetValue;
  end;

  TJclClrTableCustomAttribute = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableCustomAttributeRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableCustomAttributeRow read GetRow; default;
  end;

  TJclClrTableDeclSecurityRow = class(TJclClrTableRow)
  private
    FPermissionSetOffset: DWORD;
    FParentIdx: DWORD;
    FAction: Word;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property Action: Word read FAction;
    property ParentIdx: DWORD read FParentIdx;
    property PermissionSetOffset: DWORD read FPermissionSetOffset;  
  end;

  TJclClrTableDeclSecurity = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableDeclSecurityRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableDeclSecurityRow read GetRow; default;
  end;

  TJclClrTableEventMapRow = class(TJclClrTableRow)
  private
    FEventListIdx: DWORD;
    FParentIdx: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property ParentIdx: DWORD read FParentIdx;
    property EventListIdx: DWORD read FEventListIdx;
  end;

  TJclClrTableEventMap = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableEventMapRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableEventMapRow read GetRow; default;
  end;

  TJclClrTableEventRow = class(TJclClrTableRow)
  private
    FNameOffset: DWORD;
    FEventTypeIdx: DWORD;
    FEventFlags: Word;
    function GetName: WideString;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property EventFlags: Word read FEventFlags;
    property NameOffset: DWORD read FNameOffset;
    property EventTypeIdx: DWORD read FEventTypeIdx;

    property Name: WideString read GetName;
  end;

  TJclClrTableEvent = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableEventRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableEventRow read GetRow; default;
  end;

  TJclClrTableExportedTypeRow = class(TJclClrTableRow)
  private
    FTypeDefIdx: DWORD;
    FFlags: DWORD;
    FImplementationIdx: DWORD;
    FTypeNamespaceOffset: DWORD;
    FTypeNameOffset: DWORD;
    function GetTypeName: WideString;
    function GetTypeNamespace: WideString;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property Flags: DWORD read FFlags;
    property TypeDefIdx: DWORD read FTypeDefIdx;
    property TypeNameOffset: DWORD read FTypeNameOffset;
    property TypeNamespaceOffset: DWORD read FTypeNamespaceOffset;
    property ImplementationIdx: DWORD read FImplementationIdx;

    property TypeName: WideString read GetTypeName;
    property TypeNamespace: WideString read GetTypeNamespace;
  end;

  TJclClrTableExportedType = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableExportedTypeRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableExportedTypeRow read GetRow; default;
  end;

  TJclClrTableTypeDefRow = class;

  TJclClrTableFieldRow = class(TJclClrTableRow)
  private
    FFlags: Word;
    FNameOffset: DWORD;
    FSignatureOffset: DWORD;
    FParentToken: TJclClrTableTypeDefRow;
    function GetName: WideString;
    function GetSignature: TJclClrBlobRecord;
  protected
    constructor Create(const ATable: TJclClrTable); override;

    procedure SetParentToken(const ARow: TJclClrTableTypeDefRow);
  public
    property Flags: Word read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property SignatureOffset: DWORD read FSignatureOffset;

    property Name: WideString read GetName;
    property Signature: TJclClrBlobRecord read GetSignature;

    property ParentToken: TJclClrTableTypeDefRow read FParentToken;
  end;

  TJclClrTableField = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableFieldRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableFieldRow read GetRow; default;
  end;

  TJclClrTableFieldLayoutRow = class(TJclClrTableRow)
  private
    FOffset: DWORD;
    FFieldIdx: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property Offset: DWORD read FOffset;
    property FieldIdx: DWORD read FFieldIdx;
  end;

  TJclClrTableFieldLayout = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableFieldLayoutRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableFieldLayoutRow read GetRow; default;
  end;

  TJclClrTableFieldMarshalRow = class(TJclClrTableRow)
  private
    FParentIdx: DWORD;
    FNativeTypeOffset: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property ParentIdx: DWORD read FParentIdx;
    property NativeTypeOffset: DWORD read FNativeTypeOffset;
  end;

  TJclClrTableFieldMarshal = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableFieldMarshalRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableFieldMarshalRow read GetRow; default;
  end;

  TJclClrTableFieldRVARow = class(TJclClrTableRow)
  private
    FRVA: DWORD;
    FFieldIdx: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property RVA: DWORD read FRVA;
    property FieldIdx: DWORD read FFieldIdx;
  end;

  TJclClrTableFieldRVA = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableFieldRVARow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableFieldRVARow read GetRow; default;
  end;

  TJclClrTableFileRow = class(TJclClrTableRow)
  private
    FHashValueOffset: DWORD;
    FNameOffset: DWORD;
    FFlags: DWORD;
    function GetName: WideString;
    function GetHashValue: TJclClrBlobRecord;
    function GetContainsMetadata: Boolean;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property Flags: DWORD read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property HashValueOffset: DWORD read FHashValueOffset;

    property Name: WideString read GetName;
    property HashValue: TJclClrBlobRecord read GetHashValue;
    property ContainsMetadata: Boolean read GetContainsMetadata;
  end;

  TJclClrTableFile = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableFileRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableFileRow read GetRow; default;
  end;

  TJclClrTableImplMapRow = class(TJclClrTableRow)
  private
    FImportNameOffset: DWORD;
    FMemberForwardedIdx: DWORD;
    FImportScopeIdx: DWORD;
    FMappingFlags: Word;
    function GetImportName: WideString;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property MappingFlags: Word read FMappingFlags;
    property MemberForwardedIdx: DWORD read FMemberForwardedIdx;
    property ImportNameOffset: DWORD read FImportNameOffset;
    property ImportScopeIdx: DWORD read FImportScopeIdx;

    property ImportName: WideString read GetImportName;
  end;

  TJclClrTableImplMap = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableImplMapRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableImplMapRow read GetRow; default;
  end;

  TJclClrTableInterfaceImplRow = class(TJclClrTableRow)
  private
    FInterfaceIdx: DWORD;
    FClassIdx: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property ClassIdx: DWORD read FClassIdx;
    property InterfaceIdx: DWORD read FInterfaceIdx;
  end;

  TJclClrTableInterfaceImpl = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableInterfaceImplRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableInterfaceImplRow read GetRow; default;
  end;

  TJclClrTableManifestResourceRow = class(TJclClrTableRow)
  private
    FOffset: DWORD;
    FFlags: DWORD;
    FImplementationIdx: DWORD;
    FNameOffset: DWORD;
    function GetName: WideString;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property Offset: DWORD read FOffset;
    property Flags: DWORD read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property ImplementationIdx: DWORD read FImplementationIdx;

    property Name: WideString read GetName;
  end;

  TJclClrTableManifestResource = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableManifestResourceRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableManifestResourceRow read GetRow; default;
  end;

  TJclClrTableMemberRefRow = class(TJclClrTableRow)
  private
    FClassIdx: DWORD;
    FNameOffset: DWORD;
    FSignatureOffset: DWORD;
    function GetName: WideString;
    function GetSignature: TJclClrBlobRecord;
    function GetParentClass: TJclClrTableRow;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property ClassIdx: DWORD read FClassIdx;
    property NameOffset: DWORD read FNameOffset;
    property SignatureOffset: DWORD read FSignatureOffset;

    property Name: WideString read GetName;
    property Signature: TJclClrBlobRecord read GetSignature;
    property ParentClass: TJclClrTableRow read GetParentClass;
  end;

  TJclClrTableMemberRef = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableMemberRefRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableMemberRefRow read GetRow; default;
  end;

  TJclClrTableMethodDefRow = class;

  TJclClrTableParamDefRow = class(TJclClrTableRow)
  private
    FFlags: Word;
    FSequence: Word;
    FNameOffset: DWORD;
    FMethod: TJclClrTableMethodDefRow;
    function GetName: WideString;
  protected
    constructor Create(const ATable: TJclClrTable); override;

    procedure SetMethod(const AMethod: TJclClrTableMethodDefRow);
  public
    property Flags: Word read FFlags;
    property Sequence: Word read FSequence;
    property NameOffset: DWORD read FNameOffset;

    property Name: WideString read GetName;
    property Method: TJclClrTableMethodDefRow read FMethod;
  end;

  TJclClrTableParamDef = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableParamDefRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableParamDefRow read GetRow; default;
  end;

  TJclClrTableMethodDefRow = class(TJclClrTableRow)
  private
    FRVA: DWORD;
    FImplFlags: Word;
    FFlags: Word;
    FNameOffset: DWORD;
    FSignatureOffset: DWORD;
    FParamListIdx: DWORD;
    FParentToken: TJclClrTableTypeDefRow;
    FParams: TList;
    function GetName: WideString;
    function GetSignature: TJclClrBlobRecord;
    function GetParam(const Idx: Integer): TJclClrTableParamDefRow;
    function GetParamCount: Integer;
    function GetHasParam: Boolean;
    procedure UpdateParams;
  protected
    constructor Create(const ATable: TJclClrTable); override;

    procedure Update; override;

    procedure SetParentToken(const ARow: TJclClrTableTypeDefRow);
  public
    destructor Destroy; override;

    property RVA: DWORD read FRVA;
    property ImplFlags: Word read FImplFlags;
    property Flags: Word read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property SignatureOffset: DWORD read FSignatureOffset;
    property ParamListIdx: DWORD read FParamListIdx;

    property Name: WideString read GetName;
    property Signature: TJclClrBlobRecord read GetSignature;
    property ParentToken: TJclClrTableTypeDefRow read FParentToken;
    property HasParam: Boolean read GetHasParam;
    property Params[const Idx: Integer]: TJclClrTableParamDefRow read GetParam;
    property ParamCount: Integer read GetParamCount;
  end;

  TJclClrTableMethodDef = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableMethodDefRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableMethodDefRow read GetRow; default;
  end;

  TJclClrTableMethodImplRow = class(TJclClrTableRow)
  private
    FClassIdx: DWORD;
    FMethodBodyIdx: DWORD;
    FMethodDeclarationIdx: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property ClassIdx: DWORD read FClassIdx;
    property MethodBodyIdx: DWORD read FMethodBodyIdx;
    property MethodDeclarationIdx: DWORD read FMethodDeclarationIdx;    
  end;

  TJclClrTableMethodImpl = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableMethodImplRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableMethodImplRow read GetRow; default;
  end;

  TJclClrTableMethodSemanticsRow = class(TJclClrTableRow)
  private
    FSemantics: Word;
    FMethodIdx: DWORD;
    FAssociationIdx: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property Semantics: Word read FSemantics;
    property MethodIdx: DWORD read FMethodIdx;
    property AssociationIdx: DWORD read FAssociationIdx;  
  end;

  TJclClrTableMethodSemantics = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableMethodSemanticsRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableMethodSemanticsRow read GetRow; default;
  end;

  TJclClrTableNestedClassRow = class(TJclClrTableRow)
  private
    FEnclosingClassIdx: DWORD;
    FNestedClassIdx: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property NestedClassIdx: DWORD read FNestedClassIdx;
    property EnclosingClassIdx: DWORD read FEnclosingClassIdx;
  end;

  TJclClrTableNestedClass = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableNestedClassRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableNestedClassRow read GetRow; default;
  end;

  TJclClrTablePropertyRow = class(TJclClrTableRow)
  private
    FKindIdx: DWORD;
    FNameOffset: DWORD;
    FFlags: Word;
    function GetName: WideString;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property Flags: Word read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property KindIdx: DWORD read FKindIdx;

    property Name: WideString read GetName;
  end;

  TJclClrTableProperty = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTablePropertyRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTablePropertyRow read GetRow; default;
  end;

  TJclClrTablePropertyMapRow = class(TJclClrTableRow)
  private
    FParentIdx: DWORD;
    FPropertyListIdx: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property ParentIdx: DWORD read FParentIdx;
    property PropertyListIdx: DWORD read FPropertyListIdx;
  end;

  TJclClrTablePropertyMap = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTablePropertyMapRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTablePropertyMapRow read GetRow; default;
  end;

  TJclClrTableStandAloneSigRow = class(TJclClrTableRow)
  private
    FSignatureOffset: DWORD;
    function GetSignature: TJclClrBlobRecord;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property SignatureOffset: DWORD read FSignatureOffset;

    property Signature: TJclClrBlobRecord read GetSignature;
  end;

  TJclClrTableStandAloneSig = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableStandAloneSigRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableStandAloneSigRow read GetRow; default;
  end;

  TJclClrTypeVisibility = (tvNotPublic, tvPublic, tvNestedPublic,
                           tvNestedPrivate, tvNestedFamily, tvNestedAssembly,
                           tvNestedFamANDAssem, tvNestedFamORAssem);
  TJclClrClassLayout = (clAuto, clSequential, clExplicit);
  TJclClrClassSemantics = (csClass, csInterface);
  TJclClrStringFormatting = (sfAnsi, sfUnicode, sfAutoChar);

  TJclClrTypeAttribute = (taAbstract, taSealed, taSpecialName, taImport,
    taSerializable, taBeforeFieldInit, taRTSpecialName, taHasSecurity);
  TJclClrTypeAttributes = set of TJclClrTypeAttribute;

  TJclClrTableTypeDefRow = class(TJclClrTableRow)
  private
    FNamespaceOffset: DWORD;
    FNameOffset: DWORD;
    FFlags: DWORD;
    FExtendsIdx: DWORD;
    FFieldListIdx: DWORD;
    FMethodListIdx: DWORD;
    FFields,
    FMethods: TList;
    FClassLayout: TJclClrClassLayout;
    FClassSemantics: TJclClrClassSemantics;
    FStringFormatting: TJclClrStringFormatting;
    FVisibility: TJclClrTypeVisibility;
    FAttributes: TJclClrTypeAttributes;
    function GetName: WideString;
    function GetNamespace: WideString;
    function GetField(const Idx: Integer): TJclClrTableFieldRow;
    function GetFieldCount: Integer;
    function GetMethod(const Idx: Integer): TJclClrTableMethodDefRow;
    function GetMethodCount: Integer;
    procedure UpdateFields;
    procedure UpdateMethods;
    function GetFullName: WideString;
  protected
    constructor Create(const ATable: TJclClrTable); override;

    procedure Update; override;
  public
    destructor Destroy; override;

    function HasField: Boolean;
    function HasMethod: Boolean;

    property Flags: DWORD read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property NamespaceOffset: DWORD read FNamespaceOffset;
    property ExtendsIdx: DWORD read FExtendsIdx;
    property FieldListIdx: DWORD read FFieldListIdx;
    property MethodListIdx: DWORD read FMethodListIdx;

    property Name: WideString read GetName;
    property Namespace: WideString read GetNamespace;
    property FullName: WideString read GetFullName;

    property Attributes: TJclClrTypeAttributes read FAttributes;

    property Visibility: TJclClrTypeVisibility read FVisibility;
    property ClassLayout: TJclClrClassLayout read FClassLayout;
    property ClassSemantics: TJclClrClassSemantics read FClassSemantics;
    property StringFormatting: TJclClrStringFormatting read FStringFormatting;

    property Fields[const Idx: Integer]: TJclClrTableFieldRow read GetField;
    property FieldCount: Integer read GetFieldCount;
    property Methods[const Idx: Integer]: TJclClrTableMethodDefRow read GetMethod;
    property MethodCount: Integer read GetMethodCount;
  end;

  TJclClrTableTypeDef = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableTypeDefRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableTypeDefRow read GetRow; default;
  end;

  TJclClrTableTypeRefRow = class(TJclClrTableRow)
  private
    FResolutionScopeIdx,
    FNamespaceOffset,
    FNameOffset: DWORD;
    function GetName: WideString;
    function GetNamespace: WideString;
    function GetResolutionScopeIdx: DWORD;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property ResolutionScopeIdx: DWORD read GetResolutionScopeIdx;
    property NameOffset: DWORD read FNameOffset;
    property NamespaceOffset: DWORD read FNamespaceOffset;

    property Name: WideString read GetName;
    property Namespace: WideString read GetNamespace;
  end;

  TJclClrTableTypeRef = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableTypeRefRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableTypeRefRow read GetRow; default;
  end;

  TJclClrTableTypeSpecRow = class(TJclClrTableRow)
  private
    FSignatureOffset: DWORD;
    function GetSignature: TJclClrBlobRecord;
  protected
    constructor Create(const ATable: TJclClrTable); override;
  public
    property SignatureOffset: DWORD read FSignatureOffset;

    property Signature: TJclClrBlobRecord read GetSignature;
  end;

  TJclClrTableTypeSpec = class(TJclClrTable)
  private
    function GetRow(const Idx: Integer): TJclClrTableTypeSpecRow;
  protected
    class function TableRowClass: TJclClrTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclClrTableTypeSpecRow read GetRow; default;
  end;

  TJclClrTableStream = class(TJclClrStream)
  private
    FHeader: PClrTableStreamHeader;
    FTables: array[TJclClrTableKind] of TJclClrTable;
    FTableCount: Integer;
    function GetVersionString: string;
    function GetTable(const AKind: TJclClrTableKind): TJclClrTable;
    function GetBigHeap(const AHeapKind: TJclClrHeapKind): Boolean;
  protected
    constructor Create(const AMetadata: TJclPeMetadata;
                       const AHeader: PClrStreamHeader); override;
  public
    destructor Destroy; override;

    procedure Update; virtual;

    function FindTable(const AKind: TJclClrTableKind;
      var ATable: TJclClrTable): Boolean;

    property Header: PClrTableStreamHeader read FHeader;

    property VersionString: string read GetVersionString;
    property BigHeap[const AHeapKind: TJclClrHeapKind]: Boolean read GetBigHeap;

    property Tables[const AKind: TJclClrTableKind]: TJclClrTable read GetTable;
    property TableCount: Integer read FTableCount;
  end;

  TJclPeMetadata = class
  private
    FImage: TJclPeImage;
    FHeader: PClrMetadataHeader;
    FStreams: TObjectList;
    FStringStream: TJclClrStringsStream;
    FGuidStream: TJclClrGuidStream;
    FBlobStream: TJclClrBlobStream;
    FTableStream: TJclClrTableStream;
    function GetStream(const Idx: Integer): TJclClrStream;
    function GetStreamCount: Integer;
    function GetString(const Idx: Integer): WideString;
    function GetStringCount: Integer;
    function GetGuid(const Idx: Integer): TGUID;
    function GetGuidCount: Integer;
    function GetBlob(const Idx: Integer): TJclClrBlobRecord;
    function GetBlobCount: Integer;
    function GetTable(const AKind: TJclClrTableKind): TJclClrTable;
    function GetTableCount: Integer;
    function GetToken(const AToken: TJclClrToken): TJclClrTableRow;
    function GetVersion: string;
    function GetVersionString: WideString;
  protected
    constructor Create(const AImage: TJclPeImage);
  public
    destructor Destroy; override;

    function FindStream(const AName: string; var Stream: TJclClrStream): Boolean; overload;
    function FindStream(const AClass: TJclClrStreamClass; var Stream: TJclClrStream): Boolean; overload;

    function StringAt(const Offset: DWORD): WideString;
    function BlobAt(const Offset: DWORD): TJclClrBlobRecord;

    function TokenExists(const Token: TJclClrToken): Boolean;

    class function TokenTable(const Token: TJclClrToken): TJclClrTableKind;
    class function TokenIndex(const Token: TJclClrToken): Integer;
    class function TokenCode(const Token: TJclClrToken): Integer;
    class function MakeToken(const Table: TJclClrTableKind; const Idx: Integer): TJclClrToken;

    property Image: TJclPeImage read FImage;
    property Header: PClrMetadataHeader read FHeader;

    property Version: string read GetVersion;
    property VersionString: WideString read GetVersionString;

    property Streams[const Idx: Integer]: TJclClrStream read GetStream; default;
    property StreamCount: Integer read GetStreamCount;

    property Strings[const Idx: Integer]: WideString read GetString;
    property StringCount: Integer read GetStringCount;
    property Guids[const Idx: Integer]: TGUID read GetGuid;
    property GuidCount: Integer read GetGuidCount;
    property Blobs[const Idx: Integer]: TJclClrBlobRecord read GetBlob;
    property BlobCount: Integer read GetBlobCount;
    property Tables[const AKind: TJclClrTableKind]: TJclClrTable read GetTable;
    property TableCount: Integer read GetTableCount;
    property Tokens[const AToken: TJclClrToken]: TJclClrTableRow read GetToken;
  end;

  TJclClrResourceRecord = class(TJClreferenceMemoryStream)
  private
    FData: Pointer;
    FOffset: DWORD;
    FRVA: DWORD;
  protected
    constructor Create(const AData: PChar; const AOffset: DWORD; const ARVA: DWORD);
  public
    property Data: Pointer read FData;
    property Offset: DWORD read FOffset;
    property RVA: DWORD read FRVA;
  end;

  TJclClrVTableFixupRecord = class
  private
    FData: PImageCorVTableFixup;
    function GetCount: DWORD;
    function GetKinds: TJclClrVTableKinds;
    function GetRVA: DWORD;
  protected
    constructor Create(const AData: PImageCorVTableFixup);

    class function VTableKinds(const Kinds: TJclClrVTableKinds): DWORD; overload;
    class function VTableKinds(const Kinds: DWORD): TJclClrVTableKinds; overload;
  public
    property Data: PImageCorVTableFixup read FData;
    property RVA: DWORD read GetRVA;               // RVA of Vtable
    property Count: DWORD read GetCount;           // Number of entries in Vtable
    property Kinds: TJclClrVTableKinds read GetKinds; // Type of the entries
  end;

  TJclClrHeaderEx = class(TJclPeClrHeader)
  private
    FMetadata: TJclPeMetadata;
    FFlags: TJclClrImageFlags;
    FStrongNameSignature: TCustomMemoryStream;
    FResources,
    FVTableFixups: TObjectList;
    function GetMetadata: TJclPeMetadata;
    function GetStrongNameSignature: TCustomMemoryStream;
    function GetEntryPointToken: TJclClrTableRow;
    function GetVTableFixup(const Idx: Integer): TJclClrVTableFixupRecord;
    function GetVTableFixupCount: Integer;
    procedure UpdateResources;
    function GetResource(const Idx: Integer): TJclClrResourceRecord;
    function GetResourceCount: Integer;
  public
    constructor Create(const AImage: TJclPeImage);
    destructor Destroy; override;

    function HasResources: Boolean;
    function HasStrongNameSignature: Boolean;
    function HasVTableFixup: Boolean;

    function ResourceAt(const Offset: DWORD): TJclClrResourceRecord;

    class function ClrImageFlag(const Flags: DWORD): TJclClrImageFlags; overload;
    class function ClrImageFlag(const Flags: TJclClrImageFlags): DWORD; overload;

    property Metadata: TJclPeMetadata read GetMetadata;

    property Flags: TJclClrImageFlags read FFlags;
    property EntryPointToken: TJclClrTableRow read GetEntryPointToken;
    property StrongNameSignature: TCustomMemoryStream read GetStrongNameSignature;

    property Resources[const Idx: Integer]: TJclClrResourceRecord read GetResource;
    property ResourceCount: Integer read GetResourceCount;
    property VTableFixups[const Idx: Integer]: TJclClrVTableFixupRecord read GetVTableFixup;
    property VTableFixupCount: Integer read GetVTableFixupCount;
  end;

implementation

uses
  Math, TypInfo, JclUnicode, JClresources;

const
  GUID_NULL : TGUID = '{00000000-0000-0000-0000-000000000000}';

  ValidTableMapping: array[TJclClrTableKind] of TJclClrTableClass = (
    TJclClrTableModule,               //  $00
    TJclClrTableTypeRef,              //  $01
    TJclClrTableTypeDef,              //  $02
    TJclClrTable,                     //  $03
    TJclClrTableField,                //  $04
    TJclClrTable,                     //  $05
    TJclClrTableMethodDef,            //  $06
    TJclClrTable,                     //  $07
    TJclClrTableParamDef,             //  $08
    TJclClrTableInterfaceImpl,        //  $09
    TJclClrTableMemberRef,            //  $0a
    TJclClrTableConstant,             //  $0b
    TJclClrTableCustomAttribute,      //  $0c
    TJclClrTableFieldMarshal,         //  $0d
    TJclClrTableDeclSecurity,         //  $0e
    TJclClrTableClassLayout,          //  $0f
    TJclClrTableFieldLayout,          //  $10
    TJclClrTableStandAloneSig,        //  $11
    TJclClrTableEventMap,             //  $12
    TJclClrTable,                     //  $13
    TJclClrTableEvent,                //  $14
    TJclClrTablePropertyMap,          //  $15
    TJclClrTable,                     //  $16
    TJclClrTableProperty,             //  $17
    TJclClrTableMethodSemantics,      //  $18
    TJclClrTableMethodImpl,           //  $19
    TJclClrTableModuleRef,            //  $1a
    TJclClrTableTypeSpec,             //  $1b
    TJclClrTableImplMap,              //  $1c
    TJclClrTableFieldRVA,             //  $1d
    TJclClrTable,                     //  $1e
    TJclClrTable,                     //  $1f
    TJclClrTableAssembly,             //  $20
    TJclClrTableAssemblyProcessor,    //  $21
    TJclClrTableAssemblyOS,           //  $22
    TJclClrTableAssemblyRef,          //  $23
    TJclClrTableAssemblyRefProcessor, //  $24
    TJclClrTableAssemblyRefOS,        //  $25
    TJclClrTableFile,                 //  $26
    TJclClrTableExportedType,         //  $27
    TJclClrTableManifestResource,     //  $28
    TJclClrTableNestedClass,          //  $29
    TJclClrTable,                     //  $2A
    TJclClrTable);                    //  $2B

  ClrImageFlagMapping: array[TJclClrImageFlag] of DWORD =
    (COMIMAGE_FLAGS_ILONLY, COMIMAGE_FLAGS_32BITREQUIRED,
     COMIMAGE_FLAGS_STRONGNAMESIGNED, COMIMAGE_FLAGS_TRACKDEBUGDATA);

  ClrVTableKindMapping: array[TJclClrVTableKind] of DWORD =
    (COR_VTABLE_32BIT, COR_VTABLE_64BIT,
     COR_VTABLE_FROM_UNMANAGED, COR_VTABLE_CALL_MOST_DERIVED);

  ClrAssemblyFlagMapping: array[TJclClrAssemblyFlag] of DWORD =
    (afPublicKey, afCompatibilityMask, afSideBySideCompatible,
     afNonSideBySideAppDomain, afNonSideBySideProcess,
     afNonSideBySideMachine, afEnableJITcompileTracking,
     afDisableJITcompileOptimizer);

{ TODO : Move resourcestring to JclResources }
resourcestring
  RsUnknownClassLayout      = 'Unknown class layout - $%.8x';
  RsUnknownStringFormatting = 'Unknown string formatting - $%.8x';

{ TJclClrStream }

constructor TJclClrStream.Create(const AMetadata: TJclPeMetadata;
  const AHeader: PClrStreamHeader);
begin
  inherited Create;

  FMetadata := AMetadata;
  FHeader   := AHeader;
end;

function TJclClrStream.GetName: string;
begin
  Result := FHeader.Name;
end;

function TJclClrStream.GetOffset: DWORD;
begin
  Result := Data - Metadata.Image.LoadedImage.MappedAddress;
end;

function TJclClrStream.GetSize: DWORD;
begin
  Result := FHeader.Size;
end;

function TJclClrStream.GetData: Pointer;
begin
  Result := Pointer(DWORD(FMetadata.Header) + FHeader.Offset);
end;

{ TJclClrStringsStream }

constructor TJclClrStringsStream.Create(
  const AMetadata: TJclPeMetadata; const AHeader: PClrStreamHeader);
var
  pch: PChar;
  off: DWORD;
begin
  inherited;

  FStrings := TStringList.Create;
  pch      := Data;
  off      := 0;
  while off < Size do
  begin
    if pch^ <> #0 then
      FStrings.AddObject(pch, TObject(off));
    pch := pch + StrLen(pch) + 1;
    off := DWORD(pch - Data);
  end;
end;

destructor TJclClrStringsStream.Destroy;
begin
  FreeAndNil(FStrings);

  inherited;
end;

function TJclClrStringsStream.GetString(const Idx: Integer): WideString;
begin
  Result := UTF8ToWideString(FStrings.Strings[Idx]);
end;

function TJclClrStringsStream.GetStringCount: Integer;
begin
  Result := FStrings.Count;
end;

function TJclClrStringsStream.At(const Offset: DWORD): WideString;
var
  Idx: Integer;
begin
  Idx := FStrings.IndexOfObject(TObject(Offset));
  if Idx <> -1 then
    Result := GetString(Idx)
  else
    Result := '';
end;

{ TJclClrGuidStream }

constructor TJclClrGuidStream.Create(
  const AMetadata: TJclPeMetadata; const AHeader: PClrStreamHeader);
var
  I: Integer;
  pg: PGUID;
begin
  inherited;

  SetLength(FGuids, Size div SizeOf(TGuid));
  pg := Data;
  for I:=0 to GetGuidCount-1 do
  begin
    FGuids[I] := pg^;
    Inc(pg);
  end;
end;

function TJclClrGuidStream.GetGuid(const Idx: Integer): TGUID;
begin
  Assert((0 <= Idx) and (Idx < GetGuidCount));
  Result := FGuids[Idx];
end;

function TJclClrGuidStream.GetGuidCount: Integer;
begin
  Result := Length(FGuids);
end;

{ TJclClrBlobRecord }

constructor TJclClrBlobRecord.Create(const AStream: TJclClrStream; const APtr: PByteArray);
var
  b: Byte;
  AData: Pointer;
  ASize: DWORD;
begin
  FPtr    := APtr;
  FOffset := DWORD(FPtr) - DWORD(AStream.Data);

  b := FPtr[0];
  if b = 0 then
  begin
    AData := @FPtr[1];
    ASize := 0;
  end
  else if ((b and $C0) = $C0) and ((b and $20) = 0) then    // 110bs
  begin
    AData := @FPtr[4];
    ASize := ((b and $1F) shl 24) + (FPtr[1] shl 16) + (FPtr[2] shl 8) + FPtr[3];
  end
  else if ((b and $80) = $80) and ((b and $40) = 0) then    // 10bs
  begin
    AData := @FPtr[2];
    ASize := ((b and $3F) shl 8) + FPtr[1];
  end
  else
  begin
    AData := @FPtr[1];
    ASize := b and $7F;
  end;

  Assert(not IsBadReadPtr(AData, ASize));

  inherited Create(AData, ASize);
end;

{ TJclClrBlobStream }

constructor TJclClrBlobStream.Create(
  const AMetadata: TJclPeMetadata; const AHeader: PClrStreamHeader);
var
  ABlob: TJclClrBlobRecord;
begin
  inherited;

  FBlobs := TObjectList.Create;

  ABlob := TJclClrBlobRecord.Create(Self, Data);
  while Assigned(ABlob) do
  begin
    if ABlob.Size > 0 then
      FBlobs.Add(ABlob);
    if (Integer(ABlob.Memory) + ABlob.Size) < (Integer(Self.Data) + Integer(Self.Size)) then
      ABlob := TJclClrBlobRecord.Create(Self, Pointer(Integer(ABlob.Memory) + ABlob.Size))
    else
      ABlob := nil;
  end;
end;

destructor TJclClrBlobStream.Destroy;
begin
  FreeAndNil(FBlobs);

  inherited;
end;

function TJclClrBlobStream.At(const Offset: DWORD): TJclClrBlobRecord;
var
  I: Integer;
begin
  for I:=0 to FBlobs.Count-1 do
  begin
    Result := TJclClrBlobRecord(FBlobs.Items[I]);
    if Result.Offset = Offset then
      Exit;
  end;
  Result := nil;
end;

function TJclClrBlobStream.GetBlob(const Idx: Integer): TJclClrBlobRecord;
begin
  Result := TJclClrBlobRecord(FBlobs.Items[Idx])
end;

function TJclClrBlobStream.GetBlobCount: Integer;
begin
  Result := FBlobs.Count;
end;

{ TJclClrUserStringStream }

function TJclClrUserStringStream.GetString(const Idx: Integer): WideString;
var
  ABlob: TJclClrBlobRecord;
begin
  ABlob := Blobs[Idx];
  SetLength(Result, ABlob.Size div 2 + 1);
  StrLCopyW(PWideChar(Result), PWideChar(ABlob.Memory), ABlob.Size div 2);
end;

function TJclClrUserStringStream.GetStringCount: Integer;
begin
  Result := BlobCount;
end;

function TJclClrUserStringStream.At(const Offset: DWORD): WideString;
var
  ABlob: TJclClrBlobRecord;
begin
  ABlob := inherited At(Offset);
  if Assigned(ABlob) then
  begin
    SetLength(Result, ABlob.Size div 2 + 1);
    StrLCopyW(PWideChar(Result), PWideChar(ABlob.Memory), ABlob.Size div 2);
  end
  else
    Result := '';
end;

{ TJclClrTableRow }

constructor TJclClrTableRow.Create(const ATable: TJclClrTable);
begin
  inherited Create;

  FTable := ATable;
  FIndex := Table.RealRowCount;
end;

procedure TJclClrTableRow.Update;
begin
  // do nothing, just for override
end;

{ TJclClrTable }

constructor TJclClrTable.Create(const AStream: TJclClrTableStream;
  const Ptr: Pointer; const ARowCount: Integer);
begin
  inherited Create;

  FStream   := AStream;
  FData     := Ptr;
  FRows     := nil; // Create on demand
  FRowCount := ARowCount;

  Reset;
  Load;
  SetSize(FPtr - FData);
end;

destructor TJclClrTable.Destroy;
begin
  FreeAndNil(FRows);

  inherited;
end;

procedure TJclClrTable.Reset;
begin
  FPtr := FData;
end;

procedure TJclClrTable.Load;
var
  I: Integer;
begin
  Assert(RowCount > 0);

  if TableRowClass <> TJclClrTableRow then
    for I:=0 to RowCount-1 do
      AddRow(TableRowClass.Create(Self));
end;

procedure TJclClrTable.SetSize(const Value: Integer);
begin
  FSize := Value;
  Assert(not IsBadReadPtr(FData, FSize));
end;

function TJclClrTable.GetOffset: DWORD;
begin
  Result := DWORD(Data) - DWORD(Stream.Metadata.Image.LoadedImage.MappedAddress);
end;

function TJclClrTable.GetRow(const Idx: Integer): TJclClrTableRow;
begin
  Result := TJclClrTableRow(FRows.Items[Idx]);
end;

function TJclClrTable.GetRowCount: Integer;
begin
  Result := FRowCount;
end;

function TJclClrTable.AddRow(const ARow: TJclClrTableRow): Integer;
begin
  if not Assigned(FRows) then
    FRows := TObjectList.Create;

  Result := FRows.Add(ARow);
end;

function TJclClrTable.RealRowCount: Integer;
begin
  if Assigned(FRows) then
    Result := FRows.Count
  else
    Result := 0;
end;

function TJclClrTable.ReadIndex(const HeapKind: TJclClrHeapKind): DWORD;
begin
  if IsWideIndex(HeapKind) then
    Result := ReadDWORD
  else
    Result := ReaDWORD;
end;

function TJclClrTable.ReadIndex(const TableKinds: array of TJclClrTableKind): DWORD;
begin
  if IsWideIndex(TableKinds) then
    Result := ReadDWORD
  else
    Result := ReaDWORD;
end;

function TJclClrTable.IsWideIndex(const HeapKind: TJclClrHeapKind): Boolean;
begin
  Result := Stream.BigHeap[HeapKind];
end;

function TJclClrTable.IsWideIndex(const TableKinds: array of TJclClrTableKind): Boolean;
var
  I: Integer;
  ATable: TJclClrTable;
begin
  Result := False;
  for I:=Low(TableKinds) to High(TableKinds) do
    if Stream.FindTable(TableKinds[I], ATable) then
      Result := Result or (ATable.RowCount > MAXWORD);
end;

function TJclClrTable.ReadByte: Byte;
begin
  Result := PByte(FPtr)^;
  Inc(FPtr, SizeOf(Byte));
end;

function TJclClrTable.ReaDWORD: Word;
begin
  Result := PWord(FPtr)^;
  Inc(FPtr, SizeOf(Word));
end;

function TJclClrTable.ReadDWORD: DWORD;
begin
  Result := PDWORD(FPtr)^;
  Inc(FPtr, SizeOf(DWORD));
end;

class function TJclClrTable.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableRow;
end;

procedure TJclClrTable.Update;
var
  I: Integer;
begin
  if Assigned(FRows) then
  for I:=0 to RowCount-1 do
    Rows[I].Update;
end;

function TJclClrTable.GetCodedIndexTag(const CodedIndex, TagWidth: DWORD;
  const WideIndex: Boolean): DWORD;
var
  I, TagMask: DWORD;
begin
  TagMask := 0;
  for I:=0 to TagWidth-1 do
    TagMask := TagMask or (1 shl I);
  Result := CodedIndex and TagMask;
end;

function TJclClrTable.GetCodedIndexValue(const CodedIndex, TagWidth: DWORD;
  const WideIndex: Boolean): DWORD;
const
  IndexBits: array[Boolean] of DWORD = (SizeOf(WORD) * 8, SizeOf(DWORD) * 8);
var
  I, ValueMask: DWORD;
begin
  ValueMask := 0;
  for I:=TagWidth to IndexBits[WideIndex]-1 do
    ValueMask := ValueMask or (1 shl I);
  Result := (CodedIndex and ValueMask) shr TagWidth;
end;

{ TJclClrTableModuleRow }

constructor TJclClrTableModuleRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FGeneration   := Table.ReaDWORD;            // Generation (reserved, shall be zero)
  FNameOffset   := Table.ReadIndex(hkString); // Name (index into String heap)
  FMvidIdx      := Table.ReadIndex(hkGuid);   // Mvid (index into Guid heap)
  FEncIdIdx     := Table.ReadIndex(hkGuid);   // Mvid (index into Guid heap)
  FEncBaseIdIdx := Table.ReadIndex(hkGuid);   // Mvid (index into Guid heap)
end;

function TJclClrTableModuleRow.HasEncId: Boolean;
begin
  Result := FEncIdIdx > 0;
end;

function TJclClrTableModuleRow.HasEncBaseId: Boolean;
begin
  Result := FEncBaseIdIdx > 0;
end;

function TJclClrTableModuleRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
  Assert(Result <> ''); // Name shall index a non-null string.
  Assert(Length(Result) < MAX_PATH_NAME);
end;

function TJclClrTableModuleRow.GetMvid: TGUID;
begin
  // Mvid shall index a non-null GUID in the Guid heap
  Assert(FMvidIdx <= DWORD(Table.Stream.Metadata.GuidCount));
  Result := Table.Stream.Metadata.Guids[FMvidIdx-1];
end;

function TJclClrTableModuleRow.GetEncId: TGUID;
begin
  Result := Table.Stream.Metadata.Guids[FEncIdIdx-1];
end;

function TJclClrTableModuleRow.GetEncBaseId: TGUID;
begin
  Result := Table.Stream.Metadata.Guids[FEncBaseIdIdx-1];
end;

{ TJclClrTableModule }

function TJclClrTableModule.GetRow(const Idx: Integer): TJclClrTableModuleRow;
begin
  Result := TJclClrTableModuleRow(inherited GetRow(Idx));
end;

class function TJclClrTableModule.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableModuleRow;
end;

{ TJclClrTableModuleRefRow }

constructor TJclClrTableModuleRefRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FNameOffset := Table.ReadIndex(hkString);
end;

function TJclClrTableModuleRefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

{ TJclClrTableModuleRef }

function TJclClrTableModuleRef.GetRow(const Idx: Integer): TJclClrTableModuleRefRow;
begin
  Result := TJclClrTableModuleRefRow(inherited GetRow(Idx));
end;

class function TJclClrTableModuleRef.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableModuleRefRow;
end;

{ TJclClrTableAssemblyRow }

constructor TJclClrTableAssemblyRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FHashAlgId       := Table.ReadDWORD;

  FMajorVersion    := Table.ReaDWORD;
  FMinorVersion    := Table.ReaDWORD;
  FBuildNumber     := Table.ReaDWORD;
  FRevisionNumber  := Table.ReaDWORD;

  FFlagMask        := Table.ReadDWORD;

  FPublicKeyOffset := Table.ReadIndex(hkBlob);
  FNameOffset      := Table.ReadIndex(hkString);
  FCultureOffset   := Table.ReadIndex(hkString);
end;

function TJclClrTableAssemblyRow.GetCulture: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FCultureOffset);
end;

function TJclClrTableAssemblyRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclClrTableAssemblyRow.GetPublicKey: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FPublicKeyOffset);
end;

function TJclClrTableAssemblyRow.GetVersion: string;
begin
  Result := FormatVersionString(FMajorVersion, FMinorVersion, FBuildNumber, FRevisionNumber);
end;

function TJclClrTableAssemblyRow.GetFlags: TJclClrAssemblyFlags;
begin
  Result := AssemblyFlags(FFlagMask);
end;

class function TJclClrTableAssemblyRow.AssemblyFlags(const Flags: DWORD): TJclClrAssemblyFlags;
var
  AFlag: TJclClrAssemblyFlag;
begin
  Result := [];
  for AFlag := Low(TJclClrAssemblyFlag) to High(TJclClrAssemblyFlag) do
    if (Flags and ClrAssemblyFlagMapping[AFlag]) = ClrAssemblyFlagMapping[AFlag] then
      Include(Result, AFlag);
end;

class function TJclClrTableAssemblyRow.AssemblyFlags(const Flags: TJclClrAssemblyFlags): DWORD;
var
  AFlag: TJclClrAssemblyFlag;
begin
  Result := 0;
  for AFlag := Low(TJclClrAssemblyFlag) to High(TJclClrAssemblyFlag) do
    if AFlag in Flags then
      Result := Result or ClrAssemblyFlagMapping[AFlag];
end;

{ TJclClrTableAssembly }
function TJclClrTableAssembly.GetRow(const Idx: Integer): TJclClrTableAssemblyRow;
begin
  Result := TJclClrTableAssemblyRow(inherited GetRow(Idx));
end;

class function TJclClrTableAssembly.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableAssemblyRow;
end;

{ TJclClrTableAssemblyOSRow }

constructor TJclClrTableAssemblyOSRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FPlatformID   := Table.ReadDWORD;
  FMajorVersion := Table.ReadDWORD;
  FMinorVersion := Table.ReadDWORD;
end;

function TJclClrTableAssemblyOSRow.GetVersion: string;
begin
  Result := FormatVersionString(FMajorVersion, FMinorVersion);
end;

{ TJclClrTableAssemblyOS }

function TJclClrTableAssemblyOS.GetRow(const Idx: Integer): TJclClrTableAssemblyOSRow;
begin
  Result := TJclClrTableAssemblyOSRow(inherited GetRow(Idx));
end;

class function TJclClrTableAssemblyOS.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableAssemblyOSRow;
end;

{ TJclClrTableAssemblyProcessorRow }

constructor TJclClrTableAssemblyProcessorRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FProcessor := Table.ReadDWORD;
end;

{ TJclClrTableAssemblyProcessor }

function TJclClrTableAssemblyProcessor.GetRow(const Idx: Integer): TJclClrTableAssemblyProcessorRow;
begin
  Result := TJclClrTableAssemblyProcessorRow(inherited GetRow(Idx));
end;

class function TJclClrTableAssemblyProcessor.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableAssemblyProcessorRow;
end;

{ TJclClrTableAssemblyRefRow }

constructor TJclClrTableAssemblyRefRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FMajorVersion           := Table.ReaDWORD;
  FMinorVersion           := Table.ReaDWORD;
  FBuildNumber            := Table.ReaDWORD;
  FRevisionNumber         := Table.ReaDWORD;

  FFlagMask               := Table.ReadDWORD;

  FPublicKeyOrTokenOffset := Table.ReadIndex(hkBlob);
  FNameOffset             := Table.ReadIndex(hkString);
  FCultureOffset          := Table.ReadIndex(hkString);
  FHashValueOffsetOffset  := Table.ReadIndex(hkBlob);
end;

function TJclClrTableAssemblyRefRow.GetCulture: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FCultureOffset);
end;

function TJclClrTableAssemblyRefRow.GetFlags: TJclClrAssemblyFlags;
begin
  Result := TJclClrTableAssemblyRow.AssemblyFlags(FFlagMask);
end;

function TJclClrTableAssemblyRefRow.GetHashValue: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FHashValueOffsetOffset);
end;

function TJclClrTableAssemblyRefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclClrTableAssemblyRefRow.GetPublicKeyOrToken: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FPublicKeyOrTokenOffset);
end;

function TJclClrTableAssemblyRefRow.GetVersion: string;
begin
  Result := FormatVersionString(FMajorVersion, FMinorVersion, FBuildNumber, FRevisionNumber);
end;

{ TJclClrTableAssemblyRef }

function TJclClrTableAssemblyRef.GetRow(const Idx: Integer): TJclClrTableAssemblyRefRow;
begin
  Result := TJclClrTableAssemblyRefRow(inherited GetRow(Idx));
end;

class function TJclClrTableAssemblyRef.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableAssemblyRefRow;
end;

{ TJclClrTableAssemblyRefOSRow }

constructor TJclClrTableAssemblyRefOSRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FAssemblyRefIdx := Table.ReadIndex([ttAssemblyRef]);
end;

function TJclClrTableAssemblyRefOSRow.GetAssemblyRef: TJclClrTableAssemblyRefRow;
var
  AssemblyRefTable: TJclClrTableAssemblyRef;
begin
  if Table.Stream.FindTable(ttAssemblyRef, TJclClrTable(AssemblyRefTable)) then
    Result := AssemblyRefTable[FAssemblyRefIdx-1]
  else
    Result := nil;
end;

{ TJclClrTableAssemblyRefOS }

function TJclClrTableAssemblyRefOS.GetRow(const Idx: Integer): TJclClrTableAssemblyRefOSRow;
begin
  Result := TJclClrTableAssemblyRefOSRow(inherited GetRow(Idx));
end;

class function TJclClrTableAssemblyRefOS.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableAssemblyRefOSRow;
end;

{ TJclClrTableAssemblyRefProcessorRow }

constructor TJclClrTableAssemblyRefProcessorRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FAssemblyRefIdx := Table.ReadIndex([ttAssemblyRef]);
end;

function TJclClrTableAssemblyRefProcessorRow.GetAssemblyRef: TJclClrTableAssemblyRefRow;
var
  AssemblyRefTable: TJclClrTableAssemblyRef;
begin
  if Table.Stream.FindTable(ttAssemblyRef, TJclClrTable(AssemblyRefTable)) then
    Result := AssemblyRefTable[FAssemblyRefIdx-1]
  else
    Result := nil;
end;

{ TJclClrTableAssemblyRefProcessor }

function TJclClrTableAssemblyRefProcessor.GetRow(
  const Idx: Integer): TJclClrTableAssemblyRefProcessorRow;
begin
  Result := TJclClrTableAssemblyRefProcessorRow(inherited GetRow(Idx));
end;

class function TJclClrTableAssemblyRefProcessor.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableAssemblyRefProcessorRow;
end;

{ TJclClrTableClassLayoutRow }

constructor TJclClrTableClassLayoutRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FPackingSize := Table.ReaDWORD;
  FClassSize   := Table.ReadDWORD;
  FParentIdx   := Table.ReadIndex([ttTypeDef]);
end;

{ TJclClrTableClassLayout }

function TJclClrTableClassLayout.GetRow(const Idx: Integer): TJclClrTableClassLayoutRow;
begin
  Result := TJclClrTableClassLayoutRow(inherited GetRow(Idx));
end;

class function TJclClrTableClassLayout.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableClassLayoutRow;
end;

{ TJclClrTableConstantRow }

constructor TJclClrTableConstantRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FKind      := Table.ReadByte;
  Table.ReadByte; // padding zero
  FParentIdx := Table.ReadIndex([ttParamDef, ttFieldDef, ttProperty]);
  FValueOffset  := Table.ReadIndex(hkBlob);
end;

{ TJclClrTableConstant }

function TJclClrTableConstant.GetRow(const Idx: Integer): TJclClrTableConstantRow;
begin
  Result := TJclClrTableConstantRow(inherited GetRow(Idx));
end;

class function TJclClrTableConstant.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableConstantRow;
end;

{ TJclClrTableCustomAttributeRow }

constructor TJclClrTableCustomAttributeRow.Create(
  const ATable: TJclClrTable);
begin
  inherited;

  FParentIdx   := Table.ReadIndex([ttModule, ttTypeRef, ttTypeDef, ttFieldDef,
    ttMethodDef, ttParamDef, ttInterfaceImpl, ttMemberRef, ttConstant,
    ttFieldMarshal, ttDeclSecurity, ttClassLayout, ttFieldLayout, ttSignature,
    ttEventMap, ttEvent, ttPropertyMap, ttProperty, ttMethodSemantics,
    ttMethodImpl, ttModuleRef, ttTypeSpec, ttImplMap, ttFieldRVA, ttAssembly,
    ttAssemblyProcessor, ttAssemblyOS, ttAssemblyRef, ttAssemblyRefProcessor,
    ttAssemblyRefOS, ttFile, ttExportedType, ttManifestResource, ttNestedClass]);
  FTypeIdx     := Table.ReadIndex([ttMethodDef, ttMemberRef]);
  FValueOffset := Table.ReadIndex(hkBlob);
end;

function TJclClrTableCustomAttributeRow.GetParent: TJclClrTableRow;
const
  MapTagToTable: array[0..18] of TJclClrTableKind =
  (ttMethodDef, ttFieldDef, ttTypeRef, ttTypeDef, ttParamDef, ttInterfaceImpl,
   ttMemberRef, ttModule, ttDeclSecurity, ttProperty, ttEvent, ttSignature,
   ttModuleRef, ttTypeSpec, ttAssembly, ttAssemblyRef, ttFile, ttExportedType,
   ttManifestResource);
var
  WideIndex: Boolean;
begin
  WideIndex := Table.IsWideIndex([ttModule, ttTypeRef, ttTypeDef, ttFieldDef,
    ttMethodDef, ttParamDef, ttInterfaceImpl, ttMemberRef, ttConstant,
    ttFieldMarshal, ttDeclSecurity, ttClassLayout, ttFieldLayout, ttSignature,
    ttEventMap, ttEvent, ttPropertyMap, ttProperty, ttMethodSemantics,
    ttMethodImpl, ttModuleRef, ttTypeSpec, ttImplMap, ttFieldRVA, ttAssembly,
    ttAssemblyProcessor, ttAssemblyOS, ttAssemblyRef, ttAssemblyRefProcessor,
    ttAssemblyRefOS, ttFile, ttExportedType, ttManifestResource, ttNestedClass]);

  Assert(Table.GetCodedIndexTag(FParentIdx, 5, WideIndex) <= 18);
  Result := Table.Stream.Tables[
    MapTagToTable[Table.GetCodedIndexTag(FParentIdx, 5, WideIndex)]].
    Rows[Table.GetCodedIndexValue(FParentIdx, 5, WideIndex)-1];
end;

function TJclClrTableCustomAttributeRow.GetMethod: TJclClrTableRow;
const
  MapTagToTable: array[2..3] of TJclClrTableKind = (ttMethodDef, ttMemberRef);
var
  WideIndex: Boolean;
begin
  WideIndex := Table.IsWideIndex([ttMethodDef, ttMemberRef]);
  Assert(Table.GetCodedIndexTag(FTypeIdx, 3, WideIndex) in [2, 3]);
  Result := Table.Stream.Tables[
    MapTagToTable[Table.GetCodedIndexTag(FTypeIdx, 3, WideIndex)]].
    Rows[Table.GetCodedIndexValue(FTypeIdx, 3, WideIndex)-1];
end;

function TJclClrTableCustomAttributeRow.GetValue: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FValueOffset);
end;

{ TJclClrTableCustomAttribute }

function TJclClrTableCustomAttribute.GetRow(const Idx: Integer): TJclClrTableCustomAttributeRow;
begin
  Result := TJclClrTableCustomAttributeRow(inherited GetRow(Idx));
end;

class function TJclClrTableCustomAttribute.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableCustomAttributeRow;
end;

{ TJclClrTableDeclSecurityRow }

constructor TJclClrTableDeclSecurityRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FAction              := Table.ReaDWORD;
  FParentIdx           := Table.ReadIndex([ttTypeDef, ttMethodDef, ttAssembly]);
  FPermissionSetOffset := Table.ReadIndex(hkBlob);
end;

{ TJclClrTableDeclSecurity }

function TJclClrTableDeclSecurity.GetRow(const Idx: Integer): TJclClrTableDeclSecurityRow;
begin
  Result := TJclClrTableDeclSecurityRow(inherited GetRow(Idx));
end;

class function TJclClrTableDeclSecurity.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableDeclSecurityRow;
end;

{ TJclClrTableEventMapRow }

constructor TJclClrTableEventMapRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FParentIdx := Table.ReadIndex([ttTypeDef]);
  FEventListIdx := Table.ReadIndex([ttEvent]);
end;

{ TJclClrTableEventMap }

function TJclClrTableEventMap.GetRow(const Idx: Integer): TJclClrTableEventMapRow;
begin
  Result := TJclClrTableEventMapRow(inherited GetRow(Idx));
end;

class function TJclClrTableEventMap.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableEventMapRow;
end;

{ TJclClrTableEventRow }

constructor TJclClrTableEventRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FEventFlags   := Table.ReaDWORD;
  FNameOffset   := Table.ReadIndex(hkString);
  FEventTypeIdx := Table.ReadIndex([ttTypeDef, ttTypeRef, ttTypeSpec]);
end;

function TJclClrTableEventRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

{ TJclClrTableEvent }

function TJclClrTableEvent.GetRow(const Idx: Integer): TJclClrTableEventRow;
begin
  Result := TJclClrTableEventRow(inherited GetRow(Idx));
end;

class function TJclClrTableEvent.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableEventRow;
end;

{ TJclClrTableExportedTypeRow }

constructor TJclClrTableExportedTypeRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FFlags               := Table.ReadDWORD;
  FTypeDefIdx          := Table.ReadDWORD;
  FTypeNameOffset      := Table.ReadIndex(hkString);
  FTypeNamespaceOffset := Table.ReadIndex(hkString);
  FImplementationIdx   := Table.ReadIndex([ttFile, ttExportedType]);
end;

function TJclClrTableExportedTypeRow.GetTypeName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FTypeNameOffset);
end;

function TJclClrTableExportedTypeRow.GetTypeNamespace: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FTypeNamespaceOffset);
end;

{ TJclClrTableExportedType }

function TJclClrTableExportedType.GetRow(const Idx: Integer): TJclClrTableExportedTypeRow;
begin
  Result := TJclClrTableExportedTypeRow(inherited GetRow(Idx));
end;

class function TJclClrTableExportedType.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableExportedTypeRow;
end;

{ TJclClrTableFieldRow }

constructor TJclClrTableFieldRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FFlags        := Table.ReaDWORD;
  FNameOffset   := Table.ReadIndex(hkString);
  FSignatureOffset := Table.ReadIndex(hkBlob);
  FParentToken  := nil;
end;

function TJclClrTableFieldRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclClrTableFieldRow.GetSignature: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FSignatureOffset);
end;

procedure TJclClrTableFieldRow.SetParentToken(const ARow: TJclClrTableTypeDefRow);
begin
  FParentToken := ARow;
end;

{ TJclClrTableField }

function TJclClrTableField.GetRow(const Idx: Integer): TJclClrTableFieldRow;
begin
  Result := TJclClrTableFieldRow(inherited GetRow(Idx));
end;

class function TJclClrTableField.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableFieldRow;
end;

{ TJclClrTableFieldLayoutRow }

constructor TJclClrTableFieldLayoutRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FOffset   := Table.ReadDWORD;
  FFieldIdx := Table.ReadIndex([ttFieldDef]);
end;

{ TJclClrTableFieldLayout }

function TJclClrTableFieldLayout.GetRow(
  const Idx: Integer): TJclClrTableFieldLayoutRow;
begin
  Result := TJclClrTableFieldLayoutRow(inherited GetRow(Idx));
end;

class function TJclClrTableFieldLayout.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableFieldLayoutRow;
end;

{ TJclClrTableFieldMarshalRow }

constructor TJclClrTableFieldMarshalRow.Create(
  const ATable: TJclClrTable);
begin
  inherited;

  FParentIdx        := Table.ReadIndex([ttFieldDef, ttParamDef]);
  FNativeTypeOffset := Table.ReadIndex(hkBlob);
end;

{ TJclClrTableFieldMarshal }

function TJclClrTableFieldMarshal.GetRow(
  const Idx: Integer): TJclClrTableFieldMarshalRow;
begin
  Result := TJclClrTableFieldMarshalRow(inherited GetRow(Idx));
end;

class function TJclClrTableFieldMarshal.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableFieldMarshalRow;
end;

{ TJclClrTableFieldRVARow }

constructor TJclClrTableFieldRVARow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FRVA      := Table.ReadDWORD;
  FFieldIdx := Table.ReadIndex([ttFieldDef]);
end;

{ TJclClrTableFieldRVA }

function TJclClrTableFieldRVA.GetRow(const Idx: Integer): TJclClrTableFieldRVARow;
begin
  Result := TJclClrTableFieldRVARow(inherited GetRow(Idx));
end;

class function TJclClrTableFieldRVA.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableFieldRVARow;
end;

{ TJclClrTableFileRow }

constructor TJclClrTableFileRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FFlags           := Table.ReadDWORD;
  FNameOffset      := Table.ReadIndex(hkString);
  FHashValueOffset := Table.ReadIndex(hkBlob);
end;

function TJclClrTableFileRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclClrTableFileRow.GetHashValue: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FHashValueOffset);
end;

function TJclClrTableFileRow.GetContainsMetadata: Boolean;
const
  ffContainsNoMetaData = $0001;
begin
  Result := (FFlags and ffContainsNoMetaData) = ffContainsNoMetaData;
end;

{ TJclClrTableFile }

function TJclClrTableFile.GetRow(const Idx: Integer): TJclClrTableFileRow;
begin
  Result := TJclClrTableFileRow(inherited GetRow(Idx));
end;

class function TJclClrTableFile.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableFileRow;
end;

{ TJclClrTableImplMapRow }

constructor TJclClrTableImplMapRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FMappingFlags       := Table.ReaDWORD;
  FMemberForwardedIdx := Table.ReadIndex([ttFieldDef, ttMethodDef]);
  FImportNameOffset   := Table.ReadIndex(hkString);
  FImportScopeIdx     := Table.ReadIndex([ttModuleRef]);
end;

function TJclClrTableImplMapRow.GetImportName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FImportNameOffset);
end;

{ TJclClrTableImplMap }

function TJclClrTableImplMap.GetRow(const Idx: Integer): TJclClrTableImplMapRow;
begin
  Result := TJclClrTableImplMapRow(inherited GetRow(Idx));
end;

class function TJclClrTableImplMap.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableImplMapRow;
end;

{ TJclClrTableInterfaceImplRow }

constructor TJclClrTableInterfaceImplRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FClassIdx     := Table.ReadIndex([ttTypeDef]);
  FInterfaceIdx := Table.ReadIndex([ttTypeDef, ttTypeRef, ttTypeSpec]);
end;

{ TJclClrTableInterfaceImpl }

function TJclClrTableInterfaceImpl.GetRow(
  const Idx: Integer): TJclClrTableInterfaceImplRow;
begin
  Result := TJclClrTableInterfaceImplRow(inherited GetRow(Idx));
end;

class function TJclClrTableInterfaceImpl.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableInterfaceImplRow;
end;

{ TJclClrTableManifestResourceRow }

constructor TJclClrTableManifestResourceRow.Create(
  const ATable: TJclClrTable);
begin
  inherited;

  FOffset            := Table.ReadDWORD;
  FFlags             := Table.ReadDWORD;
  FImplementationIdx := Table.ReadIndex(hkString);
  FNameOffset        := Table.ReadIndex([ttFile, ttAssemblyRef]);
end;

function TJclClrTableManifestResourceRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

{ TJclClrTableManifestResource }

function TJclClrTableManifestResource.GetRow(
  const Idx: Integer): TJclClrTableManifestResourceRow;
begin
  Result := TJclClrTableManifestResourceRow(inherited GetRow(Idx));
end;

class function TJclClrTableManifestResource.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableManifestResourceRow;
end;

{ TJclClrTableMemberRefRow }

constructor TJclClrTableMemberRefRow.Create(
  const ATable: TJclClrTable);
begin
  inherited;

  FClassIdx     := Table.ReadIndex([ttTypeRef, ttModuleRef, ttMethodDef, ttTypeSpec, ttTypeDef]);
  FNameOffset   := Table.ReadIndex(hkString);
  FSignatureOffset := Table.ReadIndex(hkBlob);
end;

function TJclClrTableMemberRefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclClrTableMemberRefRow.GetSignature: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FSignatureOffset);
end;

function TJclClrTableMemberRefRow.GetParentClass: TJclClrTableRow;
const
  MapTagToTable: array[1..5] of TJclClrTableKind = (ttTypeRef, ttModuleRef, ttMethodDef, ttTypeSpec, ttTypeDef);
var
  WideIndex: Boolean;
begin
  WideIndex := Table.IsWideIndex([ttTypeRef, ttModuleRef, ttMethodDef, ttTypeSpec, ttTypeDef]);
  Assert(Table.GetCodedIndexTag(FClassIdx, 3, WideIndex) in [1..5]);
  Result := Table.Stream.Tables[
    MapTagToTable[Table.GetCodedIndexTag(FClassIdx, 3, WideIndex)]].
    Rows[Table.GetCodedIndexValue(FClassIdx, 3, WideIndex)-1];
end;

{ TJclClrTableMemberRef }

function TJclClrTableMemberRef.GetRow(const Idx: Integer): TJclClrTableMemberRefRow;
begin
  Result := TJclClrTableMemberRefRow(inherited GetRow(Idx));
end;

class function TJclClrTableMemberRef.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableMemberRefRow;
end;

{ TJclClrTableParamDefRow }

constructor TJclClrTableParamDefRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FFlags      := Table.ReaDWORD;
  FSequence   := Table.ReaDWORD;
  FNameOffset := Table.ReadIndex(hkString);
  FMethod     := nil;
end;

function TJclClrTableParamDefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

procedure TJclClrTableParamDefRow.SetMethod(const AMethod: TJclClrTableMethodDefRow);
begin
  FMethod := AMethod;
end;

{ TJclClrTableParamDef }

function TJclClrTableParamDef.GetRow(const Idx: Integer): TJclClrTableParamDefRow;
begin
  Result := TJclClrTableParamDefRow(inherited GetRow(Idx));
end;

class function TJclClrTableParamDef.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableParamDefRow;
end;

{ TJclClrTableMethodDefRow }

constructor TJclClrTableMethodDefRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FRVA          := Table.ReadDWORD;
  FImplFlags    := Table.ReaDWORD;
  FFlags        := Table.ReaDWORD;
  FNameOffset   := Table.ReadIndex(hkString);
  FSignatureOffset := Table.ReadIndex(hkBlob);
  FParamListIdx := Table.ReadIndex([ttParamDef]);
  FParentToken  := nil;
  FParams       := nil;
end;

destructor TJclClrTableMethodDefRow.Destroy;
begin
  FreeAndNil(FParams);

  inherited;
end;

function TJclClrTableMethodDefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclClrTableMethodDefRow.GetSignature: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FSignatureOffset);
end;

procedure TJclClrTableMethodDefRow.SetParentToken(const ARow: TJclClrTableTypeDefRow);
begin
  FParentToken := ARow;
end;

procedure TJclClrTableMethodDefRow.UpdateParams;
var
  ParamTable: TJclClrTableParamDef;
  Idx, MaxParamListIdx: DWORD;
begin
  with Table as TJclClrTableMethodDef do
  if not Assigned(FParams) and (ParamListIdx <> 0) and
     Stream.FindTable(ttParamDef, TJclClrTable(ParamTable)) then
  begin
    if RowCount > (Index+1) then
      MaxParamListIdx := Rows[Index+1].ParamListIdx-1
    else
      MaxParamListIdx := ParamTable.RowCount;
    if (ParamListIdx-1) < MaxParamListIdx then
    begin
      FParams := TList.Create;
      for Idx:=ParamListIdx-1 to MaxParamListIdx-1 do
      begin
        FParams.Add(ParamTable.Rows[Idx]);
        ParamTable.Rows[Idx].SetMethod(Self);
      end;
    end;
  end;
end;

procedure TJclClrTableMethodDefRow.Update;
begin
  UpdateParams;
end;
              
function TJclClrTableMethodDefRow.GetHasParam: Boolean;
begin
  Result := Assigned(FParams);
end;

function TJclClrTableMethodDefRow.GetParam(const Idx: Integer): TJclClrTableParamDefRow;
begin
  Result := TJclClrTableParamDefRow(FParams.Items[Idx]);
end;

function TJclClrTableMethodDefRow.GetParamCount: Integer;
begin
  Result := FParams.Count;
end;

{ TJclClrTableMethodDef }

function TJclClrTableMethodDef.GetRow(const Idx: Integer): TJclClrTableMethodDefRow;
begin
  Result := TJclClrTableMethodDefRow(inherited GetRow(Idx));
end;

class function TJclClrTableMethodDef.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableMethodDefRow;
end;

{ TJclClrTableMethodImplRow }

constructor TJclClrTableMethodImplRow.Create(
  const ATable: TJclClrTable);
begin
  inherited;

  FClassIdx             := Table.ReadIndex([ttTypeDef]);
  FMethodBodyIdx        := Table.ReadIndex([ttMethodDef, ttMemberRef]);
  FMethodDeclarationIdx := Table.ReadIndex([ttMethodDef, ttMemberRef]);
end;

{ TJclClrTableMethodImpl }

function TJclClrTableMethodImpl.GetRow(
  const Idx: Integer): TJclClrTableMethodImplRow;
begin
  Result := TJclClrTableMethodImplRow(inherited GetRow(Idx));
end;

class function TJclClrTableMethodImpl.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableMethodImplRow;
end;

{ TJclClrTableMethodSemanticsRow }

constructor TJclClrTableMethodSemanticsRow.Create(
  const ATable: TJclClrTable);
begin
  inherited;

  FSemantics      := Table.ReaDWORD;
  FMethodIdx      := Table.ReadIndex([ttMethodDef]);
  FAssociationIdx := Table.ReadIndex([ttEvent, ttProperty]);
end;

{ TJclClrTableMethodSemantics }

function TJclClrTableMethodSemantics.GetRow(
  const Idx: Integer): TJclClrTableMethodSemanticsRow;
begin
  Result := TJclClrTableMethodSemanticsRow(inherited GetRow(Idx));
end;

class function TJclClrTableMethodSemantics.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableMethodSemanticsRow;
end;

{ TJclClrTableNestedClassRow }

constructor TJclClrTableNestedClassRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FNestedClassIdx    := Table.ReadIndex([ttTypeDef]);
  FEnclosingClassIdx := Table.ReadIndex([ttTypeDef]);
end;

{ TJclClrTableNestedClass }

function TJclClrTableNestedClass.GetRow(const Idx: Integer): TJclClrTableNestedClassRow;
begin
  Result := TJclClrTableNestedClassRow(inherited GetRow(Idx));
end;

class function TJclClrTableNestedClass.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableNestedClassRow;
end;

{ TJclClrTablePropertyRow }

constructor TJclClrTablePropertyRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FFlags      := Table.ReaDWORD;
  FNameOffset := Table.ReadIndex(hkString);
  FKindIdx    := Table.ReadIndex(hkBlob);
end;

function TJclClrTablePropertyRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

{ TJclClrTableProperty }

function TJclClrTableProperty.GetRow(const Idx: Integer): TJclClrTablePropertyRow;
begin
  Result := TJclClrTablePropertyRow(inherited GetRow(Idx));
end;

class function TJclClrTableProperty.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTablePropertyRow;
end;

{ TJclClrTablePropertyMapRow }

constructor TJclClrTablePropertyMapRow.Create(
  const ATable: TJclClrTable);
begin
  inherited;

  FParentIdx       := Table.ReadIndex([ttTypeDef]);
  FPropertyListIdx := Table.ReadIndex([ttProperty]);
end;

{ TJclClrTablePropertyMap }

function TJclClrTablePropertyMap.GetRow(
  const Idx: Integer): TJclClrTablePropertyMapRow;
begin
  Result := TJclClrTablePropertyMapRow(inherited GetRow(Idx));
end;

class function TJclClrTablePropertyMap.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTablePropertyMapRow;
end;

{ TJclClrTableStandAloneSigRow }

constructor TJclClrTableStandAloneSigRow.Create(
  const ATable: TJclClrTable);
begin
  inherited;

  FSignatureOffset := Table.ReadIndex(hkBlob);
end;

function TJclClrTableStandAloneSigRow.GetSignature: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FSignatureOffset);
end;

{ TJclClrTableStandAloneSig }

function TJclClrTableStandAloneSig.GetRow(
  const Idx: Integer): TJclClrTableStandAloneSigRow;
begin
  Result := TJclClrTableStandAloneSigRow(inherited GetRow(Idx));
end;

class function TJclClrTableStandAloneSig.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableStandAloneSigRow;
end;

{ TJclClrTableTypeDefRow }

constructor TJclClrTableTypeDefRow.Create(const ATable: TJclClrTable);
  function GetClassLayout: TJclClrClassLayout;
  begin
    case FFlags and tdLayoutMask of
      tdAutoLayout:       Result := clAuto;
      tdSequentialLayout: Result := clSequential;
      tdExplicitLayout:   Result := clExplicit;
    else
      raise EJclError.CreateResRecFmt(@RsUnknownClassLayout, [FFlags and tdLayoutMask]);
    end;
  end;
  function GetClassSemantics: TJclClrClassSemantics;
  const
    ClassSemanticsMapping: array[Boolean] of TJclClrClassSemantics =
      (csClass, csInterface);
  begin
    Result := ClassSemanticsMapping[(FFlags and tdClassSemanticsMask) = tdInterface];
  end;
  function GetStringFormatting: TJclClrStringFormatting;
  begin
    case FFlags and tdStringFormatMask of
      tdAnsiClass:    Result := sfAnsi;
      tdUnicodeClass: Result := sfUnicode;
      tdAutoClass:    Result := sfAutoChar;
    else
      raise EJclError.CreateResRecFmt(@RsUnknownStringFormatting, [FFlags and tdStringFormatMask]);
    end;
  end;
  function GetTypeAttributes: TJclClrTypeAttributes;
  const
    TypeAttributesMapping: array[TJclClrTypeAttribute] of DWORD =
      (tdAbstract, tdSealed, tdSpecialName, tdImport,
       tdSerializable, tdBeforeFieldInit, tdRTSpecialName, tdHasSecurity);
  var
    Attr: TJclClrTypeAttribute;
  begin
    Result := [];
    for Attr:=Low(TJclClrTypeAttribute) to High(TJclClrTypeAttribute) do
      if (FFlags and TypeAttributesMapping[Attr]) = TypeAttributesMapping[Attr] then
        Include(Result, Attr); 
  end;
begin
  inherited;

  FFlags            := Table.ReadDWORD;
  FNameOffset       := Table.ReadIndex(hkString);
  FNamespaceOffset  := Table.ReadIndex(hkString);
  FExtendsIdx       := Table.ReadIndex([ttTypeDef, ttTypeRef, ttTypeSpec]);
  FFieldListIdx     := Table.ReadIndex([ttFieldDef]);
  FMethodListIdx    := Table.ReadIndex([ttMethodDef]);

  FFields           := nil;
  FMethods          := nil;

  FClassLayout      := GetClassLayout;
  FClassSemantics   := GetClassSemantics;
  FStringFormatting := GetStringFormatting;
  FVisibility       := TJclClrTypeVisibility(FFlags and tdVisibilityMask);

  FAttributes       := GetTypeAttributes;
end;

destructor TJclClrTableTypeDefRow.Destroy;
begin
  FreeAndNil(FFields);
  FreeAndNil(FMethods);

  inherited;
end;

function TJclClrTableTypeDefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclClrTableTypeDefRow.GetNamespace: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNamespaceOffset);
end;

function TJclClrTableTypeDefRow.GetField(const Idx: Integer): TJclClrTableFieldRow;
begin
  Result := TJclClrTableFieldRow(FFields.Items[Idx])
end;

function TJclClrTableTypeDefRow.GetFieldCount: Integer;
begin
  Result := FFields.Count
end;

function TJclClrTableTypeDefRow.HasField: Boolean;
begin
  Result := Assigned(FFields);
end;

function TJclClrTableTypeDefRow.GetMethod(const Idx: Integer): TJclClrTableMethodDefRow;
begin
  Result := TJclClrTableMethodDefRow(FMethods.Items[Idx])
end;

function TJclClrTableTypeDefRow.GetMethodCount: Integer;
begin
  Result := FMethods.Count
end;

function TJclClrTableTypeDefRow.HasMethod: Boolean;
begin
  Result := Assigned(FMethods);
end;

procedure TJclClrTableTypeDefRow.UpdateFields;
var
  FieldTable: TJclClrTableField;
  Idx, MaxFieldListIdx: DWORD;
begin
  with Table as TJclClrTableTypeDef do
  if not Assigned(FFields) and (FieldListIdx <> 0) and
     Stream.FindTable(ttFieldDef, TJclClrTable(FieldTable)) then
  begin
    if RowCount > (Index+1) then
      MaxFieldListIdx := Rows[Index+1].FieldListIdx-1
    else
      MaxFieldListIdx := FieldTable.RowCount;
    if (FieldListIdx-1) < MaxFieldListIdx then
    begin
      FFields := TList.Create;
      for Idx:=FieldListIdx-1 to MaxFieldListIdx-1 do
      begin
        FFields.Add(FieldTable.Rows[Idx]);
        FieldTable.Rows[Idx].SetParentToken(Self);
      end;
    end;
  end;
end;

procedure TJclClrTableTypeDefRow.UpdateMethods;
var
  MethodTable: TJclClrTableMethodDef;
  Idx, MaxMethodListIdx: DWORD;
begin
  with Table as TJclClrTableTypeDef do
  if not Assigned(FMethods) and (MethodListIdx <> 0) and
     Stream.FindTable(ttMethodDef, TJclClrTable(MethodTable)) then
  begin
    if RowCount > (Index+1) then
      MaxMethodListIdx := Rows[Index+1].MethodListIdx-1
    else
      MaxMethodListIdx := MethodTable.RowCount;
    if (MethodListIdx-1) < MaxMethodListIdx then
    begin
      FMethods := TList.Create;
      for Idx:=MethodListIdx-1 to MaxMethodListIdx-1 do
      begin
        FMethods.Add(MethodTable.Rows[Idx]);
        MethodTable.Rows[Idx].SetParentToken(Self);
      end;
    end;
  end;
end;

procedure TJclClrTableTypeDefRow.Update;
begin
  inherited;

  UpdateFields;
  UpdateMethods;
end;

function TJclClrTableTypeDefRow.GetFullName: WideString;
begin
  if FNamespaceOffset <> 0 then
    Result := Namespace + '.' + Name
  else
    Result := Name;
end;

{ TJclClrTableTypeDef }

class function TJclClrTableTypeDef.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableTypeDefRow;
end;

function TJclClrTableTypeDef.GetRow(const Idx: Integer): TJclClrTableTypeDefRow;
begin
  Result := TJclClrTableTypeDefRow(inherited GetRow(Idx));
end;

{ TJclClrTableTypeRefRow }

constructor TJclClrTableTypeRefRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FResolutionScopeIdx := Table.ReadIndex([ttModule, ttModuleRef, ttAssemblyRef, ttTypeRef]);
  FNameOffset         := Table.ReadIndex(hkString);
  FNamespaceOffset    := Table.ReadIndex(hkString);
end;

function TJclClrTableTypeRefRow.GetResolutionScopeIdx: DWORD;
begin
  { TODO : Implement GetResolutionScopeIdx }
  Result := 0;
end;

function TJclClrTableTypeRefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclClrTableTypeRefRow.GetNamespace: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNamespaceOffset);
end;

{ TJclClrTableTypeRef }

class function TJclClrTableTypeRef.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableTypeRefRow;
end;

function TJclClrTableTypeRef.GetRow(const Idx: Integer): TJclClrTableTypeRefRow;
begin
  Result := TJclClrTableTypeRefRow(inherited GetRow(Idx));
end;

{ TJclClrTableTypeSpecRow }

constructor TJclClrTableTypeSpecRow.Create(const ATable: TJclClrTable);
begin
  inherited;

  FSignatureOffset := Table.ReadIndex(hkBlob);
end;

function TJclClrTableTypeSpecRow.GetSignature: TJclClrBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FSignatureOffset);
end;

{ TJclClrTableTypeSpec }

function TJclClrTableTypeSpec.GetRow(const Idx: Integer): TJclClrTableTypeSpecRow;
begin
  Result := TJclClrTableTypeSpecRow(inherited GetRow(Idx));
end;

class function TJclClrTableTypeSpec.TableRowClass: TJclClrTableRowClass;
begin
  Result := TJclClrTableTypeSpecRow;
end;

{ TJclClrTableStream }

constructor TJclClrTableStream.Create(const AMetadata: TJclPeMetadata;
  const AHeader: PClrStreamHeader);

  function BitCount(const Value: Int64): Integer;
  var
    AKind: TJclClrTableKind;
  begin
    Result := 0;
    for AKind:=Low(TJclClrTableKind) to High(TJclClrTableKind) do
      if (Value and (Int64(1) shl Integer(AKind))) <> 0 then
        Inc(Result);
  end;

  procedure EnumTables;
  var
    AKind: TJclClrTableKind;
    pTable: Pointer;
  begin
    pTable      := @Header.Rows[BitCount(Header.Valid)];
    FTableCount := 0;
    for AKind:=Low(TJclClrTableKind) to High(TJclClrTableKind) do
    begin
      if (Header.Valid and (Int64(1) shl Integer(AKind))) <> 0 then
      begin
        FTables[AKind] := ValidTableMapping[AKind].Create(Self, pTable, Header.Rows[FTableCount]);
        pTable := Pointer(DWORD(pTable) + FTables[AKind].Size);
        Inc(FTableCount);
      end
      else
        FTables[AKind] := nil;
    end;
  end;
begin
  inherited;

  FHeader := Data;

  EnumTables;
end;

destructor TJclClrTableStream.Destroy;
begin
  FreeAndNil(FTables);

  inherited;
end;

function TJclClrTableStream.GetVersionString: string;
begin
  Result := FormatVersionString(Header.MajorVersion, Header.MinorVersion);
end;

function TJclClrTableStream.GetTable(const AKind: TJclClrTableKind): TJclClrTable;
begin
  Result := TJclClrTable(FTables[AKind]);
end;

function TJclClrTableStream.GetBigHeap(const AHeapKind: TJclClrHeapKind): Boolean;
const
  HeapSizesMapping: array[TJclClrHeapKind] of DWORD = (1, 2, 4);
begin
  Result := (Header.HeapSizes and HeapSizesMapping[AHeapKind]) <> 0;
end;

function TJclClrTableStream.FindTable(const AKind: TJclClrTableKind;
  var ATable: TJclClrTable): Boolean;
begin
  ATable := FTables[AKind];
  Result := Assigned(ATable);
end;

procedure TJclClrTableStream.Update;
var
  AKind: TJclClrTableKind;
begin
  for AKind:=Low(TJclClrTableKind) to High(TJclClrTableKind) do
    if Assigned(FTables[AKind]) then
      FTables[AKind].Update;
end;

{ TJclPeMetadata }

constructor TJclPeMetadata.Create(const AImage: TJclPeImage);

  function GetStreamClass(const Name: string): TJclClrStreamClass;
  begin
    if CompareText(Name, '#Strings') = 0 then
      Result := TJclClrStringsStream
    else if CompareText(Name, '#GUID') = 0 then
      Result := TJclClrGuidStream
    else if CompareText(Name, '#Blob') = 0 then
      Result := TJclClrBlobStream
    else if CompareText(Name, '#US') = 0 then
      Result := TJclClrUserStringStream
    else if CompareText(Name, '#~') = 0 then
      Result := TJclClrTableStream
    else
      Result := TJclClrStream;
  end;

  procedure UpdateStreams;
  type
    PStreamPartitionHeader = ^TStreamPartitionHeader;
    TStreamPartitionHeader = packed record
      Flags,
      StreamCount: Word;
      StreamHeaders: array[0..0] of TClrStreamHeader;
    end;
  var
    pStreamPart: PStreamPartitionHeader;
    pStream: PClrStreamHeader;
    I: Integer;
    TableStream: TJclClrTableStream;
  begin
    pStreamPart := PStreamPartitionHeader(DWORD(@Header.Version[0]) + Header.Length);
    pStream     := @pStreamPart.StreamHeaders[0];
    for I:=0 to pStreamPart.StreamCount-1 do
    begin
      FStreams.Add(GetStreamClass(pStream.Name).Create(Self, pStream));

      pStream := PClrStreamHeader(DWORD(@pStream.Name[0]) +
                 (((StrLen(@pStream.Name[0])+1)+3) and (not $3)));
    end;
    if FindStream(TJclClrTableStream, TJclClrStream(TableStream)) then
      TableStream.Update;
  end;
begin
  Assert(AImage.IsClr and AImage.ClrHeader.HasMetadata);

  inherited Create;

  FImage := AImage;

  with Image.ClrHeader.Header.MetaData do
  begin
    Assert(Size > SizeOf(FHeader^));
    FHeader := Image.RvaToVa(VirtualAddress);
    Assert(not IsBadReadPtr(FHeader, Size));
  end;

  FStreams := TObjectList.Create;
  UpdateStreams;

  FStringStream := nil;
  FGuidStream   := nil;
  FBlobStream   := nil;
  FTableStream  := nil;
end;

destructor TJclPeMetadata.Destroy;
begin
  FreeAndNil(FStreams);

  inherited;
end;

function TJclPeMetadata.GetVersionString: WideString;
var
  VerStr: string;
begin
  SetLength(VerStr, Header.Length+1);
  StrlCopy(PChar(VerStr), @Header.Version[0], Header.Length);
  SetLength(VerStr, StrLen(PChar(VerStr)));
  Result := UTF8ToWideString(VerStr)
end;

function TJclPeMetadata.GetVersion: string;
begin
  Result := FormatVersionString(Header.MajorVersion, Header.MinorVersion);
end;

function TJclPeMetadata.GetStream(const Idx: Integer): TJclClrStream;
begin
  Result := TJclClrStream(FStreams.Items[Idx]);
end;

function TJclPeMetadata.GetStreamCount: Integer;
begin
  Result := FStreams.Count;
end;

function TJclPeMetadata.FindStream(const AName: string;
  var Stream: TJclClrStream): Boolean;
var
  I: Integer;
begin
  for I:=0 to GetStreamCount-1 do
  begin
    Stream := Streams[I];
    if CompareText(Stream.Name, AName) = 0 then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
  Stream := nil;
end;

function TJclPeMetadata.FindStream(const AClass: TJclClrStreamClass;
  var Stream: TJclClrStream): Boolean;
var
  I: Integer;
begin
  for I:=0 to GetStreamCount-1 do
  begin
    Stream := Streams[I];
    if Stream.ClassType = AClass then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
  Stream := nil;
end;


function TJclPeMetadata.GetToken(const AToken: TJclClrToken): TJclClrTableRow;
begin
  try
    Result := Tables[TokenTable(AToken)].Rows[TokenIndex(AToken)-1];
  except
    Result := nil;
  end;
end;

function TJclPeMetadata.GetString(const Idx: Integer): WideString;
begin
  if Assigned(FStringStream) or
     FindStream(TJclClrStringsStream, TJclClrStream(FStringStream)) then
    Result := FStringStream.Strings[Idx];
end;

function TJclPeMetadata.GetStringCount: Integer;
begin
  if Assigned(FStringStream) or
     FindStream(TJclClrStringsStream, TJclClrStream(FStringStream)) then
    Result := FStringStream.StringCount
  else
    Result := 0;
end;

function TJclPeMetadata.StringAt(const Offset: DWORD): WideString;
begin
  if Assigned(FStringStream) or
     FindStream(TJclClrStringsStream, TJclClrStream(FStringStream)) then
    Result := TJclClrStringsStream(FStringStream).At(Offset)
  else
    Result := '';
end;

function TJclPeMetadata.BlobAt(const Offset: DWORD): TJclClrBlobRecord;
begin
  if Assigned(FBlobStream) or
     FindStream(TJclClrBlobStream, TJclClrStream(FBlobStream)) then
    Result := TJclClrBlobStream(FBlobStream).At(Offset)
  else
    Result := nil;
end;

function TJclPeMetadata.GetGuid(const Idx: Integer): TGUID;
begin
  if Assigned(FGuidStream) or
     FindStream(TJclClrGuidStream, TJclClrStream(FGuidStream)) then
    Result := FGuidStream.Guids[Idx]
  else
    Result := GUID_NULL;
end;

function TJclPeMetadata.GetGuidCount: Integer;
begin
  if Assigned(FGuidStream) or
     FindStream(TJclClrGuidStream, TJclClrStream(FGuidStream)) then
    Result := FGuidStream.GuidCount
  else
    Result := 0;
end;

function TJclPeMetadata.GetBlob(const Idx: Integer): TJclClrBlobRecord;
begin
  if Assigned(FBlobStream) or
     FindStream(TJclClrBlobStream, TJclClrStream(FBlobStream)) then
    Result := FBlobStream.Blobs[Idx]
  else
    Result := nil;
end;

function TJclPeMetadata.GetBlobCount: Integer;
begin
  if Assigned(FBlobStream) or
     FindStream(TJclClrBlobStream, TJclClrStream(FBlobStream)) then
    Result := FBlobStream.BlobCount
  else
    Result := 0;
end;

function TJclPeMetadata.GetTable(const AKind: TJclClrTableKind): TJclClrTable;
begin
  if Assigned(FTableStream) or
     FindStream(TJclClrTableStream, TJclClrStream(FTableStream)) then
    Result := FTableStream.Tables[AKind]
  else
    Result := nil;
end;

function TJclPeMetadata.GetTableCount: Integer;
begin
  if Assigned(FTableStream) or
     FindStream(TJclClrTableStream, TJclClrStream(FTableStream)) then
    Result := FTableStream.TableCount
  else
    Result := 0;
end;

function TJclPeMetadata.TokenExists(const Token: TJclClrToken): Boolean;
begin
  Result := TokenIndex(Token) in [1..Tables[TokenTable(Token)].RowCount];
end;

class function TJclPeMetadata.TokenTable(const Token: TJclClrToken): TJclClrTableKind;
begin
  Result := TJclClrTableKind(Token shr 24);
end;

class function TJclPeMetadata.TokenIndex(const Token: TJclClrToken): Integer;
begin
  Result := Token and DWORD($FFFFFF);
end;

class function TJclPeMetadata.TokenCode(const Token: TJclClrToken): Integer;
begin
  Result := Token and $FF000000;
end;

class function TJclPeMetadata.MakeToken(
  const Table: TJclClrTableKind; const Idx: Integer): TJclClrToken;
begin
  Result := (DWORD(Table) shl 24) and TokenIndex(Idx);
end;

{ TJclClrResourceRecord }

constructor TJclClrResourceRecord.Create(const AData: PChar;
  const AOffset: DWORD; const ARVA: DWORD);
begin
  FData   := AData;
  FOffset := AOffset;
  FRVA    := ARVA;

  inherited Create(Pointer(DWORD(Data)+SizeOf(DWORD)), PDWORD(Data)^);
end;

{ TJclClrVTableFixupRecord }

constructor TJclClrVTableFixupRecord.Create(
  const AData: PImageCorVTableFixup);
begin
  inherited Create;

  FData := AData;
end;

function TJclClrVTableFixupRecord.GetCount: DWORD;
begin
  Result := Data.Count;
end;

function TJclClrVTableFixupRecord.GetKinds: TJclClrVTableKinds;
begin
  Result := VTableKinds(Data.Kind);
end;

function TJclClrVTableFixupRecord.GetRVA: DWORD;
begin
  Result := Data.RVA;
end;

class function TJclClrVTableFixupRecord.VTableKinds(
  const Kinds: TJclClrVTableKinds): DWORD;
var
  AKind: TJclClrVTableKind;
begin
  Result := 0;
  for AKind:=Low(TJclClrVTableKind) to High(TJclClrVTableKind) do
    if AKind in Kinds then
      Result := Result or ClrVTableKindMapping[AKind];
end;

class function TJclClrVTableFixupRecord.VTableKinds(
  const Kinds: DWORD): TJclClrVTableKinds;
var
  AKind: TJclClrVTableKind;
begin
  Result := [];
  for AKind:=Low(TJclClrVTableKind) to High(TJclClrVTableKind) do
    if (ClrVTableKindMapping[AKind] and Kinds) = ClrVTableKindMapping[AKind] then
      Include(Result, AKind);
end;

{ TJclClrInformation }

constructor TJclClrHeaderEx.Create(const AImage: TJclPeImage);
  procedure UpdateVTableFixups;
  begin
    if Header.VTableFixups.VirtualAddress = 0 then
  end;
begin
  inherited Create(AImage);

  FFlags               := ClrImageFlag(Header.Flags);
  FMetadata            := nil;
  FResources           := nil;
  FStrongNameSignature := nil;
  FVTableFixups        := nil;
end;

destructor TJclClrHeaderEx.Destroy;
begin
  FreeAndNil(FVTableFixups);
  FreeAndNil(FStrongNameSignature);
  FreeAndNil(FResources);
  FreeAndNil(FMetadata);

  inherited;
end;

class function TJclClrHeaderEx.ClrImageFlag(const Flags: DWORD): TJclClrImageFlags;
var
  AFlag: TJclClrImageFlag;
begin
  Result := [];
  for AFlag:=Low(TJclClrImageFlag) to High(TJclClrImageFlag) do
    if (ClrImageFlagMapping[AFlag] and Flags) = ClrImageFlagMapping[AFlag] then
      Include(Result, AFlag);
end;

class function TJclClrHeaderEx.ClrImageFlag(const Flags: TJclClrImageFlags): DWORD;
var
  AFlag: TJclClrImageFlag;
begin
  Result := 0;
  for AFlag:=Low(TJclClrImageFlag) to High(TJclClrImageFlag) do
    if AFlag in Flags then
      Result := Result or ClrImageFlagMapping[AFlag];
end;

function TJclClrHeaderEx.GetMetadata: TJclPeMetadata;
begin
  if not Assigned(FMetadata) and HasMetadata then
    FMetadata := TJclPeMetadata.Create(Image);
  Result := FMetadata;
end;

function TJclClrHeaderEx.HasStrongNameSignature: Boolean;
begin
  with Header.StrongNameSignature do
  Result := Assigned(FStrongNameSignature) or
            ((Size > 0) and not IsBadReadPtr(Image.RvaToVa(VirtualAddress), Size));
end;

function TJclClrHeaderEx.HasVTableFixup: Boolean;
begin
  with Header.VTableFixups do
  Result := Assigned(FVTableFixups) or
            ((Size > 0) and not IsBadReadPtr(Image.RvaToVa(VirtualAddress), Size));
end;

function TJclClrHeaderEx.GetStrongNameSignature: TCustomMemoryStream;
begin
  if not Assigned(FStrongNameSignature) and HasStrongNameSignature then
  with Header.StrongNameSignature do
    FStrongNameSignature := TJClreferenceMemoryStream.Create(Image.RvaToVa(VirtualAddress), Size);
  Result := FStrongNameSignature;
end;

function TJclClrHeaderEx.HasResources: Boolean;
begin
  with Header.Resources do
  Result := Assigned(FResources) or
            ((Size > 0) and not IsBadReadPtr(Image.RvaToVa(VirtualAddress), Size));
end;

procedure TJclClrHeaderEx.UpdateResources;
var
  Base, Ptr: PChar;
  ARes: TJclClrResourceRecord;
begin
  FResources := TObjectList.Create;

  with Header.Resources do
  begin
    Base := Image.RvaToVa(VirtualAddress);
    Ptr  := Base;
    while DWORD(Ptr-Base) < Size do
    begin
      ARes := TJclClrResourceRecord.Create(Ptr, Ptr-Base, Ptr-Image.LoadedImage.MappedAddress);
      FResources.Add(ARes);
      Ptr := PChar(ARes.Memory) + ARes.Size;
    end;
  end;
end;

function TJclClrHeaderEx.GetResource(
  const Idx: Integer): TJclClrResourceRecord;
begin
  if not Assigned(FResources) and HasResources then
    UpdateResources;

  Result := TJclClrResourceRecord(FResources.Items[Idx]);
end;

function TJclClrHeaderEx.GetResourceCount: Integer;
begin
  if not Assigned(FResources) and HasResources then
    UpdateResources;

  if Assigned(FResources) then
    Result := FResources.Count
  else
    Result := 0;
end;

function TJclClrHeaderEx.GetEntryPointToken: TJclClrTableRow;
begin
  if Header.EntryPointToken <> 0 then
    Result := Metadata.Tokens[Header.EntryPointToken]
  else
    Result := nil;
end;

function TJclClrHeaderEx.GetVTableFixup(
  const Idx: Integer): TJclClrVTableFixupRecord;
var
  I: Integer;
  pData: PImageCorVTableFixup;
begin
  if not Assigned(FVTableFixups) and HasVTableFixup then
  begin
    FVTableFixups := TObjectList.Create;

    with Header.VTableFixups do
    begin
      pData := PImageCorVTableFixup(Image.RvaToVa(VirtualAddress));
      for I:=0 to GetVTableFixupCount-1 do
      begin
        FVTableFixups.Add(TJclClrVTableFixupRecord.Create(pData));
        Inc(pData);
      end;
    end;
  end;
  Result := TJclClrVTableFixupRecord(FVTableFixups.Items[Idx]);
end;

function TJclClrHeaderEx.GetVTableFixupCount: Integer;
begin
  Result := Header.VTableFixups.Size div SizeOf(TImageCorVTableFixup);
end;

function TJclClrHeaderEx.ResourceAt(const Offset: DWORD): TJclClrResourceRecord;
var
  I: Integer;
begin
  if HasResources then
  for I:=0 to ResourceCount-1 do
  begin
    Result := Resources[I];
    if Result.Offset = Offset then
      Exit;
  end;
  Result := nil;
end;

end.
