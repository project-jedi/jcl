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
{ The Original Code is JclCLR.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Microsoft .Net framework CLR information support routines and classes.                           }
{                                                                                                  }
{ Unit owner: Flier Lu                                                                             }
{ Last modified: March 10, 2002                                                                    }
{                                                                                                  }
{**************************************************************************************************}

unit JclCLR;

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
  TJclCLRToken = DWORD;

//==================================================================================================
// Flag	Value	Description
//==================================================================================================
const  COMIMAGE_FLAGS_ILONLY	          = $00000001;	// Always 1 (see Section 23.1).
  COMIMAGE_FLAGS_32BITREQUIRED	  = $00000002;	// Image may only be loaded into a 32-bit process, for instance if there are 32-bit vtablefixups, or casts from native integers to int32. CLI implementations that have 64 bit native integers shall refuse loading binaries with this flag set.
  COMIMAGE_FLAGS_STRONGNAMESIGNED = $00000008;	// Image has a strong name signature.
  COMIMAGE_FLAGS_TRACKDEBUGDATA	  = $00010000;	// Always 0 (see Section 23.1).type  TJclClrImageFlag = (cifILOnly, cif32BitRequired, cifStrongNameSinged, cifTrackDebugData);
  TJclClrImageFlags = set of TJclClrImageFlag;

//==================================================================================================
// Values for AssemblyHashAlgorithm
//==================================================================================================
const
  haNone = $0000;
  haMD5  = $8003;
  haSHA1 = $8004;

type
  TJclClrAssemblyHashAlgorithm = (chaNone, chaMD5, chaSHA1);

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
// for the method they need to call.  At image load time, the CLR Loader will
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
  PCLRStreamHeader = ^TCLRStreamHeader;
  TCLRStreamHeader = packed record
    Offset,      // Memory offset to start of this stream from start of the metadata root
    Size: DWORD; // Size of this stream in bytes, shall be a multiple of 4.
    // Name of the stream as null terminated variable length
    // array of ASCII characters, padded with \0 characters
    Name: array[0..MaxWord] of Char;
  end;

  PCLRTableStreamHeader = ^TCLRTableStreamHeader;
  TCLRTableStreamHeader = packed record
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

  PCLRMetadataHeader = ^TCLRMetadataHeader;
  TCLRMetadataHeader = packed record
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
    StreamHeaders: array[0..n-1] of TCLRStreamHeader;
    }
  end;

type
  TJclCLRTableKind = (
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
  TJclCLRHeaderEx = class;
  TJclPeMetadata = class;

  TJclCLRStreamClass = class of TJclCLRStream;
  TJclCLRStream = class
  private
    FMetadata: TJclPeMetadata;
    FHeader: PCLRStreamHeader;
    function GetName: string;
    function GetOffset: DWORD;
    function GetSize: DWORD;
    function GetData: Pointer;
  protected
    constructor Create(const AMetadata: TJclPeMetadata;
                       const AHeader: PCLRStreamHeader); virtual;
  public
    property Metadata: TJclPeMetadata read FMetadata;
    property Header: PCLRStreamHeader read FHeader;

    property Name: string read GetName;
    property Offset: DWORD read GetOffset;
    property Size: DWORD read GetSize;
    property Data: Pointer read GetData;
  end;

  TJclCLRStringsStream = class(TJclCLRStream)
  private
    FStrings: TStrings;

    function GetString(const Idx: Integer): WideString;
    function GetStringCount: Integer;
  protected
    constructor Create(const AMetadata: TJclPeMetadata;
                       const AHeader: PCLRStreamHeader); override;
  public
    destructor Destroy; override;

    function At(const Offset: DWORD): WideString;

    property Strings[const Idx: Integer]: WideString read GetString; default;
    property StringCount: Integer read GetStringCount;
  end;

  TJclCLRGuidStream = class(TJclCLRStream)
  private
    FGuids: array of TGUID;
    function GetGuid(const Idx: Integer): TGUID;
    function GetGuidCount: Integer;
  protected
    constructor Create(const AMetadata: TJclPeMetadata;
                       const AHeader: PCLRStreamHeader); override;
  public
    property Guids[const Idx: Integer]: TGUID read GetGuid; default;
    property GuidCount: Integer read GetGuidCount;
  end;

  TJclCLRBlobRecord = class(TJclReferenceMemoryStream)
  private
    FPtr: PByteArray;
    FOffset: DWORD;
  protected
    constructor Create(const AStream: TJclCLRStream; const APtr: PByteArray);
  public
    property Ptr: PByteArray read FPtr;
    property Offset: DWORD read FOffset;
  end;

  TJclCLRBlobStream = class(TJclCLRStream)
  private
    FBlobs: TObjectList;
    function GetBlob(const Idx: Integer): TJclCLRBlobRecord;
    function GetBlobCount: Integer;
  protected
    constructor Create(const AMetadata: TJclPeMetadata;
                       const AHeader: PCLRStreamHeader); override;
  public
    destructor Destroy; override;

    function At(const Offset: DWORD): TJclCLRBlobRecord;

    property Blobs[const Idx: Integer]: TJclCLRBlobRecord read GetBlob; default;
    property BlobCount: Integer read GetBlobCount;
  end;

  TJclCLRUserStringStream = class(TJclCLRBlobStream)
  private
    function GetString(const Idx: Integer): WideString;
    function GetStringCount: Integer;
  public
    function At(const Offset: DWORD): WideString;

    property Strings[const Idx: Integer]: WideString read GetString; default;
    property StringCount: Integer read GetStringCount;
  end;

  TJclCLRTableStream = class;

  TJclCLRHeapKind = (hkString, hkGuid, hkBlob);
  TJclCLRComboIndex = (ciResolutionScope);

  TJclCLRTable = class;

  TJclCLRTableRowClass = class of TJclCLRTableRow;
  TJclCLRTableRow = class
  private
    FTable: TJclCLRTable;
    FIndex: Integer;
  protected
    constructor Create(const ATable: TJclCLRTable); virtual;

    procedure Update; virtual;
  public
    property Table: TJclCLRTable read FTable;
    property Index: Integer read FIndex;
  end;

  TJclCLRTableClass = class of TJclCLRTable;
  TJclCLRTable = class
  private
    FStream: TJclCLRTableStream;
    FData,
    FPtr: PChar;
    FRows: TObjectList;
    FRowCount: Integer;
    FSize: DWORD;
    function GetOffset: DWORD;
    function GetRow(const Idx: Integer): TJclCLRTableRow;
    function GetRowCount: Integer;
  protected
    constructor Create(const AStream: TJclCLRTableStream;
      const Ptr: Pointer; const ARowCount: Integer); virtual;
    procedure Load; virtual;
    procedure SetSize(const Value: Integer);

    procedure Update; virtual;

    function AddRow(const ARow: TJclCLRTableRow): Integer;
    function RealRowCount: Integer;

    procedure Reset;
    function ReadIndex(const HeapKind: TJclCLRHeapKind): DWORD; overload;
    function ReadIndex(const TableKinds: array of TJclCLRTableKind): DWORD; overload;
    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadDWord: DWORD;

    class function TableRowClass: TJclCLRTableRowClass; virtual;

    property Rows[const Idx: Integer]: TJclCLRTableRow read GetRow; default;
  public
    destructor Destroy; override;

    property Stream: TJclCLRTableStream read FStream;

    property Data: PChar read FData;
    property Size: DWORD read FSize;
    property Offset: DWORD read GetOffset;
    property RowCount: Integer read GetRowCount;
  end;

  TJclCLRTableModule = class(TJclCLRTable)
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
    procedure Load; override;
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

  TJclCLRTableModuleRefRow = class(TJclCLRTableRow)
  private
    FNameOffset: DWORD;
    function GetName: WideString;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property NameOffset: DWORD read FNameOffset;

    property Name: WideString read GetName;
  end;

  TJclCLRTableModuleRef = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableModuleRefRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableModuleRefRow read GetRow; default;
  end;

  TJclCLRTableAssembly = class(TJclCLRTable)
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
    function GetPublicKey: TJclCLRBlobRecord;
    function GetVersion: string;
    function GetFlags: TJclClrAssemblyFlags;
    function GetHashAlgorithm: TJclClrAssemblyHashAlgorithm;
  protected
    procedure Load; override;
  public
    class function AssemblyFlags(const Flags: TJclClrAssemblyFlags): DWORD; overload;
    class function AssemblyFlags(const Flags: DWORD): TJclClrAssemblyFlags; overload;
    class function AssemblyHashAlgorithm(const HashAlg: DWORD): TJclClrAssemblyHashAlgorithm; overload;
    class function AssemblyHashAlgorithm(const HashAlg: TJclClrAssemblyHashAlgorithm): DWORD; overload;

    property HashAlgId: DWORD read FHashAlgId;
    property MajorVersion: Word read FMajorVersion;
    property MinorVersion: Word read FMinorVersion;
    property BuildNumber: Word read FBuildNumber;
    property RevisionNumber: Word read FRevisionNumber;
    property FlagMask: DWORD read FFlagMask;
    property PublicKeyOffset: DWORD read FPublicKeyOffset;
    property NameOffset: DWORD read FNameOffset;
    property CultureOffset: DWORD read FCultureOffset;

    property PublicKey: TJclCLRBlobRecord read GetPublicKey;
    property Name: WideString read GetName;
    property Culture: WideString read GetCulture;
    property Version: string read GetVersion;
    property Flags: TJclClrAssemblyFlags read GetFlags;
    property HashAlgorithm: TJclClrAssemblyHashAlgorithm read GetHashAlgorithm;
  end;

  TJclCLRTableAssemblyOS = class(TJclCLRTable)
  private
    FPlatformID,
    FMajorVersion,
    FMinorVersion: DWORD;
    function GetString: string;
  protected
    procedure Load; override;
  public
    property PlatformID: DWORD read FPlatformID;
    property MajorVersion: DWORD read FMajorVersion;
    property MinorVersion: DWORD read FMinorVersion;

    property Version: string read GetString;
  end;

  TJclCLRTableAssemblyProcessor = class(TJclCLRTable)
  private
    FProcessor: DWORD;
  protected
    procedure Load; override;
  public
    property Processor: DWORD read FProcessor;
  end;

  TJclCLRTableAssemblyRefRow = class(TJclCLRTableRow)
  private
    FCultureOffset,
    FNameOffset,
    FPublicKeyOrTokenOffset,
    FHashValueOffsetOffset: DWORD;
    FMajorVersion,
    FRevisionNumber,
    FBuildNumber,
    FMinorVersion: Word;
    FFlags: DWORD;
    function GetCulture: WideString;
    function GetHashValue: TJclCLRBlobRecord;
    function GetName: WideString;
    function GetPublicKeyOrToken: TJclCLRBlobRecord;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property MajorVersion: Word read FMajorVersion;
    property MinorVersion: Word read FMinorVersion;
    property BuildNumber: Word read FBuildNumber;
    property RevisionNumber: Word read FRevisionNumber;
    property Flags: DWORD read FFlags;
    property PublicKeyOrTokenOffset: DWORD read FPublicKeyOrTokenOffset;
    property NameOffset: DWORD read FNameOffset;
    property CultureOffset: DWORD read FCultureOffset;
    property HashValueOffsetOffset: DWORD read FHashValueOffsetOffset;

    property PublicKeyOrToken: TJclCLRBlobRecord read GetPublicKeyOrToken;
    property Name: WideString read GetName;
    property Culture: WideString read GetCulture;
    property HashValue: TJclCLRBlobRecord read GetHashValue;
  end;

  TJclCLRTableAssemblyRef = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableAssemblyRefRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableAssemblyRefRow read GetRow; default;
  end;

  TJclCLRTableAssemblyRefOS = class(TJclCLRTableAssemblyOS)
  private
    FAssemblyRefIdx: DWORD;
    function GetAssemblyRef: TJclCLRTableAssemblyRef;
  protected
    procedure Load; override;
  public
    property AssemblyRefIdx: DWORD read FAssemblyRefIdx;

    property AssemblyRef: TJclCLRTableAssemblyRef read GetAssemblyRef;
  end;

  TJclCLRTableAssemblyRefProcessor = class(TJclCLRTableAssemblyProcessor)
  private
    FAssemblyRefIdx: DWORD;
    function GetAssemblyRef: TJclCLRTableAssemblyRef;
  protected
    procedure Load; override;
  public
    property AssemblyRefIdx: DWORD read FAssemblyRefIdx;

    property AssemblyRef: TJclCLRTableAssemblyRef read GetAssemblyRef;
  end;

  TJclCLRTableClassLayoutRow = class(TJclCLRTableRow)
  private
    FClassSize: DWORD;
    FParentIdx: DWORD;
    FPackingSize: Word;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property PackingSize: Word read FPackingSize;
    property ClassSize: DWORD read FClassSize;
    property ParentIdx: DWORD read FParentIdx;
  end;

  TJclCLRTableClassLayout = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableClassLayoutRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableClassLayoutRow read GetRow; default;
  end;

  TJclCLRTableConstantRow = class(TJclCLRTableRow)
  private
    FKind: Byte;
    FParentIdx: DWORD;
    FValueOffset: DWORD;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property Kind: Byte read FKind;
    property ParentIdx: DWORD read FParentIdx;
    property ValueOffset: DWORD read FValueOffset;
  end;

  TJclCLRTableConstant = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableConstantRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableConstantRow read GetRow; default;
  end;

  TJclCLRTableCustomAttributeRow = class(TJclCLRTableRow)
  private
    FParentIdx: DWORD;
    FTypeIdx: DWORD;
    FValueOffset: DWORD;
    function GetValue: TJclCLRBlobRecord;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property ParentIdx: DWORD read FParentIdx;
    property TypeIdx: DWORD read FTypeIdx;
    property ValueOffset: DWORD read FValueOffset;

    property Value: TJclCLRBlobRecord read GetValue;
  end;

  TJclCLRTableCustomAttribute = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableCustomAttributeRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableCustomAttributeRow read GetRow; default;
  end;

  TJclCLRTableDeclSecurityRow = class(TJclCLRTableRow)
  private
    FPermissionSetOffset: DWORD;
    FParentIdx: DWORD;
    FAction: Word;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property Action: Word read FAction;
    property ParentIdx: DWORD read FParentIdx;
    property PermissionSetOffset: DWORD read FPermissionSetOffset;  
  end;

  TJclCLRTableDeclSecurity = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableDeclSecurityRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableDeclSecurityRow read GetRow; default;
  end;

  TJclCLRTableEventMapRow = class(TJclCLRTableRow)
  private
    FEventListIdx: DWORD;
    FParentIdx: DWORD;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property ParentIdx: DWORD read FParentIdx;
    property EventListIdx: DWORD read FEventListIdx;
  end;

  TJclCLRTableEventMap = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableEventMapRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableEventMapRow read GetRow; default;
  end;

  TJclCLRTableEventRow = class(TJclCLRTableRow)
  private
    FNameOffset: DWORD;
    FEventTypeIdx: DWORD;
    FEventFlags: Word;
    function GetName: WideString;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property EventFlags: Word read FEventFlags;
    property NameOffset: DWORD read FNameOffset;
    property EventTypeIdx: DWORD read FEventTypeIdx;

    property Name: WideString read GetName;
  end;

  TJclCLRTableEvent = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableEventRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableEventRow read GetRow; default;
  end;

  TJclCLRTableExportedTypeRow = class(TJclCLRTableRow)
  private
    FTypeDefIdx: DWORD;
    FFlags: DWORD;
    FImplementationIdx: DWORD;
    FTypeNamespaceOffset: DWORD;
    FTypeNameOffset: DWORD;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property Flags: DWORD read FFlags;
    property TypeDefIdx: DWORD read FTypeDefIdx;
    property TypeNameOffset: DWORD read FTypeNameOffset;
    property TypeNamespaceOffset: DWORD read FTypeNamespaceOffset;
    property ImplementationIdx: DWORD read FImplementationIdx;      
  end;

  TJclCLRTableExportedType = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableExportedTypeRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableExportedTypeRow read GetRow; default;
  end;

  TJclCLRTableFieldRow = class(TJclCLRTableRow)
  private
    FFlags: Word;
    FNameOffset: DWORD;
    FSignatureOffset: DWORD;
    FParentToken: TJclCLRTableRow;
    function GetName: WideString;
    function GetSignature: TJclCLRBlobRecord;
  protected
    constructor Create(const ATable: TJclCLRTable); override;

    procedure SetParentToken(const ARow: TJclCLRTableRow);
  public
    property Flags: Word read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property SignatureOffset: DWORD read FSignatureOffset;

    property Name: WideString read GetName;
    property Signature: TJclCLRBlobRecord read GetSignature;

    property ParentToken: TJclCLRTableRow read FParentToken;
  end;

  TJclCLRTableField = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableFieldRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableFieldRow read GetRow; default;
  end;

  TJclCLRTableFieldLayoutRow = class(TJclCLRTableRow)
  private
    FOffset: DWORD;
    FFieldIdx: DWORD;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property Offset: DWORD read FOffset;
    property FieldIdx: DWORD read FFieldIdx;
  end;

  TJclCLRTableFieldLayout = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableFieldLayoutRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableFieldLayoutRow read GetRow; default;
  end;

  TJclCLRTableFieldMarshalRow = class(TJclCLRTableRow)
  private
    FParentIdx: DWORD;
    FNativeTypeOffset: DWORD;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property ParentIdx: DWORD read FParentIdx;
    property NativeTypeOffset: DWORD read FNativeTypeOffset;
  end;

  TJclCLRTableFieldMarshal = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableFieldMarshalRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableFieldMarshalRow read GetRow; default;
  end;

  TJclCLRTableFieldRVARow = class(TJclCLRTableRow)
  private
    FRVA: DWORD;
    FFieldIdx: DWORD;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property RVA: DWORD read FRVA;
    property FieldIdx: DWORD read FFieldIdx;
  end;

  TJclCLRTableFieldRVA = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableFieldRVARow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableFieldRVARow read GetRow; default;
  end;

  TJclCLRTableFileRow = class(TJclCLRTableRow)
  private
    FHashValueOffset: DWORD;
    FNameOffset: DWORD;
    FFlags: DWORD;
    function GetName: WideString;
    function GetHashValue: TJclCLRBlobRecord;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property Flags: DWORD read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property HashValueOffset: DWORD read FHashValueOffset;

    property Name: WideString read GetName;
    property HashValue: TJclCLRBlobRecord read GetHashValue;
  end;

  TJclCLRTableFile = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableFileRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableFileRow read GetRow; default;
  end;

  TJclCLRTableImplMapRow = class(TJclCLRTableRow)
  private
    FImportNameOffset: DWORD;
    FMemberForwardedIdx: DWORD;
    FImportScopeIdx: DWORD;
    FMappingFlags: Word;
    function GetImportName: WideString;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property MappingFlags: Word read FMappingFlags;
    property MemberForwardedIdx: DWORD read FMemberForwardedIdx;
    property ImportNameOffset: DWORD read FImportNameOffset;
    property ImportScopeIdx: DWORD read FImportScopeIdx;

    property ImportName: WideString read GetImportName;
  end;

  TJclCLRTableImplMap = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableImplMapRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableImplMapRow read GetRow; default;
  end;

  TJclCLRTableInterfaceImplRow = class(TJclCLRTableRow)
  private
    FInterfaceIdx: DWORD;
    FClassIdx: DWORD;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property ClassIdx: DWORD read FClassIdx;
    property InterfaceIdx: DWORD read FInterfaceIdx;
  end;

  TJclCLRTableInterfaceImpl = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableInterfaceImplRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableInterfaceImplRow read GetRow; default;
  end;

  TJclCLRTableManifestResourceRow = class(TJclCLRTableRow)
  private
    FOffset: DWORD;
    FFlags: DWORD;
    FImplementationIdx: DWORD;
    FNameOffset: DWORD;
    function GetName: WideString;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property Offset: DWORD read FOffset;
    property Flags: DWORD read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property ImplementationIdx: DWORD read FImplementationIdx;

    property Name: WideString read GetName;
  end;

  TJclCLRTableManifestResource = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableManifestResourceRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableManifestResourceRow read GetRow; default;
  end;

  TJclCLRTableMemberRefRow = class(TJclCLRTableRow)
  private
    FClassIdx: DWORD;
    FNameOffset: DWORD;
    FSignatureOffset: DWORD;
    function GetName: WideString;
    function GetSignature: TJclCLRBlobRecord;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property ClassIdx: DWORD read FClassIdx;
    property NameOffset: DWORD read FNameOffset;
    property SignatureOffset: DWORD read FSignatureOffset;

    property Name: WideString read GetName;
    property Signature: TJclCLRBlobRecord read GetSignature;
  end;

  TJclCLRTableMemberRef = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableMemberRefRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableMemberRefRow read GetRow; default;
  end;

  TJclCLRTableMethodDefRow = class(TJclCLRTableRow)
  private
    FRVA: DWORD;
    FImplFlags: Word;
    FFlags: Word;
    FNameOffset: DWORD;
    FSignatureOffset: DWORD;
    FParamListIdx: DWORD;
    FParentToken: TJclCLRTableRow;
    function GetName: WideString;
    function GetSignature: TJclCLRBlobRecord;
  protected
    constructor Create(const ATable: TJclCLRTable); override;

    procedure SetParentToken(const ARow: TJclCLRTableRow);
  public
    property RVA: DWORD read FRVA;
    property ImplFlags: Word read FImplFlags;
    property Flags: Word read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property SignatureOffset: DWORD read FSignatureOffset;
    property ParamListIdx: DWORD read FParamListIdx;

    property Name: WideString read GetName;
    property Signature: TJclCLRBlobRecord read GetSignature;

    property ParentToken: TJclCLRTableRow read FParentToken;
  end;

  TJclCLRTableMethodDef = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableMethodDefRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableMethodDefRow read GetRow; default;
  end;

  TJclCLRTableMethodImplRow = class(TJclCLRTableRow)
  private
    FClassIdx: DWORD;
    FMethodBodyIdx: DWORD;
    FMethodDeclarationIdx: DWORD;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property ClassIdx: DWORD read FClassIdx;
    property MethodBodyIdx: DWORD read FMethodBodyIdx;
    property MethodDeclarationIdx: DWORD read FMethodDeclarationIdx;    
  end;

  TJclCLRTableMethodImpl = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableMethodImplRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableMethodImplRow read GetRow; default;
  end;

  TJclCLRTableMethodSemanticsRow = class(TJclCLRTableRow)
  private
    FSemantics: Word;
    FMethodIdx: DWORD;
    FAssociationIdx: DWORD;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property Semantics: Word read FSemantics;
    property MethodIdx: DWORD read FMethodIdx;
    property AssociationIdx: DWORD read FAssociationIdx;  
  end;

  TJclCLRTableMethodSemantics = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableMethodSemanticsRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableMethodSemanticsRow read GetRow; default;
  end;

  TJclCLRTableNestedClassRow = class(TJclCLRTableRow)
  private
    FEnclosingClassIdx: DWORD;
    FNestedClassIdx: DWORD;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property NestedClassIdx: DWORD read FNestedClassIdx;
    property EnclosingClassIdx: DWORD read FEnclosingClassIdx;
  end;

  TJclCLRTableNestedClass = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableNestedClassRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableNestedClassRow read GetRow; default;
  end;

  TJclCLRTableParamDefRow = class(TJclCLRTableRow)
  private
    FFlags: Word;
    FSequence: Word;
    FNameOffset: DWORD;
    function GetName: WideString;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property Flags: Word read FFlags;
    property Sequence: Word read FSequence;
    property NameOffset: DWORD read FNameOffset;

    property Name: WideString read GetName;
  end;

  TJclCLRTableParamDef = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableParamDefRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableParamDefRow read GetRow; default;
  end;

  TJclCLRTablePropertyRow = class(TJclCLRTableRow)
  private
    FKindIdx: DWORD;
    FNameOffset: DWORD;
    FFlags: Word;
    function GetName: WideString;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property Flags: Word read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property KindIdx: DWORD read FKindIdx;

    property Name: WideString read GetName;
  end;

  TJclCLRTableProperty = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTablePropertyRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTablePropertyRow read GetRow; default;
  end;

  TJclCLRTablePropertyMapRow = class(TJclCLRTableRow)
  private
    FParentIdx: DWORD;
    FPropertyListIdx: DWORD;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property ParentIdx: DWORD read FParentIdx;
    property PropertyListIdx: DWORD read FPropertyListIdx;
  end;

  TJclCLRTablePropertyMap = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTablePropertyMapRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTablePropertyMapRow read GetRow; default;
  end;

  TJclCLRTableStandAloneSigRow = class(TJclCLRTableRow)
  private
    FSignatureOffset: DWORD;
    function GetSignature: TJclCLRBlobRecord;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property SignatureOffset: DWORD read FSignatureOffset;

    property Signature: TJclCLRBlobRecord read GetSignature;
  end;

  TJclCLRTableStandAloneSig = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableStandAloneSigRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableStandAloneSigRow read GetRow; default;
  end;

  TJclCLRTableTypeDefRow = class(TJclCLRTableRow)
  private
    FNamespaceOffset: DWORD;
    FNameOffset: DWORD;
    FFlags: DWORD;
    FExtendsIdx: DWORD;
    FFieldListIdx: DWORD;
    FMethodListIdx: DWORD;
    FFields,
    FMethods: TList;
    function GetName: WideString;
    function GetNamespace: WideString;
    function GetField(const Idx: Integer): TJclCLRTableFieldRow;
    function GetFieldCount: Integer;
    function GetMethod(const Idx: Integer): TJclCLRTableMethodDefRow;
    function GetMethodCount: Integer;
    procedure UpdateFields;
    procedure UpdateMethods;
  protected
    constructor Create(const ATable: TJclCLRTable); override;

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

    property Fields[const Idx: Integer]: TJclCLRTableFieldRow read GetField;
    property FieldCount: Integer read GetFieldCount;
    property Methods[const Idx: Integer]: TJclCLRTableMethodDefRow read GetMethod;
    property MethodCount: Integer read GetMethodCount;
  end;

  TJclCLRTableTypeDef = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableTypeDefRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableTypeDefRow read GetRow; default;
  end;

  TJclCLRTableTypeRefRow = class(TJclCLRTableRow)
  private
    FResolutionScopeIdx,
    FNamespaceOffset,
    FNameOffset: DWORD;
    function GetName: WideString;
    function GetNamespace: WideString;
    function GetResolutionScopeIdx: DWORD;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property ResolutionScopeIdx: DWORD read GetResolutionScopeIdx;
    property NameOffset: DWORD read FNameOffset;
    property NamespaceOffset: DWORD read FNamespaceOffset;

    property Name: WideString read GetName;
    property Namespace: WideString read GetNamespace;
  end;

  TJclCLRTableTypeRef = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableTypeRefRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableTypeRefRow read GetRow; default;
  end;

  TJclCLRTableTypeSpecRow = class(TJclCLRTableRow)
  private
    FSignatureOffset: DWORD;
    function GetSignature: TJclCLRBlobRecord;
  protected
    constructor Create(const ATable: TJclCLRTable); override;
  public
    property SignatureOffset: DWORD read FSignatureOffset;

    property Signature: TJclCLRBlobRecord read GetSignature;
  end;

  TJclCLRTableTypeSpec = class(TJclCLRTable)
  private
    function GetRow(const Idx: Integer): TJclCLRTableTypeSpecRow;
  protected
    class function TableRowClass: TJclCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclCLRTableTypeSpecRow read GetRow; default;
  end;

  TJclCLRTableStream = class(TJclCLRStream)
  private
    FHeader: PCLRTableStreamHeader;
    FTables: array[TJclCLRTableKind] of TJclCLRTable;
    FTableCount: Integer;
    function GetVersionString: string;
    function GetTable(const AKind: TJclCLRTableKind): TJclCLRTable;
    function GetBigHeap(const AHeapKind: TJclCLRHeapKind): Boolean;
  protected
    constructor Create(const AMetadata: TJclPeMetadata;
                       const AHeader: PCLRStreamHeader); override;
  public
    destructor Destroy; override;

    procedure Update; virtual;

    function FindTable(const AKind: TJclCLRTableKind;
      var ATable: TJclCLRTable): Boolean;

    property Header: PCLRTableStreamHeader read FHeader;

    property VersionString: string read GetVersionString;
    property BigHeap[const AHeapKind: TJclCLRHeapKind]: Boolean read GetBigHeap;

    property Tables[const AKind: TJclCLRTableKind]: TJclCLRTable read GetTable;
    property TableCount: Integer read FTableCount;
  end;

  TJclPeMetadata = class
  private
    FImage: TJclPeImage;
    FHeader: PCLRMetadataHeader;
    FStreams: TObjectList;
    FStringStream: TJclCLRStringsStream;
    FGuidStream: TJclCLRGuidStream;
    FBlobStream: TJclCLRBlobStream;
    FTableStream: TJclCLRTableStream;
    function GetVersionString: WideString;
    function GetStream(const Idx: Integer): TJclCLRStream;
    function GetStreamCount: Integer;
    function GetString(const Idx: Integer): WideString;
    function GetStringCount: Integer;
    function GetGuid(const Idx: Integer): TGUID;
    function GetGuidCount: Integer;
    function GetBlob(const Idx: Integer): TJclCLRBlobRecord;
    function GetBlobCount: Integer;
    function GetTable(const AKind: TJclCLRTableKind): TJclCLRTable;
    function GetTableCount: Integer;
    function GetToken(const AToken: TJclCLRToken): TJclCLRTableRow;
  protected
    constructor Create(const AImage: TJclPeImage);
  public
    destructor Destroy; override;

    function FindStream(const AName: string; var Stream: TJclCLRStream): Boolean; overload;
    function FindStream(const AClass: TJclCLRStreamClass; var Stream: TJclCLRStream): Boolean; overload;

    function StringAt(const Offset: DWORD): WideString;
    function BlobAt(const Offset: DWORD): TJclCLRBlobRecord;

    function TokenExists(const Token: TJclCLRToken): Boolean;

    class function TokenTable(const Token: TJclCLRToken): TJclCLRTableKind;
    class function TokenIndex(const Token: TJclCLRToken): Integer;
    class function TokenCode(const Token: TJclCLRToken): Integer;
    class function MakeToken(const Table: TJclCLRTableKind; const Idx: Integer): TJclCLRToken;

    property Image: TJclPeImage read FImage;
    property Header: PCLRMetadataHeader read FHeader;

    property VersionString: WideString read GetVersionString;

    property Streams[const Idx: Integer]: TJclCLRStream read GetStream; default;
    property StreamCount: Integer read GetStreamCount;

    property Strings[const Idx: Integer]: WideString read GetString;
    property StringCount: Integer read GetStringCount;
    property Guids[const Idx: Integer]: TGUID read GetGuid;
    property GuidCount: Integer read GetGuidCount;
    property Blobs[const Idx: Integer]: TJclCLRBlobRecord read GetBlob;
    property BlobCount: Integer read GetBlobCount;
    property Tables[const AKind: TJclCLRTableKind]: TJclCLRTable read GetTable;
    property TableCount: Integer read GetTableCount;
    property Tokens[const AToken: TJclCLRToken]: TJclCLRTableRow read GetToken;
  end;

  TJclCLRResourceRecord = class(TJclReferenceMemoryStream)
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

  TJclCLRVTableFixupRecord = class
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

  TJclCLRHeaderEx = class(TJclPeCLRHeader)
  private
    FMetadata: TJclPeMetadata;
    FFlags: TJclClrImageFlags;
    FStrongNameSignature: TCustomMemoryStream;
    FResources,
    FVTableFixups: TObjectList;
    function GetMetadata: TJclPeMetadata;
    function GetStrongNameSignature: TCustomMemoryStream;
    function GetEntryPointToken: TJclCLRTableRow;
    function GetVTableFixup(const Idx: Integer): TJclCLRVTableFixupRecord;
    function GetVTableFixupCount: Integer;
    procedure UpdateResources;
    function GetResource(const Idx: Integer): TJclCLRResourceRecord;
    function GetResourceCount: Integer;
  public
    constructor Create(const AImage: TJclPeImage);
    destructor Destroy; override;

    function HasResources: Boolean;
    function HasStrongNameSignature: Boolean;
    function HasVTableFixup: Boolean;

    function ResourceAt(const Offset: DWORD): TJclCLRResourceRecord;

    class function ClrImageFlag(const Flags: DWORD): TJclClrImageFlags; overload;
    class function ClrImageFlag(const Flags: TJclClrImageFlags): DWORD; overload;

    property Metadata: TJclPeMetadata read GetMetadata;

    property Flags: TJclClrImageFlags read FFlags;
    property EntryPointToken: TJclCLRTableRow read GetEntryPointToken;
    property StrongNameSignature: TCustomMemoryStream read GetStrongNameSignature;

    property Resources[const Idx: Integer]: TJclCLRResourceRecord read GetResource;
    property ResourceCount: Integer read GetResourceCount;
    property VTableFixups[const Idx: Integer]: TJclCLRVTableFixupRecord read GetVTableFixup;
    property VTableFixupCount: Integer read GetVTableFixupCount;
  end;

implementation

uses
  Math, TypInfo, JclUnicode, JclResources;

const
  GUID_NULL : TGUID = '{00000000-0000-0000-0000-000000000000}';

  ValidTableMapping: array[TJclCLRTableKind] of TJclCLRTableClass = (
    TJclCLRTableModule,               //  $00
    TJclCLRTableTypeRef,              //  $01
    TJclCLRTableTypeDef,              //  $02
    TJclCLRTable,                     //  $03
    TJclCLRTableField,                //  $04
    TJclCLRTable,                     //  $05
    TJclCLRTableMethodDef,            //  $06
    TJclCLRTable,                     //  $07
    TJclCLRTableParamDef,             //  $08
    TJclCLRTableInterfaceImpl,        //  $09
    TJclCLRTableMemberRef,            //  $0a
    TJclCLRTableConstant,             //  $0b
    TJclCLRTableCustomAttribute,      //  $0c
    TJclCLRTableFieldMarshal,         //  $0d
    TJclCLRTableDeclSecurity,         //  $0e
    TJclCLRTableClassLayout,          //  $0f
    TJclCLRTableFieldLayout,          //  $10
    TJclCLRTableStandAloneSig,        //  $11
    TJclCLRTableEventMap,             //  $12
    TJclCLRTable,                     //  $13
    TJclCLRTableEvent,                //  $14
    TJclCLRTablePropertyMap,          //  $15
    TJclCLRTable,                     //  $16
    TJclCLRTableProperty,             //  $17
    TJclCLRTableMethodSemantics,      //  $18
    TJclCLRTableMethodImpl,           //  $19
    TJclCLRTableModuleRef,            //  $1a
    TJclCLRTableTypeSpec,             //  $1b
    TJclCLRTableImplMap,              //  $1c
    TJclCLRTableFieldRVA,             //  $1d
    TJclCLRTable,                     //  $1e
    TJclCLRTable,                     //  $1f
    TJclCLRTableAssembly,             //  $20
    TJclCLRTableAssemblyProcessor,    //  $21
    TJclCLRTableAssemblyOS,           //  $22
    TJclCLRTableAssemblyRef,          //  $23
    TJclCLRTableAssemblyRefProcessor, //  $24
    TJclCLRTableAssemblyRefOS,        //  $25
    TJclCLRTableFile,                 //  $26
    TJclCLRTableExportedType,         //  $27
    TJclCLRTableManifestResource,     //  $28
    TJclCLRTableNestedClass,          //  $29
    TJclCLRTable,                     //  $2A
    TJclCLRTable);                    //  $2B

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

  ClrAssemblyHashAlgorithmMapping: array[TJclClrAssemblyHashAlgorithm] of DWORD =
    (haNone, haMD5, haSHA1);

{ TJclCLRStream }

constructor TJclCLRStream.Create(const AMetadata: TJclPeMetadata;
  const AHeader: PCLRStreamHeader);
begin
  inherited Create;

  FMetadata := AMetadata;
  FHeader   := AHeader;
end;

function TJclCLRStream.GetName: string;
begin
  Result := FHeader.Name;
end;

function TJclCLRStream.GetOffset: DWORD;
begin
  Result := Data - Metadata.Image.LoadedImage.MappedAddress;
end;

function TJclCLRStream.GetSize: DWORD;
begin
  Result := FHeader.Size;
end;

function TJclCLRStream.GetData: Pointer;
begin
  Result := Pointer(DWORD(FMetadata.Header) + FHeader.Offset);
end;

{ TJclCLRStringsStream }

constructor TJclCLRStringsStream.Create(
  const AMetadata: TJclPeMetadata; const AHeader: PCLRStreamHeader);
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

destructor TJclCLRStringsStream.Destroy;
begin
  FreeAndNil(FStrings);

  inherited;
end;

function TJclCLRStringsStream.GetString(const Idx: Integer): WideString;
begin
  Result := UTF8ToWideString(FStrings.Strings[Idx]);
end;

function TJclCLRStringsStream.GetStringCount: Integer;
begin
  Result := FStrings.Count;
end;

function TJclCLRStringsStream.At(const Offset: DWORD): WideString;
var
  Idx: Integer;
begin
  Idx := FStrings.IndexOfObject(TObject(Offset));
  if Idx <> -1 then
    Result := GetString(Idx)
  else
    Result := '';
end;

{ TJclCLRGuidStream }

constructor TJclCLRGuidStream.Create(
  const AMetadata: TJclPeMetadata; const AHeader: PCLRStreamHeader);
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

function TJclCLRGuidStream.GetGuid(const Idx: Integer): TGUID;
begin
  Assert((0 <= Idx) and (Idx < GetGuidCount));
  Result := FGuids[Idx];
end;

function TJclCLRGuidStream.GetGuidCount: Integer;
begin
  Result := Length(FGuids);
end;

{ TJclCLRBlobRecord }

constructor TJclCLRBlobRecord.Create(const AStream: TJclCLRStream; const APtr: PByteArray);
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

{ TJclCLRBlobStream }

constructor TJclCLRBlobStream.Create(
  const AMetadata: TJclPeMetadata; const AHeader: PCLRStreamHeader);
var
  ABlob: TJclCLRBlobRecord;
begin
  inherited;

  FBlobs := TObjectList.Create;

  ABlob := TJclCLRBlobRecord.Create(Self, Data);
  while Assigned(ABlob) do
  begin
    if ABlob.Size > 0 then
      FBlobs.Add(ABlob);
    if (Integer(ABlob.Memory) + ABlob.Size) < (Integer(Self.Data) + Integer(Self.Size)) then
      ABlob := TJclCLRBlobRecord.Create(Self, Pointer(Integer(ABlob.Memory) + ABlob.Size))
    else
      ABlob := nil;
  end;
end;

destructor TJclCLRBlobStream.Destroy;
begin
  FreeAndNil(FBlobs);

  inherited;
end;

function TJclCLRBlobStream.At(const Offset: DWORD): TJclCLRBlobRecord;
var
  I: Integer;
begin
  for I:=0 to FBlobs.Count-1 do
  begin
    Result := TJclCLRBlobRecord(FBlobs.Items[I]);
    if Result.Offset = Offset then
      Exit;
  end;
  Result := nil;
end;

function TJclCLRBlobStream.GetBlob(const Idx: Integer): TJclCLRBlobRecord;
begin
  Result := TJclCLRBlobRecord(FBlobs.Items[Idx])
end;

function TJclCLRBlobStream.GetBlobCount: Integer;
begin
  Result := FBlobs.Count;
end;

{ TJclCLRUserStringStream }

function TJclCLRUserStringStream.GetString(const Idx: Integer): WideString;
var
  ABlob: TJclCLRBlobRecord;
begin
  ABlob := Blobs[Idx];
  SetLength(Result, ABlob.Size div 2 + 1);
  StrLCopyW(PWideChar(Result), PWideChar(ABlob.Memory), ABlob.Size div 2);
end;

function TJclCLRUserStringStream.GetStringCount: Integer;
begin
  Result := BlobCount;
end;

function TJclCLRUserStringStream.At(const Offset: DWORD): WideString;
var
  ABlob: TJclCLRBlobRecord;
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

{ TJclCLRTableRow }

constructor TJclCLRTableRow.Create(const ATable: TJclCLRTable);
begin
  inherited Create;

  FTable := ATable;
  FIndex := Table.RealRowCount;
end;

procedure TJclCLRTableRow.Update;
begin
  // do nothing, just for override
end;

{ TJclCLRTable }

constructor TJclCLRTable.Create(const AStream: TJclCLRTableStream;
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

destructor TJclCLRTable.Destroy;
begin
  FreeAndNil(FRows);

  inherited;
end;

procedure TJclCLRTable.Reset;
begin
  FPtr := FData;
end;

procedure TJclCLRTable.Load;
var
  I: Integer;
begin
  Assert(RowCount > 0);

  if TableRowClass <> TJclCLRTableRow then
    for I:=0 to RowCount-1 do
      AddRow(TableRowClass.Create(Self));
end;

procedure TJclCLRTable.SetSize(const Value: Integer);
begin
  FSize := Value;
  Assert(not IsBadReadPtr(FData, FSize));
end;

function TJclCLRTable.GetOffset: DWORD;
begin
  Result := DWORD(Data) - DWORD(Stream.Metadata.Image.LoadedImage.MappedAddress);
end;

function TJclCLRTable.GetRow(const Idx: Integer): TJclCLRTableRow;
begin
  Result := TJclCLRTableRow(FRows.Items[Idx]);
end;

function TJclCLRTable.GetRowCount: Integer;
begin
  Result := FRowCount;
end;

function TJclCLRTable.AddRow(const ARow: TJclCLRTableRow): Integer;
begin
  if not Assigned(FRows) then
    FRows := TObjectList.Create;

  Result := FRows.Add(ARow);
end;

function TJclCLRTable.RealRowCount: Integer;
begin
  if Assigned(FRows) then
    Result := FRows.Count
  else
    Result := 0;
end;

function TJclCLRTable.ReadIndex(const HeapKind: TJclCLRHeapKind): DWORD;
begin
  if Stream.BigHeap[HeapKind] then
    Result := ReadDWord
  else
    Result := ReadWord;
end;

function TJclCLRTable.ReadIndex(const TableKinds: array of TJclCLRTableKind): DWORD;
const
  TableIndexSize: array[Boolean] of Integer = (2, 4);
var
  BigHeap: Boolean;
  I: Integer;
  ATable: TJclCLRTable;
begin
  BigHeap := False;
  for I:=Low(TableKinds) to High(TableKinds) do
    if Stream.FindTable(TableKinds[I], ATable) then
      BigHeap := BigHeap or (ATable.RowCount > MAXWORD);

  if BigHeap then
    Result := ReadDWord
  else
    Result := ReadWord;
end;

function TJclCLRTable.ReadByte: Byte;
begin
  Result := PByte(FPtr)^;
  Inc(FPtr, SizeOf(Byte));
end;

function TJclCLRTable.ReadWord: Word;
begin
  Result := PWord(FPtr)^;
  Inc(FPtr, SizeOf(Word));
end;

function TJclCLRTable.ReadDWord: DWORD;
begin
  Result := PDword(FPtr)^;
  Inc(FPtr, SizeOf(DWORD));
end;

class function TJclCLRTable.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableRow;
end;

procedure TJclCLRTable.Update;
var
  I: Integer;
begin
  if Assigned(FRows) then
  for I:=0 to RowCount-1 do
    Rows[I].Update;
end;

{ TJclCLRTableModule }

procedure TJclCLRTableModule.Load;
begin
  Assert(RowCount = 1); // The Module table shall contain one and only one row

  inherited;

  FGeneration   := ReadWord;            // Generation (reserved, shall be zero)
  FNameOffset   := ReadIndex(hkString); // Name (index into String heap)
  FMvidIdx      := ReadIndex(hkGuid);   // Mvid (index into Guid heap)
  FEncIdIdx     := ReadIndex(hkGuid);   // Mvid (index into Guid heap)
  FEncBaseIdIdx := ReadIndex(hkGuid);   // Mvid (index into Guid heap)
end;

function TJclCLRTableModule.HasEncId: Boolean;
begin
  Result := FEncIdIdx > 0;
end;

function TJclCLRTableModule.HasEncBaseId: Boolean;
begin
  Result := FEncBaseIdIdx > 0;
end;

function TJclCLRTableModule.GetName: WideString;
begin
  Result := Stream.Metadata.StringAt(FNameOffset);
  Assert(Result <> ''); // Name shall index a non-null string.
  Assert(Length(Result) < MAX_PATH_NAME);
end;

function TJclCLRTableModule.GetMvid: TGUID;
begin
  // Mvid shall index a non-null GUID in the Guid heap
  Assert(FMvidIdx <= DWORD(Stream.Metadata.GuidCount));
  Result := Stream.Metadata.Guids[FMvidIdx-1];
end;

function TJclCLRTableModule.GetEncId: TGUID;
begin
  Result := Stream.Metadata.Guids[FEncIdIdx-1];
end;

function TJclCLRTableModule.GetEncBaseId: TGUID;
begin
  Result := Stream.Metadata.Guids[FEncBaseIdIdx-1];
end;

{ TJclCLRTableModuleRefRow }

constructor TJclCLRTableModuleRefRow.Create(const ATable: TJclCLRTable);
begin
  inherited;

  FNameOffset := Table.ReadIndex(hkString);
end;

function TJclCLRTableModuleRefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

{ TJclCLRTableModuleRef }

function TJclCLRTableModuleRef.GetRow(const Idx: Integer): TJclCLRTableModuleRefRow;
begin
  Result := TJclCLRTableModuleRefRow(inherited GetRow(Idx));
end;

class function TJclCLRTableModuleRef.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableModuleRefRow;
end;

{ TJclCLRTableAssemblyRow }

procedure TJclCLRTableAssembly.Load;
begin
  inherited;

  FHashAlgId       := ReadDWord;

  FMajorVersion    := ReadWord;
  FMinorVersion    := ReadWord;
  FBuildNumber     := ReadWord;
  FRevisionNumber  := ReadWord;

  FFlagMask        := ReadDWord;

  FPublicKeyOffset := ReadIndex(hkBlob);
  FNameOffset      := ReadIndex(hkString);
  FCultureOffset   := ReadIndex(hkString);
end;

function TJclCLRTableAssembly.GetCulture: WideString;
begin
  Result := Stream.Metadata.StringAt(FCultureOffset);
end;

function TJclCLRTableAssembly.GetName: WideString;
begin
  Result := Stream.Metadata.StringAt(FNameOffset);
end;

function TJclCLRTableAssembly.GetPublicKey: TJclCLRBlobRecord;
begin
  Result := Stream.Metadata.BlobAt(FPublicKeyOffset);
end;

function TJclCLRTableAssembly.GetVersion: string;
begin
  Result := FormatVersionString(FMajorVersion, FMinorVersion, FBuildNumber, FRevisionNumber);
end;

function TJclCLRTableAssembly.GetFlags: TJclClrAssemblyFlags;
begin
  Result := AssemblyFlags(FFlagMask);
end;

class function TJclCLRTableAssembly.AssemblyFlags(const Flags: DWORD): TJclClrAssemblyFlags;
var
  AFlag: TJclClrAssemblyFlag;
begin
  Result := [];
  for AFlag := Low(TJclClrAssemblyFlag) to High(TJclClrAssemblyFlag) do
    if (Flags and ClrAssemblyFlagMapping[AFlag]) = ClrAssemblyFlagMapping[AFlag] then
      Include(Result, AFlag);
end;

class function TJclCLRTableAssembly.AssemblyFlags(const Flags: TJclClrAssemblyFlags): DWORD;
var
  AFlag: TJclClrAssemblyFlag;
begin
  Result := 0;
  for AFlag := Low(TJclClrAssemblyFlag) to High(TJclClrAssemblyFlag) do
    if AFlag in Flags then
      Result := Result or ClrAssemblyFlagMapping[AFlag];
end;

function TJclCLRTableAssembly.GetHashAlgorithm: TJclClrAssemblyHashAlgorithm;
begin
  Result := AssemblyHashAlgorithm(FHashAlgId);
end;

class function TJclCLRTableAssembly.AssemblyHashAlgorithm(const HashAlg: DWORD): TJclClrAssemblyHashAlgorithm;
begin
  for Result := Low(TJclClrAssemblyHashAlgorithm) to High(TJclClrAssemblyHashAlgorithm) do
    if HashAlg = ClrAssemblyHashAlgorithmMapping[Result] then
      Exit;
  Result := chaNone;
end;

class function TJclCLRTableAssembly.AssemblyHashAlgorithm(const HashAlg: TJclClrAssemblyHashAlgorithm): DWORD;
begin
  Result := ClrAssemblyHashAlgorithmMapping[HashAlg];
end;

{ TJclCLRTableAssemblyOS }

procedure TJclCLRTableAssemblyOS.Load;
begin
  inherited;

  FPlatformID   := ReadDWord;
  FMajorVersion := ReadDWord;
  FMinorVersion := ReadDWord;
end;

function TJclCLRTableAssemblyOS.GetString: string;
begin
  Result := FormatVersionString(FMajorVersion, FMinorVersion);
end;

{ TJclCLRTableAssemblyProcessor }

procedure TJclCLRTableAssemblyProcessor.Load;
begin
  inherited;

  FProcessor := ReadDWord;
end;

{ TJclCLRTableAssemblyRefRow }

constructor TJclCLRTableAssemblyRefRow.Create(
  const ATable: TJclCLRTable);
begin
  inherited;

  FMajorVersion        := Table.ReadWord;
  FMinorVersion        := Table.ReadWord;
  FBuildNumber         := Table.ReadWord;
  FRevisionNumber      := Table.ReadWord;

  FFlags               := Table.ReadDWord;

  FPublicKeyOrTokenOffset := Table.ReadIndex(hkBlob);
  FNameOffset          := Table.ReadIndex(hkString);
  FCultureOffset       := Table.ReadIndex(hkString);
  FHashValueOffsetOffset        := Table.ReadIndex(hkBlob);
end;

function TJclCLRTableAssemblyRefRow.GetCulture: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FCultureOffset);
end;

function TJclCLRTableAssemblyRefRow.GetHashValue: TJclCLRBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FHashValueOffsetOffset);
end;

function TJclCLRTableAssemblyRefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclCLRTableAssemblyRefRow.GetPublicKeyOrToken: TJclCLRBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FPublicKeyOrTokenOffset);
end;

{ TJclCLRTableAssemblyRef }

function TJclCLRTableAssemblyRef.GetRow(const Idx: Integer): TJclCLRTableAssemblyRefRow;
begin
  Result := TJclCLRTableAssemblyRefRow(inherited GetRow(Idx));
end;

class function TJclCLRTableAssemblyRef.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableAssemblyRefRow;
end;

{ TJclCLRTableAssemblyRefOS }

procedure TJclCLRTableAssemblyRefOS.Load;
begin
  inherited;

  FAssemblyRefIdx := ReadIndex([ttAssemblyRef]);
end;

function TJclCLRTableAssemblyRefOS.GetAssemblyRef: TJclCLRTableAssemblyRef;
begin
  { TODO : Implement GetAssemblyRef }
  Result := nil;
end;

{ TJclCLRTableAssemblyRefProcessor }

procedure TJclCLRTableAssemblyRefProcessor.Load;
begin
  inherited;

  FAssemblyRefIdx := ReadIndex([ttAssemblyRef]);
end;

function TJclCLRTableAssemblyRefProcessor.GetAssemblyRef: TJclCLRTableAssemblyRef;
begin
  { TODO : Implement GetAssemblyRef }
  Result := nil;
end;

{ TJclCLRTableClassLayoutRow }

constructor TJclCLRTableClassLayoutRow.Create(const ATable: TJclCLRTable);
begin
  inherited;

  FPackingSize := Table.ReadWord;
  FClassSize   := Table.ReadDWord;
  FParentIdx   := Table.ReadIndex([ttTypeDef]);
end;

{ TJclCLRTableClassLayout }

function TJclCLRTableClassLayout.GetRow(const Idx: Integer): TJclCLRTableClassLayoutRow;
begin
  Result := TJclCLRTableClassLayoutRow(inherited GetRow(Idx));
end;

class function TJclCLRTableClassLayout.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableClassLayoutRow;
end;

{ TJclCLRTableConstantRow }

constructor TJclCLRTableConstantRow.Create(const ATable: TJclCLRTable);
begin
  inherited;

  FKind      := Table.ReadByte;
  Table.ReadByte; // padding zero
  FParentIdx := Table.ReadIndex([ttParamDef, ttFieldDef, ttProperty]);
  FValueOffset  := Table.ReadIndex(hkBlob);
end;

{ TJclCLRTableConstant }

function TJclCLRTableConstant.GetRow(const Idx: Integer): TJclCLRTableConstantRow;
begin
  Result := TJclCLRTableConstantRow(inherited GetRow(Idx));
end;

class function TJclCLRTableConstant.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableConstantRow;
end;

{ TJclCLRTableCustomAttributeRow }

constructor TJclCLRTableCustomAttributeRow.Create(
  const ATable: TJclCLRTable);
begin
  inherited;

  FParentIdx := Table.ReadIndex([ttModule, ttTypeRef, ttTypeDef, ttFieldDef,
    ttMethodDef, ttParamDef, ttInterfaceImpl, ttMemberRef, ttConstant,
    ttFieldMarshal, ttDeclSecurity, ttClassLayout, ttFieldLayout, ttSignature,
    ttEventMap, ttEvent, ttPropertyMap, ttProperty, ttMethodSemantics,
    ttMethodImpl, ttModuleRef, ttTypeSpec, ttImplMap, ttFieldRVA, ttAssembly,
    ttAssemblyProcessor, ttAssemblyOS, ttAssemblyRef, ttAssemblyRefProcessor,
    ttAssemblyRefOS, ttFile, ttExportedType, ttManifestResource, ttNestedClass]);
  FTypeIdx   := Table.ReadIndex([ttMethodDef, ttMemberRef]);
  FValueOffset  := Table.ReadIndex(hkBlob);
end;

function TJclCLRTableCustomAttributeRow.GetValue: TJclCLRBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FValueOffset);
end;

{ TJclCLRTableCustomAttribute }

function TJclCLRTableCustomAttribute.GetRow(const Idx: Integer): TJclCLRTableCustomAttributeRow;
begin
  Result := TJclCLRTableCustomAttributeRow(inherited GetRow(Idx));
end;

class function TJclCLRTableCustomAttribute.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableCustomAttributeRow;
end;

{ TJclCLRTableDeclSecurityRow }

constructor TJclCLRTableDeclSecurityRow.Create(const ATable: TJclCLRTable);
begin
  inherited;

  FAction              := Table.ReadWord;
  FParentIdx           := Table.ReadIndex([ttTypeDef, ttMethodDef, ttAssembly]);
  FPermissionSetOffset := Table.ReadIndex(hkBlob);
end;

{ TJclCLRTableDeclSecurity }

function TJclCLRTableDeclSecurity.GetRow(const Idx: Integer): TJclCLRTableDeclSecurityRow;
begin
  Result := TJclCLRTableDeclSecurityRow(inherited GetRow(Idx));
end;

class function TJclCLRTableDeclSecurity.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableDeclSecurityRow;
end;

{ TJclCLRTableEventMapRow }

constructor TJclCLRTableEventMapRow.Create(const ATable: TJclCLRTable);
begin
  inherited;

  FParentIdx := Table.ReadIndex([ttTypeDef]);
  FEventListIdx := Table.ReadIndex([ttEvent]);
end;

{ TJclCLRTableEventMap }

function TJclCLRTableEventMap.GetRow(const Idx: Integer): TJclCLRTableEventMapRow;
begin
  Result := TJclCLRTableEventMapRow(inherited GetRow(Idx));
end;

class function TJclCLRTableEventMap.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableEventMapRow;
end;

{ TJclCLRTableEventRow }

constructor TJclCLRTableEventRow.Create(const ATable: TJclCLRTable);
begin
  inherited;

  FEventFlags   := Table.ReadWord;
  FNameOffset   := Table.ReadIndex(hkString);
  FEventTypeIdx := Table.ReadIndex([ttTypeDef, ttTypeRef, ttTypeSpec]);
end;

function TJclCLRTableEventRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

{ TJclCLRTableEvent }

function TJclCLRTableEvent.GetRow(const Idx: Integer): TJclCLRTableEventRow;
begin
  Result := TJclCLRTableEventRow(inherited GetRow(Idx));
end;

class function TJclCLRTableEvent.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableEventRow;
end;

{ TJclCLRTableExportedTypeRow }

constructor TJclCLRTableExportedTypeRow.Create(const ATable: TJclCLRTable);
begin
  inherited;

  FFlags               := Table.ReadDWord;
  FTypeDefIdx          := Table.ReadDWord;
  FTypeNameOffset      := Table.ReadIndex(hkString);
  FTypeNamespaceOffset := Table.ReadIndex(hkString);
  FImplementationIdx   := Table.ReadIndex([ttFile, ttExportedType]);
end;

{ TJclCLRTableExportedType }

function TJclCLRTableExportedType.GetRow(const Idx: Integer): TJclCLRTableExportedTypeRow;
begin
  Result := TJclCLRTableExportedTypeRow(inherited GetRow(Idx));
end;

class function TJclCLRTableExportedType.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableExportedTypeRow;
end;

{ TJclCLRTableFieldRow }

constructor TJclCLRTableFieldRow.Create(const ATable: TJclCLRTable);
begin
  inherited;

  FFlags        := Table.ReadWord;
  FNameOffset   := Table.ReadIndex(hkString);
  FSignatureOffset := Table.ReadIndex(hkBlob);
  FParentToken  := nil;
end;

function TJclCLRTableFieldRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclCLRTableFieldRow.GetSignature: TJclCLRBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FSignatureOffset);
end;

procedure TJclCLRTableFieldRow.SetParentToken(const ARow: TJclCLRTableRow);
begin
  FParentToken := ARow;
end;

{ TJclCLRTableField }

function TJclCLRTableField.GetRow(const Idx: Integer): TJclCLRTableFieldRow;
begin
  Result := TJclCLRTableFieldRow(inherited GetRow(Idx));
end;

class function TJclCLRTableField.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableFieldRow;
end;

{ TJclCLRTableFieldLayoutRow }

constructor TJclCLRTableFieldLayoutRow.Create(const ATable: TJclCLRTable);
begin
  inherited;

  FOffset   := Table.ReadDWord;
  FFieldIdx := Table.ReadIndex([ttFieldDef]);
end;

{ TJclCLRTableFieldLayout }

function TJclCLRTableFieldLayout.GetRow(
  const Idx: Integer): TJclCLRTableFieldLayoutRow;
begin
  Result := TJclCLRTableFieldLayoutRow(inherited GetRow(Idx));
end;

class function TJclCLRTableFieldLayout.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableFieldLayoutRow;
end;

{ TJclCLRTableFieldMarshalRow }

constructor TJclCLRTableFieldMarshalRow.Create(
  const ATable: TJclCLRTable);
begin
  inherited;

  FParentIdx        := Table.ReadIndex([ttFieldDef, ttParamDef]);
  FNativeTypeOffset := Table.ReadIndex(hkBlob);
end;

{ TJclCLRTableFieldMarshal }

function TJclCLRTableFieldMarshal.GetRow(
  const Idx: Integer): TJclCLRTableFieldMarshalRow;
begin
  Result := TJclCLRTableFieldMarshalRow(inherited GetRow(Idx));
end;

class function TJclCLRTableFieldMarshal.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableFieldMarshalRow;
end;

{ TJclCLRTableFieldRVARow }

constructor TJclCLRTableFieldRVARow.Create(const ATable: TJclCLRTable);
begin
  inherited;

  FRVA      := Table.ReadDWord;
  FFieldIdx := Table.ReadIndex([ttFieldDef]);
end;

{ TJclCLRTableFieldRVA }

function TJclCLRTableFieldRVA.GetRow(const Idx: Integer): TJclCLRTableFieldRVARow;
begin
  Result := TJclCLRTableFieldRVARow(inherited GetRow(Idx));
end;

class function TJclCLRTableFieldRVA.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableFieldRVARow;
end;

{ TJclCLRTableFileRow }

constructor TJclCLRTableFileRow.Create(const ATable: TJclCLRTable);
begin
  inherited;

  FFlags           := Table.ReadDWord;
  FNameOffset      := Table.ReadIndex(hkString);
  FHashValueOffset := Table.ReadIndex(hkBlob);
end;

function TJclCLRTableFileRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclCLRTableFileRow.GetHashValue: TJclCLRBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FHashValueOffset);
end;

{ TJclCLRTableFile }

function TJclCLRTableFile.GetRow(const Idx: Integer): TJclCLRTableFileRow;
begin
  Result := TJclCLRTableFileRow(inherited GetRow(Idx));
end;

class function TJclCLRTableFile.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableFileRow;
end;

{ TJclCLRTableImplMapRow }

constructor TJclCLRTableImplMapRow.Create(const ATable: TJclCLRTable);
begin
  inherited;

  FMappingFlags       := Table.ReadWord;
  FMemberForwardedIdx := Table.ReadIndex([ttFieldDef, ttMethodDef]);
  FImportNameOffset   := Table.ReadIndex(hkString);
  FImportScopeIdx     := Table.ReadIndex([ttModuleRef]);
end;

function TJclCLRTableImplMapRow.GetImportName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FImportNameOffset);
end;

{ TJclCLRTableImplMap }

function TJclCLRTableImplMap.GetRow(const Idx: Integer): TJclCLRTableImplMapRow;
begin
  Result := TJclCLRTableImplMapRow(inherited GetRow(Idx));
end;

class function TJclCLRTableImplMap.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableImplMapRow;
end;

{ TJclCLRTableInterfaceImplRow }

constructor TJclCLRTableInterfaceImplRow.Create(const ATable: TJclCLRTable);
begin
  inherited;

  FClassIdx     := Table.ReadIndex([ttTypeDef]);
  FInterfaceIdx := Table.ReadIndex([ttTypeDef, ttTypeRef, ttTypeSpec]);
end;

{ TJclCLRTableInterfaceImpl }

function TJclCLRTableInterfaceImpl.GetRow(
  const Idx: Integer): TJclCLRTableInterfaceImplRow;
begin
  Result := TJclCLRTableInterfaceImplRow(inherited GetRow(Idx));
end;

class function TJclCLRTableInterfaceImpl.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableInterfaceImplRow;
end;

{ TJclCLRTableManifestResourceRow }

constructor TJclCLRTableManifestResourceRow.Create(
  const ATable: TJclCLRTable);
begin
  inherited;

  FOffset            := Table.ReadDWord;
  FFlags             := Table.ReadDWord;
  FImplementationIdx := Table.ReadIndex(hkString);
  FNameOffset        := Table.ReadIndex([ttFile, ttAssemblyRef]);
end;

function TJclCLRTableManifestResourceRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

{ TJclCLRTableManifestResource }

function TJclCLRTableManifestResource.GetRow(
  const Idx: Integer): TJclCLRTableManifestResourceRow;
begin
  Result := TJclCLRTableManifestResourceRow(inherited GetRow(Idx));
end;

class function TJclCLRTableManifestResource.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableManifestResourceRow;
end;

{ TJclCLRTableMemberRefRow }

constructor TJclCLRTableMemberRefRow.Create(
  const ATable: TJclCLRTable);
begin
  inherited;

  FClassIdx     := Table.ReadIndex([ttTypeRef, ttModuleRef, ttMethodDef, ttTypeSpec, ttTypeDef]); 
  FNameOffset   := Table.ReadIndex(hkString);
  FSignatureOffset := Table.ReadIndex(hkBlob);
end;

function TJclCLRTableMemberRefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclCLRTableMemberRefRow.GetSignature: TJclCLRBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FSignatureOffset);
end;

{ TJclCLRTableMemberRef }

function TJclCLRTableMemberRef.GetRow(const Idx: Integer): TJclCLRTableMemberRefRow;
begin
  Result := TJclCLRTableMemberRefRow(inherited GetRow(Idx));
end;

class function TJclCLRTableMemberRef.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableMemberRefRow;
end;

{ TJclCLRTableMethodDefRow }

constructor TJclCLRTableMethodDefRow.Create(const ATable: TJclCLRTable);
begin
  inherited;

  FRVA          := Table.ReadDWord;
  FImplFlags    := Table.ReadWord;
  FFlags        := Table.ReadWord;
  FNameOffset   := Table.ReadIndex(hkString);
  FSignatureOffset := Table.ReadIndex(hkBlob);
  FParamListIdx := Table.ReadIndex([ttParamDef]);
  FParentToken  := nil;
end;

function TJclCLRTableMethodDefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclCLRTableMethodDefRow.GetSignature: TJclCLRBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FSignatureOffset);
end;

procedure TJclCLRTableMethodDefRow.SetParentToken(const ARow: TJclCLRTableRow);
begin
  FParentToken := ARow;
end;

{ TJclCLRTableMethodDef }

function TJclCLRTableMethodDef.GetRow(const Idx: Integer): TJclCLRTableMethodDefRow;
begin
  Result := TJclCLRTableMethodDefRow(inherited GetRow(Idx));
end;

class function TJclCLRTableMethodDef.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableMethodDefRow;
end;

{ TJclCLRTableMethodImplRow }

constructor TJclCLRTableMethodImplRow.Create(
  const ATable: TJclCLRTable);
begin
  inherited;

  FClassIdx             := Table.ReadIndex([ttTypeDef]);
  FMethodBodyIdx        := Table.ReadIndex([ttMethodDef, ttMemberRef]);
  FMethodDeclarationIdx := Table.ReadIndex([ttMethodDef, ttMemberRef]);
end;

{ TJclCLRTableMethodImpl }

function TJclCLRTableMethodImpl.GetRow(
  const Idx: Integer): TJclCLRTableMethodImplRow;
begin
  Result := TJclCLRTableMethodImplRow(inherited GetRow(Idx));
end;

class function TJclCLRTableMethodImpl.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableMethodImplRow;
end;

{ TJclCLRTableMethodSemanticsRow }

constructor TJclCLRTableMethodSemanticsRow.Create(
  const ATable: TJclCLRTable);
begin
  inherited;

  FSemantics      := Table.ReadWord;
  FMethodIdx      := Table.ReadIndex([ttMethodDef]);
  FAssociationIdx := Table.ReadIndex([ttEvent, ttProperty]);
end;

{ TJclCLRTableMethodSemantics }

function TJclCLRTableMethodSemantics.GetRow(
  const Idx: Integer): TJclCLRTableMethodSemanticsRow;
begin
  Result := TJclCLRTableMethodSemanticsRow(inherited GetRow(Idx));
end;

class function TJclCLRTableMethodSemantics.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableMethodSemanticsRow;
end;

{ TJclCLRTableNestedClassRow }

constructor TJclCLRTableNestedClassRow.Create(const ATable: TJclCLRTable);
begin
  inherited;

  FNestedClassIdx    := Table.ReadIndex([ttTypeDef]);
  FEnclosingClassIdx := Table.ReadIndex([ttTypeDef]);
end;

{ TJclCLRTableNestedClass }

function TJclCLRTableNestedClass.GetRow(const Idx: Integer): TJclCLRTableNestedClassRow;
begin
  Result := TJclCLRTableNestedClassRow(inherited GetRow(Idx));
end;

class function TJclCLRTableNestedClass.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableNestedClassRow;
end;

{ TJclCLRTableParamDefRow }

constructor TJclCLRTableParamDefRow.Create(const ATable: TJclCLRTable);
begin
  inherited;

  FFlags      := Table.ReadWord;
  FSequence   := Table.ReadWord;
  FNameOffset := Table.ReadIndex(hkString);
end;

function TJclCLRTableParamDefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

{ TJclCLRTableParamDef }

function TJclCLRTableParamDef.GetRow(const Idx: Integer): TJclCLRTableParamDefRow;
begin
  Result := TJclCLRTableParamDefRow(inherited GetRow(Idx));
end;

class function TJclCLRTableParamDef.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableParamDefRow;
end;


{ TJclCLRTablePropertyRow }

constructor TJclCLRTablePropertyRow.Create(const ATable: TJclCLRTable);
begin
  inherited;

  FFlags      := Table.ReadWord;
  FNameOffset := Table.ReadIndex(hkString);
  FKindIdx    := Table.ReadIndex(hkBlob);
end;

function TJclCLRTablePropertyRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

{ TJclCLRTableProperty }

function TJclCLRTableProperty.GetRow(const Idx: Integer): TJclCLRTablePropertyRow;
begin
  Result := TJclCLRTablePropertyRow(inherited GetRow(Idx));
end;

class function TJclCLRTableProperty.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTablePropertyRow;
end;

{ TJclCLRTablePropertyMapRow }

constructor TJclCLRTablePropertyMapRow.Create(
  const ATable: TJclCLRTable);
begin
  inherited;

  FParentIdx       := Table.ReadIndex([ttTypeDef]);
  FPropertyListIdx := Table.ReadIndex([ttProperty]);
end;

{ TJclCLRTablePropertyMap }

function TJclCLRTablePropertyMap.GetRow(
  const Idx: Integer): TJclCLRTablePropertyMapRow;
begin
  Result := TJclCLRTablePropertyMapRow(inherited GetRow(Idx));
end;

class function TJclCLRTablePropertyMap.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTablePropertyMapRow;
end;

{ TJclCLRTableStandAloneSigRow }

constructor TJclCLRTableStandAloneSigRow.Create(
  const ATable: TJclCLRTable);
begin
  inherited;

  FSignatureOffset := Table.ReadIndex(hkBlob);
end;

function TJclCLRTableStandAloneSigRow.GetSignature: TJclCLRBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FSignatureOffset);
end;

{ TJclCLRTableStandAloneSig }

function TJclCLRTableStandAloneSig.GetRow(
  const Idx: Integer): TJclCLRTableStandAloneSigRow;
begin
  Result := TJclCLRTableStandAloneSigRow(inherited GetRow(Idx));
end;

class function TJclCLRTableStandAloneSig.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableStandAloneSigRow;
end;

{ TJclCLRTableTypeDefRow }

constructor TJclCLRTableTypeDefRow.Create(const ATable: TJclCLRTable);
begin
  inherited;

  FFlags           := Table.ReadDWord;
  FNameOffset      := Table.ReadIndex(hkString);
  FNamespaceOffset := Table.ReadIndex(hkString);
  FExtendsIdx      := Table.ReadIndex([ttTypeDef, ttTypeRef, ttTypeSpec]);
  FFieldListIdx    := Table.ReadIndex([ttFieldDef]);
  FMethodListIdx   := Table.ReadIndex([ttMethodDef]);

  FFields          := nil;
  FMethods         := nil;
end;

destructor TJclCLRTableTypeDefRow.Destroy;
begin
  FreeAndNil(FFields);
  FreeAndNil(FMethods);

  inherited;
end;

function TJclCLRTableTypeDefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclCLRTableTypeDefRow.GetNamespace: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNamespaceOffset);
end;

function TJclCLRTableTypeDefRow.GetField(const Idx: Integer): TJclCLRTableFieldRow;
begin
  Result := TJclCLRTableFieldRow(FFields.Items[Idx])
end;

function TJclCLRTableTypeDefRow.GetFieldCount: Integer;
begin
  Result := FFields.Count
end;

function TJclCLRTableTypeDefRow.HasField: Boolean;
begin
  Result := Assigned(FFields);
end;

function TJclCLRTableTypeDefRow.GetMethod(const Idx: Integer): TJclCLRTableMethodDefRow;
begin
  Result := TJclCLRTableMethodDefRow(FMethods.Items[Idx])
end;

function TJclCLRTableTypeDefRow.GetMethodCount: Integer;
begin
  Result := FMethods.Count
end;

function TJclCLRTableTypeDefRow.HasMethod: Boolean;
begin
  Result := Assigned(FMethods);
end;

procedure TJclCLRTableTypeDefRow.UpdateFields;
var
  FieldTable: TJclCLRTableField;
  Idx, MaxFieldListIdx: DWORD;
begin
  with Table as TJclCLRTableTypeDef do
  if not Assigned(FFields) and (FieldListIdx <> 0) and
     Stream.FindTable(ttFieldDef, TJclCLRTable(FieldTable)) then
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

procedure TJclCLRTableTypeDefRow.UpdateMethods;
var
  MethodTable: TJclCLRTableMethodDef;
  Idx, MaxMethodListIdx: DWORD;
begin
  with Table as TJclCLRTableTypeDef do
  if not Assigned(FMethods) and (MethodListIdx <> 0) and
     Stream.FindTable(ttMethodDef, TJclCLRTable(MethodTable)) then
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

procedure TJclCLRTableTypeDefRow.Update;
begin
  inherited;

  UpdateFields;
  UpdateMethods;
end;

{ TJclCLRTableTypeDef }

class function TJclCLRTableTypeDef.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableTypeDefRow;
end;

function TJclCLRTableTypeDef.GetRow(const Idx: Integer): TJclCLRTableTypeDefRow;
begin
  Result := TJclCLRTableTypeDefRow(inherited GetRow(Idx));
end;

{ TJclCLRTableTypeRefRow }

constructor TJclCLRTableTypeRefRow.Create(const ATable: TJclCLRTable);
begin
  inherited;

  FResolutionScopeIdx := Table.ReadIndex([ttModule, ttModuleRef, ttAssemblyRef, ttTypeRef]);
  FNameOffset         := Table.ReadIndex(hkString);
  FNamespaceOffset    := Table.ReadIndex(hkString);
end;

function TJclCLRTableTypeRefRow.GetResolutionScopeIdx: DWORD;
begin
  { TODO : Implement GetResolutionScopeIdx }
  Result := 0;
end;

function TJclCLRTableTypeRefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclCLRTableTypeRefRow.GetNamespace: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNamespaceOffset);
end;

{ TJclCLRTableTypeRef }

class function TJclCLRTableTypeRef.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableTypeRefRow;
end;

function TJclCLRTableTypeRef.GetRow(const Idx: Integer): TJclCLRTableTypeRefRow;
begin
  Result := TJclCLRTableTypeRefRow(inherited GetRow(Idx));
end;

{ TJclCLRTableTypeSpecRow }

constructor TJclCLRTableTypeSpecRow.Create(const ATable: TJclCLRTable);
begin
  inherited;

  FSignatureOffset := Table.ReadIndex(hkBlob);
end;

function TJclCLRTableTypeSpecRow.GetSignature: TJclCLRBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FSignatureOffset);
end;

{ TJclCLRTableTypeSpec }

function TJclCLRTableTypeSpec.GetRow(const Idx: Integer): TJclCLRTableTypeSpecRow;
begin
  Result := TJclCLRTableTypeSpecRow(inherited GetRow(Idx));
end;

class function TJclCLRTableTypeSpec.TableRowClass: TJclCLRTableRowClass;
begin
  Result := TJclCLRTableTypeSpecRow;
end;

{ TJclCLRTableStream }

constructor TJclCLRTableStream.Create(const AMetadata: TJclPeMetadata;
  const AHeader: PCLRStreamHeader);

  function BitCount(const Value: Int64): Integer;
  var
    AKind: TJclCLRTableKind;
  begin
    Result := 0;
    for AKind:=Low(TJclCLRTableKind) to High(TJclCLRTableKind) do
      if (Value and (Int64(1) shl Integer(AKind))) <> 0 then
        Inc(Result);
  end;

  procedure EnumTables;
  var
    AKind: TJclCLRTableKind;
    pTable: Pointer;
  begin
    pTable      := @Header.Rows[BitCount(Header.Valid)];
    FTableCount := 0;
    for AKind:=Low(TJclCLRTableKind) to High(TJclCLRTableKind) do
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

destructor TJclCLRTableStream.Destroy;
begin
  FreeAndNil(FTables);

  inherited;
end;

function TJclCLRTableStream.GetVersionString: string;
begin
  Result := FormatVersionString(Header.MajorVersion, Header.MinorVersion);
end;

function TJclCLRTableStream.GetTable(const AKind: TJclCLRTableKind): TJclCLRTable;
begin
  Result := TJclCLRTable(FTables[AKind]);
end;

function TJclCLRTableStream.GetBigHeap(const AHeapKind: TJclCLRHeapKind): Boolean;
const
  HeapSizesMapping: array[TJclCLRHeapKind] of DWORD = (1, 2, 4);
begin
  Result := (Header.HeapSizes and HeapSizesMapping[AHeapKind]) <> 0;
end;

function TJclCLRTableStream.FindTable(const AKind: TJclCLRTableKind;
  var ATable: TJclCLRTable): Boolean;
begin
  ATable := FTables[AKind];
  Result := Assigned(ATable);
end;

procedure TJclCLRTableStream.Update;
var
  AKind: TJclCLRTableKind;
begin
  for AKind:=Low(TJclCLRTableKind) to High(TJclCLRTableKind) do
    if Assigned(FTables[AKind]) then
      FTables[AKind].Update;
end;

{ TJclPeMetadata }

constructor TJclPeMetadata.Create(const AImage: TJclPeImage);

  function GetStreamClass(const Name: string): TJclCLRStreamClass;
  begin
    if CompareText(Name, '#Strings') = 0 then
      Result := TJclCLRStringsStream
    else if CompareText(Name, '#GUID') = 0 then
      Result := TJclCLRGuidStream
    else if CompareText(Name, '#Blob') = 0 then
      Result := TJclCLRBlobStream
    else if CompareText(Name, '#US') = 0 then
      Result := TJclCLRUserStringStream
    else if CompareText(Name, '#~') = 0 then
      Result := TJclCLRTableStream
    else
      Result := TJclCLRStream;
  end;

  procedure UpdateStreams;
  type
    PStreamPartitionHeader = ^TStreamPartitionHeader;
    TStreamPartitionHeader = packed record
      Flags,
      StreamCount: Word;
      StreamHeaders: array[0..0] of TCLRStreamHeader;
    end;
  var
    pStreamPart: PStreamPartitionHeader;
    pStream: PCLRStreamHeader;
    I: Integer;
    TableStream: TJclCLRTableStream;
  begin
    pStreamPart := PStreamPartitionHeader(DWORD(@Header.Version[0]) + Header.Length);
    pStream     := @pStreamPart.StreamHeaders[0];
    for I:=0 to pStreamPart.StreamCount-1 do
    begin
      FStreams.Add(GetStreamClass(pStream.Name).Create(Self, pStream));

      pStream := PCLRStreamHeader(DWORD(@pStream.Name[0]) +
                 (((StrLen(@pStream.Name[0])+1)+3) and (not $3)));
    end;
    if FindStream(TJclCLRTableStream, TJclCLRStream(TableStream)) then
      TableStream.Update;
  end;
begin
  Assert(AImage.IsCLR and AImage.CLRHeader.HasMetadata);

  inherited Create;

  FImage := AImage;

  with Image.CLRHeader.Header.MetaData do
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

function TJclPeMetadata.GetStream(const Idx: Integer): TJclCLRStream;
begin
  Result := TJclCLRStream(FStreams.Items[Idx]);
end;

function TJclPeMetadata.GetStreamCount: Integer;
begin
  Result := FStreams.Count;
end;

function TJclPeMetadata.FindStream(const AName: string;
  var Stream: TJclCLRStream): Boolean;
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

function TJclPeMetadata.FindStream(const AClass: TJclCLRStreamClass;
  var Stream: TJclCLRStream): Boolean;
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


function TJclPeMetadata.GetToken(const AToken: TJclCLRToken): TJclCLRTableRow;
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
     FindStream(TJclCLRStringsStream, TJclCLRStream(FStringStream)) then
    Result := FStringStream.Strings[Idx];
end;

function TJclPeMetadata.GetStringCount: Integer;
begin
  if Assigned(FStringStream) or
     FindStream(TJclCLRStringsStream, TJclCLRStream(FStringStream)) then
    Result := FStringStream.StringCount
  else
    Result := 0;
end;

function TJclPeMetadata.StringAt(const Offset: DWORD): WideString;
begin
  if Assigned(FStringStream) or
     FindStream(TJclCLRStringsStream, TJclCLRStream(FStringStream)) then
    Result := TJclCLRStringsStream(FStringStream).At(Offset)
  else
    Result := '';
end;

function TJclPeMetadata.BlobAt(const Offset: DWORD): TJclCLRBlobRecord;
begin
  if Assigned(FBlobStream) or
     FindStream(TJclCLRBlobStream, TJclCLRStream(FBlobStream)) then
    Result := TJclCLRBlobStream(FBlobStream).At(Offset)
  else
    Result := nil;
end;

function TJclPeMetadata.GetGuid(const Idx: Integer): TGUID;
begin
  if Assigned(FGuidStream) or
     FindStream(TJclCLRGuidStream, TJclCLRStream(FGuidStream)) then
    Result := FGuidStream.Guids[Idx]
  else
    Result := GUID_NULL;
end;

function TJclPeMetadata.GetGuidCount: Integer;
begin
  if Assigned(FGuidStream) or
     FindStream(TJclCLRGuidStream, TJclCLRStream(FGuidStream)) then
    Result := FGuidStream.GuidCount
  else
    Result := 0;
end;

function TJclPeMetadata.GetBlob(const Idx: Integer): TJclCLRBlobRecord;
begin
  if Assigned(FBlobStream) or
     FindStream(TJclCLRBlobStream, TJclCLRStream(FBlobStream)) then
    Result := FBlobStream.Blobs[Idx]
  else
    Result := nil;
end;

function TJclPeMetadata.GetBlobCount: Integer;
begin
  if Assigned(FBlobStream) or
     FindStream(TJclCLRBlobStream, TJclCLRStream(FBlobStream)) then
    Result := FBlobStream.BlobCount
  else
    Result := 0;
end;

function TJclPeMetadata.GetTable(const AKind: TJclCLRTableKind): TJclCLRTable;
begin
  if Assigned(FTableStream) or
     FindStream(TJclCLRTableStream, TJclCLRStream(FTableStream)) then
    Result := FTableStream.Tables[AKind]
  else
    Result := nil;
end;

function TJclPeMetadata.GetTableCount: Integer;
begin
  if Assigned(FTableStream) or
     FindStream(TJclCLRTableStream, TJclCLRStream(FTableStream)) then
    Result := FTableStream.TableCount
  else
    Result := 0;
end;

function TJclPeMetadata.TokenExists(const Token: TJclCLRToken): Boolean;
begin
  Result := TokenIndex(Token) in [1..Tables[TokenTable(Token)].RowCount];
end;

class function TJclPeMetadata.TokenTable(const Token: TJclCLRToken): TJclCLRTableKind;
begin
  Result := TJclCLRTableKind(Token shr 24);
end;

class function TJclPeMetadata.TokenIndex(const Token: TJclCLRToken): Integer;
begin
  Result := Token and DWORD($FFFFFF);
end;

class function TJclPeMetadata.TokenCode(const Token: TJclCLRToken): Integer;
begin
  Result := Token and $FF000000;
end;

class function TJclPeMetadata.MakeToken(
  const Table: TJclCLRTableKind; const Idx: Integer): TJclCLRToken;
begin
  Result := (DWORD(Table) shl 24) and TokenIndex(Idx);
end;

{ TJclCLRResourceRecord }

constructor TJclCLRResourceRecord.Create(const AData: PChar;
  const AOffset: DWORD; const ARVA: DWORD);
begin
  FData   := AData;
  FOffset := AOffset;
  FRVA    := ARVA;

  inherited Create(Pointer(DWORD(Data)+SizeOf(DWORD)), PDWORD(Data)^);
end;

{ TJclCLRVTableFixupRecord }

constructor TJclCLRVTableFixupRecord.Create(
  const AData: PImageCorVTableFixup);
begin
  inherited Create;

  FData := AData;
end;

function TJclCLRVTableFixupRecord.GetCount: DWORD;
begin
  Result := Data.Count;
end;

function TJclCLRVTableFixupRecord.GetKinds: TJclClrVTableKinds;
begin
  Result := VTableKinds(Data.Kind);
end;

function TJclCLRVTableFixupRecord.GetRVA: DWORD;
begin
  Result := Data.RVA;
end;

class function TJclCLRVTableFixupRecord.VTableKinds(
  const Kinds: TJclClrVTableKinds): DWORD;
var
  AKind: TJclClrVTableKind;
begin
  Result := 0;
  for AKind:=Low(TJclClrVTableKind) to High(TJclClrVTableKind) do
    if AKind in Kinds then
      Result := Result or ClrVTableKindMapping[AKind];
end;

class function TJclCLRVTableFixupRecord.VTableKinds(
  const Kinds: DWORD): TJclClrVTableKinds;
var
  AKind: TJclClrVTableKind;
begin
  Result := [];
  for AKind:=Low(TJclClrVTableKind) to High(TJclClrVTableKind) do
    if (ClrVTableKindMapping[AKind] and Kinds) = ClrVTableKindMapping[AKind] then
      Include(Result, AKind);
end;

{ TJclCLRInformation }

constructor TJclCLRHeaderEx.Create(const AImage: TJclPeImage);
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

destructor TJclCLRHeaderEx.Destroy;
begin
  FreeAndNil(FVTableFixups);
  FreeAndNil(FStrongNameSignature);
  FreeAndNil(FResources);
  FreeAndNil(FMetadata);

  inherited;
end;

class function TJclCLRHeaderEx.ClrImageFlag(const Flags: DWORD): TJclClrImageFlags;
var
  AFlag: TJclClrImageFlag;
begin
  Result := [];
  for AFlag:=Low(TJclClrImageFlag) to High(TJclClrImageFlag) do
    if (ClrImageFlagMapping[AFlag] and Flags) = ClrImageFlagMapping[AFlag] then
      Include(Result, AFlag);
end;

class function TJclCLRHeaderEx.ClrImageFlag(const Flags: TJclClrImageFlags): DWORD;
var
  AFlag: TJclClrImageFlag;
begin
  Result := 0;
  for AFlag:=Low(TJclClrImageFlag) to High(TJclClrImageFlag) do
    if AFlag in Flags then
      Result := Result or ClrImageFlagMapping[AFlag];
end;

function TJclCLRHeaderEx.GetMetadata: TJclPeMetadata;
begin
  if not Assigned(FMetadata) and HasMetadata then
    FMetadata := TJclPeMetadata.Create(Image);
  Result := FMetadata;
end;

function TJclCLRHeaderEx.HasStrongNameSignature: Boolean;
begin
  with Header.StrongNameSignature do
  Result := Assigned(FStrongNameSignature) or
            ((Size > 0) and not IsBadReadPtr(Image.RvaToVa(VirtualAddress), Size));
end;

function TJclCLRHeaderEx.HasVTableFixup: Boolean;
begin
  with Header.VTableFixups do
  Result := Assigned(FVTableFixups) or
            ((Size > 0) and not IsBadReadPtr(Image.RvaToVa(VirtualAddress), Size));
end;

function TJclCLRHeaderEx.GetStrongNameSignature: TCustomMemoryStream;
begin
  if not Assigned(FStrongNameSignature) and HasStrongNameSignature then
  with Header.StrongNameSignature do
    FStrongNameSignature := TJclReferenceMemoryStream.Create(Image.RvaToVa(VirtualAddress), Size);
  Result := FStrongNameSignature;
end;

function TJclCLRHeaderEx.HasResources: Boolean;
begin
  with Header.Resources do
  Result := Assigned(FResources) or
            ((Size > 0) and not IsBadReadPtr(Image.RvaToVa(VirtualAddress), Size));
end;

procedure TJclCLRHeaderEx.UpdateResources;
var
  Base, Ptr: PChar;
  ARes: TJclCLRResourceRecord;
begin
  FResources := TObjectList.Create;

  with Header.Resources do
  begin
    Base := Image.RvaToVa(VirtualAddress);
    Ptr  := Base;
    while DWORD(Ptr-Base) < Size do
    begin
      ARes := TJclCLRResourceRecord.Create(Ptr, Ptr-Base, Ptr-Image.LoadedImage.MappedAddress);
      FResources.Add(ARes);
      Ptr := PChar(ARes.Memory) + ARes.Size;
    end;
  end;
end;

function TJclCLRHeaderEx.GetResource(
  const Idx: Integer): TJclCLRResourceRecord;
begin
  if not Assigned(FResources) and HasResources then
    UpdateResources;

  Result := TJclCLRResourceRecord(FResources.Items[Idx]);
end;

function TJclCLRHeaderEx.GetResourceCount: Integer;
begin
  if not Assigned(FResources) and HasResources then
    UpdateResources;

  if Assigned(FResources) then
    Result := FResources.Count
  else
    Result := 0;
end;

function TJclCLRHeaderEx.GetEntryPointToken: TJclCLRTableRow;
begin
  if Header.EntryPointToken <> 0 then
    Result := Metadata.Tokens[Header.EntryPointToken]
  else
    Result := nil;
end;

function TJclCLRHeaderEx.GetVTableFixup(
  const Idx: Integer): TJclCLRVTableFixupRecord;
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
        FVTableFixups.Add(TJclCLRVTableFixupRecord.Create(pData));
        Inc(pData);
      end;
    end;
  end;
  Result := TJclCLRVTableFixupRecord(FVTableFixups.Items[Idx]);
end;

function TJclCLRHeaderEx.GetVTableFixupCount: Integer;
begin
  Result := Header.VTableFixups.Size div SizeOf(TImageCorVTableFixup);
end;

function TJclCLRHeaderEx.ResourceAt(const Offset: DWORD): TJclCLRResourceRecord;
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
