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
  JclBase, JclFileUtils, JclPeImage;

{ TODO -cDOC : Original code: "Flier Lu" <flier_lu@yahoo.com.cn> }

//==================================================================================================
// TJclReferenceMemoryStream
//==================================================================================================
{ TODO : Move TJclReferenceMemoryStream to JclSysUtil unit or other }
type
  TJclReferenceMemoryStream = class (TCustomMemoryStream)
  public
    constructor Create(const Ptr: Pointer; Size: Longint);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

const
  MAX_CLASS_NAME = 1024;
  MAX_PATH_NAME  = 260;

  MetadataHeaderSignature = $424A5342; // 'BSJB'

type
  TJclCLRToken = DWORD;

//==================================================================================================
// Flag	Value	Description
//==================================================================================================
const  COMIMAGE_FLAGS_ILONLY	          = $00000001;	// Always 1 (see Section 23.1).  COMIMAGE_FLAGS_32BITREQUIRED	  = $00000002;	// Image may only be loaded into a 32-bit process, for instance if there are 32-bit vtablefixups, or casts from native integers to int32. CLI implementations that have 64 bit native integers shall refuse loading binaries with this flag set.
  COMIMAGE_FLAGS_STRONGNAMESIGNED = $00000008;	// Image has a strong name signature.
  COMIMAGE_FLAGS_TRACKDEBUGDATA	  = $00010000;	// Always 0 (see Section 23.1).
type
  TClrImageFlag = (cifILOnly, cif32BitRequired, cifStrongNameSinged, cifTrackDebugData);
  TClrImageFlags = set of TClrImageFlag;

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
  TClrVTableKind = (vtk32Bit, vtk64Bit, vtkFromUnmanaged, vtkCallMostDerived);
  TClrVTableKinds = set of TClrVTableKind;

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
    Signature: DWORD;   // Magic signature for physical metadata : 0x424A5342.
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
  TJclPeCLRTableKind = (
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
  TJclPeCLRHeaderEx = class;
  TJclPeMetadata = class;

  TJclPeCLRStreamClass = class of TJclPeCLRStream;
  TJclPeCLRStream = class
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

  TJclPeCLRStringsStream = class(TJclPeCLRStream)
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

  TJclPeCLRGuidStream = class(TJclPeCLRStream)
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

  TJclPeCLRBlobRecord = class(TJclReferenceMemoryStream)
  private
    FPtr: PByteArray;
    FOffset: DWORD;
  protected
    constructor Create(const AStream: TJclPeCLRStream; const APtr: PByteArray);
  public
    property Ptr: PByteArray read FPtr;
    property Offset: DWORD read FOffset;
  end;

  TJclPeCLRBlobStream = class(TJclPeCLRStream)
  private
    FBlobs: TObjectList;
    function GetBlob(const Idx: Integer): TJclPeCLRBlobRecord;
    function GetBlobCount: Integer;
  protected
    constructor Create(const AMetadata: TJclPeMetadata;
                       const AHeader: PCLRStreamHeader); override;
  public
    destructor Destroy; override;

    function At(const Offset: DWORD): TJclPeCLRBlobRecord;

    property Blobs[const Idx: Integer]: TJclPeCLRBlobRecord read GetBlob; default;
    property BlobCount: Integer read GetBlobCount;
  end;

  TJclPeCLRUserStringStream = class(TJclPeCLRBlobStream)
  private
    function GetString(const Idx: Integer): WideString;
    function GetStringCount: Integer;
  public
    function At(const Offset: DWORD): WideString;

    property Strings[const Idx: Integer]: WideString read GetString; default;
    property StringCount: Integer read GetStringCount;
  end;

  TJclPeCLRTableStream = class;

  TJclPeCLRHeapKind = (hkString, hkGuid, hkBlob);
  TJclPeCLRComboIndex = (ciResolutionScope);

  TJclPeCLRTable = class;

  TJclPeCLRTableRowClass = class of TJclPeCLRTableRow;
  TJclPeCLRTableRow = class
  private
    FTable: TJclPeCLRTable;
    FIndex: Integer;
  protected
    constructor Create(const ATable: TJclPeCLRTable); virtual;

    procedure Update; virtual;
  public
    property Table: TJclPeCLRTable read FTable;
    property Index: Integer read FIndex;
  end;

  TJclPeCLRTableClass = class of TJclPeCLRTable;
  TJclPeCLRTable = class
  private
    FStream: TJclPeCLRTableStream;
    FData,
    FPtr: PChar;
    FRows: TObjectList;
    FRowCount: Integer;
    FSize: DWORD;
    function GetOffset: DWORD;
    function GetRow(const Idx: Integer): TJclPeCLRTableRow;
    function GetRowCount: Integer;
  protected
    constructor Create(const AStream: TJclPeCLRTableStream;
      const Ptr: Pointer; const ARowCount: Integer); virtual;
    procedure Load; virtual;
    procedure SetSize(const Value: Integer);

    procedure Update; virtual;

    function AddRow(const ARow: TJclPeCLRTableRow): Integer;
    function RealRowCount: Integer;

    procedure Reset;
    function ReadIndex(const HeapKind: TJclPeCLRHeapKind): DWORD; overload;
    function ReadIndex(const TableKinds: array of TJclPeCLRTableKind): DWORD; overload;
    function ReadByte: Byte;
    function ReadWord: Word;
    function ReadDWord: DWORD;

    class function TableRowClass: TJclPeCLRTableRowClass; virtual;

    property Rows[const Idx: Integer]: TJclPeCLRTableRow read GetRow; default;
  public
    destructor Destroy; override;

    property Stream: TJclPeCLRTableStream read FStream;

    property Data: PChar read FData;
    property Size: DWORD read FSize;
    property Offset: DWORD read GetOffset;
    property RowCount: Integer read GetRowCount;
  end;

  TJclPeCLRTableModule = class(TJclPeCLRTable)
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

  TJclPeCLRTableModuleRef = class(TJclPeCLRTable);

  TJclPeCLRTableAssembly = class(TJclPeCLRTable)
  private
    FCultureOffset,
    FPublicKeyOffset,
    FHashAlgId,
    FNameOffset: DWORD;
    FMajorVersion,
    FBuildNumber,
    FRevisionNumber,
    FMinorVersion: Word;
    FFlags: DWORD;
    function GetCulture: WideString;
    function GetName: WideString;
    function GetPublicKey: TJclPeCLRBlobRecord;
  protected
    procedure Load; override;
  public
    property HashAlgId: DWORD read FHashAlgId;
    property MajorVersion: Word read FMajorVersion;
    property MinorVersion: Word read FMinorVersion;
    property BuildNumber: Word read FBuildNumber;
    property RevisionNumber: Word read FRevisionNumber;
    property Flags: DWORD read FFlags;
    property PublicKeyOffset: DWORD read FPublicKeyOffset;
    property NameOffset: DWORD read FNameOffset;
    property CultureOffset: DWORD read FCultureOffset;

    property PublicKey: TJclPeCLRBlobRecord read GetPublicKey;
    property Name: WideString read GetName;
    property Culture: WideString read GetCulture;
  end;

  TJclPeCLRTableAssemblyOS = class(TJclPeCLRTable)
  private
    FOSPlatformID,
    FOSMajorVersion,
    FOSMinorVersion: DWORD;
  protected
    procedure Load; override;
  public
    property OSPlatformID: DWORD read FOSPlatformID;
    property OSMajorVersion: DWORD read FOSMajorVersion;
    property OSMinorVersion: DWORD read FOSMinorVersion;
  end;

  TJclPeCLRTableAssemblyProcessor = class(TJclPeCLRTable)
  private
    FProcessor: DWORD;
  protected
    procedure Load; override;
  public
    property Processor: DWORD read FProcessor;
  end;

  TJclPeCLRTableAssemblyRefRow = class(TJclPeCLRTableRow)
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
    function GetHashValue: TJclPeCLRBlobRecord;
    function GetName: WideString;
    function GetPublicKeyOrToken: TJclPeCLRBlobRecord;
  protected
    constructor Create(const ATable: TJclPeCLRTable); override;
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

    property PublicKeyOrToken: TJclPeCLRBlobRecord read GetPublicKeyOrToken;
    property Name: WideString read GetName;
    property Culture: WideString read GetCulture;
    property HashValue: TJclPeCLRBlobRecord read GetHashValue;
  end;

  TJclPeCLRTableAssemblyRef = class(TJclPeCLRTable)
  private
    function GetRow(const Idx: Integer): TJclPeCLRTableAssemblyRefRow;
  protected
    class function TableRowClass: TJclPeCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclPeCLRTableAssemblyRefRow read GetRow; default;
  end;

  TJclPeCLRTableAssemblyRefOS = class(TJclPeCLRTableAssemblyOS)
  private
    FAssemblyRefIdx: DWORD;
    function GetAssemblyRef: TJclPeCLRTableAssemblyRef;
  protected
    procedure Load; override;
  public
    property AssemblyRefIdx: DWORD read FAssemblyRefIdx;

    property AssemblyRef: TJclPeCLRTableAssemblyRef read GetAssemblyRef;
  end;

  TJclPeCLRTableAssemblyRefProcessor = class(TJclPeCLRTableAssemblyProcessor)
  private
    FAssemblyRefIdx: DWORD;
    function GetAssemblyRef: TJclPeCLRTableAssemblyRef;
  protected
    procedure Load; override;
  public
    property AssemblyRefIdx: DWORD read FAssemblyRefIdx;

    property AssemblyRef: TJclPeCLRTableAssemblyRef read GetAssemblyRef;
  end;

  TJclPeCLRTableClassLayout = class(TJclPeCLRTable);

  TJclPeCLRTableConstantRow = class(TJclPeCLRTableRow)
  private
    FKind: Byte;
    FParentIdx: DWORD;
    FValueOffset: DWORD;
  protected
    constructor Create(const ATable: TJclPeCLRTable); override;
  public
    property Kind: Byte read FKind;
    property ParentIdx: DWORD read FParentIdx;
    property ValueOffset: DWORD read FValueOffset;
  end;

  TJclPeCLRTableConstant = class(TJclPeCLRTable)
  private
    function GetRow(const Idx: Integer): TJclPeCLRTableConstantRow;
  protected
    class function TableRowClass: TJclPeCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclPeCLRTableConstantRow read GetRow; default;
  end;

  TJclPeCLRTableCustomAttributeRow = class(TJclPeCLRTableRow)
  private
    FParentIdx: DWORD;
    FTypeIdx: DWORD;
    FValueOffset: DWORD;
    function GetValue: TJclPeCLRBlobRecord;
  protected
    constructor Create(const ATable: TJclPeCLRTable); override;
  public
    property ParentIdx: DWORD read FParentIdx;
    property TypeIdx: DWORD read FTypeIdx;
    property ValueOffset: DWORD read FValueOffset;

    property Value: TJclPeCLRBlobRecord read GetValue;
  end;

  TJclPeCLRTableCustomAttribute = class(TJclPeCLRTable)
  private
    function GetRow(const Idx: Integer): TJclPeCLRTableCustomAttributeRow;
  protected
    class function TableRowClass: TJclPeCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclPeCLRTableCustomAttributeRow read GetRow; default;
  end;

  TJclPeCLRTableDeclSecurity = class(TJclPeCLRTable);
  TJclPeCLRTableEventMap = class(TJclPeCLRTable);
  TJclPeCLRTableEvent = class(TJclPeCLRTable);
  TJclPeCLRTableExportedType = class(TJclPeCLRTable);

  TJclPeCLRTableFieldRow = class(TJclPeCLRTableRow)
  private
    FFlags: Word;
    FNameOffset: DWORD;
    FSignatureOffset: DWORD;
    FParentToken: TJclPeCLRTableRow;
    function GetName: WideString;
    function GetSignature: TJclPeCLRBlobRecord;
  protected
    constructor Create(const ATable: TJclPeCLRTable); override;

    procedure SetParentToken(const ARow: TJclPeCLRTableRow);
  public
    property Flags: Word read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property SignatureOffset: DWORD read FSignatureOffset;

    property Name: WideString read GetName;
    property Signature: TJclPeCLRBlobRecord read GetSignature;

    property ParentToken: TJclPeCLRTableRow read FParentToken;
  end;

  TJclPeCLRTableField = class(TJclPeCLRTable)
  private
    function GetRow(const Idx: Integer): TJclPeCLRTableFieldRow;
  protected
    class function TableRowClass: TJclPeCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclPeCLRTableFieldRow read GetRow; default;
  end;

  TJclPeCLRTableFieldLayout = class(TJclPeCLRTable);
  TJclPeCLRTableFieldMarshal = class(TJclPeCLRTable);
  TJclPeCLRTableFieldRVA = class(TJclPeCLRTable);

  TJclPeCLRTableFileRow = class(TJclPeCLRTableRow)
  end;

  TJclPeCLRTableFile = class(TJclPeCLRTable)
  end;

  TJclPeCLRTableImplMap = class(TJclPeCLRTable);

  TJclPeCLRTableInterfaceImplRow = class(TJclPeCLRTableRow)
  private
    FInterfaceIdx: DWORD;
    FClassIdx: DWORD;
  protected
    constructor Create(const ATable: TJclPeCLRTable); override;
  public
    property ClassIdx: DWORD read FClassIdx;
    property InterfaceIdx: DWORD read FInterfaceIdx;
  end;

  TJclPeCLRTableInterfaceImpl = class(TJclPeCLRTable)
  private
    function GetRow(const Idx: Integer): TJclPeCLRTableInterfaceImplRow;
  protected
    class function TableRowClass: TJclPeCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclPeCLRTableInterfaceImplRow read GetRow; default;
  end;

  TJclPeCLRTableManifestResourceRow = class(TJclPeCLRTableRow)
  private
    FOffset: DWORD;
    FFlags: DWORD;
    FImplementationIdx: DWORD;
    FNameOffset: DWORD;
    function GetName: WideString;
  protected
    constructor Create(const ATable: TJclPeCLRTable); override;
  public
    property Offset: DWORD read FOffset;
    property Flags: DWORD read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property ImplementationIdx: DWORD read FImplementationIdx;

    property Name: WideString read GetName;
  end;

  TJclPeCLRTableManifestResource = class(TJclPeCLRTable)
  private
    function GetRow(const Idx: Integer): TJclPeCLRTableManifestResourceRow;
  protected
    class function TableRowClass: TJclPeCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclPeCLRTableManifestResourceRow read GetRow; default;
  end;

  TJclPeCLRTableMemberRefRow = class(TJclPeCLRTableRow)
  private
    FClassIdx: DWORD;
    FNameOffset: DWORD;
    FSignatureOffset: DWORD;
    function GetName: WideString;
    function GetSignature: TJclPeCLRBlobRecord;
  protected
    constructor Create(const ATable: TJclPeCLRTable); override;
  public
    property ClassIdx: DWORD read FClassIdx;
    property NameOffset: DWORD read FNameOffset;
    property SignatureOffset: DWORD read FSignatureOffset;

    property Name: WideString read GetName;
    property Signature: TJclPeCLRBlobRecord read GetSignature;
  end;

  TJclPeCLRTableMemberRef = class(TJclPeCLRTable)
  private
    function GetRow(const Idx: Integer): TJclPeCLRTableMemberRefRow;
  protected
    class function TableRowClass: TJclPeCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclPeCLRTableMemberRefRow read GetRow; default;
  end;

  TJclPeCLRTableMethodDefRow = class(TJclPeCLRTableRow)
  private
    FRVA: DWORD;
    FImplFlags: Word;
    FFlags: Word;
    FNameOffset: DWORD;
    FSignatureOffset: DWORD;
    FParamListIdx: DWORD;
    FParentToken: TJclPeCLRTableRow;
    function GetName: WideString;
    function GetSignature: TJclPeCLRBlobRecord;
  protected
    constructor Create(const ATable: TJclPeCLRTable); override;

    procedure SetParentToken(const ARow: TJclPeCLRTableRow);
  public
    property RVA: DWORD read FRVA;
    property ImplFlags: Word read FImplFlags;
    property Flags: Word read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property SignatureOffset: DWORD read FSignatureOffset;
    property ParamListIdx: DWORD read FParamListIdx;

    property Name: WideString read GetName;
    property Signature: TJclPeCLRBlobRecord read GetSignature;

    property ParentToken: TJclPeCLRTableRow read FParentToken;
  end;

  TJclPeCLRTableMethodDef = class(TJclPeCLRTable)
  private
    function GetRow(const Idx: Integer): TJclPeCLRTableMethodDefRow;
  protected
    class function TableRowClass: TJclPeCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclPeCLRTableMethodDefRow read GetRow; default;
  end;

  TJclPeCLRTableMethodImplRow = class(TJclPeCLRTableRow)
  private
    FClassIdx: DWORD;
    FMethodBodyIdx: DWORD;
    FMethodDeclarationIdx: DWORD;
  protected
    constructor Create(const ATable: TJclPeCLRTable); override;
  public
    property ClassIdx: DWORD read FClassIdx;
    property MethodBodyIdx: DWORD read FMethodBodyIdx;
    property MethodDeclarationIdx: DWORD read FMethodDeclarationIdx;    
  end;

  TJclPeCLRTableMethodImpl = class(TJclPeCLRTable)
  private
    function GetRow(const Idx: Integer): TJclPeCLRTableMethodImplRow;
  protected
    class function TableRowClass: TJclPeCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclPeCLRTableMethodImplRow read GetRow; default;
  end;

  TJclPeCLRTableMethodSemanticsRow = class(TJclPeCLRTableRow)
  private
    FSemantics: Word;
    FMethodIdx: DWORD;
    FAssociationIdx: DWORD;
  protected
    constructor Create(const ATable: TJclPeCLRTable); override;
  public
    property Semantics: Word read FSemantics;
    property MethodIdx: DWORD read FMethodIdx;
    property AssociationIdx: DWORD read FAssociationIdx;  
  end;

  TJclPeCLRTableMethodSemantics = class(TJclPeCLRTable)
  private
    function GetRow(const Idx: Integer): TJclPeCLRTableMethodSemanticsRow;
  protected
    class function TableRowClass: TJclPeCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclPeCLRTableMethodSemanticsRow read GetRow; default;
  end;

  TJclPeCLRTableNestedClass = class(TJclPeCLRTable);

  TJclPeCLRTableParamDefRow = class(TJclPeCLRTableRow)
  private
    FFlags: Word;
    FSequence: Word;
    FNameOffset: DWORD;
    function GetName: WideString;
  protected
    constructor Create(const ATable: TJclPeCLRTable); override;
  public
    property Flags: Word read FFlags;
    property Sequence: Word read FSequence;
    property NameOffset: DWORD read FNameOffset;

    property Name: WideString read GetName;
  end;

  TJclPeCLRTableParamDef = class(TJclPeCLRTable)
  private
    function GetRow(const Idx: Integer): TJclPeCLRTableParamDefRow;
  protected
    class function TableRowClass: TJclPeCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclPeCLRTableParamDefRow read GetRow; default;
  end;

  TJclPeCLRTablePropertyRow = class(TJclPeCLRTableRow)
  private
    FKindIdx: DWORD;
    FNameOffset: DWORD;
    FFlags: Word;
    function GetName: WideString;
  protected
    constructor Create(const ATable: TJclPeCLRTable); override;
  public
    property Flags: Word read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property KindIdx: DWORD read FKindIdx;

    property Name: WideString read GetName;
  end;

  TJclPeCLRTableProperty = class(TJclPeCLRTable)
  private
    function GetRow(const Idx: Integer): TJclPeCLRTablePropertyRow;
  protected
    class function TableRowClass: TJclPeCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclPeCLRTablePropertyRow read GetRow; default;
  end;

  TJclPeCLRTablePropertyMapRow = class(TJclPeCLRTableRow)
  private
    FParentIdx: DWORD;
    FPropertyListIdx: DWORD;
  protected
    constructor Create(const ATable: TJclPeCLRTable); override;
  public
    property ParentIdx: DWORD read FParentIdx;
    property PropertyListIdx: DWORD read FPropertyListIdx;
  end;

  TJclPeCLRTablePropertyMap = class(TJclPeCLRTable)
  private
    function GetRow(const Idx: Integer): TJclPeCLRTablePropertyMapRow;
  protected
    class function TableRowClass: TJclPeCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclPeCLRTablePropertyMapRow read GetRow; default;
  end;

  TJclPeCLRTableStandAloneSigRow = class(TJclPeCLRTableRow)
  private
    FSignatureOffset: DWORD;
    function GetSignature: TJclPeCLRBlobRecord;
  protected
    constructor Create(const ATable: TJclPeCLRTable); override;
  public
    property SignatureOffset: DWORD read FSignatureOffset;

    property Signature: TJclPeCLRBlobRecord read GetSignature;
  end;

  TJclPeCLRTableStandAloneSig = class(TJclPeCLRTable)
  private
    function GetRow(const Idx: Integer): TJclPeCLRTableStandAloneSigRow;
  protected
    class function TableRowClass: TJclPeCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclPeCLRTableStandAloneSigRow read GetRow; default;
  end;

  TJclPeCLRTableTypeDefRow = class(TJclPeCLRTableRow)
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
    function GetField(const Idx: Integer): TJclPeCLRTableFieldRow;
    function GetFieldCount: Integer;
    function GetMethod(const Idx: Integer): TJclPeCLRTableMethodDefRow;
    function GetMethodCount: Integer;
    procedure UpdateFields;
    procedure UpdateMethods;
  protected
    constructor Create(const ATable: TJclPeCLRTable); override;

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

    property Fields[const Idx: Integer]: TJclPeCLRTableFieldRow read GetField;
    property FieldCount: Integer read GetFieldCount;
    property Methods[const Idx: Integer]: TJclPeCLRTableMethodDefRow read GetMethod;
    property MethodCount: Integer read GetMethodCount;
  end;

  TJclPeCLRTableTypeDef = class(TJclPeCLRTable)
  private
    function GetRow(const Idx: Integer): TJclPeCLRTableTypeDefRow;
  protected
    class function TableRowClass: TJclPeCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclPeCLRTableTypeDefRow read GetRow; default;
  end;

  TJclPeCLRTableTypeRefRow = class(TJclPeCLRTableRow)
  private
    FResolutionScopeIdx,
    FNamespaceOffset,
    FNameOffset: DWORD;
    function GetName: WideString;
    function GetNamespace: WideString;
    function GetResolutionScopeIdx: DWORD;
  protected
    constructor Create(const ATable: TJclPeCLRTable); override;
  public
    property ResolutionScopeIdx: DWORD read GetResolutionScopeIdx;
    property NameOffset: DWORD read FNameOffset;
    property NamespaceOffset: DWORD read FNamespaceOffset;

    property Name: WideString read GetName;
    property Namespace: WideString read GetNamespace;
  end;

  TJclPeCLRTableTypeRef = class(TJclPeCLRTable)
  private
    function GetRow(const Idx: Integer): TJclPeCLRTableTypeRefRow;
  protected
    class function TableRowClass: TJclPeCLRTableRowClass; override;
  public
    property Rows[const Idx: Integer]: TJclPeCLRTableTypeRefRow read GetRow; default;
  end;

  TJclPeCLRTableTypeSpec = class(TJclPeCLRTable);
  TJclPeCLRTableStream = class(TJclPeCLRStream)
  private
    FHeader: PCLRTableStreamHeader;
    FTables: array[TJclPeCLRTableKind] of TJclPeCLRTable;
    FTableCount: Integer;
    function GetVersionString: string;
    function GetTable(const AKind: TJclPeCLRTableKind): TJclPeCLRTable;
    function GetBigHeap(const AHeapKind: TJclPeCLRHeapKind): Boolean;
  protected
    constructor Create(const AMetadata: TJclPeMetadata;
                       const AHeader: PCLRStreamHeader); override;
  public
    destructor Destroy; override;

    procedure Update; virtual;

    function FindTable(const AKind: TJclPeCLRTableKind;
      var ATable: TJclPeCLRTable): Boolean;

    property Header: PCLRTableStreamHeader read FHeader;

    property VersionString: string read GetVersionString;
    property BigHeap[const AHeapKind: TJclPeCLRHeapKind]: Boolean read GetBigHeap;

    property Tables[const AKind: TJclPeCLRTableKind]: TJclPeCLRTable read GetTable;
    property TableCount: Integer read FTableCount;
  end;

  TJclPeMetadata = class
  private
    FImage: TJclPeImage;
    FHeader: PCLRMetadataHeader;
    FStreams: TObjectList;
    FStringStream: TJclPeCLRStringsStream;
    FGuidStream: TJclPeCLRGuidStream;
    FBlobStream: TJclPeCLRBlobStream;
    FTableStream: TJclPeCLRTableStream;
    function GetVersionString: WideString;
    function GetStream(const Idx: Integer): TJclPeCLRStream;
    function GetStreamCount: Integer;
    function GetString(const Idx: Integer): WideString;
    function GetStringCount: Integer;
    function GetGuid(const Idx: Integer): TGUID;
    function GetGuidCount: Integer;
    function GetBlob(const Idx: Integer): TJclPeCLRBlobRecord;
    function GetBlobCount: Integer;
    function GetTable(const AKind: TJclPeCLRTableKind): TJclPeCLRTable;
    function GetTableCount: Integer;
    function GetToken(const AToken: TJclCLRToken): TJclPeCLRTableRow;
  protected
    constructor Create(const AImage: TJclPeImage);
  public
    destructor Destroy; override;

    function FindStream(const AName: string; var Stream: TJclPeCLRStream): Boolean; overload;
    function FindStream(const AClass: TJclPeCLRStreamClass; var Stream: TJclPeCLRStream): Boolean; overload;

    function StringAt(const Offset: DWORD): WideString;
    function BlobAt(const Offset: DWORD): TJclPeCLRBlobRecord;

    function TokenExists(const Token: TJclCLRToken): Boolean;

    class function TokenTable(const Token: TJclCLRToken): TJclPeCLRTableKind;
    class function TokenIndex(const Token: TJclCLRToken): Integer;
    class function TokenCode(const Token: TJclCLRToken): Integer;
    class function MakeToken(const Table: TJclPeCLRTableKind; const Idx: Integer): TJclCLRToken;

    property Image: TJclPeImage read FImage;
    property Header: PCLRMetadataHeader read FHeader;

    property VersionString: WideString read GetVersionString;

    property Streams[const Idx: Integer]: TJclPeCLRStream read GetStream; default;
    property StreamCount: Integer read GetStreamCount;

    property Strings[const Idx: Integer]: WideString read GetString;
    property StringCount: Integer read GetStringCount;
    property Guids[const Idx: Integer]: TGUID read GetGuid;
    property GuidCount: Integer read GetGuidCount;
    property Blobs[const Idx: Integer]: TJclPeCLRBlobRecord read GetBlob;
    property BlobCount: Integer read GetBlobCount;
    property Tables[const AKind: TJclPeCLRTableKind]: TJclPeCLRTable read GetTable;
    property TableCount: Integer read GetTableCount;
    property Tokens[const AToken: TJclCLRToken]: TJclPeCLRTableRow read GetToken;
  end;

  TJclPeCLRResourceRecord = class(TJclReferenceMemoryStream)
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

  TJclPeCLRVTableFixupRecord = class
  private
    FData: PImageCorVTableFixup;
    function GetCount: DWORD;
    function GetKinds: TClrVTableKinds;
    function GetRVA: DWORD;
  protected
    constructor Create(const AData: PImageCorVTableFixup);

    class function VTableKinds(const Kinds: TClrVTableKinds): DWORD; overload;
    class function VTableKinds(const Kinds: DWORD): TClrVTableKinds; overload;
  public
    property Data: PImageCorVTableFixup read FData;
    property RVA: DWORD read GetRVA;               // RVA of Vtable
    property Count: DWORD read GetCount;           // Number of entries in Vtable
    property Kinds: TClrVTableKinds read GetKinds; // Type of the entries
  end;

  TJclPeCLRHeaderEx = class(TJclPeCLRHeader)
  private
    FMetadata: TJclPeMetadata;
    FFlags: TClrImageFlags;
    FStrongNameSignature: TCustomMemoryStream;
    FResources,
    FVTableFixups: TObjectList;
    function GetMetadata: TJclPeMetadata;
    function GetStrongNameSignature: TCustomMemoryStream;
    function GetEntryPointToken: TJclPeCLRTableRow;
    function GetVTableFixup(const Idx: Integer): TJclPeCLRVTableFixupRecord;
    function GetVTableFixupCount: Integer;
    procedure UpdateResources;
    function GetResource(const Idx: Integer): TJclPeCLRResourceRecord;
    function GetResourceCount: Integer;
  public
    constructor Create(const AImage: TJclPeImage);
    destructor Destroy; override;

    function HasResources: Boolean;
    function HasStrongNameSignature: Boolean;
    function HasVTableFixup: Boolean;

    function ResourceAt(const Offset: DWORD): TJclPeCLRResourceRecord;

    class function ClrImageFlag(const Flags: DWORD): TClrImageFlags; overload;
    class function ClrImageFlag(const Flags: TClrImageFlags): DWORD; overload;

    property Metadata: TJclPeMetadata read GetMetadata;

    property Flags: TClrImageFlags read FFlags;
    property EntryPointToken: TJclPeCLRTableRow read GetEntryPointToken;
    property StrongNameSignature: TCustomMemoryStream read GetStrongNameSignature;

    property Resources[const Idx: Integer]: TJclPeCLRResourceRecord read GetResource;
    property ResourceCount: Integer read GetResourceCount;
    property VTableFixups[const Idx: Integer]: TJclPeCLRVTableFixupRecord read GetVTableFixup;
    property VTableFixupCount: Integer read GetVTableFixupCount;
  end;

implementation

uses
  Math, TypInfo, JclUnicode, JclResources;

const
  GUID_NULL : TGUID = '{00000000-0000-0000-0000-000000000000}';

  ValidTableMapping: array[TJclPeCLRTableKind] of TJclPeCLRTableClass = (
    TJclPeCLRTableModule,               //  $00
    TJclPeCLRTableTypeRef,              //  $01
    TJclPeCLRTableTypeDef,              //  $02
    TJclPeCLRTable,                     //  $03
    TJclPeCLRTableField,                //  $04
    TJclPeCLRTable,                     //  $05
    TJclPeCLRTableMethodDef,            //  $06
    TJclPeCLRTable,                     //  $07
    TJclPeCLRTableParamDef,             //  $08
    TJclPeCLRTableInterfaceImpl,        //  $09
    TJclPeCLRTableMemberRef,            //  $0a
    TJclPeCLRTableConstant,             //  $0b
    TJclPeCLRTableCustomAttribute,      //  $0c
    TJclPeCLRTableFieldMarshal,         //  $0d
    TJclPeCLRTableDeclSecurity,         //  $0e
    TJclPeCLRTableClassLayout,          //  $0f
    TJclPeCLRTableFieldLayout,          //  $10
    TJclPeCLRTableStandAloneSig,        //  $11
    TJclPeCLRTableEventMap,             //  $12
    TJclPeCLRTable,                     //  $13
    TJclPeCLRTableEvent,                //  $14
    TJclPeCLRTablePropertyMap,          //  $15
    TJclPeCLRTable,                     //  $16
    TJclPeCLRTableProperty,             //  $17
    TJclPeCLRTableMethodSemantics,      //  $18
    TJclPeCLRTableMethodImpl,           //  $19
    TJclPeCLRTableModuleRef,            //  $1a
    TJclPeCLRTableTypeSpec,             //  $1b
    TJclPeCLRTableImplMap,              //  $1c
    TJclPeCLRTableFieldRVA,             //  $1d
    TJclPeCLRTable,                     //  $1e
    TJclPeCLRTable,                     //  $1f
    TJclPeCLRTableAssembly,             //  $20
    TJclPeCLRTableAssemblyProcessor,    //  $21
    TJclPeCLRTableAssemblyOS,           //  $22
    TJclPeCLRTableAssemblyRef,          //  $23
    TJclPeCLRTableAssemblyRefProcessor, //  $24
    TJclPeCLRTableAssemblyRefOS,        //  $25
    TJclPeCLRTableFile,                 //  $26
    TJclPeCLRTableExportedType,         //  $27
    TJclPeCLRTableManifestResource,     //  $28
    TJclPeCLRTableNestedClass,          //  $29
    TJclPeCLRTable,                     //  $2A
    TJclPeCLRTable);                    //  $2B

//--------------------------------------------------------------------------------------------------

constructor TJclReferenceMemoryStream.Create(const Ptr: Pointer; Size: Longint);
begin
  Assert(not IsBadReadPtr(Ptr, Size));
  inherited Create;
  SetPointer(Ptr, Size);
end;

//--------------------------------------------------------------------------------------------------

function TJclReferenceMemoryStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EJclError.CreateResRec(@RsCannotWriteRefStream);
end;

{ TODO -cDesign : Move FormatVersionString to other unit }
function FormatVersionString(const HiV, LoV: Word): string; overload;
begin
  Result := Format('%u.%.2u', [HiV, LoV]);
end;

function FormatVersionString(const Major, Minor, Build, Revision: Word): string; overload;
begin
  Result := Format('%u.%u.%u.%u', [Major, Minor, Build, Revision]);
end;

{ TJclPeCLRStream }

constructor TJclPeCLRStream.Create(const AMetadata: TJclPeMetadata;
  const AHeader: PCLRStreamHeader);
begin
  inherited Create;

  FMetadata := AMetadata;
  FHeader   := AHeader;
end;

function TJclPeCLRStream.GetName: string;
begin
  Result := FHeader.Name;
end;

function TJclPeCLRStream.GetOffset: DWORD;
begin
  Result := Data - Metadata.Image.LoadedImage.MappedAddress;
end;

function TJclPeCLRStream.GetSize: DWORD;
begin
  Result := FHeader.Size;
end;

function TJclPeCLRStream.GetData: Pointer;
begin
  Result := Pointer(DWORD(FMetadata.Header) + FHeader.Offset);
end;

{ TJclPeCLRStringsStream }

constructor TJclPeCLRStringsStream.Create(
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

destructor TJclPeCLRStringsStream.Destroy;
begin
  FreeAndNil(FStrings);

  inherited;
end;

function TJclPeCLRStringsStream.GetString(const Idx: Integer): WideString;
begin
  Result := UTF8ToWideString(FStrings.Strings[Idx]);
end;

function TJclPeCLRStringsStream.GetStringCount: Integer;
begin
  Result := FStrings.Count;
end;

function TJclPeCLRStringsStream.At(const Offset: DWORD): WideString;
var
  Idx: Integer;
begin
  Idx := FStrings.IndexOfObject(TObject(Offset));
  if Idx <> -1 then
    Result := GetString(Idx)
  else
    Result := '';
end;

{ TJclPeCLRGuidStream }

constructor TJclPeCLRGuidStream.Create(
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

function TJclPeCLRGuidStream.GetGuid(const Idx: Integer): TGUID;
begin
  Assert((0 <= Idx) and (Idx < GetGuidCount));
  Result := FGuids[Idx];
end;

function TJclPeCLRGuidStream.GetGuidCount: Integer;
begin
  Result := Length(FGuids);
end;

{ TJclPeCLRBlobRecord }

constructor TJclPeCLRBlobRecord.Create(const AStream: TJclPeCLRStream; const APtr: PByteArray);
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

{ TJclPeCLRBlobStream }

constructor TJclPeCLRBlobStream.Create(
  const AMetadata: TJclPeMetadata; const AHeader: PCLRStreamHeader);
var
  ABlob: TJclPeCLRBlobRecord;
begin
  inherited;

  FBlobs := TObjectList.Create;

  ABlob := TJclPeCLRBlobRecord.Create(Self, Data);
  while Assigned(ABlob) do
  begin
    if ABlob.Size > 0 then
      FBlobs.Add(ABlob);
    if (Integer(ABlob.Memory) + ABlob.Size) < (Integer(Self.Data) + Integer(Self.Size)) then
      ABlob := TJclPeCLRBlobRecord.Create(Self, Pointer(Integer(ABlob.Memory) + ABlob.Size))
    else
      ABlob := nil;
  end;
end;

destructor TJclPeCLRBlobStream.Destroy;
begin
  FreeAndNil(FBlobs);

  inherited;
end;

function TJclPeCLRBlobStream.At(const Offset: DWORD): TJclPeCLRBlobRecord;
var
  I: Integer;
begin
  for I:=0 to FBlobs.Count-1 do
  begin
    Result := TJclPeCLRBlobRecord(FBlobs.Items[I]);
    if Result.Offset = Offset then
      Exit;
  end;
  Result := nil;
end;

function TJclPeCLRBlobStream.GetBlob(const Idx: Integer): TJclPeCLRBlobRecord;
begin
  Result := TJclPeCLRBlobRecord(FBlobs.Items[Idx])
end;

function TJclPeCLRBlobStream.GetBlobCount: Integer;
begin
  Result := FBlobs.Count;
end;

{ TJclPeCLRUserStringStream }

function TJclPeCLRUserStringStream.GetString(const Idx: Integer): WideString;
var
  ABlob: TJclPeCLRBlobRecord;
begin
  ABlob := Blobs[Idx];
  SetLength(Result, ABlob.Size div 2 + 1);
  StrLCopyW(PWideChar(Result), PWideChar(ABlob.Memory), ABlob.Size div 2);
end;

function TJclPeCLRUserStringStream.GetStringCount: Integer;
begin
  Result := BlobCount;
end;

function TJclPeCLRUserStringStream.At(const Offset: DWORD): WideString;
var
  ABlob: TJclPeCLRBlobRecord;
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

{ TJclPeCLRTableRow }

constructor TJclPeCLRTableRow.Create(const ATable: TJclPeCLRTable);
begin
  inherited Create;

  FTable := ATable;
  FIndex := Table.RealRowCount;
end;

procedure TJclPeCLRTableRow.Update;
begin
  // do nothing, just for override
end;

{ TJclPeCLRTable }

constructor TJclPeCLRTable.Create(const AStream: TJclPeCLRTableStream;
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

destructor TJclPeCLRTable.Destroy;
begin
  FreeAndNil(FRows);

  inherited;
end;

procedure TJclPeCLRTable.Reset;
begin
  FPtr := FData;
end;

procedure TJclPeCLRTable.Load;
var
  I: Integer;
begin
  Assert(RowCount > 0);

  if TableRowClass <> TJclPeCLRTableRow then
    for I:=0 to RowCount-1 do
      AddRow(TableRowClass.Create(Self));
end;

procedure TJclPeCLRTable.SetSize(const Value: Integer);
begin
  FSize := Value;
  Assert(not IsBadReadPtr(FData, FSize));
end;

function TJclPeCLRTable.GetOffset: DWORD;
begin
  Result := DWORD(Data) - DWORD(Stream.Metadata.Image.LoadedImage.MappedAddress);
end;

function TJclPeCLRTable.GetRow(const Idx: Integer): TJclPeCLRTableRow;
begin
  Result := TJclPeCLRTableRow(FRows.Items[Idx]);
end;

function TJclPeCLRTable.GetRowCount: Integer;
begin
  Result := FRowCount;
end;

function TJclPeCLRTable.AddRow(const ARow: TJclPeCLRTableRow): Integer;
begin
  if not Assigned(FRows) then
    FRows := TObjectList.Create;

  Result := FRows.Add(ARow);
end;

function TJclPeCLRTable.RealRowCount: Integer;
begin
  if Assigned(FRows) then
    Result := FRows.Count
  else
    Result := 0;
end;

function TJclPeCLRTable.ReadIndex(const HeapKind: TJclPeCLRHeapKind): DWORD;
begin
  if Stream.BigHeap[HeapKind] then
    Result := ReadDWord
  else
    Result := ReadWord;
end;

function TJclPeCLRTable.ReadIndex(const TableKinds: array of TJclPeCLRTableKind): DWORD;
const
  TableIndexSize: array[Boolean] of Integer = (2, 4);
var
  BigHeap: Boolean;
  I: Integer;
  ATable: TJclPeCLRTable;
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

function TJclPeCLRTable.ReadByte: Byte;
begin
  Result := PByte(FPtr)^;
  Inc(FPtr, SizeOf(Byte));
end;

function TJclPeCLRTable.ReadWord: Word;
begin
  Result := PWord(FPtr)^;
  Inc(FPtr, SizeOf(Word));
end;

function TJclPeCLRTable.ReadDWord: DWORD;
begin
  Result := PDword(FPtr)^;
  Inc(FPtr, SizeOf(DWORD));
end;

class function TJclPeCLRTable.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTableRow;
end;

procedure TJclPeCLRTable.Update;
var
  I: Integer;
begin
  if Assigned(FRows) then
  for I:=0 to RowCount-1 do
    Rows[I].Update;
end;

{ TJclPeCLRTableModule }

procedure TJclPeCLRTableModule.Load;
begin
  Assert(RowCount = 1); // The Module table shall contain one and only one row

  inherited;

  FGeneration   := ReadWord;            // Generation (reserved, shall be zero)
  FNameOffset   := ReadIndex(hkString); // Name (index into String heap)
  FMvidIdx      := ReadIndex(hkGuid);   // Mvid (index into Guid heap)
  FEncIdIdx     := ReadIndex(hkGuid);   // Mvid (index into Guid heap)
  FEncBaseIdIdx := ReadIndex(hkGuid);   // Mvid (index into Guid heap)
end;

function TJclPeCLRTableModule.HasEncId: Boolean;
begin
  Result := FEncIdIdx > 0;
end;

function TJclPeCLRTableModule.HasEncBaseId: Boolean;
begin
  Result := FEncBaseIdIdx > 0;
end;

function TJclPeCLRTableModule.GetName: WideString;
begin
  Result := Stream.Metadata.StringAt(FNameOffset);
  Assert(Result <> ''); // Name shall index a non-null string.
  Assert(Length(Result) < MAX_PATH_NAME);
end;

function TJclPeCLRTableModule.GetMvid: TGUID;
begin
  // Mvid shall index a non-null GUID in the Guid heap
  Assert(FMvidIdx <= DWORD(Stream.Metadata.GuidCount));
  Result := Stream.Metadata.Guids[FMvidIdx-1];
end;

function TJclPeCLRTableModule.GetEncId: TGUID;
begin
  Result := Stream.Metadata.Guids[FEncIdIdx-1];
end;

function TJclPeCLRTableModule.GetEncBaseId: TGUID;
begin
  Result := Stream.Metadata.Guids[FEncBaseIdIdx-1];
end;

{ TJclPeCLRTableAssembly }

procedure TJclPeCLRTableAssembly.Load;
begin
  inherited;

  FHashAlgId      := ReadDWord;

  FMajorVersion   := ReadWord;
  FMinorVersion   := ReadWord;
  FBuildNumber    := ReadWord;
  FRevisionNumber := ReadWord;

  FFlags          := ReadDWord;

  FPublicKeyOffset   := ReadIndex(hkBlob);
  FNameOffset     := ReadIndex(hkString);
  FCultureOffset  := ReadIndex(hkString);
end;

function TJclPeCLRTableAssembly.GetCulture: WideString;
begin
  Result := Stream.Metadata.StringAt(FCultureOffset);
end;

function TJclPeCLRTableAssembly.GetName: WideString;
begin
  Result := Stream.Metadata.StringAt(FNameOffset);
end;

function TJclPeCLRTableAssembly.GetPublicKey: TJclPeCLRBlobRecord;
begin
  Result := Stream.Metadata.BlobAt(FPublicKeyOffset);
end;

{ TJclPeCLRTableAssemblyOS }

procedure TJclPeCLRTableAssemblyOS.Load;
begin
  inherited;

  FOSPlatformID   := ReadDWord;
  FOSMajorVersion := ReadDWord;
  FOSMinorVersion := ReadDWord;
end;

{ TJclPeCLRTableAssemblyProcessor }

procedure TJclPeCLRTableAssemblyProcessor.Load;
begin
  inherited;

  FProcessor := ReadDWord;
end;

{ TJclPeCLRTableAssemblyRefRow }

constructor TJclPeCLRTableAssemblyRefRow.Create(
  const ATable: TJclPeCLRTable);
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

function TJclPeCLRTableAssemblyRefRow.GetCulture: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FCultureOffset);
end;

function TJclPeCLRTableAssemblyRefRow.GetHashValue: TJclPeCLRBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FHashValueOffsetOffset);
end;

function TJclPeCLRTableAssemblyRefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclPeCLRTableAssemblyRefRow.GetPublicKeyOrToken: TJclPeCLRBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FPublicKeyOrTokenOffset);
end;

{ TJclPeCLRTableAssemblyRef }

function TJclPeCLRTableAssemblyRef.GetRow(const Idx: Integer): TJclPeCLRTableAssemblyRefRow;
begin
  Result := TJclPeCLRTableAssemblyRefRow(inherited GetRow(Idx));
end;

class function TJclPeCLRTableAssemblyRef.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTableAssemblyRefRow;
end;

{ TJclPeCLRTableAssemblyRefOS }

procedure TJclPeCLRTableAssemblyRefOS.Load;
begin
  inherited;

  FAssemblyRefIdx := ReadIndex([ttAssemblyRef]);
end;

function TJclPeCLRTableAssemblyRefOS.GetAssemblyRef: TJclPeCLRTableAssemblyRef;
begin
  { TODO : Implement GetAssemblyRef }
  Result := nil;
end;

{ TJclPeCLRTableAssemblyRefProcessor }

procedure TJclPeCLRTableAssemblyRefProcessor.Load;
begin
  inherited;

  FAssemblyRefIdx := ReadIndex([ttAssemblyRef]);
end;

function TJclPeCLRTableAssemblyRefProcessor.GetAssemblyRef: TJclPeCLRTableAssemblyRef;
begin
  { TODO : Implement GetAssemblyRef }
  Result := nil;
end;

{ TJclPeCLRTableConstantRow }

constructor TJclPeCLRTableConstantRow.Create(const ATable: TJclPeCLRTable);
begin
  inherited;

  FKind      := Table.ReadByte;
  Table.ReadByte; // padding zero
  FParentIdx := Table.ReadIndex([ttParamDef, ttFieldDef, ttProperty]);
  FValueOffset  := Table.ReadIndex(hkBlob);
end;

{ TJclPeCLRTableConstant }

function TJclPeCLRTableConstant.GetRow(const Idx: Integer): TJclPeCLRTableConstantRow;
begin
  Result := TJclPeCLRTableConstantRow(inherited GetRow(Idx));
end;

class function TJclPeCLRTableConstant.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTableConstantRow;
end;

{ TJclPeCLRTableCustomAttributeRow }

constructor TJclPeCLRTableCustomAttributeRow.Create(
  const ATable: TJclPeCLRTable);
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

function TJclPeCLRTableCustomAttributeRow.GetValue: TJclPeCLRBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FValueOffset);
end;

{ TJclPeCLRTableCustomAttribute }

function TJclPeCLRTableCustomAttribute.GetRow(const Idx: Integer): TJclPeCLRTableCustomAttributeRow;
begin
  Result := TJclPeCLRTableCustomAttributeRow(inherited GetRow(Idx));
end;

class function TJclPeCLRTableCustomAttribute.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTableCustomAttributeRow;
end;

{ TJclPeCLRTableFieldRow }

constructor TJclPeCLRTableFieldRow.Create(const ATable: TJclPeCLRTable);
begin
  inherited;

  FFlags        := Table.ReadWord;
  FNameOffset   := Table.ReadIndex(hkString);
  FSignatureOffset := Table.ReadIndex(hkBlob);
  FParentToken  := nil;
end;

function TJclPeCLRTableFieldRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclPeCLRTableFieldRow.GetSignature: TJclPeCLRBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FSignatureOffset);
end;

procedure TJclPeCLRTableFieldRow.SetParentToken(const ARow: TJclPeCLRTableRow);
begin
  FParentToken := ARow;
end;

{ TJclPeCLRTableField }

function TJclPeCLRTableField.GetRow(const Idx: Integer): TJclPeCLRTableFieldRow;
begin
  Result := TJclPeCLRTableFieldRow(inherited GetRow(Idx));
end;

class function TJclPeCLRTableField.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTableFieldRow;
end;

{ TJclPeCLRTableInterfaceImplRow }

constructor TJclPeCLRTableInterfaceImplRow.Create(const ATable: TJclPeCLRTable);
begin
  inherited;

  FClassIdx     := Table.ReadIndex([ttTypeDef]);
  FInterfaceIdx := Table.ReadIndex([ttTypeDef, ttTypeRef, ttTypeSpec]);
end;

{ TJclPeCLRTableInterfaceImpl }

function TJclPeCLRTableInterfaceImpl.GetRow(
  const Idx: Integer): TJclPeCLRTableInterfaceImplRow;
begin
  Result := TJclPeCLRTableInterfaceImplRow(inherited GetRow(Idx));
end;

class function TJclPeCLRTableInterfaceImpl.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTableInterfaceImplRow;
end;

{ TJclPeCLRTableManifestResourceRow }

constructor TJclPeCLRTableManifestResourceRow.Create(
  const ATable: TJclPeCLRTable);
begin
  inherited;

  FOffset            := Table.ReadDWord;
  FFlags             := Table.ReadDWord;
  FImplementationIdx := Table.ReadIndex(hkString);
  FNameOffset        := Table.ReadIndex([ttFile, ttAssemblyRef]);
end;

function TJclPeCLRTableManifestResourceRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

{ TJclPeCLRTableManifestResource }

function TJclPeCLRTableManifestResource.GetRow(
  const Idx: Integer): TJclPeCLRTableManifestResourceRow;
begin
  Result := TJclPeCLRTableManifestResourceRow(inherited GetRow(Idx));
end;

class function TJclPeCLRTableManifestResource.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTableManifestResourceRow;
end;

{ TJclPeCLRTableMemberRefRow }

constructor TJclPeCLRTableMemberRefRow.Create(
  const ATable: TJclPeCLRTable);
begin
  inherited;

  FClassIdx     := Table.ReadIndex([ttTypeRef, ttModuleRef, ttMethodDef, ttTypeSpec, ttTypeDef]); 
  FNameOffset   := Table.ReadIndex(hkString);
  FSignatureOffset := Table.ReadIndex(hkBlob);
end;

function TJclPeCLRTableMemberRefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclPeCLRTableMemberRefRow.GetSignature: TJclPeCLRBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FSignatureOffset);
end;

{ TJclPeCLRTableMemberRef }

function TJclPeCLRTableMemberRef.GetRow(const Idx: Integer): TJclPeCLRTableMemberRefRow;
begin
  Result := TJclPeCLRTableMemberRefRow(inherited GetRow(Idx));
end;

class function TJclPeCLRTableMemberRef.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTableMemberRefRow;
end;

{ TJclPeCLRTableMethodDefRow }

constructor TJclPeCLRTableMethodDefRow.Create(const ATable: TJclPeCLRTable);
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

function TJclPeCLRTableMethodDefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclPeCLRTableMethodDefRow.GetSignature: TJclPeCLRBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FSignatureOffset);
end;

procedure TJclPeCLRTableMethodDefRow.SetParentToken(const ARow: TJclPeCLRTableRow);
begin
  FParentToken := ARow;
end;

{ TJclPeCLRTableMethodDef }

function TJclPeCLRTableMethodDef.GetRow(const Idx: Integer): TJclPeCLRTableMethodDefRow;
begin
  Result := TJclPeCLRTableMethodDefRow(inherited GetRow(Idx));
end;

class function TJclPeCLRTableMethodDef.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTableMethodDefRow;
end;

{ TJclPeCLRTableMethodImplRow }

constructor TJclPeCLRTableMethodImplRow.Create(
  const ATable: TJclPeCLRTable);
begin
  inherited;

  FClassIdx             := Table.ReadIndex([ttTypeDef]);
  FMethodBodyIdx        := Table.ReadIndex([ttMethodDef, ttMemberRef]);
  FMethodDeclarationIdx := Table.ReadIndex([ttMethodDef, ttMemberRef]);
end;

{ TJclPeCLRTableMethodImpl }

function TJclPeCLRTableMethodImpl.GetRow(
  const Idx: Integer): TJclPeCLRTableMethodImplRow;
begin
  Result := TJclPeCLRTableMethodImplRow(inherited GetRow(Idx));
end;

class function TJclPeCLRTableMethodImpl.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTableMethodImplRow;
end;

{ TJclPeCLRTableMethodSemanticsRow }

constructor TJclPeCLRTableMethodSemanticsRow.Create(
  const ATable: TJclPeCLRTable);
begin
  inherited;

  FSemantics      := Table.ReadWord;
  FMethodIdx      := Table.ReadIndex([ttMethodDef]);
  FAssociationIdx := Table.ReadIndex([ttEvent, ttProperty]);
end;

{ TJclPeCLRTableMethodSemantics }

function TJclPeCLRTableMethodSemantics.GetRow(
  const Idx: Integer): TJclPeCLRTableMethodSemanticsRow;
begin
  Result := TJclPeCLRTableMethodSemanticsRow(inherited GetRow(Idx));
end;

class function TJclPeCLRTableMethodSemantics.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTableMethodSemanticsRow;
end;

{ TJclPeCLRTableParamDefRow }

constructor TJclPeCLRTableParamDefRow.Create(const ATable: TJclPeCLRTable);
begin
  inherited;

  FFlags      := Table.ReadWord;
  FSequence   := Table.ReadWord;
  FNameOffset := Table.ReadIndex(hkString);
end;

function TJclPeCLRTableParamDefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

{ TJclPeCLRTableParamDef }

function TJclPeCLRTableParamDef.GetRow(const Idx: Integer): TJclPeCLRTableParamDefRow;
begin
  Result := TJclPeCLRTableParamDefRow(inherited GetRow(Idx));
end;

class function TJclPeCLRTableParamDef.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTableParamDefRow;
end;


{ TJclPeCLRTablePropertyRow }

constructor TJclPeCLRTablePropertyRow.Create(const ATable: TJclPeCLRTable);
begin
  inherited;

  FFlags      := Table.ReadWord;
  FNameOffset := Table.ReadIndex(hkString);
  FKindIdx    := Table.ReadIndex(hkBlob);
end;

function TJclPeCLRTablePropertyRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

{ TJclPeCLRTableProperty }

function TJclPeCLRTableProperty.GetRow(const Idx: Integer): TJclPeCLRTablePropertyRow;
begin
  Result := TJclPeCLRTablePropertyRow(inherited GetRow(Idx));
end;

class function TJclPeCLRTableProperty.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTablePropertyRow;
end;

{ TJclPeCLRTablePropertyMapRow }

constructor TJclPeCLRTablePropertyMapRow.Create(
  const ATable: TJclPeCLRTable);
begin
  inherited;

  FParentIdx       := Table.ReadIndex([ttTypeDef]);
  FPropertyListIdx := Table.ReadIndex([ttProperty]);
end;

{ TJclPeCLRTablePropertyMap }

function TJclPeCLRTablePropertyMap.GetRow(
  const Idx: Integer): TJclPeCLRTablePropertyMapRow;
begin
  Result := TJclPeCLRTablePropertyMapRow(inherited GetRow(Idx));
end;

class function TJclPeCLRTablePropertyMap.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTablePropertyMapRow;
end;

{ TJclPeCLRTableStandAloneSigRow }

constructor TJclPeCLRTableStandAloneSigRow.Create(
  const ATable: TJclPeCLRTable);
begin
  inherited;

  FSignatureOffset := Table.ReadIndex(hkBlob);
end;

function TJclPeCLRTableStandAloneSigRow.GetSignature: TJclPeCLRBlobRecord;
begin
  Result := Table.Stream.Metadata.BlobAt(FSignatureOffset);
end;

{ TJclPeCLRTableStandAloneSig }

function TJclPeCLRTableStandAloneSig.GetRow(
  const Idx: Integer): TJclPeCLRTableStandAloneSigRow;
begin
  Result := TJclPeCLRTableStandAloneSigRow(inherited GetRow(Idx));
end;

class function TJclPeCLRTableStandAloneSig.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTableStandAloneSigRow;
end;

{ TJclPeCLRTableTypeDefRow }

constructor TJclPeCLRTableTypeDefRow.Create(const ATable: TJclPeCLRTable);
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

destructor TJclPeCLRTableTypeDefRow.Destroy;
begin
  FreeAndNil(FFields);
  FreeAndNil(FMethods);

  inherited;
end;

function TJclPeCLRTableTypeDefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclPeCLRTableTypeDefRow.GetNamespace: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNamespaceOffset);
end;

function TJclPeCLRTableTypeDefRow.GetField(const Idx: Integer): TJclPeCLRTableFieldRow;
begin
  Result := TJclPeCLRTableFieldRow(FFields.Items[Idx])
end;

function TJclPeCLRTableTypeDefRow.GetFieldCount: Integer;
begin
  Result := FFields.Count
end;

function TJclPeCLRTableTypeDefRow.HasField: Boolean;
begin
  Result := Assigned(FFields);
end;

function TJclPeCLRTableTypeDefRow.GetMethod(const Idx: Integer): TJclPeCLRTableMethodDefRow;
begin
  Result := TJclPeCLRTableMethodDefRow(FMethods.Items[Idx])
end;

function TJclPeCLRTableTypeDefRow.GetMethodCount: Integer;
begin
  Result := FMethods.Count
end;

function TJclPeCLRTableTypeDefRow.HasMethod: Boolean;
begin
  Result := Assigned(FMethods);
end;

procedure TJclPeCLRTableTypeDefRow.UpdateFields;
var
  FieldTable: TJclPeCLRTableField;
  Idx, MaxFieldListIdx: DWORD;
begin
  with Table as TJclPeCLRTableTypeDef do
  if not Assigned(FFields) and (FieldListIdx <> 0) and
     Stream.FindTable(ttFieldDef, TJclPeCLRTable(FieldTable)) then
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

procedure TJclPeCLRTableTypeDefRow.UpdateMethods;
var
  MethodTable: TJclPeCLRTableMethodDef;
  Idx, MaxMethodListIdx: DWORD;
begin
  with Table as TJclPeCLRTableTypeDef do
  if not Assigned(FMethods) and (MethodListIdx <> 0) and
     Stream.FindTable(ttMethodDef, TJclPeCLRTable(MethodTable)) then
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

procedure TJclPeCLRTableTypeDefRow.Update;
begin
  inherited;

  UpdateFields;
  UpdateMethods;
end;

{ TJclPeCLRTableTypeDef }

class function TJclPeCLRTableTypeDef.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTableTypeDefRow;
end;

function TJclPeCLRTableTypeDef.GetRow(const Idx: Integer): TJclPeCLRTableTypeDefRow;
begin
  Result := TJclPeCLRTableTypeDefRow(inherited GetRow(Idx));
end;

{ TJclPeCLRTableTypeRefRow }

constructor TJclPeCLRTableTypeRefRow.Create(const ATable: TJclPeCLRTable);
begin
  inherited;

  FResolutionScopeIdx := Table.ReadIndex([ttModule, ttModuleRef, ttAssemblyRef, ttTypeRef]);
  FNameOffset         := Table.ReadIndex(hkString);
  FNamespaceOffset    := Table.ReadIndex(hkString);
end;

function TJclPeCLRTableTypeRefRow.GetResolutionScopeIdx: DWORD;
begin
  { TODO : Implement GetResolutionScopeIdx }
  Result := 0;
end;

function TJclPeCLRTableTypeRefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclPeCLRTableTypeRefRow.GetNamespace: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNamespaceOffset);
end;

{ TJclPeCLRTableTypeRef }

class function TJclPeCLRTableTypeRef.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTableTypeRefRow;
end;

function TJclPeCLRTableTypeRef.GetRow(const Idx: Integer): TJclPeCLRTableTypeRefRow;
begin
  Result := TJclPeCLRTableTypeRefRow(inherited GetRow(Idx));
end;

{ TJclPeCLRTableStream }

constructor TJclPeCLRTableStream.Create(const AMetadata: TJclPeMetadata;
  const AHeader: PCLRStreamHeader);

  function BitCount(const Value: Int64): Integer;
  var
    AKind: TJclPeCLRTableKind;
  begin
    Result := 0;
    for AKind:=Low(TJclPeCLRTableKind) to High(TJclPeCLRTableKind) do
      if (Value and (Int64(1) shl Integer(AKind))) <> 0 then
        Inc(Result);
  end;

  procedure EnumTables;
  var
    AKind: TJclPeCLRTableKind;
    pTable: Pointer;
  begin
    pTable      := @Header.Rows[BitCount(Header.Valid)];
    FTableCount := 0;
    for AKind:=Low(TJclPeCLRTableKind) to High(TJclPeCLRTableKind) do
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

destructor TJclPeCLRTableStream.Destroy;
begin
  FreeAndNil(FTables);

  inherited;
end;

function TJclPeCLRTableStream.GetVersionString: string;
begin
  Result := FormatVersionString(Header.MajorVersion, Header.MinorVersion);
end;

function TJclPeCLRTableStream.GetTable(const AKind: TJclPeCLRTableKind): TJclPeCLRTable;
begin
  Result := TJclPeCLRTable(FTables[AKind]);
end;

function TJclPeCLRTableStream.GetBigHeap(const AHeapKind: TJclPeCLRHeapKind): Boolean;
const
  HeapSizesMapping: array[TJclPeCLRHeapKind] of DWORD = (1, 2, 4);
begin
  Result := (Header.HeapSizes and HeapSizesMapping[AHeapKind]) <> 0;
end;

function TJclPeCLRTableStream.FindTable(const AKind: TJclPeCLRTableKind;
  var ATable: TJclPeCLRTable): Boolean;
begin
  ATable := FTables[AKind];
  Result := Assigned(ATable);
end;

procedure TJclPeCLRTableStream.Update;
var
  AKind: TJclPeCLRTableKind;
begin
  for AKind:=Low(TJclPeCLRTableKind) to High(TJclPeCLRTableKind) do
    if Assigned(FTables[AKind]) then
      FTables[AKind].Update;
end;

{ TJclPeMetadata }

constructor TJclPeMetadata.Create(const AImage: TJclPeImage);

  function GetStreamClass(const Name: string): TJclPeCLRStreamClass;
  begin
    if CompareText(Name, '#Strings') = 0 then
      Result := TJclPeCLRStringsStream
    else if CompareText(Name, '#GUID') = 0 then
      Result := TJclPeCLRGuidStream
    else if CompareText(Name, '#Blob') = 0 then
      Result := TJclPeCLRBlobStream
    else if CompareText(Name, '#US') = 0 then
      Result := TJclPeCLRUserStringStream
    else if CompareText(Name, '#~') = 0 then
      Result := TJclPeCLRTableStream
    else
      Result := TJclPeCLRStream;
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
    TableStream: TJclPeCLRTableStream;
  begin
    pStreamPart := PStreamPartitionHeader(DWORD(@Header.Version[0]) + Header.Length);
    pStream     := @pStreamPart.StreamHeaders[0];
    for I:=0 to pStreamPart.StreamCount-1 do
    begin
      FStreams.Add(GetStreamClass(pStream.Name).Create(Self, pStream));

      pStream := PCLRStreamHeader(DWORD(@pStream.Name[0]) +
                 (((StrLen(@pStream.Name[0])+1)+3) and (not $3)));
    end;
    if FindStream(TJclPeCLRTableStream, TJclPeCLRStream(TableStream)) then
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

function TJclPeMetadata.GetStream(const Idx: Integer): TJclPeCLRStream;
begin
  Result := TJclPeCLRStream(FStreams.Items[Idx]);
end;

function TJclPeMetadata.GetStreamCount: Integer;
begin
  Result := FStreams.Count;
end;

function TJclPeMetadata.FindStream(const AName: string;
  var Stream: TJclPeCLRStream): Boolean;
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

function TJclPeMetadata.FindStream(const AClass: TJclPeCLRStreamClass;
  var Stream: TJclPeCLRStream): Boolean;
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


function TJclPeMetadata.GetToken(const AToken: TJclCLRToken): TJclPeCLRTableRow;
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
     FindStream(TJclPeCLRStringsStream, TJclPeCLRStream(FStringStream)) then
    Result := FStringStream.Strings[Idx];
end;

function TJclPeMetadata.GetStringCount: Integer;
begin
  if Assigned(FStringStream) or
     FindStream(TJclPeCLRStringsStream, TJclPeCLRStream(FStringStream)) then
    Result := FStringStream.StringCount
  else
    Result := 0;
end;

function TJclPeMetadata.StringAt(const Offset: DWORD): WideString;
begin
  if Assigned(FStringStream) or
     FindStream(TJclPeCLRStringsStream, TJclPeCLRStream(FStringStream)) then
    Result := TJclPeCLRStringsStream(FStringStream).At(Offset)
  else
    Result := '';
end;

function TJclPeMetadata.BlobAt(const Offset: DWORD): TJclPeCLRBlobRecord;
begin
  if Assigned(FBlobStream) or
     FindStream(TJclPeCLRBlobStream, TJclPeCLRStream(FBlobStream)) then
    Result := TJclPeCLRBlobStream(FBlobStream).At(Offset)
  else
    Result := nil;
end;

function TJclPeMetadata.GetGuid(const Idx: Integer): TGUID;
begin
  if Assigned(FGuidStream) or
     FindStream(TJclPeCLRGuidStream, TJclPeCLRStream(FGuidStream)) then
    Result := FGuidStream.Guids[Idx]
  else
    Result := GUID_NULL;
end;

function TJclPeMetadata.GetGuidCount: Integer;
begin
  if Assigned(FGuidStream) or
     FindStream(TJclPeCLRGuidStream, TJclPeCLRStream(FGuidStream)) then
    Result := FGuidStream.GuidCount
  else
    Result := 0;
end;

function TJclPeMetadata.GetBlob(const Idx: Integer): TJclPeCLRBlobRecord;
begin
  if Assigned(FBlobStream) or
     FindStream(TJclPeCLRBlobStream, TJclPeCLRStream(FBlobStream)) then
    Result := FBlobStream.Blobs[Idx]
  else
    Result := nil;
end;

function TJclPeMetadata.GetBlobCount: Integer;
begin
  if Assigned(FBlobStream) or
     FindStream(TJclPeCLRBlobStream, TJclPeCLRStream(FBlobStream)) then
    Result := FBlobStream.BlobCount
  else
    Result := 0;
end;

function TJclPeMetadata.GetTable(const AKind: TJclPeCLRTableKind): TJclPeCLRTable;
begin
  if Assigned(FTableStream) or
     FindStream(TJclPeCLRTableStream, TJclPeCLRStream(FTableStream)) then
    Result := FTableStream.Tables[AKind]
  else
    Result := nil;
end;

function TJclPeMetadata.GetTableCount: Integer;
begin
  if Assigned(FTableStream) or
     FindStream(TJclPeCLRTableStream, TJclPeCLRStream(FTableStream)) then
    Result := FTableStream.TableCount
  else
    Result := 0;
end;

function TJclPeMetadata.TokenExists(const Token: TJclCLRToken): Boolean;
begin
  Result := TokenIndex(Token) in [1..Tables[TokenTable(Token)].RowCount];
end;

class function TJclPeMetadata.TokenTable(const Token: TJclCLRToken): TJclPeCLRTableKind;
begin
  Result := TJclPeCLRTableKind(Token shr 24);
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
  const Table: TJclPeCLRTableKind; const Idx: Integer): TJclCLRToken;
begin
  Result := (DWORD(Table) shl 24) and TokenIndex(Idx);
end;

{ TJclPeCLRResourceRecord }

constructor TJclPeCLRResourceRecord.Create(const AData: PChar;
  const AOffset: DWORD; const ARVA: DWORD);
begin
  FData   := AData;
  FOffset := AOffset;
  FRVA    := ARVA;

  inherited Create(Pointer(DWORD(Data)+SizeOf(DWORD)), PDWORD(Data)^);
end;

{ TJclPeCLRVTableFixupRecord }

constructor TJclPeCLRVTableFixupRecord.Create(
  const AData: PImageCorVTableFixup);
begin
  inherited Create;

  FData := AData;
end;

function TJclPeCLRVTableFixupRecord.GetCount: DWORD;
begin
  Result := Data.Count;
end;

function TJclPeCLRVTableFixupRecord.GetKinds: TClrVTableKinds;
begin
  Result := VTableKinds(Data.Kind);
end;

function TJclPeCLRVTableFixupRecord.GetRVA: DWORD;
begin
  Result := Data.RVA;
end;

const
  ClrVTableKindMapping: array[TClrVTableKind] of DWORD =
    (COR_VTABLE_32BIT, COR_VTABLE_64BIT,
     COR_VTABLE_FROM_UNMANAGED, COR_VTABLE_CALL_MOST_DERIVED);

class function TJclPeCLRVTableFixupRecord.VTableKinds(
  const Kinds: TClrVTableKinds): DWORD;
var
  AKind: TClrVTableKind;
begin
  Result := 0;
  for AKind:=Low(TClrVTableKind) to High(TClrVTableKind) do
    if AKind in Kinds then
      Result := Result or ClrVTableKindMapping[AKind];
end;

class function TJclPeCLRVTableFixupRecord.VTableKinds(
  const Kinds: DWORD): TClrVTableKinds;
var
  AKind: TClrVTableKind;
begin
  Result := [];
  for AKind:=Low(TClrVTableKind) to High(TClrVTableKind) do
    if (ClrVTableKindMapping[AKind] and Kinds) = ClrVTableKindMapping[AKind] then
      Include(Result, AKind);
end;

{ TJclPeCLRInformation }

constructor TJclPeCLRHeaderEx.Create(const AImage: TJclPeImage);
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

destructor TJclPeCLRHeaderEx.Destroy;
begin
  FreeAndNil(FVTableFixups);
  FreeAndNil(FStrongNameSignature);
  FreeAndNil(FResources);
  FreeAndNil(FMetadata);

  inherited;
end;

const
  ClrImageFlagMapping: array[TClrImageFlag] of DWORD =
    (COMIMAGE_FLAGS_ILONLY, COMIMAGE_FLAGS_32BITREQUIRED,
     COMIMAGE_FLAGS_STRONGNAMESIGNED, COMIMAGE_FLAGS_TRACKDEBUGDATA);

class function TJclPeCLRHeaderEx.ClrImageFlag(const Flags: DWORD): TClrImageFlags;
var
  AFlag: TClrImageFlag;
begin
  Result := [];
  for AFlag:=Low(TClrImageFlag) to High(TClrImageFlag) do
    if (ClrImageFlagMapping[AFlag] and Flags) = ClrImageFlagMapping[AFlag] then
      Include(Result, AFlag);
end;

class function TJclPeCLRHeaderEx.ClrImageFlag(const Flags: TClrImageFlags): DWORD;
var
  AFlag: TClrImageFlag;
begin
  Result := 0;
  for AFlag:=Low(TClrImageFlag) to High(TClrImageFlag) do
    if AFlag in Flags then
      Result := Result or ClrImageFlagMapping[AFlag];
end;

function TJclPeCLRHeaderEx.GetMetadata: TJclPeMetadata;
begin
  if not Assigned(FMetadata) and HasMetadata then
    FMetadata := TJclPeMetadata.Create(Image);
  Result := FMetadata;
end;

function TJclPeCLRHeaderEx.HasStrongNameSignature: Boolean;
begin
  with Header.StrongNameSignature do
  Result := Assigned(FStrongNameSignature) or
            ((Size > 0) and not IsBadReadPtr(Image.RvaToVa(VirtualAddress), Size));
end;

function TJclPeCLRHeaderEx.HasVTableFixup: Boolean;
begin
  with Header.VTableFixups do
  Result := Assigned(FVTableFixups) or
            ((Size > 0) and not IsBadReadPtr(Image.RvaToVa(VirtualAddress), Size));
end;

function TJclPeCLRHeaderEx.GetStrongNameSignature: TCustomMemoryStream;
begin
  if not Assigned(FStrongNameSignature) and HasStrongNameSignature then
  with Header.StrongNameSignature do
    FStrongNameSignature := TJclReferenceMemoryStream.Create(Image.RvaToVa(VirtualAddress), Size);
  Result := FStrongNameSignature;
end;

function TJclPeCLRHeaderEx.HasResources: Boolean;
begin
  with Header.Resources do
  Result := Assigned(FResources) or
            ((Size > 0) and not IsBadReadPtr(Image.RvaToVa(VirtualAddress), Size));
end;

procedure TJclPeCLRHeaderEx.UpdateResources;
var
  Base, Ptr: PChar;
  ARes: TJclPeCLRResourceRecord;
begin
  FResources := TObjectList.Create;

  with Header.Resources do
  begin
    Base := Image.RvaToVa(VirtualAddress);
    Ptr  := Base;
    while DWORD(Ptr-Base) < Size do
    begin
      ARes := TJclPeCLRResourceRecord.Create(Ptr, Ptr-Base, Ptr-Image.LoadedImage.MappedAddress);
      FResources.Add(ARes);
      Ptr := PChar(ARes.Memory) + ARes.Size;
    end;
  end;
end;

function TJclPeCLRHeaderEx.GetResource(
  const Idx: Integer): TJclPeCLRResourceRecord;
begin
  if not Assigned(FResources) and HasResources then
    UpdateResources;

  Result := TJclPeCLRResourceRecord(FResources.Items[Idx]);
end;

function TJclPeCLRHeaderEx.GetResourceCount: Integer;
begin
  if not Assigned(FResources) and HasResources then
    UpdateResources;

  if Assigned(FResources) then
    Result := FResources.Count
  else
    Result := 0;
end;

function TJclPeCLRHeaderEx.GetEntryPointToken: TJclPeCLRTableRow;
begin
  if Header.EntryPointToken <> 0 then
    Result := Metadata.Tokens[Header.EntryPointToken]
  else
    Result := nil;
end;

function TJclPeCLRHeaderEx.GetVTableFixup(
  const Idx: Integer): TJclPeCLRVTableFixupRecord;
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
        FVTableFixups.Add(TJclPeCLRVTableFixupRecord.Create(pData));
        Inc(pData);
      end;
    end;
  end;
  Result := TJclPeCLRVTableFixupRecord(FVTableFixups.Items[Idx]);
end;

function TJclPeCLRHeaderEx.GetVTableFixupCount: Integer;
begin
  Result := Header.VTableFixups.Size div SizeOf(TImageCorVTableFixup);
end;

function TJclPeCLRHeaderEx.ResourceAt(const Offset: DWORD): TJclPeCLRResourceRecord;
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
