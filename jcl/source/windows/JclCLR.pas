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
{ Microsoft .Net framework CLR information support routines and classes.                           }
{                                                                                                  }
{ Unit owner: Flier Lu <flier_lu@yahoo.com.cn>                                                     }
{ Last modified: February 24, 2002                                                                 }
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

{ TODO -cDesign : Move TJclIndex define to JclBase? }
type
  TJclIndex = Cardinal;

const
  MetadataHeaderSignature = $424A5342; // 'BSJB'

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
    //Rows: array[0..n-1] of DWord;
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
  TJclPeCLRInformation = class;
  TJclPeMetadataHeader = class;

  TJclPeCLRStreamClass = class of TJclPeCLRStream;
  TJclPeCLRStream = class
  private
    FMetadata: TJclPeMetadataHeader;
    FHeader: PCLRStreamHeader;
    function GetName: string;
    function GetOffset: DWORD;
    function GetSize: DWORD;
    function GetData: Pointer;
  protected
    constructor Create(const AMetadata: TJclPeMetadataHeader;
                       const AHeader: PCLRStreamHeader); virtual;
  public
    property Metadata: TJclPeMetadataHeader read FMetadata;
    property Header: PCLRStreamHeader read FHeader;

    property Name: string read GetName;
    property Offset: DWORD read GetOffset;
    property Size: DWORD read GetSize;
    property Data: Pointer read GetData;
  end;

  TJclPeCLRStringsStream = class(TJclPeCLRStream)
  private
    FStrings: TStrings;

    function GetString(const Idx: TJclIndex): WideString;
    function GetStringCount: TJclIndex;
  protected
    constructor Create(const AMetadata: TJclPeMetadataHeader;
                       const AHeader: PCLRStreamHeader); override;
  public
    destructor Destroy; override;

    function At(const Offset: DWORD): WideString;

    property Strings[const Idx: TJclIndex]: WideString read GetString; default;
    property StringCount: TJclIndex read GetStringCount;
  end;

  TJclPeCLRGuidStream = class(TJclPeCLRStream)
  private
    FGuids: array of TGUID;
    function GetGuid(const Idx: TJclIndex): TGUID;
    function GetGuidCount: TJclIndex;
  protected
    constructor Create(const AMetadata: TJclPeMetadataHeader;
                       const AHeader: PCLRStreamHeader); override;
  public
    property Guids[const Idx: TJclIndex]: TGUID read GetGuid; default;
    property GuidCount: TJclIndex read GetGuidCount;
  end;

  TJclPeCLRBlobRecord = class
  private
    FPtr: PByteArray;
    FData: Pointer;
    FSize: DWORD;
  protected
    constructor Create(const APtr: PByteArray);
  public
    property Ptr: PByteArray read FPtr;
    property Data: Pointer read FData;
    property Size: DWORD read FSize;
  end;

  TJclPeCLRBlobStream = class(TJclPeCLRStream)
  private
    FBlobs: TObjectList;
    function GetBlob(const Idx: TJclIndex): TJclPeCLRBlobRecord;
    function GetBlobCount: TJclIndex;
  protected
    constructor Create(const AMetadata: TJclPeMetadataHeader;
                       const AHeader: PCLRStreamHeader); override;
  public
    destructor Destroy; override;

    property Blobs[const Idx: TJclIndex]: TJclPeCLRBlobRecord read GetBlob; default;
    property BlobCount: TJclIndex read GetBlobCount;
  end;

  TJclPeCLRUserStringStream = class(TJclPeCLRBlobStream)
  private
    function GetString(const Idx: TJclIndex): WideString;
    function GetStringCount: TJclIndex;
  public
    property Strings[const Idx: TJclIndex]: WideString read GetString; default;
    property StringCount: TJclIndex read GetStringCount;
  end;

  TJclPeCLRTableStream = class;

  TJclPeCLRHeapKind = (hkString, hkGuid, hkBlob);
  TJclPeCLRComboIndex = (ciResolutionScope);

  TJclPeCLRTable = class;

  TJclPeCLRTableRowClass = class of TJclPeCLRTableRow;
  TJclPeCLRTableRow = class
  private
    FTable: TJclPeCLRTable;
  protected
    constructor Create(const ATable: TJclPeCLRTable); virtual;
  public
    property Table: TJclPeCLRTable read FTable;
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
    function GetRow(const Idx: TJclIndex): TJclPeCLRTableRow;
    function GetRowCount: TJclIndex;
  protected
    constructor Create(const AStream: TJclPeCLRTableStream;
      const Ptr: Pointer; const ARowCount: Integer); virtual;
    procedure Load; virtual;
    procedure SetSize(const Value: Integer);

    function AddRow(const ARow: TJclPeCLRTableRow): Integer;

    procedure Reset;
    function ReadIndex(const AKind: TJclPeCLRHeapKind): DWORD; overload;
    function ReadIndex(const AKind: TJclPeCLRTableKind): DWORD; overload;
    function ReadIndex(const Tables: array of TJclPeCLRTableKind): DWORD; overload;
    function ReadDWord: DWORD;
    function ReadWord: Word;

    class function TableRowClass: TJclPeCLRTableRowClass; virtual;
  public
    destructor Destroy; override;

    property Stream: TJclPeCLRTableStream read FStream;

    property Data: PChar read FData;
    property Size: DWORD read FSize;
    property Offset: DWORD read GetOffset;
    property Rows[const Idx: TJclIndex]: TJclPeCLRTableRow read GetRow;
    property RowCount: TJclIndex read GetRowCount;
  end;

  TJclPeCLRTableModule = class(TJclPeCLRTable)
  private
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
    property NameOffset: DWord read FNameOffset;
    property MvidIdx: DWord read FMvidIdx;
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
    FPublicKeyIdx,
    FHashAlgId,
    FNameOffset: DWORD;
    FMajorVersion,
    FBuildNumber,
    FRevisionNumber,
    FMinorVersion: Word;
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
    property PublicKeyIdx: DWORD read FPublicKeyIdx;
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

  TJclPeCLRTableAssemblyRef = class(TJclPeCLRTable)
  private
    FCultureOffset,
    FNameOffset,
    FPublicKeyOrTokenIdx,
    FHashValueIdx: DWORD;
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
    procedure Load; override;
  public
    property MajorVersion: Word read FMajorVersion;
    property MinorVersion: Word read FMinorVersion;
    property BuildNumber: Word read FBuildNumber;
    property RevisionNumber: Word read FRevisionNumber;
    property Flags: DWORD read FFlags;
    property PublicKeyOrTokenIdx: DWORD read FPublicKeyOrTokenIdx;
    property NameOffset: DWORD read FNameOffset;
    property CultureOffset: DWORD read FCultureOffset;
    property HashValueIdx: DWORD read FHashValueIdx;

    property PublicKeyOrToken: TJclPeCLRBlobRecord read GetPublicKeyOrToken;
    property Name: WideString read GetName;
    property Culture: WideString read GetCulture;
    property HashValue: TJclPeCLRBlobRecord read GetHashValue;
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
  TJclPeCLRTableConstant = class(TJclPeCLRTable);
  TJclPeCLRTableCustomAttribute = class(TJclPeCLRTable);
  TJclPeCLRTableDeclSecurity = class(TJclPeCLRTable);
  TJclPeCLRTableEventMap = class(TJclPeCLRTable);
  TJclPeCLRTableEvent = class(TJclPeCLRTable);
  TJclPeCLRTableExportedType = class(TJclPeCLRTable);

  TJclPeCLRTableFieldRow = class(TJclPeCLRTableRow)
  private
    FFlags: Word;
    FNameOffset: DWORD;
    FSignatureIdx: DWORD;
    function GetName: WideString;
    function GetSignature: TJclPeCLRBlobRecord;
  protected
    constructor Create(const ATable: TJclPeCLRTable); override;
  public
    property Flags: Word read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property SignatureIdx: DWORD read FSignatureIdx;

    property Name: WideString read GetName;
    property Signature: TJclPeCLRBlobRecord read GetSignature;
  end;

  TJclPeCLRTableField = class(TJclPeCLRTable)
  private
    function GetRow(const Idx: TJclIndex): TJclPeCLRTableFieldRow;
  protected
    class function TableRowClass: TJclPeCLRTableRowClass; override;
  public
    property Rows[const Idx: TJclIndex]: TJclPeCLRTableFieldRow read GetRow;
  end;

  TJclPeCLRTableFieldLayout = class(TJclPeCLRTable);
  TJclPeCLRTableFieldMarshal = class(TJclPeCLRTable);
  TJclPeCLRTableFieldRVA = class(TJclPeCLRTable);
  TJclPeCLRTableFile = class(TJclPeCLRTable);
  TJclPeCLRTableImplMap = class(TJclPeCLRTable);
  TJclPeCLRTableInterfaceImpl = class(TJclPeCLRTable);
  TJclPeCLRTableManifestResource = class(TJclPeCLRTable);
  TJclPeCLRTableMemberRef = class(TJclPeCLRTable);

  TJclPeCLRTableMethodDefRow = class(TJclPeCLRTableRow)
  private
    FRVA: DWORD;
    FImplFlags: Word;
    FFlags: Word;
    FNameOffset: DWORD;
    FSignatureIdx: DWORD;
    FParamListIdx: DWORD;
    function GetName: WideString;
    function GetSignature: TJclPeCLRBlobRecord;
  protected
    constructor Create(const ATable: TJclPeCLRTable); override;
  public
    property RVA: DWORD read FRVA;
    property ImplFlags: Word read FImplFlags;
    property Flags: Word read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property SignatureIdx: DWORD read FSignatureIdx;
    property ParamListIdx: DWORD read FParamListIdx;

    property Name: WideString read GetName;
    property Signature: TJclPeCLRBlobRecord read GetSignature;
  end;

  TJclPeCLRTableMethodDef = class(TJclPeCLRTable)
  private
    function GetRow(const Idx: TJclIndex): TJclPeCLRTableMethodDefRow;
  protected
    class function TableRowClass: TJclPeCLRTableRowClass; override;
  public
    property Rows[const Idx: TJclIndex]: TJclPeCLRTableMethodDefRow read GetRow;
  end;

  TJclPeCLRTableMethodImpl = class(TJclPeCLRTable);
  TJclPeCLRTableMethodSemantics = class(TJclPeCLRTable);
  TJclPeCLRTableNestedClass = class(TJclPeCLRTable);
  TJclPeCLRTableParamDef = class(TJclPeCLRTable);
  TJclPeCLRTableProperty = class(TJclPeCLRTable);
  TJclPeCLRTablePropertyMap = class(TJclPeCLRTable);
  TJclPeCLRTableStandAloneSig = class(TJclPeCLRTable);

  TJclPeCLRTableTypeDefRow = class(TJclPeCLRTableRow)
  private
    FNamespaceOffset: DWORD;
    FNameOffset: DWORD;
    FFlags: DWORD;
    FExtendsIdx: DWORD;
    FFieldListIdx: DWORD;
    FMethodListIdx: DWORD;
    function GetName: WideString;
    function GetNamespace: WideString;
  protected
    constructor Create(const ATable: TJclPeCLRTable); override;
  public
    property Flags: DWORD read FFlags;
    property NameOffset: DWORD read FNameOffset;
    property NamespaceOffset: DWORD read FNamespaceOffset;
    property ExtendsIdx: DWORD read FExtendsIdx;
    property FieldListIdx: DWORD read FFieldListIdx;
    property MethodListIdx: DWORD read FMethodListIdx;

    property Name: WideString read GetName;
    property Namespace: WideString read GetNamespace;
  end;

  TJclPeCLRTableTypeDef = class(TJclPeCLRTable)
  private
    function GetRow(const Idx: TJclIndex): TJclPeCLRTableTypeDefRow;
  protected
    class function TableRowClass: TJclPeCLRTableRowClass; override;
  public
    property Rows[const Idx: TJclIndex]: TJclPeCLRTableTypeDefRow read GetRow;
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
    function GetRow(const Idx: TJclIndex): TJclPeCLRTableTypeRefRow;
  protected
    class function TableRowClass: TJclPeCLRTableRowClass; override;
  public
    property Rows[const Idx: TJclIndex]: TJclPeCLRTableTypeRefRow read GetRow;
  end;

  TJclPeCLRTableTypeSpec = class(TJclPeCLRTable);
  TJclPeCLRTableStream = class(TJclPeCLRStream)
  private
    FHeader: PCLRTableStreamHeader;
    FTables: array[TJclPeCLRTableKind] of TJclPeCLRTable;
    FTableCount: TJclIndex;
    function GetVersionString: string;
    function GetTable(const AKind: TJclPeCLRTableKind): TJclPeCLRTable;
    function GetBigHeap(const AHeapKind: TJclPeCLRHeapKind): Boolean;
  protected
    constructor Create(const AMetadata: TJclPeMetadataHeader;
                       const AHeader: PCLRStreamHeader); override;
  public
    destructor Destroy; override;

    function FindTable(const AKind: TJclPeCLRTableKind;
      var ATable: TJclPeCLRTable): Boolean;

    property Header: PCLRTableStreamHeader read FHeader;

    property VersionString: string read GetVersionString;
    property BigHeap[const AHeapKind: TJclPeCLRHeapKind]: Boolean read GetBigHeap;

    property Tables[const AKind: TJclPeCLRTableKind]: TJclPeCLRTable read GetTable;
    property TableCount: TJclIndex read FTableCount;
  end;

  TJclPeMetadataHeader = class
  private
    FImage: TJclPeImage;
    FHeader: PCLRMetadataHeader;
    FStreams: TObjectList;
    FStringStream: TJclPeCLRStringsStream;
    FGuidStream: TJclPeCLRGuidStream;
    FBlobStream: TJclPeCLRBlobStream;
    function GetVersionString: WideString;
    function GetStream(const Idx: TJclIndex): TJclPeCLRStream;
    function GetStreamCount: TJclIndex;
    function GetString(const Idx: TJclIndex): WideString;
    function GetStringCount: TJclIndex;
    function GetGuid(const Idx: TJclIndex): TGUID;
    function GetGuidCount: TJclIndex;
    function GetBlob(const Idx: TJclIndex): TJclPeCLRBlobRecord;
    function GetBlobCount: TJclIndex;
  protected
    constructor Create(const AImage: TJclPeImage);
  public
    destructor Destroy; override;

    function FindStream(const AName: string; var Stream: TJclPeCLRStream): Boolean; overload;
    function FindStream(const AClass: TJclPeCLRStreamClass; var Stream: TJclPeCLRStream): Boolean; overload;

    function StringAt(const Offset: DWORD): WideString;

    property Image: TJclPeImage read FImage;
    property Header: PCLRMetadataHeader read FHeader;

    property VersionString: WideString read GetVersionString;

    property Streams[const Idx: TJclIndex]: TJclPeCLRStream read GetStream; default;
    property StreamCount: TJclIndex read GetStreamCount;

    property Strings[const Idx: TJclIndex]: WideString read GetString;
    property StringCount: TJclIndex read GetStringCount;
    property Guids[const Idx: TJclIndex]: TGUID read GetGuid;
    property GuidCount: TJclIndex read GetGuidCount;
    property Blobs[const Idx: TJclIndex]: TJclPeCLRBlobRecord read GetBlob; 
    property BlobCount: TJclIndex read GetBlobCount;
  end;

  TJclPeCLRInformation = class
  private
    FImage: TJclPeImage;
    FMetadataHeader: TJclPeMetadataHeader;
  public
    constructor Create(const AImage: TJclPeImage);
    destructor Destroy; override;

    property Image: TJclPeImage read FImage;

    property MetadataHeader: TJclPeMetadataHeader read FMetadataHeader;
  end;

implementation

uses
  Math, TypInfo, JclUnicode;

const
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

{ TODO -cDesign : Move FormatVersionString to other unit }
function FormatVersionString(HiV, LoV: Word): string;
begin
  Result := Format('%u.%.2u', [HiV, LoV]);
end;

{ TJclPeCLRStream }

constructor TJclPeCLRStream.Create(const AMetadata: TJclPeMetadataHeader;
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
  Result := Pointer(DWord(FMetadata.Header) + FHeader.Offset);
end;

{ TJclPeCLRStringsStream }

constructor TJclPeCLRStringsStream.Create(
  const AMetadata: TJclPeMetadataHeader; const AHeader: PCLRStreamHeader);
var
  pch: PChar;
  off: DWORD;
begin
  inherited;

  FStrings := TStringList.Create;
  pch      := Data;
  off      := 0;
  while off <= Size do
  begin
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

function TJclPeCLRStringsStream.GetString(const Idx: TJclIndex): WideString;
begin
  Result := UTF8ToWideString(FStrings.Strings[Idx]);
end;

function TJclPeCLRStringsStream.GetStringCount: TJclIndex;
begin
  Result := FStrings.Count;
end;

function TJclPeCLRStringsStream.At(const Offset: DWORD): WideString;
var
  Idx: Integer;
begin
  Idx := FStrings.IndexOfObject(TObject(Offset));
  if Idx <> -1 then
    Result := GetString(Idx);
end;

{ TJclPeCLRGuidStream }

constructor TJclPeCLRGuidStream.Create(
  const AMetadata: TJclPeMetadataHeader; const AHeader: PCLRStreamHeader);
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

function TJclPeCLRGuidStream.GetGuid(const Idx: TJclIndex): TGUID;
begin
  Assert(Idx < GetGuidCount);
  Result := FGuids[Idx];
end;

function TJclPeCLRGuidStream.GetGuidCount: TJclIndex;
begin
  Result := Length(FGuids);
end;

{ TJclPeCLRBlobRecord }

constructor TJclPeCLRBlobRecord.Create(const APtr: PByteArray);
var
  b: Byte;
begin
  inherited Create;

  FPtr := APtr;

  b := FPtr[0];
  if b = 0 then
  begin
    FData := @FPtr[1];
    FSize := 0;
  end
  else if ((b and $C0) = $C0) and ((b and $20) = 0) then    // 110bs
  begin
    FData := @FPtr[4];
    FSize := ((b and $1F) shl 24) + (FPtr[1] shl 16) + (FPtr[2] shl 8) + FPtr[3];
  end
  else if ((b and $80) = $80) and ((b and $40) = 0) then    // 10bs
  begin
    FData := @FPtr[2];
    FSize := ((b and $3F) shl 8) + FPtr[1];
  end
  else
  begin
    FData := @FPtr[1];
    FSize := b and $7F;
  end;

  Assert(not IsBadReadPtr(FData, FSize));
end;

{ TJclPeCLRBlobStream }

constructor TJclPeCLRBlobStream.Create(
  const AMetadata: TJclPeMetadataHeader; const AHeader: PCLRStreamHeader);
var
  ABlob: TJclPeCLRBlobRecord;
begin
  inherited;

  FBlobs := TObjectList.Create;

  ABlob := TJclPeCLRBlobRecord.Create(Data);
  while Assigned(ABlob) do
  begin
    if ABlob.Size > 0 then
      FBlobs.Add(ABlob);
    if (DWord(ABlob.Data) + ABlob.Size) < (DWord(Data) + Size) then
      ABlob := TJclPeCLRBlobRecord.Create(Pointer(DWord(ABlob.Data) + ABlob.Size))
    else
      ABlob := nil;
  end;
end;

destructor TJclPeCLRBlobStream.Destroy;
begin
  FreeAndNil(FBlobs);

  inherited;
end;

function TJclPeCLRBlobStream.GetBlob(const Idx: TJclIndex): TJclPeCLRBlobRecord;
begin
  Assert(Idx < GetBlobCount);
  Result := TJclPeCLRBlobRecord(FBlobs.Items[Idx]);
end;

function TJclPeCLRBlobStream.GetBlobCount: TJclIndex;
begin
  Result := FBlobs.Count;
end;

{ TJclPeCLRUserStringStream }

function TJclPeCLRUserStringStream.GetString(const Idx: TJclIndex): WideString;
begin
  SetLength(Result, Blobs[Idx].Size div 2 + 1);
  StrLCopyW(PWideChar(Result), PWideChar(Blobs[Idx].Data), Blobs[Idx].Size div 2);
end;

function TJclPeCLRUserStringStream.GetStringCount: TJclIndex;
begin
  Result := BlobCount;
end;

{ TJclPeCLRTableRow }

constructor TJclPeCLRTableRow.Create(const ATable: TJclPeCLRTable);
begin
  inherited Create;

  FTable := ATable;
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
  if RowCount > 1 then
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
  Result := DWord(Data) - DWord(Stream.Metadata.Image.LoadedImage.MappedAddress);
end;

function TJclPeCLRTable.GetRow(const Idx: TJclIndex): TJclPeCLRTableRow;
begin
  Result := TJclPeCLRTableRow(FRows.Items[Idx]);
end;

function TJclPeCLRTable.GetRowCount: TJclIndex;
begin
  Result := FRowCount;
end;

function TJclPeCLRTable.AddRow(const ARow: TJclPeCLRTableRow): Integer;
begin
  if not Assigned(FRows) then
    FRows := TObjectList.Create;

  Result := FRows.Add(ARow);
end;

function TJclPeCLRTable.ReadIndex(const AKind: TJclPeCLRHeapKind): DWORD;
begin
  if Stream.BigHeap[AKind] then
    Result := ReadDWord
  else
    Result := ReadWord;
end;

function TJclPeCLRTable.ReadIndex(const AKind: TJclPeCLRTableKind): DWORD;
var
  ATable: TJclPeCLRTable;
begin
  if Stream.FindTable(AKind, ATable) then
  begin
    if ATable.RowCount > MAXWORD then
      Result := ReadDWord
    else
      Result := ReadWord;
  end
  else
    Result := 0;
end;

function TJclPeCLRTable.ReadIndex(const Tables: array of TJclPeCLRTableKind): DWORD;
const
  TableIndexSize: array[Boolean] of Integer = (2, 4);
var
  I, IndexSize: Integer;
  ATable: TJclPeCLRTable;
begin
  IndexSize := 0;
  for I:=Low(Tables) to High(Tables) do
    if Stream.FindTable(Tables[I], ATable) then
      IndexSize := Max(IndexSize, TableIndexSize[ATable.RowCount > MAXWORD]);

  case IndexSize of
    2: Result := ReadWord;
    4: Result := ReadDWord;
  else
    Result := 0;
  end;
end;

function TJclPeCLRTable.ReadDWord: DWORD;
begin
  Result := PDword(FPtr)^;
  Inc(FPtr, SizeOf(DWord));
end;

function TJclPeCLRTable.ReadWord: Word;
begin
  Result := PWord(FPtr)^;
  Inc(FPtr, SizeOf(Word));
end;

class function TJclPeCLRTable.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTableRow;
end;

{ TJclPeCLRTableModule }

procedure TJclPeCLRTableModule.Load;
begin
  inherited;

  ReadWord; // Generation (2 byte value, reserved, shall be zero)
  FNameOffset   := ReadIndex(hkString); // Name (index into String heap)
  FMvidIdx      := ReadIndex(hkGuid);   // Mvid (index into Guid heap)
  FEncIdIdx     := ReadIndex(hkGuid);   // Mvid (index into Guid heap)
  FEncBaseIdIdx := ReadIndex(hkGuid);   // Mvid (index into Guid heap)
end;

function TJclPeCLRTableModule.GetName: WideString;
begin
  Result := Stream.Metadata.StringAt(FNameOffset);
end;

function TJclPeCLRTableModule.GetMvid: TGUID;
begin
  Result := Stream.Metadata.Guids[FMvidIdx-1];
end;

function TJclPeCLRTableModule.GetEncId: TGUID;
begin
  if FEncIdIdx > 0 then
    Result := Stream.Metadata.Guids[FEncIdIdx-1]
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TJclPeCLRTableModule.GetEncBaseId: TGUID;
begin
  if FEncBaseIdIdx > 0 then
    Result := Stream.Metadata.Guids[FEncBaseIdIdx-1]
  else
    FillChar(Result, SizeOf(Result), 0);
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

  FPublicKeyIdx   := ReadIndex(hkBlob);
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
  Result := Stream.Metadata.Blobs[FPublicKeyIdx];
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

{ TJclPeCLRTableAssemblyRef }

procedure TJclPeCLRTableAssemblyRef.Load;
begin
  inherited;

  FMajorVersion        := ReadWord;
  FMinorVersion        := ReadWord;
  FBuildNumber         := ReadWord;
  FRevisionNumber      := ReadWord;

  FFlags               := ReadDWord;

  FPublicKeyOrTokenIdx := ReadIndex(hkBlob);
  FNameOffset          := ReadIndex(hkString);
  FCultureOffset       := ReadIndex(hkString);
  FHashValueIdx        := ReadIndex(hkBlob);
end;

function TJclPeCLRTableAssemblyRef.GetCulture: WideString;
begin
  Result := Stream.Metadata.StringAt(FCultureOffset);
end;

function TJclPeCLRTableAssemblyRef.GetHashValue: TJclPeCLRBlobRecord;
begin
  Result := Stream.Metadata.Blobs[FHashValueIdx];
end;

function TJclPeCLRTableAssemblyRef.GetName: WideString;
begin
  Result := Stream.Metadata.StringAt(FNameOffset);
end;

function TJclPeCLRTableAssemblyRef.GetPublicKeyOrToken: TJclPeCLRBlobRecord;
begin
  Result := Stream.Metadata.Blobs[FPublicKeyOrTokenIdx];
end;

{ TJclPeCLRTableAssemblyRefOS }

procedure TJclPeCLRTableAssemblyRefOS.Load;
begin
  inherited;

  FAssemblyRefIdx := ReadIndex(ttAssemblyRef);
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

  FAssemblyRefIdx := ReadIndex(ttAssemblyRef);
end;

function TJclPeCLRTableAssemblyRefProcessor.GetAssemblyRef: TJclPeCLRTableAssemblyRef;
begin
  { TODO : Implement GetAssemblyRef }
  Result := nil;
end;

{ TJclPeCLRTableFieldRow }

constructor TJclPeCLRTableFieldRow.Create(const ATable: TJclPeCLRTable);
begin
  inherited;

  FFlags        := Table.ReadWord;
  FNameOffset   := Table.ReadIndex(hkString);
  FSignatureIdx := Table.ReadIndex(hkBlob);
end;

function TJclPeCLRTableFieldRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclPeCLRTableFieldRow.GetSignature: TJclPeCLRBlobRecord;
begin
  Result := Table.Stream.Metadata.Blobs[FSignatureIdx];
end;

{ TJclPeCLRTableField }

function TJclPeCLRTableField.GetRow(const Idx: TJclIndex): TJclPeCLRTableFieldRow;
begin
  Result := TJclPeCLRTableFieldRow(inherited GetRow(Idx));
end;

class function TJclPeCLRTableField.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTableFieldRow;
end;

{ TJclPeCLRTableMethodDefRow }

constructor TJclPeCLRTableMethodDefRow.Create(const ATable: TJclPeCLRTable);
begin
  inherited;

  FRVA          := Table.ReadDWord;
  FImplFlags    := Table.ReadWord;
  FFlags        := Table.ReadWord;
  FNameOffset   := Table.ReadIndex(hkString);
  FSignatureIdx := Table.ReadIndex(hkBlob);
  FParamListIdx := Table.ReadIndex(ttParamDef);
end;

function TJclPeCLRTableMethodDefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclPeCLRTableMethodDefRow.GetSignature: TJclPeCLRBlobRecord;
begin
  Result := Table.Stream.Metadata.Blobs[FSignatureIdx];
end;

{ TJclPeCLRTableMethodDef }

function TJclPeCLRTableMethodDef.GetRow(const Idx: TJclIndex): TJclPeCLRTableMethodDefRow;
begin
  Result := TJclPeCLRTableMethodDefRow(inherited GetRow(Idx));
end;

class function TJclPeCLRTableMethodDef.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTableMethodDefRow;
end;

{ TJclPeCLRTableTypeDefRow }

constructor TJclPeCLRTableTypeDefRow.Create(const ATable: TJclPeCLRTable);
begin
  inherited;

  { TODO : Complete TypeDef }
  FFlags           := Table.ReadWord;
  FNameOffset      := Table.ReadIndex(hkString);
  FNamespaceOffset := Table.ReadIndex(hkString);
  FExtendsIdx      := Table.ReadIndex([ttTypeDef, ttTypeRef, ttTypeSpec]);
  FFieldListIdx    := Table.ReadIndex(ttFieldDef);
  FMethodListIdx   := Table.ReadIndex(ttMethodDef);
end;

function TJclPeCLRTableTypeDefRow.GetName: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNameOffset);
end;

function TJclPeCLRTableTypeDefRow.GetNamespace: WideString;
begin
  Result := Table.Stream.Metadata.StringAt(FNamespaceOffset);
end;

{ TJclPeCLRTableTypeDef }

class function TJclPeCLRTableTypeDef.TableRowClass: TJclPeCLRTableRowClass;
begin
  Result := TJclPeCLRTableTypeDefRow;
end;

function TJclPeCLRTableTypeDef.GetRow(const Idx: TJclIndex): TJclPeCLRTableTypeDefRow;
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

function TJclPeCLRTableTypeRef.GetRow(const Idx: TJclIndex): TJclPeCLRTableTypeRefRow;
begin
  Result := TJclPeCLRTableTypeRefRow(inherited GetRow(Idx));
end;

{ TJclPeCLRTableStream }

constructor TJclPeCLRTableStream.Create(const AMetadata: TJclPeMetadataHeader;
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
        pTable := Pointer(DWord(pTable) + FTables[AKind].Size);
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

{ TJclPeMetadataHeader }

constructor TJclPeMetadataHeader.Create(const AImage: TJclPeImage);

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
  begin
    pStreamPart := PStreamPartitionHeader(DWord(@Header.Version[0]) + Header.Length);
    pStream     := @pStreamPart.StreamHeaders[0];
    for I:=0 to pStreamPart.StreamCount-1 do
    begin
      FStreams.Add(GetStreamClass(pStream.Name).Create(Self, pStream));

      pStream := PCLRStreamHeader(DWord(@pStream.Name[0]) +
                 (((StrLen(@pStream.Name[0])+1)+3) and (not $3)));
    end;
  end;
begin
  Assert(AImage.CLRHeader.HasMetadata);

  inherited Create;

  FImage := AImage;

  with Image.CLRHeader.Header.MetaData do
  begin
    Assert(Size > SizeOf(FHeader^));
    FHeader  := Image.RvaToVa(VirtualAddress);
    Assert(not IsBadReadPtr(FHeader, Size));
  end;

  FStreams := TObjectList.Create;
  UpdateStreams;

  FStringStream := nil;
  FGuidStream   := nil;
  FBlobStream   := nil;
end;

destructor TJclPeMetadataHeader.Destroy;
begin
  FreeAndNil(FStreams);

  inherited;
end;

function TJclPeMetadataHeader.GetVersionString: WideString;
var
  VerStr: string;
begin
  SetLength(VerStr, Header.Length);
  StrlCopy(PChar(VerStr), @Header.Version[0], Header.Length);
  Result := UTF8ToWideString(VerStr)
end;

function TJclPeMetadataHeader.GetStream(const Idx: TJclIndex): TJclPeCLRStream;
begin
  Assert(Idx < GetStreamCount);
  Result := TJclPeCLRStream(FStreams.Items[Idx]);
end;

function TJclPeMetadataHeader.GetStreamCount: TJclIndex;
begin
  Result := FStreams.Count;
end;

function TJclPeMetadataHeader.FindStream(const AName: string;
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

function TJclPeMetadataHeader.FindStream(const AClass: TJclPeCLRStreamClass;
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

function TJclPeMetadataHeader.GetString(const Idx: TJclIndex): WideString;
begin
  if Assigned(FStringStream) or
     FindStream(TJclPeCLRStringsStream, TJclPeCLRStream(FStringStream)) then
    Result := FStringStream.Strings[Idx];
end;

function TJclPeMetadataHeader.GetStringCount: TJclIndex;
begin
  if Assigned(FStringStream) or
     FindStream(TJclPeCLRStringsStream, TJclPeCLRStream(FStringStream)) then
    Result := FStringStream.StringCount
  else
    Result := 0;
end;

function TJclPeMetadataHeader.StringAt(const Offset: DWORD): WideString;
begin
  if Assigned(FStringStream) or
     FindStream(TJclPeCLRStringsStream, TJclPeCLRStream(FStringStream)) then
    Result := TJclPeCLRStringsStream(FStringStream).At(Offset);
end;

function TJclPeMetadataHeader.GetGuid(const Idx: TJclIndex): TGUID;
begin
  if Assigned(FGuidStream) or
     FindStream(TJclPeCLRGuidStream, TJclPeCLRStream(FGuidStream)) then
    Result := FGuidStream.Guids[Idx];
end;

function TJclPeMetadataHeader.GetGuidCount: TJclIndex;
begin
  if Assigned(FGuidStream) or
     FindStream(TJclPeCLRGuidStream, TJclPeCLRStream(FGuidStream)) then
    Result := FGuidStream.GuidCount
  else
    Result := 0;
end;

function TJclPeMetadataHeader.GetBlob(const Idx: TJclIndex): TJclPeCLRBlobRecord;
begin
  if Assigned(FBlobStream) or
     FindStream(TJclPeCLRBlobStream, TJclPeCLRStream(FBlobStream)) then
    Result := FBlobStream.Blobs[Idx]
  else
    Result := nil;
end;

function TJclPeMetadataHeader.GetBlobCount: TJclIndex;
begin
  if Assigned(FBlobStream) or
     FindStream(TJclPeCLRBlobStream, TJclPeCLRStream(FBlobStream)) then
    Result := FBlobStream.BlobCount
  else
    Result := 0;
end;

{ TJclPeCLRInformation }

constructor TJclPeCLRInformation.Create(const AImage: TJclPeImage);
begin
  Assert(Assigned(AImage));
  Assert(AImage.IsCLR);

  inherited Create;

  FImage := AImage;

  FMetadataHeader := TJclPeMetadataHeader.Create(Image);
end;

destructor TJclPeCLRInformation.Destroy;
begin
  FreeAndNil(FMetadataHeader);

  inherited;
end;

end.
