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
  TMetadataHeaderSignature = $424A5342;

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
    ttModule,           //  $00
    ttTypeRef,          //  $01
    ttTypeDef,          //  $02
    ttUnknown03,        //  $03
    ttFieldDef,         //  $04
    ttUnknown05,        //  $05
    ttMethodDef,        //  $06
    ttUnknown07,        //  $07
    ttParamDef,         //  $08
    ttInterfaceImpl,    //  $09
    ttMemberRef,        //  $0a
    ttConstant,         //  $0b
    ttCustomAttribute,  //  $0c
    ttFieldMarshal,     //  $0d
    ttPermission,       //  $0e DeclSecurity
    ttClassLayout,      //  $0f
    ttFieldLayout,      //  $10
    ttSignature,        //  $11
    ttEventMap,         //  $12
    ttUnknown13,        //  $13
    ttEvent,            //  $14
    ttPropertyMap,      //  $15
    ttUnknown16,        //  $16
    ttProperty,         //  $17
    ttMethodSemantics,  //  $18
    ttMethodImpl,       //  $19
    ttModuleRef,        //  $1a
    ttTypeSpec,         //  $1b
    ttImplMap,          //  $1c
    ttFieldRVA,         //  $1d
    ttUnknown1e,        //  $1e
    ttUnknown1f,        //  $1f
    ttAssembly,         //  $20
    ttAssemblyProcessor,//  $21
    ttAssemblyOS,       //  $22
    ttAssemblyRef,      //  $23
    ttUnknown24,        //  $24
    ttUnknown25,        //  $25
    ttFile,             //  $26
    ttExportedType,     //  $27
    ttManifestResource, //  $28
    ttNestedClass,      //  $29
    ttUnknown2a,        //  $2a
    ttUnknown2b);       //  $2b

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

  TJclPeCLRTableClass = class of TJclPeCLRTable;
  TJclPeCLRTable = class
  private
    FStream: TJclPeCLRTableStream;
    FData: PChar;
    FRowCount: Integer;
    FSize: DWORD;
    function GetOffset: DWORD;
  protected
    constructor Create(const AStream: TJclPeCLRTableStream;
      const Ptr: Pointer; const ARowCount: Integer); virtual;

    procedure SetSize(const Value: Integer);

    function GetRow(const Idx: TJclIndex): Pointer; virtual;
    function GetRowCount: TJclIndex; virtual;

    function GetHeapIndex(const AHeap: TJclPeCLRHeapKind; var pData: PByte): DWord;
    function GetDWord(var pData: PByte): DWORD;
    function GetWord(var pData: PByte): DWord;
  public
    property Stream: TJclPeCLRTableStream read FStream;

    property Data: PChar read FData;
    property Size: DWORD read FSize;
    property Offset: DWORD read GetOffset;
    property Rows[const Idx: TJclIndex]: Pointer read GetRow;
    property RowCount: TJclIndex read GetRowCount;
  end;

  TJclPeCLRTableModule = class(TJclPeCLRTable)
  private
    FNameIdx: DWord;
    FMvidIdx: DWord;
    FEncIdIdx: DWORD;
    FEncBaseIdIdx: DWORD;
    function GetMvid: TGUID;
    function GetName: WideString;
    function GetEncBaseId: TGUID;
    function GetEncId: TGUID;
  protected
    constructor Create(const AStream: TJclPeCLRTableStream;
      const Ptr: Pointer; const ARowCount: Integer); override;
  public
    property NameIdx: DWord read FNameIdx;
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
    FCultureIdx: DWORD;
    FPublicKeyIdx: DWORD;
    FHashAlgId: DWORD;
    FNameIdx: DWORD;
    FMajorVersion: Word;
    FBuildNumber: Word;
    FRevisionNumber: Word;
    FMinorVersion: Word;
    function GetCulture: WideString;
    function GetName: WideString;
    function GetPublicKey: TJclPeCLRBlobRecord;
  protected
    constructor Create(const AStream: TJclPeCLRTableStream;
      const Ptr: Pointer; const ARowCount: Integer); override;
  public
    property HashAlgId: DWORD read FHashAlgId;
    property MajorVersion: Word read FMajorVersion;
    property MinorVersion: Word read FMinorVersion;
    property BuildNumber: Word read FBuildNumber;
    property RevisionNumber: Word read FRevisionNumber;
    property PublicKeyIdx: DWORD read FPublicKeyIdx;
    property NameIdx: DWORD read FNameIdx;
    property CultureIdx: DWORD read FCultureIdx;

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
    constructor Create(const AStream: TJclPeCLRTableStream;
      const Ptr: Pointer; const ARowCount: Integer); override;
  public
    property OSPlatformID: DWORD read FOSPlatformID;
    property OSMajorVersion: DWORD read FOSMajorVersion;
    property OSMinorVersion: DWORD read FOSMinorVersion;
  end;

  TJclPeCLRTableAssemblyProcessor = class(TJclPeCLRTable)
  private
    FProcessor: DWORD;
  protected
    constructor Create(const AStream: TJclPeCLRTableStream;
      const Ptr: Pointer; const ARowCount: Integer); override;
  public
    property Processor: DWORD read FProcessor;
  end;

  TJclPeCLRTableAssemblyRef = class(TJclPeCLRTable)
  private
    FCultureIdx: DWORD;
    FNameIdx: DWORD;
    FPublicKeyOrTokenIdx: DWORD;
    FHashValueIdx: DWORD;
    FMajorVersion: Word;
    FRevisionNumber: Word;
    FBuildNumber: Word;
    FMinorVersion: Word;
    FFlags: DWORD;
    function GetCulture: WideString;
    function GetHashValue: TJclPeCLRBlobRecord;
    function GetName: WideString;
    function GetPublicKeyOrToken: TJclPeCLRBlobRecord;
  protected
    constructor Create(const AStream: TJclPeCLRTableStream;
      const Ptr: Pointer; const ARowCount: Integer); override;
  public
    property MajorVersion: Word read FMajorVersion;
    property MinorVersion: Word read FMinorVersion;
    property BuildNumber: Word read FBuildNumber;
    property RevisionNumber: Word read FRevisionNumber;
    property Flags: DWORD read FFlags;
    property PublicKeyOrTokenIdx: DWORD read FPublicKeyOrTokenIdx;
    property NameIdx: DWORD read FNameIdx;
    property CultureIdx: DWORD read FCultureIdx;
    property HashValueIdx: DWORD read FHashValueIdx;

    property PublicKeyOrToken: TJclPeCLRBlobRecord read GetPublicKeyOrToken;
    property Name: WideString read GetName;
    property Culture: WideString read GetCulture;
    property HashValue: TJclPeCLRBlobRecord read GetHashValue;
  end;

  TJclPeCLRTableAssemblyRefOS = class(TJclPeCLRTable)
  private
    FOSPlatformID,
    FOSMajorVersion,
    FOSMinorVersion: DWORD;
    FAssemblyRefIdx: DWORD;
    function GetAssemblyRef: TJclPeCLRTableAssemblyRef;
  protected
    constructor Create(const AStream: TJclPeCLRTableStream;
      const Ptr: Pointer; const ARowCount: Integer); override;
  public
    property OSPlatformID: DWORD read FOSPlatformID;
    property OSMajorVersion: DWORD read FOSMajorVersion;
    property OSMinorVersion: DWORD read FOSMinorVersion;
    property AssemblyRefIdx: DWORD read FAssemblyRefIdx;

    property AssemblyRef: TJclPeCLRTableAssemblyRef read GetAssemblyRef;
  end;

  TJclPeCLRTableAssemblyRefProcessor = class(TJclPeCLRTable)
  private
    FProcessor: DWORD;
    FAssemblyRefIdx: DWORD;
    function GetAssemblyRef: TJclPeCLRTableAssemblyRef;
  protected
    constructor Create(const AStream: TJclPeCLRTableStream;
      const Ptr: Pointer; const ARowCount: Integer); override;
  public
    property Processor: DWORD read FProcessor;
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
  TJclPeCLRTableField = class(TJclPeCLRTable);
  TJclPeCLRTableFieldLayout = class(TJclPeCLRTable);
  TJclPeCLRTableFieldMarshal = class(TJclPeCLRTable);
  TJclPeCLRTableFieldRVA = class(TJclPeCLRTable);
  TJclPeCLRTableFile = class(TJclPeCLRTable);
  TJclPeCLRTableImplMap = class(TJclPeCLRTable);
  TJclPeCLRTableInterfaceImpl = class(TJclPeCLRTable);
  TJclPeCLRTableManifestResource = class(TJclPeCLRTable);
  TJclPeCLRTableMemberRef = class(TJclPeCLRTable);
  TJclPeCLRTableMethod = class(TJclPeCLRTable);
  TJclPeCLRTableMethodImpl = class(TJclPeCLRTable);
  TJclPeCLRTableMethodSemantics = class(TJclPeCLRTable);
  TJclPeCLRTableNestedClass = class(TJclPeCLRTable);
  TJclPeCLRTableParam = class(TJclPeCLRTable);
  TJclPeCLRTableProperty = class(TJclPeCLRTable);
  TJclPeCLRTablePropertyMap = class(TJclPeCLRTable);
  TJclPeCLRTableStandAloneSig = class(TJclPeCLRTable);
  TJclPeCLRTableTypeDef = class(TJclPeCLRTable);
  TJclPeCLRTableTypeRef = class(TJclPeCLRTable);
  TJclPeCLRTableTypeSpec = class(TJclPeCLRTable);

  TJclPeCLRTableStream = class(TJclPeCLRStream)
  private
    FHeader: PCLRTableStreamHeader;
    FBitHeap: array[TJclPeCLRHeapKind] of Boolean;
    FTables: TObjectList;
    function GetVersionString: string;
    function GetTable(const Idx: TJclIndex): TJclPeCLRTable;
    function GetTableCount: TJclIndex;
    function GetBigHeap(const AHeapKind: TJclPeCLRHeapKind): Boolean;
  protected
    constructor Create(const AMetadata: TJclPeMetadataHeader;
                       const AHeader: PCLRStreamHeader); override;
  public
    destructor Destroy; override;

    property Header: PCLRTableStreamHeader read FHeader;

    property VersionString: string read GetVersionString;
    property BigHeap[const AHeapKind: TJclPeCLRHeapKind]: Boolean read GetBigHeap;

    property Tables[const Idx: TJclIndex]: TJclPeCLRTable read GetTable;
    property TableCount: TJclIndex read GetTableCount;
  end;

  TJclPeMetadataHeader = class
  private
    FImage: TJclPeImage;
    FHeader: PCLRMetadataHeader;
    FStreams: TObjectList;
    FStringStream,
    FGuidStream,
    FBlobStream: TJclPeCLRStream;
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
  JclUnicode;

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
begin
  inherited;

  FStrings := TStringList.Create;
  pch      := Data;
  while DWord(pch - Data) <= Size do
  begin
    FStrings.Add(pch);
    pch := pch + StrLen(pch) + 1;
  end;
end;

destructor TJclPeCLRStringsStream.Destroy;
begin
  FreeAndNil(FStrings);

  inherited;
end;

function TJclPeCLRStringsStream.GetString(const Idx: TJclIndex): WideString;
begin
  Assert(Idx < GetStringCount);

  Result := UTF8ToWideString(FStrings.Strings[Idx]);
end;

function TJclPeCLRStringsStream.GetStringCount: TJclIndex;
begin
  Result := FStrings.Count;
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

constructor TJclPeCLRTable.Create(const AStream: TJclPeCLRTableStream;
  const Ptr: Pointer; const ARowCount: Integer);
begin
  inherited Create;

  FStream   := AStream;
  FData     := Ptr;
  FSize     := 0;
  FRowCount := ARowCount;
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

function TJclPeCLRTable.GetHeapIndex(const AHeap: TJclPeCLRHeapKind;
  var pData: PByte): DWORD;
begin
  if Stream.BigHeap[AHeap] then
  begin
    Result := PDWord(pData)^;
    Inc(pData, SizeOf(DWord));
  end
  else
  begin
    Result := PWord(pData)^;
    Inc(pData, SizeOf(Word));
  end;
end;

function TJclPeCLRTable.GetDWord(var pData: PByte): DWORD;
begin
  Result := PDword(pData)^;
  Inc(pData, SizeOf(DWord));
end;

function TJclPeCLRTable.GetWord(var pData: PByte): DWord;
begin
  Result := PWord(pData)^;
  Inc(pData, SizeOf(Word));
end;

function TJclPeCLRTable.GetRow(const Idx: TJclIndex): Pointer;
begin
  Result := nil;
end;

function TJclPeCLRTable.GetRowCount: TJclIndex;
begin
  Result := FRowCount;
end;

{ TJclPeCLRTableModule }

constructor TJclPeCLRTableModule.Create(
  const AStream: TJclPeCLRTableStream; const Ptr: Pointer;
  const ARowCount: Integer);
var
  pData: PByte;
begin
  inherited;

  pData := Ptr;

  GetWord(pData); // Generation (2 byte value, reserved, shall be zero)
  FNameIdx      := GetHeapIndex(hkString, pData); // Name (index into String heap)
  FMvidIdx      := GetHeapIndex(hkGuid, pData);   // Mvid (index into Guid heap)
  FEncIdIdx     := GetHeapIndex(hkGuid, pData);   // Mvid (index into Guid heap)
  FEncBaseIdIdx := GetHeapIndex(hkGuid, pData);   // Mvid (index into Guid heap)

  SetSize(DWord(pData) - DWord(Ptr));
end;

function TJclPeCLRTableModule.GetName: WideString;
begin
  Result := Stream.Metadata.Strings[FNameIdx-1];
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

constructor TJclPeCLRTableAssembly.Create(const AStream: TJclPeCLRTableStream;
  const Ptr: Pointer; const ARowCount: Integer);
var
  pData: PByte;
begin
  inherited;

  pData := Ptr;

  FHashAlgId      := GetDWord(pData);
  FMajorVersion   := GetWord(pData);
  FMinorVersion   := GetWord(pData);
  FBuildNumber    := GetWord(pData);
  FRevisionNumber := GetWord(pData);

  FPublicKeyIdx   := GetHeapIndex(hkBlob, pData);
  FNameIdx        := GetHeapIndex(hkString, pData);
  FCultureIdx     := GetHeapIndex(hkString, pData);

  SetSize(DWord(pData) - DWord(Ptr));
end;

function TJclPeCLRTableAssembly.GetCulture: WideString;
begin
  Result := Stream.Metadata.Strings[FCultureIdx-1];
end;

function TJclPeCLRTableAssembly.GetName: WideString;
begin
  Result := Stream.Metadata.Strings[FNameIdx-1];
end;

function TJclPeCLRTableAssembly.GetPublicKey: TJclPeCLRBlobRecord;
begin
  Result := Stream.Metadata.Blobs[FPublicKeyIdx];
end;

{ TJclPeCLRTableAssemblyOS }

constructor TJclPeCLRTableAssemblyOS.Create(const AStream: TJclPeCLRTableStream;
  const Ptr: Pointer; const ARowCount: Integer);
var
  pData: PByte;
begin
  inherited;

  pData := Ptr;

  FOSPlatformID   := GetDWord(pData);
  FOSMajorVersion := GetDWord(pData);
  FOSMinorVersion := GetDWord(pData);

  SetSize(DWord(pData) - DWord(Ptr));
end;

{ TJclPeCLRTableAssemblyProcessor }

constructor TJclPeCLRTableAssemblyProcessor.Create(const AStream: TJclPeCLRTableStream;
  const Ptr: Pointer; const ARowCount: Integer);
var
  pData: PByte;
begin
  inherited;

  pData := Ptr;

  FProcessor := GetDWord(pData);

  SetSize(DWord(pData) - DWord(Ptr));
end;

{ TJclPeCLRTableAssemblyRef }

constructor TJclPeCLRTableAssemblyRef.Create(
  const AStream: TJclPeCLRTableStream; const Ptr: Pointer;
  const ARowCount: Integer);
var
  pData: PByte;
begin
  inherited;

  pData := Ptr;

  FMajorVersion        := GetWord(pData);
  FMinorVersion        := GetWord(pData);
  FBuildNumber         := GetWord(pData);
  FRevisionNumber      := GetWord(pData);

  FFlags               := GetDWord(pData);

  FPublicKeyOrTokenIdx := GetHeapIndex(hkBlob, pData);
  FNameIdx             := GetHeapIndex(hkString, pData);
  FCultureIdx          := GetHeapIndex(hkString, pData);
  FHashValueIdx        := GetHeapIndex(hkBlob, pData);

  SetSize(DWord(pData) - DWord(Ptr));
end;

function TJclPeCLRTableAssemblyRef.GetCulture: WideString;
begin
  Result := Stream.Metadata.Strings[FCultureIdx-1];
end;

function TJclPeCLRTableAssemblyRef.GetHashValue: TJclPeCLRBlobRecord;
begin
  Result := Stream.Metadata.Blobs[FHashValueIdx];
end;

function TJclPeCLRTableAssemblyRef.GetName: WideString;
begin
  Result := Stream.Metadata.Strings[FNameIdx-1];
end;

function TJclPeCLRTableAssemblyRef.GetPublicKeyOrToken: TJclPeCLRBlobRecord;
begin
  Result := Stream.Metadata.Blobs[FPublicKeyOrTokenIdx];
end;

{ TJclPeCLRTableAssemblyRefOS }

constructor TJclPeCLRTableAssemblyRefOS.Create(const AStream: TJclPeCLRTableStream;
  const Ptr: Pointer; const ARowCount: Integer);
var
  pData: PByte;
begin
  inherited;

  pData := Ptr;

  FOSPlatformID   := GetDWord(pData);
  FOSMajorVersion := GetDWord(pData);
  FOSMinorVersion := GetDWord(pData);
  FAssemblyRefIdx := GetHeapIndex(hkBlob, pData);

  SetSize(DWord(pData) - DWord(Ptr));
end;

function TJclPeCLRTableAssemblyRefOS.GetAssemblyRef: TJclPeCLRTableAssemblyRef;
begin
  Result := nil;
end;

{ TJclPeCLRTableAssemblyRefProcessor }

constructor TJclPeCLRTableAssemblyRefProcessor.Create(const AStream: TJclPeCLRTableStream;
  const Ptr: Pointer; const ARowCount: Integer);
var
  pData: PByte;
begin
  inherited;

  pData := Ptr;

  FProcessor := GetDWord(pData);
  FAssemblyRefIdx := GetHeapIndex(hkBlob, pData);

  SetSize(DWord(pData) - DWord(Ptr));
end;

function TJclPeCLRTableAssemblyRefProcessor.GetAssemblyRef: TJclPeCLRTableAssemblyRef;
begin
  Result := nil;
end;

{ TJclPeCLRTableStream }

constructor TJclPeCLRTableStream.Create(const AMetadata: TJclPeMetadataHeader;
  const AHeader: PCLRStreamHeader);

  procedure EnumHeapSize;
  begin
    FBitHeap[hkString] := (Header.HeapSizes and $01) <> 0;
    FBitHeap[hkGuid]   := (Header.HeapSizes and $02) <> 0;
    FBitHeap[hkBlob]   := (Header.HeapSizes and $04) <> 0;
  end;

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
  const
    ValidTableMapping: array[TJclPeCLRTableKind] of TJclPeCLRTableClass = (
      TJclPeCLRTableModule,               //  $00
      TJclPeCLRTableTypeRef,              //  $01
      TJclPeCLRTableTypeDef,              //  $02
      TJclPeCLRTable,                     //  $03
      TJclPeCLRTableField,                //  $04
      TJclPeCLRTable,                     //  $05
      TJclPeCLRTableMethod,               //  $06
      TJclPeCLRTable,                     //  $07
      TJclPeCLRTableParam,                //  $08
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
      TJclPeCLRTable,                     //  $2a
      TJclPeCLRTable);                    //  $2b
  var
    AKind: TJclPeCLRTableKind;
    TableCount: Integer;
    pTable: Pointer;
    ATable: TJclPeCLRTable;
  begin
    TableCount := BitCount(Header.Valid);
    pTable     := @Header.Rows[TableCount];
    for AKind:=Low(TJclPeCLRTableKind) to High(TJclPeCLRTableKind) do
    begin
      if (Header.Valid and (Int64(1) shl Integer(AKind))) <> 0 then
      begin
        ATable := ValidTableMapping[AKind].Create(Self, pTable, Header.Rows[FTables.Count]);
        FTables.Add(ATable);
        pTable := Pointer(DWord(pTable) + ATable.Size);
      end;
    end;
  end;
begin
  inherited;

  FHeader := Data;
  FTables := TObjectList.Create;

  EnumHeapSize;
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

function TJclPeCLRTableStream.GetTable(const Idx: TJclIndex): TJclPeCLRTable;
begin
  Assert(Idx < GetTableCount);
  Result := TJclPeCLRTable(FTables.Items[Idx]);
end;

function TJclPeCLRTableStream.GetTableCount: TJclIndex;
begin
  Result := FTables.Count;
end;

function TJclPeCLRTableStream.GetBigHeap(const AHeapKind: TJclPeCLRHeapKind): Boolean;
begin
  Result := FBitHeap[AHeapKind];
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
     FindStream(TJclPeCLRStringsStream, FStringStream) then
    Result := TJclPeCLRStringsStream(FStringStream).Strings[Idx];
end;

function TJclPeMetadataHeader.GetStringCount: TJclIndex;
begin
  if Assigned(FStringStream) or
     FindStream(TJclPeCLRStringsStream, FStringStream) then
    Result := TJclPeCLRStringsStream(FStringStream).StringCount
  else
    Result := 0;
end;

function TJclPeMetadataHeader.GetGuid(const Idx: TJclIndex): TGUID;
begin
  if Assigned(FGuidStream) or
     FindStream(TJclPeCLRGuidStream, FGuidStream) then
    Result := TJclPeCLRGuidStream(FGuidStream).Guids[Idx];
end;

function TJclPeMetadataHeader.GetGuidCount: TJclIndex;
begin
  if Assigned(FGuidStream) or
     FindStream(TJclPeCLRGuidStream, FGuidStream) then
    Result := TJclPeCLRGuidStream(FGuidStream).GuidCount
  else
    Result := 0;
end;

function TJclPeMetadataHeader.GetBlob(const Idx: TJclIndex): TJclPeCLRBlobRecord;
begin
  if Assigned(FBlobStream) or
     FindStream(TJclPeCLRBlobStream, FBlobStream) then
    Result := TJclPeCLRBlobStream(FBlobStream).Blobs[Idx]
  else
    Result := nil;
end;

function TJclPeMetadataHeader.GetBlobCount: TJclIndex;
begin
  if Assigned(FBlobStream) or
     FindStream(TJclPeCLRBlobStream, FBlobStream) then
    Result := TJclPeCLRBlobStream(FBlobStream).BlobCount
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
