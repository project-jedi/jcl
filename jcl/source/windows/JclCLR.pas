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
  protected
    constructor Create(const AStream: TJclPeCLRTableStream;
      const Ptr: Pointer; const RowCount: Integer);
  public
    property Stream: TJclPeCLRTableStream read FStream;
  end;

  TJclPeCLRTableStream = class(TJclPeCLRStream)
  private
    FHeader: PCLRTableStreamHeader;
    FBitHeap: array[TJclPeCLRHeapKind] of Boolean;
    FRows: TObjectList;
    function GetVersionString: string;
    function GetRow(const Idx: TJclIndex): TJclPeCLRTable;
    function GetRowCount: TJclIndex;
    function GetBigHeap(const AHeapKind: TJclPeCLRHeapKind): Boolean;
  protected
    constructor Create(const AMetadata: TJclPeMetadataHeader;
                       const AHeader: PCLRStreamHeader); override;
  public
    destructor Destroy; override;

    property Header: PCLRTableStreamHeader read FHeader;

    property VersionString: string read GetVersionString;
    property BigHeap[const AHeapKind: TJclPeCLRHeapKind]: Boolean read GetBigHeap;
    property Rows[const Idx: TJclIndex]: TJclPeCLRTable read GetRow;
    property RowCount: TJclIndex read GetRowCount;
  end;

  TJclPeMetadataHeader = class
  private
    FImage: TJclPeImage;
    FHeader: PCLRMetadataHeader;
    FStreams: TObjectList;
    function GetVersionString: WideString;
    function GetStream(const Idx: TJclIndex): TJclPeCLRStream;
    function GetStreamCount: TJclIndex;
  protected
    constructor Create(const AImage: TJclPeImage);
  public
    destructor Destroy; override;

    property Image: TJclPeImage read FImage;
    property Header: PCLRMetadataHeader read FHeader;

    property VersionString: WideString read GetVersionString;

    property Streams[const Idx: TJclIndex]: TJclPeCLRStream read GetStream;
    property StreamCount: TJclIndex read GetStreamCount;
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
    if pch^ <> #0 then
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
  else if ((b and $C0) = $C0) and ((b and $20) = 0) then         // 110bs
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

{ TJclPeCLRTableRow }

constructor TJclPeCLRTable.Create(const AStream: TJclPeCLRTableStream;
  const Ptr: Pointer; const RowCount: Integer);
begin
  inherited Create;

  FStream := AStream;
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
    I: Integer;
  begin
    Result := 0;
    for I:=0 to SizeOf(Value)*8-1 do
      if (Value and (1 shl I)) <> 0 then
        Inc(Result);
  end;

  procedure EnumTables;
  var
    I, TableCount: Integer;
    pTable: Pointer;
  begin
    TableCount := BitCount(Header.Valid);
    pTable := @Header.Rows[TableCount];
    for I:=0 to TableCount-1 do
    begin
      FRows.Add(TJclPeCLRTable.Create(Self, pTable, Header.Rows[I]));
      { TODO : Inc the pTable to point to next table header }
    end;
  end;
begin
  inherited;

  FHeader := Data;
  FRows   := TObjectList.Create;

  EnumHeapSize;
  EnumTables;
end;

destructor TJclPeCLRTableStream.Destroy;
begin
  FreeAndNil(FRows);

  inherited;
end;

function TJclPeCLRTableStream.GetVersionString: string;
begin
  Result := FormatVersionString(Header.MajorVersion, Header.MinorVersion);
end;

function TJclPeCLRTableStream.GetRow(const Idx: TJclIndex): TJclPeCLRTable;
begin
  Assert(Idx < GetRowCount);
  Result := TJclPeCLRTable(FRows.Items[Idx]);
end;

function TJclPeCLRTableStream.GetRowCount: TJclIndex;
begin
  Result := FRows.Count;
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
