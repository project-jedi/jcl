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
{ The Original Code is JclDFM.pas.                                                                 }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Contains routines for DFM reading and writing...                                                 }
{                                                                                                  }
{ Known Issues:                                                                                    }
{   This is a preview - class and functionnames might be changed                               }
{                                                                                                  }
{ Unit owner: Uwe Schuster                                                                         }
{ Last modified: October 03, 2003                                                                  }
{                                                                                                  }
{**************************************************************************************************}

{
Is
"The Initial Developer of the Original Code is documented in the accompanying
help file JCL.chm."
enough and Peter3 must not explicit listet in the header as author of DFMCleaner
on with JclDFM is based ?

JCL Style checks/fixes:
- "for i := 0 to Count - 1" instead of "to Pred(Count)"
- .GetCount w/o check that FList is Assigned
  "Result := FList.Count"
- .GetItem w/o indexcheck - TList will trow the exception
- .Delete w/o indexcheck - TList will trow the exception
- TObject(FList[i]).Free should be okay in TDFMPropertys.Clear
- inherited functions with "inherited function;" and not "inherited;"
- no assign alignment
    ShortVar := 1;
    LongVariable := 2;
  instead of
    ShortVar     := 1;
    LongVariable := 2;
JCL Style todo:
- class/function seperation by
  //--------------------------------------------------------------------------------------------------
  // ...
  //--------------------------------------------------------------------------------------------------
- check/fix function order in interface and implementation
TODO:
- use TObjectList in TDFMPropertys and TDFMComponents
- support vaList in AsString
- make TDFMProperty.As... writable
- improve DFMLevel writing (autoclean, binary write for D2 or lower)
- tests
}

unit JclDFM;

{$I jcl.inc}

interface

uses
  SysUtils, Classes, TypInfo;

type
  TDFMFiler = class (TFiler)
  public
    ReadProc, WriteProc: TStreamProc;
    procedure DefineProperty(const Name: string;
      ReadData: TReaderProc; WriteData: TWriterProc;
      HasData: Boolean); override;
    procedure DefineBinaryProperty(const Name: string;
      ReadData, WriteData: TStreamProc;
      HasData: Boolean); override;
    procedure FlushBuffer; override;
  end;

  TDFMReader = class (TReader)
  private
    procedure ReadBinary(AStream: TStream);
  end;

  TDFMWriteLevel = (dwlD1, dwlD2, dwlD3, dwlD4, dwlD5, dwlD6, dwlD7);

  TDFMWriter = class (TWriter)
  private
    FNestingLevel: Integer;
    FWriteLevel: TDFMWriteLevel;
    procedure WriteIndent;
    procedure WriteStr(const S: string);
    procedure NewLine;
    procedure WriteBinary(AStream: TStream);
    function GetSkipUnicode: Boolean;
  public
    constructor Create(Stream: TStream; BufSize: Integer);

    function IncNestingLevel: Integer;
    function DecNestingLevel: Integer;

    property NestingLevel: Integer read FNestingLevel write FNestingLevel;
    property WriteLevel: TDFMWriteLevel read FWriteLevel write FWriteLevel;
    property SkipUnicode: Boolean read GetSkipUnicode;
  end;

  TDFMCollectionProperty = class;

  TDFMProperty = class (TObject)
  private
    FName: string;
    FTyp: TValueType;
    FData: Pointer;
    procedure ReadValue(AReader: TDFMReader);
    procedure FreeData;
    function GetAsInteger: Integer;
    function GetAsString: string;

    procedure WriteValue(AWriter: TDFMWriter);
    function GetAsExtended: Extended;
    function GetAsWideString: WideString;
    function GetAsInt64: Int64;
    function GetAsStream: TMemoryStream;
    function GetAsCollectionProperty: TDFMCollectionProperty;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ReadProperty(AReader: TDFMReader);
    procedure WriteProperty(AWriter: TDFMWriter);

    property Name: string read FName;
    property Typ: TValueType read FTyp;

    property AsInteger: Integer read GetAsInteger;
    property AsString: string read GetAsString;
    property AsWideString: WideString read GetAsWideString;
    property AsExtended: Extended read GetAsExtended;
    property AsInt64: Int64 read GetAsInt64;
    property AsStream: TMemoryStream read GetAsStream;
    property AsCollectionProperty: TDFMCollectionProperty read
      GetAsCollectionProperty;
  end;

  TDFMPropertys = class (TObject)
  private
    FList: TList;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TDFMProperty;
  public
    constructor Create;
    destructor Destroy; override;

    function Add: TDFMProperty;
    function AddPropertys: TDFMPropertys;
    procedure Clear;
    procedure Delete(AIndex: Integer);

    procedure ReadPropertys(AReader: TDFMReader);
    procedure WritePropertys(AWriter: TDFMWriter);

    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TDFMProperty read GetItem; default;
  end;

  TDFMCollectionPropertyData = class (TObject)
  private
    FHasIndex: Boolean;
    FIndex: Integer;
    FPropertys: TDFMPropertys;
  public
    constructor Create;
    destructor Destroy; override;

    property HasIndex: Boolean read FHasIndex write FHasIndex;
    property Idx: Integer read FIndex write FIndex;

    property Propertys: TDFMPropertys read FPropertys;
  end;

  TDFMCollectionProperty = class (TObject)
  private
    FList: TList;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TDFMCollectionPropertyData;
  public
    constructor Create;
    destructor Destroy; override;

    function Add: TDFMCollectionPropertyData;

    procedure Clear;

    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TDFMCollectionPropertyData read GetItem; default;
  end;

  TDFMComponents = class;

  TDFMComponent = class (TObject)
  private
    FComponentClassName,
    FComponentName: string;
    FFilerFlags: TFilerFlags;
    FFilerPosition: Integer;
    FPropertys: TDFMPropertys;
    FSubComponents: TDFMComponents;

    procedure ReadHeader(AReader: TReader);
    procedure WriteHeader(AWriter: TDFMWriter);
    function InternalFindComponent(ADFMComponent: TDFMComponent; AComponentName: string): TDFMComponent;
    procedure InternalFindComponentsByClass(ADFMComponent: TDFMComponent;
      AComponentClassName: string; AResultList: TList);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ReadComponent(AReader: TDFMReader);
    procedure WriteComponent(AWriter: TDFMWriter; AWithChilds: Boolean = True);

    function FindComponent(AComponentName: string): TDFMComponent;
    function FindComponentsByClass(AComponentClassName: string;
      AResultList: TList): Integer;
    procedure GetObjectText(AStream: TStream; AWithChilds: Boolean = True);
    procedure GetObjectBinary(AStream: TStream; AWithChilds: Boolean = True);

    property ComponentClassName: string read FComponentClassName;
    property ComponentName: string read FComponentName;
    property FilerFlags: TFilerFlags read FFilerFlags;
    property FilerPosition: Integer read FFilerPosition;

    property Propertys: TDFMPropertys read FPropertys;
    property SubComponents: TDFMComponents read FSubComponents;
  end;

  TDFMRootComponent = class (TDFMComponent)
  public
    procedure LoadFromFile(AFileName: string);
    procedure LoadFromStream(AInput: TStream);
    procedure SaveToFile(AFileName: string);
    procedure SaveToStream(AOutput: TStream);
  end;

  TDFMComponents = class (TObject)
  private
    FList: TList;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TDFMComponent;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Delete(AIndex: Integer);

    procedure ReadComponents(AReader: TDFMReader);
    procedure WriteComponents(AWriter: TDFMWriter);

    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TDFMComponent read GetItem; default;
  end;

function ValueType2String(const AValueType: TValueType): string;
procedure DFMRemoveUnwantedComponentsAndProps(ADFMComponent: TDFMComponent;
  AComponentSkipList, APropertySkipList: TStrings);
procedure DFMGetAllComponentTypes(ADFMComponent: TDFMComponent;
  AComponentList: TStrings); overload;
procedure DFMGetAllComponentTypes(AFileName: string; AComponentList: TStrings);  overload;

implementation

function IsBinDFM(Stream: TStream): boolean;
var
  ASignature: byte;
begin
  Stream.Read(ASignature, sizeof(ASignature));
  Result := ASignature = $FF;
  Stream.Seek(-sizeof(ASignature), soFromCurrent);
end;

{$IFNDEF DELPHI6_UP}
const
  sLineBreak = #13#10;
{$ENDIF}

function ValueType2String(const AValueType: TValueType): string;
begin
  Result := GetEnumName(TypeInfo(TValueType), Integer(AValueType));
end;

{ TDFMComponent }

constructor TDFMComponent.Create;
begin
  inherited Create;

  FComponentClassName := '';
  FComponentName := '';
  FFilerFlags := [];
  FFilerPosition := 0;

  FPropertys := TDFMPropertys.Create;
  FSubComponents := TDFMComponents.Create;
end;

destructor TDFMComponent.Destroy;
begin
  FPropertys.Free;
  FSubComponents.Free;

  inherited Destroy;
end;

procedure TDFMComponent.ReadHeader(AReader: TReader);
begin
  AReader.ReadPrefix(FFilerFlags, FFilerPosition);
  FComponentClassName := AReader.ReadStr;
  FComponentName := AReader.ReadStr;

  //todo - the componentname shouldn't be empty -> exception ?
  if FComponentName = '' then
    FComponentName := FComponentClassName;
end;

procedure TDFMComponent.ReadComponent(AReader: TDFMReader);
begin
  ReadHeader(AReader);
  FPropertys.ReadPropertys(AReader);
  AReader.ReadListEnd;
  SubComponents.ReadComponents(AReader);
  AReader.ReadListEnd;
end;

procedure TDFMComponent.WriteHeader(AWriter: TDFMWriter);
begin
  AWriter.WriteIndent;
  if ffInherited in FFilerFlags then
    AWriter.WriteStr('inherited ')
  else if ffInline in FFilerFlags then
    AWriter.WriteStr('inline ')
  else
    AWriter.WriteStr('object ');
  if FComponentName <> '' then
  begin
    AWriter.WriteStr(FComponentName);
    AWriter.WriteStr(': ');
  end;
  AWriter.WriteStr(FComponentClassName);
  if ffChildPos in FFilerFlags then
  begin
    AWriter.WriteStr(' [');
    AWriter.WriteStr(IntToStr(FFilerPosition));
    AWriter.WriteStr(']');
  end;
  AWriter.WriteStr(sLineBreak);
end;

procedure TDFMComponent.WriteComponent(AWriter: TDFMWriter;
  AWithChilds: Boolean = True);
begin
  WriteHeader(AWriter);
  AWriter.IncNestingLevel;

  Propertys.WritePropertys(AWriter);
  if AWithChilds then
    SubComponents.WriteComponents(AWriter);

  AWriter.DecNestingLevel;

  AWriter.WriteIndent;
  AWriter.WriteStr('end' + sLineBreak);
end;

function TDFMComponent.InternalFindComponent(ADFMComponent: TDFMComponent;
  AComponentName: string): TDFMComponent;
var
  i: Integer;
begin
  Result := nil;
  if Assigned(ADFMComponent) then
  begin
    if SameText(ADFMComponent.ComponentName, AComponentName) then
      Result := ADFMComponent;
    if (not Assigned(Result)) and (ADFMComponent.SubComponents.Count > 0) then
      for i := 0 to ADFMComponent.SubComponents.Count - 1 do
      begin
        Result := InternalFindComponent(ADFMComponent.SubComponents[i], AComponentName);
        if Assigned(Result) then
          Break;
      end;
  end;
end;

function TDFMComponent.FindComponent(AComponentName: string): TDFMComponent;
begin
  Result := nil;
  if AComponentName <> '' then
    Result := InternalFindComponent(Self, AComponentName);
end;

procedure TDFMComponent.InternalFindComponentsByClass(ADFMComponent: TDFMComponent;
  AComponentClassName: string; AResultList: TList);
var
  i: Integer;
begin
  if Assigned(ADFMComponent) then
  begin
    if SameText(ADFMComponent.ComponentClassName, AComponentClassName) then
      AResultList.Add(ADFMComponent);
    if ADFMComponent.SubComponents.Count > 0 then
      for i := 0 to ADFMComponent.SubComponents.Count - 1 do
        InternalFindComponentsByClass(ADFMComponent.SubComponents[i],
          AComponentClassName, AResultList);
  end;
end;

function TDFMComponent.FindComponentsByClass(AComponentClassName: string;
  AResultList: TList): Integer;
begin
  Result := 0;
  if (AComponentClassName <> '') and Assigned(AResultList) then
  begin
    AResultList.Clear;
    InternalFindComponentsByClass(Self, AComponentClassName, AResultList);
    Result := AResultList.Count;
  end;
end;

procedure TDFMComponent.GetObjectText(AStream: TStream; AWithChilds: Boolean = True);
var
  SaveSeparator: Char;
  Writer: TDFMWriter;
begin
  SaveSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  try
    Writer := TDFMWriter.Create(AStream, 4096);
    try
      WriteComponent(Writer, AWithChilds);
    finally
      Writer.Free;
    end;
  finally
    DecimalSeparator := SaveSeparator;
  end;
end;

procedure TDFMComponent.GetObjectBinary(AStream: TStream; AWithChilds: Boolean = True);
var
  TextStream: TMemoryStream;
begin
  TextStream := nil;
  try
    TextStream := TMemoryStream.Create;
    GetObjectText(TextStream, AWithChilds);
    TextStream.Position := 0;
    ObjectTextToBinary(TextStream, AStream);
  finally
    TextStream.Free;
  end;
end;

{ TDFMPropertys }

function TDFMPropertys.Add: TDFMProperty;
begin
  Result := TDFMProperty.Create;
  FList.Add(Result);
end;

function TDFMPropertys.AddPropertys: TDFMPropertys;
begin
  Result := TDFMPropertys.Create;
  FList.Add(Result);
end;

procedure TDFMPropertys.Clear;
var
  i: Integer;
begin
  if FList.Count > 0 then
  begin
    for i := 0 to FList.Count - 1 do
      TObject(FList[i]).Free;
    FList.Clear;
  end;
end;

constructor TDFMPropertys.Create;
begin
  inherited Create;

  FList := TList.Create;
end;

procedure TDFMPropertys.Delete(AIndex: Integer);
begin
  TObject(FList[AIndex]).Free;
  FList.Delete(AIndex);
end;

destructor TDFMPropertys.Destroy;
begin
  Clear;
  FList.Free;

  inherited Destroy;
end;

function TDFMPropertys.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDFMPropertys.GetItem(AIndex: Integer): TDFMProperty;
begin
  Result := FList[AIndex];
end;

procedure TDFMPropertys.ReadPropertys(AReader: TDFMReader);
var
  DFMProperty: TDFMProperty;
begin
  Clear;
  while not AReader.EndOfList do
  begin
    DFMProperty := TDFMProperty.Create;
    DFMProperty.ReadProperty(AReader);
    FList.Add(DFMProperty);
  end;
end;

procedure TDFMPropertys.WritePropertys(AWriter: TDFMWriter);
var
  i: Integer;
begin //todo - perhaps use internal vars
  for i := 0 to Count - 1 do
    Items[i].WriteProperty(AWriter);
end;

{ TDFMProperty }

constructor TDFMProperty.Create;
begin
  inherited Create;

  FName := '';
  FTyp  := vaNull;
  FData := nil;
end;

destructor TDFMProperty.Destroy;
begin
  FreeData;

  inherited Destroy;
end;

procedure TDFMProperty.FreeData;
begin
  if Assigned(FData) and (not (FTyp in [vaInt8, vaInt16, vaInt32]))
  then
  begin
    if (FTyp in [vaString, vaLString])
      or
      (FTyp in [vaWString{$IFDEF DELPHI6_UP}, vaUTF8String{$ENDIF}])
      or
      (FTyp in [vaIdent, vaFalse, vaTrue, vaNil, vaNull])
    then
      Dispose(FData)
    else if (FTyp = vaList) or (FTyp = vaBinary) or (FTyp = vaSet) or
      (FTyp = vaCollection)
    then
      TObject(FData).Free
    else
      FreeMem(FData);
  end;
end;

function TDFMProperty.GetAsExtended: Extended;
begin
  Result := 0;
  if FTyp in [vaExtended, vaSingle, vaCurrency, vaDate] then   //todo - single, currency, data
    Result := PExtended(FData)^;
end;

function TDFMProperty.GetAsInt64: Int64;
begin
  if FTyp = vaInt64 then
    Result := PInt64(FData)^
  else
    Result := GetAsInteger;
end;

function TDFMProperty.GetAsInteger: Integer;
begin
  Result := 0;
  if FTyp in [vaInt8, vaInt16, vaInt32] then
    Result := Integer(FData);
end;

function TDFMProperty.GetAsStream: TMemoryStream;
begin
  Result := nil;
  if FTyp = vaBinary then
    Result := FData;
end;

function TDFMProperty.GetAsString: string;
var
  i: Integer;
  s: string;
begin
  Result := '';
  if FTyp in [vaInt8, vaInt16, vaInt32] then
    Result := IntToStr(GetAsInteger)
  else if FTyp = vaExtended then
    Result := FloatToStr(GetAsExtended)
  else if FTyp = vaSingle then
    Result := FloatToStr(GetAsExtended)
  else if FTyp = vaCurrency then
    Result := FloatToStr(GetAsExtended)
  else if FTyp = vaDate then
    Result := FloatToStr(GetAsExtended)
  else if FTyp in [vaString, vaLString] then
    Result := PString(FData)^
  else if FTyp in [vaIdent, vaFalse, vaTrue, vaNil, vaNull] then
    Result := PString(FData)^
  else if FTyp = vaSet then
  begin
    Result := '[';
    with TStringList(FData) do
      for i := 0 to Count - 1 do
      begin
        s := Strings[i];
        if s = '' then
          Break;
        if i > 0 then
          Result := Result + ', ';
        Result := Result + s;
      end;
    Result := Result + ']';
  end
  else if FTyp = vaInt64 then
    Result := IntToStr(GetAsInt64);
end;

function TDFMProperty.GetAsWideString: WideString;
begin
  Result := '';
  if FTyp in [vaWString{$IFDEF DELPHI6_UP}, vaUTF8String{$ENDIF}] then
    Result := PWideString(FData)^
  else
    Result := GetAsString;
end;

function TDFMProperty.GetAsCollectionProperty: TDFMCollectionProperty;
begin
  Result := nil;
  if FTyp = vaCollection then
    Result := FData;
end;

procedure TDFMProperty.ReadProperty(AReader: TDFMReader);
begin
  FName := AReader.ReadStr;
  ReadValue(AReader);
end;

procedure TDFMProperty.ReadValue(AReader: TDFMReader);
var
  APos: Integer;
  pStr: PString;
  TSL: TStringList;
  s: string;

  pE: PExtended;
  ListPropertys: TDFMPropertys;
  pWStr: PWideString;
  pI64: PInt64;
  Collection: TDFMCollectionProperty;
  CollectionData: TDFMCollectionPropertyData;
  TempDFMProperty: TDFMProperty;
begin
  FreeData;
  APos := AReader.Position;
  FTyp := AReader.ReadValue;
  AReader.Position := APos;
  case FTyp of
    vaList:
    begin
      AReader.ReadValue;
      ListPropertys := TDFMPropertys.Create;
      FData := ListPropertys;
      while not AReader.EndOfList do
        ListPropertys.Add.ReadValue(AReader);
      AReader.ReadListEnd;
    end;
    vaInt8, vaInt16, vaInt32:
      Integer(FData) := AReader.ReadInteger;
    vaExtended:
    begin
      New(pE);
      pE^ := AReader.ReadFloat;
      FData := pE;
    end;
    vaSingle:
    begin
      New(pE);
      pE^ := AReader.ReadSingle; //todo - check if saving in extended is okay
      FData := pE;
    end;
    vaCurrency:
    begin
      New(pE);
      pE^ := AReader.ReadCurrency; //todo - check if saving in extended is okay
      pE^ := pE^ * 10000;
      FData := pE;
    end;
    vaDate:
    begin
      New(pE);
      pE^ := AReader.ReadDate; //todo - check if saving in extended is okay
      FData := pE;
    end;
    vaWString{$IFDEF DELPHI6_UP}, vaUTF8String{$ENDIF}:
    begin
      New(pWStr);
      pWStr^ := AReader.ReadWideString;
      FData := pWStr;
    end;
    vaString, vaLString:
    begin
      New(pStr);
      pStr^ := AReader.ReadString;
      FData := pStr;
    end;
    vaIdent, vaFalse, vaTrue, vaNil, vaNull:
    begin
      New(pStr);
      pStr^ := AReader.ReadIdent;
      FData := pStr;
    end;
    vaBinary:
    begin
      FData := TMemoryStream.Create;
      AReader.ReadBinary(FData);
    end;
    vaSet:
    begin
      TSL := TStringList.Create;
      AReader.ReadValue;
      while True do
      begin
        s := AReader.ReadStr;
        if s = '' then
          Break
        else
          TSL.Add(s);
      end;
      FData := TSL;
    end;
    vaCollection:
    begin
      Collection := TDFMCollectionProperty.Create;
      FData := Collection;
      AReader.ReadValue;
      while not AReader.EndOfList do
      begin
        CollectionData := Collection.Add;

        if AReader.NextValue in [vaInt8, vaInt16, vaInt32] then
        begin
          CollectionData.HasIndex := True;
          TempDFMProperty := TDFMProperty.Create;
          TempDFMProperty.ReadValue(AReader);
          CollectionData.Idx := TempDFMProperty.AsInteger;
          TempDFMProperty.Free;
        end
        else
          CollectionData.HasIndex := False;

        AReader.CheckValue(vaList);
        while not AReader.EndOfList do
          CollectionData.Propertys.Add.ReadProperty(AReader);
        AReader.ReadListEnd;
      end;
      AReader.ReadListEnd;
    end;
    vaInt64:
    begin
      New(pI64);
      pI64^ := AReader.ReadInt64;
      FData := pI64;
    end
    else
      AReader.SkipValue;
  end;
end;

procedure TDFMProperty.WriteProperty(AWriter: TDFMWriter);
begin
  AWriter.WriteIndent;
  AWriter.WriteStr(FName);
  AWriter.WriteStr(' = ');
  WriteValue(AWriter);
  AWriter.WriteStr(sLineBreak);
end;

procedure TDFMProperty.WriteValue(AWriter: TDFMWriter);
const
  LineLength = 64;
var
  I, J, K, L: Integer;
  S: string;
  W: WideString;
  LineBreak: Boolean;

  ListPropertys: TDFMPropertys;
  Collection: TDFMCollectionProperty;
  CollectionData: TDFMCollectionPropertyData;
begin
  case FTyp of
    vaList:
      begin
        ListPropertys := FData;
        AWriter.WriteStr('(');
        AWriter.IncNestingLevel;
        for i := 0 to ListPropertys.Count - 1 do
        begin
          AWriter.NewLine;
          ListPropertys[i].WriteValue(AWriter);
        end;
        AWriter.DecNestingLevel;
        AWriter.WriteStr(')');
      end;
    vaInt8, vaInt16, vaInt32:
      AWriter.WriteStr(GetAsString);
    vaExtended:
      AWriter.WriteStr(GetAsString); //todo - writefloat
    vaSingle:
      AWriter.WriteStr(GetAsString + 's'); //todo - writesingle
    vaCurrency:
      AWriter.WriteStr(GetAsString + 'c'); //todo - writecurrency
    vaDate:
      AWriter.WriteStr(GetAsString + 'd'); //todo - writedate
    vaWString{$IFDEF DELPHI6_UP}, vaUTF8String{$ENDIF}:
      begin
        W := GetAsWideString;
        L := Length(W);
        if L = 0 then
          AWriter.WriteStr('''''')
        else
        begin
          I := 1;
          AWriter.IncNestingLevel;
          try
            if L > LineLength then
              AWriter.NewLine;
            K := I;
            repeat
              LineBreak := False;
              if (W[I] >= ' ') and (W[I] <> '''') and (Ord(W[i]) <= 127) then
              begin
                J := I;
                repeat
                  Inc(I)
                until (I > L) or (W[I] < ' ') or (W[I] = '''') or
                  ((I - K) >= LineLength) or (Ord(W[i]) > 127);
                if ((I - K) >= LineLength) then
                  LineBreak := True;
                AWriter.WriteStr('''');
                while J < I do
                begin
                  AWriter.WriteStr(Char(W[J]));
                  Inc(J);
                end;
                AWriter.WriteStr('''');
              end
              else
              begin
                AWriter.WriteStr('#');
                if (Ord(W[I]) > 255) and AWriter.SkipUnicode then
                  AWriter.WriteStr('32')
                else
                  AWriter.WriteStr(IntToStr(Ord(W[I])));
                Inc(I);
                if ((I - K) >= LineLength) then
                  LineBreak := True;
              end;
              if LineBreak and (I <= L) then
              begin
                AWriter.WriteStr(' +');
                AWriter.NewLine;
                K := I;
              end;
            until I > L;
          finally
            AWriter.DecNestingLevel;
          end;
        end;
      end;
    vaString, vaLString:
      begin
        S := GetAsString;
        L := Length(S);
        if L = 0 then
          AWriter.WriteStr('''''')
        else
        begin
          I := 1;
          AWriter.IncNestingLevel;
          try
            if L > LineLength then
              AWriter.NewLine;
            K := I;
            repeat
              LineBreak := False;
              if (S[I] >= ' ') and (S[I] <> '''') then
              begin
                J := I;
                repeat
                  Inc(I)
                until (I > L) or (S[I] < ' ') or (S[I] = '''') or
                  ((I - K) >= LineLength);
                if ((I - K) >= LineLength) then
                begin
                  LIneBreak := True;
                  if ByteType(S, I) = mbTrailByte then
                    Dec(I);
                end;
                AWriter.WriteStr('''');
                AWriter.Write(S[J], I - J);
                AWriter.WriteStr('''');
              end
              else
              begin
                AWriter.WriteStr('#');
                AWriter.WriteStr(IntToStr(Ord(S[I])));
                Inc(I);
                if ((I - K) >= LineLength) then
                  LineBreak := True;
              end;
              if LineBreak and (I <= L) then
              begin
                AWriter.WriteStr(' +');
                AWriter.NewLine;
                K := I;
              end;
            until I > L;
          finally
            AWriter.DecNestingLevel;
          end;
        end;
      end;
    vaIdent, vaFalse, vaTrue, vaNil, vaNull:
      AWriter.WriteStr(GetAsString);
    vaBinary:
      AWriter.WriteBinary(TMemoryStream(FData));
    vaSet:
      AWriter.WriteStr(GetAsString);
    vaCollection:
      begin
        AWriter.WriteStr('<');
        AWriter.IncNestingLevel;
        Collection := FData;
        for i := 0 to Collection.Count - 1 do
        begin
          CollectionData := Collection[i];

          AWriter.NewLine;
          AWriter.WriteStr('item');

          if CollectionData.HasIndex then
          begin
            AWriter.WriteStr(' [');
            AWriter.WriteStr(IntToStr(CollectionData.Idx));
            AWriter.WriteStr(']');
          end;

          AWriter.WriteStr(sLineBreak);
          AWriter.IncNestingLevel;

          CollectionData.Propertys.WritePropertys(AWriter);
          AWriter.DecNestingLevel;
          AWriter.WriteIndent;
          AWriter.WriteStr('end');
        end;
        AWriter.DecNestingLevel;
        AWriter.WriteStr('>');
      end;
    vaInt64:
      AWriter.WriteStr(GetAsString);
  end;
end;

{ TDFMComponents }

procedure TDFMComponents.Clear;
var
  i: Integer;
begin
  if FList.Count > 0 then
  begin
    for i := 0 to FList.Count - 1 do
      TObject(FList[i]).Free;
    FList.Clear;
  end;
end;

constructor TDFMComponents.Create;
begin
  inherited Create;

  FList := TList.Create;
end;

procedure TDFMComponents.Delete(AIndex: Integer);
begin
  TObject(FList[AIndex]).Free;
  FList.Delete(AIndex);
end;

destructor TDFMComponents.Destroy;
begin
  Clear;
  FList.Free;

  inherited Destroy;
end;

function TDFMComponents.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDFMComponents.GetItem(AIndex: Integer): TDFMComponent;
begin
  Result := FList[AIndex];
end;

procedure TDFMComponents.ReadComponents(AReader: TDFMReader);
var
  DFMComponent: TDFMComponent;
begin
  Clear;
  while not AReader.EndOfList do
  begin
    DFMComponent := TDFMComponent.Create;
    DFMComponent.ReadComponent(AReader);
    FList.Add(DFMComponent);
  end;
end;

procedure TDFMComponents.WriteComponents(AWriter: TDFMWriter);
var
  i: Integer;
begin //todo - perhaps use internal vars
  for i := 0 to Count - 1 do
    Items[i].WriteComponent(AWriter);
end;

{ TDFMRootComponent }

procedure TDFMRootComponent.LoadFromFile(AFileName: string);
var
  fs: TFileStream;
begin
  fs := nil;
  try
    fs := TFileStream.Create(AFileName, fmOpenRead); //todo -> sharemode
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TDFMRootComponent.LoadFromStream(AInput: TStream);
var
  SaveSeparator: Char;
  Reader: TDFMReader;
  tmpStream: TMemoryStream;
begin
  FPropertys.Clear;
  FSubComponents.Clear;
  tmpStream := TMemoryStream.Create;
  try
    if not IsBinDFM(AInput) then
    begin
      ObjectTextToResource(AInput, tmpStream);
      tmpStream.Seek(0, soFromBeginning);
      AInput := tmpStream;
    end;

    AInput.ReadResHeader;
    Reader := TDFMReader.Create(AInput, 4096);
    SaveSeparator := DecimalSeparator;
    DecimalSeparator := '.';
    try
      Reader.ReadSignature;
      ReadComponent(Reader);
    finally
      DecimalSeparator := SaveSeparator;
      Reader.Free;
    end;
  finally
    tmpStream.Free;
  end;
end;

procedure TDFMRootComponent.SaveToFile(AFileName: string);
var
  fs: TFileStream;
begin
  fs := nil;
  try
    fs := TFileStream.Create(AFileName, fmCreate);
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TDFMRootComponent.SaveToStream(AOutput: TStream);
begin
  GetObjectText(AOutput);
end;

{ TDFMWriter }

constructor TDFMWriter.Create(Stream: TStream; BufSize: Integer);
begin
  inherited Create(Stream, BufSize);
  FNestingLevel := 0;
//it doesn't compile with D3 or lower because of overload, int64 and widestrings
//but it's not dangerous to list it here
{$IFDEF DELPHI1_UP}
  FWriteLevel := dwlD1;
{$ENDIF}
{$IFDEF DELPHI2_UP}
  FWriteLevel := dwlD2;
{$ENDIF}
{$IFDEF DELPHI3_UP}
  FWriteLevel := dwlD3;
{$ENDIF}
{$IFDEF DELPHI4_UP}
  FWriteLevel := dwlD4;
{$ENDIF}
{$IFDEF DELPHI5_UP}
  FWriteLevel := dwlD5;
{$ENDIF}
{$IFDEF DELPHI6_UP}
  FWriteLevel := dwlD6;
{$ENDIF}
{$IFDEF DELPHI7_UP}
  FWriteLevel := dwlD7;
{$ENDIF}
end;

function TDFMWriter.IncNestingLevel: Integer;
begin
  Inc(FNestingLevel);
  Result := FNestingLevel;
end;

function TDFMWriter.DecNestingLevel: Integer;
begin
  Dec(FNestingLevel);
  Result := FNestingLevel;  
end;

procedure TDFMWriter.WriteIndent;
const
  Blanks: array[0..1] of Char = '  ';
var
  i: Integer;
begin
  for i := 1 to FNestingLevel do
    Write(Blanks, SizeOf(Blanks));
end;

procedure TDFMWriter.WriteStr(const S: string);
begin
  Write(S[1], Length(S));
end;

procedure TDFMWriter.NewLine;
begin
  WriteStr(sLineBreak);
  WriteIndent;
end;

procedure TDFMWriter.WriteBinary(AStream: TStream);
const
  BytesPerLine = 32;
var
  MultiLine: Boolean;
  i: Integer;
  Count: Longint;
  Buffer: array[0..BytesPerLine - 1] of Char;
  Text: array[0..BytesPerLine * 2 - 1] of Char;
begin
  Count := AStream.Size;
  AStream.Position := 0;

  WriteStr('{');
  Inc(FNestingLevel);
  MultiLine := Count >= BytesPerLine;
  while Count > 0 do
  begin
    if MultiLine then
      NewLine;
    if Count >= 32 then
      i := 32
    else
      i := Count;
    AStream.read(Buffer, i);
    BinToHex(Buffer, Text, i);
    Write(Text, i * 2);
    Dec(Count, i);
  end;
  Dec(FNestingLevel);
  WriteStr('}');
end;

function TDFMWriter.GetSkipUnicode: Boolean;
begin
  Result := FWriteLevel < dwlD6;
end;

{ TDFMFiler }

procedure TDFMFiler.DefineProperty(const Name: string;
  ReadData: TReaderProc; WriteData: TWriterProc;
  HasData: Boolean);
begin
//do nothing
end;

procedure TDFMFiler.DefineBinaryProperty(const Name: string;
  ReadData, WriteData: TStreamProc;
  HasData: Boolean);
begin
  ReadProc  := ReadData;
  WriteProc := WriteData;
end;

procedure TDFMFiler.FlushBuffer;
begin
//do nothing
end;

{ TDFMReader }

procedure TDFMReader.ReadBinary(AStream: TStream);
const
  BytesPerLine = 32;
var
  i: Integer;
  Count: Longint;
  Buffer: array[0..BytesPerLine - 1] of Char;
begin
  ReadValue;
  Read(Count, SizeOf(Count));

  while Count > 0 do
  begin
    if Count >= 32 then
      i := 32
    else
      i := Count;
    Read(Buffer, i);
    AStream.Write(Buffer, i);
    Dec(Count, i);
  end;
end;

{ TDFMCollectionPropertyData }

constructor TDFMCollectionPropertyData.Create;
begin
  inherited Create;

  FHasIndex := False;
  FIndex := 0;
  FPropertys := TDFMPropertys.Create;
end;

destructor TDFMCollectionPropertyData.Destroy;
begin
  FPropertys.Free;

  inherited Destroy;
end;

{ TDFMCollectionProperty }

function TDFMCollectionProperty.Add: TDFMCollectionPropertyData;
begin
  Result := TDFMCollectionPropertyData.Create;
  FList.Add(Result);
end;

procedure TDFMCollectionProperty.Clear;
var
  i: Integer;
begin
  if FList.Count > 0 then
  begin
    for i := 0 to FList.Count - 1 do
      TObject(FList[i]).Free;
    FList.Clear;
  end;
end;

constructor TDFMCollectionProperty.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TDFMCollectionProperty.Destroy;
begin
  Clear;
  FList.Free;

  inherited Destroy;
end;

function TDFMCollectionProperty.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDFMCollectionProperty.GetItem(
  AIndex: Integer): TDFMCollectionPropertyData;
begin
  Result := FList[AIndex];
end;

function IsUnwantedComponent(const AClassName: string;
  AComponentSkipList: TStrings): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Assigned(AComponentSkipList) and (AComponentSkipList.Count > 0) then
    for i := 0 to AComponentSkipList.Count - 1 do
      if SameText(AClassName, AComponentSkipList[i]) then
      begin
        Result := True;
        Break;
      end;
end;

function IsUnwantedProperty(const AClassName, APropName: string;
  APropertySkipList: TStrings): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Assigned(APropertySkipList) and (APropertySkipList.Count > 0) then
    for i := 0 to APropertySkipList.Count - 1 do
    begin
      if SameText(AClassName+'.' + APropName, APropertySkipList[i]) or
        SameText('*.' + APropName, APropertySkipList[i]) then
      begin
        Result := True;
        Break;
      end;
    end;
end;

procedure DFMRemoveUnwantedComponentsAndProps(ADFMComponent: TDFMComponent;
  AComponentSkipList, APropertySkipList: TStrings);
var
  i: Integer;
begin
  with ADFMComponent do
  begin
    if Assigned(APropertySkipList) and (APropertySkipList.Count > 0) then
      for i := Propertys.Count - 1 downto 0 do
        if IsUnwantedProperty(ComponentClassName, Propertys[i].Name,
          APropertySkipList)
        then
          Propertys.Delete(i);
    if (Assigned(APropertySkipList) and (APropertySkipList.Count > 0)) or
      (Assigned(AComponentSkipList) and (AComponentSkipList.Count > 0))
    then
      for i := SubComponents.Count - 1 downto 0 do
        if IsUnwantedComponent(ComponentClassName, AComponentSkipList) then
          SubComponents.Delete(i)
        else
          DFMRemoveUnwantedComponentsAndProps(SubComponents[i], AComponentSkipList,
            APropertySkipList);
  end;
end;

procedure DFMGetAllComponentTypes(ADFMComponent: TDFMComponent;
  AComponentList: TStrings);
var
  i: Integer;
begin
  if AComponentList.IndexOf(ADFMComponent.ComponentClassName) = -1 then
    AComponentList.Add(ADFMComponent.ComponentClassName);
  for i := 0 to ADFMComponent.SubComponents.Count - 1 do
    DFMGetAllComponentTypes(ADFMComponent.SubComponents[i], AComponentList);
end;

procedure DFMGetAllComponentTypes(AFileName: string; AComponentList: TStrings);
var
  RCOMP: TDFMRootComponent;
begin
  AComponentList.Clear;
  RCOMP := nil;
  try
    RCOMP := TDFMRootComponent.Create;
    RCOMP.LoadFromFile(AFileName);
    DFMGetAllComponentTypes(RCOMP, AComponentList);
  finally
    RCOMP.Free;
  end;
end;

end.
