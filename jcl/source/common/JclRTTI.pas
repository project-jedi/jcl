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
{ The Original Code is JclRTTI.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel Bestebroer.                                 }
{ Portions created Marcel Bestebroer are Copyright (C) Marcel Bestebroer. All rights reserved.     }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Theo Bebekis, Marcel Bestebroer (marcelb), Peter J. Haas, Robert Marquardt, Robert Rossmair,   }
{   Matthias Thoma, Petr Vones                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Various RunTime Type Information routines. Includes retrieving RTTI information for different    }
{ types, declaring/generating new types, data conversion to user displayable values and 'is'/'as'  }
{ operator hooking.                                                                                }
{                                                                                                  }
{ Unit owner: Marcel Bestebroer                                                                    }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclRTTI;

{$I jcl.inc}

interface

uses
  {$IFDEF HAS_UNIT_TYPES}
  Types,
  {$ELSE}
  Windows,
  {$ENDIF HAS_UNIT_TYPES}
  Classes, SysUtils, TypInfo,
  JclBase;

type
  EJclRTTI = class(EJclError);

//--------------------------------------------------------------------------------------------------
// TypeInfo writing
//--------------------------------------------------------------------------------------------------

type
  IJclInfoWriter = interface
    ['{7DAD522D-46EA-11D5-B0C0-4854E825F345}']
    function GetWrap: Integer;
    procedure SetWrap(const Value: Integer);
    procedure Write(const S: string);
    procedure Writeln(const S: string = '');
    procedure Indent;
    procedure Outdent;
    property Wrap: Integer read GetWrap write SetWrap;
  end;

  TJclInfoWriter = class(TInterfacedObject, IJclInfoWriter)
  private
    FCurLine: string;
    FIndentLevel: Integer;
    FWrap: Integer;
  protected
    function GetWrap: Integer;
    procedure SetWrap(const Value: Integer);
    procedure DoWrap;
    procedure DoWriteCompleteLines;
    procedure PrimWrite(const S: string); virtual; abstract;

    property CurLine: string read FCurLine write FCurLine;
    property IndentLevel: Integer read FIndentLevel write FIndentLevel;
  public
    constructor Create(const AWrap: Integer = 80);
    destructor Destroy; override;
    procedure Indent;
    procedure Outdent;
    procedure Write(const S: string);
    procedure Writeln(const S: string = '');

    property Wrap: Integer read GetWrap write SetWrap;
  end;

  TJclInfoStringsWriter = class(TJclInfoWriter)
  private
    FStrings: TStrings;
  protected
    procedure PrimWrite(const S: string); override;
  public
    constructor Create(const AStrings: TStrings; const AWrap: Integer = 80);

    property Strings: TStrings read FStrings;
  end;

//--------------------------------------------------------------------------------------------------
// TypeInfo retreival
//--------------------------------------------------------------------------------------------------

type
  IJclBaseInfo = interface
    procedure WriteTo(const Dest: IJclInfoWriter);
    procedure DeclarationTo(const Dest: IJclInfoWriter);
  end;

  IJclTypeInfo = interface(IJclBaseInfo)
    ['{7DAD5220-46EA-11D5-B0C0-4854E825F345}']
    function GetName: string;
    function GetTypeData: PTypeData;
    function GetTypeInfo: PTypeInfo;
    function GetTypeKind: TTypeKind;

    property Name: string read GetName;
    property TypeData: PTypeData read GetTypeData;
    property TypeInfo: PTypeInfo read GetTypeInfo;
    property TypeKind: TTypeKind read GetTypeKind;
  end;

  // Ordinal types
  IJclOrdinalTypeInfo = interface(IJclTypeInfo)
    ['{7DAD5221-46EA-11D5-B0C0-4854E825F345}']
    function GetOrdinalType: TOrdType;

    property OrdinalType: TOrdType read GetOrdinalType;
  end;

  IJclOrdinalRangeTypeInfo = interface(IJclOrdinalTypeInfo)
    ['{7DAD5222-46EA-11D5-B0C0-4854E825F345}']
    function GetMinValue: Int64;
    function GetMaxValue: Int64;

    property MinValue: Int64 read GetMinValue;
    property MaxValue: Int64 read GetMaxValue;
  end;

  IJclEnumerationTypeInfo = interface(IJclOrdinalRangeTypeInfo)
    ['{7DAD5223-46EA-11D5-B0C0-4854E825F345}']
    function GetBaseType: IJclEnumerationTypeInfo;
    function GetNames(const I: Integer): string;
    {$IFDEF COMPILER6_UP}
    function GetUnitName: string;
    {$ENDIF COMPILER6_UP}

    function IndexOfName(const Name: string): Integer;

    property BaseType: IJclEnumerationTypeInfo read GetBaseType;
    property Names[const I: Integer]: string read GetNames; default;
    {$IFDEF COMPILER6_UP}
    property UnitName: string read GetUnitName;
    {$ENDIF COMPILER6_UP}
  end;

  IJclSetTypeInfo = interface(IJclOrdinalTypeInfo)
    ['{7DAD5224-46EA-11D5-B0C0-4854E825F345}']
    function GetBaseType: IJclOrdinalTypeInfo;

    procedure GetAsList(const Value;  const WantRanges: Boolean;
      const Strings: TStrings);
    procedure SetAsList(out Value; const Strings: TStrings);

    property BaseType: IJclOrdinalTypeInfo read GetBaseType;
  end;

  // Float types
  IJclFloatTypeInfo = interface(IJclTypeInfo)
    ['{7DAD5225-46EA-11D5-B0C0-4854E825F345}']
    function GetFloatType: TFloatType;

    property FloatType: TFloatType read GetFloatType;
  end;

  // Short string types
  IJclStringTypeInfo = interface(IJclTypeInfo)
    ['{7DAD5226-46EA-11D5-B0C0-4854E825F345}']
    function GetMaxLength: Integer;

    property MaxLength: Integer read GetMaxLength;
  end;

  // Class types
  TJclPropSpecKind = (pskNone, pskStaticMethod, pskVirtualMethod, pskField,
    pskConstant);

  IJclPropInfo = interface
    ['{7DAD5227-46EA-11D5-B0C0-4854E825F345}']
    function GetPropType: IJclTypeInfo;
    function GetReader: Pointer;
    function GetWriter: Pointer;
    function GetStoredProc: Pointer;
    function GetIndex: Integer;
    function GetDefault: Longint;
    function GetNameIndex: Smallint;
    function GetName: string;
    function GetReaderType: TJclPropSpecKind;
    function GetWriterType: TJclPropSpecKind;
    function GetStoredType: TJclPropSpecKind;
    function GetReaderValue: Integer;
    function GetWriterValue: Integer;
    function GetStoredValue: Integer;

    function IsStored(const AInstance: TObject): Boolean;
    function HasDefault: Boolean;
    function HasIndex: Boolean;

    property PropType: IJclTypeInfo read GetPropType;
    property Reader: Pointer read GetReader;
    property Writer: Pointer read GetWriter;
    property StoredProc: Pointer read GetStoredProc;
    property ReaderType: TJclPropSpecKind read GetReaderType;
    property WriterType: TJclPropSpecKind read GetWriterType;
    property StoredType: TJclPropSpecKind read GetStoredType;
    property ReaderValue: Integer read GetReaderValue;
    property WriterValue: Integer read GetWriterValue;
    property StoredValue: Integer read GetStoredValue;
    property Index: Integer read GetIndex;
    property Default: Longint read GetDefault;
    property NameIndex: Smallint read GetNameIndex;
    property Name: string read GetName;
  end;

  IJclClassTypeInfo = interface(IJclTypeInfo)
    ['{7DAD5228-46EA-11D5-B0C0-4854E825F345}']
    function GetClassRef: TClass;
    function GetParent: IJclClassTypeInfo;
    function GetTotalPropertyCount: Integer;
    function GetPropertyCount: Integer;
    function GetProperties(const PropIdx: Integer): IJclPropInfo;
    function GetUnitName: string;

    property ClassRef: TClass read GetClassRef;
    property Parent: IJclClassTypeInfo read GetParent;
    property TotalPropertyCount: Integer read GetPropertyCount;
    property PropertyCount: Integer read GetPropertyCount;
    property Properties[const PropIdx: Integer]: IJclPropInfo
      read GetProperties;
    property UnitName: string read GetUnitName;
  end;

  // Event types
  IJclEventParamInfo = interface
    ['{7DAD5229-46EA-11D5-B0C0-4854E825F345}']
    function GetFlags: TParamFlags;
    function GetName: string;
    function GetRecSize: Integer;
    function GetTypeName: string;
    function GetParam: Pointer;

    property Flags: TParamFlags read GetFlags;
    property Name: string read GetName;
    property RecSize: Integer read GetRecSize;
    property TypeName: string read GetTypeName;
    property Param: Pointer read GetParam;
  end;

  IJclEventTypeInfo = interface(IJclTypeInfo)
    ['{7DAD522A-46EA-11D5-B0C0-4854E825F345}']
    function GetMethodKind: TMethodKind;
    function GetParameterCount: Integer;
    function GetParameters(const ParamIdx: Integer): IJclEventParamInfo;
    function GetResultTypeName: string;

    property MethodKind: TMethodKind read GetMethodKind;
    property ParameterCount: Integer read GetParameterCount;
    property Parameters[const ParamIdx: Integer]: IJclEventParamInfo
      read GetParameters;
    property ResultTypeName: string read GetResultTypeName;
  end;

  // Interface types
  IJclInterfaceTypeInfo = interface(IJclTypeInfo)
    ['{7DAD522B-46EA-11D5-B0C0-4854E825F345}']
    function GetParent: IJclInterfaceTypeInfo;
    function GetFlags: TIntfFlagsBase;
    function GetGUID: TGUID;
    {$IFDEF COMPILER6_UP}
    function GetPropertyCount: Integer;
    {$ENDIF COMPILER6_UP}
    function GetUnitName: string;

    property Parent: IJclInterfaceTypeInfo read GetParent;
    property Flags: TIntfFlagsBase read GetFlags;
    property GUID: TGUID read GetGUID;
    {$IFDEF COMPILER6_UP}
    property PropertyCount: Integer read GetPropertyCount;
    {$ENDIF COMPILER6_UP}
    property UnitName: string read GetUnitName;
  end;

  // Int64 types
  IJclInt64TypeInfo = interface(IJclTypeInfo)
    ['{7DAD522C-46EA-11D5-B0C0-4854E825F345}']
    function GetMinValue: Int64;
    function GetMaxValue: Int64;

    property MinValue: Int64 read GetMinValue;
    property MaxValue: Int64 read GetMaxValue;
  end;

  {$IFDEF COMPILER6_UP}
  // Dynamic array types
  IJclDynArrayTypeInfo = interface(IJclTypeInfo)
    ['{7DAD522E-46EA-11D5-B0C0-4854E825F345}']
    function GetElementSize: Longint;
    function GetElementType: IJclTypeInfo;
    function GetElementsNeedCleanup: Boolean;
    function GetVarType: Integer;
    function GetUnitName: string;

    property ElementSize: Longint read GetElementSize;
    property ElementType: IJclTypeInfo read GetElementType;
    property ElementsNeedCleanup: Boolean read GetElementsNeedCleanup;
    property VarType: Integer read GetVarType;
    property UnitName: string read GetUnitName;
  end;
  {$ENDIF COMPILER6_UP}

function JclTypeInfo(ATypeInfo: PTypeInfo): IJclTypeInfo;

//--------------------------------------------------------------------------------------------------
// Enumeration types
//--------------------------------------------------------------------------------------------------

const
  PREFIX_CUT_LOWERCASE = 255;
  PREFIX_CUT_EQUAL     = 254;

  MaxPrefixCut = 250;

function JclEnumValueToIdent(TypeInfo: PTypeInfo; const Value): string;
function JclGenerateEnumType(const TypeName: ShortString;
  const Literals: array of string): PTypeInfo;
function JclGenerateEnumTypeBasedOn(const TypeName: ShortString;
  BaseType: PTypeInfo; const PrefixCut: Byte): PTypeInfo;
function JclGenerateSubRange(BaseType: PTypeInfo; const TypeName: string;
  const MinValue, MaxValue: Integer): PTypeInfo;

//--------------------------------------------------------------------------------------------------
// Integer types
//--------------------------------------------------------------------------------------------------

function JclStrToTypedInt(Value: string; TypeInfo: PTypeInfo): Integer;
function JclTypedIntToStr(Value: Integer; TypeInfo: PTypeInfo): string;

//--------------------------------------------------------------------------------------------------
// Sets
//--------------------------------------------------------------------------------------------------

function JclSetToList(TypeInfo: PTypeInfo; const Value; const WantBrackets: Boolean;
  const WantRanges: Boolean; const Strings: TStrings): string;
function JclSetToStr(TypeInfo: PTypeInfo; const Value;
  const WantBrackets: Boolean = False; const WantRanges: Boolean = False): string;
procedure JclStrToSet(TypeInfo: PTypeInfo; var SetVar; const Value: string);
procedure JclIntToSet(TypeInfo: PTypeInfo; var SetVar; const Value: Integer);
function JclSetToInt(TypeInfo: PTypeInfo; const SetVar): Integer;
function JclGenerateSetType(BaseType: PTypeInfo; const TypeName: ShortString): PTypeInfo;

//--------------------------------------------------------------------------------------------------
// GUID
//--------------------------------------------------------------------------------------------------

function JclGUIDToString(const GUID: TGUID): string;
function JclStringToGUID(const S: string): TGUID;

//--------------------------------------------------------------------------------------------------
// User generated type info managment
//--------------------------------------------------------------------------------------------------

procedure RemoveTypeInfo(TypeInfo: PTypeInfo);

//--------------------------------------------------------------------------------------------------
// Is/As hooking
//--------------------------------------------------------------------------------------------------

function JclIsClass(const AnObj: TObject; const AClass: TClass): Boolean;
function JclIsClassByName(const AnObj: TObject; const AClass: TClass): Boolean;

implementation

uses
  {$IFDEF HAS_UNIT_RTLCONSTS}
  RtlConsts,
  {$ENDIF HAS_UNIT_RTLCONSTS}
  SysConst,
  JclLogic, JclResources, JclStrings;     

//==================================================================================================
// TJclInfoWriter
//==================================================================================================

constructor TJclInfoWriter.Create(const AWrap: Integer);
begin
  inherited Create;
  Wrap := AWrap;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclInfoWriter.Destroy;
begin
  if CurLine <> '' then
    Writeln('');
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TJclInfoWriter.GetWrap: Integer;
begin
  Result := FWrap;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclInfoWriter.SetWrap(const Value: Integer);
begin
  FWrap := Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclInfoWriter.DoWrap;
var
  TmpLines: TStringList;
  I: Integer;
  TmpLines2: TStringList;
  EndedInCRLF: Boolean;
  LineBreakLength: integer;
begin
  LineBreakLength := Length(AnsiLineBreak);
  EndedInCRLF := Copy(CurLine, Length(CurLine) - LineBreakLength + 1, LineBreakLength) = AnsiLineBreak;
  TmpLines := TStringList.Create;
  try
    TmpLines.Text := CurLine;
    TmpLines2 := TStringList.Create;
    try
      I := TmpLines.Count-1;
      if not EndedInCRLF then
        Dec(I);
      while I >= 0 do
      begin
        TmpLines[I] := StringOfChar(' ', 2 * IndentLevel) + TmpLines[I];
        if (Wrap > 0) and (Length(TmpLines[I]) > Wrap) then
        begin
          TmpLines2.Text := WrapText(
            TmpLines[I],
            AnsiLineBreak + StringOfChar(' ', 2 * (IndentLevel+1)),
            [#0 .. ' ', '-'],
            Wrap);
          TmpLines.Delete(I);
          TmpLines.Insert(I, Copy(TmpLines2.Text, 1,
            Length(TmpLines2.Text) - 2));
        end;
        Dec(I);
      end;
      CurLine := TmpLines.Text;
      if not EndedInCRLF then
        Delete(FCurLine, Length(FCurLine) - LineBreakLength + 1, LineBreakLength);
    finally
      TmpLines2.Free;
    end;
  finally
    TmpLines.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclInfoWriter.DoWriteCompleteLines;
var
  CRLFPos: Integer;
begin
  CRLFPos := StrLastPos(AnsiLineBreak, CurLine);
  if CRLFPos > 0 then
  begin
    PrimWrite(Copy(CurLine, 1, CRLFPos-1));
    Delete(FCurLine, 1, CRLFPos+1);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclInfoWriter.Indent;
begin
  IndentLevel := IndentLevel + 1;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclInfoWriter.Outdent;
begin
  IndentLevel := IndentLevel - 1;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclInfoWriter.Write(const S: string);
begin
  CurLine := CurLine + S;
  DoWrap;
  DoWriteCompleteLines;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclInfoWriter.Writeln(const S: string);
begin
  Write(S + AnsiLineBreak);
end;

//--------------------------------------------------------------------------------------------------
// TJclInfoStringsWriter
//--------------------------------------------------------------------------------------------------

constructor TJclInfoStringsWriter.Create(const AStrings: TStrings;
  const AWrap: Integer);
begin
  inherited Create(AWrap);
  FStrings := AStrings;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclInfoStringsWriter.PrimWrite(const S: string);
begin
  Strings.Add(S);
end;

//--------------------------------------------------------------------------------------------------
// TJclTypeInfo
//--------------------------------------------------------------------------------------------------

type
  TJclTypeInfo = class(TInterfacedObject, IJclTypeInfo)
  private
    FTypeData: PTypeData;
    FTypeInfo: PTypeInfo;
  protected
    function GetName: string;
    function GetTypeData: PTypeData;
    function GetTypeInfo: PTypeInfo;
    function GetTypeKind: TTypeKind;
    procedure WriteTo(const Dest: IJclInfoWriter); virtual;
    procedure DeclarationTo(const Dest: IJclInfoWriter); virtual;
  public
    constructor Create(ATypeInfo: PTypeInfo);

    property Name: string read GetName;
    property TypeData: PTypeData read GetTypeData;
    property TypeInfo: PTypeInfo read GetTypeInfo;
    property TypeKind: TTypeKind read GetTypeKind;
  end;

//--------------------------------------------------------------------------------------------------

constructor TJclTypeInfo.Create(ATypeInfo: PTypeInfo);
begin
  inherited Create;
  FTypeInfo := ATypeInfo;
  FTypeData := TypInfo.GetTypeData(ATypeInfo);
end;

//--------------------------------------------------------------------------------------------------

function TJclTypeInfo.GetName: string;
begin
  Result := TypeInfo.Name;
end;

//--------------------------------------------------------------------------------------------------

function TJclTypeInfo.GetTypeData: PTypeData;
begin
  Result := FTypeData;
end;

//--------------------------------------------------------------------------------------------------

function TJclTypeInfo.GetTypeInfo: PTypeInfo;
begin
  Result := FTypeInfo;
end;

//--------------------------------------------------------------------------------------------------

function TJclTypeInfo.GetTypeKind: TTypeKind;
begin
  Result := TypeInfo.Kind
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  Dest.Writeln(RsRTTIName + Name);
  Dest.Writeln(RsRTTITypeKind + JclEnumValueToIdent(System.TypeInfo(TTypeKind),
    TypeInfo.Kind));
  Dest.Writeln(Format(RsRTTITypeInfoAt, [TypeInfo]));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
begin
  { TODO : localize? }
  Dest.Write('// Declaration for ''' + Name + ''' not supported.');
end;

//--------------------------------------------------------------------------------------------------
// TJclOrdinalTypeInfo
//--------------------------------------------------------------------------------------------------

type
  TJclOrdinalTypeInfo = class(TJclTypeInfo, IJclOrdinalTypeInfo)
  private
  protected
    function GetOrdinalType: TOrdType;
    procedure WriteTo(const Dest: IJclInfoWriter); override;
  public
    property OrdinalType: TOrdType read GetOrdinalType;
  end;

//--------------------------------------------------------------------------------------------------

function TJclOrdinalTypeInfo.GetOrdinalType: TOrdType;
begin
  Result := TypeData.OrdType;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclOrdinalTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  Dest.Writeln(RsRTTIOrdinalType + JclEnumValueToIdent(
    System.TypeInfo(TOrdType), TypeData.OrdType));
end;

//--------------------------------------------------------------------------------------------------
// TJclOrdinalRangeTypeInfo
//--------------------------------------------------------------------------------------------------

type
  TJclOrdinalRangeTypeInfo = class(TJclOrdinalTypeInfo,
    IJclOrdinalRangeTypeInfo)
  private
  protected
    function GetMinValue: Int64;
    function GetMaxValue: Int64;
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
  public
    property MinValue: Int64 read GetMinValue;
    property MaxValue: Int64 read GetMaxValue;
  end;

//--------------------------------------------------------------------------------------------------

function TJclOrdinalRangeTypeInfo.GetMinValue: Int64;
begin
  if OrdinalType = otULong then
    Result := Longword(TypeData.MinValue)
  else
    Result := TypeData.MinValue;
end;

//--------------------------------------------------------------------------------------------------

function TJclOrdinalRangeTypeInfo.GetMaxValue: Int64;
begin
  if OrdinalType = otULong then
    Result := Longword(TypeData.MaxValue)
  else
    Result := TypeData.MaxValue;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclOrdinalRangeTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  Dest.Writeln(RsRTTIMinValue + IntToStr(MinValue));
  Dest.Writeln(RsRTTIMaxValue + IntToStr(MaxValue));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclOrdinalRangeTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
begin
  Dest.Write(Name + ' = ');
  if TypeInfo.Kind in [tkChar, tkWChar] then
  begin
    if (MinValue < Ord(' ')) or (MinValue > Ord('~')) then
      Dest.Write('#' + IntToStr(MinValue) + ' .. ')
    else
      Dest.Write('''' + Chr(Byte(MinValue)) + ''' .. ');
    if (MaxValue < Ord(' ')) or (MaxValue > Ord('~')) then
      Dest.Write('#' + IntToStr(MaxValue) + '; // ')
    else
      Dest.Write('''' + Chr(Byte(MaxValue)) + '''; // ');
  end
  else
    Dest.Write(IntToStr(MinValue) + ' .. ' + IntToStr(MaxValue) + '; // ');
  Dest.Write(JclEnumValueToIdent(System.TypeInfo(TOrdType), TypeData.OrdType));
  Dest.Writeln('');
end;

//--------------------------------------------------------------------------------------------------
// TJclEnumerationTypeInfo
//--------------------------------------------------------------------------------------------------

type
  TJclEnumerationTypeInfo = class(TJclOrdinalRangeTypeInfo,
    IJclEnumerationTypeInfo)
  private
  protected
    function GetBaseType: IJclEnumerationTypeInfo;
    function GetNames(const I: Integer): string;
    {$IFDEF COMPILER6_UP}
    function GetUnitName: string;
    {$ENDIF COMPILER6_UP}
    function IndexOfName(const Name: string): Integer;
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
  public
    property BaseType: IJclEnumerationTypeInfo read GetBaseType;
    property Names[const I: Integer]: string read GetNames; default;
    {$IFDEF COMPILER6_UP}
    property UnitName: string read GetUnitName;
    {$ENDIF COMPILER6_UP}
  end;

//--------------------------------------------------------------------------------------------------

function TJclEnumerationTypeInfo.GetBaseType: IJclEnumerationTypeInfo;
begin
  if TypeData.BaseType^ = TypeInfo then
    Result := Self
  else
    Result := TJclEnumerationTypeInfo.Create(TypeData.BaseType^);
end;

//--------------------------------------------------------------------------------------------------

function TJclEnumerationTypeInfo.GetNames(const I: Integer): string;
var
  Base: IJclEnumerationTypeInfo;
  Idx: Integer;
  P: ^ShortString;
begin
  Base := BaseType;
  Idx := I;
  P := @Base.TypeData.NameList;
  while Idx <> 0 do
  begin
    Inc(Integer(P), Length(P^) + 1);
    Dec(Idx);
  end;
  Result := P^;
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF COMPILER6_UP}

function TJclEnumerationTypeInfo.GetUnitName: string;
var
  I: Integer;
  P: ^ShortString;
begin
  if BaseType.TypeInfo = TypeInfo then
  begin
    I := MaxValue - MinValue;
    P := @TypeData.NameList;
    while I >= 0 do
    begin
      Inc(Integer(P), Length(P^) + 1);
      Dec(I);
    end;
    Result := P^;
  end
  else
    Result := TypeData.NameList;
end;

{$ENDIF COMPILER6_UP}

//--------------------------------------------------------------------------------------------------

function TJclEnumerationTypeInfo.IndexOfName(const Name: string): Integer;
begin
  Result := MaxValue;
  while (Result >= MinValue) and not AnsiSameText(Name, Names[Result]) do
    Dec(Result);
  if Result < MinValue then
    Result := -1;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclEnumerationTypeInfo.WriteTo(const Dest: IJclInfoWriter);
var
  Idx: Integer;
  Prefix: string;
begin
  inherited WriteTo(Dest);
  {$IFDEF COMPILER6_UP}
  Dest.Writeln(RsRTTIUnitName + UnitName);
  {$ENDIF COMPILER6_UP}
  Dest.Write(RsRTTINameList);
  Prefix := '(';
  for Idx := MinValue to MaxValue do
  begin
    Dest.Write(Prefix + Names[Idx]);
    Prefix := ', ';
  end;
  Dest.Writeln(')');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclEnumerationTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
var
  Prefix: string;
  I: Integer;
begin
  if Name[1] <> '.' then
    Dest.Write(Name + ' = ');
  if BaseType.TypeInfo = TypeInfo then
  begin
    Dest.Write('(');
    Prefix := '';
    for I := MinValue to MaxValue do
    begin
      Dest.Write(Prefix + Names[I]);
      Prefix := ', ';
    end;
    Dest.Write(')');
  end
  else
    Dest.Write(Names[MinValue] + ' .. ' + Names[MaxValue]);
  if Name[1] <> '.' then
  begin
    Dest.Write('; // ' + JclEnumValueToIdent(System.TypeInfo(TOrdType),
      TypeData.OrdType));
    Dest.Writeln('');
  end;
end;

//--------------------------------------------------------------------------------------------------
// TJclSetTypeInfo
//--------------------------------------------------------------------------------------------------

type
  TJclSetTypeInfo = class(TJclOrdinalTypeInfo, IJclSetTypeInfo)
  protected
    function GetBaseType: IJclOrdinalTypeInfo;
    procedure GetAsList(const Value; const WantRanges: Boolean;
      const Strings: TStrings);
    procedure SetAsList(out Value; const Strings: TStrings);
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
  public
    property BaseType: IJclOrdinalTypeInfo read GetBaseType;
  end;

//--------------------------------------------------------------------------------------------------

function TJclSetTypeInfo.GetBaseType: IJclOrdinalTypeInfo;
begin
  Result := JclTypeInfo(TypeData.CompType^) as IJclOrdinalTypeInfo;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclSetTypeInfo.GetAsList(const Value; const WantRanges: Boolean;
  const Strings: TStrings);
var
  BaseInfo: IJclOrdinalRangeTypeInfo;
  FirstBit: Byte;
  LastBit: Byte;
  Bit: Byte;
  StartBit: Integer;

  procedure AddRange;
  var
    FirstOrdNum: Int64;
    LastOrdNum: Int64;
    OrdNum: Int64;
  begin
    FirstOrdNum := (StartBit - FirstBit) + BaseInfo.MinValue;
    LastOrdNum := (Bit - 1 - FirstBit) + BaseInfo.MinValue;
    if WantRanges and (LastOrdNum <> FirstOrdNum) then
    begin
      if BaseInfo.TypeKind = tkEnumeration then
        Strings.Add((BaseInfo as IJclEnumerationTypeInfo).Names[FirstOrdNum] +
          ' .. ' + (BaseInfo as IJclEnumerationTypeInfo).Names[LastOrdNum])
      else
        Strings.Add(IntToStr(FirstOrdNum) + ' .. ' + IntToStr(LastOrdNum));
    end
    else
    begin
      OrdNum := FirstOrdNum;
      while OrdNum <= LastOrdNum do
      begin
        if BaseInfo.TypeKind = tkEnumeration then
          Strings.Add((BaseInfo as IJclEnumerationTypeInfo).Names[OrdNum])
        else
          Strings.Add(IntToStr(OrdNum));
        Inc(OrdNum);
      end;
    end;
  end;

begin
  BaseInfo := BaseType as IJclOrdinalRangeTypeInfo;
  FirstBit := BaseInfo.MinValue mod 8;
  LastBit := BaseInfo.MaxValue - (BaseInfo.MinValue - FirstBit);
  Bit := FirstBit;
  StartBit := -1;
  Strings.BeginUpdate;
  try
    while Bit <= LastBit do
    begin
      if TestBitBuffer(Value, Bit) then
      begin
        if StartBit = -1 then
          StartBit := Bit;
      end
      else
      begin
        if StartBit <> -1 then
        begin
          AddRange;
          StartBit := -1;
        end;
      end;
      Inc(Bit);
    end;
    if StartBit <> -1 then
      AddRange;
  finally
    Strings.EndUpdate;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclSetTypeInfo.SetAsList(out Value; const Strings: TStrings);
var
  BaseInfo: IJclOrdinalRangeTypeInfo;
  FirstBit: Integer;
  I: Integer;
  FirstIdent: string;
  LastIdent: string;
  RangePos: Integer;
  FirstOrd: Int64;
  LastOrd: Int64;
  CurOrd: Integer;

  procedure ClearValue;
  var
    LastBit: Integer;
    ByteCount: Integer;
  begin
    LastBit := BaseInfo.MaxValue - BaseInfo.MinValue + 1 + FirstBit;
    ByteCount := (LastBit - FirstBit) div 8;
    if LastBit mod 8 <> 0 then
      Inc(ByteCount);
    FillChar(Value, ByteCount, 0);
  end;

begin
  BaseInfo := BaseType as IJclOrdinalRangeTypeInfo;
  FirstBit := BaseInfo.MinValue mod 8;
  ClearValue;
  Strings.BeginUpdate;
  try
  for I := 0 to Strings.Count - 1 do
    begin
      if Trim(Strings[I]) <> '' then
      begin
        FirstIdent := Trim(Strings[I]);
        RangePos := Pos('..', FirstIdent);
        if RangePos > 0 then
        begin
          LastIdent := Trim(StrRestOf(FirstIdent, RangePos + 2));
          FirstIdent := Trim(Copy(FirstIdent, 1, RangePos - 1));
        end
        else
          LastIdent := FirstIdent;
        if BaseInfo.TypeKind = tkEnumeration then
        begin
          FirstOrd := (BaseInfo as IJclEnumerationTypeInfo).IndexOfName(FirstIdent);
          LastOrd := (BaseInfo as IJclEnumerationTypeInfo).IndexOfName(LastIdent);
          if FirstOrd = -1 then
            raise EJclRTTI.CreateResRecFmt(@RsRTTIUnknownIdentifier, [FirstIdent]);
          if LastOrd = -1 then
            raise EJclRTTI.CreateResRecFmt(@RsRTTIUnknownIdentifier, [LastIdent]);
        end
        else
        begin
          FirstOrd := StrToInt(FirstIdent);
          LastOrd := StrToInt(LastIdent);
        end;
        Dec(FirstOrd, BaseInfo.MinValue);
        Dec(LastOrd, BaseInfo.MinValue);
        for CurOrd := FirstOrd to LastOrd do
          SetBitBuffer(Value, CurOrd + FirstBit);
      end;
    end;
  finally
    Strings.EndUpdate;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclSetTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  Dest.Writeln(RsRTTIBasedOn);
  Dest.Indent;
  try
    BaseType.WriteTo(Dest);
  finally
    Dest.Outdent;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclSetTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
var
  Base: IJclOrdinalTypeInfo;
  BaseEnum: IJclEnumerationTypeInfo;
begin
  if Name[1] <> '.' then
    Dest.Write(Name + ' = set of ');
  Base := BaseType;

  if Base.Name[1] = '.' then
  begin
    if Base.QueryInterface(IJclEnumerationTypeInfo, BaseEnum) = S_OK then
      BaseEnum.DeclarationTo(Dest)
    else
      Dest.Write(RsRTTITypeError);
  end
  else
    Dest.Write(Base.Name);
  if Name[1] <> '.' then
  begin
    Dest.Write('; // ' + JclEnumValueToIdent(System.TypeInfo(TOrdType),
      TypeData.OrdType));
    Dest.Writeln('');
  end;
end;

//--------------------------------------------------------------------------------------------------
// TJclFloatTypeInfo
//--------------------------------------------------------------------------------------------------

type
  TJclFloatTypeInfo = class(TJclTypeInfo, IJclFloatTypeInfo)
  protected
    function GetFloatType: TFloatType;
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
  public
    property FloatType: TFloatType read GetFloatType;
  end;

//--------------------------------------------------------------------------------------------------

function TJclFloatTypeInfo.GetFloatType: TFloatType;
begin
  Result := TypeData.FloatType;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFloatTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  Dest.Writeln(RsRTTIFloatType + JclEnumValueToIdent(
    System.TypeInfo(TFloatType), TypeData.FloatType));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclFloatTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
var
  S: string;
  FT: TFloatType;
begin
  FT := FloatType;
  S := StrRestOf(JclEnumValueToIdent(System.TypeInfo(TFloatType), FT), 3);
  Dest.Writeln(Name + ' = type ' + S + ';');
end;

//--------------------------------------------------------------------------------------------------
// TJclStringTypeInfo
//--------------------------------------------------------------------------------------------------

type
  TJclStringTypeInfo = class(TJclTypeInfo, IJclStringTypeInfo)
  protected
    function GetMaxLength: Integer;
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
  public
    property MaxLength: Integer read GetMaxLength;
  end;

//--------------------------------------------------------------------------------------------------

function TJclStringTypeInfo.GetMaxLength: Integer;
begin
  Result := TypeData.MaxLength;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  Dest.Writeln(RsRTTIMaxLen + IntToStr(MaxLength));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclStringTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
begin
  if Name[1] <> '.' then
    Dest.Write(Name + ' = ');
  Dest.Write('string[' + IntToStr(MaxLength) + ']');
  if Name[1] <> '.' then
    Dest.Writeln(';');
end;

//--------------------------------------------------------------------------------------------------
// TJclPropInfo
//--------------------------------------------------------------------------------------------------

type
  TJclPropInfo = class(TInterfacedObject, IJclPropInfo)
  private
    FPropInfo: PPropInfo;
  protected
    function GetPropInfo: PPropInfo;
    function GetPropType: IJclTypeInfo;
    function GetReader: Pointer;
    function GetWriter: Pointer;
    function GetStoredProc: Pointer;
    function GetIndex: Integer;
    function GetDefault: Longint;
    function GetNameIndex: Smallint;
    function GetName: string;
    function GetSpecKind(const Value: Integer): TJclPropSpecKind;
    function GetSpecValue(const Value: Integer): Integer;
    function GetReaderType: TJclPropSpecKind;
    function GetWriterType: TJclPropSpecKind;
    function GetStoredType: TJclPropSpecKind;
    function GetReaderValue: Integer;
    function GetWriterValue: Integer;
    function GetStoredValue: Integer;
  public
    constructor Create(const APropInfo: PPropInfo);
    function IsStored(const AInstance: TObject): Boolean;
    function HasDefault: Boolean;
    function HasIndex: Boolean;

    property PropInfo: PPropInfo read GetPropInfo;
    property PropType: IJclTypeInfo read GetPropType;
    property Reader: Pointer read GetReader;
    property Writer: Pointer read GetWriter;
    property StoredProc: Pointer read GetStoredProc;
    property ReaderType: TJclPropSpecKind read GetReaderType;
    property WriterType: TJclPropSpecKind read GetWriterType;
    property StoredType: TJclPropSpecKind read GetStoredType;
    property ReaderValue: Integer read GetReaderValue;
    property WriterValue: Integer read GetWriterValue;
    property StoredValue: Integer read GetStoredValue;
    property Index: Integer read GetIndex;
    property Default: Longint read GetDefault;
    property NameIndex: Smallint read GetNameIndex;
    property Name: string read GetName;
  end;

//--------------------------------------------------------------------------------------------------

constructor TJclPropInfo.Create(const APropInfo: PPropInfo);
begin
  inherited Create;
  FPropInfo := APropInfo;
end;

//--------------------------------------------------------------------------------------------------

function TJclPropInfo.GetPropInfo: PPropInfo;
begin
  Result := FPropInfo;
end;

//--------------------------------------------------------------------------------------------------

function TJclPropInfo.GetPropType: IJclTypeInfo;
begin
  Result := JclTypeInfo(PropInfo.PropType^);
end;

//--------------------------------------------------------------------------------------------------

function TJclPropInfo.GetReader: Pointer;
begin
  Result := PropInfo.GetProc;
end;

//--------------------------------------------------------------------------------------------------

function TJclPropInfo.GetWriter: Pointer;
begin
  Result := PropInfo.SetProc;
end;

//--------------------------------------------------------------------------------------------------

function TJclPropInfo.GetStoredProc: Pointer;
begin
  Result := PropInfo.StoredProc;
end;

//--------------------------------------------------------------------------------------------------

function TJclPropInfo.GetIndex: Integer;
begin
  Result := PropInfo.Index;
end;

//--------------------------------------------------------------------------------------------------

function TJclPropInfo.GetDefault: Longint;
begin
  Result := PropInfo.Default;
end;

//--------------------------------------------------------------------------------------------------

function TJclPropInfo.GetNameIndex: Smallint;
begin
  Result := PropInfo.NameIndex;
end;

//--------------------------------------------------------------------------------------------------

function TJclPropInfo.GetName: string;
begin
  Result := PropInfo.Name;
end;

//--------------------------------------------------------------------------------------------------

function TJclPropInfo.GetSpecKind(const Value: Integer): TJclPropSpecKind;
var
  P: Integer;

begin
  P := Value shr 24;
  case P of
    $00:
      if Value < 2 then
        Result := pskConstant
      else
        Result := pskStaticMethod;
    $FE:
      Result := pskVirtualMethod;
    $FF:
      Result := pskField;
  else
    Result := pskStaticMethod;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclPropInfo.GetSpecValue(const Value: Integer): Integer;
begin
  case GetSpecKind(Value) of
    pskStaticMethod, pskConstant:
      Result := Value;
    pskVirtualMethod:
      Result := Smallint(Value and $0000FFFF);
    pskField:
      Result := Value and $00FFFFFF;
  else
    Result := 0;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclPropInfo.GetReaderType: TJclPropSpecKind;
begin
  Result := GetSpecKind(Integer(Reader));
end;

//--------------------------------------------------------------------------------------------------

function TJclPropInfo.GetWriterType: TJclPropSpecKind;
begin
  Result := GetSpecKind(Integer(Writer));
end;

//--------------------------------------------------------------------------------------------------

function TJclPropInfo.GetStoredType: TJclPropSpecKind;
begin
  Result := GetSpecKind(Integer(StoredProc));
end;

//--------------------------------------------------------------------------------------------------

function TJclPropInfo.GetReaderValue: Integer;
begin
  Result := GetSpecValue(Integer(Reader));
end;

//--------------------------------------------------------------------------------------------------

function TJclPropInfo.GetWriterValue: Integer;
begin
  Result := GetSpecValue(Integer(Writer));
end;

//--------------------------------------------------------------------------------------------------

function TJclPropInfo.GetStoredValue: Integer;
begin
  Result := GetSpecValue(Integer(StoredProc));
end;

//--------------------------------------------------------------------------------------------------

function TJclPropInfo.IsStored(const AInstance: TObject): Boolean;
begin
  Result := IsStoredProp(AInstance, FPropInfo);
end;

//--------------------------------------------------------------------------------------------------

function TJclPropInfo.HasDefault: Boolean;
begin
  Result := Longword(Default) <> $80000000;
end;

//--------------------------------------------------------------------------------------------------

function TJclPropInfo.HasIndex: Boolean;
begin
  Result := Longword(Index) <> $80000000;
end;

//--------------------------------------------------------------------------------------------------
// TJclClassTypeInfo
//--------------------------------------------------------------------------------------------------

type
  TJclClassTypeInfo = class(TJclTypeInfo, IJclClassTypeInfo)
  private
  protected
    function GetClassRef: TClass;
    function GetParent: IJclClassTypeInfo;
    function GetTotalPropertyCount: Integer;
    function GetPropertyCount: Integer;
    function GetProperties(const PropIdx: Integer): IJclPropInfo;
    function GetUnitName: string;
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
  public
    property ClassRef: TClass read GetClassRef;
    property Parent: IJclClassTypeInfo read GetParent;
    property TotalPropertyCount: Integer read GetTotalPropertyCount;
    property PropertyCount: Integer read GetPropertyCount;
    property Properties[const PropIdx: Integer]: IJclPropInfo
      read GetProperties;
    property UnitName: string read GetUnitName;
  end;

//--------------------------------------------------------------------------------------------------

function TJclClassTypeInfo.GetClassRef: TClass;
begin
  Result := TypeData.ClassType;
end;

//--------------------------------------------------------------------------------------------------

function TJclClassTypeInfo.GetParent: IJclClassTypeInfo;
begin
  if (TypeData.ParentInfo <> nil) and (TypeData.ParentInfo^ <> nil) then
    Result := JclTypeInfo(TypeData.ParentInfo^) as IJclClassTypeInfo
  else
    Result := nil;
end;

//--------------------------------------------------------------------------------------------------

function TJclClassTypeInfo.GetTotalPropertyCount: Integer;
begin
  Result := TypeData.PropCount;
end;

//--------------------------------------------------------------------------------------------------

function TJclClassTypeInfo.GetPropertyCount: Integer;
var
  PropData: ^TPropData;
begin
  PropData := @TypeData.UnitName;
  Inc(Integer(PropData), 1 + Length(UnitName));
  Result := PropData.PropCount;
end;

//--------------------------------------------------------------------------------------------------

function TJclClassTypeInfo.GetProperties(const PropIdx: Integer): IJclPropInfo;
var
  PropData: ^TPropData;
  Prop: PPropInfo;
  Idx: Integer;
  RecSize: Integer;
begin
  PropData := @TypeData.UnitName;
  Inc(Integer(PropData), 1 + Length(UnitName));
  if PropIdx + 1 > PropData.PropCount then
    Result := Parent.Properties[PropIdx - PropData.PropCount]
  else
  begin
    Prop := PPropInfo(PropData);
    Inc(Integer(Prop), 2);
    if PropIdx > 0 then
    begin
      RecSize := SizeOf(TPropInfo) - SizeOf(ShortString);
      Idx := PropIdx;
      while Idx > 0 do
      begin
        Inc(Integer(Prop), RecSize);
        Inc(Integer(Prop), 1 + PByte(Prop)^);
        Dec(Idx);
      end;
    end;
    Result := TJclPropInfo.Create(Prop);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclClassTypeInfo.GetUnitName: string;
begin
  Result := TypeData^.UnitName;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclClassTypeInfo.WriteTo(const Dest: IJclInfoWriter);
var
  I: Integer;
  Prop: IJclPropInfo;
begin
  inherited WriteTo(Dest);
  Dest.Writeln(RsRTTIClassName + ClassRef.ClassName);
  Dest.Writeln(RsRTTIParent + Parent.ClassRef.ClassName);
  Dest.Writeln(RsRTTIUnitName + UnitName);
  Dest.Writeln(RsRTTIPropCount + IntToStr(PropertyCount) + ' (' +
    IntToStr(TotalPropertyCount) + ')');
  Dest.Indent;
  try
    for I := 0 to PropertyCount-1 do
    begin
      Prop := Properties[I];
      Dest.Writeln(Prop.Name + ': ' + Prop.PropType.Name);
      Dest.Indent;
      try
        if Prop.HasIndex then
          Dest.Writeln(Format('[%s %d]', [RsRTTIIndex, Prop.Index]));
        if Prop.HasDefault then
          Dest.Writeln(Format('[%s %d]', [RsRTTIDefault, Prop.Default]));
        case Prop.ReaderType of
          pskStaticMethod:
            Dest.Writeln(Format('[%s %s $%p]',
              [RsRTTIPropRead, RsRTTIStaticMethod,
               Pointer(Prop.ReaderValue)]));
          pskField:
            Dest.Writeln(Format('[%s %s $%p]',
              [RsRTTIPropRead, RsRTTIField,
               Pointer(Prop.ReaderValue)]));
          pskVirtualMethod:
            Dest.Writeln(Format('[%s %s $%p]',
              [RsRTTIPropRead, RsRTTIVirtualMethod,
               Pointer(Prop.ReaderValue)]));
        end;
        case Prop.WriterType of
          pskStaticMethod:
            Dest.Writeln(Format('[%s %s $%p]',
              [RsRTTIPropWrite, RsRTTIStaticMethod,
               Pointer(Prop.WriterValue)]));
          pskField:
            Dest.Writeln(Format('[%s %s $%p]',
              [RsRTTIPropWrite, RsRTTIField,
               Pointer(Prop.WriterValue)]));
          pskVirtualMethod:
            Dest.Writeln(Format('[%s %s $%p]',
              [RsRTTIPropWrite, RsRTTIVirtualMethod,
               Pointer(Prop.WriterValue)]));
        end;
        case Prop.StoredType of
          pskConstant:
            if Boolean(Prop.StoredValue) then
              Dest.Writeln(Format('[%s=%s]',
                [RsRTTIPropStored, RsRTTITrue]))
            else
              Dest.Writeln(Format('[%s=%s]',
                [RsRTTIPropStored, RsRTTIFalse]));
          pskStaticMethod:
            Dest.Writeln(Format('[%s=%s $%p]',
              [RsRTTIPropStored, RsRTTIStaticMethod,
               Pointer(Prop.StoredValue)]));
          pskField:
            Dest.Writeln(Format('[%s=%s $%p]',
              [RsRTTIPropStored, RsRTTIField,
               Pointer(Prop.StoredValue)]));
          pskVirtualMethod:
            Dest.Writeln(Format('[%s=%s $%p]',
              [RsRTTIPropStored, RsRTTIVirtualMethod,
               Pointer(Prop.StoredValue)]));
        end;
      finally
        Dest.Outdent;
      end;
    end;
  finally
    Dest.Outdent;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclClassTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
var
  IntfTbl: PInterfaceTable;
  I: Integer;
  Prop: IJclPropInfo;
begin
  if (Parent <> nil) and not AnsiSameText(Parent.Name, 'TObject') then
  begin
    Dest.Write(Name + ' = class(' + Parent.Name);
    IntfTbl := ClassRef.GetInterfaceTable;
    if IntfTbl <> nil then
      for I := 0 to IntfTbl.EntryCount-1 do
        Dest.Write(', [''' + JclGUIDToString(IntfTbl.Entries[I].IID) + ''']');
    Dest.Writeln(') // unit ' + UnitName);
  end
  else
    Dest.Writeln(Name + ' = class // unit ' + UnitName);
  if PropertyCount > 0 then
  begin
    Dest.Writeln('published');
    Dest.Indent;
    try
      for I := 0 to PropertyCount-1 do
      begin
        Prop := Properties[I];
        Dest.Write('property ' + Prop.Name + ': ' +  Prop.PropType.Name);
        if Prop.HasIndex then
          Dest.Write(Format(' index %d', [Prop.Index]));

        case Prop.ReaderType of
          pskStaticMethod:
            Dest.Write(Format(' read [static method $%p]',
              [Pointer(Prop.ReaderValue)]));
          pskField:
            Dest.Write(Format(' read [field $%p]',
              [Pointer(Prop.ReaderValue)]));
          pskVirtualMethod:
            Dest.Write(Format(' read [virtual method $%p]',
              [Pointer(Prop.ReaderValue)]));
        end;

        case Prop.WriterType of
          pskStaticMethod:
            Dest.Write(Format(' write [static method $%p]',
              [Pointer(Prop.WriterValue)]));
          pskField:
            Dest.Write(Format(' write [field $%p]',
              [Pointer(Prop.WriterValue)]));
          pskVirtualMethod:
            Dest.Write(Format(' write [virtual method $%p]',
              [Pointer(Prop.WriterValue)]));
        end;

        case Prop.StoredType of
          pskConstant:
            if Boolean(Prop.StoredValue) then
              Dest.Write(' stored = True')
            else
              Dest.Write(' stored = False');
          pskStaticMethod:
            Dest.Write(Format(' stored = [static method $%p]',
              [Pointer(Prop.StoredValue)]));
          pskField:
            Dest.Write(Format(' stored = [field $%p]',
              [Pointer(Prop.StoredValue)]));
          pskVirtualMethod:
            Dest.Write(Format(' stored = [virtual method $%p]',
              [Pointer(Prop.StoredValue)]));
        end;
        if Prop.HasDefault then
          Dest.Write(' default ' + IntToStr(Prop.Default));
        Dest.Writeln(';');
      end;
    finally
      Dest.Outdent;
    end;
  end;
  Dest.Writeln('end;');
end;

//--------------------------------------------------------------------------------------------------
// TJclEventParamInfo
//--------------------------------------------------------------------------------------------------

type
  TJclEventParamInfo = class(TInterfacedObject, IJclEventParamInfo)
  private
    FParam: Pointer;
  protected
    function GetFlags: TParamFlags;
    function GetName: string;
    function GetRecSize: Integer;
    function GetTypeName: string;
    function GetParam: Pointer;
  public
    constructor Create(const AParam: Pointer);

    property Flags: TParamFlags read GetFlags;
    property Name: string read GetName;
    property RecSize: Integer read GetRecSize;
    property TypeName: string read GetTypeName;
    property Param: Pointer read GetParam;
  end;

//--------------------------------------------------------------------------------------------------

constructor TJclEventParamInfo.Create(const AParam: Pointer);
begin
  inherited Create;
  FParam := AParam;
end;

//--------------------------------------------------------------------------------------------------

function TJclEventParamInfo.GetFlags: TParamFlags;
begin
  Result := TParamFlags(PByte(Param)^);
end;

//--------------------------------------------------------------------------------------------------

function TJclEventParamInfo.GetName: string;
var
  PName: PShortString;
begin
  PName := Param;
  Inc(Integer(PName));
  Result := PName^;
end;

//--------------------------------------------------------------------------------------------------

function TJclEventParamInfo.GetRecSize: Integer;
begin
  Result := 3 + Length(Name) + Length(TypeName);
end;

//--------------------------------------------------------------------------------------------------

function TJclEventParamInfo.GetTypeName: string;
var
  PName: PShortString;
begin
  PName := Param;
  Inc(Integer(PName));
  Inc(Integer(PName), PByte(PName)^ + 1);
  Result := PName^;
end;

//--------------------------------------------------------------------------------------------------

function TJclEventParamInfo.GetParam: Pointer;
begin
  Result := FParam;
end;

//--------------------------------------------------------------------------------------------------
// TJclEventTypeInfo
//--------------------------------------------------------------------------------------------------

type
  TJclEventTypeInfo = class(TJclTypeInfo, IJclEventTypeInfo)
  protected
    function GetMethodKind: TMethodKind;
    function GetParameterCount: Integer;
    function GetParameters(const ParamIdx: Integer): IJclEventParamInfo;
    function GetResultTypeName: string;
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
  public
    property MethodKind: TMethodKind read GetMethodKind;
    property ParameterCount: Integer read GetParameterCount;
    property Parameters[const ParamIdx: Integer]: IJclEventParamInfo
      read GetParameters;
    property ResultTypeName: string read GetResultTypeName;
  end;

//--------------------------------------------------------------------------------------------------

function TJclEventTypeInfo.GetMethodKind: TMethodKind;
begin
  Result := TypeData.MethodKind;
end;

//--------------------------------------------------------------------------------------------------

function TJclEventTypeInfo.GetParameterCount: Integer;
begin
  Result := TypeData.ParamCount;
end;

//--------------------------------------------------------------------------------------------------

function TJclEventTypeInfo.GetParameters(
  const ParamIdx: Integer): IJclEventParamInfo;
var
  I: Integer;
  Param: Pointer;
begin
  Param := @TypeData.ParamList[0];
  I := ParamIdx;
  Result := nil;
  while I >= 0 do
  begin
    Result := TJclEventParamInfo.Create(Param);
    Inc(Integer(Param), Result.RecSize);
    Dec(I);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclEventTypeInfo.GetResultTypeName: string;
var
  LastParam: IJclEventParamInfo;
  ResPtr: PShortString;
begin
  if MethodKind = mkFunction then
  begin
    if ParameterCount > 0 then
    begin
      LastParam := Parameters[ParameterCount-1];
      ResPtr := Pointer(Longint(LastParam.Param) + LastParam.RecSize);
    end
    else
      ResPtr := @TypeData.ParamList[0];
    Result := ResPtr^;
  end
  else
    Result := '';
end;

//--------------------------------------------------------------------------------------------------

procedure TJclEventTypeInfo.WriteTo(const Dest: IJclInfoWriter);
var
  I: Integer;
  Param: IJclEventParamInfo;
  ParamFlags: TParamFlags;
begin
  inherited WriteTo(Dest);
  Dest.Writeln(RsRTTIMethodKind + JclEnumValueToIdent(
    System.TypeInfo(TMethodKind), TypeData.MethodKind));
  Dest.Writeln(RsRTTIParamCount + IntToStr(ParameterCount));
  Dest.Indent;
  try
    for I := 0 to ParameterCount-1 do
    begin
      if I > 0 then
        Dest.Writeln('');
      Param := Parameters[I];
      ParamFlags := Param.Flags;
      Dest.Writeln(RsRTTIName + Param.Name);
      Dest.Writeln(RsRTTIType + Param.TypeName);
      Dest.Writeln(RsRTTIFlags + JclSetToStr(System.TypeInfo(TParamFlags),
        ParamFlags, True, False));
    end;
  finally
    Dest.Outdent;
  end;
  if MethodKind = mkFunction then
    Dest.Writeln(RsRTTIReturnType + ResultTypeName);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclEventTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
var
  Prefix: string;
  I: Integer;
  Param: IJclEventParamInfo;
begin
  Dest.Write(Name + ' = ');
  if MethodKind = mkFunction then
    Dest.Write('function')
  else
    Dest.Write('procedure');
  Prefix := '(';
  for I := 0 to ParameterCount-1 do
  begin
    Dest.Write(Prefix);
    Prefix := '; ';
    Param := Parameters[I];
    if pfVar in Param.Flags then
      Dest.Write(RsRTTIVar)
    else
    if pfConst in Param.Flags then
      Dest.Write(RsRTTIConst)
    else
    if pfOut in Param.Flags then
      Dest.Write(RsRTTIOut);
    Dest.Write(Param.Name);
    if Param.TypeName <> '' then
    begin
      Dest.Write(': ');
      if pfArray in Param.Flags then
        Dest.Write(RsRTTIArrayOf);
      if AnsiSameText(Param.TypeName, 'TVarRec') and (pfArray in Param.Flags) then
        Dest.Write(TrimRight(RsRTTIConst))
      else
        Dest.Write(Param.TypeName);
    end;
  end;
  if ParameterCount <> 0 then
    Dest.Write(')');
  if MethodKind = mkFunction then
    Dest.Write(': ' + ResultTypeName);
  Dest.Writeln(' of object;');
end;

//--------------------------------------------------------------------------------------------------
// TJclInterfaceTypeInfo
//--------------------------------------------------------------------------------------------------

type
  TJclInterfaceTypeInfo = class(TJclTypeInfo, IJclInterfaceTypeInfo)
  protected
    function GetParent: IJclInterfaceTypeInfo;
    function GetFlags: TIntfFlagsBase;
    function GetGUID: TGUID;
    {$IFDEF COMPILER6_UP}
    function GetPropertyCount: Integer;
    {$ENDIF COMPILER6_UP}
    function GetUnitName: string;
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
  public
    property Parent: IJclInterfaceTypeInfo read GetParent;
    property Flags: TIntfFlagsBase read GetFlags;
    property GUID: TGUID read GetGUID;
    {$IFDEF COMPILER6_UP}
    property PropertyCount: Integer read GetPropertyCount;
    {$ENDIF COMPILER6_UP}
    property UnitName: string read GetUnitName;
  end;

//--------------------------------------------------------------------------------------------------

function TJclInterfaceTypeInfo.GetParent: IJclInterfaceTypeInfo;
begin
  if (TypeData.IntfParent <> nil) and (TypeData.IntfParent^ <> nil) then
    Result := JclTypeInfo(TypeData.IntfParent^) as IJclInterfaceTypeInfo
  else
    Result := nil;
end;

//--------------------------------------------------------------------------------------------------

function TJclInterfaceTypeInfo.GetFlags: TIntfFlagsBase;
begin
  Result := TypeData.IntfFlags;
end;

//--------------------------------------------------------------------------------------------------

const
  NullGUID: TGUID = '{00000000-0000-0000-0000-000000000000}';

function TJclInterfaceTypeInfo.GetGUID: TGUID;
begin
  if ifHasGuid in Flags then
    Result := TypeData.Guid
  else
    Result := NullGUID;
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF COMPILER6_UP}
function TJclInterfaceTypeInfo.GetPropertyCount: Integer;
var
  PropData: ^TPropData;
begin
  PropData := @TypeData.IntfUnit;
  Inc(Integer(PropData), 1 + Length(UnitName));
  Result := PropData.PropCount;
end;
{$ENDIF COMPILER6_UP}

//--------------------------------------------------------------------------------------------------

function TJclInterfaceTypeInfo.GetUnitName: string;
begin
  Result := TypeData.IntfUnit;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclInterfaceTypeInfo.WriteTo(const Dest: IJclInfoWriter);
var
  IntfFlags: TIntfFlagsBase;
begin
  inherited WriteTo(Dest);
  if ifHasGuid in Flags then
    Dest.Writeln(RsRTTIGUID + JclGuidToString(GUID));
  IntfFlags := Flags;
  Dest.Writeln(RsRTTIFlags + JclSetToStr(System.TypeInfo(TIntfFlagsBase),
    IntfFlags, True, False));
  Dest.Writeln(RsRTTIUnitName + UnitName);
  if Parent <> nil then
    Dest.Writeln(RsRTTIParent + Parent.Name);
  {$IFDEF COMPILER6_UP}
  Dest.Writeln(RsRTTIPropCount + IntToStr(PropertyCount));
  {$ENDIF COMPILER6_UP}
end;

//--------------------------------------------------------------------------------------------------

procedure TJclInterfaceTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
begin
  Dest.Write(Name + ' = ');
  if ifDispInterface in Flags then
    Dest.Write('dispinterface')
  else
    Dest.Write('interface');
  if (Parent <> nil) and not (ifDispInterface in Flags) and not
      AnsiSameText(Parent.Name, 'IUnknown') then
    Dest.Write('(' + Parent.Name + ')');
  Dest.Writeln(' // unit ' + UnitName);
  Dest.Indent;
  try
    if ifHasGuid in Flags then
      Dest.Writeln('[''' + JclGuidToString(GUID) + ''']');
  finally
    Dest.Outdent;
    Dest.Writeln('end;');
  end;
end;

//--------------------------------------------------------------------------------------------------
// TJclInt64TypeInfo
//--------------------------------------------------------------------------------------------------

type
  TJclInt64TypeInfo = class(TJclTypeInfo, IJclInt64TypeInfo)
  protected
    function GetMinValue: Int64;
    function GetMaxValue: Int64;
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
  public
    property MinValue: Int64 read GetMinValue;
    property MaxValue: Int64 read GetMaxValue;
  end;

//--------------------------------------------------------------------------------------------------

function TJclInt64TypeInfo.GetMinValue: Int64;
begin
  Result := TypeData.MinInt64Value;
end;

//--------------------------------------------------------------------------------------------------

function TJclInt64TypeInfo.GetMaxValue: Int64;
begin
  Result := TypeData.MaxInt64Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclInt64TypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  Dest.Writeln(RsRTTIMinValue + IntToStr(MinValue));
  Dest.Writeln(RsRTTIMaxValue + IntToStr(MaxValue));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclInt64TypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
begin
  Dest.Writeln(Name + ' = ' + IntToStr(MinValue) + ' .. ' + IntToStr(MaxValue) +
    ';');
end;

{$IFDEF COMPILER6_UP}
//--------------------------------------------------------------------------------------------------
// TJclDynArrayTypeInfo
//--------------------------------------------------------------------------------------------------

type
  TJclDynArrayTypeInfo = class(TJclTypeInfo, IJclDynArrayTypeInfo)
  private
  protected
    function GetElementSize: Longint;
    function GetElementType: IJclTypeInfo;
    function GetElementsNeedCleanup: Boolean;
    function GetVarType: Integer;
    function GetUnitName: string;
    procedure WriteTo(const Dest: IJclInfoWriter); override;
    procedure DeclarationTo(const Dest: IJclInfoWriter); override;
  public
    property ElementSize: Longint read GetElementSize;
    property ElementType: IJclTypeInfo read GetElementType;
    property ElementsNeedCleanup: Boolean read GetElementsNeedCleanup;
    property VarType: Integer read GetVarType;
    property UnitName: string read GetUnitName;
  end;

function TJclDynArrayTypeInfo.GetElementSize: Longint;
begin
  Result := TypeData.elSize;
end;

//--------------------------------------------------------------------------------------------------

function TJclDynArrayTypeInfo.GetElementType: IJclTypeInfo;
begin
  if TypeData.elType = nil then
  begin
    if TypeData.elType2 <> nil then
      Result := JclTypeInfo(TypeData.elType2^)
    else
      Result := nil;
  end
  else
    Result := JclTypeInfo(TypeData.elType^);
end;

//--------------------------------------------------------------------------------------------------

function TJclDynArrayTypeInfo.GetElementsNeedCleanup: Boolean;
begin
  Result := TypeData.elType <> nil;
end;

//--------------------------------------------------------------------------------------------------

function TJclDynArrayTypeInfo.GetVarType: Integer;
begin
  Result := TypeData.varType;
end;

//--------------------------------------------------------------------------------------------------

function TJclDynArrayTypeInfo.GetUnitName: string;
begin
  Result := TypeData.DynUnitName;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDynArrayTypeInfo.WriteTo(const Dest: IJclInfoWriter);
begin
  inherited WriteTo(Dest);
  Dest.Writeln(RsRTTIElSize + IntToStr(ElementSize));
  if ElementType = nil then
    Dest.Writeln(RsRTTIElType + RsRTTITypeError)
  else
  if ElementType.Name[1] <> '.' then
    Dest.Writeln(RsRTTIElType + ElementType.Name)
  else
  begin
    Dest.Writeln(RsRTTIElType);
    Dest.Indent;
    try
      ElementType.WriteTo(Dest);
    finally
      Dest.Outdent;
    end;
  end;
  Dest.Write(RsRTTIElNeedCleanup);
  if ElementsNeedCleanup then
    Dest.Writeln(RsRTTITrue)
  else
    Dest.Writeln(RsRTTIFalse);
  Dest.Writeln(RsRTTIVarType + IntToStr(VarType));
  Dest.Writeln(RsRTTIUnitName + UnitName);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDynArrayTypeInfo.DeclarationTo(const Dest: IJclInfoWriter);
begin
  if Name[1] <> '.' then
    Dest.Write(Name + ' = ' + RsRTTIArrayOf)
  else
    Dest.Write(RsRTTIArrayOf);
  if ElementType = nil then
    Dest.Write(RsRTTITypeError)
  else
  if ElementType.Name[1] = '.' then
    ElementType.DeclarationTo(Dest)
  else
    Dest.Write(ElementType.Name);
  if Name[1] <> '.' then
    Dest.Writeln('; // Unit ' + UnitName);
end;
{$ENDIF COMPILER6_UP}

//--------------------------------------------------------------------------------------------------
// Typeinfo retreival
//--------------------------------------------------------------------------------------------------

function JclTypeInfo(ATypeInfo: PTypeInfo): IJclTypeInfo;
begin
  case ATypeInfo.Kind of
    tkInteger, tkChar, tkWChar:
      Result := TJclOrdinalRangeTypeInfo.Create(ATypeInfo);
    tkEnumeration:
      Result := TJclEnumerationTypeInfo.Create(ATypeInfo);
    tkSet:
      Result := TJclSetTypeInfo.Create(ATypeInfo);
    tkFloat:
      Result := TJclFloatTypeInfo.Create(ATypeInfo);
    tkString:
      Result := TJclStringTypeInfo.Create(ATypeInfo);
    tkClass:
      Result := TJclClassTypeInfo.Create(ATypeInfo);
    tkMethod:
      Result := TJclEventTypeInfo.Create(ATypeInfo);
    tkInterface:
      Result := TJclInterfaceTypeInfo.Create(ATypeInfo);
    tkInt64:
      Result := TJclInt64TypeInfo.Create(ATypeInfo);
{$IFDEF COMPILER6_UP}
    tkDynArray:
      Result := TJclDynArrayTypeInfo.Create(ATypeInfo);
{$ENDIF COMPILER6_UP}
  else
    Result := TJclTypeInfo.Create(ATypeInfo);
  end;
end;

//--------------------------------------------------------------------------------------------------
// User generated type info managment
//--------------------------------------------------------------------------------------------------

var
  TypeList: TThreadList;

type
  PTypeItem = ^TTypeItem;
  TTypeItem = record
    TypeInfo: PTypeInfo;
    RefCount: Integer;
  end;

//--------------------------------------------------------------------------------------------------

procedure FreeTypeData(const TypeInfo: PTypeInfo);
var
  TD: PTypeData;
begin
  TD := GetTypeData(TypeInfo);
  if TypeInfo.Kind = tkSet then
    RemoveTypeInfo(TD^.CompType^)
  else
  if (TypeInfo.Kind = tkEnumeration) and (TD^.BaseType^ <> TypeInfo) then
    RemoveTypeInfo(GetTypeData(TypeInfo)^.BaseType^);
  FreeMem(GetTypeData(TypeInfo)^.BaseType);
  FreeMem(TypeInfo);
end;

//--------------------------------------------------------------------------------------------------

procedure AddType(const TypeInfo: PTypeInfo);
var
  Item: PTypeItem;
begin
  New(Item);
  try
    Item.TypeInfo := TypeInfo;
    Item.RefCount := 1;
    TypeList.Add(Item);
  except
    Dispose(Item);
    raise;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure DeleteType(const TypeItem: PTypeItem);
begin
  FreeTypeData(TypeItem.TypeInfo);
  TypeList.Remove(TypeItem);
  Dispose(TypeItem);
end;

//--------------------------------------------------------------------------------------------------

procedure DoRefType(const TypeInfo: PTypeInfo; Add: Integer);
var
  I: Integer;
  List: TList;
begin
  List := TypeList.LockList;
  try
    I := List.Count-1;
    while (I >= 0) and (PTypeItem(List[I]).TypeInfo <> TypeInfo) do
      Dec(I);
    if I > -1 then
      Inc(PTypeItem(List[I]).RefCount, Add);
  finally
    TypeList.UnlockList;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure ReferenceType(const TypeInfo: PTypeInfo);
begin
  DoRefType(TypeInfo, 1);
end;

//--------------------------------------------------------------------------------------------------

procedure DeReferenceType(const TypeInfo: PTypeInfo);
begin
  DoRefType(TypeInfo, -1);
end;

//--------------------------------------------------------------------------------------------------

procedure ClearInfoList;
var
  L: TList;
begin
  L := TypeList.LockList;
  try
    while L.Count > 0 do
      RemoveTypeInfo(PTypeItem(L[L.Count-1])^.TypeInfo);
  finally
    TypeList.UnlockList;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure NewInfoItem(const TypeInfo: PTypeInfo);
begin
  TypeList.Add(TypeInfo);
end;

//--------------------------------------------------------------------------------------------------

procedure RemoveTypeInfo(TypeInfo: PTypeInfo);
var
  I: Integer;
  List: TList;
  Item: PTypeItem;
begin
  Item := nil;
  List := TypeList.LockList;
  try
    I := List.Count-1;
    while (I >= 0) and (PTypeItem(List[I]).TypeInfo <> TypeInfo) do
      Dec(I);
    if I > -1 then
      Item := PTypeItem(List[I]);
  finally
    TypeList.UnlockList;
  end;
  if Item <> nil then
  begin
    Dec(Item.RefCount);
    if Item.RefCount <= 0 then
      DeleteType(Item);
  end;
end;

//--------------------------------------------------------------------------------------------------
// Enumerations
//--------------------------------------------------------------------------------------------------

function JclEnumValueToIdent(TypeInfo: PTypeInfo; const Value): string;
var
  MinEnum: Integer;
  MaxEnum: Integer;
  EnumVal: Int64;
  OrdType: TOrdType;
begin
  OrdType := GetTypeData(TypeInfo).OrdType;
  MinEnum := GetTypeData(TypeInfo).MinValue;
  MaxEnum := GetTypeData(TypeInfo).MaxValue;
  case OrdType of
    otSByte:
      EnumVal := Smallint(Value);
    otUByte:
      EnumVal := Byte(Value);
    otSWord:
      EnumVal := Shortint(Value);
    otUWord:
      EnumVal := Word(Value);
    otSLong:
      EnumVal := Integer(Value);
    otULong:
      EnumVal := Longword(Value);
  else
    EnumVal := 0;
  end;
  // Check range...
  if (EnumVal < MinEnum) or (EnumVal > MaxEnum) then
    Result := Format(LoadResString(@RsRTTIValueOutOfRange),
      [LoadResString(@RsRTTIOrdinal) + IntToStr(EnumVal)])
  else
    Result := GetEnumName(TypeInfo, EnumVal);
end;

//--------------------------------------------------------------------------------------------------

function JclGenerateEnumType(const TypeName: ShortString;
  const Literals: array of string): PTypeInfo;
type
  PInteger = ^Integer;

var
  StringSize: Integer;
  I: Integer;
  TypeData: PTypeData;
  CurName: PShortString;
begin
  StringSize := 0;
  for I := Low(Literals) to High(Literals) do
    StringSize := StringSize + 1 + Length(Literals[I]);
  Result := AllocMem(SizeOf(TTypeInfo) + SizeOf(TOrdType) +
    (2*SizeOf(Integer)) + SizeOf(PPTypeInfo) +
    StringSize {$IFDEF COMPILER6_UP}+ 1{$ENDIF COMPILER6_UP});
  try
    with Result^ do
    begin
      Kind := tkEnumeration;
      Name := TypeName;
    end;
    TypeData := GetTypeData(Result);
    TypeData^.BaseType := AllocMem(SizeOf(Pointer));
    if Length(Literals) < 256 then
      TypeData^.OrdType := otUByte
    else
    if Length(Literals) < 65536 then
      TypeData^.OrdType := otUWord
    else
      TypeData^.OrdType := otULong;
    TypeData^.MinValue := 0;
    TypeData^.MaxValue := Length(Literals)-1;
    TypeData^.BaseType^ := Result;   // No sub-range: basetype points to itself
    CurName := @TypeData^.NameList;
    for I := Low(Literals) to High(Literals) do
    begin
      CurName^ := Literals[I];
      Inc(Integer(CurName), Length(Literals[I])+1);
    end;
    {$IFDEF COMPILER6_UP}
    CurName^ := ''; // Unit name unknown
    {$ENDIF COMPILER6_UP}
    AddType(Result);
  except
    try
      ReallocMem(Result, 0);
    except
      Result := nil;
    end;
    raise;
  end;
end;

//--------------------------------------------------------------------------------------------------

function JclGenerateEnumTypeBasedOn(const TypeName: ShortString;
  BaseType: PTypeInfo; const PrefixCut: Byte): PTypeInfo;
var
  BaseInfo: IJclTypeInfo;
  BaseKind: TTypeKind;
  Literals: array of string;
  I: Integer;
  S: string;
begin
  BaseInfo := JclTypeInfo(BaseType);
  BaseKind := BaseInfo.TypeKind;
  if BaseInfo.TypeKind <> tkEnumeration then
    raise EJclRTTI.CreateResRecFmt(@RsRTTIInvalidBaseType, [BaseInfo.Name,
      JclEnumValueToIdent(System.TypeInfo(TTypeKind), BaseKind)]);
  with BaseInfo as IJclEnumerationTypeInfo do
  begin
    SetLength(Literals, MaxValue - MinValue + 1);
    for I := MinValue to MaxValue do
    begin
      S := Names[I];
      if PrefixCut = PREFIX_CUT_LOWERCASE then
        while (Length(S) > 0) and (S[1] in ['a' .. 'z']) do
          Delete(S, 1, 1);
      if (PrefixCut > 0) and (PrefixCut < MaxPrefixCut) then
        Delete(S, 1, PrefixCut);
      if S = '' then
        S := Names[I];
      Literals[I- MinValue] := S;
    end;
    if PrefixCut = PREFIX_CUT_EQUAL then
    begin
      S := Literals[High(Literals)];
      I := High(Literals)-1;
      while (I >= 0) and (S > '') do
      begin
        while Copy(Literals[I], 1, Length(S)) <> S do
          Delete(S, Length(S), 1);
        Dec(I);
      end;
      if S > '' then
        for I := Low(Literals) to High(Literals) do
        begin
          Literals[I] := StrRestOf(Literals[I], Length(S));
          if Literals[I] = '' then
            Literals[I] := Names[I + MinValue];
        end;
    end;
  end;
  Result := JclGenerateEnumType(TypeName, Literals);
end;

//--------------------------------------------------------------------------------------------------

function JclGenerateSubRange(BaseType: PTypeInfo; const TypeName: string;
  const MinValue, MaxValue: Integer): PTypeInfo;
var
  TypeData: PTypeData;
begin
  Result := AllocMem(SizeOf(TTypeInfo) + SizeOf(TOrdType) +
    (2*SizeOf(Integer)) + SizeOf(PPTypeInfo));
  try
    with Result^ do
    begin
      Kind := BaseType^.Kind;
      Name := TypeName;
    end;
    TypeData := GetTypeData(Result);
    TypeData^.OrdType := GetTypeData(BaseType)^.OrdType;
    TypeData^.MinValue := MinValue;
    TypeData^.MaxValue := MaxValue;
    TypeData^.BaseType := AllocMem(SizeOf(Pointer));
    TypeData^.BaseType^ := BaseType;
    AddType(Result);
  except
    try
      ReallocMem(Result, 0);
    except
      Result := nil;
    end;
    raise;
  end;
  ReferenceType(BaseType);
end;

//--------------------------------------------------------------------------------------------------
// Integers
//--------------------------------------------------------------------------------------------------

function JclStrToTypedInt(Value: string; TypeInfo: PTypeInfo): Integer;
var
  Conv: TIdentToInt;
  HaveConversion: Boolean;
  Info: IJclTypeInfo;
  RangeInfo: IJclOrdinalRangeTypeInfo;
  TmpVal: Int64;
begin
  if TypeInfo <> nil then
    Conv := FindIdentToInt(TypeInfo)
  else
    Conv := nil;
  HaveConversion := (@Conv <> nil) and Conv(Value, Result);
  if not HaveConversion then
  begin
    if TypeInfo <> nil then
    begin
      Info := JclTypeInfo(TypeInfo);
      if Info.QueryInterface(IJclOrdinalRangeTypeInfo, RangeInfo) <> S_OK then
        RangeInfo := nil;
      TmpVal := StrToInt64(Value);
      if (RangeInfo <> nil) and ((TmpVal < RangeInfo.MinValue) or
          (TmpVal > RangeInfo.MaxValue)) then
        raise EConvertError.CreateFmt(LoadResString(@SInvalidInteger), [Value]);
      Result := Integer(TmpVal);
    end
    else
      Result := StrToInt(Value)
  end;
end;

//--------------------------------------------------------------------------------------------------

function JclTypedIntToStr(Value: Integer; TypeInfo: PTypeInfo): string;
var
  Conv: TIntToIdent;
  HaveConversion: Boolean;
begin
  if TypeInfo <> nil then
    Conv := FindIntToIdent(TypeInfo)
  else
    Conv := nil;
  HaveConversion := (@Conv <> nil) and Conv(Value, Result);
  if not HaveConversion then
  begin
    if (TypeInfo <> nil) and (GetTypeData(TypeInfo).OrdType = otULong) then
      Result := IntToStr(Int64(Cardinal(Value)))
    else
      Result := IntToStr(Value)
  end;
end;

//--------------------------------------------------------------------------------------------------
// Sets
//--------------------------------------------------------------------------------------------------

function JclSetToList(TypeInfo: PTypeInfo; const Value;
  const WantBrackets: Boolean; const WantRanges: Boolean;
  const Strings: TStrings): string;
var
  SetType: IJclSetTypeInfo;
  I: Integer;
begin
  I := Strings.Count;
  Result := '';
  SetType := JclTypeInfo(TypeInfo) as IJclSetTypeInfo;
  SetType.GetAsList(Value, WantRanges, Strings);
  for I := I to Strings.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ', ' + Strings[I]
    else
      Result := Result + Strings[I];
  end;
  if WantBrackets then
    Result := '[' + Result + ']';
end;

//--------------------------------------------------------------------------------------------------

function JclSetToStr(TypeInfo: PTypeInfo; const Value;
  const WantBrackets: Boolean; const WantRanges: Boolean): string;
var
  Dummy: TStringList;
begin
  Dummy := TStringList.Create;
  try
    Result := JclSetToList(TypeInfo, Value, WantBrackets, WantRanges, Dummy);
  finally
    Dummy.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure JclStrToSet(TypeInfo: PTypeInfo; var SetVar; const Value: string);
var
  SetInfo: IJclSetTypeInfo;
  S: TStringList;
begin
  SetInfo := JclTypeInfo(TypeInfo) as IJclSetTypeInfo;
  S := TStringList.Create;
  try
    StrToStrings(Value, ',', S);
    if S.Count > 0 then
    begin
      if S[0][1] = '[' then
      begin
        S[0] := Copy(S[0], 2, Length(S[0]));
        S[S.Count-1] := Copy(S[S.Count-1], 1,
          Length(S[S.Count-1]) - 1);
      end;
    end;
    SetInfo.SetAsList(SetVar, S);
  finally
    S.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure JclIntToSet(TypeInfo: PTypeInfo; var SetVar;
  const Value: Integer);
var
  BitShift: Integer;
  TmpInt64: Int64;
  EnumMin: Integer;
  EnumMax: Integer;
  ResBytes: Integer;
  CompType: PTypeInfo;
begin
  CompType := GetTypeData(TypeInfo).CompType^;
  EnumMin := GetTypeData(CompType).MinValue;
  EnumMax := GetTypeData(CompType).MaxValue;
  ResBytes := (EnumMax div 8) - (EnumMin div 8) + 1;
  BitShift := EnumMin mod 8;
  TmpInt64 := Longword(Value) shl BitShift;
  Move(TmpInt64, SetVar, ResBytes);
end;

//--------------------------------------------------------------------------------------------------

function JclSetToInt(TypeInfo: PTypeInfo; const SetVar): Integer;
var
  BitShift: Integer;
  TmpInt64: Int64;
  EnumMin: Integer;
  EnumMax: Integer;
  ResBytes: Integer;
  CompType: PTypeInfo;
begin
  Result := 0;
  TmpInt64 := 0;
  CompType := GetTypeData(TypeInfo).CompType^;
  EnumMin := GetTypeData(CompType).MinValue;
  EnumMax := GetTypeData(CompType).MaxValue;
  ResBytes := (EnumMax div 8) - (EnumMin div 8) + 1;
  if (EnumMax - EnumMin) > 32 then
    raise EJclRTTI.CreateResRecFmt(@RsRTTIValueOutOfRange,
      [IntToStr(EnumMax - EnumMin) + ' ' + LoadResString(@RsRTTIBits)]);
  BitShift := EnumMin mod 8;
  Move(SetVar, TmpInt64, ResBytes + 1);
  TmpInt64 := TmpInt64 shr BitShift;
  Move(TmpInt64, Result, ResBytes);
end;

//--------------------------------------------------------------------------------------------------

function JclGenerateSetType(BaseType: PTypeInfo;
  const TypeName: ShortString): PTypeInfo;
var
  TypeData: PTypeData;
  ValCount: Integer;
begin
  Result := AllocMem(SizeOf(TTypeInfo) + SizeOf(TOrdType) + SizeOf(PPTypeInfo));
  try
    with Result^ do
    begin
      Kind := tkSet;
      Name := TypeName;
    end;
    with GetTypeData(BaseType)^ do
      ValCount := MaxValue - MinValue + (MinValue mod 8);
    TypeData := GetTypeData(Result);
    case ValCount of
      0..8:
        TypeData^.OrdType := otUByte;
      9..16:
        TypeData^.OrdType := otUWord;
      17..32:
        TypeData^.OrdType := otULong;
      33..64:
        Byte(TypeData^.OrdType) := 8;
      65..128:
        Byte(TypeData^.OrdType) := 16;
      129..256:
        Byte(TypeData^.OrdType) := 32;
    else
      Byte(TypeData^.OrdType) := 255;
    end;
    TypeData^.CompType := AllocMem(SizeOf(Pointer));
    TypeData^.CompType^ := BaseType;
    AddType(Result);
  except
    try
      ReallocMem(Result, 0);
    except
      Result := nil;
    end;
    raise;
  end;
  ReferenceType(BaseType);
end;

//--------------------------------------------------------------------------------------------------
// GUID
//--------------------------------------------------------------------------------------------------

function JclGUIDToString(const GUID: TGUID): string;
begin
  Result := Format('{%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x}',
    [GUID.D1, GUID.D2, GUID.D3, GUID.D4[0], GUID.D4[1], GUID.D4[2],
     GUID.D4[3], GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]]);
end;

//--------------------------------------------------------------------------------------------------

function JclStringToGUID(const S: string): TGUID;
begin
  if (Length(S) <> 38) or (S[1] <> '{') or (S[10] <> '-') or (S[15] <> '-') or
      (S[20] <> '-') or (S[25] <> '-') or (S[38] <> '}') then
    raise EJclRTTI.CreateResRecFmt(@RsRTTIInvalidGUIDString, [S]);
  Result.D1 := StrToInt('$' + Copy(S, 2, 8));
  Result.D2 := StrToInt('$' + Copy(S, 11, 4));
  Result.D3 := StrToInt('$' + Copy(S, 16, 4));
  Result.D4[0] := StrToInt('$' + Copy(S, 21, 2));
  Result.D4[1] := StrToInt('$' + Copy(S, 23, 2));
  Result.D4[2] := StrToInt('$' + Copy(S, 26, 2));
  Result.D4[3] := StrToInt('$' + Copy(S, 28, 2));
  Result.D4[4] := StrToInt('$' + Copy(S, 30, 2));
  Result.D4[5] := StrToInt('$' + Copy(S, 32, 2));
  Result.D4[6] := StrToInt('$' + Copy(S, 34, 2));
  Result.D4[7] := StrToInt('$' + Copy(S, 36, 2));
end;

//--------------------------------------------------------------------------------------------------
// Is/As hooking
//--------------------------------------------------------------------------------------------------

type
  PReadLoc = ^TReadLoc;
  TReadLoc = packed record
    {$IFDEF OPTIMIZATION_ON}
    Code: array [0..9] of Byte;
    {$ELSE}
    Code: array [0..17] of Byte;
    {$ENDIF OPTIMIZATION_ON}
    OpCode_Call: Byte;
    CallOffset: Longint;
  end;

  PJmp = ^TJmp;
  TJmp = packed record
    case OpCodeJmp: Byte of
      $E9:
        (JmpOffset: Longint);
      $FF:
        (OpCode2: Byte;
         EntryOffset: Longint);
  end;

//--------------------------------------------------------------------------------------------------

function JclIsClass(const AnObj: TObject; const AClass: TClass): Boolean;
type
  PClass = ^TClass;
var
  ClassPtr: PClass;
  CurrentClass: TClass;
begin
  Result := False;
  ClassPtr := PClass(AnObj);
  while Assigned(ClassPtr) do
  begin
    CurrentClass := ClassPtr^;
    Result := CurrentClass = AClass;
    if Result then
      Break;
    ClassPtr := PClass(PPointer(Integer(CurrentClass) + vmtParent)^);
  end;
end;

//--------------------------------------------------------------------------------------------------

function JclIsClassByName(const AnObj: TObject; const AClass: TClass): Boolean;
var
  CurClass: TClass;
  CurClass2: TClass;
begin
  Result := AnObj <> nil;
  if Result then
  begin
    CurClass := AnObj.ClassType;
    Result := False;
    while not Result and (CurClass <> nil) do
    begin
      Result := CurClass.ClassNameIs(AClass.ClassName);
      if not Result then
        CurClass := CurClass.ClassParent;
    end;
    if CurClass <> nil then
      CurClass := CurClass.ClassParent;
    CurClass2 := AClass.ClassParent;
    while Result and (CurClass <> nil) and (CurClass2 <> nil) do
    begin
      Result := CurClass.ClassNameIs(CurClass2.ClassName);
      if Result then
      begin
        CurClass := CurClass.ClassParent;
        CurClass2 := CurClass2.ClassParent;
      end;
    end;
    Result := Result and (CurClass = CurClass2);
  end;
end;

//--------------------------------------------------------------------------------------------------

function JclAsClass(const AnObj: TObject; const AClass: TClass): TObject;
begin
  if (AnObj = nil) or (AnObj is AClass) then
    Result := AnObj
  else
    raise EInvalidCast.Create(SInvalidCast);
end;

//--------------------------------------------------------------------------------------------------

initialization
  TypeList := TThreadList.Create;

finalization
  ClearInfoList;
  FreeAndNil(TypeList);

// History:

// $Log$
// Revision 1.13  2004/08/01 05:52:11  marquardt
// move constructors/destructors
//
// Revision 1.12  2004/07/31 06:21:01  marquardt
// fixing TStringLists, adding BeginUpdate/EndUpdate, finalization improved
//
// Revision 1.11  2004/07/28 18:00:51  marquardt
// various style cleanings, some minor fixes
//
// Revision 1.10  2004/06/14 11:05:51  marquardt
// symbols added to all ENDIFs and some other minor style changes like removing IFOPT
//
// Revision 1.9  2004/06/11 14:08:51  twm
// Bugfix: now uses AnsiLineBreak rather than AnsiCrLf so it will work with unix systems
//
// Revision 1.8  2004/05/05 00:09:59  mthoma
// Updated headers: Added donors as contributors, adjusted the initial authors, added cvs names when they were not obvious. Changed $data to $date where necessary,
//
// Revision 1.7  2004/04/23 22:08:39  mthoma
// Removed non delphi language version of JclIsClass.
//
// Revision 1.6  2004/04/15 16:19:36  peterjhaas
// add pure pascal implementation (JclIsClass)
//
// Revision 1.5  2004/04/06 04:53:18  peterjhaas
// adapt compiler conditions, add log entry
//

end.
