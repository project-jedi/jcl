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
{ The Original Code is JclSysUtils.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Description: Various pointer and class related routines.                                         }
{ Unit Owner: Jeroen Speldekamp                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains various routine for manipulating the math coprocessor. This includes such     }
{ things as querying and setting the rounding precision of floating point operations and           }
{ retrieving the coprocessor's status word.                                                        }
{                                                                                                  }
{ Unit owner: Eric S. Fisher                                                                       }
{ Last modified: September 25, 2002                                                                }
{                                                                                                  }
{**************************************************************************************************}

unit JclSysUtils;

{$I jcl.inc}

{$WEAKPACKAGEUNIT ON}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, TypInfo,
  JclBase;

//--------------------------------------------------------------------------------------------------
// Pointer manipulation
//--------------------------------------------------------------------------------------------------

{$IFNDEF COMPILER5_UP}
procedure FreeAndNil(var Obj);
{$ENDIF COMPILER5_UP}

procedure GetAndFillMem(var P: Pointer; const Size: Integer; const Value: Byte);
procedure FreeMemAndNil(var P: Pointer);
function PCharOrNil(const S: AnsiString): PAnsiChar;
{$IFDEF SUPPORTS_WIDESTRING}
function PWideCharOrNil(const W: WideString): PWideChar;
{$ENDIF SUPPORTS_WIDESTRING}

function SizeOfMem(const APointer: Pointer): Integer;

//--------------------------------------------------------------------------------------------------
// Guards
//--------------------------------------------------------------------------------------------------

type
  ISafeGuard = interface
    function ReleaseItem: Pointer;
    function GetItem: Pointer;
    procedure FreeItem;
    property Item: Pointer read GetItem;
  end;

  IMultiSafeGuard = interface (IInterface)
    function AddItem(Item: Pointer): Pointer;
    procedure FreeItem(Index: Integer);
    function GetCount: Integer;
    function GetItem(Index: Integer): Pointer;
    function ReleaseItem(Index: Integer): Pointer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: Pointer read GetItem;
  end;

function Guard(Mem: Pointer; out SafeGuard: ISafeGuard): Pointer; overload;
function Guard(Obj: TObject; out SafeGuard: ISafeGuard): TObject; overload;

function Guard(Mem: Pointer; var SafeGuard: IMultiSafeGuard): Pointer; overload;
function Guard(Obj: TObject; var SafeGuard: IMultiSafeGuard): TObject; overload;

function GuardGetMem(Size: Cardinal; out SafeGuard: ISafeGuard): Pointer;
function GuardAllocMem(Size: Cardinal; out SafeGuard: ISafeGuard): Pointer;

//--------------------------------------------------------------------------------------------------
// Binary search
//--------------------------------------------------------------------------------------------------

function SearchSortedList(List: TList; SortFunc: TListSortCompare; Item: Pointer;
  Nearest: Boolean = False): Integer;

type
  TUntypedSearchCompare = function(Param: Pointer; ItemIndex: Integer; const Value): Integer;

function SearchSortedUntyped(Param: Pointer; ItemCount: Integer; SearchFunc: TUntypedSearchCompare;
  const Value; Nearest: Boolean = False): Integer;

//--------------------------------------------------------------------------------------------------
// Dynamic array sort and search routines
//--------------------------------------------------------------------------------------------------

type
  TDynArraySortCompare = function (Item1, Item2: Pointer): Integer;

procedure SortDynArray(const ArrayPtr: Pointer; ElementSize: Cardinal; SortFunc: TDynArraySortCompare);
// Usage: SortDynArray(Array, SizeOf(Array[0]), SortFunction);
function SearchDynArray(const ArrayPtr: Pointer; ElementSize: Cardinal; SortFunc: TDynArraySortCompare;
  ValuePtr: Pointer; Nearest: Boolean = False): Integer;
// Usage: SearchDynArray(Array, SizeOf(Array[0]), SortFunction, @SearchedValue);

{ Various compare functions for basic types }

function DynArrayCompareByte(Item1, Item2: Pointer): Integer;
function DynArrayCompareShortInt(Item1, Item2: Pointer): Integer;
function DynArrayCompareWord(Item1, Item2: Pointer): Integer;
function DynArrayCompareSmallInt(Item1, Item2: Pointer): Integer;
function DynArrayCompareInteger(Item1, Item2: Pointer): Integer;
function DynArrayCompareCardinal(Item1, Item2: Pointer): Integer;
function DynArrayCompareInt64(Item1, Item2: Pointer): Integer;

function DynArrayCompareSingle(Item1, Item2: Pointer): Integer;
function DynArrayCompareDouble(Item1, Item2: Pointer): Integer;
function DynArrayCompareExtended(Item1, Item2: Pointer): Integer;
function DynArrayCompareFloat(Item1, Item2: Pointer): Integer;

function DynArrayCompareAnsiString(Item1, Item2: Pointer): Integer;
function DynArrayCompareAnsiText(Item1, Item2: Pointer): Integer;
function DynArrayCompareString(Item1, Item2: Pointer): Integer;
function DynArrayCompareText(Item1, Item2: Pointer): Integer;

//--------------------------------------------------------------------------------------------------
// Object lists
//--------------------------------------------------------------------------------------------------

procedure ClearObjectList(List: TList);
procedure FreeObjectList(var List: TList);

//--------------------------------------------------------------------------------------------------
// Reference memory stream
//--------------------------------------------------------------------------------------------------

type
  TJclReferenceMemoryStream = class (TCustomMemoryStream)
  public
    constructor Create(const Ptr: Pointer; Size: Longint);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

//--------------------------------------------------------------------------------------------------
// Replacement for the C ternary conditional operator ? :
//--------------------------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: string): string; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Char): Char; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Byte): Byte; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Integer): Integer; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Cardinal): Cardinal; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Float): Float; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Boolean): Boolean; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Pointer): Pointer; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Int64): Int64; overload;
function Iff(const Condition: Boolean; const TruePart, FalsePart: Variant): Variant; overload;

//--------------------------------------------------------------------------------------------------
// Classes information and manipulation
//--------------------------------------------------------------------------------------------------

type
  EJclVMTError = class (EJclError);

//--------------------------------------------------------------------------------------------------
// Virtual Methods
//--------------------------------------------------------------------------------------------------

function GetVirtualMethodCount(AClass: TClass): Integer;
function GetVirtualMethod(AClass: TClass; const Index: Integer): Pointer;
{$IFDEF MSWINDOWS}
procedure SetVirtualMethod(AClass: TClass; const Index: Integer; const Method: Pointer);
{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------
// Dynamic Methods
//--------------------------------------------------------------------------------------------------

type
  TDynamicIndexList = array [0..MaxInt div 16] of Word;
  PDynamicIndexList = ^TDynamicIndexList;
  TDynamicAddressList = array [0..MaxInt div 16] of Pointer;
  PDynamicAddressList = ^TDynamicAddressList;

function GetDynamicMethodCount(AClass: TClass): Integer;
function GetDynamicIndexList(AClass: TClass): PDynamicIndexList;
function GetDynamicAddressList(AClass: TClass): PDynamicAddressList;
function HasDynamicMethod(AClass: TClass; Index: Integer): Boolean;
function GetDynamicMethod(AClass: TClass; Index: Integer): Pointer;

{ init table methods }

function GetInitTable(AClass: TClass): PTypeInfo;

{ field table methods }

type
  PFieldEntry = ^TFieldEntry;
  TFieldEntry = packed record
    OffSet: Integer;
    IDX: Word;
    Name: ShortString;
  end;

  PFieldClassTable = ^TFieldClassTable;
  TFieldClassTable = packed record
    Count: Smallint;
    Classes: array [0..8191] of ^TPersistentClass;
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = packed record
    EntryCount: Word;
    FieldClassTable: PFieldClassTable;
    FirstEntry: TFieldEntry;
   {Entries: array [1..65534] of TFieldEntry;}
  end;

function GetFieldTable(AClass: TClass): PFieldTable;

{ method table }

type
  PMethodEntry = ^TMethodEntry;
  TMethodEntry = packed record
    EntrySize: Word;
    Address: Pointer;
    Name: ShortString;
  end;

  PMethodTable = ^TMethodTable;
  TMethodTable = packed record
    Count: Word;
    FirstEntry: TMethodEntry;
   {Entries: array [1..65534] of TMethodEntry;}
  end;

function GetMethodTable(AClass: TClass): PMethodTable;
function GetMethodEntry(MethodTable: PMethodTable; Index: Integer): PMethodEntry;

//--------------------------------------------------------------------------------------------------
// Class Parent
//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
procedure SetClassParent(AClass: TClass; NewClassParent: TClass);
{$ENDIF MSWINDOWS}
function GetClassParent(AClass: TClass): TClass;

function IsClass(Address: Pointer): Boolean;
function IsObject(Address: Pointer): Boolean;

//--------------------------------------------------------------------------------------------------
// Interface information
//--------------------------------------------------------------------------------------------------

function GetImplementorOfInterface(const I: IInterface): TObject;

//--------------------------------------------------------------------------------------------------
// Numeric formatting routines
//--------------------------------------------------------------------------------------------------

function IntToStrZeroPad(Value, Count: Integer): AnsiString;

//--------------------------------------------------------------------------------------------------
// Loading of modules (DLLs)
//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

type
  TModuleHandle = HINST;

const
  INVALID_MODULEHANDLE_VALUE = TModuleHandle(0);

function LoadModule(var Module: TModuleHandle; FileName: string): Boolean;
function LoadModuleEx(var Module: TModuleHandle; FileName: string; Flags: Cardinal): Boolean;
procedure UnloadModule(var Module: TModuleHandle);
function GetModuleSymbol(Module: TModuleHandle; SymbolName: string): Pointer;
function GetModuleSymbolEx(Module: TModuleHandle; SymbolName: string; var Accu: Boolean): Pointer;
function ReadModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
function WriteModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;

{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------
// Conversion Utilities
//--------------------------------------------------------------------------------------------------

type
  EJclConversionError = class (EJclError);

function StrToBoolean(const S: string): Boolean;
function IntToBool(I: Integer): Boolean;
function BoolToInt(B: Boolean): Integer;

//--------------------------------------------------------------------------------------------------
// RTL package information
//--------------------------------------------------------------------------------------------------

function SystemTObjectInstance: LongWord;
function IsCompiledWithPackages: Boolean;

implementation

uses
  SysUtils,
  JclResources, JclStrings;

//==================================================================================================
// Pointer manipulation
//==================================================================================================

{$IFNDEF COMPILER5_UP}

procedure FreeAndNil(var Obj);
var
  O: TObject;
begin
  O := TObject(Obj);
  Pointer(Obj) := nil;
  O.Free;
end;

{$ENDIF COMPILER5_UP}

//--------------------------------------------------------------------------------------------------

procedure GetAndFillMem(var P: Pointer; const Size: Integer; const Value: Byte);
begin
  GetMem(P, Size);
  FillChar(P^, Size, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure FreeMemAndNil(var P: Pointer);
var
  Q: Pointer;
begin
  Q := P;
  P := nil;
  FreeMem(Q);
end;

//--------------------------------------------------------------------------------------------------

function PCharOrNil(const S: AnsiString): PAnsiChar;
begin
  if Length(S) = 0 then
    Result := nil
  else
    Result := PAnsiChar(S);
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF SUPPORTS_WIDESTRING}

function PWideCharOrNil(const W: WideString): PWideChar;
begin
  if Length(W) = 0 then
    Result := nil
  else
    Result := PWideChar(W);
end;

{$ENDIF SUPPORTS_WIDESTRING}

//--------------------------------------------------------------------------------------------------

type
  PUsed = ^TUsed;
  TUsed = record
    SizeFlags: Integer;
  end;

const
  cThisUsedFlag = 2;
  cPrevFreeFlag = 1;
  cFillerFlag   = Integer($80000000);
  cFlags        = cThisUsedFlag or cPrevFreeFlag or cFillerFlag;

function SizeOfMem(const APointer: Pointer): Integer;
var
  U: PUsed;
begin
  if IsMemoryManagerSet then
    Result:= -1
  else
  begin
    Result := 0;
    if APointer <> nil then
    begin
      U := APointer;
      U := PUsed(PChar(U) - SizeOf(TUsed));
      if (U.SizeFlags and cThisUsedFlag) <> 0 then
        Result := (U.SizeFlags) and (not cFlags - SizeOf(TUsed));
    end;
  end;
end;

//==================================================================================================
// Guards
//==================================================================================================

type
  TSafeGuard = class (TInterfacedObject, ISafeGuard)
  private
    FItem: Pointer;
  public
    constructor Create(Mem: Pointer);
    destructor Destroy; override;
    function ReleaseItem: Pointer;
    function GetItem: Pointer;
    procedure FreeItem; virtual;
  end;

  TObjSafeGuard = class (TSafeGuard, ISafeGuard)
  public
    constructor Create(Obj: TObject);
    procedure FreeItem; override;
  end;

  TMultiSafeGuard = class (TInterfacedObject, IMultiSafeGuard)
  private
    FItems: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function AddItem(Mem: Pointer): Pointer;
    procedure FreeItem(Index: Integer); virtual;
    function GetCount: Integer;
    function GetItem(Index: Integer): Pointer;
    function ReleaseItem(Index: Integer): Pointer;
  end;

  TObjMultiSafeGuard = class (TMultiSafeGuard, IMultiSafeGuard)
  public
    procedure FreeItem(Index: Integer); override;
  end;

//--------------------------------------------------------------------------------------------------
// TSafeGuard
//--------------------------------------------------------------------------------------------------

constructor TSafeGuard.Create(Mem: Pointer);
begin
  FItem := Mem;
end;

//--------------------------------------------------------------------------------------------------

destructor TSafeGuard.Destroy;
begin
  FreeItem;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TSafeGuard.ReleaseItem: Pointer;
begin
  Result := FItem;
  FItem := nil;
end;

//--------------------------------------------------------------------------------------------------

function TSafeGuard.GetItem: Pointer;
begin
  Result := FItem;
end;

//--------------------------------------------------------------------------------------------------

procedure TSafeGuard.FreeItem;
begin
  if FItem <> nil then
    FreeMem(FItem);
  FItem := nil;
end;

//--------------------------------------------------------------------------------------------------
// TObjSafeGuard
//--------------------------------------------------------------------------------------------------

constructor TObjSafeGuard.Create(Obj: TObject);
begin
  inherited Create(Obj);
end;

//--------------------------------------------------------------------------------------------------

procedure TObjSafeGuard.FreeItem;
begin
  if FItem <> nil then
  begin
    TObject(FItem).Free;
    FItem := nil;
  end;
end;

//--------------------------------------------------------------------------------------------------
// TMultiSafeGuard
//--------------------------------------------------------------------------------------------------

function TMultiSafeGuard.AddItem(Mem: Pointer): Pointer;
begin
  Result := Mem;
  FItems.Add(Mem);
end;

//--------------------------------------------------------------------------------------------------

constructor TMultiSafeGuard.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TMultiSafeGuard.Destroy;
var
  I: Integer;
begin
  for I := FItems.Count - 1 downto 0 do FreeItem(I);
  FItems.Free;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TMultiSafeGuard.FreeItem(Index: Integer);
begin
  FreeMem(FItems[Index]);
  FItems.Delete(Index);
end;

//--------------------------------------------------------------------------------------------------

function TMultiSafeGuard.GetCount: Integer;
begin
  Result := FItems.Count;
end;

//--------------------------------------------------------------------------------------------------

function TMultiSafeGuard.GetItem(Index: Integer): Pointer;
begin
  Result := FItems[Index];
end;

//--------------------------------------------------------------------------------------------------

function TMultiSafeGuard.ReleaseItem(Index: Integer): Pointer;
begin
  Result := FItems[Index];
  FItems.Delete(Index);
end;

//--------------------------------------------------------------------------------------------------

function Guard(Mem: Pointer; var SafeGuard: IMultiSafeGuard): Pointer; overload;
begin
  if SafeGuard = nil then
    SafeGuard := TMultiSafeGuard.Create;
  Result := SafeGuard.AddItem(Mem);
end;

//--------------------------------------------------------------------------------------------------
// TObjMultiSafeGuard
//--------------------------------------------------------------------------------------------------

procedure TObjMultiSafeGuard.FreeItem(Index: Integer);
begin
  TObject(FItems[Index]).Free;
  FItems.Delete(Index);
end;

//--------------------------------------------------------------------------------------------------

function Guard(Obj: TObject; var SafeGuard: IMultiSafeGuard): TObject; overload;
begin
  if SafeGuard = nil then
    SafeGuard := TObjMultiSafeGuard.Create;
  Result := SafeGuard.AddItem(Obj);
end;

//--------------------------------------------------------------------------------------------------

function Guard(Mem: Pointer; out SafeGuard: ISafeGuard): Pointer; overload;
begin
  Result := Mem;
  SafeGuard := TSafeGuard.Create(Mem);
end;

//--------------------------------------------------------------------------------------------------

function Guard(Obj: TObject; out SafeGuard: ISafeGuard): TObject; overload;
begin
  Result := Obj;
  SafeGuard := TObjSafeGuard.Create(Obj);
end;

//--------------------------------------------------------------------------------------------------

function GuardGetMem(Size: Cardinal; out SafeGuard: ISafeGuard): Pointer;
begin
  GetMem(Result, Size);
  Guard(Result, SafeGuard);
end;

//--------------------------------------------------------------------------------------------------

function GuardAllocMem(Size: Cardinal; out SafeGuard: ISafeGuard): Pointer;
begin
  Result := AllocMem(Size);
  Guard(Result, SafeGuard);
end;

//==================================================================================================
// Binary search
//==================================================================================================

function SearchSortedList(List: TList; SortFunc: TListSortCompare; Item: Pointer; Nearest: Boolean): Integer;
var
  L, H, I, C: Integer;
  B: Boolean;
begin
  Result := -1;
  if List <> nil then
  begin
    L := 0;
    H := List.Count - 1;
    B := False;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := SortFunc(List.List^[I], Item);
      if C < 0 then
        L := I + 1
      else
      begin
        H := I - 1;
        if C = 0 then
        begin
          B := True;
          L := I;
        end;
      end;
    end;
    if B then
      Result := L
    else
    if Nearest and (H >= 0) then
      Result := H;
  end;
end;

//--------------------------------------------------------------------------------------------------

function SearchSortedUntyped(Param: Pointer; ItemCount: Integer; SearchFunc: TUntypedSearchCompare;
  const Value; Nearest: Boolean): Integer;
var
  L, H, I, C: Integer;
  B: Boolean;
begin
  Result := -1;
  if ItemCount > 0 then
  begin
    L := 0;
    H := ItemCount - 1;
    B := False;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := SearchFunc(Param, I, Value);
      if C < 0 then
        L := I + 1
      else
      begin
        H := I - 1;
        if C = 0 then
        begin
          B := True;
          L := I;
        end;
      end;
    end;
    if B then
      Result := L
    else
    if Nearest and (H >= 0) then
      Result := H;
  end;
end;

//==================================================================================================
// Dynamic array sort and search routines
//==================================================================================================

procedure SortDynArray(const ArrayPtr: Pointer; ElementSize: Cardinal; SortFunc: TDynArraySortCompare);
var
  TempBuf: TDynByteArray;

  function ArrayItemPointer(Item: Integer): Pointer;
  begin
    Result := Pointer(Cardinal(ArrayPtr) + (Cardinal(Item) * ElementSize));
  end;

  procedure QuickSort(L, R: Integer);
  var
    I, J, T: Integer;
    P, IPtr, JPtr: Pointer;
  begin
    repeat
      I := L;
      J := R;
      P := ArrayItemPointer((L + R) shr 1);
      repeat
        while SortFunc(ArrayItemPointer(I), P) < 0 do
          Inc(I);
        while SortFunc(ArrayItemPointer(J), P) > 0 do
          Dec(J);
        if I <= J then
        begin
          IPtr := ArrayItemPointer(I);
          JPtr := ArrayItemPointer(J);
          case ElementSize of
            SizeOf(Byte):
              begin
                T := PByte(IPtr)^;
                PByte(IPtr)^ := PByte(JPtr)^;
                PByte(JPtr)^ := T;
              end;
            SizeOf(Word):
              begin
                T := PWord(IPtr)^;
                PWord(IPtr)^ := PWord(JPtr)^;
                PWord(JPtr)^ := T;
              end;
            SizeOf(Integer):
              begin
                T := PInteger(IPtr)^;
                PInteger(IPtr)^ := PInteger(JPtr)^;
                PInteger(JPtr)^ := T;
              end;
          else
            Move(IPtr^, TempBuf[0], ElementSize);
            Move(JPtr^, IPtr^, ElementSize);
            Move(TempBuf[0], JPtr^, ElementSize);
          end;
          if P = IPtr then
            P := JPtr
          else
          if P = JPtr then
            P := IPtr;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  if ArrayPtr <> nil then
  begin
    SetLength(TempBuf, ElementSize);
    QuickSort(0, PInteger(Cardinal(ArrayPtr) - 4)^ - 1);
  end;
end;

//--------------------------------------------------------------------------------------------------

function SearchDynArray(const ArrayPtr: Pointer; ElementSize: Cardinal; SortFunc: TDynArraySortCompare;
  ValuePtr: Pointer; Nearest: Boolean): Integer;
var
  L, H, I, C: Integer;
  B: Boolean;
begin
  Result := -1;
  if ArrayPtr <> nil then
  begin
    L := 0;
    H := PInteger(Cardinal(ArrayPtr) - 4)^ - 1;
    B := False;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := SortFunc(Pointer(Cardinal(ArrayPtr) + (Cardinal(I) * ElementSize)), ValuePtr);
      if C < 0 then
        L := I + 1
      else
      begin
        H := I - 1;
        if C = 0 then
        begin
          B := True;
          L := I;
        end;
      end;
    end;
    if B then
      Result := L
    else
    if Nearest and (H >= 0) then
      Result := H;
  end;
end;

//--------------------------------------------------------------------------------------------------

{ Various compare functions for basic types }

function DynArrayCompareByte(Item1, Item2: Pointer): Integer;
begin
  Result := PByte(Item1)^ - PByte(Item2)^;
end;

//--------------------------------------------------------------------------------------------------

function DynArrayCompareShortInt(Item1, Item2: Pointer): Integer;
begin
  Result := PShortInt(Item1)^ - PShortInt(Item2)^;
end;

//--------------------------------------------------------------------------------------------------

function DynArrayCompareWord(Item1, Item2: Pointer): Integer;
begin
  Result := PWord(Item1)^ - PWord(Item2)^;
end;

//--------------------------------------------------------------------------------------------------

function DynArrayCompareSmallInt(Item1, Item2: Pointer): Integer;
begin
  Result := PSmallInt(Item1)^ - PSmallInt(Item2)^;
end;

//--------------------------------------------------------------------------------------------------

function DynArrayCompareInteger(Item1, Item2: Pointer): Integer;
begin
  Result := PInteger(Item1)^ - PInteger(Item2)^;
end;

//--------------------------------------------------------------------------------------------------

function DynArrayCompareCardinal(Item1, Item2: Pointer): Integer;
begin
  Result := PInteger(Item1)^ - PInteger(Item2)^;
end;

//--------------------------------------------------------------------------------------------------

function DynArrayCompareInt64(Item1, Item2: Pointer): Integer;
begin
  Result := PInt64(Item1)^ - PInt64(Item2)^;
end;

//--------------------------------------------------------------------------------------------------

function DynArrayCompareSingle(Item1, Item2: Pointer): Integer;
begin
  if PSingle(Item1)^ < PSingle(Item2)^ then
    Result := -1
  else
  if PSingle(Item1)^ > PSingle(Item2)^ then
    Result := 1
  else
    Result := 0;
end;

//--------------------------------------------------------------------------------------------------

function DynArrayCompareDouble(Item1, Item2: Pointer): Integer;
begin
  if PDouble(Item1)^ < PDouble(Item2)^ then
    Result := -1
  else
  if PDouble(Item1)^ > PDouble(Item2)^ then
    Result := 1
  else
    Result := 0;
end;

//--------------------------------------------------------------------------------------------------

function DynArrayCompareExtended(Item1, Item2: Pointer): Integer;
begin
  if PExtended(Item1)^ < PExtended(Item2)^ then
    Result := -1
  else
  if PExtended(Item1)^ > PExtended(Item2)^ then
    Result := 1
  else
    Result := 0;
end;

//--------------------------------------------------------------------------------------------------

function DynArrayCompareFloat(Item1, Item2: Pointer): Integer;
begin
  if PFloat(Item1)^ < PFloat(Item2)^ then
    Result := -1
  else
  if PFloat(Item1)^ > PFloat(Item2)^ then
    Result := 1
  else
    Result := 0;
end;

//--------------------------------------------------------------------------------------------------

function DynArrayCompareAnsiString(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareStr(PAnsiString(Item1)^, PAnsiString(Item2)^);
end;

//--------------------------------------------------------------------------------------------------

function DynArrayCompareAnsiText(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareText(PAnsiString(Item1)^, PAnsiString(Item2)^);
end;

//--------------------------------------------------------------------------------------------------

function DynArrayCompareString(Item1, Item2: Pointer): Integer;
begin
  Result := CompareStr(PAnsiString(Item1)^, PAnsiString(Item2)^);
end;

//--------------------------------------------------------------------------------------------------

function DynArrayCompareText(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(PAnsiString(Item1)^, PAnsiString(Item2)^);
end;

//==================================================================================================
// Object lists
//==================================================================================================

procedure ClearObjectList(List: TList);
var
  I: Integer;
begin
  if List <> nil then
  begin
    for I := 0 to List.Count - 1 do
    begin
      if List[I] <> nil then
      begin
        if TObject(List[I]) is TList then
        begin
          // recursively delete TList sublists
          ClearObjectList(TList(List[I]));
        end;
        TObject(List[I]).Free;
        List[I] := nil;
      end;
    end;
    List.Clear;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure FreeObjectList(var List: TList);
begin
  if List <> nil then
  begin
    ClearObjectList(List);
    FreeAndNil(List);
  end;
end;

//==================================================================================================
// TJclReferenceMemoryStream
//==================================================================================================

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

//==================================================================================================
// replacement for the C distfix operator ? :
//==================================================================================================

function Iff(const Condition: Boolean; const TruePart, FalsePart: string): string;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//--------------------------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Char): Char;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//--------------------------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Byte): Byte;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//--------------------------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Integer): Integer;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//--------------------------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Cardinal): Cardinal;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//--------------------------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Float): Float;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//--------------------------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Boolean): Boolean;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//--------------------------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Pointer): Pointer;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//--------------------------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Int64): Int64;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//--------------------------------------------------------------------------------------------------

function Iff(const Condition: Boolean; const TruePart, FalsePart: Variant): Variant;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

//==================================================================================================
// Classes information and manipulation
//==================================================================================================

//==================================================================================================
// Virtual Methods
//==================================================================================================

function GetVirtualMethodCount(AClass: TClass): Integer;
var
  BeginVMT: Longint;
  EndVMT: Longint;
  TablePointer: Longint;
  I: Integer;
begin
  BeginVMT := Longint(AClass);

  // Scan the offset entries in the class table for the various fields,
  // namely vmtIntfTable, vmtAutoTable, ..., vmtDynamicTable
  // The last entry is always the vmtClassName, so stop once we got there
  // After the last virtual method there is one of these entries.

  EndVMT := PLongint(Longint(AClass) + vmtClassName)^;
  // Set iterator to first item behind VMT table pointer
  I := vmtSelfPtr + SizeOf(Pointer);
  repeat
    TablePointer := PLongint(Longint(AClass) + I)^;
    if (TablePointer <> 0) and (TablePointer >= BeginVMT) and
       (TablePointer < EndVMT) then
      EndVMT := Longint(TablePointer);
    Inc(I, SizeOf(Pointer));
  until I >= vmtClassName;

  Result := (EndVMT - BeginVMT) div SizeOf(Pointer);
end;

//--------------------------------------------------------------------------------------------------

function GetVirtualMethod(AClass: TClass; const Index: Integer): Pointer;
begin
  Result := PPointer(Integer(AClass) + Index * SizeOf(Pointer))^;
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

procedure SetVirtualMethod(AClass: TClass; const Index: Integer; const Method: Pointer);
var
  WrittenBytes: DWORD;
  PatchAddress: Pointer;
begin
  PatchAddress := PPointer(Integer(AClass) + Index * SizeOf(Pointer))^;
  //! StH: WriteProcessMemory IMO is not exactly the politically correct approach;
  // better VirtualProtect, direct patch, VirtualProtect
  if not WriteProcessMemory(GetCurrentProcess, PatchAddress, {@}Method,
    SizeOf(Pointer), WrittenBytes) then
    raise EJclVMTError.CreateResRecFmt(@RsVMTMemoryWriteError, [SysErrorMessage(GetLastError)]);

  if WrittenBytes <> SizeOf(Pointer) then
    raise EJclVMTError.CreateResRecFmt(@RsVMTMemoryWriteError, [IntToStr(WrittenBytes)]);

  // make sure that everything keeps working in a dual processor setting
  FlushInstructionCache(GetCurrentProcess, PatchAddress, SizeOf(Pointer));
end;

{$ENDIF MSWINDOWS}

//==================================================================================================
// Dynamic Methods
//==================================================================================================

type
  TvmtDynamicTable = packed record
    Count: Word;
   {IndexList: array [1..Count] of Word;
    AddressList: array [1..Count] of Pointer;}
  end;

//--------------------------------------------------------------------------------------------------

function GetDynamicMethodCount(AClass: TClass): Integer; assembler;
asm
        MOV     EAX, [EAX].vmtDynamicTable
        TEST    EAX, EAX
        JE      @@Exit
        MOVZX   EAX, WORD PTR [EAX]
@@Exit:
end;

//--------------------------------------------------------------------------------------------------

function GetDynamicIndexList(AClass: TClass): PDynamicIndexList; assembler;
asm
        MOV     EAX, [EAX].vmtDynamicTable
        ADD     EAX, 2
end;

//--------------------------------------------------------------------------------------------------

function GetDynamicAddressList(AClass: TClass): PDynamicAddressList; assembler;
asm
        MOV     EAX, [EAX].vmtDynamicTable
        MOVZX   EDX, Word ptr [EAX]
        ADD     EAX, EDX
        ADD     EAX, EDX
        ADD     EAX, 2
end;

//--------------------------------------------------------------------------------------------------

function HasDynamicMethod(AClass: TClass; Index: Integer): Boolean; assembler;
// Mainly copied from System.GetDynaMethod
asm
        { ->    EAX     vmt of class            }
        {       DX      dynamic method index    }

        PUSH    EDI
        XCHG    EAX, EDX
        JMP     @@HaveVMT
@@OuterLoop:
        MOV     EDX, [EDX]
@@HaveVMT:
        MOV     EDI, [EDX].vmtDynamicTable
        TEST    EDI, EDI
        JE      @@Parent
        MOVZX   ECX, WORD PTR [EDI]
        PUSH    ECX
        ADD     EDI,2
        REPNE   SCASW
        JE      @@Found
        POP     ECX
@@Parent:
        MOV     EDX,[EDX].vmtParent
        TEST    EDX,EDX
        JNE     @@OuterLoop
        MOV     EAX, 0
        JMP     @@Exit
@@Found:
        POP     EAX
        MOV     EAX, 1
@@Exit:
        POP     EDI
end;

//--------------------------------------------------------------------------------------------------

function GetDynamicMethod(AClass: TClass; Index: Integer): Pointer; assembler;
asm
        CALL    System.@FindDynaClass
end;

//==================================================================================================
// Interface Table
//==================================================================================================

function GetInitTable(AClass: TClass): PTypeInfo; assembler;
asm
        MOV     EAX, [EAX].vmtInitTable
end;

//--------------------------------------------------------------------------------------------------

function GetFieldTable(AClass: TClass): PFieldTable; assembler;
asm
        MOV     EAX, [EAX].vmtFieldTable
end;

//--------------------------------------------------------------------------------------------------

function GetMethodTable(AClass: TClass): PMethodTable; assembler;
asm
        MOV     EAX, [EAX].vmtMethodTable
end;

//--------------------------------------------------------------------------------------------------

function GetMethodEntry(MethodTable: PMethodTable; Index: Integer): PMethodEntry;
begin
  Result := Pointer(Cardinal(MethodTable) + 2);
  for Index := Index downto 1 do
    Inc(Cardinal(Result), Result^.EntrySize);
end;

//==================================================================================================
// Class Parent methods
//==================================================================================================

{$IFDEF MSWINDOWS}

procedure SetClassParent(AClass: TClass; NewClassParent: TClass);
var
  WrittenBytes: DWORD;
  PatchAddress: Pointer;
begin
  PatchAddress := PPointer(Integer(AClass) + vmtParent)^;
  //! StH: WriteProcessMemory IMO is not exactly the politically correct approach;
  // better VirtualProtect, direct patch, VirtualProtect
  if not WriteProcessMemory(GetCurrentProcess, PatchAddress, @NewClassParent,
    SizeOf(Pointer), WrittenBytes) then
    raise EJclVMTError.CreateResRecFmt(@RsVMTMemoryWriteError, [SysErrorMessage(GetLastError)]);
  if WrittenBytes <> SizeOf(Pointer) then
    raise EJclVMTError.CreateResRecFmt(@RsVMTMemoryWriteError, [IntToStr(WrittenBytes)]);
  // make sure that everything keeps working in a dual processor setting
  FlushInstructionCache(GetCurrentProcess, PatchAddress, SizeOf(Pointer));
end;

{$ENDIF MSWINDOWS}

//--------------------------------------------------------------------------------------------------

function GetClassParent(AClass: TClass): TClass; assembler;
asm
        MOV     EAX, [AClass].vmtParent
        TEST    Result, EAX
        JE      @@Exit
        MOV     EAX, [EAX]
@@Exit:
end;

//--------------------------------------------------------------------------------------------------

function IsClass(Address: Pointer): Boolean; assembler;
asm
        CMP     Address, Address.vmtSelfPtr
        JNZ     @False
        MOV     Result, True
        JMP     @Exit
@False:
        MOV     Result, False
@Exit:
end;

//--------------------------------------------------------------------------------------------------

function IsObject(Address: Pointer): Boolean; assembler;
asm
// or IsClass(Pointer(Address^));
        MOV     EAX, [Address]
        CMP     EAX, EAX.vmtSelfPtr
        JNZ     @False
        MOV     Result, True
        JMP     @Exit
@False:
        MOV     Result, False
@Exit:
end;

//==================================================================================================
// Interface information
//==================================================================================================

function GetImplementorOfInterface(const I: IInterface): TObject;
{ TODO -cDOC : Original code by Hallvard Vassbotn }
{ TODO -cTesting : Check the implemetation for any further version of compiler }
const
  AddByte = $04244483; // opcode for ADD DWORD PTR [ESP+4], Shortint
  AddLong = $04244481; // opcode for ADD DWORD PTR [ESP+4], Longint
type
  PAdjustSelfThunk = ^TAdjustSelfThunk;
  TAdjustSelfThunk = packed record
    case AddInstruction: LongInt of
      AddByte: (AdjustmentByte: ShortInt);
      AddLong: (AdjustmentLong: LongInt);
  end;
  PInterfaceMT = ^TInterfaceMT;
  TInterfaceMT = packed record
    QueryInterfaceThunk: PAdjustSelfThunk;
  end;
  TInterfaceRef = ^PInterfaceMT;
var
  QueryInterfaceThunk: PAdjustSelfThunk;
begin
  try
    Result := Pointer(I);
    if Assigned(Result) then
    begin
      QueryInterfaceThunk := TInterfaceRef(I)^.QueryInterfaceThunk;
      case QueryInterfaceThunk.AddInstruction of
        AddByte:
          Inc(PChar(Result), QueryInterfaceThunk.AdjustmentByte);
        AddLong:
          Inc(PChar(Result), QueryInterfaceThunk.AdjustmentLong);
      else
        Result := nil;
      end;
    end;
  except
    Result := nil;
  end;
end;

//==================================================================================================
// Numeric formatting routines
//==================================================================================================

function IntToStrZeroPad(Value, Count: Integer): AnsiString;
begin
  Result := IntToStr(Value);
  if Length(Result) < Count then
    Result := StrFillChar('0', Count - Length(Result)) + Result;
end;

//==================================================================================================
// Loading of modules (DLLs)
//==================================================================================================

{$IFDEF MSWINDOWS}

function LoadModule(var Module: TModuleHandle; FileName: string): Boolean;
begin
  if Module = INVALID_MODULEHANDLE_VALUE then
    Module := LoadLibrary(PChar(FileName));
  Result := Module <> INVALID_MODULEHANDLE_VALUE;
end;

//--------------------------------------------------------------------------------------------------

function LoadModuleEx(var Module: TModuleHandle; FileName: string; Flags: Cardinal): Boolean;
begin
  if Module = INVALID_MODULEHANDLE_VALUE then
    Module := LoadLibraryEx(PChar(FileName), 0, Flags);
  Result := Module <> INVALID_MODULEHANDLE_VALUE;
end;

//--------------------------------------------------------------------------------------------------

procedure UnloadModule(var Module: TModuleHandle);
begin
  if Module <> INVALID_MODULEHANDLE_VALUE then
    FreeLibrary(Module);
  Module := INVALID_MODULEHANDLE_VALUE;
end;

//--------------------------------------------------------------------------------------------------

function GetModuleSymbol(Module: TModuleHandle; SymbolName: string): Pointer;
begin
  Result := nil;
  if Module <> INVALID_MODULEHANDLE_VALUE then
    Result := GetProcAddress(Module, PChar(SymbolName));
end;

//--------------------------------------------------------------------------------------------------

function GetModuleSymbolEx(Module: TModuleHandle; SymbolName: string; var Accu: Boolean): Pointer;
begin
  Result := nil;
  if Module <> INVALID_MODULEHANDLE_VALUE then
    Result := GetProcAddress(Module, PChar(SymbolName));
  Accu := Accu and (Result <> nil);
end;

//--------------------------------------------------------------------------------------------------

function ReadModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
var
  Sym: Pointer;
begin
  Result := True;
  Sym := GetModuleSymbolEx(Module, SymbolName, Result);
  if Result then
    Move(Sym^, Buffer, Size);
end;

//--------------------------------------------------------------------------------------------------

function WriteModuleData(Module: TModuleHandle; SymbolName: string; var Buffer; Size: Cardinal): Boolean;
var
  Sym: Pointer;
begin
  Result := True;
  Sym := GetModuleSymbolEx(Module, SymbolName, Result);
  if Result then
    Move(Buffer, Sym^, Size);
end;

{$ENDIF MSWINDOWS}

//==================================================================================================
// Conversion Utilities
//==================================================================================================

{ TODOC
  Author: Jeff

  StrToBoolean: converts a string S to a boolean. S may be 'Yes/No', 'True/False' or '0/1' or 'T/F' or 'Y/N'.
                raises an EJclConversionError exception on failure.
  IntToBool: converts an integer to a boolean where 0 means false and anything else is tue.
  BoolToInt: converts a boolean to an integer: True=>1 and False=>0
}

const
  DefaultTrueBoolStr   = 'True';  // DO NOT LOCALIZE
  DefaultFalseBoolStr  = 'False'; // DO NOT LOCALIZE

  DefaultYesBoolStr    = 'Yes';   // DO NOT LOCALIZE
  DefaultNoBoolStr     = 'No';    // DO NOT LOCALIZE

//--------------------------------------------------------------------------------------------------

function StrToBoolean(const S: string): Boolean;
var
  LowerCasedText: string;
begin
  LowerCasedText := LowerCase(S);
  Result := ((S = '1') or
    (LowerCasedText = LowerCase(DefaultTrueBoolStr)) or (LowerCasedText = LowerCase(DefaultYesBoolStr))) or
    (LowerCasedText = LowerCase(DefaultTrueBoolStr[1])) or (LowerCasedText = LowerCase(DefaultYesBoolStr[1]));
  if not Result then
  begin
    Result := not ((S = '0') or
      (LowerCasedText = LowerCase(DefaultFalseBoolStr)) or (LowerCasedText = LowerCase(DefaultNoBoolStr)) or
      (LowerCasedText = LowerCase(DefaultFalseBoolStr[1])) or (LowerCasedText = LowerCase(DefaultNoBoolStr[1])));
    if Result then
      raise EJclConversionError.CreateResRecFmt(@RsStringToBoolean, [S]);
  end;
end;

//--------------------------------------------------------------------------------------------------

function IntToBool(I: Integer): Boolean;
begin
  Result := I <> 0;
end;

//--------------------------------------------------------------------------------------------------

function BoolToInt(B: Boolean): Integer;
begin
  Result := Ord(B);
end;

//==================================================================================================
// RTL package information
//==================================================================================================

function SystemTObjectInstance: LongWord;
begin
  {$IFDEF COMPILER4}
  Result := LongWord(FindClassHInstance(System.TObject));
  {$ELSE COMPILER4}
  Result := FindClassHInstance(System.TObject);
  {$ENDIF COMPILER4}
end;

//--------------------------------------------------------------------------------------------------

function IsCompiledWithPackages: Boolean;
begin
  Result := SystemTObjectInstance <> HInstance;
end;

//--------------------------------------------------------------------------------------------------

end.
