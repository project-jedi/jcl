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
{ The Original Code is HashMap.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclHashMaps;

{$I jcl.inc}

interface

uses
  JclBase, JclAbstractContainers, JclContainerIntf;

type
  TJclIntfIntfEntry = record
    Key: IInterface;
    Value: IInterface;
  end;

  TJclStrIntfEntry = record
    Key: string;
    Value: IInterface;
  end;

  TJclStrStrEntry = record
    Key: string;
    Value: string;
  end;

  TJclStrEntry = record
    Key: string;
    Value: TObject;
  end;

  TJclEntry = record
    Key: TObject;
    Value: TObject;
  end;

  TJclIntfIntfEntryArray = array of TJclIntfIntfEntry;
  TJclStrIntfEntryArray = array of TJclStrIntfEntry;
  TJclStrStrEntryArray = array of TJclStrStrEntry;
  TJclStrEntryArray = array of TJclStrEntry;
  TJclEntryArray = array of TJclEntry;

  {$IFDEF CLR}
  TJclIntfIntfBucket = class;
  PJclIntfIntfBucket = TJclIntfIntfBucket;
  TJclIntfIntfBucket = class
  {$ELSE}
  PJclIntfIntfBucket = ^TJclIntfIntfBucket;
  TJclIntfIntfBucket = record
  {$ENDIF CLR}
    Count: Integer;
    Entries: TJclIntfIntfEntryArray;
  end;

  {$IFDEF CLR}
  TJclStrIntfBucket = class;
  PJclStrIntfBucket = TJclStrIntfBucket;
  TJclStrIntfBucket = class
  {$ELSE}
  PJclStrIntfBucket = ^TJclStrIntfBucket;
  TJclStrIntfBucket = record
  {$ENDIF CLR}
    Count: Integer;
    Entries: TJclStrIntfEntryArray;
  end;

  {$IFDEF CLR}
  TJclStrStrBucket = class;
  PJclStrStrBucket = TJclStrStrBucket;
  TJclStrStrBucket = class
  {$ELSE}
  PJclStrStrBucket = ^TJclStrStrBucket;
  TJclStrStrBucket = record
  {$ENDIF CLR}
    Count: Integer;
    Entries: TJclStrStrEntryArray;
  end;

  {$IFDEF CLR}
  TJclStrBucket = class;
  PJclStrBucket = TJclStrBucket;
  TJclStrBucket = class
  {$ELSE}
  PJclStrBucket = ^TJclStrBucket;
  TJclStrBucket = record
  {$ENDIF CLR}
    Count: Integer;
    Entries: TJclStrEntryArray;
  end;

  {$IFDEF CLR}
  TJclBucket = class;
  PJclBucket = TJclBucket;
  TJclBucket = class
  {$ELSE}
  PJclBucket = ^TJclBucket;
  TJclBucket = record
  {$ENDIF CLR}
    Count: Integer;
    Entries: TJclEntryArray;
  end;

  TJclIntfIntfBucketArray = array of TJclIntfIntfBucket;
  TJclStrIntfBucketArray = array of TJclStrIntfBucket;
  TJclStrStrBucketArray = array of TJclStrStrBucket;
  TJclStrBucketArray = array of TJclStrBucket;
  TJclBucketArray = array of TJclBucket;

  // Hash Function
  TJclHashFunction = function(Key: Cardinal): Cardinal of object;

  TJclIntfIntfHashMap = class(TJclAbstractContainer, IJclIntfIntfMap,
    IJclIntfCloneable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TJclIntfIntfBucketArray;
    FHashFunction: TJclHashFunction;
    function HashMul(Key: Cardinal): Cardinal;
  protected
    procedure GrowEntries(BucketIndex: Integer); virtual;
    { IJclIntfIntfMap }
    procedure Clear;
    function ContainsKey(Key: IInterface): Boolean;
    function ContainsValue(Value: IInterface): Boolean;
    function Equals(AMap: IJclIntfIntfMap): Boolean;
    function GetValue(Key: IInterface): IInterface;
    function IsEmpty: Boolean;
    function KeySet: IJclIntfSet;
    procedure PutAll(AMap: IJclIntfIntfMap);
    procedure PutValue(Key, Value: IInterface);
    function Remove(Key: IInterface): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    { IJclIntfCloneable }
    function Clone: IInterface;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write
      FHashFunction;
  end;

  TJclStrIntfHashMap = class(TJclAbstractContainer, IJclStrIntfMap, IJclIntfCloneable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TJclStrIntfBucketArray;
    FHashFunction: TJclHashFunction;
    function HashMul(Key: Cardinal): Cardinal;
    function HashString(const Key: string): Cardinal;
  protected
    procedure GrowEntries(BucketIndex: Integer); virtual;
    { IJclIntfMap }
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(Value: IInterface): Boolean;
    function Equals(AMap: IJclStrIntfMap): Boolean;
    function GetValue(const Key: string): IInterface;
    function IsEmpty: Boolean;
    function KeySet: IJclStrSet;
    procedure PutAll(AMap: IJclStrIntfMap);
    procedure PutValue(const Key: string; Value: IInterface);
    function Remove(const Key: string): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    { IJclIntfCloneable }
    function Clone: IInterface;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write
      FHashFunction;
  end;

  TJclStrStrHashMap = class(TJclAbstractContainer, IJclStrStrMap, IJclIntfCloneable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TJclStrStrBucketArray;
    FHashFunction: TJclHashFunction;
    function HashMul(Key: Cardinal): Cardinal;
    function HashString(const Key: string): Cardinal;
  protected
    procedure GrowEntries(BucketIndex: Integer); virtual;
    { IJclStrStrMap }
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(const Value: string): Boolean;
    function Equals(AMap: IJclStrStrMap): Boolean;
    function GetValue(const Key: string): string;
    function IsEmpty: Boolean;
    function KeySet: IJclStrSet;
    procedure PutAll(AMap: IJclStrStrMap);
    procedure PutValue(const Key, Value: string);
    function Remove(const Key: string): string;
    function Size: Integer;
    function Values: IJclStrCollection;
    // Daniele Teti
    function KeyOfValue(const Value: string): string;
    { IJclIntfCloneable }
    function Clone: IInterface;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write
      FHashFunction;
  end;

  TJclStrHashMap = class(TJclAbstractContainer, IJclStrMap, IJclCloneable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TJclStrBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsObjects: Boolean;
    function HashMul(Key: Cardinal): Cardinal;
    function HashString(const Key: string): Cardinal;
  protected
    procedure GrowEntries(BucketIndex: Integer); virtual;
    procedure FreeObject(var AObject: TObject);
    { IJclStrMap }
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(AMap: IJclStrMap): Boolean;
    function GetValue(const Key: string): TObject;
    function IsEmpty: Boolean;
    function KeySet: IJclStrSet;
    procedure PutAll(AMap: IJclStrMap);
    procedure PutValue(const Key: string; Value: TObject);
    function Remove(const Key: string): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    { IJclCloneable }
    function Clone: TObject;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity;
      AOwnsObjects: Boolean = True);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write
      FHashFunction;
    property OwnsObjects: Boolean read FOwnsObjects;
  end;

  TJclHashMap = class(TJclAbstractContainer, IJclMap, IJclCloneable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TJclBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsObjects: Boolean;
    function HashMul(Key: Cardinal): Cardinal;
  protected
    procedure GrowEntries(BucketIndex: Integer); virtual;
    procedure FreeObject(var AObject: TObject);
    { IJclCloneable }
    function Clone: TObject;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity;
      AOwnsObjects: Boolean = True);
    destructor Destroy; override;
    { IJclMap }
    procedure Clear;
    function ContainsKey(Key: TObject): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(AMap: IJclMap): Boolean;
    function GetValue(Key: TObject): TObject;
    function IsEmpty: Boolean;
    function KeySet: IJclSet;
    procedure PutAll(AMap: IJclMap);
    procedure PutValue(Key, Value: TObject);
    function Remove(Key: TObject): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsObjects: Boolean read FOwnsObjects;
  end;

implementation

uses
  SysUtils,
  JclArrayLists, JclArraySets, JclResources;

procedure MoveArray(var List: TJclIntfIntfEntryArray; FromIndex, ToIndex, Count: Integer); overload;
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I]
  else
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
{$ELSE}
begin
  Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
  { Keep reference counting working }
  if FromIndex < ToIndex then
    FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
  else if FromIndex > ToIndex then
    FillChar(List[FromIndex + Count - 1], (FromIndex - ToIndex) * SizeOf(List[0]), 0);
{$ENDIF CLR}
end;

procedure MoveArray(var List: TJclStrIntfEntryArray; FromIndex, ToIndex, Count: Integer); overload;
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I]
  else
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
{$ELSE}
begin
  Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
  { Keep reference counting working }
  if FromIndex < ToIndex then
    FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
  else if FromIndex > ToIndex then
    FillChar(List[FromIndex + Count - 1], (FromIndex - ToIndex) * SizeOf(List[0]), 0);
{$ENDIF CLR}
end;

procedure MoveArray(var List: TJclStrStrEntryArray; FromIndex, ToIndex, Count: Integer); overload;
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I]
  else
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
{$ELSE}
begin
  Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
  { Keep reference counting working }
  if FromIndex < ToIndex then
    FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
  else if FromIndex > ToIndex then
    FillChar(List[FromIndex + Count - 1], (FromIndex - ToIndex) * SizeOf(List[0]), 0);
{$ENDIF CLR}
end;

procedure MoveArray(var List: TJclStrEntryArray; FromIndex, ToIndex, Count: Integer); overload;
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I]
  else
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
{$ELSE}
begin
  Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
  { Keep reference counting working }
  if FromIndex < ToIndex then
    FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
  else if FromIndex > ToIndex then
    FillChar(List[FromIndex + Count - 1], (FromIndex - ToIndex) * SizeOf(List[0]), 0);
{$ENDIF CLR}
end;

procedure MoveArray(var List: TJclEntryArray; FromIndex, ToIndex, Count: Integer); overload;
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I]
  else
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
{$ELSE}
begin
  Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
{$ENDIF CLR}
end;

//=== { TJclIntfIntfHashMap } ================================================

constructor TJclIntfIntfHashMap.Create(ACapacity: Integer = DefaultContainerCapacity);
var
  I: Integer;
begin
  inherited Create;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FBuckets, FCapacity);
  for I := 0 to FCapacity - 1 do
    SetLength(FBuckets[I].Entries, 1);
  FHashFunction := HashMul;
end;

destructor TJclIntfIntfHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclIntfIntfHashMap.Clear;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  for I := 0 to FCapacity - 1 do
  begin
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      FBuckets[I].Entries[J].Key := nil;
      FBuckets[I].Entries[J].Value := nil;
    end;
    FBuckets[I].Count := 0;
  end;
  FCount := 0;
end;

function TJclIntfIntfHashMap.Clone: IInterface;
var
  I, J: Integer;
  NewEntryArray: TJclIntfIntfEntryArray;
  NewMap: TJclIntfIntfHashMap;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  NewMap := TJclIntfIntfHashMap.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
  begin
    NewEntryArray := NewMap.FBuckets[I].Entries;
    SetLength(NewEntryArray, Length(FBuckets[I].Entries));
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      NewEntryArray[J].Key := FBuckets[I].Entries[J].Key;
      NewEntryArray[J].Value := FBuckets[I].Entries[J].Value;
    end;
    NewMap.FBuckets[I].Count := FBuckets[I].Count;
  end;
  Result := NewMap;
end;

function TJclIntfIntfHashMap.ContainsKey(Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: PJclIntfIntfBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if Key = nil then
    Exit;
  Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[FHashFunction(Integer(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := True;
      Break;
    end;
end;

function TJclIntfIntfHashMap.ContainsValue(Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: PJclIntfIntfBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if Value = nil then
    Exit;
  for J := 0 to FCapacity - 1 do
  begin
    Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[J];
    for I := 0 to Bucket.Count - 1 do
      if Bucket.Entries[I].Value = Value then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

function TJclIntfIntfHashMap.Equals(AMap: IJclIntfIntfMap): Boolean;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AMap = nil then
    Exit;
  if FCount <> AMap.Size then
    Exit;
  Result := True;
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      if AMap.ContainsKey(FBuckets[I].Entries[J].Key) then
      begin
        if AMap.GetValue(FBuckets[I].Entries[J].Key) <>
          FBuckets[I].Entries[J].Value then
        begin
          Result := False;
          Exit;
        end;
      end
      else
      begin
        Result := False;
        Exit;
      end;
end;

function TJclIntfIntfHashMap.GetValue(Key: IInterface): IInterface;
var
  I: Integer;
  Bucket: PJclIntfIntfBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if Key = nil then
    Exit;
  Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[FHashFunction(Integer(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      Break;
    end;
end;

procedure TJclIntfIntfHashMap.GrowEntries(BucketIndex: Integer);
var
  Capacity: Integer;
begin
  Capacity := Length(FBuckets[BucketIndex].Entries);
  if Capacity > 64 then
    Capacity := Capacity + Capacity div 4
  else
    Capacity := Capacity * 4;
  SetLength(FBuckets[BucketIndex].Entries, Capacity);
end;

function TJclIntfIntfHashMap.HashMul(Key: Cardinal): Cardinal;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(FCapacity * (Frac(Key * A)));
end;

function TJclIntfIntfHashMap.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclIntfIntfHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := TJclIntfArraySet.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Key);
end;

procedure TJclIntfIntfHashMap.PutAll(AMap: IJclIntfIntfMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if AMap = nil then
    Exit;
  It := AMap.KeySet.First;
  while It.HasNext do
  begin
    Key := It.Next;
    PutValue(Key, AMap.GetValue(Key));
  end;
end;

procedure TJclIntfIntfHashMap.PutValue(Key, Value: IInterface);
var
  Index: Integer;
  Bucket: PJclIntfIntfBucket;
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if Key = nil then
    Exit;
  if Value = nil then
    Exit;
  Index := FHashFunction(Integer(Key));
  Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[Index];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Bucket.Entries[I].Value := Value;
      Exit;
    end;
  if Bucket.Count = Length(Bucket.Entries) then
    GrowEntries(Index);
  Bucket.Entries[Bucket.Count].Key := Key;
  Bucket.Entries[Bucket.Count].Value := Value;
  Inc(Bucket.Count);
  Inc(FCount);
end;

function TJclIntfIntfHashMap.Remove(Key: IInterface): IInterface;
var
  Bucket: PJclIntfIntfBucket;
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if Key = nil then
    Exit;
  Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[FHashFunction(Integer(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      Bucket.Entries[I].Key := nil;
      Bucket.Entries[I].Value := nil;
      if I < Length(Bucket.Entries) - 1 then
        MoveArray(Bucket.Entries, I + 1, I, Bucket.Count - I - 1);
      Dec(Bucket.Count);
      Dec(FCount);
      Break;
    end;
end;

function TJclIntfIntfHashMap.Size: Integer;
begin
  Result := FCount;
end;

function TJclIntfIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := TJclIntfArrayList.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Value);
end;

//=== { TJclStrIntfHashMap } =================================================

constructor TJclStrIntfHashMap.Create(ACapacity: Integer = DefaultContainerCapacity);
var
  I: Integer;
begin
  inherited Create;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FBuckets, FCapacity);
  for I := 0 to FCapacity - 1 do
    SetLength(FBuckets[I].Entries, 1);
  FHashFunction := HashMul;
end;

destructor TJclStrIntfHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclStrIntfHashMap.Clear;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  for I := 0 to FCapacity - 1 do
  begin
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      FBuckets[I].Entries[J].Key := '';
      FBuckets[I].Entries[J].Value := nil;
    end;
    FBuckets[I].Count := 0;
  end;
  FCount := 0;
end;

function TJclStrIntfHashMap.Clone: IInterface;
var
  I, J: Integer;
  NewEntryArray: TJclStrIntfEntryArray;
  NewMap: TJclStrIntfHashMap;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  NewMap := TJclStrIntfHashMap.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
  begin
    NewEntryArray := NewMap.FBuckets[I].Entries;
    SetLength(NewEntryArray, Length(FBuckets[I].Entries));
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      NewEntryArray[J].Key := FBuckets[I].Entries[J].Key;
      NewEntryArray[J].Value := FBuckets[I].Entries[J].Value;
    end;
    NewMap.FBuckets[I].Count := FBuckets[I].Count;
  end;
  Result := NewMap;
end;

function TJclStrIntfHashMap.ContainsKey(const Key: string): Boolean;
var
  I: Integer;
  Bucket: PJclStrIntfBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if Key = '' then
    Exit;
  Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[FHashFunction(HashString(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := True;
      Break;
    end;
end;

function TJclStrIntfHashMap.ContainsValue(Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: PJclStrIntfBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if Value = nil then
    Exit;
  for J := 0 to FCapacity - 1 do
  begin
    Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[J];
    for I := 0 to Bucket.Count - 1 do
      if Bucket.Entries[I].Value = Value then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

function TJclStrIntfHashMap.Equals(AMap: IJclStrIntfMap): Boolean;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AMap = nil then
    Exit;
  if FCount <> AMap.Size then
    Exit;
  Result := True;
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      if AMap.ContainsKey(FBuckets[I].Entries[J].Key) then
      begin
        if AMap.GetValue(FBuckets[I].Entries[J].Key) <>
          FBuckets[I].Entries[J].Value then
        begin
          Result := False;
          Exit;
        end;
      end
      else
      begin
        Result := False;
        Exit;
      end;
end;

function TJclStrIntfHashMap.GetValue(const Key: string): IInterface;
var
  I: Integer;
  Index: Integer;
  Bucket: PJclStrIntfBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if Key = '' then
    Exit;
  Index := FHashFunction(HashString(Key));
  Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[Index];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      Break;
    end;
end;

procedure TJclStrIntfHashMap.GrowEntries(BucketIndex: Integer);
var
  Capacity: Integer;
begin
  Capacity := Length(FBuckets[BucketIndex].Entries);
  if Capacity > 64 then
    Capacity := Capacity + Capacity div 4
  else
    Capacity := Capacity * 4;
  SetLength(FBuckets[BucketIndex].Entries, Capacity);
end;

function TJclStrIntfHashMap.HashMul(Key: Cardinal): Cardinal;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(FCapacity * (Frac(Key * A)));
end;

function TJclStrIntfHashMap.HashString(const Key: string): Cardinal;
var
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := 0;
  for I := 1 to Length(Key) do
    Result := Result + Cardinal(Ord(Key[I]) * (I - 1) * 256);
end;

function TJclStrIntfHashMap.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclStrIntfHashMap.KeySet: IJclStrSet;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := TJclStrArraySet.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Key);
end;

procedure TJclStrIntfHashMap.PutAll(AMap: IJclStrIntfMap);
var
  It: IJclStrIterator;
  Key: string;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if AMap = nil then
    Exit;
  It := AMap.KeySet.First;
  while It.HasNext do
  begin
    Key := It.Next;
    PutValue(Key, AMap.GetValue(Key));
  end;
end;

procedure TJclStrIntfHashMap.PutValue(const Key: string; Value: IInterface);
var
  Index: Integer;
  Bucket: PJclStrIntfBucket;
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if Key = '' then
    Exit;
  if Value = nil then
    Exit;
  Index := FHashFunction(HashString(Key));
  Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[Index];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Bucket.Entries[I].Value := Value;
      Exit;
    end;
  if Bucket.Count = Length(Bucket.Entries) then
    GrowEntries(Index);
  Bucket.Entries[Bucket.Count].Key := Key;
  Bucket.Entries[Bucket.Count].Value := Value;
  Inc(Bucket.Count);
  Inc(FCount);
end;

function TJclStrIntfHashMap.Remove(const Key: string): IInterface;
var
  Bucket: PJclStrIntfBucket;
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if Key = '' then
    Exit;
  Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[FHashFunction(HashString(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      Bucket.Entries[I].Key := '';
      Bucket.Entries[I].Value := nil;
      if I < Length(Bucket.Entries) - 1 then
        MoveArray(Bucket.Entries, I + 1, I, Bucket.Count - I - 1);
      Dec(Bucket.Count);
      Dec(FCount);
      Break;
    end;
end;

function TJclStrIntfHashMap.Size: Integer;
begin
  Result := FCount;
end;

function TJclStrIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := TJclIntfArrayList.Create;
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Value);
end;

//=== { TJclStrStrHashMap } ==================================================

constructor TJclStrStrHashMap.Create(ACapacity: Integer = DefaultContainerCapacity);
var
  I: Integer;
begin
  inherited Create;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FBuckets, FCapacity);
  for I := 0 to FCapacity - 1 do
    SetLength(FBuckets[I].Entries, 1);
  FHashFunction := HashMul;
end;

destructor TJclStrStrHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclStrStrHashMap.Clear;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  for I := 0 to FCapacity - 1 do
  begin
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      FBuckets[I].Entries[J].Key := '';
      FBuckets[I].Entries[J].Value := '';
    end;
    FBuckets[I].Count := 0;
  end;
  FCount := 0;
end;

function TJclStrStrHashMap.Clone: IInterface;
var
  I, J: Integer;
  NewEntryArray: TJclStrStrEntryArray;
  NewMap: TJclStrStrHashMap;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  NewMap := TJclStrStrHashMap.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
  begin
    NewEntryArray := NewMap.FBuckets[I].Entries;
    SetLength(NewEntryArray, Length(FBuckets[I].Entries));
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      NewEntryArray[J].Key := FBuckets[I].Entries[J].Key;
      NewEntryArray[J].Value := FBuckets[I].Entries[J].Value;
    end;
    NewMap.FBuckets[I].Count := FBuckets[I].Count;
  end;
  Result := NewMap;
end;

function TJclStrStrHashMap.ContainsKey(const Key: string): Boolean;
var
  I: Integer;
  Bucket: PJclStrStrBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if Key = '' then
    Exit;
  Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[FHashFunction(HashString(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := True;
      Break;
    end;
end;

function TJclStrStrHashMap.ContainsValue(const Value: string): Boolean;
var
  I, J: Integer;
  Bucket: PJclStrStrBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if Value = '' then
    Exit;
  for J := 0 to FCapacity - 1 do
  begin
    Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[J];
    for I := 0 to Bucket.Count - 1 do
      if Bucket.Entries[I].Value = Value then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

function TJclStrStrHashMap.Equals(AMap: IJclStrStrMap): Boolean;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AMap = nil then
    Exit;
  if FCount <> AMap.Size then
    Exit;
  Result := True;
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      if AMap.ContainsKey(FBuckets[I].Entries[J].Key) then
      begin
        if AMap.GetValue(FBuckets[I].Entries[J].Key) <>
          FBuckets[I].Entries[J].Value then
        begin
          Result := False;
          Exit;
        end;
      end
      else
      begin
        Result := False;
        Exit;
      end;
end;

function TJclStrStrHashMap.GetValue(const Key: string): string;
var
  I: Integer;
  Bucket: PJclStrStrBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := '';
  if Key = '' then
    Exit;
  Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[FHashFunction(HashString(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      Break;
    end;
end;

procedure TJclStrStrHashMap.GrowEntries(BucketIndex: Integer);
var
  Capacity: Integer;
begin
  Capacity := Length(FBuckets[BucketIndex].Entries);
  if Capacity > 64 then
    Capacity := Capacity + Capacity div 4
  else
    Capacity := Capacity * 4;
  SetLength(FBuckets[BucketIndex].Entries, Capacity);
end;

function TJclStrStrHashMap.HashMul(Key: Cardinal): Cardinal;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(FCapacity * (Frac(Key * A)));
end;

function TJclStrStrHashMap.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclStrStrHashMap.KeyOfValue(const Value: string): string;
var
  I, J: Integer;
  Bucket: PJclStrStrBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if Value = '' then
    Exit;
  for J := 0 to FCapacity - 1 do
  begin
    Bucket := {$IFNDEF CLR}@{$ENDIF}(FBuckets[J]);
    for I := 0 to Bucket.Count - 1 do
      if Bucket.Entries[I].Value = Value then
      begin
        Result := Bucket.Entries[I].Key;
        Exit;
      end;
  end;
  {$IFDEF CLR}
  raise EJclError.CreateFmt(RsEValueNotFound, [Value]);
  {$ELSE}
  raise EJclError.CreateResFmt(@RsEValueNotFound, [Value]);
  {$ENDIF CLR}
end;

function TJclStrStrHashMap.KeySet: IJclStrSet;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := TJclStrArraySet.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Key);
end;

procedure TJclStrStrHashMap.PutAll(AMap: IJclStrStrMap);
var
  It: IJclStrIterator;
  Key: string;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if AMap = nil then
    Exit;
  It := AMap.KeySet.First;
  while It.HasNext do
  begin
    Key := It.Next;
    PutValue(Key, AMap.GetValue(Key));
  end;
end;

procedure TJclStrStrHashMap.PutValue(const Key, Value: string);
var
  Index: Integer;
  Bucket: PJclStrStrBucket;
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if Key = '' then
    Exit;
  if Value = '' then
    Exit;
  Index := FHashFunction(HashString(Key));
  Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[Index];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Bucket.Entries[I].Value := Value;
      Exit;
    end;
  if Bucket.Count = Length(Bucket.Entries) then
    GrowEntries(Index);
  Bucket.Entries[Bucket.Count].Key := Key;
  Bucket.Entries[Bucket.Count].Value := Value;
  Inc(Bucket.Count);
  Inc(FCount);
end;

function TJclStrStrHashMap.Remove(const Key: string): string;
var
  Bucket: PJclStrStrBucket;
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := '';
  if Key = '' then
    Exit;
  Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[FHashFunction(HashString(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      Bucket.Entries[I].Key := '';
      Bucket.Entries[I].Value := '';
      if I < Length(Bucket.Entries) - 1 then
        MoveArray(Bucket.Entries, I + 1, I, Bucket.Count - I - 1);
      Dec(Bucket.Count);
      Dec(FCount);
      Break;
    end;
end;

function TJclStrStrHashMap.Size: Integer;
begin
  Result := FCount;
end;

function TJclStrStrHashMap.Values: IJclStrCollection;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := TJclStrArrayList.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Value);
end;

function TJclStrStrHashMap.HashString(const Key: string): Cardinal;
var
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := 0;
  for I := 1 to Length(Key) do
    Result := Result + Cardinal(Ord(Key[I]) * (I - 1) * 256);
end;

//=== { TJclStrHashMap } =====================================================

constructor TJclStrHashMap.Create(ACapacity: Integer = DefaultContainerCapacity;
  AOwnsObjects: Boolean = True);
var
  I: Integer;
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FBuckets, FCapacity);
  for I := 0 to FCapacity - 1 do
    SetLength(FBuckets[I].Entries, 1);
  FHashFunction := HashMul;
end;

destructor TJclStrHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclStrHashMap.Clear;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  for I := 0 to FCapacity - 1 do
  begin
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      FBuckets[I].Entries[J].Key := '';
      FreeObject(FBuckets[I].Entries[J].Value);
    end;
    FBuckets[I].Count := 0;
  end;
  FCount := 0;
end;

function TJclStrHashMap.Clone: TObject;
var
  I, J: Integer;
  NewEntryArray: TJclStrEntryArray;
  NewMap: TJclStrHashMap;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  NewMap := TJclStrHashMap.Create(FCapacity, False);
  // Only one can have FOwnsObjects = True
  for I := 0 to FCapacity - 1 do
  begin
    NewEntryArray := NewMap.FBuckets[I].Entries;
    SetLength(NewEntryArray, Length(FBuckets[I].Entries));
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      NewEntryArray[J].Key := FBuckets[I].Entries[J].Key;
      NewEntryArray[J].Value := FBuckets[I].Entries[J].Value;
    end;
    NewMap.FBuckets[I].Count := FBuckets[I].Count;
  end;
  Result := NewMap;
end;

function TJclStrHashMap.ContainsKey(const Key: string): Boolean;
var
  I: Integer;
  Bucket: PJclStrBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if Key = '' then
    Exit;
  Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[FHashFunction(HashString(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := True;
      Break;
    end;
end;

function TJclStrHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: PJclStrBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if Value = nil then
    Exit;
  for J := 0 to FCapacity - 1 do
  begin
    Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[J];
    for I := 0 to Bucket.Count - 1 do
      if Bucket.Entries[I].Value = Value then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

function TJclStrHashMap.Equals(AMap: IJclStrMap): Boolean;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AMap = nil then
    Exit;
  if FCount <> AMap.Size then
    Exit;
  Result := True;
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      if AMap.ContainsKey(FBuckets[I].Entries[J].Key) then
      begin
        if AMap.GetValue(FBuckets[I].Entries[J].Key) <>
          FBuckets[I].Entries[J].Value then
        begin
          Result := False;
          Exit;
        end;
      end
      else
      begin
        Result := False;
        Exit;
      end;
end;

function TJclStrHashMap.GetValue(const Key: string): TObject;
var
  I: Integer;
  Bucket: PJclStrBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if Key = '' then
    Exit;
  I := FHashFunction(HashString(Key));
  Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[I];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      Break;
    end;
end;

procedure TJclStrHashMap.FreeObject(var AObject: TObject);
begin
  // TODO: trap destructor exceptions
  if FOwnsObjects then
  begin
    AObject.Free;
    AObject := nil;
  end;
end;

procedure TJclStrHashMap.GrowEntries(BucketIndex: Integer);
var
  Capacity: Integer;
begin
  Capacity := Length(FBuckets[BucketIndex].Entries);
  if Capacity > 64 then
    Capacity := Capacity + Capacity div 4
  else
    Capacity := Capacity * 4;
  SetLength(FBuckets[BucketIndex].Entries, Capacity);
end;

function TJclStrHashMap.HashMul(Key: Cardinal): Cardinal;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(FCapacity * (Frac(Key * A)));
  //Result := LongRec(Key).Bytes[1] and $FF;
end;

function TJclStrHashMap.HashString(const Key: string): Cardinal;
var
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := 0;
  for I := 1 to Length(Key) do
    Result := Result + Cardinal(Ord(Key[I]) * (I - 1) * 256);
end;

function TJclStrHashMap.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclStrHashMap.KeySet: IJclStrSet;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := TJclStrArraySet.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Key);
end;

procedure TJclStrHashMap.PutAll(AMap: IJclStrMap);
var
  It: IJclStrIterator;
  Key: string;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if AMap = nil then
    Exit;
  It := AMap.KeySet.First;
  while It.HasNext do
  begin
    Key := It.Next;
    PutValue(Key, AMap.GetValue(Key));
  end;
end;

procedure TJclStrHashMap.PutValue(const Key: string; Value: TObject);
var
  Index: Integer;
  Bucket: PJclStrBucket;
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if Key = '' then
    Exit;
  if Value = nil then
    Exit;
  Index := FHashFunction(HashString(Key));
  Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[Index];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      FreeObject(Bucket.Entries[I].Value);
      Bucket.Entries[I].Value := Value;
      Exit;
    end;
  if Bucket.Count = Length(Bucket.Entries) then
    GrowEntries(Index);
  Bucket.Entries[Bucket.Count].Key := Key;
  Bucket.Entries[Bucket.Count].Value := Value;
  Inc(Bucket.Count);
  Inc(FCount);
end;

function TJclStrHashMap.Remove(const Key: string): TObject;
var
  Bucket: PJclStrBucket;
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if Key = '' then
    Exit;
  Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[FHashFunction(HashString(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      FreeObject(Bucket.Entries[I].Value);
      Result := Bucket.Entries[I].Value;
      Bucket.Entries[I].Key := '';
      if I < Length(Bucket.Entries) - 1 then
        MoveArray(Bucket.Entries, I + 1, I, Bucket.Count - I - 1);
      Dec(Bucket.Count);
      Dec(FCount);
      Break;
    end;
end;

function TJclStrHashMap.Size: Integer;
begin
  Result := FCount;
end;

function TJclStrHashMap.Values: IJclCollection;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := TJclArrayList.Create(FCapacity, False); // NEVER Owns Objects !
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Value);
end;

//=== { TJclHashMap } ========================================================

constructor TJclHashMap.Create(ACapacity: Integer = DefaultContainerCapacity;
  AOwnsObjects: Boolean = True);
var
  I: Integer;
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FBuckets, FCapacity);
  for I := 0 to FCapacity - 1 do
    SetLength(FBuckets[I].Entries, 64);
  FHashFunction := HashMul;
end;

destructor TJclHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclHashMap.Clear;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  for I := 0 to FCapacity - 1 do
  begin
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      FBuckets[I].Entries[J].Key := nil; 
      FreeObject(FBuckets[I].Entries[J].Value);
    end;
    FBuckets[I].Count := 0;
  end;
  FCount := 0;
end;

function TJclHashMap.Clone: TObject;
var
  I, J: Integer;
  NewEntryArray: TJclEntryArray;
  NewMap: TJclHashMap;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  // only one can owns objects
  NewMap := TJclHashMap.Create(FCapacity, False);
  for I := 0 to FCapacity - 1 do
  begin
    NewEntryArray := NewMap.FBuckets[I].Entries;
    SetLength(NewEntryArray, Length(FBuckets[I].Entries));
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      NewEntryArray[J].Key := FBuckets[I].Entries[J].Key;
      NewEntryArray[J].Value := FBuckets[I].Entries[J].Value;
    end;
    NewMap.FBuckets[I].Count := FBuckets[I].Count;
  end;
  Result := NewMap;
end;

function TJclHashMap.ContainsKey(Key: TObject): Boolean;
var
  I: Integer;
  Bucket: PJclBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if Key = nil then
    Exit;
  Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[FHashFunction(Integer(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := True;
      Exit;
    end;
end;

function TJclHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: PJclBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if Value = nil then
    Exit;
  for J := 0 to FCapacity - 1 do
  begin
    Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[J];
    for I := 0 to Bucket.Count - 1 do
      if Bucket.Entries[I].Value = Value then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

function TJclHashMap.Equals(AMap: IJclMap): Boolean;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AMap = nil then
    Exit;
  if FCount <> AMap.Size then
    Exit;
  Result := True;
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      if AMap.ContainsKey(FBuckets[I].Entries[J].Key) then
      begin
        if AMap.GetValue(FBuckets[I].Entries[J].Key) <>
          FBuckets[I].Entries[J].Value then
        begin
          Result := False;
          Exit;
        end;
      end
      else
      begin
        Result := False;
        Exit;
      end;
end;

procedure TJclHashMap.FreeObject(var AObject: TObject);
begin
  // TODO: trap destructor exceptions
  if FOwnsObjects then
  begin
    AObject.Free;
    AObject := nil;
  end;
end;

function TJclHashMap.GetValue(Key: TObject): TObject;
var
  I: Integer;
  Bucket: PJclBucket;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if Key = nil then
    Exit;
  Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[FHashFunction(Integer(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      Break;
    end;
end;

procedure TJclHashMap.GrowEntries(BucketIndex: Integer);
var
  Capacity: Integer;
begin
  Capacity := Length(FBuckets[BucketIndex].Entries);
  if Capacity > 64 then
    Capacity := Capacity + Capacity div 4
  else
    Capacity := Capacity * 4;
  SetLength(FBuckets[BucketIndex].Entries, Capacity);
end;

function TJclHashMap.HashMul(Key: Cardinal): Cardinal;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(FCapacity * (Frac(Key * A)));
  //Result := LongRec(Key).Bytes[1] and $FF;
end;

function TJclHashMap.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclHashMap.KeySet: IJclSet;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := TJclArraySet.Create(FCapacity, False); // NEVER Owns Objects !
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Key);
end;

procedure TJclHashMap.PutAll(AMap: IJclMap);
var
  It: IJclIterator;
  Key: TObject;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if AMap = nil then
    Exit;
  It := AMap.KeySet.First;
  while It.HasNext do
  begin
    Key := It.Next;
    PutValue(Key, AMap.GetValue(Key));
  end;
end;

procedure TJclHashMap.PutValue(Key, Value: TObject);
var
  Index: Integer;
  Bucket: PJclBucket;
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if Key = nil then
    Exit;
  if Value = nil then
    Exit;
  Index := FHashFunction(Integer(Key));
  Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[Index];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      FreeObject(Bucket.Entries[I].Value);
      Bucket.Entries[I].Value := Value;
      Exit;
    end;
  if Bucket.Count = Length(Bucket.Entries) then
    GrowEntries(Index);
  begin
    Bucket.Entries[Bucket.Count].Key := Key;
    Bucket.Entries[Bucket.Count].Value := Value;
  end;
  Inc(Bucket.Count);
  Inc(FCount);
end;

function TJclHashMap.Remove(Key: TObject): TObject;
var
  Bucket: PJclBucket;
  I: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := nil;
  if Key = nil then
    Exit;
  Bucket := {$IFNDEF CLR}@{$ENDIF}FBuckets[FHashFunction(Integer(Key))];
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      FreeObject(Bucket.Entries[I].Value);
      Result := Bucket.Entries[I].Value;
      if I < Length(Bucket.Entries) - 1 then
        MoveArray(Bucket.Entries, I + 1, I, Bucket.Count - I - 1);
      Dec(Bucket.Count);
      Dec(FCount);
      Break;
    end;
end;

function TJclHashMap.Size: Integer;
begin
  Result := FCount;
end;

function TJclHashMap.Values: IJclCollection;
var
  I, J: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := TJclArrayList.Create(FCapacity, False); // NEVER Owns Objects !
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Value);
end;

// History:

// $Log$
// Revision 1.9  2006/03/26 00:41:20  outchy
// IT3605 and IT3536: memory corruption and memory leaks fixed.
//
// Revision 1.8  2005/08/09 10:30:21  ahuser
// JCL.NET changes
//
// Revision 1.7  2005/08/07 14:14:34  outchy
// IT3044: The Count was not decremented after the removal of an item.
//
// Revision 1.6  2005/05/05 20:08:42  ahuser
// JCL.NET support
//
// Revision 1.5  2005/04/29 15:31:56  outchy
// IT2890, a string reference was not decremented as expected.
//
// Revision 1.4  2005/03/08 08:33:16  marquardt
// overhaul of exceptions and resourcestrings, minor style cleaning
//
// Revision 1.3  2005/02/27 11:36:20  marquardt
// fixed and secured Capacity/Grow mechanism, raise exceptions with efficient CreateResRec
//
// Revision 1.2  2005/02/27 07:27:47  marquardt
// changed interface names from I to IJcl, moved resourcestrings to JclResource.pas
//
// Revision 1.1  2005/02/24 03:57:10  rrossmair
// - donated DCL code, initial check-in
//

end.

