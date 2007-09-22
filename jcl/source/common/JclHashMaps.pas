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
{ Contributors:                                                                                    }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclHashMaps;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF SUPPORTS_GENERICS}
  {$IFDEF CLR}
  System.Collections.Generic,
  {$ENDIF CLR}
  JclAlgorithms,
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclContainerIntf;

type
  TJclIntfIntfEntry = record
    Key: IInterface;
    Value: IInterface;
  end;

  TJclIntfIntfEntryArray = array of TJclIntfIntfEntry;

  TJclIntfIntfBucket = class
  public
    Count: Integer;
    Entries: TJclIntfIntfEntryArray;
  end;

  TJclIntfIntfBucketArray = array of TJclIntfIntfBucket;

  // Hash Function
  // Result must be in 0..Range-1
  TJclHashFunction = function(Key, Range: Cardinal): Cardinal;

  TJclIntfIntfHashMap = class(TJclAbstractContainer, IJclIntfIntfMap,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TJclIntfIntfBucketArray;
    FHashFunction: TJclHashFunction;
    procedure MoveArray(var List: TJclIntfIntfEntryArray; FromIndex, ToIndex, Count: Integer);
  protected
    procedure GrowEntries(Bucket: TJclIntfIntfBucket); virtual;
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfIntfMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclIntfIntfMap): Boolean;
    function GetValue(const Key: IInterface): IInterface;
    function IsEmpty: Boolean;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfIntfMap);
    procedure PutValue(const Key, Value: IInterface);
    function Remove(const Key: IInterface): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write
      FHashFunction;
  end;

  TJclStrIntfEntry = record
    Key: string;
    Value: IInterface;
  end;

  TJclStrIntfEntryArray = array of TJclStrIntfEntry;

  TJclStrIntfBucket = class
  public
    Count: Integer;
    Entries: TJclStrIntfEntryArray;
  end;

  TJclStrIntfBucketArray = array of TJclStrIntfBucket;

  TJclStrIntfHashMap = class(TJclAbstractContainer, IJclStrIntfMap,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TJclStrIntfBucketArray;
    FHashFunction: TJclHashFunction;
    function HashString(const Key: string): Cardinal;
    procedure MoveArray(var List: TJclStrIntfEntryArray; FromIndex, ToIndex, Count: Integer);
  protected
    procedure GrowEntries(Bucket: TJclStrIntfBucket); virtual;
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfMap }
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclStrIntfMap): Boolean;
    function GetValue(const Key: string): IInterface;
    function IsEmpty: Boolean;
    function KeySet: IJclStrSet;
    procedure PutAll(const AMap: IJclStrIntfMap);
    procedure PutValue(const Key: string; const Value: IInterface);
    function Remove(const Key: string): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write
      FHashFunction;
  end;

  TJclStrStrEntry = record
    Key: string;
    Value: string;
  end;

  TJclStrStrEntryArray = array of TJclStrStrEntry;

  TJclStrStrBucket = class
  public
    Count: Integer;
    Entries: TJclStrStrEntryArray;
  end;

  TJclStrStrBucketArray = array of TJclStrStrBucket;

  TJclStrStrHashMap = class(TJclAbstractContainer, IJclStrStrMap,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TJclStrStrBucketArray;
    FHashFunction: TJclHashFunction;
    function HashString(const Key: string): Cardinal;
    procedure MoveArray(var List: TJclStrStrEntryArray; FromIndex, ToIndex, Count: Integer);
  protected
    procedure GrowEntries(Bucket: TJclStrStrBucket); virtual;
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclCloneable }
    function Clone: TObject;
    { IJclStrStrMap }
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(const Value: string): Boolean;
    function Equals(const AMap: IJclStrStrMap): Boolean;
    function GetValue(const Key: string): string;
    function IsEmpty: Boolean;
    function KeySet: IJclStrSet;
    procedure PutAll(const AMap: IJclStrStrMap);
    procedure PutValue(const Key, Value: string);
    function Remove(const Key: string): string;
    function Size: Integer;
    function Values: IJclStrCollection;
    // Daniele Teti
    function KeyOfValue(const Value: string): string;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write
      FHashFunction;
  end;

  TJclStrEntry = record
    Key: string;
    Value: TObject;
  end;

  TJclStrEntryArray = array of TJclStrEntry;

  TJclStrBucket = class
  public
    Count: Integer;
    Entries: TJclStrEntryArray;
  end;

  TJclStrBucketArray = array of TJclStrBucket;

  TJclStrHashMap = class(TJclAbstractContainer, IJclStrMap,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TJclStrBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsValues: Boolean;
    function HashString(const Key: string): Cardinal;
    procedure MoveArray(var List: TJclStrEntryArray; FromIndex, ToIndex, Count: Integer);
  protected
    procedure GrowEntries(Bucket: TJclStrBucket); virtual;
    procedure FreeValue(var AValue: TObject);
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclCloneable }
    function Clone: TObject;
    { IJclStrMap }
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclStrMap): Boolean;
    function GetValue(const Key: string): TObject;
    function IsEmpty: Boolean;
    function KeySet: IJclStrSet;
    procedure PutAll(const AMap: IJclStrMap);
    procedure PutValue(const Key: string; Value: TObject);
    function Remove(const Key: string): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity; AOwnsValues: Boolean = True);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write
      FHashFunction;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  TJclEntry = record
    Key: TObject;
    Value: TObject;
  end;

  TJclEntryArray = array of TJclEntry;

  TJclBucket = class
  public
    Count: Integer;
    Entries: TJclEntryArray;
  end;

  TJclBucketArray = array of TJclBucket;

  TJclHashMap = class(TJclAbstractContainer, IJclMap,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TJclBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
    procedure MoveArray(var List: TJclEntryArray; FromIndex, ToIndex, Count: Integer);
  protected
    procedure GrowEntries(Bucket: TJclBucket); virtual;
    procedure FreeValue(var AValue: TObject);
    procedure FreeKey(var AKey: TObject);
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclCloneable }
    function Clone: TObject;
    { IJclMap }
    procedure Clear;
    function ContainsKey(Key: TObject): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclMap): Boolean;
    function GetValue(Key: TObject): TObject;
    function IsEmpty: Boolean;
    function KeySet: IJclSet;
    procedure PutAll(const AMap: IJclMap);
    procedure PutValue(Key, Value: TObject);
    function Remove(Key: TObject): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity; AOwnsValues: Boolean = True;
      AOwnsKeys: Boolean = False);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  TJclEntry<TKey, TValue> = record
    Key: TKey;
    Value: TValue;
  end;

  TJclEntryArray<TKey, TValue> = array of TJclEntry<TKey, TValue>;

  TJclBucket<TKey, TValue> = class
  public
    Count: Integer;
    Entries: TJclEntryArray<TKey, TValue>;
  end;

  TJclBucketArray<TKey, TValue> = array of TJclBucket<TKey, TValue>;

  TJclHashMap<TKey, TValue> = class(TJclAbstractContainer, IJclMap<TKey,TValue>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TJclBucketArray<TKey, TValue>;
    FOwnsValues: Boolean;
    FOwnsKeys: Boolean;
    FHashFunction: TJclHashFunction;
    procedure MoveArray(var List: TJclEntryArray<TKey, TValue>; FromIndex, ToIndex, Count: Integer);
  protected
    procedure GrowEntries(Bucket: TJclBucket<TKey,TValue>); virtual;
    procedure FreeValue(var AValue: TValue);
    procedure FreeKey(var AKey: TKey);
    function KeysEqual(const A, B: TKey): Boolean; virtual; abstract;
    function ValuesEqual(const A, B: TValue): Boolean; virtual; abstract;
    function KeyToOrd(const AKey: TKey): Cardinal; virtual; abstract;
    function CreateEmptyArrayList(ACapacity: Integer): IJclCollection<TValue>; virtual; abstract;
    function CreateEmptyHashMap(ACapacity: Integer): TJclHashMap<TKey, TValue>; virtual; abstract;
    function CreateEmptyArraySet(ACapacity: Integer): IJclSet<TKey>; virtual; abstract;
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclCloneable }
    function Clone: TObject;
    { IJclMap<TKey,TValue> }
    procedure Clear;
    function ContainsKey(const Key: TKey): Boolean;
    function ContainsValue(const Value: TValue): Boolean;
    function Equals(const AMap: IJclMap<TKey, TValue>): Boolean;
    function GetValue(Key: TKey): TValue;
    function IsEmpty: Boolean;
    function KeySet: IJclSet<TKey>;
    procedure PutAll(const AMap: IJclMap<TKey, TValue>);
    procedure PutValue(Key: TKey; const Value: TValue);
    function Remove(const Key: TKey): TValue;
    function Size: Integer;
    function Values: IJclCollection<TValue>;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity; AOwnsValues: Boolean = True;
      AOwnsKeys: Boolean = False);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsValues: Boolean read FOwnsValues;
    property OwnsKeys: Boolean read FOwnsKeys;
  end;

  // E = external helper to compare and hash items
  // KeyComparer is used only when getting KeySet
  // GetHashCode and Equals methods of KeyEqualityComparer are used
  // GetHashCode of ValueEqualityComparer is not used
  TJclHashMapE<TKey, TValue> = class(TJclHashMap<TKey, TValue>, IJclMap<TKey,TValue>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable)
  private
    FKeyEqualityComparer: IEqualityComparer<TKey>;
    FKeyComparer: IComparer<TKey>;
    FValueEqualityComparer: IEqualityComparer<TValue>;
  protected
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function KeyToOrd(const AKey: TKey): Cardinal; override;
    function CreateEmptyArrayList(ACapacity: Integer): IJclCollection<TValue>; override;
    function CreateEmptyHashMap(ACapacity: Integer): TJclHashMap<TKey, TValue>; override;
    function CreateEmptyArraySet(ACapacity: Integer): IJclSet<TKey>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AKeyEqualityComparer: IEqualityComparer<TKey>;
      const AValueEqualityComparer: IEqualityComparer<TValue>;
      const AKeyComparer: IComparer<TKey>; ACapacity: Integer = DefaultContainerCapacity;
      AOwnsValues: Boolean = True; AOwnsKeys: Boolean = False);

    property KeyEqualityComparer: IEqualityComparer<TKey> read FKeyEqualityComparer write FKeyEqualityComparer;
    property KeyComparer: IComparer<TKey> read FKeyComparer write FKeyComparer;
    property ValueEqualityComparer: IEqualityComparer<TValue> read FValueEqualityComparer write FValueEqualityComparer;
  end;

  // F = Functions to compare and hash items
  // KeyComparer is used only when getting KeySet
  TJclHashMapF<TKey, TValue> = class(TJclHashMap<TKey, TValue>, IJclMap<TKey,TValue>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable)
  private
    FKeyEqualityCompare: TEqualityCompare<TKey>;
    FKeyHash: THash<TKey>;
    FKeyCompare: TCompare<TKey>;
    FValueEqualityCompare: TEqualityCompare<TValue>;
  protected
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function KeyToOrd(const AKey: TKey): Cardinal; override;
    function CreateEmptyArrayList(ACapacity: Integer): IJclCollection<TValue>; override;
    function CreateEmptyHashMap(ACapacity: Integer): TJclHashMap<TKey, TValue>; override;
    function CreateEmptyArraySet(ACapacity: Integer): IJclSet<TKey>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(AKeyEqualityCompare: TEqualityCompare<TKey>; AKeyHash: THash<TKey>;
      AValueEqualityCompare: TEqualityCompare<TValue>; AKeyCompare: TCompare<TKey>;
      ACapacity: Integer = DefaultContainerCapacity; AOwnsValues: Boolean = True; AOwnsKeys: Boolean = False);

    property KeyEqualityCompare: TEqualityCompare<TKey> read FKeyEqualityCompare write FKeyEqualityCompare;
    property KeyCompare: TCompare<TKey> read FKeyCompare write FKeyCompare;
    property KeyHash: THash<TKey> read FKeyHash write FKeyHash;
    property ValueEqualityCompare: TEqualityCompare<TValue> read FValueEqualityCompare write FValueEqualityCompare;
  end;

  // I = items can compare themselves to an other, items can create hash value from themselves
  TJclHashMapI<TKey: IComparable<TKey>, IEquatable<TKey>, IHashable; TValue: IEquatable<TValue>> = class(TJclHashMap<TKey, TValue>,
    IJclMap<TKey,TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable,
    IJclPackable)
  protected
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function KeyToOrd(const AKey: TKey): Cardinal; override;
    function CreateEmptyArrayList(ACapacity: Integer): IJclCollection<TValue>; override;
    function CreateEmptyHashMap(ACapacity: Integer): TJclHashMap<TKey, TValue>; override;
    function CreateEmptyArraySet(ACapacity: Integer): IJclSet<TKey>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;
  {$ENDIF SUPPORTS_GENERICS}

function HashMul(Key, Range: Cardinal): Cardinal;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils,
  JclArrayLists, JclArraySets, JclResources;

function HashMul(Key, Range: Cardinal): Cardinal;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(Range * (Frac(Key * A)));
end;

//=== { TJclIntfIntfHashMap } ================================================

constructor TJclIntfIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
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
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Count - 1 do
        begin
          Bucket.Entries[J].Key := nil;
          Bucket.Entries[J].Value := nil;
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FCount := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.Clone: TObject;
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfIntfBucket;
  NewMap: TJclIntfIntfHashMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := TJclIntfIntfHashMap.Create(FCapacity);
    NewMap.HashFunction := HashFunction;
    for I := 0 to FCapacity - 1 do
    begin
      SelfBucket := FBuckets[I];
      if SelfBucket <> nil then
      begin
        NewBucket := TJclIntfIntfBucket.Create;
        SetLength(NewBucket.Entries, SelfBucket.Count);
        for J := 0 to SelfBucket.Count - 1 do
        begin
          NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
          NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
        end;
        NewBucket.Count := SelfBucket.Count;
        NewMap.FBuckets[I] := NewBucket;
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Integer(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Count - 1 do
        if Bucket.Entries[I].Key = Key then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Count - 1 do
          if Bucket.Entries[I].Value = Value then
          begin
            Result := True;
            Exit;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.Equals(const AMap: IJclIntfIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FCount <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Count - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if AMap.GetValue(Bucket.Entries[J].Key) <> Bucket.Entries[J].Value then
              Exit;
          end
          else
            Exit;
      end;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TJclIntfIntfHashMap.GetValue(const Key: IInterface): IInterface;
var
  I: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Integer(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Count - 1 do
        if Bucket.Entries[I].Key = Key then
        begin
          Result := Bucket.Entries[I].Value;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.GrowEntries(Bucket: TJclIntfIntfBucket);
var
  BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  if BucketCapacity > 64 then
    BucketCapacity := BucketCapacity + BucketCapacity div 4
  else
    BucketCapacity := BucketCapacity * 4;
  SetLength(Bucket.Entries, BucketCapacity);
end;

function TJclIntfIntfHashMap.IntfClone: IInterface;
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfIntfBucket;
  NewMap: TJclIntfIntfHashMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := TJclIntfIntfHashMap.Create(FCapacity);
    for I := 0 to FCapacity - 1 do
    begin
      SelfBucket := FBuckets[I];
      if SelfBucket <> nil then
      begin
        NewBucket := TJclIntfIntfBucket.Create;
        SetLength(NewBucket.Entries, SelfBucket.Count);
        for J := 0 to SelfBucket.Count - 1 do
        begin
          NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
          NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
        end;
        NewBucket.Count := SelfBucket.Count;
        NewMap.FBuckets[I] := NewBucket;
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclIntfIntfHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FCount);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Count - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.MoveArray(var List: TJclIntfIntfEntryArray;
  FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    for I := 0 to ToIndex - FromIndex - 1 do
    begin
      List[FromIndex + I].Key := nil;
      List[FromIndex + I].Value := nil;
    end;
  end
  else
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    for I := 0 to FromIndex - ToIndex - 1 do
    begin
      List[FromIndex + Count - 1 + I].Key := nil;
      List[FromIndex + Count - 1 + I].Value := nil;
    end;
  end;
{$ELSE}
begin
  Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
  { Keep reference counting working }
  if FromIndex < ToIndex then
    FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
  else
  if FromIndex > ToIndex then
    FillChar(List[FromIndex + Count - 1], (FromIndex - ToIndex) * SizeOf(List[0]), 0);
{$ENDIF CLR}
end;

procedure TJclIntfIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Count > 0 then
          SetLength(Bucket.Entries, Bucket.Count)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.PutAll(const AMap: IJclIntfIntfMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.PutValue(const Key, Value: IInterface);
var
  Index: Integer;
  Bucket: TJclIntfIntfBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Index := FHashFunction(Integer(Key), FCapacity);
    Bucket := FBuckets[Index];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Count - 1 do
        if Bucket.Entries[I].Key = Key then
        begin
          Bucket.Entries[I].Value := Value;
          Exit;
        end;
    end
    else
    begin
      Bucket := TJclIntfIntfBucket.Create;
      SetLength(Bucket.Entries, 1);
      FBuckets[Index] := Bucket;
    end;

    if Bucket.Count = Length(Bucket.Entries) then
      GrowEntries(Bucket);
    Bucket.Entries[Bucket.Count].Key := Key;
    Bucket.Entries[Bucket.Count].Value := Value;
    Inc(Bucket.Count);
    Inc(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.Remove(const Key: IInterface): IInterface;
var
  Bucket: TJclIntfIntfBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Integer(Key), FCapacity)];
    if Bucket <> nil then
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
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value >= FCount then
    begin
      SetLength(FBuckets, Value);
      FCapacity := Value;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.Size: Integer;
begin
  Result := FCount;
end;

function TJclIntfIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FCount);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Count - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclStrIntfHashMap } =================================================

constructor TJclStrIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
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
  Bucket: TJclStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Count - 1 do
        begin
          Bucket.Entries[J].Key := '';
          Bucket.Entries[J].Value := nil;
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FCount := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrIntfHashMap.Clone: TObject;
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclStrIntfBucket;
  NewMap: TJclStrIntfHashMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := TJclStrIntfHashMap.Create(FCapacity);
    NewMap.HashFunction := HashFunction;
    for I := 0 to FCapacity - 1 do
    begin
      SelfBucket := FBuckets[I];
      if SelfBucket <> nil then
      begin
        NewBucket := TJclStrIntfBucket.Create;
        SetLength(NewBucket.Entries, SelfBucket.Count);
        for J := 0 to SelfBucket.Count - 1 do
        begin
          NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
          NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
        end;
        NewBucket.Count := SelfBucket.Count;
        NewMap.FBuckets[I] := NewBucket;
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrIntfHashMap.ContainsKey(const Key: string): Boolean;
var
  I: Integer;
  Bucket: TJclStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(HashString(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Count - 1 do
      if Bucket.Entries[I].Key = Key then
      begin
        Result := True;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Count - 1 do
          if Bucket.Entries[I].Value = Value then
          begin
            Result := True;
            Exit;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrIntfHashMap.Equals(const AMap: IJclStrIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FCount <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Count - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if AMap.GetValue(Bucket.Entries[J].Key) <> Bucket.Entries[J].Value then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrIntfHashMap.GetCapacity: Integer;
begin
  Result := GetCapacity;
end;

function TJclStrIntfHashMap.GetValue(const Key: string): IInterface;
var
  I: Integer;
  Index: Integer;
  Bucket: TJclStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Index := FHashFunction(HashString(Key), FCapacity);
    Bucket := FBuckets[Index];
    if Bucket <> nil then
      for I := 0 to Bucket.Count - 1 do
        if Bucket.Entries[I].Key = Key then
        begin
          Result := Bucket.Entries[I].Value;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrIntfHashMap.GrowEntries(Bucket: TJclStrIntfBucket);
var
  BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  if BucketCapacity > 64 then
    BucketCapacity := BucketCapacity + BucketCapacity div 4
  else
    BucketCapacity := BucketCapacity * 4;
  SetLength(Bucket.Entries, BucketCapacity);
end;

function TJclStrIntfHashMap.HashString(const Key: string): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(Key) do
    Result := Result + Cardinal(Ord(Key[I]) * (I - 1) * 256);
end;

function TJclStrIntfHashMap.IntfClone: IInterface;
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclStrIntfBucket;
  NewMap: TJclStrIntfHashMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := TJclStrIntfHashMap.Create(FCapacity);
    NewMap.HashFunction := HashFunction;
    for I := 0 to FCapacity - 1 do
    begin
      SelfBucket := FBuckets[I];
      if SelfBucket <> nil then
      begin
        NewBucket := TJclStrIntfBucket.Create;
        SetLength(NewBucket.Entries, SelfBucket.Count);
        for J := 0 to SelfBucket.Count - 1 do
        begin
          NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
          NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
        end;
        NewBucket.Count := SelfBucket.Count;
        NewMap.FBuckets[I] := NewBucket;
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrIntfHashMap.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclStrIntfHashMap.KeySet: IJclStrSet;
var
  I, J: Integer;
  Bucket: TJclStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclStrArraySet.Create(FCount);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Count - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrIntfHashMap.MoveArray(var List: TJclStrIntfEntryArray;
  FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
    if FromIndex < ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    for I := 0 to ToIndex - FromIndex - 1 do
    begin
      List[FromIndex + I].Key := '';
      List[FromIndex + I].Value := nil;
    end;
  end
  else
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    for I := 0 to FromIndex - ToIndex - 1 do
    begin
      List[FromIndex + Count - 1 + I].Key := '';
      List[FromIndex + Count - 1 + I].Value := nil;
    end;
  end;

{$ELSE}
begin
  Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
  { Keep reference counting working }
  if FromIndex < ToIndex then
    FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
  else
  if FromIndex > ToIndex then
    FillChar(List[FromIndex + Count - 1], (FromIndex - ToIndex) * SizeOf(List[0]), 0);
{$ENDIF CLR}
end;

procedure TJclStrIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Count > 0 then
          SetLength(Bucket.Entries, Bucket.Count)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrIntfHashMap.PutAll(const AMap: IJclStrIntfMap);
var
  It: IJclStrIterator;
  Key: string;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrIntfHashMap.PutValue(const Key: string; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclStrIntfBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Index := FHashFunction(HashString(Key), FCapacity);
    Bucket := FBuckets[Index];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Count - 1 do
        if Bucket.Entries[I].Key = Key then
        begin
          Bucket.Entries[I].Value := Value;
          Exit;
        end;
    end
    else
    begin
      Bucket := TJclStrIntfBucket.Create;
      SetLength(Bucket.Entries, 1);
      FBuckets[Index] := Bucket;
    end;
    if Bucket.Count = Length(Bucket.Entries) then
      GrowEntries(Bucket);
    Bucket.Entries[Bucket.Count].Key := Key;
    Bucket.Entries[Bucket.Count].Value := Value;
    Inc(Bucket.Count);
    Inc(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrIntfHashMap.Remove(const Key: string): IInterface;
var
  Bucket: TJclStrIntfBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(HashString(Key), FCapacity)];
    if Bucket <> nil then
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
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrIntfHashMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value >= FCount then
    begin
      SetLength(FBuckets, Value);
      FCapacity := Value;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrIntfHashMap.Size: Integer;
begin
  Result := FCount;
end;

function TJclStrIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FCount);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Count - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclStrStrHashMap } ==================================================

constructor TJclStrStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
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
  Bucket: TJclStrStrBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Count - 1 do
        begin
          Bucket.Entries[J].Key := '';
          Bucket.Entries[J].Value := '';
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FCount := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStrHashMap.Clone: TObject;
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclStrStrBucket;
  NewMap: TJclStrStrHashMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := TJclStrStrHashMap.Create(FCapacity);
    NewMap.HashFunction := HashFunction;
    for I := 0 to FCapacity - 1 do
    begin
      SelfBucket := FBuckets[I];
      if SelfBucket <> nil then
      begin
        NewBucket := TJclStrStrBucket.Create;
        SetLength(NewBucket.Entries, SelfBucket.Count);
        for J := 0 to SelfBucket.Count - 1 do
        begin
          NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
          NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
        end;
        NewBucket.Count := SelfBucket.Count;
        NewMap.FBuckets[I] := NewBucket;
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStrHashMap.ContainsKey(const Key: string): Boolean;
var
  I: Integer;
  Bucket: TJclStrStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(HashString(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Count - 1 do
        if Bucket.Entries[I].Key = Key then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStrHashMap.ContainsValue(const Value: string): Boolean;
var
  I, J: Integer;
  Bucket: TJclStrStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Count - 1 do
          if Bucket.Entries[I].Value = Value then
          begin
            Result := True;
            Exit;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStrHashMap.Equals(const AMap: IJclStrStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclStrStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FCount <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Count - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if AMap.GetValue(Bucket.Entries[J].Key) <> Bucket.Entries[J].Value then
              Exit;
        end
        else
          Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStrHashMap.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TJclStrStrHashMap.GetValue(const Key: string): string;
var
  I: Integer;
  Bucket: TJclStrStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Bucket := FBuckets[FHashFunction(HashString(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Count - 1 do
        if Bucket.Entries[I].Key = Key then
        begin
          Result := Bucket.Entries[I].Value;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrStrHashMap.GrowEntries(Bucket: TJclStrStrBucket);
var
  BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  if BucketCapacity > 64 then
    BucketCapacity := BucketCapacity + BucketCapacity div 4
  else
    BucketCapacity := BucketCapacity * 4;
  SetLength(Bucket.Entries, BucketCapacity);
end;

function TJclStrStrHashMap.HashString(const Key: string): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(Key) do
    Result := Result + Cardinal(Ord(Key[I]) * (I - 1) * 256);
end;

function TJclStrStrHashMap.IntfClone: IInterface;
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclStrStrBucket;
  NewMap: TJclStrStrHashMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := TJclStrStrHashMap.Create(FCapacity);
    NewMap.HashFunction := HashFunction;
    for I := 0 to FCapacity - 1 do
    begin
      SelfBucket := FBuckets[I];
      if SelfBucket <> nil then
      begin
        NewBucket := TJclStrStrBucket.Create;
        SetLength(NewBucket.Entries, SelfBucket.Count);
        for J := 0 to SelfBucket.Count - 1 do
        begin
          NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
          NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
        end;
        NewBucket.Count := SelfBucket.Count;
        NewMap.FBuckets[I] := NewBucket;
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStrHashMap.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclStrStrHashMap.KeyOfValue(const Value: string): string;
var
  I, J: Integer;
  Bucket: TJclStrStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Count - 1 do
          if Bucket.Entries[I].Value = Value then
          begin
            Result := Bucket.Entries[I].Key;
            Exit;
          end;
    end;
    raise EJclNoSuchElementError.Create(Value);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStrHashMap.KeySet: IJclStrSet;
var
  I, J: Integer;
  Bucket: TJclStrStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclStrArraySet.Create(FCount);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Count - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrStrHashMap.MoveArray(var List: TJclStrStrEntryArray;
  FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    for I := 0 to ToIndex - FromIndex - 1 do
    begin
      List[FromIndex + I].Key := '';
      List[FromIndex + I].Value := '';
    end;
  end
  else
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    for I := 0 to FromIndex - ToIndex - 1 do
    begin
      List[FromIndex + Count - 1 + I].Key := '';
      List[FromIndex + Count - 1 + I].Value := '';
    end;
  end;
{$ELSE}
begin
  Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
  { Keep reference counting working }
  if FromIndex < ToIndex then
    FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
  else
  if FromIndex > ToIndex then
    FillChar(List[FromIndex + Count - 1], (FromIndex - ToIndex) * SizeOf(List[0]), 0);
{$ENDIF CLR}
end;

procedure TJclStrStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclStrStrBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Count > 0 then
          SetLength(Bucket.Entries, Bucket.Count)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrStrHashMap.PutAll(const AMap: IJclStrStrMap);
var
  It: IJclStrIterator;
  Key: string;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrStrHashMap.PutValue(const Key, Value: string);
var
  Index: Integer;
  Bucket: TJclStrStrBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Index := FHashFunction(HashString(Key), FCapacity);
    Bucket := FBuckets[Index];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Count - 1 do
        if Bucket.Entries[I].Key = Key then
        begin
          Bucket.Entries[I].Value := Value;
          Exit;
        end;
    end
    else
    begin
      Bucket := TJclStrStrBucket.Create;
      SetLength(Bucket.Entries, 1);
      FBuckets[Index] := Bucket;
    end;
    if Bucket.Count = Length(Bucket.Entries) then
      GrowEntries(Bucket);
    Bucket.Entries[Bucket.Count].Key := Key;
    Bucket.Entries[Bucket.Count].Value := Value;
    Inc(Bucket.Count);
    Inc(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStrHashMap.Remove(const Key: string): string;
var
  Bucket: TJclStrStrBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Bucket := FBuckets[FHashFunction(HashString(Key), FCapacity)];
    if Bucket <> nil then
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
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrStrHashMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value >= FCount then
    begin
      SetLength(FBuckets, Value);
      FCapacity := Value;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStrHashMap.Size: Integer;
begin
  Result := FCount;
end;

function TJclStrStrHashMap.Values: IJclStrCollection;
var
  I, J: Integer;
  Bucket: TJclStrStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclStrArrayList.Create(FCount);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Count - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclStrHashMap } =====================================================

constructor TJclStrHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create(nil);
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
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
  Bucket: TJclStrBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Count - 1 do
        begin
          Bucket.Entries[J].Key := '';
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FCount := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrHashMap.Clone: TObject;
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclStrBucket;
  NewMap: TJclStrHashMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := TJclStrHashMap.Create(FCapacity, False);
    NewMap.HashFunction := HashFunction;
    for I := 0 to FCapacity - 1 do
    begin
      SelfBucket := FBuckets[I];
      if SelfBucket <> nil then
      begin
        NewBucket := TJclStrBucket.Create;
        SetLength(NewBucket.Entries, SelfBucket.Count);
        for J := 0 to SelfBucket.Count - 1 do
        begin
          NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
          NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
        end;
        NewBucket.Count := SelfBucket.Count;
        NewMap.FBuckets[I] := NewBucket;
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrHashMap.ContainsKey(const Key: string): Boolean;
var
  I: Integer;
  Bucket: TJclStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(HashString(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Count - 1 do
        if Bucket.Entries[I].Key = Key then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Count - 1 do
          if Bucket.Entries[I].Value = Value then
          begin
            Result := True;
            Exit;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrHashMap.Equals(const AMap: IJclStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FCount <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Count - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if AMap.GetValue(Bucket.Entries[J].Key) <> Bucket.Entries[J].Value then
              Exit;
          end
          else
            Exit;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrHashMap.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TJclStrHashMap.GetValue(const Key: string): TObject;
var
  I: Integer;
  Bucket: TJclStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    I := FHashFunction(HashString(Key), FCapacity);
    Bucket := FBuckets[I];
    if Bucket <> nil then
      for I := 0 to Bucket.Count - 1 do
        if Bucket.Entries[I].Key = Key then
        begin
          Result := Bucket.Entries[I].Value;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrHashMap.FreeValue(var AValue: TObject);
begin
  // TODO: trap destructor exceptions
  if FOwnsValues then
    FreeAndNil(AValue)
  else
    AValue := nil;
end;

procedure TJclStrHashMap.GrowEntries(Bucket: TJclStrBucket);
var
  BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  if BucketCapacity > 64 then
    BucketCapacity := BucketCapacity + BucketCapacity div 4
  else
    BucketCapacity := BucketCapacity * 4;
  SetLength(Bucket.Entries, BucketCapacity);
end;

function TJclStrHashMap.HashString(const Key: string): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(Key) do
    Result := Result + Cardinal(Ord(Key[I]) * (I - 1) * 256);
end;

function TJclStrHashMap.IntfClone: IInterface;
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclStrBucket;
  NewMap: TJclStrHashMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := TJclStrHashMap.Create(FCapacity, False);
    NewMap.HashFunction := HashFunction;
    for I := 0 to FCapacity - 1 do
    begin
      SelfBucket := FBuckets[I];
      if SelfBucket <> nil then
      begin
        NewBucket := TJclStrBucket.Create;
        SetLength(NewBucket.Entries, SelfBucket.Count);
        for J := 0 to SelfBucket.Count - 1 do
        begin
          NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
          NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
        end;
        NewBucket.Count := SelfBucket.Count;
        NewMap.FBuckets[I] := NewBucket;
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrHashMap.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclStrHashMap.KeySet: IJclStrSet;
var
  I, J: Integer;
  Bucket: TJclStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclStrArraySet.Create(FCount);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Count - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrHashMap.MoveArray(var List: TJclStrEntryArray;
  FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    for I := 0 to ToIndex - FromIndex - 1 do
    begin
      List[FromIndex + I].Key := '';
      List[FromIndex + I].Value := nil;
    end;
  end
  else
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    for I := 0 to FromIndex - ToIndex - 1 do
    begin
      List[FromIndex + Count - 1 + I].Key := '';
      List[FromIndex + Count - 1 + I].Value := nil;
    end;
  end;
{$ELSE}
begin
  Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
  { Keep reference counting working }
  if FromIndex < ToIndex then
    FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
  else
  if FromIndex > ToIndex then
    FillChar(List[FromIndex + Count - 1], (FromIndex - ToIndex) * SizeOf(List[0]), 0);
{$ENDIF CLR}
end;

procedure TJclStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclStrBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Count > 0 then
          SetLength(Bucket.Entries, Bucket.Count)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrHashMap.PutAll(const AMap: IJclStrMap);
var
  It: IJclStrIterator;
  Key: string;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrHashMap.PutValue(const Key: string; Value: TObject);
var
  Index: Integer;
  Bucket: TJclStrBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Index := FHashFunction(HashString(Key), FCapacity);
    Bucket := FBuckets[Index];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Count - 1 do
        if Bucket.Entries[I].Key = Key then
        begin
          FreeValue(Bucket.Entries[I].Value);
          Bucket.Entries[I].Value := Value;
          Exit;
        end;
    end
    else
    begin
      Bucket := TJclStrBucket.Create;
      SetLength(Bucket.Entries, 1);
      FBuckets[Index] := Bucket;
    end;

    if Bucket.Count = Length(Bucket.Entries) then
      GrowEntries(Bucket);
    Bucket.Entries[Bucket.Count].Key := Key;
    Bucket.Entries[Bucket.Count].Value := Value;
    Inc(Bucket.Count);
    Inc(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrHashMap.Remove(const Key: string): TObject;
var
  Bucket: TJclStrBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(HashString(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Count - 1 do
        if Bucket.Entries[I].Key = Key then
        begin
          if OwnsValues then
            Result := nil
          else
            Result := Bucket.Entries[I].Value;
          FreeValue(Bucket.Entries[I].Value);
          Bucket.Entries[I].Key := '';
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Count - I - 1);
          Dec(Bucket.Count);
          Dec(FCount);
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrHashMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value >= FCount then
    begin
      SetLength(FBuckets, Value);
      FCapacity := Value;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrHashMap.Size: Integer;
begin
  Result := FCount;
end;

function TJclStrHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FCount, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Count - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclHashMap } ========================================================

constructor TJclHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(nil);
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
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
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Count - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FCount := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.Clone: TObject;
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclBucket;
  NewMap: TJclHashMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := TJclHashMap.Create(FCapacity, False, False);
    NewMap.HashFunction := HashFunction;
    for I := 0 to FCapacity - 1 do
    begin
      SelfBucket := FBuckets[I];
      if SelfBucket <> nil then
      begin
        NewBucket := TJclBucket.Create;
        SetLength(NewBucket.Entries, SelfBucket.Count);
        for J := 0 to SelfBucket.Count - 1 do
        begin
          NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
          NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
        end;
        NewBucket.Count := SelfBucket.Count;
        NewMap.FBuckets[I] := NewBucket;
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.ContainsKey(Key: TObject): Boolean;
var
  I: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Integer(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Count - 1 do
      if Bucket.Entries[I].Key = Key then
      begin
        Result := True;
        Exit;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Count - 1 do
          if Bucket.Entries[I].Value = Value then
          begin
            Result := True;
            Exit;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.Equals(const AMap: IJclMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FCount <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Count - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if AMap.GetValue(Bucket.Entries[J].Key) <> Bucket.Entries[J].Value then
              Exit;
          end
          else
            Exit;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.FreeKey(var AKey: TObject);
begin
  if FOwnsKeys then
    FreeAndNil(AKey)
  else
    AKey := nil;
end;

procedure TJclHashMap.FreeValue(var AValue: TObject);
begin
  if FOwnsValues then
    FreeAndNil(AValue)
  else
    AValue := nil;
end;

function TJclHashMap.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TJclHashMap.GetValue(Key: TObject): TObject;
var
  I: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Integer(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Count - 1 do
        if Bucket.Entries[I].Key = Key then
        begin
          Result := Bucket.Entries[I].Value;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.GrowEntries(Bucket: TJclBucket);
var
  BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  if BucketCapacity > 64 then
    BucketCapacity := BucketCapacity + BucketCapacity div 4
  else
    BucketCapacity := BucketCapacity * 4;
  SetLength(Bucket.Entries, BucketCapacity);
end;

function TJclHashMap.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclHashMap.KeySet: IJclSet;
var
  I, J: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArraySet.Create(FCapacity, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Count - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.IntfClone: IInterface;
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclBucket;
  NewMap: TJclHashMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := TJclHashMap.Create(FCapacity, False, False);
    NewMap.HashFunction := HashFunction;
    for I := 0 to FCapacity - 1 do
    begin
      SelfBucket := FBuckets[I];
      if SelfBucket <> nil then
      begin
        NewBucket := TJclBucket.Create;
        SetLength(NewBucket.Entries, SelfBucket.Count);
        for J := 0 to SelfBucket.Count - 1 do
        begin
          NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
          NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
        end;
        NewBucket.Count := SelfBucket.Count;
        NewMap.FBuckets[I] := NewBucket;
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.MoveArray(var List: TJclEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    for I := 0 to ToIndex - FromIndex - 1 do
    begin
      List[FromIndex + I].Key := nil;
      List[FromIndex + I].Value := nil;
    end;
  end
  else
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    for I := 0 to FromIndex - ToIndex - 1 do
    begin
      List[FromIndex + Count - 1 + I].Key := nil;
      List[FromIndex + Count - 1 + I].Value := nil;
    end;
  end;
{$ELSE}
begin
  Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
{$ENDIF CLR}
end;

procedure TJclHashMap.Pack;
var
  I: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Count > 0 then
          SetLength(Bucket.Entries, Bucket.Count)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.PutAll(const AMap: IJclMap);
var
  It: IJclIterator;
  Key: TObject;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.PutValue(Key, Value: TObject);
var
  Index: Integer;
  Bucket: TJclBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Index := FHashFunction(Integer(Key), FCapacity);
    Bucket := FBuckets[Index];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Count - 1 do
        if Bucket.Entries[I].Key = Key then
        begin
          FreeValue(Bucket.Entries[I].Value);
          Bucket.Entries[I].Value := Value;
          Exit;
        end;
    end
    else
    begin
      Bucket := TJclBucket.Create;
      SetLength(Bucket.Entries, 1);
      FBuckets[Index] := Bucket;
    end;
    if Bucket.Count = Length(Bucket.Entries) then
      GrowEntries(Bucket);
    Bucket.Entries[Bucket.Count].Key := Key;
    Bucket.Entries[Bucket.Count].Value := Value;
    Inc(Bucket.Count);
    Inc(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.Remove(Key: TObject): TObject;
var
  Bucket: TJclBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Integer(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Count - 1 do
        if Bucket.Entries[I].Key = Key then
        begin
          if OwnsValues then
            Result := nil
          else
            Result := Bucket.Entries[I].Value;
          FreeKey(Bucket.Entries[I].Key);
          FreeValue(Bucket.Entries[I].Value);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Count - I - 1);
          Dec(Bucket.Count);
          Dec(FCount);
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value >= FCount then
    begin
      SetLength(FBuckets, Value);
      FCapacity := Value;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.Size: Integer;
begin
  Result := FCount;
end;

function TJclHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FCapacity, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Count - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_GENERICS}

//=== { TJclHashMap<TKey, TValue> } ==========================================

constructor TJclHashMap<TKey, TValue>.Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(nil);
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclHashMap<TKey, TValue>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclHashMap<TKey, TValue>.Clear;
var
  I, J: Integer;
  Bucket: TJclBucket<TKey,TValue>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Count - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FCount := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.Clone: TObject;
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclBucket<TKey, TValue>;
  NewMap: TJclHashMap<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyHashMap(FCapacity);
    for I := 0 to FCapacity - 1 do
    begin
      SelfBucket := FBuckets[I];
      if SelfBucket <> nil then
      begin
        NewBucket := TJclBucket<TKey, TValue>.Create;
        SetLength(NewBucket.Entries, SelfBucket.Count);
        for J := 0 to SelfBucket.Count - 1 do
        begin
          NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
          NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
        end;
        NewBucket.Count := SelfBucket.Count;
        NewMap.FBuckets[I] := NewBucket;
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.ContainsKey(const Key: TKey): Boolean;
var
  I: Integer;
  Bucket: TJclBucket<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(KeyToOrd(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Count - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := True;
          Exit;
        end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.ContainsValue(const Value: TValue): Boolean;
var
  I, J: Integer;
  Bucket: TJclBucket<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Count - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Exit;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.Equals(const AMap: IJclMap<TKey, TValue>): Boolean;
var
  I, J: Integer;
  Bucket: TJclBucket<TKey,TValue>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FCount <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Count - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.FreeKey(var AKey: TKey);
begin
  if FOwnsKeys then
    FreeAndNil(AKey)
  else
    AKey := Default(TKey);
end;

procedure TJclHashMap<TKey, TValue>.FreeValue(var AValue: TValue);
begin
  // TODO: trap destructor exceptions
  if FOwnsValues then
    FreeAndNil(AValue)
  else
    AValue := Default(TValue);
end;

function TJclHashMap<TKey, TValue>.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TJclHashMap<TKey, TValue>.GetValue(Key: TKey): TValue;
var
  I: Integer;
  Bucket: TJclBucket<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(TValue);
    Bucket := FBuckets[FHashFunction(KeyToOrd(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Count - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.GrowEntries(Bucket: TJclBucket<TKey,TValue>);
var
  BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  if BucketCapacity > 64 then
    BucketCapacity := BucketCapacity + BucketCapacity div 4
  else
    BucketCapacity := BucketCapacity * 4;
  SetLength(Bucket.Entries, BucketCapacity);
end;

function TJclHashMap<TKey, TValue>.IntfClone: IInterface;
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclBucket<TKey, TValue>;
  NewMap: TJclHashMap<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyHashMap(FCapacity);
    for I := 0 to FCapacity - 1 do
    begin
      SelfBucket := FBuckets[I];
      if SelfBucket <> nil then
      begin
        NewBucket := TJclBucket<TKey, TValue>.Create;
        SetLength(NewBucket.Entries, SelfBucket.Count);
        for J := 0 to SelfBucket.Count - 1 do
        begin
          NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
          NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
        end;
        NewBucket.Count := SelfBucket.Count;
        NewMap.FBuckets[I] := NewBucket;
      end;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclHashMap<TKey, TValue>.KeySet: IJclSet<TKey>;
var
  I, J: Integer;
  Bucket: TJclBucket<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyArraySet(FCount);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Count - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.MoveArray(var List: TJclEntryArray<TKey, TValue>;
  FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    for I := 0 to ToIndex - FromIndex - 1 do
    begin
      List[FromIndex + I].Key := Default(TKey);
      List[FromIndex + I].Value := Default(TValue);
    end;
  end
  else
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];
    for I := 0 to FromIndex - ToIndex - 1 do
    begin
      List[FromIndex + Count - 1 + I].Key := Default(TKey);
      List[FromIndex + Count - 1 + I].Value := Default(TValue);
    end;
  end;
{$ELSE}
begin
  Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
{$ENDIF CLR}
end;

procedure TJclHashMap<TKey, TValue>.Pack;
var
  I: Integer;
  Bucket: TJclBucket<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Count > 0 then
          SetLength(Bucket.Entries, Bucket.Count)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.PutAll(const AMap: IJclMap<TKey, TValue>);
var
  It: IJclIterator<TKey>;
  Key: TKey;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.PutValue(Key: TKey; const Value: TValue);
var
  Index: Integer;
  Bucket: TJclBucket<TKey, TValue>;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Index := FHashFunction(KeyToOrd(Key), FCapacity);
    Bucket := FBuckets[Index];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Count - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          FreeValue(Bucket.Entries[I].Value);
          Bucket.Entries[I].Value := Value;
          Exit;
        end;
    end
    else
    begin
      Bucket := TJclBucket<TKey, TValue>.Create;
      SetLength(Bucket.Entries, 1);
      FBuckets[Index] := Bucket;
    end;
    if Bucket.Count = Length(Bucket.Entries) then
      GrowEntries(Bucket);
    Bucket.Entries[Bucket.Count].Key := Key;
    Bucket.Entries[Bucket.Count].Value := Value;
    Inc(Bucket.Count);
    Inc(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.Remove(const Key: TKey): TValue;
var
  Bucket: TJclBucket<TKey, TValue>;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(TValue);
    Bucket := FBuckets[FHashFunction(KeyToOrd(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Count - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          if OwnsValues then
            Result := Default(TValue)
          else
            Result := Bucket.Entries[I].Value;
          FreeKey(Bucket.Entries[I].Key);
          FreeValue(Bucket.Entries[I].Value);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Count - I - 1);
          Dec(Bucket.Count);
          Dec(FCount);
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value >= FCount then
    begin
      SetLength(FBuckets, Value);
      FCapacity := Value;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.Size: Integer;
begin
  Result := FCount;
end;

function TJclHashMap<TKey, TValue>.Values: IJclCollection<TValue>;
var
  I, J: Integer;
  Bucket: TJclBucket<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyArrayList(FCount);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Count - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclHashMapE<TKey, TValue> } =========================================

constructor TJclHashMapE<TKey, TValue>.Create(const AKeyEqualityComparer: IEqualityComparer<TKey>;
  const AValueEqualityComparer: IEqualityComparer<TValue>; const AKeyComparer: IComparer<TKey>; ACapacity: Integer;
  AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(ACapacity, AOwnsKeys, AOwnsValues);
  FKeyEqualityComparer := AKeyEqualityComparer;
  FValueEqualityComparer := AValueEqualityComparer;
  FKeyComparer := AKeyComparer;
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer): IJclCollection<TValue>;
begin
  Result := TJclArrayListE<TValue>.Create(ValueEqualityComparer, ACapacity, False);
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer): IJclSet<TKey>;
begin
  Result := TJclArraySetE<TKey>.Create(KeyComparer, ACapacity, False);
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyHashMap(ACapacity: Integer): TJclHashMap<TKey, TValue>;
begin
  Result := TJclHashMapE<TKey, TValue>.Create(KeyEqualityComparer, ValueEqualityComparer,
    KeyComparer, ACapacity, False, False);
end;

function TJclHashMapE<TKey, TValue>.KeysEqual(const A, B: TKey): Boolean;
begin
  if KeyEqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := KeyEqualityComparer.Equals(A, B);
end;

function TJclHashMapE<TKey, TValue>.ValuesEqual(const A, B: TValue): Boolean;
begin
  if ValueEqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := ValueEqualityComparer.Equals(A, B);
end;

function TJclHashMapE<TKey, TValue>.KeyToOrd(const AKey: TKey): Cardinal;
begin
  if KeyEqualityComparer = nil then
    raise EJclNoHashConverterError.Create;
  Result := KeyEqualityComparer.GetHashCode(AKey);
end;

//=== { TJclHashMapF<TKey, TValue> } =========================================

constructor TJclHashMapF<TKey, TValue>.Create(AKeyEqualityCompare: TEqualityCompare<TKey>;
  AKeyHash: THash<TKey>; AValueEqualityCompare: TEqualityCompare<TValue>; AKeyCompare: TCompare<TKey>;
  ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(ACapacity, AOwnsKeys, AOwnsValues);
  FKeyEqualityCompare := AKeyEqualityCompare;
  FKeyHash := AKeyHash;
  FValueEqualityCompare := AValueEqualityCompare;
  FKeyCompare := AKeyCompare;
end;

function TJclHashMapF<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer): IJclCollection<TValue>;
begin
  Result := TJclArrayListF<TValue>.Create(ValueEqualityCompare, ACapacity, False);
end;

function TJclHashMapF<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer): IJclSet<TKey>;
begin
  Result := TJclArraySetF<TKey>.Create(KeyCompare, ACapacity, False);
end;

function TJclHashMapF<TKey, TValue>.CreateEmptyHashMap(ACapacity: Integer): TJclHashMap<TKey, TValue>;
begin
  Result := TJclHashMapF<TKey, TValue>.Create(KeyEqualityCompare, KeyHash, ValueEqualityCompare, KeyCompare, ACapacity,
    False, False);
end;

function TJclHashMapF<TKey, TValue>.KeysEqual(const A, B: TKey): Boolean;
begin
  if not Assigned(KeyEqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := KeyEqualityCompare(A, B);
end;

function TJclHashMapF<TKey, TValue>.ValuesEqual(const A, B: TValue): Boolean;
begin
  if not Assigned(ValueEqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := ValueEqualityCompare(A, B);
end;

function TJclHashMapF<TKey, TValue>.KeyToOrd(const AKey: TKey): Cardinal;
begin
  if not Assigned(KeyHash) then
    raise EJclNoHashConverterError.Create;
  Result := KeyHash(AKey);
end;

//=== { TJclHashMapI<TKey, TValue> } =========================================

function TJclHashMapI<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer): IJclCollection<TValue>;
begin
  Result := TJclArrayListI<TValue>.Create(ACapacity, False);
end;

function TJclHashMapI<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer): IJclSet<TKey>;
begin
  Result := TJclArraySetI<TKey>.Create(ACapacity, False);
end;

function TJclHashMapI<TKey, TValue>.CreateEmptyHashMap(ACapacity: Integer): TJclHashMap<TKey, TValue>;
begin
  Result := TJclHashMapI<TKey, TValue>.Create(ACapacity, False, False);
end;

function TJclHashMapI<TKey, TValue>.KeysEqual(const A, B: TKey): Boolean;
begin
  Result := A.Equals(B);
end;

function TJclHashMapI<TKey, TValue>.ValuesEqual(const A, B: TValue): Boolean;
begin
  Result := A.Equals(B);
end;

function TJclHashMapI<TKey, TValue>.KeyToOrd(const AKey: TKey): Cardinal;
begin
  Result := AKey.GetHashCode;
end;
{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

