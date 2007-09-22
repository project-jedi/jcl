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
{ The Original Code is HashSet.pas.                                                                }
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

unit JclHashSets;

{$I jcl.inc}

interface
// generics
uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes,
  {$IFDEF SUPPORTS_GENERICS}
  {$IFDEF CLR}
  System.Collections.Generic,
  {$ENDIF CLR}
  JclAlgorithms,
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclContainerIntf, JclHashMaps;

type
  TJclIntfHashSet = class(TJclAbstractContainer, IJclIntfCollection, IJclIntfSet,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable)
  private
    FMap: IJclIntfIntfMap;
  protected
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclIntfCollection }
    function Add(const AInterface: IInterface): Boolean;
    function AddAll(const ACollection: IJclIntfCollection): Boolean;
    procedure Clear;
    function Contains(const AInterface: IInterface): Boolean;
    function ContainsAll(const ACollection: IJclIntfCollection): Boolean;
    function Equals(const ACollection: IJclIntfCollection): Boolean;
    function First: IJclIntfIterator;
    function IsEmpty: Boolean;
    function Last: IJclIntfIterator;
    function Remove(const AInterface: IInterface): Boolean;
    function RemoveAll(const ACollection: IJclIntfCollection): Boolean;
    function RetainAll(const ACollection: IJclIntfCollection): Boolean;
    function Size: Integer;
    { IJclIntfSet }
    procedure Intersect(const ACollection: IJclIntfCollection);
    procedure Subtract(const ACollection: IJclIntfCollection);
    procedure Union(const ACollection: IJclIntfCollection);
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AMap: IJclIntfIntfMap); overload;
    constructor Create(ACapacity: Integer = DefaultContainerCapacity); overload;
    destructor Destroy; override;
  end;

  //Daniele Teti 02/03/2005
  TJclStrHashSet = class(TJclStrCollection, IJclStrSet,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable)
  private
    FMap: IJclStrMap;
  protected
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclStrCollection }
    function Add(const AString: string): Boolean; override;
    function AddAll(const ACollection: IJclStrCollection): Boolean; override;
    procedure Clear; override;
    function Contains(const AString: string): Boolean; override;
    function ContainsAll(const ACollection: IJclStrCollection): Boolean; override;
    function Equals(const ACollection: IJclStrCollection): Boolean; override;
    function First: IJclStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclStrIterator; override;
    function Remove(const AString: string): Boolean; override;
    function RemoveAll(const ACollection: IJclStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclStrCollection): Boolean; override;
    function Size: Integer; override;
    { IJclIntfSet }
    procedure Intersect(const ACollection: IJclStrCollection);
    procedure Subtract(const ACollection: IJclStrCollection);
    procedure Union(const ACollection: IJclStrCollection);
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AMap: IJclStrMap); overload;
    constructor Create(ACapacity: Integer = DefaultContainerCapacity); overload;
    destructor Destroy; override;
  end;

  TJclHashSet = class(TJclAbstractContainer, IJclCollection, IJclSet,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable)
  private
    FMap: IJclMap;
  protected
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclCollection }
    function Add(AObject: TObject): Boolean;
    function AddAll(const ACollection: IJclCollection): Boolean;
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function ContainsAll(const ACollection: IJclCollection): Boolean;
    function Equals(const ACollection: IJclCollection): Boolean;
    function First: IJclIterator;
    function IsEmpty: Boolean;
    function Last: IJclIterator;
    function Remove(AObject: TObject): Boolean;
    function RemoveAll(const ACollection: IJclCollection): Boolean;
    function RetainAll(const ACollection: IJclCollection): Boolean;
    function Size: Integer;
    { IJclSet }
    procedure Intersect(const ACollection: IJclCollection);
    procedure Subtract(const ACollection: IJclCollection);
    procedure Union(const ACollection: IJclCollection);
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AMap: IJclMap); overload;
    constructor Create(ACapacity: Integer = DefaultContainerCapacity; AOwnsObjects: Boolean = True); overload;
    destructor Destroy; override;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  TRefUnique = class;
  TRefUnique = class(TEquatable<TRefUnique>)
  end;

  TJclHashSet<T> = class(TJclAbstractContainer, IJclCollection<T>, IJclSet<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable)
  private
    FMap: IJclMap<T, TRefUnique>;
  protected
    function CreateHashSet(const AMap: IJclMap<T, TRefUnique>): TJclHashSet<T>; virtual; abstract;
    function ItemsEqual(const A, B: T): Boolean; virtual; abstract;
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclCollection<T> }
    function Add(const AItem: T): Boolean;
    function AddAll(const ACollection: IJclCollection<T>): Boolean;
    procedure Clear;
    function Contains(const AItem: T): Boolean;
    function ContainsAll(const ACollection: IJclCollection<T>): Boolean;
    function Equals(const ACollection: IJclCollection<T>): Boolean;
    function First: IJclIterator<T>;
    function IsEmpty: Boolean;
    function Last: IJclIterator<T>;
    function Remove(const AItem: T): Boolean;
    function RemoveAll(const ACollection: IJclCollection<T>): Boolean;
    function RetainAll(const ACollection: IJclCollection<T>): Boolean;
    function Size: Integer;
    { IJclSet<T> }
    procedure Intersect(const ACollection: IJclCollection<T>);
    procedure Subtract(const ACollection: IJclCollection<T>);
    procedure Union(const ACollection: IJclCollection<T>);
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;

    constructor Create(const AMap: IJclMap<T, TRefUnique>);
  public
    destructor Destroy; override;
  end;

  // E = External helper to compare items for equality
  TJclHashSetE<T> = class(TJclHashSet<T>, IJclCollection<T>, IJclSet<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    function CreateHashSet(const AMap: IJclMap<T, TRefUnique>): TJclHashSet<T>; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; const AMap: IJclMap<T, TRefUnique>); overload;
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; const AComparer: IComparer<T>;
      ACapacity: Integer = DefaultContainerCapacity; AOwnsItems: Boolean = True); overload;

    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclHashSetF<T> = class(TJclHashSet<T>, IJclCollection<T>, IJclSet<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable)
  private
    FEqualityCompare: TEqualityCompare<T>;
  protected
    function CreateHashSet(const AMap: IJclMap<T, TRefUnique>): TJclHashSet<T>; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const AMap: IJclMap<T, TRefUnique>); overload;
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const AHash: THash<T>; const ACompare: TCompare<T>;
      ACapacity: Integer = DefaultContainerCapacity; AOwnsItems: Boolean = True); overload;

    property EqualityCompare: TEqualityCompare<T> read FEqualityCompare write FEqualityCompare;
  end;

  // I = Items can compare themselves to an other
  TJclHashSetI<T: IEquatable<T>, IComparable<T>, IHashable> = class(TJclHashSet<T>, IJclCollection<T>, IJclSet<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable)
  protected
    function CreateHashSet(const AMap: IJclMap<T, TRefUnique>): TJclHashSet<T>; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AMap: IJclMap<T, TRefUnique>); overload;
    constructor Create(ACapacity: Integer = DefaultContainerCapacity; AOwnsItems: Boolean = True); overload;
  end;
  {$ELSE ~SUPPORTS_GENERICS}
  TRefUnique = TObject;
  {$ENDIF ~SUPPORTS_GENERICS}

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

var
  GlobalRefUnique: TRefUnique = nil;

function RefUnique: TRefUnique;
begin
  // We keep the reference till program end. A unique memory address is not
  // possible under a garbage collector.
  if GlobalRefUnique = nil then
    GlobalRefUnique := TRefUnique.Create;
  Result := GlobalRefUnique;
end;

var
  GlobalIRefUnique: IInterface = nil;

function IRefUnique: IInterface;
begin
  if GlobalIRefUnique = nil then
    GlobalIRefUnique := TInterfacedObject.Create;
  Result := GlobalIRefUnique;
end;

//=== { TJclIntfHashSet } ====================================================

constructor TJclIntfHashSet.Create(const AMap: IJclIntfIntfMap);
begin
  inherited Create(AMap);
  FMap := AMap;
end;

constructor TJclIntfHashSet.Create(ACapacity: Integer);
begin
  Create(TJclIntfIntfHashMap.Create(ACapacity));
end;

destructor TJclIntfHashSet.Destroy;
begin
  Clear;
  FMap := nil;
  inherited Destroy;
end;

function TJclIntfHashSet.Add(const AInterface: IInterface): Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := not FMap.ContainsKey(AInterface);
    if Result then
      FMap.PutValue(AInterface, IRefUnique);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSet.AddAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := ACollection <> nil;
    if Result then
    begin
      It := ACollection.First;
      while It.HasNext do
        Result := Add(It.Next) or Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclIntfHashSet.Clone: TObject;
begin
  Result := TJclIntfHashSet.Create(IJclIntfCloneable(FMap).Clone as IJclIntfIntfMap);
end;

function TJclIntfHashSet.Contains(const AInterface: IInterface): Boolean;
begin
  Result := FMap.ContainsKey(AInterface);
end;

function TJclIntfHashSet.ContainsAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := FMap.ContainsKey(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSet.Equals(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  ItMap: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FMap.Size <> ACollection.Size then
      Exit;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if ItMap.Next <> It.Next then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSet.First: IJclIntfIterator;
begin
  Result := FMap.KeySet.First;
end;

function TJclIntfHashSet.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

procedure TJclIntfHashSet.Intersect(const ACollection: IJclIntfCollection);
begin
  RetainAll(ACollection);
end;

function TJclIntfHashSet.IntfClone: IInterface;
begin
  Result := TJclIntfHashSet.Create(IJclIntfCloneable(FMap).Clone as IJclIntfIntfMap);
end;

function TJclIntfHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclIntfHashSet.Last: IJclIntfIterator;
begin
  Result := FMap.KeySet.Last;
end;

procedure TJclIntfHashSet.Pack;
begin
  (FMap as IJclPackable).Pack;
end;

function TJclIntfHashSet.Remove(const AInterface: IInterface): Boolean;
begin
  Result := FMap.Remove(AInterface) = IRefUnique;
end;

function TJclIntfHashSet.RemoveAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  ARefUnique: IInterface;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    ARefUnique := IRefUnique;
    It := ACollection.First;
    while It.HasNext do
      Result := (FMap.Remove(It.Next) = ARefUnique) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSet.RetainAll(const ACollection: IJclIntfCollection): Boolean;
var
  ItMap: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ACollection.Contains(ItMap.Next) then
        ItMap.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashSet.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

function TJclIntfHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclIntfHashSet.Subtract(const ACollection: IJclIntfCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclIntfHashSet.Union(const ACollection: IJclIntfCollection);
begin
  AddAll(ACollection);
end;

//=== { TJclStrHashSet } =====================================================

constructor TJclStrHashSet.Create(const AMap: IJclStrMap);
begin
  inherited Create(nil);
  FMap := AMap;
end;

constructor TJclStrHashSet.Create(ACapacity: Integer);
begin
  Create(TJclStrHashMap.Create(ACapacity, False));
end;

destructor TJclStrHashSet.Destroy;
begin
  Clear;
  FMap := nil;
  inherited Destroy;
end;

function TJclStrHashSet.Add(const AString: string): Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := not FMap.ContainsKey(AString);
    if Result then
      FMap.PutValue(AString, RefUnique);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrHashSet.AddAll(const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) or Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclStrHashSet.Clone: TObject;
begin
  Result := TJclStrHashSet.Create(IJclIntfCloneable(FMap).Clone as IJclStrMap);
end;

function TJclStrHashSet.Contains(const AString: string): Boolean;
begin
  Result := FMap.ContainsKey(AString);
end;

function TJclStrHashSet.ContainsAll(const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := FMap.ContainsKey(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrHashSet.Equals(const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
  ItMap: IJclStrIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FMap.Size <> ACollection.Size then
      Exit;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if ItMap.Next <> It.Next then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrHashSet.First: IJclStrIterator;
begin
  Result := FMap.KeySet.First;
end;

function TJclStrHashSet.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

procedure TJclStrHashSet.Intersect(const ACollection: IJclStrCollection);
begin
  RetainAll(ACollection);
end;

function TJclStrHashSet.IntfClone: IInterface;
begin
  Result := TJclStrHashSet.Create(IJclIntfCloneable(FMap).Clone as IJclStrMap);
end;

function TJclStrHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclStrHashSet.Last: IJclStrIterator;
begin
  Result := FMap.KeySet.Last;
end;

procedure TJclStrHashSet.Pack;
begin
  (FMap as IJclPackable).Pack;
end;

function TJclStrHashSet.Remove(const AString: string): Boolean;
begin
  Result := FMap.Remove(AString) = RefUnique;
end;

function TJclStrHashSet.RemoveAll(const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
  ARefUnique: TObject;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    ARefUnique := RefUnique;
    It := ACollection.First;
    while It.HasNext do
      Result := (FMap.Remove(It.Next) = ARefUnique) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrHashSet.RetainAll(const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      if not FMap.ContainsKey(It.Next) then
        FMap.Remove(It.Next);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrHashSet.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

function TJclStrHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclStrHashSet.Subtract(const ACollection: IJclStrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclStrHashSet.Union(const ACollection: IJclStrCollection);
begin
  AddAll(ACollection);
end;

//=== { TJclHashSet } ========================================================

constructor TJclHashSet.Create(const AMap: IJclMap);
begin
  inherited Create(AMap);
  FMap := AMap;
end;

constructor TJclHashSet.Create(ACapacity: Integer; AOwnsObjects: Boolean);
begin
  Create(TJclHashMap.Create(ACapacity, AOwnsObjects));
end;

destructor TJclHashSet.Destroy;
begin
  Clear;
  FMap := nil;
  inherited Destroy;
end;

function TJclHashSet.Add(AObject: TObject): Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := not FMap.ContainsKey(AObject);
    if Result then
      FMap.PutValue(AObject, RefUnique);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet.AddAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) or Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclHashSet.Clone: TObject;
begin
  Result := TJclHashSet.Create(IJclIntfCloneable(FMap).Clone as IJclMap);
end;

function TJclHashSet.Contains(AObject: TObject): Boolean;
begin
  Result := FMap.ContainsKey(AObject);
end;

function TJclHashSet.ContainsAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := FMap.ContainsKey(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet.Equals(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  ItMap: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FMap.Size <> ACollection.Size then
      Exit;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if ItMap.Next <> It.Next then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet.First: IJclIterator;
begin
  Result := FMap.KeySet.First;
end;

function TJclHashSet.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

procedure TJclHashSet.Intersect(const ACollection: IJclCollection);
begin
  RetainAll(ACollection);
end;

function TJclHashSet.IntfClone: IInterface;
begin
  Result := TJclHashSet.Create(IJclIntfCloneable(FMap).Clone as IJclMap);
end;

function TJclHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclHashSet.Last: IJclIterator;
begin
  Result := FMap.KeySet.Last;
end;

procedure TJclHashSet.Pack;
begin
  (FMap as IJclPackable).Pack;
end;

function TJclHashSet.Remove(AObject: TObject): Boolean;
begin
  Result := FMap.Remove(AObject) = RefUnique;
end;

function TJclHashSet.RemoveAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  ARefUnique: TObject;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    ARefUnique := RefUnique;
    It := ACollection.First;
    while It.HasNext do
      Result := (FMap.Remove(It.Next) = ARefUnique) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet.RetainAll(const ACollection: IJclCollection): Boolean;
var
  ItMap: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ACollection.Contains(ItMap.Next) then
        ItMap.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashSet.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

function TJclHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclHashSet.Subtract(const ACollection: IJclCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclHashSet.Union(const ACollection: IJclCollection);
begin
  AddAll(ACollection);
end;

{$IFDEF SUPPORTS_GENERICS}

//=== { TJclHashSet<T> } =====================================================

constructor TJclHashSet<T>.Create(const AMap: IJclMap<T, TRefUnique>);
begin
  inherited Create(AMap);
  FMap := AMap;
end;

destructor TJclHashSet<T>.Destroy;
begin
  Clear;
  FMap := nil;
  inherited Destroy;
end;

function TJclHashSet<T>.Add(const AItem: T): Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := not FMap.ContainsKey(AItem);
    if Result then
      FMap.PutValue(AItem, RefUnique);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet<T>.AddAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) or Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashSet<T>.Clear;
begin
  FMap.Clear;
end;

function TJclHashSet<T>.Clone: TObject;
begin
  Result := CreateHashSet(IJclIntfCloneable(FMap).Clone as IJclMap<T, TRefUnique>);
end;

function TJclHashSet<T>.Contains(const AItem: T): Boolean;
begin
  Result := FMap.ContainsKey(AItem);
end;

function TJclHashSet<T>.ContainsAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := FMap.ContainsKey(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet<T>.Equals(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
  ItMap: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FMap.Size <> ACollection.Size then
      Exit;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ItemsEqual(ItMap.Next, It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet<T>.First: IJclIterator<T>;
begin
  Result := FMap.KeySet.First;
end;

function TJclHashSet<T>.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

procedure TJclHashSet<T>.Intersect(const ACollection: IJclCollection<T>);
begin
  RetainAll(ACollection);
end;

function TJclHashSet<T>.IntfClone: IInterface;
begin
  Result := CreateHashSet(IJclIntfCloneable(FMap).Clone as IJclMap<T, TRefUnique>);
end;

function TJclHashSet<T>.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclHashSet<T>.Last: IJclIterator<T>;
begin
  Result := FMap.KeySet.Last;
end;

procedure TJclHashSet<T>.Pack;
begin
  (FMap as IJclPackable).Pack;
end;

function TJclHashSet<T>.Remove(const AItem: T): Boolean;
begin
  Result := FMap.Remove(AItem) = RefUnique;
end;

function TJclHashSet<T>.RemoveAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
  ARefUnique: TObject;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    ARefUnique := RefUnique;
    It := ACollection.First;
    while It.HasNext do
      Result := (FMap.Remove(It.Next) = ARefUnique) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet<T>.RetainAll(const ACollection: IJclCollection<T>): Boolean;
var
  ItMap: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ACollection.Contains(ItMap.Next) then
        ItMap.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashSet<T>.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

function TJclHashSet<T>.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclHashSet<T>.Subtract(const ACollection: IJclCollection<T>);
begin
  RemoveAll(ACollection);
end;

procedure TJclHashSet<T>.Union(const ACollection: IJclCollection<T>);
begin
  AddAll(ACollection);
end;

//=== { TJclHashSetE<T> } ====================================================

constructor TJclHashSetE<T>.Create(const AEqualityComparer: IEqualityComparer<T>; const AMap: IJclMap<T, TRefUnique>);
begin
  inherited Create(AMap);
  FEqualityComparer := AEqualityComparer;
end;

constructor TJclHashSetE<T>.Create(const AEqualityComparer: IEqualityComparer<T>; const AComparer: IComparer<T>;
  ACapacity: Integer; AOwnsItems: Boolean);
begin
  Create(AEqualityComparer, TJclHashMapE<T, TRefUnique>.Create(AEqualityComparer, RefUnique, AComparer, ACapacity, AOwnsItems, False));
end;

function TJclHashSetE<T>.CreateHashSet(const AMap: IJclMap<T, TRefUnique>): TJclHashSet<T>;
begin
  Result := TJclHashSetE<T>.Create(FEqualityComparer, AMap);
end;

function TJclHashSetE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityComparer.Equals(A, B);
end;

//=== { TJclHashSetF<T> } ====================================================

function EqualityCompareEqObjects(Obj1, Obj2: TRefUnique): Boolean;
begin
  Result := Obj1 = Obj2;
end;

constructor TJclHashSetF<T>.Create(const AEqualityCompare: TEqualityCompare<T>; const AMap: IJclMap<T, TRefUnique>);
begin
  inherited Create(AMap);
  FEqualityCompare := AEqualityCompare;
end;

constructor TJclHashSetF<T>.Create(const AEqualityCompare: TEqualityCompare<T>; const AHash: THash<T>; const ACompare: TCompare<T>;
  ACapacity: Integer; AOwnsItems: Boolean);
begin
  Create(AEqualityCompare, TJclHashMapF<T, TRefUnique>.Create(AEqualityCompare, AHash, EqualityCompareEqObjects, ACompare, ACapacity, AOwnsItems, False));
end;

function TJclHashSetF<T>.CreateHashSet(const AMap: IJclMap<T, TRefUnique>): TJclHashSet<T>;
begin
  Result := TJclHashSetF<T>.Create(FEqualityCompare, AMap);
end;

function TJclHashSetF<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if not Assigned(EqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityCompare(A, B);
end;

//=== { TJclHashSetI<T> } ====================================================

constructor TJclHashSetI<T>.Create(const AMap: IJclMap<T, TRefUnique>);
begin
  inherited Create(AMap);
end;

constructor TJclHashSetI<T>.Create(ACapacity: Integer; AOwnsItems: Boolean);
begin
  Create(TJclHashMapI<T, TRefUnique>.Create(ACapacity, AOwnsItems, False));
end;

function TJclHashSetI<T>.CreateHashSet(const AMap: IJclMap<T, TRefUnique>): TJclHashSet<T>;
begin
  Result := TJclHashSetI<T>.Create(AMap);
end;

function TJclHashSetI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  Result := A.Equals(B);
end;

{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

