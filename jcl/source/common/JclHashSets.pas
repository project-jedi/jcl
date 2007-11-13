{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

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
  {$IFDEF SUPPORTS_GENERICS}
  TRefUnique = class;
  TRefUnique = class(TEquatable<TRefUnique>)
  end;
  {$ELSE ~SUPPORTS_GENERICS}
  TRefUnique = TObject;
  {$ENDIF ~SUPPORTS_GENERICS}


  TJclIntfHashSet = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclIntfEqualityComparer,
    IJclIntfCollection, IJclIntfSet)
  private
    FMap: IJclIntfMap;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    function GetAutoPackParameter: Integer; override;
    function GetAutoPackStrategy: TJclAutoPackStrategy; override;
    function GetCapacity: Integer; override;
    procedure Pack; override;
    procedure SetAutoPackParameter(Value: Integer); override;
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy); override;
    procedure SetCapacity(Value: Integer); override;
    { IJclContainer }
    function GetAllowDefaultElements: Boolean; override;
    function GetDuplicates: TDuplicates; override;
    function GetRemoveSingleElement: Boolean; override;
    function GetReturnDefaultElements: Boolean; override;
    procedure SetAllowDefaultElements(Value: Boolean); override;
    procedure SetDuplicates(Value: TDuplicates); override;
    procedure SetRemoveSingleElement(Value: Boolean); override;
    procedure SetReturnDefaultElements(Value: Boolean); override;
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
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIntfIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclIntfSet }
    procedure Intersect(const ACollection: IJclIntfCollection);
    procedure Subtract(const ACollection: IJclIntfCollection);
    procedure Union(const ACollection: IJclIntfCollection);
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AMap: IJclIntfMap); overload;
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
  end;


  TJclAnsiStrHashSet = class(TJclAnsiStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer, IJclAnsiStrEqualityComparer,
    IJclAnsiStrCollection, IJclAnsiStrSet)
  private
    FMap: IJclAnsiStrMap;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    function GetAutoPackParameter: Integer; override;
    function GetAutoPackStrategy: TJclAutoPackStrategy; override;
    function GetCapacity: Integer; override;
    procedure Pack; override;
    procedure SetAutoPackParameter(Value: Integer); override;
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy); override;
    procedure SetCapacity(Value: Integer); override;
    { IJclContainer }
    function GetAllowDefaultElements: Boolean; override;
    function GetDuplicates: TDuplicates; override;
    function GetRemoveSingleElement: Boolean; override;
    function GetReturnDefaultElements: Boolean; override;
    procedure SetAllowDefaultElements(Value: Boolean); override;
    procedure SetDuplicates(Value: TDuplicates); override;
    procedure SetRemoveSingleElement(Value: Boolean); override;
    procedure SetReturnDefaultElements(Value: Boolean); override;
    { IJclAnsiStrCollection }
    function Add(const AString: AnsiString): Boolean; override;
    function AddAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    procedure Clear; override;
    function Contains(const AString: AnsiString): Boolean; override;
    function ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Equals(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function First: IJclAnsiStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclAnsiStrIterator; override;
    function Remove(const AString: AnsiString): Boolean; override;
    function RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Size: Integer; override;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclAnsiStrIterator; override;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclAnsiStrSet }
    procedure Intersect(const ACollection: IJclAnsiStrCollection);
    procedure Subtract(const ACollection: IJclAnsiStrCollection);
    procedure Union(const ACollection: IJclAnsiStrCollection);
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclStrContainer }
    function GetCaseSensitive: Boolean; override;
    procedure SetCaseSensitive(Value: Boolean); override;
    { IJclAnsiStrContainer }
    function GetEncoding: TJclAnsiStrEncoding; override;
    procedure SetEncoding(Value: TJclAnsiStrEncoding); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AMap: IJclAnsiStrMap); overload;
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
  end;


  TJclWideStrHashSet = class(TJclWideStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclStrContainer, IJclWideStrContainer, IJclWideStrEqualityComparer,
    IJclWideStrCollection, IJclWideStrSet)
  private
    FMap: IJclWideStrMap;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    function GetAutoPackParameter: Integer; override;
    function GetAutoPackStrategy: TJclAutoPackStrategy; override;
    function GetCapacity: Integer; override;
    procedure Pack; override;
    procedure SetAutoPackParameter(Value: Integer); override;
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy); override;
    procedure SetCapacity(Value: Integer); override;
    { IJclContainer }
    function GetAllowDefaultElements: Boolean; override;
    function GetDuplicates: TDuplicates; override;
    function GetRemoveSingleElement: Boolean; override;
    function GetReturnDefaultElements: Boolean; override;
    procedure SetAllowDefaultElements(Value: Boolean); override;
    procedure SetDuplicates(Value: TDuplicates); override;
    procedure SetRemoveSingleElement(Value: Boolean); override;
    procedure SetReturnDefaultElements(Value: Boolean); override;
    { IJclWideStrCollection }
    function Add(const AString: WideString): Boolean; override;
    function AddAll(const ACollection: IJclWideStrCollection): Boolean; override;
    procedure Clear; override;
    function Contains(const AString: WideString): Boolean; override;
    function ContainsAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function Equals(const ACollection: IJclWideStrCollection): Boolean; override;
    function First: IJclWideStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclWideStrIterator; override;
    function Remove(const AString: WideString): Boolean; override;
    function RemoveAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function Size: Integer; override;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclWideStrIterator; override;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclWideStrSet }
    procedure Intersect(const ACollection: IJclWideStrCollection);
    procedure Subtract(const ACollection: IJclWideStrCollection);
    procedure Union(const ACollection: IJclWideStrCollection);
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclStrContainer }
    function GetCaseSensitive: Boolean; override;
    procedure SetCaseSensitive(Value: Boolean); override;
    { IJclWideStrContainer }
    function GetEncoding: TJclWideStrEncoding; override;
    procedure SetEncoding(Value: TJclWideStrEncoding); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AMap: IJclWideStrMap); overload;
    constructor Create(ACapacity: Integer); overload;
    destructor Destroy; override;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrHashSet = TJclAnsiStrHashSet;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrHashSet = TJclWideStrHashSet;
  {$ENDIF CONTAINER_WIDESTR}


  TJclHashSet = class(TJclAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclObjectOwner, IJclEqualityComparer,
    IJclCollection, IJclSet)
  private
    FMap: IJclMap;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    function GetAutoPackParameter: Integer; override;
    function GetAutoPackStrategy: TJclAutoPackStrategy; override;
    function GetCapacity: Integer; override;
    procedure Pack; override;
    procedure SetAutoPackParameter(Value: Integer); override;
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy); override;
    procedure SetCapacity(Value: Integer); override;
    { IJclContainer }
    function GetAllowDefaultElements: Boolean; override;
    function GetDuplicates: TDuplicates; override;
    function GetRemoveSingleElement: Boolean; override;
    function GetReturnDefaultElements: Boolean; override;
    procedure SetAllowDefaultElements(Value: Boolean); override;
    procedure SetDuplicates(Value: TDuplicates); override;
    procedure SetRemoveSingleElement(Value: Boolean); override;
    procedure SetReturnDefaultElements(Value: Boolean); override;
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
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclSet }
    procedure Intersect(const ACollection: IJclCollection);
    procedure Subtract(const ACollection: IJclCollection);
    procedure Union(const ACollection: IJclCollection);
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclObjectOwner }
    function FreeObject(var AObject: TObject): TObject; override;
    function GetOwnsObjects: Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AMap: IJclMap); overload;
    constructor Create(ACapacity: Integer; AOwnsObjects: Boolean); overload;
    destructor Destroy; override;
  end;

  {$IFDEF SUPPORTS_GENERICS}


  TJclHashSet<T> = class(TJclAbstractContainer<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclSet<T>)
  private
    FMap: IJclMap<T, TRefUnique>;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    function GetAutoPackParameter: Integer; override;
    function GetAutoPackStrategy: TJclAutoPackStrategy; override;
    function GetCapacity: Integer; override;
    procedure Pack; override;
    procedure SetAutoPackParameter(Value: Integer); override;
    procedure SetAutoPackStrategy(Value: TJclAutoPackStrategy); override;
    procedure SetCapacity(Value: Integer); override;
    { IJclContainer }
    function GetAllowDefaultElements: Boolean; override;
    function GetDuplicates: TDuplicates; override;
    function GetRemoveSingleElement: Boolean; override;
    function GetReturnDefaultElements: Boolean; override;
    procedure SetAllowDefaultElements(Value: Boolean); override;
    procedure SetDuplicates(Value: TDuplicates); override;
    procedure SetRemoveSingleElement(Value: Boolean); override;
    procedure SetReturnDefaultElements(Value: Boolean); override;
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
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIterator<T>;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclSet<T> }
    procedure Intersect(const ACollection: IJclCollection<T>);
    procedure Subtract(const ACollection: IJclCollection<T>);
    procedure Union(const ACollection: IJclCollection<T>);
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclItemOwner<T> }
    function FreeItem(var AItem: T): T; override;
    function GetOwnsItems: Boolean; override;
  public
    constructor Create(const AMap: IJclMap<T, TRefUnique>); overload;
    destructor Destroy; override;
  end;

  // E = External helper to compare items for equality
  TJclHashSetE<T> = class(TJclHashSet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclCollection<T>, IJclSet<T>,
    IJclItemOwner<T>, IJclEqualityComparer<T>)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; const AMap: IJclMap<T, TRefUnique>); overload;
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; const AComparer: IComparer<T>;
      ACapacity: Integer; AOwnsItems: Boolean); overload;

    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclHashSetF<T> = class(TJclHashSet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclCollection<T>, IJclSet<T>,
    IJclItemOwner<T>, IJclEqualityComparer<T>)
  private
    FEqualityCompare: TEqualityCompare<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const AMap: IJclMap<T, TRefUnique>); overload;
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const AHash: THash<T>; const ACompare: TCompare<T>;
      ACapacity: Integer; AOwnsItems: Boolean); overload;

    property EqualityCompare: TEqualityCompare<T> read FEqualityCompare write FEqualityCompare;
  end;

  // I = Items can compare themselves to an other
  TJclHashSetI<T: IEquatable<T>, IComparable<T>, IHashable> = class(TJclHashSet<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable,
    IJclContainer, IJclCollection<T>, IJclSet<T>, IJclItemOwner<T>, IJclEqualityComparer<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AMap: IJclMap<T, TRefUnique>); overload;
    constructor Create(ACapacity: Integer; AOwnsItems: Boolean); overload;
  end;
  {$ENDIF SUPPORTS_GENERICS}

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


//=== { TJclIntfHashSet } =====================================================

constructor TJclIntfHashSet.Create(const AMap: IJclIntfMap);
begin
  inherited Create(AMap);
  FMap := AMap;
end;

constructor TJclIntfHashSet.Create(ACapacity: Integer);
begin
  Create(TJclIntfHashMap.Create(ACapacity, False));
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
      FMap.PutValue(AInterface, RefUnique);
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfHashSet then
    TJclIntfHashSet(Dest).FMap := (FMap as IJclIntfCloneable).Clone as IJclIntfMap;
end;

procedure TJclIntfHashSet.Clear;
begin
  FMap.Clear;
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := FMap.ContainsKey(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfHashSet.Create(FMap.Size);
  AssignPropertiesTo(Result);
end;

function TJclIntfHashSet.Equals(const ACollection: IJclIntfCollection): Boolean;
var
  It, ItMap: IJclIntfIterator;
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
    Result := True;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ItemsEqual(ItMap.Next, It.Next) then
      begin
        Result := False;
        Exit;
      end;
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

function TJclIntfHashSet.GetAutoPackParameter: Integer;
begin
  Result := (FMap as IJclPackable).GetAutoPackParameter;
end;

function TJclIntfHashSet.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := (FMap as IJclPackable).GetAutoPackStrategy;
end;

function TJclIntfHashSet.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

function TJclIntfHashSet.GetAllowDefaultElements: Boolean;
begin
  Result := FMap.AllowDefaultElements;
end;

function TJclIntfHashSet.GetDuplicates: TDuplicates;
begin
  Result := FMap.Duplicates;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntfHashSet.GetEnumerator: IJclIntfIterator;
begin
  Result := FMap.KeySet.First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclIntfHashSet.GetRemoveSingleElement: Boolean;
begin
  Result := FMap.RemoveSingleElement;
end;

function TJclIntfHashSet.GetReturnDefaultElements: Boolean;
begin
  Result := FMap.ReturnDefaultElements;
end;

procedure TJclIntfHashSet.Intersect(const ACollection: IJclIntfCollection);
begin
  RetainAll(ACollection);
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
  Result := FMap.Remove(AInterface) = RefUnique;
end;

function TJclIntfHashSet.RemoveAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  ARefUnique: TRefUnique;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
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
    Result := True;
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

procedure TJclIntfHashSet.SetAutoPackParameter(Value: Integer);
begin
  (FMap as IJclPackable).SetAutoPackParameter(Value);
end;

procedure TJclIntfHashSet.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  (FMap as IJclPackable).SetAutoPackStrategy(Value);
end;

procedure TJclIntfHashSet.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

procedure TJclIntfHashSet.SetAllowDefaultElements(Value: Boolean);
begin
  FMap.AllowDefaultElements := Value;
end;

procedure TJclIntfHashSet.SetDuplicates(Value: TDuplicates);
begin
  FMap.Duplicates := Value;
end;

procedure TJclIntfHashSet.SetRemoveSingleElement(Value: Boolean);
begin
  FMap.RemoveSingleElement := Value;
end;

procedure TJclIntfHashSet.SetReturnDefaultElements(Value: Boolean);
begin
  FMap.ReturnDefaultElements := Value;
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



//=== { TJclAnsiStrHashSet } =====================================================

constructor TJclAnsiStrHashSet.Create(const AMap: IJclAnsiStrMap);
begin
  inherited Create(AMap);
  FMap := AMap;
end;

constructor TJclAnsiStrHashSet.Create(ACapacity: Integer);
begin
  Create(TJclAnsiStrHashMap.Create(ACapacity, False));
end;

destructor TJclAnsiStrHashSet.Destroy;
begin
  Clear;
  FMap := nil;
  inherited Destroy;
end;

function TJclAnsiStrHashSet.Add(const AString: AnsiString): Boolean;
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

function TJclAnsiStrHashSet.AddAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrHashSet then
    TJclAnsiStrHashSet(Dest).FMap := (FMap as IJclIntfCloneable).Clone as IJclAnsiStrMap;
end;

procedure TJclAnsiStrHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclAnsiStrHashSet.Contains(const AString: AnsiString): Boolean;
begin
  Result := FMap.ContainsKey(AString);
end;

function TJclAnsiStrHashSet.ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := FMap.ContainsKey(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrHashSet.Create(FMap.Size);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrHashSet.Equals(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It, ItMap: IJclAnsiStrIterator;
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
    Result := True;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ItemsEqual(ItMap.Next, It.Next) then
      begin
        Result := False;
        Exit;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashSet.First: IJclAnsiStrIterator;
begin
  Result := FMap.KeySet.First;
end;

function TJclAnsiStrHashSet.GetAutoPackParameter: Integer;
begin
  Result := (FMap as IJclPackable).GetAutoPackParameter;
end;

function TJclAnsiStrHashSet.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := (FMap as IJclPackable).GetAutoPackStrategy;
end;

function TJclAnsiStrHashSet.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

function TJclAnsiStrHashSet.GetAllowDefaultElements: Boolean;
begin
  Result := FMap.AllowDefaultElements;
end;

function TJclAnsiStrHashSet.GetDuplicates: TDuplicates;
begin
  Result := FMap.Duplicates;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclAnsiStrHashSet.GetEnumerator: IJclAnsiStrIterator;
begin
  Result := FMap.KeySet.First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclAnsiStrHashSet.GetRemoveSingleElement: Boolean;
begin
  Result := FMap.RemoveSingleElement;
end;

function TJclAnsiStrHashSet.GetReturnDefaultElements: Boolean;
begin
  Result := FMap.ReturnDefaultElements;
end;

function TJclAnsiStrHashSet.GetCaseSensitive: Boolean;
begin
  Result := FMap.GetCaseSensitive;
end;

function TJclAnsiStrHashSet.GetEncoding: TJclAnsiStrEncoding;
begin
  Result := FMap.GetEncoding;
end;

procedure TJclAnsiStrHashSet.Intersect(const ACollection: IJclAnsiStrCollection);
begin
  RetainAll(ACollection);
end;

function TJclAnsiStrHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclAnsiStrHashSet.Last: IJclAnsiStrIterator;
begin
  Result := FMap.KeySet.Last;
end;

procedure TJclAnsiStrHashSet.Pack;
begin
  (FMap as IJclPackable).Pack;
end;

function TJclAnsiStrHashSet.Remove(const AString: AnsiString): Boolean;
begin
  Result := FMap.Remove(AString) = RefUnique;
end;

function TJclAnsiStrHashSet.RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
  ARefUnique: TRefUnique;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
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

function TJclAnsiStrHashSet.RetainAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  ItMap: IJclAnsiStrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
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

procedure TJclAnsiStrHashSet.SetAutoPackParameter(Value: Integer);
begin
  (FMap as IJclPackable).SetAutoPackParameter(Value);
end;

procedure TJclAnsiStrHashSet.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  (FMap as IJclPackable).SetAutoPackStrategy(Value);
end;

procedure TJclAnsiStrHashSet.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

procedure TJclAnsiStrHashSet.SetAllowDefaultElements(Value: Boolean);
begin
  FMap.AllowDefaultElements := Value;
end;

procedure TJclAnsiStrHashSet.SetDuplicates(Value: TDuplicates);
begin
  FMap.Duplicates := Value;
end;

procedure TJclAnsiStrHashSet.SetRemoveSingleElement(Value: Boolean);
begin
  FMap.RemoveSingleElement := Value;
end;

procedure TJclAnsiStrHashSet.SetReturnDefaultElements(Value: Boolean);
begin
  FMap.ReturnDefaultElements := Value;
end;

procedure TJclAnsiStrHashSet.SetCaseSensitive(Value: Boolean);
begin
  FMap.SetCaseSensitive(Value);
end;

procedure TJclAnsiStrHashSet.SetEncoding(Value: TJclAnsiStrEncoding);
begin
  FMap.SetEncoding(Value);
end;

function TJclAnsiStrHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclAnsiStrHashSet.Subtract(const ACollection: IJclAnsiStrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclAnsiStrHashSet.Union(const ACollection: IJclAnsiStrCollection);
begin
  AddAll(ACollection);
end;



//=== { TJclWideStrHashSet } =====================================================

constructor TJclWideStrHashSet.Create(const AMap: IJclWideStrMap);
begin
  inherited Create(AMap);
  FMap := AMap;
end;

constructor TJclWideStrHashSet.Create(ACapacity: Integer);
begin
  Create(TJclWideStrHashMap.Create(ACapacity, False));
end;

destructor TJclWideStrHashSet.Destroy;
begin
  Clear;
  FMap := nil;
  inherited Destroy;
end;

function TJclWideStrHashSet.Add(const AString: WideString): Boolean;
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

function TJclWideStrHashSet.AddAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrHashSet then
    TJclWideStrHashSet(Dest).FMap := (FMap as IJclIntfCloneable).Clone as IJclWideStrMap;
end;

procedure TJclWideStrHashSet.Clear;
begin
  FMap.Clear;
end;

function TJclWideStrHashSet.Contains(const AString: WideString): Boolean;
begin
  Result := FMap.ContainsKey(AString);
end;

function TJclWideStrHashSet.ContainsAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := FMap.ContainsKey(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrHashSet.Create(FMap.Size);
  AssignPropertiesTo(Result);
end;

function TJclWideStrHashSet.Equals(const ACollection: IJclWideStrCollection): Boolean;
var
  It, ItMap: IJclWideStrIterator;
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
    Result := True;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ItemsEqual(ItMap.Next, It.Next) then
      begin
        Result := False;
        Exit;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashSet.First: IJclWideStrIterator;
begin
  Result := FMap.KeySet.First;
end;

function TJclWideStrHashSet.GetAutoPackParameter: Integer;
begin
  Result := (FMap as IJclPackable).GetAutoPackParameter;
end;

function TJclWideStrHashSet.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := (FMap as IJclPackable).GetAutoPackStrategy;
end;

function TJclWideStrHashSet.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

function TJclWideStrHashSet.GetAllowDefaultElements: Boolean;
begin
  Result := FMap.AllowDefaultElements;
end;

function TJclWideStrHashSet.GetDuplicates: TDuplicates;
begin
  Result := FMap.Duplicates;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclWideStrHashSet.GetEnumerator: IJclWideStrIterator;
begin
  Result := FMap.KeySet.First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclWideStrHashSet.GetRemoveSingleElement: Boolean;
begin
  Result := FMap.RemoveSingleElement;
end;

function TJclWideStrHashSet.GetReturnDefaultElements: Boolean;
begin
  Result := FMap.ReturnDefaultElements;
end;

function TJclWideStrHashSet.GetCaseSensitive: Boolean;
begin
  Result := FMap.GetCaseSensitive;
end;

function TJclWideStrHashSet.GetEncoding: TJclWideStrEncoding;
begin
  Result := FMap.GetEncoding;
end;

procedure TJclWideStrHashSet.Intersect(const ACollection: IJclWideStrCollection);
begin
  RetainAll(ACollection);
end;

function TJclWideStrHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TJclWideStrHashSet.Last: IJclWideStrIterator;
begin
  Result := FMap.KeySet.Last;
end;

procedure TJclWideStrHashSet.Pack;
begin
  (FMap as IJclPackable).Pack;
end;

function TJclWideStrHashSet.Remove(const AString: WideString): Boolean;
begin
  Result := FMap.Remove(AString) = RefUnique;
end;

function TJclWideStrHashSet.RemoveAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
  ARefUnique: TRefUnique;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
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

function TJclWideStrHashSet.RetainAll(const ACollection: IJclWideStrCollection): Boolean;
var
  ItMap: IJclWideStrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
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

procedure TJclWideStrHashSet.SetAutoPackParameter(Value: Integer);
begin
  (FMap as IJclPackable).SetAutoPackParameter(Value);
end;

procedure TJclWideStrHashSet.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  (FMap as IJclPackable).SetAutoPackStrategy(Value);
end;

procedure TJclWideStrHashSet.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

procedure TJclWideStrHashSet.SetAllowDefaultElements(Value: Boolean);
begin
  FMap.AllowDefaultElements := Value;
end;

procedure TJclWideStrHashSet.SetDuplicates(Value: TDuplicates);
begin
  FMap.Duplicates := Value;
end;

procedure TJclWideStrHashSet.SetRemoveSingleElement(Value: Boolean);
begin
  FMap.RemoveSingleElement := Value;
end;

procedure TJclWideStrHashSet.SetReturnDefaultElements(Value: Boolean);
begin
  FMap.ReturnDefaultElements := Value;
end;

procedure TJclWideStrHashSet.SetCaseSensitive(Value: Boolean);
begin
  FMap.SetCaseSensitive(Value);
end;

procedure TJclWideStrHashSet.SetEncoding(Value: TJclWideStrEncoding);
begin
  FMap.SetEncoding(Value);
end;

function TJclWideStrHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TJclWideStrHashSet.Subtract(const ACollection: IJclWideStrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclWideStrHashSet.Union(const ACollection: IJclWideStrCollection);
begin
  AddAll(ACollection);
end;



//=== { TJclHashSet } =====================================================

constructor TJclHashSet.Create(const AMap: IJclMap);
begin
  inherited Create(AMap, False);
  FMap := AMap;
end;

constructor TJclHashSet.Create(ACapacity: Integer; AOwnsObjects: Boolean);
begin
  Create(TJclHashMap.Create(ACapacity, AOwnsObjects, False));
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashSet.AssignDataTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclHashSet then
    TJclHashSet(Dest).FMap := (FMap as IJclIntfCloneable).Clone as IJclMap;
end;

procedure TJclHashSet.Clear;
begin
  FMap.Clear;
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := FMap.ContainsKey(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashSet.Create(FMap.Size, False);
  AssignPropertiesTo(Result);
end;

function TJclHashSet.Equals(const ACollection: IJclCollection): Boolean;
var
  It, ItMap: IJclIterator;
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
    Result := True;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ItemsEqual(ItMap.Next, It.Next) then
      begin
        Result := False;
        Exit;
      end;
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

function TJclHashSet.FreeObject(var AObject: TObject): TObject;
begin
  Result := (FMap as IJclKeyOwner).FreeKey(AObject);
end;

function TJclHashSet.GetAutoPackParameter: Integer;
begin
  Result := (FMap as IJclPackable).GetAutoPackParameter;
end;

function TJclHashSet.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := (FMap as IJclPackable).GetAutoPackStrategy;
end;

function TJclHashSet.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

function TJclHashSet.GetAllowDefaultElements: Boolean;
begin
  Result := FMap.AllowDefaultElements;
end;

function TJclHashSet.GetDuplicates: TDuplicates;
begin
  Result := FMap.Duplicates;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclHashSet.GetEnumerator: IJclIterator;
begin
  Result := FMap.KeySet.First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclHashSet.GetOwnsObjects: Boolean;
begin
  Result := (FMap as IJclKeyOwner).GetOwnsKeys;
end;

function TJclHashSet.GetRemoveSingleElement: Boolean;
begin
  Result := FMap.RemoveSingleElement;
end;

function TJclHashSet.GetReturnDefaultElements: Boolean;
begin
  Result := FMap.ReturnDefaultElements;
end;

procedure TJclHashSet.Intersect(const ACollection: IJclCollection);
begin
  RetainAll(ACollection);
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
  ARefUnique: TRefUnique;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
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
    Result := True;
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

procedure TJclHashSet.SetAutoPackParameter(Value: Integer);
begin
  (FMap as IJclPackable).SetAutoPackParameter(Value);
end;

procedure TJclHashSet.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  (FMap as IJclPackable).SetAutoPackStrategy(Value);
end;

procedure TJclHashSet.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

procedure TJclHashSet.SetAllowDefaultElements(Value: Boolean);
begin
  FMap.AllowDefaultElements := Value;
end;

procedure TJclHashSet.SetDuplicates(Value: TDuplicates);
begin
  FMap.Duplicates := Value;
end;

procedure TJclHashSet.SetRemoveSingleElement(Value: Boolean);
begin
  FMap.RemoveSingleElement := Value;
end;

procedure TJclHashSet.SetReturnDefaultElements(Value: Boolean);
begin
  FMap.ReturnDefaultElements := Value;
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
  inherited Create(AMap, False);
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashSet<T>.AssignDataTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclHashSet<T> then
    TJclHashSet<T>(Dest).FMap := (FMap as IJclIntfCloneable).Clone as IJclMap<T, TRefUnique>;
end;

procedure TJclHashSet<T>.Clear;
begin
  FMap.Clear;
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
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
  It, ItMap: IJclIterator<T>;
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
    Result := True;
    It := ACollection.First;
    ItMap := FMap.KeySet.First;
    while ItMap.HasNext do
      if not ItemsEqual(ItMap.Next, It.Next) then
      begin
        Result := False;
        Exit;
      end;
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

function TJclHashSet<T>.FreeItem(var AItem: T): T;
begin
  Result := (FMap as IJclPairOwner<T, TRefUnique>).FreeKey(AItem);
end;

function TJclHashSet<T>.GetAutoPackParameter: Integer;
begin
  Result := (FMap as IJclPackable).GetAutoPackParameter;
end;

function TJclHashSet<T>.GetAutoPackStrategy: TJclAutoPackStrategy;
begin
  Result := (FMap as IJclPackable).GetAutoPackStrategy;
end;

function TJclHashSet<T>.GetCapacity: Integer;
begin
  Result := (FMap as IJclPackable).GetCapacity;
end;

function TJclHashSet<T>.GetAllowDefaultElements: Boolean;
begin
  Result := FMap.AllowDefaultElements;
end;

function TJclHashSet<T>.GetDuplicates: TDuplicates;
begin
  Result := FMap.Duplicates;
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclHashSet<T>.GetEnumerator: IJclIterator<T>;
begin
  Result := FMap.KeySet.First;
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclHashSet<T>.GetOwnsItems: Boolean;
begin
  Result := (FMap as IJclPairOwner<T, TRefUnique>).GetOwnsKeys;
end;

function TJclHashSet<T>.GetRemoveSingleElement: Boolean;
begin
  Result := FMap.RemoveSingleElement;
end;

function TJclHashSet<T>.GetReturnDefaultElements: Boolean;
begin
  Result := FMap.ReturnDefaultElements;
end;

procedure TJclHashSet<T>.Intersect(const ACollection: IJclCollection<T>);
begin
  RetainAll(ACollection);
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
  ARefUnique: TRefUnique;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
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
    Result := True;
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

procedure TJclHashSet<T>.SetAutoPackParameter(Value: Integer);
begin
  (FMap as IJclPackable).SetAutoPackParameter(Value);
end;

procedure TJclHashSet<T>.SetAutoPackStrategy(Value: TJclAutoPackStrategy);
begin
  (FMap as IJclPackable).SetAutoPackStrategy(Value);
end;

procedure TJclHashSet<T>.SetCapacity(Value: Integer);
begin
  (FMap as IJclPackable).SetCapacity(Value);
end;

procedure TJclHashSet<T>.SetAllowDefaultElements(Value: Boolean);
begin
  FMap.AllowDefaultElements := Value;
end;

procedure TJclHashSet<T>.SetDuplicates(Value: TDuplicates);
begin
  FMap.Duplicates := Value;
end;

procedure TJclHashSet<T>.SetRemoveSingleElement(Value: Boolean);
begin
  FMap.RemoveSingleElement := Value;
end;

procedure TJclHashSet<T>.SetReturnDefaultElements(Value: Boolean);
begin
  FMap.ReturnDefaultElements := Value;
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

procedure TJclHashSetE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclHashSetE<T> then
    TJclHashSetE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclHashSetE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
var
  AMap: IJclMap<T, TRefUnique>;
begin
  AMap := (FMap as IJclCloneable).Clone as IJclMap<T, TRefUnique>;
  AMap.Clear;
  Result := TJclHashSetE<T>.Create(FEqualityComparer, AMap);
  AssignPropertiesTo(Result);
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

procedure TJclHashSetF<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclHashSetF<T> then
    TJclHashSetF<T>(Dest).FEqualityCompare := FEqualityCompare;
end;

function TJclHashSetF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
var
  AMap: IJclMap<T, TRefUnique>;
begin
  AMap := (FMap as IJclCloneable).Clone as IJclMap<T, TRefUnique>;
  AMap.Clear;
  Result := TJclHashSetF<T>.Create(FEqualityCompare, AMap);
  AssignPropertiesTo(Result);
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

function TJclHashSetI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
var
  AMap: IJclMap<T, TRefUnique>;
begin
  AMap := (FMap as IJclCloneable).Clone as IJclMap<T, TRefUnique>;
  AMap.Clear;
  Result := TJclHashSetI<T>.Create(AMap);
  AssignPropertiesTo(Result);
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

