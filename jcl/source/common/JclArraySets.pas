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
{ The Original Code is ArraySet.pas.                                                               }
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

unit JclArraySets;

{$I jcl.inc}

interface

uses
  Classes,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF SUPPORTS_GENERICS}
  {$IFDEF CLR}
  System.Collections.Generic,
  {$ENDIF CLR}
  JclAlgorithms,
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclContainerIntf, JclArrayLists;
type

  TJclIntfArraySet = class(TJclIntfArrayList, IJclIntfCollection, IJclIntfList, IJclIntfArray, IJclIntfSet, IJclContainer, IJclIntfEqualityComparer, IJclIntfComparer,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    function BinarySearch(const AInterface: IInterface): Integer;
  protected
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIntfCollection }
    function Add(const AInterface: IInterface): Boolean;
    function AddAll(const ACollection: IJclIntfCollection): Boolean;
    function Contains(const AInterface: IInterface): Boolean;
    { IJclIntfList }
    procedure Insert(Index: Integer; const AInterface: IInterface); overload;
    { IJclIntfSet }
    procedure Intersect(const ACollection: IJclIntfCollection);
    procedure Subtract(const ACollection: IJclIntfCollection);
    procedure Union(const ACollection: IJclIntfCollection);
    function CreateEmptyContainer: TJclAbstractContainer; override;
  public
  end;

  TJclStrArraySet = class(TJclStrArrayList, IJclStrCollection, IJclStrList, IJclStrArray, IJclStrSet, IJclContainer, IJclStrContainer, IJclStrFlatContainer, IJclStrEqualityComparer, IJclStrComparer,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    function BinarySearch(const AString: string): Integer;
  protected
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclStrCollection }
    function Add(const AString: string): Boolean; override;
    function AddAll(const ACollection: IJclStrCollection): Boolean; override;
    function Contains(const AString: string): Boolean; override;
    { IJclStrList }
    procedure Insert(Index: Integer; const AString: string); overload;
    { IJclStrSet }
    procedure Intersect(const ACollection: IJclStrCollection);
    procedure Subtract(const ACollection: IJclStrCollection);
    procedure Union(const ACollection: IJclStrCollection);
    function CreateEmptyContainer: TJclAbstractContainer; override;
  public
  end;

  TJclArraySet = class(TJclArrayList, IJclCollection, IJclList, IJclArray, IJclSet, IJclContainer, IJclObjectOwner, IJclEqualityComparer, IJclComparer,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    function BinarySearch(AObject: TObject): Integer;
  protected
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclCollection }
    function Add(AObject: TObject): Boolean;
    function AddAll(const ACollection: IJclCollection): Boolean;
    function Contains(AObject: TObject): Boolean;
    { IJclList }
    procedure Insert(Index: Integer; AObject: TObject); overload;
    { IJclSet }
    procedure Intersect(const ACollection: IJclCollection);
    procedure Subtract(const ACollection: IJclCollection);
    procedure Union(const ACollection: IJclCollection);
    function CreateEmptyContainer: TJclAbstractContainer; override;
  public
  end;

  {$IFDEF SUPPORTS_GENERICS}

  TJclArraySet<T> = class(TJclArrayList<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    function BinarySearch(const AItem: T): Integer;
  protected
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclCollection<T> }
    function Add(const AItem: T): Boolean;
    function AddAll(const ACollection: IJclCollection<T>): Boolean;
    function Contains(const AItem: T): Boolean;
    { IJclList<T> }
    procedure Insert(Index: Integer; const AItem: T); overload;
    { IJclSet<T> }
    procedure Intersect(const ACollection: IJclCollection<T>);
    procedure Subtract(const ACollection: IJclCollection<T>);
    procedure Union(const ACollection: IJclCollection<T>);
  public
  end;

  // E = External helper to compare items
  TJclArraySetE<T> = class(TJclArraySet<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FComparer: IComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    function ItemsCompare(const A, B: T): Integer; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AComparer: IComparer<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const AComparer: IComparer<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;

    property Comparer: IComparer<T> read FComparer write FComparer;
  end;

  // F = Function to compare items
  TJclArraySetF<T> = class(TJclArraySet<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FCompare: TCompare<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    function ItemsCompare(const A, B: T): Integer; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const ACompare: TCompare<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const ACompare: TCompare<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;

    property Compare: TCompare<T> read FCompare write FCompare;
  end;

  // I = Items can compare themselves to others
  TJclArraySetI<T: IComparable<T>> = class(TJclArraySet<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclSet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable,
    IJclGrowable)
  protected
    function ItemsCompare(const A, B: T): Integer; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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

uses
  SysUtils;


//=== { TJclIntfArraySet } ====================================================

function TJclIntfArraySet.Add(const AInterface: IInterface): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AInterface, nil);
    if Result then
    begin
      Idx := BinarySearch(AInterface);
      if Idx >= 0 then
        Result := not ItemsEqual(GetObject(Idx), AInterface) or CheckDuplicate
      else
        Result := True;
      if Result then
        Result := inherited Insert(Idx + 1, AInterface);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArraySet.AddAll(const ACollection: IJclIntfCollection): Boolean;
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
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArraySet.BinarySearch(const AInterface: IInterface): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := Size - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := ItemsCompare(GetObject(CompPos), AInterface);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArraySet.Contains(const AInterface: IInterface): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Idx := BinarySearch(AInterface);
    if Idx >= 0 then
      Result := ItemsEqual(GetObject(Idx), AInterface)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArraySet.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclIntfArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

procedure TJclIntfArraySet.Insert(Index: Integer; const AInterface: IInterface);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntfArraySet.Intersect(const ACollection: IJclIntfCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclIntfArraySet.Subtract(const ACollection: IJclIntfCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclIntfArraySet.Union(const ACollection: IJclIntfCollection);
begin
  AddAll(ACollection);
end;


//=== { TJclStrArraySet } ====================================================

function TJclStrArraySet.Add(const AString: string): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AString, '');
    if Result then
    begin
      Idx := BinarySearch(AString);
      if Idx >= 0 then
        Result := not ItemsEqual(GetString(Idx), AString) or CheckDuplicate
      else
        Result := True;
      if Result then
        Result := inherited Insert(Idx + 1, AString);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrArraySet.AddAll(const ACollection: IJclStrCollection): Boolean;
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
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrArraySet.BinarySearch(const AString: string): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := Size - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := ItemsCompare(GetString(CompPos), AString);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrArraySet.Contains(const AString: string): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Idx := BinarySearch(AString);
    if Idx >= 0 then
      Result := ItemsEqual(GetString(Idx), AString)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrArraySet.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStrArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

procedure TJclStrArraySet.Insert(Index: Integer; const AString: string);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclStrArraySet.Intersect(const ACollection: IJclStrCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclStrArraySet.Subtract(const ACollection: IJclStrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclStrArraySet.Union(const ACollection: IJclStrCollection);
begin
  AddAll(ACollection);
end;


//=== { TJclArraySet } ====================================================

function TJclArraySet.Add(AObject: TObject): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AObject, nil);
    if Result then
    begin
      Idx := BinarySearch(AObject);
      if Idx >= 0 then
        Result := not ItemsEqual(GetObject(Idx), AObject) or CheckDuplicate
      else
        Result := True;
      if Result then
        Result := inherited Insert(Idx + 1, AObject);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArraySet.AddAll(const ACollection: IJclCollection): Boolean;
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
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArraySet.BinarySearch(AObject: TObject): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := Size - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := ItemsCompare(GetObject(CompPos), AObject);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArraySet.Contains(AObject: TObject): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Idx := BinarySearch(AObject);
    if Idx >= 0 then
      Result := ItemsEqual(GetObject(Idx), AObject)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArraySet.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclArraySet.Create(Size, False);
  AssignPropertiesTo(Result);
end;

procedure TJclArraySet.Insert(Index: Integer; AObject: TObject);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclArraySet.Intersect(const ACollection: IJclCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclArraySet.Subtract(const ACollection: IJclCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclArraySet.Union(const ACollection: IJclCollection);
begin
  AddAll(ACollection);
end;

{$IFDEF SUPPORTS_GENERICS}

//=== { TJclArraySet<T> } ====================================================

function TJclArraySet<T>.Add(const AItem: T): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AItem, Default(T));
    if Result then
    begin
      Idx := BinarySearch(AItem);
      if Idx >= 0 then
        Result := not ItemsEqual(GetItem(Idx), AItem) or CheckDuplicate
      else
        Result := True;
      if Result then
        Result := inherited Insert(Idx + 1, AItem);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArraySet<T>.AddAll(const ACollection: IJclCollection<T>): Boolean;
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
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArraySet<T>.BinarySearch(const AItem: T): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := Size - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := ItemsCompare(GetItem(CompPos), AItem);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArraySet<T>.Contains(const AItem: T): Boolean;
var
  Idx: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Idx := BinarySearch(AItem);
    if Idx >= 0 then
      Result := ItemsEqual(GetItem(Idx), AItem)
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;


procedure TJclArraySet<T>.Insert(Index: Integer; const AItem: T);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclArraySet<T>.Intersect(const ACollection: IJclCollection<T>);
begin
  RetainAll(ACollection);
end;

procedure TJclArraySet<T>.Subtract(const ACollection: IJclCollection<T>);
begin
  RemoveAll(ACollection);
end;

procedure TJclArraySet<T>.Union(const ACollection: IJclCollection<T>);
begin
  AddAll(ACollection);
end;

//=== { TJclArraySetE<T> } ===================================================

constructor TJclArraySetE<T>.Create(const AComparer: IComparer<T>; ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FComparer := AComparer;
end;

constructor TJclArraySetE<T>.Create(const AComparer: IComparer<T>; const ACollection: IJclCollection<T>;
  AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FComparer := AComparer;
end;

procedure TJclArraySetE<T>.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclArraySetE<T> then
    TJclArraySetE<T>(Dest).FComparer := Comparer;
end;

function TJclArraySetE<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclArraySetE<T>.Create(Comparer, Size, False);
  AssignPropertiesTo(Result);
end;

function TJclArraySetE<T>.ItemsCompare(const A, B: T): Integer;
begin
  if Comparer = nil then
    raise EJclNoComparerError.Create;
  Result := Comparer.Compare(A, B);
end;

function TJclArraySetE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Comparer = nil then
    raise EJclNoComparerError.Create;
  Result := Comparer.Compare(A, B) = 0;
end;

//=== { TJclArraySetF<T> } ===================================================

constructor TJclArraySetF<T>.Create(const ACompare: TCompare<T>; ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FCompare := ACompare;
end;

constructor TJclArraySetF<T>.Create(const ACompare: TCompare<T>; const ACollection: IJclCollection<T>;
  AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FCompare := ACompare;
end;

procedure TJclArraySetF<T>.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclArraySetF<T> then
    TJclArraySetF<T>(Dest).FCompare := Compare;
end;

function TJclArraySetF<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclArraySetF<T>.Create(Compare, Size, False);
  AssignPropertiesTo(Result);
end;

function TJclArraySetF<T>.ItemsCompare(const A, B: T): Integer;
begin
  if not Assigned(Compare) then
    raise EJclNoComparerError.Create;
  Result := Compare(A, B);
end;

function TJclArraySetF<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if not Assigned(Compare) then
    raise EJclNoComparerError.Create;
  Result := Compare(A, B) = 0;
end;

//=== { TJclArraySetI<T> } ===================================================

function TJclArraySetI<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclArraySetI<T>.Create(Size, False);
  AssignPropertiesTo(Result);
end;

function TJclArraySetI<T>.ItemsCompare(const A, B: T): Integer;
begin
  Result := A.CompareTo(B);
end;

function TJclArraySetI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  Result := A.CompareTo(B) = 0;
end;

{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

