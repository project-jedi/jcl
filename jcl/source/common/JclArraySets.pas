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

  TJclIntfArraySet = class(TJclIntfArrayList, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclIntfEqualityComparer, IJclIntfComparer,
    IJclIntfCollection, IJclIntfList, IJclIntfArray, IJclIntfSet)
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
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
  end;


  TJclAnsiStrArraySet = class(TJclAnsiStrArrayList, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer, IJclAnsiStrFlatContainer, IJclAnsiStrEqualityComparer, IJclAnsiStrComparer,
    IJclAnsiStrCollection, IJclAnsiStrList, IJclAnsiStrArray, IJclAnsiStrSet)
  private
    function BinarySearch(const AString: AnsiString): Integer;
  protected
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclAnsiStrCollection }
    function Add(const AString: AnsiString): Boolean; override;
    function AddAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Contains(const AString: AnsiString): Boolean; override;
    { IJclAnsiStrList }
    procedure Insert(Index: Integer; const AString: AnsiString); overload;
    { IJclAnsiStrSet }
    procedure Intersect(const ACollection: IJclAnsiStrCollection);
    procedure Subtract(const ACollection: IJclAnsiStrCollection);
    procedure Union(const ACollection: IJclAnsiStrCollection);
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
  end;


  TJclWideStrArraySet = class(TJclWideStrArrayList, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclStrContainer, IJclWideStrContainer, IJclWideStrFlatContainer, IJclWideStrEqualityComparer, IJclWideStrComparer,
    IJclWideStrCollection, IJclWideStrList, IJclWideStrArray, IJclWideStrSet)
  private
    function BinarySearch(const AString: WideString): Integer;
  protected
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclWideStrCollection }
    function Add(const AString: WideString): Boolean; override;
    function AddAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function Contains(const AString: WideString): Boolean; override;
    { IJclWideStrList }
    procedure Insert(Index: Integer; const AString: WideString); overload;
    { IJclWideStrSet }
    procedure Intersect(const ACollection: IJclWideStrCollection);
    procedure Subtract(const ACollection: IJclWideStrCollection);
    procedure Union(const ACollection: IJclWideStrCollection);
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
  end;

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrArraySet = TJclAnsiStrArraySet;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrArraySet = TJclWideStrArraySet;
  {$ENDIF CONTAINER_WIDESTR}


  TJclArraySet = class(TJclArrayList, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclObjectOwner, IJclEqualityComparer, IJclComparer,
    IJclCollection, IJclList, IJclArray, IJclSet)
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
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
  end;

  {$IFDEF SUPPORTS_GENERICS}

  TJclArraySet<T> = class(TJclArrayList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>)
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
  TJclArraySetE<T> = class(TJclArraySet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>)
  private
    FComparer: IComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function ItemsCompare(const A, B: T): Integer; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AComparer: IComparer<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const AComparer: IComparer<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;

    property Comparer: IComparer<T> read FComparer write FComparer;
  end;

  // F = Function to compare items
  TJclArraySetF<T> = class(TJclArraySet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>)
  private
    FCompare: TCompare<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function ItemsCompare(const A, B: T): Integer; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const ACompare: TCompare<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const ACompare: TCompare<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;

    property Compare: TCompare<T> read FCompare write FCompare;
  end;

  // I = Items can compare themselves to others
  TJclArraySetI<T: IComparable<T>> = class(TJclArraySet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>)
  protected
    function ItemsCompare(const A, B: T): Integer; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
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

function TJclIntfArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
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


//=== { TJclAnsiStrArraySet } ====================================================

function TJclAnsiStrArraySet.Add(const AString: AnsiString): Boolean;
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

function TJclAnsiStrArraySet.AddAll(const ACollection: IJclAnsiStrCollection): Boolean;
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
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrArraySet.BinarySearch(const AString: AnsiString): Integer;
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

function TJclAnsiStrArraySet.Contains(const AString: AnsiString): Boolean;
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

function TJclAnsiStrArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

procedure TJclAnsiStrArraySet.Insert(Index: Integer; const AString: AnsiString);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclAnsiStrArraySet.Intersect(const ACollection: IJclAnsiStrCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclAnsiStrArraySet.Subtract(const ACollection: IJclAnsiStrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclAnsiStrArraySet.Union(const ACollection: IJclAnsiStrCollection);
begin
  AddAll(ACollection);
end;


//=== { TJclWideStrArraySet } ====================================================

function TJclWideStrArraySet.Add(const AString: WideString): Boolean;
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

function TJclWideStrArraySet.AddAll(const ACollection: IJclWideStrCollection): Boolean;
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
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrArraySet.BinarySearch(const AString: WideString): Integer;
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

function TJclWideStrArraySet.Contains(const AString: WideString): Boolean;
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

function TJclWideStrArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

procedure TJclWideStrArraySet.Insert(Index: Integer; const AString: WideString);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclWideStrArraySet.Intersect(const ACollection: IJclWideStrCollection);
begin
  RetainAll(ACollection);
end;

procedure TJclWideStrArraySet.Subtract(const ACollection: IJclWideStrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TJclWideStrArraySet.Union(const ACollection: IJclWideStrCollection);
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

function TJclArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
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

procedure TJclArraySetE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclArraySetE<T> then
    TJclArraySetE<T>(Dest).FComparer := Comparer;
end;

function TJclArraySetE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
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

procedure TJclArraySetF<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclArraySetF<T> then
    TJclArraySetF<T>(Dest).FCompare := Compare;
end;

function TJclArraySetF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
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

function TJclArraySetI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
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

