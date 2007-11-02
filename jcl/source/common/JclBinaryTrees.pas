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
{ The Original Code is BinaryTree.pas.                                                             }
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

unit JclBinaryTrees;

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
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclAlgorithms, JclContainerIntf;
type

  TJclIntfBinaryNode = class
  public
    Value: IInterface;
    Left: TJclIntfBinaryNode;
    Right: TJclIntfBinaryNode;
    Parent: TJclIntfBinaryNode;
  end;

  TJclIntfBinaryTree = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclIntfEqualityComparer, IJclIntfComparer,
    IJclIntfCollection, IJclIntfTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclIntfBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    FCompare: TIntfCompare;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
    { IJclIntfTree }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    { IJclIntfComparer }
    function ItemsCompare(const A, B: IInterface): Integer;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF SUPPORTS_INLINE}
    { IJclIntfEqualityComparer }
    function ItemsEqual(const A, B: IInterface): Boolean;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF SUPPORTS_INLINE}
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TIntfCompare);
    destructor Destroy; override;
    property Compare: TIntfCompare read FCompare write FCompare;
  end;


  TJclAnsiStrBinaryNode = class
  public
    Value: AnsiString;
    Left: TJclAnsiStrBinaryNode;
    Right: TJclAnsiStrBinaryNode;
    Parent: TJclAnsiStrBinaryNode;
  end;

  TJclAnsiStrBinaryTree = class(TJclAnsiStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer, IJclAnsiStrFlatContainer, IJclAnsiStrEqualityComparer, IJclAnsiStrComparer,
    IJclAnsiStrCollection, IJclAnsiStrTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclAnsiStrBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    FCompare: TAnsiStrCompare;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
    { IJclAnsiStrTree }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    { IJclAnsiStrComparer }
    function ItemsCompare(const A, B: AnsiString): Integer;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF SUPPORTS_INLINE}
    { IJclAnsiStrEqualityComparer }
    function ItemsEqual(const A, B: AnsiString): Boolean;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF SUPPORTS_INLINE}
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TAnsiStrCompare);
    destructor Destroy; override;
    property Compare: TAnsiStrCompare read FCompare write FCompare;
  end;


  TJclWideStrBinaryNode = class
  public
    Value: WideString;
    Left: TJclWideStrBinaryNode;
    Right: TJclWideStrBinaryNode;
    Parent: TJclWideStrBinaryNode;
  end;

  TJclWideStrBinaryTree = class(TJclWideStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclStrContainer, IJclWideStrContainer, IJclWideStrFlatContainer, IJclWideStrEqualityComparer, IJclWideStrComparer,
    IJclWideStrCollection, IJclWideStrTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclWideStrBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    FCompare: TWideStrCompare;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
    { IJclWideStrTree }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    { IJclWideStrComparer }
    function ItemsCompare(const A, B: WideString): Integer;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF SUPPORTS_INLINE}
    { IJclWideStrEqualityComparer }
    function ItemsEqual(const A, B: WideString): Boolean;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF SUPPORTS_INLINE}
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TWideStrCompare);
    destructor Destroy; override;
    property Compare: TWideStrCompare read FCompare write FCompare;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrBinaryTree = TJclAnsiStrBinaryTree;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrBinaryTree = TJclWideStrBinaryTree;
  {$ENDIF CONTAINER_WIDESTR}


  TJclBinaryNode = class
  public
    Value: TObject;
    Left: TJclBinaryNode;
    Right: TJclBinaryNode;
    Parent: TJclBinaryNode;
  end;

  TJclBinaryTree = class(TJclAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclObjectOwner, IJclEqualityComparer, IJclComparer,
    IJclCollection, IJclTree)
  private
    FMaxDepth: Integer;
    FRoot: TJclBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    FCompare: TCompare;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
    { IJclTree }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    { IJclComparer }
    function ItemsCompare(A, B: TObject): Integer;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF SUPPORTS_INLINE}
    { IJclEqualityComparer }
    function ItemsEqual(A, B: TObject): Boolean;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF SUPPORTS_INLINE}
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TCompare; AOwnsObjects: Boolean);
    destructor Destroy; override;
    property Compare: TCompare read FCompare write FCompare;
  end;

  {$IFDEF SUPPORTS_GENERICS}

  TJclBinaryNode<T> = class
  public
    Value: T;
    Left: TJclBinaryNode<T>;
    Right: TJclBinaryNode<T>;
    Parent: TJclBinaryNode<T>;
  end;

  TJclBinaryTree<T> = class(TJclAbstractContainer<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  private
    FMaxDepth: Integer;
    FRoot: TJclBinaryNode<T>;
    FTraverseOrder: TJclTraverseOrder;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure AutoPack; override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
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
    { IJclTree<T> }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
  public
    constructor Create(AOwnsItems: Boolean);
    destructor Destroy; override;
  end;

  // E = External helper to compare items
  TJclBinaryTreeE<T> = class(TJclBinaryTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  private
    FComparer: IComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclComparer<T> }
    function ItemsCompare(const A, B: T): Integer; override;
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AComparer: IComparer<T>; AOwnsItems: Boolean);
    property Comparer: IComparer<T> read FComparer write FComparer;
  end;

  // F = Function to compare items
  TJclBinaryTreeF<T> = class(TJclBinaryTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  private
    FCompare: TCompare<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclComparer<T> }
    function ItemsCompare(const A, B: T): Integer; override;
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(ACompare: TCompare<T>; AOwnsItems: Boolean);
    property Compare: TCompare<T> read FCompare write FCompare;
  end;

  // I = Items can compare themselves to an other
  TJclBinaryTreeI<T: IComparable<T>> = class(TJclBinaryTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclComparer<T> }
    function ItemsCompare(const A, B: T): Integer; override;
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
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


//=== { TIntfItr } ===========================================================

type
  TIntfItr = class(TJclAbstractIterator, IJclIntfIterator)
  protected
    FCursor: TJclIntfBinaryNode;
    FOwnList: IJclIntfCollection;
    FEqualityComparer: IJclIntfEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclIntfBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclIntfBinaryNode; virtual; abstract;
    { IJclIntfIterator }
    function Add(const AInterface: IInterface): Boolean;
    function GetObject: IInterface;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AInterface: IInterface): Boolean;
    function Next: IInterface;
    function NextIndex: Integer;
    function Previous: IInterface;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetObject(const AInterface: IInterface);
  public
    constructor Create(const OwnList: IJclIntfCollection; Start: TJclIntfBinaryNode; AValid: Boolean);
  end;

constructor TIntfItr.Create(const OwnList: IJclIntfCollection; Start: TJclIntfBinaryNode; AValid: Boolean);
begin
  inherited Create(OwnList, AValid);
  FCursor := Start;
  FOwnList := OwnList;
  FEqualityComparer := FOwnList as IJclIntfEqualityComparer;
end;

function TIntfItr.Add(const AInterface: IInterface): Boolean;
begin
  Result := FOwnList.Add(AInterface);
end;

procedure TIntfItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TIntfItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TIntfItr then
  begin
    ADest := TIntfItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TIntfItr.GetObject: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnList.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.Insert(const AInterface: IInterface): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TIntfItr.Next: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnList.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TIntfItr.Previous: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnList.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TIntfItr.Remove;
var
  OldCursor: TJclIntfBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnList.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnList.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntfItr.SetObject(const AInterface: IInterface);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderIntfItr } ===================================================

type
  TPreOrderIntfItr = class(TIntfItr, IJclIntfIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntfBinaryNode; override;
    function GetPreviousCursor: TJclIntfBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderIntfItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderIntfItr.Create(FOwnList, FCursor, Valid);
end;

function TPreOrderIntfItr.GetNextCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderIntfItr.GetPreviousCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderIntfItr } ====================================================

type
  TInOrderIntfItr = class(TIntfItr, IJclIntfIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntfBinaryNode; override;
    function GetPreviousCursor: TJclIntfBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderIntfItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderIntfItr.Create(FOwnList, FCursor, Valid);
end;

function TInOrderIntfItr.GetNextCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderIntfItr.GetPreviousCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderIntfItr } ==================================================

type
  TPostOrderIntfItr = class(TIntfItr, IJclIntfIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclIntfBinaryNode; override;
    function GetPreviousCursor: TJclIntfBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderIntfItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderIntfItr.Create(FOwnList, FCursor, Valid);
end;

function TPostOrderIntfItr.GetNextCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderIntfItr.GetPreviousCursor: TJclIntfBinaryNode;
var
  LastRet: TJclIntfBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TAnsiStrItr } ===========================================================

type
  TAnsiStrItr = class(TJclAbstractIterator, IJclAnsiStrIterator)
  protected
    FCursor: TJclAnsiStrBinaryNode;
    FOwnList: IJclAnsiStrCollection;
    FEqualityComparer: IJclAnsiStrEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclAnsiStrBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclAnsiStrBinaryNode; virtual; abstract;
    { IJclAnsiStrIterator }
    function Add(const AString: AnsiString): Boolean;
    function GetString: AnsiString;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AString: AnsiString): Boolean;
    function Next: AnsiString;
    function NextIndex: Integer;
    function Previous: AnsiString;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetString(const AString: AnsiString);
  public
    constructor Create(const OwnList: IJclAnsiStrCollection; Start: TJclAnsiStrBinaryNode; AValid: Boolean);
  end;

constructor TAnsiStrItr.Create(const OwnList: IJclAnsiStrCollection; Start: TJclAnsiStrBinaryNode; AValid: Boolean);
begin
  inherited Create(OwnList, AValid);
  FCursor := Start;
  FOwnList := OwnList;
  FEqualityComparer := FOwnList as IJclAnsiStrEqualityComparer;
end;

function TAnsiStrItr.Add(const AString: AnsiString): Boolean;
begin
  Result := FOwnList.Add(AString);
end;

procedure TAnsiStrItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TAnsiStrItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TAnsiStrItr then
  begin
    ADest := TAnsiStrItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TAnsiStrItr.GetString: AnsiString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnList.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.Insert(const AString: AnsiString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TAnsiStrItr.Next: AnsiString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnList.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TAnsiStrItr.Previous: AnsiString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnList.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TAnsiStrItr.Remove;
var
  OldCursor: TJclAnsiStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnList.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnList.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TAnsiStrItr.SetString(const AString: AnsiString);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderAnsiStrItr } ===================================================

type
  TPreOrderAnsiStrItr = class(TAnsiStrItr, IJclAnsiStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclAnsiStrBinaryNode; override;
    function GetPreviousCursor: TJclAnsiStrBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderAnsiStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderAnsiStrItr.Create(FOwnList, FCursor, Valid);
end;

function TPreOrderAnsiStrItr.GetNextCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderAnsiStrItr.GetPreviousCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderAnsiStrItr } ====================================================

type
  TInOrderAnsiStrItr = class(TAnsiStrItr, IJclAnsiStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclAnsiStrBinaryNode; override;
    function GetPreviousCursor: TJclAnsiStrBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderAnsiStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderAnsiStrItr.Create(FOwnList, FCursor, Valid);
end;

function TInOrderAnsiStrItr.GetNextCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderAnsiStrItr.GetPreviousCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderAnsiStrItr } ==================================================

type
  TPostOrderAnsiStrItr = class(TAnsiStrItr, IJclAnsiStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclAnsiStrBinaryNode; override;
    function GetPreviousCursor: TJclAnsiStrBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderAnsiStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderAnsiStrItr.Create(FOwnList, FCursor, Valid);
end;

function TPostOrderAnsiStrItr.GetNextCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderAnsiStrItr.GetPreviousCursor: TJclAnsiStrBinaryNode;
var
  LastRet: TJclAnsiStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TWideStrItr } ===========================================================

type
  TWideStrItr = class(TJclAbstractIterator, IJclWideStrIterator)
  protected
    FCursor: TJclWideStrBinaryNode;
    FOwnList: IJclWideStrCollection;
    FEqualityComparer: IJclWideStrEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclWideStrBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclWideStrBinaryNode; virtual; abstract;
    { IJclWideStrIterator }
    function Add(const AString: WideString): Boolean;
    function GetString: WideString;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AString: WideString): Boolean;
    function Next: WideString;
    function NextIndex: Integer;
    function Previous: WideString;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetString(const AString: WideString);
  public
    constructor Create(const OwnList: IJclWideStrCollection; Start: TJclWideStrBinaryNode; AValid: Boolean);
  end;

constructor TWideStrItr.Create(const OwnList: IJclWideStrCollection; Start: TJclWideStrBinaryNode; AValid: Boolean);
begin
  inherited Create(OwnList, AValid);
  FCursor := Start;
  FOwnList := OwnList;
  FEqualityComparer := FOwnList as IJclWideStrEqualityComparer;
end;

function TWideStrItr.Add(const AString: WideString): Boolean;
begin
  Result := FOwnList.Add(AString);
end;

procedure TWideStrItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TWideStrItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TWideStrItr then
  begin
    ADest := TWideStrItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TWideStrItr.GetString: WideString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnList.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.Insert(const AString: WideString): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TWideStrItr.Next: WideString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnList.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TWideStrItr.Previous: WideString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := '';
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnList.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TWideStrItr.Remove;
var
  OldCursor: TJclWideStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnList.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnList.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TWideStrItr.SetString(const AString: WideString);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderWideStrItr } ===================================================

type
  TPreOrderWideStrItr = class(TWideStrItr, IJclWideStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclWideStrBinaryNode; override;
    function GetPreviousCursor: TJclWideStrBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderWideStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderWideStrItr.Create(FOwnList, FCursor, Valid);
end;

function TPreOrderWideStrItr.GetNextCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderWideStrItr.GetPreviousCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderWideStrItr } ====================================================

type
  TInOrderWideStrItr = class(TWideStrItr, IJclWideStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclWideStrBinaryNode; override;
    function GetPreviousCursor: TJclWideStrBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderWideStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderWideStrItr.Create(FOwnList, FCursor, Valid);
end;

function TInOrderWideStrItr.GetNextCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderWideStrItr.GetPreviousCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderWideStrItr } ==================================================

type
  TPostOrderWideStrItr = class(TWideStrItr, IJclWideStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclWideStrBinaryNode; override;
    function GetPreviousCursor: TJclWideStrBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderWideStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderWideStrItr.Create(FOwnList, FCursor, Valid);
end;

function TPostOrderWideStrItr.GetNextCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderWideStrItr.GetPreviousCursor: TJclWideStrBinaryNode;
var
  LastRet: TJclWideStrBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;

//=== { TItr } ===========================================================

type
  TItr = class(TJclAbstractIterator, IJclIterator)
  protected
    FCursor: TJclBinaryNode;
    FOwnList: IJclCollection;
    FEqualityComparer: IJclEqualityComparer;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclBinaryNode; virtual; abstract;
    { IJclIterator }
    function Add(AObject: TObject): Boolean;
    function GetObject: TObject;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(AObject: TObject): Boolean;
    function Next: TObject;
    function NextIndex: Integer;
    function Previous: TObject;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetObject(AObject: TObject);
  public
    constructor Create(const OwnList: IJclCollection; Start: TJclBinaryNode; AValid: Boolean);
  end;

constructor TItr.Create(const OwnList: IJclCollection; Start: TJclBinaryNode; AValid: Boolean);
begin
  inherited Create(OwnList, AValid);
  FCursor := Start;
  FOwnList := OwnList;
  FEqualityComparer := FOwnList as IJclEqualityComparer;
end;

function TItr.Add(AObject: TObject): Boolean;
begin
  Result := FOwnList.Add(AObject);
end;

procedure TItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TItr then
  begin
    ADest := TItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TItr.GetObject: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnList.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.Insert(AObject: TObject): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TItr.Next: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnList.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TItr.Previous: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := nil;
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnList.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TItr.Remove;
var
  OldCursor: TJclBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnList.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnList.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr.SetObject(AObject: TObject);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderItr } ===================================================

type
  TPreOrderItr = class(TItr, IJclIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode; override;
    function GetPreviousCursor: TJclBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderItr.Create(FOwnList, FCursor, Valid);
end;

function TPreOrderItr.GetNextCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderItr.GetPreviousCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderItr } ====================================================

type
  TInOrderItr = class(TItr, IJclIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode; override;
    function GetPreviousCursor: TJclBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderItr.Create(FOwnList, FCursor, Valid);
end;

function TInOrderItr.GetNextCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderItr.GetPreviousCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderItr } ==================================================

type
  TPostOrderItr = class(TItr, IJclIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode; override;
    function GetPreviousCursor: TJclBinaryNode; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderItr.Create(FOwnList, FCursor, Valid);
end;

function TPostOrderItr.GetNextCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderItr.GetPreviousCursor: TJclBinaryNode;
var
  LastRet: TJclBinaryNode;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;
{$IFDEF SUPPORTS_GENERICS}

//=== { TItr<T> } ===========================================================

type
  TItr<T> = class(TJclAbstractIterator, IJclIterator<T>)
  protected
    FCursor: TJclBinaryNode<T>;
    FOwnList: IJclCollection<T>;
    FEqualityComparer: IJclEqualityComparer<T>;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function GetNextCursor: TJclBinaryNode<T>; virtual; abstract;
    function GetPreviousCursor: TJclBinaryNode<T>; virtual; abstract;
    { IJclIterator<T> }
    function Add(const AItem: T): Boolean;
    function GetItem: T;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AItem: T): Boolean;
    function Next: T;
    function NextIndex: Integer;
    function Previous: T;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetItem(const AItem: T);
  public
    constructor Create(const OwnList: IJclCollection<T>; Start: TJclBinaryNode<T>; AValid: Boolean);
  end;

constructor TItr<T>.Create(const OwnList: IJclCollection<T>; Start: TJclBinaryNode<T>; AValid: Boolean);
begin
  inherited Create(OwnList, AValid);
  FCursor := Start;
  FOwnList := OwnList;
  FEqualityComparer := FOwnList as IJclEqualityComparer<T>;
end;

function TItr<T>.Add(const AItem: T): Boolean;
begin
  Result := FOwnList.Add(AItem);
end;

procedure TItr<T>.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TItr<T>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TItr<T> then
  begin
    ADest := TItr<T>(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TItr<T>.GetItem: T;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := Default(T);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnList.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetNextCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := GetPreviousCursor <> nil
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.Insert(const AItem: T): Boolean;
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TItr<T>.Next: T;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    Result := Default(T);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnList.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TItr<T>.Previous: T;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    Result := Default(T);
    if FCursor <> nil then
      Result := FCursor.Value
    else
    if not FOwnList.ReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TItr<T>.Remove;
var
  OldCursor: TJclBinaryNode<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    OldCursor := FCursor;
    if OldCursor <> nil then
    begin
      repeat
        FCursor := GetNextCursor;
      until (FCursor = nil) or FOwnList.RemoveSingleElement
        or (not FEqualityComparer.ItemsEqual(OldCursor.Value, FCursor.Value));
      FOwnList.Remove(OldCursor.Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr<T>.SetItem(const AItem: T);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderItr<T> } ===================================================

type
  TPreOrderItr<T> = class(TItr<T>, IJclIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode<T>; override;
    function GetPreviousCursor: TJclBinaryNode<T>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderItr<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPreOrderItr<T>.Create(FOwnList, FCursor, Valid);
end;

function TPreOrderItr<T>.GetNextCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  if Result.Left <> nil then
    Result := Result.Left
  else
  if Result.Right <> nil then
    Result := Result.Right
  else
  begin
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Right = nil) or (Result.Right = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Right;
  end;
end;

function TPreOrderItr<T>.GetPreviousCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Left <> LastRet) and (Result.Left <> nil)  then
    // come from Right
  begin
    Result := Result.Left;
    while (Result.Left <> nil) or (Result.Right <> nil) do // both childs
    begin
      if Result.Right <> nil then // right child first
        Result := Result.Right
      else
        Result := Result.Left;
    end;
  end;
end;

//=== { TInOrderItr<T> } ====================================================

type
  TInOrderItr<T> = class(TItr<T>, IJclIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode<T>; override;
    function GetPreviousCursor: TJclBinaryNode<T>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderItr<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TInOrderItr<T>.Create(FOwnList, FCursor, Valid);
end;

function TInOrderItr<T>.GetNextCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) do
      Result := Result.Left;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right = LastRet) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

function TInOrderItr<T>.GetPreviousCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Left <> nil then
  begin
    Result := Result.Left;
    while Result.Right <> nil do
      Result := Result.Right;
  end
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and (Result.Right <> LastRet) do // Come from Left
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
  end;
end;

//=== { TPostOrderItr<T> } ==================================================

type
  TPostOrderItr<T> = class(TItr<T>, IJclIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    function GetNextCursor: TJclBinaryNode<T>; override;
    function GetPreviousCursor: TJclBinaryNode<T>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderItr<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TPostOrderItr<T>.Create(FOwnList, FCursor, Valid);
end;

function TPostOrderItr<T>.GetNextCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  LastRet := Result;
  Result := Result.Parent;
  if (Result <> nil) and (Result.Right <> nil) and (Result.Right <> LastRet) then
  begin
    Result := Result.Right;
    while (Result.Left <> nil) or (Result.Right <> nil) do
    begin
      if Result.Left <> nil then
        Result := Result.Left
      else
        Result := Result.Right;
    end;
  end;
end;

function TPostOrderItr<T>.GetPreviousCursor: TJclBinaryNode<T>;
var
  LastRet: TJclBinaryNode<T>;
begin
  Result := FCursor;
  if Result = nil then
    Exit;
  if Result.Right <> nil then
    Result := Result.Right
  else
  if Result.Left <> nil then
    Result := Result.Left
  else
  begin
    LastRet := Result;
    Result := Result.Parent;
    while (Result <> nil) and ((Result.Left = nil) or (Result.Left = LastRet)) do
    begin
      LastRet := Result;
      Result := Result.Parent;
    end;
    if Result <> nil then // not root
      Result := Result.Left;
  end;
end;
{$ENDIF SUPPORTS_GENERICS}


//=== { TJclIntfBinaryTree } =================================================

constructor TJclIntfBinaryTree.Create(ACompare: TIntfCompare);
begin
  inherited Create(nil);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  FCompare := ACompare;
end;

destructor TJclIntfBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclIntfBinaryTree.Add(const AInterface: IInterface): Boolean;
var
  NewNode, Current, Save: TJclIntfBinaryNode;
  Comp, Depth: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AInterface, nil) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AInterface, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclIntfBinaryNode.Create;
        NewNode.Value := AInterface;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) < 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.AddAll(const ACollection: IJclIntfCollection): Boolean;
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

procedure TJclIntfBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclIntfBinaryNode): TJclIntfBinaryNode;
  begin
    Result := TJclIntfBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclIntfBinaryTree;
  ACollection: IJclIntfCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfBinaryTree then
  begin
    ADest := TJclIntfBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclIntfCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclIntfBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfBinaryTree then
    TJclIntfBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclIntfBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclIntfBinaryTree.Clear;
var
  Current, Parent: TJclIntfBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeObject(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.Contains(const AInterface: IInterface): Boolean;
var
  Comp: Integer;
  Current: TJclIntfBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AInterface);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.ContainsAll(const ACollection: IJclIntfCollection): Boolean;
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
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;

function TJclIntfBinaryTree.Equals(const ACollection: IJclIntfCollection): Boolean;
var
  It, ItSelf: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.First: IJclIntfIterator;
var
  Start: TJclIntfBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderIntfItr.Create(Self, Start, False);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TInOrderIntfItr.Create(Self, Start, False);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TPostOrderIntfItr.Create(Self, Start, False);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclIntfBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfBinaryTree.ItemsCompare(const A, B: IInterface): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclIntfBinaryTree.ItemsEqual(const A, B: IInterface): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

function TJclIntfBinaryTree.Last: IJclIntfIterator;
var
  Start: TJclIntfBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TPreOrderIntfItr.Create(Self, Start, False);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TInOrderIntfItr.Create(Self, Start, False);
        end;
      toPostOrder:
        Result := TPostOrderIntfItr.Create(Self, Start, False);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfBinaryTree.Pack;
type
  TLeafArray = array of TJclIntfBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclIntfBinaryNode;
    Offset: Integer): TJclIntfBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclIntfBinaryNode;
  Index: Integer;
begin
  SetLength(Leafarray, FSize);
  try
    // in order enumeration of nodes
    ANode := FRoot;
    if ANode <> nil then
    begin
      // find first node
      while ANode.Left <> nil do
        ANode := ANode.Left;

      Index := 0;
      while ANode <> nil do
      begin
        LeafArray[Index] := ANode;
        Inc(Index);
        if ANode.Right <> nil then
        begin
          ANode := ANode.Right;
          while (ANode.Left <> nil) do
            ANode := ANode.Left;
        end
        else
        begin
          BNode := ANode;
          ANode := ANode.Parent;
          while (ANode <> nil) and (ANode.Right = BNode) do
          begin
            BNode := ANode;
            ANode := ANode.Parent;
          end;
        end;
      end;

      Index := FSize shr 1;
      FRoot := LeafArray[Index];
      FRoot.Parent := nil;
      if Index > 0 then
        FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
      else
        FRoot.Left := nil;
      if Index < (FSize - 1) then
        FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
      else
        FRoot.Right := nil;
    end;
  finally
    SetLength(LeafArray, 0);
  end;
end;

function TJclIntfBinaryTree.Remove(const AInterface: IInterface): Boolean;
var
  Current, Successor: TJclIntfBinaryNode;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AInterface in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AInterface, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          if Successor.Parent.Left = Successor then
            Successor.Parent.Left := Successor.Right
          else
            Successor.Parent.Right := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
        end;

        // insert successor in new position
        Successor.Parent := Current.Parent;
        Successor.Left := Current.Left;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      FreeObject(Current.Value);
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.RemoveAll(const ACollection: IJclIntfCollection): Boolean;
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
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.RetainAll(const ACollection: IJclIntfCollection): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclIntfBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclIntfBinaryTree.Size: Integer;
begin
  Result := FSize;
end;


//=== { TJclAnsiStrBinaryTree } =================================================

constructor TJclAnsiStrBinaryTree.Create(ACompare: TAnsiStrCompare);
begin
  inherited Create(nil);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  FCompare := ACompare;
end;

destructor TJclAnsiStrBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclAnsiStrBinaryTree.Add(const AString: AnsiString): Boolean;
var
  NewNode, Current, Save: TJclAnsiStrBinaryNode;
  Comp, Depth: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AString, '') then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AString, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclAnsiStrBinaryNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) < 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.AddAll(const ACollection: IJclAnsiStrCollection): Boolean;
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

procedure TJclAnsiStrBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclAnsiStrBinaryNode): TJclAnsiStrBinaryNode;
  begin
    Result := TJclAnsiStrBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclAnsiStrBinaryTree;
  ACollection: IJclAnsiStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrBinaryTree then
  begin
    ADest := TJclAnsiStrBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclAnsiStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclAnsiStrBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclAnsiStrBinaryTree then
    TJclAnsiStrBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclAnsiStrBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclAnsiStrBinaryTree.Clear;
var
  Current, Parent: TJclAnsiStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeString(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.Contains(const AString: AnsiString): Boolean;
var
  Comp: Integer;
  Current: TJclAnsiStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AString);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
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
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrBinaryTree.Equals(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It, ItSelf: IJclAnsiStrIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.First: IJclAnsiStrIterator;
var
  Start: TJclAnsiStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderAnsiStrItr.Create(Self, Start, False);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TInOrderAnsiStrItr.Create(Self, Start, False);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TPostOrderAnsiStrItr.Create(Self, Start, False);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclAnsiStrBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrBinaryTree.ItemsCompare(const A, B: AnsiString): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclAnsiStrBinaryTree.ItemsEqual(const A, B: AnsiString): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

function TJclAnsiStrBinaryTree.Last: IJclAnsiStrIterator;
var
  Start: TJclAnsiStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TPreOrderAnsiStrItr.Create(Self, Start, False);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TInOrderAnsiStrItr.Create(Self, Start, False);
        end;
      toPostOrder:
        Result := TPostOrderAnsiStrItr.Create(Self, Start, False);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrBinaryTree.Pack;
type
  TLeafArray = array of TJclAnsiStrBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclAnsiStrBinaryNode;
    Offset: Integer): TJclAnsiStrBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclAnsiStrBinaryNode;
  Index: Integer;
begin
  SetLength(Leafarray, FSize);
  try
    // in order enumeration of nodes
    ANode := FRoot;
    if ANode <> nil then
    begin
      // find first node
      while ANode.Left <> nil do
        ANode := ANode.Left;

      Index := 0;
      while ANode <> nil do
      begin
        LeafArray[Index] := ANode;
        Inc(Index);
        if ANode.Right <> nil then
        begin
          ANode := ANode.Right;
          while (ANode.Left <> nil) do
            ANode := ANode.Left;
        end
        else
        begin
          BNode := ANode;
          ANode := ANode.Parent;
          while (ANode <> nil) and (ANode.Right = BNode) do
          begin
            BNode := ANode;
            ANode := ANode.Parent;
          end;
        end;
      end;

      Index := FSize shr 1;
      FRoot := LeafArray[Index];
      FRoot.Parent := nil;
      if Index > 0 then
        FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
      else
        FRoot.Left := nil;
      if Index < (FSize - 1) then
        FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
      else
        FRoot.Right := nil;
    end;
  finally
    SetLength(LeafArray, 0);
  end;
end;

function TJclAnsiStrBinaryTree.Remove(const AString: AnsiString): Boolean;
var
  Current, Successor: TJclAnsiStrBinaryNode;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AString in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AString, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          if Successor.Parent.Left = Successor then
            Successor.Parent.Left := Successor.Right
          else
            Successor.Parent.Right := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
        end;

        // insert successor in new position
        Successor.Parent := Current.Parent;
        Successor.Left := Current.Left;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      FreeString(Current.Value);
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean;
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
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrBinaryTree.RetainAll(const ACollection: IJclAnsiStrCollection): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclAnsiStrBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclAnsiStrBinaryTree.Size: Integer;
begin
  Result := FSize;
end;


//=== { TJclWideStrBinaryTree } =================================================

constructor TJclWideStrBinaryTree.Create(ACompare: TWideStrCompare);
begin
  inherited Create(nil);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  FCompare := ACompare;
end;

destructor TJclWideStrBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclWideStrBinaryTree.Add(const AString: WideString): Boolean;
var
  NewNode, Current, Save: TJclWideStrBinaryNode;
  Comp, Depth: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AString, '') then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AString, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclWideStrBinaryNode.Create;
        NewNode.Value := AString;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) < 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.AddAll(const ACollection: IJclWideStrCollection): Boolean;
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

procedure TJclWideStrBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclWideStrBinaryNode): TJclWideStrBinaryNode;
  begin
    Result := TJclWideStrBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclWideStrBinaryTree;
  ACollection: IJclWideStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrBinaryTree then
  begin
    ADest := TJclWideStrBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclWideStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclWideStrBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclWideStrBinaryTree then
    TJclWideStrBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclWideStrBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclWideStrBinaryTree.Clear;
var
  Current, Parent: TJclWideStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeString(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.Contains(const AString: WideString): Boolean;
var
  Comp: Integer;
  Current: TJclWideStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AString);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.ContainsAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
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
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;

function TJclWideStrBinaryTree.Equals(const ACollection: IJclWideStrCollection): Boolean;
var
  It, ItSelf: IJclWideStrIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.First: IJclWideStrIterator;
var
  Start: TJclWideStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderWideStrItr.Create(Self, Start, False);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TInOrderWideStrItr.Create(Self, Start, False);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TPostOrderWideStrItr.Create(Self, Start, False);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclWideStrBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrBinaryTree.ItemsCompare(const A, B: WideString): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclWideStrBinaryTree.ItemsEqual(const A, B: WideString): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

function TJclWideStrBinaryTree.Last: IJclWideStrIterator;
var
  Start: TJclWideStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TPreOrderWideStrItr.Create(Self, Start, False);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TInOrderWideStrItr.Create(Self, Start, False);
        end;
      toPostOrder:
        Result := TPostOrderWideStrItr.Create(Self, Start, False);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrBinaryTree.Pack;
type
  TLeafArray = array of TJclWideStrBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclWideStrBinaryNode;
    Offset: Integer): TJclWideStrBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclWideStrBinaryNode;
  Index: Integer;
begin
  SetLength(Leafarray, FSize);
  try
    // in order enumeration of nodes
    ANode := FRoot;
    if ANode <> nil then
    begin
      // find first node
      while ANode.Left <> nil do
        ANode := ANode.Left;

      Index := 0;
      while ANode <> nil do
      begin
        LeafArray[Index] := ANode;
        Inc(Index);
        if ANode.Right <> nil then
        begin
          ANode := ANode.Right;
          while (ANode.Left <> nil) do
            ANode := ANode.Left;
        end
        else
        begin
          BNode := ANode;
          ANode := ANode.Parent;
          while (ANode <> nil) and (ANode.Right = BNode) do
          begin
            BNode := ANode;
            ANode := ANode.Parent;
          end;
        end;
      end;

      Index := FSize shr 1;
      FRoot := LeafArray[Index];
      FRoot.Parent := nil;
      if Index > 0 then
        FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
      else
        FRoot.Left := nil;
      if Index < (FSize - 1) then
        FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
      else
        FRoot.Right := nil;
    end;
  finally
    SetLength(LeafArray, 0);
  end;
end;

function TJclWideStrBinaryTree.Remove(const AString: WideString): Boolean;
var
  Current, Successor: TJclWideStrBinaryNode;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AString in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AString, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          if Successor.Parent.Left = Successor then
            Successor.Parent.Left := Successor.Right
          else
            Successor.Parent.Right := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
        end;

        // insert successor in new position
        Successor.Parent := Current.Parent;
        Successor.Left := Current.Left;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      FreeString(Current.Value);
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.RemoveAll(const ACollection: IJclWideStrCollection): Boolean;
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
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrBinaryTree.RetainAll(const ACollection: IJclWideStrCollection): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclWideStrBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclWideStrBinaryTree.Size: Integer;
begin
  Result := FSize;
end;


//=== { TJclBinaryTree } =================================================

constructor TJclBinaryTree.Create(ACompare: TCompare; AOwnsObjects: Boolean);
begin
  inherited Create(nil, AOwnsObjects);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
  FCompare := ACompare;
end;

destructor TJclBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclBinaryTree.Add(AObject: TObject): Boolean;
var
  NewNode, Current, Save: TJclBinaryNode;
  Comp, Depth: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AObject, nil) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AObject, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclBinaryNode.Create;
        NewNode.Value := AObject;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) < 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.AddAll(const ACollection: IJclCollection): Boolean;
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

procedure TJclBinaryTree.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclBinaryNode): TJclBinaryNode;
  begin
    Result := TJclBinaryNode.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclBinaryTree;
  ACollection: IJclCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclBinaryTree then
  begin
    ADest := TJclBinaryTree(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclBinaryTree.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclBinaryTree then
    TJclBinaryTree(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclBinaryTree.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclBinaryTree.Clear;
var
  Current, Parent: TJclBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeObject(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.Contains(AObject: TObject): Boolean;
var
  Comp: Integer;
  Current: TJclBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AObject);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.ContainsAll(const ACollection: IJclCollection): Boolean;
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
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBinaryTree.Create(FCompare, False);
  AssignPropertiesTo(Result);
end;

function TJclBinaryTree.Equals(const ACollection: IJclCollection): Boolean;
var
  It, ItSelf: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.First: IJclIterator;
var
  Start: TJclBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderItr.Create(Self, Start, False);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TInOrderItr.Create(Self, Start, False);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TPostOrderItr.Create(Self, Start, False);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclBinaryTree.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclBinaryTree.ItemsCompare(A, B: TObject): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclBinaryTree.ItemsEqual(A, B: TObject): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

function TJclBinaryTree.Last: IJclIterator;
var
  Start: TJclBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TPreOrderItr.Create(Self, Start, False);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TInOrderItr.Create(Self, Start, False);
        end;
      toPostOrder:
        Result := TPostOrderItr.Create(Self, Start, False);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTree.Pack;
type
  TLeafArray = array of TJclBinaryNode;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclBinaryNode;
    Offset: Integer): TJclBinaryNode;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclBinaryNode;
  Index: Integer;
begin
  SetLength(Leafarray, FSize);
  try
    // in order enumeration of nodes
    ANode := FRoot;
    if ANode <> nil then
    begin
      // find first node
      while ANode.Left <> nil do
        ANode := ANode.Left;

      Index := 0;
      while ANode <> nil do
      begin
        LeafArray[Index] := ANode;
        Inc(Index);
        if ANode.Right <> nil then
        begin
          ANode := ANode.Right;
          while (ANode.Left <> nil) do
            ANode := ANode.Left;
        end
        else
        begin
          BNode := ANode;
          ANode := ANode.Parent;
          while (ANode <> nil) and (ANode.Right = BNode) do
          begin
            BNode := ANode;
            ANode := ANode.Parent;
          end;
        end;
      end;

      Index := FSize shr 1;
      FRoot := LeafArray[Index];
      FRoot.Parent := nil;
      if Index > 0 then
        FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
      else
        FRoot.Left := nil;
      if Index < (FSize - 1) then
        FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
      else
        FRoot.Right := nil;
    end;
  finally
    SetLength(LeafArray, 0);
  end;
end;

function TJclBinaryTree.Remove(AObject: TObject): Boolean;
var
  Current, Successor: TJclBinaryNode;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AObject in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AObject, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          if Successor.Parent.Left = Successor then
            Successor.Parent.Left := Successor.Right
          else
            Successor.Parent.Right := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
        end;

        // insert successor in new position
        Successor.Parent := Current.Parent;
        Successor.Left := Current.Left;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      FreeObject(Current.Value);
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.RemoveAll(const ACollection: IJclCollection): Boolean;
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
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.RetainAll(const ACollection: IJclCollection): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTree.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclBinaryTree.Size: Integer;
begin
  Result := FSize;
end;

{$IFDEF SUPPORTS_GENERICS}


//=== { TJclBinaryTree<T> } =================================================

constructor TJclBinaryTree<T>.Create(AOwnsItems: Boolean);
begin
  inherited Create(nil, AOwnsItems);
  FTraverseOrder := toOrder;
  FMaxDepth := 0;
  FAutoPackParameter := 2;
end;

destructor TJclBinaryTree<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclBinaryTree<T>.Add(const AItem: T): Boolean;
var
  NewNode, Current, Save: TJclBinaryNode<T>;
  Comp, Depth: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // Insert into right place
    if FAllowDefaultElements or not ItemsEqual(AItem, Default(T)) then
    begin
      Save := nil;
      Current := FRoot;
      Comp := 1;
      Depth := 0;
      while Current <> nil do
      begin
        Inc(Depth);
        Save := Current;
        Comp := ItemsCompare(AItem, Current.Value);
        if Comp < 0 then
          Current := Current.Left
        else
        if Comp > 0 then
          Current := Current.Right
        else
          Break;
      end;
      if (Comp <> 0) or CheckDuplicate then
      begin
        NewNode := TJclBinaryNode<T>.Create;
        NewNode.Value := AItem;
        NewNode.Parent := Save;
        if Save = nil then
          FRoot := NewNode
        else
        if ItemsCompare(NewNode.Value, Save.Value) < 0 then
          Save.Left := NewNode
        else
          Save.Right := NewNode;
        Inc(FSize);
        Inc(Depth);
        if Depth > FMaxDepth then
          FMaxDepth := Depth;
        Result := True;
        AutoPack;
      end
      else
        Result := False;
    end
    else
      Result := False;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.AddAll(const ACollection: IJclCollection<T>): Boolean;
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

procedure TJclBinaryTree<T>.AssignDataTo(Dest: TJclAbstractContainerBase);
  function CloneNode(Node, Parent: TJclBinaryNode<T>): TJclBinaryNode<T>;
  begin
    Result := TJclBinaryNode<T>.Create;
    Result.Value := Node.Value;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;
var
  ADest: TJclBinaryTree<T>;
  ACollection: IJclCollection<T>;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclBinaryTree<T> then
  begin
    ADest := TJclBinaryTree<T>(Dest);
    ADest.Clear;
    ADest.FSize := FSize;
    if FRoot <> nil then
      ADest.FRoot := CloneNode(FRoot, nil);
  end
  else
  if Supports(IInterface(Dest), IJclCollection<T>, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclBinaryTree<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclBinaryTree<T> then
    TJclBinaryTree<T>(Dest).FTraverseOrder := FTraverseOrder;
end;

procedure TJclBinaryTree<T>.AutoPack;
begin
  case FAutoPackStrategy of
    //apsDisabled: ;
    apsAgressive:
      if (FMaxDepth > 1) and (((1 shl (FMaxDepth - 1)) - 1) > FSize) then
        Pack;
    // apsIncremental: ;
    apsProportional:
      if (FMaxDepth > FAutoPackParameter) and (((1 shl (FMaxDepth - FAutoPackParameter)) - 1) > FSize) then
        Pack;
  end;
end;

procedure TJclBinaryTree<T>.Clear;
var
  Current, Parent: TJclBinaryNode<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first in post-order
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree in post-order
    repeat
      Parent := Current.Parent;
      // remove reference
      if Parent <> nil then
      begin
        if Parent.Left = Current then
          Parent.Left := nil
        else
        if Parent.Right = Current then
          Parent.Right := nil;
      end;

      // free item
      FreeItem(Current.Value);
      Current.Free;

      // find next item
      Current := Parent;
      if (Current <> nil) and (Current.Right <> nil) then
      begin
        Current := Current.Right;
        while (Current.Left <> nil) or (Current.Right <> nil) do
        begin
          if Current.Left <> nil then
            Current := Current.Left
          else
            Current := Current.Right;
        end;
      end;
    until Current = nil;
    FRoot := nil;
    FSize := 0;
    FMaxDepth := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.Contains(const AItem: T): Boolean;
var
  Comp: Integer;
  Current: TJclBinaryNode<T>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := ItemsCompare(Current.Value, AItem);
      if Comp = 0 then
      begin
        Result := True;
        Break;
      end
      else
      if Comp > 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.ContainsAll(const ACollection: IJclCollection<T>): Boolean;
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
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;


function TJclBinaryTree<T>.Equals(const ACollection: IJclCollection<T>): Boolean;
var
  It, ItSelf: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
      begin
        Result := False;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.First: IJclIterator<T>;
var
  Start: TJclBinaryNode<T>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderItr<T>.Create(Self, Start, False);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TInOrderItr<T>.Create(Self, Start, False);
        end;
      toPostOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Left <> nil then
              Start := Start.Left
            else
              Start := Start.Right;
          end;
          Result := TPostOrderItr<T>.Create(Self, Start, False);
        end;
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclBinaryTree<T>.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;



function TJclBinaryTree<T>.Last: IJclIterator<T>;
var
  Start: TJclBinaryNode<T>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case FTraverseOrder of
      toPreOrder:
        begin
          if Start <> nil then
            while (Start.Left <> nil) or (Start.Right <> nil) do
          begin
            if Start.Right <> nil then
              Start := Start.Right
            else
              Start := Start.Left;
          end;
          Result := TPreOrderItr<T>.Create(Self, Start, False);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TInOrderItr<T>.Create(Self, Start, False);
        end;
      toPostOrder:
        Result := TPostOrderItr<T>.Create(Self, Start, False);
    else
      Result := nil;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTree<T>.Pack;
type
  TLeafArray = array of TJclBinaryNode<T>;

  function BuildTree(var LeafArray: TLeafArray; Left, Right: Integer; Parent: TJclBinaryNode<T>;
    Offset: Integer): TJclBinaryNode<T>;
  var
    Middle: Integer;
  begin
    Middle := (Left + Right + Offset) shr 1;
    Result := LeafArray[Middle];
    Result.Parent := Parent;
    if Middle > Left then
      Result.Left := BuildTree(LeafArray, Left, Middle - 1, Result, 0)
    else
      Result.Left := nil;
    if Middle < Right then
      Result.Right := BuildTree(LeafArray, Middle + 1, Right, Result, 1)
    else
      Result.Right := nil;
  end;
var
  LeafArray: TLeafArray;
  ANode, BNode: TJclBinaryNode<T>;
  Index: Integer;
begin
  SetLength(Leafarray, FSize);
  try
    // in order enumeration of nodes
    ANode := FRoot;
    if ANode <> nil then
    begin
      // find first node
      while ANode.Left <> nil do
        ANode := ANode.Left;

      Index := 0;
      while ANode <> nil do
      begin
        LeafArray[Index] := ANode;
        Inc(Index);
        if ANode.Right <> nil then
        begin
          ANode := ANode.Right;
          while (ANode.Left <> nil) do
            ANode := ANode.Left;
        end
        else
        begin
          BNode := ANode;
          ANode := ANode.Parent;
          while (ANode <> nil) and (ANode.Right = BNode) do
          begin
            BNode := ANode;
            ANode := ANode.Parent;
          end;
        end;
      end;

      Index := FSize shr 1;
      FRoot := LeafArray[Index];
      FRoot.Parent := nil;
      if Index > 0 then
        FRoot.Left := BuildTree(LeafArray, 0, Index - 1, FRoot, 0)
      else
        FRoot.Left := nil;
      if Index < (FSize - 1) then
        FRoot.Right := BuildTree(LeafArray, Index + 1, FSize - 1, FRoot, 1)
      else
        FRoot.Right := nil;
    end;
  finally
    SetLength(LeafArray, 0);
  end;
end;

function TJclBinaryTree<T>.Remove(const AItem: T): Boolean;
var
  Current, Successor: TJclBinaryNode<T>;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AItem in the tree
    Current := FRoot;
    repeat
      while Current <> nil do
      begin
        Comp := ItemsCompare(AItem, Current.Value);
        if Comp = 0 then
          Break
        else
        if Comp < 0 then
         Current := Current.Left
        else
          Current := Current.Right;
      end;
      if Current = nil then
        Break;
      Result := True;
      // Remove Current from tree
      if (Current.Left = nil) and (Current.Right <> nil) then
      begin
        // remove references to Current
        Current.Right.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Right
          else
            Current.Parent.Right := Current.Right;
        end
        else
          // fix root
          FRoot := Current.Right;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right = nil) then
      begin
        // remove references to Current
        Current.Left.Parent := Current.Parent;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Current.Left
          else
            Current.Parent.Right := Current.Left;
        end
        else
          // fix root
          FRoot := Current.Left;
        Successor := Current.Parent;
        if Successor = nil then
          Successor := FRoot;
      end
      else
      if (Current.Left <> nil) and (Current.Right <> nil) then
      begin
        // find the successor in tree
        Successor := Current.Right;
        while Successor.Left <> nil do
          Successor := Successor.Left;

        if Successor <> Current.Right then
        begin
          // remove references to successor
          if Successor.Parent.Left = Successor then
            Successor.Parent.Left := Successor.Right
          else
            Successor.Parent.Right := Successor.Right;
          if Successor.Right <> nil then
            Successor.Right.Parent := Successor.Parent;
          Successor.Right := Current.Right;
        end;

        // insert successor in new position
        Successor.Parent := Current.Parent;
        Successor.Left := Current.Left;
        if Current.Parent <> nil then
        begin
          if Current.Parent.Left = Current then
            Current.Parent.Left := Successor
          else
            Current.Parent.Right := Successor;
        end
        else
          // fix root
          FRoot := Successor;
        Successor := Current.Parent;
        if Successor <> nil then
          Successor := FRoot;
      end
      else
      begin
        // (Current.Left = nil) and (Current.Right = nil)
        Successor := Current.Parent;
        if Successor <> nil then
        begin
          // remove references from parent
          if Successor.Left = Current then
            Successor.Left := nil
          else
            Successor.Right := nil;
        end
        else
          FRoot := nil;
      end;
      FreeItem(Current.Value);
      Current.Free;
      Dec(FSize);
      Current := Successor;
    until FRemoveSingleElement or (Current = nil);
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.RemoveAll(const ACollection: IJclCollection<T>): Boolean;
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
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.RetainAll(const ACollection: IJclCollection<T>): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTree<T>.SetCapacity(Value: Integer);
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TJclBinaryTree<T>.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclBinaryTree<T>.Size: Integer;
begin
  Result := FSize;
end;

//=== { TJclBinaryTreeE<T> } =================================================

constructor TJclBinaryTreeE<T>.Create(const AComparer: IComparer<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FComparer := AComparer;
end;

procedure TJclBinaryTreeE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclBinaryTreeE<T> then
    TJclBinaryTreeE<T>(Dest).FComparer := FComparer;
end;

function TJclBinaryTreeE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBinaryTreeE<T>.Create(Comparer, False);
  AssignPropertiesTo(Result);
end;

function TJclBinaryTreeE<T>.ItemsCompare(const A, B: T): Integer;
begin
  if Comparer = nil then
    raise EJclNoComparerError.Create;
  Result := Comparer.Compare(A, B);
end;

function TJclBinaryTreeE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Comparer = nil then
    raise EJclNoComparerError.Create;
  Result := Comparer.Compare(A, B) = 0;
end;

//=== { TJclBinaryTreeF<T> } =================================================

constructor TJclBinaryTreeF<T>.Create(ACompare: TCompare<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FCompare := ACompare;
end;

procedure TJclBinaryTreeF<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclBinaryTreeF<T> then
    TJclBinaryTreeF<T>(Dest).FCompare := FCompare;
end;

function TJclBinaryTreeF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBinaryTreeF<T>.Create(Compare, False);
  AssignPropertiesTo(Result);
end;

function TJclBinaryTreeF<T>.ItemsCompare(const A, B: T): Integer;
begin
  if not Assigned(Compare) then
    raise EJclNoComparerError.Create;
  Result := Compare(A, B);
end;

function TJclBinaryTreeF<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if not Assigned(Compare) then
    raise EJclNoComparerError.Create;
  Result := Compare(A, B) = 0;
end;

//=== { TJclBinaryTreeI<T> } =================================================

function TJclBinaryTreeI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBinaryTreeI<T>.Create(False);
  AssignPropertiesTo(Result);
end;

function TJclBinaryTreeI<T>.ItemsCompare(const A, B: T): Integer;
begin
  Result := A.CompareTo(B);
end;

function TJclBinaryTreeI<T>.ItemsEqual(const A, B: T): Boolean;
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

