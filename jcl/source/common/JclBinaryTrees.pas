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
  TJclTreeColor = (tcBlack, tcRed);

  TJclIntfBinaryNode = class
  public
    Obj: IInterface;
    Left: TJclIntfBinaryNode;
    Right: TJclIntfBinaryNode;
    Parent: TJclIntfBinaryNode;
    Color: TJclTreeColor;
  end;

  TJclStrBinaryNode = class
  public
    Str: string;
    Left: TJclStrBinaryNode;
    Right: TJclStrBinaryNode;
    Parent: TJclStrBinaryNode;
    Color: TJclTreeColor;
  end;

  TJclBinaryNode = class
  public
    Obj: TObject;
    Left: TJclBinaryNode;
    Right: TJclBinaryNode;
    Parent: TJclBinaryNode;
    Color: TJclTreeColor;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  TJclBinaryNode<T> = class
  public
    Obj: T;
    Left: TJclBinaryNode<T>;
    Right: TJclBinaryNode<T>;
    Parent: TJclBinaryNode<T>;
    Color: TJclTreeColor;
  end;
  {$ENDIF SUPPORTS_GENERICS}

  TJclIntfBinaryTree = class(TJclAbstractContainer, IJclIntfCollection, IJclIntfTree,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  private
    FComparator: TIntfCompare;
    FCount: Integer;
    FRoot: TJclIntfBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    procedure RotateLeft(Node: TJclIntfBinaryNode);
    procedure RotateRight(Node: TJclIntfBinaryNode);
  protected
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
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
  public
    constructor Create(AComparator: TIntfCompare = nil);
    destructor Destroy; override;
  end;

  {
  TJclStrBinaryTree = class(TJclAbstractContainer, IJclStrCollection,
    IJclStrTree, IJclCloneable)
    }
  TJclStrBinaryTree = class(TJclStrCollection, IJclStrCollection, IJclStrTree,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  private
    FComparator: TStrCompare;
    FCount: Integer;
    FRoot: TJclStrBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    procedure RotateLeft(Node: TJclStrBinaryNode);
    procedure RotateRight(Node: TJclStrBinaryNode);
  protected
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
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
    { IJclStrTree }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
  public
    constructor Create(AComparator: TStrCompare = nil);
    destructor Destroy; override;
  end;

  TJclBinaryTree = class(TJclAbstractContainer, IJclCollection, IJclTree,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  private
    FOwnsObjects: Boolean;
    FComparator: TCompare;
    FCount: Integer;
    FRoot: TJclBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    procedure RotateLeft(Node: TJclBinaryNode);
    procedure RotateRight(Node: TJclBinaryNode);
  protected
    procedure FreeObject(var AObject: TObject);
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
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
  public
    constructor Create(AComparator: TCompare = nil; AOwnsObjects: Boolean = True);
    destructor Destroy; override;

    property OwnsObjects: Boolean read FOwnsObjects;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  TJclBinaryTree<T> = class(TJclAbstractContainer, IJclCollection<T>, IJclTree<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  private
    FOwnsItems: Boolean;
    FCount: Integer;
    FRoot: TJclBinaryNode<T>;
    FTraverseOrder: TJclTraverseOrder;
    procedure RotateLeft(Node: TJclBinaryNode<T>);
    procedure RotateRight(Node: TJclBinaryNode<T>);
  protected
    function CompareItems(const A, B: T): Integer; virtual; abstract;
    function CreateEmptyBinaryTree: TJclBinaryTree<T>; virtual; abstract;
    procedure FreeItem(var AItem: T);
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
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
    constructor Create(AOwnsItems: Boolean = True);
    destructor Destroy; override;
  end;

  // E = External helper to compare items
  TJclBinaryTreeE<T> = class(TJclBinaryTree<T>, IJclCollection<T>, IJclTree<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  private
    FComparer: IComparer<T>;
  protected
    function CompareItems(const A, B: T): Integer; override;
    function CreateEmptyBinaryTree: TJclBinaryTree<T>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AComparer: IComparer<T>; AOwnsItems: Boolean = True);
    property Comparer: IComparer<T> read FComparer write FComparer;
  end;

  // F = Function to compare items
  TJclBinaryTreeF<T> = class(TJclBinaryTree<T>, IJclCollection<T>, IJclTree<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  private
    FCompare: TCompare<T>;
  protected
    function CompareItems(const A, B: T): Integer; override;
    function CreateEmptyBinaryTree: TJclBinaryTree<T>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(ACompare: TCompare<T>; AOwnsItems: Boolean = True);
    property Compare: TCompare<T> read FCompare write FCompare;
  end;

  // I = Items can compare themselves to an other
  TJclBinaryTreeI<T: IComparable<T>> = class(TJclBinaryTree<T>, IJclCollection<T>, IJclTree<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CompareItems(const A, B: T): Integer; override;
    function CreateEmptyBinaryTree: TJclBinaryTree<T>; override;
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
    function GetNextCursor: TJclIntfBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclIntfBinaryNode; virtual; abstract;
    { IJclIntfIterator }
    procedure Add(const AInterface: IInterface);
    function GetObject: IInterface;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    procedure Insert(const AObject: IInterface);
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
  inherited Create(OwnList);
  FCursor := Start;
  FOwnList := OwnList;
  Valid := AValid;
end;

procedure TIntfItr.Add(const AInterface: IInterface);
begin
  FOwnList.Add(AInterface);
end;

function TIntfItr.GetObject: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := True;
    if FCursor <> nil then
      Result := FCursor.Obj
    else
      Result := nil;
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

procedure TIntfItr.Insert(const AObject: IInterface);
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
    if FCursor <> nil then
      Result := FCursor.Obj
    else
      Result := nil;
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
    if FCursor <> nil then
      Result := FCursor.Obj
    else
      Result := nil;
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
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextCursor;
    if OldCursor <> nil then
      FOwnList.Remove(OldCursor.Obj);
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
    function GetNextCursor: TJclIntfBinaryNode; override;
    function GetPreviousCursor: TJclIntfBinaryNode; override;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderIntfItr.Clone: TObject;
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

function TPreOrderIntfItr.IntfClone: IInterface;
begin
  Result := TPreOrderIntfItr.Create(FOwnList, FCursor, Valid);
end;

//=== { TInOrderIntfItr } ====================================================

type
  TInOrderIntfItr = class(TIntfItr, IJclIntfIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function GetNextCursor: TJclIntfBinaryNode; override;
    function GetPreviousCursor: TJclIntfBinaryNode; override;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderIntfItr.Clone: TObject;
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

function TInOrderIntfItr.IntfClone: IInterface;
begin
  Result := TInOrderIntfItr.Create(FOwnList, FCursor, Valid);
end;

//=== { TPostOrderIntfItr } ==================================================

type
  TPostOrderIntfItr = class(TIntfItr, IJclIntfIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function GetNextCursor: TJclIntfBinaryNode; override;
    function GetPreviousCursor: TJclIntfBinaryNode; override;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderIntfItr.Clone: TObject;
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

function TPostOrderIntfItr.IntfClone: IInterface;
begin
  Result := TPostOrderIntfItr.Create(FOwnList, FCursor, Valid);
end;

//=== { TStrItr } ============================================================

type
  TStrItr = class(TJclAbstractIterator, IJclStrIterator)
  protected
    FCursor: TJclStrBinaryNode;
    FOwnList: IJclStrCollection;
    function GetNextCursor: TJclStrBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclStrBinaryNode; virtual; abstract;
    { IJclStrIterator }
    procedure Add(const AString: string);
    function GetString: string;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    procedure Insert(const AString: string);
    function Next: string;
    function NextIndex: Integer;
    function Previous: string;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetString(const AString: string);
  public
    constructor Create(const OwnList: IJclStrCollection; Start: TJclStrBinaryNode; AValid: Boolean);
  end;

constructor TStrItr.Create(const OwnList: IJclStrCollection; Start: TJclStrBinaryNode; AValid: Boolean);
begin
  inherited Create(OwnList);
  FCursor := Start;
  FOwnList := OwnList;
  Valid := AValid;
end;

procedure TStrItr.Add(const AString: string);
begin
  FOwnList.Add(AString);
end;

function TStrItr.GetString: string;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := True;
    if FCursor <> nil then
      Result := FCursor.Str
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TStrItr.HasNext: Boolean;
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

function TStrItr.HasPrevious: Boolean;
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

procedure TStrItr.Insert(const AString: string);
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TStrItr.Next: string;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetNextCursor
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Str
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TStrItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TStrItr.Previous: string;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      FCursor := GetPreviousCursor
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Str
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TStrItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TStrItr.Remove;
var
  OldCursor: TJclStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextCursor;
    if OldCursor <> nil then
      FOwnList.Remove(OldCursor.Str);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TStrItr.SetString(const AString: string);
begin
  raise EJclOperationNotSupportedError.Create;
end;

//=== { TPreOrderStrItr } ====================================================

type
  TPreOrderStrItr = class(TStrItr, IJclStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function GetNextCursor: TJclStrBinaryNode; override;
    function GetPreviousCursor: TJclStrBinaryNode; override;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderStrItr.Clone: TObject;
begin
  Result := TPreOrderStrItr.Create(FOwnList, FCursor, Valid);
end;

function TPreOrderStrItr.GetNextCursor: TJclStrBinaryNode;
var
  LastRet: TJclStrBinaryNode;
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

function TPreOrderStrItr.GetPreviousCursor: TJclStrBinaryNode;
var
  LastRet: TJclStrBinaryNode;
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

function TPreOrderStrItr.IntfClone: IInterface;
begin
  Result := TPreOrderStrItr.Create(FOwnList, FCursor, Valid);
end;

//=== { TInOrderStrItr } =====================================================

type
  TInOrderStrItr = class(TStrItr, IJclStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function GetNextCursor: TJclStrBinaryNode; override;
    function GetPreviousCursor: TJclStrBinaryNode; override;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderStrItr.Clone: TObject;
begin
  Result := TInOrderStrItr.Create(FOwnList, FCursor, Valid);
end;

function TInOrderStrItr.GetNextCursor: TJclStrBinaryNode;
var
  LastRet: TJclStrBinaryNode;
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

function TInOrderStrItr.GetPreviousCursor: TJclStrBinaryNode;
var
  LastRet: TJclStrBinaryNode;
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

function TInOrderStrItr.IntfClone: IInterface;
begin
  Result := TInOrderStrItr.Create(FOwnList, FCursor, Valid);
end;

//=== { TPostOrderStrItr } ===================================================

type
  TPostOrderStrItr = class(TStrItr, IJclStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function GetNextCursor: TJclStrBinaryNode; override;
    function GetPreviousCursor: TJclStrBinaryNode; override;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderStrItr.Clone: TObject;
begin
  Result := TPostOrderStrItr.Create(FOwnList, FCursor, Valid);
end;

function TPostOrderStrItr.GetNextCursor: TJclStrBinaryNode;
var
  LastRet: TJclStrBinaryNode;
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

function TPostOrderStrItr.GetPreviousCursor: TJclStrBinaryNode;
var
  LastRet: TJclStrBinaryNode;
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

function TPostOrderStrItr.IntfClone: IInterface;
begin
  Result := TPostOrderStrItr.Create(FOwnList, FCursor, Valid);
end;

//=== { TItr } ===============================================================

type
  TItr = class(TJclAbstractIterator, IJclIterator)
  protected
    FCursor: TJclBinaryNode;
    FOwnList: IJclCollection;
    function GetNextCursor: TJclBinaryNode; virtual; abstract;
    function GetPreviousCursor: TJclBinaryNode; virtual; abstract;
    { IJclIntfIterator }
    procedure Add(AObject: TObject);
    function GetObject: TObject;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    procedure Insert(AObject: TObject);
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
  inherited Create(OwnList);
  FCursor := Start;
  FOwnList := OwnList;
  Valid := AValid;
end;

procedure TItr.Add(AObject: TObject);
begin
  FOwnList.Add(AObject);
end;

function TItr.GetObject: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := True;
    if FCursor <> nil then
      Result := FCursor.Obj
    else
      Result := nil;
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

procedure TItr.Insert(AObject: TObject);
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
    if FCursor <> nil then
      Result := FCursor.Obj
    else
      Result := nil;
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
    if FCursor <> nil then
      Result := FCursor.Obj
    else
      Result := nil;
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
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextCursor;
    if OldCursor <> nil then
      FOwnList.Remove(OldCursor.Obj);
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

//=== { TPreOrderItr } =======================================================

type
  TPreOrderItr = class(TItr, IJclIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function GetNextCursor: TJclBinaryNode; override;
    function GetPreviousCursor: TJclBinaryNode; override;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderItr.Clone: TObject;
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

function TPreOrderItr.IntfClone: IInterface;
begin
  Result := TPreOrderItr.Create(FOwnList, FCursor, Valid);
end;

//=== { TInOrderItr } ========================================================

type
  TInOrderItr = class(TItr, IJclIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function GetNextCursor: TJclBinaryNode; override;
    function GetPreviousCursor: TJclBinaryNode; override;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderItr.Clone: TObject;
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

function TInOrderItr.IntfClone: IInterface;
begin
  Result := TInOrderItr.Create(FOwnList, FCursor, Valid);
end;

//=== { TPostOrderItr } ======================================================

type
  TPostOrderItr = class(TItr, IJclIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function GetNextCursor: TJclBinaryNode; override;
    function GetPreviousCursor: TJclBinaryNode; override;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderItr.Clone: TObject;
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

function TPostOrderItr.IntfClone: IInterface;
begin
  Result := TPostOrderItr.Create(FOwnList, FCursor, Valid);
end;

{$IFDEF SUPPORTS_GENERICS}

//=== { TItr<T> } ============================================================

type
  TItr<T> = class(TJclAbstractIterator, IJclIterator<T>)
  protected
    FCursor:  TJclBinaryNode<T>;
    FOwnList: IJclCollection<T>;
    function GetNextCursor: TJclBinaryNode<T>; virtual; abstract;
    function GetPreviousCursor: TJclBinaryNode<T>; virtual; abstract;
    { IJclIntfIterator<T> }
    procedure Add(const AItem: T);
    function GetItem: T;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    procedure Insert(const AItem: T);
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
  inherited Create(OwnList);
  FCursor := Start;
  FOwnList := OwnList;
  Valid := AValid;
end;

procedure TItr<T>.Add(const AItem: T);
begin
  FOwnList.Add(AItem);
end;

function TItr<T>.GetItem: T;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Valid := True;
    if FCursor <> nil then
      Result := FCursor.Obj
    else
      Result := Default(T);
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

procedure TItr<T>.Insert(const AItem: T);
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
    if FCursor <> nil then
      Result := FCursor.Obj;
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
    if FCursor <> nil then
      Result := FCursor.Obj;
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
    Valid := False;
    OldCursor := FCursor;
    FCursor := GetNextCursor;
    if OldCursor <> nil then
      FOwnList.Remove(OldCursor.Obj);
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

//=== { TPreOrderItr<T> } ====================================================

type
  TPreOrderItr<T> = class(TItr<T>, IJclIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function GetNextCursor: TJclBinaryNode<T>; override;
    function GetPreviousCursor: TJclBinaryNode<T>; override;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPreOrderItr<T>.Clone: TObject;
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

function TPreOrderItr<T>.IntfClone: IInterface;
begin
  Result := TPreOrderItr<T>.Create(FOwnList, FCursor, Valid);
end;

//=== { TInOrderItr<T> } =====================================================

type
  TInOrderItr<T> = class(TItr<T>, IJclIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function GetNextCursor: TJclBinaryNode<T>; override;
    function GetPreviousCursor: TJclBinaryNode<T>; override;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TInOrderItr<T>.Clone: TObject;
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

function TInOrderItr<T>.IntfClone: IInterface;
begin
  Result := TInOrderItr<T>.Create(FOwnList, FCursor, Valid);
end;

//=== { TPostOrderItr<T> } ===================================================

type
  TPostOrderItr<T> = class(TItr<T>, IJclIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  protected
    function GetNextCursor: TJclBinaryNode<T>; override;
    function GetPreviousCursor: TJclBinaryNode<T>; override;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  end;

function TPostOrderItr<T>.Clone: TObject;
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

function TPostOrderItr<T>.IntfClone: IInterface;
begin
  Result := TPostOrderItr<T>.Create(FOwnList, FCursor, Valid);
end;

{$ENDIF SUPPORTS_GENERICS}

//=== { TJclIntfBinaryTree } =================================================

constructor TJclIntfBinaryTree.Create(AComparator: TIntfCompare);
begin
  inherited Create(nil);
  if Assigned(AComparator) then
    FComparator := AComparator
  else
    FComparator := @IntfSimpleCompare;
  FTraverseOrder := toOrder;
end;

destructor TJclIntfBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclIntfBinaryTree.Add(const AInterface: IInterface): Boolean;
var
  NewNode: TJclIntfBinaryNode;
  Current, Save: TJclIntfBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    NewNode := TJclIntfBinaryNode.Create;
    NewNode.Obj := AInterface;
    // Insert into right place
    Save := nil;
    Current := FRoot;
    while Current <> nil do
    begin
      Save := Current;
      if FComparator(NewNode.Obj, Current.Obj) < 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    NewNode.Parent := Save;
    if Save = nil then
      FRoot := NewNode
    else
    if FComparator(NewNode.Obj, Save.Obj) < 0 then
      Save.Left := NewNode
    else
      Save.Right := NewNode;
    // RB balanced
    NewNode.Color := tcRed;
    while (NewNode <> FRoot) and (NewNode.Parent.Color = tcRed) do
    begin
      if (NewNode.Parent.Parent <> nil) and (NewNode.Parent = NewNode.Parent.Parent.Left) then
      begin
        Current := NewNode.Parent.Parent.Right;
        if (Current <> nil) and (Current.Color = tcRed) then
        begin
          NewNode.Parent.Color := tcBlack;
          Current.Color := tcBlack;
          NewNode.Parent.Parent.Color := tcRed;
          NewNode := NewNode.Parent.Parent;
        end
        else
        begin
          if NewNode = NewNode.Parent.Right then
          begin
            NewNode := NewNode.Parent;
            RotateLeft(NewNode);
          end;
          NewNode.Parent.Color := tcBlack;
          NewNode.Parent.Parent.Color := tcRed;
          RotateRight(NewNode.Parent.Parent);
        end;
      end
      else
      begin
        if NewNode.Parent.Parent = nil then
          Current := nil
        else
          Current := NewNode.Parent.Parent.Left;
        if (Current <> nil) and (Current.Color = tcRed) then
        begin
          NewNode.Parent.Color := tcBlack;
          Current.Color := tcBlack;
          NewNode.Parent.Parent.Color := tcRed;
          NewNode := NewNode.Parent.Parent;
        end
        else
        begin
          if NewNode = NewNode.Parent.Left then
          begin
            NewNode := NewNode.Parent;
            RotateRight(NewNode);
          end;
          NewNode.Parent.Color := tcBlack;
          if NewNode.Parent.Parent <> nil then
            NewNode.Parent.Parent.Color := tcRed;
          RotateLeft(NewNode.Parent.Parent);
        end;
      end;
    end;
    FRoot.Color := tcBlack;
    Inc(FCount);
    Result := True;
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
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) or Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    // find first
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree
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
    FCount := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfBinaryTree.Clone: TObject;
var
  NewTree: TJclIntfBinaryTree;

  function CloneNode(Node, Parent: TJclIntfBinaryNode): TJclIntfBinaryNode;
  begin
    Result := TJclIntfBinaryNode.Create;
    Result.Obj := Node.Obj;
    Result.Color := Node.Color;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;

begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewTree := TJclIntfBinaryTree.Create(FComparator);
    NewTree.FCount := FCount;
    if FRoot <> nil then
      NewTree.FRoot := CloneNode(FRoot, nil);
    Result := NewTree;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
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
      Comp := FComparator(Current.Obj, AInterface);
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
    if FCount <> ACollection.Size then
      Exit;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if FComparator(ItSelf.Next, It.Next) <> 0 then
        Exit;
    Result := True;
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
  Result := FCount = 0;
end;

function TJclIntfBinaryTree.IntfClone: IInterface;
var
  NewTree: TJclIntfBinaryTree;

  function CloneNode(Node, Parent: TJclIntfBinaryNode): TJclIntfBinaryNode;
  begin
    Result := TJclIntfBinaryNode.Create;
    Result.Obj := Node.Obj;
    Result.Color := Node.Color;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;

begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewTree := TJclIntfBinaryTree.Create(FComparator);
    NewTree.FCount := FCount;
    if FRoot <> nil then
      NewTree.FRoot := CloneNode(FRoot, nil);
    Result := NewTree;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfBinaryTree.RotateLeft(Node: TJclIntfBinaryNode);
var
  TempNode: TJclIntfBinaryNode;
begin
  if Node = nil then
    Exit;
  TempNode := Node.Right;
  //  if TempNode = nil then	Exit;
  Node.Right := TempNode.Left;
  if TempNode.Left <> nil then
    TempNode.Left.Parent := Node;
  TempNode.Parent := Node.Parent;
  if Node.Parent = nil then
    FRoot := TempNode
  else
  if Node.Parent.Left = Node then
    Node.Parent.Left := TempNode
  else
    Node.Parent.Right := TempNode;
  TempNode.Left := Node;
  Node.Parent := TempNode;
end;

procedure TJclIntfBinaryTree.RotateRight(Node: TJclIntfBinaryNode);
var
  TempNode: TJclIntfBinaryNode;
begin
  if Node = nil then
    Exit;
  TempNode := Node.Left;
  //  if TempNode = nil then 	Exit;
  Node.Left := TempNode.Right;
  if TempNode.Right <> nil then
    TempNode.Right.Parent := Node;
  TempNode.Parent := Node.Parent;
  if Node.Parent = nil then
    FRoot := TempNode
  else
  if Node.Parent.Right = Node then
    Node.Parent.Right := TempNode
  else
    Node.Parent.Left := TempNode;
  TempNode.Right := Node;
  Node.Parent := TempNode;
end;

function TJclIntfBinaryTree.Remove(const AInterface: IInterface): Boolean;
var
  Current: TJclIntfBinaryNode;
  Node: TJclIntfBinaryNode;
  Save: TJclIntfBinaryNode;
  Comp: Integer;

  procedure Correction(Node: TJclIntfBinaryNode);
  var
    TempNode: TJclIntfBinaryNode;
  begin
    while (Node <> FRoot) and (Node.Color = tcBlack) do
    begin
      if Node = Node.Parent.Left then
      begin
        TempNode := Node.Parent.Right;
        if TempNode = nil then
        begin
          Node := Node.Parent;
          Continue;
        end;
        if TempNode.Color = tcRed then
        begin
          TempNode.Color := tcBlack;
          Node.Parent.Color := tcRed;
          RotateLeft(Node.Parent);
          TempNode := Node.Parent.Right;
        end;
        if (TempNode.Left <> nil) and (TempNode.Left.Color = tcBlack) and
          (TempNode.Right <> nil) and (TempNode.Right.Color = tcBlack) then
        begin
          TempNode.Color := tcRed;
          Node := Node.Parent;
        end
        else
        begin
          if (TempNode.Right <> nil) and (TempNode.Right.Color = tcBlack) then
          begin
            TempNode.Left.Color := tcBlack;
            TempNode.Color := tcRed;
            RotateRight(TempNode);
            TempNode := Node.Parent.Right;
          end;
          TempNode.Color := Node.Parent.Color;
          Node.Parent.Color := tcBlack;
          if TempNode.Right <> nil then
            TempNode.Right.Color := tcBlack;
          RotateLeft(Node.Parent);
          Node := FRoot;
        end;
      end
      else
      begin
        TempNode := Node.Parent.Left;
        if TempNode = nil then
        begin
          Node := Node.Parent;
          Continue;
        end;
        if TempNode.Color = tcRed then
        begin
          TempNode.Color := tcBlack;
          Node.Parent.Color := tcRed;
          RotateRight(Node.Parent);
          TempNode := Node.Parent.Left;
        end;
        if (TempNode.Left.Color = tcBlack) and (TempNode.Right.Color = tcBlack) then
        begin
          TempNode.Color := tcRed;
          Node := Node.Parent;
        end
        else
        begin
          if TempNode.Left.Color = tcBlack then
          begin
            TempNode.Right.Color := tcBlack;
            TempNode.Color := tcRed;
            RotateLeft(TempNode);
            TempNode := Node.Parent.Left;
          end;
          TempNode.Color := Node.Parent.Color;
          Node.Parent.Color := tcBlack;
          if TempNode.Left <> nil then
            TempNode.Left.Color := tcBlack;
          RotateRight(Node.Parent);
          Node := FRoot;
        end;
      end;
    end;
    Node.Color := tcBlack;
  end;

begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AObject in the tree
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := FComparator(AInterface, Current.Obj);
      if Comp = 0 then
        Break
      else
      if Comp < 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    if Current = nil then
      Exit;
    // Remove
    if (Current.Left = nil) or (Current.Right = nil) then
      Save := Current
    else
    begin // Successor in Save
      if Current.Right <> nil then
      begin
        Save := Current.Right;
        while Save.Left <> nil do // Minimum
          Save := Save.Left;
      end
      else
      begin
        Save := Current.Parent;
        while (Save <> nil) and (Current = Save.Right) do
        begin
          Current := Save;
          Save := Save.Parent;
        end;
      end;
    end;
    if Save.Left <> nil then
      Node := Save.Left
    else
      Node := Save.Right;
    if Node <> nil then
    begin
      Node.Parent := Save.Parent;
      if Save.Parent = nil then
        FRoot := Node
      else
      if Save = Save.Parent.Left then
        Save.Parent.Left := Node
      else
        Save.Parent.Right := Node;
      if Save.Color = tcBlack then
        Correction(Node);
    end
    else
    if Save.Parent = nil then
      FRoot := nil
    else
    begin
      if Save.Color = tcBlack then
        Correction(Save);
      if Save.Parent <> nil then
        if Save = Save.Parent.Left then
          Save.Parent.Left := nil
        else
        if Save = Save.Parent.Right then
          Save.Parent.Right := nil
    end;
    Save.Obj := nil;
    Save.Free;
    Dec(FCount);
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
    Result := True;
    if ACollection = nil then
      Exit;
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

procedure TJclIntfBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclIntfBinaryTree.Size: Integer;
begin
  Result := FCount;
end;

//=== { TJclStrBinaryTree } ==================================================

constructor TJclStrBinaryTree.Create(AComparator: TStrCompare);
begin
  inherited Create(nil);
  if Assigned(AComparator) then
    FComparator := AComparator
  else
    FComparator := @StrSimpleCompare;
  FTraverseOrder := toOrder;
end;

destructor TJclStrBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclStrBinaryTree.Add(const AString: string): Boolean;
var
  NewNode: TJclStrBinaryNode;
  Current, Save: TJclStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    NewNode := TJclStrBinaryNode.Create;
    NewNode.Str := AString;
    // Insert into right place
    Save := nil;
    Current := FRoot;
    while Current <> nil do
    begin
      Save := Current;
      if FComparator(NewNode.Str, Current.Str) < 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    NewNode.Parent := Save;
    if Save = nil then
      FRoot := NewNode
    else
    if FComparator(NewNode.Str, Save.Str) < 0 then
      Save.Left := NewNode
    else
      Save.Right := NewNode;
    // RB balanced
    NewNode.Color := tcRed;
    while (NewNode <> FRoot) and (NewNode.Parent.Color = tcRed) do
    begin
      if (NewNode.Parent.Parent <> nil) and (NewNode.Parent = NewNode.Parent.Parent.Left) then
      begin
        Current := NewNode.Parent.Parent.Right;
        if (Current <> nil) and (Current.Color = tcRed) then
        begin
          NewNode.Parent.Color := tcBlack;
          Current.Color := tcBlack;
          NewNode.Parent.Parent.Color := tcRed;
          NewNode := NewNode.Parent.Parent;
        end
        else
        begin
          if NewNode = NewNode.Parent.Right then
          begin
            NewNode := NewNode.Parent;
            RotateLeft(NewNode);
          end;
          NewNode.Parent.Color := tcBlack;
          NewNode.Parent.Parent.Color := tcRed;
          RotateRight(NewNode.Parent.Parent);
        end;
      end
      else
      begin
        if NewNode.Parent.Parent = nil then
          Current := nil
        else
          Current := NewNode.Parent.Parent.Left;
        if (Current <> nil) and (Current.Color = tcRed) then
        begin
          NewNode.Parent.Color := tcBlack;
          Current.Color := tcBlack;
          NewNode.Parent.Parent.Color := tcRed;
          NewNode := NewNode.Parent.Parent;
        end
        else
        begin
          if NewNode = NewNode.Parent.Left then
          begin
            NewNode := NewNode.Parent;
            RotateRight(NewNode);
          end;
          NewNode.Parent.Color := tcBlack;
          if NewNode.Parent.Parent <> nil then
            NewNode.Parent.Parent.Color := tcRed;
          RotateLeft(NewNode.Parent.Parent);
        end;
      end;
    end;
    FRoot.Color := tcBlack;
    Inc(FCount);
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrBinaryTree.AddAll(const ACollection: IJclStrCollection): Boolean;
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

procedure TJclStrBinaryTree.Clear;
var
  Current, Parent: TJclStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    // postorder
    Current := FRoot;
    if Current = nil then
      Exit;
    // find first
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree
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
    FCount := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrBinaryTree.Clone: TObject;
var
  NewTree: TJclStrBinaryTree;

  function CloneNode(Node, Parent: TJclStrBinaryNode): TJclStrBinaryNode;
  begin
    Result := TJclStrBinaryNode.Create;
    Result.Str := Node.Str;
    Result.Color := Node.Color;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;

begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewTree := TJclStrBinaryTree.Create(FComparator);
    NewTree.FCount := FCount;
    if FRoot <> nil then
      NewTree.FRoot := CloneNode(FRoot, nil);
    Result := NewTree;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrBinaryTree.Contains(const AString: string): Boolean;
var
  Comp: Integer;
  Current: TJclStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := FComparator(Current.Str, AString);
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

function TJclStrBinaryTree.ContainsAll(const ACollection: IJclStrCollection): Boolean;
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
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrBinaryTree.Equals(const ACollection: IJclStrCollection): Boolean;
var
  It, ItSelf: IJclStrIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FCount <> ACollection.Size then
      Exit;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if FComparator(ItSelf.Next, It.Next) <> 0 then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrBinaryTree.First: IJclStrIterator;
var
  Start: TJclStrBinaryNode;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Start := FRoot;
    case GetTraverseOrder of
      toPreOrder:
        Result := TPreOrderStrItr.Create(Self, Start, False);
      toOrder:
        begin
          if Start <> nil then
            while Start.Left <> nil do
              Start := Start.Left;
          Result := TInOrderStrItr.Create(Self, Start, False);
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
          Result := TPostOrderStrItr.Create(Self, Start, False);
        end;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclStrBinaryTree.IntfClone: IInterface;
var
  NewTree: TJclStrBinaryTree;

  function CloneNode(Node, Parent: TJclStrBinaryNode): TJclStrBinaryNode;
  begin
    Result := TJclStrBinaryNode.Create;
    Result.Str := Node.Str;
    Result.Color := Node.Color;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;

begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewTree := TJclStrBinaryTree.Create(FComparator);
    NewTree.FCount := FCount;
    if FRoot <> nil then
      NewTree.FRoot := CloneNode(FRoot, nil);
    Result := NewTree;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrBinaryTree.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclStrBinaryTree.Last: IJclStrIterator;
var
  Start: TJclStrBinaryNode;
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
          Result := TPreOrderStrItr.Create(Self, Start, False);
        end;
      toOrder:
        begin
          if Start <> nil then
            while Start.Right <> nil do
              Start := Start.Right;
          Result := TInOrderStrItr.Create(Self, Start, False);
        end;
      toPostOrder:
        Result := TPostOrderStrItr.Create(Self, Start, False);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrBinaryTree.Remove(const AString: string): Boolean;
var
  Current: TJclStrBinaryNode;
  Node: TJclStrBinaryNode;
  Save: TJclStrBinaryNode;
  Comp: Integer;

  procedure Correction(Node: TJclStrBinaryNode);
  var
    TempNode: TJclStrBinaryNode;
  begin
    while (Node <> FRoot) and (Node.Color = tcBlack) do
    begin
      if Node = Node.Parent.Left then
      begin
        TempNode := Node.Parent.Right;
        if TempNode = nil then
        begin
          Node := Node.Parent;
          Continue;
        end;
        if TempNode.Color = tcRed then
        begin
          TempNode.Color := tcBlack;
          Node.Parent.Color := tcRed;
          RotateLeft(Node.Parent);
          TempNode := Node.Parent.Right;
        end;
        if (TempNode.Left <> nil) and (TempNode.Left.Color = tcBlack) and
          (TempNode.Right <> nil) and (TempNode.Right.Color = tcBlack) then
        begin
          TempNode.Color := tcRed;
          Node := Node.Parent;
        end
        else
        begin
          if (TempNode.Right <> nil) and (TempNode.Right.Color = tcBlack) then
          begin
            TempNode.Left.Color := tcBlack;
            TempNode.Color := tcRed;
            RotateRight(TempNode);
            TempNode := Node.Parent.Right;
          end;
          TempNode.Color := Node.Parent.Color;
          Node.Parent.Color := tcBlack;
          if TempNode.Right <> nil then
            TempNode.Right.Color := tcBlack;
          RotateLeft(Node.Parent);
          Node := FRoot;
        end;
      end
      else
      begin
        TempNode := Node.Parent.Left;
        if TempNode = nil then
        begin
          Node := Node.Parent;
          Continue;
        end;
        if TempNode.Color = tcRed then
        begin
          TempNode.Color := tcBlack;
          Node.Parent.Color := tcRed;
          RotateRight(Node.Parent);
          TempNode := Node.Parent.Left;
        end;
        if (TempNode.Left.Color = tcBlack) and (TempNode.Right.Color = tcBlack) then
        begin
          TempNode.Color := tcRed;
          Node := Node.Parent;
        end
        else
        begin
          if TempNode.Left.Color = tcBlack then
          begin
            TempNode.Right.Color := tcBlack;
            TempNode.Color := tcRed;
            RotateLeft(TempNode);
            TempNode := Node.Parent.Left;
          end;
          TempNode.Color := Node.Parent.Color;
          Node.Parent.Color := tcBlack;
          if TempNode.Left <> nil then
            TempNode.Left.Color := tcBlack;
          RotateRight(Node.Parent);
          Node := FRoot;
        end;
      end
    end;
    Node.Color := tcBlack;
  end;

begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AObject in the tree
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := FComparator(AString, Current.Str);
      if Comp = 0 then
        Break
      else
      if Comp < 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    if Current = nil then
      Exit;
    // Remove
    if (Current.Left = nil) or (Current.Right = nil) then
      Save := Current
    else
    begin // Successor in Save
      if Current.Right <> nil then
      begin
        Save := Current.Right;
        while Save.Left <> nil do // Minimum
          Save := Save.Left;
      end
      else
      begin
        Save := Current.Parent;
        while (Save <> nil) and (Current = Save.Right) do
        begin
          Current := Save;
          Save := Save.Parent;
        end;
      end;
    end;
    if Save.Left <> nil then
      Node := Save.Left
    else
      Node := Save.Right;
    if Node <> nil then
    begin
      Node.Parent := Save.Parent;
      if Save.Parent = nil then
        FRoot := Node
      else
      if Save = Save.Parent.Left then
        Save.Parent.Left := Node
      else
        Save.Parent.Right := Node;
      if Save.Color = tcBlack then // Correction
        Correction(Node);
    end
    else
    if Save.Parent = nil then
      FRoot := nil
    else
    begin
      if Save.Color = tcBlack then // Correction
        Correction(Save);
      if Save.Parent <> nil then
        if Save = Save.Parent.Left then
          Save.Parent.Left := nil
        else
        if Save = Save.Parent.Right then
          Save.Parent.Right := nil
    end;
    Save.Free;
    Dec(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrBinaryTree.RemoveAll(const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrBinaryTree.RetainAll(const ACollection: IJclStrCollection): Boolean;
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

procedure TJclStrBinaryTree.RotateLeft(Node: TJclStrBinaryNode);
var
  TempNode: TJclStrBinaryNode;
begin
  if Node = nil then
    Exit;
  TempNode := Node.Right;
  //  if TempNode = nil then	Exit;
  Node.Right := TempNode.Left;
  if TempNode.Left <> nil then
    TempNode.Left.Parent := Node;
  TempNode.Parent := Node.Parent;
  if Node.Parent = nil then
    FRoot := TempNode
  else
  if Node.Parent.Left = Node then
    Node.Parent.Left := TempNode
  else
    Node.Parent.Right := TempNode;
  TempNode.Left := Node;
  Node.Parent := TempNode;
end;

procedure TJclStrBinaryTree.RotateRight(Node: TJclStrBinaryNode);
var
  TempNode: TJclStrBinaryNode;
begin
  if Node = nil then
    Exit;
  TempNode := Node.Left;
  //  if TempNode = nil then 	Exit;
  Node.Left := TempNode.Right;
  if TempNode.Right <> nil then
    TempNode.Right.Parent := Node;
  TempNode.Parent := Node.Parent;
  if Node.Parent = nil then
    FRoot := TempNode
  else
  if Node.Parent.Right = Node then
    Node.Parent.Right := TempNode
  else
    Node.Parent.Left := TempNode;
  TempNode.Right := Node;
  Node.Parent := TempNode;
end;

procedure TJclStrBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclStrBinaryTree.Size: Integer;
begin
  Result := FCount;
end;

//=== { TJclBinaryTree } =====================================================

constructor TJclBinaryTree.Create(AComparator: TCompare; AOwnsObjects: Boolean);
begin
  inherited Create(nil);
  if Assigned(AComparator) then
    FComparator := AComparator
  else
    FComparator := @SimpleCompare;
  FTraverseOrder := toOrder;
end;

destructor TJclBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclBinaryTree.Add(AObject: TObject): Boolean;
var
  NewNode: TJclBinaryNode;
  Current, Save: TJclBinaryNode;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    NewNode := TJclBinaryNode.Create;
    NewNode.Obj := AObject;
    // Insert into right place
    Save := nil;
    Current := FRoot;
    while Current <> nil do
    begin
      Save := Current;
      if FComparator(NewNode.Obj, Current.Obj) < 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    NewNode.Parent := Save;
    if Save = nil then
      FRoot := NewNode
    else
    if FComparator(NewNode.Obj, Save.Obj) < 0 then
      Save.Left := NewNode
    else
      Save.Right := NewNode;
    // RB balanced
    NewNode.Color := tcRed;
    while (NewNode <> FRoot) and (NewNode.Parent.Color = tcRed) do
    begin
      if (NewNode.Parent.Parent <> nil) and (NewNode.Parent = NewNode.Parent.Parent.Left) then
      begin
        Current := NewNode.Parent.Parent.Right;
        if (Current <> nil) and (Current.Color = tcRed) then
        begin
          NewNode.Parent.Color := tcBlack;
          Current.Color := tcBlack;
          NewNode.Parent.Parent.Color := tcRed;
          NewNode := NewNode.Parent.Parent;
        end
        else
        begin
          if NewNode = NewNode.Parent.Right then
          begin
            NewNode := NewNode.Parent;
            RotateLeft(NewNode);
          end;
          NewNode.Parent.Color := tcBlack;
          NewNode.Parent.Parent.Color := tcRed;
          RotateRight(NewNode.Parent.Parent);
        end;
      end
      else
      begin
        if NewNode.Parent.Parent = nil then
          Current := nil
        else
          Current := NewNode.Parent.Parent.Left;
        if (Current <> nil) and (Current.Color = tcRed) then
        begin
          NewNode.Parent.Color := tcBlack;
          Current.Color := tcBlack;
          NewNode.Parent.Parent.Color := tcRed;
          NewNode := NewNode.Parent.Parent;
        end
        else
        begin
          if NewNode = NewNode.Parent.Left then
          begin
            NewNode := NewNode.Parent;
            RotateRight(NewNode);
          end;
          NewNode.Parent.Color := tcBlack;
          if NewNode.Parent.Parent <> nil then
            NewNode.Parent.Parent.Color := tcRed;
          RotateLeft(NewNode.Parent.Parent);
        end;
      end;
    end;
    FRoot.Color := tcBlack;
    Inc(FCount);
    Result := True;
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
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) or Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    // find first
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree
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
      FreeObject(Current.Obj);
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
    FCount := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.Clone: TObject;
var
  NewTree: TJclBinaryTree;

  function CloneNode(Node, Parent: TJclBinaryNode): TJclBinaryNode;
  begin
    Result := TJclBinaryNode.Create;
    Result.Obj := Node.Obj;
    Result.Color := Node.Color;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;

begin
  NewTree := TJclBinaryTree.Create(FComparator, False);
  NewTree.FCount := FCount;
  if FRoot <> nil then
    NewTree.FRoot := CloneNode(FRoot, nil);
  Result := NewTree;
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
    // iterative version
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := FComparator(Current.Obj, AObject);
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
    if FCount <> ACollection.Size then
      Exit;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if FComparator(ItSelf.Next, It.Next) <> 0 then
        Exit;
    Result := True;
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
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclBinaryTree.FreeObject(var AObject: TObject);
begin
  if OwnsObjects then
    FreeAndNil(AObject)
  else
    AObject := nil;
end;

function TJclBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclBinaryTree.IntfClone: IInterface;
var
  NewTree: TJclBinaryTree;

  function CloneNode(Node, Parent: TJclBinaryNode): TJclBinaryNode;
  begin
    Result := TJclBinaryNode.Create;
    Result.Obj := Node.Obj;
    Result.Color := Node.Color;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;

begin
  NewTree := TJclBinaryTree.Create(FComparator, False);
  NewTree.FCount := FCount;
  if FRoot <> nil then
    NewTree.FRoot := CloneNode(FRoot, nil);
  Result := NewTree;
end;

function TJclBinaryTree.IsEmpty: Boolean;
begin
  Result := FCount = 0;
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
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree.Remove(AObject: TObject): Boolean;
var
  Current: TJclBinaryNode;
  Node: TJclBinaryNode;
  Save: TJclBinaryNode;
  Comp: Integer;

  procedure Correction(Node: TJclBinaryNode);
  var
    TempNode: TJclBinaryNode;
  begin
    while (Node <> FRoot) and (Node.Color = tcBlack) do
    begin
      if Node = Node.Parent.Left then
      begin
        TempNode := Node.Parent.Right;
        if TempNode = nil then
        begin
          Node := Node.Parent;
          Continue;
        end;
        if TempNode.Color = tcRed then
        begin
          TempNode.Color := tcBlack;
          Node.Parent.Color := tcRed;
          RotateLeft(Node.Parent);
          TempNode := Node.Parent.Right;
        end;
        if (TempNode.Left <> nil) and (TempNode.Left.Color = tcBlack) and
          (TempNode.Right <> nil) and (TempNode.Right.Color = tcBlack) then
        begin
          TempNode.Color := tcRed;
          Node := Node.Parent;
        end
        else
        begin
          if (TempNode.Right <> nil) and (TempNode.Right.Color = tcBlack) then
          begin
            TempNode.Left.Color := tcBlack;
            TempNode.Color := tcRed;
            RotateRight(TempNode);
            TempNode := Node.Parent.Right;
          end;
          TempNode.Color := Node.Parent.Color;
          Node.Parent.Color := tcBlack;
          if TempNode.Right <> nil then
            TempNode.Right.Color := tcBlack;
          RotateLeft(Node.Parent);
          Node := FRoot;
        end;
      end
      else
      begin
        TempNode := Node.Parent.Left;
        if TempNode = nil then
        begin
          Node := Node.Parent;
          Continue;
        end;
        if TempNode.Color = tcRed then
        begin
          TempNode.Color := tcBlack;
          Node.Parent.Color := tcRed;
          RotateRight(Node.Parent);
          TempNode := Node.Parent.Left;
        end;
        if (TempNode.Left.Color = tcBlack) and (TempNode.Right.Color = tcBlack) then
        begin
          TempNode.Color := tcRed;
          Node := Node.Parent;
        end
        else
        begin
          if TempNode.Left.Color = tcBlack then
          begin
            TempNode.Right.Color := tcBlack;
            TempNode.Color := tcRed;
            RotateLeft(TempNode);
            TempNode := Node.Parent.Left;
          end;
          TempNode.Color := Node.Parent.Color;
          Node.Parent.Color := tcBlack;
          if TempNode.Left <> nil then
            TempNode.Left.Color := tcBlack;
          RotateRight(Node.Parent);
          Node := FRoot;
        end;
      end
    end;
    Node.Color := tcBlack;
  end;

begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AObject in the tree
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := FComparator(AObject, Current.Obj);
      if Comp = 0 then
        Break
      else
      if Comp < 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    if Current = nil then
      Exit;
    // Remove
    if (Current.Left = nil) or (Current.Right = nil) then
      Save := Current
    else
    begin // Successor in Save
      if Current.Right <> nil then
      begin
        Save := Current.Right;
        while Save.Left <> nil do // Minimum
          Save := Save.Left;
      end
      else
      begin
        Save := Current.Parent;
        while (Save <> nil) and (Current = Save.Right) do
        begin
          Current := Save;
          Save := Save.Parent;
        end;
      end;
    end;
    if Save.Left <> nil then
      Node := Save.Left
    else
      Node := Save.Right;
    if Node <> nil then
    begin
      Node.Parent := Save.Parent;
      if Save.Parent = nil then
        FRoot := Node
      else
      if Save = Save.Parent.Left then
        Save.Parent.Left := Node
      else
        Save.Parent.Right := Node;
      if Save.Color = tcBlack then // Correction
        Correction(Node);
    end
    else
    if Save.Parent = nil then
      FRoot := nil
    else
    begin
      if Save.Color = tcBlack then // Correction
        Correction(Save);
      if Save.Parent <> nil then
        if Save = Save.Parent.Left then
          Save.Parent.Left := nil
        else
        if Save = Save.Parent.Right then
          Save.Parent.Right := nil
    end;
    FreeObject(Save.Obj);
    Save.Free;
    Dec(FCount);
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
    Result := True;
    if ACollection = nil then
      Exit;
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

procedure TJclBinaryTree.RotateLeft(Node: TJclBinaryNode);
var
  TempNode: TJclBinaryNode;
begin
  if Node = nil then
    Exit;
  TempNode := Node.Right;
  //  if TempNode = nil then	Exit;
  Node.Right := TempNode.Left;
  if TempNode.Left <> nil then
    TempNode.Left.Parent := Node;
  TempNode.Parent := Node.Parent;
  if Node.Parent = nil then
    FRoot := TempNode
  else
  if Node.Parent.Left = Node then
    Node.Parent.Left := TempNode
  else
    Node.Parent.Right := TempNode;
  TempNode.Left := Node;
  Node.Parent := TempNode;
end;

procedure TJclBinaryTree.RotateRight(Node: TJclBinaryNode);
var
  TempNode: TJclBinaryNode;
begin
  if Node = nil then
    Exit;
  TempNode := Node.Left;
  //  if TempNode = nil then 	Exit;
  Node.Left := TempNode.Right;
  if TempNode.Right <> nil then
    TempNode.Right.Parent := Node;
  TempNode.Parent := Node.Parent;
  if Node.Parent = nil then
    FRoot := TempNode
  else
  if Node.Parent.Right = Node then
    Node.Parent.Right := TempNode
  else
    Node.Parent.Left := TempNode;
  TempNode.Right := Node;
  Node.Parent := TempNode;
end;

procedure TJclBinaryTree.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclBinaryTree.Size: Integer;
begin
  Result := FCount;
end;

{$IFDEF SUPPORTS_GENERICS}

//=== { TJclBinaryTree<T> } ==================================================

constructor TJclBinaryTree<T>.Create(AOwnsItems: Boolean);
begin
  inherited Create(nil);
  FTraverseOrder := toOrder;
  FOwnsItems := AOwnsItems;
end;

destructor TJclBinaryTree<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclBinaryTree<T>.Add(const AItem: T): Boolean;
var
  NewNode: TJclBinaryNode<T>;
  Current, Save: TJclBinaryNode<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    NewNode := TJclBinaryNode<T>.Create;
    NewNode.Obj := AItem;
    // Insert into right place
    Save := nil;
    Current := FRoot;
    while Current <> nil do
    begin
      Save := Current;
      if CompareItems(NewNode.Obj, Current.Obj) < 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    NewNode.Parent := Save;
    if Save = nil then
      FRoot := NewNode
    else
    if CompareItems(NewNode.Obj, Save.Obj) < 0 then
      Save.Left := NewNode
    else
      Save.Right := NewNode;
    // RB balanced
    NewNode.Color := tcRed;
    while (NewNode <> FRoot) and (NewNode.Parent.Color = tcRed) do
    begin
      if (NewNode.Parent.Parent <> nil) and (NewNode.Parent = NewNode.Parent.Parent.Left) then
      begin
        Current := NewNode.Parent.Parent.Right;
        if (Current <> nil) and (Current.Color = tcRed) then
        begin
          NewNode.Parent.Color := tcBlack;
          Current.Color := tcBlack;
          NewNode.Parent.Parent.Color := tcRed;
          NewNode := NewNode.Parent.Parent;
        end
        else
        begin
          if NewNode = NewNode.Parent.Right then
          begin
            NewNode := NewNode.Parent;
            RotateLeft(NewNode);
          end;
          NewNode.Parent.Color := tcBlack;
          NewNode.Parent.Parent.Color := tcRed;
          RotateRight(NewNode.Parent.Parent);
        end;
      end
      else
      begin
        if NewNode.Parent.Parent = nil then
          Current := nil
        else
          Current := NewNode.Parent.Parent.Left;
        if (Current <> nil) and (Current.Color = tcRed) then
        begin
          NewNode.Parent.Color := tcBlack;
          Current.Color := tcBlack;
          NewNode.Parent.Parent.Color := tcRed;
          NewNode := NewNode.Parent.Parent;
        end
        else
        begin
          if NewNode = NewNode.Parent.Left then
          begin
            NewNode := NewNode.Parent;
            RotateRight(NewNode);
          end;
          NewNode.Parent.Color := tcBlack;
          if NewNode.Parent.Parent <> nil then
            NewNode.Parent.Parent.Color := tcRed;
          RotateLeft(NewNode.Parent.Parent);
        end;
      end;
    end;
    FRoot.Color := tcBlack;
    Inc(FCount);
    Result := True;
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
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) or Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    // find first
    while (Current.Left <> nil) or (Current.Right <> nil) do
    begin
      if Current.Left <> nil then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    // for all items in the tree
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
      FreeItem(Current.Obj);
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
    FCount := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.Clone: TObject;
var
  NewTree: TJclBinaryTree<T>;

  function CloneNode(Node, Parent: TJclBinaryNode<T>): TJclBinaryNode<T>;
  begin
    Result := TJclBinaryNode<T>.Create;
    Result.Obj := Node.Obj;
    Result.Color := Node.Color;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;

begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewTree := CreateEmptyBinaryTree;
    NewTree.FCount := FCount;
    if FRoot <> nil then
      NewTree.FRoot := CloneNode(FRoot, nil);
    Result := NewTree;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
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
    // iterative version
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := CompareItems(Current.Obj, AItem);
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
    if FCount <> ACollection.Size then
      Exit;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext do
      if CompareItems(ItSelf.Next, It.Next) <> 0 then
        Exit;
    Result := True;
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

procedure TJclBinaryTree<T>.FreeItem(var AItem: T);
begin
  if FOwnsItems then
    FreeAndNil(AItem)
  else
    AItem := Default(T);
end;

function TJclBinaryTree<T>.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclBinaryTree<T>.IntfClone: IInterface;
var
  NewTree: TJclBinaryTree<T>;

  function CloneNode(Node, Parent: TJclBinaryNode<T>): TJclBinaryNode<T>;
  begin
    Result := TJclBinaryNode<T>.Create;
    Result.Obj := Node.Obj;
    Result.Color := Node.Color;
    Result.Parent := Parent;
    if Node.Left <> nil then
      Result.Left := CloneNode(Node.Left, Result); // recursive call
    if Node.Right <> nil then
      Result.Right := CloneNode(Node.Right, Result); // recursive call
  end;

begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewTree := CreateEmptyBinaryTree;
    NewTree.FCount := FCount;
    if FRoot <> nil then
      NewTree.FRoot := CloneNode(FRoot, nil);
    Result := NewTree;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclBinaryTree<T>.IsEmpty: Boolean;
begin
  Result := FCount = 0;
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

function TJclBinaryTree<T>.Remove(const AItem: T): Boolean;
var
  Current: TJclBinaryNode<T>;
  Node: TJclBinaryNode<T>;
  Save: TJclBinaryNode<T>;
  Comp: Integer;

  procedure Correction(Node: TJclBinaryNode<T>);
  var
    TempNode: TJclBinaryNode<T>;
  begin
    while (Node <> FRoot) and (Node.Color = tcBlack) do
    begin
      if Node = Node.Parent.Left then
      begin
        TempNode := Node.Parent.Right;
        if TempNode = nil then
        begin
          Node := Node.Parent;
          Continue;
        end;
        if TempNode.Color = tcRed then
        begin
          TempNode.Color := tcBlack;
          Node.Parent.Color := tcRed;
          RotateLeft(Node.Parent);
          TempNode := Node.Parent.Right;
        end;
        if (TempNode.Left <> nil) and (TempNode.Left.Color = tcBlack) and
          (TempNode.Right <> nil) and (TempNode.Right.Color = tcBlack) then
        begin
          TempNode.Color := tcRed;
          Node := Node.Parent;
        end
        else
        begin
          if (TempNode.Right <> nil) and (TempNode.Right.Color = tcBlack) then
          begin
            TempNode.Left.Color := tcBlack;
            TempNode.Color := tcRed;
            RotateRight(TempNode);
            TempNode := Node.Parent.Right;
          end;
          TempNode.Color := Node.Parent.Color;
          Node.Parent.Color := tcBlack;
          if TempNode.Right <> nil then
            TempNode.Right.Color := tcBlack;
          RotateLeft(Node.Parent);
          Node := FRoot;
        end;
      end
      else
      begin
        TempNode := Node.Parent.Left;
        if TempNode = nil then
        begin
          Node := Node.Parent;
          Continue;
        end;
        if TempNode.Color = tcRed then
        begin
          TempNode.Color := tcBlack;
          Node.Parent.Color := tcRed;
          RotateRight(Node.Parent);
          TempNode := Node.Parent.Left;
        end;
        if (TempNode.Left.Color = tcBlack) and (TempNode.Right.Color = tcBlack) then
        begin
          TempNode.Color := tcRed;
          Node := Node.Parent;
        end
        else
        begin
          if TempNode.Left.Color = tcBlack then
          begin
            TempNode.Right.Color := tcBlack;
            TempNode.Color := tcRed;
            RotateLeft(TempNode);
            TempNode := Node.Parent.Left;
          end;
          TempNode.Color := Node.Parent.Color;
          Node.Parent.Color := tcBlack;
          if TempNode.Left <> nil then
            TempNode.Left.Color := tcBlack;
          RotateRight(Node.Parent);
          Node := FRoot;
        end;
      end
    end;
    Node.Color := tcBlack;
  end;

begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    // locate AItem in the tree
    Current := FRoot;
    while Current <> nil do
    begin
      Comp := CompareItems(AItem, Current.Obj);
      if Comp = 0 then
        Break
      else
      if Comp < 0 then
        Current := Current.Left
      else
        Current := Current.Right;
    end;
    if Current = nil then
      Exit;
    // Remove
    if (Current.Left = nil) or (Current.Right = nil) then
      Save := Current
    else
    begin // Successor in Save
      if Current.Right <> nil then
      begin
        Save := Current.Right;
        while Save.Left <> nil do // Minimum
          Save := Save.Left;
      end
      else
      begin
        Save := Current.Parent;
        while (Save <> nil) and (Current = Save.Right) do
        begin
          Current := Save;
          Save := Save.Parent;
        end;
      end;
    end;
    if Save.Left <> nil then
      Node := Save.Left
    else
      Node := Save.Right;
    if Node <> nil then
    begin
      Node.Parent := Save.Parent;
      if Save.Parent = nil then
        FRoot := Node
      else
      if Save = Save.Parent.Left then
        Save.Parent.Left := Node
      else
        Save.Parent.Right := Node;
      if Save.Color = tcBlack then // Correction
        Correction(Node);
    end
    else
    if Save.Parent = nil then
      FRoot := nil
    else
    begin
      if Save.Color = tcBlack then // Correction
        Correction(Save);
      if Save.Parent <> nil then
        if Save = Save.Parent.Left then
          Save.Parent.Left := nil
        else
        if Save = Save.Parent.Right then
          Save.Parent.Right := nil
    end;
    FreeItem(Save.Obj);
    Save.Free;
    Dec(FCount);
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
    Result := True;
    if ACollection = nil then
      Exit;
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

procedure TJclBinaryTree<T>.RotateLeft(Node: TJclBinaryNode<T>);
var
  TempNode: TJclBinaryNode<T>;
begin
  if Node = nil then
    Exit;
  TempNode := Node.Right;
  //  if TempNode = nil then	Exit;
  Node.Right := TempNode.Left;
  if TempNode.Left <> nil then
    TempNode.Left.Parent := Node;
  TempNode.Parent := Node.Parent;
  if Node.Parent = nil then
    FRoot := TempNode
  else
  if Node.Parent.Left = Node then
    Node.Parent.Left := TempNode
  else
    Node.Parent.Right := TempNode;
  TempNode.Left := Node;
  Node.Parent := TempNode;
end;

procedure TJclBinaryTree<T>.RotateRight(Node: TJclBinaryNode<T>);
var
  TempNode: TJclBinaryNode<T>;
begin
  if Node = nil then
    Exit;
  TempNode := Node.Left;
  //  if TempNode = nil then 	Exit;
  Node.Left := TempNode.Right;
  if TempNode.Right <> nil then
    TempNode.Right.Parent := Node;
  TempNode.Parent := Node.Parent;
  if Node.Parent = nil then
    FRoot := TempNode
  else
  if Node.Parent.Right = Node then
    Node.Parent.Right := TempNode
  else
    Node.Parent.Left := TempNode;
  TempNode.Right := Node;
  Node.Parent := TempNode;
end;

procedure TJclBinaryTree<T>.SetTraverseOrder(Value: TJclTraverseOrder);
begin
  FTraverseOrder := Value;
end;

function TJclBinaryTree<T>.Size: Integer;
begin
  Result := FCount;
end;

//=== { TJclBinaryTreeE<T> } =================================================

constructor TJclBinaryTreeE<T>.Create(const AComparer: IComparer<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FComparer := AComparer;
end;

function TJclBinaryTreeE<T>.CompareItems(const A, B: T): Integer;
begin
  if Comparer = nil then
    raise EJclNoComparerError.Create;
  Result := Comparer.Compare(A, B);
end;

function TJclBinaryTreeE<T>.CreateEmptyBinaryTree: TJclBinaryTree<T>;
begin
  Result := TJclBinaryTreeE<T>.Create(Comparer, False);
end;

//=== { TJclBinaryTreeF<T> } =================================================

constructor TJclBinaryTreeF<T>.Create(ACompare: TCompare<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FCompare := ACompare;
end;

function TJclBinaryTreeF<T>.CompareItems(const A, B: T): Integer;
begin
  if not Assigned(Compare) then
    raise EJclNoComparerError.Create;
  Result := Compare(A, B);
end;

function TJclBinaryTreeF<T>.CreateEmptyBinaryTree: TJclBinaryTree<T>;
begin
  Result := TJclBinaryTreeF<T>.Create(Compare, False);
end;

//=== { TJclBinaryTreeI<T> } =================================================

function TJclBinaryTreeI<T>.CompareItems(const A, B: T): Integer;
begin
  Result := A.CompareTo(B);
end;

function TJclBinaryTreeI<T>.CreateEmptyBinaryTree: TJclBinaryTree<T>;
begin
  Result := TJclBinaryTreeI<T>.Create(False);
end;

{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

