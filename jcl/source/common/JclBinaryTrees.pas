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
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclBinaryTrees;

{$I jcl.inc}

{.DEFINE RECURSIVE}

interface

uses
  Classes,
  JclBase, JclAbstractContainers, JclAlgorithms, JclContainerIntf;

type
  TJclTreeColor = (tcBlack, tcRed);

  {$IFDEF CLR}
  TJclIntfBinaryNode = class;
  PJclIntfBinaryNode = TJclIntfBinaryNode;
  TJclIntfBinaryNode = class
  {$ELSE}
  PJclIntfBinaryNode = ^TJclIntfBinaryNode;
  TJclIntfBinaryNode = record
  {$ENDIF CLR}
    Obj: IInterface;
    Left: PJclIntfBinaryNode;
    Right: PJclIntfBinaryNode;
    Parent: PJclIntfBinaryNode;
    Color: TJclTreeColor;
  end;

  {$IFDEF CLR}
  TJclStrBinaryNode = class;
  PJclStrBinaryNode = TJclStrBinaryNode;
  TJclStrBinaryNode = class
  {$ELSE}
  PJclStrBinaryNode = ^TJclStrBinaryNode;
  TJclStrBinaryNode = record
  {$ENDIF CLR}
    Str: string;
    Left: PJclStrBinaryNode;
    Right: PJclStrBinaryNode;
    Parent: PJclStrBinaryNode;
    Color: TJclTreeColor;
  end;

  {$IFDEF CLR}
  TJclBinaryNode = class;
  PJclBinaryNode = TJclBinaryNode;
  TJclBinaryNode = class
  {$ELSE}
  PJclBinaryNode = ^TJclBinaryNode;
  TJclBinaryNode = record
  {$ENDIF CLR}
    Obj: TObject;
    Left: PJclBinaryNode;
    Right: PJclBinaryNode;
    Parent: PJclBinaryNode;
    Color: TJclTreeColor;
  end;

  TJclIntfBinaryTree = class(TJclAbstractContainer, IJclIntfCollection,
    IJclIntfTree, IJclIntfCloneable)
  private
    FComparator: TIntfCompare;
    FCount: Integer;
    FRoot: PJclIntfBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    procedure RotateLeft(Node: PJclIntfBinaryNode);
    procedure RotateRight(Node: PJclIntfBinaryNode);
  protected
    { IJclIntfCollection }
    function Add(AInterface: IInterface): Boolean;
    function AddAll(ACollection: IJclIntfCollection): Boolean;
    procedure Clear;
    function Contains(AInterface: IInterface): Boolean;
    function ContainsAll(ACollection: IJclIntfCollection): Boolean;
    function Equals(ACollection: IJclIntfCollection): Boolean;
    function First: IJclIntfIterator;
    function IsEmpty: Boolean;
    function Last: IJclIntfIterator;
    function Remove(AInterface: IInterface): Boolean;
    function RemoveAll(ACollection: IJclIntfCollection): Boolean;
    function RetainAll(ACollection: IJclIntfCollection): Boolean;
    function Size: Integer;
    { IJclIntfTree }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    { IJclIntfCloneable }
    function Clone: IInterface;
  public
    constructor Create(AComparator: TIntfCompare = nil);
    destructor Destroy; override;
  end;

  {
  TJclStrBinaryTree = class(TJclAbstractContainer, IJclStrCollection,
    IJclStrTree, IJclCloneable)
    }
  TJclStrBinaryTree = class(TJclStrCollection, IJclStrTree, IJclCloneable)
  private
    FComparator: TStrCompare;
    FCount: Integer;
    FRoot: PJclStrBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    procedure RotateLeft(Node: PJclStrBinaryNode);
    procedure RotateRight(Node: PJclStrBinaryNode);
  protected
    { IJclStrCollection }
    function Add(const AString: string): Boolean; override;
    function AddAll(ACollection: IJclStrCollection): Boolean; override;
    procedure Clear; override;
    function Contains(const AString: string): Boolean; override;
    function ContainsAll(ACollection: IJclStrCollection): Boolean; override;
    function Equals(ACollection: IJclStrCollection): Boolean; override;
    function First: IJclStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclStrIterator; override;
    function Remove(const AString: string): Boolean; override;
    function RemoveAll(ACollection: IJclStrCollection): Boolean; override;
    function RetainAll(ACollection: IJclStrCollection): Boolean; override;
    function Size: Integer; override;
    { IJclStrTree }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    { IJclCloneable }
    function Clone: TObject;
  public
    constructor Create(AComparator: TStrCompare = nil);
    destructor Destroy; override;
  end;

  TJclBinaryTree = class(TJclAbstractContainer, IJclCollection, IJclTree,
    IJclCloneable)
  private
    FComparator: TCompare;
    FCount: Integer;
    FRoot: PJclBinaryNode;
    FTraverseOrder: TJclTraverseOrder;
    procedure RotateLeft(Node: PJclBinaryNode);
    procedure RotateRight(Node: PJclBinaryNode);
  protected
    { IJclCollection }
    function Add(AObject: TObject): Boolean;
    function AddAll(ACollection: IJclCollection): Boolean;
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function ContainsAll(ACollection: IJclCollection): Boolean;
    function Equals(ACollection: IJclCollection): Boolean;
    function First: IJclIterator;
    function IsEmpty: Boolean;
    function Last: IJclIterator;
    function Remove(AObject: TObject): Boolean;
    function RemoveAll(ACollection: IJclCollection): Boolean;
    function RetainAll(ACollection: IJclCollection): Boolean;
    function Size: Integer;
    { IJclTree }
    function GetTraverseOrder: TJclTraverseOrder;
    procedure SetTraverseOrder(Value: TJclTraverseOrder);
    { IJclCloneable }
    function Clone: TObject;
  public
    constructor Create(AComparator: TCompare = nil);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  JclResources;

//=== { TIntfItr } ===========================================================

type
  TIntfItr = class(TJclAbstractContainer, IJclIntfIterator)
  private
    FCursor: PJclIntfBinaryNode;
    FOwnList: IJclIntfCollection;
  protected
    { IJclIntfIterator }
    procedure Add(AInterface: IInterface);
    function GetObject: IInterface;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Next: IInterface; virtual;
    function NextIndex: Integer;
    function Previous: IInterface; virtual;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetObject(AInterface: IInterface);
  public
    constructor Create(OwnList: IJclIntfCollection; Start: PJclIntfBinaryNode);
  end;

constructor TIntfItr.Create(OwnList: IJclIntfCollection; Start: PJclIntfBinaryNode);
begin
  inherited Create;
  FCursor := Start;
  FOwnList := OwnList;
end;

procedure TIntfItr.Add(AInterface: IInterface);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  FOwnList.Add(AInterface);
end;

function TIntfItr.GetObject: IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Obj;
end;

function TIntfItr.HasNext: Boolean;
begin
  Result := FCursor <> nil;
end;

function TIntfItr.HasPrevious: Boolean;
begin
  Result := FCursor <> nil;
end;

function TIntfItr.Next: IInterface;
begin
end;

function TIntfItr.NextIndex: Integer;
begin
  // No index
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

function TIntfItr.Previous: IInterface;
begin
end;

function TIntfItr.PreviousIndex: Integer;
begin
  // No index
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

procedure TIntfItr.Remove;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  FOwnList.Remove(Next);
end;

procedure TIntfItr.SetObject(AInterface: IInterface);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  FCursor.Obj := AInterface;
end;

//=== { TPreOrderIntfItr } ===================================================

type
  TPreOrderIntfItr = class(TIntfItr, IJclIntfIterator)
  protected
    { IJclIntfIterator }
    function Next: IInterface; override;
    function Previous: IInterface; override;
  end;

function TPreOrderIntfItr.Next: IInterface;
var
  LastRet: PJclIntfBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Obj;
  LastRet := FCursor;
  if FCursor.Left <> nil then
    FCursor := FCursor.Left
  else
  if FCursor.Right <> nil then
    FCursor := FCursor.Right
  else
  begin
    FCursor := FCursor.Parent;
    while (FCursor <> nil) and ((FCursor.Right = nil) or (FCursor.Right = LastRet)) do
    begin
      LastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
    if FCursor <> nil then // not root
      FCursor := FCursor.Right;
  end;
end;

function TPreOrderIntfItr.Previous: IInterface;
var
  LastRet: PJclIntfBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Obj;
  LastRet := FCursor;
  FCursor := FCursor.Parent;
  if (FCursor <> nil) and (FCursor.Left <> LastRet) and (FCursor.Left <> nil)  then
    // come from Right
  begin
    FCursor := FCursor.Left;
    while (FCursor.Left <> nil) or (FCursor.Right <> nil) do // both childs
    begin
      if FCursor.Right <> nil then // right child first
        FCursor := FCursor.Right
      else
        FCursor := FCursor.Left;
    end;
  end;
end;

//=== { TInOrderIntfItr } ====================================================

type
  TInOrderIntfItr = class(TIntfItr, IJclIntfIterator)
  protected
    { IJclIntfIterator }
    function Next: IInterface; override;
    function Previous: IInterface; override;
  end;

function TInOrderIntfItr.Next: IInterface;
var
  LastRet: PJclIntfBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Obj;
  if FCursor.Right <> nil then
  begin
    FCursor := FCursor.Right;
    while (FCursor.Left <> nil) do
      FCursor := FCursor.Left;
  end
  else
  begin
    LastRet := FCursor;
    FCursor := FCursor.Parent;
    while (FCursor <> nil) and (FCursor.Right = LastRet) do
    begin
      LastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
  end;
end;

function TInOrderIntfItr.Previous: IInterface;
var
  LastRet: PJclIntfBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Obj;
  LastRet := FCursor;
  if FCursor.Left <> nil then
  begin
    FCursor := FCursor.Left;
    while FCursor.Right <> nil do
      FCursor := FCursor.Right;
  end
  else
  begin
    FCursor := FCursor.Parent;
    while (FCursor <> nil) and (FCursor.Right <> LastRet) do // Come from Left
    begin
      LastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
  end;
end;

//=== { TPostOrderIntfItr } ==================================================

type
  TPostOrderIntfItr = class(TIntfItr, IJclIntfIterator)
  protected
    { IJclIntfIterator }
    function Next: IInterface; override;
    function Previous: IInterface; override;
  end;

function TPostOrderIntfItr.Next: IInterface;
var
  LastRet: PJclIntfBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Obj;
  LastRet := FCursor;
  FCursor := FCursor.Parent;
  if (FCursor <> nil) and (FCursor.Right <> nil) and (FCursor.Right <> LastRet) then
  begin
    FCursor := FCursor.Right;
    while (FCursor.Left <> nil) or (FCursor.Right <> nil) do
    begin
      if FCursor.Left <> nil then
        FCursor := FCursor.Left
      else
        FCursor := FCursor.Right;
    end;
  end;
end;

function TPostOrderIntfItr.Previous: IInterface;
var
  LastRet: PJclIntfBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Obj;
  if (FCursor.Right <> nil) then
    FCursor := FCursor.Right
  else if (FCursor.Left <> nil) then
    FCursor := FCursor.Left
  else
  begin
    LastRet := FCursor;
    FCursor := FCursor.Parent;
    while (FCursor <> nil) and ((FCursor.Left = nil) or (FCursor.Left = LastRet)) do
    begin
      LastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
    if FCursor <> nil then // not root
      FCursor := FCursor.Left;
  end;
end;

//=== { TStrItr } ============================================================

type
  TStrItr = class(TJclAbstractContainer, IJclStrIterator)
  protected
    FCursor: PJclStrBinaryNode;
    FOwnList: IJclStrCollection;
    { IJclStrIterator }
    procedure Add(const AString: string);
    function GetString: string;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Next: string; virtual;
    function NextIndex: Integer;
    function Previous: string; virtual;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetString(const AString: string);
  public
    constructor Create(OwnList: IJclStrCollection; Start: PJclStrBinaryNode);
  end;

constructor TStrItr.Create(OwnList: IJclStrCollection; Start: PJclStrBinaryNode);
begin
  inherited Create;
  FCursor := Start;
  FOwnList := OwnList;
end;

procedure TStrItr.Add(const AString: string);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  FOwnList.Add(AString);
end;

function TStrItr.GetString: string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Str;
end;

function TStrItr.HasNext: Boolean;
begin
  Result := FCursor <> nil;
end;

function TStrItr.HasPrevious: Boolean;
begin
  Result := FCursor <> nil;
end;

function TStrItr.Next: string;
begin
end;

function TStrItr.NextIndex: Integer;
begin
  // No index
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

function TStrItr.Previous: string;
begin
end;

function TStrItr.PreviousIndex: Integer;
begin
  // No index
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

procedure TStrItr.Remove;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  FOwnList.Remove(Next);
end;

procedure TStrItr.SetString(const AString: string);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  FCursor.Str := AString;
end;

//=== { TPreOrderStrItr } ====================================================

type
  TPreOrderStrItr = class(TStrItr, IJclStrIterator)
  protected
    { IJclStrIterator }
    function Next: string; override;
    function Previous: string; override;
  end;

function TPreOrderStrItr.Next: string;
var
  LastRet: PJclStrBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Str;
  LastRet := FCursor;
  if FCursor.Left <> nil then
    FCursor := FCursor.Left
  else
  if FCursor.Right <> nil then
    FCursor := FCursor.Right
  else
  begin
    FCursor := FCursor.Parent;
    while (FCursor <> nil) and ((FCursor.Right = nil) or (FCursor.Right = LastRet)) do
    begin
      LastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
    if FCursor <> nil then // not root
      FCursor := FCursor.Right;
  end;
end;

function TPreOrderStrItr.Previous: string;
var
  LastRet: PJclStrBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Str;
  LastRet := FCursor;
  FCursor := FCursor.Parent;
  if (FCursor <> nil) and (FCursor.Left <> LastRet) and (FCursor.Left <> nil)  then
    // come from Right
  begin
    FCursor := FCursor.Left;
    while (FCursor.Left <> nil) or (FCursor.Right <> nil) do // both childs
    begin
      if FCursor.Right <> nil then // right child first
        FCursor := FCursor.Right
      else
        FCursor := FCursor.Left;
    end;
  end;
end;

//=== { TInOrderStrItr } =====================================================

type
  TInOrderStrItr = class(TStrItr, IJclStrIterator)
  protected
    { IJclStrIterator }
    function Next: string; override;
    function Previous: string; override;
  end;

function TInOrderStrItr.Next: string;
var
  LastRet: PJclStrBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Str;
  if FCursor.Right <> nil then
  begin
    FCursor := FCursor.Right;
    while (FCursor.Left <> nil) do
      FCursor := FCursor.Left;
  end
  else
  begin
    LastRet := FCursor;
    FCursor := FCursor.Parent;
    while (FCursor <> nil) and (FCursor.Right = LastRet) do
    begin
      LastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
  end;
end;

function TInOrderStrItr.Previous: string;
var
  LastRet: PJclStrBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Str;
  LastRet := FCursor;
  if FCursor.Left <> nil then
  begin
    FCursor := FCursor.Left;
    while FCursor.Right <> nil do
      FCursor := FCursor.Right;
  end
  else
  begin
    FCursor := FCursor.Parent;
    while (FCursor <> nil) and (FCursor.Right <> LastRet) do // Come from Left
    begin
      LastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
  end;
end;

//=== { TPostOrderStrItr } ===================================================

type
  TPostOrderStrItr = class(TStrItr, IJclStrIterator)
  protected
    { IJclStrIterator }
    function Next: string; override;
    function Previous: string; override;
  end;

function TPostOrderStrItr.Next: string;
var
  LastRet: PJclStrBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Str;
  LastRet := FCursor;
  FCursor := FCursor.Parent;
  if (FCursor <> nil) and (FCursor.Right <> nil) and (FCursor.Right <> LastRet) then
  begin
    FCursor := FCursor.Right;
    while (FCursor.Left <> nil) or (FCursor.Right <> nil) do
    begin
      if FCursor.Left <> nil then
        FCursor := FCursor.Left
      else
        FCursor := FCursor.Right;
    end;
  end;
end;

function TPostOrderStrItr.Previous: string;
var
  LastRet: PJclStrBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Str;
  if (FCursor.Right <> nil) then
    FCursor := FCursor.Right
  else if (FCursor.Left <> nil) then
    FCursor := FCursor.Left
  else
  begin
    LastRet := FCursor;
    FCursor := FCursor.Parent;
    while (FCursor <> nil) and ((FCursor.Left = nil) or (FCursor.Left = LastRet)) do
    begin
      LastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
    if FCursor <> nil then // not root
      FCursor := FCursor.Left;
  end;
end;

//=== { TItr } ===============================================================

type
  TItr = class(TJclAbstractContainer, IJclIterator)
  protected
    FCursor: PJclBinaryNode;
    FOwnList: IJclCollection;
    { IJclIntfIterator }
    procedure Add(AObject: TObject);
    function GetObject: TObject;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Next: TObject; virtual;
    function NextIndex: Integer;
    function Previous: TObject; virtual;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetObject(AObject: TObject);
  public
    constructor Create(OwnList: IJclCollection; Start: PJclBinaryNode);
  end;

constructor TItr.Create(OwnList: IJclCollection; Start: PJclBinaryNode);
begin
  inherited Create;
  FCursor := Start;
  FOwnList := OwnList;
end;

procedure TItr.Add(AObject: TObject);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  FOwnList.Add(AObject);
end;

function TItr.GetObject: TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Obj;
end;

function TItr.HasNext: Boolean;
begin
  Result := FCursor <> nil;
end;

function TItr.HasPrevious: Boolean;
begin
  Result := FCursor <> nil;
end;

function TItr.Next: TObject;
begin
  Result := nil; // overriden in derived class
end;

function TItr.NextIndex: Integer;
begin
  // No index
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

function TItr.Previous: TObject;
begin
  Result := nil; // overriden in derived class
end;

function TItr.PreviousIndex: Integer;
begin
  // No index
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

procedure TItr.Remove;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  FOwnList.Remove(Next);
end;

procedure TItr.SetObject(AObject: TObject);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  FCursor.Obj := AObject;
end;

//=== { TPreOrderItr } =======================================================

type
  TPreOrderItr = class(TItr, IJclIterator)
  protected
    { IJclIterator }
    function Next: TObject; override;
    function Previous: TObject; override;
  end;

function TPreOrderItr.Next: TObject;
var
  LastRet: PJclBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Obj;
  LastRet := FCursor;
  if FCursor.Left <> nil then
    FCursor := FCursor.Left
  else
  if FCursor.Right <> nil then
    FCursor := FCursor.Right
  else
  begin
    FCursor := FCursor.Parent;
    while (FCursor <> nil) and ((FCursor.Right = nil) or (FCursor.Right = LastRet)) do
    begin
      LastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
    if FCursor <> nil then // not root
      FCursor := FCursor.Right;
  end;
end;

function TPreOrderItr.Previous: TObject;
var
  LastRet: PJclBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Obj;
  LastRet := FCursor;
  FCursor := FCursor.Parent;
  if (FCursor <> nil) and (FCursor.Left <> LastRet) and (FCursor.Left <> nil)  then
    // come from Right
  begin
    FCursor := FCursor.Left;
    while (FCursor.Left <> nil) or (FCursor.Right <> nil) do // both childs
    begin
      if FCursor.Right <> nil then // right child first
        FCursor := FCursor.Right
      else
        FCursor := FCursor.Left;
    end;
  end;
end;

//=== { TInOrderItr } ========================================================

type
  TInOrderItr = class(TItr, IJclIterator)
  protected
    { IJclIterator }
    function Next: TObject; override;
    function Previous: TObject; override;
  end;

function TInOrderItr.Next: TObject;
var
  LastRet: PJclBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Obj;
  if FCursor.Right <> nil then
  begin
    FCursor := FCursor.Right;
    while (FCursor.Left <> nil) do
      FCursor := FCursor.Left;
  end
  else
  begin
    LastRet := FCursor;
    FCursor := FCursor.Parent;
    while (FCursor <> nil) and (FCursor.Right = LastRet) do
    begin
      LastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
  end;
end;

function TInOrderItr.Previous: TObject;
var
  LastRet: PJclBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Obj;
  LastRet := FCursor;
  if FCursor.Left <> nil then
  begin
    FCursor := FCursor.Left;
    while FCursor.Right <> nil do
      FCursor := FCursor.Right;
  end
  else
  begin
    FCursor := FCursor.Parent;
    while (FCursor <> nil) and (FCursor.Right <> LastRet) do // Come from Left
    begin
      LastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
  end;
end;

//=== { TPostOrderItr } ======================================================

type
  TPostOrderItr = class(TItr, IJclIterator)
  protected
    { IJclIterator }
    function Next: TObject; override;
    function Previous: TObject; override;
  end;

function TPostOrderItr.Next: TObject;
var
  LastRet: PJclBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Obj;
  LastRet := FCursor;
  FCursor := FCursor.Parent;
  if (FCursor <> nil) and (FCursor.Right <> nil) and (FCursor.Right <> LastRet) then
  begin
    FCursor := FCursor.Right;
    while (FCursor.Left <> nil) or (FCursor.Right <> nil) do
    begin
      if FCursor.Left <> nil then
        FCursor := FCursor.Left
      else
        FCursor := FCursor.Right;
    end;
  end;
end;

function TPostOrderItr.Previous: TObject;
var
  LastRet: PJclBinaryNode;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := FCursor.Obj;
  if (FCursor.Right <> nil) then
    FCursor := FCursor.Right
  else if (FCursor.Left <> nil) then
    FCursor := FCursor.Left
  else
  begin
    LastRet := FCursor;
    FCursor := FCursor.Parent;
    while (FCursor <> nil) and ((FCursor.Left = nil) or (FCursor.Left = LastRet)) do
    begin
      LastRet := FCursor;
      FCursor := FCursor.Parent;
    end;
    if FCursor <> nil then // not root
      FCursor := FCursor.Left;
  end;
end;

//=== { TJclIntfBinaryTree } =================================================

constructor TJclIntfBinaryTree.Create(AComparator: TIntfCompare = nil);
begin
  inherited Create;
  if Assigned(AComparator) then
    FComparator := AComparator
  else
    FComparator := @IntfSimpleCompare;
  FTraverseOrder := toPreOrder;
end;

destructor TJclIntfBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclIntfBinaryTree.Add(AInterface: IInterface): Boolean;
var
  NewNode: PJclIntfBinaryNode;
  Current, Save: PJclIntfBinaryNode;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AInterface = nil then
    Exit;
  {$IFDEF CLR}
  NewNode := TJclIntfBinaryNode.Create;
  {$ELSE}
  NewNode := AllocMem(SizeOf(TJclIntfBinaryNode));
  {$ENDIF CLR}
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
end;

function TJclIntfBinaryTree.AddAll(ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) or Result;
end;

procedure TJclIntfBinaryTree.Clear;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}

{$IFDEF RECURSIVE}
  procedure FreeChild(Node: PJclIntfBinaryNode);
  begin
    if Node.Left <> nil then
      FreeChild(Node.Left);
    if Node.Right <> nil then
      FreeChild(Node.Right);
    Node.Obj := nil; // Force Release
    FreeMem(Node);
  end;
{$ELSE}
var
  Current: PJclIntfBinaryNode;
  Save: PJclIntfBinaryNode;
{$ENDIF RECURSIVE}

begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  {$IFDEF RECURSIVE}
  // recursive version
  if FRoot <> nil then
  begin
    FreeChild(FRoot);
    FRoot := nil;
  end;
  {$ELSE}
  // iterative version
  Current := FRoot;
  while Current <> nil do
  begin
    if Current.Left <> nil then
      Current := Current.Left
    else
    if Current.Right <> nil then
      Current := Current.Right
    else
    begin
      Current.Obj := nil; // Force Release
      if Current.Parent = nil then // Root
      begin
        {$IFDEF CLR}
        Current.Free;
        {$ELSE}
        FreeMem(Current);
        {$ENDIF CLR}
        Current := nil;
        FRoot := nil;
      end
      else
      begin
        Save := Current;
        Current := Current.Parent;
        if Save = Current.Right then // True = from Right
        begin
          {$IFDEF CLR}
          Save.Free;
          {$ELSE}
          FreeMem(Save);
          {$ENDIF CLR}
          Current.Right := nil;
        end
        else
        begin
          {$IFDEF CLR}
          Save.Free;
          {$ELSE}
          FreeMem(Save);
          {$ENDIF CLR}
          Current.Left := nil;
        end
      end;
    end;
  end;
  {$ENDIF RECURSIVE}
  FCount := 0;
end;

function TJclIntfBinaryTree.Clone: IInterface;
var
  NewTree: TJclIntfBinaryTree;

  function CloneNode(Node, Parent: PJclIntfBinaryNode): PJclIntfBinaryNode;
  begin
    if Node <> nil then
    begin
      {$IFDEF CLR}
      Result := TJclIntfBinaryNode.Create;
      {$ELSE}
      GetMem(Result, SizeOf(TJclIntfBinaryNode));
      {$ENDIF CLR}
      Result.Obj := Node.Obj;
      Result.Color := Node.Color;
      Result.Parent := Parent;
      Result.Left := CloneNode(Node.Left, Result); // recursive call
      Result.Right := CloneNode(Node.Right, Result); // recursive call
    end
    else
      Result := nil;
  end;

begin
  NewTree := TJclIntfBinaryTree.Create(FComparator);
  NewTree.FCount := FCount;
  NewTree.FRoot := CloneNode(FRoot, nil);
  Result := NewTree;
end;

function TJclIntfBinaryTree.Contains(AInterface: IInterface): Boolean;
var
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
  Comp: Integer;

{$IFDEF RECURSIVE}
  function ContainsChild(Node: PJclIntfBinaryNode): Boolean;
  begin
    Result := False;
    if Node = nil then
      Exit;
    Comp := FComparator(Node.Obj, AInterface);
    if Comp = 0 then
      Result := True
    else
    if Comp > 0 then
      Result := ContainsChild(Node.Left)
    else
      Result := ContainsChild(Node.Right);
  end;
{$ELSE}
var
  Current: PJclIntfBinaryNode;
{$ENDIF RECURSIVE}

begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AInterface = nil then
    Exit;
  {$IFDEF RECURSIVE}
  // recursive version
  Result := ContainsChild(FRoot);
  {$ELSE}
  // iterative version
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
  {$ENDIF RECURSIVE}
end;

function TJclIntfBinaryTree.ContainsAll(ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
    Result := Contains(It.Next);
end;

function TJclIntfBinaryTree.Equals(ACollection: IJclIntfCollection): Boolean;
var
  It, ItSelf: IJclIntfIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
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
end;

function TJclIntfBinaryTree.First: IJclIntfIterator;
var
  Start: PJclIntfBinaryNode;
begin
  Start := FRoot;
  case GetTraverseOrder of
    toPreOrder:
      Result := TPreOrderIntfItr.Create(Self, Start);
    toOrder:
      begin
        if Start <> nil then
          while Start.Left <> nil do
            Start := Start.Left;
        Result := TInOrderIntfItr.Create(Self, Start);
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
        Result := TPostOrderIntfItr.Create(Self, Start);
      end;
  end;
end;

function TJclIntfBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclIntfBinaryTree.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclIntfBinaryTree.Last: IJclIntfIterator;
var
  Start: PJclIntfBinaryNode;
begin
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
        Result := TPreOrderIntfItr.Create(Self, Start);
      end;
    toOrder:
      begin
        if Start <> nil then
          while Start.Right <> nil do
            Start := Start.Right;
        Result := TInOrderIntfItr.Create(Self, Start);
      end;
    toPostOrder:
      Result := TPostOrderIntfItr.Create(Self, Start);
  end;
end;

procedure TJclIntfBinaryTree.RotateLeft(Node: PJclIntfBinaryNode);
var
  TempNode: PJclIntfBinaryNode;
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

procedure TJclIntfBinaryTree.RotateRight(Node: PJclIntfBinaryNode);
var
  TempNode: PJclIntfBinaryNode;
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

function TJclIntfBinaryTree.Remove(AInterface: IInterface): Boolean;
var
  Current: PJclIntfBinaryNode;
  Node: PJclIntfBinaryNode;
  Save: PJclIntfBinaryNode;
  Comp: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}

  procedure Correction(Node: PJclIntfBinaryNode);
  var
    TempNode: PJclIntfBinaryNode;
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
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AInterface = nil then
    Exit;
  // locate AInterface in the tree
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
  {$IFDEF CLR}
  Save.Free;
  {$ELSE}
  FreeMem(Save);
  {$ENDIF CLR}
  Dec(FCount);
end;

function TJclIntfBinaryTree.RemoveAll(ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
end;

function TJclIntfBinaryTree.RetainAll(ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
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

constructor TJclStrBinaryTree.Create(AComparator: TStrCompare = nil);
begin
  inherited Create;
  if Assigned(AComparator) then
    FComparator := AComparator
  else
    FComparator := @StrSimpleCompare;
  FTraverseOrder := toPreOrder;
end;

destructor TJclStrBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclStrBinaryTree.Add(const AString: string): Boolean;
var
  NewNode: PJclStrBinaryNode;
  Current, Save: PJclStrBinaryNode;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AString = '' then
    Exit;
  {$IFDEF CLR}
  NewNode := TJclStrBinaryNode.Create;
  {$ELSE}
  NewNode := AllocMem(SizeOf(TJclStrBinaryNode));
  {$ENDIF CLR}
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
end;

function TJclStrBinaryTree.AddAll(ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) or Result;
end;
{

function TJclStrBinaryTree.GetAsStrings: TStrings;
begin
  Result := TStringList.Create;
  try
    AppendToStrings(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TJclStrBinaryTree.LoadFromStrings(Strings: TStrings);
begin
  Clear;
  AppendFromStrings(Strings);
end;

procedure TJclStrBinaryTree.AppendToStrings(Strings: TStrings);
var
  It: IJclStrIterator;
begin
  It := First;
  Strings.BeginUpdate;
  try
    while It.HasNext do
      Strings.Add(It.Next);
  finally
    Strings.EndUpdate;
  end;
end;

procedure TJclStrBinaryTree.SaveToStrings(Strings: TStrings);
begin
  Strings.Clear;
  AppendToStrings(Strings);
end;

procedure TJclStrBinaryTree.AppendFromStrings(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    Add(Strings[I]);
end;

function TJclStrBinaryTree.GetAsDelimited(Separator: string): string;
var
  It: IJclStrIterator;
begin
  It := First;
  Result := '';
  if It.HasNext then
    Result := It.Next;
  while It.HasNext do
    Result := Result + Separator + It.Next;
end;

procedure TJclStrBinaryTree.LoadDelimited(AString, Separator: string);
begin
  Clear;
  AppendDelimited(AString, Separator);
end;

procedure TJclStrBinaryTree.AppendDelimited(AString, Separator: string);
begin
  DCLAppendDelimited(Self, AString, Separator);
end;
}

procedure TJclStrBinaryTree.Clear;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}

{$IFDEF RECURSIVE}
  procedure FreeChild(Node: PJclStrBinaryNode);
  begin
    if Node.Left <> nil then
      FreeChild(Node.Left);
    if Node.Right <> nil then
      FreeChild(Node.Right);
    Node.Str := ''; // Force Release
    FreeMem(Node);
  end;
{$ELSE}
var
  Current: PJclStrBinaryNode;
  Save: PJclStrBinaryNode;
{$ENDIF RECURSIVE}

begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  {$IFDEF RECURSIVE}
  // recursive version
  if FRoot <> nil then
  begin
    FreeChild(FRoot);
    FRoot := nil;
  end;
  {$ELSE}
  // iterative version
  Current := FRoot;
  while Current <> nil do
  begin
    if Current.Left <> nil then
      Current := Current.Left
    else
    if Current.Right <> nil then
      Current := Current.Right
    else
    begin
      Current.Str := ''; // Force Release
      if Current.Parent = nil then // Root
      begin
        {$IFDEF CLR}
        Current.Free;
        {$ELSE}
        FreeMem(Current);
        {$ENDIF CLR}
        Current := nil;
        FRoot := nil;
      end
      else
      begin
        Save := Current;
        Current := Current.Parent;
        if Save = Current.Right then // True = from Right
        begin
          {$IFDEF CLR}
          Save.Free;
          {$ELSE}
          FreeMem(Save);
          {$ENDIF CLR}
          Current.Right := nil;
        end
        else
        begin
          {$IFDEF CLR}
          Save.Free;
          {$ELSE}
          FreeMem(Save);
          {$ENDIF CLR}
          Current.Left := nil;
        end
      end;
    end;
  end;
  {$ENDIF RECURSIVE}
  FCount := 0;
end;

function TJclStrBinaryTree.Clone: TObject;
var
  NewTree: TJclStrBinaryTree;

  function CloneNode(Node, Parent: PJclStrBinaryNode): PJclStrBinaryNode;
  begin
    if Node <> nil then
    begin
      {$IFDEF CLR}
      Result := TJclStrBinaryNode.Create;
      {$ELSE}
      GetMem(Result, SizeOf(TJclStrBinaryNode));
      {$ENDIF CLR}
      Result.Str := Node.Str;
      Result.Color := Node.Color;
      Result.Parent := Parent;
      Result.Left := CloneNode(Node.Left, Result); // recursive call
      Result.Right := CloneNode(Node.Right, Result); // recursive call
    end
    else
      Result := nil;
  end;

begin
  NewTree := TJclStrBinaryTree.Create(FComparator);
  NewTree.FCount := FCount;
  NewTree.FRoot := CloneNode(FRoot, nil);
  Result := NewTree;
end;

function TJclStrBinaryTree.Contains(const AString: string): Boolean;
var
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
  Comp: Integer;

{$IFDEF RECURSIVE}
  function ContainsChild(Node: PJclStrBinaryNode): Boolean;
  begin
    Result := False;
    if Node = nil then
      Exit;
    Comp := FComparator(Node.Str, AString);
    if Comp = 0 then
      Result := True
    else
    if Comp > 0 then
      Result := ContainsChild(Node.Left)
    else
      Result := ContainsChild(Node.Right)
  end;
{$ELSE}
var
  Current: PJclStrBinaryNode;
{$ENDIF RECURSIVE}

begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AString = '' then
    Exit;
  {$IFDEF RECURSIVE}
  // recursive version
  Result := ContainsChild(FRoot);
  {$ELSE}
  // iterative version
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
  {$ENDIF RECURSIVE}
end;

function TJclStrBinaryTree.ContainsAll(ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
    Result := Contains(It.Next);
end;

function TJclStrBinaryTree.Equals(ACollection: IJclStrCollection): Boolean;
var
  It, ItSelf: IJclStrIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
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
end;

function TJclStrBinaryTree.First: IJclStrIterator;
var
  Start: PJclStrBinaryNode;
begin
  Start := FRoot;
  case GetTraverseOrder of
    toPreOrder:
      Result := TPreOrderStrItr.Create(Self, Start);
    toOrder:
      begin
        if Start <> nil then
          while Start.Left <> nil do
            Start := Start.Left;
        Result := TInOrderStrItr.Create(Self, Start);
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
        Result := TPostOrderStrItr.Create(Self, Start);
      end;
  end;
end;

function TJclStrBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclStrBinaryTree.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclStrBinaryTree.Last: IJclStrIterator;
var
  Start: PJclStrBinaryNode;
begin
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
        Result := TPreOrderStrItr.Create(Self, Start);
      end;
    toOrder:
      begin
        if Start <> nil then
          while Start.Right <> nil do
            Start := Start.Right;
        Result := TInOrderStrItr.Create(Self, Start);
      end;
    toPostOrder:
      Result := TPostOrderStrItr.Create(Self, Start);
  end;
end;

function TJclStrBinaryTree.Remove(const AString: string): Boolean;
var
  Current: PJclStrBinaryNode;
  Node: PJclStrBinaryNode;
  Save: PJclStrBinaryNode;
  Comp: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}

  procedure Correction(Node: PJclStrBinaryNode);
  var
    TempNode: PJclStrBinaryNode;
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
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AString = '' then
    Exit;
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
  {$IFDEF CLR}
  Save.Free;
  {$ELSE}
  FreeMem(Save);
  {$ENDIF CLR}
  Dec(FCount);
end;

function TJclStrBinaryTree.RemoveAll(ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
end;

function TJclStrBinaryTree.RetainAll(ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
end;

procedure TJclStrBinaryTree.RotateLeft(Node: PJclStrBinaryNode);
var
  TempNode: PJclStrBinaryNode;
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

procedure TJclStrBinaryTree.RotateRight(Node: PJclStrBinaryNode);
var
  TempNode: PJclStrBinaryNode;
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

constructor TJclBinaryTree.Create(AComparator: TCompare = nil);
begin
  inherited Create;
  if Assigned(AComparator) then
    FComparator := AComparator
  else
    FComparator := @SimpleCompare;
  FTraverseOrder := toPreOrder;
end;

destructor TJclBinaryTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclBinaryTree.Add(AObject: TObject): Boolean;
var
  NewNode: PJclBinaryNode;
  Current, Save: PJclBinaryNode;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AObject = nil then
    Exit;
  {$IFDEF CLR}
  NewNode := TJclBinaryNode.Create;
  {$ELSE}
  NewNode := AllocMem(SizeOf(TJclBinaryNode));
  {$ENDIF CLR}
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
end;

function TJclBinaryTree.AddAll(ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) or Result;
end;

procedure TJclBinaryTree.Clear;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}

{$IFDEF RECURSIVE}
  procedure FreeChild(Node: PJclBinaryNode);
  begin
    if Node.Left <> nil then
      FreeChild(Node.Left);
    if Node.Right <> nil then
      FreeChild(Node.Right);
    Node.Obj := nil; // Force Release
    FreeMem(Node);
  end;
{$ELSE}
var
  Current: PJclBinaryNode;
  Save: PJclBinaryNode;
{$ENDIF RECURSIVE}

begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  {$IFDEF RECURSIVE}
  // recursive version
  if FRoot <> nil then
  begin
    FreeChild(FRoot);
    FRoot := nil;
  end;
  {$ELSE}
  // iterative version
  Current := FRoot;
  while Current <> nil do
  begin
    if Current.Left <> nil then
      Current := Current.Left
    else
    if Current.Right <> nil then
      Current := Current.Right
    else
    begin
      Current.Obj := nil; // Force Release
      if Current.Parent = nil then // Root
      begin
        {$IFDEF CLR}
        Current.Free;
        {$ELSE}
        FreeMem(Current);
        {$ENDIF CLR}
        Current := nil;
        FRoot := nil;
      end
      else
      begin
        Save := Current;
        Current := Current.Parent;
        if Save = Current.Right then // True = from Right
        begin
          {$IFDEF CLR}
          Save.Free;
          {$ELSE}
          FreeMem(Save);
          {$ENDIF CLR}
          Current.Right := nil;
        end
        else
        begin
          {$IFDEF CLR}
          Save.Free;
          {$ELSE}
          FreeMem(Save);
          {$ENDIF CLR}
          Current.Left := nil;
        end
      end;
    end;
  end;
  {$ENDIF RECURSIVE}
  FCount := 0;
end;

function TJclBinaryTree.Clone: TObject;
var
  NewTree: TJclBinaryTree;

  function CloneNode(Node, Parent: PJclBinaryNode): PJclBinaryNode;
  begin
    if Node <> nil then
    begin
      {$IFDEF CLR}
      Result := TJclBinaryNode.Create;
      {$ELSE}
      GetMem(Result, SizeOf(TJclBinaryNode));
      {$ENDIF CLR}
      Result.Obj := Node.Obj;
      Result.Color := Node.Color;
      Result.Parent := Parent;
      Result.Left := CloneNode(Node.Left, Result); // recursive call
      Result.Right := CloneNode(Node.Right, Result); // recursive call
    end
    else
      Result := nil;
  end;

begin
  NewTree := TJclBinaryTree.Create(FComparator);
  NewTree.FCount := FCount;
  NewTree.FRoot := CloneNode(FRoot, nil);
  Result := NewTree;
end;

function TJclBinaryTree.Contains(AObject: TObject): Boolean;
var
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
  Comp: Integer;

{$IFDEF RECURSIVE}
  function ContainsChild(Node: PJclBinaryNode): Boolean;
  begin
    Result := False;
    if Node = nil then
      Exit;
    Comp := FComparator(Node.Obj, AObject);
    if Comp = 0 then
      Result := True
    else
    if Comp > 0 then
      Result := ContainsChild(Node.Left)
    else
      Result := ContainsChild(Node.Right);
  end;
{$ELSE}
var
  Current: PJclBinaryNode;
{$ENDIF RECURSIVE}

begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AObject = nil then
    Exit;
  {$IFDEF RECURSIVE}
  // recursive version
  Result := ContainsChild(FRoot);
  {$ELSE}
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
  {$ENDIF RECURSIVE}
end;

function TJclBinaryTree.ContainsAll(ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
    Result := Contains(It.Next);
end;

function TJclBinaryTree.Equals(ACollection: IJclCollection): Boolean;
var
  It, ItSelf: IJclIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
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
end;

function TJclBinaryTree.First: IJclIterator;
var
  Start: PJclBinaryNode;
begin
  Start := FRoot;
  case GetTraverseOrder of
    toPreOrder:
      Result := TPreOrderItr.Create(Self, Start);
    toOrder:
      begin
        if Start <> nil then
          while Start.Left <> nil do
            Start := Start.Left;
        Result := TInOrderItr.Create(Self, Start);
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
        Result := TPostOrderItr.Create(Self, Start);
      end;
  end;
end;

function TJclBinaryTree.GetTraverseOrder: TJclTraverseOrder;
begin
  Result := FTraverseOrder;
end;

function TJclBinaryTree.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclBinaryTree.Last: IJclIterator;
var
  Start: PJclBinaryNode;
begin
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
        Result := TPreOrderItr.Create(Self, Start);
      end;
    toOrder:
      begin
        if Start <> nil then
          while Start.Right <> nil do
            Start := Start.Right;
        Result := TInOrderItr.Create(Self, Start);
      end;
    toPostOrder:
      Result := TPostOrderItr.Create(Self, Start);
  end;
end;

function TJclBinaryTree.Remove(AObject: TObject): Boolean;
var
  Current: PJclBinaryNode;
  Node: PJclBinaryNode;
  Save: PJclBinaryNode;
  Comp: Integer;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}

  procedure Correction(Node: PJclBinaryNode);
  var
    TempNode: PJclBinaryNode;
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
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if AObject = nil then
    Exit;
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
  {$IFDEF CLR}
  Save.Free;
  {$ELSE}
  FreeMem(Save);
  {$ENDIF CLR}
  Dec(FCount);
end;

function TJclBinaryTree.RemoveAll(ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
end;

function TJclBinaryTree.RetainAll(ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  {$IFDEF THREADSAFE}
  CS: IInterface;
  {$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
end;

procedure TJclBinaryTree.RotateLeft(Node: PJclBinaryNode);
var
  TempNode: PJclBinaryNode;
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

procedure TJclBinaryTree.RotateRight(Node: PJclBinaryNode);
var
  TempNode: PJclBinaryNode;
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

// History:

// $Log$
// Revision 1.9  2005/05/05 20:08:42  ahuser
// JCL.NET support
//
// Revision 1.8  2005/03/08 08:33:15  marquardt
// overhaul of exceptions and resourcestrings, minor style cleaning
//
// Revision 1.7  2005/03/04 06:40:25  marquardt
// changed overloaded constructors to constructor with default parameter (BCB friendly)
//
// Revision 1.6  2005/03/03 08:02:56  marquardt
// various style cleanings, bugfixes and improvements
//
// Revision 1.5  2005/03/02 09:59:30  dade2004
// Added
//  -TJclStrCollection in JclContainerIntf
//        Every common methods for IJclStrCollection are implemented here
//
// -Every class that implement IJclStrCollection now derive from  TJclStrCollection instead of TJclAbstractContainer
// -Every abstract method in TJclStrCollection has been marked as "override" in descendent classes
//
// DCLAppendDelimited has been removed from JclAlgorothms, his body has been fixed for a bug and put into
// relative method in TJclStrCollection
//
// Revision 1.4  2005/02/27 11:36:20  marquardt
// fixed and secured Capacity/Grow mechanism, raise exceptions with efficient CreateResRec
//
// Revision 1.3  2005/02/27 07:27:47  marquardt
// changed interface names from I to IJcl, moved resourcestrings to JclResource.pas
//
// Revision 1.2  2005/02/26 16:42:08  marquardt
// deactivated THREADSAFE and fixed bugs stemming from that
//
// Revision 1.1  2005/02/24 03:57:10  rrossmair
// - donated DCL code, initial check-in
//

end.

