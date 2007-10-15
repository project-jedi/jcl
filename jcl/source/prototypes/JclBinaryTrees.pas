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
{$I containers\JclBinaryTrees.imp}
type
(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclIntfBinaryNode,TJclIntfBinaryTree,TJclIntfContainer,IJclIntfCollection,IJclIntfTree,IJclIntfIterator, IJclContainer\, IJclIntfEqualityComparer\, IJclIntfComparer\,,
    FCompare: TIntfCompare;,
    { IJclIntfComparer }
    function ItemsCompare(const A, B: IInterface): Integer;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF SUPPORTS_INLINE}
    { IJclIntfEqualityComparer }
    function ItemsEqual(const A, B: IInterface): Boolean;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF SUPPORTS_INLINE}
    function CreateEmptyContainer: TJclAbstractContainer; override;,
    property Compare: TIntfCompare read FCompare write FCompare;,ACompare: TIntfCompare,,const AInterface: IInterface,IInterface)*)
(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclStrBinaryNode,TJclStrBinaryTree,TJclStrAbstractCollection,IJclStrCollection,IJclStrTree,IJclStrIterator, IJclContainer\, IJclStrContainer\, IJclStrFlatContainer\, IJclStrEqualityComparer\, IJclStrComparer\,,
    FCompare: TStrCompare;,
    { IJclStrComparer }
    function ItemsCompare(const A, B: string): Integer;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF SUPPORTS_INLINE}
    { IJclStrEqualityComparer }
    function ItemsEqual(const A, B: string): Boolean;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF SUPPORTS_INLINE}
    function CreateEmptyContainer: TJclAbstractContainer; override;,
    property Compare: TStrCompare read FCompare write FCompare;,ACompare: TStrCompare, override;,const AString: string,string)*)
(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclBinaryNode,TJclBinaryTree,TJclContainer,IJclCollection,IJclTree,IJclIterator, IJclContainer\, IJclObjectOwner\, IJclEqualityComparer\, IJclComparer\,,
    FCompare: TCompare;,
    { IJclComparer }
    function ItemsCompare(A, B: TObject): Integer;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF SUPPORTS_INLINE}
    { IJclEqualityComparer }
    function ItemsEqual(A, B: TObject): Boolean;{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF SUPPORTS_INLINE}
    function CreateEmptyContainer: TJclAbstractContainer; override;,
    property Compare: TCompare read FCompare write FCompare;,ACompare: TCompare; AOwnsObjects: Boolean,,AObject: TObject,TObject)*)

  {$IFDEF SUPPORTS_GENERICS}
(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclBinaryNode<T>,TJclBinaryTree<T>,TJclContainer<T>,IJclCollection<T>,IJclTree<T>,IJclIterator<T>, IJclContainer\, IJclItemOwner<T>\, IJclEqualityComparer<T>\, IJclComparer<T>\,,,,,AOwnsItems: Boolean,,const AItem: T,T)*)

  // E = External helper to compare items
  TJclBinaryTreeE<T> = class(TJclBinaryTree<T>, IJclCollection<T>, IJclTree<T>, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  private
    FComparer: IComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
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
  TJclBinaryTreeF<T> = class(TJclBinaryTree<T>, IJclCollection<T>, IJclTree<T>, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  private
    FCompare: TCompare<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
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
  TJclBinaryTreeI<T: IComparable<T>> = class(TJclBinaryTree<T>, IJclCollection<T>, IJclTree<T>, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  protected
    function CreateEmptyContainer: TJclAbstractContainer; override;
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

{$JPPEXPANDMACRO JCLBINARYTREEITR(TIntfItr,TPreOrderIntfItr,TInOrderIntfItr,TPostOrderIntfItr,IJclIntfIterator,IJclIntfCollection,IJclIntfEqualityComparer,TJclIntfBinaryNode,const AInterface: IInterface,IInterface,AInterface,nil,GetObject,SetObject,FreeObject)}
{$JPPEXPANDMACRO JCLBINARYTREEITR(TStrItr,TPreOrderStrItr,TInOrderStrItr,TPostOrderStrItr,IJclStrIterator,IJclStrCollection,IJclStrEqualityComparer,TJclStrBinaryNode,const AString: string,string,AString,'',GetString,SetString,FreeString)}
{$JPPEXPANDMACRO JCLBINARYTREEITR(TItr,TPreOrderItr,TInOrderItr,TPostOrderItr,IJclIterator,IJclCollection,IJclEqualityComparer,TJclBinaryNode,AObject: TObject,TObject,AObject,nil,GetObject,SetObject,FreeObject)}
{$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO JCLBINARYTREEITR(TItr<T>,TPreOrderItr<T>,TInOrderItr<T>,TPostOrderItr<T>,IJclIterator<T>,IJclCollection<T>,IJclEqualityComparer<T>,TJclBinaryNode<T>,const AItem: T,T,AItem,Default(T),GetItem,SetItem,FreeItem)}
{$ENDIF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO ITEMSCOMPARE
function TJclIntfBinaryTree.ItemsCompare(const A, B: IInterface): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO ITEMSEQUAL
function TJclIntfBinaryTree.ItemsEqual(const A, B: IInterface): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfBinaryTree.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclIntfBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclIntfBinaryTree,TJclIntfBinaryNode,TPreOrderIntfItr,TInOrderIntfItr,TPostOrderIntfItr,IJclIntfCollection,IJclIntfIterator,ACompare: TIntfCompare,
  FCompare := ACompare;,,const AInterface: IInterface,AInterface,nil,FreeObject)*)
{$JPPUNDEFMACRO ITEMSCOMPARE}
{$JPPUNDEFMACRO ITEMSEQUAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO ITEMSCOMPARE
function TJclStrBinaryTree.ItemsCompare(const A, B: string): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO ITEMSEQUAL
function TJclStrBinaryTree.ItemsEqual(const A, B: string): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclStrBinaryTree.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStrBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclStrBinaryTree,TJclStrBinaryNode,TPreOrderStrItr,TInOrderStrItr,TPostOrderStrItr,IJclStrCollection,IJclStrIterator,ACompare: TStrCompare,
  FCompare := ACompare;,,const AString: string,AString,'',FreeString)*)
{$JPPUNDEFMACRO ITEMSCOMPARE}
{$JPPUNDEFMACRO ITEMSEQUAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO ITEMSCOMPARE
function TJclBinaryTree.ItemsCompare(A, B: TObject): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO ITEMSEQUAL
function TJclBinaryTree.ItemsEqual(A, B: TObject): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclBinaryTree.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclBinaryTree.Create(FCompare, False);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclBinaryTree,TJclBinaryNode,TPreOrderItr,TInOrderItr,TPostOrderItr,IJclCollection,IJclIterator,ACompare: TCompare; AOwnsObjects: Boolean,
  FCompare := ACompare;,\, AOwnsObjects,AObject: TObject,AObject,nil,FreeObject)*)
{$JPPUNDEFMACRO ITEMSCOMPARE}
{$JPPUNDEFMACRO ITEMSEQUAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$IFDEF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO ITEMSCOMPARE}
{$JPPDEFINEMACRO ITEMSEQUAL}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclBinaryTree<T>,TJclBinaryNode<T>,TPreOrderItr<T>,TInOrderItr<T>,TPostOrderItr<T>,IJclCollection<T>,IJclIterator<T>,AOwnsItems: Boolean,,\, AOwnsItems,const AItem: T,AItem,Default(T),FreeItem)*)
{$JPPUNDEFMACRO ITEMSCOMPARE}
{$JPPUNDEFMACRO ITEMSEQUAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

//=== { TJclBinaryTreeE<T> } =================================================

constructor TJclBinaryTreeE<T>.Create(const AComparer: IComparer<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FComparer := AComparer;
end;

procedure TJclBinaryTreeE<T>.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclBinaryTreeE<T> then
    TJclBinaryTreeE<T>(Dest).FComparer := FComparer;
end;

function TJclBinaryTreeE<T>.CreateEmptyContainer: TJclAbstractContainer;
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

procedure TJclBinaryTreeF<T>.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclBinaryTreeF<T> then
    TJclBinaryTreeF<T>(Dest).FCompare := FCompare;
end;

function TJclBinaryTreeF<T>.CreateEmptyContainer: TJclAbstractContainer;
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

function TJclBinaryTreeI<T>.CreateEmptyContainer: TJclAbstractContainer;
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

