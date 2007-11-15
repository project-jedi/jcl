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
(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclIntfBinaryNode,TJclIntfBinaryTree,TJclIntfAbstractContainer,IJclIntfCollection,IJclIntfTree,IJclIntfIterator, IJclIntfEqualityComparer\, IJclIntfComparer\,,
    FCompare: TIntfCompare;,
    { IJclIntfComparer }
    function ItemsCompare(const A, B: IInterface): Integer;
    { IJclIntfEqualityComparer }
    function ItemsEqual(const A, B: IInterface): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,
    property Compare: TIntfCompare read FCompare write FCompare;,ACompare: TIntfCompare,,const AInterface: IInterface,IInterface)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclAnsiStrBinaryNode,TJclAnsiStrBinaryTree,TJclAnsiStrAbstractCollection,IJclAnsiStrCollection,IJclAnsiStrTree,IJclAnsiStrIterator, IJclStrContainer\, IJclAnsiStrContainer\, IJclAnsiStrFlatContainer\, IJclAnsiStrEqualityComparer\, IJclAnsiStrComparer\,,
    FCompare: TAnsiStrCompare;,
    { IJclAnsiStrComparer }
    function ItemsCompare(const A, B: AnsiString): Integer;
    { IJclAnsiStrEqualityComparer }
    function ItemsEqual(const A, B: AnsiString): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,
    property Compare: TAnsiStrCompare read FCompare write FCompare;,ACompare: TAnsiStrCompare, override;,const AString: AnsiString,AnsiString)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclWideStrBinaryNode,TJclWideStrBinaryTree,TJclWideStrAbstractCollection,IJclWideStrCollection,IJclWideStrTree,IJclWideStrIterator, IJclStrContainer\, IJclWideStrContainer\, IJclWideStrFlatContainer\, IJclWideStrEqualityComparer\, IJclWideStrComparer\,,
    FCompare: TWideStrCompare;,
    { IJclWideStrComparer }
    function ItemsCompare(const A, B: WideString): Integer;
    { IJclWideStrEqualityComparer }
    function ItemsEqual(const A, B: WideString): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,
    property Compare: TWideStrCompare read FCompare write FCompare;,ACompare: TWideStrCompare, override;,const AString: WideString,WideString)*)

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrBinaryTree = TJclAnsiStrBinaryTree;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrBinaryTree = TJclWideStrBinaryTree;
  {$ENDIF CONTAINER_WIDESTR}

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclSingleBinaryNode,TJclSingleBinaryTree,TJclSingleAbstractContainer,IJclSingleCollection,IJclSingleTree,IJclSingleIterator, IJclSingleContainer\, IJclSingleEqualityComparer\, IJclSingleComparer\,,
    FCompare: TSingleCompare;,
    { IJclSingleComparer }
    function ItemsCompare(const A, B: Single): Integer;
    { IJclSingleEqualityComparer }
    function ItemsEqual(const A, B: Single): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,
    property Compare: TSingleCompare read FCompare write FCompare;,ACompare: TSingleCompare,,const AValue: Single,Single)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclDoubleBinaryNode,TJclDoubleBinaryTree,TJclDoubleAbstractContainer,IJclDoubleCollection,IJclDoubleTree,IJclDoubleIterator, IJclDoubleContainer\, IJclDoubleEqualityComparer\, IJclDoubleComparer\,,
    FCompare: TDoubleCompare;,
    { IJclDoubleComparer }
    function ItemsCompare(const A, B: Double): Integer;
    { IJclDoubleEqualityComparer }
    function ItemsEqual(const A, B: Double): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,
    property Compare: TDoubleCompare read FCompare write FCompare;,ACompare: TDoubleCompare,,const AValue: Double,Double)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclExtendedBinaryNode,TJclExtendedBinaryTree,TJclExtendedAbstractContainer,IJclExtendedCollection,IJclExtendedTree,IJclExtendedIterator, IJclExtendedContainer\, IJclExtendedEqualityComparer\, IJclExtendedComparer\,,
    FCompare: TExtendedCompare;,
    { IJclExtendedComparer }
    function ItemsCompare(const A, B: Extended): Integer;
    { IJclExtendedEqualityComparer }
    function ItemsEqual(const A, B: Extended): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,
    property Compare: TExtendedCompare read FCompare write FCompare;,ACompare: TExtendedCompare,,const AValue: Extended,Extended)*)

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatBinaryTree = TJclExtendedBinaryTree;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatBinaryTree = TJclDoubleBinaryTree;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatBinaryTree = TJclSingleBinaryTree;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclIntegerBinaryNode,TJclIntegerBinaryTree,TJclIntegerAbstractContainer,IJclIntegerCollection,IJclIntegerTree,IJclIntegerIterator, IJclIntegerEqualityComparer\, IJclIntegerComparer\,,
    FCompare: TIntegerCompare;,
    { IJclIntegerComparer }
    function ItemsCompare(A, B: Integer): Integer;
    { IJclIntegerEqualityComparer }
    function ItemsEqual(A, B: Integer): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,
    property Compare: TIntegerCompare read FCompare write FCompare;,ACompare: TIntegerCompare,,AValue: Integer,Integer)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclCardinalBinaryNode,TJclCardinalBinaryTree,TJclCardinalAbstractContainer,IJclCardinalCollection,IJclCardinalTree,IJclCardinalIterator, IJclCardinalEqualityComparer\, IJclCardinalComparer\,,
    FCompare: TCardinalCompare;,
    { IJclCardinalComparer }
    function ItemsCompare(A, B: Cardinal): Integer;
    { IJclCardinalEqualityComparer }
    function ItemsEqual(A, B: Cardinal): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,
    property Compare: TCardinalCompare read FCompare write FCompare;,ACompare: TCardinalCompare,,AValue: Cardinal,Cardinal)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclInt64BinaryNode,TJclInt64BinaryTree,TJclInt64AbstractContainer,IJclInt64Collection,IJclInt64Tree,IJclInt64Iterator, IJclInt64EqualityComparer\, IJclInt64Comparer\,,
    FCompare: TInt64Compare;,
    { IJclInt64Comparer }
    function ItemsCompare(const A, B: Int64): Integer;
    { IJclInt64EqualityComparer }
    function ItemsEqual(const A, B: Int64): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,
    property Compare: TInt64Compare read FCompare write FCompare;,ACompare: TInt64Compare,,const AValue: Int64,Int64)*)

  {$IFNDEF CLR}
(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclPtrBinaryNode,TJclPtrBinaryTree,TJclPtrAbstractContainer,IJclPtrCollection,IJclPtrTree,IJclPtrIterator, IJclPtrEqualityComparer\, IJclPtrComparer\,,
    FCompare: TPtrCompare;,
    { IJclPtrComparer }
    function ItemsCompare(A, B: Pointer): Integer;
    { IJclPtrEqualityComparer }
    function ItemsEqual(A, B: Pointer): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,
    property Compare: TPtrCompare read FCompare write FCompare;,ACompare: TPtrCompare,,APtr: Pointer,Pointer)*)
  {$ENDIF ~CLR}

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclBinaryNode,TJclBinaryTree,TJclAbstractContainer,IJclCollection,IJclTree,IJclIterator, IJclObjectOwner\, IJclEqualityComparer\, IJclComparer\,,
    FCompare: TCompare;,
    { IJclComparer }
    function ItemsCompare(A, B: TObject): Integer;
    { IJclEqualityComparer }
    function ItemsEqual(A, B: TObject): Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,
    property Compare: TCompare read FCompare write FCompare;,ACompare: TCompare; AOwnsObjects: Boolean,,AObject: TObject,TObject)*)

  {$IFDEF SUPPORTS_GENERICS}
(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclBinaryNode<T>,TJclBinaryTree<T>,TJclAbstractContainer<T>,IJclCollection<T>,IJclTree<T>,IJclIterator<T>, IJclItemOwner<T>\, IJclEqualityComparer<T>\, IJclComparer<T>\,,,,,AOwnsItems: Boolean,,const AItem: T,T)*)

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

{$JPPEXPANDMACRO JCLBINARYTREEITR(TIntfItr,TPreOrderIntfItr,TInOrderIntfItr,TPostOrderIntfItr,IJclIntfIterator,IJclIntfCollection,IJclIntfEqualityComparer,TJclIntfBinaryNode,const AInterface: IInterface,IInterface,AInterface,nil,GetObject,SetObject,FreeObject)}
{$JPPEXPANDMACRO JCLBINARYTREEITR(TAnsiStrItr,TPreOrderAnsiStrItr,TInOrderAnsiStrItr,TPostOrderAnsiStrItr,IJclAnsiStrIterator,IJclAnsiStrCollection,IJclAnsiStrEqualityComparer,TJclAnsiStrBinaryNode,const AString: AnsiString,AnsiString,AString,'',GetString,SetString,FreeString)}
{$JPPEXPANDMACRO JCLBINARYTREEITR(TWideStrItr,TPreOrderWideStrItr,TInOrderWideStrItr,TPostOrderWideStrItr,IJclWideStrIterator,IJclWideStrCollection,IJclWideStrEqualityComparer,TJclWideStrBinaryNode,const AString: WideString,WideString,AString,'',GetString,SetString,FreeString)}
{$JPPEXPANDMACRO JCLBINARYTREEITR(TSingleItr,TPreOrderSingleItr,TInOrderSingleItr,TPostOrderSingleItr,IJclSingleIterator,IJclSingleCollection,IJclSingleEqualityComparer,TJclSingleBinaryNode,const AValue: Single,Single,AValue,0.0,GetValue,SetValue,FreeSingle)}
{$JPPEXPANDMACRO JCLBINARYTREEITR(TDoubleItr,TPreOrderDoubleItr,TInOrderDoubleItr,TPostOrderDoubleItr,IJclDoubleIterator,IJclDoubleCollection,IJclDoubleEqualityComparer,TJclDoubleBinaryNode,const AValue: Double,Double,AValue,0.0,GetValue,SetValue,FreeDouble)}
{$JPPEXPANDMACRO JCLBINARYTREEITR(TExtendedItr,TPreOrderExtendedItr,TInOrderExtendedItr,TPostOrderExtendedItr,IJclExtendedIterator,IJclExtendedCollection,IJclExtendedEqualityComparer,TJclExtendedBinaryNode,const AValue: Extended,Extended,AValue,0.0,GetValue,SetValue,FreeExtended)}
{$JPPEXPANDMACRO JCLBINARYTREEITR(TIntegerItr,TPreOrderIntegerItr,TInOrderIntegerItr,TPostOrderIntegerItr,IJclIntegerIterator,IJclIntegerCollection,IJclIntegerEqualityComparer,TJclIntegerBinaryNode,AValue: Integer,Integer,AValue,0,GetValue,SetValue,FreeInteger)}
{$JPPEXPANDMACRO JCLBINARYTREEITR(TCardinalItr,TPreOrderCardinalItr,TInOrderCardinalItr,TPostOrderCardinalItr,IJclCardinalIterator,IJclCardinalCollection,IJclCardinalEqualityComparer,TJclCardinalBinaryNode,AValue: Cardinal,Cardinal,AValue,0,GetValue,SetValue,FreeCardinal)}
{$JPPEXPANDMACRO JCLBINARYTREEITR(TInt64Itr,TPreOrderInt64Itr,TInOrderInt64Itr,TPostOrderInt64Itr,IJclInt64Iterator,IJclInt64Collection,IJclInt64EqualityComparer,TJclInt64BinaryNode,const AValue: Int64,Int64,AValue,0,GetValue,SetValue,FreeInt64)}
{$IFNDEF CLR}
{$JPPEXPANDMACRO JCLBINARYTREEITR(TPtrItr,TPreOrderPtrItr,TInOrderPtrItr,TPostOrderPtrItr,IJclPtrIterator,IJclPtrCollection,IJclPtrEqualityComparer,TJclPtrBinaryNode,APtr: Pointer,Pointer,APtr,nil,GetPtr,SetPtr,FreePtr)}
{$ENDIF ~CLR}
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
function TJclIntfBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
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
function TJclAnsiStrBinaryTree.ItemsCompare(const A, B: AnsiString): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO ITEMSEQUAL
function TJclAnsiStrBinaryTree.ItemsEqual(const A, B: AnsiString): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclAnsiStrBinaryTree,TJclAnsiStrBinaryNode,TPreOrderAnsiStrItr,TInOrderAnsiStrItr,TPostOrderAnsiStrItr,IJclAnsiStrCollection,IJclAnsiStrIterator,ACompare: TAnsiStrCompare,
  FCompare := ACompare;,,const AString: AnsiString,AString,'',FreeString)*)
{$JPPUNDEFMACRO ITEMSCOMPARE}
{$JPPUNDEFMACRO ITEMSEQUAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO ITEMSCOMPARE
function TJclWideStrBinaryTree.ItemsCompare(const A, B: WideString): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO ITEMSEQUAL
function TJclWideStrBinaryTree.ItemsEqual(const A, B: WideString): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclWideStrBinaryTree,TJclWideStrBinaryNode,TPreOrderWideStrItr,TInOrderWideStrItr,TPostOrderWideStrItr,IJclWideStrCollection,IJclWideStrIterator,ACompare: TWideStrCompare,
  FCompare := ACompare;,,const AString: WideString,AString,'',FreeString)*)
{$JPPUNDEFMACRO ITEMSCOMPARE}
{$JPPUNDEFMACRO ITEMSEQUAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO ITEMSCOMPARE
function TJclSingleBinaryTree.ItemsCompare(const A, B: Single): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO ITEMSEQUAL
function TJclSingleBinaryTree.ItemsEqual(const A, B: Single): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclSingleBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclSingleBinaryTree,TJclSingleBinaryNode,TPreOrderSingleItr,TInOrderSingleItr,TPostOrderSingleItr,IJclSingleCollection,IJclSingleIterator,ACompare: TSingleCompare,
  FCompare := ACompare;,,const AValue: Single,AValue,0.0,FreeSingle)*)
{$JPPUNDEFMACRO ITEMSCOMPARE}
{$JPPUNDEFMACRO ITEMSEQUAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO ITEMSCOMPARE
function TJclDoubleBinaryTree.ItemsCompare(const A, B: Double): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO ITEMSEQUAL
function TJclDoubleBinaryTree.ItemsEqual(const A, B: Double): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclDoubleBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclDoubleBinaryTree,TJclDoubleBinaryNode,TPreOrderDoubleItr,TInOrderDoubleItr,TPostOrderDoubleItr,IJclDoubleCollection,IJclDoubleIterator,ACompare: TDoubleCompare,
  FCompare := ACompare;,,const AValue: Double,AValue,0.0,FreeDouble)*)
{$JPPUNDEFMACRO ITEMSCOMPARE}
{$JPPUNDEFMACRO ITEMSEQUAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO ITEMSCOMPARE
function TJclExtendedBinaryTree.ItemsCompare(const A, B: Extended): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO ITEMSEQUAL
function TJclExtendedBinaryTree.ItemsEqual(const A, B: Extended): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclExtendedBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclExtendedBinaryTree,TJclExtendedBinaryNode,TPreOrderExtendedItr,TInOrderExtendedItr,TPostOrderExtendedItr,IJclExtendedCollection,IJclExtendedIterator,ACompare: TExtendedCompare,
  FCompare := ACompare;,,const AValue: Extended,AValue,0.0,FreeExtended)*)
{$JPPUNDEFMACRO ITEMSCOMPARE}
{$JPPUNDEFMACRO ITEMSEQUAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO ITEMSCOMPARE
function TJclIntegerBinaryTree.ItemsCompare(A, B: Integer): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO ITEMSEQUAL
function TJclIntegerBinaryTree.ItemsEqual(A, B: Integer): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntegerBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclIntegerBinaryTree,TJclIntegerBinaryNode,TPreOrderIntegerItr,TInOrderIntegerItr,TPostOrderIntegerItr,IJclIntegerCollection,IJclIntegerIterator,ACompare: TIntegerCompare,
  FCompare := ACompare;,,AValue: Integer,AValue,0,FreeInteger)*)
{$JPPUNDEFMACRO ITEMSCOMPARE}
{$JPPUNDEFMACRO ITEMSEQUAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO ITEMSCOMPARE
function TJclCardinalBinaryTree.ItemsCompare(A, B: Cardinal): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO ITEMSEQUAL
function TJclCardinalBinaryTree.ItemsEqual(A, B: Cardinal): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclCardinalBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclCardinalBinaryTree,TJclCardinalBinaryNode,TPreOrderCardinalItr,TInOrderCardinalItr,TPostOrderCardinalItr,IJclCardinalCollection,IJclCardinalIterator,ACompare: TCardinalCompare,
  FCompare := ACompare;,,AValue: Cardinal,AValue,0,FreeCardinal)*)
{$JPPUNDEFMACRO ITEMSCOMPARE}
{$JPPUNDEFMACRO ITEMSEQUAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO ITEMSCOMPARE
function TJclInt64BinaryTree.ItemsCompare(const A, B: Int64): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO ITEMSEQUAL
function TJclInt64BinaryTree.ItemsEqual(const A, B: Int64): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclInt64BinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64BinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclInt64BinaryTree,TJclInt64BinaryNode,TPreOrderInt64Itr,TInOrderInt64Itr,TPostOrderInt64Itr,IJclInt64Collection,IJclInt64Iterator,ACompare: TInt64Compare,
  FCompare := ACompare;,,const AValue: Int64,AValue,0,FreeInt64)*)
{$JPPUNDEFMACRO ITEMSCOMPARE}
{$JPPUNDEFMACRO ITEMSEQUAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$IFNDEF CLR}
{$JPPDEFINEMACRO ITEMSCOMPARE
function TJclPtrBinaryTree.ItemsCompare(A, B: Pointer): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;
}
{$JPPDEFINEMACRO ITEMSEQUAL
function TJclPtrBinaryTree.ItemsEqual(A, B: Pointer): Boolean;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclPtrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrBinaryTree.Create(FCompare);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclPtrBinaryTree,TJclPtrBinaryNode,TPreOrderPtrItr,TInOrderPtrItr,TPostOrderPtrItr,IJclPtrCollection,IJclPtrIterator,ACompare: TPtrCompare,
  FCompare := ACompare;,,APtr: Pointer,APtr,nil,FreePointer)*)
{$JPPUNDEFMACRO ITEMSCOMPARE}
{$JPPUNDEFMACRO ITEMSEQUAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$ENDIF ~CLR}

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
function TJclBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
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
