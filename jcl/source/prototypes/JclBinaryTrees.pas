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
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclAlgorithms, JclContainerIntf, JclSynch;
{$I containers\JclContainerCommon.imp}
{$I containers\JclBinaryTrees.imp}
{$I containers\JclBinaryTrees.int}
type
  TItrStart = (isFirst, isLast, isRoot);

(*$JPPEXPANDMACRO JCLBINARYTREETYPESINT(TJclIntfBinaryNode,IInterface)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclIntfBinaryNode,TJclIntfBinaryTree,TJclIntfAbstractContainer,IJclIntfCollection,IJclIntfTree,IJclIntfIterator,IJclIntfTreeIterator, IJclIntfEqualityComparer\, IJclIntfComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,ACompare: TIntfCompare,,const ,AInterface,IInterface)*)

{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TJclIntfBinaryTreeIterator,TJclPreOrderIntfBinaryTreeIterator,TJclInOrderIntfBinaryTreeIterator,TJclPostOrderIntfBinaryTreeIterator,IJclIntfIterator,IJclIntfTreeIterator,IJclIntfBinaryTreeIterator,IJclIntfCollection,IJclIntfEqualityComparer,TJclIntfBinaryNode,const ,AInterface,IInterface,GetObject,SetObject)}

(*$JPPEXPANDMACRO JCLBINARYTREETYPESINT(TJclAnsiStrBinaryNode,AnsiString)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclAnsiStrBinaryNode,TJclAnsiStrBinaryTree,TJclAnsiStrAbstractCollection,IJclAnsiStrCollection,IJclAnsiStrTree,IJclAnsiStrIterator,IJclAnsiStrTreeIterator, IJclStrContainer\, IJclAnsiStrContainer\, IJclAnsiStrFlatContainer\, IJclAnsiStrEqualityComparer\, IJclAnsiStrComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,ACompare: TAnsiStrCompare, override;,const ,AString,AnsiString)*)

{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TJclAnsiStrBinaryTreeIterator,TJclPreOrderAnsiStrBinaryTreeIterator,TJclInOrderAnsiStrBinaryTreeIterator,TJclPostOrderAnsiStrBinaryTreeIterator,IJclAnsiStrIterator,IJclAnsiStrTreeIterator,IJclAnsiStrBinaryTreeIterator,IJclAnsiStrCollection,IJclAnsiStrEqualityComparer,TJclAnsiStrBinaryNode,const ,AString,AnsiString,GetString,SetString)}

(*$JPPEXPANDMACRO JCLBINARYTREETYPESINT(TJclWideStrBinaryNode,WideString)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclWideStrBinaryNode,TJclWideStrBinaryTree,TJclWideStrAbstractCollection,IJclWideStrCollection,IJclWideStrTree,IJclWideStrIterator,IJclWideStrTreeIterator, IJclStrContainer\, IJclWideStrContainer\, IJclWideStrFlatContainer\, IJclWideStrEqualityComparer\, IJclWideStrComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,ACompare: TWideStrCompare, override;,const ,AString,WideString)*)

{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TJclWideStrBinaryTreeIterator,TJclPreOrderWideStrBinaryTreeIterator,TJclInOrderWideStrBinaryTreeIterator,TJclPostOrderWideStrBinaryTreeIterator,IJclWideStrIterator,IJclWideStrTreeIterator,IJclWideStrBinaryTreeIterator,IJclWideStrCollection,IJclWideStrEqualityComparer,TJclWideStrBinaryNode,const ,AString,WideString,GetString,SetString)}


{$IFDEF SUPPORTS_UNICODE_STRING}
(*$JPPEXPANDMACRO JCLBINARYTREETYPESINT(TJclUnicodeStrBinaryNode,UnicodeString)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclUnicodeStrBinaryNode,TJclUnicodeStrBinaryTree,TJclUnicodeStrAbstractCollection,IJclUnicodeStrCollection,IJclUnicodeStrTree,IJclUnicodeStrIterator,IJclUnicodeStrTreeIterator, IJclStrContainer\, IJclUnicodeStrContainer\, IJclUnicodeStrFlatContainer\, IJclUnicodeStrEqualityComparer\, IJclUnicodeStrComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,ACompare: TUnicodeStrCompare, override;,const ,AString,UnicodeString)*)

{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TJclUnicodeStrBinaryTreeIterator,TJclPreOrderUnicodeStrBinaryTreeIterator,TJclInOrderUnicodeStrBinaryTreeIterator,TJclPostOrderUnicodeStrBinaryTreeIterator,IJclUnicodeStrIterator,IJclUnicodeStrTreeIterator,IJclUnicodeStrBinaryTreeIterator,IJclUnicodeStrCollection,IJclUnicodeStrEqualityComparer,TJclUnicodeStrBinaryNode,const ,AString,UnicodeString,GetString,SetString)}
{$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrBinaryTree = TJclAnsiStrBinaryTree;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrBinaryTree = TJclWideStrBinaryTree;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrBinaryTree = TJclUnicodeStrBinaryTree;
  {$ENDIF CONTAINER_UNICODESTR}

(*$JPPEXPANDMACRO JCLBINARYTREETYPESINT(TJclSingleBinaryNode,Single)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclSingleBinaryNode,TJclSingleBinaryTree,TJclSingleAbstractContainer,IJclSingleCollection,IJclSingleTree,IJclSingleIterator,IJclSingleTreeIterator, IJclSingleContainer\, IJclSingleEqualityComparer\, IJclSingleComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,ACompare: TSingleCompare,,const ,AValue,Single)*)

{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TJclSingleBinaryTreeIterator,TJclPreOrderSingleBinaryTreeIterator,TJclInOrderSingleBinaryTreeIterator,TJclPostOrderSingleBinaryTreeIterator,IJclSingleIterator,IJclSingleTreeIterator,IJclSingleBinaryTreeIterator,IJclSingleCollection,IJclSingleEqualityComparer,TJclSingleBinaryNode,const ,AValue,Single,GetValue,SetValue)}

(*$JPPEXPANDMACRO JCLBINARYTREETYPESINT(TJclDoubleBinaryNode,Double)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclDoubleBinaryNode,TJclDoubleBinaryTree,TJclDoubleAbstractContainer,IJclDoubleCollection,IJclDoubleTree,IJclDoubleIterator,IJclDoubleTreeIterator, IJclDoubleContainer\, IJclDoubleEqualityComparer\, IJclDoubleComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,ACompare: TDoubleCompare,,const ,AValue,Double)*)

{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TJclDoubleBinaryTreeIterator,TJclPreOrderDoubleBinaryTreeIterator,TJclInOrderDoubleBinaryTreeIterator,TJclPostOrderDoubleBinaryTreeIterator,IJclDoubleIterator,IJclDoubleTreeIterator,IJclDoubleBinaryTreeIterator,IJclDoubleCollection,IJclDoubleEqualityComparer,TJclDoubleBinaryNode,const ,AValue,Double,GetValue,SetValue)}

(*$JPPEXPANDMACRO JCLBINARYTREETYPESINT(TJclExtendedBinaryNode,Extended)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclExtendedBinaryNode,TJclExtendedBinaryTree,TJclExtendedAbstractContainer,IJclExtendedCollection,IJclExtendedTree,IJclExtendedIterator,IJclExtendedTreeIterator, IJclExtendedContainer\, IJclExtendedEqualityComparer\, IJclExtendedComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,ACompare: TExtendedCompare,,const ,AValue,Extended)*)

{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TJclExtendedBinaryTreeIterator,TJclPreOrderExtendedBinaryTreeIterator,TJclInOrderExtendedBinaryTreeIterator,TJclPostOrderExtendedBinaryTreeIterator,IJclExtendedIterator,IJclExtendedTreeIterator,IJclExtendedBinaryTreeIterator,IJclExtendedCollection,IJclExtendedEqualityComparer,TJclExtendedBinaryNode,const ,AValue,Extended,GetValue,SetValue)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatBinaryTree = TJclExtendedBinaryTree;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatBinaryTree = TJclDoubleBinaryTree;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatBinaryTree = TJclSingleBinaryTree;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLBINARYTREETYPESINT(TJclIntegerBinaryNode,Integer)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclIntegerBinaryNode,TJclIntegerBinaryTree,TJclIntegerAbstractContainer,IJclIntegerCollection,IJclIntegerTree,IJclIntegerIterator,IJclIntegerTreeIterator, IJclIntegerEqualityComparer\, IJclIntegerComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,ACompare: TIntegerCompare,,,AValue,Integer)*)

{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TJclIntegerBinaryTreeIterator,TJclPreOrderIntegerBinaryTreeIterator,TJclInOrderIntegerBinaryTreeIterator,TJclPostOrderIntegerBinaryTreeIterator,IJclIntegerIterator,IJclIntegerTreeIterator,IJclIntegerBinaryTreeIterator,IJclIntegerCollection,IJclIntegerEqualityComparer,TJclIntegerBinaryNode,,AValue,Integer,GetValue,SetValue)}

(*$JPPEXPANDMACRO JCLBINARYTREETYPESINT(TJclCardinalBinaryNode,Cardinal)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclCardinalBinaryNode,TJclCardinalBinaryTree,TJclCardinalAbstractContainer,IJclCardinalCollection,IJclCardinalTree,IJclCardinalIterator,IJclCardinalTreeIterator, IJclCardinalEqualityComparer\, IJclCardinalComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,ACompare: TCardinalCompare,,,AValue,Cardinal)*)

{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TJclCardinalBinaryTreeIterator,TJclPreOrderCardinalBinaryTreeIterator,TJclInOrderCardinalBinaryTreeIterator,TJclPostOrderCardinalBinaryTreeIterator,IJclCardinalIterator,IJclCardinalTreeIterator,IJclCardinalBinaryTreeIterator,IJclCardinalCollection,IJclCardinalEqualityComparer,TJclCardinalBinaryNode,,AValue,Cardinal,GetValue,SetValue)}

(*$JPPEXPANDMACRO JCLBINARYTREETYPESINT(TJclInt64BinaryNode,Int64)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclInt64BinaryNode,TJclInt64BinaryTree,TJclInt64AbstractContainer,IJclInt64Collection,IJclInt64Tree,IJclInt64Iterator,IJclInt64TreeIterator, IJclInt64EqualityComparer\, IJclInt64Comparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,ACompare: TInt64Compare,,const ,AValue,Int64)*)

{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TJclInt64BinaryTreeIterator,TJclPreOrderInt64BinaryTreeIterator,TJclInOrderInt64BinaryTreeIterator,TJclPostOrderInt64BinaryTreeIterator,IJclInt64Iterator,IJclInt64TreeIterator,IJclInt64BinaryTreeIterator,IJclInt64Collection,IJclInt64EqualityComparer,TJclInt64BinaryNode,const ,AValue,Int64,GetValue,SetValue)}

(*$JPPEXPANDMACRO JCLBINARYTREETYPESINT(TJclPtrBinaryNode,Pointer)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclPtrBinaryNode,TJclPtrBinaryTree,TJclPtrAbstractContainer,IJclPtrCollection,IJclPtrTree,IJclPtrIterator,IJclPtrTreeIterator, IJclPtrEqualityComparer\, IJclPtrComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,ACompare: TPtrCompare,,,APtr,Pointer)*)

{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TJclPtrBinaryTreeIterator,TJclPreOrderPtrBinaryTreeIterator,TJclInOrderPtrBinaryTreeIterator,TJclPostOrderPtrBinaryTreeIterator,IJclPtrIterator,IJclPtrTreeIterator,IJclPtrBinaryTreeIterator,IJclPtrCollection,IJclPtrEqualityComparer,TJclPtrBinaryNode,,APtr,Pointer,GetPointer,SetPointer)}

(*$JPPEXPANDMACRO JCLBINARYTREETYPESINT(TJclBinaryNode,TObject)*)

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TJclBinaryNode,TJclBinaryTree,TJclAbstractContainer,IJclCollection,IJclTree,IJclIterator,IJclTreeIterator, IJclObjectOwner\, IJclEqualityComparer\, IJclComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,ACompare: TCompare; AOwnsObjects: Boolean,,,AObject,TObject)*)

{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TJclBinaryTreeIterator,TJclPreOrderBinaryTreeIterator,TJclInOrderBinaryTreeIterator,TJclPostOrderBinaryTreeIterator,IJclIterator,IJclTreeIterator,IJclBinaryTreeIterator,IJclCollection,IJclEqualityComparer,TJclBinaryNode,,AObject,TObject,GetObject,SetObject)}

  {$IFDEF SUPPORTS_GENERICS}
(*$JPPEXPANDMACRO JCLBINARYTREETYPESINT(TJclBinaryNode<T>,T)*)

  TJclBinaryTreeIterator<T> = class;
  TJclPreOrderBinaryTreeIterator<T> = class;
  TJclInOrderBinaryTreeIterator<T> = class;
  TJclPostOrderBinaryTreeIterator<T> = class;

(*$JPPEXPANDMACRO JCLBINARYTREEINT(TBinaryNode,TJclBinaryTree<T>,TJclAbstractContainer<T>,IJclCollection<T>,IJclTree<T>,IJclIterator<T>,IJclTreeIterator<T>, IJclItemOwner<T>\, IJclEqualityComparer<T>\, IJclComparer<T>\,,
  protected
    type
      TBinaryNode = TJclBinaryNode<T>;
      TPreOrderBinaryTreeIterator = TJclPreOrderBinaryTreeIterator<T>;
      TInOrderBinaryTreeIterator = TJclInOrderBinaryTreeIterator<T>;
      TPostOrderBinaryTreeIterator = TJclPostOrderBinaryTreeIterator<T>;,AOwnsItems: Boolean,,const ,AItem,T)*)

{$JPPEXPANDMACRO JCLBINARYTREEITRINT(TJclBinaryTreeIterator<T>,TJclPreOrderBinaryTreeIterator<T>,TJclInOrderBinaryTreeIterator<T>,TJclPostOrderBinaryTreeIterator<T>,IJclIterator<T>,IJclTreeIterator<T>,IJclBinaryTreeIterator<T>,IJclCollection<T>,IJclEqualityComparer<T>,TJclBinaryNode<T>,const ,AItem,T,GetItem,SetItem)}

  // E = External helper to compare items
  TJclBinaryTreeE<T> = class(TJclBinaryTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  private
    FComparer: IJclComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclComparer<T> }
    function ItemsCompare(const A, B: T): Integer; override;
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
  public
    constructor Create(const AComparer: IJclComparer<T>; AOwnsItems: Boolean);
    property Comparer: IJclComparer<T> read FComparer write FComparer;
  end;

  // F = Function to compare items
  TJclBinaryTreeF<T> = class(TJclBinaryTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TCompare<T>; AOwnsItems: Boolean);
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

(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclIntfBinaryTree,TJclIntfBinaryNode,TJclPreOrderIntfBinaryTreeIterator,TJclInOrderIntfBinaryTreeIterator,TJclPostOrderIntfBinaryTreeIterator,IJclIntfCollection,IJclIntfIterator,IJclIntfTreeIterator,ACompare: TIntfCompare,
  SetCompare(ACompare);,,const ,AInterface,IInterface,nil,FreeObject)*)

function TJclIntfBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TJclIntfBinaryTreeIterator,TJclPreOrderIntfBinaryTreeIterator,TJclInOrderIntfBinaryTreeIterator,TJclPostOrderIntfBinaryTreeIterator,IJclIntfIterator,IJclIntfCollection,IJclIntfEqualityComparer,TJclIntfBinaryNode,const ,AInterface,IInterface,nil,GetObject,SetObject,FreeObject)}

(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclAnsiStrBinaryTree,TJclAnsiStrBinaryNode,TJclPreOrderAnsiStrBinaryTreeIterator,TJclInOrderAnsiStrBinaryTreeIterator,TJclPostOrderAnsiStrBinaryTreeIterator,IJclAnsiStrCollection,IJclAnsiStrIterator,IJclAnsiStrTreeIterator,ACompare: TAnsiStrCompare,
  SetCompare(ACompare);,,const ,AString,AnsiString,'',FreeString)*)

function TJclAnsiStrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TJclAnsiStrBinaryTreeIterator,TJclPreOrderAnsiStrBinaryTreeIterator,TJclInOrderAnsiStrBinaryTreeIterator,TJclPostOrderAnsiStrBinaryTreeIterator,IJclAnsiStrIterator,IJclAnsiStrCollection,IJclAnsiStrEqualityComparer,TJclAnsiStrBinaryNode,const ,AString,AnsiString,'',GetString,SetString,FreeString)}

(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclWideStrBinaryTree,TJclWideStrBinaryNode,TJclPreOrderWideStrBinaryTreeIterator,TJclInOrderWideStrBinaryTreeIterator,TJclPostOrderWideStrBinaryTreeIterator,IJclWideStrCollection,IJclWideStrIterator,IJclWideStrTreeIterator,ACompare: TWideStrCompare,
  SetCompare(ACompare);,,const ,AString,WideString,'',FreeString)*)

function TJclWideStrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TJclWideStrBinaryTreeIterator,TJclPreOrderWideStrBinaryTreeIterator,TJclInOrderWideStrBinaryTreeIterator,TJclPostOrderWideStrBinaryTreeIterator,IJclWideStrIterator,IJclWideStrCollection,IJclWideStrEqualityComparer,TJclWideStrBinaryNode,const ,AString,WideString,'',GetString,SetString,FreeString)}

{$IFDEF SUPPORTS_UNICODE_STRING}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclUnicodeStrBinaryTree,TJclUnicodeStrBinaryNode,TJclPreOrderUnicodeStrBinaryTreeIterator,TJclInOrderUnicodeStrBinaryTreeIterator,TJclPostOrderUnicodeStrBinaryTreeIterator,IJclUnicodeStrCollection,IJclUnicodeStrIterator,IJclUnicodeStrTreeIterator,ACompare: TUnicodeStrCompare,
  SetCompare(ACompare);,,const ,AString,UnicodeString,'',FreeString)*)

function TJclUnicodeStrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TJclUnicodeStrBinaryTreeIterator,TJclPreOrderUnicodeStrBinaryTreeIterator,TJclInOrderUnicodeStrBinaryTreeIterator,TJclPostOrderUnicodeStrBinaryTreeIterator,IJclUnicodeStrIterator,IJclUnicodeStrCollection,IJclUnicodeStrEqualityComparer,TJclUnicodeStrBinaryNode,const ,AString,UnicodeString,'',GetString,SetString,FreeString)}
{$ENDIF SUPPORTS_UNICODE_STRING}

(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclSingleBinaryTree,TJclSingleBinaryNode,TJclPreOrderSingleBinaryTreeIterator,TJclInOrderSingleBinaryTreeIterator,TJclPostOrderSingleBinaryTreeIterator,IJclSingleCollection,IJclSingleIterator,IJclSingleTreeIterator,ACompare: TSingleCompare,
  SetCompare(ACompare);,,const ,AValue,Single,0.0,FreeSingle)*)

function TJclSingleBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TJclSingleBinaryTreeIterator,TJclPreOrderSingleBinaryTreeIterator,TJclInOrderSingleBinaryTreeIterator,TJclPostOrderSingleBinaryTreeIterator,IJclSingleIterator,IJclSingleCollection,IJclSingleEqualityComparer,TJclSingleBinaryNode,const ,AValue,Single,0.0,GetValue,SetValue,FreeValue)}

(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclDoubleBinaryTree,TJclDoubleBinaryNode,TJclPreOrderDoubleBinaryTreeIterator,TJclInOrderDoubleBinaryTreeIterator,TJclPostOrderDoubleBinaryTreeIterator,IJclDoubleCollection,IJclDoubleIterator,IJclDoubleTreeIterator,ACompare: TDoubleCompare,
  SetCompare(ACompare);,,const ,AValue,Double,0.0,FreeDouble)*)

function TJclDoubleBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TJclDoubleBinaryTreeIterator,TJclPreOrderDoubleBinaryTreeIterator,TJclInOrderDoubleBinaryTreeIterator,TJclPostOrderDoubleBinaryTreeIterator,IJclDoubleIterator,IJclDoubleCollection,IJclDoubleEqualityComparer,TJclDoubleBinaryNode,const ,AValue,Double,0.0,GetValue,SetValue,FreeValue)}

(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclExtendedBinaryTree,TJclExtendedBinaryNode,TJclPreOrderExtendedBinaryTreeIterator,TJclInOrderExtendedBinaryTreeIterator,TJclPostOrderExtendedBinaryTreeIterator,IJclExtendedCollection,IJclExtendedIterator,IJclExtendedTreeIterator,ACompare: TExtendedCompare,
  SetCompare(ACompare);,,const ,AValue,Extended,0.0,FreeExtended)*)

function TJclExtendedBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TJclExtendedBinaryTreeIterator,TJclPreOrderExtendedBinaryTreeIterator,TJclInOrderExtendedBinaryTreeIterator,TJclPostOrderExtendedBinaryTreeIterator,IJclExtendedIterator,IJclExtendedCollection,IJclExtendedEqualityComparer,TJclExtendedBinaryNode,const ,AValue,Extended,0.0,GetValue,SetValue,FreeValue)}

(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclIntegerBinaryTree,TJclIntegerBinaryNode,TJclPreOrderIntegerBinaryTreeIterator,TJclInOrderIntegerBinaryTreeIterator,TJclPostOrderIntegerBinaryTreeIterator,IJclIntegerCollection,IJclIntegerIterator,IJclIntegerTreeIterator,ACompare: TIntegerCompare,
  SetCompare(ACompare);,,,AValue,Integer,0,FreeInteger)*)

function TJclIntegerBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TJclIntegerBinaryTreeIterator,TJclPreOrderIntegerBinaryTreeIterator,TJclInOrderIntegerBinaryTreeIterator,TJclPostOrderIntegerBinaryTreeIterator,IJclIntegerIterator,IJclIntegerCollection,IJclIntegerEqualityComparer,TJclIntegerBinaryNode,,AValue,Integer,0,GetValue,SetValue,FreeValue)}

(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclCardinalBinaryTree,TJclCardinalBinaryNode,TJclPreOrderCardinalBinaryTreeIterator,TJclInOrderCardinalBinaryTreeIterator,TJclPostOrderCardinalBinaryTreeIterator,IJclCardinalCollection,IJclCardinalIterator,IJclCardinalTreeIterator,ACompare: TCardinalCompare,
  SetCompare(ACompare);,,,AValue,Cardinal,0,FreeCardinal)*)

function TJclCardinalBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TJclCardinalBinaryTreeIterator,TJclPreOrderCardinalBinaryTreeIterator,TJclInOrderCardinalBinaryTreeIterator,TJclPostOrderCardinalBinaryTreeIterator,IJclCardinalIterator,IJclCardinalCollection,IJclCardinalEqualityComparer,TJclCardinalBinaryNode,,AValue,Cardinal,0,GetValue,SetValue,FreeValue)}

(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclInt64BinaryTree,TJclInt64BinaryNode,TJclPreOrderInt64BinaryTreeIterator,TJclInOrderInt64BinaryTreeIterator,TJclPostOrderInt64BinaryTreeIterator,IJclInt64Collection,IJclInt64Iterator,IJclInt64TreeIterator,ACompare: TInt64Compare,
  SetCompare(ACompare);,,const ,AValue,Int64,0,FreeInt64)*)

function TJclInt64BinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64BinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TJclInt64BinaryTreeIterator,TJclPreOrderInt64BinaryTreeIterator,TJclInOrderInt64BinaryTreeIterator,TJclPostOrderInt64BinaryTreeIterator,IJclInt64Iterator,IJclInt64Collection,IJclInt64EqualityComparer,TJclInt64BinaryNode,const ,AValue,Int64,0,GetValue,SetValue,FreeValue)}

(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclPtrBinaryTree,TJclPtrBinaryNode,TJclPreOrderPtrBinaryTreeIterator,TJclInOrderPtrBinaryTreeIterator,TJclPostOrderPtrBinaryTreeIterator,IJclPtrCollection,IJclPtrIterator,IJclPtrTreeIterator,ACompare: TPtrCompare,
  SetCompare(ACompare);,,,APtr,Pointer,nil,FreePointer)*)

function TJclPtrBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrBinaryTree.Create(Compare);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TJclPtrBinaryTreeIterator,TJclPreOrderPtrBinaryTreeIterator,TJclInOrderPtrBinaryTreeIterator,TJclPostOrderPtrBinaryTreeIterator,IJclPtrIterator,IJclPtrCollection,IJclPtrEqualityComparer,TJclPtrBinaryNode,,APtr,Pointer,nil,GetPointer,SetPointer,FreePointer)}

(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclBinaryTree,TJclBinaryNode,TJclPreOrderBinaryTreeIterator,TJclInOrderBinaryTreeIterator,TJclPostOrderBinaryTreeIterator,IJclCollection,IJclIterator,IJclTreeIterator,ACompare: TCompare; AOwnsObjects: Boolean,
  SetCompare(ACompare);,AOwnsObjects,,AObject,TObject,nil,FreeObject)*)

function TJclBinaryTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBinaryTree.Create(Compare, False);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TJclBinaryTreeIterator,TJclPreOrderBinaryTreeIterator,TJclInOrderBinaryTreeIterator,TJclPostOrderBinaryTreeIterator,IJclIterator,IJclCollection,IJclEqualityComparer,TJclBinaryNode,,AObject,TObject,nil,GetObject,SetObject,FreeObject)}

{$IFDEF SUPPORTS_GENERICS}
(*$JPPEXPANDMACRO JCLBINARYTREEIMP(TJclBinaryTree<T>,TJclBinaryNode<T>,TPreOrderBinaryTreeIterator,TInOrderBinaryTreeIterator,TPostOrderBinaryTreeIterator,IJclCollection<T>,IJclIterator<T>,IJclTreeIterator<T>,AOwnsItems: Boolean,,AOwnsItems,const ,AItem,T,Default(T),FreeItem)*)

{$JPPEXPANDMACRO JCLBINARYTREEITRIMP(TJclBinaryTreeIterator<T>,TJclPreOrderBinaryTreeIterator<T>,TJclInOrderBinaryTreeIterator<T>,TJclPostOrderBinaryTreeIterator<T>,IJclIterator<T>,IJclCollection<T>,IJclEqualityComparer<T>,TJclBinaryNode<T>,const ,AItem,T,Default(T),GetItem,SetItem,FreeItem)}

//=== { TJclBinaryTreeE<T> } =================================================

constructor TJclBinaryTreeE<T>.Create(const AComparer: IJclComparer<T>; AOwnsItems: Boolean);
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
  if Comparer <> nil then
    Result := Comparer.Compare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclBinaryTreeE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Comparer <> nil then
    Result := Comparer.Compare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclBinaryTreeF<T> } =================================================

constructor TJclBinaryTreeF<T>.Create(ACompare: TCompare<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  SetCompare(ACompare);
end;

function TJclBinaryTreeF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBinaryTreeF<T>.Create(Compare, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclBinaryTreeI<T> } =================================================

function TJclBinaryTreeI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclBinaryTreeI<T>.Create(False);
  AssignPropertiesTo(Result);
end;

function TJclBinaryTreeI<T>.ItemsCompare(const A, B: T): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := A.CompareTo(B);
end;

function TJclBinaryTreeI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
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

