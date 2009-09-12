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
{ The Original Code is JclTrees.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet. Portions created by                }
{ Florent Ouchet are Copyright (C) Florent Ouchet <outchy att users dott sourceforge dott net      }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
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

unit JclTrees;

interface

{$I jcl.inc}

uses
  Classes,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF SUPPORTS_GENERICS}
  JclAlgorithms,
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclContainerIntf, JclSynch;
{$I containers\JclContainerCommon.imp}
{$I containers\JclTrees.imp}
{$I containers\JclTrees.int}
type
  TItrStart = (isFirst, isLast, isRoot);

{$JPPEXPANDMACRO JCLTREETYPESINT(TJclIntfTreeNode,IJclIntfEqualityComparer,const ,AInterface,IInterface)}

{$JPPEXPANDMACRO JCLTREEINT(TJclIntfTreeNode,TJclIntfTree,TJclIntfAbstractContainer,IJclIntfEqualityComparer,IJclIntfCollection,IJclIntfTree,IJclIntfIterator,IJclIntfTreeIterator,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AInterface,IInterface,nil)}

{$JPPEXPANDMACRO JCLTREEITRINT(TJclIntfTreeIterator,TJclPreOrderIntfTreeIterator,TJclPostOrderIntfTreeIterator,TJclIntfTreeNode,TJclIntfTree,IJclIntfIterator,IJclIntfTreeIterator,IJclIntfEqualityComparer,const ,AInterface,IInterface,nil,GetObject,SetObject)}

{$JPPEXPANDMACRO JCLTREETYPESINT(TJclAnsiStrTreeNode,IJclAnsiStrEqualityComparer,const ,AString,AnsiString)}

{$JPPEXPANDMACRO JCLTREEINT(TJclAnsiStrTreeNode,TJclAnsiStrTree,TJclAnsiStrAbstractCollection,IJclAnsiStrEqualityComparer,IJclAnsiStrCollection,IJclAnsiStrTree,IJclAnsiStrIterator,IJclAnsiStrTreeIterator, IJclStrContainer\, IJclAnsiStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;, override;,,const ,AString,AnsiString,'')}

{$JPPEXPANDMACRO JCLTREEITRINT(TJclAnsiStrTreeIterator,TJclPreOrderAnsiStrTreeIterator,TJclPostOrderAnsiStrTreeIterator,TJclAnsiStrTreeNode,TJclAnsiStrTree,IJclAnsiStrIterator,IJclAnsiStrTreeIterator,IJclAnsiStrEqualityComparer,const ,AString,AnsiString,'',GetString,SetString)}

{$JPPEXPANDMACRO JCLTREETYPESINT(TJclWideStrTreeNode,IJclWideStrEqualityComparer,const ,AString,WideString)}

{$JPPEXPANDMACRO JCLTREEINT(TJclWideStrTreeNode,TJclWideStrTree,TJclWideStrAbstractCollection,IJclWideStrEqualityComparer,IJclWideStrCollection,IJclWideStrTree,IJclWideStrIterator,IJclWideStrTreeIterator, IJclStrContainer\, IJclWideStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;, override;,,const ,AString,WideString,'')}

{$JPPEXPANDMACRO JCLTREEITRINT(TJclWideStrTreeIterator,TJclPreOrderWideStrTreeIterator,TJclPostOrderWideStrTreeIterator,TJclWideStrTreeNode,TJclWideStrTree,IJclWideStrIterator,IJclWideStrTreeIterator,IJclWideStrEqualityComparer,const ,AString,WideString,'',GetString,SetString)}

{$IFDEF SUPPORTS_UNICODE_STRING}
{$JPPEXPANDMACRO JCLTREETYPESINT(TJclUnicodeStrTreeNode,IJclUnicodeStrEqualityComparer,const ,AString,UnicodeString)}

{$JPPEXPANDMACRO JCLTREEINT(TJclUnicodeStrTreeNode,TJclUnicodeStrTree,TJclUnicodeStrAbstractCollection,IJclUnicodeStrEqualityComparer,IJclUnicodeStrCollection,IJclUnicodeStrTree,IJclUnicodeStrIterator,IJclUnicodeStrTreeIterator, IJclStrContainer\, IJclUnicodeStrContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;, override;,,const ,AString,UnicodeString,'')}

{$JPPEXPANDMACRO JCLTREEITRINT(TJclUnicodeStrTreeIterator,TJclPreOrderUnicodeStrTreeIterator,TJclPostOrderUnicodeStrTreeIterator,TJclUnicodeStrTreeNode,TJclUnicodeStrTree,IJclUnicodeStrIterator,IJclUnicodeStrTreeIterator,IJclUnicodeStrEqualityComparer,const ,AString,UnicodeString,'',GetString,SetString)}
{$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrTree = TJclAnsiStrTree;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrTree = TJclWideStrTree;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrTree = TJclUnicodeStrTree;
  {$ENDIF CONTAINER_UNICODESTR}

{$JPPEXPANDMACRO JCLTREETYPESINT(TJclSingleTreeNode,IJclSingleEqualityComparer,const ,AValue,Single)}

{$JPPEXPANDMACRO JCLTREEINT(TJclSingleTreeNode,TJclSingleTree,TJclSingleAbstractContainer,IJclSingleEqualityComparer,IJclSingleCollection,IJclSingleTree,IJclSingleIterator,IJclSingleTreeIterator, IJclSingleContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AValue,Single,0.0)}

{$JPPEXPANDMACRO JCLTREEITRINT(TJclSingleTreeIterator,TJclPreOrderSingleTreeIterator,TJclPostOrderSingleTreeIterator,TJclSingleTreeNode,TJclSingleTree,IJclSingleIterator,IJclSingleTreeIterator,IJclSingleEqualityComparer,const ,AValue,Single,0.0,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLTREETYPESINT(TJclDoubleTreeNode,IJclDoubleEqualityComparer,const ,AValue,Double)}

{$JPPEXPANDMACRO JCLTREEINT(TJclDoubleTreeNode,TJclDoubleTree,TJclDoubleAbstractContainer,IJclDoubleEqualityComparer,IJclDoubleCollection,IJclDoubleTree,IJclDoubleIterator,IJclDoubleTreeIterator, IJclDoubleContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AValue,Double,0.0)}

{$JPPEXPANDMACRO JCLTREEITRINT(TJclDoubleTreeIterator,TJclPreOrderDoubleTreeIterator,TJclPostOrderDoubleTreeIterator,TJclDoubleTreeNode,TJclDoubleTree,IJclDoubleIterator,IJclDoubleTreeIterator,IJclDoubleEqualityComparer,const ,AValue,Double,0.0,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLTREETYPESINT(TJclExtendedTreeNode,IJclExtendedEqualityComparer,const ,AValue,Extended)}

{$JPPEXPANDMACRO JCLTREEINT(TJclExtendedTreeNode,TJclExtendedTree,TJclExtendedAbstractContainer,IJclExtendedEqualityComparer,IJclExtendedCollection,IJclExtendedTree,IJclExtendedIterator,IJclExtendedTreeIterator, IJclExtendedContainer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AValue,Extended,0.0)}

{$JPPEXPANDMACRO JCLTREEITRINT(TJclExtendedTreeIterator,TJclPreOrderExtendedTreeIterator,TJclPostOrderExtendedTreeIterator,TJclExtendedTreeNode,TJclExtendedTree,IJclExtendedIterator,IJclExtendedTreeIterator,IJclExtendedEqualityComparer,const ,AValue,Extended,0.0,GetValue,SetValue)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatTree = TJclExtendedTree;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatTree = TJclDoubleTree;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatTree = TJclSingleTree;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO JCLTREETYPESINT(TJclIntegerTreeNode,IJclIntegerEqualityComparer,,AValue,Integer)}

{$JPPEXPANDMACRO JCLTREEINT(TJclIntegerTreeNode,TJclIntegerTree,TJclIntegerAbstractContainer,IJclIntegerEqualityComparer,IJclIntegerCollection,IJclIntegerTree,IJclIntegerIterator,IJclIntegerTreeIterator,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,AValue,Integer,0)}

{$JPPEXPANDMACRO JCLTREEITRINT(TJclIntegerTreeIterator,TJclPreOrderIntegerTreeIterator,TJclPostOrderIntegerTreeIterator,TJclIntegerTreeNode,TJclIntegerTree,IJclIntegerIterator,IJclIntegerTreeIterator,IJclIntegerEqualityComparer,,AValue,Integer,0,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLTREETYPESINT(TJclCardinalTreeNode,IJclCardinalEqualityComparer,,AValue,Cardinal)}

{$JPPEXPANDMACRO JCLTREEINT(TJclCardinalTreeNode,TJclCardinalTree,TJclCardinalAbstractContainer,IJclCardinalEqualityComparer,IJclCardinalCollection,IJclCardinalTree,IJclCardinalIterator,IJclCardinalTreeIterator,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,AValue,Cardinal,0)}

{$JPPEXPANDMACRO JCLTREEITRINT(TJclCardinalTreeIterator,TJclPreOrderCardinalTreeIterator,TJclPostOrderCardinalTreeIterator,TJclCardinalTreeNode,TJclCardinalTree,IJclCardinalIterator,IJclCardinalTreeIterator,IJclCardinalEqualityComparer,,AValue,Cardinal,0,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLTREETYPESINT(TJclInt64TreeNode,IJclInt64EqualityComparer,const ,AValue,Int64)}

{$JPPEXPANDMACRO JCLTREEINT(TJclInt64TreeNode,TJclInt64Tree,TJclInt64AbstractContainer,IJclInt64EqualityComparer,IJclInt64Collection,IJclInt64Tree,IJclInt64Iterator,IJclInt64TreeIterator,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AValue,Int64,0)}

{$JPPEXPANDMACRO JCLTREEITRINT(TJclInt64TreeIterator,TJclPreOrderInt64TreeIterator,TJclPostOrderInt64TreeIterator,TJclInt64TreeNode,TJclInt64Tree,IJclInt64Iterator,IJclInt64TreeIterator,IJclInt64EqualityComparer,const ,AValue,Int64,0,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLTREETYPESINT(TJclPtrTreeNode,IJclPtrEqualityComparer,,APtr,Pointer)}

{$JPPEXPANDMACRO JCLTREEINT(TJclPtrTreeNode,TJclPtrTree,TJclPtrAbstractContainer,IJclPtrEqualityComparer,IJclPtrCollection,IJclPtrTree,IJclPtrIterator,IJclPtrTreeIterator,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,APtr,Pointer,nil)}

{$JPPEXPANDMACRO JCLTREEITRINT(TJclPtrTreeIterator,TJclPreOrderPtrTreeIterator,TJclPostOrderPtrTreeIterator,TJclPtrTreeNode,TJclPtrTree,IJclPtrIterator,IJclPtrTreeIterator,IJclPtrEqualityComparer,,APtr,Pointer,nil,GetPointer,SetPointer)}

{$JPPEXPANDMACRO JCLTREETYPESINT(TJclTreeNode,IJclEqualityComparer,,AObject,TObject)}

{$JPPEXPANDMACRO JCLTREEINT(TJclTreeNode,TJclTree,TJclAbstractContainer,IJclEqualityComparer,IJclCollection,IJclTree,IJclIterator,IJclTreeIterator, IJclObjectOwner\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,AOwnsObjects: Boolean,,AObject,TObject,nil)}

{$JPPEXPANDMACRO JCLTREEITRINT(TJclTreeIterator,TJclPreOrderTreeIterator,TJclPostOrderTreeIterator,TJclTreeNode,TJclTree,IJclIterator,IJclTreeIterator,IJclEqualityComparer,,AObject,TObject,nil,GetObject,SetObject)}

{$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO JCLTREETYPESINT(TJclTreeNode<T>,IJclEqualityComparer<T>,const ,AItem,T)}

  TJclPreOrderTreeIterator<T> = class;
  TJclPostOrderTreeIterator<T> = class;

{$JPPEXPANDMACRO JCLTREEINT(TTreeNode,TJclTree<T>,TJclAbstractContainer<T>,IJclEqualityComparer<T>,IJclCollection<T>,IJclTree<T>,IJclIterator<T>,IJclTreeIterator<T>, IJclItemOwner<T>\,,
  protected
    type
      TTreeNode = TJclTreeNode<T>;
      TPreOrderTreeIterator = TJclPreOrderTreeIterator<T>;
      TPostOrderTreeIterator = TJclPostOrderTreeIterator<T>;,,AOwnsItems: Boolean,const ,AItem,T,Default(T))}

{$JPPEXPANDMACRO JCLTREEITRINT(TJclTreeIterator<T>,TJclPreOrderTreeIterator<T>,TJclPostOrderTreeIterator<T>,TJclTree<T>.TTreeNode,TJclTree<T>,IJclIterator<T>,IJclTreeIterator<T>,IJclEqualityComparer<T>,const ,AItem,T,Default(T),GetItem,SetItem)}

  // E = External helper to compare items for equality
  TJclTreeE<T> = class(TJclTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  private
    FEqualityComparer: IJclEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AEqualityComparer: IJclEqualityComparer<T>; AOwnsItems: Boolean);
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
    property EqualityComparer: IJclEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclTreeF<T> = class(TJclTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACompare: TCompare<T>; AOwnsItems: Boolean);
  end;

  // I = Items can compare themselves to an other for equality
  TJclTreeI<T: IEquatable<T>> = class(TJclTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
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
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils;

{$JPPEXPANDMACRO JCLTREETYPESIMP(TJclIntfTreeNode,IJclIntfEqualityComparer,const ,AInterface,IInterface)}

{$JPPEXPANDMACRO JCLTREEIMP(TJclIntfTreeNode,TJclIntfTree,TJclPreOrderIntfTreeIterator,TJclPostOrderIntfTreeIterator,IJclIntfCollection,IJclIntfIterator,IJclIntfTreeIterator,IJclIntfEqualityComparer,,,const ,AInterface,IInterface,nil,FreeObject)}

function TJclIntfTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfTree.Create;
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLTREEITRIMP(TJclIntfTreeIterator,TJclPreOrderIntfTreeIterator,TJclPostOrderIntfTreeIterator,TJclIntfTreeNode,TJclIntfTree,IJclIntfIterator,IJclIntfTreeIterator,IJclIntfEqualityComparer,const ,AInterface,IInterface,nil,GetObject,SetObject,FreeObject)}

{$JPPEXPANDMACRO JCLTREETYPESIMP(TJclAnsiStrTreeNode,IJclAnsiStrEqualityComparer,const ,AString,AnsiString)}

{$JPPEXPANDMACRO JCLTREEIMP(TJclAnsiStrTreeNode,TJclAnsiStrTree,TJclPreOrderAnsiStrTreeIterator,TJclPostOrderAnsiStrTreeIterator,IJclAnsiStrCollection,IJclAnsiStrIterator,IJclAnsiStrTreeIterator,IJclAnsiStrEqualityComparer,,,const ,AString,AnsiString,'',FreeString)}

function TJclAnsiStrTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrTree.Create;
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLTREEITRIMP(TJclAnsiStrTreeIterator,TJclPreOrderAnsiStrTreeIterator,TJclPostOrderAnsiStrTreeIterator,TJclAnsiStrTreeNode,TJclAnsiStrTree,IJclAnsiStrIterator,IJclAnsiStrTreeIterator,IJclAnsiStrEqualityComparer,const ,AString,AnsiString,'',GetString,SetString,FreeString)}

{$JPPEXPANDMACRO JCLTREETYPESIMP(TJclWideStrTreeNode,IJclWideStrEqualityComparer,const ,AString,WideString)}

{$JPPEXPANDMACRO JCLTREEIMP(TJclWideStrTreeNode,TJclWideStrTree,TJclPreOrderWideStrTreeIterator,TJclPostOrderWideStrTreeIterator,IJclWideStrCollection,IJclWideStrIterator,IJclWideStrTreeIterator,IJclWideStrEqualityComparer,,,const ,AString,WideString,'',FreeString)}

function TJclWideStrTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrTree.Create;
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLTREEITRIMP(TJclWideStrTreeIterator,TJclPreOrderWideStrTreeIterator,TJclPostOrderWideStrTreeIterator,TJclWideStrTreeNode,TJclWideStrTree,IJclWideStrIterator,IJclWideStrTreeIterator,IJclWideStrEqualityComparer,const ,AString,WideString,'',GetString,SetString,FreeString)}

{$IFDEF SUPPORTS_UNICODE_STRING}
{$JPPEXPANDMACRO JCLTREETYPESIMP(TJclUnicodeStrTreeNode,IJclUnicodeStrEqualityComparer,const ,AString,UnicodeString)}

{$JPPEXPANDMACRO JCLTREEIMP(TJclUnicodeStrTreeNode,TJclUnicodeStrTree,TJclPreOrderUnicodeStrTreeIterator,TJclPostOrderUnicodeStrTreeIterator,IJclUnicodeStrCollection,IJclUnicodeStrIterator,IJclUnicodeStrTreeIterator,IJclUnicodeStrEqualityComparer,,,const ,AString,UnicodeString,'',FreeString)}

function TJclUnicodeStrTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrTree.Create;
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLTREEITRIMP(TJclUnicodeStrTreeIterator,TJclPreOrderUnicodeStrTreeIterator,TJclPostOrderUnicodeStrTreeIterator,TJclUnicodeStrTreeNode,TJclUnicodeStrTree,IJclUnicodeStrIterator,IJclUnicodeStrTreeIterator,IJclUnicodeStrEqualityComparer,const ,AString,UnicodeString,'',GetString,SetString,FreeString)}
{$ENDIF SUPPORTS_UNICODE_STRING}

{$JPPEXPANDMACRO JCLTREETYPESIMP(TJclSingleTreeNode,IJclSingleEqualityComparer,const ,AValue,Single)}

{$JPPEXPANDMACRO JCLTREEIMP(TJclSingleTreeNode,TJclSingleTree,TJclPreOrderSingleTreeIterator,TJclPostOrderSingleTreeIterator,IJclSingleCollection,IJclSingleIterator,IJclSingleTreeIterator,IJclSingleEqualityComparer,,,const ,AValue,Single,0.0,FreeSingle)}

function TJclSingleTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleTree.Create;
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLTREEITRIMP(TJclSingleTreeIterator,TJclPreOrderSingleTreeIterator,TJclPostOrderSingleTreeIterator,TJclSingleTreeNode,TJclSingleTree,IJclSingleIterator,IJclSingleTreeIterator,IJclSingleEqualityComparer,const ,AValue,Single,0.0,GetValue,SetValue,FreeSingle)}

{$JPPEXPANDMACRO JCLTREETYPESIMP(TJclDoubleTreeNode,IJclDoubleEqualityComparer,const ,AValue,Double)}

{$JPPEXPANDMACRO JCLTREEIMP(TJclDoubleTreeNode,TJclDoubleTree,TJclPreOrderDoubleTreeIterator,TJclPostOrderDoubleTreeIterator,IJclDoubleCollection,IJclDoubleIterator,IJclDoubleTreeIterator,IJclDoubleEqualityComparer,,,const ,AValue,Double,0.0,FreeDouble)}

function TJclDoubleTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleTree.Create;
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLTREEITRIMP(TJclDoubleTreeIterator,TJclPreOrderDoubleTreeIterator,TJclPostOrderDoubleTreeIterator,TJclDoubleTreeNode,TJclDoubleTree,IJclDoubleIterator,IJclDoubleTreeIterator,IJclDoubleEqualityComparer,const ,AValue,Double,0.0,GetValue,SetValue,FreeDouble)}

{$JPPEXPANDMACRO JCLTREETYPESIMP(TJclExtendedTreeNode,IJclExtendedEqualityComparer,const ,AValue,Extended)}

{$JPPEXPANDMACRO JCLTREEIMP(TJclExtendedTreeNode,TJclExtendedTree,TJclPreOrderExtendedTreeIterator,TJclPostOrderExtendedTreeIterator,IJclExtendedCollection,IJclExtendedIterator,IJclExtendedTreeIterator,IJclExtendedEqualityComparer,,,const ,AValue,Extended,0.0,FreeExtended)}

function TJclExtendedTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedTree.Create;
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLTREEITRIMP(TJclExtendedTreeIterator,TJclPreOrderExtendedTreeIterator,TJclPostOrderExtendedTreeIterator,TJclExtendedTreeNode,TJclExtendedTree,IJclExtendedIterator,IJclExtendedTreeIterator,IJclExtendedEqualityComparer,const ,AValue,Extended,0.0,GetValue,SetValue,FreeExtended)}

{$JPPEXPANDMACRO JCLTREETYPESIMP(TJclIntegerTreeNode,IJclIntegerEqualityComparer,,AValue,Integer)}

{$JPPEXPANDMACRO JCLTREEIMP(TJclIntegerTreeNode,TJclIntegerTree,TJclPreOrderIntegerTreeIterator,TJclPostOrderIntegerTreeIterator,IJclIntegerCollection,IJclIntegerIterator,IJclIntegerTreeIterator,IJclIntegerEqualityComparer,,,,AValue,Integer,0,FreeInteger)}

function TJclIntegerTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerTree.Create;
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLTREEITRIMP(TJclIntegerTreeIterator,TJclPreOrderIntegerTreeIterator,TJclPostOrderIntegerTreeIterator,TJclIntegerTreeNode,TJclIntegerTree,IJclIntegerIterator,IJclIntegerTreeIterator,IJclIntegerEqualityComparer,,AValue,Integer,0,GetValue,SetValue,FreeInteger)}

{$JPPEXPANDMACRO JCLTREETYPESIMP(TJclCardinalTreeNode,IJclCardinalEqualityComparer,,AValue,Cardinal)}

{$JPPEXPANDMACRO JCLTREEIMP(TJclCardinalTreeNode,TJclCardinalTree,TJclPreOrderCardinalTreeIterator,TJclPostOrderCardinalTreeIterator,IJclCardinalCollection,IJclCardinalIterator,IJclCardinalTreeIterator,IJclCardinalEqualityComparer,,,,AValue,Cardinal,0,FreeCardinal)}

function TJclCardinalTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalTree.Create;
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLTREEITRIMP(TJclCardinalTreeIterator,TJclPreOrderCardinalTreeIterator,TJclPostOrderCardinalTreeIterator,TJclCardinalTreeNode,TJclCardinalTree,IJclCardinalIterator,IJclCardinalTreeIterator,IJclCardinalEqualityComparer,,AValue,Cardinal,0,GetValue,SetValue,FreeCardinal)}

{$JPPEXPANDMACRO JCLTREETYPESIMP(TJclInt64TreeNode,IJclInt64EqualityComparer,const ,AValue,Int64)}

{$JPPEXPANDMACRO JCLTREEIMP(TJclInt64TreeNode,TJclInt64Tree,TJclPreOrderInt64TreeIterator,TJclPostOrderInt64TreeIterator,IJclInt64Collection,IJclInt64Iterator,IJclInt64TreeIterator,IJclInt64EqualityComparer,,,const ,AValue,Int64,0,FreeInt64)}

function TJclInt64Tree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64Tree.Create;
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLTREEITRIMP(TJclInt64TreeIterator,TJclPreOrderInt64TreeIterator,TJclPostOrderInt64TreeIterator,TJclInt64TreeNode,TJclInt64Tree,IJclInt64Iterator,IJclInt64TreeIterator,IJclInt64EqualityComparer,const ,AValue,Int64,0,GetValue,SetValue,FreeInt64)}

{$JPPEXPANDMACRO JCLTREETYPESIMP(TJclPtrTreeNode,IJclPtrEqualityComparer,,APtr,Pointer)}

{$JPPEXPANDMACRO JCLTREEIMP(TJclPtrTreeNode,TJclPtrTree,TJclPreOrderPtrTreeIterator,TJclPostOrderPtrTreeIterator,IJclPtrCollection,IJclPtrIterator,IJclPtrTreeIterator,IJclPtrEqualityComparer,,,,APtr,Pointer,nil,FreePointer)}

function TJclPtrTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrTree.Create;
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLTREEITRIMP(TJclPtrTreeIterator,TJclPreOrderPtrTreeIterator,TJclPostOrderPtrTreeIterator,TJclPtrTreeNode,TJclPtrTree,IJclPtrIterator,IJclPtrTreeIterator,IJclPtrEqualityComparer,,APtr,Pointer,nil,GetPointer,SetPointer,FreePointer)}

{$JPPEXPANDMACRO JCLTREETYPESIMP(TJclTreeNode,IJclEqualityComparer,,AObject,TObject)}

{$JPPEXPANDMACRO JCLTREEIMP(TJclTreeNode,TJclTree,TJclPreOrderTreeIterator,TJclPostOrderTreeIterator,IJclCollection,IJclIterator,IJclTreeIterator,IJclEqualityComparer,AOwnsObjects: Boolean,AOwnsObjects,,AObject,TObject,nil,FreeObject)}

function TJclTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclTree.Create(False);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLTREEITRIMP(TJclTreeIterator,TJclPreOrderTreeIterator,TJclPostOrderTreeIterator,TJclTreeNode,TJclTree,IJclIterator,IJclTreeIterator,IJclEqualityComparer,,AObject,TObject,nil,GetObject,SetObject,FreeObject)}

{$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO JCLTREETYPESIMP(TJclTreeNode<T>,IJclEqualityComparer<T>,const ,AItem,T)}

{$JPPEXPANDMACRO JCLTREEIMP(TTreeNode,TJclTree<T>,TPreOrderTreeIterator,TPostOrderTreeIterator,IJclCollection<T>,IJclIterator<T>,IJclTreeIterator<T>,IJclEqualityComparer<T>,AOwnsItems: Boolean,AOwnsItems,const ,AItem,T,Default(T),FreeItem)}

{$JPPEXPANDMACRO JCLTREEITRIMP(TJclTreeIterator<T>,TJclPreOrderTreeIterator<T>,TJclPostOrderTreeIterator<T>,TJclTreeNode<T>,TJclTree<T>,IJclIterator<T>,IJclTreeIterator<T>,IJclEqualityComparer<T>,const ,AItem,T,Default(T),GetItem,SetItem,FreeItem)}

//=== { TJclTreeE<T> } =======================================================

constructor TJclTreeE<T>.Create(const AEqualityComparer: IJclEqualityComparer<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

procedure TJclTreeE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclTreeE<T> then
    TJclTreeE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclTreeE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclTreeE<T>.Create(EqualityComparer, False);
  AssignPropertiesTo(Result);
end;

function TJclTreeE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer <> nil then
    Result := EqualityComparer.ItemsEqual(A, B)
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclTreeF<T> } =======================================================

constructor TJclTreeF<T>.Create(ACompare: TCompare<T>; AOwnsItems: Boolean);
begin
  inherited Create(AOwnsItems);
  SetCompare(ACompare);
end;

function TJclTreeF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclTreeF<T>.Create(Compare, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclTreeI<T> } =======================================================

function TJclTreeI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclTreeI<T>.Create(False);
  AssignPropertiesTo(Result);
end;

function TJclTreeI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
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
