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
  {$IFDEF CLR}
  System.Collections.Generic,
  {$ENDIF CLR}
  JclAlgorithms,
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclContainerIntf;
{$I containers\JclTrees.imp}
type
{$JPPEXPANDMACRO JCLTREEINT(TJclIntfTreeNode,TJclIntfTree,TJclIntfAbstractContainer,IJclIntfEqualityComparer,IJclIntfCollection,IJclIntfTree,IJclIntfIterator,IJclIntfTreeIterator,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,IInterface,const AInterface: IInterface,nil)}

{$JPPEXPANDMACRO JCLTREEINT(TJclAnsiStrTreeNode,TJclAnsiStrTree,TJclAnsiStrAbstractCollection,IJclAnsiStrEqualityComparer,IJclAnsiStrCollection,IJclAnsiStrTree,IJclAnsiStrIterator,IJclAnsiStrTreeIterator, IJclStrContainer\, IJclAnsiStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,, override;,,AnsiString,const AString: AnsiString,'')}

{$JPPEXPANDMACRO JCLTREEINT(TJclWideStrTreeNode,TJclWideStrTree,TJclWideStrAbstractCollection,IJclWideStrEqualityComparer,IJclWideStrCollection,IJclWideStrTree,IJclWideStrIterator,IJclWideStrTreeIterator, IJclStrContainer\, IJclWideStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,, override;,,WideString,const AString: WideString,'')}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrTree = TJclAnsiStrTree;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrTree = TJclWideStrTree;
  {$ENDIF CONTAINER_WIDESTR}

{$JPPEXPANDMACRO JCLTREEINT(TJclSingleTreeNode,TJclSingleTree,TJclSingleAbstractContainer,IJclSingleEqualityComparer,IJclSingleCollection,IJclSingleTree,IJclSingleIterator,IJclSingleTreeIterator, IJclSingleContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,Single,const AValue: Single,0.0)}

{$JPPEXPANDMACRO JCLTREEINT(TJclDoubleTreeNode,TJclDoubleTree,TJclDoubleAbstractContainer,IJclDoubleEqualityComparer,IJclDoubleCollection,IJclDoubleTree,IJclDoubleIterator,IJclDoubleTreeIterator, IJclDoubleContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,Double,const AValue: Double,0.0)}

{$JPPEXPANDMACRO JCLTREEINT(TJclExtendedTreeNode,TJclExtendedTree,TJclExtendedAbstractContainer,IJclExtendedEqualityComparer,IJclExtendedCollection,IJclExtendedTree,IJclExtendedIterator,IJclExtendedTreeIterator, IJclExtendedContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,Extended,const AValue: Extended,0.0)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatTree = TJclExtendedTree;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatTree = TJclDoubleTree;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatTree = TJclSingleTree;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO JCLTREEINT(TJclIntegerTreeNode,TJclIntegerTree,TJclIntegerAbstractContainer,IJclIntegerEqualityComparer,IJclIntegerCollection,IJclIntegerTree,IJclIntegerIterator,IJclIntegerTreeIterator,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,Integer,AValue: Integer,0)}

{$JPPEXPANDMACRO JCLTREEINT(TJclCardinalTreeNode,TJclCardinalTree,TJclCardinalAbstractContainer,IJclCardinalEqualityComparer,IJclCardinalCollection,IJclCardinalTree,IJclCardinalIterator,IJclCardinalTreeIterator,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,Cardinal,AValue: Cardinal,0)}

{$JPPEXPANDMACRO JCLTREEINT(TJclInt64TreeNode,TJclInt64Tree,TJclInt64AbstractContainer,IJclInt64EqualityComparer,IJclInt64Collection,IJclInt64Tree,IJclInt64Iterator,IJclInt64TreeIterator,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,Int64,const AValue: Int64,0)}

{$IFNDEF CLR}
{$JPPEXPANDMACRO JCLTREEINT(TJclPtrTreeNode,TJclPtrTree,TJclPtrAbstractContainer,IJclPtrEqualityComparer,IJclPtrCollection,IJclPtrTree,IJclPtrIterator,IJclPtrTreeIterator,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,Pointer,APtr: Pointer,nil)}
{$ENDIF ~CLR}

{$JPPEXPANDMACRO JCLTREEINT(TJclTreeNode,TJclTree,TJclAbstractContainer,IJclEqualityComparer,IJclCollection,IJclTree,IJclIterator,IJclTreeIterator, IJclObjectOwner\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,AOwnsObjects: Boolean,TObject,AObject: TObject,nil)}

{$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO JCLTREEINT(TJclTreeNode<T>,TJclTree<T>,TJclAbstractContainer<T>,IJclEqualityComparer<T>,IJclCollection<T>,IJclTree<T>,IJclIterator<T>,IJclTreeIterator<T>, IJclItemOwner<T>\,,,,,,AOwnsItems: Boolean,T,const AItem: T,Default(T))}

  // E = External helper to compare items for equality
  TJclTreeE<T> = class(TJclTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; AOwnsItems: Boolean);
    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclTreeF<T> = class(TJclTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(ACompare: TCompare<T>; AOwnsItems: Boolean);
  end;

  // I = Items can compare themselves to an other for equality
  TJclTreeI<T: IEquatable<T>> = class(TJclTree<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclTree<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
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

type
  TItrStart = (isFirst, isLast, isRoot);

{$JPPEXPANDMACRO JCLTREEITR(TIntfItr,TPreOrderIntfItr,TPostOrderIntfItr,TJclIntfTreeNode,TJclIntfTree,IJclIntfIterator,IJclIntfTreeIterator,IJclIntfEqualityComparer,IInterface,AInterface,const AInterface: IInterface,nil,GetObject,SetObject,FreeObject)}
{$JPPEXPANDMACRO JCLTREEITR(TAnsiStrItr,TPreOrderAnsiStrItr,TPostOrderAnsiStrItr,TJclAnsiStrTreeNode,TJclAnsiStrTree,IJclAnsiStrIterator,IJclAnsiStrTreeIterator,IJclAnsiStrEqualityComparer,AnsiString,AString,const AString: AnsiString,'',GetString,SetString,FreeString)}
{$JPPEXPANDMACRO JCLTREEITR(TWideStrItr,TPreOrderWideStrItr,TPostOrderWideStrItr,TJclWideStrTreeNode,TJclWideStrTree,IJclWideStrIterator,IJclWideStrTreeIterator,IJclWideStrEqualityComparer,WideString,AString,const AString: WideString,'',GetString,SetString,FreeString)}
{$JPPEXPANDMACRO JCLTREEITR(TSingleItr,TPreOrderSingleItr,TPostOrderSingleItr,TJclSingleTreeNode,TJclSingleTree,IJclSingleIterator,IJclSingleTreeIterator,IJclSingleEqualityComparer,Single,AValue,const AValue: Single,0.0,GetValue,SetValue,FreeSingle)}
{$JPPEXPANDMACRO JCLTREEITR(TDoubleItr,TPreOrderDoubleItr,TPostOrderDoubleItr,TJclDoubleTreeNode,TJclDoubleTree,IJclDoubleIterator,IJclDoubleTreeIterator,IJclDoubleEqualityComparer,Double,AValue,const AValue: Double,0.0,GetValue,SetValue,FreeDouble)}
{$JPPEXPANDMACRO JCLTREEITR(TExtendedItr,TPreOrderExtendedItr,TPostOrderExtendedItr,TJclExtendedTreeNode,TJclExtendedTree,IJclExtendedIterator,IJclExtendedTreeIterator,IJclExtendedEqualityComparer,Extended,AValue,const AValue: Extended,0.0,GetValue,SetValue,FreeExtended)}
{$JPPEXPANDMACRO JCLTREEITR(TIntegerItr,TPreOrderIntegerItr,TPostOrderIntegerItr,TJclIntegerTreeNode,TJclIntegerTree,IJclIntegerIterator,IJclIntegerTreeIterator,IJclIntegerEqualityComparer,Integer,AValue,AValue: Integer,0,GetValue,SetValue,FreeInteger)}
{$JPPEXPANDMACRO JCLTREEITR(TCardinalItr,TPreOrderCardinalItr,TPostOrderCardinalItr,TJclCardinalTreeNode,TJclCardinalTree,IJclCardinalIterator,IJclCardinalTreeIterator,IJclCardinalEqualityComparer,Cardinal,AValue,AValue: Cardinal,0,GetValue,SetValue,FreeCardinal)}
{$JPPEXPANDMACRO JCLTREEITR(TInt64Itr,TPreOrderInt64Itr,TPostOrderInt64Itr,TJclInt64TreeNode,TJclInt64Tree,IJclInt64Iterator,IJclInt64TreeIterator,IJclInt64EqualityComparer,Int64,AValue,const AValue: Int64,0,GetValue,SetValue,FreeInt64)}
{$IFNDEF CLR}
{$JPPEXPANDMACRO JCLTREEITR(TPtrItr,TPreOrderPtrItr,TPostOrderPtrItr,TJclPtrTreeNode,TJclPtrTree,IJclPtrIterator,IJclPtrTreeIterator,IJclPtrEqualityComparer,Pointer,APtr,APtr: Pointer,nil,GetPtr,SetPtr,FreePointer)}
{$ENDIF ~CLR}
{$JPPEXPANDMACRO JCLTREEITR(TItr,TPreOrderItr,TPostOrderItr,TJclTreeNode,TJclTree,IJclIterator,IJclTreeIterator,IJclEqualityComparer,TObject,AObject,AObject: TObject,nil,GetObject,SetObject,FreeObject)}
{$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO JCLTREEITR(TItr<T>,TPreOrderItr<T>,TPostOrderItr<T>,TJclTreeNode<T>,TJclTree<T>,IJclIterator<T>,IJclTreeIterator<T>,IJclEqualityComparer<T>,T,AItem,const AItem: T,Default(T),GetItem,SetItem,FreeItem)}
{$ENDIF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfTree.Create;
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLTREEIMP(TJclIntfTreeNode,TJclIntfTree,TPreOrderIntfItr,TPostOrderIntfItr,IJclIntfCollection,IJclIntfIterator,IJclIntfTreeIterator,IJclIntfEqualityComparer,,,IInterface,AInterface,const AInterface: IInterface,nil,FreeObject)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrTree.Create;
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLTREEIMP(TJclAnsiStrTreeNode,TJclAnsiStrTree,TPreOrderAnsiStrItr,TPostOrderAnsiStrItr,IJclAnsiStrCollection,IJclAnsiStrIterator,IJclAnsiStrTreeIterator,IJclAnsiStrEqualityComparer,,,AnsiString,AString,const AString: AnsiString,'',FreeString)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrTree.Create;
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLTREEIMP(TJclWideStrTreeNode,TJclWideStrTree,TPreOrderWideStrItr,TPostOrderWideStrItr,IJclWideStrCollection,IJclWideStrIterator,IJclWideStrTreeIterator,IJclWideStrEqualityComparer,,,WideString,AString,const AString: WideString,'',FreeString)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclSingleTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleTree.Create;
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLTREEIMP(TJclSingleTreeNode,TJclSingleTree,TPreOrderSingleItr,TPostOrderSingleItr,IJclSingleCollection,IJclSingleIterator,IJclSingleTreeIterator,IJclSingleEqualityComparer,,,Single,AValue,const AValue: Single,0.0,FreeSingle)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclDoubleTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleTree.Create;
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLTREEIMP(TJclDoubleTreeNode,TJclDoubleTree,TPreOrderDoubleItr,TPostOrderDoubleItr,IJclDoubleCollection,IJclDoubleIterator,IJclDoubleTreeIterator,IJclDoubleEqualityComparer,,,Double,AValue,const AValue: Double,0.0,FreeDouble)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclExtendedTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedTree.Create;
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLTREEIMP(TJclExtendedTreeNode,TJclExtendedTree,TPreOrderExtendedItr,TPostOrderExtendedItr,IJclExtendedCollection,IJclExtendedIterator,IJclExtendedTreeIterator,IJclExtendedEqualityComparer,,,Extended,AValue,const AValue: Extended,0.0,FreeExtended)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntegerTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerTree.Create;
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLTREEIMP(TJclIntegerTreeNode,TJclIntegerTree,TPreOrderIntegerItr,TPostOrderIntegerItr,IJclIntegerCollection,IJclIntegerIterator,IJclIntegerTreeIterator,IJclIntegerEqualityComparer,,,Integer,AValue,AValue: Integer,0,FreeInteger)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclCardinalTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalTree.Create;
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLTREEIMP(TJclCardinalTreeNode,TJclCardinalTree,TPreOrderCardinalItr,TPostOrderCardinalItr,IJclCardinalCollection,IJclCardinalIterator,IJclCardinalTreeIterator,IJclCardinalEqualityComparer,,,Cardinal,AValue,AValue: Cardinal,0,FreeCardinal)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclInt64Tree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64Tree.Create;
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLTREEIMP(TJclInt64TreeNode,TJclInt64Tree,TPreOrderInt64Itr,TPostOrderInt64Itr,IJclInt64Collection,IJclInt64Iterator,IJclInt64TreeIterator,IJclInt64EqualityComparer,,,Int64,AValue,const AValue: Int64,0,FreeInt64)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$IFNDEF CLR}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclPtrTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrTree.Create;
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLTREEIMP(TJclPtrTreeNode,TJclPtrTree,TPreOrderPtrItr,TPostOrderPtrItr,IJclPtrCollection,IJclPtrIterator,IJclPtrTreeIterator,IJclPtrEqualityComparer,,,Pointer,APtr,APtr: Pointer,nil,FreePointer)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$ENDIF ~CLR}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclTree.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclTree.Create(False);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLTREEIMP(TJclTreeNode,TJclTree,TPreOrderItr,TPostOrderItr,IJclCollection,IJclIterator,IJclTreeIterator,IJclEqualityComparer,AOwnsObjects: Boolean,\, AOwnsObjects,TObject,AObject,AObject: TObject,nil,FreeObject)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$IFDEF SUPPORTS_GENERICS}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER}
{$JPPEXPANDMACRO JCLTREEIMP(TJclTreeNode<T>,TJclTree<T>,TPreOrderItr<T>,TPostOrderItr<T>,IJclCollection<T>,IJclIterator<T>,IJclTreeIterator<T>,IJclEqualityComparer<T>,AOwnsItems: Boolean,\, AOwnsItems,T,AItem,const AItem: T,Default(T),FreeItem)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

//=== { TJclTreeE<T> } =======================================================

constructor TJclTreeE<T>.Create(const AEqualityComparer: IEqualityComparer<T>; AOwnsItems: Boolean);
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
    Result := EqualityComparer.Equals(A, B)
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
