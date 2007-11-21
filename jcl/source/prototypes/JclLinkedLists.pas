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
{ The Original Code is LinkedList.pas.                                                             }
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

unit JclLinkedLists;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF SUPPORTS_GENERICS}
  {$IFDEF CLR}
  System.Collections.Generic,
  {$ENDIF CLR}
  JclAlgorithms,
  {$ENDIF SUPPORTS_GENERICS}
  Classes,
  JclBase, JclAbstractContainers, JclContainerIntf;
{$I containers\JclLinkedLists.imp}
type
(*$JPPEXPANDMACRO JCLLINKEDLISTINT(IInterface,TJclIntfLinkedListItem,TJclIntfLinkedList,TJclIntfAbstractContainer,IJclIntfCollection,IJclIntfList,IJclIntfIterator, IJclIntfEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,const AInterface: IInterface,GetObject,SetObject)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(AnsiString,TJclAnsiStrLinkedListItem,TJclAnsiStrLinkedList,TJclAnsiStrAbstractCollection,IJclAnsiStrCollection,IJclAnsiStrList,IJclAnsiStrIterator, IJclStrContainer\, IJclAnsiStrContainer\, IJclAnsiStrFlatContainer\, IJclAnsiStrEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,, override;,,const AString: AnsiString,GetString,SetString)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(WideString,TJclWideStrLinkedListItem,TJclWideStrLinkedList,TJclWideStrAbstractCollection,IJclWideStrCollection,IJclWideStrList,IJclWideStrIterator, IJclStrContainer\, IJclWideStrContainer\, IJclWideStrFlatContainer\, IJclWideStrEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,, override;,,const AString: WideString,GetString,SetString)*)

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrLinkedList = TJclAnsiStrLinkedList;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrLinkedList = TJclWideStrLinkedList;
  {$ENDIF CONTAINER_WIDESTR}

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(Single,TJclSingleLinkedListItem,TJclSingleLinkedList,TJclSingleAbstractContainer,IJclSingleCollection,IJclSingleList,IJclSingleIterator, IJclSingleContainer\, IJclSingleEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,const AValue: Single,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(Double,TJclDoubleLinkedListItem,TJclDoubleLinkedList,TJclDoubleAbstractContainer,IJclDoubleCollection,IJclDoubleList,IJclDoubleIterator, IJclDoubleContainer\, IJclDoubleEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,const AValue: Double,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(Extended,TJclExtendedLinkedListItem,TJclExtendedLinkedList,TJclExtendedAbstractContainer,IJclExtendedCollection,IJclExtendedList,IJclExtendedIterator, IJclExtendedContainer\, IJclExtendedEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,const AValue: Extended,GetValue,SetValue)*)

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatLinkedList = TJclExtendedLinkedList;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatLinkedList = TJclDoubleLinkedList;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatLinkedList = TJclSingleLinkedList;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(Integer,TJclIntegerLinkedListItem,TJclIntegerLinkedList,TJclIntegerAbstractContainer,IJclIntegerCollection,IJclIntegerList,IJclIntegerIterator, IJclIntegerEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,AValue: Integer,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(Cardinal,TJclCardinalLinkedListItem,TJclCardinalLinkedList,TJclCardinalAbstractContainer,IJclCardinalCollection,IJclCardinalList,IJclCardinalIterator, IJclCardinalEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,AValue: Cardinal,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(Int64,TJclInt64LinkedListItem,TJclInt64LinkedList,TJclInt64AbstractContainer,IJclInt64Collection,IJclInt64List,IJclInt64Iterator, IJclInt64EqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,const AValue: Int64,GetValue,SetValue)*)

{$IFNDEF CLR}
(*$JPPEXPANDMACRO JCLLINKEDLISTINT(Pointer,TJclPtrLinkedListItem,TJclPtrLinkedList,TJclPtrAbstractContainer,IJclPtrCollection,IJclPtrList,IJclPtrIterator, IJclPtrEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,APtr: Pointer,GetPtr,SetPtr)*)
{$ENDIF ~CLR}

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TObject,TJclLinkedListItem,TJclLinkedList,TJclAbstractContainer,IJclCollection,IJclList,IJclIterator, IJclObjectOwner\, IJclEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,; AOwnsObjects: Boolean,AObject: TObject,GetObject,SetObject)*)

  {$IFDEF SUPPORTS_GENERICS}
(*$JPPEXPANDMACRO JCLLINKEDLISTINT(T,TJclLinkedListItem<T>,TJclLinkedList<T>,TJclAbstractContainer<T>,IJclCollection<T>,IJclList<T>,IJclIterator<T>, IJclItemOwner<T>\, IJclEqualityComparer<T>\,,,,,,; AOwnsItems: Boolean,const AItem: T,GetItem,SetItem)*)

  // E = External helper to compare items
  // GetHashCode is never called
  TJclLinkedListE<T> = class(TJclLinkedList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclCollection<T>, IJclList<T>, IJclEqualityComparer<T>,
    IJclItemOwner<T>)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; const ACollection: IJclCollection<T>;
      AOwnsItems: Boolean);
    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclLinkedListF<T> = class(TJclLinkedList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclCollection<T>, IJclList<T>, IJclEqualityComparer<T>,
    IJclItemOwner<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const ACollection: IJclCollection<T>;
      AOwnsItems: Boolean);
  end;

  // I = Items can compare themselves to an other
  TJclLinkedListI<T: IEquatable<T>> = class(TJclLinkedList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclCollection<T>, IJclList<T>, IJclEqualityComparer<T>,
    IJclItemOwner<T>)
  protected
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

{$JPPDEFINEMACRO ITEMFREE(Item)Item := nil}
(*$JPPEXPANDMACRO JCLLINKEDLISTITR(TIntfItr,IJclIntfIterator,IJclIntfList,IJclIntfEqualityComparer,TJclIntfLinkedListItem,IInterface,const AInterface: IInterface,AInterface,nil,GetObject,SetObject)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

{$JPPDEFINEMACRO ITEMFREE(Item)Item := ''}
(*$JPPEXPANDMACRO JCLLINKEDLISTITR(TAnsiStrItr,IJclAnsiStrIterator,IJclAnsiStrList,IJclAnsiStrEqualityComparer,TJclAnsiStrLinkedListItem,AnsiString,const AString: AnsiString,AString,'',GetString,SetString)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

{$JPPDEFINEMACRO ITEMFREE(Item)Item := ''}
(*$JPPEXPANDMACRO JCLLINKEDLISTITR(TWideStrItr,IJclWideStrIterator,IJclWideStrList,IJclWideStrEqualityComparer,TJclWideStrLinkedListItem,WideString,const AString: WideString,AString,'',GetString,SetString)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

{$JPPDEFINEMACRO ITEMFREE(Item)Item := 0.0}
(*$JPPEXPANDMACRO JCLLINKEDLISTITR(TSingleItr,IJclSingleIterator,IJclSingleList,IJclSingleEqualityComparer,TJclSingleLinkedListItem,Single,const AValue: Single,AValue,0.0,GetValue,SetValue)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

{$JPPDEFINEMACRO ITEMFREE(Item)Item := 0.0}
(*$JPPEXPANDMACRO JCLLINKEDLISTITR(TDoubleItr,IJclDoubleIterator,IJclDoubleList,IJclDoubleEqualityComparer,TJclDoubleLinkedListItem,Double,const AValue: Double,AValue,0.0,GetValue,SetValue)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

{$JPPDEFINEMACRO ITEMFREE(Item)Item := 0.0}
(*$JPPEXPANDMACRO JCLLINKEDLISTITR(TExtendedItr,IJclExtendedIterator,IJclExtendedList,IJclExtendedEqualityComparer,TJclExtendedLinkedListItem,Extended,const AValue: Extended,AValue,0.0,GetValue,SetValue)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

{$JPPDEFINEMACRO ITEMFREE(Item)Item := 0}
(*$JPPEXPANDMACRO JCLLINKEDLISTITR(TIntegerItr,IJclIntegerIterator,IJclIntegerList,IJclIntegerEqualityComparer,TJclIntegerLinkedListItem,Integer,AValue: Integer,AValue,0,GetValue,SetValue)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

{$JPPDEFINEMACRO ITEMFREE(Item)Item := 0}
(*$JPPEXPANDMACRO JCLLINKEDLISTITR(TCardinalItr,IJclCardinalIterator,IJclCardinalList,IJclCardinalEqualityComparer,TJclCardinalLinkedListItem,Cardinal,AValue: Cardinal,AValue,0,GetValue,SetValue)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

{$JPPDEFINEMACRO ITEMFREE(Item)Item := 0}
(*$JPPEXPANDMACRO JCLLINKEDLISTITR(TInt64Itr,IJclInt64Iterator,IJclInt64List,IJclInt64EqualityComparer,TJclInt64LinkedListItem,Int64,const AValue: Int64,AValue,0,GetValue,SetValue)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

  {$IFNDEF CLR}
{$JPPDEFINEMACRO ITEMFREE(Item)Item := nil}
(*$JPPEXPANDMACRO JCLLINKEDLISTITR(TPtrItr,IJclPtrIterator,IJclPtrList,IJclPtrEqualityComparer,TJclPtrLinkedListItem,Pointer,AValue: Pointer,AValue,nil,GetPtr,SetPtr)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}
  {$ENDIF ~CLR}

{$JPPDEFINEMACRO ITEMFREE(AObject)(FownList as IJclObjectOwner).FreeObject(AObject)}
(*$JPPEXPANDMACRO JCLLINKEDLISTITR(TItr,IJclIterator,IJclList,IJclEqualityComparer,TJclLinkedListItem,TObject,AObject: TObject,AObject,nil,GetObject,SetObject)*)
{$JPPUNDEFMACRO ITEMFREE(AObject)}

{$IFDEF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO ITEMFREE(AItem)(FownList as IJclItemOwner<T>).FreeItem(AItem)}
(*$JPPEXPANDMACRO JCLLINKEDLISTITR(TItr<T>,IJclIterator<T>,IJclList<T>,IJclEqualityComparer<T>,TJclLinkedListItem<T>,T,const AItem: T,AItem,Default(T),GetItem,SetItem)*)
{$JPPUNDEFMACRO ITEMFREE(AObject)}

{$ENDIF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclIntfLinkedList,IInterface,TJclIntfLinkedListItem,IJclIntfCollection,IJclIntfList,IJclIntfIterator,TIntfItr,,,const AInterface: IInterface,AInterface,nil,GetObject,SetObject,FreeObject)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclAnsiStrLinkedList,AnsiString,TJclAnsiStrLinkedListItem,IJclAnsiStrCollection,IJclAnsiStrList,IJclAnsiStrIterator,TAnsiStrItr,,,const AString: AnsiString,AString,'',GetString,SetString,FreeString)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclWideStrLinkedList,WideString,TJclWideStrLinkedListItem,IJclWideStrCollection,IJclWideStrList,IJclWideStrIterator,TWideStrItr,,,const AString: WideString,AString,'',GetString,SetString,FreeString)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclSingleLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclSingleLinkedList,Single,TJclSingleLinkedListItem,IJclSingleCollection,IJclSingleList,IJclSingleIterator,TSingleItr,,,const AValue: Single,AValue,0.0,GetValue,SetValue,FreeSingle)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclDoubleLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclDoubleLinkedList,Double,TJclDoubleLinkedListItem,IJclDoubleCollection,IJclDoubleList,IJclDoubleIterator,TDoubleItr,,,const AValue: Double,AValue,0.0,GetValue,SetValue,FreeDouble)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclExtendedLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclExtendedLinkedList,Extended,TJclExtendedLinkedListItem,IJclExtendedCollection,IJclExtendedList,IJclExtendedIterator,TExtendedItr,,,const AValue: Extended,AValue,0.0,GetValue,SetValue,FreeExtended)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntegerLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclIntegerLinkedList,Integer,TJclIntegerLinkedListItem,IJclIntegerCollection,IJclIntegerList,IJclIntegerIterator,TIntegerItr,,,AValue: Integer,AValue,0,GetValue,SetValue,FreeInteger)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclCardinalLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclCardinalLinkedList,Cardinal,TJclCardinalLinkedListItem,IJclCardinalCollection,IJclCardinalList,IJclCardinalIterator,TCardinalItr,,,AValue: Cardinal,AValue,0,GetValue,SetValue,FreeCardinal)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclInt64LinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64LinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclInt64LinkedList,Int64,TJclInt64LinkedListItem,IJclInt64Collection,IJclInt64List,IJclInt64Iterator,TInt64Itr,,,const AValue: Int64,AValue,0,GetValue,SetValue,FreeInt64)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$IFNDEF CLR}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclPtrLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclPtrLinkedList,Pointer,TJclPtrLinkedListItem,IJclPtrCollection,IJclPtrList,IJclPtrIterator,TPtrItr,,,APtr: Pointer,APtr,nil,GetPtr,SetPtr,FreePointer)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$ENDIF ~CLR}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclLinkedList.Create(nil, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclLinkedList,TObject,TJclLinkedListItem,IJclCollection,IJclList,IJclIterator,TItr,; AOwnsObjects: Boolean,\, AOwnsObjects,AObject: TObject,AObject,nil,GetObject,SetObject,FreeObject)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$IFDEF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclLinkedList<T>,T,TJclLinkedListItem<T>,IJclCollection<T>,IJclList<T>,IJclIterator<T>,TItr<T>,; AOwnsItems: Boolean,\, AOwnsItems,const AItem: T,AItem,Default(T),GetItem,SetItem,FreeItem)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

//=== { TJclLinkedListE<T> } =================================================

constructor TJclLinkedListE<T>.Create(const AEqualityComparer: IEqualityComparer<T>;
  const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

procedure TJclLinkedListE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclLinkedListE<T> then
    TJclLinkedListE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclLinkedListE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclLinkedListE<T>.Create(EqualityComparer, nil, False);
  AssignPropertiesTo(Result);
end;

function TJclLinkedListE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer <> nil then
    Result := EqualityComparer.Equals(A, B)
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclLinkedListF<T> } =================================================

constructor TJclLinkedListF<T>.Create(const AEqualityCompare: TEqualityCompare<T>;
  const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  SetEqualityCompare(AEqualityCompare);
end;

function TJclLinkedListF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclLinkedListF<T>.Create(EqualityCompare, nil, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclLinkedListI<T> } =================================================

function TJclLinkedListI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclLinkedListI<T>.Create(nil, False);
  AssignPropertiesTo(Result);
end;

function TJclLinkedListI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
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

