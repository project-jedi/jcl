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
{ Last modified: $Date::                                                                       $ }
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
  JclBase, JclAbstractContainers, JclContainerIntf, JclSynch;
{$I containers\JclContainerCommon.imp}
{$I containers\JclLinkedLists.imp}
{$I containers\JclLinkedLists.int}
type
  TItrStart = (isFirst, isLast);

(*$JPPEXPANDMACRO JCLLINKEDLISTTYPESINT(TJclIntfLinkedListItem,IInterface)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclIntfLinkedListItem,TJclIntfLinkedList,TJclIntfAbstractContainer,IJclIntfCollection,IJclIntfList,IJclIntfIterator, IJclIntfEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AInterface,IInterface,GetObject,SetObject)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TJclIntfLinkedListIterator,IJclIntfIterator,IJclIntfList,IJclIntfEqualityComparer,TJclIntfLinkedListItem,const ,AInterface,IInterface,nil,GetObject,SetObject)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTTYPESINT(TJclAnsiStrLinkedListItem,AnsiString)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclAnsiStrLinkedListItem,TJclAnsiStrLinkedList,TJclAnsiStrAbstractCollection,IJclAnsiStrCollection,IJclAnsiStrList,IJclAnsiStrIterator, IJclStrContainer\, IJclAnsiStrContainer\, IJclAnsiStrFlatContainer\, IJclAnsiStrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;, override;,,const ,AString,AnsiString,GetString,SetString)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TJclAnsiStrLinkedListIterator,IJclAnsiStrIterator,IJclAnsiStrList,IJclAnsiStrEqualityComparer,TJclAnsiStrLinkedListItem,const ,AString,AnsiString,'',GetString,SetString)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTTYPESINT(TJclWideStrLinkedListItem,WideString)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclWideStrLinkedListItem,TJclWideStrLinkedList,TJclWideStrAbstractCollection,IJclWideStrCollection,IJclWideStrList,IJclWideStrIterator, IJclStrContainer\, IJclWideStrContainer\, IJclWideStrFlatContainer\, IJclWideStrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;, override;,,const ,AString,WideString,GetString,SetString)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TJclWideStrLinkedListIterator,IJclWideStrIterator,IJclWideStrList,IJclWideStrEqualityComparer,TJclWideStrLinkedListItem,const ,AString,WideString,'',GetString,SetString)*)

{$IFDEF SUPPORTS_UNICODE_STRING}
(*$JPPEXPANDMACRO JCLLINKEDLISTTYPESINT(TJclUnicodeStrLinkedListItem,UnicodeString)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclUnicodeStrLinkedListItem,TJclUnicodeStrLinkedList,TJclUnicodeStrAbstractCollection,IJclUnicodeStrCollection,IJclUnicodeStrList,IJclUnicodeStrIterator, IJclStrContainer\, IJclUnicodeStrContainer\, IJclUnicodeStrFlatContainer\, IJclUnicodeStrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;, override;,,const ,AString,UnicodeString,GetString,SetString)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TJclUnicodeStrLinkedListIterator,IJclUnicodeStrIterator,IJclUnicodeStrList,IJclUnicodeStrEqualityComparer,TJclUnicodeStrLinkedListItem,const ,AString,UnicodeString,'',GetString,SetString)*)
{$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrLinkedList = TJclAnsiStrLinkedList;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrLinkedList = TJclWideStrLinkedList;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrLinkedList = TJclUnicodeStrLinkedList;
  {$ENDIF CONTAINER_UNICODESTR}

(*$JPPEXPANDMACRO JCLLINKEDLISTTYPESINT(TJclSingleLinkedListItem,Single)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclSingleLinkedListItem,TJclSingleLinkedList,TJclSingleAbstractContainer,IJclSingleCollection,IJclSingleList,IJclSingleIterator, IJclSingleContainer\, IJclSingleEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AValue,Single,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TJclSingleLinkedListIterator,IJclSingleIterator,IJclSingleList,IJclSingleEqualityComparer,TJclSingleLinkedListItem,const ,AValue,Single,0.0,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTTYPESINT(TJclDoubleLinkedListItem,Double)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclDoubleLinkedListItem,TJclDoubleLinkedList,TJclDoubleAbstractContainer,IJclDoubleCollection,IJclDoubleList,IJclDoubleIterator, IJclDoubleContainer\, IJclDoubleEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AValue,Double,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TJclDoubleLinkedListIterator,IJclDoubleIterator,IJclDoubleList,IJclDoubleEqualityComparer,TJclDoubleLinkedListItem,const ,AValue,Double,0.0,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTTYPESINT(TJclExtendedLinkedListItem,Extended)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclExtendedLinkedListItem,TJclExtendedLinkedList,TJclExtendedAbstractContainer,IJclExtendedCollection,IJclExtendedList,IJclExtendedIterator, IJclExtendedContainer\, IJclExtendedEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AValue,Extended,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TJclExtendedLinkedListIterator,IJclExtendedIterator,IJclExtendedList,IJclExtendedEqualityComparer,TJclExtendedLinkedListItem,const ,AValue,Extended,0.0,GetValue,SetValue)*)

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatLinkedList = TJclExtendedLinkedList;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatLinkedList = TJclDoubleLinkedList;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatLinkedList = TJclSingleLinkedList;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLLINKEDLISTTYPESINT(TJclIntegerLinkedListItem,Integer)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclIntegerLinkedListItem,TJclIntegerLinkedList,TJclIntegerAbstractContainer,IJclIntegerCollection,IJclIntegerList,IJclIntegerIterator, IJclIntegerEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,AValue,Integer,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TJclIntegerLinkedListIterator,IJclIntegerIterator,IJclIntegerList,IJclIntegerEqualityComparer,TJclIntegerLinkedListItem,,AValue,Integer,0,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTTYPESINT(TJclCardinalLinkedListItem,Cardinal)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclCardinalLinkedListItem,TJclCardinalLinkedList,TJclCardinalAbstractContainer,IJclCardinalCollection,IJclCardinalList,IJclCardinalIterator, IJclCardinalEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,AValue,Cardinal,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TJclCardinalLinkedListIterator,IJclCardinalIterator,IJclCardinalList,IJclCardinalEqualityComparer,TJclCardinalLinkedListItem,,AValue,Cardinal,0,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTTYPESINT(TJclInt64LinkedListItem,Int64)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclInt64LinkedListItem,TJclInt64LinkedList,TJclInt64AbstractContainer,IJclInt64Collection,IJclInt64List,IJclInt64Iterator, IJclInt64EqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AValue,Int64,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TJclInt64LinkedListIterator,IJclInt64Iterator,IJclInt64List,IJclInt64EqualityComparer,TJclInt64LinkedListItem,const ,AValue,Int64,0,GetValue,SetValue)*)

{$IFNDEF CLR}
(*$JPPEXPANDMACRO JCLLINKEDLISTTYPESINT(TJclPtrLinkedListItem,Pointer)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclPtrLinkedListItem,TJclPtrLinkedList,TJclPtrAbstractContainer,IJclPtrCollection,IJclPtrList,IJclPtrIterator, IJclPtrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,APtr,Pointer,GetPointer,SetPointer)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TJclPtrLinkedListIterator,IJclPtrIterator,IJclPtrList,IJclPtrEqualityComparer,TJclPtrLinkedListItem,,AValue,Pointer,nil,GetPointer,SetPointer)*)
{$ENDIF ~CLR}

(*$JPPEXPANDMACRO JCLLINKEDLISTTYPESINT(TJclLinkedListItem,TObject)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TJclLinkedListItem,TJclLinkedList,TJclAbstractContainer,IJclCollection,IJclList,IJclIterator, IJclObjectOwner\, IJclEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,; AOwnsObjects: Boolean,,AObject,TObject,GetObject,SetObject)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TJclLinkedListIterator,IJclIterator,IJclList,IJclEqualityComparer,TJclLinkedListItem,,AObject,TObject,nil,GetObject,SetObject)*)

  {$IFDEF SUPPORTS_GENERICS}
(*$JPPEXPANDMACRO JCLLINKEDLISTTYPESINT(TJclLinkedListItem<T>,T)*)

  TJclLinkedListIterator<T> = class;

(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TLinkedListItem,TJclLinkedList<T>,TJclAbstractContainer<T>,IJclCollection<T>,IJclList<T>,IJclIterator<T>, IJclItemOwner<T>\, IJclEqualityComparer<T>\,,
  protected
    type
      TLinkedListItem = TJclLinkedListItem<T>;
      TLinkedListIterator = TJclLinkedListIterator<T>;,,; AOwnsItems: Boolean,const ,AItem,T,GetItem,SetItem)*)

(*$JPPEXPANDMACRO JCLLINKEDLISTITRINT(TJclLinkedListIterator<T>,IJclIterator<T>,IJclList<T>,IJclEqualityComparer<T>,TJclLinkedList<T>.TLinkedListItem,const ,AItem,T,Default(T),GetItem,SetItem)*)

  // E = External helper to compare items
  // GetHashCode is never called
  TJclLinkedListE<T> = class(TJclLinkedList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclCollection<T>, IJclList<T>, IJclEqualityComparer<T>,
    IJclItemOwner<T>)
  private
    FEqualityComparer: IJclEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AEqualityComparer: IJclEqualityComparer<T>; const ACollection: IJclCollection<T>;
      AOwnsItems: Boolean);
    property EqualityComparer: IJclEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclLinkedListF<T> = class(TJclLinkedList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclCollection<T>, IJclList<T>, IJclEqualityComparer<T>,
    IJclItemOwner<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
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

{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclIntfLinkedList,TJclIntfLinkedListItem,IJclIntfCollection,IJclIntfList,IJclIntfIterator,TJclIntfLinkedListIterator,,,const ,AInterface,IInterface,nil,GetObject,SetObject,FreeObject)}

function TJclIntfLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

{$JPPDEFINEMACRO ITEMFREE(Item)Item := nil}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TJclIntfLinkedListIterator,IJclIntfIterator,IJclIntfList,IJclIntfEqualityComparer,TJclIntfLinkedListItem,const ,AInterface,IInterface,nil,GetObject,SetObject)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclAnsiStrLinkedList,TJclAnsiStrLinkedListItem,IJclAnsiStrCollection,IJclAnsiStrList,IJclAnsiStrIterator,TJclAnsiStrLinkedListIterator,,,const ,AString,AnsiString,'',GetString,SetString,FreeString)}

function TJclAnsiStrLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

{$JPPDEFINEMACRO ITEMFREE(Item)Item := ''}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TJclAnsiStrLinkedListIterator,IJclAnsiStrIterator,IJclAnsiStrList,IJclAnsiStrEqualityComparer,TJclAnsiStrLinkedListItem,const ,AString,AnsiString,'',GetString,SetString)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclWideStrLinkedList,TJclWideStrLinkedListItem,IJclWideStrCollection,IJclWideStrList,IJclWideStrIterator,TJclWideStrLinkedListIterator,,,const ,AString,WideString,'',GetString,SetString,FreeString)}

function TJclWideStrLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

{$JPPDEFINEMACRO ITEMFREE(Item)Item := ''}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TJclWideStrLinkedListIterator,IJclWideStrIterator,IJclWideStrList,IJclWideStrEqualityComparer,TJclWideStrLinkedListItem,const ,AString,WideString,'',GetString,SetString)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

{$IFDEF SUPPORTS_UNICODE_STRING}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclUnicodeStrLinkedList,TJclUnicodeStrLinkedListItem,IJclUnicodeStrCollection,IJclUnicodeStrList,IJclUnicodeStrIterator,TJclUnicodeStrLinkedListIterator,,,const ,AString,UnicodeString,'',GetString,SetString,FreeString)}

function TJclUnicodeStrLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

{$JPPDEFINEMACRO ITEMFREE(Item)Item := ''}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TJclUnicodeStrLinkedListIterator,IJclUnicodeStrIterator,IJclUnicodeStrList,IJclUnicodeStrEqualityComparer,TJclUnicodeStrLinkedListItem,const ,AString,UnicodeString,'',GetString,SetString)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}
{$ENDIF SUPPORTS_UNICODE_STRING}

{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclSingleLinkedList,TJclSingleLinkedListItem,IJclSingleCollection,IJclSingleList,IJclSingleIterator,TJclSingleLinkedListIterator,,,const ,AValue,Single,0.0,GetValue,SetValue,FreeSingle)}

function TJclSingleLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

{$JPPDEFINEMACRO ITEMFREE(Item)Item := 0.0}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TJclSingleLinkedListIterator,IJclSingleIterator,IJclSingleList,IJclSingleEqualityComparer,TJclSingleLinkedListItem,const ,AValue,Single,0.0,GetValue,SetValue)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclDoubleLinkedList,TJclDoubleLinkedListItem,IJclDoubleCollection,IJclDoubleList,IJclDoubleIterator,TJclDoubleLinkedListIterator,,,const ,AValue,Double,0.0,GetValue,SetValue,FreeDouble)}

function TJclDoubleLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

{$JPPDEFINEMACRO ITEMFREE(Item)Item := 0.0}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TJclDoubleLinkedListIterator,IJclDoubleIterator,IJclDoubleList,IJclDoubleEqualityComparer,TJclDoubleLinkedListItem,const ,AValue,Double,0.0,GetValue,SetValue)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclExtendedLinkedList,TJclExtendedLinkedListItem,IJclExtendedCollection,IJclExtendedList,IJclExtendedIterator,TJclExtendedLinkedListIterator,,,const ,AValue,Extended,0.0,GetValue,SetValue,FreeExtended)}

function TJclExtendedLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

{$JPPDEFINEMACRO ITEMFREE(Item)Item := 0.0}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TJclExtendedLinkedListIterator,IJclExtendedIterator,IJclExtendedList,IJclExtendedEqualityComparer,TJclExtendedLinkedListItem,const ,AValue,Extended,0.0,GetValue,SetValue)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclIntegerLinkedList,TJclIntegerLinkedListItem,IJclIntegerCollection,IJclIntegerList,IJclIntegerIterator,TJclIntegerLinkedListIterator,,,,AValue,Integer,0,GetValue,SetValue,FreeInteger)}

function TJclIntegerLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

{$JPPDEFINEMACRO ITEMFREE(Item)Item := 0}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TJclIntegerLinkedListIterator,IJclIntegerIterator,IJclIntegerList,IJclIntegerEqualityComparer,TJclIntegerLinkedListItem,,AValue,Integer,0,GetValue,SetValue)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclCardinalLinkedList,TJclCardinalLinkedListItem,IJclCardinalCollection,IJclCardinalList,IJclCardinalIterator,TJclCardinalLinkedListIterator,,,,AValue,Cardinal,0,GetValue,SetValue,FreeCardinal)}

function TJclCardinalLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

{$JPPDEFINEMACRO ITEMFREE(Item)Item := 0}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TJclCardinalLinkedListIterator,IJclCardinalIterator,IJclCardinalList,IJclCardinalEqualityComparer,TJclCardinalLinkedListItem,,AValue,Cardinal,0,GetValue,SetValue)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclInt64LinkedList,TJclInt64LinkedListItem,IJclInt64Collection,IJclInt64List,IJclInt64Iterator,TJclInt64LinkedListIterator,,,const ,AValue,Int64,0,GetValue,SetValue,FreeInt64)}

function TJclInt64LinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64LinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

{$JPPDEFINEMACRO ITEMFREE(Item)Item := 0}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TJclInt64LinkedListIterator,IJclInt64Iterator,IJclInt64List,IJclInt64EqualityComparer,TJclInt64LinkedListItem,const ,AValue,Int64,0,GetValue,SetValue)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

{$IFNDEF CLR}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclPtrLinkedList,TJclPtrLinkedListItem,IJclPtrCollection,IJclPtrList,IJclPtrIterator,TJclPtrLinkedListIterator,,,,APtr,Pointer,nil,GetPointer,SetPointer,FreePointer)}

function TJclPtrLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

{$JPPDEFINEMACRO ITEMFREE(Item)Item := nil}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TJclPtrLinkedListIterator,IJclPtrIterator,IJclPtrList,IJclPtrEqualityComparer,TJclPtrLinkedListItem,,AValue,Pointer,nil,GetPointer,SetPointer)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}
{$ENDIF ~CLR}

{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclLinkedList,TJclLinkedListItem,IJclCollection,IJclList,IJclIterator,TJclLinkedListIterator,; AOwnsObjects: Boolean,AOwnsObjects,,AObject,TObject,nil,GetObject,SetObject,FreeObject)}

function TJclLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclLinkedList.Create(nil, False);
  AssignPropertiesTo(Result);
end;

{$JPPDEFINEMACRO ITEMFREE(AObject)(FownList as IJclObjectOwner).FreeObject(AObject)}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TJclLinkedListIterator,IJclIterator,IJclList,IJclEqualityComparer,TJclLinkedListItem,,AObject,TObject,nil,GetObject,SetObject)*)
{$JPPUNDEFMACRO ITEMFREE(AObject)}

{$IFDEF SUPPORTS_GENERICS}

{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclLinkedList<T>,TLinkedListItem,IJclCollection<T>,IJclList<T>,IJclIterator<T>,TLinkedListIterator,; AOwnsItems: Boolean,AOwnsItems,const ,AItem,T,Default(T),GetItem,SetItem,FreeItem)}

{$JPPDEFINEMACRO ITEMFREE(AItem)(FownList as IJclItemOwner<T>).FreeItem(AItem)}
(*$JPPEXPANDMACRO JCLLINKEDLISTITRIMP(TJclLinkedListIterator<T>,IJclIterator<T>,IJclList<T>,IJclEqualityComparer<T>,TJclLinkedList<T>.TLinkedListItem,const ,AItem,T,Default(T),GetItem,SetItem)*)
{$JPPUNDEFMACRO ITEMFREE(AObject)}

//=== { TJclLinkedListE<T> } =================================================

constructor TJclLinkedListE<T>.Create(const AEqualityComparer: IJclEqualityComparer<T>;
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
    Result := EqualityComparer.ItemsEqual(A, B)
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

