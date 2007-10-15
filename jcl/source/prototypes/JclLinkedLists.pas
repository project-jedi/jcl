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
(*$JPPEXPANDMACRO JCLLINKEDLISTINT(IInterface,TJclIntfLinkedListItem,TJclIntfLinkedList,TJclIntfContainer,IJclIntfCollection,IJclIntfList,IJclIntfIterator, IJclContainer\, IJclIntfEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainer; override;,,,,const AInterface: IInterface,GetObject,SetObject)*)
(*$JPPEXPANDMACRO JCLLINKEDLISTINT(string,TJclStrLinkedListItem,TJclStrLinkedList,TJclStrAbstractCollection,IJclStrCollection,IJclStrList,IJclStrIterator, IJclContainer\, IJclStrContainer\, IJclStrEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainer; override;,, override;,,const AString: string,GetString,SetString)*)
(*$JPPEXPANDMACRO JCLLINKEDLISTINT(TObject,TJclLinkedListItem,TJclLinkedList,TJclContainer,IJclCollection,IJclList,IJclIterator, IJclContainer\, IJclObjectOwner\, IJclEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainer; override;,,,; AOwnsObjects: Boolean,AObject: TObject,GetObject,SetObject)*)

  {$IFDEF SUPPORTS_GENERICS}
(*$JPPEXPANDMACRO JCLLINKEDLISTINT(T,TJclLinkedListItem<T>,TJclLinkedList<T>,TJclContainer<T>,IJclCollection<T>,IJclList<T>,IJclIterator<T>, IJclContainer\, IJclItemOwner<T>\, IJclEqualityComparer<T>\,,,,,,; AOwnsItems: Boolean,const AItem: T,GetItem,SetItem)*)

  // E = External helper to compare items
  // GetHashCode is never called
  TJclLinkedListE<T> = class(TJclLinkedList<T>, IJclCollection<T>, IJclList<T>, IJclContainer, IJclEqualityComparer<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclItemOwner<T>)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; const ACollection: IJclCollection<T>;
      AOwnsItems: Boolean);
    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclLinkedListF<T> = class(TJclLinkedList<T>, IJclCollection<T>, IJclList<T>, IJclContainer, IJclEqualityComparer<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclItemOwner<T>)
  private
    FEqualityCompare: TEqualityCompare<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const ACollection: IJclCollection<T>;
      AOwnsItems: Boolean);
    property EqualityCompare: TEqualityCompare<T> read FEqualityCompare write FEqualityCompare;
  end;

  // I = Items can compare themselves to an other
  TJclLinkedListI<T: IEquatable<T>> = class(TJclLinkedList<T>, IJclCollection<T>, IJclList<T>, IJclContainer, IJclEqualityComparer<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclItemOwner<T>)
  protected
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
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
(*$JPPEXPANDMACRO JCLLINKEDLISTITR(TStrItr,IJclStrIterator,IJclStrList,IJclStrEqualityComparer,TJclStrLinkedListItem,string,const AString: string,AString,'',GetString,SetString)*)
{$JPPUNDEFMACRO ITEMFREE(Item)}

{$JPPDEFINEMACRO ITEMFREE(AObject)(FownList as IJclObjectOwner).FreeObject(AObject)}
(*$JPPEXPANDMACRO JCLLINKEDLISTITR(TItr,IJclIterator,IJclList,IJclEqualityComparer,TJclLinkedListItem,TObject,AObject: TObject,AObject,nil,GetObject,SetObject)*)
{$JPPUNDEFMACRO ITEMFREE(AObject)}

{$IFDEF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO ITEMFREE(AItem)(FownList as IJclItemOwner<T>).FreeItem(AItem)}
(*$JPPEXPANDMACRO JCLLINKEDLISTITR(TItr<T>,IJclIterator<T>,IJclList<T>,IJclEqualityComparer<T>,TJclLinkedListItem<T>,T,const AItem: T,AItem,Default(T),GetItem,SetItem)*)
{$JPPUNDEFMACRO ITEMFREE(AObject)}

{$ENDIF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfLinkedList.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclIntfLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclIntfLinkedList,IInterface,TJclIntfLinkedListItem,IJclIntfCollection,IJclIntfList,IJclIntfIterator,TIntfItr,,,const AInterface: IInterface,AInterface,nil,GetObject,SetObject,FreeObject)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclStrLinkedList.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStrLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;
}
{$JPPEXPANDMACRO JCLLINKEDLISTIMP(TJclStrLinkedList,string,TJclStrLinkedListItem,IJclStrCollection,IJclStrList,IJclStrIterator,TStrItr,,,const AString: string,AString,'',GetString,SetString,FreeString)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclLinkedList.CreateEmptyContainer: TJclAbstractContainer;
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

procedure TJclLinkedListE<T>.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclLinkedListE<T> then
    TJclLinkedListE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclLinkedListE<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclLinkedListE<T>.Create(EqualityComparer, nil, False);
  AssignPropertiesTo(Result);
end;

function TJclLinkedListE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityComparer.Equals(A, B);
end;

//=== { TJclLinkedListF<T> } =================================================

constructor TJclLinkedListF<T>.Create(const AEqualityCompare: TEqualityCompare<T>;
  const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FEqualityCompare := AEqualityCompare;
end;

procedure TJclLinkedListF<T>.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclLinkedListF<T> then
    TJclLinkedListF<T>(Dest).FEqualityCompare := FEqualityCompare;
end;

function TJclLinkedListF<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclLinkedListF<T>.Create(EqualityCompare, nil, False);
  AssignPropertiesTo(Result);
end;

function TJclLinkedListF<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if not Assigned(EqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityCompare(A, B);
end;

//=== { TJclLinkedListI<T> } =================================================

function TJclLinkedListI<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclLinkedListI<T>.Create(nil, False);
  AssignPropertiesTo(Result);
end;

function TJclLinkedListI<T>.ItemsEqual(const A, B: T): Boolean;
begin
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

