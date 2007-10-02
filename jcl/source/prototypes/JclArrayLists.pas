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
{ The Original Code is ArrayList.pas.                                                              }
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

unit JclArrayLists;

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
{$I Containers\JclArrayLists.imp}
type
{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclIntfArrayList,TJclAbstractContainer,IJclIntfCollection,IJclIntfList,IJclIntfArray,IJclIntfIterator,JclBase.TDynIInterfaceArray,,,,,,,const AInterface: IInterface,IInterface,GetObject,SetObject)}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclStrArrayList,TJclStrCollection,IJclStrCollection,IJclStrList,IJclStrArray,IJclStrIterator,JclBase.TDynStringArray,,,,, override;,,const AString: string,string,GetString,SetString)}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclArrayList,TJclAbstractContainer,IJclCollection,IJclList,IJclArray,IJclIterator,JclBase.TDynObjectArray,,
    FOwnsObjects: Boolean;,
    procedure FreeObject(var AObject: TObject);,
    property OwnsObjects: Boolean read FOwnsObjects;,,; AOwnsObjects: Boolean = True,AObject: TObject,TObject,GetObject,SetObject)}

  {$IFDEF SUPPORTS_GENERICS}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclArrayList<T>,TJclAbstractContainer,IJclCollection<T>,IJclList<T>,IJclArray<T>,IJclIterator<T>,TJclBase<T>.TDynArray,,
    FOwnsItems: Boolean;,
    function ItemsEqual(const A, B: T): Boolean; virtual; abstract;
    function CreateEmptyArrayList(const ACollection: IJclCollection<T>): TJclArrayList<T>; overload; virtual; abstract;
    function CreateEmptyArrayList(ACapacity: Integer): TJclArrayList<T>; overload; virtual; abstract;
    procedure FreeItem(var AItem: T);,
    property OwnsItems: Boolean read FOwnsItems;,,; AOwnsItems: Boolean = True,const AItem: T,T,GetItem,SetItem)}

  // E = External helper to compare items for equality
  // GetHashCode is not used
  TJclArrayListE<T> = class(TJclArrayList<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyArrayList(const ACollection: IJclCollection<T>): TJclArrayList<T>; override;
    function CreateEmptyArrayList(ACapacity: Integer): TJclArrayList<T>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; ACapacity: Integer = DefaultContainerCapacity;
      AOwnsItems: Boolean = True); overload;
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; const ACollection: IJclCollection<T>;
      AOwnsItems: Boolean = True); overload;

    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclArrayListF<T> = class(TJclArrayList<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FEqualityCompare: TEqualityCompare<T>;
  protected
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyArrayList(const ACollection: IJclCollection<T>): TJclArrayList<T>; override;
    function CreateEmptyArrayList(ACapacity: Integer): TJclArrayList<T>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer = DefaultContainerCapacity;
      AOwnsItems: Boolean = True); overload;
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const ACollection: IJclCollection<T>;
      AOwnsItems: Boolean = True); overload;

    property EqualityCompare: TEqualityCompare<T> read FEqualityCompare write FEqualityCompare;
  end;

  // I = Items can compare themselves to others
  TJclArrayListI<T: IEquatable<T>> = class(TJclArrayList<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  protected
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyArrayList(const ACollection: IJclCollection<T>): TJclArrayList<T>; override;
    function CreateEmptyArrayList(ACapacity: Integer): TJclArrayList<T>; override;
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

{$JPPEXPANDMACRO JCLITRIMP(TIntfItr,IJclIntfIterator,IJclIntfList,const AInterface: IInterface,AInterface,IInterface,GetObject,SetObject)}
{$JPPEXPANDMACRO JCLITRIMP(TStrItr,IJclStrIterator,IJclStrList,const AString: string,AString,string,GetString,SetString)}
{$JPPEXPANDMACRO JCLITRIMP(TItr,IJclIterator,IJclList,AObject: TObject,AObject,TObject,GetObject,SetObject)}

{$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO JCLITRIMP(TItr<T>,IJclIterator<T>,IJclList<T>,const AItem: T,AItem,T,GetItem,SetItem)}
{$ENDIF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO ITEMSEQUAL(Item1, Item2)(Item1 =Item2)}
{$JPPDEFINEMACRO ITEMFREE(AItem)AItem := nil;}
{$JPPDEFINEMACRO LISTCREATE(Param1)TJclIntfArrayList.Create(Param1);}
{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclIntfArrayList,,,,,Result := FElementData[Index];,IJclIntfCollection,IJclIntfIterator,TIntfItr,IJclIntfList,const AInterface: IInterface,AInterface,GetObject,SetObject,IInterface,nil,JclBase.)}
{$JPPUNDEFMACRO ITEMSEQUAL(Item1, Item2)}
{$JPPUNDEFMACRO ITEMFREE(AItem)}
{$JPPUNDEFMACRO LISTCREATE(Param1)}

{$JPPDEFINEMACRO ITEMSEQUAL(Item1, Item2)(Item1 =Item2)}
{$JPPDEFINEMACRO ITEMFREE(AItem)AItem := '';}
{$JPPDEFINEMACRO LISTCREATE(Param1)TJclStrArrayList.Create(Param1);}
{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclStrArrayList,,,,,Result := FElementData[Index];,IJclStrCollection,IJclStrIterator,TStrItr,IJclStrList,const AString: string,AString,GetString,SetString,string,'',JclBase.)}
{$JPPUNDEFMACRO ITEMSEQUAL(Item1, Item2)}
{$JPPUNDEFMACRO ITEMFREE(AItem)}
{$JPPUNDEFMACRO LISTCREATE(Param1)}

{$JPPDEFINEMACRO ITEMSEQUAL(Item1, Item2)(Item1 =Item2)}
{$JPPDEFINEMACRO ITEMFREE(AItem)FreeObject(AItem);}
{$JPPDEFINEMACRO LISTCREATE(Param1)TJclArrayList.Create(Param1, False); // Only one container can own objects}
{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclArrayList,; AOwnsObjects: Boolean,FOwnsObjects := AOwnsObjects;,\, AOwnsObjects,
procedure TJclArrayList.FreeObject(var AObject: TObject);
begin
  if FOwnsObjects then
    FreeAndNil(AObject)
  else
    AObject := nil;
end;
,if OwnsObjects then
      Result := nil
    else
      Result := FElementData[Index];,IJclCollection,IJclIterator,TItr,IJclList,AObject: TObject,AObject,GetObject,SetObject,TObject,nil,JclBase.)}
{$JPPUNDEFMACRO ITEMSEQUAL(Item1, Item2)}
{$JPPUNDEFMACRO ITEMFREE(AItem)}
{$JPPUNDEFMACRO LISTCREATE(Param1)}
{$IFDEF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO ITEMSEQUAL(Item1, Item2)ItemsEqual(Item1,Item2)}
{$JPPDEFINEMACRO ITEMFREE(AItem)FreeItem(AItem);}
{$JPPDEFINEMACRO LISTCREATE(Param1)CreateEmptyArrayList(Param1, False); // Only one container can own items}
{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclArrayList<T>,; AOwnsItems: Boolean,FOwnsItems := AOwnsItems;,\, AOwnsItems,
procedure TJclArrayList<T>.FreeItem(var AItem: T);
begin
  if FOwnsItems then
    FreeAndNil(AItem)
  else
    AItem := Default(T);
end;
,if OwnsItems then
      Result := Default(T)
    else
      Result := FElementData[Index];,IJclCollection<T>,IJclIterator<T>,TItr<T>,IJclList<T>,const AItem: T,AItem,GetItem,SetItem,T,Default(T),TJclBase<T>.)}
{$JPPUNDEFMACRO ITEMSEQUAL(Item1, Item2)}
{$JPPUNDEFMACRO ITEMFREE(AItem)}
{$JPPUNDEFMACRO LISTCREATE(Param1)}

//=== { TJclArrayListE<T> } ==================================================

constructor TJclArrayListE<T>.Create(const AEqualityComparer: IEqualityComparer<T>; ACapacity: Integer;
  AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

constructor TJclArrayListE<T>.Create(const AEqualityComparer: IEqualityComparer<T>;
  const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

function TJclArrayListE<T>.CreateEmptyArrayList(ACapacity: Integer): TJclArrayList<T>;
begin
  Result := TJclArrayListE<T>.Create(EqualityComparer, ACapacity, False);
end;

function TJclArrayListE<T>.CreateEmptyArrayList(const ACollection: IJclCollection<T>): TJclArrayList<T>;
begin
  Result := TJclArrayListE<T>.Create(EqualityComparer, ACollection, False);
end;

function TJclArrayListE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityComparer.Equals(A, B);
end;

//=== { TJclArrayListF<T> } ==================================================

constructor TJclArrayListF<T>.Create(const AEqualityCompare: TEqualityCompare<T>;
  ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityCompare := AEqualityCompare;
end;

constructor TJclArrayListF<T>.Create(const AEqualityCompare: TEqualityCompare<T>; const ACollection: IJclCollection<T>;
  AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FEqualityCompare := AEqualityCompare;
end;

function TJclArrayListF<T>.CreateEmptyArrayList(ACapacity: Integer): TJclArrayList<T>;
begin
  Result := TJclArrayListF<T>.Create(EqualityCompare, ACapacity, False);
end;

function TJclArrayListF<T>.CreateEmptyArrayList(const ACollection: IJclCollection<T>): TJclArrayList<T>;
begin
  Result := TJclArrayListF<T>.Create(EqualityCompare, ACollection, False);
end;

function TJclArrayListF<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if not Assigned(EqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityCompare(A, B);
end;

//=== { TJclArrayListI<T> } ==================================================

function TJclArrayListI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  Result := A.Equals(B);
end;

function TJclArrayListI<T>.CreateEmptyArrayList(ACapacity: Integer): TJclArrayList<T>;
begin
  Result := TJclArrayListI<T>.Create(ACapacity, False);
end;

function TJclArrayListI<T>.CreateEmptyArrayList(const ACollection: IJclCollection<T>): TJclArrayList<T>;
begin
  Result := TJclArrayListI<T>.Create(ACollection, False);
end;

{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

