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
{ The Original Code is ArraySet.pas.                                                               }
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

unit JclArraySets;

{$I jcl.inc}

interface

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
  JclBase, JclAbstractContainers, JclContainerIntf, JclArrayLists;
{$I containers\JclArraySets.imp}
type
(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclIntfArraySet,TJclIntfArrayList,IJclIntfCollection,IJclIntfList,IJclIntfArray,IJclIntfSet, IJclContainer\, IJclIntfEqualityComparer\, IJclIntfComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainer; override;,,,const AInterface: IInterface)*)
(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclStrArraySet,TJclStrArrayList,IJclStrCollection,IJclStrList,IJclStrArray,IJclStrSet, IJclContainer\, IJclStrContainer\, IJclStrFlatContainer\, IJclStrEqualityComparer\, IJclStrComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainer; override;,, override;,const AString: string)*)
(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclArraySet,TJclArrayList,IJclCollection,IJclList,IJclArray,IJclSet, IJclContainer\, IJclObjectOwner\, IJclEqualityComparer\, IJclComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainer; override;,,,AObject: TObject)*)

  {$IFDEF SUPPORTS_GENERICS}
(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclArraySet<T>,TJclArrayList<T>,IJclCollection<T>,IJclList<T>,IJclArray<T>,IJclSet<T>, IJclContainer\, IJclItemOwner<T>\, IJclEqualityComparer<T>\, IJclComparer<T>\,,,,,,const AItem: T)*)

  // E = External helper to compare items
  TJclArraySetE<T> = class(TJclArraySet<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FComparer: IComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    function ItemsCompare(const A, B: T): Integer; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AComparer: IComparer<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const AComparer: IComparer<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;

    property Comparer: IComparer<T> read FComparer write FComparer;
  end;

  // F = Function to compare items
  TJclArraySetF<T> = class(TJclArraySet<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FCompare: TCompare<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    function ItemsCompare(const A, B: T): Integer; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const ACompare: TCompare<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const ACompare: TCompare<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;

    property Compare: TCompare<T> read FCompare write FCompare;
  end;

  // I = Items can compare themselves to others
  TJclArraySetI<T: IComparable<T>> = class(TJclArraySet<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclSet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable,
    IJclGrowable)
  protected
    function ItemsCompare(const A, B: T): Integer; override;
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

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfArraySet.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclIntfArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclIntfArraySet,IJclIntfCollection,IJclIntfIterator,const AInterface: IInterface,AInterface,nil,GetObject)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclStrArraySet.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStrArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclStrArraySet,IJclStrCollection,IJclStrIterator,const AString: string,AString,'',GetString)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclArraySet.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclArraySet.Create(Size, False);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclArraySet,IJclCollection,IJclIterator,AObject: TObject,AObject,nil,GetObject)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$IFDEF SUPPORTS_GENERICS}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER}
{$JPPEXPANDMACRO JCLARRAYSETIMP(TJclArraySet<T>,IJclCollection<T>,IJclIterator<T>,const AItem: T,AItem,Default(T),GetItem)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

//=== { TJclArraySetE<T> } ===================================================

constructor TJclArraySetE<T>.Create(const AComparer: IComparer<T>; ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FComparer := AComparer;
end;

constructor TJclArraySetE<T>.Create(const AComparer: IComparer<T>; const ACollection: IJclCollection<T>;
  AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FComparer := AComparer;
end;

procedure TJclArraySetE<T>.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclArraySetE<T> then
    TJclArraySetE<T>(Dest).FComparer := Comparer;
end;

function TJclArraySetE<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclArraySetE<T>.Create(Comparer, Size, False);
  AssignPropertiesTo(Result);
end;

function TJclArraySetE<T>.ItemsCompare(const A, B: T): Integer;
begin
  if Comparer = nil then
    raise EJclNoComparerError.Create;
  Result := Comparer.Compare(A, B);
end;

function TJclArraySetE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Comparer = nil then
    raise EJclNoComparerError.Create;
  Result := Comparer.Compare(A, B) = 0;
end;

//=== { TJclArraySetF<T> } ===================================================

constructor TJclArraySetF<T>.Create(const ACompare: TCompare<T>; ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FCompare := ACompare;
end;

constructor TJclArraySetF<T>.Create(const ACompare: TCompare<T>; const ACollection: IJclCollection<T>;
  AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FCompare := ACompare;
end;

procedure TJclArraySetF<T>.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclArraySetF<T> then
    TJclArraySetF<T>(Dest).FCompare := Compare;
end;

function TJclArraySetF<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclArraySetF<T>.Create(Compare, Size, False);
  AssignPropertiesTo(Result);
end;

function TJclArraySetF<T>.ItemsCompare(const A, B: T): Integer;
begin
  if not Assigned(Compare) then
    raise EJclNoComparerError.Create;
  Result := Compare(A, B);
end;

function TJclArraySetF<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if not Assigned(Compare) then
    raise EJclNoComparerError.Create;
  Result := Compare(A, B) = 0;
end;

//=== { TJclArraySetI<T> } ===================================================

function TJclArraySetI<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclArraySetI<T>.Create(Size, False);
  AssignPropertiesTo(Result);
end;

function TJclArraySetI<T>.ItemsCompare(const A, B: T): Integer;
begin
  Result := A.CompareTo(B);
end;

function TJclArraySetI<T>.ItemsEqual(const A, B: T): Boolean;
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

