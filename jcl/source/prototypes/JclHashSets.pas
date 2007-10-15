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
{ The Original Code is HashSet.pas.                                                                }
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

unit JclHashSets;

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
  JclAlgorithms,
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclContainerIntf, JclHashMaps;
{$I containers\JclHashSets.imp}
type
  {$IFDEF SUPPORTS_GENERICS}
  TRefUnique = class;
  TRefUnique = class(TEquatable<TRefUnique>)
  end;
  {$ELSE ~SUPPORTS_GENERICS}
  TRefUnique = TObject;
  {$ENDIF ~SUPPORTS_GENERICS}

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclIntfHashSet,TJclIntfContainer,IJclIntfCollection,IJclIntfSet,IJclIntfMap,IJclIntfIterator, IJclContainer\, IJclIntfEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainer; override;,,,
    constructor Create(ACapacity: Integer); overload;,const AInterface: IInterface,AInterface,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclStrHashSet,TJclStrAbstractCollection,IJclStrCollection,IJclStrSet,IJclStrMap,IJclStrIterator, IJclContainer\, IJclStrContainer\, IJclStrEqualityComparer\,,,
    { IJclAnsiStrContainer }
    function GetCaseSensitive: Boolean; override;
    function GetEncoding: TJclAnsiStrEncoding; override;
    procedure SetCaseSensitive(Value: Boolean); override;
    procedure SetEncoding(Value: TJclAnsiStrEncoding); override;
    function CreateEmptyContainer: TJclAbstractContainer; override;,, override;,
    constructor Create(ACapacity: Integer); overload;,const AString: string,AString,string)*)

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclHashSet,TJclContainer,IJclCollection,IJclSet,IJclMap,IJclIterator, IJclContainer\, IJclObjectOwner\, IJclEqualityComparer\,,,
    { IJclObjectOwner }
    function FreeObject(var AObject: TObject): TObject; override;
    function GetOwnsObjects: Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainer; override;,,,
    constructor Create(ACapacity: Integer; AOwnsObjects: Boolean); overload;,AObject: TObject,AObject,TObject)*)

  {$IFDEF SUPPORTS_GENERICS}

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclHashSet<T>,TJclContainer<T>,IJclCollection<T>,IJclSet<T>,IJclMap<T\, TRefUnique>,IJclIterator<T>, IJclContainer\, IJclItemOwner<T>\, IJclEqualityComparer<T>\,,,
    { IJclItemOwner<T> }
    function FreeItem(var AItem: T): T; override;
    function GetOwnsItems: Boolean; override;,,,,const AItem: T,AItem,T)*)

  // E = External helper to compare items for equality
  TJclHashSetE<T> = class(TJclHashSet<T>, IJclCollection<T>, IJclSet<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; const AMap: IJclMap<T, TRefUnique>); overload;
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; const AComparer: IComparer<T>;
      ACapacity: Integer; AOwnsItems: Boolean); overload;

    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclHashSetF<T> = class(TJclHashSet<T>, IJclCollection<T>, IJclSet<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable)
  private
    FEqualityCompare: TEqualityCompare<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const AMap: IJclMap<T, TRefUnique>); overload;
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const AHash: THash<T>; const ACompare: TCompare<T>;
      ACapacity: Integer; AOwnsItems: Boolean); overload;

    property EqualityCompare: TEqualityCompare<T> read FEqualityCompare write FEqualityCompare;
  end;

  // I = Items can compare themselves to an other
  TJclHashSetI<T: IEquatable<T>, IComparable<T>, IHashable> = class(TJclHashSet<T>, IJclCollection<T>, IJclSet<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable)
  protected
    function CreateEmptyContainer: TJclAbstractContainer; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AMap: IJclMap<T, TRefUnique>); overload;
    constructor Create(ACapacity: Integer; AOwnsItems: Boolean); overload;
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

var
  GlobalRefUnique: TRefUnique = nil;

function RefUnique: TRefUnique;
begin
  // We keep the reference till program end. A unique memory address is not
  // possible under a garbage collector.
  if GlobalRefUnique = nil then
    GlobalRefUnique := TRefUnique.Create;
  Result := GlobalRefUnique;
end;

{$JPPDEFINEMACRO CONSTRUCTORADDITIONAL
constructor TJclIntfHashSet.Create(ACapacity: Integer);
begin
  Create(TJclIntfHashMap.Create(ACapacity, False));
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfHashSet.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclIntfHashSet.Create(FMap.Size);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO GETTERADDITIONAL}
{$JPPDEFINEMACRO SETTERADDITIONAL}
{$JPPDEFINEMACRO FREEITEM}
{$JPPDEFINEMACRO GETOWNSITEMS}
(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclIntfHashSet,IJclIntfMap,IJclIntfCollection,IJclIntfIterator,,const AInterface: IInterface,AInterface)*)
{$JPPUNDEFMACRO CONSTRUCTORADDITIONAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO GETTERADDITIONAL}
{$JPPUNDEFMACRO SETTERADDITIONAL}
{$JPPUNDEFMACRO FREEITEM}
{$JPPUNDEFMACRO GETOWNSITEMS}

{$JPPDEFINEMACRO CONSTRUCTORADDITIONAL
constructor TJclStrHashSet.Create(ACapacity: Integer);
begin
  Create(TJclStrHashMap.Create(ACapacity, False));
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclStrHashSet.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStrHashSet.Create(FMap.Size);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO GETTERADDITIONAL
function TJclStrHashSet.GetCaseSensitive: Boolean;
begin
  Result := FMap.GetCaseSensitive;
end;

function TJclStrHashSet.GetEncoding: TJclAnsiStrEncoding;
begin
  Result := FMap.GetEncoding;
end;
}
{$JPPDEFINEMACRO SETTERADDITIONAL
procedure TJclStrHashSet.SetCaseSensitive(Value: Boolean);
begin
  FMap.SetCaseSensitive(Value);
end;

procedure TJclStrHashSet.SetEncoding(Value: TJclAnsiStrEncoding);
begin
  FMap.SetEncoding(Value);
end;
}
{$JPPDEFINEMACRO FREEITEM}
{$JPPDEFINEMACRO GETOWNSITEMS}
(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclStrHashSet,IJclStrMap,IJclStrCollection,IJclStrIterator,,const AString: string,AString)*)
{$JPPUNDEFMACRO CONSTRUCTORADDITIONAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO GETTERADDITIONAL}
{$JPPUNDEFMACRO SETTERADDITIONAL}
{$JPPUNDEFMACRO FREEITEM}
{$JPPUNDEFMACRO GETOWNSITEMS}

{$JPPDEFINEMACRO CONSTRUCTORADDITIONAL
constructor TJclHashSet.Create(ACapacity: Integer; AOwnsObjects: Boolean);
begin
  Create(TJclHashMap.Create(ACapacity, AOwnsObjects, False));
end;
}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclHashSet.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclHashSet.Create(FMap.Size, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO GETTERADDITIONAL}
{$JPPDEFINEMACRO SETTERADDITIONAL}
{$JPPDEFINEMACRO FREEITEM
function TJclHashSet.FreeObject(var AObject: TObject): TObject;
begin
  Result := (FMap as IJclKeyOwner).FreeKey(AObject);
end;
}
{$JPPDEFINEMACRO GETOWNSITEMS
function TJclHashSet.GetOwnsObjects: Boolean;
begin
  Result := (FMap as IJclKeyOwner).GetOwnsKeys;
end;
}
(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclHashSet,IJclMap,IJclCollection,IJclIterator,\, False,AObject: TObject,AObject)*)
{$JPPUNDEFMACRO CONSTRUCTORADDITIONAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO GETTERADDITIONAL}
{$JPPUNDEFMACRO SETTERADDITIONAL}
{$JPPUNDEFMACRO FREEITEM}
{$JPPUNDEFMACRO GETOWNSITEMS}

{$IFDEF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO CONSTRUCTORADDITIONAL}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER}
{$JPPDEFINEMACRO GETTERADDITIONAL}
{$JPPDEFINEMACRO SETTERADDITIONAL}
{$JPPDEFINEMACRO FREEITEM
function TJclHashSet<T>.FreeItem(var AItem: T): T;
begin
  Result := (FMap as IJclPairOwner<T, TRefUnique>).FreeKey(AItem);
end;
}
{$JPPDEFINEMACRO GETOWNSITEMS
function TJclHashSet<T>.GetOwnsItems: Boolean;
begin
  Result := (FMap as IJclPairOwner<T, TRefUnique>).GetOwnsKeys;
end;
}
(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclHashSet<T>,IJclMap<T\, TRefUnique>,IJclCollection<T>,IJclIterator<T>,\, False,const AItem: T,AItem)*)
{$JPPUNDEFMACRO CONSTRUCTORADDITIONAL}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO ITEMSEQUAL(ItemA,ItemB)}
{$JPPUNDEFMACRO GETTERADDITIONAL}
{$JPPUNDEFMACRO SETTERADDITIONAL}
{$JPPUNDEFMACRO FREEITEM}
{$JPPUNDEFMACRO GETOWNSITEMS}

//=== { TJclHashSetE<T> } ====================================================

constructor TJclHashSetE<T>.Create(const AEqualityComparer: IEqualityComparer<T>; const AMap: IJclMap<T, TRefUnique>);
begin
  inherited Create(AMap);
  FEqualityComparer := AEqualityComparer;
end;

constructor TJclHashSetE<T>.Create(const AEqualityComparer: IEqualityComparer<T>; const AComparer: IComparer<T>;
  ACapacity: Integer; AOwnsItems: Boolean);
begin
  Create(AEqualityComparer, TJclHashMapE<T, TRefUnique>.Create(AEqualityComparer, RefUnique, AComparer, ACapacity, AOwnsItems, False));
end;

procedure TJclHashSetE<T>.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclHashSetE<T> then
    TJclHashSetE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclHashSetE<T>.CreateEmptyContainer: TJclAbstractContainer;
var
  AMap: IJclMap<T, TRefUnique>;
begin
  AMap := (FMap as IJclCloneable).Clone as IJclMap<T, TRefUnique>;
  AMap.Clear;
  Result := TJclHashSetE<T>.Create(FEqualityComparer, AMap);
  AssignPropertiesTo(Result);
end;

function TJclHashSetE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityComparer.Equals(A, B);
end;

//=== { TJclHashSetF<T> } ====================================================

function EqualityCompareEqObjects(Obj1, Obj2: TRefUnique): Boolean;
begin
  Result := Obj1 = Obj2;
end;

constructor TJclHashSetF<T>.Create(const AEqualityCompare: TEqualityCompare<T>; const AMap: IJclMap<T, TRefUnique>);
begin
  inherited Create(AMap);
  FEqualityCompare := AEqualityCompare;
end;

constructor TJclHashSetF<T>.Create(const AEqualityCompare: TEqualityCompare<T>; const AHash: THash<T>; const ACompare: TCompare<T>;
  ACapacity: Integer; AOwnsItems: Boolean);
begin
  Create(AEqualityCompare, TJclHashMapF<T, TRefUnique>.Create(AEqualityCompare, AHash, EqualityCompareEqObjects, ACompare, ACapacity, AOwnsItems, False));
end;

procedure TJclHashSetF<T>.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclHashSetF<T> then
    TJclHashSetF<T>(Dest).FEqualityCompare := FEqualityCompare;
end;

function TJclHashSetF<T>.CreateEmptyContainer: TJclAbstractContainer;
var
  AMap: IJclMap<T, TRefUnique>;
begin
  AMap := (FMap as IJclCloneable).Clone as IJclMap<T, TRefUnique>;
  AMap.Clear;
  Result := TJclHashSetF<T>.Create(FEqualityCompare, AMap);
  AssignPropertiesTo(Result);
end;

function TJclHashSetF<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if not Assigned(EqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityCompare(A, B);
end;

//=== { TJclHashSetI<T> } ====================================================

constructor TJclHashSetI<T>.Create(const AMap: IJclMap<T, TRefUnique>);
begin
  inherited Create(AMap);
end;

constructor TJclHashSetI<T>.Create(ACapacity: Integer; AOwnsItems: Boolean);
begin
  Create(TJclHashMapI<T, TRefUnique>.Create(ACapacity, AOwnsItems, False));
end;

function TJclHashSetI<T>.CreateEmptyContainer: TJclAbstractContainer;
var
  AMap: IJclMap<T, TRefUnique>;
begin
  AMap := (FMap as IJclCloneable).Clone as IJclMap<T, TRefUnique>;
  AMap.Clear;
  Result := TJclHashSetI<T>.Create(AMap);
  AssignPropertiesTo(Result);
end;

function TJclHashSetI<T>.ItemsEqual(const A, B: T): Boolean;
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

