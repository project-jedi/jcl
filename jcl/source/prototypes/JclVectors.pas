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
{ The Original Code is Vector.pas.                                                                 }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Daniele Teti (dade2004)                                                                        }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
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

unit JclVectors;

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
{$I containers\JclVectors.imp}
type
(*$JPPEXPANDMACRO JCLVECTORINT(TJclIntfVector,TJclIntfAbstractContainer,IJclIntfCollection,IJclIntfList,IJclIntfArray,IJclIntfIterator, IJclIntfEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,const AInterface: IInterface,IInterface,nil,JclBase.TDynIInterfaceArray,GetObject,SetObject)*)

(*$JPPEXPANDMACRO JCLVECTORINT(TJclAnsiStrVector,TJclAnsiStrAbstractCollection,IJclAnsiStrCollection,IJclAnsiStrList,IJclAnsiStrArray,IJclAnsiStrIterator, IJclStrContainer\, IJclAnsiStrContainer\, IJclAnsiStrFlatContainer\, IJclAnsiStrEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,, override;,,const AString: AnsiString,AnsiString,'',JclBase.TDynAnsiStringArray,GetString,SetString)*)

(*$JPPEXPANDMACRO JCLVECTORINT(TJclWideStrVector,TJclWideStrAbstractCollection,IJclWideStrCollection,IJclWideStrList,IJclWideStrArray,IJclWideStrIterator, IJclStrContainer\, IJclWideStrContainer\, IJclWideStrFlatContainer\, IJclWideStrEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,, override;,,const AString: WideString,WideString,'',JclBase.TDynWideStringArray,GetString,SetString)*)

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrVector = TJclAnsiStrVector;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrVector = TJclWideStrVector;
  {$ENDIF CONTAINER_WIDESTR}

(*$JPPEXPANDMACRO JCLVECTORINT(TJclVector,TJclAbstractContainer,IJclCollection,IJclList,IJclArray,IJclIterator, IJclObjectOwner\, IJclEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,; AOwnsObjects: Boolean,AObject: TObject,TObject,nil,JclBase.TDynObjectArray,GetObject,SetObject)*)

  {$IFDEF SUPPORTS_GENERICS}

(*$JPPEXPANDMACRO JCLVECTORINT(TJclVector<T>,TJclAbstractContainer<T>,IJclCollection<T>,IJclList<T>,IJclArray<T>,IJclIterator<T>, IJclItemOwner<T>\, IJclEqualityComparer<T>\,,,,,,; AOwnsItems: Boolean,const AItem: T,T,Default(T),TJclBase<T>.TDynArray,GetItem,SetItem)*)

  // E = External helper to compare items for equality (GetHashCode is not used)
  TJclVectorE<T> = class(TJclVector<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclItemOwner<T>)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; ACapacity: Integer; AOwnsItems: Boolean);
    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclVectorF<T> = class(TJclVector<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclItemOwner<T>)
  private
    FEqualityCompare: TEqualityCompare<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer; AOwnsItems: Boolean);
    property EqualityCompare: TEqualityCompare<T> read FEqualityCompare write FEqualityCompare;
  end;

  // I = Items can compare themselves to an other for equality
  TJclVectorI<T: IEquatable<T>> = class(TJclVector<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclItemOwner<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
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

(*$JPPEXPANDMACRO JCLVECTORITR(TIntfItr,IJclIntfIterator,IJclIntfList,const AInterface: IInterface,AInterface,IInterface,GetObject,SetObject)*)

(*$JPPEXPANDMACRO JCLVECTORITR(TAnsiStrItr,IJclAnsiStrIterator,IJclAnsiStrList,const AString: AnsiString,AString,AnsiString,GetString,SetString)*)

(*$JPPEXPANDMACRO JCLVECTORITR(TWideStrItr,IJclWideStrIterator,IJclWideStrList,const AString: WideString,AString,WideString,GetString,SetString)*)

(*$JPPEXPANDMACRO JCLVECTORITR(TItr,IJclIterator,IJclList,AObject: TObject,AObject,TObject,GetObject,SetObject)*)

{$IFDEF SUPPORTS_GENERICS}

(*$JPPEXPANDMACRO JCLVECTORITR(TItr<T>,IJclIterator<T>,IJclList<T>,const AItem: T,AItem,T,GetItem,SetItem)*)

{$ENDIF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfVector.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLVECTORIMP(TJclIntfVector,IJclIntfCollection,IJclIntfList,IJclIntfIterator,TIntfItr,,,const AInterface: IInterface,AInterface,IInterface,nil,GetObject,SetObject,FreeObject,JclBase.MoveArray)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrVector.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLVECTORIMP(TJclAnsiStrVector,IJclAnsiStrCollection,IJclAnsiStrList,IJclAnsiStrIterator,TAnsiStrItr,,,const AString: AnsiString,AString,AnsiString,'',GetString,SetString,FreeString,JclBase.MoveArray)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrVector.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLVECTORIMP(TJclWideStrVector,IJclWideStrCollection,IJclWideStrList,IJclWideStrIterator,TWideStrItr,,,const AString: WideString,AString,WideString,'',GetString,SetString,FreeString,JclBase.MoveArray)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclVector.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLVECTORIMP(TJclVector,IJclCollection,IJclList,IJclIterator,TItr,; AOwnsObjects: Boolean,\, AOwnsObjects,AObject: TObject,AObject,TObject,nil,GetObject,SetObject,FreeObject,JclBase.MoveArray)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$IFDEF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER}
(*$JPPEXPANDMACRO JCLVECTORIMP(TJclVector<T>,IJclCollection<T>,IJclList<T>,IJclIterator<T>,TItr<T>,; AOwnsItems: Boolean,\, AOwnsItems,const AItem: T,AItem,T,Default(T),GetItem,SetItem,FreeItem,TJclBase<T>.MoveArray)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

//=== { TJclVectorE<T> } =====================================================

constructor TJclVectorE<T>.Create(const AEqualityComparer: IEqualityComparer<T>; ACapacity: Integer;
  AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

procedure TJclVectorE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclVectorE<T> then
    TJclVectorE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclVectorE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclVectorE<T>.Create(EqualityComparer, FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclVectorE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityComparer.Equals(A, B);
end;

//=== { TJclVectorF<T> } =====================================================

constructor TJclVectorF<T>.Create(const AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer;
  AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityCompare := AEqualityCompare;
end;

procedure TJclVectorF<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclVectorF<T> then
    TJclVectorF<T>(Dest).FEqualityCompare := FEqualityCompare;
end;

function TJclVectorF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclVectorF<T>.Create(EqualityCompare, FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclVectorF<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if not Assigned(EqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityCompare(A, B);
end;

//=== { TJclVectorI<T> } =====================================================

function TJclVectorI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclVectorI<T>.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclVectorI<T>.ItemsEqual(const A, B: T): Boolean;
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

