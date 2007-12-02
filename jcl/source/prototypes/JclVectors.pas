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
{ Last modified: $Date::                                                                        $ }
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
{$I containers\JclContainerCommon.imp}
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

(*$JPPEXPANDMACRO JCLVECTORINT(TJclSingleVector,TJclSingleAbstractContainer,IJclSingleCollection,IJclSingleList,IJclSingleArray,IJclSingleIterator, IJclSingleContainer\, IJclSingleEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,const AValue: Single,Single,0.0,JclBase.TDynSingleArray,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLVECTORINT(TJclDoubleVector,TJclDoubleAbstractContainer,IJclDoubleCollection,IJclDoubleList,IJclDoubleArray,IJclDoubleIterator, IJclDoubleContainer\, IJclDoubleEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,const AValue: Double,Double,0.0,JclBase.TDynDoubleArray,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLVECTORINT(TJclExtendedVector,TJclExtendedAbstractContainer,IJclExtendedCollection,IJclExtendedList,IJclExtendedArray,IJclExtendedIterator, IJclExtendedContainer\, IJclExtendedEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,const AValue: Extended,Extended,0.0,JclBase.TDynExtendedArray,GetValue,SetValue)*)

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatVector = TJclExtendedVector;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatVector = TJclDoubleVector;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatVector = TJclSingleVector;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLVECTORINT(TJclIntegerVector,TJclIntegerAbstractContainer,IJclIntegerCollection,IJclIntegerList,IJclIntegerArray,IJclIntegerIterator, IJclIntegerEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,AValue: Integer,Integer,0,JclBase.TDynIntegerArray,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLVECTORINT(TJclCardinalVector,TJclCardinalAbstractContainer,IJclCardinalCollection,IJclCardinalList,IJclCardinalArray,IJclCardinalIterator, IJclCardinalEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,AValue: Cardinal,Cardinal,0,JclBase.TDynCardinalArray,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLVECTORINT(TJclInt64Vector,TJclInt64AbstractContainer,IJclInt64Collection,IJclInt64List,IJclInt64Array,IJclInt64Iterator, IJclInt64EqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,const AValue: Int64,Int64,0,JclBase.TDynInt64Array,GetValue,SetValue)*)

  {$IFNDEF CLR}
(*$JPPEXPANDMACRO JCLVECTORINT(TJclPtrVector,TJclPtrAbstractContainer,IJclPtrCollection,IJclPtrList,IJclPtrArray,IJclPtrIterator, IJclPtrEqualityComparer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,APtr: Pointer,Pointer,nil,JclBase.TDynPointerArray,GetPtr,SetPtr)*)
  {$ENDIF ~CLR}

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
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer; AOwnsItems: Boolean);
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

type
  TItrStart = (isFirst, isLast);

(*$JPPEXPANDMACRO JCLVECTORITR(TIntfItr,IJclIntfIterator,IJclIntfList,const AInterface: IInterface,AInterface,IInterface,GetObject,SetObject)*)

(*$JPPEXPANDMACRO JCLVECTORITR(TAnsiStrItr,IJclAnsiStrIterator,IJclAnsiStrList,const AString: AnsiString,AString,AnsiString,GetString,SetString)*)

(*$JPPEXPANDMACRO JCLVECTORITR(TWideStrItr,IJclWideStrIterator,IJclWideStrList,const AString: WideString,AString,WideString,GetString,SetString)*)

(*$JPPEXPANDMACRO JCLVECTORITR(TSingleItr,IJclSingleIterator,IJclSingleList,const AValue: Single,AValue,Single,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLVECTORITR(TDoubleItr,IJclDoubleIterator,IJclDoubleList,const AValue: Double,AValue,Double,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLVECTORITR(TExtendedItr,IJclExtendedIterator,IJclExtendedList,const AValue: Extended,AValue,Extended,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLVECTORITR(TIntegerItr,IJclIntegerIterator,IJclIntegerList,AValue: Integer,AValue,Integer,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLVECTORITR(TCardinalItr,IJclCardinalIterator,IJclCardinalList,AValue: Cardinal,AValue,Cardinal,GetValue,SetValue)*)

(*$JPPEXPANDMACRO JCLVECTORITR(TInt64Itr,IJclInt64Iterator,IJclInt64List,const AValue: Int64,AValue,Int64,GetValue,SetValue)*)

{$IFNDEF CLR}
(*$JPPEXPANDMACRO JCLVECTORITR(TPtrItr,IJclPtrIterator,IJclPtrList,APtr: Pointer,APtr,Pointer,GetPtr,SetPtr)*)
{$ENDIF ~CLR}

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
function TJclSingleVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleVector.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLVECTORIMP(TJclSingleVector,IJclSingleCollection,IJclSingleList,IJclSingleIterator,TSingleItr,,,const AValue: Single,AValue,Single,0.0,GetValue,SetValue,FreeSingle,JclBase.MoveArray)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclDoubleVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleVector.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLVECTORIMP(TJclDoubleVector,IJclDoubleCollection,IJclDoubleList,IJclDoubleIterator,TDoubleItr,,,const AValue: Double,AValue,Double,0.0,GetValue,SetValue,FreeDouble,JclBase.MoveArray)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclExtendedVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedVector.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLVECTORIMP(TJclExtendedVector,IJclExtendedCollection,IJclExtendedList,IJclExtendedIterator,TExtendedItr,,,const AValue: Extended,AValue,Extended,0.0,GetValue,SetValue,FreeExtended,JclBase.MoveArray)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntegerVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerVector.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLVECTORIMP(TJclIntegerVector,IJclIntegerCollection,IJclIntegerList,IJclIntegerIterator,TIntegerItr,,,AValue: Integer,AValue,Integer,0,GetValue,SetValue,FreeInteger,JclBase.MoveArray)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclCardinalVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalVector.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLVECTORIMP(TJclCardinalVector,IJclCardinalCollection,IJclCardinalList,IJclCardinalIterator,TCardinalItr,,,AValue: Cardinal,AValue,Cardinal,0,GetValue,SetValue,FreeCardinal,JclBase.MoveArray)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclInt64Vector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64Vector.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLVECTORIMP(TJclInt64Vector,IJclInt64Collection,IJclInt64List,IJclInt64Iterator,TInt64Itr,,,const AValue: Int64,AValue,Int64,0,GetValue,SetValue,FreeInt64,JclBase.MoveArray)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}


{$IFNDEF CLR}
{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclPtrVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrVector.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLVECTORIMP(TJclPtrVector,IJclPtrCollection,IJclPtrList,IJclPtrIterator,TPtrItr,,,APtr: Pointer,APtr,Pointer,nil,GetPtr,SetPtr,FreePointer,JclBase.MoveArray)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$ENDIF ~CLR}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclVector.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclVector.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
(*$JPPEXPANDMACRO JCLVECTORIMP(TJclVector,IJclCollection,IJclList,IJclIterator,TItr,; AOwnsObjects: Boolean,AOwnsObjects,AObject: TObject,AObject,TObject,nil,GetObject,SetObject,FreeObject,JclBase.MoveArray)*)
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}

{$IFDEF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER}
(*$JPPEXPANDMACRO JCLVECTORIMP(TJclVector<T>,IJclCollection<T>,IJclList<T>,IJclIterator<T>,TItr<T>,; AOwnsItems: Boolean,AOwnsItems,const AItem: T,AItem,T,Default(T),GetItem,SetItem,FreeItem,TJclBase<T>.MoveArray)*)
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
  if EqualityComparer <> nil then
    Result := EqualityComparer.Equals(A, B)
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclVectorF<T> } =====================================================

constructor TJclVectorF<T>.Create(const AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer;
  AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  SetEqualityCompare(AEqualityCompare);
end;

function TJclVectorF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclVectorF<T>.Create(EqualityCompare, FSize, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclVectorI<T> } =====================================================

function TJclVectorI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclVectorI<T>.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclVectorI<T>.ItemsEqual(const A, B: T): Boolean;
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

