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
  JclAlgorithms,
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclContainerIntf, JclArrayLists, JclSynch;
{$I containers\JclContainerCommon.imp}
{$I containers\JclArraySets.int}
{$I containers\JclArraySets.imp}
type
(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclIntfArraySet,TJclIntfArrayList,IJclIntfCollection,IJclIntfList,IJclIntfArray,IJclIntfSet, IJclIntfEqualityComparer\, IJclIntfComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AInterface,IInterface)*)

(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclAnsiStrArraySet,TJclAnsiStrArrayList,IJclAnsiStrCollection,IJclAnsiStrList,IJclAnsiStrArray,IJclAnsiStrSet, IJclStrContainer\, IJclAnsiStrContainer\, IJclAnsiStrFlatContainer\, IJclAnsiStrEqualityComparer\, IJclAnsiStrComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;, override;,const ,AString,AnsiString)*)

(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclWideStrArraySet,TJclWideStrArrayList,IJclWideStrCollection,IJclWideStrList,IJclWideStrArray,IJclWideStrSet, IJclStrContainer\, IJclWideStrContainer\, IJclWideStrFlatContainer\, IJclWideStrEqualityComparer\, IJclWideStrComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;, override;,const ,AString,WideString)*)

{$IFDEF SUPPORTS_UNICODE_STRING}
(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclUnicodeStrArraySet,TJclUnicodeStrArrayList,IJclUnicodeStrCollection,IJclUnicodeStrList,IJclUnicodeStrArray,IJclUnicodeStrSet, IJclStrContainer\, IJclUnicodeStrContainer\, IJclUnicodeStrFlatContainer\, IJclUnicodeStrEqualityComparer\, IJclUnicodeStrComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;, override;,const ,AString,UnicodeString)*)
{$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrArraySet = TJclAnsiStrArraySet;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrArraySet = TJclWideStrArraySet;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrArraySet = TJclUnicodeStrArraySet;
  {$ENDIF CONTAINER_UNICODESTR}

(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclSingleArraySet,TJclSingleArrayList,IJclSingleCollection,IJclSingleList,IJclSingleArray,IJclSingleSet, IJclSingleContainer\, IJclSingleEqualityComparer\, IJclSingleComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AValue,Single)*)

(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclDoubleArraySet,TJclDoubleArrayList,IJclDoubleCollection,IJclDoubleList,IJclDoubleArray,IJclDoubleSet, IJclDoubleContainer\, IJclDoubleEqualityComparer\, IJclDoubleComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AValue,Double)*)

(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclExtendedArraySet,TJclExtendedArrayList,IJclExtendedCollection,IJclExtendedList,IJclExtendedArray,IJclExtendedSet, IJclExtendedContainer\, IJclExtendedEqualityComparer\, IJclExtendedComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AValue,Extended)*)

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatArraySet = TJclExtendedArraySet;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatArraySet = TJclDoubleArraySet;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatArraySet = TJclSingleArraySet;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclIntegerArraySet,TJclIntegerArrayList,IJclIntegerCollection,IJclIntegerList,IJclIntegerArray,IJclIntegerSet, IJclIntegerEqualityComparer\, IJclIntegerComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,AValue,Integer)*)

(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclCardinalArraySet,TJclCardinalArrayList,IJclCardinalCollection,IJclCardinalList,IJclCardinalArray,IJclCardinalSet, IJclCardinalEqualityComparer\, IJclCardinalComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,AValue,Cardinal)*)

(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclInt64ArraySet,TJclInt64ArrayList,IJclInt64Collection,IJclInt64List,IJclInt64Array,IJclInt64Set, IJclInt64EqualityComparer\, IJclInt64Comparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,const ,AValue,Int64)*)

(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclPtrArraySet,TJclPtrArrayList,IJclPtrCollection,IJclPtrList,IJclPtrArray,IJclPtrSet, IJclPtrEqualityComparer\, IJclPtrComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,APtr,Pointer)*)

(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclArraySet,TJclArrayList,IJclCollection,IJclList,IJclArray,IJclSet, IJclObjectOwner\, IJclEqualityComparer\, IJclComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,AObject,TObject)*)

  {$IFDEF SUPPORTS_GENERICS}
(*$JPPEXPANDMACRO JCLARRAYSETINT(TJclArraySet<T>,TJclArrayList<T>,IJclCollection<T>,IJclList<T>,IJclArray<T>,IJclSet<T>, IJclItemOwner<T>\, IJclEqualityComparer<T>\, IJclComparer<T>\,,,,const ,AItem,T)*)

  // E = External helper to compare items
  TJclArraySetE<T> = class(TJclArraySet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>)
  private
    FComparer: IJclComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function ItemsCompare(const A, B: T): Integer; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AComparer: IJclComparer<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const AComparer: IJclComparer<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;

    property Comparer: IJclComparer<T> read FComparer write FComparer;
  end;

  // F = Function to compare items
  TJclArraySetF<T> = class(TJclArraySet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const ACompare: TCompare<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const ACompare: TCompare<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;
  end;

  // I = Items can compare themselves to others
  TJclArraySetI<T: IComparable<T>> = class(TJclArraySet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>, IJclComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclSet<T>)
  protected
    function ItemsCompare(const A, B: T): Integer; override;
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
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils;

(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclIntfArraySet,IJclIntfCollection,IJclIntfIterator,const ,AInterface,IInterface,nil,GetObject)*)

function TJclIntfArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclAnsiStrArraySet,IJclAnsiStrCollection,IJclAnsiStrIterator,const ,AString,AnsiString,'',GetString)*)

function TJclAnsiStrArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclWideStrArraySet,IJclWideStrCollection,IJclWideStrIterator,const ,AString,WideString,'',GetString)*)

function TJclWideStrArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclUnicodeStrArraySet,IJclUnicodeStrCollection,IJclUnicodeStrIterator,const ,AString,UnicodeString,'',GetString)*)

function TJclUnicodeStrArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclSingleArraySet,IJclSingleCollection,IJclSingleIterator,const ,AValue,Single,0.0,GetValue)*)

function TJclSingleArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclDoubleArraySet,IJclDoubleCollection,IJclDoubleIterator,const ,AValue,Double,0.0,GetValue)*)

function TJclDoubleArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclExtendedArraySet,IJclExtendedCollection,IJclExtendedIterator,const ,AValue,Extended,0.0,GetValue)*)

function TJclExtendedArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclIntegerArraySet,IJclIntegerCollection,IJclIntegerIterator,,AValue,Integer,0,GetValue)*)

function TJclIntegerArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclCardinalArraySet,IJclCardinalCollection,IJclCardinalIterator,,AValue,Cardinal,0,GetValue)*)

function TJclCardinalArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclInt64ArraySet,IJclInt64Collection,IJclInt64Iterator,const ,AValue,Int64,0,GetValue)*)

function TJclInt64ArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64ArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclPtrArraySet,IJclPtrCollection,IJclPtrIterator,,APtr,Pointer,nil,GetPointer)*)

function TJclPtrArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrArraySet.Create(Size);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLARRAYSETIMP(TJclArraySet,IJclCollection,IJclIterator,,AObject,TObject,nil,GetObject)*)

function TJclArraySet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArraySet.Create(Size, False);
  AssignPropertiesTo(Result);
end;

{$IFDEF SUPPORTS_GENERICS}
{$JPPEXPANDMACRO JCLARRAYSETIMP(TJclArraySet<T>,IJclCollection<T>,IJclIterator<T>,const ,AItem,T,Default(T),GetItem)}

//=== { TJclArraySetE<T> } ===================================================

constructor TJclArraySetE<T>.Create(const AComparer: IJclComparer<T>; ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FComparer := AComparer;
end;

constructor TJclArraySetE<T>.Create(const AComparer: IJclComparer<T>; const ACollection: IJclCollection<T>;
  AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FComparer := AComparer;
end;

procedure TJclArraySetE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclArraySetE<T> then
    TJclArraySetE<T>(Dest).FComparer := Comparer;
end;

function TJclArraySetE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArraySetE<T>.Create(Comparer, Size, False);
  AssignPropertiesTo(Result);
end;

function TJclArraySetE<T>.ItemsCompare(const A, B: T): Integer;
begin
  if Comparer <> nil then
    Result := Comparer.Compare(A, B)
  else
    Result := inherited ItemsCompare(A, B);
end;

function TJclArraySetE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Comparer <> nil then
    Result := Comparer.Compare(A, B) = 0
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclArraySetF<T> } ===================================================

constructor TJclArraySetF<T>.Create(const ACompare: TCompare<T>; ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  SetCompare(ACompare);
end;

constructor TJclArraySetF<T>.Create(const ACompare: TCompare<T>; const ACollection: IJclCollection<T>;
  AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  SetCompare(ACompare);
end;

function TJclArraySetF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArraySetF<T>.Create(Compare, Size, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclArraySetI<T> } ===================================================

function TJclArraySetI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArraySetI<T>.Create(Size, False);
  AssignPropertiesTo(Result);
end;

function TJclArraySetI<T>.ItemsCompare(const A, B: T): Integer;
begin
  if Assigned(FCompare) then
    Result := FCompare(A, B)
  else
    Result := A.CompareTo(B);
end;

function TJclArraySetI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
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

