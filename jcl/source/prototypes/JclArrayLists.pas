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
  Classes,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF SUPPORTS_GENERICS}
  JclAlgorithms,
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclContainerIntf, JclSynch;
{$I containers\JclContainerCommon.imp}
{$I containers\JclArrayLists.int}
{$I containers\JclArrayLists.imp}
type
  TItrStart = (isFirst, isLast);

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclIntfArrayList,TJclIntfAbstractContainer,IJclIntfCollection,IJclIntfList,IJclIntfArray,IJclIntfIterator,TDynIInterfaceArray, IJclIntfEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AInterface,IInterface,GetObject,SetObject)}

{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TJclIntfArrayIterator,IJclIntfIterator,IJclIntfList,const ,AInterface,IInterface,GetObject,SetObject)}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclAnsiStrArrayList,TJclAnsiStrAbstractCollection,IJclAnsiStrCollection,IJclAnsiStrList,IJclAnsiStrArray,IJclAnsiStrIterator,TDynAnsiStringArray, IJclStrContainer\, IJclAnsiStrContainer\, IJclAnsiStrFlatContainer\, IJclAnsiStrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;, override;,,const ,AString,AnsiString,GetString,SetString)}

{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TJclAnsiStrArrayIterator,IJclAnsiStrIterator,IJclAnsiStrList,const ,AString,AnsiString,GetString,SetString)}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclWideStrArrayList,TJclWideStrAbstractCollection,IJclWideStrCollection,IJclWideStrList,IJclWideStrArray,IJclWideStrIterator,TDynWideStringArray, IJclStrContainer\, IJclWideStrContainer\, IJclWideStrFlatContainer\, IJclWideStrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;, override;,,const ,AString,WideString,GetString,SetString)}

{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TJclWideStrArrayIterator,IJclWideStrIterator,IJclWideStrList,const ,AString,WideString,GetString,SetString)}

{$IFDEF SUPPORTS_UNICODE_STRING}
{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclUnicodeStrArrayList,TJclUnicodeStrAbstractCollection,IJclUnicodeStrCollection,IJclUnicodeStrList,IJclUnicodeStrArray,IJclUnicodeStrIterator,TDynUnicodeStringArray, IJclStrContainer\, IJclUnicodeStrContainer\, IJclUnicodeStrFlatContainer\, IJclUnicodeStrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;, override;,,const ,AString,UnicodeString,GetString,SetString)}

{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TJclUnicodeStrArrayIterator,IJclUnicodeStrIterator,IJclUnicodeStrList,const ,AString,UnicodeString,GetString,SetString)}
{$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrArrayList = TJclAnsiStrArrayList;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrArrayList = TJclWideStrArrayList;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrArrayList = TJclUnicodeStrArrayList;
  {$ENDIF CONTAINER_UNICODESTR}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclSingleArrayList,TJclSingleAbstractContainer,IJclSingleCollection,IJclSingleList,IJclSingleArray,IJclSingleIterator,TDynSingleArray, IJclSingleContainer\, IJclSingleEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AValue,Single,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TJclSingleArrayIterator,IJclSingleIterator,IJclSingleList,const ,AValue,Single,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclDoubleArrayList,TJclDoubleAbstractContainer,IJclDoubleCollection,IJclDoubleList,IJclDoubleArray,IJclDoubleIterator,TDynDoubleArray, IJclDoubleContainer\, IJclDoubleEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AValue,Double,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TJclDoubleArrayIterator,IJclDoubleIterator,IJclDoubleList,const ,AValue,Double,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclExtendedArrayList,TJclExtendedAbstractContainer,IJclExtendedCollection,IJclExtendedList,IJclExtendedArray,IJclExtendedIterator,TDynExtendedArray, IJclExtendedContainer\, IJclExtendedEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AValue,Extended,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TJclExtendedArrayIterator,IJclExtendedIterator,IJclExtendedList,const ,AValue,Extended,GetValue,SetValue)}

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatArrayList = TJclExtendedArrayList;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatArrayList = TJclDoubleArrayList;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatArrayList = TJclSingleArrayList;
  {$ENDIF MATH_SINGLE_PRECISION}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclIntegerArrayList,TJclIntegerAbstractContainer,IJclIntegerCollection,IJclIntegerList,IJclIntegerArray,IJclIntegerIterator,TDynIntegerArray, IJclIntegerEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,AValue,Integer,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TJclIntegerArrayIterator,IJclIntegerIterator,IJclIntegerList,,AValue,Integer,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclCardinalArrayList,TJclCardinalAbstractContainer,IJclCardinalCollection,IJclCardinalList,IJclCardinalArray,IJclCardinalIterator,TDynCardinalArray, IJclCardinalEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,AValue,Cardinal,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TJclCardinalArrayIterator,IJclCardinalIterator,IJclCardinalList,,AValue,Cardinal,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclInt64ArrayList,TJclInt64AbstractContainer,IJclInt64Collection,IJclInt64List,IJclInt64Array,IJclInt64Iterator,TDynInt64Array, IJclInt64EqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,const ,AValue,Int64,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TJclInt64ArrayIterator,IJclInt64Iterator,IJclInt64List,const ,AValue,Int64,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclPtrArrayList,TJclPtrAbstractContainer,IJclPtrCollection,IJclPtrList,IJclPtrArray,IJclPtrIterator,TDynPointerArray, IJclPtrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,,,APtr,Pointer,GetPointer,SetPointer)}

{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TJclPtrArrayIterator,IJclPtrIterator,IJclPtrList,,APtr,Pointer,GetPointer,SetPointer)}

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclArrayList,TJclAbstractContainer,IJclCollection,IJclList,IJclArray,IJclIterator,TDynObjectArray, IJclObjectOwner\, IJclEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;,,; AOwnsObjects: Boolean,,AObject,TObject,GetObject,SetObject)}

{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TJclArrayIterator,IJclIterator,IJclList,,AObject,TObject,GetObject,SetObject)}

  {$IFDEF SUPPORTS_GENERICS}
  TJclArrayIterator<T> = class;

{$JPPEXPANDMACRO JCLARRAYLISTINT(TJclArrayList<T>,TJclAbstractContainer<T>,IJclCollection<T>,IJclList<T>,IJclArray<T>,IJclIterator<T>,TDynArray, IJclItemOwner<T>\, IJclEqualityComparer<T>\,,
  protected
    type
      TDynArray = array of T;
      TArrayIterator = TJclArrayIterator<T>;
    procedure MoveArray(var List: TDynArray; FromIndex, ToIndex, Count: Integer);,,; AOwnsItems: Boolean,const ,AItem,T,GetItem,SetItem)}

{$JPPEXPANDMACRO JCLARRAYLISTITRINT(TJclArrayIterator<T>,IJclIterator<T>,IJclList<T>,const ,AItem,T,GetItem,SetItem)}

  // E = External helper to compare items for equality
  // GetHashCode is not used
  TJclArrayListE<T> = class(TJclArrayList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>)
  private
    FEqualityComparer: IJclEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AEqualityComparer: IJclEqualityComparer<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const AEqualityComparer: IJclEqualityComparer<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;

    property EqualityComparer: IJclEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclArrayListF<T> = class(TJclArrayList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;
  end;

  // I = Items can compare themselves to others
  TJclArrayListI<T: IEquatable<T>> = class(TJclArrayList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclList<T>, IJclArray<T>)
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

{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclIntfArrayList,,,IJclIntfCollection,IJclIntfIterator,TJclIntfArrayIterator,IJclIntfList,const ,AInterface,GetObject,SetObject,FreeObject,IInterface,nil)}

function TJclIntfArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TJclIntfArrayIterator,IJclIntfIterator,IJclIntfList,const ,AInterface,IInterface,GetObject,SetObject)}

{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclAnsiStrArrayList,,,IJclAnsiStrCollection,IJclAnsiStrIterator,TJclAnsiStrArrayIterator,IJclAnsiStrList,const ,AString,GetString,SetString,FreeString,AnsiString,'')}

function TJclAnsiStrArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TJclAnsiStrArrayIterator,IJclAnsiStrIterator,IJclAnsiStrList,const ,AString,AnsiString,GetString,SetString)}

{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclWideStrArrayList,,,IJclWideStrCollection,IJclWideStrIterator,TJclWideStrArrayIterator,IJclWideStrList,const ,AString,GetString,SetString,FreeString,WideString,'')}

function TJclWideStrArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TJclWideStrArrayIterator,IJclWideStrIterator,IJclWideStrList,const ,AString,WideString,GetString,SetString)}

{$IFDEF SUPPORTS_UNICODE_STRING}
{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclUnicodeStrArrayList,,,IJclUnicodeStrCollection,IJclUnicodeStrIterator,TJclUnicodeStrArrayIterator,IJclUnicodeStrList,const ,AString,GetString,SetString,FreeString,UnicodeString,'')}

function TJclUnicodeStrArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TJclUnicodeStrArrayIterator,IJclUnicodeStrIterator,IJclUnicodeStrList,const ,AString,UnicodeString,GetString,SetString)}
{$ENDIF SUPPORTS_UNICODE_STRING}


{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclSingleArrayList,,,IJclSingleCollection,IJclSingleIterator,TJclSingleArrayIterator,IJclSingleList,const ,AValue,GetValue,SetValue,FreeSingle,Single,0.0)}

function TJclSingleArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TJclSingleArrayIterator,IJclSingleIterator,IJclSingleList,const ,AValue,Single,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclDoubleArrayList,,,IJclDoubleCollection,IJclDoubleIterator,TJclDoubleArrayIterator,IJclDoubleList,const ,AValue,GetValue,SetValue,FreeDouble,Double,0.0)}

function TJclDoubleArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TJclDoubleArrayIterator,IJclDoubleIterator,IJclDoubleList,const ,AValue,Double,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclExtendedArrayList,,,IJclExtendedCollection,IJclExtendedIterator,TJclExtendedArrayIterator,IJclExtendedList,const ,AValue,GetValue,SetValue,FreeExtended,Extended,0.0)}

function TJclExtendedArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TJclExtendedArrayIterator,IJclExtendedIterator,IJclExtendedList,const ,AValue,Extended,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclIntegerArrayList,,,IJclIntegerCollection,IJclIntegerIterator,TJclIntegerArrayIterator,IJclIntegerList,,AValue,GetValue,SetValue,FreeInteger,Integer,0)}

function TJclIntegerArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TJclIntegerArrayIterator,IJclIntegerIterator,IJclIntegerList,,AValue,Integer,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclCardinalArrayList,,,IJclCardinalCollection,IJclCardinalIterator,TJclCardinalArrayIterator,IJclCardinalList,,AValue,GetValue,SetValue,FreeCardinal,Cardinal,0)}

function TJclCardinalArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TJclCardinalArrayIterator,IJclCardinalIterator,IJclCardinalList,,AValue,Cardinal,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclInt64ArrayList,,,IJclInt64Collection,IJclInt64Iterator,TJclInt64ArrayIterator,IJclInt64List,const ,AValue,GetValue,SetValue,FreeInt64,Int64,0)}

function TJclInt64ArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64ArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TJclInt64ArrayIterator,IJclInt64Iterator,IJclInt64List,const ,AValue,Int64,GetValue,SetValue)}

{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclPtrArrayList,,,IJclPtrCollection,IJclPtrIterator,TJclPtrArrayIterator,IJclPtrList,,APtr,GetPointer,SetPointer,FreePointer,Pointer,nil)}

function TJclPtrArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrArrayList.Create(FSize);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TJclPtrArrayIterator,IJclPtrIterator,IJclPtrList,,APtr,Pointer,GetPointer,SetPointer)}

{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclArrayList,; AOwnsObjects: Boolean,AOwnsObjects,IJclCollection,IJclIterator,TJclArrayIterator,IJclList,,AObject,GetObject,SetObject,FreeObject,TObject,nil)}

function TJclArrayList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArrayList.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TJclArrayIterator,IJclIterator,IJclList,,AObject,TObject,GetObject,SetObject)}

{$IFDEF SUPPORTS_GENERICS}

{$JPPEXPANDMACRO JCLARRAYLISTIMP(TJclArrayList<T>,; AOwnsItems: Boolean,AOwnsItems,IJclCollection<T>,IJclIterator<T>,TArrayIterator,IJclList<T>,const ,AItem,GetItem,SetItem,FreeItem,T,Default(T))}

{$JPPEXPANDMACRO JCLARRAYLISTITRIMP(TJclArrayIterator<T>,IJclIterator<T>,IJclList<T>,const ,AItem,T,GetItem,SetItem)}

procedure TJclArrayList<T>.MoveArray(var List: TDynArray; FromIndex, ToIndex, Count: Integer);
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      List[ToIndex + I] := List[FromIndex + I];

    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
        List[FromIndex + I] := Default(T)
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := Default(T);
  end
  else
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];

    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
        List[FromIndex + I] := Default(T)
    else
      // independant
      for I := 0 to Count - 1 do
        List[FromIndex + I] := Default(T);
  end; 
end;

//=== { TJclArrayListE<T> } ==================================================

constructor TJclArrayListE<T>.Create(const AEqualityComparer: IJclEqualityComparer<T>; ACapacity: Integer;
  AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

constructor TJclArrayListE<T>.Create(const AEqualityComparer: IJclEqualityComparer<T>;
  const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

procedure TJclArrayListE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclArrayListE<T> then
    TJclArrayListE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclArrayListE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArrayListE<T>.Create(EqualityComparer, FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclArrayListE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer <> nil then
    Result := EqualityComparer.ItemsEqual(A, B)
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclArrayListF<T> } ==================================================

constructor TJclArrayListF<T>.Create(const AEqualityCompare: TEqualityCompare<T>;
  ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  SetEqualityCompare(AEqualityCompare);
end;

constructor TJclArrayListF<T>.Create(const AEqualityCompare: TEqualityCompare<T>; const ACollection: IJclCollection<T>;
  AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  SetEqualityCompare(AEqualityCompare);
end;

function TJclArrayListF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArrayListF<T>.Create(EqualityCompare, FSize, False);
  AssignPropertiesTo(Result);
end;

//=== { TJclArrayListI<T> } ==================================================

function TJclArrayListI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclArrayListI<T>.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclArrayListI<T>.ItemsEqual(const A, B: T): Boolean;
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

