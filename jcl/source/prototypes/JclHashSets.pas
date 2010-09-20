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
  SysUtils,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Classes,
  {$IFDEF SUPPORTS_GENERICS}
  JclAlgorithms,
  {$ENDIF SUPPORTS_GENERICS}
  JclBase, JclAbstractContainers, JclContainerIntf, JclHashMaps, JclSynch;
{$I containers\JclContainerCommon.imp}
{$I containers\JclHashSets.imp}
{$I containers\JclHashSets.int}
type
  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN
  TRefUnique = class;
  TRefUnique = class(TInterfacedObject, IEquatable<TRefUnique>, IJclEqualityComparer<TRefUnique>)
  public
    { IEquatable<TRefUnique> }
    function Equals(Other: TRefUnique): Boolean; reintroduce;
    { IJclEqualityComparer<TRefUnique> }
    function GetEqualityCompare: TEqualityCompare<TRefUnique>;
    procedure SetEqualityCompare(Value: TEqualityCompare<TRefUnique>);
    function ItemsEqual(const A, B: TRefUnique): Boolean;
    property EqualityCompare: TEqualityCompare<TRefUnique> read GetEqualityCompare write SetEqualityCompare;
  end;
  //DOM-IGNORE-END
  {$ELSE ~SUPPORTS_GENERICS}
  TRefUnique = TInterfacedObject;
  {$ENDIF ~SUPPORTS_GENERICS}

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclIntfHashSet,TJclIntfAbstractContainer,IJclIntfCollection,IJclIntfSet,IJclIntfMap,IJclIntfIterator, IJclIntfEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;,,const ,AInterface,IInterface)*)

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclAnsiStrHashSet,TJclAnsiStrAbstractCollection,IJclAnsiStrCollection,IJclAnsiStrSet,IJclAnsiStrMap,IJclAnsiStrIterator, IJclStrContainer\, IJclAnsiStrContainer\, IJclAnsiStrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;
    { IJclStrContainer }
    function GetCaseSensitive: Boolean; override;
    procedure SetCaseSensitive(Value: Boolean); override;
    { IJclAnsiStrContainer }
    function GetEncoding: TJclAnsiStrEncoding; override;
    procedure SetEncoding(Value: TJclAnsiStrEncoding); override;, override;,const ,AString,AnsiString)*)

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclWideStrHashSet,TJclWideStrAbstractCollection,IJclWideStrCollection,IJclWideStrSet,IJclWideStrMap,IJclWideStrIterator, IJclStrContainer\, IJclWideStrContainer\, IJclWideStrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;
    { IJclStrContainer }
    function GetCaseSensitive: Boolean; override;
    procedure SetCaseSensitive(Value: Boolean); override;
    { IJclWideStrContainer }
    function GetEncoding: TJclWideStrEncoding; override;
    procedure SetEncoding(Value: TJclWideStrEncoding); override;, override;,const ,AString,WideString)*)

{$IFDEF SUPPORTS_UNICODE_STRING}
(*$JPPEXPANDMACRO JCLHASHSETINT(TJclUnicodeStrHashSet,TJclUnicodeStrAbstractCollection,IJclUnicodeStrCollection,IJclUnicodeStrSet,IJclUnicodeStrMap,IJclUnicodeStrIterator, IJclStrContainer\, IJclUnicodeStrContainer\, IJclUnicodeStrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;
    { IJclStrContainer }
    function GetCaseSensitive: Boolean; override;
    procedure SetCaseSensitive(Value: Boolean); override;, override;,const ,AString,UnicodeString)*)
{$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrHashSet = TJclAnsiStrHashSet;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrHashSet = TJclWideStrHashSet;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrHashSet = TJclUnicodeStrHashSet;
  {$ENDIF CONTAINER_UNICODESTR}

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclSingleHashSet,TJclSingleAbstractContainer,IJclSingleCollection,IJclSingleSet,IJclSingleMap,IJclSingleIterator, IJclSingleContainer\, IJclSingleEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;
    { IJclSingleContainer }
    function GetPrecision: Single; override;
    procedure SetPrecision(const Value: Single); override;,,const ,AValue,Single)*)

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclDoubleHashSet,TJclDoubleAbstractContainer,IJclDoubleCollection,IJclDoubleSet,IJclDoubleMap,IJclDoubleIterator, IJclDoubleContainer\, IJclDoubleEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;
    { IJclDoubleContainer }
    function GetPrecision: Double; override;
    procedure SetPrecision(const Value: Double); override;,,const ,AValue,Double)*)

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclExtendedHashSet,TJclExtendedAbstractContainer,IJclExtendedCollection,IJclExtendedSet,IJclExtendedMap,IJclExtendedIterator, IJclExtendedContainer\, IJclExtendedEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;
    { IJclExtendedContainer }
    function GetPrecision: Extended; override;
    procedure SetPrecision(const Value: Extended); override;,,const ,AValue,Extended)*)

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatHashSet = TJclExtendedHashSet;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatHashSet = TJclDoubleHashSet;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatHashSet = TJclSingleHashSet;
  {$ENDIF MATH_SINGLE_PRECISION}

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclIntegerHashSet,TJclIntegerAbstractContainer,IJclIntegerCollection,IJclIntegerSet,IJclIntegerMap,IJclIntegerIterator, IJclIntegerEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;,,,AValue,Integer)*)

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclCardinalHashSet,TJclCardinalAbstractContainer,IJclCardinalCollection,IJclCardinalSet,IJclCardinalMap,IJclCardinalIterator, IJclCardinalEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;,,,AValue,Cardinal)*)

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclInt64HashSet,TJclInt64AbstractContainer,IJclInt64Collection,IJclInt64Set,IJclInt64Map,IJclInt64Iterator, IJclInt64EqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;,,const ,AValue,Int64)*)

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclPtrHashSet,TJclPtrAbstractContainer,IJclPtrCollection,IJclPtrSet,IJclPtrMap,IJclPtrIterator, IJclPtrEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer); overload;,,,AValue,Pointer)*)

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclHashSet,TJclAbstractContainer,IJclCollection,IJclSet,IJclMap,IJclIterator, IJclObjectOwner\, IJclEqualityComparer\,,
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(ACapacity: Integer; AOwnsObjects: Boolean); overload;
    { IJclObjectOwner }
    function FreeObject(var AObject: TObject): TObject; override;
    function GetOwnsObjects: Boolean; override;,,,AObject,TObject)*)

  {$IFDEF SUPPORTS_GENERICS}
  //DOM-IGNORE-BEGIN

(*$JPPEXPANDMACRO JCLHASHSETINT(TJclHashSet<T>,TJclAbstractContainer<T>,IJclCollection<T>,IJclSet<T>,IJclMap<T\, TRefUnique>,IJclIterator<T>, IJclItemOwner<T>\, IJclEqualityComparer<T>\,,
  public
    { IJclItemOwner<T> }
    function FreeItem(var AItem: T): T; override;
    function GetOwnsItems: Boolean; override;,,const ,AItem,T)*)

  // E = External helper to compare items for equality
  TJclHashSetE<T> = class(TJclHashSet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclCollection<T>, IJclSet<T>,
    IJclItemOwner<T>, IJclEqualityComparer<T>)
  private
    FEqualityComparer: IJclEqualityComparer<T>;
    FHashConverter: IJclHashconverter<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AEqualityComparer: IJclEqualityComparer<T>; const AHashConverter: IJclHashConverter<T>;
      const AMap: IJclMap<T, TRefUnique>); overload;
    constructor Create(const AEqualityComparer: IJclEqualityComparer<T>; const AHashConverter: IJclHashConverter<T>;
      const AComparer: IJclComparer<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
    property EqualityComparer: IJclEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
    property HashConverter: IJclHashConverter<T> read FHashConverter write FHashConverter;
  end;

  // F = Function to compare items for equality
  TJclHashSetF<T> = class(TJclHashSet<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclCollection<T>, IJclSet<T>,
    IJclItemOwner<T>, IJclEqualityComparer<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const AMap: IJclMap<T, TRefUnique>); overload;
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const AHash: THashConvert<T>; const ACompare: TCompare<T>;
      ACapacity: Integer; AOwnsItems: Boolean); overload;
  end;

  // I = Items can compare themselves to an other
  TJclHashSetI<T: IEquatable<T>, IComparable<T>, IHashable> = class(TJclHashSet<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable,
    IJclContainer, IJclCollection<T>, IJclSet<T>, IJclItemOwner<T>, IJclEqualityComparer<T>)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const AMap: IJclMap<T, TRefUnique>); overload;
    constructor Create(ACapacity: Integer; AOwnsItems: Boolean); overload;
    { IJclEqualityComparer<T> }
    function ItemsEqual(const A, B: T): Boolean; override;
  end;

  //DOM-IGNORE-END
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

function RefUnique: TRefUnique;
function EqualityCompareEqObjects(const Obj1, Obj2: TRefUnique): Boolean;

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

function EqualityCompareEqObjects(const Obj1, Obj2: TRefUnique): Boolean;
begin
  Result := Obj1 = Obj2;
end;

{$IFDEF SUPPORTS_GENERICS}
//DOM-IGNORE-BEGIN

//=== { TRefUnique } ==========================================================

function TRefUnique.GetEqualityCompare: TEqualityCompare<TRefUnique>;
begin
  raise EJclOperationNotSupportedError.Create;
end;

procedure TRefUnique.SetEqualityCompare(Value: TEqualityCompare<TRefUnique>);
begin
  raise EJclOperationNotSupportedError.Create;
end;

function TRefUnique.ItemsEqual(const A, B: TRefUnique): Boolean;
begin
  Result := A = B;
end;

function TRefUnique.Equals(Other: TRefUnique): Boolean;
begin
  Result := Self = Other;
end;

//DOM-IGNORE-END
{$ENDIF SUPPORTS_GENERICS}

(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclIntfHashSet,IJclIntfMap,IJclIntfCollection,IJclIntfIterator,,const ,AInterface,IInterface)*)
{$JPPUNDEFMACRO CONSTRUCTORADDITIONAL}

constructor TJclIntfHashSet.Create(ACapacity: Integer);
begin
  Create(TJclIntfHashMap.Create(ACapacity, False));
end;

function TJclIntfHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclAnsiStrHashSet,IJclAnsiStrMap,IJclAnsiStrCollection,IJclAnsiStrIterator,,const ,AString,AnsiString)*)

constructor TJclAnsiStrHashSet.Create(ACapacity: Integer);
begin
  Create(TJclAnsiStrHashMap.Create(ACapacity, False));
end;

function TJclAnsiStrHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrHashSet.GetCaseSensitive: Boolean;
begin
  Result := FMap.GetCaseSensitive;
end;

function TJclAnsiStrHashSet.GetEncoding: TJclAnsiStrEncoding;
begin
  Result := FMap.GetEncoding;
end;

procedure TJclAnsiStrHashSet.SetCaseSensitive(Value: Boolean);
begin
  FMap.SetCaseSensitive(Value);
end;

procedure TJclAnsiStrHashSet.SetEncoding(Value: TJclAnsiStrEncoding);
begin
  FMap.SetEncoding(Value);
end;

(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclWideStrHashSet,IJclWideStrMap,IJclWideStrCollection,IJclWideStrIterator,,const ,AString,WideString)*)

constructor TJclWideStrHashSet.Create(ACapacity: Integer);
begin
  Create(TJclWideStrHashMap.Create(ACapacity, False));
end;

function TJclWideStrHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

function TJclWideStrHashSet.GetCaseSensitive: Boolean;
begin
  Result := FMap.GetCaseSensitive;
end;

function TJclWideStrHashSet.GetEncoding: TJclWideStrEncoding;
begin
  Result := FMap.GetEncoding;
end;

procedure TJclWideStrHashSet.SetCaseSensitive(Value: Boolean);
begin
  FMap.SetCaseSensitive(Value);
end;

procedure TJclWideStrHashSet.SetEncoding(Value: TJclWideStrEncoding);
begin
  FMap.SetEncoding(Value);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclUnicodeStrHashSet,IJclUnicodeStrMap,IJclUnicodeStrCollection,IJclUnicodeStrIterator,,const ,AString,UnicodeString)*)

constructor TJclUnicodeStrHashSet.Create(ACapacity: Integer);
begin
  Create(TJclUnicodeStrHashMap.Create(ACapacity, False));
end;

function TJclUnicodeStrHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

function TJclUnicodeStrHashSet.GetCaseSensitive: Boolean;
begin
  Result := FMap.GetCaseSensitive;
end;

procedure TJclUnicodeStrHashSet.SetCaseSensitive(Value: Boolean);
begin
  FMap.SetCaseSensitive(Value);
end;

{$ENDIF SUPPORTS_UNICODE_STRING}

(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclSingleHashSet,IJclSingleMap,IJclSingleCollection,IJclSingleIterator,,const ,AValue,Single)*)

constructor TJclSingleHashSet.Create(ACapacity: Integer);
begin
  Create(TJclSingleHashMap.Create(ACapacity, False));
end;

function TJclSingleHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

function TJclSingleHashSet.GetPrecision: Single;
begin
  Result := FMap.GetPrecision;
end;

procedure TJclSingleHashSet.SetPrecision(const Value: Single);
begin
  FMap.SetPrecision(Value);
end;

(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclDoubleHashSet,IJclDoubleMap,IJclDoubleCollection,IJclDoubleIterator,,const ,AValue,Double)*)

constructor TJclDoubleHashSet.Create(ACapacity: Integer);
begin
  Create(TJclDoubleHashMap.Create(ACapacity, False));
end;

function TJclDoubleHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

function TJclDoubleHashSet.GetPrecision: Double;
begin
  Result := FMap.GetPrecision;
end;

procedure TJclDoubleHashSet.SetPrecision(const Value: Double);
begin
  FMap.SetPrecision(Value);
end;

(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclExtendedHashSet,IJclExtendedMap,IJclExtendedCollection,IJclExtendedIterator,,const ,AValue,Extended)*)

constructor TJclExtendedHashSet.Create(ACapacity: Integer);
begin
  Create(TJclExtendedHashMap.Create(ACapacity, False));
end;

function TJclExtendedHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

function TJclExtendedHashSet.GetPrecision: Extended;
begin
  Result := FMap.GetPrecision;
end;

procedure TJclExtendedHashSet.SetPrecision(const Value: Extended);
begin
  FMap.SetPrecision(Value);
end;

(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclIntegerHashSet,IJclIntegerMap,IJclIntegerCollection,IJclIntegerIterator,,,AValue,Integer)*)

constructor TJclIntegerHashSet.Create(ACapacity: Integer);
begin
  Create(TJclIntegerHashMap.Create(ACapacity, False));
end;

function TJclIntegerHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclCardinalHashSet,IJclCardinalMap,IJclCardinalCollection,IJclCardinalIterator,,,AValue,Cardinal)*)

constructor TJclCardinalHashSet.Create(ACapacity: Integer);
begin
  Create(TJclCardinalHashMap.Create(ACapacity, False));
end;

function TJclCardinalHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclInt64HashSet,IJclInt64Map,IJclInt64Collection,IJclInt64Iterator,,const ,AValue,Int64)*)

constructor TJclInt64HashSet.Create(ACapacity: Integer);
begin
  Create(TJclInt64HashMap.Create(ACapacity, False));
end;

function TJclInt64HashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64HashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclPtrHashSet,IJclPtrMap,IJclPtrCollection,IJclPtrIterator,,,AValue,Pointer)*)

constructor TJclPtrHashSet.Create(ACapacity: Integer);
begin
  Create(TJclPtrHashMap.Create(ACapacity, False));
end;

function TJclPtrHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrHashSet.Create(GetCapacity);
  AssignPropertiesTo(Result);
end;

(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclHashSet,IJclMap,IJclCollection,IJclIterator,False,,AObject,TObject)*)

constructor TJclHashSet.Create(ACapacity: Integer; AOwnsObjects: Boolean);
begin
  Create(TJclHashMap.Create(ACapacity, AOwnsObjects, False));
end;

function TJclHashSet.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashSet.Create(GetCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclHashSet.FreeObject(var AObject: TObject): TObject;
begin
  Result := (FMap as IJclKeyOwner).FreeKey(AObject);
end;

function TJclHashSet.GetOwnsObjects: Boolean;
begin
  Result := (FMap as IJclKeyOwner).GetOwnsKeys;
end;

{$IFDEF SUPPORTS_GENERICS}
//DOM-IGNORE-BEGIN

(*$JPPEXPANDMACRO JCLHASHSETIMP(TJclHashSet<T>,IJclMap<T\, TRefUnique>,IJclCollection<T>,IJclIterator<T>,False,const ,AItem,T)*)

function TJclHashSet<T>.FreeItem(var AItem: T): T;
begin
  Result := (FMap as IJclPairOwner<T, TRefUnique>).FreeKey(AItem);
end;

function TJclHashSet<T>.GetOwnsItems: Boolean;
begin
  Result := (FMap as IJclPairOwner<T, TRefUnique>).GetOwnsKeys;
end;

//=== { TJclHashSetE<T> } ====================================================

constructor TJclHashSetE<T>.Create(const AEqualityComparer: IJclEqualityComparer<T>; const AHashConverter: IJclHashConverter<T>;
  const AMap: IJclMap<T, TRefUnique>);
begin
  inherited Create(AMap);
  FEqualityComparer := AEqualityComparer;
  FHashConverter := AHashConverter;
end;

constructor TJclHashSetE<T>.Create(const AEqualityComparer: IJclEqualityComparer<T>; const AHashConverter: IJclHashConverter<T>;
  const AComparer: IJclComparer<T>; ACapacity: Integer; AOwnsItems: Boolean);
begin
  Create(AEqualityComparer, AHashConverter, TJclHashMapE<T, TRefUnique>.Create(AEqualityComparer, AHashConverter, RefUnique, AComparer, ACapacity, False, AOwnsItems));
end;

procedure TJclHashSetE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclHashSetE<T> then
    TJclHashSetE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclHashSetE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
var
  AMap: IJclMap<T, TRefUnique>;
begin
  AMap := (FMap as IJclIntfCloneable).IntfClone as IJclMap<T, TRefUnique>;
  AMap.Clear;
  Result := TJclHashSetE<T>.Create(FEqualityComparer, FHashConverter, AMap);
  AssignPropertiesTo(Result);
end;

function TJclHashSetE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer <> nil then
    Result := EqualityComparer.ItemsEqual(A, B)
  else
    Result := inherited ItemsEqual(A, B);
end;

//=== { TJclHashSetF<T> } ====================================================

constructor TJclHashSetF<T>.Create(const AEqualityCompare: TEqualityCompare<T>; const AMap: IJclMap<T, TRefUnique>);
begin
  inherited Create(AMap);
  SetEqualityCompare(AEqualityCompare);
end;

constructor TJclHashSetF<T>.Create(const AEqualityCompare: TEqualityCompare<T>; const AHash: THashConvert<T>; const ACompare: TCompare<T>;
  ACapacity: Integer; AOwnsItems: Boolean);
begin
  Create(AEqualityCompare, TJclHashMapF<T, TRefUnique>.Create(AEqualityCompare, AHash, EqualityCompareEqObjects, ACompare, ACapacity, AOwnsItems, False));
end;

function TJclHashSetF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
var
  AMap: IJclMap<T, TRefUnique>;
begin
  AMap := (FMap as IJclIntfCloneable).IntfClone as IJclMap<T, TRefUnique>;
  AMap.Clear;
  Result := TJclHashSetF<T>.Create(FEqualityCompare, AMap);
  AssignPropertiesTo(Result);
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

function TJclHashSetI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
var
  AMap: IJclMap<T, TRefUnique>;
begin
  AMap := (FMap as IJclIntfCloneable).IntfClone as IJclMap<T, TRefUnique>;
  AMap.Clear;
  Result := TJclHashSetI<T>.Create(AMap);
  AssignPropertiesTo(Result);
end;

function TJclHashSetI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if Assigned(FEqualityCompare) then
    Result := FEqualityCompare(A, B)
  else
  if Assigned(FCompare) then
    Result := FCompare(A, B) = 0
  else
    Result := A.Equals(B);
end;

//DOM-IGNORE-END
{$ENDIF SUPPORTS_GENERICS}

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}
  FreeAndNil(GlobalRefUnique);

end.

