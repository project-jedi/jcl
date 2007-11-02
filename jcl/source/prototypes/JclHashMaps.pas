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
{ The Original Code is HashMap.pas.                                                                }
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

unit JclHashMaps;

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
  JclBase, JclAbstractContainers, JclContainerIntf;

{$I containers\JclHashMaps.imp}
type
  // Hash Function
  // Result must be in 0..Range-1
  TJclHashFunction = function(Key, Range: Integer): Integer;

(*$JPPEXPANDMACRO JCLHASHMAPINT(IInterface,IInterface,TJclIntfIntfEntry,TJclIntfIntfEntryArray,TJclIntfIntfBucket,TJclIntfIntfBucketArray,TJclIntfIntfHashMap,TJclAbstractContainerBase,IJclIntfIntfMap,IJclIntfSet,IJclIntfCollection,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function Hash(const AInterface: IInterface): Integer; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A\, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A\, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS},,,,const Key: IInterface,const Value: IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(AnsiString,IInterface,TJclAnsiStrIntfEntry,TJclAnsiStrIntfEntryArray,TJclAnsiStrIntfBucket,TJclAnsiStrIntfBucketArray,TJclAnsiStrIntfHashMap,TJclAnsiStrAbstractContainer,IJclAnsiStrIntfMap,IJclAnsiStrSet,IJclIntfCollection, IJclStrContainer\, IJclAnsiStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A\, B: AnsiString): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A\, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS},,,,const Key: AnsiString,const Value: IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(IInterface,AnsiString,TJclIntfAnsiStrEntry,TJclIntfAnsiStrEntryArray,TJclIntfAnsiStrBucket,TJclIntfAnsiStrBucketArray,TJclIntfAnsiStrHashMap,TJclAnsiStrAbstractContainer,IJclIntfAnsiStrMap,IJclIntfSet,IJclAnsiStrCollection, IJclStrContainer\, IJclAnsiStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: AnsiString): AnsiString; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function Hash(const AInterface: IInterface): Integer; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A\, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A\, B: AnsiString): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS},,,,const Key: IInterface,const Value: AnsiString)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(AnsiString,AnsiString,TJclAnsiStrAnsiStrEntry,TJclAnsiStrAnsiStrEntryArray,TJclAnsiStrAnsiStrBucket,TJclAnsiStrAnsiStrBucketArray,TJclAnsiStrAnsiStrHashMap,TJclAnsiStrAbstractContainer,IJclAnsiStrAnsiStrMap,IJclAnsiStrSet,IJclAnsiStrCollection, IJclStrContainer\, IJclAnsiStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: AnsiString): AnsiString; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A\, B: AnsiString): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A\, B: AnsiString): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS},,,,const Key: AnsiString,const Value: AnsiString)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(WideString,IInterface,TJclWideStrIntfEntry,TJclWideStrIntfEntryArray,TJclWideStrIntfBucket,TJclWideStrIntfBucketArray,TJclWideStrIntfHashMap,TJclWideStrAbstractContainer,IJclWideStrIntfMap,IJclWideStrSet,IJclIntfCollection, IJclStrContainer\, IJclWideStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A\, B: WideString): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A\, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS},,,,const Key: WideString,const Value: IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(IInterface,WideString,TJclIntfWideStrEntry,TJclIntfWideStrEntryArray,TJclIntfWideStrBucket,TJclIntfWideStrBucketArray,TJclIntfWideStrHashMap,TJclWideStrAbstractContainer,IJclIntfWideStrMap,IJclIntfSet,IJclWideStrCollection, IJclStrContainer\, IJclWideStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: WideString): WideString; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function Hash(const AInterface: IInterface): Integer; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A\, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A\, B: WideString): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS},,,,const Key: IInterface,const Value: WideString)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(WideString,WideString,TJclWideStrWideStrEntry,TJclWideStrWideStrEntryArray,TJclWideStrWideStrBucket,TJclWideStrWideStrBucketArray,TJclWideStrWideStrHashMap,TJclWideStrAbstractContainer,IJclWideStrWideStrMap,IJclWideStrSet,IJclWideStrCollection, IJclStrContainer\, IJclWideStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: WideString): WideString; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A\, B: WideString): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A\, B: WideString): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS},,,,const Key: WideString,const Value: WideString)*)

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrIntfHashMap = TJclAnsiStrIntfHashMap;
  TJclIntfStrHashMap = TJclIntfAnsiStrHashMap;
  TJclStrStrHashMap = TJclAnsiStrAnsiStrHashMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrIntfHashMap = TJclWideStrIntfHashMap;
  TJclIntfStrHashMap = TJclIntfWideStrHashMap;
  TJclStrStrHashMap = TJclWideStrWideStrHashMap;
  {$ENDIF CONTAINER_WIDESTR}

(*$JPPEXPANDMACRO JCLHASHMAPINT(IInterface,TObject,TJclIntfEntry,TJclIntfEntryArray,TJclIntfBucket,TJclIntfBucketArray,TJclIntfHashMap,TJclAbstractContainerBase,IJclIntfMap,IJclIntfSet,IJclCollection, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function Hash(const AInterface: IInterface): Integer; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A\, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(A\, B: TObject): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS},
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const Key: IInterface,Value: TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(AnsiString,TObject,TJclAnsiStrEntry,TJclAnsiStrEntryArray,TJclAnsiStrBucket,TJclAnsiStrBucketArray,TJclAnsiStrHashMap,TJclAnsiStrAbstractContainer,IJclAnsiStrMap,IJclAnsiStrSet,IJclCollection, IJclStrContainer\, IJclAnsiStrContainer\, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A\, B: AnsiString): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(A\, B: TObject): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS},
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const Key: AnsiString,Value: TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(WideString,TObject,TJclWideStrEntry,TJclWideStrEntryArray,TJclWideStrBucket,TJclWideStrBucketArray,TJclWideStrHashMap,TJclwideStrAbstractContainer,IJclWideStrMap,IJclWideStrSet,IJclCollection, IJclStrContainer\, IJclWideStrContainer\, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A\, B: WideString): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(A\, B: TObject): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS},
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const Key: WideString,Value: TObject)*)

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrHashMap = TJclAnsiStrHashMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrHashMap = TJclWideStrHashMap;
  {$ENDIF CONTAINER_WIDESTR}

(*$JPPEXPANDMACRO JCLHASHMAPINT(TObject,TObject,TJclEntry,TJclEntryArray,TJclBucket,TJclBucketArray,TJclHashMap,TJclAbstractContainerBase,IJclMap,IJclSet,IJclCollection, IJclKeyOwner\, IJclValueOwner\,,
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;,
    { IJclKeyOwner }
    function FreeKey(var Key: TObject): TObject;
    function GetOwnsKeys: Boolean;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function Hash(AObject: TObject): Integer; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(A\, B: TObject): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(A\, B: TObject): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS},
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,Key: TObject,Value: TObject)*)

  {$IFDEF SUPPORTS_GENERICS}
(*$JPPEXPANDMACRO JCLHASHMAPINT(TKey,TValue,TJclEntry<TKey\,TValue>,TJclEntryArray<TKey\,TValue>,TJclBucket<TKey\,TValue>,TJclBucketArray<TKey\,TValue>,TJclHashMap<TKey\,TValue>,TJclAbstractContainerBase,IJclMap<TKey\,TValue>,IJclSet<TKey>,IJclCollection<TValue>, IJclPairOwner<TKey\, TValue>\,,
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;,
    { IJclPairOwner }
    function FreeKey(var Key: TKey): TKey;
    function FreeValue(var Value: TValue): TValue;
    function GetOwnsKeys: Boolean;
    function GetOwnsValues: Boolean;
    function Hash(const AKey: TKey): Integer; virtual; abstract;
    function KeysEqual(const A\, B: TKey): Boolean; virtual; abstract;
    function ValuesEqual(const A\, B: TValue): Boolean; virtual; abstract;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; virtual; abstract;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; virtual; abstract;,
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,const Key: TKey,const Value: TValue)*)

  // E = external helper to compare and hash items
  // KeyComparer is used only when getting KeySet
  // GetHashCode and Equals methods of KeyEqualityComparer are used
  // GetHashCode of ValueEqualityComparer is not used
  TJclHashMapE<TKey, TValue> = class(TJclHashMap<TKey, TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  private
    FKeyEqualityComparer: IEqualityComparer<TKey>;
    FKeyComparer: IComparer<TKey>;
    FValueEqualityComparer: IEqualityComparer<TValue>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function Hash(const AKey: TKey): Integer; override;
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AKeyEqualityComparer: IEqualityComparer<TKey>;
      const AValueEqualityComparer: IEqualityComparer<TValue>;
      const AKeyComparer: IComparer<TKey>; ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);

    property KeyEqualityComparer: IEqualityComparer<TKey> read FKeyEqualityComparer write FKeyEqualityComparer;
    property KeyComparer: IComparer<TKey> read FKeyComparer write FKeyComparer;
    property ValueEqualityComparer: IEqualityComparer<TValue> read FValueEqualityComparer write FValueEqualityComparer;
  end;

  // F = Functions to compare and hash items
  // KeyComparer is used only when getting KeySet
  TJclHashMapF<TKey, TValue> = class(TJclHashMap<TKey, TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  private
    FKeyEqualityCompare: TEqualityCompare<TKey>;
    FKeyHash: THash<TKey>;
    FKeyCompare: TCompare<TKey>;
    FValueEqualityCompare: TEqualityCompare<TValue>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function Hash(const AKey: TKey): Integer; override;
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(AKeyEqualityCompare: TEqualityCompare<TKey>; AKeyHash: THash<TKey>;
      AValueEqualityCompare: TEqualityCompare<TValue>; AKeyCompare: TCompare<TKey>;
      ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);

    property KeyEqualityCompare: TEqualityCompare<TKey> read FKeyEqualityCompare write FKeyEqualityCompare;
    property KeyCompare: TCompare<TKey> read FKeyCompare write FKeyCompare;
    property KeyHash: THash<TKey> read FKeyHash write FKeyHash;
    property ValueEqualityCompare: TEqualityCompare<TValue> read FValueEqualityCompare write FValueEqualityCompare;
  end;

  // I = items can compare themselves to an other, items can create hash value from themselves
  TJclHashMapI<TKey: IComparable<TKey>, IEquatable<TKey>, IHashable; TValue: IEquatable<TValue>> = class(TJclHashMap<TKey, TValue>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer,
    IJclMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  protected
    function Hash(const AKey: TKey): Integer; override;
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  end;
  {$ENDIF SUPPORTS_GENERICS}

function HashMul(Key, Range: Integer): Integer;

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
  SysUtils,
  JclArrayLists, JclArraySets, JclResources;

function HashMul(Key, Range: Integer): Integer;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(Range * (Frac(Key * A)));
end;

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfIntfHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfIntfHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH
function TJclIntfIntfHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;
}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclIntfIntfHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclIntfIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfIntfHashMap,TJclIntfIntfEntryArray,TJclIntfIntfBucket,IJclIntfIntfMap,IJclIntfSet,IJclIntfIterator,IJclIntfCollection,,,,const Key: IInterface,IInterface,nil,const Value: IInterface,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrIntfHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclAnsiStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclAnsiStrIntfHashMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclAnsiStrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclAnsiStrIntfHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclAnsiStrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclAnsiStrIntfHashMap,TJclAnsiStrIntfEntryArray,TJclAnsiStrIntfBucket,IJclAnsiStrIntfMap,IJclAnsiStrSet,IJclAnsiStrIterator,IJclIntfCollection,,,,const Key: AnsiString,AnsiString,'',const Value: IInterface,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfAnsiStrHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclAnsiStrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfAnsiStrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfAnsiStrHashMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH
function TJclIntfAnsiStrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;
}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclIntfAnsiStrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclIntfAnsiStrHashMap.ValuesEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfAnsiStrHashMap,TJclIntfAnsiStrEntryArray,TJclIntfAnsiStrBucket,IJclIntfAnsiStrMap,IJclIntfSet,IJclIntfIterator,IJclAnsiStrCollection,,,,const Key: IInterface,IInterface,nil,const Value: AnsiString,AnsiString,'')}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrAnsiStrHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclAnsiStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclAnsiStrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclAnsiStrAnsiStrHashMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclAnsiStrAnsiStrHashMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclAnsiStrAnsiStrHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclAnsiStrAnsiStrHashMap.ValuesEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclAnsiStrAnsiStrHashMap,TJclAnsiStrAnsiStrEntryArray,TJclAnsiStrAnsiStrBucket,IJclAnsiStrAnsiStrMap,IJclAnsiStrSet,IJclAnsiStrIterator,IJclAnsiStrCollection,,,,const Key: AnsiString,AnsiString,'',const Value: AnsiString,AnsiString,'')}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrIntfHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclWideStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclWideStrIntfHashMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclWideStrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclWideStrIntfHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclWideStrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclWideStrIntfHashMap,TJclWideStrIntfEntryArray,TJclWideStrIntfBucket,IJclWideStrIntfMap,IJclWideStrSet,IJclWideStrIterator,IJclIntfCollection,,,,const Key: WideString,WideString,'',const Value: IInterface,IInterface,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfWideStrHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclWideStrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfWideStrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfWideStrHashMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH
function TJclIntfWideStrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;
}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclIntfWideStrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclIntfWideStrHashMap.ValuesEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfWideStrHashMap,TJclIntfWideStrEntryArray,TJclIntfWideStrBucket,IJclIntfWideStrMap,IJclIntfSet,IJclIntfIterator,IJclWideStrCollection,,,,const Key: IInterface,IInterface,nil,const Value: WideString,WideString,'')}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrWideStrHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclWideStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclWideStrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclWideStrWideStrHashMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclWideStrWideStrHashMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclWideStrWideStrHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclWideStrWideStrHashMap.ValuesEqual(const A, B: Widestring): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclWideStrWideStrHashMap,TJclWideStrWideStrEntryArray,TJclWideStrWideStrBucket,IJclWideStrWideStrMap,IJclWideStrSet,IJclWideStrIterator,IJclWideStrCollection,,,,const Key: WideString,WideString,'',const Value: WideString,WideString,'')}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfHashMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclIntfHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO HASH
function TJclIntfHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;
}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclIntfHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclIntfHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfHashMap,TJclIntfEntryArray,TJclIntfBucket,IJclIntfMap,IJclIntfSet,IJclIntfIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,const Key: IInterface,IInterface,nil,Value: TObject,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrHashMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclAnsiStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclAnsiStrHashMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclAnsiStrHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclAnsiStrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclAnsiStrHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclAnsiStrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclAnsiStrHashMap,TJclAnsiStrEntryArray,TJclAnsiStrBucket,IJclAnsiStrMap,IJclAnsiStrSet,IJclAnsiStrIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,const Key: AnsiString,AnsiString,'',Value: TObject,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrHashMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclWideStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclWideStrHashMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclWideStrHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclWideStrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclWideStrHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclWideStrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclWideStrHashMap,TJclWideStrEntryArray,TJclWideStrBucket,IJclWideStrMap,IJclWideStrSet,IJclWideStrIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,const Key: WideString,WideString,'',Value: TObject,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER
function TJclHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMap.Create(FSize, False, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclArraySet.Create(Param, False)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclHashMap.FreeKey(var Key: TObject): TObject;
begin
  if FOwnsKeys then
  begin
    Result := nil;
    FreeAndNil(Key);
  end
  else
  begin
    Result := Key;
    Key := nil;
  end;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclHashMap.FreeValue(var Value: TObject): TObject;
begin
  if FOwnsValues then
  begin
    Result := nil;
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := nil;
  end;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS
function TJclHashMap.GetOwnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;
}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO HASH
function TJclHashMap.Hash(AObject: TObject): Integer;
begin
  Result := Integer(AObject);
end;
}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclHashMap.KeysEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclHashMap,TJclEntryArray,TJclBucket,IJclMap,IJclSet,IJclIterator,IJclCollection,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;,Key: TObject,TObject,nil,Value: TObject,TObject,nil)}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

{$IFDEF SUPPORTS_GENERICS}

{$JPPDEFINEMACRO CREATEEMPTYCONTAINER}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)CreateEmptyArraySet(Param, False)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)CreateEmptyArrayList(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclHashMap<TKey, TValue>.FreeKey(var Key: TKey): TKey;
begin
  if FOwnsKeys then
  begin
    Result := Default(TKey);
    FreeAndNil(Key);
  end
  else
  begin
    Result := Key;
    Key := Default(TKey);
  end;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclHashMap<TKey, TValue>.FreeValue(var Value: TValue): TValue;
begin
  if FOwnsValues then
  begin
    Result := Default(TValue);
    FreeAndNil(Value);
  end
  else
  begin
    Result := Value;
    Value := Default(TValue);
  end;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS
function TJclHashMap<TKey, TValue>.GetOwnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;
}
{$JPPDEFINEMACRO GETOWNSVALUES
function TJclHashMap<TKey, TValue>.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL}
{$JPPDEFINEMACRO VALUESEQUAL}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclHashMap<TKey\, TValue>,TJclEntryArray<TKey\, TValue>,TJclBucket<TKey\, TValue>,IJclMap<TKey\, TValue>,IJclSet<TKey>,IJclIterator<TKey>,IJclCollection<TValue>,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;,const Key: TKey,TKey,Default(TKey),const Value: TValue,TValue,Default(TValue))}
{$JPPUNDEFMACRO CREATEEMPTYCONTAINER}
{$JPPUNDEFMACRO CREATEEMPTYARRAYSET(Param)}
{$JPPUNDEFMACRO CREATEEMPTYARRAYLIST(Param)}
{$JPPUNDEFMACRO FREEKEY}
{$JPPUNDEFMACRO FREEVALUE}
{$JPPUNDEFMACRO GETOWNSKEYS}
{$JPPUNDEFMACRO GETOWNSVALUES}
{$JPPUNDEFMACRO HASH}
{$JPPUNDEFMACRO KEYSEQUAL}
{$JPPUNDEFMACRO VALUESEQUAL}

//=== { TJclHashMapE<TKey, TValue> } =========================================

constructor TJclHashMapE<TKey, TValue>.Create(const AKeyEqualityComparer: IEqualityComparer<TKey>;
  const AValueEqualityComparer: IEqualityComparer<TValue>; const AKeyComparer: IComparer<TKey>; ACapacity: Integer;
  AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(ACapacity, AOwnsKeys, AOwnsValues);
  FKeyEqualityComparer := AKeyEqualityComparer;
  FValueEqualityComparer := AValueEqualityComparer;
  FKeyComparer := AKeyComparer;
end;

procedure TJclHashMapE<TKey, TValue>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclHashMapE<TKey, TValue>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclHashMapE<TKey, TValue> then
  begin
    ADest := TJclHashMapE<TKey, TValue>(Dest);
    ADest.FKeyEqualityComparer := FKeyEqualityComparer;
    ADest.FValueEqualityComparer := FValueEqualityComparer;
    ADest.FKeyComparer := FKeyComparer;
  end;
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>;
begin
  Result := TJclArrayListE<TValue>.Create(ValueEqualityComparer, ACapacity, AOwnsObjects);
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TJclArraySetE<TKey>.Create(KeyComparer, ACapacity, AOwnsObjects);
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMapE<TKey, TValue>.Create(KeyEqualityComparer, ValueEqualityComparer,
    KeyComparer, FSize, False, False);
  AssignPropertiesTo(Result);
end;

function TJclHashMapE<TKey, TValue>.Hash(const AKey: TKey): Integer;
begin
  if KeyEqualityComparer = nil then
    raise EJclNoHashConverterError.Create;
  Result := KeyEqualityComparer.GetHashCode(AKey);
end;

function TJclHashMapE<TKey, TValue>.KeysEqual(const A, B: TKey): Boolean;
begin
  if KeyEqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := KeyEqualityComparer.Equals(A, B);
end;

function TJclHashMapE<TKey, TValue>.ValuesEqual(const A, B: TValue): Boolean;
begin
  if ValueEqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := ValueEqualityComparer.Equals(A, B);
end;

//=== { TJclHashMapF<TKey, TValue> } =========================================

constructor TJclHashMapF<TKey, TValue>.Create(AKeyEqualityCompare: TEqualityCompare<TKey>;
  AKeyHash: THash<TKey>; AValueEqualityCompare: TEqualityCompare<TValue>; AKeyCompare: TCompare<TKey>;
  ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(ACapacity, AOwnsKeys, AOwnsValues);
  FKeyEqualityCompare := AKeyEqualityCompare;
  FKeyHash := AKeyHash;
  FValueEqualityCompare := AValueEqualityCompare;
  FKeyCompare := AKeyCompare;
end;

procedure TJclHashMapF<TKey, TValue>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
var
  ADest: TJclHashMapF<TKey, TValue>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclHashMapF<TKey, TValue> then
  begin
    ADest := TJclHashMapF<TKey, TValue>(Dest);
    ADest.FKeyEqualityCompare := FKeyEqualityCompare;
    ADest.FKeyHash := FKeyHash;
    ADest.FValueEqualityCompare := FValueEqualityCompare;
    ADest.FKeyCompare := FKeyCompare;
  end;
end;

function TJclHashMapF<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>;
begin
  Result := TJclArrayListF<TValue>.Create(ValueEqualityCompare, ACapacity, AOwnsObjects);
end;

function TJclHashMapF<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TJclArraySetF<TKey>.Create(KeyCompare, ACapacity, AOwnsObjects);
end;

function TJclHashMapF<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMapF<TKey, TValue>.Create(KeyEqualityCompare, KeyHash, ValueEqualityCompare, KeyCompare, FSize,
    False, False);
  AssignPropertiesTo(Result);
end;

function TJclHashMapF<TKey, TValue>.Hash(const AKey: TKey): Integer;
begin
  if not Assigned(KeyHash) then
    raise EJclNoHashConverterError.Create;
  Result := KeyHash(AKey);
end;

function TJclHashMapF<TKey, TValue>.KeysEqual(const A, B: TKey): Boolean;
begin
  if not Assigned(KeyEqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := KeyEqualityCompare(A, B);
end;

function TJclHashMapF<TKey, TValue>.ValuesEqual(const A, B: TValue): Boolean;
begin
  if not Assigned(ValueEqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := ValueEqualityCompare(A, B);
end;

//=== { TJclHashMapI<TKey, TValue> } =========================================

function TJclHashMapI<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>;
begin
  Result := TJclArrayListI<TValue>.Create(ACapacity, AOwnsObjects);
end;

function TJclHashMapI<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TJclArraySetI<TKey>.Create(ACapacity, AOwnsObjects);
end;

function TJclHashMapI<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMapI<TKey, TValue>.Create(FSize, False, False);
  AssignPropertiesTo(Result);
end;

function TJclHashMapI<TKey, TValue>.Hash(const AKey: TKey): Integer;
begin
  Result := AKey.GetHashCode;
end;

function TJclHashMapI<TKey, TValue>.KeysEqual(const A, B: TKey): Boolean;
begin
  Result := A.Equals(B);
end;

function TJclHashMapI<TKey, TValue>.ValuesEqual(const A, B: TValue): Boolean;
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

