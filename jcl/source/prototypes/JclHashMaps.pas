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

(*$JPPEXPANDMACRO JCLHASHMAPINT(IInterface,IInterface,TJclIntfIntfEntry,TJclIntfIntfEntryArray,TJclIntfIntfBucket,TJclIntfIntfBucketArray,TJclIntfIntfHashMap,TJclAbstractContainer,IJclIntfIntfMap,IJclIntfSet,IJclIntfCollection, IJclContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainer; override;
    function FreeKey(var Key: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function Hash(const AInterface: IInterface): Integer; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A\, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A\, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS},,,,const Key: IInterface,const Value: IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(string,IInterface,TJclStrIntfEntry,TJclStrIntfEntryArray,TJclStrIntfBucket,TJclStrIntfBucketArray,TJclStrIntfHashMap,TJclStrContainer,IJclStrIntfMap,IJclStrSet,IJclIntfCollection, IJclContainer\, IJclStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainer; override;
    function FreeKey(var Key: string): string; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A\, B: string): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A\, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS},,,,const Key: string,const Value: IInterface)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(IInterface,string,TJclIntfStrEntry,TJclIntfStrEntryArray,TJclIntfStrBucket,TJclIntfStrBucketArray,TJclIntfStrHashMap,TJclStrContainer,IJclIntfStrMap,IJclIntfSet,IJclStrCollection, IJclContainer\, IJclStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainer; override;
    function FreeKey(var Key: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: string): string; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function Hash(const AInterface: IInterface): Integer; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A\, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A\, B: string): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS},,,,const Key: IInterface,const Value: string)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(string,string,TJclStrStrEntry,TJclStrStrEntryArray,TJclStrStrBucket,TJclStrStrBucketArray,TJclStrStrHashMap,TJclStrContainer,IJclStrStrMap,IJclStrSet,IJclStrCollection, IJclContainer\, IJclStrContainer\,,,
    function CreateEmptyContainer: TJclAbstractContainer; override;
    function FreeKey(var Key: string): string; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: string): string; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A\, B: string): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A\, B: string): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS},,,,const Key: string,const Value: string)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(IInterface,TObject,TJclIntfEntry,TJclIntfEntryArray,TJclIntfBucket,TJclIntfBucketArray,TJclIntfHashMap,TJclAbstractContainer,IJclIntfMap,IJclIntfSet,IJclCollection, IJclContainer\, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    function FreeKey(var Key: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function Hash(const AInterface: IInterface): Integer; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A\, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(A\, B: TObject): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS},
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const Key: IInterface,Value: TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(string,TObject,TJclStrEntry,TJclStrEntryArray,TJclStrBucket,TJclStrBucketArray,TJclStrHashMap,TJclStrContainer,IJclStrMap,IJclStrSet,IJclCollection, IJclContainer\, IJclStrContainer\, IJclValueOwner\,,
    FOwnsValues: Boolean;,
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    function FreeKey(var Key: string): string; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A\, B: string): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(A\, B: TObject): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS},
    property OwnsValues: Boolean read FOwnsValues;,,; AOwnsValues: Boolean,const Key: string,Value: TObject)*)

(*$JPPEXPANDMACRO JCLHASHMAPINT(TObject,TObject,TJclEntry,TJclEntryArray,TJclBucket,TJclBucketArray,TJclHashMap,TJclAbstractContainer,IJclMap,IJclSet,IJclCollection, IJclContainer\, IJclKeyOwner\, IJclValueOwner\,,
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;,
    { IJclKeyOwner }
    function FreeKey(var Key: TObject): TObject;
    function GetOwnsKeys: Boolean;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    function Hash(AObject: TObject): Integer; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(A\, B: TObject): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(A\, B: TObject): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS},
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;,; AOwnsKeys: Boolean,; AOwnsValues: Boolean,Key: TObject,Value: TObject)*)

  {$IFDEF SUPPORTS_GENERICS}
(*$JPPEXPANDMACRO JCLHASHMAPINT(TKey,TValue,TJclEntry<TKey\,TValue>,TJclEntryArray<TKey\,TValue>,TJclBucket<TKey\,TValue>,TJclBucketArray<TKey\,TValue>,TJclHashMap<TKey\,TValue>,TJclAbstractContainer,IJclMap<TKey\,TValue>,IJclSet<TKey>,IJclCollection<TValue>, IJclContainer\, IJclPairOwner<TKey\, TValue>\,,
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
  TJclHashMapE<TKey, TValue> = class(TJclHashMap<TKey, TValue>, IJclMap<TKey,TValue>, IJclContainer, IJclPairOwner<TKey, TValue>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable)
  private
    FKeyEqualityComparer: IEqualityComparer<TKey>;
    FKeyComparer: IComparer<TKey>;
    FValueEqualityComparer: IEqualityComparer<TValue>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    function Hash(const AKey: TKey): Integer; override;
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
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
  TJclHashMapF<TKey, TValue> = class(TJclHashMap<TKey, TValue>, IJclMap<TKey,TValue>, IJclContainer, IJclPairOwner<TKey, TValue>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable)
  private
    FKeyEqualityCompare: TEqualityCompare<TKey>;
    FKeyHash: THash<TKey>;
    FKeyCompare: TCompare<TKey>;
    FValueEqualityCompare: TEqualityCompare<TValue>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    function Hash(const AKey: TKey): Integer; override;
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
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
    IJclMap<TKey,TValue>,  IJclContainer, IJclPairOwner<TKey, TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable)
  protected
    function Hash(const AKey: TKey): Integer; override;
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
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
function TJclIntfIntfHashMap.CreateEmptyContainer: TJclAbstractContainer;
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
function TJclStrIntfHashMap.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStrIntfHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclIntfArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclStrIntfHashMap.FreeKey(var Key: string): string;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclStrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclStrIntfHashMap.KeysEqual(const A, B: string): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclStrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclStrIntfHashMap,TJclStrIntfEntryArray,TJclStrIntfBucket,IJclStrIntfMap,IJclStrSet,IJclStrIterator,IJclIntfCollection,,,,const Key: string,string,'',const Value: IInterface,IInterface,nil)}
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
function TJclIntfStrHashMap.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclIntfStrHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclIntfArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclStrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclIntfStrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclIntfStrHashMap.FreeValue(var Value: string): string;
begin
  Result := Value;
  Value := '';
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH
function TJclIntfStrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;
}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclIntfStrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclIntfStrHashMap.ValuesEqual(const A, B: string): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclIntfStrHashMap,TJclIntfStrEntryArray,TJclIntfStrBucket,IJclIntfStrMap,IJclIntfSet,IJclIntfIterator,IJclStrCollection,,,,const Key: IInterface,IInterface,nil,const Value: string,string,'')}
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
function TJclStrStrHashMap.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStrStrHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclStrArrayList.Create(Param)}
{$JPPDEFINEMACRO FREEKEY
function TJclStrStrHashMap.FreeKey(var Key: string): string;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclStrStrHashMap.FreeValue(var Value: string): string;
begin
  Result := Value;
  Value := '';
end;
}
{$JPPDEFINEMACRO GETOWNSKEYS}
{$JPPDEFINEMACRO GETOWNSVALUES}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclStrStrHashMap.KeysEqual(const A, B: string): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclStrStrHashMap.ValuesEqual(const A, B: string): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclStrStrHashMap,TJclStrStrEntryArray,TJclStrStrBucket,IJclStrStrMap,IJclStrSet,IJclStrIterator,IJclStrCollection,,,,const Key: string,string,'',const Value: string,string,'')}
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
function TJclIntfHashMap.CreateEmptyContainer: TJclAbstractContainer;
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
function TJclStrHashMap.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStrHashMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;
}
{$JPPDEFINEMACRO CREATEEMPTYARRAYSET(Param)TJclStrArraySet.Create(Param)}
{$JPPDEFINEMACRO CREATEEMPTYARRAYLIST(Param)TJclArrayList.Create(Param, False)}
{$JPPDEFINEMACRO FREEKEY
function TJclStrHashMap.FreeKey(var Key: string): string;
begin
  Result := Key;
  Key := '';
end;
}
{$JPPDEFINEMACRO FREEVALUE
function TJclStrHashMap.FreeValue(var Value: TObject): TObject;
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
function TJclStrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;
}
{$JPPDEFINEMACRO HASH}
{$JPPDEFINEMACRO KEYSEQUAL
function TJclStrHashMap.KeysEqual(const A, B: string): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
}
{$JPPDEFINEMACRO VALUESEQUAL
function TJclStrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
}
{$JPPEXPANDMACRO JCLHASHMAPIMP(TJclStrHashMap,TJclStrEntryArray,TJclStrBucket,IJclStrMap,IJclStrSet,IJclStrIterator,IJclCollection,; AOwnsValues: Boolean,,
  FOwnsValues := AOwnsValues;,const Key: string,string,'',Value: TObject,TObject,nil)}
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
function TJclHashMap.CreateEmptyContainer: TJclAbstractContainer;
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

procedure TJclHashMapE<TKey, TValue>.AssignPropertiesTo(Dest: TJclAbstractContainer);
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

function TJclHashMapE<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainer;
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

procedure TJclHashMapF<TKey, TValue>.AssignPropertiesTo(Dest: TJclAbstractContainer);
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

function TJclHashMapF<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainer;
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

function TJclHashMapI<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainer;
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

