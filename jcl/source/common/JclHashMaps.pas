{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}

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

type
  // Hash Function
  // Result must be in 0..Range-1
  TJclHashFunction = function(Key, Range: Integer): Integer;


  TJclIntfIntfEntry = record
    Key: IInterface;
    Value: IInterface;
  end;

  TJclIntfIntfEntryArray = array of TJclIntfIntfEntry;

  TJclIntfIntfBucket = class
  public
    Size: Integer;
    Entries: TJclIntfIntfEntryArray;
  end;

  TJclIntfIntfBucketArray = array of TJclIntfIntfBucket;

  TJclIntfIntfHashMap = class(TJclAbstractContainer, IJclIntfIntfMap, IJclContainer,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable)
  private
    FBuckets: TJclIntfIntfBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainer); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    procedure MoveArray(var List: TJclIntfIntfEntryArray; FromIndex, ToIndex, Count: Integer);
    procedure GrowEntries(Bucket: TJclIntfIntfBucket); virtual;
    procedure PackEntries(Bucket: TJclIntfIntfBucket); virtual;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfIntfMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclIntfIntfMap): Boolean;
    function GetValue(const Key: IInterface): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfIntfMap);
    procedure PutValue(const Key: IInterface; const Value: IInterface);
    function Remove(const Key: IInterface): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    function FreeKey(var Key: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function Hash(const AInterface: IInterface): Integer; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;


  TJclStrIntfEntry = record
    Key: string;
    Value: IInterface;
  end;

  TJclStrIntfEntryArray = array of TJclStrIntfEntry;

  TJclStrIntfBucket = class
  public
    Size: Integer;
    Entries: TJclStrIntfEntryArray;
  end;

  TJclStrIntfBucketArray = array of TJclStrIntfBucket;

  TJclStrIntfHashMap = class(TJclStrContainer, IJclStrIntfMap, IJclContainer, IJclStrContainer,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable)
  private
    FBuckets: TJclStrIntfBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainer); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    procedure MoveArray(var List: TJclStrIntfEntryArray; FromIndex, ToIndex, Count: Integer);
    procedure GrowEntries(Bucket: TJclStrIntfBucket); virtual;
    procedure PackEntries(Bucket: TJclStrIntfBucket); virtual;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclStrIntfMap }
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclStrIntfMap): Boolean;
    function GetValue(const Key: string): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): string;
    function KeySet: IJclStrSet;
    procedure PutAll(const AMap: IJclStrIntfMap);
    procedure PutValue(const Key: string; const Value: IInterface);
    function Remove(const Key: string): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    function FreeKey(var Key: string): string; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A, B: string): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;


  TJclIntfStrEntry = record
    Key: IInterface;
    Value: string;
  end;

  TJclIntfStrEntryArray = array of TJclIntfStrEntry;

  TJclIntfStrBucket = class
  public
    Size: Integer;
    Entries: TJclIntfStrEntryArray;
  end;

  TJclIntfStrBucketArray = array of TJclIntfStrBucket;

  TJclIntfStrHashMap = class(TJclStrContainer, IJclIntfStrMap, IJclContainer, IJclStrContainer,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable)
  private
    FBuckets: TJclIntfStrBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainer); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    procedure MoveArray(var List: TJclIntfStrEntryArray; FromIndex, ToIndex, Count: Integer);
    procedure GrowEntries(Bucket: TJclIntfStrBucket); virtual;
    procedure PackEntries(Bucket: TJclIntfStrBucket); virtual;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfStrMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: string): Boolean;
    function Equals(const AMap: IJclIntfStrMap): Boolean;
    function GetValue(const Key: IInterface): string;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: string): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfStrMap);
    procedure PutValue(const Key: IInterface; const Value: string);
    function Remove(const Key: IInterface): string;
    function Size: Integer;
    function Values: IJclStrCollection;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    function FreeKey(var Key: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: string): string; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function Hash(const AInterface: IInterface): Integer; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A, B: string): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;


  TJclStrStrEntry = record
    Key: string;
    Value: string;
  end;

  TJclStrStrEntryArray = array of TJclStrStrEntry;

  TJclStrStrBucket = class
  public
    Size: Integer;
    Entries: TJclStrStrEntryArray;
  end;

  TJclStrStrBucketArray = array of TJclStrStrBucket;

  TJclStrStrHashMap = class(TJclStrContainer, IJclStrStrMap, IJclContainer, IJclStrContainer,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable)
  private
    FBuckets: TJclStrStrBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainer); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    procedure MoveArray(var List: TJclStrStrEntryArray; FromIndex, ToIndex, Count: Integer);
    procedure GrowEntries(Bucket: TJclStrStrBucket); virtual;
    procedure PackEntries(Bucket: TJclStrStrBucket); virtual;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclStrStrMap }
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(const Value: string): Boolean;
    function Equals(const AMap: IJclStrStrMap): Boolean;
    function GetValue(const Key: string): string;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: string): string;
    function KeySet: IJclStrSet;
    procedure PutAll(const AMap: IJclStrStrMap);
    procedure PutValue(const Key: string; const Value: string);
    function Remove(const Key: string): string;
    function Size: Integer;
    function Values: IJclStrCollection;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    function FreeKey(var Key: string): string; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: string): string; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A, B: string): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A, B: string): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;


  TJclIntfEntry = record
    Key: IInterface;
    Value: TObject;
  end;

  TJclIntfEntryArray = array of TJclIntfEntry;

  TJclIntfBucket = class
  public
    Size: Integer;
    Entries: TJclIntfEntryArray;
  end;

  TJclIntfBucketArray = array of TJclIntfBucket;

  TJclIntfHashMap = class(TJclAbstractContainer, IJclIntfMap, IJclContainer, IJclValueOwner,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable)
  private
    FBuckets: TJclIntfBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainer); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    procedure MoveArray(var List: TJclIntfEntryArray; FromIndex, ToIndex, Count: Integer);
    procedure GrowEntries(Bucket: TJclIntfBucket); virtual;
    procedure PackEntries(Bucket: TJclIntfBucket); virtual;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclIntfMap): Boolean;
    function GetValue(const Key: IInterface): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfMap);
    procedure PutValue(const Key: IInterface; Value: TObject);
    function Remove(const Key: IInterface): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    function FreeKey(var Key: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function Hash(const AInterface: IInterface): Integer; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(A, B: TObject): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsValues: Boolean read FOwnsValues;
  end;


  TJclStrEntry = record
    Key: string;
    Value: TObject;
  end;

  TJclStrEntryArray = array of TJclStrEntry;

  TJclStrBucket = class
  public
    Size: Integer;
    Entries: TJclStrEntryArray;
  end;

  TJclStrBucketArray = array of TJclStrBucket;

  TJclStrHashMap = class(TJclStrContainer, IJclStrMap, IJclContainer, IJclStrContainer, IJclValueOwner,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable)
  private
    FBuckets: TJclStrBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainer); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    procedure MoveArray(var List: TJclStrEntryArray; FromIndex, ToIndex, Count: Integer);
    procedure GrowEntries(Bucket: TJclStrBucket); virtual;
    procedure PackEntries(Bucket: TJclStrBucket); virtual;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclStrMap }
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclStrMap): Boolean;
    function GetValue(const Key: string): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): string;
    function KeySet: IJclStrSet;
    procedure PutAll(const AMap: IJclStrMap);
    procedure PutValue(const Key: string; Value: TObject);
    function Remove(const Key: string): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    function FreeKey(var Key: string): string; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A, B: string): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(A, B: TObject): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsValues: Boolean read FOwnsValues;
  end;


  TJclEntry = record
    Key: TObject;
    Value: TObject;
  end;

  TJclEntryArray = array of TJclEntry;

  TJclBucket = class
  public
    Size: Integer;
    Entries: TJclEntryArray;
  end;

  TJclBucketArray = array of TJclBucket;

  TJclHashMap = class(TJclAbstractContainer, IJclMap, IJclContainer, IJclKeyOwner, IJclValueOwner,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable)
  private
    FBuckets: TJclBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainer); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    procedure MoveArray(var List: TJclEntryArray; FromIndex, ToIndex, Count: Integer);
    procedure GrowEntries(Bucket: TJclBucket); virtual;
    procedure PackEntries(Bucket: TJclBucket); virtual;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclMap }
    procedure Clear;
    function ContainsKey(Key: TObject): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclMap): Boolean;
    function GetValue(Key: TObject): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): TObject;
    function KeySet: IJclSet;
    procedure PutAll(const AMap: IJclMap);
    procedure PutValue(Key: TObject; Value: TObject);
    function Remove(Key: TObject): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclKeyOwner }
    function FreeKey(var Key: TObject): TObject;
    function GetOwnsKeys: Boolean;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    function Hash(AObject: TObject): Integer; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(A, B: TObject): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(A, B: TObject): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  {$IFDEF SUPPORTS_GENERICS}

  TJclEntry<TKey,TValue> = record
    Key: TKey;
    Value: TValue;
  end;

  TJclEntryArray<TKey,TValue> = array of TJclEntry<TKey,TValue>;

  TJclBucket<TKey,TValue> = class
  public
    Size: Integer;
    Entries: TJclEntryArray<TKey,TValue>;
  end;

  TJclBucketArray<TKey,TValue> = array of TJclBucket<TKey,TValue>;

  TJclHashMap<TKey,TValue> = class(TJclAbstractContainer, IJclMap<TKey,TValue>, IJclContainer, IJclPairOwner<TKey, TValue>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable)
  private
    FBuckets: TJclBucketArray<TKey,TValue>;
    FHashFunction: TJclHashFunction;
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainer); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    procedure MoveArray(var List: TJclEntryArray<TKey,TValue>; FromIndex, ToIndex, Count: Integer);
    procedure GrowEntries(Bucket: TJclBucket<TKey,TValue>); virtual;
    procedure PackEntries(Bucket: TJclBucket<TKey,TValue>); virtual;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclMap<TKey,TValue> }
    procedure Clear;
    function ContainsKey(const Key: TKey): Boolean;
    function ContainsValue(const Value: TValue): Boolean;
    function Equals(const AMap: IJclMap<TKey,TValue>): Boolean;
    function GetValue(const Key: TKey): TValue;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: TValue): TKey;
    function KeySet: IJclSet<TKey>;
    procedure PutAll(const AMap: IJclMap<TKey,TValue>);
    procedure PutValue(const Key: TKey; const Value: TValue);
    function Remove(const Key: TKey): TValue;
    function Size: Integer;
    function Values: IJclCollection<TValue>;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclPairOwner }
    function FreeKey(var Key: TKey): TKey;
    function FreeValue(var Value: TValue): TValue;
    function GetOwnsKeys: Boolean;
    function GetOwnsValues: Boolean;
    function Hash(const AKey: TKey): Integer; virtual; abstract;
    function KeysEqual(const A, B: TKey): Boolean; virtual; abstract;
    function ValuesEqual(const A, B: TValue): Boolean; virtual; abstract;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; virtual; abstract;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; virtual; abstract;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;
  end;

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


//=== { TJclIntfIntfHashMap } ==========================================

constructor TJclIntfIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfIntfHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclIntfIntfHashMap.AssignDataTo(Dest: TJclAbstractContainer);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfIntfBucket;
  ADest: TJclIntfIntfHashMap;
  AMap: IJclIntfIntfMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfIntfHashMap then
  begin
    ADest := TJclIntfIntfHashMap(Dest);
    {$IFDEF THREADSAFE}
    ReadLock;
    try
    {$ENDIF THREADSAFE}
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfIntfBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    {$IFDEF THREADSAFE}
    finally
      ReadUnlock;
    end;
    {$ENDIF THREADSAFE}
  end
  else
  if Supports(Dest, IJclIntfIntfMap, AMap) then
  begin
    AMap.Clear;
    AMap.PutAll(Self);
  end;
end;

procedure TJclIntfIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfIntfHashMap then
    TJclIntfIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclIntfIntfHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfIntfHashMap.Equals(const AMap: IJclIntfIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclIntfIntfHashMap.GetValue(const Key: IInterface): IInterface;
var
  I: Integer;
  Bucket: TJclIntfIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.GrowEntries(Bucket: TJclIntfIntfBucket);
var
  BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  case FAutoGrowStrategy of
    agsDisabled: ;
    agsAgressive:
      Inc(BucketCapacity, 1);
    agsProportional:
      Inc(BucketCapacity, BucketCapacity div FAutoGrowParameter);
    agsIncremental:
      Inc(BucketCapacity, FAutoGrowParameter);
  end;
  SetLength(Bucket.Entries, BucketCapacity);
end;

function TJclIntfIntfHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfIntfHashMap.KeyOfValue(const Value: IInterface): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfIntfHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.MoveArray(var List: TJclIntfIntfEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
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
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.PackEntries(Bucket: TJclIntfIntfBucket);
var
  Decrement, BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  case FAutoPackStrategy of
    apsDisabled:
      Decrement := 0;
    apsAgressive:
      Decrement := 1;
    apsProportional:
      Decrement := BucketCapacity div FAutoPackParameter;
    apsIncremental:
      Decrement := FAutoPackParameter;
  else
    Decrement := 0;
  end;
  if (Decrement > 0) and ((Bucket.Size + Decrement) <= BucketCapacity) then
    SetLength(Bucket.Entries, Bucket.Size);
end;

procedure TJclIntfIntfHashMap.PutAll(const AMap: IJclIntfIntfMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.PutValue(const Key: IInterface; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclIntfIntfBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfIntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        GrowEntries(Bucket);
      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.Remove(const Key: IInterface): IInterface;
var
  Bucket: TJclIntfIntfBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;
      PackEntries(Bucket);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;



//=== { TJclStrIntfHashMap } ==========================================

constructor TJclStrIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclStrIntfHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclStrIntfHashMap.AssignDataTo(Dest: TJclAbstractContainer);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclStrIntfBucket;
  ADest: TJclStrIntfHashMap;
  AMap: IJclStrIntfMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclStrIntfHashMap then
  begin
    ADest := TJclStrIntfHashMap(Dest);
    {$IFDEF THREADSAFE}
    ReadLock;
    try
    {$ENDIF THREADSAFE}
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclStrIntfBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    {$IFDEF THREADSAFE}
    finally
      ReadUnlock;
    end;
    {$ENDIF THREADSAFE}
  end
  else
  if Supports(Dest, IJclStrIntfMap, AMap) then
  begin
    AMap.Clear;
    AMap.PutAll(Self);
  end;
end;

procedure TJclStrIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclStrIntfHashMap then
    TJclStrIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclStrIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrIntfHashMap.ContainsKey(const Key: string): Boolean;
var
  I: Integer;
  Bucket: TJclStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrIntfHashMap.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStrIntfHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclStrIntfHashMap.Equals(const AMap: IJclStrIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrIntfHashMap.FreeKey(var Key: string): string;
begin
  Result := Key;
  Key := '';
end;

function TJclStrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclStrIntfHashMap.GetValue(const Key: string): IInterface;
var
  I: Integer;
  Bucket: TJclStrIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrIntfHashMap.GrowEntries(Bucket: TJclStrIntfBucket);
var
  BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  case FAutoGrowStrategy of
    agsDisabled: ;
    agsAgressive:
      Inc(BucketCapacity, 1);
    agsProportional:
      Inc(BucketCapacity, BucketCapacity div FAutoGrowParameter);
    agsIncremental:
      Inc(BucketCapacity, FAutoGrowParameter);
  end;
  SetLength(Bucket.Entries, BucketCapacity);
end;


function TJclStrIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclStrIntfHashMap.KeyOfValue(const Value: IInterface): string;
var
  I, J: Integer;
  Bucket: TJclStrIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrIntfHashMap.KeysEqual(const A, B: string): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclStrIntfHashMap.KeySet: IJclStrSet;
var
  I, J: Integer;
  Bucket: TJclStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrIntfHashMap.MoveArray(var List: TJclStrIntfEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
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
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclStrIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrIntfHashMap.PackEntries(Bucket: TJclStrIntfBucket);
var
  Decrement, BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  case FAutoPackStrategy of
    apsDisabled:
      Decrement := 0;
    apsAgressive:
      Decrement := 1;
    apsProportional:
      Decrement := BucketCapacity div FAutoPackParameter;
    apsIncremental:
      Decrement := FAutoPackParameter;
  else
    Decrement := 0;
  end;
  if (Decrement > 0) and ((Bucket.Size + Decrement) <= BucketCapacity) then
    SetLength(Bucket.Entries, Bucket.Size);
end;

procedure TJclStrIntfHashMap.PutAll(const AMap: IJclStrIntfMap);
var
  It: IJclStrIterator;
  Key: string;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrIntfHashMap.PutValue(const Key: string; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclStrIntfBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, '') and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclStrIntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        GrowEntries(Bucket);
      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrIntfHashMap.Remove(const Key: string): IInterface;
var
  Bucket: TJclStrIntfBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;
      PackEntries(Bucket);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrIntfHashMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclStrIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;



//=== { TJclIntfStrHashMap } ==========================================

constructor TJclIntfStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfStrHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclIntfStrHashMap.AssignDataTo(Dest: TJclAbstractContainer);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfStrBucket;
  ADest: TJclIntfStrHashMap;
  AMap: IJclIntfStrMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfStrHashMap then
  begin
    ADest := TJclIntfStrHashMap(Dest);
    {$IFDEF THREADSAFE}
    ReadLock;
    try
    {$ENDIF THREADSAFE}
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfStrBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    {$IFDEF THREADSAFE}
    finally
      ReadUnlock;
    end;
    {$ENDIF THREADSAFE}
  end
  else
  if Supports(Dest, IJclIntfStrMap, AMap) then
  begin
    AMap.Clear;
    AMap.PutAll(Self);
  end;
end;

procedure TJclIntfStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfStrHashMap then
    TJclIntfStrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfStrBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStrHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStrHashMap.ContainsValue(const Value: string): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStrHashMap.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclIntfStrHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfStrHashMap.Equals(const AMap: IJclIntfStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfStrHashMap.FreeValue(var Value: string): string;
begin
  Result := Value;
  Value := '';
end;

function TJclIntfStrHashMap.GetValue(const Key: IInterface): string;
var
  I: Integer;
  Bucket: TJclIntfStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfStrHashMap.GrowEntries(Bucket: TJclIntfStrBucket);
var
  BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  case FAutoGrowStrategy of
    agsDisabled: ;
    agsAgressive:
      Inc(BucketCapacity, 1);
    agsProportional:
      Inc(BucketCapacity, BucketCapacity div FAutoGrowParameter);
    agsIncremental:
      Inc(BucketCapacity, FAutoGrowParameter);
  end;
  SetLength(Bucket.Entries, BucketCapacity);
end;

function TJclIntfStrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfStrHashMap.KeyOfValue(const Value: string): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfStrHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfStrHashMap.MoveArray(var List: TJclIntfStrEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
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
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := '';
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := '';
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfStrBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfStrHashMap.PackEntries(Bucket: TJclIntfStrBucket);
var
  Decrement, BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  case FAutoPackStrategy of
    apsDisabled:
      Decrement := 0;
    apsAgressive:
      Decrement := 1;
    apsProportional:
      Decrement := BucketCapacity div FAutoPackParameter;
    apsIncremental:
      Decrement := FAutoPackParameter;
  else
    Decrement := 0;
  end;
  if (Decrement > 0) and ((Bucket.Size + Decrement) <= BucketCapacity) then
    SetLength(Bucket.Entries, Bucket.Size);
end;

procedure TJclIntfStrHashMap.PutAll(const AMap: IJclIntfStrMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfStrHashMap.PutValue(const Key: IInterface; const Value: string);
var
  Index: Integer;
  Bucket: TJclIntfStrBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, '')) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfStrBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        GrowEntries(Bucket);
      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStrHashMap.Remove(const Key: IInterface): string;
var
  Bucket: TJclIntfStrBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;
      PackEntries(Bucket);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfStrHashMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfStrHashMap.Values: IJclStrCollection;
var
  I, J: Integer;
  Bucket: TJclIntfStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfStrHashMap.ValuesEqual(const A, B: string): Boolean;
begin
  Result := ItemsEqual(A, B);
end;



//=== { TJclStrStrHashMap } ==========================================

constructor TJclStrStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclStrStrHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclStrStrHashMap.AssignDataTo(Dest: TJclAbstractContainer);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclStrStrBucket;
  ADest: TJclStrStrHashMap;
  AMap: IJclStrStrMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclStrStrHashMap then
  begin
    ADest := TJclStrStrHashMap(Dest);
    {$IFDEF THREADSAFE}
    ReadLock;
    try
    {$ENDIF THREADSAFE}
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclStrStrBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    {$IFDEF THREADSAFE}
    finally
      ReadUnlock;
    end;
    {$ENDIF THREADSAFE}
  end
  else
  if Supports(Dest, IJclStrStrMap, AMap) then
  begin
    AMap.Clear;
    AMap.PutAll(Self);
  end;
end;

procedure TJclStrStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclStrStrHashMap then
    TJclStrStrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclStrStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclStrStrBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStrHashMap.ContainsKey(const Key: string): Boolean;
var
  I: Integer;
  Bucket: TJclStrStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStrHashMap.ContainsValue(const Value: string): Boolean;
var
  I, J: Integer;
  Bucket: TJclStrStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStrHashMap.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStrStrHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclStrStrHashMap.Equals(const AMap: IJclStrStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclStrStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStrHashMap.FreeKey(var Key: string): string;
begin
  Result := Key;
  Key := '';
end;

function TJclStrStrHashMap.FreeValue(var Value: string): string;
begin
  Result := Value;
  Value := '';
end;



function TJclStrStrHashMap.GetValue(const Key: string): string;
var
  I: Integer;
  Bucket: TJclStrStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrStrHashMap.GrowEntries(Bucket: TJclStrStrBucket);
var
  BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  case FAutoGrowStrategy of
    agsDisabled: ;
    agsAgressive:
      Inc(BucketCapacity, 1);
    agsProportional:
      Inc(BucketCapacity, BucketCapacity div FAutoGrowParameter);
    agsIncremental:
      Inc(BucketCapacity, FAutoGrowParameter);
  end;
  SetLength(Bucket.Entries, BucketCapacity);
end;


function TJclStrStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclStrStrHashMap.KeyOfValue(const Value: string): string;
var
  I, J: Integer;
  Bucket: TJclStrStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStrHashMap.KeysEqual(const A, B: string): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclStrStrHashMap.KeySet: IJclStrSet;
var
  I, J: Integer;
  Bucket: TJclStrStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrStrHashMap.MoveArray(var List: TJclStrStrEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
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
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := '';
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := '';
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclStrStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclStrStrBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrStrHashMap.PackEntries(Bucket: TJclStrStrBucket);
var
  Decrement, BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  case FAutoPackStrategy of
    apsDisabled:
      Decrement := 0;
    apsAgressive:
      Decrement := 1;
    apsProportional:
      Decrement := BucketCapacity div FAutoPackParameter;
    apsIncremental:
      Decrement := FAutoPackParameter;
  else
    Decrement := 0;
  end;
  if (Decrement > 0) and ((Bucket.Size + Decrement) <= BucketCapacity) then
    SetLength(Bucket.Entries, Bucket.Size);
end;

procedure TJclStrStrHashMap.PutAll(const AMap: IJclStrStrMap);
var
  It: IJclStrIterator;
  Key: string;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrStrHashMap.PutValue(const Key: string; const Value: string);
var
  Index: Integer;
  Bucket: TJclStrStrBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, '') and not ValuesEqual(Value, '')) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclStrStrBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        GrowEntries(Bucket);
      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStrHashMap.Remove(const Key: string): string;
var
  Bucket: TJclStrStrBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;
      PackEntries(Bucket);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrStrHashMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclStrStrHashMap.Values: IJclStrCollection;
var
  I, J: Integer;
  Bucket: TJclStrStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrStrHashMap.ValuesEqual(const A, B: string): Boolean;
begin
  Result := ItemsEqual(A, B);
end;



//=== { TJclIntfHashMap } ==========================================

constructor TJclIntfHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create(nil);
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclIntfHashMap.AssignDataTo(Dest: TJclAbstractContainer);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfBucket;
  ADest: TJclIntfHashMap;
  AMap: IJclIntfMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfHashMap then
  begin
    ADest := TJclIntfHashMap(Dest);
    {$IFDEF THREADSAFE}
    ReadLock;
    try
    {$ENDIF THREADSAFE}
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    {$IFDEF THREADSAFE}
    finally
      ReadUnlock;
    end;
    {$ENDIF THREADSAFE}
  end
  else
  if Supports(Dest, IJclIntfMap, AMap) then
  begin
    AMap.Clear;
    AMap.PutAll(Self);
  end;
end;

procedure TJclIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfHashMap then
    TJclIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclIntfHashMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclIntfHashMap.Equals(const AMap: IJclIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

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


function TJclIntfHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclIntfHashMap.GetValue(const Key: IInterface): TObject;
var
  I: Integer;
  Bucket: TJclIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashMap.GrowEntries(Bucket: TJclIntfBucket);
var
  BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  case FAutoGrowStrategy of
    agsDisabled: ;
    agsAgressive:
      Inc(BucketCapacity, 1);
    agsProportional:
      Inc(BucketCapacity, BucketCapacity div FAutoGrowParameter);
    agsIncremental:
      Inc(BucketCapacity, FAutoGrowParameter);
  end;
  SetLength(Bucket.Entries, BucketCapacity);
end;

function TJclIntfHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfHashMap.KeyOfValue(Value: TObject): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashMap.MoveArray(var List: TJclIntfEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
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
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashMap.PackEntries(Bucket: TJclIntfBucket);
var
  Decrement, BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  case FAutoPackStrategy of
    apsDisabled:
      Decrement := 0;
    apsAgressive:
      Decrement := 1;
    apsProportional:
      Decrement := BucketCapacity div FAutoPackParameter;
    apsIncremental:
      Decrement := FAutoPackParameter;
  else
    Decrement := 0;
  end;
  if (Decrement > 0) and ((Bucket.Size + Decrement) <= BucketCapacity) then
    SetLength(Bucket.Entries, Bucket.Size);
end;

procedure TJclIntfHashMap.PutAll(const AMap: IJclIntfMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashMap.PutValue(const Key: IInterface; Value: TObject);
var
  Index: Integer;
  Bucket: TJclIntfBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclIntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        GrowEntries(Bucket);
      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.Remove(const Key: IInterface): TObject;
var
  Bucket: TJclIntfBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;
      PackEntries(Bucket);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;



//=== { TJclStrHashMap } ==========================================

constructor TJclStrHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create(nil);
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclStrHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclStrHashMap.AssignDataTo(Dest: TJclAbstractContainer);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclStrBucket;
  ADest: TJclStrHashMap;
  AMap: IJclStrMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclStrHashMap then
  begin
    ADest := TJclStrHashMap(Dest);
    {$IFDEF THREADSAFE}
    ReadLock;
    try
    {$ENDIF THREADSAFE}
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclStrBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    {$IFDEF THREADSAFE}
    finally
      ReadUnlock;
    end;
    {$ENDIF THREADSAFE}
  end
  else
  if Supports(Dest, IJclStrMap, AMap) then
  begin
    AMap.Clear;
    AMap.PutAll(Self);
  end;
end;

procedure TJclStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclStrHashMap then
    TJclStrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclStrBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrHashMap.ContainsKey(const Key: string): Boolean;
var
  I: Integer;
  Bucket: TJclStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrHashMap.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStrHashMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclStrHashMap.Equals(const AMap: IJclStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrHashMap.FreeKey(var Key: string): string;
begin
  Result := Key;
  Key := '';
end;

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


function TJclStrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclStrHashMap.GetValue(const Key: string): TObject;
var
  I: Integer;
  Bucket: TJclStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrHashMap.GrowEntries(Bucket: TJclStrBucket);
var
  BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  case FAutoGrowStrategy of
    agsDisabled: ;
    agsAgressive:
      Inc(BucketCapacity, 1);
    agsProportional:
      Inc(BucketCapacity, BucketCapacity div FAutoGrowParameter);
    agsIncremental:
      Inc(BucketCapacity, FAutoGrowParameter);
  end;
  SetLength(Bucket.Entries, BucketCapacity);
end;


function TJclStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclStrHashMap.KeyOfValue(Value: TObject): string;
var
  I, J: Integer;
  Bucket: TJclStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrHashMap.KeysEqual(const A, B: string): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclStrHashMap.KeySet: IJclStrSet;
var
  I, J: Integer;
  Bucket: TJclStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrHashMap.MoveArray(var List: TJclStrEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
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
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := '';
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclStrBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrHashMap.PackEntries(Bucket: TJclStrBucket);
var
  Decrement, BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  case FAutoPackStrategy of
    apsDisabled:
      Decrement := 0;
    apsAgressive:
      Decrement := 1;
    apsProportional:
      Decrement := BucketCapacity div FAutoPackParameter;
    apsIncremental:
      Decrement := FAutoPackParameter;
  else
    Decrement := 0;
  end;
  if (Decrement > 0) and ((Bucket.Size + Decrement) <= BucketCapacity) then
    SetLength(Bucket.Entries, Bucket.Size);
end;

procedure TJclStrHashMap.PutAll(const AMap: IJclStrMap);
var
  It: IJclStrIterator;
  Key: string;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrHashMap.PutValue(const Key: string; Value: TObject);
var
  Index: Integer;
  Bucket: TJclStrBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, '') and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclStrBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        GrowEntries(Bucket);
      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrHashMap.Remove(const Key: string): TObject;
var
  Bucket: TJclStrBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;
      PackEntries(Bucket);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrHashMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclStrHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;



//=== { TJclHashMap } ==========================================

constructor TJclHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(nil);
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclHashMap.AssignDataTo(Dest: TJclAbstractContainer);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclBucket;
  ADest: TJclHashMap;
  AMap: IJclMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclHashMap then
  begin
    ADest := TJclHashMap(Dest);
    {$IFDEF THREADSAFE}
    ReadLock;
    try
    {$ENDIF THREADSAFE}
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclBucket.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    {$IFDEF THREADSAFE}
    finally
      ReadUnlock;
    end;
    {$ENDIF THREADSAFE}
  end
  else
  if Supports(Dest, IJclMap, AMap) then
  begin
    AMap.Clear;
    AMap.PutAll(Self);
  end;
end;

procedure TJclHashMap.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclHashMap then
    TJclHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.ContainsKey(Key: TObject): Boolean;
var
  I: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclHashMap.Create(FSize, False, False);
  AssignPropertiesTo(Result);
end;

function TJclHashMap.Equals(const AMap: IJclMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

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

function TJclHashMap.GetOwnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;

function TJclHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclHashMap.GetValue(Key: TObject): TObject;
var
  I: Integer;
  Bucket: TJclBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.GrowEntries(Bucket: TJclBucket);
var
  BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  case FAutoGrowStrategy of
    agsDisabled: ;
    agsAgressive:
      Inc(BucketCapacity, 1);
    agsProportional:
      Inc(BucketCapacity, BucketCapacity div FAutoGrowParameter);
    agsIncremental:
      Inc(BucketCapacity, FAutoGrowParameter);
  end;
  SetLength(Bucket.Entries, BucketCapacity);
end;

function TJclHashMap.Hash(AObject: TObject): Integer;
begin
  Result := Integer(AObject);
end;

function TJclHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclHashMap.KeyOfValue(Value: TObject): TObject;
var
  I, J: Integer;
  Bucket: TJclBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.KeysEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclHashMap.KeySet: IJclSet;
var
  I, J: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArraySet.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.MoveArray(var List: TJclEntryArray; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
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
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := nil;
        List[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclHashMap.Pack;
var
  I: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.PackEntries(Bucket: TJclBucket);
var
  Decrement, BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  case FAutoPackStrategy of
    apsDisabled:
      Decrement := 0;
    apsAgressive:
      Decrement := 1;
    apsProportional:
      Decrement := BucketCapacity div FAutoPackParameter;
    apsIncremental:
      Decrement := FAutoPackParameter;
  else
    Decrement := 0;
  end;
  if (Decrement > 0) and ((Bucket.Size + Decrement) <= BucketCapacity) then
    SetLength(Bucket.Entries, Bucket.Size);
end;

procedure TJclHashMap.PutAll(const AMap: IJclMap);
var
  It: IJclIterator;
  Key: TObject;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.PutValue(Key: TObject; Value: TObject);
var
  Index: Integer;
  Bucket: TJclBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, nil)) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        GrowEntries(Bucket);
      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.Remove(Key: TObject): TObject;
var
  Bucket: TJclBucket;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;
      PackEntries(Bucket);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;


{$IFDEF SUPPORTS_GENERICS}


//=== { TJclHashMap<TKey, TValue> } ==========================================

constructor TJclHashMap<TKey, TValue>.Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(nil);
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclHashMap<TKey, TValue>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclHashMap<TKey, TValue>.AssignDataTo(Dest: TJclAbstractContainer);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclBucket<TKey, TValue>;
  ADest: TJclHashMap<TKey, TValue>;
  AMap: IJclMap<TKey, TValue>;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclHashMap<TKey, TValue> then
  begin
    ADest := TJclHashMap<TKey, TValue>(Dest);
    {$IFDEF THREADSAFE}
    ReadLock;
    try
    {$ENDIF THREADSAFE}
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclBucket<TKey, TValue>.Create;
          SetLength(NewBucket.Entries, SelfBucket.Size);
          for J := 0 to SelfBucket.Size - 1 do
          begin
            NewBucket.Entries[J].Key := SelfBucket.Entries[J].Key;
            NewBucket.Entries[J].Value := SelfBucket.Entries[J].Value;
          end;
          NewBucket.Size := SelfBucket.Size;
          ADest.FBuckets[I] := NewBucket;
        end;
      end;
    {$IFDEF THREADSAFE}
    finally
      ReadUnlock;
    end;
    {$ENDIF THREADSAFE}
  end
  else
  if Supports(Dest, IJclMap<TKey, TValue>, AMap) then
  begin
    AMap.Clear;
    AMap.PutAll(Self);
  end;
end;

procedure TJclHashMap<TKey, TValue>.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclHashMap<TKey, TValue> then
    TJclHashMap<TKey, TValue>(Dest).HashFunction := HashFunction;
end;

procedure TJclHashMap<TKey, TValue>.Clear;
var
  I, J: Integer;
  Bucket: TJclBucket<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        for J := 0 to Bucket.Size - 1 do
        begin
          FreeKey(Bucket.Entries[J].Key);
          FreeValue(Bucket.Entries[J].Value);
        end;
        FreeAndNil(FBuckets[I]);
      end;
    end;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.ContainsKey(const Key: TKey): Boolean;
var
  I: Integer;
  Bucket: TJclBucket<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := True;
          Break;
        end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.ContainsValue(const Value: TValue): Boolean;
var
  I, J: Integer;
  Bucket: TJclBucket<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := True;
            Break;
          end;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.Equals(const AMap: IJclMap<TKey, TValue>): Boolean;
var
  I, J: Integer;
  Bucket: TJclBucket<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if AMap = nil then
      Exit;
    if FSize <> AMap.Size then
      Exit;
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          if AMap.ContainsKey(Bucket.Entries[J].Key) then
          begin
            if not ValuesEqual(AMap.GetValue(Bucket.Entries[J].Key), Bucket.Entries[J].Value) then
              Exit;
          end
          else
            Exit;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

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

function TJclHashMap<TKey, TValue>.GetOwnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;

function TJclHashMap<TKey, TValue>.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclHashMap<TKey, TValue>.GetValue(const Key: TKey): TValue;
var
  I: Integer;
  Bucket: TJclBucket<TKey, TValue>;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := Default(TValue);
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := Bucket.Entries[I].Value;
          Found := True;
          Break;
        end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.GrowEntries(Bucket: TJclBucket<TKey, TValue>);
var
  BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  case FAutoGrowStrategy of
    agsDisabled: ;
    agsAgressive:
      Inc(BucketCapacity, 1);
    agsProportional:
      Inc(BucketCapacity, BucketCapacity div FAutoGrowParameter);
    agsIncremental:
      Inc(BucketCapacity, FAutoGrowParameter);
  end;
  SetLength(Bucket.Entries, BucketCapacity);
end;

function TJclHashMap<TKey, TValue>.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclHashMap<TKey, TValue>.KeyOfValue(const Value: TValue): TKey;
var
  I, J: Integer;
  Bucket: TJclBucket<TKey, TValue>;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := Default(TKey);
    for J := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[J];
      if Bucket <> nil then
        for I := 0 to Bucket.Size - 1 do
          if ValuesEqual(Bucket.Entries[I].Value, Value) then
          begin
            Result := Bucket.Entries[I].Key;
            Found := True;
            Break;
          end;
    end;
    if (not Found) and (not FReturnDefaultElements) then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.KeySet: IJclSet<TKey>;
var
  I, J: Integer;
  Bucket: TJclBucket<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyArraySet(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.MoveArray(var List: TJclEntryArray<TKey, TValue>; FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
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
      begin
        List[FromIndex + I].Key := Default(TKey);
        List[FromIndex + I].Value := Default(TValue);
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := Default(TKey);
        List[FromIndex + I].Value := Default(TValue);
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      List[ToIndex + I] := List[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        List[FromIndex + I].Key := Default(TKey);
        List[FromIndex + I].Value := Default(TValue);
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        List[FromIndex + I].Key := Default(TKey);
        List[FromIndex + I].Value := Default(TValue);
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(List[FromIndex], List[ToIndex], Count * SizeOf(List[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(List[FromIndex], (ToIndex - FromIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(List[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(List[0]), 0)
      else
        FillChar(List[FromIndex], Count * SizeOf(List[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclHashMap<TKey, TValue>.Pack;
var
  I: Integer;
  Bucket: TJclBucket<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
      begin
        if Bucket.Size > 0 then
          SetLength(Bucket.Entries, Bucket.Size)
        else
          FreeAndNil(FBuckets[I]);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.PackEntries(Bucket: TJclBucket<TKey, TValue>);
var
  Decrement, BucketCapacity: Integer;
begin
  BucketCapacity := Length(Bucket.Entries);
  case FAutoPackStrategy of
    apsDisabled:
      Decrement := 0;
    apsAgressive:
      Decrement := 1;
    apsProportional:
      Decrement := BucketCapacity div FAutoPackParameter;
    apsIncremental:
      Decrement := FAutoPackParameter;
  else
    Decrement := 0;
  end;
  if (Decrement > 0) and ((Bucket.Size + Decrement) <= BucketCapacity) then
    SetLength(Bucket.Entries, Bucket.Size);
end;

procedure TJclHashMap<TKey, TValue>.PutAll(const AMap: IJclMap<TKey, TValue>);
var
  It: IJclIterator<TKey>;
  Key: TKey;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if AMap = nil then
      Exit;
    It := AMap.KeySet.First;
    while It.HasNext do
    begin
      Key := It.Next;
      PutValue(Key, AMap.GetValue(Key));
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.PutValue(const Key: TKey; const Value: TValue);
var
  Index: Integer;
  Bucket: TJclBucket<TKey, TValue>;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, Default(TKey)) and not ValuesEqual(Value, Default(TValue))) then
    begin
      Index := FHashFunction(Hash(Key), FCapacity);
      Bucket := FBuckets[Index];
      if Bucket <> nil then
      begin
        for I := 0 to Bucket.Size - 1 do
          if KeysEqual(Bucket.Entries[I].Key, Key) then
          begin
            FreeValue(Bucket.Entries[I].Value);
            Bucket.Entries[I].Value := Value;
            Exit;
          end;
      end
      else
      begin
        Bucket := TJclBucket<TKey, TValue>.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        GrowEntries(Bucket);
      if Bucket.Size < Length(Bucket.Entries) then
      begin
        Bucket.Entries[Bucket.Size].Key := Key;
        Bucket.Entries[Bucket.Size].Value := Value;
        Inc(Bucket.Size);
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.Remove(const Key: TKey): TValue;
var
  Bucket: TJclBucket<TKey, TValue>;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(TValue);
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            MoveArray(Bucket.Entries, I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;
      PackEntries(Bucket);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize = 0 then
    begin
      SetLength(FBuckets, Value);
      inherited SetCapacity(Value);
    end
    else
      raise EJclOperationNotSupportedError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.Size: Integer;
begin
  Result := FSize;
end;

function TJclHashMap<TKey, TValue>.Values: IJclCollection<TValue>;
var
  I, J: Integer;
  Bucket: TJclBucket<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyArrayList(FSize, False);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;


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

