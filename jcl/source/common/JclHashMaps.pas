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

  TJclIntfIntfHashMap = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, 
    IJclIntfIntfMap)
  private
    FBuckets: TJclIntfIntfBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
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
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
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


  TJclAnsiStrIntfEntry = record
    Key: AnsiString;
    Value: IInterface;
  end;

  TJclAnsiStrIntfEntryArray = array of TJclAnsiStrIntfEntry;

  TJclAnsiStrIntfBucket = class
  public
    Size: Integer;
    Entries: TJclAnsiStrIntfEntryArray;
  end;

  TJclAnsiStrIntfBucketArray = array of TJclAnsiStrIntfBucket;

  TJclAnsiStrIntfHashMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,  IJclStrContainer, IJclAnsiStrContainer,
    IJclAnsiStrIntfMap)
  private
    FBuckets: TJclAnsiStrIntfBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclAnsiStrIntfEntryArray; FromIndex, ToIndex, Count: Integer);
    procedure GrowEntries(Bucket: TJclAnsiStrIntfBucket); virtual;
    procedure PackEntries(Bucket: TJclAnsiStrIntfBucket); virtual;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclAnsiStrIntfMap }
    procedure Clear;
    function ContainsKey(const Key: AnsiString): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclAnsiStrIntfMap): Boolean;
    function GetValue(const Key: AnsiString): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): AnsiString;
    function KeySet: IJclAnsiStrSet;
    procedure PutAll(const AMap: IJclAnsiStrIntfMap);
    procedure PutValue(const Key: AnsiString; const Value: IInterface);
    function Remove(const Key: AnsiString): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A, B: AnsiString): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;


  TJclIntfAnsiStrEntry = record
    Key: IInterface;
    Value: AnsiString;
  end;

  TJclIntfAnsiStrEntryArray = array of TJclIntfAnsiStrEntry;

  TJclIntfAnsiStrBucket = class
  public
    Size: Integer;
    Entries: TJclIntfAnsiStrEntryArray;
  end;

  TJclIntfAnsiStrBucketArray = array of TJclIntfAnsiStrBucket;

  TJclIntfAnsiStrHashMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,  IJclStrContainer, IJclAnsiStrContainer,
    IJclIntfAnsiStrMap)
  private
    FBuckets: TJclIntfAnsiStrBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclIntfAnsiStrEntryArray; FromIndex, ToIndex, Count: Integer);
    procedure GrowEntries(Bucket: TJclIntfAnsiStrBucket); virtual;
    procedure PackEntries(Bucket: TJclIntfAnsiStrBucket); virtual;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfAnsiStrMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: AnsiString): Boolean;
    function Equals(const AMap: IJclIntfAnsiStrMap): Boolean;
    function GetValue(const Key: IInterface): AnsiString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: AnsiString): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfAnsiStrMap);
    procedure PutValue(const Key: IInterface; const Value: AnsiString);
    function Remove(const Key: IInterface): AnsiString;
    function Size: Integer;
    function Values: IJclAnsiStrCollection;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: AnsiString): AnsiString; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function Hash(const AInterface: IInterface): Integer; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A, B: AnsiString): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;


  TJclAnsiStrAnsiStrEntry = record
    Key: AnsiString;
    Value: AnsiString;
  end;

  TJclAnsiStrAnsiStrEntryArray = array of TJclAnsiStrAnsiStrEntry;

  TJclAnsiStrAnsiStrBucket = class
  public
    Size: Integer;
    Entries: TJclAnsiStrAnsiStrEntryArray;
  end;

  TJclAnsiStrAnsiStrBucketArray = array of TJclAnsiStrAnsiStrBucket;

  TJclAnsiStrAnsiStrHashMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,  IJclStrContainer, IJclAnsiStrContainer,
    IJclAnsiStrAnsiStrMap)
  private
    FBuckets: TJclAnsiStrAnsiStrBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclAnsiStrAnsiStrEntryArray; FromIndex, ToIndex, Count: Integer);
    procedure GrowEntries(Bucket: TJclAnsiStrAnsiStrBucket); virtual;
    procedure PackEntries(Bucket: TJclAnsiStrAnsiStrBucket); virtual;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclAnsiStrAnsiStrMap }
    procedure Clear;
    function ContainsKey(const Key: AnsiString): Boolean;
    function ContainsValue(const Value: AnsiString): Boolean;
    function Equals(const AMap: IJclAnsiStrAnsiStrMap): Boolean;
    function GetValue(const Key: AnsiString): AnsiString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: AnsiString): AnsiString;
    function KeySet: IJclAnsiStrSet;
    procedure PutAll(const AMap: IJclAnsiStrAnsiStrMap);
    procedure PutValue(const Key: AnsiString; const Value: AnsiString);
    function Remove(const Key: AnsiString): AnsiString;
    function Size: Integer;
    function Values: IJclAnsiStrCollection;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: AnsiString): AnsiString; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A, B: AnsiString): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A, B: AnsiString): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;


  TJclWideStrIntfEntry = record
    Key: WideString;
    Value: IInterface;
  end;

  TJclWideStrIntfEntryArray = array of TJclWideStrIntfEntry;

  TJclWideStrIntfBucket = class
  public
    Size: Integer;
    Entries: TJclWideStrIntfEntryArray;
  end;

  TJclWideStrIntfBucketArray = array of TJclWideStrIntfBucket;

  TJclWideStrIntfHashMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,  IJclStrContainer, IJclWideStrContainer,
    IJclWideStrIntfMap)
  private
    FBuckets: TJclWideStrIntfBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclWideStrIntfEntryArray; FromIndex, ToIndex, Count: Integer);
    procedure GrowEntries(Bucket: TJclWideStrIntfBucket); virtual;
    procedure PackEntries(Bucket: TJclWideStrIntfBucket); virtual;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclWideStrIntfMap }
    procedure Clear;
    function ContainsKey(const Key: WideString): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function Equals(const AMap: IJclWideStrIntfMap): Boolean;
    function GetValue(const Key: WideString): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): WideString;
    function KeySet: IJclWideStrSet;
    procedure PutAll(const AMap: IJclWideStrIntfMap);
    procedure PutValue(const Key: WideString; const Value: IInterface);
    function Remove(const Key: WideString): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A, B: WideString): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;


  TJclIntfWideStrEntry = record
    Key: IInterface;
    Value: WideString;
  end;

  TJclIntfWideStrEntryArray = array of TJclIntfWideStrEntry;

  TJclIntfWideStrBucket = class
  public
    Size: Integer;
    Entries: TJclIntfWideStrEntryArray;
  end;

  TJclIntfWideStrBucketArray = array of TJclIntfWideStrBucket;

  TJclIntfWideStrHashMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,  IJclStrContainer, IJclWideStrContainer,
    IJclIntfWideStrMap)
  private
    FBuckets: TJclIntfWideStrBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclIntfWideStrEntryArray; FromIndex, ToIndex, Count: Integer);
    procedure GrowEntries(Bucket: TJclIntfWideStrBucket); virtual;
    procedure PackEntries(Bucket: TJclIntfWideStrBucket); virtual;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfWideStrMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: WideString): Boolean;
    function Equals(const AMap: IJclIntfWideStrMap): Boolean;
    function GetValue(const Key: IInterface): WideString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: WideString): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfWideStrMap);
    procedure PutValue(const Key: IInterface; const Value: WideString);
    function Remove(const Key: IInterface): WideString;
    function Size: Integer;
    function Values: IJclWideStrCollection;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: WideString): WideString; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function Hash(const AInterface: IInterface): Integer; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A, B: IInterface): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A, B: WideString): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;


  TJclWideStrWideStrEntry = record
    Key: WideString;
    Value: WideString;
  end;

  TJclWideStrWideStrEntryArray = array of TJclWideStrWideStrEntry;

  TJclWideStrWideStrBucket = class
  public
    Size: Integer;
    Entries: TJclWideStrWideStrEntryArray;
  end;

  TJclWideStrWideStrBucketArray = array of TJclWideStrWideStrBucket;

  TJclWideStrWideStrHashMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,  IJclStrContainer, IJclWideStrContainer,
    IJclWideStrWideStrMap)
  private
    FBuckets: TJclWideStrWideStrBucketArray;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclWideStrWideStrEntryArray; FromIndex, ToIndex, Count: Integer);
    procedure GrowEntries(Bucket: TJclWideStrWideStrBucket); virtual;
    procedure PackEntries(Bucket: TJclWideStrWideStrBucket); virtual;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclWideStrWideStrMap }
    procedure Clear;
    function ContainsKey(const Key: WideString): Boolean;
    function ContainsValue(const Value: WideString): Boolean;
    function Equals(const AMap: IJclWideStrWideStrMap): Boolean;
    function GetValue(const Key: WideString): WideString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: WideString): WideString;
    function KeySet: IJclWideStrSet;
    procedure PutAll(const AMap: IJclWideStrWideStrMap);
    procedure PutValue(const Key: WideString; const Value: WideString);
    function Remove(const Key: WideString): WideString;
    function Size: Integer;
    function Values: IJclWideStrCollection;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function FreeValue(var Value: WideString): WideString; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A, B: WideString): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(const A, B: WideString): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

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

  TJclIntfHashMap = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,  IJclValueOwner,
    IJclIntfMap)
  private
    FBuckets: TJclIntfBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
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
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
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


  TJclAnsiStrEntry = record
    Key: AnsiString;
    Value: TObject;
  end;

  TJclAnsiStrEntryArray = array of TJclAnsiStrEntry;

  TJclAnsiStrBucket = class
  public
    Size: Integer;
    Entries: TJclAnsiStrEntryArray;
  end;

  TJclAnsiStrBucketArray = array of TJclAnsiStrBucket;

  TJclAnsiStrHashMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,  IJclStrContainer, IJclAnsiStrContainer, IJclValueOwner,
    IJclAnsiStrMap)
  private
    FBuckets: TJclAnsiStrBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclAnsiStrEntryArray; FromIndex, ToIndex, Count: Integer);
    procedure GrowEntries(Bucket: TJclAnsiStrBucket); virtual;
    procedure PackEntries(Bucket: TJclAnsiStrBucket); virtual;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclAnsiStrMap }
    procedure Clear;
    function ContainsKey(const Key: AnsiString): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclAnsiStrMap): Boolean;
    function GetValue(const Key: AnsiString): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): AnsiString;
    function KeySet: IJclAnsiStrSet;
    procedure PutAll(const AMap: IJclAnsiStrMap);
    procedure PutValue(const Key: AnsiString; Value: TObject);
    function Remove(const Key: AnsiString): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A, B: AnsiString): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(A, B: TObject): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsValues: Boolean read FOwnsValues;
  end;


  TJclWideStrEntry = record
    Key: WideString;
    Value: TObject;
  end;

  TJclWideStrEntryArray = array of TJclWideStrEntry;

  TJclWideStrBucket = class
  public
    Size: Integer;
    Entries: TJclWideStrEntryArray;
  end;

  TJclWideStrBucketArray = array of TJclWideStrBucket;

  TJclWideStrHashMap = class(TJclwideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,  IJclStrContainer, IJclWideStrContainer, IJclValueOwner,
    IJclWideStrMap)
  private
    FBuckets: TJclWideStrBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(var List: TJclWideStrEntryArray; FromIndex, ToIndex, Count: Integer);
    procedure GrowEntries(Bucket: TJclWideStrBucket); virtual;
    procedure PackEntries(Bucket: TJclWideStrBucket); virtual;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclWideStrMap }
    procedure Clear;
    function ContainsKey(const Key: WideString): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(const AMap: IJclWideStrMap): Boolean;
    function GetValue(const Key: WideString): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): WideString;
    function KeySet: IJclWideStrSet;
    procedure PutAll(const AMap: IJclWideStrMap);
    procedure PutValue(const Key: WideString; Value: TObject);
    function Remove(const Key: WideString): TObject;
    function Size: Integer;
    function Values: IJclCollection;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function KeysEqual(const A, B: WideString): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
    function ValuesEqual(A, B: TObject): Boolean; {$IFDEF SUPPORTS_GENERICS}inline;{$ENDIF SUPPORTS_GENERICS}
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrHashMap = TJclAnsiStrHashMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrHashMap = TJclWideStrHashMap;
  {$ENDIF CONTAINER_WIDESTR}


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

  TJclHashMap = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,  IJclKeyOwner, IJclValueOwner,
    IJclMap)
  private
    FBuckets: TJclBucketArray;
    FHashFunction: TJclHashFunction;
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
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
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
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

  TJclHashMap<TKey,TValue> = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,  IJclPairOwner<TKey, TValue>,
    IJclMap<TKey,TValue>)
  private
    FBuckets: TJclBucketArray<TKey,TValue>;
    FHashFunction: TJclHashFunction;
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
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

procedure TJclIntfIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
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
  if Supports(IInterface(Dest), IJclIntfIntfMap, AMap) then
  begin
    AMap.Clear;
    AMap.PutAll(Self);
  end;
end;

procedure TJclIntfIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
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

function TJclIntfIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
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



//=== { TJclAnsiStrIntfHashMap } ==========================================

constructor TJclAnsiStrIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclAnsiStrIntfHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclAnsiStrIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclAnsiStrIntfBucket;
  ADest: TJclAnsiStrIntfHashMap;
  AMap: IJclAnsiStrIntfMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrIntfHashMap then
  begin
    ADest := TJclAnsiStrIntfHashMap(Dest);
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
          NewBucket := TJclAnsiStrIntfBucket.Create;
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
  if Supports(IInterface(Dest), IJclAnsiStrIntfMap, AMap) then
  begin
    AMap.Clear;
    AMap.PutAll(Self);
  end;
end;

procedure TJclAnsiStrIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclAnsiStrIntfHashMap then
    TJclAnsiStrIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclAnsiStrIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfBucket;
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

function TJclAnsiStrIntfHashMap.ContainsKey(const Key: AnsiString): Boolean;
var
  I: Integer;
  Bucket: TJclAnsiStrIntfBucket;
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

function TJclAnsiStrIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfBucket;
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

function TJclAnsiStrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrIntfHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrIntfHashMap.Equals(const AMap: IJclAnsiStrIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfBucket;
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

function TJclAnsiStrIntfHashMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

function TJclAnsiStrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclAnsiStrIntfHashMap.GetValue(const Key: AnsiString): IInterface;
var
  I: Integer;
  Bucket: TJclAnsiStrIntfBucket;
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

procedure TJclAnsiStrIntfHashMap.GrowEntries(Bucket: TJclAnsiStrIntfBucket);
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


function TJclAnsiStrIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrIntfHashMap.KeyOfValue(const Value: IInterface): AnsiString;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfBucket;
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

function TJclAnsiStrIntfHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclAnsiStrIntfHashMap.KeySet: IJclAnsiStrSet;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArraySet.Create(FSize);
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

procedure TJclAnsiStrIntfHashMap.MoveArray(var List: TJclAnsiStrIntfEntryArray; FromIndex, ToIndex, Count: Integer);
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

procedure TJclAnsiStrIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclAnsiStrIntfBucket;
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

procedure TJclAnsiStrIntfHashMap.PackEntries(Bucket: TJclAnsiStrIntfBucket);
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

procedure TJclAnsiStrIntfHashMap.PutAll(const AMap: IJclAnsiStrIntfMap);
var
  It: IJclAnsiStrIterator;
  Key: AnsiString;
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

procedure TJclAnsiStrIntfHashMap.PutValue(const Key: AnsiString; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclAnsiStrIntfBucket;
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
        Bucket := TJclAnsiStrIntfBucket.Create;
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

function TJclAnsiStrIntfHashMap.Remove(const Key: AnsiString): IInterface;
var
  Bucket: TJclAnsiStrIntfBucket;
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

procedure TJclAnsiStrIntfHashMap.SetCapacity(Value: Integer);
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

function TJclAnsiStrIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfBucket;
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

function TJclAnsiStrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;



//=== { TJclIntfAnsiStrHashMap } ==========================================

constructor TJclIntfAnsiStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfAnsiStrHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclIntfAnsiStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfAnsiStrBucket;
  ADest: TJclIntfAnsiStrHashMap;
  AMap: IJclIntfAnsiStrMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfAnsiStrHashMap then
  begin
    ADest := TJclIntfAnsiStrHashMap(Dest);
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
          NewBucket := TJclIntfAnsiStrBucket.Create;
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
  if Supports(IInterface(Dest), IJclIntfAnsiStrMap, AMap) then
  begin
    AMap.Clear;
    AMap.PutAll(Self);
  end;
end;

procedure TJclIntfAnsiStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfAnsiStrHashMap then
    TJclIntfAnsiStrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfAnsiStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrBucket;
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

function TJclIntfAnsiStrHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfAnsiStrBucket;
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

function TJclIntfAnsiStrHashMap.ContainsValue(const Value: AnsiString): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrBucket;
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

function TJclIntfAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfAnsiStrHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfAnsiStrHashMap.Equals(const AMap: IJclIntfAnsiStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrBucket;
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

function TJclIntfAnsiStrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfAnsiStrHashMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;



function TJclIntfAnsiStrHashMap.GetValue(const Key: IInterface): AnsiString;
var
  I: Integer;
  Bucket: TJclIntfAnsiStrBucket;
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

procedure TJclIntfAnsiStrHashMap.GrowEntries(Bucket: TJclIntfAnsiStrBucket);
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

function TJclIntfAnsiStrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfAnsiStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfAnsiStrHashMap.KeyOfValue(const Value: AnsiString): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrBucket;
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

function TJclIntfAnsiStrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfAnsiStrHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrBucket;
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

procedure TJclIntfAnsiStrHashMap.MoveArray(var List: TJclIntfAnsiStrEntryArray; FromIndex, ToIndex, Count: Integer);
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

procedure TJclIntfAnsiStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfAnsiStrBucket;
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

procedure TJclIntfAnsiStrHashMap.PackEntries(Bucket: TJclIntfAnsiStrBucket);
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

procedure TJclIntfAnsiStrHashMap.PutAll(const AMap: IJclIntfAnsiStrMap);
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

procedure TJclIntfAnsiStrHashMap.PutValue(const Key: IInterface; const Value: AnsiString);
var
  Index: Integer;
  Bucket: TJclIntfAnsiStrBucket;
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
        Bucket := TJclIntfAnsiStrBucket.Create;
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

function TJclIntfAnsiStrHashMap.Remove(const Key: IInterface): AnsiString;
var
  Bucket: TJclIntfAnsiStrBucket;
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

procedure TJclIntfAnsiStrHashMap.SetCapacity(Value: Integer);
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

function TJclIntfAnsiStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfAnsiStrHashMap.Values: IJclAnsiStrCollection;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArrayList.Create(FSize);
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

function TJclIntfAnsiStrHashMap.ValuesEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;



//=== { TJclAnsiStrAnsiStrHashMap } ==========================================

constructor TJclAnsiStrAnsiStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclAnsiStrAnsiStrHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclAnsiStrAnsiStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclAnsiStrAnsiStrBucket;
  ADest: TJclAnsiStrAnsiStrHashMap;
  AMap: IJclAnsiStrAnsiStrMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrAnsiStrHashMap then
  begin
    ADest := TJclAnsiStrAnsiStrHashMap(Dest);
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
          NewBucket := TJclAnsiStrAnsiStrBucket.Create;
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
  if Supports(IInterface(Dest), IJclAnsiStrAnsiStrMap, AMap) then
  begin
    AMap.Clear;
    AMap.PutAll(Self);
  end;
end;

procedure TJclAnsiStrAnsiStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclAnsiStrAnsiStrHashMap then
    TJclAnsiStrAnsiStrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclAnsiStrAnsiStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
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

function TJclAnsiStrAnsiStrHashMap.ContainsKey(const Key: AnsiString): Boolean;
var
  I: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
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

function TJclAnsiStrAnsiStrHashMap.ContainsValue(const Value: AnsiString): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
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

function TJclAnsiStrAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrAnsiStrHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrAnsiStrHashMap.Equals(const AMap: IJclAnsiStrAnsiStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
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

function TJclAnsiStrAnsiStrHashMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

function TJclAnsiStrAnsiStrHashMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;



function TJclAnsiStrAnsiStrHashMap.GetValue(const Key: AnsiString): AnsiString;
var
  I: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
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

procedure TJclAnsiStrAnsiStrHashMap.GrowEntries(Bucket: TJclAnsiStrAnsiStrBucket);
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


function TJclAnsiStrAnsiStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrAnsiStrHashMap.KeyOfValue(const Value: AnsiString): AnsiString;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
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

function TJclAnsiStrAnsiStrHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclAnsiStrAnsiStrHashMap.KeySet: IJclAnsiStrSet;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArraySet.Create(FSize);
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

procedure TJclAnsiStrAnsiStrHashMap.MoveArray(var List: TJclAnsiStrAnsiStrEntryArray; FromIndex, ToIndex, Count: Integer);
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

procedure TJclAnsiStrAnsiStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
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

procedure TJclAnsiStrAnsiStrHashMap.PackEntries(Bucket: TJclAnsiStrAnsiStrBucket);
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

procedure TJclAnsiStrAnsiStrHashMap.PutAll(const AMap: IJclAnsiStrAnsiStrMap);
var
  It: IJclAnsiStrIterator;
  Key: AnsiString;
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

procedure TJclAnsiStrAnsiStrHashMap.PutValue(const Key: AnsiString; const Value: AnsiString);
var
  Index: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
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
        Bucket := TJclAnsiStrAnsiStrBucket.Create;
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

function TJclAnsiStrAnsiStrHashMap.Remove(const Key: AnsiString): AnsiString;
var
  Bucket: TJclAnsiStrAnsiStrBucket;
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

procedure TJclAnsiStrAnsiStrHashMap.SetCapacity(Value: Integer);
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

function TJclAnsiStrAnsiStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrAnsiStrHashMap.Values: IJclAnsiStrCollection;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArrayList.Create(FSize);
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

function TJclAnsiStrAnsiStrHashMap.ValuesEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;



//=== { TJclWideStrIntfHashMap } ==========================================

constructor TJclWideStrIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclWideStrIntfHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclWideStrIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclWideStrIntfBucket;
  ADest: TJclWideStrIntfHashMap;
  AMap: IJclWideStrIntfMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrIntfHashMap then
  begin
    ADest := TJclWideStrIntfHashMap(Dest);
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
          NewBucket := TJclWideStrIntfBucket.Create;
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
  if Supports(IInterface(Dest), IJclWideStrIntfMap, AMap) then
  begin
    AMap.Clear;
    AMap.PutAll(Self);
  end;
end;

procedure TJclWideStrIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclWideStrIntfHashMap then
    TJclWideStrIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclWideStrIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfBucket;
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

function TJclWideStrIntfHashMap.ContainsKey(const Key: WideString): Boolean;
var
  I: Integer;
  Bucket: TJclWideStrIntfBucket;
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

function TJclWideStrIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfBucket;
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

function TJclWideStrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrIntfHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclWideStrIntfHashMap.Equals(const AMap: IJclWideStrIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfBucket;
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

function TJclWideStrIntfHashMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

function TJclWideStrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclWideStrIntfHashMap.GetValue(const Key: WideString): IInterface;
var
  I: Integer;
  Bucket: TJclWideStrIntfBucket;
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

procedure TJclWideStrIntfHashMap.GrowEntries(Bucket: TJclWideStrIntfBucket);
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


function TJclWideStrIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrIntfHashMap.KeyOfValue(const Value: IInterface): WideString;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfBucket;
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

function TJclWideStrIntfHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclWideStrIntfHashMap.KeySet: IJclWideStrSet;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArraySet.Create(FSize);
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

procedure TJclWideStrIntfHashMap.MoveArray(var List: TJclWideStrIntfEntryArray; FromIndex, ToIndex, Count: Integer);
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

procedure TJclWideStrIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclWideStrIntfBucket;
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

procedure TJclWideStrIntfHashMap.PackEntries(Bucket: TJclWideStrIntfBucket);
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

procedure TJclWideStrIntfHashMap.PutAll(const AMap: IJclWideStrIntfMap);
var
  It: IJclWideStrIterator;
  Key: WideString;
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

procedure TJclWideStrIntfHashMap.PutValue(const Key: WideString; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclWideStrIntfBucket;
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
        Bucket := TJclWideStrIntfBucket.Create;
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

function TJclWideStrIntfHashMap.Remove(const Key: WideString): IInterface;
var
  Bucket: TJclWideStrIntfBucket;
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

procedure TJclWideStrIntfHashMap.SetCapacity(Value: Integer);
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

function TJclWideStrIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfBucket;
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

function TJclWideStrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;



//=== { TJclIntfWideStrHashMap } ==========================================

constructor TJclIntfWideStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfWideStrHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclIntfWideStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfWideStrBucket;
  ADest: TJclIntfWideStrHashMap;
  AMap: IJclIntfWideStrMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfWideStrHashMap then
  begin
    ADest := TJclIntfWideStrHashMap(Dest);
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
          NewBucket := TJclIntfWideStrBucket.Create;
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
  if Supports(IInterface(Dest), IJclIntfWideStrMap, AMap) then
  begin
    AMap.Clear;
    AMap.PutAll(Self);
  end;
end;

procedure TJclIntfWideStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfWideStrHashMap then
    TJclIntfWideStrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfWideStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrBucket;
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

function TJclIntfWideStrHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfWideStrBucket;
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

function TJclIntfWideStrHashMap.ContainsValue(const Value: WideString): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrBucket;
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

function TJclIntfWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfWideStrHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfWideStrHashMap.Equals(const AMap: IJclIntfWideStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrBucket;
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

function TJclIntfWideStrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfWideStrHashMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;



function TJclIntfWideStrHashMap.GetValue(const Key: IInterface): WideString;
var
  I: Integer;
  Bucket: TJclIntfWideStrBucket;
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

procedure TJclIntfWideStrHashMap.GrowEntries(Bucket: TJclIntfWideStrBucket);
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

function TJclIntfWideStrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfWideStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfWideStrHashMap.KeyOfValue(const Value: WideString): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrBucket;
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

function TJclIntfWideStrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfWideStrHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrBucket;
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

procedure TJclIntfWideStrHashMap.MoveArray(var List: TJclIntfWideStrEntryArray; FromIndex, ToIndex, Count: Integer);
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

procedure TJclIntfWideStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfWideStrBucket;
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

procedure TJclIntfWideStrHashMap.PackEntries(Bucket: TJclIntfWideStrBucket);
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

procedure TJclIntfWideStrHashMap.PutAll(const AMap: IJclIntfWideStrMap);
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

procedure TJclIntfWideStrHashMap.PutValue(const Key: IInterface; const Value: WideString);
var
  Index: Integer;
  Bucket: TJclIntfWideStrBucket;
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
        Bucket := TJclIntfWideStrBucket.Create;
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

function TJclIntfWideStrHashMap.Remove(const Key: IInterface): WideString;
var
  Bucket: TJclIntfWideStrBucket;
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

procedure TJclIntfWideStrHashMap.SetCapacity(Value: Integer);
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

function TJclIntfWideStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfWideStrHashMap.Values: IJclWideStrCollection;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArrayList.Create(FSize);
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

function TJclIntfWideStrHashMap.ValuesEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;



//=== { TJclWideStrWideStrHashMap } ==========================================

constructor TJclWideStrWideStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclWideStrWideStrHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclWideStrWideStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclWideStrWideStrBucket;
  ADest: TJclWideStrWideStrHashMap;
  AMap: IJclWideStrWideStrMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrWideStrHashMap then
  begin
    ADest := TJclWideStrWideStrHashMap(Dest);
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
          NewBucket := TJclWideStrWideStrBucket.Create;
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
  if Supports(IInterface(Dest), IJclWideStrWideStrMap, AMap) then
  begin
    AMap.Clear;
    AMap.PutAll(Self);
  end;
end;

procedure TJclWideStrWideStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclWideStrWideStrHashMap then
    TJclWideStrWideStrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclWideStrWideStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrBucket;
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

function TJclWideStrWideStrHashMap.ContainsKey(const Key: WideString): Boolean;
var
  I: Integer;
  Bucket: TJclWideStrWideStrBucket;
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

function TJclWideStrWideStrHashMap.ContainsValue(const Value: WideString): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrBucket;
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

function TJclWideStrWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrWideStrHashMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclWideStrWideStrHashMap.Equals(const AMap: IJclWideStrWideStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrBucket;
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

function TJclWideStrWideStrHashMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

function TJclWideStrWideStrHashMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;



function TJclWideStrWideStrHashMap.GetValue(const Key: WideString): WideString;
var
  I: Integer;
  Bucket: TJclWideStrWideStrBucket;
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

procedure TJclWideStrWideStrHashMap.GrowEntries(Bucket: TJclWideStrWideStrBucket);
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


function TJclWideStrWideStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrWideStrHashMap.KeyOfValue(const Value: WideString): WideString;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrBucket;
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

function TJclWideStrWideStrHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclWideStrWideStrHashMap.KeySet: IJclWideStrSet;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArraySet.Create(FSize);
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

procedure TJclWideStrWideStrHashMap.MoveArray(var List: TJclWideStrWideStrEntryArray; FromIndex, ToIndex, Count: Integer);
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

procedure TJclWideStrWideStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclWideStrWideStrBucket;
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

procedure TJclWideStrWideStrHashMap.PackEntries(Bucket: TJclWideStrWideStrBucket);
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

procedure TJclWideStrWideStrHashMap.PutAll(const AMap: IJclWideStrWideStrMap);
var
  It: IJclWideStrIterator;
  Key: WideString;
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

procedure TJclWideStrWideStrHashMap.PutValue(const Key: WideString; const Value: WideString);
var
  Index: Integer;
  Bucket: TJclWideStrWideStrBucket;
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
        Bucket := TJclWideStrWideStrBucket.Create;
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

function TJclWideStrWideStrHashMap.Remove(const Key: WideString): WideString;
var
  Bucket: TJclWideStrWideStrBucket;
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

procedure TJclWideStrWideStrHashMap.SetCapacity(Value: Integer);
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

function TJclWideStrWideStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrWideStrHashMap.Values: IJclWideStrCollection;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArrayList.Create(FSize);
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

function TJclWideStrWideStrHashMap.ValuesEqual(const A, B: Widestring): Boolean;
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

procedure TJclIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
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
  if Supports(IInterface(Dest), IJclIntfMap, AMap) then
  begin
    AMap.Clear;
    AMap.PutAll(Self);
  end;
end;

procedure TJclIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
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

function TJclIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
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



//=== { TJclAnsiStrHashMap } ==========================================

constructor TJclAnsiStrHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create(nil);
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclAnsiStrHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclAnsiStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclAnsiStrBucket;
  ADest: TJclAnsiStrHashMap;
  AMap: IJclAnsiStrMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrHashMap then
  begin
    ADest := TJclAnsiStrHashMap(Dest);
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
          NewBucket := TJclAnsiStrBucket.Create;
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
  if Supports(IInterface(Dest), IJclAnsiStrMap, AMap) then
  begin
    AMap.Clear;
    AMap.PutAll(Self);
  end;
end;

procedure TJclAnsiStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclAnsiStrHashMap then
    TJclAnsiStrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclAnsiStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclAnsiStrBucket;
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

function TJclAnsiStrHashMap.ContainsKey(const Key: AnsiString): Boolean;
var
  I: Integer;
  Bucket: TJclAnsiStrBucket;
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

function TJclAnsiStrHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrBucket;
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

function TJclAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrHashMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrHashMap.Equals(const AMap: IJclAnsiStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrBucket;
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

function TJclAnsiStrHashMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

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


function TJclAnsiStrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclAnsiStrHashMap.GetValue(const Key: AnsiString): TObject;
var
  I: Integer;
  Bucket: TJclAnsiStrBucket;
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

procedure TJclAnsiStrHashMap.GrowEntries(Bucket: TJclAnsiStrBucket);
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


function TJclAnsiStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrHashMap.KeyOfValue(Value: TObject): AnsiString;
var
  I, J: Integer;
  Bucket: TJclAnsiStrBucket;
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

function TJclAnsiStrHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclAnsiStrHashMap.KeySet: IJclAnsiStrSet;
var
  I, J: Integer;
  Bucket: TJclAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArraySet.Create(FSize);
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

procedure TJclAnsiStrHashMap.MoveArray(var List: TJclAnsiStrEntryArray; FromIndex, ToIndex, Count: Integer);
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

procedure TJclAnsiStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclAnsiStrBucket;
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

procedure TJclAnsiStrHashMap.PackEntries(Bucket: TJclAnsiStrBucket);
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

procedure TJclAnsiStrHashMap.PutAll(const AMap: IJclAnsiStrMap);
var
  It: IJclAnsiStrIterator;
  Key: AnsiString;
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

procedure TJclAnsiStrHashMap.PutValue(const Key: AnsiString; Value: TObject);
var
  Index: Integer;
  Bucket: TJclAnsiStrBucket;
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
        Bucket := TJclAnsiStrBucket.Create;
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

function TJclAnsiStrHashMap.Remove(const Key: AnsiString): TObject;
var
  Bucket: TJclAnsiStrBucket;
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

procedure TJclAnsiStrHashMap.SetCapacity(Value: Integer);
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

function TJclAnsiStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclAnsiStrBucket;
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

function TJclAnsiStrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;



//=== { TJclWideStrHashMap } ==========================================

constructor TJclWideStrHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create(nil);
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclWideStrHashMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclWideStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclWideStrBucket;
  ADest: TJclWideStrHashMap;
  AMap: IJclWideStrMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrHashMap then
  begin
    ADest := TJclWideStrHashMap(Dest);
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
          NewBucket := TJclWideStrBucket.Create;
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
  if Supports(IInterface(Dest), IJclWideStrMap, AMap) then
  begin
    AMap.Clear;
    AMap.PutAll(Self);
  end;
end;

procedure TJclWideStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclWideStrHashMap then
    TJclWideStrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclWideStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclWideStrBucket;
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

function TJclWideStrHashMap.ContainsKey(const Key: WideString): Boolean;
var
  I: Integer;
  Bucket: TJclWideStrBucket;
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

function TJclWideStrHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrBucket;
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

function TJclWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrHashMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclWideStrHashMap.Equals(const AMap: IJclWideStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrBucket;
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

function TJclWideStrHashMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

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


function TJclWideStrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclWideStrHashMap.GetValue(const Key: WideString): TObject;
var
  I: Integer;
  Bucket: TJclWideStrBucket;
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

procedure TJclWideStrHashMap.GrowEntries(Bucket: TJclWideStrBucket);
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


function TJclWideStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrHashMap.KeyOfValue(Value: TObject): WideString;
var
  I, J: Integer;
  Bucket: TJclWideStrBucket;
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

function TJclWideStrHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclWideStrHashMap.KeySet: IJclWideStrSet;
var
  I, J: Integer;
  Bucket: TJclWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArraySet.Create(FSize);
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

procedure TJclWideStrHashMap.MoveArray(var List: TJclWideStrEntryArray; FromIndex, ToIndex, Count: Integer);
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

procedure TJclWideStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclWideStrBucket;
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

procedure TJclWideStrHashMap.PackEntries(Bucket: TJclWideStrBucket);
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

procedure TJclWideStrHashMap.PutAll(const AMap: IJclWideStrMap);
var
  It: IJclWideStrIterator;
  Key: WideString;
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

procedure TJclWideStrHashMap.PutValue(const Key: WideString; Value: TObject);
var
  Index: Integer;
  Bucket: TJclWideStrBucket;
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
        Bucket := TJclWideStrBucket.Create;
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

function TJclWideStrHashMap.Remove(const Key: WideString): TObject;
var
  Bucket: TJclWideStrBucket;
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

procedure TJclWideStrHashMap.SetCapacity(Value: Integer);
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

function TJclWideStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclWideStrBucket;
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

function TJclWideStrHashMap.ValuesEqual(A, B: TObject): Boolean;
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

procedure TJclHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
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
  if Supports(IInterface(Dest), IJclMap, AMap) then
  begin
    AMap.Clear;
    AMap.PutAll(Self);
  end;
end;

procedure TJclHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
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

function TJclHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
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

procedure TJclHashMap<TKey, TValue>.AssignDataTo(Dest: TJclAbstractContainerBase);
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
  if Supports(IInterface(Dest), IJclMap<TKey, TValue>, AMap) then
  begin
    AMap.Clear;
    AMap.PutAll(Self);
  end;
end;

procedure TJclHashMap<TKey, TValue>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
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

