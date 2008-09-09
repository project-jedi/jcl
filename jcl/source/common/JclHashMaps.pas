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
{ Last modified: $Date::                                                                        $ }
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
  JclBase, JclSynch,
  JclContainerIntf, JclAbstractContainers, JclArrayLists, JclArraySets;


type
  // Hash Function
  // Result must be in 0..Range-1
  TJclHashFunction = function(Key, Range: Integer): Integer;

  TJclIntfIntfHashEntry = record
    Key: IInterface;
    Value: IInterface;
  end;

  TJclIntfIntfBucket = class
  public
    Size: Integer;
    Entries: array of TJclIntfIntfHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclIntfIntfHashMap = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntfIntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclIntfIntfBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfIntfMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function MapEquals(const AMap: IJclIntfIntfMap): Boolean;
    function GetValue(const Key: IInterface): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfIntfMap);
    procedure PutValue(const Key: IInterface; const Value: IInterface);
    function Remove(const Key: IInterface): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclAnsiStrIntfHashEntry = record
    Key: AnsiString;
    Value: IInterface;
  end;

  TJclAnsiStrIntfBucket = class
  public
    Size: Integer;
    Entries: array of TJclAnsiStrIntfHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclAnsiStrIntfHashMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer,
    IJclAnsiStrIntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: AnsiString): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclAnsiStrIntfBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclAnsiStrIntfMap }
    procedure Clear;
    function ContainsKey(const Key: AnsiString): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function MapEquals(const AMap: IJclAnsiStrIntfMap): Boolean;
    function GetValue(const Key: AnsiString): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): AnsiString;
    function KeySet: IJclAnsiStrSet;
    procedure PutAll(const AMap: IJclAnsiStrIntfMap);
    procedure PutValue(const Key: AnsiString; const Value: IInterface);
    function Remove(const Key: AnsiString): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntfAnsiStrHashEntry = record
    Key: IInterface;
    Value: AnsiString;
  end;

  TJclIntfAnsiStrBucket = class
  public
    Size: Integer;
    Entries: array of TJclIntfAnsiStrHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclIntfAnsiStrHashMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer,
    IJclIntfAnsiStrMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: AnsiString): AnsiString;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: AnsiString): Boolean;
  private
    FBuckets: array of TJclIntfAnsiStrBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfAnsiStrMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: AnsiString): Boolean;
    function MapEquals(const AMap: IJclIntfAnsiStrMap): Boolean;
    function GetValue(const Key: IInterface): AnsiString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: AnsiString): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfAnsiStrMap);
    procedure PutValue(const Key: IInterface; const Value: AnsiString);
    function Remove(const Key: IInterface): AnsiString;
    function Size: Integer;
    function Values: IJclAnsiStrCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclAnsiStrAnsiStrHashEntry = record
    Key: AnsiString;
    Value: AnsiString;
  end;

  TJclAnsiStrAnsiStrBucket = class
  public
    Size: Integer;
    Entries: array of TJclAnsiStrAnsiStrHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclAnsiStrAnsiStrHashMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer,
    IJclAnsiStrAnsiStrMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function FreeValue(var Value: AnsiString): AnsiString;
    function KeysEqual(const A, B: AnsiString): Boolean;
    function ValuesEqual(const A, B: AnsiString): Boolean;
  private
    FBuckets: array of TJclAnsiStrAnsiStrBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclAnsiStrAnsiStrMap }
    procedure Clear;
    function ContainsKey(const Key: AnsiString): Boolean;
    function ContainsValue(const Value: AnsiString): Boolean;
    function MapEquals(const AMap: IJclAnsiStrAnsiStrMap): Boolean;
    function GetValue(const Key: AnsiString): AnsiString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: AnsiString): AnsiString;
    function KeySet: IJclAnsiStrSet;
    procedure PutAll(const AMap: IJclAnsiStrAnsiStrMap);
    procedure PutValue(const Key: AnsiString; const Value: AnsiString);
    function Remove(const Key: AnsiString): AnsiString;
    function Size: Integer;
    function Values: IJclAnsiStrCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclWideStrIntfHashEntry = record
    Key: WideString;
    Value: IInterface;
  end;

  TJclWideStrIntfBucket = class
  public
    Size: Integer;
    Entries: array of TJclWideStrIntfHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclWideStrIntfHashMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclWideStrContainer,
    IJclWideStrIntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: WideString): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclWideStrIntfBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclWideStrIntfMap }
    procedure Clear;
    function ContainsKey(const Key: WideString): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function MapEquals(const AMap: IJclWideStrIntfMap): Boolean;
    function GetValue(const Key: WideString): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): WideString;
    function KeySet: IJclWideStrSet;
    procedure PutAll(const AMap: IJclWideStrIntfMap);
    procedure PutValue(const Key: WideString; const Value: IInterface);
    function Remove(const Key: WideString): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntfWideStrHashEntry = record
    Key: IInterface;
    Value: WideString;
  end;

  TJclIntfWideStrBucket = class
  public
    Size: Integer;
    Entries: array of TJclIntfWideStrHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclIntfWideStrHashMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclWideStrContainer,
    IJclIntfWideStrMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: WideString): WideString;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: WideString): Boolean;
  private
    FBuckets: array of TJclIntfWideStrBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfWideStrMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: WideString): Boolean;
    function MapEquals(const AMap: IJclIntfWideStrMap): Boolean;
    function GetValue(const Key: IInterface): WideString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: WideString): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfWideStrMap);
    procedure PutValue(const Key: IInterface; const Value: WideString);
    function Remove(const Key: IInterface): WideString;
    function Size: Integer;
    function Values: IJclWideStrCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclWideStrWideStrHashEntry = record
    Key: WideString;
    Value: WideString;
  end;

  TJclWideStrWideStrBucket = class
  public
    Size: Integer;
    Entries: array of TJclWideStrWideStrHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclWideStrWideStrHashMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclWideStrContainer,
    IJclWideStrWideStrMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function FreeValue(var Value: WideString): WideString;
    function KeysEqual(const A, B: WideString): Boolean;
    function ValuesEqual(const A, B: WideString): Boolean;
  private
    FBuckets: array of TJclWideStrWideStrBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclWideStrWideStrMap }
    procedure Clear;
    function ContainsKey(const Key: WideString): Boolean;
    function ContainsValue(const Value: WideString): Boolean;
    function MapEquals(const AMap: IJclWideStrWideStrMap): Boolean;
    function GetValue(const Key: WideString): WideString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: WideString): WideString;
    function KeySet: IJclWideStrSet;
    procedure PutAll(const AMap: IJclWideStrWideStrMap);
    procedure PutValue(const Key: WideString; const Value: WideString);
    function Remove(const Key: WideString): WideString;
    function Size: Integer;
    function Values: IJclWideStrCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

{$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrIntfHashEntry = record
    Key: UnicodeString;
    Value: IInterface;
  end;

  TJclUnicodeStrIntfBucket = class
  public
    Size: Integer;
    Entries: array of TJclUnicodeStrIntfHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclUnicodeStrIntfHashMap = class(TJclUnicodeStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclUnicodeStrContainer,
    IJclUnicodeStrIntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: UnicodeString): UnicodeString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: UnicodeString): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclUnicodeStrIntfBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclUnicodeStrIntfMap }
    procedure Clear;
    function ContainsKey(const Key: UnicodeString): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function MapEquals(const AMap: IJclUnicodeStrIntfMap): Boolean;
    function GetValue(const Key: UnicodeString): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): UnicodeString;
    function KeySet: IJclUnicodeStrSet;
    procedure PutAll(const AMap: IJclUnicodeStrIntfMap);
    procedure PutValue(const Key: UnicodeString; const Value: IInterface);
    function Remove(const Key: UnicodeString): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntfUnicodeStrHashEntry = record
    Key: IInterface;
    Value: UnicodeString;
  end;

  TJclIntfUnicodeStrBucket = class
  public
    Size: Integer;
    Entries: array of TJclIntfUnicodeStrHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclIntfUnicodeStrHashMap = class(TJclUnicodeStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclUnicodeStrContainer,
    IJclIntfUnicodeStrMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: UnicodeString): UnicodeString;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: UnicodeString): Boolean;
  private
    FBuckets: array of TJclIntfUnicodeStrBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfUnicodeStrMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: UnicodeString): Boolean;
    function MapEquals(const AMap: IJclIntfUnicodeStrMap): Boolean;
    function GetValue(const Key: IInterface): UnicodeString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: UnicodeString): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfUnicodeStrMap);
    procedure PutValue(const Key: IInterface; const Value: UnicodeString);
    function Remove(const Key: IInterface): UnicodeString;
    function Size: Integer;
    function Values: IJclUnicodeStrCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclUnicodeStrUnicodeStrHashEntry = record
    Key: UnicodeString;
    Value: UnicodeString;
  end;

  TJclUnicodeStrUnicodeStrBucket = class
  public
    Size: Integer;
    Entries: array of TJclUnicodeStrUnicodeStrHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclUnicodeStrUnicodeStrHashMap = class(TJclUnicodeStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclUnicodeStrContainer,
    IJclUnicodeStrUnicodeStrMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: UnicodeString): UnicodeString;
    function FreeValue(var Value: UnicodeString): UnicodeString;
    function KeysEqual(const A, B: UnicodeString): Boolean;
    function ValuesEqual(const A, B: UnicodeString): Boolean;
  private
    FBuckets: array of TJclUnicodeStrUnicodeStrBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclUnicodeStrUnicodeStrMap }
    procedure Clear;
    function ContainsKey(const Key: UnicodeString): Boolean;
    function ContainsValue(const Value: UnicodeString): Boolean;
    function MapEquals(const AMap: IJclUnicodeStrUnicodeStrMap): Boolean;
    function GetValue(const Key: UnicodeString): UnicodeString;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: UnicodeString): UnicodeString;
    function KeySet: IJclUnicodeStrSet;
    procedure PutAll(const AMap: IJclUnicodeStrUnicodeStrMap);
    procedure PutValue(const Key: UnicodeString; const Value: UnicodeString);
    function Remove(const Key: UnicodeString): UnicodeString;
    function Size: Integer;
    function Values: IJclUnicodeStrCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;
{$ENDIF SUPPORTS_UNICODE_STRING}

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
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrIntfHashMap = TJclUnicodeStrIntfHashMap;
  TJclIntfStrHashMap = TJclIntfUnicodeStrHashMap;
  TJclStrStrHashMap = TJclUnicodeStrUnicodeStrHashMap;
  {$ENDIF CONTAINER_UNICODESTR}

  TJclSingleIntfHashEntry = record
    Key: Single;
    Value: IInterface;
  end;

  TJclSingleIntfBucket = class
  public
    Size: Integer;
    Entries: array of TJclSingleIntfHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclSingleIntfHashMap = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclSingleContainer,
    IJclSingleIntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: Single): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclSingleIntfBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclSingleIntfMap }
    procedure Clear;
    function ContainsKey(const Key: Single): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function MapEquals(const AMap: IJclSingleIntfMap): Boolean;
    function GetValue(const Key: Single): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Single;
    function KeySet: IJclSingleSet;
    procedure PutAll(const AMap: IJclSingleIntfMap);
    procedure PutValue(const Key: Single; const Value: IInterface);
    function Remove(const Key: Single): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntfSingleHashEntry = record
    Key: IInterface;
    Value: Single;
  end;

  TJclIntfSingleBucket = class
  public
    Size: Integer;
    Entries: array of TJclIntfSingleHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclIntfSingleHashMap = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclSingleContainer,
    IJclIntfSingleMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Single): Single;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: Single): Boolean;
  private
    FBuckets: array of TJclIntfSingleBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfSingleMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: Single): Boolean;
    function MapEquals(const AMap: IJclIntfSingleMap): Boolean;
    function GetValue(const Key: IInterface): Single;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Single): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfSingleMap);
    procedure PutValue(const Key: IInterface; const Value: Single);
    function Remove(const Key: IInterface): Single;
    function Size: Integer;
    function Values: IJclSingleCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclSingleSingleHashEntry = record
    Key: Single;
    Value: Single;
  end;

  TJclSingleSingleBucket = class
  public
    Size: Integer;
    Entries: array of TJclSingleSingleHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclSingleSingleHashMap = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclSingleContainer,
    IJclSingleSingleMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function FreeValue(var Value: Single): Single;
    function KeysEqual(const A, B: Single): Boolean;
    function ValuesEqual(const A, B: Single): Boolean;
  private
    FBuckets: array of TJclSingleSingleBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclSingleSingleMap }
    procedure Clear;
    function ContainsKey(const Key: Single): Boolean;
    function ContainsValue(const Value: Single): Boolean;
    function MapEquals(const AMap: IJclSingleSingleMap): Boolean;
    function GetValue(const Key: Single): Single;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Single): Single;
    function KeySet: IJclSingleSet;
    procedure PutAll(const AMap: IJclSingleSingleMap);
    procedure PutValue(const Key: Single; const Value: Single);
    function Remove(const Key: Single): Single;
    function Size: Integer;
    function Values: IJclSingleCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclDoubleIntfHashEntry = record
    Key: Double;
    Value: IInterface;
  end;

  TJclDoubleIntfBucket = class
  public
    Size: Integer;
    Entries: array of TJclDoubleIntfHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclDoubleIntfHashMap = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclDoubleContainer,
    IJclDoubleIntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: Double): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclDoubleIntfBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclDoubleIntfMap }
    procedure Clear;
    function ContainsKey(const Key: Double): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function MapEquals(const AMap: IJclDoubleIntfMap): Boolean;
    function GetValue(const Key: Double): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Double;
    function KeySet: IJclDoubleSet;
    procedure PutAll(const AMap: IJclDoubleIntfMap);
    procedure PutValue(const Key: Double; const Value: IInterface);
    function Remove(const Key: Double): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntfDoubleHashEntry = record
    Key: IInterface;
    Value: Double;
  end;

  TJclIntfDoubleBucket = class
  public
    Size: Integer;
    Entries: array of TJclIntfDoubleHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclIntfDoubleHashMap = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclDoubleContainer,
    IJclIntfDoubleMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Double): Double;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: Double): Boolean;
  private
    FBuckets: array of TJclIntfDoubleBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfDoubleMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: Double): Boolean;
    function MapEquals(const AMap: IJclIntfDoubleMap): Boolean;
    function GetValue(const Key: IInterface): Double;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Double): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfDoubleMap);
    procedure PutValue(const Key: IInterface; const Value: Double);
    function Remove(const Key: IInterface): Double;
    function Size: Integer;
    function Values: IJclDoubleCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclDoubleDoubleHashEntry = record
    Key: Double;
    Value: Double;
  end;

  TJclDoubleDoubleBucket = class
  public
    Size: Integer;
    Entries: array of TJclDoubleDoubleHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclDoubleDoubleHashMap = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclDoubleContainer,
    IJclDoubleDoubleMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function FreeValue(var Value: Double): Double;
    function KeysEqual(const A, B: Double): Boolean;
    function ValuesEqual(const A, B: Double): Boolean;
  private
    FBuckets: array of TJclDoubleDoubleBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclDoubleDoubleMap }
    procedure Clear;
    function ContainsKey(const Key: Double): Boolean;
    function ContainsValue(const Value: Double): Boolean;
    function MapEquals(const AMap: IJclDoubleDoubleMap): Boolean;
    function GetValue(const Key: Double): Double;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Double): Double;
    function KeySet: IJclDoubleSet;
    procedure PutAll(const AMap: IJclDoubleDoubleMap);
    procedure PutValue(const Key: Double; const Value: Double);
    function Remove(const Key: Double): Double;
    function Size: Integer;
    function Values: IJclDoubleCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclExtendedIntfHashEntry = record
    Key: Extended;
    Value: IInterface;
  end;

  TJclExtendedIntfBucket = class
  public
    Size: Integer;
    Entries: array of TJclExtendedIntfHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclExtendedIntfHashMap = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclExtendedContainer,
    IJclExtendedIntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: Extended): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclExtendedIntfBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclExtendedIntfMap }
    procedure Clear;
    function ContainsKey(const Key: Extended): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function MapEquals(const AMap: IJclExtendedIntfMap): Boolean;
    function GetValue(const Key: Extended): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Extended;
    function KeySet: IJclExtendedSet;
    procedure PutAll(const AMap: IJclExtendedIntfMap);
    procedure PutValue(const Key: Extended; const Value: IInterface);
    function Remove(const Key: Extended): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntfExtendedHashEntry = record
    Key: IInterface;
    Value: Extended;
  end;

  TJclIntfExtendedBucket = class
  public
    Size: Integer;
    Entries: array of TJclIntfExtendedHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclIntfExtendedHashMap = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclExtendedContainer,
    IJclIntfExtendedMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Extended): Extended;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: Extended): Boolean;
  private
    FBuckets: array of TJclIntfExtendedBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfExtendedMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: Extended): Boolean;
    function MapEquals(const AMap: IJclIntfExtendedMap): Boolean;
    function GetValue(const Key: IInterface): Extended;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Extended): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfExtendedMap);
    procedure PutValue(const Key: IInterface; const Value: Extended);
    function Remove(const Key: IInterface): Extended;
    function Size: Integer;
    function Values: IJclExtendedCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclExtendedExtendedHashEntry = record
    Key: Extended;
    Value: Extended;
  end;

  TJclExtendedExtendedBucket = class
  public
    Size: Integer;
    Entries: array of TJclExtendedExtendedHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclExtendedExtendedHashMap = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclExtendedContainer,
    IJclExtendedExtendedMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function FreeValue(var Value: Extended): Extended;
    function KeysEqual(const A, B: Extended): Boolean;
    function ValuesEqual(const A, B: Extended): Boolean;
  private
    FBuckets: array of TJclExtendedExtendedBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclExtendedExtendedMap }
    procedure Clear;
    function ContainsKey(const Key: Extended): Boolean;
    function ContainsValue(const Value: Extended): Boolean;
    function MapEquals(const AMap: IJclExtendedExtendedMap): Boolean;
    function GetValue(const Key: Extended): Extended;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Extended): Extended;
    function KeySet: IJclExtendedSet;
    procedure PutAll(const AMap: IJclExtendedExtendedMap);
    procedure PutValue(const Key: Extended; const Value: Extended);
    function Remove(const Key: Extended): Extended;
    function Size: Integer;
    function Values: IJclExtendedCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatIntfHashMap = TJclExtendedIntfHashMap;
  TJclIntfFloatHashMap = TJclIntfExtendedHashMap;
  TJclFloatFloatHashMap = TJclExtendedExtendedHashMap;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatIntfHashMap = TJclDoubleIntfHashMap;
  TJclIntfFloatHashMap = TJclIntfDoubleHashMap;
  TJclFloatFloatHashMap = TJclDoubleDoubleHashMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatIntfHashMap = TJclSingleIntfHashMap;
  TJclIntfFloatHashMap = TJclIntfSingleHashMap;
  TJclFloatFloatHashMap = TJclSingleSingleHashMap;
  {$ENDIF MATH_SINGLE_PRECISION}

  TJclIntegerIntfHashEntry = record
    Key: Integer;
    Value: IInterface;
  end;

  TJclIntegerIntfBucket = class
  public
    Size: Integer;
    Entries: array of TJclIntegerIntfHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclIntegerIntfHashMap = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntegerIntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(A, B: Integer): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclIntegerIntfBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntegerIntfMap }
    procedure Clear;
    function ContainsKey(Key: Integer): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function MapEquals(const AMap: IJclIntegerIntfMap): Boolean;
    function GetValue(Key: Integer): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Integer;
    function KeySet: IJclIntegerSet;
    procedure PutAll(const AMap: IJclIntegerIntfMap);
    procedure PutValue(Key: Integer; const Value: IInterface);
    function Remove(Key: Integer): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntfIntegerHashEntry = record
    Key: IInterface;
    Value: Integer;
  end;

  TJclIntfIntegerBucket = class
  public
    Size: Integer;
    Entries: array of TJclIntfIntegerHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclIntfIntegerHashMap = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntfIntegerMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Integer): Integer;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(A, B: Integer): Boolean;
  private
    FBuckets: array of TJclIntfIntegerBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfIntegerMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(Value: Integer): Boolean;
    function MapEquals(const AMap: IJclIntfIntegerMap): Boolean;
    function GetValue(const Key: IInterface): Integer;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Integer): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfIntegerMap);
    procedure PutValue(const Key: IInterface; Value: Integer);
    function Remove(const Key: IInterface): Integer;
    function Size: Integer;
    function Values: IJclIntegerCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntegerIntegerHashEntry = record
    Key: Integer;
    Value: Integer;
  end;

  TJclIntegerIntegerBucket = class
  public
    Size: Integer;
    Entries: array of TJclIntegerIntegerHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclIntegerIntegerHashMap = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntegerIntegerMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function FreeValue(var Value: Integer): Integer;
    function KeysEqual(A, B: Integer): Boolean;
    function ValuesEqual(A, B: Integer): Boolean;
  private
    FBuckets: array of TJclIntegerIntegerBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntegerIntegerMap }
    procedure Clear;
    function ContainsKey(Key: Integer): Boolean;
    function ContainsValue(Value: Integer): Boolean;
    function MapEquals(const AMap: IJclIntegerIntegerMap): Boolean;
    function GetValue(Key: Integer): Integer;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Integer): Integer;
    function KeySet: IJclIntegerSet;
    procedure PutAll(const AMap: IJclIntegerIntegerMap);
    procedure PutValue(Key: Integer; Value: Integer);
    function Remove(Key: Integer): Integer;
    function Size: Integer;
    function Values: IJclIntegerCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclCardinalIntfHashEntry = record
    Key: Cardinal;
    Value: IInterface;
  end;

  TJclCardinalIntfBucket = class
  public
    Size: Integer;
    Entries: array of TJclCardinalIntfHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclCardinalIntfHashMap = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclCardinalIntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(A, B: Cardinal): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclCardinalIntfBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCardinalIntfMap }
    procedure Clear;
    function ContainsKey(Key: Cardinal): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function MapEquals(const AMap: IJclCardinalIntfMap): Boolean;
    function GetValue(Key: Cardinal): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Cardinal;
    function KeySet: IJclCardinalSet;
    procedure PutAll(const AMap: IJclCardinalIntfMap);
    procedure PutValue(Key: Cardinal; const Value: IInterface);
    function Remove(Key: Cardinal): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntfCardinalHashEntry = record
    Key: IInterface;
    Value: Cardinal;
  end;

  TJclIntfCardinalBucket = class
  public
    Size: Integer;
    Entries: array of TJclIntfCardinalHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclIntfCardinalHashMap = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntfCardinalMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Cardinal): Cardinal;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(A, B: Cardinal): Boolean;
  private
    FBuckets: array of TJclIntfCardinalBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCardinalMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(Value: Cardinal): Boolean;
    function MapEquals(const AMap: IJclIntfCardinalMap): Boolean;
    function GetValue(const Key: IInterface): Cardinal;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Cardinal): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfCardinalMap);
    procedure PutValue(const Key: IInterface; Value: Cardinal);
    function Remove(const Key: IInterface): Cardinal;
    function Size: Integer;
    function Values: IJclCardinalCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclCardinalCardinalHashEntry = record
    Key: Cardinal;
    Value: Cardinal;
  end;

  TJclCardinalCardinalBucket = class
  public
    Size: Integer;
    Entries: array of TJclCardinalCardinalHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclCardinalCardinalHashMap = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclCardinalCardinalMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function FreeValue(var Value: Cardinal): Cardinal;
    function KeysEqual(A, B: Cardinal): Boolean;
    function ValuesEqual(A, B: Cardinal): Boolean;
  private
    FBuckets: array of TJclCardinalCardinalBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCardinalCardinalMap }
    procedure Clear;
    function ContainsKey(Key: Cardinal): Boolean;
    function ContainsValue(Value: Cardinal): Boolean;
    function MapEquals(const AMap: IJclCardinalCardinalMap): Boolean;
    function GetValue(Key: Cardinal): Cardinal;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Cardinal): Cardinal;
    function KeySet: IJclCardinalSet;
    procedure PutAll(const AMap: IJclCardinalCardinalMap);
    procedure PutValue(Key: Cardinal; Value: Cardinal);
    function Remove(Key: Cardinal): Cardinal;
    function Size: Integer;
    function Values: IJclCardinalCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclInt64IntfHashEntry = record
    Key: Int64;
    Value: IInterface;
  end;

  TJclInt64IntfBucket = class
  public
    Size: Integer;
    Entries: array of TJclInt64IntfHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclInt64IntfHashMap = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclInt64IntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(const A, B: Int64): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclInt64IntfBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclInt64IntfMap }
    procedure Clear;
    function ContainsKey(const Key: Int64): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function MapEquals(const AMap: IJclInt64IntfMap): Boolean;
    function GetValue(const Key: Int64): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Int64;
    function KeySet: IJclInt64Set;
    procedure PutAll(const AMap: IJclInt64IntfMap);
    procedure PutValue(const Key: Int64; const Value: IInterface);
    function Remove(const Key: Int64): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntfInt64HashEntry = record
    Key: IInterface;
    Value: Int64;
  end;

  TJclIntfInt64Bucket = class
  public
    Size: Integer;
    Entries: array of TJclIntfInt64HashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclIntfInt64HashMap = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntfInt64Map)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Int64): Int64;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(const A, B: Int64): Boolean;
  private
    FBuckets: array of TJclIntfInt64Bucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfInt64Map }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(const Value: Int64): Boolean;
    function MapEquals(const AMap: IJclIntfInt64Map): Boolean;
    function GetValue(const Key: IInterface): Int64;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Int64): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfInt64Map);
    procedure PutValue(const Key: IInterface; const Value: Int64);
    function Remove(const Key: IInterface): Int64;
    function Size: Integer;
    function Values: IJclInt64Collection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclInt64Int64HashEntry = record
    Key: Int64;
    Value: Int64;
  end;

  TJclInt64Int64Bucket = class
  public
    Size: Integer;
    Entries: array of TJclInt64Int64HashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclInt64Int64HashMap = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclInt64Int64Map)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function FreeValue(var Value: Int64): Int64;
    function KeysEqual(const A, B: Int64): Boolean;
    function ValuesEqual(const A, B: Int64): Boolean;
  private
    FBuckets: array of TJclInt64Int64Bucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclInt64Int64Map }
    procedure Clear;
    function ContainsKey(const Key: Int64): Boolean;
    function ContainsValue(const Value: Int64): Boolean;
    function MapEquals(const AMap: IJclInt64Int64Map): Boolean;
    function GetValue(const Key: Int64): Int64;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: Int64): Int64;
    function KeySet: IJclInt64Set;
    procedure PutAll(const AMap: IJclInt64Int64Map);
    procedure PutValue(const Key: Int64; const Value: Int64);
    function Remove(const Key: Int64): Int64;
    function Size: Integer;
    function Values: IJclInt64Collection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  {$IFNDEF CLR}
  TJclPtrIntfHashEntry = record
    Key: Pointer;
    Value: IInterface;
  end;

  TJclPtrIntfBucket = class
  public
    Size: Integer;
    Entries: array of TJclPtrIntfHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclPtrIntfHashMap = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclPtrIntfMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysEqual(A, B: Pointer): Boolean;
    function ValuesEqual(const A, B: IInterface): Boolean;
  private
    FBuckets: array of TJclPtrIntfBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclPtrIntfMap }
    procedure Clear;
    function ContainsKey(Key: Pointer): Boolean;
    function ContainsValue(const Value: IInterface): Boolean;
    function MapEquals(const AMap: IJclPtrIntfMap): Boolean;
    function GetValue(Key: Pointer): IInterface;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: IInterface): Pointer;
    function KeySet: IJclPtrSet;
    procedure PutAll(const AMap: IJclPtrIntfMap);
    procedure PutValue(Key: Pointer; const Value: IInterface);
    function Remove(Key: Pointer): IInterface;
    function Size: Integer;
    function Values: IJclIntfCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclIntfPtrHashEntry = record
    Key: IInterface;
    Value: Pointer;
  end;

  TJclIntfPtrBucket = class
  public
    Size: Integer;
    Entries: array of TJclIntfPtrHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclIntfPtrHashMap = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntfPtrMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: Pointer): Pointer;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(A, B: Pointer): Boolean;
  private
    FBuckets: array of TJclIntfPtrBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfPtrMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(Value: Pointer): Boolean;
    function MapEquals(const AMap: IJclIntfPtrMap): Boolean;
    function GetValue(const Key: IInterface): Pointer;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Pointer): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfPtrMap);
    procedure PutValue(const Key: IInterface; Value: Pointer);
    function Remove(const Key: IInterface): Pointer;
    function Size: Integer;
    function Values: IJclPtrCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclPtrPtrHashEntry = record
    Key: Pointer;
    Value: Pointer;
  end;

  TJclPtrPtrBucket = class
  public
    Size: Integer;
    Entries: array of TJclPtrPtrHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclPtrPtrHashMap = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclPtrPtrMap)
  protected
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function FreeValue(var Value: Pointer): Pointer;
    function KeysEqual(A, B: Pointer): Boolean;
    function ValuesEqual(A, B: Pointer): Boolean;
  private
    FBuckets: array of TJclPtrPtrBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclPtrPtrMap }
    procedure Clear;
    function ContainsKey(Key: Pointer): Boolean;
    function ContainsValue(Value: Pointer): Boolean;
    function MapEquals(const AMap: IJclPtrPtrMap): Boolean;
    function GetValue(Key: Pointer): Pointer;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: Pointer): Pointer;
    function KeySet: IJclPtrSet;
    procedure PutAll(const AMap: IJclPtrPtrMap);
    procedure PutValue(Key: Pointer; Value: Pointer);
    function Remove(Key: Pointer): Pointer;
    function Size: Integer;
    function Values: IJclPtrCollection;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;
  {$ENDIF ~CLR}

  TJclIntfHashEntry = record
    Key: IInterface;
    Value: TObject;
  end;

  TJclIntfBucket = class
  public
    Size: Integer;
    Entries: array of TJclIntfHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclIntfHashMap = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclValueOwner,
    IJclIntfMap)
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function Hash(const AInterface: IInterface): Integer; reintroduce;
    function KeysEqual(const A, B: IInterface): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclIntfBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfMap }
    procedure Clear;
    function ContainsKey(const Key: IInterface): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function MapEquals(const AMap: IJclIntfMap): Boolean;
    function GetValue(const Key: IInterface): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): IInterface;
    function KeySet: IJclIntfSet;
    procedure PutAll(const AMap: IJclIntfMap);
    procedure PutValue(const Key: IInterface; Value: TObject);
    function Remove(const Key: IInterface): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclAnsiStrHashEntry = record
    Key: AnsiString;
    Value: TObject;
  end;

  TJclAnsiStrBucket = class
  public
    Size: Integer;
    Entries: array of TJclAnsiStrHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclAnsiStrHashMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer, IJclValueOwner,
    IJclAnsiStrMap)
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function KeysEqual(const A, B: AnsiString): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclAnsiStrBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclAnsiStrMap }
    procedure Clear;
    function ContainsKey(const Key: AnsiString): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function MapEquals(const AMap: IJclAnsiStrMap): Boolean;
    function GetValue(const Key: AnsiString): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): AnsiString;
    function KeySet: IJclAnsiStrSet;
    procedure PutAll(const AMap: IJclAnsiStrMap);
    procedure PutValue(const Key: AnsiString; Value: TObject);
    function Remove(const Key: AnsiString): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclWideStrHashEntry = record
    Key: WideString;
    Value: TObject;
  end;

  TJclWideStrBucket = class
  public
    Size: Integer;
    Entries: array of TJclWideStrHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclWideStrHashMap = class(TJclwideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclWideStrContainer, IJclValueOwner,
    IJclWideStrMap)
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function KeysEqual(const A, B: WideString): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclWideStrBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclWideStrMap }
    procedure Clear;
    function ContainsKey(const Key: WideString): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function MapEquals(const AMap: IJclWideStrMap): Boolean;
    function GetValue(const Key: WideString): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): WideString;
    function KeySet: IJclWideStrSet;
    procedure PutAll(const AMap: IJclWideStrMap);
    procedure PutValue(const Key: WideString; Value: TObject);
    function Remove(const Key: WideString): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

{$IFDEF SUPPORTS_UNICODE_STRING}
  TJclUnicodeStrHashEntry = record
    Key: UnicodeString;
    Value: TObject;
  end;

  TJclUnicodeStrBucket = class
  public
    Size: Integer;
    Entries: array of TJclUnicodeStrHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclUnicodeStrHashMap = class(TJclUnicodeStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclUnicodeStrContainer, IJclValueOwner,
    IJclUnicodeStrMap)
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: UnicodeString): UnicodeString;
    function KeysEqual(const A, B: UnicodeString): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclUnicodeStrBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclUnicodeStrMap }
    procedure Clear;
    function ContainsKey(const Key: UnicodeString): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function MapEquals(const AMap: IJclUnicodeStrMap): Boolean;
    function GetValue(const Key: UnicodeString): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): UnicodeString;
    function KeySet: IJclUnicodeStrSet;
    procedure PutAll(const AMap: IJclUnicodeStrMap);
    procedure PutValue(const Key: UnicodeString; Value: TObject);
    function Remove(const Key: UnicodeString): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;
{$ENDIF SUPPORTS_UNICODE_STRING}

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrHashMap = TJclAnsiStrHashMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrHashMap = TJclWideStrHashMap;
  {$ENDIF CONTAINER_WIDESTR}
  {$IFDEF CONTAINER_UNICODESTR}
  TJclStrHashMap = TJclUnicodeStrHashMap;
  {$ENDIF CONTAINER_UNICODESTR}

  TJclSingleHashEntry = record
    Key: Single;
    Value: TObject;
  end;

  TJclSingleBucket = class
  public
    Size: Integer;
    Entries: array of TJclSingleHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclSingleHashMap = class(TJclSingleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclSingleContainer, IJclValueOwner,
    IJclSingleMap)
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Single): Single;
    function KeysEqual(const A, B: Single): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclSingleBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclSingleMap }
    procedure Clear;
    function ContainsKey(const Key: Single): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function MapEquals(const AMap: IJclSingleMap): Boolean;
    function GetValue(const Key: Single): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Single;
    function KeySet: IJclSingleSet;
    procedure PutAll(const AMap: IJclSingleMap);
    procedure PutValue(const Key: Single; Value: TObject);
    function Remove(const Key: Single): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclDoubleHashEntry = record
    Key: Double;
    Value: TObject;
  end;

  TJclDoubleBucket = class
  public
    Size: Integer;
    Entries: array of TJclDoubleHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclDoubleHashMap = class(TJclDoubleAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclDoubleContainer, IJclValueOwner,
    IJclDoubleMap)
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Double): Double;
    function KeysEqual(const A, B: Double): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclDoubleBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclDoubleMap }
    procedure Clear;
    function ContainsKey(const Key: Double): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function MapEquals(const AMap: IJclDoubleMap): Boolean;
    function GetValue(const Key: Double): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Double;
    function KeySet: IJclDoubleSet;
    procedure PutAll(const AMap: IJclDoubleMap);
    procedure PutValue(const Key: Double; Value: TObject);
    function Remove(const Key: Double): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclExtendedHashEntry = record
    Key: Extended;
    Value: TObject;
  end;

  TJclExtendedBucket = class
  public
    Size: Integer;
    Entries: array of TJclExtendedHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclExtendedHashMap = class(TJclExtendedAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclExtendedContainer, IJclValueOwner,
    IJclExtendedMap)
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Extended): Extended;
    function KeysEqual(const A, B: Extended): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclExtendedBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclExtendedMap }
    procedure Clear;
    function ContainsKey(const Key: Extended): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function MapEquals(const AMap: IJclExtendedMap): Boolean;
    function GetValue(const Key: Extended): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Extended;
    function KeySet: IJclExtendedSet;
    procedure PutAll(const AMap: IJclExtendedMap);
    procedure PutValue(const Key: Extended; Value: TObject);
    function Remove(const Key: Extended): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  {$IFDEF MATH_EXTENDED_PRECISION}
  TJclFloatHashMap = TJclExtendedHashMap;
  {$ENDIF MATH_EXTENDED_PRECISION}
  {$IFDEF MATH_DOUBLE_PRECISION}
  TJclFloatHashMap = TJclDoubleHashMap;
  {$ENDIF MATH_DOUBLE_PRECISION}
  {$IFDEF MATH_SINGLE_PRECISION}
  TJclFloatHashMap = TJclSingleHashMap;
  {$ENDIF MATH_SINGLE_PRECISION}

  TJclIntegerHashEntry = record
    Key: Integer;
    Value: TObject;
  end;

  TJclIntegerBucket = class
  public
    Size: Integer;
    Entries: array of TJclIntegerHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclIntegerHashMap = class(TJclIntegerAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclValueOwner,
    IJclIntegerMap)
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Integer): Integer;
    function KeysEqual(A, B: Integer): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclIntegerBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclIntegerMap }
    procedure Clear;
    function ContainsKey(Key: Integer): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function MapEquals(const AMap: IJclIntegerMap): Boolean;
    function GetValue(Key: Integer): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Integer;
    function KeySet: IJclIntegerSet;
    procedure PutAll(const AMap: IJclIntegerMap);
    procedure PutValue(Key: Integer; Value: TObject);
    function Remove(Key: Integer): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclCardinalHashEntry = record
    Key: Cardinal;
    Value: TObject;
  end;

  TJclCardinalBucket = class
  public
    Size: Integer;
    Entries: array of TJclCardinalHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclCardinalHashMap = class(TJclCardinalAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclValueOwner,
    IJclCardinalMap)
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Cardinal): Cardinal;
    function KeysEqual(A, B: Cardinal): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclCardinalBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclCardinalMap }
    procedure Clear;
    function ContainsKey(Key: Cardinal): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function MapEquals(const AMap: IJclCardinalMap): Boolean;
    function GetValue(Key: Cardinal): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Cardinal;
    function KeySet: IJclCardinalSet;
    procedure PutAll(const AMap: IJclCardinalMap);
    procedure PutValue(Key: Cardinal; Value: TObject);
    function Remove(Key: Cardinal): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  TJclInt64HashEntry = record
    Key: Int64;
    Value: TObject;
  end;

  TJclInt64Bucket = class
  public
    Size: Integer;
    Entries: array of TJclInt64HashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclInt64HashMap = class(TJclInt64AbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclValueOwner,
    IJclInt64Map)
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Int64): Int64;
    function KeysEqual(const A, B: Int64): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclInt64Bucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclInt64Map }
    procedure Clear;
    function ContainsKey(const Key: Int64): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function MapEquals(const AMap: IJclInt64Map): Boolean;
    function GetValue(const Key: Int64): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Int64;
    function KeySet: IJclInt64Set;
    procedure PutAll(const AMap: IJclInt64Map);
    procedure PutValue(const Key: Int64; Value: TObject);
    function Remove(const Key: Int64): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  {$IFNDEF CLR}
  TJclPtrHashEntry = record
    Key: Pointer;
    Value: TObject;
  end;

  TJclPtrBucket = class
  public
    Size: Integer;
    Entries: array of TJclPtrHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclPtrHashMap = class(TJclPtrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclValueOwner,
    IJclPtrMap)
  private
    FOwnsValues: Boolean;
  protected
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: Pointer): Pointer;
    function KeysEqual(A, B: Pointer): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclPtrBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclPtrMap }
    procedure Clear;
    function ContainsKey(Key: Pointer): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function MapEquals(const AMap: IJclPtrMap): Boolean;
    function GetValue(Key: Pointer): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): Pointer;
    function KeySet: IJclPtrSet;
    procedure PutAll(const AMap: IJclPtrMap);
    procedure PutValue(Key: Pointer; Value: TObject);
    function Remove(Key: Pointer): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;
  {$ENDIF ~CLR}

  TJclHashEntry = record
    Key: TObject;
    Value: TObject;
  end;

  TJclBucket = class
  public
    Size: Integer;
    Entries: array of TJclHashEntry;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclHashMap = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclKeyOwner, IJclValueOwner,
    IJclMap)
  private
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
  protected
    { IJclKeyOwner }
    function FreeKey(var Key: TObject): TObject;
    function GetOwnsKeys: Boolean;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function Hash(AObject: TObject): Integer;
    function KeysEqual(A, B: TObject): Boolean;
    function ValuesEqual(A, B: TObject): Boolean;
  public
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TJclBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclMap }
    procedure Clear;
    function ContainsKey(Key: TObject): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function MapEquals(const AMap: IJclMap): Boolean;
    function GetValue(Key: TObject): TObject;
    function IsEmpty: Boolean;
    function KeyOfValue(Value: TObject): TObject;
    function KeySet: IJclSet;
    procedure PutAll(const AMap: IJclMap);
    procedure PutValue(Key: TObject; Value: TObject);
    function Remove(Key: TObject): TObject;
    function Size: Integer;
    function Values: IJclCollection;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  TJclHashEntry<TKey,TValue> = record
    Key: TKey;
    Value: TValue;
  end;

  TJclBucket<TKey,TValue> = class
  public
    Size: Integer;
    Entries: array of TJclHashEntry<TKey,TValue>;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
  end;

  TJclHashMap<TKey,TValue> = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclPairOwner<TKey, TValue>,
    IJclMap<TKey,TValue>)
  protected
    type
      TBucket = TJclBucket<TKey,TValue>;
  private
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
  protected
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
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;
  private
    FBuckets: array of TBucket;
    FHashFunction: TJclHashFunction;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    { IJclPackable }
    procedure Pack; override;
    procedure SetCapacity(Value: Integer); override;
    { IJclMap<TKey,TValue> }
    procedure Clear;
    function ContainsKey(const Key: TKey): Boolean;
    function ContainsValue(const Value: TValue): Boolean;
    function MapEquals(const AMap: IJclMap<TKey,TValue>): Boolean;
    function GetValue(const Key: TKey): TValue;
    function IsEmpty: Boolean;
    function KeyOfValue(const Value: TValue): TKey;
    function KeySet: IJclSet<TKey>;
    procedure PutAll(const AMap: IJclMap<TKey,TValue>);
    procedure PutValue(const Key: TKey; const Value: TValue);
    function Remove(const Key: TKey): TValue;
    function Size: Integer;
    function Values: IJclCollection<TValue>;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
    destructor Destroy; override;
    property HashFunction: TJclHashFunction read FHashFunction write FHashFunction;
  end;

  // E = external helper to compare and hash items
  // KeyComparer is used only when getting KeySet
  // GetHashCode and Equals methods of KeyEqualityComparer are used
  // GetHashCode of ValueEqualityComparer is not used
  TJclHashMapE<TKey, TValue> = class(TJclHashMap<TKey, TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  protected
    type
      TArrayList = TJclArrayListE<TValue>;
      TArraySet = TJclArraySetE<TKey>;
  private
    FKeyEqualityComparer: IJclEqualityComparer<TKey>;
    FKeyHashConverter: IJclHashConverter<TKey>;
    FKeyComparer: IJclComparer<TKey>;
    FValueEqualityComparer: IJclEqualityComparer<TValue>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function Hash(const AKey: TKey): Integer; override;
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
  public
    constructor Create(const AKeyEqualityComparer: IJclEqualityComparer<TKey>;
      const AKeyHashConverter: IJclHashConverter<TKey>; const AValueEqualityComparer: IJclEqualityComparer<TValue>;
      const AKeyComparer: IJclComparer<TKey>; ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);

    property KeyEqualityComparer: IJclEqualityComparer<TKey> read FKeyEqualityComparer write FKeyEqualityComparer;
    property KeyHashConverter: IJclHashConverter<TKey> read FKeyHashConverter write FKeyHashConverter;
    property KeyComparer: IJclComparer<TKey> read FKeyComparer write FKeyComparer;
    property ValueEqualityComparer: IJclEqualityComparer<TValue> read FValueEqualityComparer write FValueEqualityComparer;
  end;

  // F = Functions to compare and hash items
  // KeyComparer is used only when getting KeySet
  TJclHashMapF<TKey, TValue> = class(TJclHashMap<TKey, TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  protected
    type
      TArrayList = TJclArrayListF<TValue>;
      TArraySet = TJclArraySetF<TKey>;
  private
    FKeyEqualityCompare: TEqualityCompare<TKey>;
    FKeyHash: THashConvert<TKey>;
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
  public
    constructor Create(AKeyEqualityCompare: TEqualityCompare<TKey>; AKeyHash: THashConvert<TKey>;
      AValueEqualityCompare: TEqualityCompare<TValue>; AKeyCompare: TCompare<TKey>;
      ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);

    property KeyEqualityCompare: TEqualityCompare<TKey> read FKeyEqualityCompare write FKeyEqualityCompare;
    property KeyCompare: TCompare<TKey> read FKeyCompare write FKeyCompare;
    property KeyHash: THashConvert<TKey> read FKeyHash write FKeyHash;
    property ValueEqualityCompare: TEqualityCompare<TValue> read FValueEqualityCompare write FValueEqualityCompare;
  end;

  // I = items can compare themselves to an other, items can create hash value from themselves
  TJclHashMapI<TKey: IComparable<TKey>, IEquatable<TKey>, IHashable; TValue: IEquatable<TValue>> = class(TJclHashMap<TKey, TValue>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer,
    IJclMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  protected
    type
      TArrayList = TJclArrayListI<TValue>;
      TArraySet = TJclArraySetI<TKey>;
  protected
    function Hash(const AKey: TKey): Integer; override;
    function KeysEqual(const A, B: TKey): Boolean; override;
    function ValuesEqual(const A, B: TValue): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
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
  JclResources;

function HashMul(Key, Range: Integer): Integer;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(Range * (Frac(Key * A)));
end;

//=== { TJclIntfIntfBucket } ==========================================

procedure TJclIntfIntfBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclIntfIntfHashMap } ==========================================

constructor TJclIntfIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfIntfHashMap.Destroy;
begin
  FReadOnly := False;
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
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfIntfHashMap then
    begin
      ADest := TJclIntfIntfHashMap(Dest);
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
    end
    else
    if Supports(IInterface(Dest), IJclIntfIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.MapEquals(const AMap: IJclIntfIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.GetValue(const Key: IInterface): IInterface;
var
  I: Integer;
  Bucket: TJclIntfIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.PutAll(const AMap: IJclIntfIntfMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.PutValue(const Key: IInterface; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclIntfIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.Remove(const Key: IInterface): IInterface;
var
  Bucket: TJclIntfIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
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

function TJclIntfIntfHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclIntfIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclAnsiStrIntfBucket } ==========================================

procedure TJclAnsiStrIntfBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclAnsiStrIntfHashMap } ==========================================

constructor TJclAnsiStrIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclAnsiStrIntfHashMap.Destroy;
begin
  FReadOnly := False;
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
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclAnsiStrIntfHashMap then
    begin
      ADest := TJclAnsiStrIntfHashMap(Dest);
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
    end
    else
    if Supports(IInterface(Dest), IJclAnsiStrIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfHashMap.ContainsKey(const Key: AnsiString): Boolean;
var
  I: Integer;
  Bucket: TJclAnsiStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfHashMap.MapEquals(const AMap: IJclAnsiStrIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfHashMap.GetValue(const Key: AnsiString): IInterface;
var
  I: Integer;
  Bucket: TJclAnsiStrIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfHashMap.KeySet: IJclAnsiStrSet;
var
  I, J: Integer;
  Bucket: TJclAnsiStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclAnsiStrIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrIntfHashMap.PutAll(const AMap: IJclAnsiStrIntfMap);
var
  It: IJclAnsiStrIterator;
  Key: AnsiString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrIntfHashMap.PutValue(const Key: AnsiString; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclAnsiStrIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfHashMap.Remove(const Key: AnsiString): IInterface;
var
  Bucket: TJclAnsiStrIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
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

function TJclAnsiStrIntfHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclAnsiStrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

//=== { TJclIntfAnsiStrBucket } ==========================================

procedure TJclIntfAnsiStrBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := '';
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := '';
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclIntfAnsiStrHashMap } ==========================================

constructor TJclIntfAnsiStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfAnsiStrHashMap.Destroy;
begin
  FReadOnly := False;
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
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfAnsiStrHashMap then
    begin
      ADest := TJclIntfAnsiStrHashMap(Dest);
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
    end
    else
    if Supports(IInterface(Dest), IJclIntfAnsiStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrHashMap.ContainsValue(const Value: AnsiString): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrHashMap.MapEquals(const AMap: IJclIntfAnsiStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrHashMap.GetValue(const Key: IInterface): AnsiString;
var
  I: Integer;
  Bucket: TJclIntfAnsiStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfAnsiStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfAnsiStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfAnsiStrHashMap.PutAll(const AMap: IJclIntfAnsiStrMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfAnsiStrHashMap.PutValue(const Key: IInterface; const Value: AnsiString);
var
  Index: Integer;
  Bucket: TJclIntfAnsiStrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrHashMap.Remove(const Key: IInterface): AnsiString;
var
  Bucket: TJclIntfAnsiStrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfAnsiStrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfAnsiStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
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

function TJclIntfAnsiStrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfAnsiStrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfAnsiStrHashMap.ValuesEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclAnsiStrAnsiStrBucket } ==========================================

procedure TJclAnsiStrAnsiStrBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := '';
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := '';
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclAnsiStrAnsiStrHashMap } ==========================================

constructor TJclAnsiStrAnsiStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclAnsiStrAnsiStrHashMap.Destroy;
begin
  FReadOnly := False;
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
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclAnsiStrAnsiStrHashMap then
    begin
      ADest := TJclAnsiStrAnsiStrHashMap(Dest);
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
    end
    else
    if Supports(IInterface(Dest), IJclAnsiStrAnsiStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrHashMap.ContainsKey(const Key: AnsiString): Boolean;
var
  I: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrHashMap.ContainsValue(const Value: AnsiString): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrHashMap.MapEquals(const AMap: IJclAnsiStrAnsiStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrHashMap.GetValue(const Key: AnsiString): AnsiString;
var
  I: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrHashMap.KeySet: IJclAnsiStrSet;
var
  I, J: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrAnsiStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrAnsiStrHashMap.PutAll(const AMap: IJclAnsiStrAnsiStrMap);
var
  It: IJclAnsiStrIterator;
  Key: AnsiString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrAnsiStrHashMap.PutValue(const Key: AnsiString; const Value: AnsiString);
var
  Index: Integer;
  Bucket: TJclAnsiStrAnsiStrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrHashMap.Remove(const Key: AnsiString): AnsiString;
var
  Bucket: TJclAnsiStrAnsiStrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrAnsiStrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrAnsiStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
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

function TJclAnsiStrAnsiStrHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclAnsiStrAnsiStrHashMap.ValuesEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclWideStrIntfBucket } ==========================================

procedure TJclWideStrIntfBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclWideStrIntfHashMap } ==========================================

constructor TJclWideStrIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclWideStrIntfHashMap.Destroy;
begin
  FReadOnly := False;
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
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclWideStrIntfHashMap then
    begin
      ADest := TJclWideStrIntfHashMap(Dest);
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
    end
    else
    if Supports(IInterface(Dest), IJclWideStrIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfHashMap.ContainsKey(const Key: WideString): Boolean;
var
  I: Integer;
  Bucket: TJclWideStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfHashMap.MapEquals(const AMap: IJclWideStrIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfHashMap.GetValue(const Key: WideString): IInterface;
var
  I: Integer;
  Bucket: TJclWideStrIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfHashMap.KeySet: IJclWideStrSet;
var
  I, J: Integer;
  Bucket: TJclWideStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclWideStrIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrIntfHashMap.PutAll(const AMap: IJclWideStrIntfMap);
var
  It: IJclWideStrIterator;
  Key: WideString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrIntfHashMap.PutValue(const Key: WideString; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclWideStrIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfHashMap.Remove(const Key: WideString): IInterface;
var
  Bucket: TJclWideStrIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
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

function TJclWideStrIntfHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclWideStrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

//=== { TJclIntfWideStrBucket } ==========================================

procedure TJclIntfWideStrBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := '';
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := '';
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclIntfWideStrHashMap } ==========================================

constructor TJclIntfWideStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfWideStrHashMap.Destroy;
begin
  FReadOnly := False;
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
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfWideStrHashMap then
    begin
      ADest := TJclIntfWideStrHashMap(Dest);
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
    end
    else
    if Supports(IInterface(Dest), IJclIntfWideStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrHashMap.ContainsValue(const Value: WideString): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrHashMap.MapEquals(const AMap: IJclIntfWideStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrHashMap.GetValue(const Key: IInterface): WideString;
var
  I: Integer;
  Bucket: TJclIntfWideStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfWideStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfWideStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfWideStrHashMap.PutAll(const AMap: IJclIntfWideStrMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfWideStrHashMap.PutValue(const Key: IInterface; const Value: WideString);
var
  Index: Integer;
  Bucket: TJclIntfWideStrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrHashMap.Remove(const Key: IInterface): WideString;
var
  Bucket: TJclIntfWideStrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfWideStrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfWideStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
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

function TJclIntfWideStrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfWideStrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfWideStrHashMap.ValuesEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclWideStrWideStrBucket } ==========================================

procedure TJclWideStrWideStrBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := '';
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := '';
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclWideStrWideStrHashMap } ==========================================

constructor TJclWideStrWideStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclWideStrWideStrHashMap.Destroy;
begin
  FReadOnly := False;
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
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclWideStrWideStrHashMap then
    begin
      ADest := TJclWideStrWideStrHashMap(Dest);
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
    end
    else
    if Supports(IInterface(Dest), IJclWideStrWideStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrHashMap.ContainsKey(const Key: WideString): Boolean;
var
  I: Integer;
  Bucket: TJclWideStrWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrHashMap.ContainsValue(const Value: WideString): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrHashMap.MapEquals(const AMap: IJclWideStrWideStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrHashMap.GetValue(const Key: WideString): WideString;
var
  I: Integer;
  Bucket: TJclWideStrWideStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrHashMap.KeySet: IJclWideStrSet;
var
  I, J: Integer;
  Bucket: TJclWideStrWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrWideStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclWideStrWideStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrWideStrHashMap.PutAll(const AMap: IJclWideStrWideStrMap);
var
  It: IJclWideStrIterator;
  Key: WideString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrWideStrHashMap.PutValue(const Key: WideString; const Value: WideString);
var
  Index: Integer;
  Bucket: TJclWideStrWideStrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrHashMap.Remove(const Key: WideString): WideString;
var
  Bucket: TJclWideStrWideStrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrWideStrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrWideStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
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

function TJclWideStrWideStrHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclWideStrWideStrHashMap.ValuesEqual(const A, B: Widestring): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrIntfBucket } ==========================================

procedure TJclUnicodeStrIntfBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclUnicodeStrIntfHashMap } ==========================================

constructor TJclUnicodeStrIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclUnicodeStrIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclUnicodeStrIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclUnicodeStrIntfBucket;
  ADest: TJclUnicodeStrIntfHashMap;
  AMap: IJclUnicodeStrIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclUnicodeStrIntfHashMap then
    begin
      ADest := TJclUnicodeStrIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclUnicodeStrIntfBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclUnicodeStrIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclUnicodeStrIntfHashMap then
    TJclUnicodeStrIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclUnicodeStrIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrIntfHashMap.ContainsKey(const Key: UnicodeString): Boolean;
var
  I: Integer;
  Bucket: TJclUnicodeStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrIntfHashMap.MapEquals(const AMap: IJclUnicodeStrIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrIntfHashMap.GetValue(const Key: UnicodeString): IInterface;
var
  I: Integer;
  Bucket: TJclUnicodeStrIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclUnicodeStrIntfHashMap.KeyOfValue(const Value: IInterface): UnicodeString;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrIntfHashMap.KeySet: IJclUnicodeStrSet;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclUnicodeStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclUnicodeStrIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrIntfHashMap.PutAll(const AMap: IJclUnicodeStrIntfMap);
var
  It: IJclUnicodeStrIterator;
  Key: UnicodeString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrIntfHashMap.PutValue(const Key: UnicodeString; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclUnicodeStrIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
        Bucket := TJclUnicodeStrIntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrIntfHashMap.Remove(const Key: UnicodeString): IInterface;
var
  Bucket: TJclUnicodeStrIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclUnicodeStrIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclUnicodeStrIntfHashMap.FreeKey(var Key: UnicodeString): UnicodeString;
begin
  Result := Key;
  Key := '';
end;

function TJclUnicodeStrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclUnicodeStrIntfHashMap.KeysEqual(const A, B: UnicodeString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclUnicodeStrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

//=== { TJclIntfUnicodeStrBucket } ==========================================

procedure TJclIntfUnicodeStrBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := '';
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := '';
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclIntfUnicodeStrHashMap } ==========================================

constructor TJclIntfUnicodeStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfUnicodeStrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfUnicodeStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfUnicodeStrBucket;
  ADest: TJclIntfUnicodeStrHashMap;
  AMap: IJclIntfUnicodeStrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfUnicodeStrHashMap then
    begin
      ADest := TJclIntfUnicodeStrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfUnicodeStrBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclIntfUnicodeStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfUnicodeStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfUnicodeStrHashMap then
    TJclIntfUnicodeStrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfUnicodeStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfUnicodeStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfUnicodeStrHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfUnicodeStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfUnicodeStrHashMap.ContainsValue(const Value: UnicodeString): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfUnicodeStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfUnicodeStrHashMap.MapEquals(const AMap: IJclIntfUnicodeStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfUnicodeStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfUnicodeStrHashMap.GetValue(const Key: IInterface): UnicodeString;
var
  I: Integer;
  Bucket: TJclIntfUnicodeStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfUnicodeStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfUnicodeStrHashMap.KeyOfValue(const Value: UnicodeString): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfUnicodeStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfUnicodeStrHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfUnicodeStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfUnicodeStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfUnicodeStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfUnicodeStrHashMap.PutAll(const AMap: IJclIntfUnicodeStrMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfUnicodeStrHashMap.PutValue(const Key: IInterface; const Value: UnicodeString);
var
  Index: Integer;
  Bucket: TJclIntfUnicodeStrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
        Bucket := TJclIntfUnicodeStrBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfUnicodeStrHashMap.Remove(const Key: IInterface): UnicodeString;
var
  Bucket: TJclIntfUnicodeStrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfUnicodeStrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfUnicodeStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfUnicodeStrHashMap.Values: IJclUnicodeStrCollection;
var
  I, J: Integer;
  Bucket: TJclIntfUnicodeStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclUnicodeStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfUnicodeStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfUnicodeStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfUnicodeStrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfUnicodeStrHashMap.FreeValue(var Value: UnicodeString): UnicodeString;
begin
  Result := Value;
  Value := '';
end;

function TJclIntfUnicodeStrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfUnicodeStrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfUnicodeStrHashMap.ValuesEqual(const A, B: UnicodeString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclUnicodeStrUnicodeStrBucket } ==========================================

procedure TJclUnicodeStrUnicodeStrBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := '';
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := '';
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclUnicodeStrUnicodeStrHashMap } ==========================================

constructor TJclUnicodeStrUnicodeStrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclUnicodeStrUnicodeStrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclUnicodeStrUnicodeStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclUnicodeStrUnicodeStrBucket;
  ADest: TJclUnicodeStrUnicodeStrHashMap;
  AMap: IJclUnicodeStrUnicodeStrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclUnicodeStrUnicodeStrHashMap then
    begin
      ADest := TJclUnicodeStrUnicodeStrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclUnicodeStrUnicodeStrBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclUnicodeStrUnicodeStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrUnicodeStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclUnicodeStrUnicodeStrHashMap then
    TJclUnicodeStrUnicodeStrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclUnicodeStrUnicodeStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrUnicodeStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrUnicodeStrHashMap.ContainsKey(const Key: UnicodeString): Boolean;
var
  I: Integer;
  Bucket: TJclUnicodeStrUnicodeStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrUnicodeStrHashMap.ContainsValue(const Value: UnicodeString): Boolean;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrUnicodeStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrUnicodeStrHashMap.MapEquals(const AMap: IJclUnicodeStrUnicodeStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrUnicodeStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrUnicodeStrHashMap.GetValue(const Key: UnicodeString): UnicodeString;
var
  I: Integer;
  Bucket: TJclUnicodeStrUnicodeStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrUnicodeStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclUnicodeStrUnicodeStrHashMap.KeyOfValue(const Value: UnicodeString): UnicodeString;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrUnicodeStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrUnicodeStrHashMap.KeySet: IJclUnicodeStrSet;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrUnicodeStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclUnicodeStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrUnicodeStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclUnicodeStrUnicodeStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrUnicodeStrHashMap.PutAll(const AMap: IJclUnicodeStrUnicodeStrMap);
var
  It: IJclUnicodeStrIterator;
  Key: UnicodeString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrUnicodeStrHashMap.PutValue(const Key: UnicodeString; const Value: UnicodeString);
var
  Index: Integer;
  Bucket: TJclUnicodeStrUnicodeStrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
        Bucket := TJclUnicodeStrUnicodeStrBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrUnicodeStrHashMap.Remove(const Key: UnicodeString): UnicodeString;
var
  Bucket: TJclUnicodeStrUnicodeStrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrUnicodeStrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrUnicodeStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclUnicodeStrUnicodeStrHashMap.Values: IJclUnicodeStrCollection;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrUnicodeStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclUnicodeStrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrUnicodeStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrUnicodeStrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclUnicodeStrUnicodeStrHashMap.FreeKey(var Key: UnicodeString): UnicodeString;
begin
  Result := Key;
  Key := '';
end;

function TJclUnicodeStrUnicodeStrHashMap.FreeValue(var Value: UnicodeString): UnicodeString;
begin
  Result := Value;
  Value := '';
end;

function TJclUnicodeStrUnicodeStrHashMap.KeysEqual(const A, B: UnicodeString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclUnicodeStrUnicodeStrHashMap.ValuesEqual(const A, B: Unicodestring): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$ENDIF SUPPORTS_UNICODE_STRING}

//=== { TJclSingleIntfBucket } ==========================================

procedure TJclSingleIntfBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclSingleIntfHashMap } ==========================================

constructor TJclSingleIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclSingleIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclSingleIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclSingleIntfBucket;
  ADest: TJclSingleIntfHashMap;
  AMap: IJclSingleIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclSingleIntfHashMap then
    begin
      ADest := TJclSingleIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclSingleIntfBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclSingleIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclSingleIntfHashMap then
    TJclSingleIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclSingleIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclSingleIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.ContainsKey(const Key: Single): Boolean;
var
  I: Integer;
  Bucket: TJclSingleIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclSingleIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.MapEquals(const AMap: IJclSingleIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclSingleIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.GetValue(const Key: Single): IInterface;
var
  I: Integer;
  Bucket: TJclSingleIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclSingleIntfHashMap.KeyOfValue(const Value: IInterface): Single;
var
  I, J: Integer;
  Bucket: TJclSingleIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.KeySet: IJclSingleSet;
var
  I, J: Integer;
  Bucket: TJclSingleIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclSingleIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleIntfHashMap.PutAll(const AMap: IJclSingleIntfMap);
var
  It: IJclSingleIterator;
  Key: Single;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleIntfHashMap.PutValue(const Key: Single; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclSingleIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, nil)) then
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
        Bucket := TJclSingleIntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.Remove(const Key: Single): IInterface;
var
  Bucket: TJclSingleIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclSingleIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclSingleIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclSingleIntfHashMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclSingleIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclSingleIntfHashMap.KeysEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclSingleIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

//=== { TJclIntfSingleBucket } ==========================================

procedure TJclIntfSingleBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0.0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0.0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclIntfSingleHashMap } ==========================================

constructor TJclIntfSingleHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfSingleHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfSingleHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfSingleBucket;
  ADest: TJclIntfSingleHashMap;
  AMap: IJclIntfSingleMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfSingleHashMap then
    begin
      ADest := TJclIntfSingleHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfSingleBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclIntfSingleMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfSingleHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfSingleHashMap then
    TJclIntfSingleHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfSingleHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfSingleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.ContainsValue(const Value: Single): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.MapEquals(const AMap: IJclIntfSingleMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.GetValue(const Key: IInterface): Single;
var
  I: Integer;
  Bucket: TJclIntfSingleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfSingleHashMap.KeyOfValue(const Value: Single): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfSingleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfSingleHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfSingleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfSingleHashMap.PutAll(const AMap: IJclIntfSingleMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfSingleHashMap.PutValue(const Key: IInterface; const Value: Single);
var
  Index: Integer;
  Bucket: TJclIntfSingleBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, 0.0)) then
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
        Bucket := TJclIntfSingleBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.Remove(const Key: IInterface): Single;
var
  Bucket: TJclIntfSingleBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfSingleHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfSingleHashMap.Values: IJclSingleCollection;
var
  I, J: Integer;
  Bucket: TJclIntfSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSingleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfSingleHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfSingleHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfSingleHashMap.FreeValue(var Value: Single): Single;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclIntfSingleHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfSingleHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfSingleHashMap.ValuesEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclSingleSingleBucket } ==========================================

procedure TJclSingleSingleBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := 0.0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := 0.0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclSingleSingleHashMap } ==========================================

constructor TJclSingleSingleHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclSingleSingleHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclSingleSingleHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclSingleSingleBucket;
  ADest: TJclSingleSingleHashMap;
  AMap: IJclSingleSingleMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclSingleSingleHashMap then
    begin
      ADest := TJclSingleSingleHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclSingleSingleBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclSingleSingleMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleSingleHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclSingleSingleHashMap then
    TJclSingleSingleHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclSingleSingleHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclSingleSingleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.ContainsKey(const Key: Single): Boolean;
var
  I: Integer;
  Bucket: TJclSingleSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.ContainsValue(const Value: Single): Boolean;
var
  I, J: Integer;
  Bucket: TJclSingleSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.MapEquals(const AMap: IJclSingleSingleMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclSingleSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.GetValue(const Key: Single): Single;
var
  I: Integer;
  Bucket: TJclSingleSingleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclSingleSingleHashMap.KeyOfValue(const Value: Single): Single;
var
  I, J: Integer;
  Bucket: TJclSingleSingleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.KeySet: IJclSingleSet;
var
  I, J: Integer;
  Bucket: TJclSingleSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleSingleHashMap.Pack;
var
  I: Integer;
  Bucket: TJclSingleSingleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleSingleHashMap.PutAll(const AMap: IJclSingleSingleMap);
var
  It: IJclSingleIterator;
  Key: Single;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleSingleHashMap.PutValue(const Key: Single; const Value: Single);
var
  Index: Integer;
  Bucket: TJclSingleSingleBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, 0.0)) then
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
        Bucket := TJclSingleSingleBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.Remove(const Key: Single): Single;
var
  Bucket: TJclSingleSingleBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleSingleHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclSingleSingleHashMap.Values: IJclSingleCollection;
var
  I, J: Integer;
  Bucket: TJclSingleSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleSingleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleSingleHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclSingleSingleHashMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclSingleSingleHashMap.FreeValue(var Value: Single): Single;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclSingleSingleHashMap.KeysEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclSingleSingleHashMap.ValuesEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclDoubleIntfBucket } ==========================================

procedure TJclDoubleIntfBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclDoubleIntfHashMap } ==========================================

constructor TJclDoubleIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclDoubleIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclDoubleIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclDoubleIntfBucket;
  ADest: TJclDoubleIntfHashMap;
  AMap: IJclDoubleIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclDoubleIntfHashMap then
    begin
      ADest := TJclDoubleIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclDoubleIntfBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclDoubleIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclDoubleIntfHashMap then
    TJclDoubleIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclDoubleIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclDoubleIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.ContainsKey(const Key: Double): Boolean;
var
  I: Integer;
  Bucket: TJclDoubleIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclDoubleIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.MapEquals(const AMap: IJclDoubleIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclDoubleIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.GetValue(const Key: Double): IInterface;
var
  I: Integer;
  Bucket: TJclDoubleIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclDoubleIntfHashMap.KeyOfValue(const Value: IInterface): Double;
var
  I, J: Integer;
  Bucket: TJclDoubleIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.KeySet: IJclDoubleSet;
var
  I, J: Integer;
  Bucket: TJclDoubleIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclDoubleIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleIntfHashMap.PutAll(const AMap: IJclDoubleIntfMap);
var
  It: IJclDoubleIterator;
  Key: Double;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleIntfHashMap.PutValue(const Key: Double; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclDoubleIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, nil)) then
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
        Bucket := TJclDoubleIntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.Remove(const Key: Double): IInterface;
var
  Bucket: TJclDoubleIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclDoubleIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclDoubleIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclDoubleIntfHashMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclDoubleIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclDoubleIntfHashMap.KeysEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclDoubleIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

//=== { TJclIntfDoubleBucket } ==========================================

procedure TJclIntfDoubleBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0.0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0.0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclIntfDoubleHashMap } ==========================================

constructor TJclIntfDoubleHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfDoubleHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfDoubleHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfDoubleBucket;
  ADest: TJclIntfDoubleHashMap;
  AMap: IJclIntfDoubleMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfDoubleHashMap then
    begin
      ADest := TJclIntfDoubleHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfDoubleBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclIntfDoubleMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfDoubleHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfDoubleHashMap then
    TJclIntfDoubleHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfDoubleHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfDoubleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.ContainsValue(const Value: Double): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.MapEquals(const AMap: IJclIntfDoubleMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.GetValue(const Key: IInterface): Double;
var
  I: Integer;
  Bucket: TJclIntfDoubleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfDoubleHashMap.KeyOfValue(const Value: Double): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfDoubleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfDoubleHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfDoubleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfDoubleHashMap.PutAll(const AMap: IJclIntfDoubleMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfDoubleHashMap.PutValue(const Key: IInterface; const Value: Double);
var
  Index: Integer;
  Bucket: TJclIntfDoubleBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, 0.0)) then
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
        Bucket := TJclIntfDoubleBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.Remove(const Key: IInterface): Double;
var
  Bucket: TJclIntfDoubleBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfDoubleHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfDoubleHashMap.Values: IJclDoubleCollection;
var
  I, J: Integer;
  Bucket: TJclIntfDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfDoubleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfDoubleHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfDoubleHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfDoubleHashMap.FreeValue(var Value: Double): Double;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclIntfDoubleHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfDoubleHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfDoubleHashMap.ValuesEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclDoubleDoubleBucket } ==========================================

procedure TJclDoubleDoubleBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := 0.0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := 0.0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclDoubleDoubleHashMap } ==========================================

constructor TJclDoubleDoubleHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclDoubleDoubleHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclDoubleDoubleHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclDoubleDoubleBucket;
  ADest: TJclDoubleDoubleHashMap;
  AMap: IJclDoubleDoubleMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclDoubleDoubleHashMap then
    begin
      ADest := TJclDoubleDoubleHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclDoubleDoubleBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclDoubleDoubleMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleDoubleHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclDoubleDoubleHashMap then
    TJclDoubleDoubleHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclDoubleDoubleHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclDoubleDoubleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.ContainsKey(const Key: Double): Boolean;
var
  I: Integer;
  Bucket: TJclDoubleDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.ContainsValue(const Value: Double): Boolean;
var
  I, J: Integer;
  Bucket: TJclDoubleDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.MapEquals(const AMap: IJclDoubleDoubleMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclDoubleDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.GetValue(const Key: Double): Double;
var
  I: Integer;
  Bucket: TJclDoubleDoubleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclDoubleDoubleHashMap.KeyOfValue(const Value: Double): Double;
var
  I, J: Integer;
  Bucket: TJclDoubleDoubleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.KeySet: IJclDoubleSet;
var
  I, J: Integer;
  Bucket: TJclDoubleDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleDoubleHashMap.Pack;
var
  I: Integer;
  Bucket: TJclDoubleDoubleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleDoubleHashMap.PutAll(const AMap: IJclDoubleDoubleMap);
var
  It: IJclDoubleIterator;
  Key: Double;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleDoubleHashMap.PutValue(const Key: Double; const Value: Double);
var
  Index: Integer;
  Bucket: TJclDoubleDoubleBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, 0.0)) then
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
        Bucket := TJclDoubleDoubleBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.Remove(const Key: Double): Double;
var
  Bucket: TJclDoubleDoubleBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleDoubleHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclDoubleDoubleHashMap.Values: IJclDoubleCollection;
var
  I, J: Integer;
  Bucket: TJclDoubleDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleDoubleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleDoubleHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclDoubleDoubleHashMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclDoubleDoubleHashMap.FreeValue(var Value: Double): Double;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclDoubleDoubleHashMap.KeysEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclDoubleDoubleHashMap.ValuesEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclExtendedIntfBucket } ==========================================

procedure TJclExtendedIntfBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclExtendedIntfHashMap } ==========================================

constructor TJclExtendedIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclExtendedIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclExtendedIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclExtendedIntfBucket;
  ADest: TJclExtendedIntfHashMap;
  AMap: IJclExtendedIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclExtendedIntfHashMap then
    begin
      ADest := TJclExtendedIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclExtendedIntfBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclExtendedIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclExtendedIntfHashMap then
    TJclExtendedIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclExtendedIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclExtendedIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.ContainsKey(const Key: Extended): Boolean;
var
  I: Integer;
  Bucket: TJclExtendedIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclExtendedIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.MapEquals(const AMap: IJclExtendedIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclExtendedIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.GetValue(const Key: Extended): IInterface;
var
  I: Integer;
  Bucket: TJclExtendedIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclExtendedIntfHashMap.KeyOfValue(const Value: IInterface): Extended;
var
  I, J: Integer;
  Bucket: TJclExtendedIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.KeySet: IJclExtendedSet;
var
  I, J: Integer;
  Bucket: TJclExtendedIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclExtendedIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedIntfHashMap.PutAll(const AMap: IJclExtendedIntfMap);
var
  It: IJclExtendedIterator;
  Key: Extended;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedIntfHashMap.PutValue(const Key: Extended; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclExtendedIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, nil)) then
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
        Bucket := TJclExtendedIntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.Remove(const Key: Extended): IInterface;
var
  Bucket: TJclExtendedIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclExtendedIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclExtendedIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclExtendedIntfHashMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclExtendedIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclExtendedIntfHashMap.KeysEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclExtendedIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

//=== { TJclIntfExtendedBucket } ==========================================

procedure TJclIntfExtendedBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0.0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0.0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclIntfExtendedHashMap } ==========================================

constructor TJclIntfExtendedHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfExtendedHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfExtendedHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfExtendedBucket;
  ADest: TJclIntfExtendedHashMap;
  AMap: IJclIntfExtendedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfExtendedHashMap then
    begin
      ADest := TJclIntfExtendedHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfExtendedBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclIntfExtendedMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfExtendedHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfExtendedHashMap then
    TJclIntfExtendedHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfExtendedHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfExtendedBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.ContainsValue(const Value: Extended): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.MapEquals(const AMap: IJclIntfExtendedMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.GetValue(const Key: IInterface): Extended;
var
  I: Integer;
  Bucket: TJclIntfExtendedBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfExtendedHashMap.KeyOfValue(const Value: Extended): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfExtendedBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfExtendedHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfExtendedBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfExtendedHashMap.PutAll(const AMap: IJclIntfExtendedMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfExtendedHashMap.PutValue(const Key: IInterface; const Value: Extended);
var
  Index: Integer;
  Bucket: TJclIntfExtendedBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, 0.0)) then
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
        Bucket := TJclIntfExtendedBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.Remove(const Key: IInterface): Extended;
var
  Bucket: TJclIntfExtendedBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfExtendedHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfExtendedHashMap.Values: IJclExtendedCollection;
var
  I, J: Integer;
  Bucket: TJclIntfExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfExtendedHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfExtendedHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfExtendedHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfExtendedHashMap.FreeValue(var Value: Extended): Extended;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclIntfExtendedHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfExtendedHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfExtendedHashMap.ValuesEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclExtendedExtendedBucket } ==========================================

procedure TJclExtendedExtendedBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := 0.0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := 0.0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := 0.0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclExtendedExtendedHashMap } ==========================================

constructor TJclExtendedExtendedHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclExtendedExtendedHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclExtendedExtendedHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclExtendedExtendedBucket;
  ADest: TJclExtendedExtendedHashMap;
  AMap: IJclExtendedExtendedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclExtendedExtendedHashMap then
    begin
      ADest := TJclExtendedExtendedHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclExtendedExtendedBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclExtendedExtendedMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedExtendedHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclExtendedExtendedHashMap then
    TJclExtendedExtendedHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclExtendedExtendedHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclExtendedExtendedBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.ContainsKey(const Key: Extended): Boolean;
var
  I: Integer;
  Bucket: TJclExtendedExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.ContainsValue(const Value: Extended): Boolean;
var
  I, J: Integer;
  Bucket: TJclExtendedExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.MapEquals(const AMap: IJclExtendedExtendedMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclExtendedExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.GetValue(const Key: Extended): Extended;
var
  I: Integer;
  Bucket: TJclExtendedExtendedBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclExtendedExtendedHashMap.KeyOfValue(const Value: Extended): Extended;
var
  I, J: Integer;
  Bucket: TJclExtendedExtendedBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.KeySet: IJclExtendedSet;
var
  I, J: Integer;
  Bucket: TJclExtendedExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedExtendedHashMap.Pack;
var
  I: Integer;
  Bucket: TJclExtendedExtendedBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedExtendedHashMap.PutAll(const AMap: IJclExtendedExtendedMap);
var
  It: IJclExtendedIterator;
  Key: Extended;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedExtendedHashMap.PutValue(const Key: Extended; const Value: Extended);
var
  Index: Integer;
  Bucket: TJclExtendedExtendedBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, 0.0)) then
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
        Bucket := TJclExtendedExtendedBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.Remove(const Key: Extended): Extended;
var
  Bucket: TJclExtendedExtendedBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0.0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedExtendedHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclExtendedExtendedHashMap.Values: IJclExtendedCollection;
var
  I, J: Integer;
  Bucket: TJclExtendedExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedExtendedHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedExtendedHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclExtendedExtendedHashMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclExtendedExtendedHashMap.FreeValue(var Value: Extended): Extended;
begin
  Result := Value;
  Value := 0.0;
end;

function TJclExtendedExtendedHashMap.KeysEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclExtendedExtendedHashMap.ValuesEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclIntegerIntfBucket } ==========================================

procedure TJclIntegerIntfBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclIntegerIntfHashMap } ==========================================

constructor TJclIntegerIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntegerIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntegerIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntegerIntfBucket;
  ADest: TJclIntegerIntfHashMap;
  AMap: IJclIntegerIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntegerIntfHashMap then
    begin
      ADest := TJclIntegerIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntegerIntfBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclIntegerIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntegerIntfHashMap then
    TJclIntegerIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntegerIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntegerIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.ContainsKey(Key: Integer): Boolean;
var
  I: Integer;
  Bucket: TJclIntegerIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntegerIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.MapEquals(const AMap: IJclIntegerIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntegerIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.GetValue(Key: Integer): IInterface;
var
  I: Integer;
  Bucket: TJclIntegerIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntegerIntfHashMap.KeyOfValue(const Value: IInterface): Integer;
var
  I, J: Integer;
  Bucket: TJclIntegerIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.KeySet: IJclIntegerSet;
var
  I, J: Integer;
  Bucket: TJclIntegerIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntegerIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntfHashMap.PutAll(const AMap: IJclIntegerIntfMap);
var
  It: IJclIntegerIterator;
  Key: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntfHashMap.PutValue(Key: Integer; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclIntegerIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, nil)) then
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
        Bucket := TJclIntegerIntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.Remove(Key: Integer): IInterface;
var
  Bucket: TJclIntegerIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntegerIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclIntegerIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntegerIntfHashMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;

function TJclIntegerIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclIntegerIntfHashMap.KeysEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclIntegerIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

//=== { TJclIntfIntegerBucket } ==========================================

procedure TJclIntfIntegerBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclIntfIntegerHashMap } ==========================================

constructor TJclIntfIntegerHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfIntegerHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfIntegerHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfIntegerBucket;
  ADest: TJclIntfIntegerHashMap;
  AMap: IJclIntfIntegerMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfIntegerHashMap then
    begin
      ADest := TJclIntfIntegerHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfIntegerBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclIntfIntegerMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntegerHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfIntegerHashMap then
    TJclIntfIntegerHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfIntegerHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfIntegerBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.ContainsValue(Value: Integer): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.MapEquals(const AMap: IJclIntfIntegerMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.GetValue(const Key: IInterface): Integer;
var
  I: Integer;
  Bucket: TJclIntfIntegerBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfIntegerHashMap.KeyOfValue(Value: Integer): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfIntegerBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntegerHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfIntegerBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntegerHashMap.PutAll(const AMap: IJclIntfIntegerMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntegerHashMap.PutValue(const Key: IInterface; Value: Integer);
var
  Index: Integer;
  Bucket: TJclIntfIntegerBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, 0)) then
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
        Bucket := TJclIntfIntegerBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.Remove(const Key: IInterface): Integer;
var
  Bucket: TJclIntfIntegerBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntegerHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfIntegerHashMap.Values: IJclIntegerCollection;
var
  I, J: Integer;
  Bucket: TJclIntfIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntegerHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfIntegerHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfIntegerHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfIntegerHashMap.FreeValue(var Value: Integer): Integer;
begin
  Result := Value;
  Value := 0;
end;

function TJclIntfIntegerHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfIntegerHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfIntegerHashMap.ValuesEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclIntegerIntegerBucket } ==========================================

procedure TJclIntegerIntegerBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := 0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := 0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclIntegerIntegerHashMap } ==========================================

constructor TJclIntegerIntegerHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntegerIntegerHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntegerIntegerHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntegerIntegerBucket;
  ADest: TJclIntegerIntegerHashMap;
  AMap: IJclIntegerIntegerMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntegerIntegerHashMap then
    begin
      ADest := TJclIntegerIntegerHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntegerIntegerBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclIntegerIntegerMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntegerHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntegerIntegerHashMap then
    TJclIntegerIntegerHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntegerIntegerHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntegerIntegerBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.ContainsKey(Key: Integer): Boolean;
var
  I: Integer;
  Bucket: TJclIntegerIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.ContainsValue(Value: Integer): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntegerIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.MapEquals(const AMap: IJclIntegerIntegerMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntegerIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.GetValue(Key: Integer): Integer;
var
  I: Integer;
  Bucket: TJclIntegerIntegerBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntegerIntegerHashMap.KeyOfValue(Value: Integer): Integer;
var
  I, J: Integer;
  Bucket: TJclIntegerIntegerBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.KeySet: IJclIntegerSet;
var
  I, J: Integer;
  Bucket: TJclIntegerIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntegerHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntegerIntegerBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntegerHashMap.PutAll(const AMap: IJclIntegerIntegerMap);
var
  It: IJclIntegerIterator;
  Key: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntegerHashMap.PutValue(Key: Integer; Value: Integer);
var
  Index: Integer;
  Bucket: TJclIntegerIntegerBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, 0)) then
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
        Bucket := TJclIntegerIntegerBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.Remove(Key: Integer): Integer;
var
  Bucket: TJclIntegerIntegerBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerIntegerHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntegerIntegerHashMap.Values: IJclIntegerCollection;
var
  I, J: Integer;
  Bucket: TJclIntegerIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerIntegerHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerIntegerHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntegerIntegerHashMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;

function TJclIntegerIntegerHashMap.FreeValue(var Value: Integer): Integer;
begin
  Result := Value;
  Value := 0;
end;

function TJclIntegerIntegerHashMap.KeysEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclIntegerIntegerHashMap.ValuesEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclCardinalIntfBucket } ==========================================

procedure TJclCardinalIntfBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclCardinalIntfHashMap } ==========================================

constructor TJclCardinalIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclCardinalIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclCardinalIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclCardinalIntfBucket;
  ADest: TJclCardinalIntfHashMap;
  AMap: IJclCardinalIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclCardinalIntfHashMap then
    begin
      ADest := TJclCardinalIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclCardinalIntfBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclCardinalIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclCardinalIntfHashMap then
    TJclCardinalIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclCardinalIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclCardinalIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.ContainsKey(Key: Cardinal): Boolean;
var
  I: Integer;
  Bucket: TJclCardinalIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclCardinalIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.MapEquals(const AMap: IJclCardinalIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclCardinalIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.GetValue(Key: Cardinal): IInterface;
var
  I: Integer;
  Bucket: TJclCardinalIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclCardinalIntfHashMap.KeyOfValue(const Value: IInterface): Cardinal;
var
  I, J: Integer;
  Bucket: TJclCardinalIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.KeySet: IJclCardinalSet;
var
  I, J: Integer;
  Bucket: TJclCardinalIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclCardinalIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalIntfHashMap.PutAll(const AMap: IJclCardinalIntfMap);
var
  It: IJclCardinalIterator;
  Key: Cardinal;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalIntfHashMap.PutValue(Key: Cardinal; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclCardinalIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, nil)) then
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
        Bucket := TJclCardinalIntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.Remove(Key: Cardinal): IInterface;
var
  Bucket: TJclCardinalIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclCardinalIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclCardinalIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclCardinalIntfHashMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;

function TJclCardinalIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclCardinalIntfHashMap.KeysEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclCardinalIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

//=== { TJclIntfCardinalBucket } ==========================================

procedure TJclIntfCardinalBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclIntfCardinalHashMap } ==========================================

constructor TJclIntfCardinalHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfCardinalHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfCardinalHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfCardinalBucket;
  ADest: TJclIntfCardinalHashMap;
  AMap: IJclIntfCardinalMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfCardinalHashMap then
    begin
      ADest := TJclIntfCardinalHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfCardinalBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclIntfCardinalMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfCardinalHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfCardinalHashMap then
    TJclIntfCardinalHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfCardinalHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfCardinalBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.ContainsValue(Value: Cardinal): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.MapEquals(const AMap: IJclIntfCardinalMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.GetValue(const Key: IInterface): Cardinal;
var
  I: Integer;
  Bucket: TJclIntfCardinalBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfCardinalHashMap.KeyOfValue(Value: Cardinal): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfCardinalBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfCardinalHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfCardinalBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfCardinalHashMap.PutAll(const AMap: IJclIntfCardinalMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfCardinalHashMap.PutValue(const Key: IInterface; Value: Cardinal);
var
  Index: Integer;
  Bucket: TJclIntfCardinalBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, 0)) then
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
        Bucket := TJclIntfCardinalBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.Remove(const Key: IInterface): Cardinal;
var
  Bucket: TJclIntfCardinalBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfCardinalHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfCardinalHashMap.Values: IJclCardinalCollection;
var
  I, J: Integer;
  Bucket: TJclIntfCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfCardinalHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfCardinalHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfCardinalHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfCardinalHashMap.FreeValue(var Value: Cardinal): Cardinal;
begin
  Result := Value;
  Value := 0;
end;

function TJclIntfCardinalHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfCardinalHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfCardinalHashMap.ValuesEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclCardinalCardinalBucket } ==========================================

procedure TJclCardinalCardinalBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := 0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := 0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclCardinalCardinalHashMap } ==========================================

constructor TJclCardinalCardinalHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclCardinalCardinalHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclCardinalCardinalHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclCardinalCardinalBucket;
  ADest: TJclCardinalCardinalHashMap;
  AMap: IJclCardinalCardinalMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclCardinalCardinalHashMap then
    begin
      ADest := TJclCardinalCardinalHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclCardinalCardinalBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclCardinalCardinalMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalCardinalHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclCardinalCardinalHashMap then
    TJclCardinalCardinalHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclCardinalCardinalHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclCardinalCardinalBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.ContainsKey(Key: Cardinal): Boolean;
var
  I: Integer;
  Bucket: TJclCardinalCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.ContainsValue(Value: Cardinal): Boolean;
var
  I, J: Integer;
  Bucket: TJclCardinalCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.MapEquals(const AMap: IJclCardinalCardinalMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclCardinalCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.GetValue(Key: Cardinal): Cardinal;
var
  I: Integer;
  Bucket: TJclCardinalCardinalBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclCardinalCardinalHashMap.KeyOfValue(Value: Cardinal): Cardinal;
var
  I, J: Integer;
  Bucket: TJclCardinalCardinalBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.KeySet: IJclCardinalSet;
var
  I, J: Integer;
  Bucket: TJclCardinalCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalCardinalHashMap.Pack;
var
  I: Integer;
  Bucket: TJclCardinalCardinalBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalCardinalHashMap.PutAll(const AMap: IJclCardinalCardinalMap);
var
  It: IJclCardinalIterator;
  Key: Cardinal;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalCardinalHashMap.PutValue(Key: Cardinal; Value: Cardinal);
var
  Index: Integer;
  Bucket: TJclCardinalCardinalBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, 0)) then
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
        Bucket := TJclCardinalCardinalBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.Remove(Key: Cardinal): Cardinal;
var
  Bucket: TJclCardinalCardinalBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalCardinalHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclCardinalCardinalHashMap.Values: IJclCardinalCollection;
var
  I, J: Integer;
  Bucket: TJclCardinalCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalCardinalHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalCardinalHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclCardinalCardinalHashMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;

function TJclCardinalCardinalHashMap.FreeValue(var Value: Cardinal): Cardinal;
begin
  Result := Value;
  Value := 0;
end;

function TJclCardinalCardinalHashMap.KeysEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclCardinalCardinalHashMap.ValuesEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclInt64IntfBucket } ==========================================

procedure TJclInt64IntfBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclInt64IntfHashMap } ==========================================

constructor TJclInt64IntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclInt64IntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclInt64IntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclInt64IntfBucket;
  ADest: TJclInt64IntfHashMap;
  AMap: IJclInt64IntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclInt64IntfHashMap then
    begin
      ADest := TJclInt64IntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclInt64IntfBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclInt64IntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64IntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclInt64IntfHashMap then
    TJclInt64IntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclInt64IntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclInt64IntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.ContainsKey(const Key: Int64): Boolean;
var
  I: Integer;
  Bucket: TJclInt64IntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclInt64IntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.MapEquals(const AMap: IJclInt64IntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclInt64IntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.GetValue(const Key: Int64): IInterface;
var
  I: Integer;
  Bucket: TJclInt64IntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclInt64IntfHashMap.KeyOfValue(const Value: IInterface): Int64;
var
  I, J: Integer;
  Bucket: TJclInt64IntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.KeySet: IJclInt64Set;
var
  I, J: Integer;
  Bucket: TJclInt64IntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64IntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclInt64IntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64IntfHashMap.PutAll(const AMap: IJclInt64IntfMap);
var
  It: IJclInt64Iterator;
  Key: Int64;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64IntfHashMap.PutValue(const Key: Int64; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclInt64IntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, nil)) then
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
        Bucket := TJclInt64IntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.Remove(const Key: Int64): IInterface;
var
  Bucket: TJclInt64IntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64IntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclInt64IntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclInt64IntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64IntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64IntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclInt64IntfHashMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;

function TJclInt64IntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclInt64IntfHashMap.KeysEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclInt64IntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

//=== { TJclIntfInt64Bucket } ==========================================

procedure TJclIntfInt64Bucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := 0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclIntfInt64HashMap } ==========================================

constructor TJclIntfInt64HashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfInt64HashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfInt64HashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfInt64Bucket;
  ADest: TJclIntfInt64HashMap;
  AMap: IJclIntfInt64Map;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfInt64HashMap then
    begin
      ADest := TJclIntfInt64HashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfInt64Bucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclIntfInt64Map, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfInt64HashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfInt64HashMap then
    TJclIntfInt64HashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfInt64HashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfInt64Bucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfInt64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.ContainsValue(const Value: Int64): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfInt64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.MapEquals(const AMap: IJclIntfInt64Map): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfInt64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.GetValue(const Key: IInterface): Int64;
var
  I: Integer;
  Bucket: TJclIntfInt64Bucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfInt64HashMap.KeyOfValue(const Value: Int64): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfInt64Bucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfInt64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfInt64HashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfInt64Bucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfInt64HashMap.PutAll(const AMap: IJclIntfInt64Map);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfInt64HashMap.PutValue(const Key: IInterface; const Value: Int64);
var
  Index: Integer;
  Bucket: TJclIntfInt64Bucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, nil) and not ValuesEqual(Value, 0)) then
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
        Bucket := TJclIntfInt64Bucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.Remove(const Key: IInterface): Int64;
var
  Bucket: TJclIntfInt64Bucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfInt64HashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfInt64HashMap.Values: IJclInt64Collection;
var
  I, J: Integer;
  Bucket: TJclIntfInt64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfInt64HashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfInt64HashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfInt64HashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfInt64HashMap.FreeValue(var Value: Int64): Int64;
begin
  Result := Value;
  Value := 0;
end;

function TJclIntfInt64HashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfInt64HashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfInt64HashMap.ValuesEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclInt64Int64Bucket } ==========================================

procedure TJclInt64Int64Bucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := 0;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := 0;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := 0;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclInt64Int64HashMap } ==========================================

constructor TJclInt64Int64HashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclInt64Int64HashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclInt64Int64HashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclInt64Int64Bucket;
  ADest: TJclInt64Int64HashMap;
  AMap: IJclInt64Int64Map;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclInt64Int64HashMap then
    begin
      ADest := TJclInt64Int64HashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclInt64Int64Bucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclInt64Int64Map, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Int64HashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclInt64Int64HashMap then
    TJclInt64Int64HashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclInt64Int64HashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclInt64Int64Bucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.ContainsKey(const Key: Int64): Boolean;
var
  I: Integer;
  Bucket: TJclInt64Int64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.ContainsValue(const Value: Int64): Boolean;
var
  I, J: Integer;
  Bucket: TJclInt64Int64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.MapEquals(const AMap: IJclInt64Int64Map): Boolean;
var
  I, J: Integer;
  Bucket: TJclInt64Int64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.GetValue(const Key: Int64): Int64;
var
  I: Integer;
  Bucket: TJclInt64Int64Bucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclInt64Int64HashMap.KeyOfValue(const Value: Int64): Int64;
var
  I, J: Integer;
  Bucket: TJclInt64Int64Bucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.KeySet: IJclInt64Set;
var
  I, J: Integer;
  Bucket: TJclInt64Int64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Int64HashMap.Pack;
var
  I: Integer;
  Bucket: TJclInt64Int64Bucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Int64HashMap.PutAll(const AMap: IJclInt64Int64Map);
var
  It: IJclInt64Iterator;
  Key: Int64;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Int64HashMap.PutValue(const Key: Int64; const Value: Int64);
var
  Index: Integer;
  Bucket: TJclInt64Int64Bucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, 0)) then
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
        Bucket := TJclInt64Int64Bucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.Remove(const Key: Int64): Int64;
var
  Bucket: TJclInt64Int64Bucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    Result := 0;
    Bucket := FBuckets[FHashFunction(Hash(Key), FCapacity)];
    if Bucket <> nil then
    begin
      for I := 0 to Bucket.Size - 1 do
        if KeysEqual(Bucket.Entries[I].Key, Key) then
        begin
          Result := FreeValue(Bucket.Entries[I].Value);
          FreeKey(Bucket.Entries[I].Key);
          if I < Length(Bucket.Entries) - 1 then
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64Int64HashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclInt64Int64HashMap.Values: IJclInt64Collection;
var
  I, J: Integer;
  Bucket: TJclInt64Int64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64Int64HashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64Int64HashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclInt64Int64HashMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;

function TJclInt64Int64HashMap.FreeValue(var Value: Int64): Int64;
begin
  Result := Value;
  Value := 0;
end;

function TJclInt64Int64HashMap.KeysEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclInt64Int64HashMap.ValuesEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

{$IFNDEF CLR}
//=== { TJclPtrIntfBucket } ==========================================

procedure TJclPtrIntfBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclPtrIntfHashMap } ==========================================

constructor TJclPtrIntfHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclPtrIntfHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclPtrIntfHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclPtrIntfBucket;
  ADest: TJclPtrIntfHashMap;
  AMap: IJclPtrIntfMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclPtrIntfHashMap then
    begin
      ADest := TJclPtrIntfHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclPtrIntfBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclPtrIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrIntfHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclPtrIntfHashMap then
    TJclPtrIntfHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclPtrIntfHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclPtrIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.ContainsKey(Key: Pointer): Boolean;
var
  I: Integer;
  Bucket: TJclPtrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.ContainsValue(const Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: TJclPtrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.MapEquals(const AMap: IJclPtrIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclPtrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.GetValue(Key: Pointer): IInterface;
var
  I: Integer;
  Bucket: TJclPtrIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclPtrIntfHashMap.KeyOfValue(const Value: IInterface): Pointer;
var
  I, J: Integer;
  Bucket: TJclPtrIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.KeySet: IJclPtrSet;
var
  I, J: Integer;
  Bucket: TJclPtrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclPtrIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrIntfHashMap.PutAll(const AMap: IJclPtrIntfMap);
var
  It: IJclPtrIterator;
  Key: Pointer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrIntfHashMap.PutValue(Key: Pointer; const Value: IInterface);
var
  Index: Integer;
  Bucket: TJclPtrIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
        Bucket := TJclPtrIntfBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.Remove(Key: Pointer): IInterface;
var
  Bucket: TJclPtrIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclPtrIntfHashMap.Values: IJclIntfCollection;
var
  I, J: Integer;
  Bucket: TJclPtrIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrIntfHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclPtrIntfHashMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;

function TJclPtrIntfHashMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclPtrIntfHashMap.KeysEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclPtrIntfHashMap.ValuesEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

//=== { TJclIntfPtrBucket } ==========================================

procedure TJclIntfPtrBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclIntfPtrHashMap } ==========================================

constructor TJclIntfPtrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfPtrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntfPtrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntfPtrBucket;
  ADest: TJclIntfPtrHashMap;
  AMap: IJclIntfPtrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfPtrHashMap then
    begin
      ADest := TJclIntfPtrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntfPtrBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclIntfPtrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfPtrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntfPtrHashMap then
    TJclIntfPtrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntfPtrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntfPtrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.ContainsValue(Value: Pointer): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.MapEquals(const AMap: IJclIntfPtrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.GetValue(const Key: IInterface): Pointer;
var
  I: Integer;
  Bucket: TJclIntfPtrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfPtrHashMap.KeyOfValue(Value: Pointer): IInterface;
var
  I, J: Integer;
  Bucket: TJclIntfPtrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfPtrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfPtrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfPtrHashMap.PutAll(const AMap: IJclIntfPtrMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfPtrHashMap.PutValue(const Key: IInterface; Value: Pointer);
var
  Index: Integer;
  Bucket: TJclIntfPtrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
        Bucket := TJclIntfPtrBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.Remove(const Key: IInterface): Pointer;
var
  Bucket: TJclIntfPtrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfPtrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfPtrHashMap.Values: IJclPtrCollection;
var
  I, J: Integer;
  Bucket: TJclIntfPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfPtrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfPtrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclIntfPtrHashMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfPtrHashMap.FreeValue(var Value: Pointer): Pointer;
begin
  Result := Value;
  Value := nil;
end;

function TJclIntfPtrHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfPtrHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfPtrHashMap.ValuesEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

//=== { TJclPtrPtrBucket } ==========================================

procedure TJclPtrPtrBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclPtrPtrHashMap } ==========================================

constructor TJclPtrPtrHashMap.Create(ACapacity: Integer);
begin
  inherited Create;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclPtrPtrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclPtrPtrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclPtrPtrBucket;
  ADest: TJclPtrPtrHashMap;
  AMap: IJclPtrPtrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclPtrPtrHashMap then
    begin
      ADest := TJclPtrPtrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclPtrPtrBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclPtrPtrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrPtrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclPtrPtrHashMap then
    TJclPtrPtrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclPtrPtrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclPtrPtrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.ContainsKey(Key: Pointer): Boolean;
var
  I: Integer;
  Bucket: TJclPtrPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.ContainsValue(Value: Pointer): Boolean;
var
  I, J: Integer;
  Bucket: TJclPtrPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.MapEquals(const AMap: IJclPtrPtrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclPtrPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.GetValue(Key: Pointer): Pointer;
var
  I: Integer;
  Bucket: TJclPtrPtrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclPtrPtrHashMap.KeyOfValue(Value: Pointer): Pointer;
var
  I, J: Integer;
  Bucket: TJclPtrPtrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.KeySet: IJclPtrSet;
var
  I, J: Integer;
  Bucket: TJclPtrPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrPtrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclPtrPtrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrPtrHashMap.PutAll(const AMap: IJclPtrPtrMap);
var
  It: IJclPtrIterator;
  Key: Pointer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrPtrHashMap.PutValue(Key: Pointer; Value: Pointer);
var
  Index: Integer;
  Bucket: TJclPtrPtrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
        Bucket := TJclPtrPtrBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.Remove(Key: Pointer): Pointer;
var
  Bucket: TJclPtrPtrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrPtrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclPtrPtrHashMap.Values: IJclPtrCollection;
var
  I, J: Integer;
  Bucket: TJclPtrPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArrayList.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Value);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrPtrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrPtrHashMap.Create(FCapacity);
  AssignPropertiesTo(Result);
end;

function TJclPtrPtrHashMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;

function TJclPtrPtrHashMap.FreeValue(var Value: Pointer): Pointer;
begin
  Result := Value;
  Value := nil;
end;

function TJclPtrPtrHashMap.KeysEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclPtrPtrHashMap.ValuesEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;
{$ENDIF ~CLR}

//=== { TJclIntfBucket } ==========================================

procedure TJclIntfBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclIntfHashMap } ==========================================

constructor TJclIntfHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntfHashMap.Destroy;
begin
  FReadOnly := False;
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
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntfHashMap then
    begin
      ADest := TJclIntfHashMap(Dest);
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
    end
    else
    if Supports(IInterface(Dest), IJclIntfMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.ContainsKey(const Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: TJclIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.MapEquals(const AMap: IJclIntfMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.GetValue(const Key: IInterface): TObject;
var
  I: Integer;
  Bucket: TJclIntfBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.KeySet: IJclIntfSet;
var
  I, J: Integer;
  Bucket: TJclIntfBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntfBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashMap.PutAll(const AMap: IJclIntfMap);
var
  It: IJclIntfIterator;
  Key: IInterface;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashMap.PutValue(const Key: IInterface; Value: TObject);
var
  Index: Integer;
  Bucket: TJclIntfBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.Remove(const Key: IInterface): TObject;
var
  Bucket: TJclIntfBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
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

function TJclIntfHashMap.Hash(const AInterface: IInterface): Integer;
begin
  Result := Integer(AInterface);
end;

function TJclIntfHashMap.KeysEqual(const A, B: IInterface): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclIntfHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

//=== { TJclAnsiStrBucket } ==========================================

procedure TJclAnsiStrBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclAnsiStrHashMap } ==========================================

constructor TJclAnsiStrHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclAnsiStrHashMap.Destroy;
begin
  FReadOnly := False;
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
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclAnsiStrHashMap then
    begin
      ADest := TJclAnsiStrHashMap(Dest);
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
    end
    else
    if Supports(IInterface(Dest), IJclAnsiStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashMap.ContainsKey(const Key: AnsiString): Boolean;
var
  I: Integer;
  Bucket: TJclAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashMap.MapEquals(const AMap: IJclAnsiStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashMap.GetValue(const Key: AnsiString): TObject;
var
  I: Integer;
  Bucket: TJclAnsiStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashMap.KeySet: IJclAnsiStrSet;
var
  I, J: Integer;
  Bucket: TJclAnsiStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclAnsiStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrHashMap.PutAll(const AMap: IJclAnsiStrMap);
var
  It: IJclAnsiStrIterator;
  Key: AnsiString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrHashMap.PutValue(const Key: AnsiString; Value: TObject);
var
  Index: Integer;
  Bucket: TJclAnsiStrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashMap.Remove(const Key: AnsiString): TObject;
var
  Bucket: TJclAnsiStrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
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

function TJclAnsiStrHashMap.KeysEqual(const A, B: AnsiString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclAnsiStrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

//=== { TJclWideStrBucket } ==========================================

procedure TJclWideStrBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclWideStrHashMap } ==========================================

constructor TJclWideStrHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclWideStrHashMap.Destroy;
begin
  FReadOnly := False;
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
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclWideStrHashMap then
    begin
      ADest := TJclWideStrHashMap(Dest);
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
    end
    else
    if Supports(IInterface(Dest), IJclWideStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashMap.ContainsKey(const Key: WideString): Boolean;
var
  I: Integer;
  Bucket: TJclWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashMap.MapEquals(const AMap: IJclWideStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashMap.GetValue(const Key: WideString): TObject;
var
  I: Integer;
  Bucket: TJclWideStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashMap.KeySet: IJclWideStrSet;
var
  I, J: Integer;
  Bucket: TJclWideStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclWideStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrHashMap.PutAll(const AMap: IJclWideStrMap);
var
  It: IJclWideStrIterator;
  Key: WideString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrHashMap.PutValue(const Key: WideString; Value: TObject);
var
  Index: Integer;
  Bucket: TJclWideStrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashMap.Remove(const Key: WideString): TObject;
var
  Bucket: TJclWideStrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
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

function TJclWideStrHashMap.KeysEqual(const A, B: WideString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclWideStrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
//=== { TJclUnicodeStrBucket } ==========================================

procedure TJclUnicodeStrBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := '';
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclUnicodeStrHashMap } ==========================================

constructor TJclUnicodeStrHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclUnicodeStrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclUnicodeStrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclUnicodeStrBucket;
  ADest: TJclUnicodeStrHashMap;
  AMap: IJclUnicodeStrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclUnicodeStrHashMap then
    begin
      ADest := TJclUnicodeStrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclUnicodeStrBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclUnicodeStrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclUnicodeStrHashMap then
    TJclUnicodeStrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclUnicodeStrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashMap.ContainsKey(const Key: UnicodeString): Boolean;
var
  I: Integer;
  Bucket: TJclUnicodeStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashMap.MapEquals(const AMap: IJclUnicodeStrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashMap.GetValue(const Key: UnicodeString): TObject;
var
  I: Integer;
  Bucket: TJclUnicodeStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclUnicodeStrHashMap.KeyOfValue(Value: TObject): UnicodeString;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashMap.KeySet: IJclUnicodeStrSet;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclUnicodeStrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclUnicodeStrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrHashMap.PutAll(const AMap: IJclUnicodeStrMap);
var
  It: IJclUnicodeStrIterator;
  Key: UnicodeString;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrHashMap.PutValue(const Key: UnicodeString; Value: TObject);
var
  Index: Integer;
  Bucket: TJclUnicodeStrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
        Bucket := TJclUnicodeStrBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashMap.Remove(const Key: UnicodeString): TObject;
var
  Bucket: TJclUnicodeStrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclUnicodeStrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclUnicodeStrHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclUnicodeStrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclUnicodeStrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclUnicodeStrHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclUnicodeStrHashMap.FreeKey(var Key: UnicodeString): UnicodeString;
begin
  Result := Key;
  Key := '';
end;

function TJclUnicodeStrHashMap.FreeValue(var Value: TObject): TObject;
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

function TJclUnicodeStrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclUnicodeStrHashMap.KeysEqual(const A, B: UnicodeString): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclUnicodeStrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

//=== { TJclSingleBucket } ==========================================

procedure TJclSingleBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclSingleHashMap } ==========================================

constructor TJclSingleHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclSingleHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclSingleHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclSingleBucket;
  ADest: TJclSingleHashMap;
  AMap: IJclSingleMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclSingleHashMap then
    begin
      ADest := TJclSingleHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclSingleBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclSingleMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclSingleHashMap then
    TJclSingleHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclSingleHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclSingleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.ContainsKey(const Key: Single): Boolean;
var
  I: Integer;
  Bucket: TJclSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.MapEquals(const AMap: IJclSingleMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.GetValue(const Key: Single): TObject;
var
  I: Integer;
  Bucket: TJclSingleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclSingleHashMap.KeyOfValue(Value: TObject): Single;
var
  I, J: Integer;
  Bucket: TJclSingleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.KeySet: IJclSingleSet;
var
  I, J: Integer;
  Bucket: TJclSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclSingleArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleHashMap.Pack;
var
  I: Integer;
  Bucket: TJclSingleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleHashMap.PutAll(const AMap: IJclSingleMap);
var
  It: IJclSingleIterator;
  Key: Single;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleHashMap.PutValue(const Key: Single; Value: TObject);
var
  Index: Integer;
  Bucket: TJclSingleBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, nil)) then
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
        Bucket := TJclSingleBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.Remove(const Key: Single): TObject;
var
  Bucket: TJclSingleBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSingleHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclSingleHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclSingleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSingleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSingleHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclSingleHashMap.FreeKey(var Key: Single): Single;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclSingleHashMap.FreeValue(var Value: TObject): TObject;
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

function TJclSingleHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclSingleHashMap.KeysEqual(const A, B: Single): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclSingleHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

//=== { TJclDoubleBucket } ==========================================

procedure TJclDoubleBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclDoubleHashMap } ==========================================

constructor TJclDoubleHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclDoubleHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclDoubleHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclDoubleBucket;
  ADest: TJclDoubleHashMap;
  AMap: IJclDoubleMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclDoubleHashMap then
    begin
      ADest := TJclDoubleHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclDoubleBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclDoubleMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclDoubleHashMap then
    TJclDoubleHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclDoubleHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclDoubleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.ContainsKey(const Key: Double): Boolean;
var
  I: Integer;
  Bucket: TJclDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.MapEquals(const AMap: IJclDoubleMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.GetValue(const Key: Double): TObject;
var
  I: Integer;
  Bucket: TJclDoubleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclDoubleHashMap.KeyOfValue(Value: TObject): Double;
var
  I, J: Integer;
  Bucket: TJclDoubleBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.KeySet: IJclDoubleSet;
var
  I, J: Integer;
  Bucket: TJclDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclDoubleArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleHashMap.Pack;
var
  I: Integer;
  Bucket: TJclDoubleBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleHashMap.PutAll(const AMap: IJclDoubleMap);
var
  It: IJclDoubleIterator;
  Key: Double;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleHashMap.PutValue(const Key: Double; Value: TObject);
var
  Index: Integer;
  Bucket: TJclDoubleBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, nil)) then
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
        Bucket := TJclDoubleBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.Remove(const Key: Double): TObject;
var
  Bucket: TJclDoubleBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclDoubleHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclDoubleHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclDoubleBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclDoubleHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclDoubleHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclDoubleHashMap.FreeKey(var Key: Double): Double;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclDoubleHashMap.FreeValue(var Value: TObject): TObject;
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

function TJclDoubleHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclDoubleHashMap.KeysEqual(const A, B: Double): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclDoubleHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

//=== { TJclExtendedBucket } ==========================================

procedure TJclExtendedBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0.0;
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclExtendedHashMap } ==========================================

constructor TJclExtendedHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclExtendedHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclExtendedHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclExtendedBucket;
  ADest: TJclExtendedHashMap;
  AMap: IJclExtendedMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclExtendedHashMap then
    begin
      ADest := TJclExtendedHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclExtendedBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclExtendedMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclExtendedHashMap then
    TJclExtendedHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclExtendedHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclExtendedBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.ContainsKey(const Key: Extended): Boolean;
var
  I: Integer;
  Bucket: TJclExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.MapEquals(const AMap: IJclExtendedMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.GetValue(const Key: Extended): TObject;
var
  I: Integer;
  Bucket: TJclExtendedBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclExtendedHashMap.KeyOfValue(Value: TObject): Extended;
var
  I, J: Integer;
  Bucket: TJclExtendedBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0.0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.KeySet: IJclExtendedSet;
var
  I, J: Integer;
  Bucket: TJclExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclExtendedArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedHashMap.Pack;
var
  I: Integer;
  Bucket: TJclExtendedBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedHashMap.PutAll(const AMap: IJclExtendedMap);
var
  It: IJclExtendedIterator;
  Key: Extended;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedHashMap.PutValue(const Key: Extended; Value: TObject);
var
  Index: Integer;
  Bucket: TJclExtendedBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0.0) and not ValuesEqual(Value, nil)) then
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
        Bucket := TJclExtendedBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.Remove(const Key: Extended): TObject;
var
  Bucket: TJclExtendedBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclExtendedHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclExtendedHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclExtendedBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclExtendedHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclExtendedHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclExtendedHashMap.FreeKey(var Key: Extended): Extended;
begin
  Result := Key;
  Key := 0.0;
end;

function TJclExtendedHashMap.FreeValue(var Value: TObject): TObject;
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

function TJclExtendedHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclExtendedHashMap.KeysEqual(const A, B: Extended): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclExtendedHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

//=== { TJclIntegerBucket } ==========================================

procedure TJclIntegerBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclIntegerHashMap } ==========================================

constructor TJclIntegerHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclIntegerHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclIntegerHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclIntegerBucket;
  ADest: TJclIntegerHashMap;
  AMap: IJclIntegerMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclIntegerHashMap then
    begin
      ADest := TJclIntegerHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclIntegerBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclIntegerMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclIntegerHashMap then
    TJclIntegerHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclIntegerHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclIntegerBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.ContainsKey(Key: Integer): Boolean;
var
  I: Integer;
  Bucket: TJclIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.MapEquals(const AMap: IJclIntegerMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.GetValue(Key: Integer): TObject;
var
  I: Integer;
  Bucket: TJclIntegerBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntegerHashMap.KeyOfValue(Value: TObject): Integer;
var
  I, J: Integer;
  Bucket: TJclIntegerBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.KeySet: IJclIntegerSet;
var
  I, J: Integer;
  Bucket: TJclIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntegerArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerHashMap.Pack;
var
  I: Integer;
  Bucket: TJclIntegerBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerHashMap.PutAll(const AMap: IJclIntegerMap);
var
  It: IJclIntegerIterator;
  Key: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerHashMap.PutValue(Key: Integer; Value: TObject);
var
  Index: Integer;
  Bucket: TJclIntegerBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, nil)) then
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
        Bucket := TJclIntegerBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.Remove(Key: Integer): TObject;
var
  Bucket: TJclIntegerBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntegerHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntegerHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclIntegerBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntegerHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntegerHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclIntegerHashMap.FreeKey(var Key: Integer): Integer;
begin
  Result := Key;
  Key := 0;
end;

function TJclIntegerHashMap.FreeValue(var Value: TObject): TObject;
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

function TJclIntegerHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclIntegerHashMap.KeysEqual(A, B: Integer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclIntegerHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

//=== { TJclCardinalBucket } ==========================================

procedure TJclCardinalBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclCardinalHashMap } ==========================================

constructor TJclCardinalHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclCardinalHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclCardinalHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclCardinalBucket;
  ADest: TJclCardinalHashMap;
  AMap: IJclCardinalMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclCardinalHashMap then
    begin
      ADest := TJclCardinalHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclCardinalBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclCardinalMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclCardinalHashMap then
    TJclCardinalHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclCardinalHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclCardinalBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.ContainsKey(Key: Cardinal): Boolean;
var
  I: Integer;
  Bucket: TJclCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.MapEquals(const AMap: IJclCardinalMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.GetValue(Key: Cardinal): TObject;
var
  I: Integer;
  Bucket: TJclCardinalBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclCardinalHashMap.KeyOfValue(Value: TObject): Cardinal;
var
  I, J: Integer;
  Bucket: TJclCardinalBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.KeySet: IJclCardinalSet;
var
  I, J: Integer;
  Bucket: TJclCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclCardinalArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalHashMap.Pack;
var
  I: Integer;
  Bucket: TJclCardinalBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalHashMap.PutAll(const AMap: IJclCardinalMap);
var
  It: IJclCardinalIterator;
  Key: Cardinal;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalHashMap.PutValue(Key: Cardinal; Value: TObject);
var
  Index: Integer;
  Bucket: TJclCardinalBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, nil)) then
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
        Bucket := TJclCardinalBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.Remove(Key: Cardinal): TObject;
var
  Bucket: TJclCardinalBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclCardinalHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclCardinalHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclCardinalBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclCardinalHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclCardinalHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclCardinalHashMap.FreeKey(var Key: Cardinal): Cardinal;
begin
  Result := Key;
  Key := 0;
end;

function TJclCardinalHashMap.FreeValue(var Value: TObject): TObject;
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

function TJclCardinalHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclCardinalHashMap.KeysEqual(A, B: Cardinal): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclCardinalHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

//=== { TJclInt64Bucket } ==========================================

procedure TJclInt64Bucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := 0;
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclInt64HashMap } ==========================================

constructor TJclInt64HashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclInt64HashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclInt64HashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclInt64Bucket;
  ADest: TJclInt64HashMap;
  AMap: IJclInt64Map;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclInt64HashMap then
    begin
      ADest := TJclInt64HashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclInt64Bucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclInt64Map, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64HashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclInt64HashMap then
    TJclInt64HashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclInt64HashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclInt64Bucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.ContainsKey(const Key: Int64): Boolean;
var
  I: Integer;
  Bucket: TJclInt64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclInt64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.MapEquals(const AMap: IJclInt64Map): Boolean;
var
  I, J: Integer;
  Bucket: TJclInt64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.GetValue(const Key: Int64): TObject;
var
  I: Integer;
  Bucket: TJclInt64Bucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclInt64HashMap.KeyOfValue(Value: TObject): Int64;
var
  I, J: Integer;
  Bucket: TJclInt64Bucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := 0;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.KeySet: IJclInt64Set;
var
  I, J: Integer;
  Bucket: TJclInt64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclInt64ArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64HashMap.Pack;
var
  I: Integer;
  Bucket: TJclInt64Bucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64HashMap.PutAll(const AMap: IJclInt64Map);
var
  It: IJclInt64Iterator;
  Key: Int64;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64HashMap.PutValue(const Key: Int64; Value: TObject);
var
  Index: Integer;
  Bucket: TJclInt64Bucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or (not KeysEqual(Key, 0) and not ValuesEqual(Value, nil)) then
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
        Bucket := TJclInt64Bucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.Remove(const Key: Int64): TObject;
var
  Bucket: TJclInt64Bucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclInt64HashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclInt64HashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclInt64Bucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclInt64HashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclInt64HashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclInt64HashMap.FreeKey(var Key: Int64): Int64;
begin
  Result := Key;
  Key := 0;
end;

function TJclInt64HashMap.FreeValue(var Value: TObject): TObject;
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

function TJclInt64HashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclInt64HashMap.KeysEqual(const A, B: Int64): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclInt64HashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$IFNDEF CLR}
//=== { TJclPtrBucket } ==========================================

procedure TJclPtrBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclPtrHashMap } ==========================================

constructor TJclPtrHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclPtrHashMap.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclPtrHashMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TJclPtrBucket;
  ADest: TJclPtrHashMap;
  AMap: IJclPtrMap;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclPtrHashMap then
    begin
      ADest := TJclPtrHashMap(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TJclPtrBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclPtrMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrHashMap.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesto(Dest);
  if Dest is TJclPtrHashMap then
    TJclPtrHashMap(Dest).HashFunction := HashFunction;
end;

procedure TJclPtrHashMap.Clear;
var
  I, J: Integer;
  Bucket: TJclPtrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.ContainsKey(Key: Pointer): Boolean;
var
  I: Integer;
  Bucket: TJclPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.MapEquals(const AMap: IJclPtrMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.GetValue(Key: Pointer): TObject;
var
  I: Integer;
  Bucket: TJclPtrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclPtrHashMap.KeyOfValue(Value: TObject): Pointer;
var
  I, J: Integer;
  Bucket: TJclPtrBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.KeySet: IJclPtrSet;
var
  I, J: Integer;
  Bucket: TJclPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    Result := TJclPtrArraySet.Create(FSize);
    for I := 0 to FCapacity - 1 do
    begin
      Bucket := FBuckets[I];
      if Bucket <> nil then
        for J := 0 to Bucket.Size - 1 do
          Result.Add(Bucket.Entries[J].Key);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrHashMap.Pack;
var
  I: Integer;
  Bucket: TJclPtrBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrHashMap.PutAll(const AMap: IJclPtrMap);
var
  It: IJclPtrIterator;
  Key: Pointer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrHashMap.PutValue(Key: Pointer; Value: TObject);
var
  Index: Integer;
  Bucket: TJclPtrBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
        Bucket := TJclPtrBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.Remove(Key: Pointer): TObject;
var
  Bucket: TJclPtrBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclPtrHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclPtrHashMap.Values: IJclCollection;
var
  I, J: Integer;
  Bucket: TJclPtrBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclPtrHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclPtrHashMap.Create(FCapacity, False);
  AssignPropertiesTo(Result);
end;

function TJclPtrHashMap.FreeKey(var Key: Pointer): Pointer;
begin
  Result := Key;
  Key := nil;
end;

function TJclPtrHashMap.FreeValue(var Value: TObject): TObject;
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

function TJclPtrHashMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclPtrHashMap.KeysEqual(A, B: Pointer): Boolean;
begin
  Result := ItemsEqual(A, B);
end;

function TJclPtrHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;
{$ENDIF ~CLR}

//=== { TJclBucket } ==========================================

procedure TJclBucket.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := nil;
        Entries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclHashMap } ==========================================

constructor TJclHashMap.Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create;
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclHashMap.Destroy;
begin
  FReadOnly := False;
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
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclHashMap then
    begin
      ADest := TJclHashMap(Dest);
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
    end
    else
    if Supports(IInterface(Dest), IJclMap, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.ContainsKey(Key: TObject): Boolean;
var
  I: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.MapEquals(const AMap: IJclMap): Boolean;
var
  I, J: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.GetValue(Key: TObject): TObject;
var
  I: Integer;
  Bucket: TJclBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.KeySet: IJclSet;
var
  I, J: Integer;
  Bucket: TJclBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.Pack;
var
  I: Integer;
  Bucket: TJclBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.PutAll(const AMap: IJclMap);
var
  It: IJclIterator;
  Key: TObject;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.PutValue(Key: TObject; Value: TObject);
var
  Index: Integer;
  Bucket: TJclBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.Remove(Key: TObject): TObject;
var
  Bucket: TJclBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
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
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMap.Create(FCapacity, False, False);
  AssignPropertiesTo(Result);
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

function TJclHashMap.Hash(AObject: TObject): Integer;
begin
  Result := Integer(AObject);
end;

function TJclHashMap.KeysEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

function TJclHashMap.ValuesEqual(A, B: TObject): Boolean;
begin
  Result := Integer(A) = Integer(B);
end;

{$IFDEF SUPPORTS_GENERICS}
//=== { TJclBucket<TKey, TValue> } ==========================================

procedure TJclBucket<TKey, TValue>.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        Entries[FromIndex + I].Key := Default(TKey);
        Entries[FromIndex + I].Value := Default(TValue);
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := Default(TKey);
        Entries[FromIndex + I].Value := Default(TValue);
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      Entries[ToIndex + I] := Entries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        Entries[FromIndex + I].Key := Default(TKey);
        Entries[FromIndex + I].Value := Default(TValue);
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        Entries[FromIndex + I].Key := Default(TKey);
        Entries[FromIndex + I].Value := Default(TValue);
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(Entries[FromIndex], Entries[ToIndex], Count * SizeOf(Entries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(Entries[FromIndex], (ToIndex - FromIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(Entries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(Entries[0]), 0)
      else
        FillChar(Entries[FromIndex], Count * SizeOf(Entries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

//=== { TJclHashMap<TKey, TValue> } ==========================================

constructor TJclHashMap<TKey, TValue>.Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create;
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
  FHashFunction := HashMul;
end;

destructor TJclHashMap<TKey, TValue>.Destroy;
begin
  FReadOnly := False;
  Clear;
  inherited Destroy;
end;

procedure TJclHashMap<TKey, TValue>.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  I, J: Integer;
  SelfBucket, NewBucket: TBucket;
  ADest: TJclHashMap<TKey, TValue>;
  AMap: IJclMap<TKey, TValue>;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
  try
  {$ENDIF THREADSAFE}
    inherited AssignDataTo(Dest);
    if Dest is TJclHashMap<TKey, TValue> then
    begin
      ADest := TJclHashMap<TKey, TValue>(Dest);
      ADest.Clear;
      for I := 0 to FCapacity - 1 do
      begin
        SelfBucket := FBuckets[I];
        if SelfBucket <> nil then
        begin
          NewBucket := TBucket.Create;
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
    end
    else
    if Supports(IInterface(Dest), IJclMap<TKey, TValue>, AMap) then
    begin
      AMap.Clear;
      AMap.PutAll(Self);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
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
  Bucket: TBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.ContainsKey(const Key: TKey): Boolean;
var
  I: Integer;
  Bucket: TBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.ContainsValue(const Value: TValue): Boolean;
var
  I, J: Integer;
  Bucket: TBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.MapEquals(const AMap: IJclMap<TKey, TValue>): Boolean;
var
  I, J: Integer;
  Bucket: TBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.GetValue(const Key: TKey): TValue;
var
  I: Integer;
  Bucket: TBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclHashMap<TKey, TValue>.KeyOfValue(const Value: TValue): TKey;
var
  I, J: Integer;
  Bucket: TBucket;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.KeySet: IJclSet<TKey>;
var
  I, J: Integer;
  Bucket: TBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.Pack;
var
  I: Integer;
  Bucket: TBucket;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.PutAll(const AMap: IJclMap<TKey, TValue>);
var
  It: IJclIterator<TKey>;
  Key: TKey;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.PutValue(const Key: TKey; const Value: TValue);
var
  Index: Integer;
  Bucket: TBucket;
  I: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
        Bucket := TBucket.Create;
        SetLength(Bucket.Entries, 1);
        FBuckets[Index] := Bucket;
      end;

      if Bucket.Size = Length(Bucket.Entries) then
        SetLength(Bucket.Entries, CalcGrowCapacity(Bucket.Size, Bucket.Size));

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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclHashMap<TKey, TValue>.Remove(const Key: TKey): TValue;
var
  Bucket: TBucket;
  I, NewCapacity: Integer;
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
            Bucket.MoveArray(I + 1, I, Bucket.Size - I - 1);
          Dec(Bucket.Size);
          Dec(FSize);
          Break;
        end;

      NewCapacity := CalcPackCapacity(Length(Bucket.Entries), Bucket.Size);
      if NewCapacity < Length(Bucket.Entries) then
        SetLength(Bucket.Entries, NewCapacity);
    end;
  {$IFDEF THREADSAFE}
  finally
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclHashMap<TKey, TValue>.SetCapacity(Value: Integer);
begin
  if ReadOnly then
    raise EJclReadOnlyError.Create;
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginWrite;
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
    if FThreadSafe then
      SyncReaderWriter.EndWrite;
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
  Bucket: TBucket;
begin
  {$IFDEF THREADSAFE}
  if FThreadSafe then
    SyncReaderWriter.BeginRead;
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
    if FThreadSafe then
      SyncReaderWriter.EndRead;
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

//=== { TJclHashMapE<TKey, TValue> } =========================================

constructor TJclHashMapE<TKey, TValue>.Create(const AKeyEqualityComparer: IJclEqualityComparer<TKey>;
  const AKeyHashConverter: IJclHashConverter<TKey>; const AValueEqualityComparer: IJclEqualityComparer<TValue>;
  const AKeyComparer: IJclComparer<TKey>; ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(ACapacity, AOwnsKeys, AOwnsValues);
  FKeyEqualityComparer := AKeyEqualityComparer;
  FKeyHashConverter := AKeyHashConverter;
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
    ADest.FKeyHashConverter := FKeyHashConverter;
    ADest.FValueEqualityComparer := FValueEqualityComparer;
    ADest.FKeyComparer := FKeyComparer;
  end;
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>;
begin
  Result := TArrayList.Create(ValueEqualityComparer, ACapacity, AOwnsObjects);
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TArraySet.Create(KeyComparer, ACapacity, AOwnsObjects);
end;

function TJclHashMapE<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMapE<TKey, TValue>.Create(KeyEqualityComparer, KeyHashConverter, ValueEqualityComparer,
    KeyComparer, FCapacity, False, False);
  AssignPropertiesTo(Result);
end;

function TJclHashMapE<TKey, TValue>.Hash(const AKey: TKey): Integer;
begin
  if KeyEqualityComparer = nil then
    raise EJclNoHashConverterError.Create;
  Result := KeyHashConverter.Hash(AKey);
end;

function TJclHashMapE<TKey, TValue>.KeysEqual(const A, B: TKey): Boolean;
begin
  if KeyEqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := KeyEqualityComparer.ItemsEqual(A, B);
end;

function TJclHashMapE<TKey, TValue>.ValuesEqual(const A, B: TValue): Boolean;
begin
  if ValueEqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := ValueEqualityComparer.ItemsEqual(A, B);
end;

//=== { TJclHashMapF<TKey, TValue> } =========================================

constructor TJclHashMapF<TKey, TValue>.Create(AKeyEqualityCompare: TEqualityCompare<TKey>;
  AKeyHash: THashConvert<TKey>; AValueEqualityCompare: TEqualityCompare<TValue>; AKeyCompare: TCompare<TKey>;
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
  Result := TArrayList.Create(ValueEqualityCompare, ACapacity, AOwnsObjects);
end;

function TJclHashMapF<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TArraySet.Create(KeyCompare, ACapacity, AOwnsObjects);
end;

function TJclHashMapF<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMapF<TKey, TValue>.Create(KeyEqualityCompare, KeyHash, ValueEqualityCompare, KeyCompare, FCapacity,
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
  Result := TArrayList.Create(ACapacity, AOwnsObjects);
end;

function TJclHashMapI<TKey, TValue>.CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>;
begin
  Result := TArraySet.Create(ACapacity, AOwnsObjects);
end;

function TJclHashMapI<TKey, TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclHashMapI<TKey, TValue>.Create(FCapacity, False, False);
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

