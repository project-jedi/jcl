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
{ The Original Code is JclSortedMaps.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet. Portions created by                }
{ Florent Ouchet are Copyright (C) Florent Ouchet <outchy att users dott sourceforge dott net      }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
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

unit JclSortedMaps;

interface

{$I jcl.inc}

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

  TJclIntfIntfEntry = record
    Key: IInterface;
    Value: IInterface;
  end;

  TJclIntfIntfEntryArray = array of TJclIntfIntfEntry;

  TJclIntfIntfSortedMap = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer,
    IJclIntfIntfMap, IJclIntfIntfSortedMap)
  private
    FEntries: TJclIntfIntfEntryArray;
    function BinarySearch(const Key: IInterface): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclIntfIntfSortedMap }
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfIntfSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfIntfSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfIntfSortedMap;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A, B: IInterface): Integer;
    function ValuesCompare(const A, B: IInterface): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;


  TJclAnsiStrIntfEntry = record
    Key: AnsiString;
    Value: IInterface;
  end;

  TJclAnsiStrIntfEntryArray = array of TJclAnsiStrIntfEntry;

  TJclAnsiStrIntfSortedMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer,
    IJclAnsiStrIntfMap, IJclAnsiStrIntfSortedMap)
  private
    FEntries: TJclAnsiStrIntfEntryArray;
    function BinarySearch(const Key: AnsiString): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclAnsiStrIntfSortedMap }
    function FirstKey: AnsiString;
    function HeadMap(const ToKey: AnsiString): IJclAnsiStrIntfSortedMap;
    function LastKey: AnsiString;
    function SubMap(const FromKey, ToKey: AnsiString): IJclAnsiStrIntfSortedMap;
    function TailMap(const FromKey: AnsiString): IJclAnsiStrIntfSortedMap;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A, B: AnsiString): Integer;
    function ValuesCompare(const A, B: IInterface): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;


  TJclIntfAnsiStrEntry = record
    Key: IInterface;
    Value: AnsiString;
  end;

  TJclIntfAnsiStrEntryArray = array of TJclIntfAnsiStrEntry;

  TJclIntfAnsiStrSortedMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer,
    IJclIntfAnsiStrMap, IJclIntfAnsiStrSortedMap)
  private
    FEntries: TJclIntfAnsiStrEntryArray;
    function BinarySearch(const Key: IInterface): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclIntfAnsiStrSortedMap }
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfAnsiStrSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfAnsiStrSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfAnsiStrSortedMap;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: AnsiString): AnsiString;
    function KeysCompare(const A, B: IInterface): Integer;
    function ValuesCompare(const A, B: AnsiString): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;


  TJclAnsiStrAnsiStrEntry = record
    Key: AnsiString;
    Value: AnsiString;
  end;

  TJclAnsiStrAnsiStrEntryArray = array of TJclAnsiStrAnsiStrEntry;

  TJclAnsiStrAnsiStrSortedMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer,
    IJclAnsiStrAnsiStrMap, IJclAnsiStrAnsiStrSortedMap)
  private
    FEntries: TJclAnsiStrAnsiStrEntryArray;
    function BinarySearch(const Key: AnsiString): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclAnsiStrAnsiStrSortedMap }
    function FirstKey: AnsiString;
    function HeadMap(const ToKey: AnsiString): IJclAnsiStrAnsiStrSortedMap;
    function LastKey: AnsiString;
    function SubMap(const FromKey, ToKey: AnsiString): IJclAnsiStrAnsiStrSortedMap;
    function TailMap(const FromKey: AnsiString): IJclAnsiStrAnsiStrSortedMap;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function FreeValue(var Value: AnsiString): AnsiString;
    function KeysCompare(const A, B: AnsiString): Integer;
    function ValuesCompare(const A, B: AnsiString): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;


  TJclWideStrIntfEntry = record
    Key: WideString;
    Value: IInterface;
  end;

  TJclWideStrIntfEntryArray = array of TJclWideStrIntfEntry;

  TJclWideStrIntfSortedMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclWideStrContainer,
    IJclWideStrIntfMap, IJclWideStrIntfSortedMap)
  private
    FEntries: TJclWideStrIntfEntryArray;
    function BinarySearch(const Key: WideString): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclWideStrIntfSortedMap }
    function FirstKey: WideString;
    function HeadMap(const ToKey: WideString): IJclWideStrIntfSortedMap;
    function LastKey: WideString;
    function SubMap(const FromKey, ToKey: WideString): IJclWideStrIntfSortedMap;
    function TailMap(const FromKey: WideString): IJclWideStrIntfSortedMap;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function FreeValue(var Value: IInterface): IInterface;
    function KeysCompare(const A, B: WideString): Integer;
    function ValuesCompare(const A, B: IInterface): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;


  TJclIntfWideStrEntry = record
    Key: IInterface;
    Value: WideString;
  end;

  TJclIntfWideStrEntryArray = array of TJclIntfWideStrEntry;

  TJclIntfWideStrSortedMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclWideStrContainer,
    IJclIntfWideStrMap, IJclIntfWideStrSortedMap)
  private
    FEntries: TJclIntfWideStrEntryArray;
    function BinarySearch(const Key: IInterface): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclIntfWideStrSortedMap }
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfWideStrSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfWideStrSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfWideStrSortedMap;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function FreeValue(var Value: WideString): WideString;
    function KeysCompare(const A, B: IInterface): Integer;
    function ValuesCompare(const A, B: WideString): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;


  TJclWideStrWideStrEntry = record
    Key: WideString;
    Value: WideString;
  end;

  TJclWideStrWideStrEntryArray = array of TJclWideStrWideStrEntry;

  TJclWideStrWideStrSortedMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclWideStrContainer,
    IJclWideStrWideStrMap, IJclWideStrWideStrSortedMap)
  private
    FEntries: TJclWideStrWideStrEntryArray;
    function BinarySearch(const Key: WideString): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclWideStrWideStrSortedMap }
    function FirstKey: WideString;
    function HeadMap(const ToKey: WideString): IJclWideStrWideStrSortedMap;
    function LastKey: WideString;
    function SubMap(const FromKey, ToKey: WideString): IJclWideStrWideStrSortedMap;
    function TailMap(const FromKey: WideString): IJclWideStrWideStrSortedMap;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function FreeValue(var Value: WideString): WideString;
    function KeysCompare(const A, B: WideString): Integer;
    function ValuesCompare(const A, B: WideString): Integer;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrIntfSortedMap = TJclAnsiStrIntfSortedMap;
  TJclIntfStrSortedMap = TJclIntfAnsiStrSortedMap;
  TJclStrStrSortedMap = TJclAnsiStrAnsiStrSortedMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrIntfSortedMap = TJclWideStrIntfSortedMap;
  TJclIntfStrSortedMap = TJclIntfWideStrSortedMap;
  TJclStrStrSortedMap = TJclWideStrWideStrSortedMap;
  {$ENDIF CONTAINER_WIDESTR}


  TJclIntfEntry = record
    Key: IInterface;
    Value: TObject;
  end;

  TJclIntfEntryArray = array of TJclIntfEntry;

  TJclIntfSortedMap = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclValueOwner,
    IJclIntfMap, IJclIntfSortedMap)
  private
    FEntries: TJclIntfEntryArray;
    FOwnsValues: Boolean;
    function BinarySearch(const Key: IInterface): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclIntfSortedMap }
    function FirstKey: IInterface;
    function HeadMap(const ToKey: IInterface): IJclIntfSortedMap;
    function LastKey: IInterface;
    function SubMap(const FromKey, ToKey: IInterface): IJclIntfSortedMap;
    function TailMap(const FromKey: IInterface): IJclIntfSortedMap;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: IInterface): IInterface;
    function KeysCompare(const A, B: IInterface): Integer;
    function ValuesCompare(A, B: TObject): Integer;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property OwnsValues: Boolean read FOwnsValues;
  end;


  TJclAnsiStrEntry = record
    Key: AnsiString;
    Value: TObject;
  end;

  TJclAnsiStrEntryArray = array of TJclAnsiStrEntry;

  TJclAnsiStrSortedMap = class(TJclAnsiStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer, IJclValueOwner,
    IJclAnsiStrMap, IJclAnsiStrSortedMap)
  private
    FEntries: TJclAnsiStrEntryArray;
    FOwnsValues: Boolean;
    function BinarySearch(const Key: AnsiString): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclAnsiStrSortedMap }
    function FirstKey: AnsiString;
    function HeadMap(const ToKey: AnsiString): IJclAnsiStrSortedMap;
    function LastKey: AnsiString;
    function SubMap(const FromKey, ToKey: AnsiString): IJclAnsiStrSortedMap;
    function TailMap(const FromKey: AnsiString): IJclAnsiStrSortedMap;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: AnsiString): AnsiString;
    function KeysCompare(const A, B: AnsiString): Integer;
    function ValuesCompare(A, B: TObject): Integer;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property OwnsValues: Boolean read FOwnsValues;
  end;


  TJclWideStrEntry = record
    Key: WideString;
    Value: TObject;
  end;

  TJclWideStrEntryArray = array of TJclWideStrEntry;

  TJclWideStrSortedMap = class(TJclWideStrAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclStrContainer, IJclWideStrContainer, IJclValueOwner,
    IJclWideStrMap, IJclWideStrSortedMap)
  private
    FEntries: TJclWideStrEntryArray;
    FOwnsValues: Boolean;
    function BinarySearch(const Key: WideString): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclWideStrSortedMap }
    function FirstKey: WideString;
    function HeadMap(const ToKey: WideString): IJclWideStrSortedMap;
    function LastKey: WideString;
    function SubMap(const FromKey, ToKey: WideString): IJclWideStrSortedMap;
    function TailMap(const FromKey: WideString): IJclWideStrSortedMap;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function FreeKey(var Key: WideString): WideString;
    function KeysCompare(const A, B: WideString): Integer;
    function ValuesCompare(A, B: TObject): Integer;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean);
    destructor Destroy; override;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrSortedMap = TJclAnsiStrSortedMap;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrSortedMap = TJclWideStrSortedMap;
  {$ENDIF CONTAINER_WIDESTR}


  TJclEntry = record
    Key: TObject;
    Value: TObject;
  end;

  TJclEntryArray = array of TJclEntry;

  TJclSortedMap = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclKeyOwner, IJclValueOwner,
    IJclMap, IJclSortedMap)
  private
    FEntries: TJclEntryArray;
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
    function BinarySearch(Key: TObject): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclSortedMap }
    function FirstKey: TObject;
    function HeadMap(ToKey: TObject): IJclSortedMap;
    function LastKey: TObject;
    function SubMap(FromKey, ToKey: TObject): IJclSortedMap;
    function TailMap(FromKey: TObject): IJclSortedMap;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclKeyOwner }
    function FreeKey(var Key: TObject): TObject;
    function GetOwnsKeys: Boolean;
    { IJclValueOwner }
    function FreeValue(var Value: TObject): TObject;
    function GetOwnsValues: Boolean;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function KeysCompare(A, B: TObject): Integer;
    function ValuesCompare(A, B: TObject): Integer;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
    destructor Destroy; override;
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  {$IFDEF SUPPORTS_GENERICS_DISABLED}

  TJclEntry<TKey,TValue> = record
    Key: TKey;
    Value: TValue;
  end;

  TJclEntryArray<TKey,TValue> = array of TJclEntry<TKey,TValue>;

  TJclSortedMap<TKey,TValue> = class(TJclAbstractContainerBase, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclGrowable, IJclPackable, IJclContainer, IJclPairOwner<TKey,TValue>,
    IJclMap<TKey,TValue>, IJclSortedMap<TKey,TValue>)
  private
    FEntries: TJclEntryArray<TKey,TValue>;
    FOwnsKeys: Boolean;
    FOwnsValues: Boolean;
    function BinarySearch(const Key: TKey): Integer;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    procedure MoveArray(FromIndex, ToIndex, Count: Integer);
    { IJclPackable }
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
    { IJclSortedMap<TKey,TValue> }
    function FirstKey: TKey;
    function HeadMap(const ToKey: TKey): IJclSortedMap<TKey,TValue>;
    function LastKey: TKey;
    function SubMap(const FromKey, ToKey: TKey): IJclSortedMap<TKey,TValue>;
    function TailMap(const FromKey: TKey): IJclSortedMap<TKey,TValue>;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclPairOwner }
    function FreeKey(var Key: TKey): TKey;
    function FreeValue(var Value: TValue): TValue;
    function GetOwnsKeys: Boolean;
    function GetOwnsValues: Boolean;
    function KeysCompare(const A, B: TKey): Integer; virtual; abstract;
    function ValuesCompare(const A, B: TValue): Integer; virtual; abstract;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; virtual; abstract;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; virtual; abstract;
  public
    constructor Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
    destructor Destroy; override;
    property OwnsKeys: Boolean read FOwnsKeys;
    property OwnsValues: Boolean read FOwnsValues;
  end;

  // E = external helper to compare items
  TJclSortedMapE<TKey, TValue> = class(TJclSortedMap<TKey,TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclMap<TKey,TValue>, IJclSortedMap<TKey,TValue>, IJclPairOwner<TKey,TValue>)
  private
    FKeyComparer: IComparer<TKey>;
    FValueComparer: IComparer<TValue>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function KeysCompare(const A, B: TKey): Integer; override;
    function ValuesCompare(const A, B: TValue): Integer; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AKeyComparer: IComparer<TKey>; const AValueComparer: IComparer<TValue>;
      ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);

    property KeyComparer: IComparer<TKey> read FKeyComparer write FKeyComparer;
    property ValueComparer: IComparer<TValue> read FValueComparer write FValueComparer;
  end;

  // F = Functions to compare items
  TJclSortedMapF<TKey, TValue> = class(TJclSortedMap<TKey, TValue>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer, IJclMap<TKey,TValue>, IJclSortedMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  private
    FKeyCompare: TCompare<TKey>;
    FValueCompare: TCompare<TValue>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function KeysCompare(const A, B: TKey): Integer; override;
    function ValuesCompare(const A, B: TValue): Integer; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(AKeyCompare: TCompare<TKey>; AValueCompare: TCompare<TValue>;
      ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);

    property KeyCompare: TCompare<TKey> read FKeyCompare write FKeyCompare;
    property ValueCompare: TCompare<TValue> read FValueCompare write FValueCompare;
  end;

  // I = items can compare themselves to an other
  TJclSortedMapI<TKey: IComparable<TKey>; TValue: IComparable<TValue>> = class(TJclSortedMap<TKey, TValue>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclContainer,
    IJclMap<TKey,TValue>, IJclSortedMap<TKey,TValue>, IJclPairOwner<TKey, TValue>)
  protected
    function KeysCompare(const A, B: TKey): Integer; override;
    function ValuesCompare(const A, B: TValue): Integer; override;
    function CreateEmptyArrayList(ACapacity: Integer; AOwnsObjects: Boolean): IJclCollection<TValue>; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    function CreateEmptyArraySet(ACapacity: Integer; AOwnsObjects: Boolean): IJclSet<TKey>; override;
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
  SysUtils,
  JclArrayLists,
  JclArraySets;


//=== { TJclIntfIntfSortedMap } ==============================================

constructor TJclIntfIntfSortedMap.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
end;

destructor TJclIntfIntfSortedMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclIntfIntfSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclIntfIntfSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfIntfSortedMap then
  begin
    MyDest := TJclIntfIntfSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclIntfIntfSortedMap.BinarySearch(const Key: IInterface): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfSortedMap.Clear;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.ContainsKey(const Key: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.ContainsValue(const Value: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclIntfIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfIntfSortedMap.Equals(const AMap: IJclIntfIntfMap): Boolean;
var
  It: IJclIntfIterator;
  Index: Integer;
  AKey: IInterface;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.FirstKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[0].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclIntfIntfSortedMap.GetValue(const Key: IInterface): IInterface;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.HeadMap(const ToKey: IInterface): IJclIntfIntfSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclIntfIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfIntfSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FEntries := Copy(FEntries, 0, ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.KeyOfValue(const Value: IInterface): IInterface;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclIntfIntfSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  Result := Integer(A) - Integer(B);
end;

function TJclIntfIntfSortedMap.KeySet: IJclIntfSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.LastKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfIntfSortedMap.PutAll(const AMap: IJclIntfIntfMap);
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

procedure TJclIntfIntfSortedMap.PutValue(const Key: IInterface; const Value: IInterface);
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or ((KeysCompare(Key, nil) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.Remove(const Key: IInterface): IInterface;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfIntfSortedMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclIntfIntfSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfIntfSortedMap.SubMap(const FromKey, ToKey: IInterface): IJclIntfIntfSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclIntfIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FEntries := Copy(FEntries, FromIndex, ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.TailMap(const FromKey: IInterface): IJclIntfIntfSortedMap;
var
  FromIndex: Integer;
  NewMap: TJclIntfIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FEntries := Copy(FEntries, FromIndex, FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.Values: IJclIntfCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  Result := Integer(A) - Integer(B);
end;



//=== { TJclAnsiStrIntfSortedMap } ==============================================

constructor TJclAnsiStrIntfSortedMap.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
end;

destructor TJclAnsiStrIntfSortedMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclAnsiStrIntfSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclAnsiStrIntfSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrIntfSortedMap then
  begin
    MyDest := TJclAnsiStrIntfSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclAnsiStrIntfSortedMap.BinarySearch(const Key: AnsiString): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrIntfSortedMap.Clear;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.ContainsKey(const Key: AnsiString): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.ContainsValue(const Value: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclAnsiStrIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrIntfSortedMap.Equals(const AMap: IJclAnsiStrIntfMap): Boolean;
var
  It: IJclAnsiStrIterator;
  Index: Integer;
  AKey: AnsiString;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.FirstKey: AnsiString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[0].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

function TJclAnsiStrIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;

function TJclAnsiStrIntfSortedMap.GetValue(const Key: AnsiString): IInterface;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.HeadMap(const ToKey: AnsiString): IJclAnsiStrIntfSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclAnsiStrIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclAnsiStrIntfSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FEntries := Copy(FEntries, 0, ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.KeyOfValue(const Value: IInterface): AnsiString;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclAnsiStrIntfSortedMap.KeysCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclAnsiStrIntfSortedMap.KeySet: IJclAnsiStrSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.LastKey: AnsiString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrIntfSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclAnsiStrIntfSortedMap.PutAll(const AMap: IJclAnsiStrIntfMap);
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

procedure TJclAnsiStrIntfSortedMap.PutValue(const Key: AnsiString; const Value: IInterface);
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or ((KeysCompare(Key, '') <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.Remove(const Key: AnsiString): IInterface;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrIntfSortedMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclAnsiStrIntfSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrIntfSortedMap.SubMap(const FromKey, ToKey: AnsiString): IJclAnsiStrIntfSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclAnsiStrIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclAnsiStrIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FEntries := Copy(FEntries, FromIndex, ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.TailMap(const FromKey: AnsiString): IJclAnsiStrIntfSortedMap;
var
  FromIndex: Integer;
  NewMap: TJclAnsiStrIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclAnsiStrIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FEntries := Copy(FEntries, FromIndex, FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.Values: IJclIntfCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  Result := Integer(A) - Integer(B);
end;



//=== { TJclIntfAnsiStrSortedMap } ==============================================

constructor TJclIntfAnsiStrSortedMap.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
end;

destructor TJclIntfAnsiStrSortedMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclIntfAnsiStrSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclIntfAnsiStrSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfAnsiStrSortedMap then
  begin
    MyDest := TJclIntfAnsiStrSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclIntfAnsiStrSortedMap.BinarySearch(const Key: IInterface): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfAnsiStrSortedMap.Clear;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.ContainsKey(const Key: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.ContainsValue(const Value: AnsiString): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclIntfAnsiStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfAnsiStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfAnsiStrSortedMap.Equals(const AMap: IJclIntfAnsiStrMap): Boolean;
var
  It: IJclIntfIterator;
  Index: Integer;
  AKey: IInterface;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.FirstKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[0].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfAnsiStrSortedMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;

function TJclIntfAnsiStrSortedMap.GetValue(const Key: IInterface): AnsiString;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := '';
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.HeadMap(const ToKey: IInterface): IJclIntfAnsiStrSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclIntfAnsiStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfAnsiStrSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FEntries := Copy(FEntries, 0, ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.KeyOfValue(const Value: AnsiString): IInterface;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclIntfAnsiStrSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  Result := Integer(A) - Integer(B);
end;

function TJclIntfAnsiStrSortedMap.KeySet: IJclIntfSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.LastKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfAnsiStrSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := '';
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := '';
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfAnsiStrSortedMap.PutAll(const AMap: IJclIntfAnsiStrMap);
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

procedure TJclIntfAnsiStrSortedMap.PutValue(const Key: IInterface; const Value: AnsiString);
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or ((KeysCompare(Key, nil) <> 0) and (ValuesCompare(Value, '') <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.Remove(const Key: IInterface): AnsiString;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfAnsiStrSortedMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclIntfAnsiStrSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfAnsiStrSortedMap.SubMap(const FromKey, ToKey: IInterface): IJclIntfAnsiStrSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclIntfAnsiStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfAnsiStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FEntries := Copy(FEntries, FromIndex, ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.TailMap(const FromKey: IInterface): IJclIntfAnsiStrSortedMap;
var
  FromIndex: Integer;
  NewMap: TJclIntfAnsiStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfAnsiStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FEntries := Copy(FEntries, FromIndex, FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.Values: IJclAnsiStrCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfAnsiStrSortedMap.ValuesCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;



//=== { TJclAnsiStrAnsiStrSortedMap } ==============================================

constructor TJclAnsiStrAnsiStrSortedMap.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
end;

destructor TJclAnsiStrAnsiStrSortedMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclAnsiStrAnsiStrSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclAnsiStrAnsiStrSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrAnsiStrSortedMap then
  begin
    MyDest := TJclAnsiStrAnsiStrSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclAnsiStrAnsiStrSortedMap.BinarySearch(const Key: AnsiString): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrAnsiStrSortedMap.Clear;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.ContainsKey(const Key: AnsiString): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.ContainsValue(const Value: AnsiString): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclAnsiStrAnsiStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrAnsiStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrAnsiStrSortedMap.Equals(const AMap: IJclAnsiStrAnsiStrMap): Boolean;
var
  It: IJclAnsiStrIterator;
  Index: Integer;
  AKey: AnsiString;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.FirstKey: AnsiString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[0].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

function TJclAnsiStrAnsiStrSortedMap.FreeValue(var Value: AnsiString): AnsiString;
begin
  Result := Value;
  Value := '';
end;



function TJclAnsiStrAnsiStrSortedMap.GetValue(const Key: AnsiString): AnsiString;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := '';
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.HeadMap(const ToKey: AnsiString): IJclAnsiStrAnsiStrSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclAnsiStrAnsiStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclAnsiStrAnsiStrSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FEntries := Copy(FEntries, 0, ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.KeyOfValue(const Value: AnsiString): AnsiString;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclAnsiStrAnsiStrSortedMap.KeysCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclAnsiStrAnsiStrSortedMap.KeySet: IJclAnsiStrSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.LastKey: AnsiString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrAnsiStrSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := '';
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := '';
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclAnsiStrAnsiStrSortedMap.PutAll(const AMap: IJclAnsiStrAnsiStrMap);
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

procedure TJclAnsiStrAnsiStrSortedMap.PutValue(const Key: AnsiString; const Value: AnsiString);
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or ((KeysCompare(Key, '') <> 0) and (ValuesCompare(Value, '') <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.Remove(const Key: AnsiString): AnsiString;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrAnsiStrSortedMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclAnsiStrAnsiStrSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrAnsiStrSortedMap.SubMap(const FromKey, ToKey: AnsiString): IJclAnsiStrAnsiStrSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclAnsiStrAnsiStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclAnsiStrAnsiStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FEntries := Copy(FEntries, FromIndex, ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.TailMap(const FromKey: AnsiString): IJclAnsiStrAnsiStrSortedMap;
var
  FromIndex: Integer;
  NewMap: TJclAnsiStrAnsiStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclAnsiStrAnsiStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FEntries := Copy(FEntries, FromIndex, FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.Values: IJclAnsiStrCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrAnsiStrSortedMap.ValuesCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;



//=== { TJclWideStrIntfSortedMap } ==============================================

constructor TJclWideStrIntfSortedMap.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
end;

destructor TJclWideStrIntfSortedMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclWideStrIntfSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclWideStrIntfSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrIntfSortedMap then
  begin
    MyDest := TJclWideStrIntfSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclWideStrIntfSortedMap.BinarySearch(const Key: WideString): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrIntfSortedMap.Clear;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.ContainsKey(const Key: WideString): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.ContainsValue(const Value: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclWideStrIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrIntfSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclWideStrIntfSortedMap.Equals(const AMap: IJclWideStrIntfMap): Boolean;
var
  It: IJclWideStrIterator;
  Index: Integer;
  AKey: WideString;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.FirstKey: WideString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[0].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

function TJclWideStrIntfSortedMap.FreeValue(var Value: IInterface): IInterface;
begin
  Result := Value;
  Value := nil;
end;



function TJclWideStrIntfSortedMap.GetValue(const Key: WideString): IInterface;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.HeadMap(const ToKey: WideString): IJclWideStrIntfSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclWideStrIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclWideStrIntfSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FEntries := Copy(FEntries, 0, ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.KeyOfValue(const Value: IInterface): WideString;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclWideStrIntfSortedMap.KeysCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclWideStrIntfSortedMap.KeySet: IJclWideStrSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.LastKey: WideString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrIntfSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclWideStrIntfSortedMap.PutAll(const AMap: IJclWideStrIntfMap);
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

procedure TJclWideStrIntfSortedMap.PutValue(const Key: WideString; const Value: IInterface);
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or ((KeysCompare(Key, '') <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.Remove(const Key: WideString): IInterface;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrIntfSortedMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclWideStrIntfSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrIntfSortedMap.SubMap(const FromKey, ToKey: WideString): IJclWideStrIntfSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclWideStrIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclWideStrIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FEntries := Copy(FEntries, FromIndex, ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.TailMap(const FromKey: WideString): IJclWideStrIntfSortedMap;
var
  FromIndex: Integer;
  NewMap: TJclWideStrIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclWideStrIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FEntries := Copy(FEntries, FromIndex, FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.Values: IJclIntfCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrIntfSortedMap.ValuesCompare(const A, B: IInterface): Integer;
begin
  Result := Integer(A) - Integer(B);
end;



//=== { TJclIntfWideStrSortedMap } ==============================================

constructor TJclIntfWideStrSortedMap.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
end;

destructor TJclIntfWideStrSortedMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclIntfWideStrSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclIntfWideStrSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfWideStrSortedMap then
  begin
    MyDest := TJclIntfWideStrSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclIntfWideStrSortedMap.BinarySearch(const Key: IInterface): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfWideStrSortedMap.Clear;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.ContainsKey(const Key: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.ContainsValue(const Value: WideString): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclIntfWideStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfWideStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclIntfWideStrSortedMap.Equals(const AMap: IJclIntfWideStrMap): Boolean;
var
  It: IJclIntfIterator;
  Index: Integer;
  AKey: IInterface;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.FirstKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[0].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfWideStrSortedMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;

function TJclIntfWideStrSortedMap.GetValue(const Key: IInterface): WideString;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := '';
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.HeadMap(const ToKey: IInterface): IJclIntfWideStrSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclIntfWideStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfWideStrSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FEntries := Copy(FEntries, 0, ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.KeyOfValue(const Value: WideString): IInterface;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclIntfWideStrSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  Result := Integer(A) - Integer(B);
end;

function TJclIntfWideStrSortedMap.KeySet: IJclIntfSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.LastKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfWideStrSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := '';
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := '';
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfWideStrSortedMap.PutAll(const AMap: IJclIntfWideStrMap);
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

procedure TJclIntfWideStrSortedMap.PutValue(const Key: IInterface; const Value: WideString);
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or ((KeysCompare(Key, nil) <> 0) and (ValuesCompare(Value, '') <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.Remove(const Key: IInterface): WideString;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfWideStrSortedMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclIntfWideStrSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfWideStrSortedMap.SubMap(const FromKey, ToKey: IInterface): IJclIntfWideStrSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclIntfWideStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfWideStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FEntries := Copy(FEntries, FromIndex, ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.TailMap(const FromKey: IInterface): IJclIntfWideStrSortedMap;
var
  FromIndex: Integer;
  NewMap: TJclIntfWideStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfWideStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FEntries := Copy(FEntries, FromIndex, FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.Values: IJclWideStrCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfWideStrSortedMap.ValuesCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;



//=== { TJclWideStrWideStrSortedMap } ==============================================

constructor TJclWideStrWideStrSortedMap.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
end;

destructor TJclWideStrWideStrSortedMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclWideStrWideStrSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclWideStrWideStrSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrWideStrSortedMap then
  begin
    MyDest := TJclWideStrWideStrSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclWideStrWideStrSortedMap.BinarySearch(const Key: WideString): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrWideStrSortedMap.Clear;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.ContainsKey(const Key: WideString): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.ContainsValue(const Value: WideString): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclWideStrWideStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrWideStrSortedMap.Create(FSize);
  AssignPropertiesTo(Result);
end;

function TJclWideStrWideStrSortedMap.Equals(const AMap: IJclWideStrWideStrMap): Boolean;
var
  It: IJclWideStrIterator;
  Index: Integer;
  AKey: WideString;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.FirstKey: WideString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[0].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

function TJclWideStrWideStrSortedMap.FreeValue(var Value: WideString): WideString;
begin
  Result := Value;
  Value := '';
end;



function TJclWideStrWideStrSortedMap.GetValue(const Key: WideString): WideString;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := '';
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.HeadMap(const ToKey: WideString): IJclWideStrWideStrSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclWideStrWideStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclWideStrWideStrSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FEntries := Copy(FEntries, 0, ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.KeyOfValue(const Value: WideString): WideString;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclWideStrWideStrSortedMap.KeysCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclWideStrWideStrSortedMap.KeySet: IJclWideStrSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.LastKey: WideString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrWideStrSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := '';
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := '';
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := '';
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclWideStrWideStrSortedMap.PutAll(const AMap: IJclWideStrWideStrMap);
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

procedure TJclWideStrWideStrSortedMap.PutValue(const Key: WideString; const Value: WideString);
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or ((KeysCompare(Key, '') <> 0) and (ValuesCompare(Value, '') <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.Remove(const Key: WideString): WideString;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrWideStrSortedMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclWideStrWideStrSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrWideStrSortedMap.SubMap(const FromKey, ToKey: WideString): IJclWideStrWideStrSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclWideStrWideStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclWideStrWideStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FEntries := Copy(FEntries, FromIndex, ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.TailMap(const FromKey: WideString): IJclWideStrWideStrSortedMap;
var
  FromIndex: Integer;
  NewMap: TJclWideStrWideStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclWideStrWideStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FEntries := Copy(FEntries, FromIndex, FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.Values: IJclWideStrCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArrayList.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrWideStrSortedMap.ValuesCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;



//=== { TJclIntfSortedMap } ==============================================

constructor TJclIntfSortedMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create(nil);
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
end;

destructor TJclIntfSortedMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclIntfSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclIntfSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfSortedMap then
  begin
    MyDest := TJclIntfSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclIntfSortedMap.BinarySearch(const Key: IInterface): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfSortedMap.Clear;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.ContainsKey(const Key: IInterface): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.ContainsValue(Value: TObject): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclIntfSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclIntfSortedMap.Equals(const AMap: IJclIntfMap): Boolean;
var
  It: IJclIntfIterator;
  Index: Integer;
  AKey: IInterface;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.FirstKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[0].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.FreeKey(var Key: IInterface): IInterface;
begin
  Result := Key;
  Key := nil;
end;

function TJclIntfSortedMap.FreeValue(var Value: TObject): TObject;
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


function TJclIntfSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclIntfSortedMap.GetValue(const Key: IInterface): TObject;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.HeadMap(const ToKey: IInterface): IJclIntfSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FEntries := Copy(FEntries, 0, ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.KeyOfValue(Value: TObject): IInterface;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclIntfSortedMap.KeysCompare(const A, B: IInterface): Integer;
begin
  Result := Integer(A) - Integer(B);
end;

function TJclIntfSortedMap.KeySet: IJclIntfSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.LastKey: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclIntfSortedMap.PutAll(const AMap: IJclIntfMap);
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

procedure TJclIntfSortedMap.PutValue(const Key: IInterface; Value: TObject);
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or ((KeysCompare(Key, nil) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.Remove(const Key: IInterface): TObject;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfSortedMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclIntfSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfSortedMap.SubMap(const FromKey, ToKey: IInterface): IJclIntfSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FEntries := Copy(FEntries, FromIndex, ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.TailMap(const FromKey: IInterface): IJclIntfSortedMap;
var
  FromIndex: Integer;
  NewMap: TJclIntfSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclIntfSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FEntries := Copy(FEntries, FromIndex, FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.Values: IJclCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  Result := Integer(A) - Integer(B);
end;



//=== { TJclAnsiStrSortedMap } ==============================================

constructor TJclAnsiStrSortedMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create(nil);
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
end;

destructor TJclAnsiStrSortedMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclAnsiStrSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclAnsiStrSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclAnsiStrSortedMap then
  begin
    MyDest := TJclAnsiStrSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclAnsiStrSortedMap.BinarySearch(const Key: AnsiString): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrSortedMap.Clear;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.ContainsKey(const Key: AnsiString): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.ContainsValue(Value: TObject): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclAnsiStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrSortedMap.Equals(const AMap: IJclAnsiStrMap): Boolean;
var
  It: IJclAnsiStrIterator;
  Index: Integer;
  AKey: AnsiString;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.FirstKey: AnsiString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[0].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.FreeKey(var Key: AnsiString): AnsiString;
begin
  Result := Key;
  Key := '';
end;

function TJclAnsiStrSortedMap.FreeValue(var Value: TObject): TObject;
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


function TJclAnsiStrSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclAnsiStrSortedMap.GetValue(const Key: AnsiString): TObject;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.HeadMap(const ToKey: AnsiString): IJclAnsiStrSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclAnsiStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclAnsiStrSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FEntries := Copy(FEntries, 0, ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.KeyOfValue(Value: TObject): AnsiString;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclAnsiStrSortedMap.KeysCompare(const A, B: AnsiString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclAnsiStrSortedMap.KeySet: IJclAnsiStrSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclAnsiStrArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.LastKey: AnsiString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclAnsiStrSortedMap.PutAll(const AMap: IJclAnsiStrMap);
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

procedure TJclAnsiStrSortedMap.PutValue(const Key: AnsiString; Value: TObject);
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or ((KeysCompare(Key, '') <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.Remove(const Key: AnsiString): TObject;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrSortedMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclAnsiStrSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrSortedMap.SubMap(const FromKey, ToKey: AnsiString): IJclAnsiStrSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclAnsiStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclAnsiStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FEntries := Copy(FEntries, FromIndex, ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.TailMap(const FromKey: AnsiString): IJclAnsiStrSortedMap;
var
  FromIndex: Integer;
  NewMap: TJclAnsiStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclAnsiStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FEntries := Copy(FEntries, FromIndex, FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.Values: IJclCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  Result := Integer(A) - Integer(B);
end;



//=== { TJclWideStrSortedMap } ==============================================

constructor TJclWideStrSortedMap.Create(ACapacity: Integer; AOwnsValues: Boolean);
begin
  inherited Create(nil);
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
end;

destructor TJclWideStrSortedMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclWideStrSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclWideStrSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclWideStrSortedMap then
  begin
    MyDest := TJclWideStrSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclWideStrSortedMap.BinarySearch(const Key: WideString): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrSortedMap.Clear;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.ContainsKey(const Key: WideString): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.ContainsValue(Value: TObject): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclWideStrSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrSortedMap.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclWideStrSortedMap.Equals(const AMap: IJclWideStrMap): Boolean;
var
  It: IJclWideStrIterator;
  Index: Integer;
  AKey: WideString;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.FirstKey: WideString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[0].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.FreeKey(var Key: WideString): WideString;
begin
  Result := Key;
  Key := '';
end;

function TJclWideStrSortedMap.FreeValue(var Value: TObject): TObject;
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


function TJclWideStrSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclWideStrSortedMap.GetValue(const Key: WideString): TObject;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.HeadMap(const ToKey: WideString): IJclWideStrSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclWideStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclWideStrSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FEntries := Copy(FEntries, 0, ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.KeyOfValue(Value: TObject): WideString;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := '';
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclWideStrSortedMap.KeysCompare(const A, B: WideString): Integer;
begin
  Result := ItemsCompare(A, B);
end;

function TJclWideStrSortedMap.KeySet: IJclWideStrSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclWideStrArraySet.Create(FSize);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.LastKey: WideString;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := '';
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclWideStrSortedMap.PutAll(const AMap: IJclWideStrMap);
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

procedure TJclWideStrSortedMap.PutValue(const Key: WideString; Value: TObject);
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or ((KeysCompare(Key, '') <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.Remove(const Key: WideString): TObject;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrSortedMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclWideStrSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrSortedMap.SubMap(const FromKey, ToKey: WideString): IJclWideStrSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclWideStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclWideStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FEntries := Copy(FEntries, FromIndex, ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.TailMap(const FromKey: WideString): IJclWideStrSortedMap;
var
  FromIndex: Integer;
  NewMap: TJclWideStrSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclWideStrSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FEntries := Copy(FEntries, FromIndex, FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.Values: IJclCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  Result := Integer(A) - Integer(B);
end;



//=== { TJclSortedMap } ==============================================

constructor TJclSortedMap.Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(nil);
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
end;

destructor TJclSortedMap.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclSortedMap.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclSortedMap;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclSortedMap then
  begin
    MyDest := TJclSortedMap(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclSortedMap.BinarySearch(Key: TObject): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSortedMap.Clear;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.ContainsKey(Key: TObject): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.ContainsValue(Value: TObject): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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

function TJclSortedMap.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSortedMap.Create(FSize, False, False);
  AssignPropertiesTo(Result);
end;

function TJclSortedMap.Equals(const AMap: IJclMap): Boolean;
var
  It: IJclIterator;
  Index: Integer;
  AKey: TObject;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.FirstKey: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[0].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.FreeKey(var Key: TObject): TObject;
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

function TJclSortedMap.FreeValue(var Value: TObject): TObject;
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

function TJclSortedMap.GetOWnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;

function TJclSortedMap.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclSortedMap.GetValue(Key: TObject): TObject;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := nil;
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.HeadMap(ToKey: TObject): IJclSortedMap;
var
  ToIndex: Integer;
  NewMap: TJclSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSortedMap;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FEntries := Copy(FEntries, 0, ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.KeyOfValue(Value: TObject): TObject;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := nil;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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

function TJclSortedMap.KeysCompare(A, B: TObject): Integer;
begin
  Result := Integer(A) - Integer(B);
end;

function TJclSortedMap.KeySet: IJclSet;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArraySet.Create(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.LastKey: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSortedMap.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := nil;
        FEntries[FromIndex + I].Value := nil;
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclSortedMap.PutAll(const AMap: IJclMap);
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

procedure TJclSortedMap.PutValue(Key: TObject; Value: TObject);
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or ((KeysCompare(Key, nil) <> 0) and (ValuesCompare(Value, nil) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.Remove(Key: TObject): TObject;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSortedMap.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclSortedMap.Size: Integer;
begin
  Result := FSize;
end;

function TJclSortedMap.SubMap(FromKey, ToKey: TObject): IJclSortedMap;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FEntries := Copy(FEntries, FromIndex, ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.TailMap(FromKey: TObject): IJclSortedMap;
var
  FromIndex: Integer;
  NewMap: TJclSortedMap;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSortedMap;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FEntries := Copy(FEntries, FromIndex, FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.Values: IJclCollection;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap.ValuesCompare(A, B: TObject): Integer;
begin
  Result := Integer(A) - Integer(B);
end;


{$IFDEF SUPPORTS_GENERICS_DISABLED}


//=== { TJclSortedMap<TKey,TValue> } ==============================================

constructor TJclSortedMap<TKey,TValue>.Create(ACapacity: Integer; AOwnsValues: Boolean; AOwnsKeys: Boolean);
begin
  inherited Create(nil);
  FOwnsKeys := AOwnsKeys;
  FOwnsValues := AOwnsValues;
  SetCapacity(ACapacity);
end;

destructor TJclSortedMap<TKey,TValue>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclSortedMap<TKey,TValue>.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  MyDest: TJclSortedMap<TKey,TValue>;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclSortedMap<TKey,TValue> then
  begin
    MyDest := TJclSortedMap<TKey,TValue>(Dest);
    MyDest.SetCapacity(FSize);
    MyDest.FEntries := FEntries;
    MyDest.FSize := FSize;
  end;
end;

function TJclSortedMap<TKey,TValue>.BinarySearch(const Key: TKey): Integer;
var
  HiPos, LoPos, CompPos: Integer;
  Comp: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    LoPos := 0;
    HiPos := FSize - 1;
    CompPos := (HiPos + LoPos) div 2;
    while HiPos >= LoPos do
    begin
      Comp := KeysCompare(FEntries[CompPos].Key, Key);
      if Comp < 0 then
        LoPos := CompPos + 1
      else
      if Comp > 0 then
        HiPos := CompPos - 1
      else
      begin
        HiPos := CompPos;
        LoPos := CompPos + 1;
      end;
      CompPos := (HiPos + LoPos) div 2;
    end;
    Result := HiPos;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSortedMap<TKey,TValue>.Clear;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for Index := 0 to FSize - 1 do
    begin
      FreeKey(FEntries[Index].Key);
      FreeValue(FEntries[Index].Value);
    end;
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap<TKey,TValue>.ContainsKey(const Key: TKey): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap<TKey,TValue>.ContainsValue(const Value: TValue): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
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


function TJclSortedMap<TKey,TValue>.Equals(const AMap: IJclMap<TKey,TValue>): Boolean;
var
  It: IJclIterator<TKey>;
  Index: Integer;
  AKey: TKey;
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
    It := AMap.KeySet.First;
    Index := 0;
    while It.HasNext do
    begin
      if Index >= FSize then
        Exit;
      AKey := It.Next;
      if ValuesCompare(AMap.GetValue(AKey), FEntries[Index].Value) <> 0 then
        Exit;
      Inc(Index);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap<TKey,TValue>.FirstKey: TKey;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(TKey);
    if FSize > 0 then
      Result := FEntries[0].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap<TKey,TValue>.FreeKey(var Key: TKey): TKey;
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

function TJclSortedMap<TKey,TValue>.FreeValue(var Value: TValue): TValue;
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

function TJclSortedMap<TKey,TValue>.GetOWnsKeys: Boolean;
begin
  Result := FOwnsKeys;
end;

function TJclSortedMap<TKey,TValue>.GetOwnsValues: Boolean;
begin
  Result := FOwnsValues;
end;

function TJclSortedMap<TKey,TValue>.GetValue(const Key: TKey): TValue;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    Result := Default(TValue);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      Result := FEntries[Index].Value
    else if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap<TKey,TValue>.HeadMap(const ToKey: TKey): IJclSortedMap<TKey,TValue>;
var
  ToIndex: Integer;
  NewMap: TJclSortedMap<TKey,TValue>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSortedMap<TKey,TValue>;
    ToIndex := BinarySearch(ToKey);
    if ToIndex >= 0 then
    begin
      NewMap.SetCapacity(ToIndex + 1);
      NewMap.FEntries := Copy(FEntries, 0, ToIndex + 1);
      NewMap.FSize := ToIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap<TKey,TValue>.IsEmpty: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := FSize = 0;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap<TKey,TValue>.KeyOfValue(const Value: TValue): TKey;
var
  Index: Integer;
  Found: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Found := False;
    Result := Default(TKey);
    for Index := 0 to FSize - 1 do
      if ValuesCompare(FEntries[Index].Value, Value) = 0 then
    begin
      Result := FEntries[Index].Key;
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


function TJclSortedMap<TKey,TValue>.KeySet: IJclSet<TKey>;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArraySet<TKey>.Create(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Key);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap<TKey,TValue>.LastKey: TKey;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(TKey);
    if FSize > 0 then
      Result := FEntries[FSize - 1].Key
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSortedMap<TKey,TValue>.MoveArray(FromIndex, ToIndex, Count: Integer);
{$IFDEF CLR}
var
  I: Integer;
begin
  if FromIndex < ToIndex then
  begin
    for I := Count - 1 downto 0 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (ToIndex - FromIndex) < Count then
      // overlapped source and target
      for I := 0 to ToIndex - FromIndex - 1 do
      begin
        FEntries[FromIndex + I].Key := Default(TKey);
        FEntries[FromIndex + I].Value := Default(TValue);
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := Default(TKey);
        FEntries[FromIndex + I].Value := Default(TValue);
      end;
  end
  else
  if FromIndex > ToIndex then
  begin
    for I := 0 to Count - 1 do
      FEntries[ToIndex + I] := FEntries[FromIndex + I];
    if (FromIndex - ToIndex) < Count then
      // overlapped source and target
      for I := Count - FromIndex + ToIndex to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := Default(TKey);
        FEntries[FromIndex + I].Value := Default(TValue);
      end
    else
      // independant
      for I := 0 to Count - 1 do
      begin
        FEntries[FromIndex + I].Key := Default(TKey);
        FEntries[FromIndex + I].Value := Default(TValue);
      end;
  end;
end;
{$ELSE}
begin
  if Count > 0 then
  begin
    Move(FEntries[FromIndex], FEntries[ToIndex], Count * SizeOf(FEntries[0]));
    { Keep reference counting working }
    if FromIndex < ToIndex then
    begin
      if (ToIndex - FromIndex) < Count then
        FillChar(FEntries[FromIndex], (ToIndex - FromIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end
    else
    if FromIndex > ToIndex then
    begin
      if (FromIndex - ToIndex) < Count then
        FillChar(FEntries[ToIndex + Count], (FromIndex - ToIndex) * SizeOf(FEntries[0]), 0)
      else
        FillChar(FEntries[FromIndex], Count * SizeOf(FEntries[0]), 0);
    end;
  end;
end;
{$ENDIF CLR}

procedure TJclSortedMap<TKey,TValue>.PutAll(const AMap: IJclMap<TKey,TValue>);
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

procedure TJclSortedMap<TKey,TValue>.PutValue(const Key: TKey; const Value: TValue);
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FAllowDefaultElements or ((KeysCompare(Key, Default(TKey)) <> 0) and (ValuesCompare(Value, Default(TValue)) <> 0)) then
    begin
      Index := BinarySearch(Key);

      if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
      begin
        FreeValue(FEntries[Index].Value);
        FEntries[Index].Value := Value;
      end
      else
      begin
        if FSize = FCapacity then
          AutoGrow;
        if FSize < FCapacity then
        begin
          Inc(Index);
          if (Index < FSize) and (KeysCompare(FEntries[Index].Key, Key) <> 0) then
            MoveArray(Index, Index + 1, FSize - Index);
          FEntries[Index].Key := Key;
          FEntries[Index].Value := Value;
          Inc(FSize);
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap<TKey,TValue>.Remove(const Key: TKey): TValue;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Index := BinarySearch(Key);
    if (Index >= 0) and (KeysCompare(FEntries[Index].Key, Key) = 0) then
    begin
      Result := FreeValue(FEntries[Index].Value);
      FreeKey(FEntries[Index].Key);
      if Index < (FSize - 1) then
        MoveArray(Index + 1, Index, FSize - Index - 1);
      Dec(FSize);
      AutoPack;
    end
    else
      Result := Default(TValue);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclSortedMap<TKey,TValue>.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize <= Value then
    begin
      SetLength(FEntries, Value);
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

function TJclSortedMap<TKey,TValue>.Size: Integer;
begin
  Result := FSize;
end;

function TJclSortedMap<TKey,TValue>.SubMap(const FromKey, ToKey: TKey): IJclSortedMap<TKey,TValue>;
var
  FromIndex, ToIndex: Integer;
  NewMap: TJclSortedMap<TKey,TValue>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSortedMap<TKey,TValue>;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    ToIndex := BinarySearch(ToKey);
    if (FromIndex >= 0) and (FromIndex <= ToIndex) then
    begin
      NewMap.SetCapacity(ToIndex - FromIndex + 1);
      NewMap.FEntries := Copy(FEntries, FromIndex, ToIndex - FromIndex + 1);
      NewMap.FSize := ToIndex - FromIndex + 1;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap<TKey,TValue>.TailMap(const FromKey: TKey): IJclSortedMap<TKey,TValue>;
var
  FromIndex: Integer;
  NewMap: TJclSortedMap<TKey,TValue>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    NewMap := CreateEmptyContainer as TJclSortedMap<TKey,TValue>;
    FromIndex := BinarySearch(FromKey);
    if (FromIndex = -1) or (KeysCompare(FEntries[FromIndex].Key, FromKey) < 0) then
      Inc(FromIndex);
    if (FromIndex >= 0) and (FromIndex < FSize) then
    begin
      NewMap.SetCapacity(FSize - FromIndex);
      NewMap.FEntries := Copy(FEntries, FromIndex, FSize - FromIndex);
      NewMap.FSize := FSize - FromIndex;
    end;
    Result := NewMap;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclSortedMap<TKey,TValue>.Values: IJclCollection<TValue>;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList<TValue>.Create(FSize, False);
    for Index := 0 to FSize - 1 do
      Result.Add(FEntries[Index].Value);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;



{function TJclSortedMap<TKey,TValue>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclSortedMap<TKey,TValue>.Create(FSize, False, False);
  AssignPropertiesTo(Result);
end;}

{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
