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

type
  TJclIntfArrayList = class(TJclAbstractContainer, IJclIntfCollection, IJclIntfList, IJclIntfArray,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FElementData: TDynIInterfaceArray;
    FSize: Integer;
    FCapacity: Integer;
  protected
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclGrowable }
    procedure Grow; overload; virtual;
    procedure Grow(Increment: Integer); overload;
    procedure Grow(Num, Denom: Integer); overload;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCollection }
    function Add(const AInterface: IInterface): Boolean; overload;
    function AddAll(const ACollection: IJclIntfCollection): Boolean; overload;
    procedure Clear;
    function Contains(const AInterface: IInterface): Boolean;
    function ContainsAll(const ACollection: IJclIntfCollection): Boolean;
    function Equals(const ACollection: IJclIntfCollection): Boolean;
    function First: IJclIntfIterator;
    function IsEmpty: Boolean;
    function Last: IJclIntfIterator;
    function Remove(const AInterface: IInterface): Boolean; overload;
    function RemoveAll(const ACollection: IJclIntfCollection): Boolean;
    function RetainAll(const ACollection: IJclIntfCollection): Boolean;
    function Size: Integer;
    { IJclIntfList }
    procedure Insert(Index: Integer; const AInterface: IInterface); overload;
    function InsertAll(Index: Integer; const ACollection: IJclIntfCollection): Boolean; overload;
    function GetObject(Index: Integer): IInterface;
    function IndexOf(const AInterface: IInterface): Integer;
    function LastIndexOf(const AInterface: IInterface): Integer;
    function Remove(Index: Integer): IInterface; overload;
    procedure SetObject(Index: Integer; const AInterface: IInterface);
    function SubList(First, Count: Integer): IJclIntfList;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity); overload;
    constructor Create(const ACollection: IJclIntfCollection); overload;
    destructor Destroy; override;
    property Capacity: Integer read FCapacity write SetCapacity;
  end;

  //Daniele Teti 02/03/2005
  TJclStrArrayList = class(TJclStrCollection, IJclStrCollection, IJclStrList, IJclStrArray,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FCapacity: Integer;
    FElementData: TDynStringArray;
    FSize: Integer;
  protected
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclGrowable }
    procedure Grow; overload; virtual;
    procedure Grow(Increment: Integer); overload;
    procedure Grow(Num, Denom: Integer); overload;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclCloneable }
    function Clone: TObject;
    { IJclStrCollection }
    function Add(const AString: string): Boolean; overload; override;
    function AddAll(const ACollection: IJclStrCollection): Boolean; overload; override;
    procedure Clear; override;
    function Contains(const AString: string): Boolean; override;
    function ContainsAll(const ACollection: IJclStrCollection): Boolean; override;
    function Equals(const ACollection: IJclStrCollection): Boolean; override;
    function First: IJclStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclStrIterator; override;
    function Remove(const AString: string): Boolean; overload; override;
    function RemoveAll(const ACollection: IJclStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclStrCollection): Boolean; override;
    function Size: Integer; override;
    { IJclStrList }
    procedure Insert(Index: Integer; const AString: string); overload;
    function InsertAll(Index: Integer; const ACollection: IJclStrCollection): Boolean; overload;
    function GetString(Index: Integer): string;
    function IndexOf(const AString: string): Integer;
    function LastIndexOf(const AString: string): Integer;
    function Remove(Index: Integer): string; overload;
    procedure SetString(Index: Integer; const AString: string);
    function SubList(First, Count: Integer): IJclStrList;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity); overload;
    constructor Create(const ACollection: IJclStrCollection); overload;
    destructor Destroy; override;
    property Capacity: Integer read FCapacity write SetCapacity;
  end;

  TJclArrayList = class(TJclAbstractContainer, IJclCollection, IJclList, IJclArray,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FCapacity: Integer;
    FElementData: TDynObjectArray;
    FOwnsObjects: Boolean;
    FSize: Integer;
  protected
    procedure FreeObject(var AObject: TObject);
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclGrowable }
    procedure Grow; overload; virtual;
    procedure Grow(Increment: Integer); overload;
    procedure Grow(Num, Denom: Integer); overload;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclCloneable }
    function Clone: TObject;
    { IJclCollection }
    function Add(AObject: TObject): Boolean; overload;
    function AddAll(const ACollection: IJclCollection): Boolean; overload;
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function ContainsAll(const ACollection: IJclCollection): Boolean;
    function Equals(const ACollection: IJclCollection): Boolean;
    function First: IJclIterator;
    function IsEmpty: Boolean;
    function Last: IJclIterator;
    function Remove(AObject: TObject): Boolean; overload;
    function RemoveAll(const ACollection: IJclCollection): Boolean;
    function RetainAll(const ACollection: IJclCollection): Boolean;
    function Size: Integer;
    { IJclList }
    procedure Insert(Index: Integer; AObject: TObject); overload;
    function InsertAll(Index: Integer; const ACollection: IJclCollection): Boolean; overload;
    function GetObject(Index: Integer): TObject;
    function IndexOf(AObject: TObject): Integer;
    function LastIndexOf(AObject: TObject): Integer;
    function Remove(Index: Integer): TObject; overload;
    procedure SetObject(Index: Integer; AObject: TObject);
    function SubList(First, Count: Integer): IJclList;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity; AOwnsObjects: Boolean = True); overload;
    constructor Create(const ACollection: IJclCollection; AOwnsObjects: Boolean = True); overload;
    destructor Destroy; override;
    property Capacity: Integer read FCapacity write SetCapacity;
    property OwnsObjects: Boolean read FOwnsObjects;
  end;

  {$IFDEF SUPPORTS_GENERICS}

  TJclArrayList<T> = class(TJclAbstractContainer, IJclCollection<T>, IJclList<T>, IJclArray<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FCapacity: Integer;
    FElementData: TJclBase<T>.TDynArray;
    FSize: Integer;
    FOwnsItems: Boolean;
  protected
    function ItemsEqual(const A, B: T): Boolean; virtual; abstract;
    function CreateEmptyArrayList(const ACollection: IJclCollection<T>): TJclArrayList<T>; overload;
    function CreateEmptyArrayList(ACapacity: Integer): TJclArrayList<T>; overload; virtual; abstract;
    procedure FreeItem(var AItem: T);
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclGrowable }
    procedure Grow; overload; virtual;
    procedure Grow(Increment: Integer); overload;
    procedure Grow(Num, Denom: Integer); overload;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclCloneable }
    function Clone: TObject;
    { IJclCollection<T> }
    function Add(const AItem: T): Boolean; overload;
    function AddAll(const ACollection: IJclCollection<T>): Boolean; overload;
    procedure Clear;
    function Contains(const AItem: T): Boolean;
    function ContainsAll(const ACollection: IJclCollection<T>): Boolean;
    function Equals(const ACollection: IJclCollection<T>): Boolean;
    function First: IJclIterator<T>;
    function IsEmpty: Boolean;
    function Last: IJclIterator<T>;
    function Remove(AItem: T): Boolean; overload;
    function RemoveAll(const ACollection: IJclCollection<T>): Boolean;
    function RetainAll(const ACollection: IJclCollection<T>): Boolean;
    function Size: Integer;
    { IJclList<T> }
    procedure Insert(Index: Integer; AItem: T); overload;
    function InsertAll(Index: Integer; const ACollection: IJclCollection<T>): Boolean; overload;
    function GetItem(Index: Integer): T;
    function IndexOf(const AItem: T): Integer;
    function LastIndexOf(const AItem: T): Integer;
    function Remove(Index: Integer): T; overload;
    procedure SetItem(Index: Integer; const AItem: T);
    function SubList(First, Count: Integer): IJclList<T>;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity; AOwnsItems: Boolean = True); overload;
    constructor Create(const ACollection: IJclCollection<T>; AOwnsItems: Boolean = True); overload;
    destructor Destroy; override;
    property Capacity: Integer read FCapacity write SetCapacity;
    property OwnsItems: Boolean read FOwnsItems;
  end;

  // E = External helper to compare items for equality
  // GetHashCode is not used
  TJclArrayListE<T> = class(TJclArrayList<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer): TJclArrayList<T>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; ACapacity: Integer = DefaultContainerCapacity;
      AOwnsItems: Boolean = True); overload;
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; const ACollection: IJclCollection<T>;
      AOwnsItems: Boolean = True); overload;

    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclArrayListF<T> = class(TJclArrayList<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FEqualityCompare: TEqualityCompare<T>;
  protected
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer): TJclArrayList<T>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer = DefaultContainerCapacity;
      AOwnsItems: Boolean = True); overload;
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const ACollection: IJclCollection<T>;
      AOwnsItems: Boolean = True); overload;

    property EqualityCompare: TEqualityCompare<T> read FEqualityCompare write FEqualityCompare;
  end;

  // I = Items can compare themselves to others
  TJclArrayListI<T: IEquatable<T>> = class(TJclArrayList<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  protected
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyArrayList(ACapacity: Integer): TJclArrayList<T>; override;
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

//=== { TIntfItr } ===========================================================

type
  TIntfItr = class(TJclAbstractIterator, IJclIntfIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FOwnList: IJclIntfList;
  protected
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIntfIterator }
    procedure Add(const AInterface: IInterface);
    function GetObject: IInterface;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    procedure Insert(const AInterface: IInterface);
    function Next: IInterface;
    function NextIndex: Integer;
    function Previous: IInterface;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetObject(const AInterface: IInterface);
  public
    constructor Create(const AOwnList: IJclIntfList; ACursor: Integer; AValid: Boolean);
  end;

constructor TIntfItr.Create(const AOwnList: IJclIntfList; ACursor: Integer; AValid: Boolean);
begin
  inherited Create(AOwnList);
  FOwnList := AOwnList;
  FCursor := ACursor;
  Valid := AValid;
end;

procedure TIntfItr.Add(const AInterface: IInterface);
begin
  FOwnList.Add(AInterface);
end;

function TIntfItr.Clone: TObject;
begin
  Result := TIntfItr.Create(FOwnList, FCursor, Valid);
end;

function TIntfItr.GetObject: IInterface;
begin
  Valid := True;
  Result := FOwnList.GetObject(FCursor)
end;

function TIntfItr.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TIntfItr.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

procedure TIntfItr.Insert(const AInterface: IInterface);
begin
  Valid := True;
  FOwnList.Insert(FCursor, AInterface);
end;

function TIntfItr.IntfClone: IInterface;
begin
  Result := TIntfItr.Create(FOwnList, FCursor, Valid);
end;

function TIntfItr.Next: IInterface;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetObject(FCursor);
end;

function TIntfItr.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TIntfItr.Previous: IInterface;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetObject(FCursor);
end;

function TIntfItr.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TIntfItr.Remove;
begin
  Valid := False;
  FOwnList.Remove(FCursor);
end;

procedure TIntfItr.SetObject(const AInterface: IInterface);
begin
  Valid := True;
  FOwnList.SetObject(FCursor, AInterface);
end;

//=== { TStrItr } ============================================================

type
  TStrItr = class(TJclAbstractIterator, IJclStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FOwnList: IJclStrList;
  protected
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclStrIterator }
    procedure Add(const AString: string);
    function GetString: string;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    procedure Insert(const AString: string);
    function Next: string;
    function NextIndex: Integer;
    function Previous: string;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetString(const AString: string);
  public
    constructor Create(const AOwnList: IJclStrList; ACursor: Integer; AValid: Boolean);
  end;

constructor TStrItr.Create(const AOwnList: IJclStrList; ACursor: Integer; AValid: Boolean);
begin
  inherited Create(AOwnList);
  FOwnList := AOwnList;
  FCursor := ACursor;
  Valid := AValid;
end;

procedure TStrItr.Add(const AString: string);
begin
  FOwnList.Add(AString);
end;

function TStrItr.Clone: TObject;
begin
  Result := TStrItr.Create(FOwnList, FCursor, Valid);
end;

function TStrItr.GetString: string;
begin
  Valid := True;
  Result := FOwnList.GetString(FCursor);
end;

function TStrItr.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TStrItr.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TStrItr.IntfClone: IInterface;
begin
  Result := TStrItr.Create(FOwnList, FCursor, Valid);
end;

procedure TStrItr.Insert(const AString: string);
begin
  Valid := True;
  FOwnList.Insert(FCursor, AString);
end;

function TStrItr.Next: string;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetString(FCursor);
end;

function TStrItr.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TStrItr.Previous: string;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetString(FCursor);
end;

function TStrItr.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TStrItr.Remove;
begin
  Valid := False;
  FOwnList.Remove(FCursor);
end;

procedure TStrItr.SetString(const AString: string);
begin
  Valid := True;
  FOwnList.SetString(FCursor, AString);
end;

//=== { TItr } ===============================================================

type
  TItr = class(TJclAbstractIterator, IJclIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FOwnList: IJclList;
  protected
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIterator }
    procedure Add(AObject: TObject);
    function GetObject: TObject;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    procedure Insert(AObject: TObject);
    function Next: TObject;
    function NextIndex: Integer;
    function Previous: TObject;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetObject(AObject: TObject);
  public
    constructor Create(const AOwnList: IJclList; ACursor: Integer; AValid: Boolean);
  end;

constructor TItr.Create(const AOwnList: IJclList; ACursor: Integer; AValid: Boolean);
begin
  inherited Create(AOwnList);
  FOwnList := AOwnList;
  FCursor := ACursor;
  Valid := AValid;
end;

procedure TItr.Add(AObject: TObject);
begin
  FOwnList.Add(AObject);
end;

function TItr.Clone: TObject;
begin
  Result := TItr.Create(FOwnList, FCursor, Valid);
end;

function TItr.GetObject: TObject;
begin
  Valid := True;
  Result := FOwnList.GetObject(FCursor);
end;

function TItr.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TItr.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TItr.IntfClone: IInterface;
begin
  Result := TItr.Create(FOwnList, FCursor, Valid);
end;

procedure TItr.Insert(AObject: TObject);
begin
  Valid := True;
  FOwnList.Insert(FCursor, AObject);
end;

function TItr.Next: TObject;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetObject(FCursor);
end;

function TItr.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TItr.Previous: TObject;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetObject(FCursor);
end;

function TItr.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TItr.Remove;
begin
  Valid := False;
  FOwnList.Remove(FCursor);
end;

procedure TItr.SetObject(AObject: TObject);
begin
  Valid := True;
  FOwnList.SetObject(FCursor, AObject);
end;

{$IFDEF SUPPORTS_GENERICS}

//=== { TItr<T> } ===============================================================

type
  TItr<T> = class(TJclAbstractIterator, IJclIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FOwnList: IJclList<T>;
  protected
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIterator<T> }
    procedure Add(const AItem: T);
    function GetItem: T;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    procedure Insert(const AItem: T);
    function Next: T;
    function NextIndex: Integer;
    function Previous: T;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetItem(const AItem: T);
  public
    constructor Create(const AOwnList: IJclList<T>; ACursor: Integer; AValid: Boolean);
  end;

constructor TItr<T>.Create(const AOwnList: IJclList<T>; ACursor: Integer; AValid: Boolean);
begin
  inherited Create(AOwnList);
  FOwnList := AOwnList;
  FCursor := ACursor;
  Valid := AValid;
end;

procedure TItr<T>.Add(const AItem: T);
begin
  FOwnList.Add(AItem);
end;

function TItr<T>.Clone: TObject;
begin
  Result := TItr<T>.Create(FOwnList, FCursor, Valid);
end;

function TItr<T>.GetItem: T;
begin
  Valid := True;
  Result := FOwnList.GetItem(FCursor);
end;

function TItr<T>.HasNext: Boolean;
begin
  if Valid then
    Result := FCursor < (FOwnList.Size - 1)
  else
    Result := FCursor < FOwnList.Size;
end;

function TItr<T>.HasPrevious: Boolean;
begin
  if Valid then
    Result := FCursor > 0
  else
    Result := FCursor >= 0;
end;

function TItr<T>.IntfClone: IInterface;
begin
  Result := TItr<T>.Create(FOwnList, FCursor, Valid);
end;

procedure TItr<T>.Insert(const AItem: T);
begin
  Valid := True;
  FOwnList.Insert(FCursor, AItem);
end;

function TItr<T>.Next: T;
begin
  if Valid then
    Inc(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetItem(FCursor);
end;

function TItr<T>.NextIndex: Integer;
begin
  if Valid then
    Result := FCursor + 1
  else
    Result := FCursor;
end;

function TItr<T>.Previous: T;
begin
  if Valid then
    Dec(FCursor)
  else
    Valid := True;
  Result := FOwnList.GetItem(FCursor);
end;

function TItr<T>.PreviousIndex: Integer;
begin
  if Valid then
    Result := FCursor - 1
  else
    Result := FCursor;
end;

procedure TItr<T>.Remove;
begin
  Valid := False;
  FOwnList.Remove(FCursor);
end;

procedure TItr<T>.SetItem(const AItem: T);
begin
  Valid := True;
  FOwnList.SetItem(FCursor, AItem);
end;

{$ENDIF SUPPORTS_GENERICS}

//=== { TJclIntfArrayList } ==================================================

constructor TJclIntfArrayList.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  FSize := 0;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FElementData, FCapacity);
end;

constructor TJclIntfArrayList.Create(const ACollection: IJclIntfCollection);
begin
  // (rom) disabled because the following Create already calls inherited
  // inherited Create;
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  Create(ACollection.Size);
  AddAll(ACollection);
end;

destructor TJclIntfArrayList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclIntfArrayList.Add(const AInterface: IInterface): Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize = Capacity then
      Grow;
    FElementData[FSize] := AInterface;
    Inc(FSize);
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.AddAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
    begin
      // (rom) inlining Add() gives about 5 percent performance increase
      if FSize = Capacity then
        Grow;
      FElementData[FSize] := It.Next;
      Inc(FSize);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfArrayList.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FElementData[I] := nil;
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.Clone: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(Self);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.Contains(const AInterface: IInterface): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if FElementData[I] = AInterface then
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

function TJclIntfArrayList.ContainsAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.Equals(const ACollection: IJclIntfCollection): Boolean;
var
  I: Integer;
  It: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FSize - 1 do
      if FElementData[I] <> It.Next then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.First: IJclIntfIterator;
begin
  Result := TIntfItr.Create(Self, 0, False);
end;

function TJclIntfArrayList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TJclIntfArrayList.GetObject(Index: Integer): IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      Result := nil
    else
      Result := FElementData[Index];
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfArrayList.Grow(Increment: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Capacity := Capacity + Increment;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfArrayList.Grow(Num, Denom: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Capacity := Capacity * Num div Denom;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfArrayList.Grow;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Capacity > 64 then
      Capacity := Capacity + Capacity div 4
    else
    if Capacity = 0 then
      Capacity := 64
    else
      Grow(16);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.IndexOf(const AInterface: IInterface): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if FElementData[I] = AInterface then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfArrayList.Insert(Index: Integer; const AInterface: IInterface);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if FSize = Capacity then
      Grow;
    if FSize <> Index then
      MoveArray(FElementData, Index, Index + 1, FSize - Index);
    FElementData[Index] := AInterface;
    Inc(FSize);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.InsertAll(Index: Integer; const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  InsertionSize: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    InsertionSize := ACollection.Size;
    if (FSize + InsertionSize) >= Capacity then
      Grow(InsertionSize);
    MoveArray(FElementData, Index, Index + InsertionSize, FSize - Index);
    Inc(FSize, InsertionSize);
    It := ACollection.First;
    Result := It.HasNext;
    while It.HasNext do
    begin
      FElementData[Index] := It.Next;
      Inc(Index);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.IntfClone: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfArrayList.Create(Self);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfArrayList.Last: IJclIntfIterator;
begin
  Result := TIntfItr.Create(Self, FSize - 1, False);
end;

function TJclIntfArrayList.LastIndexOf(const AInterface: IInterface): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if FElementData[I] = AInterface then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfArrayList.Pack;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Capacity := FSize;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.Remove(const AInterface: IInterface): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := FSize - 1 downto 0 do
      if FElementData[I] = AInterface then // Removes all AInterface
      begin
        FElementData[I] := nil; // Force Release
        if FSize <> I then
          MoveArray(FElementData, I + 1, I, FSize - I);
        Dec(FSize);
        Result := True;
      end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.Remove(Index: Integer): IInterface;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    Result := FElementData[Index];
    FElementData[Index] := nil;
    if FSize <> Index then
      MoveArray(FElementData, Index + 1, Index, FSize - Index);
    Dec(FSize);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.RemoveAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.RetainAll(const ACollection: IJclIntfCollection): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FElementData[I]) then
        Remove(I);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfArrayList.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FElementData, Value);
      FCapacity := Value;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfArrayList.SetObject(Index: Integer; const AInterface: IInterface);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    FElementData[Index] := AInterface;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfArrayList.SubList(First, Count: Integer): IJclIntfList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := TJclIntfArrayList.Create(Count);
    for I := First to Last do
      Result.Add(FElementData[I]);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclStrArrayList } ===================================================

constructor TJclStrArrayList.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  FSize := 0;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FElementData, FCapacity);
end;

constructor TJclStrArrayList.Create(const ACollection: IJclStrCollection);
begin
  // (rom) disabled because the following Create already calls inherited
  // inherited Create;
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  Create(ACollection.Size);
  AddAll(ACollection);
end;

destructor TJclStrArrayList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclStrArrayList.Add(const AString: string): Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize = Capacity then
      Grow;
    FElementData[FSize] := AString;
    Inc(FSize);
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrArrayList.AddAll(const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
    begin
      // (rom) inlining Add() gives about 5 percent performance increase
      // without THREADSAFE and about 30 percent with THREADSAFE
      if FSize = Capacity then
        Grow;
      FElementData[FSize] := It.Next;
      Inc(FSize);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrArrayList.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FElementData[I] := '';
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrArrayList.Clone: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclStrArrayList.Create(Self);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrArrayList.Contains(const AString: string): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if FElementData[I] = AString then
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

function TJclStrArrayList.ContainsAll(const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrArrayList.Equals(const ACollection: IJclStrCollection): Boolean;
var
  I: Integer;
  It: IJclStrIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FSize - 1 do
      if FElementData[I] <> It.Next then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrArrayList.First: IJclStrIterator;
begin
  Result := TStrItr.Create(Self, 0, False);
end;

function TJclStrArrayList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TJclStrArrayList.GetString(Index: Integer): string;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      Result := ''
    else
      Result := FElementData[Index];
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrArrayList.Grow(Increment: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Capacity := Capacity + Increment;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrArrayList.Grow(Num, Denom: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Capacity := Capacity * Num div Denom;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrArrayList.Grow;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Capacity > 64 then
      Capacity := Capacity + Capacity div 4
    else
    if Capacity = 0 then
      Capacity := 64
    else
      Capacity := Capacity + 16;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrArrayList.IndexOf(const AString: string): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if FElementData[I] = AString then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrArrayList.Insert(Index: Integer; const AString: string);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if FSize = Capacity then
      Grow;
    if FSize <> Index then
      MoveArray(FElementData, Index, Index + 1, FSize - Index);
    FElementData[Index] := AString;
    Inc(FSize);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrArrayList.InsertAll(Index: Integer; const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
  InsertionSize: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;

    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;

    if ACollection = nil then
      Exit;
    InsertionSize := ACollection.Size;
    if FSize + InsertionSize >= Capacity then
      Grow(InsertionSize);
    MoveArray(FElementData, Index, Index + InsertionSize, FSize - Index);
    Inc(FSize, InsertionSize);
    It := ACollection.First;
    Result := It.HasNext;
    while It.HasNext do
    begin
      FElementData[Index] := It.Next;
      Inc(Index);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrArrayList.IntfClone: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclStrArrayList.Create(Self);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrArrayList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclStrArrayList.Last: IJclStrIterator;
begin
  Result := TStrItr.Create(Self, FSize - 1, False);
end;

function TJclStrArrayList.LastIndexOf(const AString: string): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if FElementData[I] = AString then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrArrayList.Pack;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Capacity := FSize;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrArrayList.Remove(const AString: string): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := FSize - 1 downto 0 do
      if FElementData[I] = AString then // Removes all AString
      begin
        FElementData[I] := ''; // Force Release
        if FSize <> I then
          MoveArray(FElementData, I + 1, I, FSize - I);
        Dec(FSize);
        Result := True;
      end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrArrayList.Remove(Index: Integer): string;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    Result := FElementData[Index];
    FElementData[Index] := '';
    if FSize <> Index then
      MoveArray(FElementData, Index + 1, Index, FSize - Index);
    Dec(FSize);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrArrayList.RemoveAll(const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrArrayList.RetainAll(const ACollection: IJclStrCollection): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    for I := FSize - 1 downto 0 do
      if not ACollection.Contains(FElementData[I]) then
        Remove(I);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrArrayList.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FElementData, Value);
      FCapacity := Value;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrArrayList.SetString(Index: Integer; const AString: string);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    FElementData[Index] := AString
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrArrayList.Size: Integer;
begin
  Result := FSize;
end;

function TJclStrArrayList.SubList(First, Count: Integer): IJclStrList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := TJclStrArrayList.Create(Count);
    for I := First to Last do
      Result.Add(FElementData[I]);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclArrayList } ======================================================

constructor TJclArrayList.Create(ACapacity: Integer; AOwnsObjects: Boolean);
begin
  inherited Create(nil);
  FSize := 0;
  FOwnsObjects := AOwnsObjects;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FElementData, FCapacity);
end;

constructor TJclArrayList.Create(const ACollection: IJclCollection; AOwnsObjects: Boolean);
begin
  // (rom) disabled because the following Create already calls inherited
  // inherited Create;
  if ACollection = nil then
    raise EJclNoCollectionError.Create;
  Create(ACollection.Size, AOwnsObjects);
  AddAll(ACollection);
end;

destructor TJclArrayList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclArrayList.Add(AObject: TObject): Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize = Capacity then
      Grow;
    FElementData[FSize] := AObject;
    Inc(FSize);
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.AddAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
    begin
      // (rom) inlining Add() gives about 5 percent performance increase
      if FSize = Capacity then
        Grow;
      FElementData[FSize] := It.Next;
      Inc(FSize);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeObject(FElementData[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.Clone: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(Self, False); // Only one can have FOwnsObject = True
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.Contains(AObject: TObject): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if FElementData[I] = AObject then
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

function TJclArrayList.ContainsAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.Equals(const ACollection: IJclCollection): Boolean;
var
  I: Integer;
  It: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FSize - 1 do
      if FElementData[I] <> It.Next then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.First: IJclIterator;
begin
  Result := TItr.Create(Self, 0, False);
end;

procedure TJclArrayList.FreeObject(var AObject: TObject);
begin
  if FOwnsObjects then
    FreeAndNil(AObject)
  else
    AObject := nil;
end;

function TJclArrayList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TJclArrayList.GetObject(Index: Integer): TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      Result := nil
    else
      Result := FElementData[Index];
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList.Grow(Increment: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Capacity := Capacity + Increment;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList.Grow(Num, Denom: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Capacity := Capacity * Num div Denom;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList.Grow;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Capacity > 64 then
      Capacity := Capacity + Capacity div 4
    else
    if Capacity = 0 then
      Capacity := 64
    else
      Capacity := Capacity + 16;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.IndexOf(AObject: TObject): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if FElementData[I] = AObject then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList.Insert(Index: Integer; AObject: TObject);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if FSize = Capacity then
      Grow;
    if FSize <> Index then
      MoveArray(FElementData, Index, Index + 1, FSize - Index);
    FElementData[Index] := AObject;
    Inc(FSize);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.InsertAll(Index: Integer; const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  InsertionSize: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    InsertionSize := ACollection.Size;
    if FSize + InsertionSize >= Capacity then
      Grow(InsertionSize);
    MoveArray(FElementData, Index, Index + InsertionSize, FSize - Index);
    Inc(FSize, InsertionSize);
    It := ACollection.First;
    Result := It.HasNext;
    while It.HasNext do
    begin
      FElementData[Index] := It.Next;
      Inc(Index);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.IntfClone: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclArrayList.Create(Self, False); // Only one can have FOwnsObject = True
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclArrayList.Last: IJclIterator;
begin
  Result := TItr.Create(Self, FSize - 1, False);
end;

function TJclArrayList.LastIndexOf(AObject: TObject): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if FElementData[I] = AObject then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList.Pack;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Capacity := FSize;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.Remove(AObject: TObject): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := FSize - 1 downto 0 do
      if FElementData[I] = AObject then // Removes all AObject
      begin
        FreeObject(FElementData[I]);
        if FSize <> I then
          MoveArray(FElementData, I + 1, I, FSize - I);
        Dec(FSize);
        Result := True;
      end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.Remove(Index: Integer): TObject;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    if OwnsObjects then
      Result := nil
    else
      Result := FElementData[Index];
    FreeObject(FElementData[Index]);
    if FSize <> Index then
      MoveArray(FElementData, Index + 1, Index, FSize - Index);
    Dec(FSize);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.RemoveAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.RetainAll(const ACollection: IJclCollection): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    for I := FSize - 1 to 0 do
      if not ACollection.Contains(FElementData[I]) then
        Remove(I);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FElementData, Value);
      FCapacity := Value;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList.SetObject(Index: Integer; AObject: TObject);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    FreeObject(FElementData[Index]);
    FElementData[Index] := AObject;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.Size: Integer;
begin
  Result := FSize;
end;

function TJclArrayList.SubList(First, Count: Integer): IJclList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := TJclArrayList.Create(Count, False);
    for I := First to Last do
      Result.Add(FElementData[I]);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_GENERICS}

//=== { TJclArrayList<T> } ===================================================

constructor TJclArrayList<T>.Create(ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(nil);
  FSize := 0;
  FOwnsItems := AOwnsItems;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FElementData, FCapacity);
end;

constructor TJclArrayList<T>.Create(const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  // (rom) disabled because the following Create already calls inherited
  // inherited Create;
  if ACollection = nil then
    raise EJclIllegalArgumentError.Create;
  Create(ACollection.Size, AOwnsItems);
  AddAll(ACollection);
end;

destructor TJclArrayList<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclArrayList<T>.Add(const AItem: T): Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FSize = Capacity then
      Grow;
    FElementData[FSize] := AItem;
    Inc(FSize);
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.AddAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
    begin
      // (rom) inlining Add() gives about 5 percent performance increase
      if FSize = Capacity then
        Grow;
      FElementData[FSize] := It.Next;
      Inc(FSize);
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList<T>.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeItem(FElementData[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.Clone: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyArrayList(Self);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.Contains(const AItem: T): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FSize - 1 do
      if ItemsEqual(AItem, FElementData[I]) then
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

function TJclArrayList<T>.ContainsAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.CreateEmptyArrayList(const ACollection: IJclCollection<T>): TJclArrayList<T>;
begin
  Result := CreateEmptyArrayList(ACollection.Size);
  Result.AddAll(ACollection);
end;

function TJclArrayList<T>.Equals(const ACollection: IJclCollection<T>): Boolean;
var
  I: Integer;
  It: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    if FSize <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FSize - 1 do
      if ItemsEqual(FElementData[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.First: IJclIterator<T>;
begin
  Result := TItr<T>.Create(Self, 0, False);
end;

procedure TJclArrayList<T>.FreeItem(var AItem: T);
begin
  if OwnsItems then
    FreeAndNil(AItem)
  else
    AItem := Default(T);
end;

function TJclArrayList<T>.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TJclArrayList<T>.GetItem(Index: Integer): T;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      Result := Default(T)
    else
      Result := FElementData[Index];
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList<T>.Grow;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Capacity > 64 then
      Capacity := Capacity + Capacity div 4
    else
    if Capacity = 0 then
      Capacity := 64
    else
      Capacity := Capacity + 16;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList<T>.Grow(Increment: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Capacity := Capacity + Increment;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList<T>.Grow(Num, Denom: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Capacity := Capacity * Num div Denom;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.IndexOf(const AItem: T): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FSize - 1 do
      if ItemsEqual(AItem, FElementData[I]) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList<T>.Insert(Index: Integer; AItem: T);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if FSize = Capacity then
      Grow;
    if FSize <> Index then
      TJclBase<T>.MoveArray(FElementData, Index, Index + 1, FSize - Index);
    FElementData[Index] := AItem;
    Inc(FSize);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.InsertAll(Index: Integer; const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
  InsertionSize: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    InsertionSize := ACollection.Size;
    if FSize + InsertionSize >= Capacity then
      Grow(InsertionSize);
    TJclBase<T>.MoveArray(FElementData, Index, Index + InsertionSize, Size - Index);
    Inc(FSize, InsertionSize);
    It := ACollection.First;
    Result := It.HasNext;
    while It.HasNext do
    begin
      FElementData[Index] := It.Next;
      Inc(Index);
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.IntfClone: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyArrayList(Self);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclArrayList<T>.Last: IJclIterator<T>;
begin
  Result := TItr<T>.Create(Self, FSize - 1, False);
end;

function TJclArrayList<T>.LastIndexOf(const AItem: T): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(AItem, FElementData[I]) then
      begin
        Result := I;
        Break;
      end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList<T>.Pack;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Capacity := FSize;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.Remove(AItem: T): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(AItem, FElementData[I]) then // Removes all AObject
      begin
        FreeItem(FElementData[I]);
        if FSize <> I then
          TJclBase<T>.MoveArray(FElementData, I + 1, I, FSize - I);
        Dec(FSize);
        Result := True;
      end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.Remove(Index: Integer): T;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    if OwnsItems then
      Result := Default(T)
    else
      Result := FElementData[Index];
    FreeItem(FElementData[Index]);
    if FSize <> Index then
      TJclBase<T>.MoveArray(FElementData, Index + 1, Index, FSize - Index);
    Dec(FSize);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.RemoveAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection = nil then
      Exit;
    It := ACollection.First;
    while It.HasNext do
      Result := Remove(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.RetainAll(const ACollection: IJclCollection<T>): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    for I := FSize - 1 to 0 do
      if not ACollection.Contains(FElementData[I]) then
        Remove(I);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList<T>.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value >= FSize then
    begin
      SetLength(FElementData, Value);
      FCapacity := Value;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList<T>.SetItem(Index: Integer; const AItem: T);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    FreeItem(FElementData[Index]);
    FElementData[Index] := AItem;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList<T>.Size: Integer;
begin
  Result := FSize;
end;

function TJclArrayList<T>.SubList(First, Count: Integer): IJclList<T>;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyArrayList(Count);
    for I := First to Last do
      Result.Add(FElementData[I]);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclArrayListE<T> } ==================================================

constructor TJclArrayListE<T>.Create(const AEqualityComparer: IEqualityComparer<T>; ACapacity: Integer;
  AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

constructor TJclArrayListE<T>.Create(const AEqualityComparer: IEqualityComparer<T>;
  const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

function TJclArrayListE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityComparer.Equals(A, B);
end;

function TJclArrayListE<T>.CreateEmptyArrayList(ACapacity: Integer): TJclArrayList<T>;
begin
  Result := TJclArrayListE<T>.Create(EqualityComparer, ACapacity, False);
end;

//=== { TJclArrayListF<T> } ==================================================

constructor TJclArrayListF<T>.Create(const AEqualityCompare: TEqualityCompare<T>;
  ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityCompare := AEqualityCompare;
end;

constructor TJclArrayListF<T>.Create(const AEqualityCompare: TEqualityCompare<T>; const ACollection: IJclCollection<T>;
  AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FEqualityCompare := AEqualityCompare;
end;

function TJclArrayListF<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if not Assigned(EqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityCompare(A, B);
end;

function TJclArrayListF<T>.CreateEmptyArrayList(ACapacity: Integer): TJclArrayList<T>;
begin
  Result := TJclArrayListF<T>.Create(EqualityCompare, ACapacity, False);
end;

//=== { TJclArrayListI<T> } ==================================================

function TJclArrayListI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  Result := A.Equals(B);
end;

function TJclArrayListI<T>.CreateEmptyArrayList(ACapacity: Integer): TJclArrayList<T>;
begin
  Result := TJclArrayListI<T>.Create(ACapacity, False);
end;

{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

