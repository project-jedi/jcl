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
{ The Original Code is Vector.pas.                                                                 }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Daniele Teti (dade2004)                                                                        }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
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

unit JclVectors;

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
  TJclIntfVector = class(TJclAbstractContainer, IJclIntfCollection, IJclIntfList, IJclIntfArray,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FCount: Integer;
    FCapacity: Integer;
    FItems: TDynIInterfaceArray;
  protected
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclGrowable }
    procedure Grow; overload; virtual;
    procedure Grow(Increment: Integer); overload;
    procedure Grow(Num, Denom: Integer); overload;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
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
    constructor Create(ACapacity: Integer = DefaultContainerCapacity);
    destructor Destroy; override;

    property Items: TDynIInterfaceArray read FItems;
  end;

  //Daniele Teti 02/03/2005
  TJclStrVector = class(TJclStrCollection, IJclStrList, IJclStrArray,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FCount: Integer;
    FCapacity: Integer;
    FItems: TDynStringArray;
  protected
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclGrowable }
    procedure Grow; overload; virtual;
    procedure Grow(Increment: Integer); overload;
    procedure Grow(Num, Denom: Integer); overload;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
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
    constructor Create(ACapacity: Integer = DefaultContainerCapacity);
    destructor Destroy; override;

    property Items: TDynStringArray read FItems;
  end;

  TJclVector = class(TJclAbstractContainer, IJclCollection, IJclList, IJclArray,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FCount: Integer;
    FCapacity: Integer;
    FOwnsObjects: Boolean;
    FItems: TDynObjectArray;
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
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
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
    constructor Create(ACapacity: Integer = DefaultContainerCapacity; AOwnsObjects: Boolean = False);
    destructor Destroy; override;

    property Items: TDynObjectArray read FItems;
    property OwnsObjects: Boolean read FOwnsObjects;
  end;

  {$IFDEF SUPPORTS_GENERICS}
  TJclVector<T> = class(TJclAbstractContainer, IJclCollection<T>, IJclList<T>, IJclArray<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FCount: Integer;
    FCapacity: Integer;
    FOwnsItems: Boolean;
    FItems: TJclBase<T>.TDynArray;
  protected
    procedure FreeItem(var AItem: T);
    function CreateEmptyVector(ACapacity: Integer): TJclVector<T>; virtual; abstract;
    function ItemsEqual(const A, B: T): Boolean; virtual; abstract;
    { IJclPackable }
    procedure Pack;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    { IJclGrowable }
    procedure Grow; overload; virtual;
    procedure Grow(Increment: Integer); overload;
    procedure Grow(Num, Denom: Integer); overload;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclCollection<T> }
    function Add(const AItem: T): Boolean;
    function AddAll(const ACollection: IJclCollection<T>): Boolean;
    procedure Clear;
    function Contains(const AItem: T): Boolean;
    function ContainsAll(const ACollection: IJclCollection<T>): Boolean;
    function Equals(const ACollection: IJclCollection<T>): Boolean;
    function First: IJclIterator<T>;
    function IsEmpty: Boolean;
    function Last: IJclIterator<T>;
    function Remove(const AItem: T): Boolean; overload;
    function RemoveAll(const ACollection: IJclCollection<T>): Boolean;
    function RetainAll(const ACollection: IJclCollection<T>): Boolean;
    function Size: Integer;
    { IJclList<T> }
    procedure Insert(Index: Integer; const AItem: T);
    function InsertAll(Index: Integer; const ACollection: IJclCollection<T>): Boolean;
    function GetItem(Index: Integer): T;
    function IndexOf(const AItem: T): Integer;
    function LastIndexOf(const AItem: T): Integer;
    function Remove(Index: Integer): T; overload;
    procedure SetItem(Index: Integer; const AItem: T);
    function SubList(First, Count: Integer): IJclList<T>;
  public
    constructor Create(ACapacity: Integer = DefaultContainerCapacity; AOwnsItems: Boolean = True);
    destructor Destroy; override;

    property Items: TJclBase<T>.TDynArray read FItems;
    property OwnsItems: Boolean read FOwnsItems;
  end;

  // E = External helper to compare items for equality (GetHashCode is not used)
  TJclVectorE<T> = class(TJclVector<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    function CreateEmptyVector(ACapacity: Integer): TJclVector<T>; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; ACapacity: Integer = DefaultContainerCapacity;
      AOwnsItems: Boolean = True);
    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclVectorF<T> = class(TJclVector<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FEqualityCompare: TEqualityCompare<T>;
  protected
    function CreateEmptyVector(ACapacity: Integer): TJclVector<T>; override;
    function ItemsEqual(const A, B: T): Boolean; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer = DefaultContainerCapacity;
      AOwnsItems: Boolean = True);
    property EqualityCompare: TEqualityCompare<T> read FEqualityCompare write FEqualityCompare;
  end;

  // I = Items can compare themselves to an other for equality
  TJclVectorI<T: IEquatable<T>> = class(TJclVector<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  protected
    function CreateEmptyVector(ACapacity: Integer): TJclVector<T>; override;
    function ItemsEqual(const A, B: T): Boolean; override;
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
    { IJclIntfIterator}
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
    constructor Create(const OwnList: IJclIntfList; ACursor: Integer; AValid: Boolean);
  end;

constructor TIntfItr.Create(const OwnList: IJclIntfList; ACursor: Integer; AValid: Boolean);
begin
  inherited Create(OwnList);
  FOwnList := OwnList;
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
  Result := FOwnList.GetObject(FCursor);
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
    { IJclStrIterator}
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
    constructor Create(const OwnList: IJclStrList; ACursor: Integer; AValid: Boolean);
  end;

constructor TStrItr.Create(const OwnList: IJclStrList; ACursor: Integer; AValid: Boolean);
begin
  inherited Create(OwnList);
  FOwnList := OwnList;
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

procedure TStrItr.Insert(const AString: string);
begin
  Valid := True;
  FOwnList.Insert(FCursor, AString);
end;

function TStrItr.IntfClone: IInterface;
begin
  Result := TStrItr.Create(FOwnList, FCursor, Valid);
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
    constructor Create(const OwnList: IJclList; ACursor: Integer; AValid: Boolean);
  end;

constructor TItr.Create(const OwnList: IJclList; ACursor: Integer; AValid: Boolean);
begin
  inherited Create(OwnList);
  FOwnList := OwnList;
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

procedure TItr.Insert(AObject: TObject);
begin
  Valid := True;
  FOwnList.Insert(FCursor, AObject);
end;

function TItr.IntfClone: IInterface;
begin
  Result := TItr.Create(FOwnList, FCursor, Valid);
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

//=== { TItr<T> } ============================================================

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
    { IJclIterator}
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
    constructor Create(const OwnList: IJclList<T>; ACursor: Integer; AValid: Boolean);
  end;

constructor TItr<T>.Create(const OwnList: IJclList<T>; ACursor: Integer; AValid: Boolean);
begin
  inherited Create(OwnList);
  FOwnList := OwnList;
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

procedure TItr<T>.Insert(const AItem: T);
begin
  Valid := True;
  FOwnList.Insert(FCursor, AItem);
end;

function TItr<T>.IntfClone: IInterface;
begin
  Result := TItr<T>.Create(FOwnList, FCursor, Valid);
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

//=== { TJclIntfVector } =====================================================

constructor TJclIntfVector.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  FCount := 0;
  SetCapacity(ACapacity);
end;

destructor TJclIntfVector.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclIntfVector.Insert(Index: Integer; const AInterface: IInterface);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index > FCount) then
      raise EJclOutOfBoundsError.Create;
    if FCount = FCapacity then
      Grow;
    MoveArray(FItems, Index, Index + 1, FCount - Index);
    FItems[Index] := AInterface;
    Inc(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.Add(const AInterface: IInterface): Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCount = FCapacity then
      Grow;
    FItems[FCount] := AInterface;
    Inc(FCount);
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.InsertAll(Index: Integer; const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  InsertionSize: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if (Index < 0) or (Index > FCount) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    InsertionSize := ACollection.Size;
    if InsertionSize <> 0 then
    begin
      if (FCount + InsertionSize) > FCapacity then
        Grow(InsertionSize);
      Inc(FCount, InsertionSize);
      MoveArray(FItems, Index, Index + InsertionSize, InsertionSize);
      It := ACollection.First;
      while It.HasNext do
      begin
        FItems[Index] := It.Next;
        Inc(Index);
      end;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.AddAll(const ACollection: IJclIntfCollection): Boolean;
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
      Add(It.Next);
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfVector.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCount - 1 do
      FItems[I] := nil;
    FCount := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.Clone: TObject;
var
  NewList: TJclIntfVector;
begin
  NewList := TJclIntfVector.Create(FCount);
  NewList.AddAll(Self);
  Result := NewList;
end;

function TJclIntfVector.Contains(const AInterface: IInterface): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FCount - 1 do
      if Items[I] = AInterface then
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

function TJclIntfVector.ContainsAll(const ACollection: IJclIntfCollection): Boolean;
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

function TJclIntfVector.Equals(const ACollection: IJclIntfCollection): Boolean;
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
    if FCount <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FCount - 1 do
      if Items[I] <> It.Next then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TJclIntfVector.GetObject(Index: Integer): IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) or (Index < FCount) then
      Result := Items[Index]
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfVector.Grow(Num, Denom: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCapacity * Num div Denom);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfVector.Grow(Increment: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCapacity + Increment);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfVector.Grow;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCapacity > 64 then
      SetCapacity(FCapacity + FCapacity div 4)
    else
      SetCapacity(FCapacity + 16);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.IndexOf(const AInterface: IInterface): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FCount - 1 do
      if Items[I] = AInterface then
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

function TJclIntfVector.First: IJclIntfIterator;
begin
  Result := TIntfItr.Create(Self, 0, False);
end;

function TJclIntfVector.IntfClone: IInterface;
var
  NewList: TJclIntfVector;
begin
  NewList := TJclIntfVector.Create(FCount);
  NewList.AddAll(Self);
  Result := NewList;
end;

function TJclIntfVector.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclIntfVector.Last: IJclIntfIterator;
begin
  Result := TIntfItr.Create(Self, FCount - 1, False);
end;

function TJclIntfVector.LastIndexOf(const AInterface: IInterface): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FCount - 1 downto 0 do
      if Items[I] = AInterface then
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

procedure TJclIntfVector.Pack;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.Remove(Index: Integer): IInterface;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FCount) then
      raise EJclOutOfBoundsError.Create;
    Result := FItems[Index];
    FItems[Index] := nil;
    MoveArray(FItems, Index + 1, Index, FCount - Index);
    Dec(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.Remove(const AInterface: IInterface): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := FCount - 1 downto 0 do
      if FItems[I] = AInterface then // Removes all AInterface
      begin
        FItems[I] := nil; // Force Release
        MoveArray(FItems, I + 1, I, FCount - I);
        Dec(FCount);
        Result := True;
      end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.RemoveAll(const ACollection: IJclIntfCollection): Boolean;
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

function TJclIntfVector.RetainAll(const ACollection: IJclIntfCollection): Boolean;
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
    for I := FCount - 1 downto 0 do
      if not ACollection.Contains(Items[I]) then
        Remove(I);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfVector.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value < FCount then
      raise EJclOutOfBoundsError.Create;
    FCapacity := Value;
    SetLength(FItems, Value);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfVector.SetObject(Index: Integer; const AInterface: IInterface);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FCount) then
      raise EJclOutOfBoundsError.Create;
    FItems[Index] := AInterface;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.Size: Integer;
begin
  Result := FCount;
end;

function TJclIntfVector.SubList(First, Count: Integer): IJclIntfList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FCount then
      Last := FCount - 1;
    Result := TJclIntfVector.Create(Count);
    for I := First to Last do
      Result.Add(Items[I]);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclStrVector } ======================================================

constructor TJclStrVector.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  FCount := 0;
  SetCapacity(ACapacity);
end;

destructor TJclStrVector.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclStrVector.Insert(Index: Integer; const AString: string);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index > FCount) then
      raise EJclOutOfBoundsError.Create;
    if FCount = FCapacity then
      Grow;
    MoveArray(FItems, Index, Index + 1, FCount - Index);
    FItems[Index] := AString;
    Inc(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrVector.Add(const AString: string): Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCount = FCapacity then
      Grow;
    FItems[FCount] := AString;
    Inc(FCount);
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrVector.InsertAll(Index: Integer; const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
  InsertionSize: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if (Index < 0) or (Index > FCount) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    InsertionSize := ACollection.Size;
    if InsertionSize <> 0 then
    begin
      if (FCount + InsertionSize) > FCapacity then
        Grow(InsertionSize);
      Inc(FCount, InsertionSize);
      MoveArray(FItems, Index, Index + InsertionSize, InsertionSize);
      It := ACollection.First;
      while It.HasNext do
      begin
        FItems[Index] := It.Next;
        Inc(Index);
      end;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrVector.AddAll(const ACollection: IJclStrCollection): Boolean;
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
      Add(It.Next);
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrVector.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCount - 1 do
      FItems[I] := '';
    FCount := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrVector.Clone: TObject;
var
  NewList: TJclStrVector;
begin
  NewList := TJclStrVector.Create(FCount);
  NewList.AddAll(Self);
  Result := NewList;
end;

function TJclStrVector.Contains(const AString: string): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FCount - 1 do
      if Items[I] = AString then
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

function TJclStrVector.ContainsAll(const ACollection: IJclStrCollection): Boolean;
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

function TJclStrVector.Equals(const ACollection: IJclStrCollection): Boolean;
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
    if FCount <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FCount - 1 do
      if Items[I] <> It.Next then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrVector.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TJclStrVector.GetString(Index: Integer): string;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) or (Index < FCount) then
      Result := Items[Index]
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrVector.Grow(Num, Denom: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCapacity * Num div Denom);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrVector.Grow(Increment: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCapacity + Increment);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrVector.Grow;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCapacity > 64 then
      SetCapacity(FCapacity + FCapacity div 4)
    else
      SetCapacity(FCapacity + 16);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrVector.IndexOf(const AString: string): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FCount - 1 do
      if Items[I] = AString then
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

function TJclStrVector.First: IJclStrIterator;
begin
  Result := TStrItr.Create(Self, 0, False);
end;

function TJclStrVector.IntfClone: IInterface;
var
  NewList: TJclStrVector;
begin
  NewList := TJclStrVector.Create(FCount);
  NewList.AddAll(Self);
  Result := NewList;
end;

function TJclStrVector.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclStrVector.Last: IJclStrIterator;
begin
  Result := TStrItr.Create(Self, FCount - 1, False);
end;

function TJclStrVector.LastIndexOf(const AString: string): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FCount - 1 downto 0 do
      if Items[I] = AString then
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

procedure TJclStrVector.Pack;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrVector.Remove(Index: Integer): string;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FCount) then
      raise EJclOutOfBoundsError.Create;
    Result := FItems[Index];
    FItems[Index] := '';
    MoveArray(FItems, Index + 1, Index, FCount - Index);
    Dec(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrVector.Remove(const AString: string): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := FCount - 1 downto 0 do
      if FItems[I] = AString then // Removes all AString
      begin
        FItems[I] := ''; // Force Release
        MoveArray(FItems, I + 1, I, FCount - I);
        Dec(FCount);
        Result := True;
      end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrVector.RemoveAll(const ACollection: IJclStrCollection): Boolean;
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

function TJclStrVector.RetainAll(const ACollection: IJclStrCollection): Boolean;
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
    for I := FCount - 1 downto 0 do
      if not ACollection.Contains(Items[I]) then
        Remove(I);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrVector.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value < FCount then
      raise EJclOutOfBoundsError.Create;
    FCapacity := Value;
    SetLength(FItems, Value);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrVector.SetString(Index: Integer; const AString: string);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FCount) then
      raise EJclOutOfBoundsError.Create;
    FItems[Index] := AString;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrVector.Size: Integer;
begin
  Result := FCount;
end;

function TJclStrVector.SubList(First, Count: Integer): IJclStrList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FCount then
      Last := FCount - 1;
    Result := TJclStrVector.Create(Count);
    for I := First to Last do
      Result.Add(Items[I]);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclVector } =========================================================

constructor TJclVector.Create(ACapacity: Integer; AOwnsObjects: Boolean);
begin
  inherited Create(nil);
  FCount := 0;
  FOwnsObjects := AOwnsObjects;
  SetCapacity(ACapacity);
end;

destructor TJclVector.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclVector.Insert(Index: Integer; AObject: TObject);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index > FCount) then
      raise EJclOutOfBoundsError.Create;
    if FCount = FCapacity then
      Grow;
    MoveArray(FItems, Index, Index + 1, FCount - Index);
    FItems[Index] := AObject;
    Inc(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.Add(AObject: TObject): Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCount = FCapacity then
      Grow;
    FItems[FCount] := AObject;
    Inc(FCount);
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.InsertAll(Index: Integer; const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  InsertionSize: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if (Index < 0) or (Index > FCount) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    InsertionSize := ACollection.Size;
    if InsertionSize <> 0 then
    begin
      if (FCount + InsertionSize) > FCapacity then
        Grow(InsertionSize);
      Inc(FCount, InsertionSize);
      MoveArray(FItems, Index, Index + InsertionSize, InsertionSize);
      It := ACollection.First;
      while It.HasNext do
      begin
        FItems[Index] := It.Next;
        Inc(Index);
      end;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.AddAll(const ACollection: IJclCollection): Boolean;
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
      Add(It.Next);
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCount - 1 do
      FreeObject(FItems[I]);
    FCount := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.Clone: TObject;
var
  NewList: TJclVector;
begin
  NewList := TJclVector.Create(FCount, False);
  NewList.AddAll(Self);
  Result := NewList;
end;

function TJclVector.Contains(AObject: TObject): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FCount - 1 do
      if Items[I] = AObject then
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

function TJclVector.ContainsAll(const ACollection: IJclCollection): Boolean;
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

function TJclVector.Equals(const ACollection: IJclCollection): Boolean;
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
    if FCount <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FCount - 1 do
      if Items[I] <> It.Next then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector.FreeObject(var AObject: TObject);
begin
  if FOwnsObjects then
    FreeAndNil(AObject)
  else
    AObject := nil;
end;

function TJclVector.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TJclVector.GetObject(Index: Integer): TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) or (Index < FCount) then
      Result := Items[Index]
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector.Grow(Num, Denom: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCapacity * Num div Denom);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector.Grow(Increment: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCapacity + Increment);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector.Grow;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCapacity > 64 then
      SetCapacity(FCapacity + FCapacity div 4)
    else
      SetCapacity(FCapacity + 16);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.IndexOf(AObject: TObject): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FCount - 1 do
      if Items[I] = AObject then
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

function TJclVector.First: IJclIterator;
begin
  Result := TItr.Create(Self, 0, False);
end;

function TJclVector.IntfClone: IInterface;
var
  NewList: TJclVector;
begin
  NewList := TJclVector.Create(FCount, False);
  NewList.AddAll(Self);
  Result := NewList;
end;

function TJclVector.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclVector.Last: IJclIterator;
begin
  Result := TItr.Create(Self, FCount - 1, False);
end;

function TJclVector.LastIndexOf(AObject: TObject): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FCount - 1 downto 0 do
      if Items[I] = AObject then
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

procedure TJclVector.Pack;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.Remove(Index: Integer): TObject;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FCount) then
      raise EJclOutOfBoundsError.Create;
    if OwnsObjects then
      Result := nil
    else
      Result := FItems[Index];
    FreeObject(FItems[Index]);
    MoveArray(FItems, Index + 1, Index, FCount - Index);
    Dec(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.Remove(AObject: TObject): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := FCount - 1 downto 0 do
      if FItems[I] = AObject then // Removes all AString
      begin
        FreeObject(FItems[I]); // Force Release
        MoveArray(FItems, I + 1, I, FCount - I);
        Dec(FCount);
        Result := True;
      end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.RemoveAll(const ACollection: IJclCollection): Boolean;
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

function TJclVector.RetainAll(const ACollection: IJclCollection): Boolean;
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
    for I := FCount - 1 downto 0 do
      if not ACollection.Contains(Items[I]) then
        Remove(I);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value < FCount then
      raise EJclOutOfBoundsError.Create;
    FCapacity := Value;
    SetLength(FItems, Value);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector.SetObject(Index: Integer; AObject: TObject);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FCount) then
      raise EJclOutOfBoundsError.Create;
    FreeObject(FItems[Index]);
    FItems[Index] := AObject;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.Size: Integer;
begin
  Result := FCount;
end;

function TJclVector.SubList(First, Count: Integer): IJclList;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FCount then
      Last := FCount - 1;
    Result := TJclVector.Create(Count);
    for I := First to Last do
      Result.Add(Items[I]);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_GENERICS}

//=== { TJclVector<T> } ======================================================

constructor TJclVector<T>.Create(ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(nil);
  FCount := 0;
  FOwnsItems := AOwnsItems;
  SetCapacity(ACapacity);
end;

destructor TJclVector<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclVector<T>.Insert(Index: Integer; const AItem: T);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index > FCount) then
      raise EJclOutOfBoundsError.Create;
    if FCount = FCapacity then
      Grow;
    TJclBase<T>.MoveArray(FItems, Index, Index + 1, FCount - Index);
    FItems[Index] := AItem;
    Inc(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.Add(const AItem: T): Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCount = FCapacity then
      Grow;
    FItems[FCount] := AItem;
    Inc(FCount);
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.InsertAll(Index: Integer; const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
  InsertionSize: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if (Index < 0) or (Index > FCount) then
      raise EJclOutOfBoundsError.Create;
    if ACollection = nil then
      Exit;
    InsertionSize := ACollection.Size;
    if InsertionSize <> 0 then
    begin
      if (FCount + InsertionSize) > FCapacity then
        Grow(InsertionSize);
      Inc(FCount, InsertionSize);
      TJclBase<T>.MoveArray(FItems, Index, Index + InsertionSize, InsertionSize);
      It := ACollection.First;
      while It.HasNext do
      begin
        FItems[Index] := It.Next;
        Inc(Index);
      end;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.AddAll(const ACollection: IJclCollection<T>): Boolean;
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
      Add(It.Next);
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector<T>.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FCount - 1 do
      FreeItem(FItems[I]);
    FCount := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.Clone: TObject;
var
  NewList: TJclVector<T>;
begin
  NewList := CreateEmptyVector(FCount);
  NewList.AddAll(Self);
  Result := NewList;
end;

function TJclVector<T>.Contains(const AItem: T): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := 0 to FCount - 1 do
      if ItemsEqual(Items[I], AItem) then
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

function TJclVector<T>.ContainsAll(const ACollection: IJclCollection<T>): Boolean;
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

function TJclVector<T>.Equals(const ACollection: IJclCollection<T>): Boolean;
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
    if FCount <> ACollection.Size then
      Exit;
    It := ACollection.First;
    for I := 0 to FCount - 1 do
      if not ItemsEqual(Items[I], It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector<T>.FreeItem(var AItem: T);
begin
  if FOwnsItems then
    FreeAndNil(AItem)
  else
    AItem := Default(T);
end;

function TJclVector<T>.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TJclVector<T>.GetItem(Index: Integer): T;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if (Index >= 0) or (Index < FCount) then
      Result := Items[Index]
    else
      Result := Default(T);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector<T>.Grow(Num, Denom: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCapacity * Num div Denom);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector<T>.Grow(Increment: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCapacity + Increment);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector<T>.Grow;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if FCapacity > 64 then
      SetCapacity(FCapacity + FCapacity div 4)
    else
      SetCapacity(FCapacity + 16);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.IndexOf(const AItem: T): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := 0 to FCount - 1 do
      if ItemsEqual(Items[I], AItem) then
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

function TJclVector<T>.First: IJclIterator<T>;
begin
  Result := TItr<T>.Create(Self, 0, False);
end;

function TJclVector<T>.IntfClone: IInterface;
var
  NewList: TJclVector<T>;
begin
  NewList := CreateEmptyVector(FCount);
  NewList.AddAll(Self);
  Result := NewList;
end;

function TJclVector<T>.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclVector<T>.Last: IJclIterator<T>;
begin
  Result := TItr<T>.Create(Self, FCount - 1, False);
end;

function TJclVector<T>.LastIndexOf(const AItem: T): Integer;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    for I := FCount - 1 downto 0 do
      if ItemsEqual(Items[I], AItem) then
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

procedure TJclVector<T>.Pack;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    SetCapacity(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.Remove(Index: Integer): T;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FCount) then
      raise EJclOutOfBoundsError.Create;
    if OwnsItems then
      Result := Default(T)
    else
      Result := FItems[Index];
    FreeItem(FItems[Index]);
    TJclBase<T>.MoveArray(FItems, Index + 1, Index, FCount - Index);
    Dec(FCount);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.Remove(const AItem: T): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := FCount - 1 downto 0 do
      if ItemsEqual(FItems[I], AItem) then // Removes all AItem
      begin
        FreeItem(FItems[I]); // Force Release
        TJclBase<T>.MoveArray(FItems, I + 1, I, FCount - I);
        Dec(FCount);
        Result := True;
      end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.RemoveAll(const ACollection: IJclCollection<T>): Boolean;
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

function TJclVector<T>.RetainAll(const ACollection: IJclCollection<T>): Boolean;
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
    for I := FCount - 1 downto 0 do
      if not ACollection.Contains(Items[I]) then
        Remove(I);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector<T>.SetCapacity(Value: Integer);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if Value < FCount then
      raise EJclOutOfBoundsError.Create;
    FCapacity := Value;
    SetLength(FItems, Value);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector<T>.SetItem(Index: Integer; const AItem: T);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FCount) then
      raise EJclOutOfBoundsError.Create;
    FreeItem(FItems[Index]);
    FItems[Index] := AItem;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.Size: Integer;
begin
  Result := FCount;
end;

function TJclVector<T>.SubList(First, Count: Integer): IJclList<T>;
var
  I: Integer;
  Last: Integer;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Last := First + Count - 1;
    if Last >= FCount then
      Last := FCount - 1;
    Result := CreateEmptyVector(Count);
    for I := First to Last do
      Result.Add(Items[I]);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclVectorE<T> } =====================================================

constructor TJclVectorE<T>.Create(const AEqualityComparer: IEqualityComparer<T>; ACapacity: Integer;
  AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

function TJclVectorE<T>.CreateEmptyVector(ACapacity: Integer): TJclVector<T>;
begin
  Result := TJclVectorE<T>.Create(EqualityComparer, ACapacity, False);
end;

function TJclVectorE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityComparer.Equals(A, B);
end;

//=== { TJclVectorF<T> } =====================================================

constructor TJclVectorF<T>.Create(const AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer;
  AOwnsItems: Boolean);
begin
  inherited Create(ACapacity, AOwnsItems);
  FEqualityCompare := AEqualityCompare;
end;

function TJclVectorF<T>.CreateEmptyVector(ACapacity: Integer): TJclVector<T>;
begin
  Result := TJclVectorF<T>.Create(EqualityCompare, ACapacity, False);
end;

function TJclVectorF<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if not Assigned(EqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityCompare(A, B);
end;

//=== { TJclVectorI<T> } =====================================================

function TJclVectorI<T>.CreateEmptyVector(ACapacity: Integer): TJclVector<T>;
begin
  Result := TJclVectorI<T>.Create(ACapacity, False);
end;

function TJclVectorI<T>.ItemsEqual(const A, B: T): Boolean;
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

