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

  TJclIntfVector = class(TJclIntfContainer, IJclIntfCollection, IJclIntfList, IJclIntfArray,IJclContainer, IJclIntfEqualityComparer,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FItems: JclBase.TDynIInterfaceArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainer); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIntfCollection }
    function Add(const AInterface: IInterface): Boolean;
    function AddAll(const ACollection: IJclIntfCollection): Boolean;
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
    function Insert(Index: Integer; const AInterface: IInterface): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclIntfCollection): Boolean;
    function GetObject(Index: Integer): IInterface;
    function IndexOf(const AInterface: IInterface): Integer;
    function LastIndexOf(const AInterface: IInterface): Integer;
    function Remove(Index: Integer): IInterface; overload;
    procedure SetObject(Index: Integer; const AInterface: IInterface);
    function SubList(First, Count: Integer): IJclIntfList;
    function CreateEmptyContainer: TJclAbstractContainer; override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property Items: JclBase.TDynIInterfaceArray read FItems;
  end;


  TJclStrVector = class(TJclStrAbstractCollection, IJclStrCollection, IJclStrList, IJclStrArray,IJclContainer, IJclStrContainer, IJclStrFlatContainer, IJclStrEqualityComparer,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FItems: JclBase.TDynStringArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainer); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclStrCollection }
    function Add(const AString: string): Boolean; override;
    function AddAll(const ACollection: IJclStrCollection): Boolean; override;
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
    function Insert(Index: Integer; const AString: string): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclStrCollection): Boolean;
    function GetString(Index: Integer): string;
    function IndexOf(const AString: string): Integer;
    function LastIndexOf(const AString: string): Integer;
    function Remove(Index: Integer): string; overload;
    procedure SetString(Index: Integer; const AString: string);
    function SubList(First, Count: Integer): IJclStrList;
    function CreateEmptyContainer: TJclAbstractContainer; override;
  public
    constructor Create(ACapacity: Integer);
    destructor Destroy; override;
    property Items: JclBase.TDynStringArray read FItems;
  end;


  TJclVector = class(TJclContainer, IJclCollection, IJclList, IJclArray,IJclContainer, IJclObjectOwner, IJclEqualityComparer,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FItems: JclBase.TDynObjectArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainer); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclCollection }
    function Add(AObject: TObject): Boolean;
    function AddAll(const ACollection: IJclCollection): Boolean;
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
    function Insert(Index: Integer; AObject: TObject): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclCollection): Boolean;
    function GetObject(Index: Integer): TObject;
    function IndexOf(AObject: TObject): Integer;
    function LastIndexOf(AObject: TObject): Integer;
    function Remove(Index: Integer): TObject; overload;
    procedure SetObject(Index: Integer; AObject: TObject);
    function SubList(First, Count: Integer): IJclList;
    function CreateEmptyContainer: TJclAbstractContainer; override;
  public
    constructor Create(ACapacity: Integer; AOwnsObjects: Boolean);
    destructor Destroy; override;
    property Items: JclBase.TDynObjectArray read FItems;
  end;

  {$IFDEF SUPPORTS_GENERICS}


  TJclVector<T> = class(TJclContainer<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>,IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FItems: TJclBase<T>.TDynArray;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainer); override;
    { IJclPackable }
    procedure SetCapacity(Value: Integer); override;
    { IJclIntfCloneable }
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
    function Insert(Index: Integer; const AItem: T): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclCollection<T>): Boolean;
    function GetItem(Index: Integer): T;
    function IndexOf(const AItem: T): Integer;
    function LastIndexOf(const AItem: T): Integer;
    function Remove(Index: Integer): T; overload;
    procedure SetItem(Index: Integer; const AItem: T);
    function SubList(First, Count: Integer): IJclList<T>;
  public
    constructor Create(ACapacity: Integer; AOwnsItems: Boolean);
    destructor Destroy; override;
    property Items: TJclBase<T>.TDynArray read FItems;
  end;

  // E = External helper to compare items for equality (GetHashCode is not used)
  TJclVectorE<T> = class(TJclVector<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
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
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
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
    function CreateEmptyContainer: TJclAbstractContainer; override;
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
    function CreateEmptyIterator: TJclAbstractIterator; override;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIntfIterator }
    function Add(const AInterface: IInterface): Boolean;
    function GetObject: IInterface;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AInterface: IInterface): Boolean;
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
  inherited Create(OwnList, AValid);
  FOwnList := OwnList;
  FCursor := ACursor;
end;

function TIntfItr.Add(const AInterface: IInterface): Boolean;
begin
  Result := FOwnList.Add(AInterface);
end;

procedure TIntfItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TIntfItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TIntfItr then
  begin
    ADest := TIntfItr(Dest);
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
  end;
end;

function TIntfItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TIntfItr.Create(FOwnList, FCursor, Valid);
end;

function TIntfItr.GetObject: IInterface;
begin
  CheckValid;
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

function TIntfItr.Insert(const AInterface: IInterface): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AInterface);
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
  CheckValid;
  Valid := False;
  FOwnList.Remove(FCursor);
end;

procedure TIntfItr.SetObject(const AInterface: IInterface);
begin
  CheckValid;
  FOwnList.SetObject(FCursor, AInterface);
end;


//=== { TStrItr } ===========================================================

type
  TStrItr = class(TJclAbstractIterator, IJclStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FOwnList: IJclStrList;
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclStrIterator }
    function Add(const AString: string): Boolean;
    function GetString: string;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AString: string): Boolean;
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
  inherited Create(OwnList, AValid);
  FOwnList := OwnList;
  FCursor := ACursor;
end;

function TStrItr.Add(const AString: string): Boolean;
begin
  Result := FOwnList.Add(AString);
end;

procedure TStrItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TStrItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TStrItr then
  begin
    ADest := TStrItr(Dest);
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
  end;
end;

function TStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TStrItr.Create(FOwnList, FCursor, Valid);
end;

function TStrItr.GetString: string;
begin
  CheckValid;
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

function TStrItr.Insert(const AString: string): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AString);
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
  CheckValid;
  Valid := False;
  FOwnList.Remove(FCursor);
end;

procedure TStrItr.SetString(const AString: string);
begin
  CheckValid;
  FOwnList.SetString(FCursor, AString);
end;


//=== { TItr } ===========================================================

type
  TItr = class(TJclAbstractIterator, IJclIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FOwnList: IJclList;
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIterator }
    function Add(AObject: TObject): Boolean;
    function GetObject: TObject;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(AObject: TObject): Boolean;
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
  inherited Create(OwnList, AValid);
  FOwnList := OwnList;
  FCursor := ACursor;
end;

function TItr.Add(AObject: TObject): Boolean;
begin
  Result := FOwnList.Add(AObject);
end;

procedure TItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TItr then
  begin
    ADest := TItr(Dest);
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
  end;
end;

function TItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TItr.Create(FOwnList, FCursor, Valid);
end;

function TItr.GetObject: TObject;
begin
  CheckValid;
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

function TItr.Insert(AObject: TObject): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AObject);
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
  CheckValid;
  Valid := False;
  FOwnList.Remove(FCursor);
end;

procedure TItr.SetObject(AObject: TObject);
begin
  CheckValid;
  FOwnList.SetObject(FCursor, AObject);
end;

{$IFDEF SUPPORTS_GENERICS}


//=== { TItr<T> } ===========================================================

type
  TItr<T> = class(TJclAbstractIterator, IJclIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FOwnList: IJclList<T>;
  protected
    function CreateEmptyIterator: TJclAbstractIterator; override;
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    { IJclIterator<T> }
    function Add(const AItem: T): Boolean;
    function GetItem: T;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AItem: T): Boolean;
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
  inherited Create(OwnList, AValid);
  FOwnList := OwnList;
  FCursor := ACursor;
end;

function TItr<T>.Add(const AItem: T): Boolean;
begin
  Result := FOwnList.Add(AItem);
end;

procedure TItr<T>.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TItr<T>;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TItr<T> then
  begin
    ADest := TItr<T>(Dest);
    ADest.FOwnList := FOwnList;
    ADest.FCursor := FCursor;
  end;
end;

function TItr<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TItr<T>.Create(FOwnList, FCursor, Valid);
end;

function TItr<T>.GetItem: T;
begin
  CheckValid;
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

function TItr<T>.Insert(const AItem: T): Boolean;
begin
  CheckValid;
  Result := FOwnList.Insert(FCursor, AItem);
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
  CheckValid;
  Valid := False;
  FOwnList.Remove(FCursor);
end;

procedure TItr<T>.SetItem(const AItem: T);
begin
  CheckValid;
  FOwnList.SetItem(FCursor, AItem);
end;

{$ENDIF SUPPORTS_GENERICS}


//=== { TJclIntfVector } ======================================================

constructor TJclIntfVector.Create(ACapacity: Integer);
begin
  inherited Create(nil);
  SetCapacity(ACapacity);
end;

destructor TJclIntfVector.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclIntfVector.Add(const AInterface: IInterface): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AInterface, nil);
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AInterface, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FItems[FSize] := AInterface;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfVector.AssignDataTo(Dest: TJclAbstractContainer);
var
  ADest: TJclIntfVector;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfVector then
  begin
    ADest := TJclIntfVector(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end;
end;

procedure TJclIntfVector.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeObject(FItems[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    for I := 0 to FSize - 1 do
      if ItemsEqual(Items[I], AInterface) then
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

function TJclIntfVector.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclIntfVector.Create(FSize);
  AssignPropertiesTo(Result);
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
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    for I := 0 to FSize - 1 do
      if not ItemsEqual(Items[I], It.Next) then
      begin
        Result := False;
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

function TJclIntfVector.GetObject(Index: Integer): IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) or (Index < FSize) then
      Result := Items[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
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
    for I := 0 to FSize - 1 do
      if ItemsEqual(Items[I], AInterface) then
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

function TJclIntfVector.Insert(Index: Integer; const AInterface: IInterface): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AInterface, nil);
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AInterface, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          JclBase.MoveArray(FItems, Index, Index + 1, FSize - Index);
          FItems[Index] := AInterface;
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

function TJclIntfVector.InsertAll(Index: Integer; const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
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
    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfVector.Last: IJclIntfIterator;
begin
  Result := TIntfItr.Create(Self, FSize - 1, False);
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
    for I := FSize - 1 downto 0 do
      if ItemsEqual(Items[I], AInterface) then
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

function TJclIntfVector.Remove(Index: Integer): IInterface;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    Result := FreeObject(FItems[Index]);
    JclBase.MoveArray(FItems, Index + 1, Index, FSize - Index);
    Dec(FSize);
    AutoPack;
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
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AInterface) then
      begin
        FreeObject(FItems[I]); // Force Release
        JclBase.MoveArray(FItems, I + 1, I, FSize - I);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
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
    Result := True;
    for I := FSize - 1 downto 0 do
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
    if Value < FSize then
      raise EJclOutOfBoundsError.Create;
    SetLength(FItems, Value);
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfVector.SetObject(Index: Integer; const AInterface: IInterface);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AInterface, nil);
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AInterface, FItems[I]) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeObject(FItems[Index]);
        FItems[Index] := AInterface;
      end;
    end;
    if not ReplaceItem then
      Remove(Index);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfVector.Size: Integer;
begin
  Result := FSize;
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
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclIntfList;
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
  SetCapacity(ACapacity);
end;

destructor TJclStrVector.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclStrVector.Add(const AString: string): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AString, '');
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AString, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FItems[FSize] := AString;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrVector.AssignDataTo(Dest: TJclAbstractContainer);
var
  ADest: TJclStrVector;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclStrVector then
  begin
    ADest := TJclStrVector(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end;
end;

procedure TJclStrVector.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeString(FItems[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    for I := 0 to FSize - 1 do
      if ItemsEqual(Items[I], AString) then
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

function TJclStrVector.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStrVector.Create(FSize);
  AssignPropertiesTo(Result);
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
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    for I := 0 to FSize - 1 do
      if not ItemsEqual(Items[I], It.Next) then
      begin
        Result := False;
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

function TJclStrVector.GetString(Index: Integer): string;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (Index >= 0) or (Index < FSize) then
      Result := Items[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
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
    for I := 0 to FSize - 1 do
      if ItemsEqual(Items[I], AString) then
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

function TJclStrVector.Insert(Index: Integer; const AString: string): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AString, '');
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AString, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          JclBase.MoveArray(FItems, Index, Index + 1, FSize - Index);
          FItems[Index] := AString;
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

function TJclStrVector.InsertAll(Index: Integer; const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
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
    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrVector.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclStrVector.Last: IJclStrIterator;
begin
  Result := TStrItr.Create(Self, FSize - 1, False);
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
    for I := FSize - 1 downto 0 do
      if ItemsEqual(Items[I], AString) then
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

function TJclStrVector.Remove(Index: Integer): string;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    Result := FreeString(FItems[Index]);
    JclBase.MoveArray(FItems, Index + 1, Index, FSize - Index);
    Dec(FSize);
    AutoPack;
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
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AString) then
      begin
        FreeString(FItems[I]); // Force Release
        JclBase.MoveArray(FItems, I + 1, I, FSize - I);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
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
    Result := True;
    for I := FSize - 1 downto 0 do
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
    if Value < FSize then
      raise EJclOutOfBoundsError.Create;
    SetLength(FItems, Value);
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrVector.SetString(Index: Integer; const AString: string);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AString, '');
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AString, FItems[I]) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeString(FItems[Index]);
        FItems[Index] := AString;
      end;
    end;
    if not ReplaceItem then
      Remove(Index);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrVector.Size: Integer;
begin
  Result := FSize;
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
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclStrList;
    for I := First to Last do
      Result.Add(Items[I]);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;


//=== { TJclVector } ======================================================

constructor TJclVector.Create(ACapacity: Integer; AOwnsObjects: Boolean);
begin
  inherited Create(nil, AOwnsObjects);
  SetCapacity(ACapacity);
end;

destructor TJclVector.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclVector.Add(AObject: TObject): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AObject, nil);
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AObject, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FItems[FSize] := AObject;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector.AssignDataTo(Dest: TJclAbstractContainer);
var
  ADest: TJclVector;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclVector then
  begin
    ADest := TJclVector(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end;
end;

procedure TJclVector.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeObject(FItems[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    for I := 0 to FSize - 1 do
      if ItemsEqual(Items[I], AObject) then
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

function TJclVector.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclVector.Create(FSize, False);
  AssignPropertiesTo(Result);
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
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    for I := 0 to FSize - 1 do
      if not ItemsEqual(Items[I], It.Next) then
      begin
        Result := False;
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

function TJclVector.GetObject(Index: Integer): TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) or (Index < FSize) then
      Result := Items[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
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
    for I := 0 to FSize - 1 do
      if ItemsEqual(Items[I], AObject) then
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

function TJclVector.Insert(Index: Integer; AObject: TObject): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AObject, nil);
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AObject, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          JclBase.MoveArray(FItems, Index, Index + 1, FSize - Index);
          FItems[Index] := AObject;
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

function TJclVector.InsertAll(Index: Integer; const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
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
    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclVector.Last: IJclIterator;
begin
  Result := TItr.Create(Self, FSize - 1, False);
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
    for I := FSize - 1 downto 0 do
      if ItemsEqual(Items[I], AObject) then
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

function TJclVector.Remove(Index: Integer): TObject;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    Result := FreeObject(FItems[Index]);
    JclBase.MoveArray(FItems, Index + 1, Index, FSize - Index);
    Dec(FSize);
    AutoPack;
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
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AObject) then
      begin
        FreeObject(FItems[I]); // Force Release
        JclBase.MoveArray(FItems, I + 1, I, FSize - I);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
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
    Result := True;
    for I := FSize - 1 downto 0 do
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
    if Value < FSize then
      raise EJclOutOfBoundsError.Create;
    SetLength(FItems, Value);
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector.SetObject(Index: Integer; AObject: TObject);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AObject, nil);
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AObject, FItems[I]) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeObject(FItems[Index]);
        FItems[Index] := AObject;
      end;
    end;
    if not ReplaceItem then
      Remove(Index);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector.Size: Integer;
begin
  Result := FSize;
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
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclList;
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
  inherited Create(nil, AOwnsItems);
  SetCapacity(ACapacity);
end;

destructor TJclVector<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclVector<T>.Add(const AItem: T): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AItem, Default(T));
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AItem, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          FItems[FSize] := AItem;
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
    Result := True;
    It := ACollection.First;
    while It.HasNext do
      Result := Add(It.Next) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector<T>.AssignDataTo(Dest: TJclAbstractContainer);
var
  ADest: TJclVector<T>;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclVector<T> then
  begin
    ADest := TJclVector<T>(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end;
end;

procedure TJclVector<T>.Clear;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    for I := 0 to FSize - 1 do
      FreeItem(FItems[I]);
    FSize := 0;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
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
    for I := 0 to FSize - 1 do
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
    if FSize <> ACollection.Size then
      Exit;
    Result := True;
    It := ACollection.First;
    for I := 0 to FSize - 1 do
      if not ItemsEqual(Items[I], It.Next) then
      begin
        Result := False;
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

function TJclVector<T>.GetItem(Index: Integer): T;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if (Index >= 0) or (Index < FSize) then
      Result := Items[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
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
    for I := 0 to FSize - 1 do
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

function TJclVector<T>.Insert(Index: Integer; const AItem: T): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AItem, Default(T));
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AItem, FItems[I]) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
      if Result then
      begin
        if FSize = FCapacity then
          AutoGrow;
        Result := FSize < FCapacity;
        if Result then
        begin
          TJclBase<T>.MoveArray(FItems, Index, Index + 1, FSize - Index);
          FItems[Index] := AItem;
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

function TJclVector<T>.InsertAll(Index: Integer; const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
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
    Result := True;
    It := ACollection.Last;
    while It.HasPrevious do
      Result := Insert(Index, It.Previous) and Result;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclVector<T>.Last: IJclIterator<T>;
begin
  Result := TItr<T>.Create(Self, FSize - 1, False);
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
    for I := FSize - 1 downto 0 do
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

function TJclVector<T>.Remove(Index: Integer): T;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    Result := FreeItem(FItems[Index]);
    TJclBase<T>.MoveArray(FItems, Index + 1, Index, FSize - Index);
    Dec(FSize);
    AutoPack;
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
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FItems[I], AItem) then
      begin
        FreeItem(FItems[I]); // Force Release
        TJclBase<T>.MoveArray(FItems, I + 1, I, FSize - I);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
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
    Result := True;
    for I := FSize - 1 downto 0 do
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
    if Value < FSize then
      raise EJclOutOfBoundsError.Create;
    SetLength(FItems, Value);
    inherited SetCapacity(Value);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclVector<T>.SetItem(Index: Integer; const AItem: T);
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AItem, Default(T));
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(AItem, FItems[I]) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeItem(FItems[Index]);
        FItems[Index] := AItem;
      end;
    end;
    if not ReplaceItem then
      Remove(Index);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclVector<T>.Size: Integer;
begin
  Result := FSize;
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
    if Last >= FSize then
      Last := FSize - 1;
    Result := CreateEmptyContainer as IJclList<T>;
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

procedure TJclVectorE<T>.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclVectorE<T> then
    TJclVectorE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclVectorE<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclVectorE<T>.Create(EqualityComparer, FSize, False);
  AssignPropertiesTo(Result);
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

procedure TJclVectorF<T>.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclVectorF<T> then
    TJclVectorF<T>(Dest).FEqualityCompare := FEqualityCompare;
end;

function TJclVectorF<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclVectorF<T>.Create(EqualityCompare, FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclVectorF<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if not Assigned(EqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityCompare(A, B);
end;

//=== { TJclVectorI<T> } =====================================================

function TJclVectorI<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclVectorI<T>.Create(FSize, False);
  AssignPropertiesTo(Result);
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

