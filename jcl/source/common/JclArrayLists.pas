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

  TJclIntfArrayList = class(TJclIntfContainer, IJclIntfCollection, IJclIntfList, IJclIntfArray, IJclContainer, IJclIntfEqualityComparer,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FElementData: JclBase.TDynIInterfaceArray;
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
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclIntfCollection); overload;
    destructor Destroy; override;
  end;


  TJclStrArrayList = class(TJclStrAbstractCollection, IJclStrCollection, IJclStrList, IJclStrArray, IJclContainer, IJclStrContainer, IJclStrFlatContainer, IJclStrEqualityComparer,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FElementData: JclBase.TDynStringArray;
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
    constructor Create(ACapacity: Integer); overload;
    constructor Create(const ACollection: IJclStrCollection); overload;
    destructor Destroy; override;
  end;


  TJclArrayList = class(TJclContainer, IJclCollection, IJclList, IJclArray, IJclContainer, IJclObjectOwner, IJclEqualityComparer,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FElementData: JclBase.TDynObjectArray;
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
    constructor Create(ACapacity: Integer; AOwnsObjects: Boolean); overload;
    constructor Create(const ACollection: IJclCollection; AOwnsObjects: Boolean); overload;
    destructor Destroy; override;
  end;

  {$IFDEF SUPPORTS_GENERICS}

  TJclArrayList<T> = class(TJclContainer<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FElementData: TJclBase<T>.TDynArray;
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
    constructor Create(ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;
    destructor Destroy; override;
  end;

  // E = External helper to compare items for equality
  // GetHashCode is not used
  TJclArrayListE<T> = class(TJclArrayList<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;

    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclArrayListF<T> = class(TJclArrayList<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  private
    FEqualityCompare: TEqualityCompare<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainer); override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; ACapacity: Integer; AOwnsItems: Boolean); overload;
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const ACollection: IJclCollection<T>; AOwnsItems: Boolean); overload;

    property EqualityCompare: TEqualityCompare<T> read FEqualityCompare write FEqualityCompare;
  end;

  // I = Items can compare themselves to others
  TJclArrayListI<T: IEquatable<T>> = class(TJclArrayList<T>, IJclCollection<T>, IJclList<T>, IJclArray<T>, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclPackable, IJclGrowable)
  protected
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainer; override;
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


//=== { TIntfItr } ===============================================================

type
  TIntfItr = class(TJclAbstractIterator, IJclIntfIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FOwnList: IJclIntfList;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
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
    constructor Create(const AOwnList: IJclIntfList; ACursor: Integer; AValid: Boolean);
  end;

constructor TIntfItr.Create(const AOwnList: IJclIntfList; ACursor: Integer; AValid: Boolean);
begin
  inherited Create(AOwnList, AValid);
  FOwnList := AOwnList;
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

//=== { TStrItr } ===============================================================

type
  TStrItr = class(TJclAbstractIterator, IJclStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FOwnList: IJclStrList;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
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
    constructor Create(const AOwnList: IJclStrList; ACursor: Integer; AValid: Boolean);
  end;

constructor TStrItr.Create(const AOwnList: IJclStrList; ACursor: Integer; AValid: Boolean);
begin
  inherited Create(AOwnList, AValid);
  FOwnList := AOwnList;
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

//=== { TItr } ===============================================================

type
  TItr = class(TJclAbstractIterator, IJclIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FOwnList: IJclList;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
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
    constructor Create(const AOwnList: IJclList; ACursor: Integer; AValid: Boolean);
  end;

constructor TItr.Create(const AOwnList: IJclList; ACursor: Integer; AValid: Boolean);
begin
  inherited Create(AOwnList, AValid);
  FOwnList := AOwnList;
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

//=== { TItr<T> } ===============================================================

type
  TItr<T> = class(TJclAbstractIterator, IJclIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: Integer;
    FOwnList: IJclList<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
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
    constructor Create(const AOwnList: IJclList<T>; ACursor: Integer; AValid: Boolean);
  end;

constructor TItr<T>.Create(const AOwnList: IJclList<T>; ACursor: Integer; AValid: Boolean);
begin
  inherited Create(AOwnList, AValid);
  FOwnList := AOwnList;
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


//=== { TJclIntfArrayList } ======================================================

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
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AInterface, nil);
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AInterface, FElementData[Index]) then
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
          FElementData[FSize] := AInterface;
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

function TJclIntfArrayList.AddAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  Item: IInterface;
  AddItem: Boolean;
  Index: Integer;
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
    begin
      Item := It.Next;
      // (rom) inlining Add() gives about 5 percent performance increase
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
          for Index := 0 to FSize - 1 do
            if ItemsEqual(Item, FElementData[Index]) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
        if AddItem then
        begin
          if FSize = FCapacity then
            AutoGrow;
          AddItem := FSize < FCapacity;
          if AddItem then
          begin
            FElementData[FSize] := Item;
            Inc(FSize);
          end;
        end;
      end;
      Result := Result and AddItem;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfArrayList.AssignDataTo(Dest: TJclAbstractContainer);
var
  ADest: TJclIntfArrayList;
  ACollection: IJclIntfCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclIntfArrayList then
  begin
    ADest := TJclIntfArrayList(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclIntfCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
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
      FreeObject(FElementData[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
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
      if ItemsEqual(FElementData[I], AInterface) then
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfArrayList.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclIntfArrayList.Create(FSize);
  AssignPropertiesTo(Result);
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
      if not ItemsEqual(FElementData[I], It.Next) then
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

function TJclIntfArrayList.GetObject(Index: Integer): IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) or (Index < FSize) then
      Result := FElementData[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
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
      if ItemsEqual(FElementData[I], AInterface) then
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

function TJclIntfArrayList.Insert(Index: Integer; const AInterface: IInterface): Boolean;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AInterface, FElementData[Index]) then
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
          if Index < FSize then
            JclBase.MoveArray(FElementData, Index, Index + 1, FSize - Index);
          FElementData[Index] := AInterface;
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

function TJclIntfArrayList.InsertAll(Index: Integer; const ACollection: IJclIntfCollection): Boolean;
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
      if ItemsEqual(FElementData[I], AInterface) then
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
      if ItemsEqual(FElementData[I], AInterface) then
      begin
        FreeObject(FElementData[I]);
        if I < (FSize - 1) then
          JclBase.MoveArray(FElementData, I + 1, I, FSize - I - 1);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
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
    Result := FreeObject(FElementData[Index]);
    if Index < (FSize - 1) then
      JclBase.MoveArray(FElementData, Index + 1, Index, FSize - Index - 1);
    Dec(FSize);
    AutoPack;
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
    Result := True;
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
      inherited SetCapacity(Value);
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
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AInterface, nil);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(FElementData[I], AInterface) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeObject(FElementData[Index]);
        FElementData[Index] := AInterface;
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
    Result := CreateEmptyContainer as IJclIntfList;
    for I := First to Last do
      Result.Add(FElementData[I]);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;


//=== { TJclStrArrayList } ======================================================

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
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AString, '');
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AString, FElementData[Index]) then
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
          FElementData[FSize] := AString;
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

function TJclStrArrayList.AddAll(const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
  Item: string;
  AddItem: Boolean;
  Index: Integer;
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
    begin
      Item := It.Next;
      // (rom) inlining Add() gives about 5 percent performance increase
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
          for Index := 0 to FSize - 1 do
            if ItemsEqual(Item, FElementData[Index]) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
        if AddItem then
        begin
          if FSize = FCapacity then
            AutoGrow;
          AddItem := FSize < FCapacity;
          if AddItem then
          begin
            FElementData[FSize] := Item;
            Inc(FSize);
          end;
        end;
      end;
      Result := Result and AddItem;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrArrayList.AssignDataTo(Dest: TJclAbstractContainer);
var
  ADest: TJclStrArrayList;
  ACollection: IJclStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclStrArrayList then
  begin
    ADest := TJclStrArrayList(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
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
      FreeString(FElementData[I]);
    FSize := 0;
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
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
      if ItemsEqual(FElementData[I], AString) then
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrArrayList.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclStrArrayList.Create(FSize);
  AssignPropertiesTo(Result);
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
      if not ItemsEqual(FElementData[I], It.Next) then
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

function TJclStrArrayList.GetString(Index: Integer): string;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (Index >= 0) or (Index < FSize) then
      Result := FElementData[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
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
      if ItemsEqual(FElementData[I], AString) then
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

function TJclStrArrayList.Insert(Index: Integer; const AString: string): Boolean;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AString, FElementData[Index]) then
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
          if Index < FSize then
            JclBase.MoveArray(FElementData, Index, Index + 1, FSize - Index);
          FElementData[Index] := AString;
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

function TJclStrArrayList.InsertAll(Index: Integer; const ACollection: IJclStrCollection): Boolean;
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
      if ItemsEqual(FElementData[I], AString) then
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
      if ItemsEqual(FElementData[I], AString) then
      begin
        FreeString(FElementData[I]);
        if I < (FSize - 1) then
          JclBase.MoveArray(FElementData, I + 1, I, FSize - I - 1);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
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
    Result := FreeString(FElementData[Index]);
    if Index < (FSize - 1) then
      JclBase.MoveArray(FElementData, Index + 1, Index, FSize - Index - 1);
    Dec(FSize);
    AutoPack;
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
    Result := True;
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
      inherited SetCapacity(Value);
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
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AString, '');
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(FElementData[I], AString) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeString(FElementData[Index]);
        FElementData[Index] := AString;
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
    Result := CreateEmptyContainer as IJclStrList;
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
  inherited Create(nil, AOwnsObjects);
  FSize := 0;
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
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AObject, nil);
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AObject, FElementData[Index]) then
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
          FElementData[FSize] := AObject;
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

function TJclArrayList.AddAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  Item: TObject;
  AddItem: Boolean;
  Index: Integer;
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
    begin
      Item := It.Next;
      // (rom) inlining Add() gives about 5 percent performance increase
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
          for Index := 0 to FSize - 1 do
            if ItemsEqual(Item, FElementData[Index]) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
        if AddItem then
        begin
          if FSize = FCapacity then
            AutoGrow;
          AddItem := FSize < FCapacity;
          if AddItem then
          begin
            FElementData[FSize] := Item;
            Inc(FSize);
          end;
        end;
      end;
      Result := Result and AddItem;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList.AssignDataTo(Dest: TJclAbstractContainer);
var
  ADest: TJclArrayList;
  ACollection: IJclCollection;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclArrayList then
  begin
    ADest := TJclArrayList(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
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
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
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
      if ItemsEqual(FElementData[I], AObject) then
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclArrayList.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclArrayList.Create(FSize, False);
  AssignPropertiesTo(Result);
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
      if not ItemsEqual(FElementData[I], It.Next) then
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

function TJclArrayList.GetObject(Index: Integer): TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) or (Index < FSize) then
      Result := FElementData[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
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
      if ItemsEqual(FElementData[I], AObject) then
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

function TJclArrayList.Insert(Index: Integer; AObject: TObject): Boolean;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AObject, FElementData[Index]) then
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
          if Index < FSize then
            JclBase.MoveArray(FElementData, Index, Index + 1, FSize - Index);
          FElementData[Index] := AObject;
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

function TJclArrayList.InsertAll(Index: Integer; const ACollection: IJclCollection): Boolean;
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
      if ItemsEqual(FElementData[I], AObject) then
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
      if ItemsEqual(FElementData[I], AObject) then
      begin
        FreeObject(FElementData[I]);
        if I < (FSize - 1) then
          JclBase.MoveArray(FElementData, I + 1, I, FSize - I - 1);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
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
    Result := FreeObject(FElementData[Index]);
    if Index < (FSize - 1) then
      JclBase.MoveArray(FElementData, Index + 1, Index, FSize - Index - 1);
    Dec(FSize);
    AutoPack;
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
    Result := True;
    for I := FSize - 1 downto 0 do
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
      inherited SetCapacity(Value);
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
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AObject, nil);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(FElementData[I], AObject) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeObject(FElementData[Index]);
        FElementData[Index] := AObject;
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
    Result := CreateEmptyContainer as IJclList;
    for I := First to Last do
      Result.Add(FElementData[I]);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_GENERICS}

//=== { TJclArrayList<T> } ======================================================

constructor TJclArrayList<T>.Create(ACapacity: Integer; AOwnsItems: Boolean);
begin
  inherited Create(nil, AOwnsItems);
  FSize := 0;
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
    raise EJclNoCollectionError.Create;
  Create(ACollection.Size, AOwnsItems);
  AddAll(ACollection);
end;

destructor TJclArrayList<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclArrayList<T>.Add(const AItem: T): Boolean;
var
  Index: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AItem, Default(T));
    if Result then
    begin
      if FDuplicates <> dupAccept then
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AItem, FElementData[Index]) then
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
          FElementData[FSize] := AItem;
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

function TJclArrayList<T>.AddAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
  Item: T;
  AddItem: Boolean;
  Index: Integer;
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
    begin
      Item := It.Next;
      // (rom) inlining Add() gives about 5 percent performance increase
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, Default(T));
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
          for Index := 0 to FSize - 1 do
            if ItemsEqual(Item, FElementData[Index]) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
        if AddItem then
        begin
          if FSize = FCapacity then
            AutoGrow;
          AddItem := FSize < FCapacity;
          if AddItem then
          begin
            FElementData[FSize] := Item;
            Inc(FSize);
          end;
        end;
      end;
      Result := Result and AddItem;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclArrayList<T>.AssignDataTo(Dest: TJclAbstractContainer);
var
  ADest: TJclArrayList<T>;
  ACollection: IJclCollection<T>;
begin
  inherited AssignDataTo(Dest);
  if Dest is TJclArrayList<T> then
  begin
    ADest := TJclArrayList<T>(Dest);
    ADest.Clear;
    ADest.AddAll(Self);
  end
  else
  if Supports(IInterface(Dest), IJclCollection<T>, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
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
    AutoPack;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
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
      if ItemsEqual(FElementData[I], AItem) then
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
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
    It := ACollection.First;
    while Result and It.HasNext do
      Result := Contains(It.Next);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
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
      if not ItemsEqual(FElementData[I], It.Next) then
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

function TJclArrayList<T>.GetItem(Index: Integer): T;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if (Index >= 0) or (Index < FSize) then
      Result := FElementData[Index]
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create(IntToStr(Index));
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
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
      if ItemsEqual(FElementData[I], AItem) then
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

function TJclArrayList<T>.Insert(Index: Integer; const AItem: T): Boolean;
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
        for Index := 0 to FSize - 1 do
          if ItemsEqual(AItem, FElementData[Index]) then
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
          if Index < FSize then
            TJclBase<T>.MoveArray(FElementData, Index, Index + 1, FSize - Index);
          FElementData[Index] := AItem;
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

function TJclArrayList<T>.InsertAll(Index: Integer; const ACollection: IJclCollection<T>): Boolean;
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
      if ItemsEqual(FElementData[I], AItem) then
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

function TJclArrayList<T>.Remove(const AItem: T): Boolean;
var
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    for I := FSize - 1 downto 0 do
      if ItemsEqual(FElementData[I], AItem) then
      begin
        FreeItem(FElementData[I]);
        if I < (FSize - 1) then
          TJclBase<T>.MoveArray(FElementData, I + 1, I, FSize - I - 1);
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
          Break;
      end;
    AutoPack;
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
    Result := FreeItem(FElementData[Index]);
    if Index < (FSize - 1) then
      TJclBase<T>.MoveArray(FElementData, Index + 1, Index, FSize - Index - 1);
    Dec(FSize);
    AutoPack;
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
    Result := True;
    for I := FSize - 1 downto 0 do
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
      inherited SetCapacity(Value);
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
var
  ReplaceItem: Boolean;
  I: Integer;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index >= FSize) then
      raise EJclOutOfBoundsError.Create;
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AItem, Default(T));
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
        for I := 0 to FSize - 1 do
          if ItemsEqual(FElementData[I], AItem) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
      if ReplaceItem then
      begin
        FreeItem(FElementData[Index]);
        FElementData[Index] := AItem;
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
    Result := CreateEmptyContainer as IJclList<T>;
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

procedure TJclArrayListE<T>.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclArrayListE<T> then
    TJclArrayListE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclArrayListE<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclArrayListE<T>.Create(EqualityComparer, FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclArrayListE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityComparer.Equals(A, B);
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

procedure TJclArrayListF<T>.AssignPropertiesTo(Dest: TJclAbstractContainer);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclArrayListF<T> then
    TJclArrayListF<T>(Dest).FEqualityCompare := FEqualityCompare;
end;

function TJclArrayListF<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclArrayListF<T>.Create(EqualityCompare, FSize, False);
  AssignPropertiesTo(Result);
end;

function TJclArrayListF<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if not Assigned(EqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityCompare(A, B);
end;

//=== { TJclArrayListI<T> } ==================================================

function TJclArrayListI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  Result := A.Equals(B);
end;

function TJclArrayListI<T>.CreateEmptyContainer: TJclAbstractContainer;
begin
  Result := TJclArrayListI<T>.Create(FSize, False);
  AssignPropertiesTo(Result);
end;

{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

