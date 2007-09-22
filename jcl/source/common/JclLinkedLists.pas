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
{ The Original Code is LinkedList.pas.                                                             }
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

unit JclLinkedLists;

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
  TJclIntfLinkedListItem = class
  public
    Obj: IInterface;
    Next: TJclIntfLinkedListItem;
    Previous: TJclIntfLinkedListItem;
  end;

  TJclIntfLinkedList = class(TJclAbstractContainer, IJclIntfCollection, IJclIntfList,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  private
    FStart: TJclIntfLinkedListItem;
    FEnd: TJclIntfLinkedListItem;
    FSize: Integer;
  protected
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
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const ACollection: IJclIntfCollection = nil);
    destructor Destroy; override;
  end;

  TJclStrLinkedListItem = class
  public
    Str: string;
    Next: TJclStrLinkedListItem;
    Previous: TJclStrLinkedListItem;
  end;

  //Daniele Teti 02/03/2005
  TJclStrLinkedList = class(TJclStrCollection, IJclStrCollection, IJclStrList,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable)
  private
    FStart: TJclStrLinkedListItem;
    FEnd: TJclStrLinkedListItem;
    FSize: Integer;
  protected
    { IJclIntfCollection }
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
    { IJclIntfList }
    procedure Insert(Index: Integer; const AString: string); overload;
    function InsertAll(Index: Integer; const ACollection: IJclStrCollection): Boolean; overload;
    function GetString(Index: Integer): string;
    function IndexOf(const AString: string): Integer;
    function LastIndexOf(const AString: string): Integer;
    function Remove(Index: Integer): string; overload;
    procedure SetString(Index: Integer; const AString: string);
    function SubList(First, Count: Integer): IJclStrList;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const ACollection: IJclStrCollection = nil);
    destructor Destroy; override;
  end;

  TJclLinkedListItem = class
  public
    Obj: TObject;
    Next: TJclLinkedListItem;
    Previous: TJclLinkedListItem;
  end;
  
  TJclLinkedList = class(TJclAbstractContainer, IJclCollection, IJclList,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclObjectOwner)
  private
    FStart: TJclLinkedListItem;
    FEnd: TJclLinkedListItem;
    FSize: Integer;
    FOwnsObjects: Boolean;
  protected
    { IJclObjectOwner }
    procedure FreeObject(var AObject: TObject);
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
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const ACollection: IJclCollection = nil; AOwnsObjects: Boolean = True);
    destructor Destroy; override;
    property OwnsObjects: Boolean read FOwnsObjects;
  end;

  {$IFDEF SUPPORTS_GENERICS}

  TJclLinkedListItem<T> = class
  public
    Value: T;
    Next: TJclLinkedListItem<T>;
    Previous: TJclLinkedListItem<T>;
  end;

  TJclLinkedList<T> = class(TJclAbstractContainer, IJclCollection<T>, IJclList<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclItemOwner<T>)
  private
    FStart: TJclLinkedListItem<T>;
    FEnd: TJclLinkedListItem<T>;
    FSize: Integer;
    FOwnsItems: Boolean;
  protected
    function ItemsEqual(const A, B: T): Boolean; virtual; abstract;
    function CreateEmptyLinkedList(const ACollection: IJclCollection<T>): TJclLinkedList<T>; virtual; abstract;
    { IJclItemOwner<T> }
    procedure FreeItem(var AItem: T);
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
    function Remove(const AItem: T): Boolean; overload;
    function RemoveAll(const ACollection: IJclCollection<T>): Boolean;
    function RetainAll(const ACollection: IJclCollection<T>): Boolean;
    function Size: Integer;
    { IJclList<T> }
    procedure Insert(Index: Integer; const AItem: T); overload;
    function InsertAll(Index: Integer; const ACollection: IJclCollection<T>): Boolean; overload;
    function GetItem(Index: Integer): T;
    function IndexOf(const AItem: T): Integer;
    function LastIndexOf(const AItem: T): Integer;
    function Remove(Index: Integer): T; overload;
    procedure SetItem(Index: Integer; const AItem: T);
    function SubList(First, Count: Integer): IJclList<T>;
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const ACollection: IJclCollection<T>; AOwnsItems: Boolean = True);
    destructor Destroy; override;
    property OwnsItems: Boolean read FOwnsItems;
  end;

  // E = External helper to compare items
  // GetHashCode is never called
  TJclLinkedListE<T> = class(TJclLinkedList<T>, IJclCollection<T>, IJclList<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclItemOwner<T>)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyLinkedList(const ACollection: IJclCollection<T>): TJclLinkedList<T>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; const ACollection: IJclCollection<T>;
      AOwnsItems: Boolean = True);
    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclLinkedListF<T> = class(TJclLinkedList<T>, IJclCollection<T>, IJclList<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclItemOwner<T>)
  private
    FEqualityCompare: TEqualityCompare<T>;
  protected
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyLinkedList(const ACollection: IJclCollection<T>): TJclLinkedList<T>; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const ACollection: IJclCollection<T>;
      AOwnsItems: Boolean = True);
    property EqualityCompare: TEqualityCompare<T> read FEqualityCompare write FEqualityCompare;
  end;

  // I = Items can compare themselves to an other
  TJclLinkedListI<T: IEquatable<T>> = class(TJclLinkedList<T>, IJclCollection<T>, IJclList<T>,
    {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE} IJclIntfCloneable, IJclCloneable, IJclItemOwner<T>)
  protected
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyLinkedList(const ACollection: IJclCollection<T>): TJclLinkedList<T>; override;
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
    FCursor: TJclIntfLinkedListItem;
    FOwnList: IJclIntfList;
  protected
    { IJclIterator}
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
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const OwnList: IJclIntfList; Start: TJclIntfLinkedListItem);
  end;

function TIntfItr.Clone: TObject;
var
  NewItr: TIntfItr;
begin
  NewItr := TIntfItr.Create(FOwnList, FCursor);
  NewItr.Valid := Valid;
  Result := NewItr;
end;

constructor TIntfItr.Create(const OwnList: IJclIntfList; Start: TJclIntfLinkedListItem);
begin
  inherited Create(OwnList);
  FCursor := Start;
  FOwnList := OwnList;
end;

procedure TIntfItr.Add(const AInterface: IInterface);
begin
  FOwnList.Add(AInterface);
end;

function TIntfItr.GetObject: IInterface;
begin
  Valid := True;
  if FCursor <> nil then
    Result := FCursor.Obj
  else
    Result := nil;
end;

function TIntfItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Previous <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntfItr.Insert(const AInterface: IInterface);
var
  NewCursor: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Valid := True;
    if FCursor <> nil then
    begin
      NewCursor := TJclIntfLinkedListItem.Create;
      NewCursor.Obj := AInterface;
      NewCursor.Next := FCursor;
      NewCursor.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := NewCursor;
      FCursor.Previous := NewCursor;
      FCursor := NewCursor;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.IntfClone: IInterface;
var
  NewItr: TIntfItr;
begin
  NewItr := TIntfItr.Create(FOwnList, FCursor);
  NewItr.Valid := Valid;
  Result := NewItr;
end;

function TIntfItr.Next: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Obj
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TIntfItr.Previous: IInterface;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Obj
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.PreviousIndex: Integer;
begin
  // No Index;
  raise EJclOperationNotSupportedError.Create;
end;

procedure TIntfItr.Remove;
var
  OldCursor: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Obj := nil;
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TIntfItr.SetObject(const AInterface: IInterface);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Valid := True;
    FCursor.Obj := AInterface;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TStrItr } ============================================================

type
  TStrItr = class(TJclAbstractIterator, IJclStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclStrLinkedListItem;
    FOwnList: IJclStrList;
  protected
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
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const OwnList: IJclStrList; Start: TJclStrLinkedListItem);
  end;

constructor TStrItr.Create(const OwnList: IJclStrList; Start: TJclStrLinkedListItem);
begin
  inherited Create(OwnList);
  FCursor := Start;
  FOwnList := OwnList;
end;

procedure TStrItr.Add(const AString: string);
begin
  FOwnList.Add(AString);
end;

function TStrItr.Clone: TObject;
var
  NewItr: TStrItr;
begin
  NewItr := TStrItr.Create(FOwnList, FCursor);
  NewItr.Valid := Valid;
  Result := NewItr;
end;

function TStrItr.GetString: string;
begin
  Valid := True;
  if FCursor <> nil then
    Result := FCursor.Str
  else
    Result := '';
end;

function TStrItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TStrItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Previous <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TStrItr.Insert(const AString: string);
var
  NewCursor: TJclStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Valid := True;
    if FCursor <> nil then
    begin
      NewCursor := TJclStrLinkedListItem.Create;
      NewCursor.Str := AString;
      NewCursor.Next := FCursor;
      NewCursor.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := NewCursor;
      FCursor.Previous := NewCursor;
      FCursor := NewCursor;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TStrItr.IntfClone: IInterface;
var
  NewItr: TStrItr;
begin
  NewItr := TStrItr.Create(FOwnList, FCursor);
  NewItr.Valid := Valid;
  Result := NewItr;
end;

function TStrItr.Next: string;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Str
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TStrItr.NextIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

function TStrItr.Previous: string;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Str
    else
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TStrItr.PreviousIndex: Integer;
begin
  // No index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TStrItr.Remove;
var
  OldCursor: TJclStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Str := '';
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TStrItr.SetString(const AString: string);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Valid := True;
    FCursor.Str := AString;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TItr } ===============================================================

type
  TItr = class(TJclAbstractIterator, IJclIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclLinkedListItem;
    FOwnList: IJclList;
    FObjectOwner: IJclObjectOwner;
  public
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
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const OwnList: IJclList; const ObjectOwner: IJclObjectOwner; Start: TJclLinkedListItem);
  end;

constructor TItr.Create(const OwnList: IJclList; const ObjectOwner: IJclObjectOwner; Start: TJclLinkedListItem);
begin
  inherited Create(OwnList);
  FCursor := Start;
  FOwnList := OwnList;
  FObjectOwner := ObjectOwner;
end;

procedure TItr.Add(AObject: TObject);
begin
  FOwnList.Add(AObject);
end;

function TItr.Clone: TObject;
var
  NewItr: TItr;
begin
  NewItr := TItr.Create(FOwnList, FObjectOwner, FCursor);
  NewItr.Valid := Valid;
  Result := NewItr;
end;

function TItr.GetObject: TObject;
begin
  Valid := True;
  if FCursor <> nil then
    Result := FCursor.Obj
  else
    Result := nil;
end;

function TItr.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Previous <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr.Insert(AObject: TObject);
var
  NewCursor: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Valid := True;
    if FCursor <> nil then
    begin
      NewCursor := TJclLinkedListItem.Create;
      NewCursor.Obj := AObject;
      NewCursor.Next := FCursor;
      NewCursor.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := NewCursor;
      FCursor.Previous := NewCursor;
      FCursor := NewCursor;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.IntfClone: IInterface;
var
  NewItr: TItr;
begin
  NewItr := TItr.Create(FOwnList, FObjectOwner, FCursor);
  NewItr.Valid := Valid;
  Result := NewItr;
end;

function TItr.Next: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Obj
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TItr.Previous: TObject;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Obj
    else
      Result := nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TItr.Remove;
var
  OldCursor: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    if FCursor <> nil then
    begin
      FObjectOwner.FreeObject(FCursor.Obj);
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr.SetObject(AObject: TObject);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Valid := True;
    FObjectOwner.FreeObject(FCursor.Obj);
    FCursor.Obj := AObject;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_GENERICS}

//=== { TItr<T> } ============================================================

type
  TItr<T> = class(TJclAbstractIterator, IJclIterator<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclLinkedListItem<T>;
    FOwnList: IJclList<T>;
    FItemOwner: IJclItemOwner<T>;
  public
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
    { IJclCloneable }
    function Clone: TObject;
    { IJclIntfCloneable }
    function IntfClone: IInterface;
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const OwnList: IJclList<T>; const ItemOwner: IJclItemOwner<T>; Start: TJclLinkedListItem<T>);
  end;

constructor TItr<T>.Create(const OwnList: IJclList<T>; const ItemOwner: IJclItemOwner<T>;
  Start: TJclLinkedListItem<T>);
begin
  inherited Create(OwnList);
  FCursor := Start;
  FOwnList := OwnList;
  FItemOwner := ItemOwner;
end;

procedure TItr<T>.Add(const AItem: T);
begin
  FOwnList.Add(AItem);
end;

function TItr<T>.Clone: TObject;
var
  NewItr: TItr<T>;
begin
  NewItr := TItr<T>.Create(FOwnList, FItemOwner, FCursor);
  NewItr.Valid := Valid;
  Result := NewItr;
end;

function TItr<T>.GetItem: T;
begin
  Valid := True;
  if FCursor <> nil then
    Result := FCursor.Value
  else
    Result := Default(T);
end;

function TItr<T>.HasNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.HasPrevious: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid then
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr<T>.Insert(const AItem: T);
var
  NewCursor: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Valid := True;
    if FCursor <> nil then
    begin
      NewCursor := TJclLinkedListItem<T>.Create;
      NewCursor.Value := AItem;
      NewCursor.Next := FCursor;
      NewCursor.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := NewCursor;
      FCursor.Previous := NewCursor;
      FCursor := NewCursor;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.IntfClone: IInterface;
var
  NewItr: TItr<T>;
begin
  NewItr := TItr<T>.Create(FOwnList, FItemOwner, FCursor);
  NewItr.Valid := Valid;
  Result := NewItr;
end;

function TItr<T>.Next: T;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := Default(T);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TItr<T>.Previous: T;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Previous
    else
      Valid := True;
    if FCursor <> nil then
      Result := FCursor.Value
    else
      Result := Default(T);
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr<T>.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TItr<T>.Remove;
var
  OldCursor: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Valid := False;
    if FCursor <> nil then
    begin
      FItemOwner.FreeItem(FCursor.Value);
      if FCursor.Next <> nil then
        FCursor.Next.Previous := FCursor.Previous;
      if FCursor.Previous <> nil then
        FCursor.Previous.Next := FCursor.Next;
      OldCursor := FCursor;
      FCursor := FCursor.Next;
      OldCursor.Free;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TItr<T>.SetItem(const AItem: T);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Valid := True;
    FItemOwner.FreeItem(FCursor.Value);
    FCursor.Value := AItem;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$ENDIF SUPPORTS_GENERICS}

//=== { TJclIntfLinkedList } =================================================

constructor TJclIntfLinkedList.Create(const ACollection: IJclIntfCollection);
var
  It: IJclIntfIterator;
begin
  inherited Create(nil);
  FStart := nil;
  FEnd := nil;
  FSize := 0;
  if ACollection <> nil then
  begin
    It := ACollection.First;
    while It.HasNext do
      Add(It.Next);
  end;
end;

destructor TJclIntfLinkedList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclIntfLinkedList.Add(const AInterface: IInterface): Boolean;
var
  NewItem: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    NewItem := TJclIntfLinkedListItem.Create;
    NewItem.Obj := AInterface;
    if FStart <> nil then
    begin
      NewItem.Next := nil;
      NewItem.Previous := FEnd;
      FEnd.Next := NewItem;
      FEnd := NewItem;
    end
    else
    begin
      FStart := NewItem;
      FEnd := NewItem;
    end;
    Inc(FSize);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.AddAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := ACollection <> nil;
    if Result then
    begin
      It := ACollection.First;
      while It.HasNext do
        Result := Add(It.Next) or Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfLinkedList.Clear;
var
  Old, Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      Current.Obj := nil;
      //FreeObject(Current.Obj); //Daniele Teti 06 Maj 2005 // (outchy) wrong line
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.Clone: TObject;
begin
  Result := TJclIntfLinkedList.Create(Self);
end;

function TJclIntfLinkedList.Contains(const AInterface: IInterface): Boolean;
var
  Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    Current := FStart;
    while Current <> nil do
    begin
      if Current.Obj = AInterface then
        Exit;
      Current := Current.Next;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.ContainsAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection <> nil then
    begin
      It := ACollection.First;
      while Result and It.HasNext do
        Result := Contains(It.Next);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.Equals(const ACollection: IJclIntfCollection): Boolean;
var
  It, ItSelf: IJclIntfIterator;
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
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if ItSelf.Next <> It.Next then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.First: IJclIntfIterator;
begin
  Result := TIntfItr.Create(Self, FStart);
end;

function TJclIntfLinkedList.GetObject(Index: Integer): IInterface;
var
  Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Obj;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.IndexOf(const AInterface: IInterface): Integer;
var
  Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and (Current.Obj <> AInterface) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfLinkedList.Insert(Index: Integer; const AInterface: IInterface);
var
  Current: TJclIntfLinkedListItem;
  NewItem: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    NewItem := TJclIntfLinkedListItem.Create;
    NewItem.Obj := AInterface;
    if Index = 0 then
    begin
      NewItem.Next := FStart;
      if FStart <> nil then
        FStart.Previous := NewItem;
      FStart := NewItem;
      if FSize = 0 then
        FEnd := NewItem;
      Inc(FSize);
    end
    else
    if Index = FSize then
    begin
      NewItem.Previous := FEnd;
      FEnd.Next := NewItem;
      FEnd := NewItem;
      Inc(FSize);
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        NewItem.Next := Current;
        NewItem.Previous := Current.Previous;
        if Current.Previous <> nil then
          Current.Previous.Next := NewItem;
        Current.Previous := NewItem;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.InsertAll(Index: Integer; const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  Current: TJclIntfLinkedListItem;
  NewItem: TJclIntfLinkedListItem;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        NewItem := TJclIntfLinkedListItem.Create;
        NewItem.Obj := It.Previous;
        NewItem.Next := FStart;
        if FStart <> nil then
          FStart.Previous := NewItem;
        FStart := NewItem;
        if FSize = 0 then
          FEnd := NewItem;
        Inc(FSize);
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        NewItem := TJclIntfLinkedListItem.Create;
        NewItem.Obj := It.Next;
        NewItem.Previous := FEnd;
        if FEnd <> nil then
          FEnd.Next := NewItem;
        FEnd := NewItem;
        Inc(FSize);
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          NewItem := TJclIntfLinkedListItem.Create;
          NewItem.Obj := It.Next;
          NewItem.Next := Current;
          NewItem.Previous := Current.Previous;
          if Current.Previous <> nil then
            Current.Previous.Next := NewItem;
          Current.Previous := NewItem;
          Inc(FSize);
        end;
      end;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.IntfClone: IInterface;
begin
  Result := TJclIntfLinkedList.Create(Self);
end;

function TJclIntfLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfLinkedList.Last: IJclIntfIterator;
begin
  Result := TIntfItr.Create(Self, FEnd);
end;

function TJclIntfLinkedList.LastIndexOf(const AInterface: IInterface): Integer;
var
  Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and (Current.Obj <> AInterface) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.Remove(const AInterface: IInterface): Boolean;
var
  Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if Current.Obj = AInterface then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        Current.Obj := nil;
        Current.Free;
        Dec(FSize);
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.Remove(Index: Integer): IInterface;
var
  Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Current := FStart;
    while Current <> nil do
    begin
      if Index = 0 then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        Result := Current.Obj;
        Current.Obj := nil;
        Current.Free;
        Dec(FSize);
        Break;
      end;
      Dec(Index);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.RemoveAll(const ACollection: IJclIntfCollection): Boolean;
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

function TJclIntfLinkedList.RetainAll(const ACollection: IJclIntfCollection): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfLinkedList.SetObject(Index: Integer; const AInterface: IInterface);
var
  Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      if Index = 0 then
      begin
        Current.Obj := AInterface;
        Break;
      end;
      Dec(Index);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfLinkedList.SubList(First, Count: Integer): IJclIntfList;
var
  Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclIntfLinkedList.Create;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Obj);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclStrLinkedList } ==================================================

constructor TJclStrLinkedList.Create(const ACollection: IJclStrCollection);
var
  It: IJclStrIterator;
begin
  inherited Create(nil);
  FStart := nil;
  FEnd := nil;
  FSize := 0;
  if ACollection <> nil then
  begin
    It := ACollection.First;
    while It.HasNext do
      Add(It.Next);
  end;
end;

destructor TJclStrLinkedList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclStrLinkedList.Add(const AString: string): Boolean;
var
  NewItem: TJclStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    NewItem := TJclStrLinkedListItem.Create;
    NewItem.Str := AString;
    if FStart <> nil then
    begin
      NewItem.Next := nil;
      NewItem.Previous := FEnd;
      FEnd.Next := NewItem;
      FEnd := NewItem;
    end
    else
    begin
      FStart := NewItem;
      FEnd := NewItem;
    end;
    Inc(FSize);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrLinkedList.AddAll(const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := ACollection <> nil;
    if Result then
    begin
      It := ACollection.First;
      while It.HasNext do
        Result := Add(It.Next) or Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrLinkedList.Clear;
var
  Old, Current: TJclStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      Current.Str := '';
      //FreeObject(Current.Obj); //Daniele Teti 06 Maj 2005 // (outchy) wrong line
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrLinkedList.Clone: TObject;
begin
  Result := TJclStrLinkedList.Create(Self);
end;

function TJclStrLinkedList.Contains(const AString: string): Boolean;
var
  Current: TJclStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    Current := FStart;
    while Current <> nil do
    begin
      if Current.Str = AString then
        Exit;
      Current := Current.Next;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrLinkedList.ContainsAll(const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection <> nil then
    begin
      It := ACollection.First;
      while Result and It.HasNext do
        Result := Contains(It.Next);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrLinkedList.Equals(const ACollection: IJclStrCollection): Boolean;
var
  It, ItSelf: IJclStrIterator;
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
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if ItSelf.Next <> It.Next then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrLinkedList.First: IJclStrIterator;
begin
  Result := TStrItr.Create(Self, FStart);
end;

function TJclStrLinkedList.GetString(Index: Integer): string;
var
  Current: TJclStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Str;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrLinkedList.IndexOf(const AString: string): Integer;
var
  Current: TJclStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and (Current.Str <> AString) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrLinkedList.Insert(Index: Integer; const AString: string);
var
  Current: TJclStrLinkedListItem;
  NewItem: TJclStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    NewItem := TJclStrLinkedListItem.Create;
    NewItem.Str := AString;
    if Index = 0 then
    begin
      NewItem.Next := FStart;
      if FStart <> nil then
        FStart.Previous := NewItem;
      FStart := NewItem;
      if FSize = 0 then
        FEnd := NewItem;
      Inc(FSize);
    end
    else
    if Index = FSize then
    begin
      NewItem.Previous := FEnd;
      FEnd.Next := NewItem;
      FEnd := NewItem;
      Inc(FSize);
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        NewItem.Next := Current;
        NewItem.Previous := Current.Previous;
        if Current.Previous <> nil then
          Current.Previous.Next := NewItem;
        Current.Previous := NewItem;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrLinkedList.InsertAll(Index: Integer; const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
  Current: TJclStrLinkedListItem;
  NewItem: TJclStrLinkedListItem;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        NewItem := TJclStrLinkedListItem.Create;
        NewItem.Str := It.Previous;
        NewItem.Next := FStart;
        if FStart <> nil then
          FStart.Previous := NewItem;
        FStart := NewItem;
        if FSize = 0 then
          FEnd := NewItem;
        Inc(FSize);
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        NewItem := TJclStrLinkedListItem.Create;
        NewItem.Str := It.Next;
        NewItem.Previous := FEnd;
        if FEnd <> nil then
          FEnd.Next := NewItem;
        FEnd := NewItem;
        Inc(FSize);
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          NewItem := TJclStrLinkedListItem.Create;
          NewItem.Str := It.Next;
          NewItem.Next := Current;
          NewItem.Previous := Current.Previous;
          if Current.Previous <> nil then
            Current.Previous.Next := NewItem;
          Current.Previous := NewItem;
          Inc(FSize);
        end;
      end;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrLinkedList.IntfClone: IInterface;
begin
  Result := TJclStrLinkedList.Create(Self);
end;

function TJclStrLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclStrLinkedList.Last: IJclStrIterator;
begin
  Result := TStrItr.Create(Self, FEnd);
end;

function TJclStrLinkedList.LastIndexOf(const AString: string): Integer;
var
  Current: TJclStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and (Current.Str <> AString) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrLinkedList.Remove(Index: Integer): string;
var
  Current: TJclStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    Current := FStart;
    while Current <> nil do
    begin
      if Index = 0 then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        Result := Current.Str;
        Current.Str := '';
        Current.Free;
        Dec(FSize);
        Break;
      end;
      Dec(Index);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrLinkedList.Remove(const AString: string): Boolean;
var
  Current: TJclStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if Current.Str = AString then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        Current.Str := '';
        Current.Free;
        Dec(FSize);
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrLinkedList.RemoveAll(const ACollection: IJclStrCollection): Boolean;
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

function TJclStrLinkedList.RetainAll(const ACollection: IJclStrCollection): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclStrLinkedList.SetString(Index: Integer; const AString: string);
var
  Current: TJclStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      if Index = 0 then
      begin
        Current.Str := AString;
        Break;
      end;
      Dec(Index);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclStrLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclStrLinkedList.SubList(First, Count: Integer): IJclStrList;
var
  Current: TJclStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclStrLinkedList.Create;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Str);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclLinkedList } =====================================================

constructor TJclLinkedList.Create(const ACollection: IJclCollection; AOwnsObjects: Boolean);
var
  It: IJclIterator;
begin
  inherited Create(nil);
  FStart := nil;
  FEnd := nil;
  FSize := 0;
  FOwnsObjects := AOwnsObjects;
  if ACollection <> nil then
  begin
    It := ACollection.First;
    while It.HasNext do
      Add(It.Next);
  end;
end;

destructor TJclLinkedList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclLinkedList.Add(AObject: TObject): Boolean;
var
  NewItem: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    NewItem := TJclLinkedListItem.Create;
    NewItem.Obj := AObject;
    if FStart <> nil then
    begin
      NewItem.Next := nil;
      NewItem.Previous := FEnd;
      FEnd.Next := NewItem;
      FEnd := NewItem;
    end
    else
    begin
      FStart := NewItem;
      FEnd := NewItem;
    end;
    Inc(FSize);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.AddAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := ACollection <> nil;
    if Result then
    begin
      It := ACollection.First;
      while It.HasNext do
        Result := Add(It.Next) or Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclLinkedList.Clear;
var
  Old, Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeObject(Current.Obj);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.Clone: TObject;
begin
  Result := TJclLinkedList.Create(Self);
end;

function TJclLinkedList.Contains(AObject: TObject): Boolean;
var
  Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    Current := FStart;
    while Current <> nil do
    begin
      if Current.Obj = AObject then
        Exit;
      Current := Current.Next;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.ContainsAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection <> nil then
    begin
      It := ACollection.First;
      while Result and It.HasNext do
        Result := Contains(It.Next);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.Equals(const ACollection: IJclCollection): Boolean;
var
  It, ItSelf: IJclIterator;
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
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if ItSelf.Next <> It.Next then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.First: IJclIterator;
begin
  Result := TItr.Create(Self, Self, FStart);
end;

procedure TJclLinkedList.FreeObject(var AObject: TObject);
begin
  if FOwnsObjects then
    FreeAndNil(AObject)
  else
    AObject := nil;
end;

function TJclLinkedList.GetObject(Index: Integer): TObject;
var
  Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Obj;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.IndexOf(AObject: TObject): Integer;
var
  Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and (Current.Obj <> AObject) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclLinkedList.Insert(Index: Integer; AObject: TObject);
var
  Current: TJclLinkedListItem;
  NewItem: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    NewItem := TJclLinkedListItem.Create;
    NewItem.Obj := AObject;
    if Index = 0 then
    begin
      NewItem.Next := FStart;
      if FStart <> nil then
        FStart.Previous := NewItem;
      FStart := NewItem;
      if FSize = 0 then
        FEnd := NewItem;
      Inc(FSize);
    end
    else
    if Index = FSize then
    begin
      NewItem.Previous := FEnd;
      FEnd.Next := NewItem;
      FEnd := NewItem;
      Inc(FSize);
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        NewItem.Next := Current;
        NewItem.Previous := Current.Previous;
        if Current.Previous <> nil then
          Current.Previous.Next := NewItem;
        Current.Previous := NewItem;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.InsertAll(Index: Integer; const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  Current: TJclLinkedListItem;
  NewItem: TJclLinkedListItem;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        NewItem := TJclLinkedListItem.Create;
        NewItem.Obj := It.Previous;
        NewItem.Next := FStart;
        if FStart <> nil then
          FStart.Previous := NewItem;
        FStart := NewItem;
        if FSize = 0 then
          FEnd := NewItem;
        Inc(FSize);
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        NewItem := TJclLinkedListItem.Create;
        NewItem.Obj := It.Next;
        NewItem.Previous := FEnd;
        if FEnd <> nil then
          FEnd.Next := NewItem;
        FEnd := NewItem;
        Inc(FSize);
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          NewItem := TJclLinkedListItem.Create;
          NewItem.Obj := It.Next;
          NewItem.Next := Current;
          NewItem.Previous := Current.Previous;
          if Current.Previous <> nil then
            Current.Previous.Next := NewItem;
          Current.Previous := NewItem;
          Inc(FSize);
        end;
      end;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.IntfClone: IInterface;
begin
  Result := TJclLinkedList.Create(Self);
end;

function TJclLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclLinkedList.Last: IJclIterator;
begin
  Result := TItr.Create(Self, Self, FEnd);
end;

function TJclLinkedList.LastIndexOf(AObject: TObject): Integer;
var
  Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and (Current.Obj <> AObject) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.Remove(Index: Integer): TObject;
var
  Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    Current := FStart;
    while Current <> nil do
    begin
      if Index = 0 then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        if OwnsObjects then
          Result := nil
        else
          Result := Current.Obj;
        FreeObject(Current.Obj);
        Current.Free;
        Dec(FSize);
        Break;
      end;
      Dec(Index);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.Remove(AObject: TObject): Boolean;
var
  Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if Current.Obj = AObject then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        FreeObject(Current.Obj);
        Current.Free;
        Dec(FSize);
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.RemoveAll(const ACollection: IJclCollection): Boolean;
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

function TJclLinkedList.RetainAll(const ACollection: IJclCollection): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclLinkedList.SetObject(Index: Integer; AObject: TObject);
var
  Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      if Index = 0 then
      begin
        FreeObject(Current.Obj);
        Current.Obj := AObject;
        Break;
      end;
      Dec(Index);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclLinkedList.SubList(First, Count: Integer): IJclList;
var
  Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := TJclLinkedList.Create;
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Obj);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_GENERICS}

//=== { TJclLinkedList<T> } ==================================================

constructor TJclLinkedList<T>.Create(const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
var
  It: IJclIterator<T>;
begin
  inherited Create(nil);
  FStart := nil;
  FEnd := nil;
  FSize := 0;
  FOwnsItems := AOwnsItems;
  if ACollection <> nil then
  begin
    It := ACollection.First;
    while It.HasNext do
      Add(It.Next);
  end;
end;

destructor TJclLinkedList<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclLinkedList<T>.Add(const AItem: T): Boolean;
var
  NewItem: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    NewItem := TJclLinkedListItem<T>.Create;
    NewItem.Value := AItem;
    if FStart <> nil then
    begin
      NewItem.Next := nil;
      NewItem.Previous := FEnd;
      FEnd.Next := NewItem;
      FEnd := NewItem;
    end
    else
    begin
      FStart := NewItem;
      FEnd := NewItem;
    end;
    Inc(FSize);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.AddAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := ACollection <> nil;
    if Result then
    begin
      It := ACollection.First;
      while It.HasNext do
        Result := Add(It.Next) or Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclLinkedList<T>.Clear;
var
  Old, Current: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeItem(Current.Value);
      Old := Current;
      Current := Current.Next;
      Old.Free;
    end;
    FSize := 0;

    //Daniele Teti 27/12/2004
    FStart := nil;
    FEnd := nil;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.Clone: TObject;
begin
  Result := CreateEmptyLinkedList(Self);
end;

function TJclLinkedList<T>.Contains(const AItem: T): Boolean;
var
  Current: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AItem) then
        Exit;
      Current := Current.Next;
    end;
    Result := False;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.ContainsAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := True;
    if ACollection <> nil then
    begin
      It := ACollection.First;
      while Result and It.HasNext do
        Result := Contains(It.Next);
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.Equals(const ACollection: IJclCollection<T>): Boolean;
var
  It, ItSelf: IJclIterator<T>;
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
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
        Exit;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.First: IJclIterator<T>;
begin
  Result := TItr<T>.Create(Self, Self, FStart);
end;

procedure TJclLinkedList<T>.FreeItem(var AItem: T);
begin
  if FOwnsItems then
    FreeAndNil(AItem)
  else
    AItem := Default(T);
end;

function TJclLinkedList<T>.GetItem(Index: Integer): T;
var
  Current: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    Current := FStart;
    while (Current <> nil) and (Index > 0) do
    begin
      Current := Current.Next;
      Dec(Index);
    end;
    if Current <> nil then
      Result := Current.Value;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.IndexOf(const AItem: T): Integer;
var
  Current: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AItem) do
    begin
      Inc(Result);
      Current := Current.Next;
    end;
    if Current = nil then
      Result := -1;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclLinkedList<T>.Insert(Index: Integer; const AItem: T);
var
  Current: TJclLinkedListItem<T>;
  NewItem: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    if (Index < 0) or (Index > FSize) then
      raise EJclOutOfBoundsError.Create;
    NewItem := TJclLinkedListItem<T>.Create;
    NewItem.Value := AItem;
    if Index = 0 then
    begin
      NewItem.Next := FStart;
      if FStart <> nil then
        FStart.Previous := NewItem;
      FStart := NewItem;
      if FSize = 0 then
        FEnd := NewItem;
      Inc(FSize);
    end
    else
    if Index = FSize then
    begin
      NewItem.Previous := FEnd;
      FEnd.Next := NewItem;
      FEnd := NewItem;
      Inc(FSize);
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        NewItem.Next := Current;
        NewItem.Previous := Current.Previous;
        if Current.Previous <> nil then
          Current.Previous.Next := NewItem;
        Current.Previous := NewItem;
        Inc(FSize);
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.InsertAll(Index: Integer; const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
  Current: TJclLinkedListItem<T>;
  NewItem: TJclLinkedListItem<T>;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        NewItem := TJclLinkedListItem<T>.Create;
        NewItem.Value := It.Previous;
        NewItem.Next := FStart;
        if FStart <> nil then
          FStart.Previous := NewItem;
        FStart := NewItem;
        if FSize = 0 then
          FEnd := NewItem;
        Inc(FSize);
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        NewItem := TJclLinkedListItem<T>.Create;
        NewItem.Value := It.Next;
        NewItem.Previous := FEnd;
        if FEnd <> nil then
          FEnd.Next := NewItem;
        FEnd := NewItem;
        Inc(FSize);
      end;
    end
    else
    begin
      Current := FStart;
      while (Current <> nil) and (Index > 0) do
      begin
        Current := Current.Next;
        Dec(Index);
      end;
      if Current <> nil then
      begin
        It := ACollection.First;
        while It.HasNext do
        begin
          NewItem := TJclLinkedListItem<T>.Create;
          NewItem.Value := It.Next;
          NewItem.Next := Current;
          NewItem.Previous := Current.Previous;
          if Current.Previous <> nil then
            Current.Previous.Next := NewItem;
          Current.Previous := NewItem;
          Inc(FSize);
        end;
      end;
    end;
    Result := True;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.IntfClone: IInterface;
begin
  Result := CreateEmptyLinkedList(Self);
end;

function TJclLinkedList<T>.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclLinkedList<T>.Last: IJclIterator<T>;
begin
  Result := TItr<T>.Create(Self, Self, FEnd);
end;

function TJclLinkedList<T>.LastIndexOf(const AItem: T): Integer;
var
  Current: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := -1;
    if FEnd <> nil then
    begin
      Current := FEnd;
      Result := FSize - 1;
      while (Current <> nil) and not ItemsEqual(Current.Value, AItem) do
      begin
        Dec(Result);
        Current := Current.Previous;
      end;
      if Current = nil then
        Result := -1;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.Remove(Index: Integer): T;
var
  Current: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    Current := FStart;
    while Current <> nil do
    begin
      if Index = 0 then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        if OwnsItems then
          Result := Default(T)
        else
          Result := Current.Value;
        FreeItem(Current.Value);
        Current.Free;
        Dec(FSize);
        Break;
      end;
      Dec(Index);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.Remove(const AItem: T): Boolean;
var
  Current: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AItem) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        FreeItem(Current.Value);
        Current.Free;
        Dec(FSize);
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.RemoveAll(const ACollection: IJclCollection<T>): Boolean;
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

function TJclLinkedList<T>.RetainAll(const ACollection: IJclCollection<T>): Boolean;
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
    It := First;
    while It.HasNext do
      if not ACollection.Contains(It.Next) then
        It.Remove;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclLinkedList<T>.SetItem(Index: Integer; const AItem: T);
var
  Current: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      if Index = 0 then
      begin
        FreeItem(Current.Value);
        Current.Value := AItem;
        Break;
      end;
      Dec(Index);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.Size: Integer;
begin
  Result := FSize;
end;

function TJclLinkedList<T>.SubList(First, Count: Integer): IJclList<T>;
var
  Current: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyLinkedList(nil);
    Current := FStart;
    while (Current <> nil) and (First > 0) do
    begin
      Dec(First);
      Current := Current.Next;
    end;
    while (Current <> nil) and (Count > 0) do
    begin
      Result.Add(Current.Value);
      Dec(Count);
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

//=== { TJclLinkedListE<T> } =================================================

constructor TJclLinkedListE<T>.Create(const AEqualityComparer: IEqualityComparer<T>;
  const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FEqualityComparer := AEqualityComparer;
end;

function TJclLinkedListE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityComparer.Equals(A, B);
end;

function TJclLinkedListE<T>.CreateEmptyLinkedList(const ACollection: IJclCollection<T>): TJclLinkedList<T>;
begin
  Result := TJclLinkedListE<T>.Create(EqualityComparer, ACollection);
end;

//=== { TJclLinkedListF<T> } =================================================

constructor TJclLinkedListF<T>.Create(const AEqualityCompare: TEqualityCompare<T>;
  const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FEqualityCompare := AEqualityCompare;
end;

function TJclLinkedListF<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if not Assigned(EqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityCompare(A, B);
end;

function TJclLinkedListF<T>.CreateEmptyLinkedList(const ACollection: IJclCollection<T>): TJclLinkedList<T>;
begin
  Result := TJclLinkedListF<T>.Create(EqualityCompare, ACollection);
end;

//=== { TJclLinkedListI<T> } =================================================

function TJclLinkedListI<T>.ItemsEqual(const A, B: T): Boolean;
begin
  Result := A.Equals(B);
end;

function TJclLinkedListI<T>.CreateEmptyLinkedList(const ACollection: IJclCollection<T>): TJclLinkedList<T>;
begin
  Result := TJclLinkedListI<T>.Create(ACollection, False);
end;

{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

