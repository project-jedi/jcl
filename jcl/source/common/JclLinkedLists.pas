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
    Value: IInterface;
    Next: TJclIntfLinkedListItem;
    Previous: TJclIntfLinkedListItem;
  end;

  TJclIntfLinkedList = class(TJclIntfAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclIntfEqualityComparer,
    IJclIntfCollection, IJclIntfList)
  private
    FStart: TJclIntfLinkedListItem;
    FEnd: TJclIntfLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
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
    function Remove(const AInterface: IInterface): Boolean;
    function RemoveAll(const ACollection: IJclIntfCollection): Boolean;
    function RetainAll(const ACollection: IJclIntfCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIntfIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclIntfList }
    function Insert(Index: Integer; const AInterface: IInterface): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclIntfCollection): Boolean;
    function GetObject(Index: Integer): IInterface;
    function IndexOf(const AInterface: IInterface): Integer;
    function LastIndexOf(const AInterface: IInterface): Integer;
    function Delete(Index: Integer): IInterface; overload;
    procedure SetObject(Index: Integer; const AInterface: IInterface);
    function SubList(First, Count: Integer): IJclIntfList;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const ACollection: IJclIntfCollection);
    destructor Destroy; override;
  end;


  TJclAnsiStrLinkedListItem = class
  public
    Value: AnsiString;
    Next: TJclAnsiStrLinkedListItem;
    Previous: TJclAnsiStrLinkedListItem;
  end;

  TJclAnsiStrLinkedList = class(TJclAnsiStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclStrContainer, IJclAnsiStrContainer, IJclAnsiStrFlatContainer, IJclAnsiStrEqualityComparer,
    IJclAnsiStrCollection, IJclAnsiStrList)
  private
    FStart: TJclAnsiStrLinkedListItem;
    FEnd: TJclAnsiStrLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclAnsiStrCollection }
    function Add(const AString: AnsiString): Boolean; override;
    function AddAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    procedure Clear; override;
    function Contains(const AString: AnsiString): Boolean; override;
    function ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Equals(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function First: IJclAnsiStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclAnsiStrIterator; override;
    function Remove(const AString: AnsiString): Boolean; override;
    function RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclAnsiStrCollection): Boolean; override;
    function Size: Integer; override;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclAnsiStrIterator; override;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclAnsiStrList }
    function Insert(Index: Integer; const AString: AnsiString): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclAnsiStrCollection): Boolean;
    function GetString(Index: Integer): AnsiString;
    function IndexOf(const AString: AnsiString): Integer;
    function LastIndexOf(const AString: AnsiString): Integer;
    function Delete(Index: Integer): AnsiString; overload;
    procedure SetString(Index: Integer; const AString: AnsiString);
    function SubList(First, Count: Integer): IJclAnsiStrList;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const ACollection: IJclAnsiStrCollection);
    destructor Destroy; override;
  end;


  TJclWideStrLinkedListItem = class
  public
    Value: WideString;
    Next: TJclWideStrLinkedListItem;
    Previous: TJclWideStrLinkedListItem;
  end;

  TJclWideStrLinkedList = class(TJclWideStrAbstractCollection, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclStrContainer, IJclWideStrContainer, IJclWideStrFlatContainer, IJclWideStrEqualityComparer,
    IJclWideStrCollection, IJclWideStrList)
  private
    FStart: TJclWideStrLinkedListItem;
    FEnd: TJclWideStrLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
    { IJclWideStrCollection }
    function Add(const AString: WideString): Boolean; override;
    function AddAll(const ACollection: IJclWideStrCollection): Boolean; override;
    procedure Clear; override;
    function Contains(const AString: WideString): Boolean; override;
    function ContainsAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function Equals(const ACollection: IJclWideStrCollection): Boolean; override;
    function First: IJclWideStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclWideStrIterator; override;
    function Remove(const AString: WideString): Boolean; override;
    function RemoveAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function RetainAll(const ACollection: IJclWideStrCollection): Boolean; override;
    function Size: Integer; override;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclWideStrIterator; override;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclWideStrList }
    function Insert(Index: Integer; const AString: WideString): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclWideStrCollection): Boolean;
    function GetString(Index: Integer): WideString;
    function IndexOf(const AString: WideString): Integer;
    function LastIndexOf(const AString: WideString): Integer;
    function Delete(Index: Integer): WideString; overload;
    procedure SetString(Index: Integer; const AString: WideString);
    function SubList(First, Count: Integer): IJclWideStrList;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const ACollection: IJclWideStrCollection);
    destructor Destroy; override;
  end;

  {$IFDEF CONTAINER_ANSISTR}
  TJclStrLinkedList = TJclAnsiStrLinkedList;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TJclStrLinkedList = TJclWideStrLinkedList;
  {$ENDIF CONTAINER_WIDESTR}


  TJclLinkedListItem = class
  public
    Value: TObject;
    Next: TJclLinkedListItem;
    Previous: TJclLinkedListItem;
  end;

  TJclLinkedList = class(TJclAbstractContainer, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclObjectOwner, IJclEqualityComparer,
    IJclCollection, IJclList)
  private
    FStart: TJclLinkedListItem;
    FEnd: TJclLinkedListItem;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
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
    function Remove(AObject: TObject): Boolean;
    function RemoveAll(const ACollection: IJclCollection): Boolean;
    function RetainAll(const ACollection: IJclCollection): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIterator;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclList }
    function Insert(Index: Integer; AObject: TObject): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclCollection): Boolean;
    function GetObject(Index: Integer): TObject;
    function IndexOf(AObject: TObject): Integer;
    function LastIndexOf(AObject: TObject): Integer;
    function Delete(Index: Integer): TObject; overload;
    procedure SetObject(Index: Integer; AObject: TObject);
    function SubList(First, Count: Integer): IJclList;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
  public
    constructor Create(const ACollection: IJclCollection; AOwnsObjects: Boolean);
    destructor Destroy; override;
  end;

  {$IFDEF SUPPORTS_GENERICS}

  TJclLinkedListItem<T> = class
  public
    Value: T;
    Next: TJclLinkedListItem<T>;
    Previous: TJclLinkedListItem<T>;
  end;

  TJclLinkedList<T> = class(TJclAbstractContainer<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclItemOwner<T>, IJclEqualityComparer<T>,
    IJclCollection<T>, IJclList<T>)
  private
    FStart: TJclLinkedListItem<T>;
    FEnd: TJclLinkedListItem<T>;
  protected
    procedure AssignDataTo(Dest: TJclAbstractContainerBase); override;
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
    function Remove(const AItem: T): Boolean;
    function RemoveAll(const ACollection: IJclCollection<T>): Boolean;
    function RetainAll(const ACollection: IJclCollection<T>): Boolean;
    function Size: Integer;
    {$IFDEF SUPPORTS_FOR_IN}
    function GetEnumerator: IJclIterator<T>;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclList<T> }
    function Insert(Index: Integer; const AItem: T): Boolean;
    function InsertAll(Index: Integer; const ACollection: IJclCollection<T>): Boolean;
    function GetItem(Index: Integer): T;
    function IndexOf(const AItem: T): Integer;
    function LastIndexOf(const AItem: T): Integer;
    function Delete(Index: Integer): T; overload;
    procedure SetItem(Index: Integer; const AItem: T);
    function SubList(First, Count: Integer): IJclList<T>;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
    destructor Destroy; override;
  end;

  // E = External helper to compare items
  // GetHashCode is never called
  TJclLinkedListE<T> = class(TJclLinkedList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclCollection<T>, IJclList<T>, IJclEqualityComparer<T>,
    IJclItemOwner<T>)
  private
    FEqualityComparer: IEqualityComparer<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityComparer: IEqualityComparer<T>; const ACollection: IJclCollection<T>;
      AOwnsItems: Boolean);
    property EqualityComparer: IEqualityComparer<T> read FEqualityComparer write FEqualityComparer;
  end;

  // F = Function to compare items for equality
  TJclLinkedListF<T> = class(TJclLinkedList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclCollection<T>, IJclList<T>, IJclEqualityComparer<T>,
    IJclItemOwner<T>)
  private
    FEqualityCompare: TEqualityCompare<T>;
  protected
    procedure AssignPropertiesTo(Dest: TJclAbstractContainerBase); override;
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AEqualityCompare: TEqualityCompare<T>; const ACollection: IJclCollection<T>;
      AOwnsItems: Boolean);
    property EqualityCompare: TEqualityCompare<T> read FEqualityCompare write FEqualityCompare;
  end;

  // I = Items can compare themselves to an other
  TJclLinkedListI<T: IEquatable<T>> = class(TJclLinkedList<T>, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable, IJclContainer, IJclCollection<T>, IJclList<T>, IJclEqualityComparer<T>,
    IJclItemOwner<T>)
  protected
    function ItemsEqual(const A, B: T): Boolean; override;
    function CreateEmptyContainer: TJclAbstractContainerBase; override;
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


//=== { TIntfItr } ============================================================

type
  TIntfItr = class(TJclAbstractIterator, IJclIntfIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclIntfLinkedListItem;
    FOwnList: IJclIntfList;
    FEqualityComparer: IJclIntfEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
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
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: IInterface read GetObject;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AOwnList: IJclIntfList; Start: TJclIntfLinkedListItem; AValid: Boolean);
  end;

constructor TIntfItr.Create(const AOwnList: IJclIntfList; Start: TJclIntfLinkedListItem; AValid: Boolean);
begin
  inherited Create(AOwnList, AValid);
  FCursor := Start;
  FOwnList := AOwnList;
  FEqualityComparer := AOwnList as IJclIntfEqualityComparer;
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
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TIntfItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TIntfItr.Create(FOwnList, FCursor, Valid);
end;

function TIntfItr.GetObject: IInterface;
begin
  CheckValid;
  Result := nil;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
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
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TIntfItr.Insert(const AInterface: IInterface): Boolean;
var
  NewCursor: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AInterface, nil);
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AInterface);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AInterface);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclIntfLinkedListItem.Create;
          NewCursor.Value := AInterface;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TIntfItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

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
      Result := FCursor.Value
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
  // No Index
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
      Result := FCursor.Value
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
  // No Index
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
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := nil;
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
    CheckValid;
    FCursor.Value := nil;
    FCursor.Value := AInterface;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;


//=== { TAnsiStrItr } ============================================================

type
  TAnsiStrItr = class(TJclAbstractIterator, IJclAnsiStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclAnsiStrLinkedListItem;
    FOwnList: IJclAnsiStrList;
    FEqualityComparer: IJclAnsiStrEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclAnsiStrIterator }
    function Add(const AString: AnsiString): Boolean;
    function GetString: AnsiString;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AString: AnsiString): Boolean;
    function Next: AnsiString;
    function NextIndex: Integer;
    function Previous: AnsiString;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetString(const AString: AnsiString);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: AnsiString read GetString;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AOwnList: IJclAnsiStrList; Start: TJclAnsiStrLinkedListItem; AValid: Boolean);
  end;

constructor TAnsiStrItr.Create(const AOwnList: IJclAnsiStrList; Start: TJclAnsiStrLinkedListItem; AValid: Boolean);
begin
  inherited Create(AOwnList, AValid);
  FCursor := Start;
  FOwnList := AOwnList;
  FEqualityComparer := AOwnList as IJclAnsiStrEqualityComparer;
end;

function TAnsiStrItr.Add(const AString: AnsiString): Boolean;
begin
  Result := FOwnList.Add(AString);
end;

procedure TAnsiStrItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TAnsiStrItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TAnsiStrItr then
  begin
    ADest := TAnsiStrItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TAnsiStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TAnsiStrItr.Create(FOwnList, FCursor, Valid);
end;

function TAnsiStrItr.GetString: AnsiString;
begin
  CheckValid;
  Result := '';
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TAnsiStrItr.HasNext: Boolean;
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

function TAnsiStrItr.HasPrevious: Boolean;
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

function TAnsiStrItr.Insert(const AString: AnsiString): Boolean;
var
  NewCursor: TJclAnsiStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, '');
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AString);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AString);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclAnsiStrLinkedListItem.Create;
          NewCursor.Value := AString;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TAnsiStrItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TAnsiStrItr.Next: AnsiString;
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
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TAnsiStrItr.Previous: AnsiString;
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
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TAnsiStrItr.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TAnsiStrItr.Remove;
var
  OldCursor: TJclAnsiStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := '';
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

procedure TAnsiStrItr.SetString(const AString: AnsiString);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := '';
    FCursor.Value := AString;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;


//=== { TWideStrItr } ============================================================

type
  TWideStrItr = class(TJclAbstractIterator, IJclWideStrIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclWideStrLinkedListItem;
    FOwnList: IJclWideStrList;
    FEqualityComparer: IJclWideStrEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
    { IJclWideStrIterator }
    function Add(const AString: WideString): Boolean;
    function GetString: WideString;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Insert(const AString: WideString): Boolean;
    function Next: WideString;
    function NextIndex: Integer;
    function Previous: WideString;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetString(const AString: WideString);
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: WideString read GetString;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AOwnList: IJclWideStrList; Start: TJclWideStrLinkedListItem; AValid: Boolean);
  end;

constructor TWideStrItr.Create(const AOwnList: IJclWideStrList; Start: TJclWideStrLinkedListItem; AValid: Boolean);
begin
  inherited Create(AOwnList, AValid);
  FCursor := Start;
  FOwnList := AOwnList;
  FEqualityComparer := AOwnList as IJclWideStrEqualityComparer;
end;

function TWideStrItr.Add(const AString: WideString): Boolean;
begin
  Result := FOwnList.Add(AString);
end;

procedure TWideStrItr.AssignPropertiesTo(Dest: TJclAbstractIterator);
var
  ADest: TWideStrItr;
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TWideStrItr then
  begin
    ADest := TWideStrItr(Dest);
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TWideStrItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TWideStrItr.Create(FOwnList, FCursor, Valid);
end;

function TWideStrItr.GetString: WideString;
begin
  CheckValid;
  Result := '';
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
end;

function TWideStrItr.HasNext: Boolean;
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

function TWideStrItr.HasPrevious: Boolean;
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

function TWideStrItr.Insert(const AString: WideString): Boolean;
var
  NewCursor: TJclWideStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AString, '');
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AString);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AString);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclWideStrLinkedListItem.Create;
          NewCursor.Value := AString;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TWideStrItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

function TWideStrItr.Next: WideString;
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
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.NextIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

function TWideStrItr.Previous: WideString;
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
      Result := '';
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TWideStrItr.PreviousIndex: Integer;
begin
  // No Index
  raise EJclOperationNotSupportedError.Create;
end;

procedure TWideStrItr.Remove;
var
  OldCursor: TJclWideStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      FCursor.Value := '';
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

procedure TWideStrItr.SetString(const AString: WideString);
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    FCursor.Value := '';
    FCursor.Value := AString;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;


//=== { TItr } ============================================================

type
  TItr = class(TJclAbstractIterator, IJclIterator, {$IFDEF THREADSAFE} IJclLockable, {$ENDIF THREADSAFE}
    IJclIntfCloneable, IJclCloneable)
  private
    FCursor: TJclLinkedListItem;
    FOwnList: IJclList;
    FEqualityComparer: IJclEqualityComparer;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
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
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: TObject read GetObject;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AOwnList: IJclList; Start: TJclLinkedListItem; AValid: Boolean);
  end;

constructor TItr.Create(const AOwnList: IJclList; Start: TJclLinkedListItem; AValid: Boolean);
begin
  inherited Create(AOwnList, AValid);
  FCursor := Start;
  FOwnList := AOwnList;
  FEqualityComparer := AOwnList as IJclEqualityComparer;
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
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TItr.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TItr.Create(FOwnList, FCursor, Valid);
end;

function TItr.GetObject: TObject;
begin
  CheckValid;
  Result := nil;
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
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
      Result := (FCursor <> nil) and (FCursor.Next <> nil)
    else
      Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TItr.Insert(AObject: TObject): Boolean;
var
  NewCursor: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AObject, nil);
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AObject);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AObject);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
        begin
          NewCursor := TJclLinkedListItem.Create;
          NewCursor.Value := AObject;
          NewCursor.Next := FCursor;
          NewCursor.Previous := FCursor.Previous;
          if FCursor.Previous <> nil then
            FCursor.Previous.Next := NewCursor;
          FCursor.Previous := NewCursor;
          FCursor := NewCursor;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TItr.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

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
      Result := FCursor.Value
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
      Result := FCursor.Value
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
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      (FownList as IJclObjectOwner).FreeObject(FCursor.Value);
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
    CheckValid;
    (FownList as IJclObjectOwner).FreeObject(FCursor.Value);
    FCursor.Value := AObject;
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
    FEqualityComparer: IJclEqualityComparer<T>;
  public
    procedure AssignPropertiesTo(Dest: TJclAbstractIterator); override;
    function CreateEmptyIterator: TJclAbstractIterator; override;
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
    {$IFDEF SUPPORTS_FOR_IN}
    function MoveNext: Boolean;
    property Current: T read GetItem;
    {$ENDIF SUPPORTS_FOR_IN}
    { IJclIntfCloneable }
    function IJclIntfCloneable.Clone = IntfClone;
  public
    constructor Create(const AOwnList: IJclList<T>; Start: TJclLinkedListItem<T>; AValid: Boolean);
  end;

constructor TItr<T>.Create(const AOwnList: IJclList<T>; Start: TJclLinkedListItem<T>; AValid: Boolean);
begin
  inherited Create(AOwnList, AValid);
  FCursor := Start;
  FOwnList := AOwnList;
  FEqualityComparer := AOwnList as IJclEqualityComparer<T>;
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
    ADest.FCursor := FCursor;
    ADest.FOwnList := FOwnList;
    ADest.FEqualityComparer := FEqualityComparer;
  end;
end;

function TItr<T>.CreateEmptyIterator: TJclAbstractIterator;
begin
  Result := TItr<T>.Create(FOwnList, FCursor, Valid);
end;

function TItr<T>.GetItem: T;
begin
  CheckValid;
  Result := Default(T);
  if FCursor <> nil then
    Result := FCursor.Value
  else
  if not FOwnList.ReturnDefaultElements then
    raise EJclNoSuchElementError.Create('');
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

function TItr<T>.Insert(const AItem: T): Boolean;
var
  NewCursor: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    CheckValid;
    Result := FCursor <> nil;
    if Result then
    begin
      Result := FOwnList.AllowDefaultElements or not FEqualityComparer.ItemsEqual(AItem, Default(T));
      if Result then
      begin
        case FOwnList.Duplicates of
          dupIgnore:
            Result := not FOwnList.Contains(AItem);
          dupAccept:
            Result := True;
          dupError:
            begin
              Result := FOwnList.Contains(AItem);
              if not Result then
                raise EJclDuplicateElementError.Create;
            end;
        end;
        if Result then
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
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$IFDEF SUPPORTS_FOR_IN}
function TItr<T>.MoveNext: Boolean;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    if Valid and (FCursor <> nil) then
      FCursor := FCursor.Next
    else
      Valid := True;
    Result := FCursor <> nil;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;
{$ENDIF SUPPORTS_FOR_IN}

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
    CheckValid;
    Valid := False;
    if FCursor <> nil then
    begin
      (FownList as IJclItemOwner<T>).FreeItem(FCursor.Value);
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
    CheckValid;
    (FownList as IJclItemOwner<T>).FreeItem(FCursor.Value);
    FCursor.Value := AItem;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

{$ENDIF SUPPORTS_GENERICS}


//=== { TJclLinkedList<T> } ==================================================

constructor TJclIntfLinkedList.Create(const ACollection: IJclIntfCollection);
begin
  inherited Create(nil);
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
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
    Result := FAllowDefaultElements or not ItemsEqual(AInterface, nil);
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AInterface, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclIntfLinkedListItem.Create;
        NewItem.Value := AInterface;
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
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.AddAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  Item: IInterface;
  AddItem: Boolean;
  NewItem: TJclIntfLinkedListItem;
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
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclIntfLinkedListItem.Create;
          NewItem.Value := Item;
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
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclIntfLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclIntfCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclIntfCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
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
      FreeObject(Current.Value);
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

function TJclIntfLinkedList.Contains(const AInterface: IInterface): Boolean;
var
  Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AInterface) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
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

function TJclIntfLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclIntfLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

function TJclIntfLinkedList.Delete(Index: Integer): IInterface;
var
  Current: TJclIntfLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) and (Index < FSize) then
    begin
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
          Result := FreeObject(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
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
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
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

function TJclIntfLinkedList.First: IJclIntfIterator;
begin
  Result := TIntfItr.Create(Self, FStart, False);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclIntfLinkedList.GetEnumerator: IJclIntfIterator;
begin
  Result := TIntfItr.Create(Self, FStart, False);
end;
{$ENDIF SUPPORTS_FOR_IN}

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
      Result := Current.Value
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
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
    while (Current <> nil) and not ItemsEqual(Current.Value, AInterface) do
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

function TJclIntfLinkedList.Insert(Index: Integer; const AInterface: IInterface): Boolean;
var
  Current, NewItem: TJclIntfLinkedListItem;
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
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AInterface, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclIntfLinkedListItem.Create;
        NewItem.Value := AInterface;
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
  Current, NewItem, Test: TJclIntfLinkedListItem;
  AddItem: Boolean;
  Item: IInterface;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclIntfLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclIntfLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
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
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclIntfLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclIntfLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfLinkedList.Last: IJclIntfIterator;
begin
  Result := TIntfItr.Create(Self, FEnd, False);
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
      while (Current <> nil) and not ItemsEqual(Current.Value, AInterface) do
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
      if ItemsEqual(Current.Value, AInterface) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        FreeObject(Current.Value);
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
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
    Result := True;
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
  ReplaceItem: Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AInterface, nil);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AInterface, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeObject(Current.Value);
            Current.Value := AInterface;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
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
    Result := CreateEmptyContainer as IJclIntfList;
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


//=== { TJclLinkedList<T> } ==================================================

constructor TJclAnsiStrLinkedList.Create(const ACollection: IJclAnsiStrCollection);
begin
  inherited Create(nil);
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclAnsiStrLinkedList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclAnsiStrLinkedList.Add(const AString: AnsiString): Boolean;
var
  NewItem: TJclAnsiStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AString, '');
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AString, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclAnsiStrLinkedListItem.Create;
        NewItem.Value := AString;
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
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.AddAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
  Item: AnsiString;
  AddItem: Boolean;
  NewItem: TJclAnsiStrLinkedListItem;
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
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclAnsiStrLinkedListItem.Create;
          NewItem.Value := Item;
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
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclAnsiStrLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclAnsiStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclAnsiStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclAnsiStrLinkedList.Clear;
var
  Old, Current: TJclAnsiStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeString(Current.Value);
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

function TJclAnsiStrLinkedList.Contains(const AString: AnsiString): Boolean;
var
  Current: TJclAnsiStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AString) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.ContainsAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
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

function TJclAnsiStrLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclAnsiStrLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

function TJclAnsiStrLinkedList.Delete(Index: Integer): AnsiString;
var
  Current: TJclAnsiStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (Index >= 0) and (Index < FSize) then
    begin
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
          Result := FreeString(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.Equals(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It, ItSelf: IJclAnsiStrIterator;
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
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
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

function TJclAnsiStrLinkedList.First: IJclAnsiStrIterator;
begin
  Result := TAnsiStrItr.Create(Self, FStart, False);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclAnsiStrLinkedList.GetEnumerator: IJclAnsiStrIterator;
begin
  Result := TAnsiStrItr.Create(Self, FStart, False);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclAnsiStrLinkedList.GetString(Index: Integer): AnsiString;
var
  Current: TJclAnsiStrLinkedListItem;
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
      Result := Current.Value
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.IndexOf(const AString: AnsiString): Integer;
var
  Current: TJclAnsiStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AString) do
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

function TJclAnsiStrLinkedList.Insert(Index: Integer; const AString: AnsiString): Boolean;
var
  Current, NewItem: TJclAnsiStrLinkedListItem;
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
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AString, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclAnsiStrLinkedListItem.Create;
        NewItem.Value := AString;
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
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.InsertAll(Index: Integer; const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
  Current, NewItem, Test: TJclAnsiStrLinkedListItem;
  AddItem: Boolean;
  Item: AnsiString;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclAnsiStrLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclAnsiStrLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
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
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclAnsiStrLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclAnsiStrLinkedList.Last: IJclAnsiStrIterator;
begin
  Result := TAnsiStrItr.Create(Self, FEnd, False);
end;

function TJclAnsiStrLinkedList.LastIndexOf(const AString: AnsiString): Integer;
var
  Current: TJclAnsiStrLinkedListItem;
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
      while (Current <> nil) and not ItemsEqual(Current.Value, AString) do
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

function TJclAnsiStrLinkedList.Remove(const AString: AnsiString): Boolean;
var
  Current: TJclAnsiStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AString) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        FreeString(Current.Value);
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
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

function TJclAnsiStrLinkedList.RemoveAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
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

function TJclAnsiStrLinkedList.RetainAll(const ACollection: IJclAnsiStrCollection): Boolean;
var
  It: IJclAnsiStrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
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

procedure TJclAnsiStrLinkedList.SetString(Index: Integer; const AString: AnsiString);
var
  Current: TJclAnsiStrLinkedListItem;
  ReplaceItem: Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AString, '');
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AString, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeString(Current.Value);
            Current.Value := AString;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclAnsiStrLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclAnsiStrLinkedList.SubList(First, Count: Integer): IJclAnsiStrList;
var
  Current: TJclAnsiStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclAnsiStrList;
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


//=== { TJclLinkedList<T> } ==================================================

constructor TJclWideStrLinkedList.Create(const ACollection: IJclWideStrCollection);
begin
  inherited Create(nil);
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
end;

destructor TJclWideStrLinkedList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJclWideStrLinkedList.Add(const AString: WideString): Boolean;
var
  NewItem: TJclWideStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := FAllowDefaultElements or not ItemsEqual(AString, '');
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AString, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclWideStrLinkedListItem.Create;
        NewItem.Value := AString;
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
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.AddAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
  Item: WideString;
  AddItem: Boolean;
  NewItem: TJclWideStrLinkedListItem;
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
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclWideStrLinkedListItem.Create;
          NewItem.Value := Item;
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
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclWideStrLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclWideStrCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclWideStrCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
end;

procedure TJclWideStrLinkedList.Clear;
var
  Old, Current: TJclWideStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    while Current <> nil do
    begin
      FreeString(Current.Value);
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

function TJclWideStrLinkedList.Contains(const AString: WideString): Boolean;
var
  Current: TJclWideStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AString) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.ContainsAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
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

function TJclWideStrLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclWideStrLinkedList.Create(nil);
  AssignPropertiesTo(Result);
end;

function TJclWideStrLinkedList.Delete(Index: Integer): WideString;
var
  Current: TJclWideStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := '';
    if (Index >= 0) and (Index < FSize) then
    begin
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
          Result := FreeString(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.Equals(const ACollection: IJclWideStrCollection): Boolean;
var
  It, ItSelf: IJclWideStrIterator;
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
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
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

function TJclWideStrLinkedList.First: IJclWideStrIterator;
begin
  Result := TWideStrItr.Create(Self, FStart, False);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclWideStrLinkedList.GetEnumerator: IJclWideStrIterator;
begin
  Result := TWideStrItr.Create(Self, FStart, False);
end;
{$ENDIF SUPPORTS_FOR_IN}

function TJclWideStrLinkedList.GetString(Index: Integer): WideString;
var
  Current: TJclWideStrLinkedListItem;
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
      Result := Current.Value
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
  {$IFDEF THREADSAFE}
  finally
    ReadUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.IndexOf(const AString: WideString): Integer;
var
  Current: TJclWideStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Current := FStart;
    Result := 0;
    while (Current <> nil) and not ItemsEqual(Current.Value, AString) do
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

function TJclWideStrLinkedList.Insert(Index: Integer; const AString: WideString): Boolean;
var
  Current, NewItem: TJclWideStrLinkedListItem;
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
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AString, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclWideStrLinkedListItem.Create;
        NewItem.Value := AString;
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
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.InsertAll(Index: Integer; const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
  Current, NewItem, Test: TJclWideStrLinkedListItem;
  AddItem: Boolean;
  Item: WideString;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclWideStrLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclWideStrLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
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
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, '');
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclWideStrLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclWideStrLinkedList.Last: IJclWideStrIterator;
begin
  Result := TWideStrItr.Create(Self, FEnd, False);
end;

function TJclWideStrLinkedList.LastIndexOf(const AString: WideString): Integer;
var
  Current: TJclWideStrLinkedListItem;
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
      while (Current <> nil) and not ItemsEqual(Current.Value, AString) do
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

function TJclWideStrLinkedList.Remove(const AString: WideString): Boolean;
var
  Current: TJclWideStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AString) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        FreeString(Current.Value);
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
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

function TJclWideStrLinkedList.RemoveAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
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

function TJclWideStrLinkedList.RetainAll(const ACollection: IJclWideStrCollection): Boolean;
var
  It: IJclWideStrIterator;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    if ACollection = nil then
      Exit;
    Result := True;
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

procedure TJclWideStrLinkedList.SetString(Index: Integer; const AString: WideString);
var
  Current: TJclWideStrLinkedListItem;
  ReplaceItem: Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AString, '');
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AString, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeString(Current.Value);
            Current.Value := AString;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclWideStrLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclWideStrLinkedList.SubList(First, Count: Integer): IJclWideStrList;
var
  Current: TJclWideStrLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := CreateEmptyContainer as IJclWideStrList;
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


//=== { TJclLinkedList<T> } ==================================================

constructor TJclLinkedList.Create(const ACollection: IJclCollection; AOwnsObjects: Boolean);
begin
  inherited Create(nil, AOwnsObjects);
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
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
    Result := FAllowDefaultElements or not ItemsEqual(AObject, nil);
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AObject, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclLinkedListItem.Create;
        NewItem.Value := AObject;
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
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.AddAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  Item: TObject;
  AddItem: Boolean;
  NewItem: TJclLinkedListItem;
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
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclLinkedListItem.Create;
          NewItem.Value := Item;
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
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclLinkedList.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclCollection;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclCollection, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
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
      FreeObject(Current.Value);
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

function TJclLinkedList.Contains(AObject: TObject): Boolean;
var
  Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AObject) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
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

function TJclLinkedList.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclLinkedList.Create(nil, False);
  AssignPropertiesTo(Result);
end;

function TJclLinkedList.Delete(Index: Integer): TObject;
var
  Current: TJclLinkedListItem;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := nil;
    if (Index >= 0) and (Index < FSize) then
    begin
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
          Result := FreeObject(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
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
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
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

function TJclLinkedList.First: IJclIterator;
begin
  Result := TItr.Create(Self, FStart, False);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclLinkedList.GetEnumerator: IJclIterator;
begin
  Result := TItr.Create(Self, FStart, False);
end;
{$ENDIF SUPPORTS_FOR_IN}

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
      Result := Current.Value
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
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
    while (Current <> nil) and not ItemsEqual(Current.Value, AObject) do
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

function TJclLinkedList.Insert(Index: Integer; AObject: TObject): Boolean;
var
  Current, NewItem: TJclLinkedListItem;
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
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AObject, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
        NewItem := TJclLinkedListItem.Create;
        NewItem.Value := AObject;
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
  Current, NewItem, Test: TJclLinkedListItem;
  AddItem: Boolean;
  Item: TObject;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclLinkedListItem.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
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
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, nil);
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclLinkedListItem.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclLinkedList.Last: IJclIterator;
begin
  Result := TItr.Create(Self, FEnd, False);
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
      while (Current <> nil) and not ItemsEqual(Current.Value, AObject) do
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
      if ItemsEqual(Current.Value, AObject) then
      begin
        if Current.Previous <> nil then
          Current.Previous.Next := Current.Next
        else
          FStart := Current.Next;
        if Current.Next <> nil then
          Current.Next.Previous := Current.Previous
        else
          FEnd := Current.Previous;
        FreeObject(Current.Value);
        Current.Free;
        Dec(FSize);
        Result := True;
        if FRemoveSingleElement then
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
    Result := True;
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
  ReplaceItem: Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AObject, nil);
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AObject, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if Index = 0 then
          begin
            FreeObject(Current.Value);
            Current.Value := AObject;
            Break;
          end;
          Dec(Index);
          Current := Current.Next;
        end;
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
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
    Result := CreateEmptyContainer as IJclList;
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

{$IFDEF SUPPORTS_GENERICS}


//=== { TJclLinkedList<T> } ==================================================

constructor TJclLinkedList<T>.Create(const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  inherited Create(nil, AOwnsItems);
  FStart := nil;
  FEnd := nil;
  if ACollection <> nil then
    AddAll(ACollection);
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
    Result := FAllowDefaultElements or not ItemsEqual(AItem, Default(T));
    if Result then
    begin
      if FDuplicates <> dupAccept then
      begin
        NewItem := FStart;
        while NewItem <> nil do
        begin
          if ItemsEqual(AItem, NewItem.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          NewItem := NewItem.Next;
        end;
      end;
      if Result then
      begin
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
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.AddAll(const ACollection: IJclCollection<T>): Boolean;
var
  It: IJclIterator<T>;
  Item: T;
  AddItem: Boolean;
  NewItem: TJclLinkedListItem<T>;
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
      AddItem := FAllowDefaultElements or not ItemsEqual(Item, Default(T));
      if AddItem then
      begin
        if FDuplicates <> dupAccept then
        begin
          NewItem := FStart;
          while NewItem <> nil do
          begin
            if ItemsEqual(Item, NewItem.Value) then
            begin
              AddItem := CheckDuplicate;
              Break;
            end;
            NewItem := NewItem.Next;
          end;
        end;
        if AddItem then
        begin
          NewItem := TJclLinkedListItem<T>.Create;
          NewItem.Value := Item;
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
        end;
      end;
      Result := AddItem and Result;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

procedure TJclLinkedList<T>.AssignDataTo(Dest: TJclAbstractContainerBase);
var
  ACollection: IJclCollection<T>;
begin
  inherited AssignDataTo(Dest);
  if Supports(IInterface(Dest), IJclCollection<T>, ACollection) then
  begin
    ACollection.Clear;
    ACollection.AddAll(Self);
  end;
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

function TJclLinkedList<T>.Contains(const AItem: T): Boolean;
var
  Current: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  ReadLock;
  try
  {$ENDIF THREADSAFE}
    Result := False;
    Current := FStart;
    while Current <> nil do
    begin
      if ItemsEqual(Current.Value, AItem) then
      begin
        Result := True;
        Break;
      end;
      Current := Current.Next;
    end;
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


function TJclLinkedList<T>.Delete(Index: Integer): T;
var
  Current: TJclLinkedListItem<T>;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    Result := Default(T);
    if (Index >= 0) and (Index < FSize) then
    begin
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
          Result := FreeItem(Current.Value);
          Current.Free;
          Dec(FSize);
          Break;
        end;
        Dec(Index);
        Current := Current.Next;
      end;
    end
    else
      raise EJclOutOfBoundsError.Create;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
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
    Result := True;
    It := ACollection.First;
    ItSelf := First;
    while ItSelf.HasNext and It.HasNext do
      if not ItemsEqual(ItSelf.Next, It.Next) then
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

function TJclLinkedList<T>.First: IJclIterator<T>;
begin
  Result := TItr<T>.Create(Self, FStart, False);
end;

{$IFDEF SUPPORTS_FOR_IN}
function TJclLinkedList<T>.GetEnumerator: IJclIterator<T>;
begin
  Result := TItr<T>.Create(Self, FStart, False);
end;
{$ENDIF SUPPORTS_FOR_IN}

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
      Result := Current.Value
    else
    if not FReturnDefaultElements then
      raise EJclNoSuchElementError.Create('');
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

function TJclLinkedList<T>.Insert(Index: Integer; const AItem: T): Boolean;
var
  Current, NewItem: TJclLinkedListItem<T>;
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
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AItem, Current.Value) then
          begin
            Result := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if Result then
      begin
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
  Current, NewItem, Test: TJclLinkedListItem<T>;
  AddItem: Boolean;
  Item: T;
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
    if Index = 0 then
    begin
      It := ACollection.Last;
      while It.HasPrevious do
      begin
        Item := It.Previous;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, Default(T));
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclLinkedListItem<T>.Create;
            NewItem.Value := Item;
            NewItem.Next := FStart;
            if FStart <> nil then
              FStart.Previous := NewItem;
            FStart := NewItem;
            if FSize = 0 then
              FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
      end;
    end
    else
    if Index = Size then
    begin
      It := ACollection.First;
      while It.HasNext do
      begin
        Item := It.Next;
        AddItem := FAllowDefaultElements or not ItemsEqual(Item, Default(T));
        if AddItem then
        begin
          if FDuplicates <> dupAccept then
          begin
            Test := FStart;
            while Test <> nil do
            begin
              if ItemsEqual(Item, Test.Value) then
              begin
                Result := CheckDuplicate;
                Break;
              end;
              Test := Test.Next;
            end;
          end;
          if AddItem then
          begin
            NewItem := TJclLinkedListItem<T>.Create;
            NewItem.Value := Item;
            NewItem.Previous := FEnd;
            if FEnd <> nil then
              FEnd.Next := NewItem;
            FEnd := NewItem;
            Inc(FSize);
          end;
        end;
        Result := Result and AddItem;
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
          Item := It.Next;
          AddItem := FAllowDefaultElements or not ItemsEqual(Item, Default(T));
          if AddItem then
          begin
            if FDuplicates <> dupAccept then
            begin
              Test := FStart;
              while Test <> nil do
              begin
                if ItemsEqual(Item, Test.Value) then
                begin
                  Result := CheckDuplicate;
                  Break;
                end;
                Test := Test.Next;
              end;
            end;
            if AddItem then
            begin
              NewItem := TJclLinkedListItem<T>.Create;
              NewItem.Value := Item;
              NewItem.Next := Current;
              NewItem.Previous := Current.Previous;
              if Current.Previous <> nil then
                Current.Previous.Next := NewItem;
              Current.Previous := NewItem;
              Inc(FSize);
            end;
          end;
          Result := Result and AddItem;
        end;
      end;
    end;
  {$IFDEF THREADSAFE}
  finally
    WriteUnlock;
  end;
  {$ENDIF THREADSAFE}
end;

function TJclLinkedList<T>.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclLinkedList<T>.Last: IJclIterator<T>;
begin
  Result := TItr<T>.Create(Self, FEnd, False);
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
        if FRemoveSingleElement then
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
    Result := True;
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
  ReplaceItem: Boolean;
begin
  {$IFDEF THREADSAFE}
  WriteLock;
  try
  {$ENDIF THREADSAFE}
    ReplaceItem := FAllowDefaultElements or not ItemsEqual(AItem, Default(T));
    if ReplaceItem then
    begin
      if FDuplicates <> dupAccept then
      begin
        Current := FStart;
        while Current <> nil do
        begin
          if ItemsEqual(AItem, Current.Value) then
          begin
            ReplaceItem := CheckDuplicate;
            Break;
          end;
          Current := Current.Next;
        end;
      end;
      if ReplaceItem then
      begin
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
      end;
    end;
    if not ReplaceItem then
      Delete(Index);
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
    Result := CreateEmptyContainer as IJclList<T>;
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

procedure TJclLinkedListE<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclLinkedListE<T> then
    TJclLinkedListE<T>(Dest).FEqualityComparer := FEqualityComparer;
end;

function TJclLinkedListE<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclLinkedListE<T>.Create(EqualityComparer, nil, False);
  AssignPropertiesTo(Result);
end;

function TJclLinkedListE<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if EqualityComparer = nil then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityComparer.Equals(A, B);
end;

//=== { TJclLinkedListF<T> } =================================================

constructor TJclLinkedListF<T>.Create(const AEqualityCompare: TEqualityCompare<T>;
  const ACollection: IJclCollection<T>; AOwnsItems: Boolean);
begin
  inherited Create(ACollection, AOwnsItems);
  FEqualityCompare := AEqualityCompare;
end;

procedure TJclLinkedListF<T>.AssignPropertiesTo(Dest: TJclAbstractContainerBase);
begin
  inherited AssignPropertiesTo(Dest);
  if Dest is TJclLinkedListF<T> then
    TJclLinkedListF<T>(Dest).FEqualityCompare := FEqualityCompare;
end;

function TJclLinkedListF<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclLinkedListF<T>.Create(EqualityCompare, nil, False);
  AssignPropertiesTo(Result);
end;

function TJclLinkedListF<T>.ItemsEqual(const A, B: T): Boolean;
begin
  if not Assigned(EqualityCompare) then
    raise EJclNoEqualityComparerError.Create;
  Result := EqualityCompare(A, B);
end;

//=== { TJclLinkedListI<T> } =================================================

function TJclLinkedListI<T>.CreateEmptyContainer: TJclAbstractContainerBase;
begin
  Result := TJclLinkedListI<T>.Create(nil, False);
  AssignPropertiesTo(Result);
end;

function TJclLinkedListI<T>.ItemsEqual(const A, B: T): Boolean;
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

