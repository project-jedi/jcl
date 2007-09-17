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
  Classes,
  JclBase, JclAbstractContainers, JclContainerIntf;

type
  {$IFDEF CLR}
  TJclIntfLinkedListItem = class;
  PJclIntfLinkedListItem = TJclIntfLinkedListItem;
  TJclIntfLinkedListItem = class
  {$ELSE}
  PJclIntfLinkedListItem = ^TJclIntfLinkedListItem;
  TJclIntfLinkedListItem = record
  {$ENDIF CLR}
    Obj: IInterface;
    Next: PJclIntfLinkedListItem;
  end;

  {$IFDEF CLR}
  TJclStrLinkedListItem = class;
  PJclStrLinkedListItem = TJclStrLinkedListItem;
  TJclStrLinkedListItem = class
  {$ELSE}
  PJclStrLinkedListItem = ^TJclStrLinkedListItem;
  TJclStrLinkedListItem = record
  {$ENDIF CLR}
    Str: string;
    Next: PJclStrLinkedListItem;
  end;

  {$IFDEF CLR}
  TJclLinkedListItem = class;
  PJclLinkedListItem = TJclLinkedListItem;
  TJclLinkedListItem = class
  {$ELSE}
  PJclLinkedListItem = ^TJclLinkedListItem;
  TJclLinkedListItem = record
  {$ENDIF CLR}
    Obj: TObject;
    Next: PJclLinkedListItem;
  end;

  TJclIntfLinkedList = class(TJclAbstractContainer, IJclIntfCollection,
      IJclIntfList, IJclIntfCloneable)
  private
    FStart: PJclIntfLinkedListItem;
    FEnd: PJclIntfLinkedListItem;
    FSize: Integer;
  protected
    procedure AddFirst(const AInterface: IInterface);
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
    { IJclIntfCloneable }
    function Clone: IInterface;
  public
    constructor Create(const ACollection: IJclIntfCollection = nil);
    destructor Destroy; override;
  end;

  //Daniele Teti 02/03/2005
  TJclStrLinkedList = class(TJclStrCollection, IJclStrList, IJclCloneable)
  private
    FStart: PJclStrLinkedListItem;
    FEnd: PJclStrLinkedListItem;
    FSize: Integer;
  protected
    procedure AddFirst(const AString: string);
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
  public
    constructor Create(const ACollection: IJclStrCollection = nil);
    destructor Destroy; override;
  end;

  TJclLinkedList = class(TJclAbstractContainer, IJclCollection, IJclList,
      IJclCloneable)
  private
    FStart: PJclLinkedListItem;
    FEnd: PJclLinkedListItem;
    FSize: Integer;
    FOwnsObjects: Boolean;
  protected
    procedure AddFirst(AObject: TObject);
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
  public
    constructor Create(const ACollection: IJclCollection = nil; AOwnsObjects: Boolean = True);
    destructor Destroy; override;
    property OwnsObjects: Boolean read FOwnsObjects;
  end;

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

//=== { TIntfItr } ===========================================================

type
  TIntfItr = class(TJclAbstractContainer, IJclIntfIterator)
  private
    FCursor: PJclIntfLinkedListItem;
    FOwnList: TJclIntfLinkedList;
    FLastRet: PJclIntfLinkedListItem;
    FSize: Integer;
  protected
    { IJclIterator}
    procedure Add(const AInterface: IInterface);
    function GetObject: IInterface;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Next: IInterface;
    function NextIndex: Integer;
    function Previous: IInterface;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetObject(const AInterface: IInterface);
  public
    constructor Create(OwnList: TJclIntfLinkedList; Start: PJclIntfLinkedListItem);
    {$IFNDEF CLR}
    destructor Destroy; override;
    {$ENDIF ~CLR}
  end;

constructor TIntfItr.Create(OwnList: TJclIntfLinkedList; Start: PJclIntfLinkedListItem);
begin
  inherited Create;
  FCursor := Start;
  FOwnList := OwnList;
  {$IFNDEF CLR}
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  {$ENDIF ~CLR}
  FLastRet := nil;
  FSize := FOwnList.Size;
end;

{$IFNDEF CLR}
destructor TIntfItr.Destroy;
begin
  FOwnList._Release;
  inherited Destroy;
end;
{$ENDIF ~CLR}

procedure TIntfItr.Add(const AInterface: IInterface);
var
  NewItem: PJclIntfLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  if AInterface = nil then
    Exit;
  {$IFDEF CLR}
  NewItem := TJclIntfLinkedListItem.Create;
  {$ELSE}
  New(NewItem);
  {$ENDIF CLR}
  NewItem.Obj := AInterface;
  if FCursor = nil then
  begin
    FCursor := NewItem;
    NewItem.Next := nil;
  end
  else
  begin
    NewItem.Next := FCursor.Next;
    FCursor.Next := NewItem;
  end;
  Inc(FOwnList.FSize);
  Inc(FSize);
end;

function TIntfItr.GetObject: IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := FCursor.Obj;
end;

function TIntfItr.HasNext: Boolean;
begin
  Result := FCursor <> nil;
end;

function TIntfItr.HasPrevious: Boolean;
begin
  // Unidirectional
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

function TIntfItr.Next: IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := FCursor.Obj;
  FLastRet := FCursor;
  FCursor := FCursor.Next;
end;

function TIntfItr.NextIndex: Integer;
begin
  // No index
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

function TIntfItr.Previous: IInterface;
begin
  // Unidirectional
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

function TIntfItr.PreviousIndex: Integer;
begin
  // No Index;
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

procedure TIntfItr.Remove;
var
  Current: PJclIntfLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  if FCursor = nil then
    Exit;
  Current := FCursor;
  FCursor := FCursor.Next;
  if FLastRet = nil then
    FOwnList.FStart := FCursor
  else
    FLastRet.Next := FCursor;
  Current.Next := nil;
  Current.Obj := nil;
  {$IFDEF CLR}
  Current.Free;
  {$ELSE}
  Dispose(Current);
  {$ENDIF CLR}
  Dec(FOwnList.FSize);
  Dec(FSize);
end;

procedure TIntfItr.SetObject(const AInterface: IInterface);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  FCursor.Obj := AInterface;
end;

//=== { TStrItr } ============================================================

type
  TStrItr = class(TJclAbstractContainer, IJclStrIterator)
  private
    FCursor: PJclStrLinkedListItem;
    FOwnList: TJclStrLinkedList;
    FLastRet: PJclStrLinkedListItem;
    FSize: Integer;
  protected
    { IJclStrIterator}
    procedure Add(const AString: string);
    function GetString: string;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Next: string;
    function NextIndex: Integer;
    function Previous: string;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetString(const AString: string);
  public
    constructor Create(OwnList: TJclStrLinkedList; Start: PJclStrLinkedListItem);
    {$IFNDEF CLR}
    destructor Destroy; override;
    {$ENDIF ~CLR}
  end;

constructor TStrItr.Create(OwnList: TJclStrLinkedList; Start: PJclStrLinkedListItem);
begin
  inherited Create;
  FCursor := Start;
  FOwnList := OwnList;
  {$IFNDEF CLR}
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  {$ENDIF ~CLR}
  FLastRet := nil;
  FSize := FOwnList.Size;
end;

{$IFNDEF CLR}
destructor TStrItr.Destroy;
begin
  FOwnList._Release;
  inherited Destroy;
end;
{$ENDIF ~CLR}

procedure TStrItr.Add(const AString: string);
var
  NewItem: PJclStrLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  if AString = '' then
    Exit;
  {$IFDEF CLR}
  NewItem := TJclStrLinkedListItem.Create;
  {$ELSE}
  New(NewItem);
  {$ENDIF CLR}
  NewItem.Str := AString;
  if FCursor = nil then
  begin
    FCursor := NewItem;
    NewItem.Next := nil;
  end
  else
  begin
    NewItem.Next := FCursor.Next;
    FCursor.Next := NewItem;
  end;
  Inc(FOwnList.FSize);
  Inc(FSize);
end;

function TStrItr.GetString: string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := FCursor.Str;
end;

function TStrItr.HasNext: Boolean;
begin
  Result := FCursor <> nil;
end;

function TStrItr.HasPrevious: Boolean;
begin
  // Unidirectional
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

function TStrItr.Next: string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := FCursor.Str;
  FLastRet := FCursor;
  FCursor := FCursor.Next;
end;

function TStrItr.NextIndex: Integer;
begin
  // No index
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

function TStrItr.Previous: string;
begin
  // Unidirectional
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

function TStrItr.PreviousIndex: Integer;
begin
  // No index
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

procedure TStrItr.Remove;
var
  Current: PJclStrLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  if FCursor = nil then
    Exit;
  Current := FCursor;
  FCursor := FCursor.Next;
  if FLastRet = nil then
    FOwnList.FStart := FCursor
  else
    FLastRet.Next := FCursor;
  Current.Next := nil;
  Current.Str := '';
  {$IFDEF CLR}
  Current.Free;
  {$ELSE}
  Dispose(Current);
  {$ENDIF CLR}
  Dec(FOwnList.FSize);
  Dec(FSize);
end;

procedure TStrItr.SetString(const AString: string);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  FCursor.Str := AString;
end;

//=== { TItr } ===============================================================

type
  TItr = class(TJclAbstractContainer, IJclIterator)
  private
    FCursor: PJclLinkedListItem;
    FOwnList: TJclLinkedList;
    FLastRet: PJclLinkedListItem;
    FSize: Integer;
  public
    { IJclIterator}
    procedure Add(AObject: TObject);
    function GetObject: TObject;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Next: TObject;
    function NextIndex: Integer;
    function Previous: TObject;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetObject(AObject: TObject);
  public
    constructor Create(OwnList: TJclLinkedList; Start: PJclLinkedListItem);
    {$IFNDEF CLR}
    destructor Destroy; override;
    {$ENDIF ~CLR}
  end;

constructor TItr.Create(OwnList: TJclLinkedList; Start: PJclLinkedListItem);
begin
  inherited Create;
  FCursor := Start;
  FOwnList := OwnList;
  {$IFNDEF CLR}
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  {$ENDIF ~CLR}
  FLastRet := nil;
  FSize := FOwnList.Size;
end;

{$IFNDEF CLR}
destructor TItr.Destroy;
begin
  FOwnList._Release;
  inherited Destroy;
end;
{$ENDIF ~CLR}

procedure TItr.Add(AObject: TObject);
var
  NewItem: PJclLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  if AObject = nil then
    Exit;
  {$IFDEF CLR}
  NewItem := TJclLinkedListItem.Create;
  {$ELSE}
  New(NewItem);
  {$ENDIF CLR}
  NewItem.Obj := AObject;
  if FCursor = nil then
  begin
    FCursor := NewItem;
    NewItem.Next := nil;
  end
  else
  begin
    NewItem.Next := FCursor.Next;
    FCursor.Next := NewItem;
  end;
  Inc(FOwnList.FSize);
  Inc(FSize);
end;

function TItr.GetObject: TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := FCursor.Obj;
end;

function TItr.HasNext: Boolean;
begin
  Result := FCursor <> nil;
end;

function TItr.HasPrevious: Boolean;
begin
  // Unidirectional
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

function TItr.Next: TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := FCursor.Obj;
  FLastRet := FCursor;
  FCursor := FCursor.Next;
end;

function TItr.NextIndex: Integer;
begin
  // No Index
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

function TItr.Previous: TObject;
begin
  // Unidirectional
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

function TItr.PreviousIndex: Integer;
begin
  // No Index
  {$IFDEF CLR}
  raise EJclOperationNotSupportedError.Create(RsEOperationNotSupported);
  {$ELSE}
  raise EJclOperationNotSupportedError.CreateRes(@RsEOperationNotSupported);
  {$ENDIF CLR}
end;

procedure TItr.Remove;
var
  Current: PJclLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  if FCursor = nil then
    Exit;
  Current := FCursor;
  FCursor := FCursor.Next;
  if FLastRet = nil then
    FOwnList.FStart := FCursor
  else
    FLastRet.Next := FCursor;
  Current.Next := nil;
  if FOwnList.FOwnsObjects then
    Current.Obj.Free;
  Current.Obj := nil;
  {$IFDEF CLR}
  Current.Free;
  {$ELSE}
  Dispose(Current);
  {$ENDIF CLR}
  Dec(FOwnList.FSize);
  Dec(FSize);
end;

procedure TItr.SetObject(AObject: TObject);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  FCursor.Obj := AObject;
end;

//=== { TJclIntfLinkedList } =================================================

constructor TJclIntfLinkedList.Create(const ACollection: IJclIntfCollection = nil);
var
  It: IJclIntfIterator;
begin
  inherited Create;
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

procedure TJclIntfLinkedList.Insert(Index: Integer; const AInterface: IInterface);
var
  I: Integer;
  Current: PJclIntfLinkedListItem;
  NewItem: PJclIntfLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  if (Index < 0) or (Index > FSize) then
    {$IFDEF CLR}
    raise EJclOutOfBoundsError.Create(RsEOutOfBounds);
    {$ELSE}
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
    {$ENDIF CLR}
  if AInterface = nil then
    Exit;
  if FStart = nil then
  begin
    AddFirst(AInterface);
    Exit;
  end;
  {$IFDEF CLR}
  NewItem := TJclIntfLinkedListItem.Create;
  {$ELSE}
  New(NewItem);
  {$ENDIF CLR}
  NewItem.Obj := AInterface;
  if Index = 0 then
  begin
    NewItem.Next := FStart;
    FStart := NewItem;
    Inc(FSize);
  end
  else
  begin
    Current := FStart;
    I := 0;
    while (Current <> nil) and (I <> Index) do
      Current := Current.Next;
    NewItem.Next := Current.Next;
    Current.Next := NewItem;
    Inc(FSize);
  end;
end;

function TJclIntfLinkedList.Add(const AInterface: IInterface): Boolean;
var
  NewItem: PJclIntfLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if AInterface = nil then
    Exit;
  Result := True;
  if FStart = nil then
  begin
    AddFirst(AInterface);
    Exit;
  end;
  {$IFDEF CLR}
  NewItem := TJclIntfLinkedListItem.Create;
  {$ELSE}
  New(NewItem);
  {$ENDIF CLR}
  NewItem.Obj := AInterface;
  NewItem.Next := nil;
  FEnd.Next := NewItem;
  FEnd := NewItem;
  Inc(FSize);
end;

function TJclIntfLinkedList.AddAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) or Result;
end;

function TJclIntfLinkedList.InsertAll(Index: Integer; const ACollection: IJclIntfCollection): Boolean;
var
  I: Integer;
  It: IJclIntfIterator;
  Current: PJclIntfLinkedListItem;
  NewItem: PJclIntfLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if (Index < 0) or (Index > FSize) then
    {$IFDEF CLR}
    raise EJclOutOfBoundsError.Create(RsEOutOfBounds);
    {$ELSE}
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
    {$ENDIF CLR}
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  // (rom) is this a bug? Only one element added.
  if (FStart = nil) and It.HasNext then
  begin
    AddFirst(It.Next);
    Exit;
  end;
  Current := FStart;
  I := 0;
  while (Current <> nil) and (I <> Index) do
    Current := Current.Next;
  while It.HasNext do
  begin
    {$IFDEF CLR}
    NewItem := TJclIntfLinkedListItem.Create;
    {$ELSE}
    New(NewItem);
    {$ENDIF CLR}
    NewItem.Obj := It.Next;
    if Index = 0 then
    begin
      NewItem.Next := FStart;
      FStart := NewItem;
      Inc(FSize);
    end
    else
    begin
      NewItem.Next := Current.Next;
      Current.Next := NewItem;
      Inc(FSize);
    end;
    Inc(Index);
  end;
  Result := True;
end;

procedure TJclIntfLinkedList.AddFirst(const AInterface: IInterface);
begin
  {$IFDEF CLR}
  FStart := TJclIntfLinkedListItem.Create;
  {$ELSE}
  New(FStart);
  {$ENDIF CLR}
  FStart.Obj := AInterface;
  FStart.Next := nil;
  FEnd := FStart;
  Inc(FSize);
end;

procedure TJclIntfLinkedList.Clear;
var
  I: Integer;
  Old, Current: PJclIntfLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    Current.Obj := nil;
    //FreeObject(Current.Obj); //Daniele Teti 06 Maj 2005 // (outchy) wrong line
    Old := Current;
    Current := Current.Next;
    {$IFDEF CLR}
    Old.Free;
    {$ELSE}
    Dispose(Old);
    {$ENDIF CLR}
  end;
  FSize := 0;

  //Daniele Teti 27/12/2004
  FStart := nil;
  FEnd := nil;
end;

function TJclIntfLinkedList.Clone: IInterface;
var
  NewList: IJclIntfList;
begin
  NewList := TJclIntfLinkedList.Create;
  NewList.AddAll(Self);
  Result := NewList;
end;

function TJclIntfLinkedList.Contains(const AInterface: IInterface): Boolean;
var
  I: Integer;
  Current: PJclIntfLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if AInterface = nil then
    Exit;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Obj = AInterface then
    begin
      Result := True;
      Exit;
    end;
    Current := Current.Next;
  end;
end;

function TJclIntfLinkedList.ContainsAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
  Result := contains(It.Next);
end;

function TJclIntfLinkedList.Equals(const ACollection: IJclIntfCollection): Boolean;
var
  It, ItSelf: IJclIntfIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  if FSize <> ACollection.Size then
    Exit;
  It := ACollection.First;
  ItSelf := First;
  while ItSelf.HasNext do
    if ItSelf.Next <> It.Next then
      Exit;
  Result := True;
end;

function TJclIntfLinkedList.GetObject(Index: Integer): IInterface;
var
  I: Integer;
  Current: PJclIntfLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := nil;
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to Index - 1 do
    Current := Current.Next;
  Result := Current.Obj;
end;

function TJclIntfLinkedList.IndexOf(const AInterface: IInterface): Integer;
var
  I: Integer;
  Current: PJclIntfLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := -1;
  if AInterface = nil then
    Exit;
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Obj = AInterface then
    begin
      Result := I;
      Break;
    end;
    Current := Current.Next;
  end;
end;

function TJclIntfLinkedList.First: IJclIntfIterator;
begin
  Result := TIntfItr.Create(Self, FStart);
end;

function TJclIntfLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclIntfLinkedList.Last: IJclIntfIterator;
begin
  Result := TIntfItr.Create(Self, FStart);
end;

function TJclIntfLinkedList.LastIndexOf(const AInterface: IInterface): Integer;
var
  I: Integer;
  Current: PJclIntfLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := -1;
  if AInterface = nil then
    Exit;
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Obj = AInterface then
      Result := I;
    Current := Current.Next;
  end;
end;

function TJclIntfLinkedList.Remove(const AInterface: IInterface): Boolean;
var
  I: Integer;
  Old, Current: PJclIntfLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if AInterface = nil then
    Exit;
  if FStart = nil then
    Exit;
  Old := nil;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Obj = AInterface then
    begin
      Current.Obj := nil;
      if Old <> nil then
      begin
        Old.Next := Current.Next;
        if Old.Next = nil then
          FEnd := Old;
      end
      else
        FStart := Current.Next;
      {$IFDEF CLR}
      Current.Free;
      {$ELSE}
      Dispose(Current);
      {$ENDIF CLR}
      Dec(FSize);
      Result := True;
      Exit;
    end;
    Old := Current;
    Current := Current.Next;
  end;
end;

function TJclIntfLinkedList.Remove(Index: Integer): IInterface;
var
  I: Integer;
  Old, Current: PJclIntfLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := nil;
  if FStart = nil then
    Exit;
  Old := nil;
  Current := FStart;
  for I := 0 to Index - 1 do
  begin
    Old := Current;
    Current := Current.Next;
  end;
  Current.Obj := nil;
  if Old <> nil then
  begin
    Old.Next := Current.Next;
    if Old.Next = nil then
      FEnd := Old;
  end
  else
    FStart := Current.Next;
  {$IFDEF CLR}
  Current.Free;
  {$ELSE}
  Dispose(Current);
  {$ENDIF CLR}
  Dec(FSize);
end;

function TJclIntfLinkedList.RemoveAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
end;

function TJclIntfLinkedList.RetainAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
end;

procedure TJclIntfLinkedList.SetObject(Index: Integer; const AInterface: IInterface);
var
  I: Integer;
  Current: PJclIntfLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to Index - 1 do
    Current := Current.Next;
  Current.Obj := AInterface;
end;

function TJclIntfLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclIntfLinkedList.SubList(First, Count: Integer): IJclIntfList;
var
  I: Integer;
  It: IJclIntfIterator;
  Last: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Last := First + Count - 1;
  if Last > FSize then
    Last := FSize - 1;
  Result := TJclIntfLinkedList.Create;
  I := 0;
  It := Self.First;
  while (I < First) and It.HasNext do
  begin
    It.Next;
    Inc(I);
  end;
  //I := 0;
  while (I <= Last) and It.HasNext do
  begin
    Result.Add(It.Next);
    Inc(I);
  end;
end;

//=== { TJclStrLinkedList } ==================================================

constructor TJclStrLinkedList.Create(const ACollection: IJclStrCollection = nil);
var
  It: IJclStrIterator;
begin
  inherited Create;
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

procedure TJclStrLinkedList.Insert(Index: Integer; const AString: string);
var
  I: Integer;
  Current: PJclStrLinkedListItem;
  NewItem: PJclStrLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  if (Index < 0) or (Index > FSize) then
    {$IFDEF CLR}
    raise EJclOutOfBoundsError.Create(RsEOutOfBounds);
    {$ELSE}
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
    {$ENDIF CLR}
  if AString = '' then
    Exit;
  if FStart = nil then
  begin
    AddFirst(AString);
    Exit;
  end;
  {$IFDEF CLR}
  NewItem := TJclStrLinkedListItem.Create;
  {$ELSE}
  New(NewItem);
  {$ENDIF CLR}
  NewItem.Str := AString;
  if Index = 0 then
  begin
    NewItem.Next := FStart;
    FStart := NewItem;
    Inc(FSize);
  end
  else
  begin
    Current := FStart;
    I := 0;
    while (Current <> nil) and (I <> Index) do
      Current := Current.Next;
    NewItem.Next := Current.Next;
    Current.Next := NewItem;
    Inc(FSize);
  end;
end;

function TJclStrLinkedList.Add(const AString: string): Boolean;
var
  NewItem: PJclStrLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if AString = '' then
    Exit;
  Result := True;
  if FStart = nil then
  begin
    AddFirst(AString);
    Exit;
  end;
  {$IFDEF CLR}
  NewItem := TJclStrLinkedListItem.Create;
  {$ELSE}
  New(NewItem);
  {$ENDIF CLR}
  NewItem.Str := AString;
  NewItem.Next := nil;
  FEnd.Next := NewItem;
  FEnd := NewItem;
  Inc(FSize);
end;

function TJclStrLinkedList.AddAll(const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) or Result;
end;

function TJclStrLinkedList.InsertAll(Index: Integer; const ACollection: IJclStrCollection): Boolean;
var
  I: Integer;
  It: IJclStrIterator;
  Current: PJclStrLinkedListItem;
  NewItem: PJclStrLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;

  if (Index < 0) or (Index >= FSize) then
    {$IFDEF CLR}
    raise EJclOutOfBoundsError.Create(RsEOutOfBounds);
    {$ELSE}
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
    {$ENDIF CLR}

  It := ACollection.First;
  // (rom) is this a bug? Only one element added.
  if (FStart = nil) and It.HasNext then
  begin
    AddFirst(It.Next);
    //Exit;  //Daniele Teti
  end;
  Current := FStart;
  I := 0;
  while (Current <> nil) and (I <> Index) do
  begin
    Current := Current.Next;
    inc(I);
  end;
  while It.HasNext do
  begin
    {$IFDEF CLR}
    NewItem := TJclStrLinkedListItem.Create;
    {$ELSE}
    New(NewItem);
    {$ENDIF CLR}
    NewItem.Str := It.Next;
    if Index = 0 then
    begin
      NewItem.Next := FStart;
      FStart := NewItem;
      Inc(FSize);
    end
    else
    begin
      NewItem.Next := Current.Next;
      Current.Next := NewItem;
      Inc(FSize);
    end;
    Inc(Index);
  end;
  Result := True;
end;

procedure TJclStrLinkedList.AddFirst(const AString: string);
begin
  {$IFDEF CLR}
  FStart := TJclStrLinkedListItem.Create;
  {$ELSE}
  New(FStart);
  {$ENDIF CLR}
  FStart.Str := AString;
  FStart.Next := nil;
  FEnd := FStart;
  Inc(FSize);
end;

procedure TJclStrLinkedList.Clear;
var
  I: Integer;
  Old, Current: PJclStrLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    Current.Str := '';
    Old := Current;
    Current := Current.Next;
    {$IFDEF CLR}
    Old.Free;
    {$ELSE}
    Dispose(Old);
    {$ENDIF CLR}
  end;
  FSize := 0;

  //Daniele Teti 27/12/2004
  FStart := nil;
  FEnd := nil;
end;

function TJclStrLinkedList.Clone: TObject;
var
  NewList: TJclStrLinkedList;
begin
  NewList := TJclStrLinkedList.Create;
  NewList.AddAll(Self);
  Result := NewList;
end;

function TJclStrLinkedList.Contains(const AString: string): Boolean;
var
  I: Integer;
  Current: PJclStrLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if AString = '' then
    Exit;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Str = AString then
    begin
      Result := True;
      Exit;
    end;
    Current := Current.Next;
  end;
end;

function TJclStrLinkedList.ContainsAll(const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
  Result := contains(It.Next);
end;

function TJclStrLinkedList.Equals(const ACollection: IJclStrCollection): Boolean;
var
  It, ItSelf: IJclStrIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  if FSize <> ACollection.Size then
    Exit;
  It := ACollection.First;
  ItSelf := First;
  while ItSelf.HasNext do
    if ItSelf.Next <> It.Next then
      Exit;
  Result := True;
end;

function TJclStrLinkedList.First: IJclStrIterator;
begin
  Result := TStrItr.Create(Self, FStart);
end;

function TJclStrLinkedList.GetString(Index: Integer): string;
var
  I: Integer;
  Current: PJclStrLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := '';
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to Index - 1 do
    Current := Current.Next;
  Result := Current.Str;
end;

function TJclStrLinkedList.IndexOf(const AString: string): Integer;
var
  I: Integer;
  Current: PJclStrLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := -1;
  if AString = '' then
    Exit;
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Str = AString then
    begin
      Result := I;
      Break;
    end;
    Current := Current.Next;
  end;
end;

function TJclStrLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclStrLinkedList.Last: IJclStrIterator;
begin
  Result := TStrItr.Create(Self, FStart);
end;

function TJclStrLinkedList.LastIndexOf(const AString: string): Integer;
var
  I: Integer;
  Current: PJclStrLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := -1;
  if AString = '' then
    Exit;
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Str = AString then
      Result := I;
    Current := Current.Next;
  end;
end;

function TJclStrLinkedList.Remove(Index: Integer): string;
var
  I: Integer;
  Old, Current: PJclStrLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := '';
  if FStart = nil then
    Exit;
  Old := nil;
  Current := FStart;
  for I := 0 to Index - 1 do
  begin
    Old := Current;
    Current := Current.Next;
  end;
  Current.Str := '';
  if Old <> nil then
  begin
    Old.Next := Current.Next;
    if Old.Next = nil then
      FEnd := Old;
  end
  else
    FStart := Current.Next;
  {$IFDEF CLR}
  Current.Free;
  {$ELSE}
  Dispose(Current);
  {$ENDIF CLR}
  Dec(FSize);
end;

function TJclStrLinkedList.Remove(const AString: string): Boolean;
var
  I: Integer;
  Old, Current: PJclStrLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if AString = '' then
    Exit;
  if FStart = nil then
    Exit;
  Old := nil;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Str = AString then
    begin
      Current.Str := '';
      if Old <> nil then
      begin
        Old.Next := Current.Next;
        if Old.Next = nil then
          FEnd := Old;
      end
      else
        FStart := Current.Next;
      {$IFDEF CLR}
      Current.Free;
      {$ELSE}
      Dispose(Current);
      {$ENDIF CLR}
      Dec(FSize);
      Result := True;
      Exit;
    end;
    Old := Current;
    Current := Current.Next;
  end;
end;

function TJclStrLinkedList.RemoveAll(const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
end;

function TJclStrLinkedList.RetainAll(const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
end;

procedure TJclStrLinkedList.SetString(Index: Integer; const AString: string);
var
  I: Integer;
  Current: PJclStrLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to Index - 1 do
    Current := Current.Next;
  Current.Str := AString;
end;

function TJclStrLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclStrLinkedList.SubList(First, Count: Integer): IJclStrList;
var
  I: Integer;
  It: IJclStrIterator;
  Last: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Last := First + Count - 1;
  if Last > FSize then
    Last := FSize - 1;
  Result := TJclStrLinkedList.Create;
  I := 0;
  It := Self.First;
  while (I < First) and It.HasNext do
  begin
    It.Next;
    Inc(I);
  end;
  //I := 0;
  while (I <= Last) and It.HasNext do
  begin
    Result.Add(It.Next);
    Inc(I);
  end;
end;

//=== { TJclLinkedList } =====================================================

constructor TJclLinkedList.Create(const ACollection: IJclCollection = nil;
  AOwnsObjects: Boolean = True);
var
  It: IJclIterator;
begin
  inherited Create;
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

procedure TJclLinkedList.Insert(Index: Integer; AObject: TObject);
var
  I: Integer;
  Current: PJclLinkedListItem;
  NewItem: PJclLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  if (Index < 0) or (Index > FSize) then
    {$IFDEF CLR}
    raise EJclOutOfBoundsError.Create(RsEOutOfBounds);
    {$ELSE}
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
    {$ENDIF CLR}
  if AObject = nil then
    Exit;
  if FStart = nil then
  begin
    AddFirst(AObject);
    Exit;
  end;
  {$IFDEF CLR}
  NewItem := TJclLinkedListItem.Create;
  {$ELSE}
  New(NewItem);
  {$ENDIF CLR}
  NewItem.Obj := AObject;
  if Index = 0 then
  begin
    NewItem.Next := FStart;
    FStart := NewItem;
    Inc(FSize);
  end
  else
  begin
    Current := FStart;
    for I := 0 to Index - 2 do
      Current := Current.Next;
    NewItem.Next := Current.Next;
    Current.Next := NewItem;
    Inc(FSize);
  end;
end;

function TJclLinkedList.Add(AObject: TObject): Boolean;
var
  NewItem: PJclLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if AObject = nil then
    Exit;
  Result := True;
  if FStart = nil then
  begin
    AddFirst(AObject);
    Exit;
  end;
  {$IFDEF CLR}
  NewItem := TJclLinkedListItem.Create;
  {$ELSE}
  New(NewItem);
  {$ENDIF CLR}
  NewItem.Obj := AObject;
  NewItem.Next := nil;
  FEnd.Next := NewItem;
  FEnd := NewItem;
  Inc(FSize);
end;

function TJclLinkedList.AddAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Add(It.Next) or Result;
  Result := True;
end;

function TJclLinkedList.InsertAll(Index: Integer;
  const ACollection: IJclCollection): Boolean;
var
  I: Integer;
  It: IJclIterator;
  Current: PJclLinkedListItem;
  NewItem: PJclLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if (Index < 0) or (Index > FSize) then
    {$IFDEF CLR}
    raise EJclOutOfBoundsError.Create(RsEOutOfBounds);
    {$ELSE}
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
    {$ENDIF CLR}
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  // (rom) is this a bug? Only one element added.
  if (FStart = nil) and It.HasNext then
  begin
    AddFirst(It.Next);
    Exit;
  end;
  Current := FStart;
  I := 0;
  while (Current <> nil) and (I <> Index) do
    Current := Current.Next;
  while It.HasNext do
  begin
    {$IFDEF CLR}
    NewItem := TJclLinkedListItem.Create;
    {$ELSE}
    New(NewItem);
    {$ENDIF CLR}
    NewItem.Obj := It.Next;
    if Index = 0 then
    begin
      NewItem.Next := FStart;
      FStart := NewItem;
      Inc(FSize);
    end
    else
    begin
      NewItem.Next := Current.Next;
      Current.Next := NewItem;
      Inc(FSize);
    end;
    Inc(Index);
  end;
  Result := True;
end;

procedure TJclLinkedList.AddFirst(AObject: TObject);
begin
  {$IFDEF CLR}
  FStart := TJclLinkedListItem.Create;
  {$ELSE}
  New(FStart);
  {$ENDIF CLR}
  FStart.Obj := AObject;
  FStart.Next := nil;
  FEnd := FStart;
  Inc(FSize);
end;

procedure TJclLinkedList.Clear;
var
  I: Integer;
  Old, Current: PJclLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    FreeObject(Current.Obj);        // (outchy) Fixed Memory Leak
    //Current.Obj := nil;
    Old := Current;
    Current := Current.Next;
    {$IFDEF CLR}
    Old.Free;
    {$ELSE}
    Dispose(Old);
    {$ENDIF CLR}
  end;
  FSize := 0;

  //Daniele Teti 27/12/2004
  FStart := nil;
  FEnd := nil;
end;

function TJclLinkedList.Clone: TObject;
var
  NewList: TJclLinkedList;
begin
  NewList := TJclLinkedList.Create;
  NewList.AddAll(Self);
  Result := NewList;
end;

function TJclLinkedList.Contains(AObject: TObject): Boolean;
var
  I: Integer;
  Current: PJclLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if AObject = nil then
    Exit;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Obj = AObject then
    begin
      Result := True;
      Break;
    end;
    Current := Current.Next;
  end;
end;

function TJclLinkedList.ContainsAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
  Result := contains(It.Next);
end;

function TJclLinkedList.Equals(const ACollection: IJclCollection): Boolean;
var
  It, ItSelf: IJclIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  if FSize <> ACollection.Size then
    Exit;
  It := ACollection.First;
  ItSelf := First;
  while ItSelf.HasNext do
    if ItSelf.Next <> It.Next then
      Exit;
  Result := True;
end;

procedure TJclLinkedList.FreeObject(var AObject: TObject);
begin
  if FOwnsObjects then
    FreeAndNil(AObject);
end;

function TJclLinkedList.GetObject(Index: Integer): TObject;
var
  I: Integer;
  Current: PJclLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := nil;
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to Index - 1 do
    Current := Current.Next;
  Result := Current.Obj;
end;

function TJclLinkedList.IndexOf(AObject: TObject): Integer;
var
  I: Integer;
  Current: PJclLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := -1;
  if AObject = nil then
    Exit;
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Obj = AObject then
    begin
      Result := I;
      Break;
    end;
    Current := Current.Next;
  end;
end;

function TJclLinkedList.First: IJclIterator;
begin
  Result := TItr.Create(Self, FStart);
end;

function TJclLinkedList.IsEmpty: Boolean;
begin
  Result := FSize = 0;
end;

function TJclLinkedList.Last: IJclIterator;
begin
  Result := TItr.Create(Self, FStart);
end;

function TJclLinkedList.LastIndexOf(AObject: TObject): Integer;
var
  I: Integer;
  Current: PJclLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := -1;
  if AObject = nil then
    Exit;
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Obj = AObject then
      Result := I;
    Current := Current.Next;
  end;
end;

function TJclLinkedList.Remove(AObject: TObject): Boolean;
var
  I: Integer;
  Old, Current: PJclLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if AObject = nil then
    Exit;
  if FStart = nil then
    Exit;
  Old := nil;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
    if Current.Obj = AObject then
    begin
      FreeObject(Current.Obj);
      if Old <> nil then
      begin
        Old.Next := Current.Next;
        if Old.Next = nil then
          FEnd := Old;
      end
      else
        FStart := Current.Next;
        {$IFDEF CLR}
        Current.Free;
        {$ELSE}
        Dispose(Current);
        {$ENDIF CLR}
      Dec(FSize);
      Result := True;
      Exit;
    end;
    Old := Current;
    Current := Current.Next;
  end;
end;

function TJclLinkedList.Remove(Index: Integer): TObject;
var
  I: Integer;
  Old, Current: PJclLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := nil;
  if FStart = nil then
    Exit;
  Old := nil;
  Current := FStart;
  for I := 0 to Index - 1 do
  begin
    Old := Current;
    Current := Current.Next;
  end;
  FreeObject(Current.Obj);
  if Old <> nil then
  begin
    Old.Next := Current.Next;
    if Old.Next = nil then
      FEnd := Old;
  end
  else
    FStart := Current.Next;
  {$IFDEF CLR}
  Current.Free;
  {$ELSE}
  Dispose(Current);
  {$ENDIF CLR}
  Dec(FSize);
end;

function TJclLinkedList.RemoveAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Result := Remove(It.Next) and Result;
end;

function TJclLinkedList.RetainAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Result := False;
  if ACollection = nil then
    Exit;
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
end;

procedure TJclLinkedList.SetObject(Index: Integer; AObject: TObject);
var
  I: Integer;
  Current: PJclLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  if FStart = nil then
    Exit;
  Current := FStart;
  for I := 0 to Index - 1 do
    Current := Current.Next;
  Current.Obj := AObject;
end;

function TJclLinkedList.Size: Integer;
begin
  Result := FSize;
end;

function TJclLinkedList.SubList(First, Count: Integer): IJclList;
var
  I: Integer;
  It: IJclIterator;
  Last: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF THREADSAFE}
  Last := First + Count - 1;
  if Last > FSize then
    Last := FSize - 1;
  Result := TJclLinkedList.Create;
  I := 0;
  It := Self.First;
  while (I < First) and It.HasNext do
  begin
    It.Next;
    Inc(I);
  end;
  while (I <= Last) and It.HasNext do
  begin
    Result.Add(It.Next);
    Inc(I);
  end;
end;

{
function TJclStrLinkedList.GetAsStrings: TStrings;
begin
  Result := TStringList.Create;
  try
    AppendToStrings(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TJclStrLinkedList.LoadFromStrings(Strings: TStrings);
begin
  Clear;
  AppendFromStrings(Strings);
end;

procedure TJclStrLinkedList.AppendToStrings(Strings: TStrings);
var
  It: IJclStrIterator;
begin
  It := First;
  Strings.BeginUpdate;
  try
    while It.HasNext do
      Strings.Add(It.Next);
  finally
    Strings.EndUpdate;
  end;
end;

procedure TJclStrLinkedList.SaveToStrings(Strings: TStrings);
begin
  Strings.Clear;
  AppendToStrings(Strings);
end;

procedure TJclStrLinkedList.AppendFromStrings(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Strings.Count - 1 do
    Add(Strings[I]);
end;

function TJclStrLinkedList.GetAsDelimited(Separator: string): string;
var
  It: IJclStrIterator;
begin
  It := First;
  Result := '';
  if It.HasNext then
    Result := It.Next;
  while It.HasNext do
    Result := Result + Separator + It.Next;
end;

procedure TJclStrLinkedList.AppendDelimited(AString, Separator: string);
begin
  DCLAppendDelimited(Self, AString, Separator);
end;

procedure TJclStrLinkedList.LoadDelimited(AString, Separator: string);
begin
  Clear;
  AppendDelimited(AString, Separator);
end;
}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

