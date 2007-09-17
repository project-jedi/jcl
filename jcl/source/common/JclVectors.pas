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
  Classes,
  JclBase, JclAbstractContainers, JclContainerIntf;

type
  TJclIntfVector = class(TJclAbstractContainer, IJclIntfCollection, IJclIntfList,
    IJclIntfArray, IJclIntfCloneable)
  private
    FCount: Integer;
    FCapacity: Integer;
    FItems: TDynIInterfaceArray;
  protected
    procedure Grow; virtual;
    { IJclCloneable }
    function Clone: IInterface;
  public
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

    constructor Create(ACapacity: Integer = DefaultContainerCapacity);
    destructor Destroy; override;
    {$IFNDEF CLR}
    procedure AfterConstruction; override;
    // Do not decrement RefCount because iterator inc/dec it.
    procedure BeforeDestruction; override;
    {$ENDIF ~CLR}
    property Items: TDynIInterfaceArray read FItems;
  end;

  //Daniele Teti 02/03/2005
  TJclStrVector = class(TJclStrCollection, IJclStrList, IJclStrArray, IJclCloneable)
  private
    FCount: Integer;
    FCapacity: Integer;
    FItems: TDynStringArray;
  protected
    procedure Grow; virtual;
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
    constructor Create(ACapacity: Integer = DefaultContainerCapacity);
    destructor Destroy; override;
    {$IFNDEF CLR}
    procedure AfterConstruction; override;
    // Do not decrement RefCount because iterator inc/dec it.
    procedure BeforeDestruction; override;
    {$ENDIF ~CLR}
    property Items: TDynStringArray read FItems;
  end;

  TJclVector = class(TJclAbstractContainer, IJclCollection, IJclList, IJclArray,
    IJclCloneable)
  private
    FCount: Integer;
    FCapacity: Integer;
    FOwnsObjects: Boolean;
    FItems: TDynObjectArray;
  protected
    procedure Grow; virtual;
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
    constructor Create(ACapacity: Integer = DefaultContainerCapacity; AOwnsObjects: Boolean = True);
    destructor Destroy; override;
    {$IFNDEF CLR}
    procedure AfterConstruction; override;
    // Do not decrement RefCount because iterator inc/dec it.
    procedure BeforeDestruction; override;
    {$ENDIF ~CLR}
    property Items: TDynObjectArray read FItems;
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
  JclResources;

//=== { TIntfItr } ===========================================================

type
  TIntfItr = class(TJclAbstractContainer, IJclIntfIterator)
  private
    FCursor: Integer;
    FOwnList: TJclIntfVector;
    FLastRet: Integer;
    FSize: Integer;
  protected
    { IJclIntfIterator}
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
    constructor Create(OwnList: TJclIntfVector);
    {$IFNDEF CLR}
    destructor Destroy; override;
    {$ENDIF ~CLR}
  end;

constructor TIntfItr.Create(OwnList: TJclIntfVector);
begin
  inherited Create;
  FCursor := 0;
  FOwnList := OwnList;
  {$IFNDEF CLR}
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  {$ENDIF ~CLR}
  FLastRet := -1;
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
begin
  FOwnList.Insert(FCursor, AInterface);
  Inc(FSize);
  Inc(FCursor);
  FLastRet := -1;
end;

function TIntfItr.GetObject: IInterface;
begin
  Result := FOwnList.Items[FCursor];
end;

function TIntfItr.HasNext: Boolean;
begin
  Result := FCursor <> FSize;
end;

function TIntfItr.HasPrevious: Boolean;
begin
  Result := FCursor > 0;
end;

function TIntfItr.Next: IInterface;
begin
  Result := FOwnList.Items[FCursor];
  FLastRet := FCursor;
  Inc(FCursor);
end;

function TIntfItr.NextIndex: Integer;
begin
  Result := FCursor;
end;

function TIntfItr.Previous: IInterface;
begin
  Dec(FCursor);
  FLastRet := FCursor;
  Result := FOwnList.Items[FCursor];
end;

function TIntfItr.PreviousIndex: Integer;
begin
  Result := FCursor - 1;
end;

procedure TIntfItr.Remove;
begin
  with FOwnList do
  begin
    FItems[FCursor] := nil; // Force Release
    MoveArray(FItems, FCursor + 1, FCursor, FSize - FCursor);
  end;
  Dec(FOwnList.FCount);
  Dec(FSize);
end;

procedure TIntfItr.SetObject(const AInterface: IInterface);
begin
  FOwnList.Items[FCursor] := AInterface;
end;

//=== { TStrItr } ============================================================

type
  TStrItr = class(TJclAbstractContainer, IJclStrIterator)
  private
    FCursor: Integer;
    FOwnList: TJclStrVector;
    FLastRet: Integer;
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
    constructor Create(OwnList: TJclStrVector);
    {$IFNDEF CLR}
    destructor Destroy; override;
    {$ENDIF ~CLR}
  end;

constructor TStrItr.Create(OwnList: TJclStrVector);
begin
  inherited Create;
  FCursor := 0;
  FOwnList := OwnList;
  {$IFNDEF CLR}
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  {$ENDIF ~CLR}
  FLastRet := -1;
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
begin
  FOwnList.Insert(FCursor, AString);
  Inc(FSize);
  Inc(FCursor);
  FLastRet := -1;
end;

function TStrItr.GetString: string;
begin
  Result := FOwnList.Items[FCursor];
end;

function TStrItr.HasNext: Boolean;
begin
  Result := FCursor < FSize;
end;

function TStrItr.HasPrevious: Boolean;
begin
  Result := FCursor > 0;
end;

function TStrItr.Next: string;
begin
  Result := FOwnList.Items[FCursor];
  FLastRet := FCursor;
  Inc(FCursor);
end;

function TStrItr.NextIndex: Integer;
begin
  Result := FCursor;
end;

function TStrItr.Previous: string;
begin
  Dec(FCursor);
  FLastRet := FCursor;
  Result := FOwnList.Items[FCursor];
end;

function TStrItr.PreviousIndex: Integer;
begin
  Result := FCursor - 1;
end;

procedure TStrItr.Remove;
begin
  with FOwnList do
  begin
    FItems[FCursor] := ''; // Force Release
    MoveArray(FItems, FCursor + 1, FCursor, FSize - FCursor);
  end;
  Dec(FOwnList.FCount);
  Dec(FSize);
end;

procedure TStrItr.SetString(const AString: string);
begin
  {
  if FLastRet = -1 then
    raise EJclIllegalState.Create(SIllegalState);
  }
  FOwnList.Items[FCursor] := AString;
end;

//=== { TItr } ===============================================================

type
  TItr = class(TJclAbstractContainer, IJclIterator)
  private
    FCursor: Integer;
    FOwnList: TJclVector;
    FLastRet: Integer;
    FSize: Integer;
  protected
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
    constructor Create(OwnList: TJclVector);
    {$IFNDEF CLR}
    destructor Destroy; override;
    {$ENDIF ~CLR}
  end;

constructor TItr.Create(OwnList: TJclVector);
begin
  inherited Create;
  FCursor := 0;
  FOwnList := OwnList;
  {$IFNDEF CLR}
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  {$ENDIF ~CLR}
  FLastRet := -1;
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
begin
  FOwnList.Insert(FCursor, AObject);
  Inc(FSize);
  Inc(FCursor);
  FLastRet := -1;
end;

function TItr.GetObject: TObject;
begin
  Result := FOwnList.Items[FCursor];
end;

function TItr.HasNext: Boolean;
begin
  Result := FCursor <> FSize;
end;

function TItr.HasPrevious: Boolean;
begin
  Result := FCursor > 0;
end;

function TItr.Next: TObject;
begin
  Result := FOwnList.Items[FCursor];
  FLastRet := FCursor;
  Inc(FCursor);
end;

function TItr.NextIndex: Integer;
begin
  Result := FCursor;
end;

function TItr.Previous: TObject;
begin
  Dec(FCursor);
  FLastRet := FCursor;
  Result := FOwnList.Items[FCursor];
end;

function TItr.PreviousIndex: Integer;
begin
  Result := FCursor - 1;
end;

procedure TItr.Remove;
begin
  with FOwnList do
  begin
    FreeObject(FItems[FCursor]);
    MoveArray(FItems, FCursor + 1, FCursor, FSize - FCursor);
  end;
  Dec(FOwnList.FCount);
  Dec(FSize);
end;

procedure TItr.SetObject(AObject: TObject);
begin
  {
  if FLastRet = -1 then
    raise EJclIllegalState.Create(SIllegalState);
  }
  FOwnList.Items[FCursor] := AObject;
end;

//=== { TJclIntfVector } =====================================================

constructor TJclIntfVector.Create(ACapacity: Integer = DefaultContainerCapacity);
begin
  inherited Create;
  FCount := 0;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FItems, FCapacity);
end;

destructor TJclIntfVector.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclIntfVector.Insert(Index: Integer; const AInterface: IInterface);
begin
  if (Index < 0) or (Index > FCount) then
    {$IFDEF CLR}
    raise EJclOutOfBoundsError.Create(RsEOutOfBounds);
    {$ELSE}
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
    {$ENDIF CLR}
  if FCount = FCapacity then
    Grow;
  MoveArray(FItems, Index, Index + 1, FCount - Index);
  FItems[Index] := AInterface;
  Inc(FCount);
end;

function TJclIntfVector.Add(const AInterface: IInterface): Boolean;
begin
  if FCount = FCapacity then
    Grow;
  FItems[FCount] := AInterface;
  Inc(FCount);
  Result := True;
end;

function TJclIntfVector.InsertAll(Index: Integer; const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  Size: Integer;
begin
  Result := False;
  if (Index < 0) or (Index >= FCount) then
    {$IFDEF CLR}
    raise EJclOutOfBoundsError.Create(RsEOutOfBounds);
    {$ELSE}
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
    {$ENDIF CLR}
  if ACollection = nil then
    Exit;
  Size := ACollection.Size;
  if Size <> 0 then
  begin
    Inc(FCapacity, Size);
    SetLength(FItems, FCapacity);
    Inc(FCount, Size);
    MoveArray(FItems, Index, Index + Size, Size);
    It := ACollection.First;
    while It.HasNext do
    begin
      FItems[Index] := It.Next;
      Inc(Index);
    end;
  end;
  Result := True;
end;

function TJclIntfVector.AddAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Add(It.Next);
  Result := True;
end;

procedure TJclIntfVector.Clear;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FItems[I] := nil;
  FCount := 0;
end;

function TJclIntfVector.Clone: IInterface;
var
  NewList: IJclIntfList;
begin
  NewList := TJclIntfVector.Create(FCapacity);
  NewList.AddAll(Self);
  Result := NewList;
end;

function TJclIntfVector.Contains(const AInterface: IInterface): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AInterface = nil then
    Exit;
  for I := 0 to FCount - 1 do
    if Items[I] = AInterface then
    begin
      Result := True;
      Break;
    end;
end;

function TJclIntfVector.ContainsAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
    Result := Contains(It.Next);
end;

function TJclIntfVector.Equals(const ACollection: IJclIntfCollection): Boolean;
var
  I: Integer;
  It: IJclIntfIterator;
begin
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
end;

function TJclIntfVector.GetObject(Index: Integer): IInterface;
begin
  if (Index < 0) or (Index >= FCount) then
  begin
    Result := nil;
    Exit;
  end;
  Result := Items[Index];
end;

procedure TJclIntfVector.Grow;
begin
  if FCapacity > 64 then
    FCapacity := FCapacity + FCapacity div 4
  else
  if FCapacity = 0 then
    FCapacity := 64
  else
    FCapacity := FCapacity * 4;
  SetLength(FItems, FCapacity);
end;

function TJclIntfVector.IndexOf(const AInterface: IInterface): Integer;
var
  I: Integer;
begin
  Result := -1;
  if AInterface = nil then
    Exit;
  for I := 0 to FCount - 1 do
    if Items[I] = AInterface then
    begin
      Result := I;
      Break;
    end;
end;

function TJclIntfVector.First: IJclIntfIterator;
begin
  Result := TIntfItr.Create(Self);
end;

function TJclIntfVector.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclIntfVector.Last: IJclIntfIterator;
var
  NewIterator: TIntfItr;
begin
  NewIterator := TIntfItr.Create(Self);
  NewIterator.FCursor := NewIterator.FOwnList.FCount;
  NewIterator.FSize := NewIterator.FOwnList.FCount;
  Result := NewIterator;
end;

function TJclIntfVector.LastIndexOf(const AInterface: IInterface): Integer;
var
  I: Integer;
begin
  Result := -1;
  if AInterface = nil then
    Exit;
  for I := FCount - 1 downto 0 do
    if Items[I] = AInterface then
    begin
      Result := I;
      Break;
    end;
end;

function TJclIntfVector.Remove(Index: Integer): IInterface;
begin
  if (Index < 0) or (Index >= FCount) then
    {$IFDEF CLR}
    raise EJclOutOfBoundsError.Create(RsEOutOfBounds);
    {$ELSE}
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
    {$ENDIF CLR}
  Result := FItems[Index];
  FItems[Index] := nil;
  MoveArray(FItems, Index + 1, Index, FCount - Index);
  Dec(FCount);
end;

function TJclIntfVector.Remove(const AInterface: IInterface): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AInterface = nil then
    Exit;
  for I := FCount - 1 downto 0 do
    if FItems[I] = AInterface then // Removes all AInterface
    begin
      FItems[I] := nil; // Force Release
      MoveArray(FItems, I + 1, I, FCount - I);
      Dec(FCount);
      Result := True;
    end;
end;

function TJclIntfVector.RemoveAll(const ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Remove(It.Next);
end;

function TJclIntfVector.RetainAll(const ACollection: IJclIntfCollection): Boolean;
var
  I: Integer;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  for I := FCount - 1 downto 0 do
    if not ACollection.Contains(Items[I]) then
      Remove(I);
end;

procedure TJclIntfVector.SetObject(Index: Integer; const AInterface: IInterface);
begin
  if (Index < 0) or (Index >= FCount) then
    {$IFDEF CLR}
    raise EJclOutOfBoundsError.Create(RsEOutOfBounds);
    {$ELSE}
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
    {$ENDIF CLR}
  FItems[Index] := AInterface;
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
  Last := First + Count - 1;
  if Last >= FCount then
    Last := FCount - 1;
  Result := TJclIntfVector.Create(Count);
  for I := First to Last do
    Result.Add(Items[I]);
end;

{$IFNDEF CLR}
procedure TJclIntfVector.AfterConstruction;
begin
end;

procedure TJclIntfVector.BeforeDestruction;
begin
end;
{$ENDIF ~CLR}

//=== { TJclStrVector } ======================================================

constructor TJclStrVector.Create(ACapacity: Integer = DefaultContainerCapacity);
begin
  inherited Create;
  FCount := 0;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FItems, FCapacity);
end;

destructor TJclStrVector.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclStrVector.Insert(Index: Integer; const AString: string);
begin
  if (Index < 0) or (Index > FCount) then
    {$IFDEF CLR}
    raise EJclOutOfBoundsError.Create(RsEOutOfBounds);
    {$ELSE}
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
    {$ENDIF CLR}
  if FCount = FCapacity then
    Grow;
  MoveArray(FItems, Index, Index + 1, FCount - Index);
  FItems[Index] := AString;
  Inc(FCount);
end;

function TJclStrVector.Add(const AString: string): Boolean;
begin
  if FCount = FCapacity then
    Grow;
  FItems[FCount] := AString;
  Inc(FCount);
  Result := True;
end;

function TJclStrVector.AddAll(const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Add(It.Next);
  Result := True;
end;

function TJclStrVector.InsertAll(Index: Integer;
  const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
  Size: Integer;
begin
  Result := False;
  if (Index < 0) or (Index >= FCount) then
    {$IFDEF CLR}
    raise EJclOutOfBoundsError.Create(RsEOutOfBounds);
    {$ELSE}
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
    {$ENDIF CLR}
  if ACollection = nil then
    Exit;
  Size := ACollection.Size;
  if Size <> 0 then
  begin
    Inc(FCapacity, Size);
    SetLength(FItems, FCapacity);
    Inc(FCount, Size);
    MoveArray(FItems, Index, Index + Size, Size);
    It := ACollection.First;
    while It.HasNext do
    begin
      FItems[Index] := It.Next;
      Inc(Index);
    end;
  end;
  Result := True;
end;

{$IFNDEF CLR}
procedure TJclStrVector.AfterConstruction;
begin
end;

procedure TJclStrVector.BeforeDestruction;
begin
end;
{$ENDIF ~CLR}

procedure TJclStrVector.Clear;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FItems[I] := '';
  FCount := 0;
end;

function TJclStrVector.Clone: TObject;
var
  NewList: TJclStrVector;
begin
  NewList := TJclStrVector.Create(FCapacity);
  NewList.AddAll(Self);
  Result := NewList;
end;

function TJclStrVector.Contains(const AString: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AString = '' then
    Exit;
  for I := 0 to FCount - 1 do
    if Items[I] = AString then
    begin
      Result := True;
      Break;
    end;
end;

function TJclStrVector.ContainsAll(const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
begin
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
    Result := Contains(It.Next);
end;

function TJclStrVector.Equals(const ACollection: IJclStrCollection): Boolean;
var
  I: Integer;
  It: IJclStrIterator;
begin
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
end;

function TJclStrVector.First: IJclStrIterator;
begin
  Result := TStrItr.Create(Self);
end;

function TJclStrVector.GetString(Index: Integer): string;
begin
  if (Index < 0) or (Index >= FCount) then
  begin
    Result := '';
    Exit;
  end;
  Result := FItems[Index];
end;

procedure TJclStrVector.Grow;
begin
  if FCapacity > 64 then
    FCapacity := FCapacity + FCapacity div 4
  else
  if FCapacity = 0 then
    FCapacity := 64
  else
    FCapacity := FCapacity * 4;
  SetLength(FItems, FCapacity);
end;

function TJclStrVector.IndexOf(const AString: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  if AString = '' then
    Exit;
  for I := 0 to FCount - 1 do
    if Items[I] = AString then
    begin
      Result := I;
      Exit;
    end;
end;

function TJclStrVector.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclStrVector.Last: IJclStrIterator;
var
  NewIterator: TStrItr;
begin
  NewIterator := TStrItr.Create(Self);
  NewIterator.FCursor := NewIterator.FOwnList.FCount;
  NewIterator.FSize := NewIterator.FOwnList.FCount;
  Result := NewIterator;
end;

function TJclStrVector.LastIndexOf(const AString: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  if AString = '' then
    Exit;
  for I := FCount - 1 downto 0 do
    if Items[I] = AString then
    begin
      Result := I;
      Break;
    end;
end;

function TJclStrVector.Remove(const AString: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AString = '' then
    Exit;
  for I := FCount - 1 downto 0 do
    if FItems[I] = AString then // Removes all AString
    begin
      FItems[I] := ''; // Force Release
      MoveArray(FItems, I + 1, I, FCount - I);
      Dec(FCount);
      Result := True;
    end;
end;

function TJclStrVector.Remove(Index: Integer): string;
begin
  if (Index < 0) or (Index >= FCount) then
    {$IFDEF CLR}
    raise EJclOutOfBoundsError.Create(RsEOutOfBounds);
    {$ELSE}
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
    {$ENDIF CLR}
  Result := FItems[Index];
  FItems[Index] := '';
  MoveArray(FItems, Index + 1, Index, FCount - Index);
  Dec(FCount);
end;

function TJclStrVector.RemoveAll(const ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Remove(It.Next);
end;

function TJclStrVector.RetainAll(const ACollection: IJclStrCollection): Boolean;
var
  I: Integer;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  for I := FCount - 1 downto 0 do
    if not ACollection.Contains(Items[I]) then
      Remove(I);
end;

procedure TJclStrVector.SetString(Index: Integer; const AString: string);
begin
  if (Index < 0) or (Index >= FCount) then
    {$IFDEF CLR}
    raise EJclOutOfBoundsError.Create(RsEOutOfBounds);
    {$ELSE}
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
    {$ENDIF CLR}
  FItems[Index] := AString;
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
  Last := First + Count - 1;
  if Last >= FCount then
    Last := FCount - 1;
  Result := TJclStrVector.Create(Count);
  for I := First to Last do
    Result.Add(Items[I]);
end;

//=== { TJclVector } =========================================================

constructor TJclVector.Create(ACapacity: Integer = DefaultContainerCapacity;
  AOwnsObjects: Boolean = True);
begin
  inherited Create;
  FCount := 0;
  FOwnsObjects := AOwnsObjects;
  if ACapacity < 0 then
    FCapacity := 0
  else
    FCapacity := ACapacity;
  SetLength(FItems, FCapacity);
end;

destructor TJclVector.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TJclVector.Insert(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index > FCount) then
    {$IFDEF CLR}
    raise EJclOutOfBoundsError.Create(RsEOutOfBounds);
    {$ELSE}
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
    {$ENDIF CLR}
  if FCount = FCapacity then
    Grow;
  MoveArray(FItems, Index, Index + 1, FCount - Index);
  FItems[Index] := AObject;
  Inc(FCount);
end;

function TJclVector.Add(AObject: TObject): Boolean;
begin
  if FCount = FCapacity then
    Grow;
  FItems[FCount] := AObject;
  Inc(FCount);
  Result := True;
end;

function TJclVector.InsertAll(Index: Integer;
  const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  Size: Integer;
begin
  Result := False;
  if (Index < 0) or (Index >= FCount) then
    {$IFDEF CLR}
    raise EJclOutOfBoundsError.Create(RsEOutOfBounds);
    {$ELSE}
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
    {$ENDIF CLR}
  if ACollection = nil then
    Exit;
  Size := ACollection.Size;
  if Size <> 0 then
  begin
    Inc(FCapacity, Size);
    SetLength(FItems, FCapacity);
    Inc(FCount, Size);
    MoveArray(FItems, Index, Index + Size, Size);
    It := ACollection.First;
    while It.HasNext do
    begin
      FItems[Index] := It.Next;
      Inc(Index);
    end;
  end;
  Result := True;
end;

function TJclVector.AddAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Add(It.Next);
  Result := True;
end;

procedure TJclVector.Clear;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    FreeObject(FItems[I]);
  FCount := 0;
end;

function TJclVector.Clone: TObject;
var
  NewList: TJclVector;
begin
  NewList := TJclVector.Create(FCapacity, False); // Only one can have FOwnsObject = True
  NewList.AddAll(Self);
  Result := NewList;
end;

function TJclVector.Contains(AObject: TObject): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AObject = nil then
    Exit;
  for I := 0 to FCount - 1 do
    if Items[I] = AObject then
    begin
      Result := True;
      Break;
    end;
end;

function TJclVector.ContainsAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while Result and It.HasNext do
    Result := Contains(It.Next);
end;

function TJclVector.Equals(const ACollection: IJclCollection): Boolean;
var
  I: Integer;
  It: IJclIterator;
begin
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
end;

procedure TJclVector.FreeObject(var AObject: TObject);
begin
  if FOwnsObjects then
  begin
    AObject.Free;
    AObject := nil;
  end;
end;

function TJclVector.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
  begin
    Result := nil;
    Exit;
  end;
  Result := Items[Index];
end;

procedure TJclVector.Grow;
begin
  if FCapacity > 64 then
    FCapacity := FCapacity + FCapacity div 4
  else
  if FCapacity = 0 then
    FCapacity := 64
  else
    FCapacity := FCapacity * 4;
  SetLength(FItems, FCapacity);
end;

function TJclVector.IndexOf(AObject: TObject): Integer;
var
  I: Integer;
begin
  Result := -1;
  if AObject = nil then
    Exit;
  for I := 0 to FCount - 1 do
    if Items[I] = AObject then
    begin
      Result := I;
      Break;
    end;
end;

function TJclVector.First: IJclIterator;
begin
  Result := TItr.Create(Self);
end;

function TJclVector.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TJclVector.Last: IJclIterator;
var
  NewIterator: TItr;
begin
  NewIterator := TItr.Create(Self);
  NewIterator.FCursor := NewIterator.FOwnList.FCount;
  NewIterator.FSize := NewIterator.FOwnList.FCount;
  Result := NewIterator;
end;

function TJclVector.LastIndexOf(AObject: TObject): Integer;
var
  I: Integer;
begin
  Result := -1;
  if AObject = nil then
    Exit;
  for I := FCount - 1 downto 0 do
    if Items[I] = AObject then
    begin
      Result := I;
      Break;
    end;
end;

function TJclVector.Remove(AObject: TObject): Boolean;
var
  I: Integer;
begin
  Result := False;
  if AObject = nil then
    Exit;
  for I := FCount - 1 downto 0 do
    if FItems[I] = AObject then // Removes all AObject
    begin
      FreeObject(FItems[I]);
      MoveArray(FItems, I + 1, I, FCount - I);
      Dec(FCount);
      Result := True;
    end;
end;

function TJclVector.Remove(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    {$IFDEF CLR}
    raise EJclOutOfBoundsError.Create(RsEOutOfBounds);
    {$ELSE}
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
    {$ENDIF CLR}
  Result := FItems[Index];
  FreeObject(FItems[Index]);
  MoveArray(FItems, Index + 1, Index, FCount - Index);
  Dec(FCount);
end;

function TJclVector.RemoveAll(const ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Remove(It.Next);
end;

function TJclVector.RetainAll(const ACollection: IJclCollection): Boolean;
var
  I: Integer;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  for I := FCount - 1 to 0 do
    if not ACollection.Contains(Items[I]) then
      Remove(I);
end;

procedure TJclVector.SetObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= FCount) then
    {$IFDEF CLR}
    raise EJclOutOfBoundsError.Create(RsEOutOfBounds);
    {$ELSE}
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
    {$ENDIF CLR}
  FItems[Index] := AObject;
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
  Last := First + Count - 1;
  if Last >= FCount then
    Last := FCount - 1;
  Result := TJclVector.Create(Count, FOwnsObjects);
  for I := First to Last do
    Result.Add(Items[I]);
end;

{$IFNDEF CLR}

procedure TJclVector.AfterConstruction;
begin
end;

procedure TJclVector.BeforeDestruction;
begin
end;

{$ENDIF ~CLR}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

