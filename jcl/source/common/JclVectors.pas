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
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclVectors;

{$I jcl.inc}

interface

uses
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
    function Add(AInterface: IInterface): Boolean; overload;
    function AddAll(ACollection: IJclIntfCollection): Boolean; overload;
    procedure Clear;
    function Contains(AInterface: IInterface): Boolean;
    function ContainsAll(ACollection: IJclIntfCollection): Boolean;
    function Equals(ACollection: IJclIntfCollection): Boolean;
    function First: IJclIntfIterator;
    function IsEmpty: Boolean;
    function Last: IJclIntfIterator;
    function Remove(AInterface: IInterface): Boolean; overload;
    function RemoveAll(ACollection: IJclIntfCollection): Boolean;
    function RetainAll(ACollection: IJclIntfCollection): Boolean;
    function Size: Integer;
    { IJclIntfList }
    procedure Insert(Index: Integer; AInterface: IInterface); overload;
    function InsertAll(Index: Integer; ACollection: IJclIntfCollection): Boolean; overload;
    function GetObject(Index: Integer): IInterface;
    function IndexOf(AInterface: IInterface): Integer;
    function LastIndexOf(AInterface: IInterface): Integer;
    function Remove(Index: Integer): IInterface; overload;
    procedure SetObject(Index: Integer; AInterface: IInterface);
    function SubList(First, Count: Integer): IJclIntfList;

    constructor Create(ACapacity: Integer = DefaultContainerCapacity);
    destructor Destroy; override;
    procedure AfterConstruction; override;
    // Do not decrement RefCount because iterator inc/dec it.
    procedure BeforeDestruction; override;
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
  public
    { IJclStrCollection }
    function Add(const AString: string): Boolean; overload; override;
    function AddAll(ACollection: IJclStrCollection): Boolean; overload; override;
    procedure Clear; override;
    function Contains(const AString: string): Boolean; override;
    function ContainsAll(ACollection: IJclStrCollection): Boolean; override;
    function Equals(ACollection: IJclStrCollection): Boolean; override;
    function First: IJclStrIterator; override;
    function IsEmpty: Boolean; override;
    function Last: IJclStrIterator; override;
    function Remove(const AString: string): Boolean; overload; override;
    function RemoveAll(ACollection: IJclStrCollection): Boolean; override;
    function RetainAll(ACollection: IJclStrCollection): Boolean; override;
    function Size: Integer; override;
    { IJclStrList }
    procedure Insert(Index: Integer; const AString: string); overload;
    function InsertAll(Index: Integer; ACollection: IJclStrCollection): Boolean; overload;
    function GetString(Index: Integer): string;
    function IndexOf(const AString: string): Integer;
    function LastIndexOf(const AString: string): Integer;
    function Remove(Index: Integer): string; overload;
    procedure SetString(Index: Integer; const AString: string);
    function SubList(First, Count: Integer): IJclStrList;

    constructor Create(ACapacity: Integer = DefaultContainerCapacity);
    destructor Destroy; override;
    procedure AfterConstruction; override;
    // Do not decrement RefCount because iterator inc/dec it.
    procedure BeforeDestruction; override;
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
  public
    { IJclCollection }
    function Add(AObject: TObject): Boolean; overload;
    function AddAll(ACollection: IJclCollection): Boolean; overload;
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function ContainsAll(ACollection: IJclCollection): Boolean;
    function Equals(ACollection: IJclCollection): Boolean;
    function First: IJclIterator;
    function IsEmpty: Boolean;
    function Last: IJclIterator;
    function Remove(AObject: TObject): Boolean; overload;
    function RemoveAll(ACollection: IJclCollection): Boolean;
    function RetainAll(ACollection: IJclCollection): Boolean;
    function Size: Integer;
    { IJclList }
    procedure Insert(Index: Integer; AObject: TObject); overload;
    function InsertAll(Index: Integer; ACollection: IJclCollection): Boolean; overload;
    function GetObject(Index: Integer): TObject;
    function IndexOf(AObject: TObject): Integer;
    function LastIndexOf(AObject: TObject): Integer;
    function Remove(Index: Integer): TObject; overload;
    procedure SetObject(Index: Integer; AObject: TObject);
    function SubList(First, Count: Integer): IJclList;
    { IJclCloneable }
    function Clone: TObject;

    constructor Create(ACapacity: Integer = DefaultContainerCapacity; AOwnsObjects: Boolean = True);
    destructor Destroy; override;
    procedure AfterConstruction; override;
    // Do not decrement RefCount because iterator inc/dec it.
    procedure BeforeDestruction; override;
    property Items: TDynObjectArray read FItems;
    property OwnsObjects: Boolean read FOwnsObjects;
  end;

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
    procedure Add(AInterface: IInterface);
    function GetObject: IInterface;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Next: IInterface;
    function NextIndex: Integer;
    function Previous: IInterface;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetObject(AInterface: IInterface);
  public
    constructor Create(OwnList: TJclIntfVector);
    destructor Destroy; override;
  end;

constructor TIntfItr.Create(OwnList: TJclIntfVector);
begin
  inherited Create;
  FCursor := 0;
  FOwnList := OwnList;
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  FLastRet := -1;
  FSize := FOwnList.Size;
end;

destructor TIntfItr.Destroy;
begin
  FOwnList._Release;
  inherited Destroy;
end;

procedure TIntfItr.Add(AInterface: IInterface);
begin
  with FOwnList do
  begin
    System.Move(FItems[FCursor], FItems[FCursor + 1],
      (FCount - FCursor) * SizeOf(TObject));
    FCapacity := Length(FItems);
    FItems[FCursor] := AInterface;
    Inc(FCount);
  end;
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
    System.Move(FItems[FCursor + 1], FItems[FCursor],
      (FSize - FCursor) * SizeOf(IInterface));
  end;
  Dec(FOwnList.FCount);
  Dec(FSize);
end;

procedure TIntfItr.SetObject(AInterface: IInterface);
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
    destructor Destroy; override;
  end;

constructor TStrItr.Create(OwnList: TJclStrVector);
begin
  inherited Create;
  FCursor := 0;
  FOwnList := OwnList;
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  FLastRet := -1;
  FSize := FOwnList.Size;
end;

destructor TStrItr.Destroy;
begin
  FOwnList._Release;
  inherited Destroy;
end;

procedure TStrItr.Add(const AString: string);
begin
  with FOwnList do
  begin
    System.Move(FItems[FCursor], FItems[FCursor + 1],
      (FOwnList.FCount - FCursor) * SizeOf(string));
    FCapacity := Length(FItems);
    FItems[FCursor] := AString;
    Inc(FOwnList.FCount);
  end;
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
    System.Move(FItems[FCursor + 1], FItems[FCursor],
      (FSize - FCursor) * SizeOf(string));
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
    destructor Destroy; override;
  end;

constructor TItr.Create(OwnList: TJclVector);
begin
  inherited Create;
  FCursor := 0;
  FOwnList := OwnList;
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  FLastRet := -1;
  FSize := FOwnList.Size;
end;

destructor TItr.Destroy;
begin
  FOwnList._Release;
  inherited Destroy;
end;

procedure TItr.Add(AObject: TObject);
begin
  with FOwnList do
  begin
    System.Move(FItems[FCursor], FItems[FCursor + 1],
      (FCount - FCursor) * SizeOf(TObject));
    FCapacity := Length(FItems);
    FItems[FCursor] := AObject;
    Inc(FCount);
  end;
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
    System.Move(FItems[FCursor + 1], FItems[FCursor],
      (FSize - FCursor) * SizeOf(TObject));
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

procedure TJclIntfVector.Insert(Index: Integer; AInterface: IInterface);
begin
  if (Index < 0) or (Index > FCount) then
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
  System.Move(FItems[Index], FItems[Index - 1],
    (FCount - Index) * SizeOf(IInterface));
  FCapacity := Length(FItems);
  FItems[Index] := AInterface;
  Inc(FCount);
end;

function TJclIntfVector.Add(AInterface: IInterface): Boolean;
begin
  if FCount = FCapacity then
    Grow;
  FItems[FCount] := AInterface;
  Inc(FCount);
  Result := True;
end;

function TJclIntfVector.InsertAll(Index: Integer; ACollection: IJclIntfCollection): Boolean;
var
  It: IJclIntfIterator;
  Size: Integer;
begin
  Result := False;
  if (Index < 0) or (Index >= FCount) then
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
  if ACollection = nil then
    Exit;
  Size := ACollection.Size;
  System.Move(FItems[Index], FItems[Index + Size], Size * SizeOf(IInterface));
  It := ACollection.First;
  while It.HasNext do
  begin
    FItems[Index] := It.Next;
    Inc(Index);
  end;
  Result := True;
end;

function TJclIntfVector.AddAll(ACollection: IJclIntfCollection): Boolean;
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

function TJclIntfVector.Contains(AInterface: IInterface): Boolean;
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

function TJclIntfVector.ContainsAll(ACollection: IJclIntfCollection): Boolean;
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

function TJclIntfVector.Equals(ACollection: IJclIntfCollection): Boolean;
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

function TJclIntfVector.IndexOf(AInterface: IInterface): Integer;
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

function TJclIntfVector.LastIndexOf(AInterface: IInterface): Integer;
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
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
  Result := FItems[Index];
  FItems[Index] := nil;
  System.Move(FItems[Index + 1], FItems[Index],
    (FCount - Index) * SizeOf(IInterface));
  Dec(FCount);
end;

function TJclIntfVector.Remove(AInterface: IInterface): Boolean;
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
      System.Move(FItems[I + 1], FItems[I], (FCount - I) * SizeOf(IInterface));
      Dec(FCount);
      Result := True;
    end;
end;

function TJclIntfVector.RemoveAll(ACollection: IJclIntfCollection): Boolean;
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

function TJclIntfVector.RetainAll(ACollection: IJclIntfCollection): Boolean;
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

procedure TJclIntfVector.SetObject(Index: Integer; AInterface: IInterface);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
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

procedure TJclIntfVector.AfterConstruction;
begin
end;

procedure TJclIntfVector.BeforeDestruction;
begin
end;

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
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
  System.Move(FItems[Index], FItems[Index - 1], (FCount - Index) * SizeOf(string));
  FCapacity := Length(FItems);
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

function TJclStrVector.AddAll(ACollection: IJclStrCollection): Boolean;
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

function TJclStrVector.InsertAll(Index: Integer; ACollection: IJclStrCollection): Boolean;
var
  It: IJclStrIterator;
  Size: Integer;
begin
  Result := False;
  if (Index < 0) or (Index >= FCount) then
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
  if ACollection = nil then
    Exit;
  Size := ACollection.Size;
  System.Move(FItems[Index], FItems[Index + Size], Size * SizeOf(string));
  It := ACollection.First;
  while It.HasNext do
  begin
    FItems[Index] := It.Next;
    Inc(Index);
  end;
  Result := True;
end;

procedure TJclStrVector.AfterConstruction;
begin
end;

procedure TJclStrVector.BeforeDestruction;
begin
end;

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

function TJclStrVector.ContainsAll(ACollection: IJclStrCollection): Boolean;
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

function TJclStrVector.Equals(ACollection: IJclStrCollection): Boolean;
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
      System.Move(FItems[I + 1], FItems[I], (FCount - I) * SizeOf(string));
      Dec(FCount);
      Result := True;
    end;
end;

function TJclStrVector.Remove(Index: Integer): string;
begin
  if (Index < 0) or (Index >= FCount) then
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
  Result := FItems[Index];
  FItems[Index] := '';
  System.Move(FItems[Index + 1], FItems[Index], (FCount - Index) * SizeOf(string));
  Dec(FCount);
end;

function TJclStrVector.RemoveAll(ACollection: IJclStrCollection): Boolean;
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

function TJclStrVector.RetainAll(ACollection: IJclStrCollection): Boolean;
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
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
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
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
  System.Move(FItems[Index], FItems[Index - 1],
    (FCount - Index) * SizeOf(TObject));
  FCapacity := Length(FItems);
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

function TJclVector.InsertAll(Index: Integer; ACollection: IJclCollection): Boolean;
var
  It: IJclIterator;
  Size: Integer;
begin
  Result := False;
  if (Index < 0) or (Index >= FCount) then
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
  if ACollection = nil then
    Exit;
  Size := ACollection.Size;
  System.Move(FItems[Index], FItems[Index + Size], Size * SizeOf(IInterface));
  It := ACollection.First;
  while It.HasNext do
  begin
    FItems[Index] := It.Next;
    Inc(Index);
  end;
  Result := True;
end;

function TJclVector.AddAll(ACollection: IJclCollection): Boolean;
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

function TJclVector.ContainsAll(ACollection: IJclCollection): Boolean;
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

function TJclVector.Equals(ACollection: IJclCollection): Boolean;
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
      System.Move(FItems[I + 1], FItems[I], (FCount - I) * SizeOf(TObject));
      Dec(FCount);
      Result := True;
    end;
end;

function TJclVector.Remove(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
  Result := FItems[Index];
  FreeObject(FItems[Index]);
  System.Move(FItems[Index + 1], FItems[Index], (FCount - Index) * SizeOf(TObject));
  Dec(FCount);
end;

function TJclVector.RemoveAll(ACollection: IJclCollection): Boolean;
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

function TJclVector.RetainAll(ACollection: IJclCollection): Boolean;
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
    raise EJclOutOfBoundsError.CreateRes(@RsEOutOfBounds);
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

procedure TJclVector.AfterConstruction;
begin
end;

procedure TJclVector.BeforeDestruction;
begin
end;

// History:

// $Log$
// Revision 1.8  2005/03/08 08:33:18  marquardt
// overhaul of exceptions and resourcestrings, minor style cleaning
//
// Revision 1.7  2005/03/03 08:02:57  marquardt
// various style cleanings, bugfixes and improvements
//
// Revision 1.6  2005/03/02 17:51:24  rrossmair
// - removed DCLAppendDelimited from JclAlgorithms, changed uses clauses accordingly
//
// Revision 1.5  2005/03/02 09:59:30  dade2004
// Added
//  -TJclStrCollection in JclContainerIntf
//        Every common methods for IJclStrCollection are implemented here
//
// -Every class that implement IJclStrCollection now derive from  TJclStrCollection instead of TJclAbstractContainer
// -Every abstract method in TJclStrCollection has been marked as "override" in descendent classes
//
// DCLAppendDelimited has been removed from JclAlgorothms, his body has been fixed for a bug and put into
// relative method in TJclStrCollection
//
// Revision 1.4  2005/02/27 11:36:20  marquardt
// fixed and secured Capacity/Grow mechanism, raise exceptions with efficient CreateResRec
//
// Revision 1.3  2005/02/27 07:27:47  marquardt
// changed interface names from I to IJcl, moved resourcestrings to JclResource.pas
//
// Revision 1.2  2005/02/24 07:36:24  marquardt
// resolved the compiler warnings, style cleanup, removed code from JclContainerIntf.pas
//
// Revision 1.1  2005/02/24 03:57:10  rrossmair
// - donated DCL code, initial check-in
//

end.

