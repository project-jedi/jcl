//------------------------------------------------------------------------------
// The Delphi Container Library
// Jean-Philippe BEMPEL aka RDM
// rdm_30@yahoo.com
//------------------------------------------------------------------------------
unit Vector;

{$I dcl.inc}

interface

uses DCL_intf, DCLUtil, AbstractContainer;

type
  TIntfVector = class(TAbstractContainer, IIntfCollection, IIntfList,
  	IIntfArray, IIntfCloneable)
  private
    FCount: Integer;
    FCapacity: Integer;
  protected
    procedure Grow; virtual;
  public
  { IIntfCollection }
    function Add(AObject: IInterface): Boolean; overload;
    function AddAll(ACollection: IIntfCollection): Boolean; overload;
    procedure Clear;
    function Contains(AObject: IInterface): Boolean;
    function ContainsAll(ACollection: IIntfCollection): Boolean;
    function Equals(ACollection: IIntfCollection): Boolean;
    function First: IIntfIterator;
    function IsEmpty: Boolean;
    function Last: IIntfIterator;
    function Remove(AObject: IInterface): Boolean; overload;
    function RemoveAll(ACollection: IIntfCollection): Boolean;
    function RetainAll(ACollection: IIntfCollection): Boolean;
    function Size: Integer;
  public
  { IIntfList }
    procedure Add(Index: Integer; AObject: IInterface); overload;
    function AddAll(Index: Integer; ACollection: IIntfCollection): Boolean; overload;
    function GetObject(Index: Integer): IInterface;
    function IndexOf(AObject: IInterface): Integer;
    function LastIndexOf(AObject: IInterface): Integer;
    function Remove(Index: Integer): IInterface; overload;
    procedure SetObject(Index: Integer; AObject: IInterface);
    function SubList(First, Count: Integer): IIntfList;
  protected
  { ICloneable }
    function Clone: IInterface;
  public
    Items: TIInterfaceArray;
  public
    constructor Create; overload;
    constructor Create(Capacity: Integer); overload;
    destructor Destroy; override;
    procedure AfterConstruction; override; // Do not decrement RefCount because iterator inc/dec it.
    procedure BeforeDestruction; override;
  end;

  TStrVector = class(TAbstractContainer, IStrCollection, IStrList,
  	IStrArray, ICloneable)
  private
    FCount: Integer;
    FCapacity: Integer;
  protected
    procedure Grow; virtual;
  public
  { IStrCollection }
    function Add(const AString: string): Boolean; overload;
    function AddAll(ACollection: IStrCollection): Boolean; overload;
    procedure Clear;
    function Contains(const AString: string): Boolean;
    function ContainsAll(ACollection: IStrCollection): Boolean;
    function Equals(ACollection: IStrCollection): Boolean;
    function First: IStrIterator;
    function IsEmpty: Boolean;
    function Last: IStrIterator;
    function Remove(const AString: string): Boolean; overload;
    function RemoveAll(ACollection: IStrCollection): Boolean;
    function RetainAll(ACollection: IStrCollection): Boolean;
    function Size: Integer;
  public
  { IStrList }
    procedure Add(Index: Integer; const AString: string); overload;
    function AddAll(Index: Integer; ACollection: IStrCollection): Boolean; overload;
    function GetString(Index: Integer): string;
    function IndexOf(const AString: string): Integer;
    function LastIndexOf(const AString: string): Integer;
    function Remove(Index: Integer): string; overload;
    procedure SetString(Index: Integer; const AString: string);
    function SubList(First, Count: Integer): IStrList;
  protected
  { ICloneable }
    function Clone: TObject;
  public
    Items: TStringArray;
  public
    constructor Create; overload;
    constructor Create(Capacity: Integer); overload;
    destructor Destroy; override;
    procedure AfterConstruction; override; // Do not decrement RefCount because iterator inc/dec it.
    procedure BeforeDestruction; override;
  end;

  TVector = class(TAbstractContainer, ICollection, IList, IArray, ICloneable)
  private
    FCount: Integer;
    FCapacity: Integer;
    FOwnsObjects: Boolean;
  protected
    procedure Grow; virtual;
    procedure FreeObject(AObject: TObject);
  public
  { ICollection }
    function Add(AObject: TObject): Boolean; overload;
    function AddAll(ACollection: ICollection): Boolean; overload;
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function ContainsAll(ACollection: ICollection): Boolean;
    function Equals(ACollection: ICollection): Boolean;
    function First: IIterator;
    function IsEmpty: Boolean;
    function Last: IIterator;
    function Remove(AObject: TObject): Boolean; overload;
    function RemoveAll(ACollection: ICollection): Boolean;
    function RetainAll(ACollection: ICollection): Boolean;
    function Size: Integer;
  public
  { IList }
    procedure Add(Index: Integer; AObject: TObject); overload;
    function AddAll(Index: Integer; ACollection: ICollection): Boolean; overload;
    function GetObject(Index: Integer): TObject;
    function IndexOf(AObject: TObject): Integer;
    function LastIndexOf(AObject: TObject): Integer;
    function Remove(Index: Integer): TObject; overload;
    procedure SetObject(Index: Integer; AObject: TObject);
    function SubList(First, Count: Integer): IList;
  public
  { ICloneable }
    function Clone: TObject;
  public
    Items: TObjectArray;
  public
    constructor Create; overload;
    constructor Create(Capacity: Integer; AOwnsObjects: Boolean); overload;
    destructor Destroy; override;
    procedure AfterConstruction; override; // Do not decrement RefCount because iterator inc/dec it.
    procedure BeforeDestruction; override;
  end;

implementation

type
  TIntfItr = class(TAbstractContainer, IIntfIterator)
  private
    FCursor: Integer;
    FOwnList: TIntfVector;
    FLastRet: Integer;
    FSize: Integer;
  protected
  { IIntfIterator}
    procedure Add(AObject: IInterface);
    function GetObject: IInterface;
    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Next: IInterface;
    function NextIndex: Integer;
    function Previous: IInterface;
    function PreviousIndex: Integer;
    procedure Remove;
    procedure SetObject(AObject: IInterface);
  public
    constructor Create(OwnList: TIntfVector);
    destructor Destroy; override;
  end;

  TStrItr = class(TAbstractContainer, IStrIterator)
  private
    FCursor: Integer;
    FOwnList: TStrVector;
    FLastRet: Integer;
    FSize: Integer;
  protected
  { IStrIterator}
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
    constructor Create(OwnList: TStrVector);
    destructor Destroy; override;
  end;

  TItr = class(TAbstractContainer, IIterator)
  private
    FCursor: Integer;
    FOwnList: TVector;
    FLastRet: Integer;
    FSize: Integer;
  protected
  { IIterator}
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
    constructor Create(OwnList: TVector);
    destructor Destroy; override;
  end;

{ TIntfItr }

procedure TIntfItr.Add(AObject: IInterface);
begin
  with FOwnList do
  begin
    System.Move(Items[FCursor], Items[FCursor + 1],
      (FCount - FCursor) * SizeOf(TObject));
    FCapacity := Length(Items);
    Items[FCursor] := AObject;
    Inc(FCount);
  end;
  Inc(FSize);
  Inc(FCursor);
  FLastRet := -1;
end;

constructor TIntfItr.Create(OwnList: TIntfVector);
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
  inherited;
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
	  Items[FCursor] := nil; // Force Release
    System.Move(Items[FCursor + 1],	Items[FCursor],
			(FSize - FCursor) * SizeOf(IInterface));
  end;
  Dec(FOwnList.FCount);
  Dec(FSize);
end;

procedure TIntfItr.SetObject(AObject: IInterface);
begin
  FOwnList.Items[FCursor] := AObject;
end;

{ TStrItr }

procedure TStrItr.Add(const AString: string);
begin
  with FOwnList do
  begin
    System.Move(Items[FCursor], Items[FCursor + 1],
      (FOwnList.FCount - FCursor) * SizeOf(string));
    FCapacity := Length(Items);
    Items[FCursor] := AString;
    Inc(FOwnList.FCount);
  end;
  Inc(FSize);
  Inc(FCursor);
  FLastRet := -1;
end;

constructor TStrItr.Create(OwnList: TStrVector);
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
  inherited;
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
	  Items[FCursor] := '';// Force Release
    System.Move(Items[FCursor + 1], Items[FCursor],
			(FSize - FCursor) * SizeOf(string));
  end;
  Dec(FOwnList.FCount);
  Dec(FSize);
end;

procedure TStrItr.SetString(const AString: string);
begin
{
  if FLastRet = -1 then
    raise EDCLIllegalState.Create(SIllegalState);
  }
  FOwnList.Items[FCursor] := AString;
end;

{ TItr }

procedure TItr.Add(AObject: TObject);
begin
  with FOwnList do
  begin
    System.Move(Items[FCursor], Items[FCursor + 1],
      (FCount - FCursor) * SizeOf(TObject));
    FCapacity := Length(Items);
    Items[FCursor] := AObject;
    Inc(FCount);
  end;
  Inc(FSize);
  Inc(FCursor);
  FLastRet := -1;
end;

constructor TItr.Create(OwnList: TVector);
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
  inherited;
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
	  FreeObject(Items[FCursor]);
    System.Move(Items[FCursor + 1],	Items[FCursor],
			(FSize - FCursor) * SizeOf(TObject));
  end;
  Dec(FOwnList.FCount);
  Dec(FSize);
end;

procedure TItr.SetObject(AObject: TObject);
begin
{
  if FLastRet = -1 then
    raise EDCLIllegalState.Create(SIllegalState);
}
  FOwnList.Items[FCursor] := AObject;
end;

{ TIntfVector }

procedure TIntfVector.Add(Index: Integer; AObject: IInterface);
begin
  System.Move(Items[Index], Items[Index - 1],
  	(FCount - Index) * SizeOf(IInterface));
  FCapacity := Length(Items);
  Items[Index] := AObject;
  Inc(FCount);
end;

function TIntfVector.Add(AObject: IInterface): Boolean;
begin
	if FCount = FCapacity then
  	Grow;
  Items[FCount] := AObject;
  Inc(FCount);
  Result := True;
end;

function TIntfVector.AddAll(Index: Integer;
  ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
  Size: Integer;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  if (Index < 0) or (Index >= FCount) then
    raise EDCLOutOfBounds.Create(SOutOfBounds);
  Size := ACollection.Size;
  System.Move(Items[Index], Items[Index + Size],
  	Size * SizeOf(IInterface));
  It := ACollection.First;
  while It.HasNext do
  begin
    Items[Index] := It.Next;
    Inc(Index);
  end;
  Result := True;
end;

function TIntfVector.AddAll(ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Add(It.Next);
  Result := True;
end;

procedure TIntfVector.Clear;
var
	I: Integer;
begin
  for I := 0 to FCount - 1 do
  	Items[I] := nil;
  FCount := 0;
end;

function TIntfVector.Clone: IInterface;
var
  NewList: IIntfList;
begin
  NewList := TIntfVector.Create(FCapacity);
  NewList.AddAll(Self);
  Result := NewList;
end;

function TIntfVector.Contains(AObject: IInterface): Boolean;
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
      Exit;
    end;
end;

function TIntfVector.ContainsAll(ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
begin
	Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
  begin
    if not Contains(It.Next) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

constructor TIntfVector.Create;
begin
  Create(16);
end;

constructor TIntfVector.Create(Capacity: Integer);
begin
  inherited Create;
	FCount := 0;
  FCapacity := Capacity;
	SetLength(Items, FCapacity);
end;

destructor TIntfVector.Destroy;
begin
  Clear;
  inherited;
end;

function TIntfVector.Equals(ACollection: IIntfCollection): Boolean;
var
  I: Integer;
  It: IIntfIterator;
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

function TIntfVector.GetObject(Index: Integer): IInterface;
begin
	if (Index < 0) or (Index >= FCount) then
  begin
    Result := nil;
    Exit;
  end;
	Result := Items[Index];
end;

procedure TIntfVector.Grow;
begin
	FCapacity := FCapacity + FCapacity div 4;
  SetLength(Items, FCapacity);
end;

function TIntfVector.IndexOf(AObject: IInterface): Integer;
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
      Exit;
    end;
end;

function TIntfVector.First: IIntfIterator;
begin
	Result := TIntfItr.Create(Self);
end;

function TIntfVector.IsEmpty: Boolean;
begin
	Result := FCount = 0;
end;

function TIntfVector.Last: IIntfIterator;
var
  NewIterator: TIntfItr;
begin
  NewIterator := TIntfItr.Create(Self);
  NewIterator.FCursor := NewIterator.FOwnList.FCount;
  NewIterator.FSize := NewIterator.FOwnList.FCount;
	Result := NewIterator;
end;

function TIntfVector.LastIndexOf(AObject: IInterface): Integer;
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
      Exit;
    end;
end;

function TIntfVector.Remove(Index: Integer): IInterface;
begin
	if (Index < 0) or (Index >= FCount) then
  	raise EDCLOutOfBounds.Create(SOutOfBounds);
  Result := Items[Index];
  Items[Index] := nil;
  System.Move(Items[Index + 1], Items[Index],
  	(FCount - Index) * SizeOf(IInterface));
  Dec(FCount);
end;

function TIntfVector.Remove(AObject: IInterface): Boolean;
var
	I: Integer;
begin
	Result := False;
  if AObject = nil then
  	Exit;
  for I := FCount - 1 downto 0 do
  	if Items[I] = AObject then // Removes all AObject
    begin
      Items[I] := nil; // Force Release
	    System.Move(Items[I + 1], Items[I],
      	(FCount - I) * SizeOf(IInterface));
      Dec(FCount);
      Result := True;
    end;
end;

function TIntfVector.RemoveAll(ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Remove(It.Next);
end;

function TIntfVector.RetainAll(ACollection: IIntfCollection): Boolean;
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

procedure TIntfVector.SetObject(Index: Integer;
  AObject: IInterface);
begin
	if (Index < 0) or (Index >= FCount) then
  	raise EDCLOutOfBounds.Create(SOutOfBounds);
  Items[Index] := AObject;
end;

function TIntfVector.Size: Integer;
begin
	Result := FCount;
end;

function TIntfVector.SubList(First, Count: Integer): IIntfList;
var
  I: Integer;
  Last: Integer;
begin
  Last := First + Count - 1;
  if Last >= FCount then
    Last := FCount - 1;
  Result := TIntfVector.Create(Count);
  for I := First to Last do
	  Result.Add(Items[I]);
end;

procedure TIntfVector.AfterConstruction;
begin
end;

procedure TIntfVector.BeforeDestruction;
begin
end;

{ TStrVector }

procedure TStrVector.Add(Index: Integer; const AString: string);
begin
  System.Move(Items[Index], Items[Index - 1],
  	(FCount - Index) * SizeOf(string));
  FCapacity := Length(Items);
  Items[Index] := AString;
  Inc(FCount);
end;

function TStrVector.Add(const AString: string): Boolean;
begin
	if FCount = FCapacity then
  	Grow;
  Items[FCount] := AString;
  Inc(FCount);
  Result := True;
end;

function TStrVector.AddAll(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Add(It.Next);
  Result := True;
end;

function TStrVector.AddAll(Index: Integer;
  ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
  Size: Integer;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  if (Index < 0) or (Index >= FCount) then
    raise EDCLOutOfBounds.Create(SOutOfBounds);
  Size := ACollection.Size;
  System.Move(Items[Index], Items[Index + Size],
  	Size * SizeOf(string));
  It := ACollection.First;
  while It.HasNext do
  begin
    Items[Index] := It.Next;
    Inc(Index);
  end;
  Result := True;
end;

procedure TStrVector.AfterConstruction;
begin
end;

procedure TStrVector.BeforeDestruction;
begin
end;

procedure TStrVector.Clear;
var
	I: Integer;
begin
  for I := 0 to FCount - 1 do
  	Items[I] := '';
  FCount := 0;
end;

function TStrVector.Clone: TObject;
var
  NewList: TStrVector;
begin
  NewList := TStrVector.Create(FCapacity);
  NewList.AddAll(Self);
  Result := NewList;
end;

function TStrVector.Contains(const AString: string): Boolean;
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
      Exit;
    end;
end;

function TStrVector.ContainsAll(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
begin
	Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
  begin
    if not Contains(It.Next) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

constructor TStrVector.Create;
begin
  Create(16);
end;

constructor TStrVector.Create(Capacity: Integer);
begin
  inherited Create;
	FCount := 0;
  FCapacity := Capacity;
	SetLength(Items, FCapacity);
end;

destructor TStrVector.Destroy;
begin
  Clear;
  inherited;
end;

function TStrVector.Equals(ACollection: IStrCollection): Boolean;
var
  I: Integer;
  It: IStrIterator;
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

function TStrVector.First: IStrIterator;
begin
	Result := TStrItr.Create(Self);
end;

function TStrVector.GetString(Index: Integer): string;
begin
	if (Index < 0) or (Index >= FCount) then
  begin
    Result := '';
    Exit;
  end;
	Result := Items[Index];
end;

procedure TStrVector.Grow;
begin
	FCapacity := FCapacity + FCapacity div 4;
  SetLength(Items, FCapacity);
end;

function TStrVector.IndexOf(const AString: string): Integer;
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

function TStrVector.IsEmpty: Boolean;
begin
	Result := FCount = 0;
end;

function TStrVector.Last: IStrIterator;
var
  NewIterator: TStrItr;
begin
  NewIterator := TStrItr.Create(Self);
  NewIterator.FCursor := NewIterator.FOwnList.FCount;
  NewIterator.FSize := NewIterator.FOwnList.FCount;
	Result := NewIterator;
end;

function TStrVector.LastIndexOf(const AString: string): Integer;
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
      Exit;
    end;
end;

function TStrVector.Remove(const AString: string): Boolean;
var
	I: Integer;
begin
	Result := False;
  if AString = '' then
  	Exit;
  for I := FCount - 1 downto 0 do
  	if Items[I] = AString then // Removes all AObject
    begin
      Items[I] := ''; // Force Release
	    System.Move(Items[I + 1], Items[I],
      	(FCount - I) * SizeOf(string));
      Dec(FCount);
      Result := True;
    end;
end;

function TStrVector.Remove(Index: Integer): string;
begin
	if (Index < 0) or (Index >= FCount) then
  	raise EDCLOutOfBounds.Create(SOutOfBounds);
  Result := Items[Index];
  Items[Index] := '';
  System.Move(Items[Index + 1], Items[Index],
  	(FCount - Index) * SizeOf(string));
  Dec(FCount);
end;

function TStrVector.RemoveAll(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Remove(It.Next);
end;

function TStrVector.RetainAll(ACollection: IStrCollection): Boolean;
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

procedure TStrVector.SetString(Index: Integer; const AString: string);
begin
	if (Index < 0) or (Index >= FCount) then
  	raise EDCLOutOfBounds.Create(SOutOfBounds);
  Items[Index] := AString;
end;

function TStrVector.Size: Integer;
begin
	Result := FCount;
end;

function TStrVector.SubList(First, Count: Integer): IStrList;
var
  I: Integer;
  Last: Integer;
begin
  Last := First + Count - 1;
  if Last >= FCount then
    Last := FCount - 1;
  Result := TStrVector.Create(Count);
  for I := First to Last do
	  Result.Add(Items[I]);
end;

{ TVector }

procedure TVector.Add(Index: Integer; AObject: TObject);
begin
  System.Move(Items[Index], Items[Index - 1],
  	(FCount - Index) * SizeOf(TObject));
  FCapacity := Length(Items);
  Items[Index] := AObject;
  Inc(FCount);
end;

function TVector.Add(AObject: TObject): Boolean;
begin
	if FCount = FCapacity then
  	Grow;
  Items[FCount] := AObject;
  Inc(FCount);
  Result := True;
end;

function TVector.AddAll(Index: Integer; ACollection: ICollection): Boolean;
var
  It: IIterator;
  Size: Integer;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  if (Index < 0) or (Index >= FCount) then
    raise EDCLOutOfBounds.Create(SOutOfBounds);
  Size := ACollection.Size;
  System.Move(Items[Index], Items[Index + Size],
  	Size * SizeOf(IInterface));
  It := ACollection.First;
  while It.HasNext do
  begin
    Items[Index] := It.Next;
    Inc(Index);
  end;
  Result := True;
end;

function TVector.AddAll(ACollection: ICollection): Boolean;
var
  It: IIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Add(It.Next);
  Result := True;
end;

procedure TVector.Clear;
var
	I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    FreeObject(Items[I]);
  	Items[I] := nil;
  end;
  FCount := 0;
end;

function TVector.Clone: TObject;
var
  NewList: TVector;
begin
  NewList := TVector.Create(FCapacity, False); // Only one can have FOwnsObject = True
  NewList.AddAll(Self);
  Result := NewList;
end;

function TVector.Contains(AObject: TObject): Boolean;
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
      Exit;
    end;
end;

function TVector.ContainsAll(ACollection: ICollection): Boolean;
var
  It: IIterator;
begin
	Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
  begin
    if not Contains(It.Next) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

constructor TVector.Create;
begin
  Create(16, True);
end;

constructor TVector.Create(Capacity: Integer; AOwnsObjects: Boolean);
begin
  inherited Create;
	FCount := 0;
  FCapacity := Capacity;
  FOwnsObjects := AOwnsObjects;
	SetLength(Items, FCapacity);
end;

destructor TVector.Destroy;
begin
  Clear;
  inherited;
end;

function TVector.Equals(ACollection: ICollection): Boolean;
var
  I: Integer;
  It: IIterator;
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

procedure TVector.FreeObject(AObject: TObject);
begin
  if FOwnsObjects then
    AObject.Free;
end;

function TVector.GetObject(Index: Integer): TObject;
begin
	if (Index < 0) or (Index >= FCount) then
  begin
    Result := nil;
    Exit;
  end;
	Result := Items[Index];
end;

procedure TVector.Grow;
begin
	FCapacity := FCapacity + FCapacity div 4;
  SetLength(Items, FCapacity);
end;

function TVector.IndexOf(AObject: TObject): Integer;
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
      Exit;
    end;
end;

function TVector.First: IIterator;
begin
  Result := TItr.Create(Self);
end;

function TVector.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TVector.Last: IIterator;
var
  NewIterator: TItr;
begin
  NewIterator := TItr.Create(Self);
  NewIterator.FCursor := NewIterator.FOwnList.FCount;
  NewIterator.FSize := NewIterator.FOwnList.FCount;
	Result := NewIterator;
end;

function TVector.LastIndexOf(AObject: TObject): Integer;
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
      Exit;
    end;
end;

function TVector.Remove(AObject: TObject): Boolean;
var
	I: Integer;
begin
	Result := False;
  if AObject = nil then
  	Exit;
  for I := FCount - 1 downto 0 do
  	if Items[I] = AObject then // Removes all AObject
    begin
      FreeObject(Items[I]);
	    System.Move(Items[I + 1], Items[I],
      	(FCount - I) * SizeOf(TObject));
      Dec(FCount);
      Result := True;
    end;
end;

function TVector.Remove(Index: Integer): TObject;
begin
	if (Index < 0) or (Index >= FCount) then
  	raise EDCLOutOfBounds.Create(SOutOfBounds);
  Result := Items[Index];
  FreeObject(Items[Index]);
  System.Move(Items[Index + 1], Items[Index],
  	(FCount - Index) * SizeOf(TObject));
  Dec(FCount);
end;

function TVector.RemoveAll(ACollection: ICollection): Boolean;
var
  It: IIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Remove(It.Next);
end;

function TVector.RetainAll(ACollection: ICollection): Boolean;
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

procedure TVector.SetObject(Index: Integer; AObject: TObject);
begin
	if (Index < 0) or (Index >= FCount) then
  	raise EDCLOutOfBounds.Create(SOutOfBounds);
  Items[Index] := AObject;
end;

function TVector.Size: Integer;
begin
  Result := FCount;
end;

function TVector.SubList(First, Count: Integer): IList;
var
  I: Integer;
  Last: Integer;
begin
  Last := First + Count - 1;
  if Last >= FCount then
    Last := FCount - 1;
  Result := TVector.Create(Count, FOwnsObjects);
  for I := First to Last do
	  Result.Add(Items[I]);
end;

procedure TVector.AfterConstruction;
begin
end;

procedure TVector.BeforeDestruction;
begin
end;

end.
