//------------------------------------------------------------------------------
// The Delphi Container Library
// Jean-Philippe BEMPEL aka RDM
// rdm_30@yahoo.com
//------------------------------------------------------------------------------
unit ArrayList;

{$I dcl.inc}

interface

uses DCL_intf, DCLUtil, AbstractContainer;

type
  TIntfArrayList = class(TAbstractContainer, IIntfCollection, IIntfList,
  	IIntfArray, IIntfCloneable)
  private
    FElementData: TIInterfaceArray;
    FSize: Integer;
    FCapacity: Integer;
  protected
    procedure Grow; virtual;
  protected
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
  protected
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
  { IIntfCloneable }
    function Clone: IInterface;
  public
    constructor Create; overload;
    constructor Create(Capacity: Integer); overload;
    constructor Create(ACollection: IIntfCollection); overload;
    destructor Destroy; override;
  end;

  TStrArrayList = class(TAbstractContainer, IStrCollection, IStrList,
  	IStrArray, ICloneable)
  private
    FCapacity: Integer;
    FElementData: TStringArray;
    FSize: Integer;
  protected
    procedure Grow; virtual;
  protected
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
  protected
  { IStrList }
    procedure Add(Index: Integer; const AString: string); overload;
    function AddAll(Index: Integer; ACollection: IStrCollection): Boolean; overload;
    function GetString(Index: Integer): string;
    function IndexOf(const AString: string): Integer;
    function LastIndexOf(const AString: string): Integer;
    function Remove(Index: Integer): string; overload;
    procedure SetString(Index: Integer; const AString: string);
    function SubList(First, Count: Integer): IStrList;
  public
  { ICloneable }
    function Clone: TObject;
  public
    constructor Create; overload;
    constructor Create(Capacity: Integer); overload;
    constructor Create(ACollection: IStrCollection); overload;
    destructor Destroy; override;
  end;

  TArrayList = class(TAbstractContainer, ICollection, IList,
  	IArray, ICloneable)
  private
    FCapacity: Integer;
    FElementData: TObjectArray;
    FOwnsObjects: Boolean;
    FSize: Integer;
  protected
    procedure Grow; virtual;
    procedure FreeObject(AObject: TObject);
  protected
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
  protected
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
    constructor Create; overload;
    constructor Create(Capacity: Integer; AOwnsObjects: Boolean); overload;
    constructor Create(ACollection: ICollection; AOwnsObjects: Boolean); overload;
    destructor Destroy; override;
  end;


implementation

uses SysUtils;

type
  TIntfItr = class(TAbstractContainer, IIntfIterator)
  private
    FCursor: Integer;
    FOwnList: TIntfArrayList;
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
    constructor Create(OwnList: TIntfArrayList);
    destructor Destroy; override;
  end;

  TStrItr = class(TAbstractContainer, IStrIterator)
  private
    FCursor: Integer;
    FOwnList: TStrArrayList;
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
    constructor Create(OwnList: TStrArrayList);
    destructor Destroy; override;
  end;

  TItr = class(TAbstractContainer, IIterator)
  private
    FCursor: Integer;
    FOwnList: TArrayList;
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
    constructor Create(OwnList: TArrayList);
    destructor Destroy; override;
  end;

{ TIntfItr }

procedure TIntfItr.Add(AObject: IInterface);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  with FOwnList do
  begin
    System.Move(FElementData[FCursor], FElementData[FCursor + 1],
      (FOwnList.FSize - FCursor) * SizeOf(TObject));
    FCapacity := Length(FElementData);
    FElementData[FCursor] := AObject;
    Inc(FOwnList.FSize);
  end;
  Inc(FSize);
  Inc(FCursor);
  FLastRet := -1;
end;

constructor TIntfItr.Create(OwnList: TIntfArrayList);
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
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FOwnList.FElementData[FCursor];
end;

function TIntfItr.HasNext: Boolean;
begin
  Result := FCursor < FSize;
end;

function TIntfItr.HasPrevious: Boolean;
begin
  Result := FCursor > 0;
end;

function TIntfItr.Next: IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FOwnList.FElementData[FCursor];
  FLastRet := FCursor;
  Inc(FCursor);
end;

function TIntfItr.NextIndex: Integer;
begin
  Result := FCursor;
end;

function TIntfItr.Previous: IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Dec(FCursor);
  FLastRet := FCursor;
  Result := FOwnList.FElementData[FCursor];
end;

function TIntfItr.PreviousIndex: Integer;
begin
  Result := FCursor - 1;
end;

procedure TIntfItr.Remove;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  with FOwnList do
  begin
	  FElementData[FCursor] := nil; // Force Release
    System.Move(FElementData[FCursor + 1], FElementData[FCursor],
			(FSize - FCursor) * SizeOf(IInterface));
  end;
  Dec(FOwnList.FSize);
  Dec(FSize);
end;

procedure TIntfItr.SetObject(AObject: IInterface);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{
  if FLastRet = -1 then
    raise EDCLIllegalState.Create(SIllegalState);
  }
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  FOwnList.FElementData[FCursor] := AObject;
end;

{ TStrItr }

procedure TStrItr.Add(const AString: string);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  with FOwnList do
  begin
    System.Move(FElementData[FCursor], FElementData[FCursor + 1],
      (FOwnList.FSize - FCursor) * SizeOf(string));
    FCapacity := Length(FElementData);
    FElementData[FCursor] := AString;
    Inc(FOwnList.FSize);
  end;
  Inc(FSize);
  Inc(FCursor);
  FLastRet := -1;
end;

constructor TStrItr.Create(OwnList: TStrArrayList);
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
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FOwnList.FElementData[FCursor];
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
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FOwnList.FElementData[FCursor];
  FLastRet := FCursor;
  Inc(FCursor);
end;

function TStrItr.NextIndex: Integer;
begin
  Result := FCursor;
end;

function TStrItr.Previous: string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Dec(FCursor);
  FLastRet := FCursor;
  Result := FOwnList.FElementData[FCursor];
end;

function TStrItr.PreviousIndex: Integer;
begin
  Result := FCursor - 1;
end;

procedure TStrItr.Remove;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  with FOwnList do
  begin
	  FElementData[FCursor] := '';// Force Release
    System.Move(FElementData[FCursor + 1], FElementData[FCursor],
			(FSize - FCursor) * SizeOf(string));
  end;
  Dec(FOwnList.FSize);
  Dec(FSize);
end;

procedure TStrItr.SetString(const AString: string);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{
  if FLastRet = -1 then
    raise EDCLIllegalState.Create(SIllegalState);
  }
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  FOwnList.FElementData[FCursor] := AString;
end;

{ TItr }

procedure TItr.Add(AObject: TObject);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  with FOwnList do
  begin
    System.Move(FElementData[FCursor], FElementData[FCursor + 1],
      (FOwnList.FSize - FCursor) * SizeOf(TObject));
    FCapacity := Length(FElementData);
    FElementData[FCursor] := AObject;
    Inc(FOwnList.FSize);
  end;
  Inc(FSize);
  Inc(FCursor);
  FLastRet := -1;
end;

constructor TItr.Create(OwnList: TArrayList);
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
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FOwnList.FElementData[FCursor];
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
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := FOwnList.FElementData[FCursor];
  FLastRet := FCursor;
  Inc(FCursor);
end;

function TItr.NextIndex: Integer;
begin
  Result := FCursor;
end;

function TItr.Previous: TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Dec(FCursor);
  FLastRet := FCursor;
  Result := FOwnList.FElementData[FCursor];
end;

function TItr.PreviousIndex: Integer;
begin
  Result := FCursor - 1;
end;

procedure TItr.Remove;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  with FOwnList do
  begin
		FreeObject(FElementData[FCursor]);
 	  System.Move(FElementData[FCursor + 1], FElementData[FCursor],
			(FSize - FCursor) * SizeOf(TObject));
   end;
  Dec(FOwnList.FSize);
  Dec(FSize);
end;

procedure TItr.SetObject(AObject: TObject);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  FOwnList.FElementData[FCursor] := AObject;
end;

{ TIntfArrayList }

procedure TIntfArrayList.Add(Index: Integer; AObject: IInterface);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	if FSize = FCapacity then
  	Grow;
  System.Move(FElementData[Index], FElementData[Index + 1],
  	(FSize - Index) * SizeOf(IInterface));
  FElementData[Index] := AObject;
  Inc(FSize);
end;

function TIntfArrayList.Add(AObject: IInterface): Boolean;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	if FSize = FCapacity then
  	Grow;
  FElementData[FSize] := AObject;
  Inc(FSize);
  Result := True;
end;

function TIntfArrayList.AddAll(ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Add(It.Next);
  Result := True;
end;

function TIntfArrayList.AddAll(Index: Integer;
  ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
  Size: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if ACollection = nil then
    Exit;
  if (Index < 0) or (Index >= FSize) then
    raise EDCLOutOfBounds.Create(SOutOfBounds);
  Size := ACollection.Size;
  System.Move(FElementData[Index], FElementData[Index + Size],
  	Size * SizeOf(IInterface));
  It := ACollection.First;
  while It.HasNext do
  begin
    FElementData[Index] := It.Next;
    Inc(Index);
  end;
  Result := True;
end;

procedure TIntfArrayList.Clear;
var
	I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  for I := 0 to FSize - 1 do
  	FElementData[I] := nil;
  FSize := 0;
end;

function TIntfArrayList.Clone: IInterface;
var
  NewList: IIntfList;
begin
  NewList := TIntfArrayList.Create(FCapacity);
  NewList.AddAll(Self);
  Result := NewList;
end;

function TIntfArrayList.Contains(AObject: IInterface): Boolean;
var
	I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := False;
	if AObject = nil then
    Exit;
  for I := 0 to FSize - 1 do
  	if FElementData[I] = AObject then
    begin
    	Result := True;
      Exit;
    end;
end;

function TIntfArrayList.ContainsAll(ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
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

constructor TIntfArrayList.Create(Capacity: Integer);
begin
  inherited Create;
	FSize := 0;
  FCapacity := Capacity;
	SetLength(FElementData, FCapacity);
end;

constructor TIntfArrayList.Create(ACollection: IIntfCollection);
var
  It: IIntfIterator;
begin
  inherited Create;
  if ACollection = nil then
    raise Exception.Create('Collection = nil');
  Create(ACollection.Size);
  It := ACollection.First;
  while it.HasNext do
    Add(It.Next);
end;

constructor TIntfArrayList.Create;
begin
	Create(16);
end;

destructor TIntfArrayList.Destroy;
begin
  Clear;
  inherited;
end;

function TIntfArrayList.Equals(ACollection: IIntfCollection): Boolean;
var
  I: Integer;
  It: IIntfIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
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
end;

function TIntfArrayList.GetObject(Index: Integer): IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	if (Index < 0) or (Index >= FSize) then
  begin
    Result := nil;
    Exit;
  end;
	Result := FElementData[Index];
end;

procedure TIntfArrayList.Grow;
begin
	FCapacity := FCapacity + FCapacity div 4;
  SetLength(FElementData, FCapacity);
end;

function TIntfArrayList.IndexOf(AObject: IInterface): Integer;
var
	I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := -1;
	if AObject = nil then
    Exit;
  for I := 0 to FSize - 1 do
  	if FElementData[I] = AObject then
    begin
    	Result := I;
      Exit;
    end;
end;

function TIntfArrayList.First: IIntfIterator;
begin
	Result := TIntfItr.Create(Self);
end;

function TIntfArrayList.IsEmpty: Boolean;
begin
	Result := FSize = 0;
end;

function TIntfArrayList.Last: IIntfIterator;
var
  NewIterator: TIntfItr;
begin
  NewIterator := TIntfItr.Create(Self);
  NewIterator.FCursor := NewIterator.FOwnList.FSize;
  NewIterator.FSize := NewIterator.FOwnList.FSize;
	Result := NewIterator;
end;

function TIntfArrayList.LastIndexOf(AObject: IInterface): Integer;
var
	I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := -1;
	if AObject = nil then
    Exit;
	for I := FSize - 1 downto 0 do
  	if FElementData[I] = AObject then
    begin
    	Result := I;
      Exit;
    end;
end;

function TIntfArrayList.Remove(AObject: IInterface): Boolean;
var
	I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := False;
  if AObject = nil then
  	Exit;
  for I := FSize - 1 downto 0 do
  	if FElementData[I] = AObject then // Removes all AObject
    begin
    	FElementData[I] := nil; // Force Release
	    System.Move(FElementData[I + 1], FElementData[I],
      	(FSize - I) * SizeOf(IInterface));
      Dec(FSize);
      Result := True;
    end;
end;

function TIntfArrayList.Remove(Index: Integer): IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	if (Index < 0) or (Index >= FSize) then
  	raise EDCLOutOfBounds.Create(SOutOfBounds);
  Result := FElementData[Index];
  FElementData[Index] := nil;
  System.Move(FElementData[Index + 1], FElementData[Index],
  	(FSize - Index) * SizeOf(IInterface));
  Dec(FSize);
end;

function TIntfArrayList.RemoveAll(ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Remove(It.Next); 
end;

function TIntfArrayList.RetainAll(ACollection: IIntfCollection): Boolean;
var
  I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if ACollection = nil then
    Exit;
  for I := FSize - 1 downto 0 do
    if not ACollection.Contains(FElementData[I]) then
      Remove(I);
end;

procedure TIntfArrayList.SetObject(Index: Integer;
  AObject: IInterface);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	if (Index < 0) or (Index >= FSize) then
  	raise EDCLOutOfBounds.Create(SOutOfBounds);
  FElementData[Index] := AObject;
end;

function TIntfArrayList.Size: Integer;
begin
	Result := FSize;
end;

function TIntfArrayList.SubList(First, Count: Integer): IIntfList;
var
  I: Integer;
  Last: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Last := First + Count - 1;
  if Last >= FSize then
    Last := FSize - 1;
  Result := TIntfArrayList.Create(Count);
  for I := First to Last do
	  Result.Add(FElementData[I]);
end;

{ TStrArrayList }

procedure TStrArrayList.Add(Index: Integer; const AString: string);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	if FSize = FCapacity then
  	Grow;
  System.Move(FElementData[Index], FElementData[Index + 1],
  	(FSize - Index) * SizeOf(string));
  FElementData[Index] := AString;
  Inc(FSize);
end;

function TStrArrayList.Add(const AString: string): Boolean;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	if FSize = FCapacity then
  	Grow;
  FElementData[FSize] := AString;
  Inc(FSize);
  Result := True;
end;

function TStrArrayList.AddAll(Index: Integer;
  ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
  Size: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if ACollection = nil then
    Exit;
  if (Index < 0) or (Index >= FSize) then
    raise EDCLOutOfBounds.Create(SOutOfBounds);
  Size := ACollection.Size;
  System.Move(FElementData[Index], FElementData[Index + Size],
  	Size * SizeOf(string));
  It := ACollection.First;
  while It.HasNext do
  begin
    FElementData[Index] := It.Next;
    Inc(Index);
  end;
  Result := True;
end;

function TStrArrayList.AddAll(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Add(It.Next);
  Result := True;
end;

procedure TStrArrayList.Clear;
var
	I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  for I := 0 to FSize - 1 do
  	FElementData[I] := '';
  FSize := 0;
end;

function TStrArrayList.Clone: TObject;
var
  NewList: TStrArrayList;
begin
  NewList := TStrArrayList.Create(FCapacity);
  NewList.AddAll(Self);
  Result := NewList;
end;

function TStrArrayList.Contains(const AString: string): Boolean;
var
	I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := False;
	if AString = '' then
    Exit;
  for I := 0 to FSize - 1 do
  	if FElementData[I] = AString then
    begin
    	Result := True;
      Exit;
    end;
end;

function TStrArrayList.ContainsAll(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
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

constructor TStrArrayList.Create;
begin
	Create(16);
end;

constructor TStrArrayList.Create(ACollection: IStrCollection);
var
  It: IStrIterator;
begin
  inherited Create;
  if ACollection = nil then
    raise Exception.Create('Collection = nil');
  Create(ACollection.Size);
  It := ACollection.First;
  while it.HasNext do
    Add(It.Next);
end;

constructor TStrArrayList.Create(Capacity: Integer);
begin
  inherited Create;
	FSize := 0;
  FCapacity := Capacity;
	SetLength(FElementData, FCapacity);
end;

destructor TStrArrayList.Destroy;
begin
	Clear;
  inherited;
end;

function TStrArrayList.Equals(ACollection: IStrCollection): Boolean;
var
  I: Integer;
  It: IStrIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
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
end;

function TStrArrayList.First: IStrIterator;
begin
	Result := TStrItr.Create(Self);
end;

function TStrArrayList.GetString(Index: Integer): string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	if (Index < 0) or (Index >= FSize) then
  begin
    Result := '';
    Exit;
  end;
	Result := FElementData[Index];
end;

procedure TStrArrayList.Grow;
begin
	FCapacity := FCapacity + FCapacity div 4;
  SetLength(FElementData, FCapacity);
end;

function TStrArrayList.IndexOf(const AString: string): Integer;
var
	I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := -1;
	if AString = '' then
    Exit;
  for I := 0 to FSize - 1 do
  	if FElementData[I] = AString then
    begin
    	Result := I;
      Exit;
    end;
end;

function TStrArrayList.IsEmpty: Boolean;
begin
	Result := FSize = 0;
end;

function TStrArrayList.Last: IStrIterator;
var
  NewIterator: TStrItr;
begin
  NewIterator := TStrItr.Create(Self);
  NewIterator.FCursor := NewIterator.FOwnList.FSize;
  NewIterator.FSize := NewIterator.FOwnList.FSize;
	Result := NewIterator;
end;

function TStrArrayList.LastIndexOf(const AString: string): Integer;
var
	I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := -1;
	if AString = '' then
    Exit;
	for I := FSize - 1 downto 0 do
  	if FElementData[I] = AString then
    begin
    	Result := I;
      Exit;
    end;
end;

function TStrArrayList.Remove(const AString: string): Boolean;
var
	I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := False;
  if AString = '' then
  	Exit;
  for I := FSize - 1 downto 0 do
  	if FElementData[I] = AString then // Removes all AObject
    begin
    	FElementData[I] := ''; // Force Release
	    System.Move(FElementData[I + 1], FElementData[I],
      	(FSize - I) * SizeOf(IInterface));
      Dec(FSize);
      Result := True;
    end;
end;

function TStrArrayList.Remove(Index: Integer): string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	if (Index < 0) or (Index >= FSize) then
  	raise EDCLOutOfBounds.Create(SOutOfBounds);
  Result := FElementData[Index];
  FElementData[Index] := '';
  System.Move(FElementData[Index + 1], FElementData[Index],
  	(FSize - Index) * SizeOf(IInterface));
  Dec(FSize);
end;

function TStrArrayList.RemoveAll(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Remove(It.Next);
end;

function TStrArrayList.RetainAll(ACollection: IStrCollection): Boolean;
var
  I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if ACollection = nil then
    Exit;
  for I := FSize - 1 downto 0 do
    if not ACollection.Contains(FElementData[I]) then
      Remove(I);
end;

procedure TStrArrayList.SetString(Index: Integer; const AString: string);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	if (Index < 0) or (Index >= FSize) then
  	raise EDCLOutOfBounds.Create(SOutOfBounds);
  FElementData[Index] := AString
end;

function TStrArrayList.Size: Integer;
begin
	Result := FSize;
end;

function TStrArrayList.SubList(First, Count: Integer): IStrList;
var
  I: Integer;
  Last: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Last := First + Count - 1;
  if Last >= FSize then
    Last := FSize - 1;
  Result := TStrArrayList.Create(Count);
  for I := First to Last do
	  Result.Add(FElementData[I]);
end;

{ TArrayList }

procedure TArrayList.Add(Index: Integer; AObject: TObject);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	if FSize = FCapacity then
  	Grow;
  System.Move(FElementData[Index], FElementData[Index + 1],
  	(FSize - Index) * SizeOf(TObject));
  FElementData[Index] := AObject;
  Inc(FSize);
end;

function TArrayList.Add(AObject: TObject): Boolean;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	if FSize = FCapacity then
  	Grow;
  FElementData[FSize] := AObject;
  Inc(FSize);
  Result := True;
end;

function TArrayList.AddAll(ACollection: ICollection): Boolean;
var
  It: IIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Add(It.Next);
  Result := True;
end;

function TArrayList.AddAll(Index: Integer;
  ACollection: ICollection): Boolean;
var
  It: IIterator;
  Size: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if ACollection = nil then
    Exit;
  if (Index < 0) or (Index >= FSize) then
    raise EDCLOutOfBounds.Create(SOutOfBounds);
  Size := ACollection.Size;
  System.Move(FElementData[Index], FElementData[Index + Size],
  	Size * SizeOf(IInterface));
  It := ACollection.First;
  while It.HasNext do
  begin
    FElementData[Index] := It.Next;
    Inc(Index);
  end;
  Result := True;
end;

procedure TArrayList.Clear;
var
	I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  for I := 0 to FSize - 1 do
  begin
  	FreeObject(FElementData[I]);
    FElementData[I] := nil;
  end;
  FSize := 0;
end;

function TArrayList.Clone: TObject;
var
  NewList: TArrayList;
begin
  NewList := TArrayList.Create(FCapacity, False); // Only one can have FOwnsObject = True
  NewList.AddAll(Self);
  Result := NewList;
end;

function TArrayList.Contains(AObject: TObject): Boolean;
var
	I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := False;
	if AObject = nil then
    Exit;
  for I := 0 to FSize - 1 do
  	if FElementData[I] = AObject then
    begin
    	Result := True;
      Exit;
    end;
end;

function TArrayList.ContainsAll(ACollection: ICollection): Boolean;
var
  It: IIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
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

constructor TArrayList.Create(Capacity: Integer; AOwnsObjects: Boolean);
begin
  inherited Create;
	FSize := 0;
  FCapacity := Capacity;
  FOwnsObjects := AOwnsObjects;
	SetLength(FElementData, FCapacity);
end;

constructor TArrayList.Create(ACollection: ICollection; AOwnsObjects: Boolean);
var
  It: IIterator;
begin
  inherited Create;
  if ACollection = nil then
    raise Exception.Create('Collection = nil');
  Create(ACollection.Size, AOwnsObjects);
  It := ACollection.First;
  while it.HasNext do
    Add(It.Next);
end;

constructor TArrayList.Create;
begin
	Create(16, True);
end;

destructor TArrayList.Destroy;
begin
	Clear;
  inherited;
end;

function TArrayList.Equals(ACollection: ICollection): Boolean;
var
  I: Integer;
  It: IIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
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
end;

procedure TArrayList.FreeObject(AObject: TObject);
begin
	if FOwnsObjects then
  	AObject.Free;
end;

function TArrayList.GetObject(Index: Integer): TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	if (Index < 0) or (Index >= FSize) then
  begin
    Result := nil;
    Exit;
  end;
	Result := FElementData[Index];
end;

procedure TArrayList.Grow;
begin
	FCapacity := FCapacity + FCapacity div 4;
  SetLength(FElementData, FCapacity);
end;

function TArrayList.IndexOf(AObject: TObject): Integer;
var
	I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := -1;
	if AObject = nil then
    Exit;
  for I := 0 to FSize - 1 do
  	if FElementData[I] = AObject then
    begin
    	Result := I;
      Exit;
    end;
end;

function TArrayList.First: IIterator;
begin
	Result := TItr.Create(Self);
end;

function TArrayList.IsEmpty: Boolean;
begin
	Result := FSize = 0;
end;

function TArrayList.Last: IIterator;
var
  NewIterator: TItr;
begin
  NewIterator := TItr.Create(Self);
  NewIterator.FCursor := NewIterator.FOwnList.FSize;
  NewIterator.FSize := NewIterator.FOwnList.FSize;
	Result := NewIterator;
end;

function TArrayList.LastIndexOf(AObject: TObject): Integer;
var
	I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := -1;
	if AObject = nil then
    Exit;
	for I := FSize - 1 downto 0 do
  	if FElementData[I] = AObject then
    begin
    	Result := I;
      Exit;
    end;
end;

function TArrayList.Remove(AObject: TObject): Boolean;
var
	I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Result := False;
  if AObject = nil then
  	Exit;
  for I := FSize - 1 downto 0 do
  	if FElementData[I] = AObject then // Removes all AObject
    begin
    	FreeObject(FElementData[I]);
      System.Move(FElementData[I + 1], FElementData[I],
      	(FSize - I) * SizeOf(TObject));
      Dec(FSize);
      Result := True;
    end;
end;

function TArrayList.Remove(Index: Integer): TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	if (Index < 0) or (Index >= FSize) then
  	raise EDCLOutOfBounds.Create(SOutOfBounds);
  Result := nil;
  FreeObject(FElementData[Index]);
  System.Move(FElementData[Index + 1], FElementData[Index],
    (FSize - Index) * SizeOf(TObject));
  Dec(FSize);
end;

function TArrayList.RemoveAll(ACollection: ICollection): Boolean;
var
  It: IIterator;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    Remove(It.Next);
end;

function TArrayList.RetainAll(ACollection: ICollection): Boolean;
var
  I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if ACollection = nil then
    Exit;
  for I := FSize - 1 to 0 do
    if not ACollection.Contains(FElementData[I]) then
      Remove(I);
end;

procedure TArrayList.SetObject(Index: Integer;
  AObject: TObject);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	if (Index < 0) or (Index >= FSize) then
  	raise EDCLOutOfBounds.Create(SOutOfBounds);
  FElementData[Index] := AObject;
end;

function TArrayList.Size: Integer;
begin
	Result := FSize;
end;

function TArrayList.SubList(First, Count: Integer): IList;
var
  I: Integer;
  Last: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Last := First + Count - 1;
  if Last >= FSize then
    Last := FSize - 1;
  Result := TArrayList.Create(Count, FOwnsObjects);
  for I := First to Last do
	  Result.Add(FElementData[I]);
end;

end.
