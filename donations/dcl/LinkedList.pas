//------------------------------------------------------------------------------
// The Delphi Container Library
// Jean-Philippe BEMPEL aka RDM
// rdm_30@yahoo.com
//------------------------------------------------------------------------------
unit LinkedList;

{$I dcl.inc}

interface

uses DCL_intf, AbstractContainer;

type
	PIntfLinkedListItem = ^TIntfLinkedListItem;
	TIntfLinkedListItem = record
  	Obj: IInterface;
    Next: PIntfLinkedListItem;
  end;

	PStrLinkedListItem = ^TStrLinkedListItem;
	TStrLinkedListItem = record
  	Str: string;
    Next: PStrLinkedListItem;
  end;

	PLinkedListItem = ^TLinkedListItem;
	TLinkedListItem = record
  	Obj: TObject;
    Next: PLinkedListItem;
  end;

  TIntfLinkedList = class(TAbstractContainer, IIntfCollection, IIntfList, IIntfCloneable)
  private
  	FStart: PIntfLinkedListItem;
    FEnd: PIntfLinkedListItem;
    FSize: Integer;
  protected
  	procedure AddFirst(AObject: IInterface);
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
    constructor Create(ACollection: IIntfCollection); overload;
    destructor Destroy; override;
  end;

  TStrLinkedList = class(TAbstractContainer, IStrCollection, IStrList, ICloneable)
  private
  	FStart: PStrLinkedListItem;
    FEnd: PStrLinkedListItem;
    FSize: Integer;
  protected
  	procedure AddFirst(const AString: string);
  protected
  { IIntfCollection }
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
  { IIntfList }
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
    constructor Create; overload;
    constructor Create(ACollection: IStrCollection); overload;
    destructor Destroy; override;
  end;

  TLinkedList = class(TAbstractContainer, ICollection, IList, ICloneable)
  private
    FEnd: PLinkedListItem;
    FOwnsObjects: Boolean;
    FSize: Integer;
  	FStart: PLinkedListItem;
  protected
  	procedure AddFirst(AObject: TObject);
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
  protected
  { ICloneable }
    function Clone: TObject;
  public
    constructor Create; overload;
    constructor Create(AOwnsObjects: Boolean); overload;
    constructor Create(ACollection: ICollection; AOwnsObjects: Boolean); overload;
    destructor Destroy; override;
  end;


implementation

uses SysUtils, DCLUtil;

type
  TIntfItr = class(TAbstractContainer, IIntfIterator)
  private
    FCursor: PIntfLinkedListItem;
    FOwnList: TIntfLinkedList;
    FLastRet: PIntfLinkedListItem;
    FSize: Integer;
  protected
  { IIterator}
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
    constructor Create(OwnList: TIntfLinkedList; Start: PIntfLinkedListItem);
    destructor Destroy; override;
  end;

  TStrItr = class(TAbstractContainer, IStrIterator)
  private
    FCursor: PStrLinkedListItem;
    FOwnList: TStrLinkedList;
    FLastRet: PStrLinkedListItem;
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
    constructor Create(OwnList: TStrLinkedList; Start: PStrLinkedListItem);
    destructor Destroy; override;
  end;

  TItr = class(TAbstractContainer, IIterator)
  private
    FCursor: PLinkedListItem;
    FOwnList: TLinkedList;
    FLastRet: PLinkedListItem;
    FSize: Integer;
  public
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
    constructor Create(OwnList: TLinkedList; Start: PLinkedListItem);
    destructor Destroy; override;
  end;

{ TIntfItr }

procedure TIntfItr.Add(AObject: IInterface);
var
  NewItem: PIntfLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if AObject = nil then
		Exit;
	New(NewItem);
  NewItem.Obj := AObject;
  if FCursor = nil then
  begin
  	FCursor := NewItem;
    NewItem.Next := nil;
  end else
  begin
		NewItem.Next := FCursor.Next;
		FCursor.Next := NewItem;
  end;
  Inc(FOwnList.FSize);
  Inc(FSize);
end;

constructor TIntfItr.Create(OwnList: TIntfLinkedList; Start: PIntfLinkedListItem);
begin
  inherited Create;
  FCursor := Start;
  FOwnList := OwnList;
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  FLastRet := nil;
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
  Result := FCursor.Obj;
end;

function TIntfItr.HasNext: Boolean;
begin
  Result := FCursor <> nil;
end;

function TIntfItr.HasPrevious: Boolean;
begin
	// Unidirectional
  raise EDCLOperationNotSupported.Create(RsEOperationNotSupported);
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
  Result := FCursor.Obj;
  FLastRet := FCursor;
  FCursor := FCursor.Next;
end;

function TIntfItr.NextIndex: Integer;
begin
  // No index
  raise EDCLOperationNotSupported.Create(RsEOperationNotSupported);
end;

function TIntfItr.Previous: IInterface;
begin
  // Unidirectional
  raise EDCLOperationNotSupported.Create(RsEOperationNotSupported);
end;

function TIntfItr.PreviousIndex: Integer;
begin
  // No Index;
  raise EDCLOperationNotSupported.Create(RsEOperationNotSupported);
end;

procedure TIntfItr.Remove;
var
  Current: PIntfLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
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
  Dispose(Current);
  Dec(FOwnList.FSize);
  Dec(FSize);
end;

procedure TIntfItr.SetObject(AObject: IInterface);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  FCursor.Obj := AObject;
end;

{ TStrItr }

procedure TStrItr.Add(const AString: string);
var
  NewItem: PStrLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if AString = '' then
		Exit;
	New(NewItem);
  NewItem.Str := AString;
  if FCursor = nil then
  begin
  	FCursor := NewItem;
    NewItem.Next := nil;
  end else
  begin
		NewItem.Next := FCursor.Next;
		FCursor.Next := NewItem;
  end;
  Inc(FOwnList.FSize);
  Inc(FSize);
end;

constructor TStrItr.Create(OwnList: TStrLinkedList;
  Start: PStrLinkedListItem);
begin
  inherited Create;
  FCursor := Start;
  FOwnList := OwnList;
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  FLastRet := nil;
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
  Result := FCursor.Str;
end;

function TStrItr.HasNext: Boolean;
begin
  Result := FCursor <> nil;
end;

function TStrItr.HasPrevious: Boolean;
begin
	// Unidirectional
  raise EDCLOperationNotSupported.Create(RsEOperationNotSupported);
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
  Result := FCursor.Str;
  FLastRet := FCursor;
  FCursor := FCursor.Next;
end;

function TStrItr.NextIndex: Integer;
begin
  // No index
  raise EDCLOperationNotSupported.Create(RsEOperationNotSupported);
end;

function TStrItr.Previous: string;
begin
  // Unidirectional
  raise EDCLOperationNotSupported.Create(RsEOperationNotSupported);
end;

function TStrItr.PreviousIndex: Integer;
begin
  // No index
  raise EDCLOperationNotSupported.Create(RsEOperationNotSupported);
end;

procedure TStrItr.Remove;
var
  Current: PStrLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
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
  Dispose(Current);
  Dec(FOwnList.FSize);
  Dec(FSize);
end;

procedure TStrItr.SetString(const AString: string);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  FCursor.Str := AString;
end;

{ TItr }

procedure TItr.Add(AObject: TObject);
var
  NewItem: PLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if AObject = nil then
		Exit;
	New(NewItem);
  NewItem.Obj := AObject;
  if FCursor = nil then
  begin
  	FCursor := NewItem;
    NewItem.Next := nil;
  end else
  begin
		NewItem.Next := FCursor.Next;
		FCursor.Next := NewItem;
  end;
  Inc(FOwnList.FSize);
  Inc(FSize);
end;

constructor TItr.Create(OwnList: TLinkedList; Start: PLinkedListItem);
begin
  inherited Create;
  FCursor := Start;
  FOwnList := OwnList;
  FOwnList._AddRef; // Add a ref because FOwnList is not an interface !
  FLastRet := nil;
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
  Result := FCursor.Obj;
end;

function TItr.HasNext: Boolean;
begin
  Result := FCursor <> nil;
end;

function TItr.HasPrevious: Boolean;
begin
	// Unidirectional
  raise EDCLOperationNotSupported.Create(RsEOperationNotSupported);
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
  Result := FCursor.Obj;
  FLastRet := FCursor;
  FCursor := FCursor.Next;
end;

function TItr.NextIndex: Integer;
begin
	// No Index
  raise EDCLOperationNotSupported.Create(RsEOperationNotSupported);
end;

function TItr.Previous: TObject;
begin
	// Unidirectional
  raise EDCLOperationNotSupported.Create(RsEOperationNotSupported);
end;

function TItr.PreviousIndex: Integer;
begin
	// No Index
  raise EDCLOperationNotSupported.Create(RsEOperationNotSupported);
end;

procedure TItr.Remove;
var
  Current: PLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
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
  Dispose(Current);
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
  FCursor.Obj := AObject;
end;

{ TIntfLinkedList }

procedure TIntfLinkedList.Add(Index: Integer; AObject: IInterface);
var
	I: Integer;
	Current: PIntfLinkedListItem;
  NewItem: PIntfLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if AObject = nil then
		Exit;
	if FStart = nil then
  begin
  	AddFirst(AObject);
    Exit;
  end;
  if (Index < 0) or (Index > FSize) then
  	raise EDCLOutOfBounds.Create(RsEOutOfBounds);
	New(NewItem);
  NewItem.Obj := AObject;
  if Index = 0 then
  begin
  	NewItem.Next := FStart;
  	FStart := NewItem;
    Inc(FSize);
  end else
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

function TIntfLinkedList.Add(AObject: IInterface): Boolean;
var
	NewItem: PIntfLinkedListItem;
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
	if FStart = nil then
  begin
  	AddFirst(AObject);
    Result := True;
    Exit;
  end;
  New(NewItem);
  NewItem.Obj := AObject;
  NewItem.Next := nil;
	FEnd.Next := NewItem;
  FEnd := NewItem;
  Inc(FSize);
  Result := True;
end;

function TIntfLinkedList.AddAll(ACollection: IIntfCollection): Boolean;
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

function TIntfLinkedList.AddAll(Index: Integer;
  ACollection: IIntfCollection): Boolean;
var
	I: Integer;
  It: IIntfIterator;
	Current: PIntfLinkedListItem;
  NewItem: PIntfLinkedListItem;
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
	if (FStart = nil) and (It.HasNext) then
  begin
  	AddFirst(It.Next);
    Exit;
  end;
  if (Index < 0) or (Index > FSize) then
  	raise EDCLOutOfBounds.Create(RsEOutOfBounds);
  Current := FStart;
  I := 0;
  while (Current <> nil) and (I <> Index) do
    Current := Current.Next;
  while (It.HasNext) do
  begin
    New(NewItem);
    NewItem.Obj := It.Next;
    if Index = 0 then
    begin
      NewItem.Next := FStart;
      FStart := NewItem;
      Inc(FSize);
    end else
    begin
      NewItem.Next := Current.Next;
      Current.Next := NewItem;
      Inc(FSize);
    end;
    Inc(Index);
  end;
  Result := True;
end;

procedure TIntfLinkedList.AddFirst(AObject: IInterface);
begin
  New(FStart);
  FStart.Obj := AObject;
  FStart.Next := nil;
  FEnd := FStart;
  Inc(FSize);
end;

procedure TIntfLinkedList.Clear;
var
	I: Integer;
	Old, Current: PIntfLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Current := FStart;
	for I := 0 to FSize - 1 do
  begin
  	Current.Obj := nil;
    Old := Current;
    Current := Current.Next;
    Dispose(Old);
  end;
  FSize := 0;
end;

function TIntfLinkedList.Clone: IInterface;
var
  NewList: IIntfList;
begin
  NewList := TIntfLinkedList.Create;
  NewList.AddAll(Self);
  Result := NewList;
end;

function TIntfLinkedList.Contains(AObject: IInterface): Boolean;
var
	I: Integer;
  Current: PIntfLinkedListItem;
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
	Current := FStart;
	for I := 0 to FSize - 1 do
  begin
		if Current.Obj = AObject then
    begin
    	Result := True;
    	Exit;
    end;
    Current := Current.Next;
  end;
end;

function TIntfLinkedList.ContainsAll(
  ACollection: IIntfCollection): Boolean;
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

constructor TIntfLinkedList.Create;
begin
  FStart := nil;
  FEnd := nil;
  FSize := 0;
end;

constructor TIntfLinkedList.Create(ACollection: IIntfCollection);
var
  It: IIntfIterator;
begin
  inherited Create;
  FStart := nil;
  FEnd := nil;
  FSize := 0;
  if ACollection = nil then
    raise Exception.Create('Collection = nil');
  It := ACollection.First;
  while it.HasNext do
    Add(It.Next);
end;

destructor TIntfLinkedList.Destroy;
begin
  Clear;
  inherited;
end;

function TIntfLinkedList.Equals(ACollection: IIntfCollection): Boolean;
var
  It, ItSelf: IIntfIterator;
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
  ItSelf := First;
  while ItSelf.HasNext do
		if ItSelf.Next <> It.Next then
      Exit;
  Result := True;
end;

function TIntfLinkedList.GetObject(Index: Integer): IInterface;
var
	I: Integer;
  Current: PIntfLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
 	Result := nil;
	if FStart = nil then
    Exit;
	Current := FStart;
	for I := 0 to Index - 1 do
  	Current := Current.Next;
  Result := Current.Obj;
end;

function TIntfLinkedList.IndexOf(AObject: IInterface): Integer;
var
	I: Integer;
  Current: PIntfLinkedListItem;
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
	if FStart = nil then
  	Exit;
	Current := FStart;
	for I := 0 to FSize - 1 do
  begin
		if Current.Obj = AObject then
    begin
    	Result := I;
      Exit;
    end;
    Current := Current.Next;
  end;
end;

function TIntfLinkedList.First: IIntfIterator;
begin
	Result := TIntfItr.Create(Self, FStart);
end;

function TIntfLinkedList.IsEmpty: Boolean;
begin
	Result := FSize = 0;
end;

function TIntfLinkedList.Last: IIntfIterator;
begin
	Result := TIntfItr.Create(Self, FStart);
end;

function TIntfLinkedList.LastIndexOf(AObject: IInterface): Integer;
var
	I: Integer;
  Current: PIntfLinkedListItem;
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

function TIntfLinkedList.Remove(AObject: IInterface): Boolean;
var
	I: Integer;
  Old, Current: PIntfLinkedListItem;
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
  if FStart = nil then
  	Exit;
  Old := nil;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
  	if Current.Obj = AObject then
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
      Dispose(Current);
      Dec(FSize);
      Exit;
    end;
    Old := Current;
    Current := Current.Next;
  end;
end;

function TIntfLinkedList.Remove(Index: Integer): IInterface;
var
	I: Integer;
  Old, Current: PIntfLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
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
  Dispose(Current);
  Dec(FSize);
end;

function TIntfLinkedList.RemoveAll(ACollection: IIntfCollection): Boolean;
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

function TIntfLinkedList.RetainAll(ACollection: IIntfCollection): Boolean;
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
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
end;

procedure TIntfLinkedList.SetObject(Index: Integer;
  AObject: IInterface);
var
	I: Integer;
  Current: PIntfLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if FStart = nil then
  	Exit;
  Current := FStart;
  for I := 0 to Index - 1 do
    Current := Current.Next;
  Current.Obj := AObject;
end;

function TIntfLinkedList.Size: Integer;
begin
	Result := FSize;
end;

function TIntfLinkedList.SubList(First, Count: Integer): IIntfList;
var
  I: Integer;
  It: IIntfIterator;
  Last: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Last := First + Count - 1;
  if Last > FSize then
    Last := FSize - 1;
  Result := TIntfLinkedList.Create;
  I := 0;
  It := Self.First;
  while (It.HasNext) and (I < First) do
  begin
    It.Next;
    Inc(I);
  end;
  I := 0;
  while (It.HasNext) and (I <= Last) do
  begin
	  Result.Add(It.Next);
    Inc(I);
  end;
end;

{ TStrLinkedList }

procedure TStrLinkedList.Add(Index: Integer; const AString: string);
var
	I: Integer;
	Current: PStrLinkedListItem;
  NewItem: PStrLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if AString = '' then
		Exit;
	if FStart = nil then
  begin
  	AddFirst(AString);
    Exit;
  end;
  if (Index < 0) or (Index > FSize) then
  	raise EDCLOutOfBounds.Create(RsEOutOfBounds);
	New(NewItem);
  NewItem.Str := AString;
  if Index = 0 then
  begin
  	NewItem.Next := FStart;
  	FStart := NewItem;
    Inc(FSize);
  end else
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

function TStrLinkedList.Add(const AString: string): Boolean;
var
	NewItem: PStrLinkedListItem;
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
	if FStart = nil then
  begin
  	AddFirst(AString);
    Result := True;
    Exit;
  end;
  New(NewItem);
  NewItem.Str := AString;
  NewItem.Next := nil;
	FEnd.Next := NewItem;
  FEnd := NewItem;
  Inc(FSize);
  Result := True;
end;

function TStrLinkedList.AddAll(ACollection: IStrCollection): Boolean;
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

function TStrLinkedList.AddAll(Index: Integer;
  ACollection: IStrCollection): Boolean;
var
	I: Integer;
  It: IStrIterator;
	Current: PStrLinkedListItem;
  NewItem: PStrLinkedListItem;
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
	if (FStart = nil) and (It.HasNext) then
  begin
  	AddFirst(It.Next);
    Exit;
  end;
  if (Index < 0) or (Index > FSize) then
  	raise EDCLOutOfBounds.Create(RsEOutOfBounds);
  Current := FStart;
  I := 0;
  while (Current <> nil) and (I <> Index) do
    Current := Current.Next;
  while (It.HasNext) do
  begin
    New(NewItem);
    NewItem.Str := It.Next;
    if Index = 0 then
    begin
      NewItem.Next := FStart;
      FStart := NewItem;
      Inc(FSize);
    end else
    begin
      NewItem.Next := Current.Next;
      Current.Next := NewItem;
      Inc(FSize);
    end;
    Inc(Index);
  end;
  Result := True;
end;

procedure TStrLinkedList.AddFirst(const AString: string);
begin
  New(FStart);
  FStart.Str := AString;
  FStart.Next := nil;
  FEnd := FStart;
  Inc(FSize);
end;

procedure TStrLinkedList.Clear;
var
	I: Integer;
	Old, Current: PStrLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Current := FStart;
	for I := 0 to FSize - 1 do
  begin
  	Current.Str := '';
    Old := Current;
    Current := Current.Next;
    Dispose(Old);
  end;
  FSize := 0;
end;

function TStrLinkedList.Clone: TObject;
var
  NewList: TStrLinkedList;
begin
  NewList := TStrLinkedList.Create;
  NewList.AddAll(Self);
  Result := NewList;
end;

function TStrLinkedList.Contains(const AString: string): Boolean;
var
	I: Integer;
  Current: PStrLinkedListItem;
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

function TStrLinkedList.ContainsAll(ACollection: IStrCollection): Boolean;
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

constructor TStrLinkedList.Create;
begin
  FStart := nil;
  FEnd := nil;
  FSize := 0;
end;

constructor TStrLinkedList.Create(ACollection: IStrCollection);
var
  It: IStrIterator;
begin
  inherited Create;
  FStart := nil;
  FEnd := nil;
  FSize := 0;
  if ACollection = nil then
    raise Exception.Create('Collection = nil');
  It := ACollection.First;
  while it.HasNext do
    Add(It.Next);
end;

destructor TStrLinkedList.Destroy;
begin
  Clear;
  inherited;
end;

function TStrLinkedList.Equals(ACollection: IStrCollection): Boolean;
var
  It, ItSelf: IStrIterator;
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
  ItSelf := First;
  while ItSelf.HasNext do
		if ItSelf.Next <> It.Next then
      Exit;
  Result := True;
end;

function TStrLinkedList.First: IStrIterator;
begin
	Result := TStrItr.Create(Self, FStart);
end;

function TStrLinkedList.GetString(Index: Integer): string;
var
	I: Integer;
  Current: PStrLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
 	Result := '';
	if FStart = nil then
    Exit;
	Current := FStart;
	for I := 0 to Index - 1 do
  	Current := Current.Next;
  Result := Current.Str;
end;

function TStrLinkedList.IndexOf(const AString: string): Integer;
var
	I: Integer;
  Current: PStrLinkedListItem;
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
	if FStart = nil then
  	Exit;
	Current := FStart;
	for I := 0 to FSize - 1 do
  begin
		if Current.Str = AString then
    begin
    	Result := I;
      Exit;
    end;
    Current := Current.Next;
  end;
end;

function TStrLinkedList.IsEmpty: Boolean;
begin
	Result := FSize = 0;
end;

function TStrLinkedList.Last: IStrIterator;
begin
	Result := TStrItr.Create(Self, FStart);
end;

function TStrLinkedList.LastIndexOf(const AString: string): Integer;
var
	I: Integer;
  Current: PStrLinkedListItem;
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

function TStrLinkedList.Remove(Index: Integer): string;
var
	I: Integer;
  Old, Current: PStrLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
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
  Dispose(Current);
  Dec(FSize);
end;

function TStrLinkedList.Remove(const AString: string): Boolean;
var
	I: Integer;
  Old, Current: PStrLinkedListItem;
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
      Dispose(Current);
      Dec(FSize);
      Exit;
    end;
    Old := Current;
    Current := Current.Next;
  end;
end;

function TStrLinkedList.RemoveAll(ACollection: IStrCollection): Boolean;
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

function TStrLinkedList.RetainAll(ACollection: IStrCollection): Boolean;
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
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
end;

procedure TStrLinkedList.SetString(Index: Integer; const AString: string);
var
	I: Integer;
  Current: PStrLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if FStart = nil then
  	Exit;
  Current := FStart;
  for I := 0 to Index - 1 do
    Current := Current.Next;
  Current.Str := AString;
end;

function TStrLinkedList.Size: Integer;
begin
	Result := FSize;
end;

function TStrLinkedList.SubList(First, Count: Integer): IStrList;
var
  I: Integer;
  It: IStrIterator;
  Last: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Last := First + Count - 1;
  if Last > FSize then
    Last := FSize - 1;
  Result := TStrLinkedList.Create;
  I := 0;
  It := Self.First;
  while (It.HasNext) and (I < First) do
  begin
    It.Next;
    Inc(I);
  end;
  I := 0;
  while (It.HasNext) and (I <= Last) do
  begin
	  Result.Add(It.Next);
    Inc(I);
  end;
end;

{ TLinkedList }

procedure TLinkedList.Add(Index: Integer; AObject: TObject);
var
	I: Integer;
	Current: PLinkedListItem;
  NewItem: PLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if AObject = nil then
		Exit;
	if FStart = nil then
  begin
  	AddFirst(AObject);
    Exit;
  end;
  if (Index < 0) or (Index > FSize) then
  	raise EDCLOutOfBounds.Create(RsEOutOfBounds);
	New(NewItem);
  NewItem.Obj := AObject;
  if Index = 0 then
  begin
  	NewItem.Next := FStart;
  	FStart := NewItem;
    Inc(FSize);
  end else
  begin
    Current := FStart;
    for I := 0 to Index - 2 do
      Current := Current.Next;
		NewItem.Next := Current.Next;
    Current.Next := NewItem;
    Inc(FSize);
  end;
end;

function TLinkedList.Add(AObject: TObject): Boolean;
var
	NewItem: PLinkedListItem;
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
	if FStart = nil then
  begin
  	AddFirst(AObject);
    Exit;
  end;
  New(NewItem);
  NewItem.Obj := AObject;
  NewItem.Next := nil;
	FEnd.Next := NewItem;
  FEnd := NewItem;
  Inc(FSize);
end;

function TLinkedList.AddAll(ACollection: ICollection): Boolean;
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

function TLinkedList.AddAll(Index: Integer;
  ACollection: ICollection): Boolean;
var
	I: Integer;
  It: IIterator;
	Current: PLinkedListItem;
  NewItem: PLinkedListItem;
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
	if (FStart = nil) and (It.HasNext) then
  begin
  	AddFirst(It.Next);
    Exit;
  end;
  if (Index < 0) or (Index > FSize) then
  	raise EDCLOutOfBounds.Create(RsEOutOfBounds);
  Current := FStart;
  I := 0;
  while (Current <> nil) and (I <> Index) do
    Current := Current.Next;
  while (It.HasNext) do
  begin
    New(NewItem);
    NewItem.Obj := It.Next;
    if Index = 0 then
    begin
      NewItem.Next := FStart;
      FStart := NewItem;
      Inc(FSize);
    end else
    begin
      NewItem.Next := Current.Next;
      Current.Next := NewItem;
      Inc(FSize);
    end;
    Inc(Index);
  end;
  Result := True;
end;

procedure TLinkedList.AddFirst(AObject: TObject);
begin
  New(FStart);
  FStart.Obj := AObject;
  FStart.Next := nil;
  FEnd := FStart;
  Inc(FSize);
end;

procedure TLinkedList.Clear;
var
	I: Integer;
	Old, Current: PLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
	Current := FStart;
	for I := 0 to FSize - 1 do
  begin
  	Current.Obj := nil;
    Old := Current;
    Current := Current.Next;
    Dispose(Old);
  end;
  FSize := 0;
end;

function TLinkedList.Clone: TObject;
var
  NewList: TLinkedList;
begin
  NewList := TLinkedList.Create;
  NewList.AddAll(Self);
  Result := NewList;
end;

function TLinkedList.Contains(AObject: TObject): Boolean;
var
	I: Integer;
  Current: PLinkedListItem;
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
	Current := FStart;
	for I := 0 to FSize - 1 do
  begin
		if Current.Obj = AObject then
    begin
    	Result := True;
    	Exit;
    end;
    Current := Current.Next;
  end;
end;

function TLinkedList.ContainsAll(ACollection: ICollection): Boolean;
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

constructor TLinkedList.Create;
begin
  Create(True);
end;

constructor TLinkedList.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FStart := nil;
  FEnd := nil;
  FSize := 0;
  FOwnsObjects := AOwnsObjects;
end;

constructor TLinkedList.Create(ACollection: ICollection; AOwnsObjects: Boolean);
var
  It: IIterator;
begin
  Create(AOwnsObjects);
  if ACollection = nil then
    raise Exception.Create('Collection = nil');
  It := ACollection.First;
  while it.HasNext do
    Add(It.Next);
end;

destructor TLinkedList.Destroy;
begin
  Clear;
  inherited;
end;

function TLinkedList.Equals(ACollection: ICollection): Boolean;
var
  It, ItSelf: IIterator;
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
  ItSelf := First;
  while ItSelf.HasNext do
		if ItSelf.Next <> It.Next then
      Exit;
  Result := True;
end;

procedure TLinkedList.FreeObject(AObject: TObject);
begin
  if FOwnsObjects then
    AObject.Free;
end;

function TLinkedList.GetObject(Index: Integer): TObject;
var
	I: Integer;
  Current: PLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
 	Result := nil;
	if FStart = nil then
    Exit;
	Current := FStart;
	for I := 0 to Index - 1 do
  	Current := Current.Next;
  Result := Current.Obj;
end;

function TLinkedList.IndexOf(AObject: TObject): Integer;
var
	I: Integer;
  Current: PLinkedListItem;
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
	if FStart = nil then
  	Exit;
	Current := FStart;
	for I := 0 to FSize - 1 do
  begin
		if Current.Obj = AObject then
    begin
    	Result := I;
      Exit;
    end;
    Current := Current.Next;
  end;
end;

function TLinkedList.First: IIterator;
begin
	Result := TItr.Create(Self, FStart);
end;

function TLinkedList.IsEmpty: Boolean;
begin
	Result := FSize = 0;
end;

function TLinkedList.Last: IIterator;
begin
	Result := TItr.Create(Self, FStart);
end;

function TLinkedList.LastIndexOf(AObject: TObject): Integer;
var
	I: Integer;
  Current: PLinkedListItem;
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

function TLinkedList.Remove(AObject: TObject): Boolean;
var
	I: Integer;
  Old, Current: PLinkedListItem;
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
  if FStart = nil then
  	Exit;
  Old := nil;
  Current := FStart;
  for I := 0 to FSize - 1 do
  begin
  	if Current.Obj = AObject then
    begin
      FreeObject(Current.Obj);
	    Current.Obj := nil;
      if Old <> nil then
      begin
      	Old.Next := Current.Next;
        if Old.Next = nil then
        	FEnd := Old;
      end
      else
      	FStart := Current.Next;
      Dispose(Current);
      Dec(FSize);
      Exit;
    end;
    Old := Current;
    Current := Current.Next;
  end;
end;

function TLinkedList.Remove(Index: Integer): TObject;
var
	I: Integer;
  Old, Current: PLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
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
  Current.Obj := nil;
  if Old <> nil then
  begin
    Old.Next := Current.Next;
    if Old.Next = nil then
      FEnd := Old;
  end
  else
  	FStart := Current.Next;
  Dispose(Current);
  Dec(FSize);
end;

function TLinkedList.RemoveAll(ACollection: ICollection): Boolean;
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

function TLinkedList.RetainAll(ACollection: ICollection): Boolean;
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
  It := First;
  while It.HasNext do
    if not ACollection.Contains(It.Next) then
      It.Remove;
end;

procedure TLinkedList.SetObject(Index: Integer; AObject: TObject);
var
	I: Integer;
  Current: PLinkedListItem;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if FStart = nil then
  	Exit;
  Current := FStart;
  for I := 0 to Index - 1 do
    Current := Current.Next;
  Current.Obj := AObject;
end;

function TLinkedList.Size: Integer;
begin
	Result := FSize;
end;

function TLinkedList.SubList(First, Count: Integer): IList;
var
  I: Integer;
  It: IIterator;
  Last: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Last := First + Count - 1;
  if Last > FSize then
    Last := FSize - 1;
  Result := TLinkedList.Create;
  I := 0;
  It := Self.First;
  while (It.HasNext) and (I < First) do
  begin
    It.Next;
    Inc(I);
  end;
  while (It.HasNext) and (I <= Last) do
  begin
	  Result.Add(It.Next);
    Inc(I);
  end;
end;

end.
