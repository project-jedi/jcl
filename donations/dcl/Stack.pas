//------------------------------------------------------------------------------
// The Delphi Container Library
// Jean-Philippe BEMPEL aka RDM
// rdm_30@yahoo.com
//------------------------------------------------------------------------------
unit Stack;

{$I dcl.inc}

interface

uses DCL_intf, DCLUtil, AbstractContainer;

type
  TIntfStack = class(TAbstractContainer, IIntfStack)
  private
    FElements: TIInterfaceArray;
    FCount: Integer;
    FCapacity: Integer;
  protected
    procedure Grow; virtual;
  protected
  { IIntfStack }
    function Contains(AObject: IInterface): Boolean;
    function Empty: Boolean;
    function Pop: IInterface;
    procedure Push(AObject: IInterface);
    function Size: Integer;
  public
    constructor Create; overload;
    constructor Create(Capacity: Integer);  overload;
  end;

  TStrStack = class(TAbstractContainer, IStrStack)
  private
    FElements: TStringArray;
    FCount: Integer;
    FCapacity: Integer;
  protected
    procedure Grow; virtual;
  protected
  { IStrStack }
    function Contains(const AString: string): Boolean;
    function Empty: Boolean;
    function Pop: string;
    procedure Push(const AString: string);
    function Size: Integer;
  public
    constructor Create; overload;
    constructor Create(Capacity: Integer);  overload;
  end;

  TStack = class(TAbstractContainer, IStack)
  private
    FElements: TObjectArray;
    FCount: Integer;
    FCapacity: Integer;
  protected
    procedure Grow; virtual;
  protected
  { IStack }
    function Contains(AObject: TObject): Boolean;
    function Empty: Boolean;
    function Pop: TObject;
    procedure Push(AObject: TObject);
    function Size: Integer;
  public
    constructor Create; overload;
    constructor Create(Capacity: Integer); overload;
  end;

implementation

{ TIntfStack }

function TIntfStack.Contains(AObject: IInterface): Boolean;
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
  for I := 0 to FCount - 1 do
    if FElements[I] = AObject then
    begin
      Result := True;
      Exit;
    end;
end;

constructor TIntfStack.Create;
begin
  Create(16);
end;

constructor TIntfStack.Create(Capacity: Integer);
begin
  inherited Create;
  FCount := 0;
  FCapacity := Capacity;
  SetLength(FElements, FCapacity);
end;

function TIntfStack.Empty: Boolean;
begin
  Result := FCount = 0;
end;

procedure TIntfStack.Grow;
begin
  FCapacity := FCapacity + FCapacity div 4;
  SetLength(FElements, FCapacity);
end;

function TIntfStack.Pop: IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if FCount = 0 then
    Exit;
  Dec(FCount);
  Result := FElements[FCount];
end;

procedure TIntfStack.Push(AObject: IInterface);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if AObject = nil then
    Exit;
  if FCount = FCapacity then
    Grow;
  FElements[FCount] := AObject;
  Inc(FCount);
end;

function TIntfStack.Size: Integer;
begin
  Result := FCount;
end;

{ TStrStack }

function TStrStack.Contains(const AString: string): Boolean;
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
  for I := 0 to FCount - 1 do
    if FElements[I] = AString then
    begin
      Result := True;
      Exit;
    end;
end;

constructor TStrStack.Create;
begin
  Create(16);
end;

constructor TStrStack.Create(Capacity: Integer);
begin
  inherited Create;
  FCount := 0;
  FCapacity := Capacity;
  SetLength(FElements, FCapacity);
end;

function TStrStack.Empty: Boolean;
begin
  Result := FCount = 0;
end;

procedure TStrStack.Grow;
begin
  FCapacity := FCapacity + FCapacity div 4;
  SetLength(FElements, FCapacity);
end;

function TStrStack.Pop: string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if FCount = 0 then
    Exit;
  Dec(FCount);
  Result := FElements[FCount];
end;

procedure TStrStack.Push(const AString: string);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if AString = '' then
    Exit;
  if FCount = FCapacity then
    Grow;
  FElements[FCount] := AString;
  Inc(FCount);
end;

function TStrStack.Size: Integer;
begin
  Result := FCount;
end;

{ TStack }

function TStack.Contains(AObject: TObject): Boolean;
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
  for I := 0 to FCount - 1 do
    if FElements[I] = AObject then
    begin
      Result := True;
      Exit;
    end;
end;

constructor TStack.Create;
begin
  Create(16);
end;

constructor TStack.Create(Capacity: Integer);
begin
  inherited Create;
  FCount := 0;
  FCapacity := Capacity;
  SetLength(FElements, FCapacity);
end;

function TStack.Empty: Boolean;
begin
  Result := FCount = 0;
end;

procedure TStack.Grow;
begin
  FCapacity := FCapacity + FCapacity div 4;
  SetLength(FElements, FCapacity);
end;

function TStack.Pop: TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := nil;
  if FCount = 0 then
    Exit;
  Dec(FCount);
  Result := FElements[FCount];
end;

procedure TStack.Push(AObject: TObject);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if AObject = nil then
    Exit;
  if FCount = FCapacity then
    Grow;
  FElements[FCount] := AObject;
  Inc(FCount);
end;

function TStack.Size: Integer;
begin
  Result := FCount;
end;

end.
