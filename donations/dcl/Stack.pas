//------------------------------------------------------------------------------
// The Delphi Container Library
// Jean-Philippe BEMPEL aka RDM
// rdm_30@yahoo.com
//------------------------------------------------------------------------------
unit Stack;

{$I dcl.inc}

interface

uses
  DCL_intf, DCLUtil, AbstractContainer;

type
  TIntfStack = class(TAbstractContainer, IIntfStack)
  private
    FElements: TIInterfaceArray;
    FCount: Integer;
    FCapacity: Integer;
  protected
    procedure Grow; virtual;
    { IIntfStack }
    function Contains(AObject: IInterface): Boolean;
    function Empty: Boolean;
    function Pop: IInterface;
    procedure Push(AObject: IInterface);
    function Size: Integer;
  public
    constructor Create(Capacity: Integer = DCLDefaultCapacity);
  end;

  TStrStack = class(TAbstractContainer, IStrStack)
  private
    FElements: TStringArray;
    FCount: Integer;
    FCapacity: Integer;
  protected
    procedure Grow; virtual;
    { IStrStack }
    function Contains(const AString: string): Boolean;
    function Empty: Boolean;
    function Pop: string;
    procedure Push(const AString: string);
    function Size: Integer;
  public
    constructor Create(Capacity: Integer = DCLDefaultCapacity);
  end;

  TStack = class(TAbstractContainer, IStack)
  private
    FElements: TObjectArray;
    FCount: Integer;
    FCapacity: Integer;
  protected
    procedure Grow; virtual;
    { IStack }
    function Contains(AObject: TObject): Boolean;
    function Empty: Boolean;
    function Pop: TObject;
    procedure Push(AObject: TObject);
    function Size: Integer;
  public
    constructor Create(Capacity: Integer = DCLDefaultCapacity);
  end;

implementation

//=== { TIntfStack } =========================================================

constructor TIntfStack.Create(Capacity: Integer = DCLDefaultCapacity);
begin
  inherited Create;
  FCount := 0;
  FCapacity := Capacity;
  SetLength(FElements, FCapacity);
end;

function TIntfStack.Contains(AObject: IInterface): Boolean;
var
  I: Integer;
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
  for I := 0 to FCount - 1 do
    if FElements[I] = AObject then
    begin
      Result := True;
      Break;
    end;
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
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if FCount = 0 then
    Exit;
  Dec(FCount);
  Result := FElements[FCount];
end;

procedure TIntfStack.Push(AObject: IInterface);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
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

//=== { TStrStack } ==========================================================

constructor TStrStack.Create(Capacity: Integer = DCLDefaultCapacity);
begin
  inherited Create;
  FCount := 0;
  FCapacity := Capacity;
  SetLength(FElements, FCapacity);
end;

function TStrStack.Contains(const AString: string): Boolean;
var
  I: Integer;
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
  for I := 0 to FCount - 1 do
    if FElements[I] = AString then
    begin
      Result := True;
      Exit;
    end;
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
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
  if FCount = 0 then
    Exit;
  Dec(FCount);
  Result := FElements[FCount];
end;

procedure TStrStack.Push(const AString: string);
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
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

//=== { TStack } =============================================================

constructor TStack.Create(Capacity: Integer = DCLDefaultCapacity);
begin
  inherited Create;
  FCount := 0;
  FCapacity := Capacity;
  SetLength(FElements, FCapacity);
end;

function TStack.Contains(AObject: TObject): Boolean;
var
  I: Integer;
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
  for I := 0 to FCount - 1 do
    if FElements[I] = AObject then
    begin
      Result := True;
      Break;
    end;
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
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
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
{$ENDIF THREADSAFE}
begin
  {$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
  {$ENDIF THREADSAFE}
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
