//------------------------------------------------------------------------------
// The Delphi Container Library
// Jean-Philippe BEMPEL aka RDM
// rdm_30@yahoo.com
//------------------------------------------------------------------------------
unit Queue;

{$I dcl.inc}

interface

uses DCL_intf, DCLUtil, AbstractContainer;

type
  TIntfQueue = class(TAbstractContainer, IIntfQueue)
  private
    FCapacity: Integer;
    FElements: TIInterfaceArray;
    FHead: Integer;
    FTail: Integer;
  protected
  { IIntfQueue }
    function Contains(AObject: IInterface): Boolean;
    function Dequeue: IInterface;
    function Empty: Boolean;
    procedure Enqueue(AObject: IInterface);
    function Size: Integer;
  public
    constructor Create; overload;
    constructor Create(Capacity: Integer); overload;
  end;

  TStrQueue = class(TAbstractContainer, IStrQueue)
  private
    FCapacity: Integer;
    FElements: TStringArray;
    FHead: Integer;
    FTail: Integer;
  protected
  { IStrQueue }
    function Contains(const AString: string): Boolean;
    function Dequeue: string;
    function Empty: Boolean;
    procedure Enqueue(const AString: string);
    function Size: Integer;
  public
    constructor Create; overload;
    constructor Create(Capacity: Integer); overload;
  end;

  TQueue = class(TAbstractContainer, IQueue)
  private
    FCapacity: Integer;
    FElements: TObjectArray;
    FHead: Integer;
    FTail: Integer;
  protected
  { IQueue }
    function Contains(AObject: TObject): Boolean;
    function Dequeue: TObject;
    function Empty: Boolean;
    procedure Enqueue(AObject: TObject);
    function Size: Integer;
  public
    constructor Create; overload;
    constructor Create(Capacity: Integer); overload;
  end;

implementation

{ TIntfQueue }

function TIntfQueue.Contains(AObject: IInterface): Boolean;
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
  I := FHead;
  while I <> FTail do
  begin
    if FElements[I] = AObject then
    begin
      Result := True;
      Exit;
    end;
    I := (I + 1) mod FCapacity;
  end;
end;

constructor TIntfQueue.Create;
begin
  Create(16);
end;

constructor TIntfQueue.Create(Capacity: Integer);
begin
  inherited Create;
  FHead := 0;
  FTail := 0;
  FCapacity := Capacity;
  SetLength(FElements, FCapacity);
end;

function TIntfQueue.Dequeue: IInterface;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := nil;
  if FTail = FHead then
    Exit;
  Result := FElements[FHead];
  FElements[FHead] := nil;
  FHead := (FHead + 1) mod FCapacity;
end;

function TIntfQueue.Empty: Boolean;
begin
  Result := FTail = FHead;
end;

procedure TIntfQueue.Enqueue(AObject: IInterface);
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
  FElements[FTail] := AObject;
  FTail := (FTail + 1) mod FCapacity;
end;

function TIntfQueue.Size: Integer;
begin
  Result := FTail - FHead;
end;

{ TStrQueue }

function TStrQueue.Contains(const AString: string): Boolean;
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
  I := FHead;
  while I <> FTail do
  begin
    if FElements[I] = AString then
    begin
      Result := True;
      Exit;
    end;
    I := (I + 1) mod FCapacity;
  end;
end;

constructor TStrQueue.Create;
begin
  Create(16);
end;

constructor TStrQueue.Create(Capacity: Integer);
begin
  inherited Create;
  FHead := 0;
  FTail := 0;
  FCapacity := Capacity;
  SetLength(FElements, FCapacity);
end;

function TStrQueue.Dequeue: string;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := '';
  if FTail = FHead then
    Exit;
  Result := FElements[FHead];
  FElements[FHead] := '';
  FHead := (FHead + 1) mod FCapacity;
end;

function TStrQueue.Empty: Boolean;
begin
  Result := FTail = FHead;
end;

procedure TStrQueue.Enqueue(const AString: string);
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
  FElements[FTail] := AString;
  FTail := (FTail + 1) mod FCapacity;
end;

function TStrQueue.Size: Integer;
begin
  Result := FTail - FHead;
end;

{ TQueue }

function TQueue.Contains(AObject: TObject): Boolean;
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
  I := FHead;
  while I <> FTail do
  begin
    if FElements[I] = AObject then
    begin
      Result := True;
      Exit;
    end;
    I := (I + 1) mod FCapacity;
  end;
end;

constructor TQueue.Create(Capacity: Integer);
begin
  inherited Create;
  FCapacity := Capacity;
  SetLength(FElements, FCapacity);
end;

constructor TQueue.Create;
begin
  Create(16);
end;

function TQueue.Dequeue: TObject;
{$IFDEF THREADSAFE}
var
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := nil;
  if FTail = FHead then
    Exit;
  Result := FElements[FHead];
  FElements[FHead] := nil;
  FHead := (FHead + 1) mod FCapacity;
end;

function TQueue.Empty: Boolean;
begin
  Result := FTail = FHead;
end;

procedure TQueue.Enqueue(AObject: TObject);
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
  FElements[FTail] := AObject;
  FTail := (FTail + 1) mod FCapacity;
end;

function TQueue.Size: Integer;
begin
  Result := FTail - FHead;
end;

end.
