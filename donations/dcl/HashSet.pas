//------------------------------------------------------------------------------
// The Delphi Container Library
// Jean-Philippe BEMPEL aka RDM
// rdm_30@yahoo.com
//------------------------------------------------------------------------------
unit HashSet;

{$I dcl.inc}

interface

uses
  DCL_intf, HashMap, AbstractContainer;

type
  TIntfHashSet = class(TAbstractContainer, IIntfCollection, IIntfSet, IIntfCloneable)
  private
    FMap: IIntfIntfMap;
  protected
    { IIntfCollection }
    function Add(AObject: IInterface): Boolean;
    function AddAll(ACollection: IIntfCollection): Boolean;
    procedure Clear;
    function Contains(AObject: IInterface): Boolean;
    function ContainsAll(ACollection: IIntfCollection): Boolean;
    function Equals(ACollection: IIntfCollection): Boolean;
    function First: IIntfIterator;
    function IsEmpty: Boolean;
    function Last: IIntfIterator;
    function Remove(AObject: IInterface): Boolean;
    function RemoveAll(ACollection: IIntfCollection): Boolean;
    function RetainAll(ACollection: IIntfCollection): Boolean;
    function Size: Integer;
    { IIntfSet }
    procedure Intersect(ACollection: IIntfCollection);
    procedure Subtract(ACollection: IIntfCollection);
    procedure Union(ACollection: IIntfCollection);
    { IIntfCloneable }
    function Clone: IInterface;
  public
    constructor Create; overload;
    constructor Create(Capacity: Integer); overload;
    destructor Destroy; override;
  end;

  TStrHashSet = class(TAbstractContainer, IStrCollection, IStrSet, ICloneable)
  private
    FMap: IStrMap;
  protected
    { IStrCollection }
    function Add(const AString: string): Boolean;
    function AddAll(ACollection: IStrCollection): Boolean;
    procedure Clear;
    function Contains(const AString: string): Boolean;
    function ContainsAll(ACollection: IStrCollection): Boolean;
    function Equals(ACollection: IStrCollection): Boolean;
    function First: IStrIterator;
    function IsEmpty: Boolean;
    function Last: IStrIterator;
    function Remove(const AString: string): Boolean;
    function RemoveAll(ACollection: IStrCollection): Boolean;
    function RetainAll(ACollection: IStrCollection): Boolean;
    function Size: Integer;
    { IIntfSet }
    procedure Intersect(ACollection: IStrCollection);
    procedure Subtract(ACollection: IStrCollection);
    procedure Union(ACollection: IStrCollection);
    { IIntfCloneable }
    function Clone: TObject;
  public
    constructor Create; overload;
    constructor Create(Capacity: Integer); overload;
    destructor Destroy; override;
  end;

  THashSet = class(TAbstractContainer, ICollection, ISet, ICloneable)
  private
    FMap: IMap;
  protected
    { ICollection }
    function Add(AObject: TObject): Boolean;
    function AddAll(ACollection: ICollection): Boolean;
    procedure Clear;
    function Contains(AObject: TObject): Boolean;
    function ContainsAll(ACollection: ICollection): Boolean;
    function Equals(ACollection: ICollection): Boolean;
    function First: IIterator;
    function IsEmpty: Boolean;
    function Last: IIterator;
    function Remove(AObject: TObject): Boolean;
    function RemoveAll(ACollection: ICollection): Boolean;
    function RetainAll(ACollection: ICollection): Boolean;
    function Size: Integer;
    { ISet }
    procedure Intersect(ACollection: ICollection);
    procedure Subtract(ACollection: ICollection);
    procedure Union(ACollection: ICollection);
    { ICloneable }
    function Clone: TObject;
  public
    constructor Create; overload;
    constructor Create(Capacity: Integer; AOwnsObject: Boolean); overload;
    destructor Destroy; override;
  end;

implementation

const
  // (rom) this needs an explanation
  RefUnique: TObject = @RefUnique;

var
  IRefUnique: IInterface = nil;

//=== { TIntfHashSet } =======================================================

constructor TIntfHashSet.Create;
begin
  Create(16);
end;

constructor TIntfHashSet.Create(Capacity: Integer);
begin
  inherited Create;
  FMap := TIntfIntfHashMap.Create(Capacity);
  if IRefUnique = nil then
    IRefUnique := TInterfacedObject.Create;
end;

destructor TIntfHashSet.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TIntfHashSet.Add(AObject: IInterface): Boolean;
begin
  Result := False;
  if FMap.ContainsKey(AObject) then
    Exit;
  FMap.PutValue(AObject, IRefUnique);
  Result := True;
end;

function TIntfHashSet.AddAll(ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
begin
  Result := ACollection <> nil;
  if Result then
  begin
    It := ACollection.First;
    while It.HasNext do
      Add(It.Next);
  end;
end;

procedure TIntfHashSet.Clear;
begin
  FMap.Clear;
end;

function TIntfHashSet.Clone: IInterface;
var
  NewSet: TIntfHashSet;
begin
  NewSet := TIntfHashSet.Create;
  NewSet.FMap := IIntfIntfMap(IIntfCloneable(FMap).Clone);
  Result := NewSet;
end;

function TIntfHashSet.Contains(AObject: IInterface): Boolean;
begin
  Result := FMap.ContainsKey(AObject);
end;

function TIntfHashSet.ContainsAll(ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
begin
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    if not Contains(It.Next) then
    begin
      Result := False;
      Break;
    end;
end;

function TIntfHashSet.Equals(ACollection: IIntfCollection): Boolean;
var
  It: IIntfIterator;
  ItMap: IIntfIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  if FMap.Size <> ACollection.Size then
    Exit;
  It := ACollection.First;
  ItMap := FMap.Values.First;
  while ItMap.HasNext do
    if ItMap.Next <> It.Next then
      Exit;
  Result := True;
end;

function TIntfHashSet.First: IIntfIterator;
begin
  Result := FMap.KeySet.First;
end;

procedure TIntfHashSet.Intersect(ACollection: IIntfCollection);
begin
  RetainAll(ACollection);
end;

function TIntfHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TIntfHashSet.Last: IIntfIterator;
begin
  Result := FMap.KeySet.Last;
end;

function TIntfHashSet.Remove(AObject: IInterface): Boolean;
begin
  Result := FMap.Remove(AObject) = IInterface(IRefUnique);
end;

function TIntfHashSet.RemoveAll(ACollection: IIntfCollection): Boolean;
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

function TIntfHashSet.RetainAll(ACollection: IIntfCollection): Boolean;
var
  ItMap: IIntfIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  ItMap := FMap.Values.First;
  while ItMap.HasNext do
    if not ACollection.Contains(ItMap.Next) then
      ItMap.Remove;
end;

function TIntfHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TIntfHashSet.Subtract(ACollection: IIntfCollection);
begin
  RemoveAll(ACollection);
end;

procedure TIntfHashSet.Union(ACollection: IIntfCollection);
begin
  AddAll(ACollection);
end;

//=== { TStrHashSet } ========================================================

constructor TStrHashSet.Create(Capacity: Integer);
begin
  inherited Create;
  FMap := TStrHashMap.Create(Capacity, False);
end;

constructor TStrHashSet.Create;
begin
  Create(16);
end;

destructor TStrHashSet.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TStrHashSet.Add(const AString: string): Boolean;
begin
  Result := False;
  if FMap.ContainsKey(AString) then
    Exit;
  FMap.PutValue(AString, RefUnique);
  Result := True;
end;

function TStrHashSet.AddAll(ACollection: IStrCollection): Boolean;
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

procedure TStrHashSet.Clear;
begin
  FMap.Clear;
end;

function TStrHashSet.Clone: TObject;
var
  NewSet: TStrHashSet;
begin
  NewSet := TStrHashSet.Create;
  NewSet.FMap := TStrHashMap(ICloneable(FMap).Clone);
  Result := NewSet;
end;

function TStrHashSet.Contains(const AString: string): Boolean;
begin
  Result := FMap.ContainsKey(AString);
end;

function TStrHashSet.ContainsAll(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
begin
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    if not contains(It.Next) then
    begin
      Result := False;
      Break;
    end;
end;

function TStrHashSet.Equals(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
  ItMap: IStrIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  if FMap.Size <> ACollection.Size then
    Exit;
  It := ACollection.First;
  ItMap := FMap.KeySet.First;
  while ItMap.HasNext do
    if ItMap.Next <> It.Next then
      Exit;
  Result := True;
end;

function TStrHashSet.First: IStrIterator;
begin
  Result := FMap.KeySet.First;
end;

procedure TStrHashSet.Intersect(ACollection: IStrCollection);
begin
  RetainAll(ACollection);
end;

function TStrHashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function TStrHashSet.Last: IStrIterator;
begin
  Result := FMap.KeySet.Last;
end;

function TStrHashSet.Remove(const AString: string): Boolean;
begin
  Result := Fmap.Remove(AString) = RefUnique;
end;

function TStrHashSet.RemoveAll(ACollection: IStrCollection): Boolean;
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

function TStrHashSet.RetainAll(ACollection: IStrCollection): Boolean;
var
  It: IStrIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    if not FMap.ContainsKey(It.Next) then
      FMap.Remove(It.Next);
end;

function TStrHashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure TStrHashSet.Subtract(ACollection: IStrCollection);
begin
  RemoveAll(ACollection);
end;

procedure TStrHashSet.Union(ACollection: IStrCollection);
begin
  AddAll(ACollection);
end;

//=== { THashSet } ===========================================================

constructor THashSet.Create;
begin
  Create(16, False);
end;

constructor THashSet.Create(Capacity: Integer; AOwnsObject: Boolean);
begin
  inherited Create;
  FMap := THashMap.Create(Capacity, AOwnsObject);
end;

destructor THashSet.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function THashSet.Add(AObject: TObject): Boolean;
begin
  Result := False;
  if FMap.ContainsKey(AObject) then
    Exit;
  FMap.PutValue(AObject, RefUnique);
  Result := True;
end;

function THashSet.AddAll(ACollection: ICollection): Boolean;
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

procedure THashSet.Clear;
begin
  FMap.Clear;
end;

function THashSet.Clone: TObject;
var
  NewSet: THashSet;
begin
  NewSet := THashSet.Create;
  NewSet.FMap := THashMap(ICloneable(FMap).Clone);
  Result := NewSet;
end;

function THashSet.Contains(AObject: TObject): Boolean;
begin
  Result := FMap.ContainsKey(AObject);
end;

function THashSet.ContainsAll(ACollection: ICollection): Boolean;
var
  It: IIterator;
begin
  Result := True;
  if ACollection = nil then
    Exit;
  It := ACollection.First;
  while It.HasNext do
    if not Contains(It.Next) then
    begin
      Result := False;
      Break;
    end;
end;

function THashSet.Equals(ACollection: ICollection): Boolean;
var
  It: IIterator;
  ItMap: IIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  if FMap.Size <> ACollection.Size then
    Exit;
  It := ACollection.First;
  ItMap := FMap.Values.First;
  while ItMap.HasNext do
    if ItMap.Next <> It.Next then
      Exit;
  Result := True;
end;

function THashSet.First: IIterator;
begin
  Result := FMap.KeySet.First;
end;

procedure THashSet.Intersect(ACollection: ICollection);
begin
  RetainAll(ACollection);
end;

function THashSet.IsEmpty: Boolean;
begin
  Result := FMap.IsEmpty;
end;

function THashSet.Last: IIterator;
begin
  Result := FMap.KeySet.Last;
end;

function THashSet.Remove(AObject: TObject): Boolean;
begin
  Result := FMap.Remove(AObject) = RefUnique;
end;

function THashSet.RemoveAll(ACollection: ICollection): Boolean;
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

function THashSet.RetainAll(ACollection: ICollection): Boolean;
var
  ItMap: IIterator;
begin
  Result := False;
  if ACollection = nil then
    Exit;
  ItMap := FMap.Values.First;
  while ItMap.HasNext do
    if not ACollection.Contains(ItMap.Next) then
      ItMap.Remove;
end;

function THashSet.Size: Integer;
begin
  Result := FMap.Size;
end;

procedure THashSet.Subtract(ACollection: ICollection);
begin
  RemoveAll(ACollection);
end;

procedure THashSet.Union(ACollection: ICollection);
begin
  AddAll(ACollection);
end;

end.

