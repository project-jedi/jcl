//------------------------------------------------------------------------------
// The Delphi Container Library
// Jean-Philippe BEMPEL aka RDM
// rdm_30@yahoo.com
//------------------------------------------------------------------------------
unit HashMap;

{$I dcl.inc}

interface

uses DCL_intf, AbstractContainer;

type
  TIntfIntfEntry = record
    Key: IInterface;
    Value: IInterface;
  end;

  TStrIntfEntry = record
    Key: string;
    Value: IInterface;
  end;

  TStrStrEntry = record
    Key: string;
    Value: string;
  end;

  TStrEntry = record
    Key: string;
    Value: TObject
  end;

  TEntry = record
    Key: TObject;
    Value: TObject;
  end;

  TIntfIntfEntryArray = array of TIntfIntfEntry;
  TStrIntfEntryArray = array of TStrIntfEntry;
  TStrStrEntryArray = array of TStrStrEntry;
  TStrEntryArray = array of TStrEntry;
  TEntryArray = array of TEntry;

  PIntfIntfBucket = ^TIntfIntfBucket;
  TIntfIntfBucket = record
    Count: Integer;
    Entries: TIntfIntfEntryArray;
  end;

  PStrIntfBucket = ^TStrIntfBucket;
  TStrIntfBucket = record
    Count: Integer;
    Entries: TStrIntfEntryArray;
  end;

  PStrStrBucket = ^TStrStrBucket;
  TStrStrBucket = record
    Count: Integer;
    Entries: TStrStrEntryArray;
  end;

  PStrBucket = ^TStrBucket;
  TStrBucket = record
    Count: Integer;
    Entries: TStrEntryArray;
  end;

  PBucket = ^TBucket;
  TBucket = record
    Count: Integer;
    Entries: TEntryArray;
  end;

  TIntfIntfBucketArray = array of TIntfIntfBucket;
  TStrIntfBucketArray = array of TStrIntfBucket;
  TStrStrBucketArray = array of TStrStrBucket;
  TStrBucketArray = array of TStrBucket;
  TBucketArray = array of TBucket;

  // Hash Function
  THashFunction = function (Key: Cardinal): Cardinal of object;

	TIntfIntfHashMap = class(TAbstractContainer, IIntfIntfMap, IIntfCloneable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TIntfIntfBucketArray;
    FHashFunction: THashFunction;
    function HashMul(Key: Cardinal): Cardinal;
  protected
    procedure GrowEntries(BucketIndex: Integer); virtual;
  protected
  { IIntfIntfMap }
    procedure Clear;
    function ContainsKey(Key: IInterface): Boolean;
    function ContainsValue(Value: IInterface): Boolean;
    function Equals(AMap: IIntfIntfMap): Boolean;
    function GetValue(Key: IInterface): IInterface;
    function IsEmpty: Boolean;
    function KeySet: IIntfSet;
    procedure PutAll(AMap: IIntfIntfMap);
    procedure PutValue(Key, Value: IInterface);
    function Remove(Key: IInterface): IInterface;
  	function Size: Integer;
    function Values: IIntfCollection;
  protected
  { IIntfCloneable }
  	function Clone: IInterface;
  public
    constructor Create; overload;
    constructor Create(Capacity: Integer); overload;
    destructor Destroy; override;
    property HashFunction: THashFunction read FHashFunction write FHashFunction;
  end;

	TStrIntfHashMap = class(TAbstractContainer, IStrIntfMap, IIntfCloneable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TStrIntfBucketArray;
    FHashFunction: THashFunction;
    function HashMul(Key: Cardinal): Cardinal;
    function HashString(const Key: string): Cardinal;
  protected
    procedure GrowEntries(BucketIndex: Integer); virtual;
  protected
  { IIntfMap }
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(Value: IInterface): Boolean;
    function Equals(AMap: IStrIntfMap): Boolean;
    function GetValue(const Key: string): IInterface;
    function IsEmpty: Boolean;
    function KeySet: IStrSet;
    procedure PutAll(AMap: IStrIntfMap);
    procedure PutValue(const Key: string; Value: IInterface);
    function Remove(const Key: string): IInterface;
  	function Size: Integer;
    function Values: IIntfCollection;
  protected
  { IIntfCloneable }
  	function Clone: IInterface;
  public
    constructor Create; overload;
    constructor Create(Capacity: Integer); overload;
    destructor Destroy; override;
    property HashFunction: THashFunction read FHashFunction write FHashFunction;
  end;

	TStrStrHashMap = class(TAbstractContainer, IStrStrMap, IIntfCloneable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TStrStrBucketArray;
    FHashFunction: THashFunction;
    function HashMul(Key: Cardinal): Cardinal;
    function HashString(const Key: string): Cardinal;
  protected
    procedure GrowEntries(BucketIndex: Integer); virtual;
  protected
  { IStrStrMap }
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(const Value: string): Boolean;
    function Equals(AMap: IStrStrMap): Boolean;
    function GetValue(const Key: string): string;
    function IsEmpty: Boolean;
    function KeySet: IStrSet;
    procedure PutAll(AMap: IStrStrMap);
    procedure PutValue(const Key, Value: string);
    function Remove(const Key: string): string;
  	function Size: Integer;
    function Values: IStrCollection;
  protected
  { IIntfCloneable }
  	function Clone: IInterface;
  public
    constructor Create; overload;
    constructor Create(Capacity: Integer); overload;
    destructor Destroy; override;
    property HashFunction: THashFunction read FHashFunction write FHashFunction;
  end;

	TStrHashMap = class(TAbstractContainer, IStrMap, ICloneable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TStrBucketArray;
    FHashFunction: THashFunction;
    FOwnsObjects: Boolean;
    function HashMul(Key: Cardinal): Cardinal;
    function HashString(const Key: string): Cardinal;
  protected
    procedure GrowEntries(BucketIndex: Integer); virtual;
    procedure FreeObject(AObject: TObject);
  protected
  { IStrMap }
    procedure Clear;
    function ContainsKey(const Key: string): Boolean;
    function ContainsValue(Value: TObject): Boolean;
    function Equals(AMap: IStrMap): Boolean;
    function GetValue(const Key: string): TObject;
    function IsEmpty: Boolean;
    function KeySet: IStrSet;
    procedure PutAll(AMap: IStrMap);
    procedure PutValue(const Key: string; Value: TObject);
    function Remove(const Key: string): TObject;
  	function Size: Integer;
    function Values: ICollection;
  protected
  { ICloneable }
  	function Clone: TObject;
  public
    constructor Create; overload;
    constructor Create(Capacity: Integer; AOwnsObjects: Boolean); overload;
    destructor Destroy; override;
    property HashFunction: THashFunction read FHashFunction write FHashFunction;
  end;

	THashMap = class(TAbstractContainer, IMap, ICloneable)
  private
    FCapacity: Integer;
    FCount: Integer;
    FBuckets: TBucketArray;
    FHashFunction: THashFunction;
    FOwnsObjects: Boolean;
    function HashMul(Key: Cardinal): Cardinal;
  protected
    procedure GrowEntries(BucketIndex: Integer); virtual;
    procedure FreeObject(AObject: TObject);
  public
  { IMap }
    procedure Clear;
    function ContainsKey(Key: TObject): Boolean; 
    function ContainsValue(Value: TObject): Boolean;
    function Equals(AMap: IMap): Boolean;
    function GetValue(Key: TObject): TObject;
    function IsEmpty: Boolean;
    function KeySet: ISet;
    procedure PutAll(AMap: IMap);
    procedure PutValue(Key, Value: TObject);
    function Remove(Key: TObject): TObject;
  	function Size: Integer;
    function Values: ICollection;
  protected
  { ICloneable }
  	function Clone: TObject;
  public
    constructor Create; overload;
    constructor Create(Capacity: Integer; AOwnsObjects: Boolean); overload;
    destructor Destroy; override;
    property HashFunction: THashFunction read FHashFunction write FHashFunction;
  end;

implementation

uses SysUtils, ArraySet, ArrayList;

{ TIntfIntfHashMap }

procedure TIntfIntfHashMap.Clear;
var
  I, J: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  for I := 0 to FCapacity - 1 do
  begin
  	for J := 0 to FBuckets[I].Count-1 do
    begin
      FBuckets[I].Entries[J].Key := nil;
			FBuckets[I].Entries[J].Value := nil;
    end;
    FBuckets[I].Count := 0;
  end;
  FCount := 0;
end;

function TIntfIntfHashMap.Clone: IInterface;
var
  I, J: Integer;
  NewEntryArray: TIntfIntfEntryArray;
  NewMap: TIntfIntfHashMap;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  NewMap := TIntfIntfHashMap.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
  begin
    NewEntryArray := NewMap.FBuckets[I].Entries;
    SetLength(NewEntryArray, Length(FBuckets[I].Entries));
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      NewEntryArray[J].Key := FBuckets[I].Entries[J].Key;
      NewEntryArray[J].Value := FBuckets[I].Entries[J].Value;
    end;
    NewMap.FBuckets[I].Count := FBuckets[I].Count;
  end;
  Result := NewMap;
end;

function TIntfIntfHashMap.ContainsKey(Key: IInterface): Boolean;
var
  I: Integer;
  Bucket: PIntfIntfBucket;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if Key = nil then
    Exit;
  Bucket := @(FBuckets[FHashFunction(Integer(Key))]);
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := True;
      Exit;
    end;
end;

function TIntfIntfHashMap.ContainsValue(Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: PIntfIntfBucket;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if Value = nil then
    Exit;
  for J := 0 to FCapacity - 1 do
  begin
    Bucket := @(FBuckets[J]);
    for I := 0 to Bucket.Count - 1 do
      if Bucket.Entries[I].Value = Value then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

constructor TIntfIntfHashMap.Create(Capacity: Integer);
var
  I: Integer;
begin
  inherited Create;
  FCapacity := Capacity;
  SetLength(FBuckets, FCapacity);
  for I := 0 to FCapacity - 1 do
    SetLength(FBuckets[I].Entries, 1);
  FHashFunction := HashMul;
end;

constructor TIntfIntfHashMap.Create;
begin
  Create(16);
end;

destructor TIntfIntfHashMap.Destroy;
begin
  Clear;
  inherited;
end;

function TIntfIntfHashMap.Equals(AMap: IIntfIntfMap): Boolean;
var
  I, J: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if AMap = nil then
    Exit;
  if FCount <> AMap.Size then
    Exit;
  Result := True;
  for I := 0 to FCapacity - 1 do
  begin
    for J := 0 to FBuckets[I].Count - 1 do
      if AMap.ContainsKey(FBuckets[I].Entries[J].Key) then
      begin
        if not (AMap.GetValue(FBuckets[I].Entries[J].Key) = FBuckets[I].Entries[J].Value) then
        begin
          Result := False;
          Exit;
        end;
      end
      else
      begin
        Result := False;
        Exit;
      end;
  end;
end;

function TIntfIntfHashMap.GetValue(Key: IInterface): IInterface;
var
  I: Integer;
  Bucket: PIntfIntfBucket;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := nil;
  if Key = nil then
    Exit;
  Bucket := @(FBuckets[FHashFunction(Integer(Key))]);
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      Exit;
    end;
end;

procedure TIntfIntfHashMap.GrowEntries(BucketIndex: Integer);
var
  Capacity: Integer;
begin
  Capacity := Length(FBuckets[BucketIndex].Entries);
  if Capacity > 64 then
	  Capacity := Capacity + Capacity div 4
  else
    Capacity := Capacity * 4;
  SetLength(FBuckets[BucketIndex].Entries, Capacity);
end;

function TIntfIntfHashMap.HashMul(Key: Cardinal): Cardinal;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(FCapacity * (Frac(Key * A)));
end;

function TIntfIntfHashMap.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TIntfIntfHashMap.KeySet: IIntfSet;
var
  I, J: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := TIntfArraySet.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
	  	Result.Add(FBuckets[I].Entries[J].Key)
end;

procedure TIntfIntfHashMap.PutAll(AMap: IIntfIntfMap);
var
  It: IIntfIterator;
  Key: IInterface;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if AMap = nil then
    Exit;
  It := AMap.KeySet.First;
  while It.HasNext do
  begin
    Key := It.Next;
    PutValue(Key, AMap.GetValue(Key));
  end;
end;

procedure TIntfIntfHashMap.PutValue(Key, Value: IInterface);
var
  Index: Integer;
  Bucket: PIntfIntfBucket;
  I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if Key = nil then
    Exit;
  if Value = nil then
    Exit;
  Index := FHashFunction(Integer(Key));
  Bucket := @(FBuckets[Index]);
  for I := 0 to Bucket.Count - 1 do
  	if Bucket.Entries[I].Key = Key then
    begin
			Bucket.Entries[I].Value := Value;
      Exit;
    end;
  if Bucket.Count = Length(Bucket.Entries) then
    GrowEntries(Index);
  Bucket.Entries[Bucket.Count].Key := Key;
  Bucket.Entries[Bucket.Count].Value := Value;
  Inc(Bucket.Count);
  Inc(FCount);
end;

function TIntfIntfHashMap.Remove(Key: IInterface): IInterface;
var
  Bucket: PIntfIntfBucket;
  I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := nil;
  if Key = nil then
    Exit;
  Bucket := @(FBuckets[FHashFunction(Integer(Key))]);
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      if I < (Length(Bucket.Entries)-1) then
	      System.Move(Bucket.Entries[I + 1], Bucket.Entries[I],
  	    	(Bucket.Count - I) * SizeOf(TStrStrEntry));
      Dec(Bucket.Count);
      Exit;
    end;
end;

function TIntfIntfHashMap.Size: Integer;
begin
  Result := FCount;
end;

function TIntfIntfHashMap.Values: IIntfCollection;
var
  I, J: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := TIntfArrayList.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Value);
end;

{ TStrIntfHashMap }

procedure TStrIntfHashMap.Clear;
var
  I, J: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  for I := 0 to FCapacity - 1 do
  begin
  	for J := 0 to FBuckets[I].Count-1 do
    begin
      FBuckets[I].Entries[J].Key := '';
			FBuckets[I].Entries[J].Value := nil;
    end;
    FBuckets[I].Count := 0;
  end;
  FCount := 0;
end;

function TStrIntfHashMap.Clone: IInterface;
var
  I, J: Integer;
  NewEntryArray: TStrIntfEntryArray;
  NewMap: TStrIntfHashMap;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  NewMap := TStrIntfHashMap.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
  begin
    NewEntryArray := NewMap.FBuckets[I].Entries;
    SetLength(NewEntryArray, Length(FBuckets[I].Entries));
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      NewEntryArray[J].Key := FBuckets[I].Entries[J].Key;
      NewEntryArray[J].Value := FBuckets[I].Entries[J].Value;
    end;
    NewMap.FBuckets[I].Count := FBuckets[I].Count;
  end;
  Result := NewMap;
end;

function TStrIntfHashMap.ContainsKey(const Key: string): Boolean;
var
  I: Integer;
  Bucket: PStrIntfBucket;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if Key = '' then
    Exit;
  Bucket := @(FBuckets[FHashFunction(HashString(Key))]);
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := True;
      Exit;
    end;
end;

function TStrIntfHashMap.ContainsValue(Value: IInterface): Boolean;
var
  I, J: Integer;
  Bucket: PStrIntfBucket;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if Value = nil then
    Exit;
  for J := 0 to FCapacity - 1 do
  begin
    Bucket := @(FBuckets[J]);
    for I := 0 to Bucket.Count - 1 do
      if Bucket.Entries[I].Value = Value then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

constructor TStrIntfHashMap.Create;
begin
  Create(16);
end;

constructor TStrIntfHashMap.Create(Capacity: Integer);
var
  I: Integer;
begin
  inherited Create;
  FCapacity := Capacity;
  SetLength(FBuckets, FCapacity);
  for I := 0 to FCapacity - 1 do
    SetLength(FBuckets[I].Entries, 1);
  FHashFunction := HashMul;
end;

destructor TStrIntfHashMap.Destroy;
begin
  Clear;
  inherited;
end;

function TStrIntfHashMap.Equals(AMap: IStrIntfMap): Boolean;
var
  I, J: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if AMap = nil then
    Exit;
  if FCount <> AMap.Size then
    Exit;
  Result := True;
  for I := 0 to FCapacity - 1 do
  begin
    for J := 0 to FBuckets[I].Count - 1 do
      if AMap.ContainsKey(FBuckets[I].Entries[J].Key) then
      begin
        if not (AMap.GetValue(FBuckets[I].Entries[J].Key) = FBuckets[I].Entries[J].Value) then
        begin
          Result := False;
          Exit;
        end;
      end
      else
      begin
        Result := False;
        Exit;
      end;
  end;
end;

function TStrIntfHashMap.GetValue(const Key: string): IInterface;
var
  I: Integer;
  Index: Integer;
  Bucket: PStrIntfBucket;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := nil;
  if Key = '' then
    Exit;
  Index := FHashFunction(HashString(Key));
  Bucket := @(FBuckets[Index]);
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      Exit;
    end;
end;

procedure TStrIntfHashMap.GrowEntries(BucketIndex: Integer);
var
  Capacity: Integer;
begin
  Capacity := Length(FBuckets[BucketIndex].Entries);
  if Capacity > 64 then
	  Capacity := Capacity + Capacity div 4
  else
    Capacity := Capacity * 4;
  SetLength(FBuckets[BucketIndex].Entries, Capacity);
end;

function TStrIntfHashMap.HashMul(Key: Cardinal): Cardinal;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(FCapacity * (Frac(Key * A)));
end;

function TStrIntfHashMap.HashString(const Key: string): Cardinal;
var
  I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := 0;
  for I := 1 to Length(Key) do
    Result := Result + (Ord(Key[I]) * (I - 1) * 256);
end;

function TStrIntfHashMap.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TStrIntfHashMap.KeySet: IStrSet;
var
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
  I, J: Integer;
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := TStrArraySet.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
	  	Result.Add(FBuckets[I].Entries[J].Key);
end;

procedure TStrIntfHashMap.PutAll(AMap: IStrIntfMap);
var
  It: IStrIterator;
  Key: string;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if AMap = nil then
    Exit;
  It := AMap.KeySet.First;
  while It.HasNext do
  begin
    Key := It.Next;
    PutValue(Key, AMap.GetValue(Key));
  end;
end;

procedure TStrIntfHashMap.PutValue(const Key: string; Value: IInterface);
var
  Index: Integer;
  Bucket: PStrIntfBucket;
  I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if Key = '' then
    Exit;
  if Value = nil then
    Exit;
  Index := FHashFunction(HashString(Key));
  Bucket := @(FBuckets[Index]);
  for I := 0 to Bucket.Count - 1 do
  	if Bucket.Entries[I].Key = Key then
    begin
			Bucket.Entries[I].Value := Value;
      Exit;
    end;
  if Bucket.Count = Length(Bucket.Entries) then
    GrowEntries(Index);
  Bucket.Entries[Bucket.Count].Key := Key;
  Bucket.Entries[Bucket.Count].Value := Value;
  Inc(Bucket.Count);
  Inc(FCount);
end;

function TStrIntfHashMap.Remove(const Key: string): IInterface;
var
  Bucket: PStrIntfBucket;
  I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := nil;
  if Key = '' then
    Exit;
  Bucket := @(FBuckets[FHashFunction(HashString(Key))]);
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      if I < (Length(Bucket.Entries)-1) then
	      System.Move(Bucket.Entries[I + 1], Bucket.Entries[I],
  	    	(Bucket.Count - I) * SizeOf(TStrStrEntry));
      Dec(Bucket.Count);
      Exit;
    end;
end;

function TStrIntfHashMap.Size: Integer;
begin
  Result := FCount;
end;

function TStrIntfHashMap.Values: IIntfCollection;
var
  I, J: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := TIntfArrayList.Create;
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Value);
end;

{ TStrStrHashMap }

procedure TStrStrHashMap.Clear;
var
  I, J: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  for I := 0 to FCapacity - 1 do
  begin
  	for J := 0 to FBuckets[I].Count-1 do
    begin
      FBuckets[I].Entries[J].Key := '';
			FBuckets[I].Entries[J].Value := '';
    end;
    FBuckets[I].Count := 0;
  end;
  FCount := 0;
end;

function TStrStrHashMap.Clone: IInterface;
var
  I, J: Integer;
  NewEntryArray: TStrStrEntryArray;
  NewMap: TStrStrHashMap;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  NewMap := TStrStrHashMap.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
  begin
    NewEntryArray := NewMap.FBuckets[I].Entries;
    SetLength(NewEntryArray, Length(FBuckets[I].Entries));
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      NewEntryArray[J].Key := FBuckets[I].Entries[J].Key;
      NewEntryArray[J].Value := FBuckets[I].Entries[J].Value;
    end;
    NewMap.FBuckets[I].Count := FBuckets[I].Count;
  end;
  Result := NewMap;
end;

function TStrStrHashMap.ContainsKey(const Key: string): Boolean;
var
  I: Integer;
  Bucket: PStrStrBucket;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if Key = '' then
    Exit;
  Bucket := @(FBuckets[FHashFunction(HashString(Key))]);
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := True;
      Exit;
    end;
end;

constructor TStrStrHashMap.Create;
begin
  Create(16);
end;

function TStrStrHashMap.ContainsValue(const Value: string): Boolean;
var
  I, J: Integer;
  Bucket: PStrStrBucket;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if Value = '' then
    Exit;
  for J := 0 to FCapacity - 1 do
  begin
    Bucket := @(FBuckets[J]);
    for I := 0 to Bucket.Count - 1 do
      if Bucket.Entries[I].Value = Value then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

constructor TStrStrHashMap.Create(Capacity: Integer);
var
  I: Integer;
begin
  inherited Create;
  FCapacity := Capacity;
  SetLength(FBuckets, FCapacity);
  for I := 0 to FCapacity - 1 do
    SetLength(FBuckets[I].Entries, 1);
  FHashFunction := HashMul;
end;

function TStrStrHashMap.Equals(AMap: IStrStrMap): Boolean;
var
  I, J: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if AMap = nil then
    Exit;
  if FCount <> AMap.Size then
    Exit;
  Result := True;
  for I := 0 to FCapacity - 1 do
  begin
    for J := 0 to FBuckets[I].Count - 1 do
      if AMap.ContainsKey(FBuckets[I].Entries[J].Key) then
      begin
        if not (AMap.GetValue(FBuckets[I].Entries[J].Key) = FBuckets[I].Entries[J].Value) then
        begin
          Result := False;
          Exit;
        end;
      end
      else
      begin
        Result := False;
        Exit;
      end;
  end;
end;

function TStrStrHashMap.GetValue(const Key: string): string;
var
  I: Integer;
  Bucket: PStrStrBucket;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := '';
  if Key = '' then
    Exit;
  Bucket := @(FBuckets[FHashFunction(HashString(Key))]);
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      Exit;
    end;
end;

procedure TStrStrHashMap.GrowEntries(BucketIndex: Integer);
var
  Capacity: Integer;
begin
  Capacity := Length(FBuckets[BucketIndex].Entries);
  if Capacity > 64 then
	  Capacity := Capacity + Capacity div 4
  else
    Capacity := Capacity * 4;
  SetLength(FBuckets[BucketIndex].Entries, Capacity);
end;

function TStrStrHashMap.HashMul(Key: Cardinal): Cardinal;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(FCapacity * (Frac(Key * A)));
end;

function TStrStrHashMap.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TStrStrHashMap.KeySet: IStrSet;
var
  I, J: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := TStrArraySet.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
	  	Result.Add(FBuckets[I].Entries[J].Key)
end;

procedure TStrStrHashMap.PutAll(AMap: IStrStrMap);
var
  It: IStrIterator;
  Key: string;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if AMap = nil then
    Exit;
  It := AMap.KeySet.First;
  while It.HasNext do
  begin
    Key := It.Next;
    PutValue(Key, AMap.GetValue(Key));
  end;
end;

procedure TStrStrHashMap.PutValue(const Key, Value: string);
var
  Index: Integer;
  Bucket: PStrStrBucket;
  I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if Key = '' then
    Exit;
  if Value = '' then
    Exit;
  Index := FHashFunction(HashString(Key));
  Bucket := @(FBuckets[Index]);
  for I := 0 to Bucket.Count - 1 do
  	if Bucket.Entries[I].Key = Key then
    begin
			Bucket.Entries[I].Value := Value;
      Exit;
    end;
  if Bucket.Count = Length(Bucket.Entries) then
    GrowEntries(Index);
  Bucket.Entries[Bucket.Count].Key := Key;
  Bucket.Entries[Bucket.Count].Value := Value;
  Inc(Bucket.Count);
  Inc(FCount);
end;

function TStrStrHashMap.Remove(const Key: string): string;
var
  Bucket: PStrStrBucket;
  I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := '';
  if Key = '' then
    Exit;
  Bucket := @(FBuckets[FHashFunction(HashString(Key))]);
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      if I < (Length(Bucket.Entries)-1) then
	      System.Move(Bucket.Entries[I + 1], Bucket.Entries[I],
  	    	(Bucket.Count - I) * SizeOf(TStrStrEntry));
      Dec(Bucket.Count);
      Exit;
    end;
end;

function TStrStrHashMap.Size: Integer;
begin
  Result := FCount;
end;

function TStrStrHashMap.Values: IStrCollection;
var
  I, J: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := TStrArrayList.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Value);
end;

destructor TStrStrHashMap.Destroy;
begin
  Clear;
  inherited;
end;

function TStrStrHashMap.HashString(const Key: string): Cardinal;
var
  I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := 0;
  for I := 1 to Length(Key) do
    Result := Result + (Ord(Key[I]) * (I - 1) * 256);
end;

{ TStrHashMap }

procedure TStrHashMap.Clear;
var
  I, J: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  for I := 0 to FCapacity - 1 do
  begin
  	for J := 0 to FBuckets[I].Count-1 do
    begin
      FBuckets[I].Entries[J].Key := '';
			FreeObject(FBuckets[I].Entries[J].Value);
    end;
    FBuckets[I].Count := 0;
  end;
  FCount := 0;
end;

function TStrHashMap.Clone: TObject;
var
  I, J: Integer;
  NewEntryArray: TStrEntryArray;
  NewMap: TStrHashMap;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  NewMap := TStrHashMap.Create(FCapacity, False); // Only one can have FOwnsObject = True
  for I := 0 to FCapacity - 1 do
  begin
    NewEntryArray := NewMap.FBuckets[I].Entries;
    SetLength(NewEntryArray, Length(FBuckets[I].Entries));
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      NewEntryArray[J].Key := FBuckets[I].Entries[J].Key;
      NewEntryArray[J].Value := FBuckets[I].Entries[J].Value;
    end;
    NewMap.FBuckets[I].Count := FBuckets[I].Count;
  end;
  Result := NewMap;
end;

function TStrHashMap.ContainsKey(const Key: string): Boolean;
var
  I: Integer;
  Bucket: PStrBucket;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if Key = '' then
    Exit;
  Bucket := @(FBuckets[FHashFunction(HashString(Key))]);
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := True;
      Exit;
    end;
end;

function TStrHashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: PStrBucket;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if Value = nil then
    Exit;
  for J := 0 to FCapacity - 1 do
  begin
    Bucket := @(FBuckets[J]);
    for I := 0 to Bucket.Count - 1 do
      if Bucket.Entries[I].Value = Value then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

constructor TStrHashMap.Create;
begin
  Create(16, True);
end;

constructor TStrHashMap.Create(Capacity: Integer; AOwnsObjects: Boolean);
var
  I: Integer;
begin
  inherited Create;
  FCapacity := Capacity;
  FOwnsObjects := AOwnsObjects;
  SetLength(FBuckets, FCapacity);
  for I := 0 to FCapacity - 1 do
    SetLength(FBuckets[I].Entries, 1);
  FHashFunction := HashMul;
end;

destructor TStrHashMap.Destroy;
begin
  Clear;
  inherited;
end;

function TStrHashMap.Equals(AMap: IStrMap): Boolean;
var
  I, J: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if AMap = nil then
    Exit;
  if FCount <> AMap.Size then
    Exit;
  Result := True;
  for I := 0 to FCapacity - 1 do
  begin
    for J := 0 to FBuckets[I].Count - 1 do
      if AMap.ContainsKey(FBuckets[I].Entries[J].Key) then
      begin
        if not (AMap.GetValue(FBuckets[I].Entries[J].Key) = FBuckets[I].Entries[J].Value) then
        begin
          Result := False;
          Exit;
        end;
      end
      else
      begin
        Result := False;
        Exit;
      end;
  end;
end;

function TStrHashMap.GetValue(const Key: string): TObject;
var
  I: Integer;
  Bucket: PStrBucket;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := nil;
  if Key = '' then
    Exit;
  I := FHashFunction(HashString(Key));
  Bucket := @(FBuckets[I]);
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      Exit;
    end;
end;

procedure TStrHashMap.FreeObject(AObject: TObject);
begin
	if FOwnsObjects then
  	AObject.Free;
end;

procedure TStrHashMap.GrowEntries(BucketIndex: Integer);
var
  Capacity: Integer;
begin
  Capacity := Length(FBuckets[BucketIndex].Entries);
  if Capacity > 64 then
	  Capacity := Capacity + Capacity div 4
  else
    Capacity := Capacity * 4;
  SetLength(FBuckets[BucketIndex].Entries, Capacity);
end;

function TStrHashMap.HashMul(Key: Cardinal): Cardinal;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(FCapacity * (Frac(Key * A)));
//  Result := LongRec(Key).Bytes[1] and $FF;
end;

function TStrHashMap.HashString(const Key: string): Cardinal;
var
  I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := 0;
  for I := 1 to Length(Key) do
    Result := Result + (Ord(Key[I]) * (I - 1) * 256);
end;

function TStrHashMap.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function TStrHashMap.KeySet: IStrSet;
var
  I, J: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := TStrArraySet.Create(FCapacity);
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
	  	Result.Add(FBuckets[I].Entries[J].Key);
end;

procedure TStrHashMap.PutAll(AMap: IStrMap);
var
  It: IStrIterator;
  Key: string;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if AMap = nil then
    Exit;
  It := AMap.KeySet.First;
  while It.HasNext do
  begin
    Key := It.Next;
    PutValue(Key, AMap.GetValue(Key));
  end;
end;

procedure TStrHashMap.PutValue(const Key: string; Value: TObject);
var
  Index: Integer;
  Bucket: PStrBucket;
  I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if Key = '' then
    Exit;
  if Value = nil then
    Exit;
  Index := FHashFunction(HashString(Key));
  Bucket := @(FBuckets[Index]);
  for I := 0 to Bucket.Count - 1 do
  	if Bucket.Entries[I].Key = Key then
    begin
			Bucket.Entries[I].Value := Value;
      Exit;
    end;
  if Bucket.Count = Length(Bucket.Entries) then
    GrowEntries(Index);
  Bucket.Entries[Bucket.Count].Key := Key;
  Bucket.Entries[Bucket.Count].Value := Value;
  Inc(Bucket.Count);
  Inc(FCount);
end;

function TStrHashMap.Remove(const Key: string): TObject;
var
  Bucket: PStrBucket;
  I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := nil;
  if Key = '' then
    Exit;
  Bucket := @(FBuckets[FHashFunction(HashString(Key))]);
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      if not FOwnsObjects then
	      Result := Bucket.Entries[I].Value
      else
      	Bucket.Entries[I].Value.Free;
      if I < (Length(Bucket.Entries)-1) then
	      System.Move(Bucket.Entries[I + 1], Bucket.Entries[I],
  	    	(Bucket.Count - I) * SizeOf(TStrEntry));
      Dec(Bucket.Count);
      Exit;
    end;
end;

function TStrHashMap.Size: Integer;
begin
  Result := FCount;
end;

function TStrHashMap.Values: ICollection;
var
  I, J: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := TArrayList.Create(FCapacity, False); // NEVER Owns Objects !
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Value);
end;

{ THashMap }

procedure THashMap.Clear;
var
  I, J: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  for I := 0 to FCapacity - 1 do
  begin
  	for J := 0 to FBuckets[I].Count-1 do
    begin
      FBuckets[I].Entries[J].Key := nil; // Free key ?
			FreeObject(FBuckets[I].Entries[J].Value);
    end;
    FBuckets[I].Count := 0;
  end;
  FCount := 0;
end;

function THashMap.Clone: TObject;
var
  I, J: Integer;
  NewEntryArray: TEntryArray;
  NewMap: THashMap;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  NewMap := THashMap.Create(FCapacity, FOwnsObjects);
  for I := 0 to FCapacity - 1 do
  begin
    NewEntryArray := NewMap.FBuckets[I].Entries;
    SetLength(NewEntryArray, Length(FBuckets[I].Entries));
    for J := 0 to FBuckets[I].Count - 1 do
    begin
      NewEntryArray[J].Key := FBuckets[I].Entries[J].Key;
      NewEntryArray[J].Value := FBuckets[I].Entries[J].Value;
    end;
    NewMap.FBuckets[I].Count := FBuckets[I].Count;
  end;
  Result := NewMap;
end;

function THashMap.ContainsKey(Key: TObject): Boolean;
var
  I: Integer;
  Bucket: PBucket;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if Key = nil then
    Exit;
  Bucket := @(FBuckets[FHashFunction(Integer(Key))]);
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := True;
      Exit;
    end;
end;

function THashMap.ContainsValue(Value: TObject): Boolean;
var
  I, J: Integer;
  Bucket: PBucket;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if Value = nil then
    Exit;
  for J := 0 to FCapacity - 1 do
  begin
    Bucket := @(FBuckets[J]);
    for I := 0 to Bucket.Count - 1 do
      if Bucket.Entries[I].Value = Value then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

constructor THashMap.Create;
begin
  Create(16, True);
end;

constructor THashMap.Create(Capacity: Integer; AOwnsObjects: Boolean);
var
  I: Integer;
begin
  inherited Create;
  FCapacity := Capacity;
  FOwnsObjects := AOwnsObjects;
  SetLength(FBuckets, FCapacity);
  for I := 0 to FCapacity - 1 do
    SetLength(FBuckets[I].Entries, 64);
  FHashFunction := HashMul;
end;

destructor THashMap.Destroy;
begin
  Clear;
  inherited;
end;

function THashMap.Equals(AMap: IMap): Boolean;
var
  I, J: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := False;
  if AMap = nil then
    Exit;
  if FCount <> AMap.Size then
    Exit;
  Result := True;
  for I := 0 to FCapacity - 1 do
  begin
    for J := 0 to FBuckets[I].Count - 1 do
      if AMap.ContainsKey(FBuckets[I].Entries[J].Key) then
      begin
        if not (AMap.GetValue(FBuckets[I].Entries[J].Key) = FBuckets[I].Entries[J].Value) then
        begin
          Result := False;
          Exit;
        end;
      end
      else
      begin
        Result := False;
        Exit;
      end;
  end;
end;

procedure THashMap.FreeObject(AObject: TObject);
begin
	if FOwnsObjects then
  	AObject.Free;
end;

function THashMap.GetValue(Key: TObject): TObject;
var
  I: Integer;
  Bucket: PBucket;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := nil;
  if Key = nil then
    Exit;
  Bucket := @(FBuckets[FHashFunction(Integer(Key))]);
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      Result := Bucket.Entries[I].Value;
      Exit;
    end;
end;

procedure THashMap.GrowEntries(BucketIndex: Integer);
var
  Capacity: Integer;
  OldCapacity: Integer;
begin
  OldCapacity := Length(FBuckets[BucketIndex].Entries);
  if OldCapacity > 64 then
	  Capacity := OldCapacity + OldCapacity div 4
  else
    Capacity := OldCapacity * 4;
  SetLength(FBuckets[BucketIndex].Entries, Capacity);
end;

function THashMap.HashMul(Key: Cardinal): Cardinal;
const
  A = 0.6180339887; // (sqrt(5) - 1) / 2
begin
  Result := Trunc(FCapacity * (Frac(Key * A)));
//  Result := LongRec(Key).Bytes[1] and $FF;
end;

function THashMap.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

function THashMap.KeySet: ISet;
var
  I, J: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := TArraySet.Create(FCapacity, False); // NEVER Owns Objects !
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
	  	Result.Add(FBuckets[I].Entries[J].Key);
end;

procedure THashMap.PutAll(AMap: IMap);
var
  It: IIterator;
  Key: TObject;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if AMap = nil then
    Exit;
  It := AMap.KeySet.First;
  while It.HasNext do
  begin
    Key := It.Next;
    PutValue(Key, AMap.GetValue(Key));
  end;
end;

procedure THashMap.PutValue(Key, Value: TObject);
var
  Index: Integer;
  Bucket: PBucket;
  I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  if Key = nil then
    Exit;
  if Value = nil then
    Exit;
  Index := FHashFunction(Integer(Key));
  Bucket := @(FBuckets[Index]);
  for I := 0 to Bucket.Count - 1 do
  	if Bucket.Entries[I].Key = Key then
    begin
			Bucket.Entries[I].Value := Value;
      Exit;
    end;
  if Bucket.Count = Length(Bucket.Entries) then
    GrowEntries(Index);
  begin
    Bucket.Entries[Bucket.Count].Key := Key;
    Bucket.Entries[Bucket.Count].Value := Value;
  end;
  Inc(Bucket.Count);
  Inc(FCount);
end;

function THashMap.Remove(Key: TObject): TObject;
var
  Bucket: PBucket;
  I: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := nil;
  if Key = nil then
    Exit;
  Bucket := @(FBuckets[FHashFunction(Integer(Key))]);
  for I := 0 to Bucket.Count - 1 do
    if Bucket.Entries[I].Key = Key then
    begin
      if not FOwnsObjects then
        Result := Bucket.Entries[I].Value
      else
      	Bucket.Entries[I].Value.Free;
      if I < (Length(Bucket.Entries)-1) then
	      System.Move(Bucket.Entries[I + 1], Bucket.Entries[I],
  	    	(Bucket.Count - I) * SizeOf(TEntry));
      Dec(Bucket.Count);
      Exit;
    end;
end;

function THashMap.Size: Integer;
begin
  Result := FCount;
end;

function THashMap.Values: ICollection;
var
  I, J: Integer;
{$IFDEF THREADSAFE}
  CS: IInterface;
{$ENDIF}
begin
{$IFDEF THREADSAFE}
  CS := EnterCriticalSection;
{$ENDIF}
  Result := TArrayList.Create(FCapacity, False); // NEVER Owns Objects !
  for I := 0 to FCapacity - 1 do
    for J := 0 to FBuckets[I].Count - 1 do
      Result.Add(FBuckets[I].Entries[J].Value);
end;

end.
