unit FpStringHash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type

  { TStringHash }

  TStringHash = class
  private
    FHash: TFPHashList;
  public
    constructor Create({%H-}Size: Cardinal);
    destructor Destroy; override;
    procedure Add(const Key: string; Value: Integer);
    procedure Clear;
    function Modify(const Key: string; Value: Integer): Boolean;
    procedure Remove(const Key: string);
    function ValueOf(const Key: string): Integer;
  end;

implementation

{ TStringHash }

constructor TStringHash.Create(Size: Cardinal);
begin
  //Ignore Size. TFpHashList adapt hash size according to Capacity
  FHash := TFPHashList.Create;
end;

destructor TStringHash.Destroy;
begin
  FHash.Destroy;
  inherited Destroy;
end;

procedure TStringHash.Add(const Key: string; Value: Integer);
begin
  FHash.Add(Key, Pointer(Value));
end;

procedure TStringHash.Clear;
begin
  FHash.Clear;
end;

function TStringHash.Modify(const Key: string; Value: Integer): Boolean;
var
  Index: Integer;
begin
  Index := FHash.FindIndexOf(Key);
  Result := Index <> -1;
  if Result then
    FHash.Items[Index] := Pointer(Value);
end;

procedure TStringHash.Remove(const Key: string);
var
  Index: Integer;
begin
  Index := FHash.FindIndexOf(Key);
  if Index <> -1 then
    FHash.Delete(Index);
end;

function TStringHash.ValueOf(const Key: string): Integer;
begin
  Result := FHash.FindIndexOf(Key);
  if Result <> -1 then
    Result := Integer(FHash.Items[Result]);
end;

end.

