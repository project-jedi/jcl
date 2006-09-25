unit ContainerPerformanceTests;

interface

uses
  Classes;

procedure TestList(Results: TStrings);
procedure TestJclArrayList(Results: TStrings);
procedure TestJclLinkedList(Results: TStrings);
procedure TestJclVector(Results: TStrings);

procedure TestBucketList(Results: TStrings);
procedure TestJclHashMap(Results: TStrings);
procedure TestHashedStringList(Results: TStrings);
procedure TestJclStrStrHashMap(Results: TStrings);

implementation

{$I jcl.inc}

uses
  SysUtils, Forms, Controls, Math,
  {$IFDEF RTL140_UP}
  Contnrs, IniFiles,
  {$ENDIF RTL140_UP}
  JclContainerIntf, JclArrayLists, JclLinkedLists, JclHashMaps, JclVectors;

const
  ResultFormat = '%.1f ms';
  MsecsPerDay = 24 * 60 * 60 * 1000;

{$IFNDEF RTL140_UP}
const
  SNeedRTL140Up = 'requires RTL > 14.0';
{$ENDIF ~RTL140_UP}

procedure TestList(Results: TStrings);
var
  List: TList;
  I, res: Integer;
  Start: TDateTime;
begin
  Randomize;
  Start := Now;
  List := TList.Create;
  Screen.Cursor := crHourGlass;
  try
    for I := 0 to 2000000 do
      List.Add(Pointer(I));
    Results[1] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to List.Count - 1 do
      Res := Integer(List[I]);
    Results[2] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to 200 do
      Res := List.IndexOf(Pointer(Random(1000000)));
    Results[3] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to 100 do
      List.Insert(10, Pointer(I));
    Results[4] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    List.Clear;
    Results[5] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
  finally
    List.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TestJclArrayList(Results: TStrings);
var
  List: IJclList;
  It: IJclIterator;
  I, Res: Integer;
  Start: TDateTime;
begin
  Randomize;
  Screen.Cursor := crHourGlass;
  try
    Start := Now;
    List := TJclArrayList.Create(16, False);
    for I := 0 to 2000000 do
      List.Add(TObject(I));
    Results[1] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    // Fast but Specific ArrayList
    //for I := 0 to List.Size - 1 do
    //  Res := Integer(List.GetObject(I));
    // Slower but same for every IJclList
    It := List.First;
    while It.HasNext do
      I := Integer(It.Next);
    Results[2] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to 200 do
      Res := List.IndexOf(TObject(Random(1000000)));
    Results[3] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    It := List.First;
    for I := 0 to 10 do
      It.Next;
    for I := 0 to 100 do
      It.Add(TObject(I));
    Results[4] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    List.Clear;
    Results[5] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TestJclLinkedList(Results: TStrings);
var
  List: IJclList;
  I, Res: Integer;
  It: IJclIterator;
  Start: TDateTime;
begin
  Randomize;
  Screen.Cursor := crHourGlass;
  try
    Start := Now;
    List := TJclLinkedList.Create(nil, False);
    for I := 0 to 2000000 do
      List.Add(TObject(I));
    Results[1] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    It := List.First;
    while It.HasNext do
      I := Integer(It.Next);
    Results[2] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to 200 do
      Res := List.IndexOf(TObject(Random(1000000)));
    Results[3] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    It := List.First;
    for I := 0 to 10 do
      It.Next;
    for I := 0 to 100 do
      It.Add(TObject(I));
    Results[4] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    List.Clear;
    Results[5] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TestJclVector(Results: TStrings);
var
  List: TJclVector;
  I, res: Integer;
  Start: TDateTime;
begin
  Randomize;
  Screen.Cursor := crHourGlass;
  Start := Now;
  List := TJclVector.Create(16, False);
  try
    for I := 0 to 2000000 do
      List.Add(TObject(I));
    Results[1] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to List.Size - 1 do
      Res := Integer(List.Items[I]);
    Results[2] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to 200 do
      Res := List.IndexOf(TObject(Random(1000000)));
    Results[3] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to 10 do
    begin
      System.Move(List.Items[10], List.Items[10 + 1],
        (List.Size - 10) * SizeOf(TObject));
      List.Items[10] := TObject(I);
    end;
    Results[4] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    List.Clear;
    Results[5] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
  finally
    List.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TestBucketList(Results: TStrings);
{$IFDEF RTL140_UP}
var
  I, Res: Integer;
  Start: TDateTime;
  List: TBucketList;
begin
  Randomize;
  Screen.Cursor := crHourGlass;
  Start := Now;
  List := TBucketList.Create(bl256);
  try
    for I := 0 to 100000 do
      List.Add(TObject(I), TObject(I));
    Results[1] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to 100000 do
      Res := Integer(List.Data[TObject(Random(100000))]);
    Results[2] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    List.Clear;
    Results[3] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
  finally
    List.Free;
    Screen.Cursor := crDefault;
  end;
end;
{$ELSE ~RTL140_UP}
var
  I: Integer;
begin
  for I := 1 to 3 do
    Results[I] := SNeedRTL140Up;
end;
{$ENDIF ~RTL140_UP}

procedure TestJclHashMap(Results: TStrings);
var
  Map: IJclMap;
  I, Res: Integer;
  Start: TDateTime;
begin
  Randomize;
  Screen.Cursor := crHourGlass;
  try
    Start := Now;
    Map := JclHashMaps.TJclHashMap.Create(256, False);
    for I := 0 to 100000 do
      Map.PutValue(TObject(Random(100000)), TObject(I));
    Results[1] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to 100000 do
      Res := Integer(Map.GetValue(TObject(Random(100000))));
    Results[2] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    Map.Clear;
    Results[3] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

function GenId(Value: Integer): string;
begin
  Result := IntToStr(Value);
end;

procedure TestHashedStringList(Results: TStrings);
{$IFDEF RTL140_UP}
var
  I: Integer;
  Index: Integer;
  List: THashedStringList;
  Start: TDateTime;
begin
  Randomize;
  Screen.Cursor := crHourGlass;
  Start := Now;
  List := THashedStringList.Create;
  try
    for I := 0 to 100000 do
      List.Add(GenId(123));
    Results[1] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to 100000 do
      Index := List.IndexOf(GenId(123));
    Results[2] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    List.Clear;
    Results[3] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
  finally
    List.Free;
    Screen.Cursor := crDefault;
  end;
end;
{$ELSE ~RTL140_UP}
var
  I: Integer;
begin
  for I := 1 to 3 do
    Results[I] := SNeedRTL140Up;
end;
{$ENDIF ~RTL140_UP}

procedure TestJclStrStrHashMap(Results: TStrings);
var
  Map: IJclStrStrMap;
  I: Integer;
  Res: string;
  Start: TDateTime;
begin
  Randomize;
  Screen.Cursor := crHourGlass;
  try
    Start := Now;
    Map := TJclStrStrHashMap.Create(256);
    for I := 0 to 100000 do
      Map.PutValue(GenId(123), '');
    Results[1] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    for I := 0 to 100000 do
      Res := Map.GetValue(GenId(123));
    Results[2] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
    Start := Now;
    Map.Clear;
    Results[3] := Format(ResultFormat, [(Now - Start) * MsecsPerDay]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
