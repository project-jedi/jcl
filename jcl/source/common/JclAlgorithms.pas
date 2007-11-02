{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is Algorithms.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Philippe BEMPEL aka RDM. Portions created by  }
{ Jean-Philippe BEMPEL are Copyright (C) Jean-Philippe BEMPEL (rdm_30 att yahoo dott com)          }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclAlgorithms;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase, JclContainerIntf;

// function pointer types
type
  // pointer functions for Apply Algorithms
  TIntfApplyFunction = function(const AInterface: IInterface): IInterface;
  TAnsiStrApplyFunction = function(const AString: AnsiString): AnsiString;
  TWideStrApplyFunction = function(const AString: WideString): WideString;
  {$IFDEF CONTAINER_ANSISTR}
  TStrApplyFunction = TAnsiStrApplyFunction;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TStrApplyFunction = TWideStrApplyFunction;
  {$ENDIF CONTAINER_WIDESTR}
  TApplyFunction = function(AObject: TObject): TObject;

  // Pointer functions for comparator
  TIntfCompare = function(const Obj1, Obj2: IInterface): Integer;
  TAnsiStrCompare = function(const Obj, Obj2: AnsiString): Integer;
  TWideStrCompare = function(const Obj, Obj2: WideString): Integer;
  {$IFDEF CONTAINER_ANSISTR}
  TStrCompare = TAnsiStrCompare;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TStrCompare = TWideStrCompare;
  {$ENDIF CONTAINER_WIDESTR}
  TCompare = function(Obj1, Obj2: TObject): Integer;

// Compare functions
function IntfSimpleCompare(const Obj1, Obj2: IInterface): Integer;
function AnsiStrSimpleCompare(const Obj1, Obj2: AnsiString): Integer;
function WideStrSimpleCompare(const Obj1, Obj2: WideString): Integer;
function StrSimpleCompare(const Obj1, Obj2: string): Integer;
function SimpleCompare(Obj1, Obj2: TObject): Integer;

function IntegerCompare(Obj1, Obj2: TObject): Integer;

// Apply algorithms
procedure Apply(const First: IJclIntfIterator; Count: Integer; F: TIntfApplyFunction); overload;
procedure Apply(const First: IJclAnsiStrIterator; Count: Integer; F: TAnsiStrApplyFunction); overload;
procedure Apply(const First: IJclWideStrIterator; Count: Integer; F: TWideStrApplyFunction); overload;
procedure Apply(const First: IJclIterator; Count: Integer; F: TApplyFunction); overload;

// Find algorithms
function Find(const First: IJclIntfIterator; Count: Integer; const AInterface: IInterface;
  AComparator: TIntfCompare): IJclIntfIterator; overload;
function Find(const First: IJclAnsiStrIterator; Count: Integer; const AString: string;
  AComparator: TAnsiStrCompare): IJclAnsiStrIterator; overload;
function Find(const First: IJclWideStrIterator; Count: Integer; const AString: string;
  AComparator: TWideStrCompare): IJclWideStrIterator; overload;
function Find(const First: IJclIterator; Count: Integer; AObject: TObject;
  AComparator: TCompare): IJclIterator; overload;

// CountObject algorithms
function CountObject(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface; AComparator: TIntfCompare): Integer; overload;
  function CountObject(const First: IJclAnsiStrIterator; Count: Integer;
  const AString: AnsiString; AComparator: TAnsiStrCompare): Integer; overload;
function CountObject(const First: IJclWideStrIterator; Count: Integer;
  const AString: WideString; AComparator: TWideStrCompare): Integer; overload;
function CountObject(const First: IJclIterator; Count: Integer; AObject: TObject;
  AComparator: TCompare): Integer; overload;

// Copy algorithms
procedure Copy(const First: IJclIntfIterator; Count: Integer;
  const Output: IJclIntfIterator); overload;
procedure Copy(const First: IJclAnsiStrIterator; Count: Integer;
  const Output: IJclAnsiStrIterator); overload;
procedure Copy(const First: IJclWideStrIterator; Count: Integer;
  const Output: IJclWideStrIterator); overload;
procedure Copy(const First: IJclIterator; Count: Integer;
  const Output: IJclIterator); overload;

// Generate algorithms
procedure Generate(const List: IJclIntfList; Count: Integer; const AInterface: IInterface); overload;
procedure Generate(const List: IJclAnsiStrList; Count: Integer; const AString: AnsiString); overload;
procedure Generate(const List: IJclWideStrList; Count: Integer; const AString: WideString); overload;
procedure Generate(const List: IJclList; Count: Integer; AObject: TObject); overload;

// Fill algorithms
procedure Fill(const First: IJclIntfIterator; Count: Integer; const AInterface: IInterface); overload;
procedure Fill(const First: IJclAnsiStrIterator; Count: Integer; const AString: AnsiString); overload;
procedure Fill(const First: IJclWideStrIterator; Count: Integer; const AString: WideString); overload;
procedure Fill(const First: IJclIterator; Count: Integer; AObject: TObject); overload;

// Reverse algorithms
procedure Reverse(const First, Last: IJclIntfIterator); overload;
procedure Reverse(const First, Last: IJclAnsiStrIterator); overload;
procedure Reverse(const First, Last: IJclWideStrIterator); overload;
procedure Reverse(const First, Last: IJclIterator); overload;

type
  // Pointer functions for sort algorithms
  TIntfSortProc = procedure(const AList: IJclIntfList; L, R: Integer; AComparator: TIntfCompare);
  TAnsiStrSortProc = procedure(const AList: IJclAnsiStrList; L, R: Integer; AComparator: TAnsiStrCompare);
  TWideStrSortProc = procedure(const AList: IJclWideStrList; L, R: Integer; AComparator: TWideStrCompare);
  {$IFDEF CONTAINER_ANSISTR}
  TStrSortProc = TAnsiStrSortProc;
  {$ENDIF CONTAINER_ANSISTR}
  {$IFDEF CONTAINER_WIDESTR}
  TStrSortProc = TWideStrSortProc;
  {$ENDIF CONTAINER_WIDESTR}
  TSortProc = procedure(const AList: IJclList; L, R: Integer; AComparator: TCompare);

procedure QuickSort(const AList: IJclIntfList; L, R: Integer; AComparator: TIntfCompare); overload;
procedure QuickSort(const AList: IJclAnsiStrList; L, R: Integer; AComparator: TAnsiStrCompare); overload;
procedure QuickSort(const AList: IJclWideStrList; L, R: Integer; AComparator: TWideStrCompare); overload;
procedure QuickSort(const AList: IJclList; L, R: Integer; AComparator: TCompare); overload;

var
  IntfSortProc: TIntfSortProc = QuickSort;
  AnsiStrSortProc: TAnsiStrSortProc = QuickSort;
  WideStrSortProc: TWideStrSortProc = QuickSort;
  SortProc: TSortProc = QuickSort;

// Sort algorithms
procedure Sort(const AList: IJclIntfList; First, Last: Integer; AComparator: TIntfCompare); overload;
procedure Sort(const AList: IJclAnsiStrList; First, Last: Integer; AComparator: TAnsiStrCompare); overload;
procedure Sort(const AList: IJclWideStrList; First, Last: Integer; AComparator: TWideStrCompare); overload;
procedure Sort(const AList: IJclList; First, Last: Integer; AComparator: TCompare); overload;

{$IFDEF SUPPORTS_GENERICS}
type
  TApplyFunction<T> = function(AItem: T): T;
  TCompare<T> = function(Obj1, Obj2: T): Integer;
  TEqualityCompare<T> = function(Obj1, Obj2: T): Boolean;
  THash<T> = function(AItem: T): Integer;
  TSortProc<T> = procedure(const AList: IJclList<T>; L, R: Integer; AComparator: TCompare<T>);

  // cannot implement generic global functions
  TJclAlgorithms<T> = class
  private
    //FSortProc: TSortProc;
  public
    class procedure Apply(const First: IJclIterator<T>; Count: Integer; F: TApplyFunction<T>);
    class function Find(const First: IJclIterator<T>; Count: Integer; AItem: T;
      AComparator: TCompare<T>): IJclIterator<T>;
    class function CountObject(const First: IJclIterator<T>; Count: Integer; AItem: T;
      AComparator: TCompare<T>): Integer;
    class procedure Copy(const First: IJclIterator<T>; Count: Integer; const Output: IJclIterator<T>);
    class procedure Generate(const List: IJclList<T>; Count: Integer; AItem: T);
    class procedure Fill(const First: IJclIterator<T>; Count: Integer; AItem: T);
    class procedure Reverse(const First, Last: IJclIterator<T>);
    class procedure QuickSort(const AList: IJclList<T>; L, R: Integer; AComparator: TCompare<T>);
    //class procedure Sort(const AList: IJclList<T>; First, Last: Integer; AComparator: TCompare<T>);
    //class property SortProc: TSortProc<T> read FSortProc write FSortProc;
  end;
{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFNDEF RTL140_UP}
  JclWideStrings,
  {$ENDIF ~RTL140_UP}
  SysUtils;

function IntfSimpleCompare(const Obj1, Obj2: IInterface): Integer;
begin
  if Cardinal(Obj1) < Cardinal(Obj2) then
    Result := -1
  else
  if Cardinal(Obj1) > Cardinal(Obj2) then
    Result := 1
  else
    Result := 0;
end;

function AnsiStrSimpleCompare(const Obj1, Obj2: AnsiString): Integer;
begin
  // (rom) changed to case sensitive compare
  Result := CompareStr(Obj1, Obj2);
end;

function WideStrSimpleCompare(const Obj1, Obj2: WideString): Integer;
begin
  // (rom) changed to case sensitive compare
  Result := WideCompareStr(Obj1, Obj2);
end;

function StrSimpleCompare(const Obj1, Obj2: string): Integer;
begin
  case SizeOf(Obj1[1]) of
    1:
      Result := CompareStr(Obj1, Obj2);
    2:
      Result := WideCompareStr(Obj1, Obj2);
  else
    raise EJclOperationNotSupportedError.Create;
  end;
end;

function SimpleCompare(Obj1, Obj2: TObject): Integer;
begin
  if Cardinal(Obj1) < Cardinal(Obj2) then
    Result := -1
  else
  if Cardinal(Obj1) > Cardinal(Obj2) then
    Result := 1
  else
    Result := 0;
end;

function IntegerCompare(Obj1, Obj2: TObject): Integer;
begin
  Result := Integer(Obj1) - Integer(Obj2);
end;

procedure Apply(const First: IJclIntfIterator; Count: Integer; F: TIntfApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetObject(F(First.Next))
    else
      Break;
end;

procedure Apply(const First: IJclAnsiStrIterator; Count: Integer; F: TAnsiStrApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetString(F(First.Next))
    else
      Break;
end;

procedure Apply(const First: IJclWideStrIterator; Count: Integer; F: TWideStrApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetString(F(First.Next))
    else
      Break;
end;

procedure Apply(const First: IJclIterator; Count: Integer; F: TApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetObject(F(First.Next))
    else
      Break;
end;

function Find(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface; AComparator: TIntfCompare): IJclIntfIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AInterface) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclAnsiStrIterator; Count: Integer;
  const AString: string; AComparator: TAnsiStrCompare): IJclAnsiStrIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AString) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclWideStrIterator; Count: Integer;
  const AString: string; AComparator: TWideStrCompare): IJclWideStrIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AString) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function Find(const First: IJclIterator; Count: Integer; AObject: TObject;
  AComparator: TCompare): IJclIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AObject) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

function CountObject(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface; AComparator: TIntfCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AInterface) = 0))
    else
      Break;
end;

function CountObject(const First: IJclAnsiStrIterator; Count: Integer;
  const AString: AnsiString; AComparator: TAnsiStrCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AString) = 0))
    else
      Break;
end;

function CountObject(const First: IJclWideStrIterator; Count: Integer;
  const AString: WideString; AComparator: TWideStrCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AString) = 0))
    else
      Break;
end;

function CountObject(const First: IJclIterator; Count: Integer; AObject: TObject;
  AComparator: TCompare): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AObject) = 0))
    else
      Break;
end;

procedure Copy(const First: IJclIntfIterator; Count: Integer;
  const Output: IJclIntfIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetObject(First.Next);
    end
    else
      Break;
end;

procedure Copy(const First: IJclAnsiStrIterator; Count: Integer;
  const Output: IJclAnsiStrIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetString(First.Next);
    end
    else
      Break;
end;

procedure Copy(const First: IJclWideStrIterator; Count: Integer;
  const Output: IJclWideStrIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetString(First.Next);
    end
    else
      Break;
end;

procedure Copy(const First: IJclIterator; Count: Integer;
  const Output: IJclIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetObject(First.Next);
    end
    else
      Break;
end;

procedure Generate(const List: IJclIntfList; Count: Integer;
  const AInterface: IInterface);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AInterface);
end;

procedure Generate(const List: IJclAnsiStrList; Count: Integer;
  const AString: AnsiString);
var
  I: Integer;
begin
  List.Clear;
  for I := Count - 1 downto 0 do
    List.Add(AString);
end;

procedure Generate(const List: IJclWideStrList; Count: Integer;
  const AString: WideString);
var
  I: Integer;
begin
  List.Clear;
  for I := Count - 1 downto 0 do
    List.Add(AString);
end;

procedure Generate(const List: IJclList; Count: Integer; AObject: TObject);
var
  I: Integer;
begin
  List.Clear;
  for I := Count - 1 downto 0 do
    List.Add(AObject);
end;

procedure Fill(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetObject(AInterface);
    end
    else
      Break;
end;

procedure Fill(const First: IJclAnsiStrIterator; Count: Integer;
  const AString: AnsiString);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetString(AString);
    end
    else
      Break;
end;

procedure Fill(const First: IJclWideStrIterator; Count: Integer;
  const AString: WideString);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetString(AString);
    end
    else
      Break;
end;

procedure Fill(const First: IJclIterator; Count: Integer; AObject: TObject);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetObject(AObject);
    end
    else
      Break;
end;

procedure Reverse(const First, Last: IJclIntfIterator);
var
  Obj: IInterface;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex < Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetObject(Last.GetObject);
    Last.SetObject(Obj);
  end;
end;

procedure Reverse(const First, Last: IJclAnsiStrIterator);
var
  Obj: AnsiString;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex <= Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetString(Last.GetString);
    Last.SetString(Obj);
  end;
end;

procedure Reverse(const First, Last: IJclWideStrIterator);
var
  Obj: WideString;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex <= Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetString(Last.GetString);
    Last.SetString(Obj);
  end;
end;

procedure Reverse(const First, Last: IJclIterator);
var
  Obj: TObject;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex <= Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetObject(Last.GetObject);
    Last.SetObject(Obj);
  end;
end;

procedure QuickSort(const AList: IJclIntfList; L, R: Integer;
  AComparator: TIntfCompare);
var
  I, J, P: Integer;
  Obj: IInterface;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while AComparator(AList.GetObject(I), AList.GetObject(P)) < 0 do
        Inc(I);
      while AComparator(AList.GetObject(J), AList.GetObject(P)) > 0 do
        Dec(J);
      if I <= J then
      begin
        Obj := AList.GetObject(I);
        AList.SetObject(I, AList.GetObject(J));
        AList.SetObject(J, Obj);
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure QuickSort(const AList: IJclAnsiStrList; L, R: Integer;
  AComparator: TAnsiStrCompare);
var
  I, J, P: Integer;
  Obj: AnsiString;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while AComparator(AList.GetString(I), AList.GetString(P)) < 0 do
        Inc(I);
      while AComparator(AList.GetString(J), AList.GetString(P)) > 0 do
        Dec(J);
      if I <= J then
      begin
        Obj := AList.GetString(I);
        AList.SetString(I, AList.GetString(J));
        AList.SetString(J, Obj);
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure QuickSort(const AList: IJclWideStrList; L, R: Integer;
  AComparator: TWideStrCompare);
var
  I, J, P: Integer;
  Obj: WideString;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while AComparator(AList.GetString(I), AList.GetString(P)) < 0 do
        Inc(I);
      while AComparator(AList.GetString(J), AList.GetString(P)) > 0 do
        Dec(J);
      if I <= J then
      begin
        Obj := AList.GetString(I);
        AList.SetString(I, AList.GetString(J));
        AList.SetString(J, Obj);
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure QuickSort(const AList: IJclList; L, R: Integer; AComparator: TCompare);
var
  I, J, P: Integer;
  Obj: TObject;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while AComparator(AList.GetObject(I), AList.GetObject(P)) < 0 do
        Inc(I);
      while AComparator(AList.GetObject(J), AList.GetObject(P)) > 0 do
        Dec(J);
      if I <= J then
      begin
        Obj := AList.GetObject(I);
        AList.SetObject(I, AList.GetObject(J));
        AList.SetObject(J, Obj);
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

procedure Sort(const AList: IJclIntfList; First, Last: Integer; AComparator: TIntfCompare);
begin
  IntfSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclAnsiStrList; First, Last: Integer; AComparator: TAnsiStrCompare);
begin
  AnsiStrSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclWideStrList; First, Last: Integer; AComparator: TWideStrCompare);
begin
  WideStrSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclList; First, Last: Integer; AComparator: TCompare);
begin
  SortProc(AList, First, Last, AComparator);
end;

{$IFDEF SUPPORTS_GENERICS}
class procedure TJclAlgorithms<T>.Apply(const First: IJclIterator<T>; Count: Integer;
  F: TApplyFunction<T>);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
      First.SetItem(F(First.Next))
    else
      Break;
end;

class function TJclAlgorithms<T>.Find(const First: IJclIterator<T>; Count: Integer; AItem: T;
  AComparator: TCompare<T>): IJclIterator<T>;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.Next, AItem) = 0 then
      begin
        Result := First;
        Break;
      end;
    end
    else
      Break;
end;

class function TJclAlgorithms<T>.CountObject(const First: IJclIterator<T>; Count: Integer;
  AItem: T; AComparator: TCompare<T>): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Count - 1 downto 0 do
    if First.HasNext then
      Inc(Result, Ord(AComparator(First.Next, AItem) = 0))
    else
      Break;
end;

class procedure TJclAlgorithms<T>.Copy(const First: IJclIterator<T>; Count: Integer;
  const Output: IJclIterator<T>);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.Next;
      Output.SetItem(First.Next);
    end
    else
      Break;
end;

class procedure TJclAlgorithms<T>.Generate(const List: IJclList<T>; Count: Integer; AItem: T);
var
  I: Integer;
begin
  List.Clear;
  for I := Count - 1 downto 0 do
    List.Add(AItem);
end;

class procedure TJclAlgorithms<T>.Fill(const First: IJclIterator<T>; Count: Integer; AItem: T);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.Next;
      First.SetItem(AItem);
    end
    else
      Break;
end;

class procedure TJclAlgorithms<T>.Reverse(const First, Last: IJclIterator<T>);
var
  Obj: T;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex <= Last.PreviousIndex do
  begin
    Obj := First.Next;
    Last.Previous;
    First.SetItem(Last.GetItem);
    Last.SetItem(Obj);
  end;
end;

class procedure TJclAlgorithms<T>.QuickSort(const AList: IJclList<T>; L, R: Integer;
  AComparator: TCompare<T>);
var
  I, J, P: Integer;
  Obj: T;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while AComparator(AList.GetItem(I), AList.GetItem(P)) < 0 do
        Inc(I);
      while AComparator(AList.GetItem(J), AList.GetItem(P)) > 0 do
        Dec(J);
      if I <= J then
      begin
        Obj := AList.GetItem(I);
        AList.SetItem(I, AList.GetItem(J));
        AList.SetItem(J, Obj);
        if P = I then
          P := J
        else
        if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(AList, L, J, AComparator);
    L := I;
  until I >= R;
end;

{class procedure TJclAlgorithms<T>.Sort(const AList: IJclList<T>; First, Last: Integer;
  AComparator: TCompare<T>);
begin

end;}
{$ENDIF SUPPORTS_GENERICS}


{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

