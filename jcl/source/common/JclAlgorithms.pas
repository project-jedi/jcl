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
{**************************************************************************************************}
{                                                                                                  }
{ The Delphi Container Library                                                                     }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$

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
  TStrApplyFunction = function(const AString: string): string;
  TApplyFunction = function(AObject: TObject): TObject;
  // Pointer functions for comparator
  TIntfCompare = function(const Obj1, Obj2: IInterface): Integer;
  TStrCompare = function(const Obj, Obj2: string): Integer;
  TCompare = function(Obj1, Obj2: TObject): Integer;

// Compare functions
function IntfSimpleCompare(const Obj1, Obj2: IInterface): Integer;
function StrSimpleCompare(const Obj1, Obj2: string): Integer;
function SimpleCompare(Obj1, Obj2: TObject): Integer;

function IntegerCompare(Obj1, Obj2: TObject): Integer;

// Apply algorithms
procedure Apply(const First: IJclIntfIterator; Count: Integer;
  F: TIntfApplyFunction); overload;
procedure Apply(const First: IJclStrIterator; Count: Integer;
  F: TStrApplyFunction); overload;
procedure Apply(const First: IJclIterator; Count: Integer;
  F: TApplyFunction); overload;

// Find algorithms
function Find(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface;
  AComparator: TIntfCompare): IJclIntfIterator; overload;
function Find(const First: IJclStrIterator; Count: Integer;
  const AString: string;
  AComparator: TStrCompare): IJclStrIterator; overload;
function Find(const First: IJclIterator; Count: Integer; AObject: TObject;
  AComparator: TCompare): IJclIterator; overload;

// CountObject algorithms
function CountObject(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface; AComparator: TIntfCompare): Integer; overload;
function CountObject(const First: IJclStrIterator; Count: Integer;
  const AString: string; AComparator: TStrCompare): Integer; overload;
function CountObject(const First: IJclIterator; Count: Integer;
  AObject: TObject;
  AComparator: TCompare): Integer; overload;

// Copy algorithms
procedure Copy(const First: IJclIntfIterator; Count: Integer;
  const Output: IJclIntfIterator); overload;
procedure Copy(const First: IJclStrIterator; Count: Integer;
  const Output: IJclStrIterator); overload;
procedure Copy(const First: IJclIterator; Count: Integer;
  const Output: IJclIterator); overload;

// Generate algorithms
procedure Generate(const List: IJclIntfList; Count: Integer;
  const AInterface: IInterface); overload;
procedure Generate(const List: IJclStrList; Count: Integer;
  const AString: string); overload;
procedure Generate(const List: IJclList; Count: Integer;
  AObject: TObject); overload;

// Fill algorithms
procedure Fill(const First: IJclIntfIterator; Count: Integer;
  const AInterface: IInterface); overload;
procedure Fill(const First: IJclStrIterator; Count: Integer;
  const AString: string); overload;
procedure Fill(const First: IJclIterator; Count: Integer;
  AObject: TObject); overload;

// Reverse algorithms
procedure Reverse(const First, Last: IJclIntfIterator); overload;
procedure Reverse(const First, Last: IJclStrIterator); overload;
procedure Reverse(const First, Last: IJclIterator); overload;

type
  // Pointer functions for sort algorithms
  TIntfSortProc = procedure(const AList: IJclIntfList;
    L, R: Integer; AComparator: TIntfCompare);
  TStrSortProc = procedure(const AList: IJclStrList; L, R: Integer;
    AComparator: TStrCompare);
  TSortProc = procedure(const AList: IJclList; L, R: Integer;
    AComparator: TCompare);

procedure QuickSort(const AList: IJclIntfList; L, R: Integer;
  AComparator: TIntfCompare); overload;
procedure QuickSort(const AList: IJclStrList; L, R: Integer;
  AComparator: TStrCompare); overload;
procedure QuickSort(const AList: IJclList; L, R: Integer;
  AComparator: TCompare); overload;

var
  IntfSortProc: TIntfSortProc = QuickSort;
  StrSortProc: TStrSortProc = QuickSort;
  SortProc: TSortProc = QuickSort;

// Sort algorithms
procedure Sort(const AList: IJclIntfList; First, Last: Integer;
  AComparator: TIntfCompare); overload;
procedure Sort(const AList: IJclStrList; First, Last: Integer;
  AComparator: TStrCompare); overload;
procedure Sort(const AList: IJclList; First, Last: Integer;
  AComparator: TCompare); overload;

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

function StrSimpleCompare(const Obj1, Obj2: string): Integer;
begin
  // (rom) changed to case sensitive compare
  Result := CompareStr(Obj1, Obj2);
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

procedure Apply(const First: IJclIntfIterator; Count: Integer;
  F: TIntfApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.SetObject(F(First.GetObject));
      First.Next;
    end
    else
      Break;
end;

procedure Apply(const First: IJclStrIterator; Count: Integer;
  F: TStrApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.SetString(F(First.GetString));
      First.Next;
    end
    else
      Break;
end;

procedure Apply(const First: IJclIterator; Count: Integer; F: TApplyFunction);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.SetObject(F(First.GetObject));
      First.Next;
    end
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
      if AComparator(First.GetObject, AInterface) = 0 then
      begin
        Result := First;
        Break;
      end;
      First.Next;
    end
    else
      Break;
end;

function Find(const First: IJclStrIterator; Count: Integer;
  const AString: string; AComparator: TStrCompare): IJclStrIterator;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      if AComparator(First.GetString, AString) = 0 then
      begin
        Result := First;
        Break;
      end;
      First.Next;
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
      if AComparator(First.GetObject, AObject) = 0 then
      begin
        Result := First;
        Break;
      end;
      First.Next;
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

function CountObject(const First: IJclStrIterator; Count: Integer;
  const AString: string; AComparator: TStrCompare): Integer;
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

function CountObject(const First: IJclIterator; Count: Integer;
  AObject: TObject;
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
      Output.SetObject(First.GetObject);
      First.Next;
      Output.Next;
    end
    else
      Break;
end;

procedure Copy(const First: IJclStrIterator; Count: Integer;
  const Output: IJclStrIterator);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Output.HasNext and First.HasNext then
    begin
      Output.SetString(First.GetString);
      First.Next;
      Output.Next;
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
      Output.SetObject(First.GetObject);
      First.Next;
      Output.Next;
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

procedure Generate(const List: IJclStrList; Count: Integer;
  const AString: string);
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
      First.SetObject(AInterface);
      First.Next;
    end
    else
      Break;
end;

procedure Fill(const First: IJclStrIterator; Count: Integer;
  const AString: string);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if First.HasNext then
    begin
      First.SetString(AString);
      First.Next;
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
      First.SetObject(AObject);
      First.Next;
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
    Obj := First.GetObject;
    Last.Previous;
    First.SetObject(Last.GetObject);
    Last.SetObject(Obj);
    First.Next;
  end;
end;

procedure Reverse(const First, Last: IJclStrIterator);
var
  Obj: string;
begin
  if not First.HasNext then
    Exit;
  if not Last.HasPrevious then
    Exit;
  while First.NextIndex <= Last.PreviousIndex do
  begin
    Obj := First.GetString;
    Last.Previous;
    First.SetString(Last.GetString);
    Last.SetString(Obj);
    First.Next;
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
    Obj := First.GetObject;
    Last.Previous;
    First.SetObject(Last.GetObject);
    Last.SetObject(Obj);
    First.Next;
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

procedure QuickSort(const AList: IJclStrList; L, R: Integer;
  AComparator: TStrCompare);
var
  I, J, P: Integer;
  Obj: string;
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

procedure QuickSort(const AList: IJclList; L, R: Integer;
  AComparator: TCompare);
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

procedure Sort(const AList: IJclIntfList; First, Last: Integer;
  AComparator: TIntfCompare);
begin
  IntfSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclStrList; First, Last: Integer;
  AComparator: TStrCompare);
begin
  StrSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclList; First, Last: Integer;
  AComparator: TCompare);
begin
  SortProc(AList, First, Last, AComparator);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
