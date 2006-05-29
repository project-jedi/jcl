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
  JclBase, JclContainerIntf;

// function pointer types
type
  // pointer functions for Apply Algorithms
  TIntfApplyFunction = function(AInterface: IInterface): IInterface;
  TStrApplyFunction = function(const AString: string): string;
  TApplyFunction = function(AObject: TObject): TObject;
  // Pointer functions for comparator
  TIntfCompare = function(Obj1, Obj2: IInterface): Integer;
  TStrCompare = function(const Obj, Obj2: string): Integer;
  TCompare = function(Obj1, Obj2: TObject): Integer;

// Compare functions
function IntfSimpleCompare(Obj1, Obj2: IInterface): Integer;
function StrSimpleCompare(const Obj1, Obj2: string): Integer;
function SimpleCompare(Obj1, Obj2: TObject): Integer;

function IntegerCompare(Obj1, Obj2: TObject): Integer;

// Apply algorithms
procedure Apply(First: IJclIntfIterator; Count: Integer; F: TIntfApplyFunction); overload;
procedure Apply(First: IJclStrIterator; Count: Integer; F: TStrApplyFunction); overload;
procedure Apply(First: IJclIterator; Count: Integer; F: TApplyFunction); overload;

// Find algorithms
function Find(First: IJclIntfIterator; Count: Integer; AInterface: IInterface;
  AComparator: TIntfCompare): IJclIntfIterator; overload;
function Find(First: IJclStrIterator; Count: Integer; const AString: string;
  AComparator: TStrCompare): IJclStrIterator; overload;
function Find(First: IJclIterator; Count: Integer; AObject: TObject;
  AComparator: TCompare): IJclIterator; overload;

// CountObject algorithms
function CountObject(First: IJclIntfIterator; Count: Integer; AInterface: IInterface;
  AComparator: TIntfCompare): Integer; overload;
function CountObject(First: IJclStrIterator; Count: Integer; const AString: string;
  AComparator: TStrCompare): Integer; overload;
function CountObject(First: IJclIterator; Count: Integer; AObject: TObject;
  AComparator: TCompare): Integer; overload;

// Copy algorithms
procedure Copy(First: IJclIntfIterator; Count: Integer; Output: IJclIntfIterator); overload;
procedure Copy(First: IJclStrIterator; Count: Integer; Output: IJclStrIterator); overload;
procedure Copy(First: IJclIterator; Count: Integer; Output: IJclIterator); overload;

// Generate algorithms
procedure Generate(List: IJclIntfList; Count: Integer; AInterface: IInterface); overload;
procedure Generate(List: IJclStrList; Count: Integer; const AString: string); overload;
procedure Generate(List: IJclList; Count: Integer; AObject: TObject); overload;

// Fill algorithms
procedure Fill(First: IJclIntfIterator; Count: Integer; AInterface: IInterface); overload;
procedure Fill(First: IJclStrIterator; Count: Integer; const AString: string); overload;
procedure Fill(First: IJclIterator; Count: Integer; AObject: TObject); overload;

// Reverse algorithms
procedure Reverse(First, Last: IJclIntfIterator); overload;
procedure Reverse(First, Last: IJclStrIterator); overload;
procedure Reverse(First, Last: IJclIterator); overload;

type
  // Pointer functions for sort algorithms
  TIntfSortProc = procedure(AList: IJclIntfList; L, R: Integer; AComparator: TIntfCompare);
  TStrSortProc = procedure(AList: IJclStrList; L, R: Integer; AComparator: TStrCompare);
  TSortProc = procedure(AList: IJclList; L, R: Integer; AComparator: TCompare);

procedure QuickSort(AList: IJclIntfList; L, R: Integer; AComparator: TIntfCompare); overload;
procedure QuickSort(AList: IJclStrList; L, R: Integer; AComparator: TStrCompare); overload;
procedure QuickSort(AList: IJclList; L, R: Integer; AComparator: TCompare); overload;

var
  IntfSortProc: TIntfSortProc = QuickSort;
  StrSortProc: TStrSortProc = QuickSort;
  SortProc: TSortProc = QuickSort;

// Sort algorithms
procedure Sort(AList: IJclIntfList; First, Last: Integer; AComparator: TIntfCompare); overload;
procedure Sort(AList: IJclStrList; First, Last: Integer; AComparator: TStrCompare); overload;
procedure Sort(AList: IJclList; First, Last: Integer; AComparator: TCompare); overload;

implementation

uses
  SysUtils;

function IntfSimpleCompare(Obj1, Obj2: IInterface): Integer;
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

procedure Apply(First: IJclIntfIterator; Count: Integer; F: TIntfApplyFunction);
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

procedure Apply(First: IJclStrIterator; Count: Integer; F: TStrApplyFunction);
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

procedure Apply(First: IJclIterator; Count: Integer; F: TApplyFunction);
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

function Find(First: IJclIntfIterator; Count: Integer; AInterface: IInterface;
  AComparator: TIntfCompare): IJclIntfIterator;
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

function Find(First: IJclStrIterator; Count: Integer; const AString: string;
  AComparator: TStrCompare): IJclStrIterator;
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

function Find(First: IJclIterator; Count: Integer; AObject: TObject;
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

function CountObject(First: IJclIntfIterator; Count: Integer; AInterface: IInterface;
  AComparator: TIntfCompare): Integer;
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

function CountObject(First: IJclStrIterator; Count: Integer; const AString: string;
  AComparator: TStrCompare): Integer;
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

function CountObject(First: IJclIterator; Count: Integer; AObject: TObject;
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

procedure Copy(First: IJclIntfIterator; Count: Integer; Output: IJclIntfIterator);
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

procedure Copy(First: IJclStrIterator; Count: Integer; Output: IJclStrIterator);
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

procedure Copy(First: IJclIterator; Count: Integer; Output: IJclIterator);
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

procedure Generate(List: IJclIntfList; Count: Integer; AInterface: IInterface);
var
  I: Integer;
begin
  List.Clear;
  for I := 0 to Count - 1 do
    List.Add(AInterface);
end;

procedure Generate(List: IJclStrList; Count: Integer; const AString: string);
var
  I: Integer;
begin
  List.Clear;
  for I := Count - 1 downto 0 do
    List.Add(AString);
end;

procedure Generate(List: IJclList; Count: Integer; AObject: TObject);
var
  I: Integer;
begin
  List.Clear;
  for I := Count - 1 downto 0 do
    List.Add(AObject);
end;

procedure Fill(First: IJclIntfIterator; Count: Integer; AInterface: IInterface);
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

procedure Fill(First: IJclStrIterator; Count: Integer; const AString: string);
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

procedure Fill(First: IJclIterator; Count: Integer; AObject: TObject);
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

procedure Reverse(First, Last: IJclIntfIterator);
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

procedure Reverse(First, Last: IJclStrIterator);
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

procedure Reverse(First, Last: IJclIterator);
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

procedure QuickSort(AList: IJclIntfList; L, R: Integer; AComparator: TIntfCompare);
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

procedure QuickSort(AList: IJclStrList; L, R: Integer; AComparator: TStrCompare);
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

procedure QuickSort(AList: IJclList; L, R: Integer; AComparator: TCompare);
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

procedure Sort(AList: IJclIntfList; First, Last: Integer; AComparator: TIntfCompare);
begin
  IntfSortProc(AList, First, Last, AComparator);
end;

procedure Sort(AList: IJclStrList; First, Last: Integer; AComparator: TStrCompare);
begin
  StrSortProc(AList, First, Last, AComparator);
end;

procedure Sort(AList: IJclList; First, Last: Integer; AComparator: TCompare);
begin
  SortProc(AList, First, Last, AComparator);
end;

end.

