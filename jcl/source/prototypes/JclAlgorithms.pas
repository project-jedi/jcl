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
{$I containers\JclAlgorithms.int}
{$I containers\JclAlgorithms.imp}
interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBase, JclContainerIntf;

// Compare functions
(*$JPPLOOP ALLTYPEINDEX ALLTYPECOUNT
{$JPPEXPANDMACRO SIMPLECOMPAREINT(,,)}*)

{$JPPEXPANDMACRO SIMPLECOMPAREINT(IntegerCompare,,TObject)}

// Compare functions for equality
(*$JPPLOOP ALLTYPEINDEX ALLTYPECOUNT
{$JPPEXPANDMACRO SIMPLEEQUALITYCOMPAREINT(,,)}*)

// Apply algorithms
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO APPLYINT(Apply,,, overload;)}*)

// Find algorithms
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO FINDINT(Find,,,,,, overload;)}
{$JPPEXPANDMACRO FINDEQINT(Find,,,,,, overload;)}*)

// CountObject algorithms
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO COUNTOBJECTINT(CountObject,,,,,, overload;)}
{$JPPEXPANDMACRO COUNTOBJECTEQINT(CountObject,,,,,, overload;)}*)

// Copy algorithms
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO COPYINT(Copy,, overload;)}*)

// Generate algorithms
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO GENERATEINT(Generate,,,,, overload;)}*)

// Fill algorithms
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO FILLINT(Fill,,,,, overload;)}*)

// Reverse algorithms
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO REVERSEINT(Reverse,, overload;)}*)

(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO SORTINT(QuickSort,,,,, overload;)}*)

var
  IntfSortProc: TIntfSortProc = QuickSort;
  AnsiStrSortProc: TAnsiStrSortProc = QuickSort;
  WideStrSortProc: TWideStrSortProc = QuickSort;
  {$IFDEF SUPPORTS_UNICODE_STRING}
  UnicodeStrSortProc: TUnicodeStrSortProc = QuickSort;
  {$ENDIF SUPPORTS_UNICODE_STRING}
  SingleSortProc: TSingleSortProc = QuickSort;
  DoubleSortProc: TDoubleSortProc = QuickSort;
  ExtendedSortProc: TExtendedSortProc = QuickSort;
  IntegerSortProc: TIntegerSortProc = QuickSort;
  CardinalSortProc: TCardinalSortProc = QuickSort;
  Int64SortProc: TInt64SortProc = QuickSort;
  PtrSortProc: TPtrSortProc = QuickSort;
  SortProc: TSortProc = QuickSort;

// Sort algorithms
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO SORTINT(Sort,,First,Last,, overload;)}*)

{$IFDEF SUPPORTS_GENERICS}
type
  // cannot implement generic global functions
  TJclAlgorithms<T> = class
  private
    //FSortProc: TSortProc;
  public
    class {$JPPEXPANDMACRO APPLYINT(Apply,IJclIterator<T>,TApplyFunction<T>,)}
    class {$JPPEXPANDMACRO FINDINT(Find,IJclIterator<T>,const ,AItem,T,TCompare<T>, overload;)}
    class {$JPPEXPANDMACRO FINDEQINT(Find,IJclIterator<T>,const ,AItem,T,TEqualityCompare<T>, overload;)}
    class {$JPPEXPANDMACRO COUNTOBJECTINT(CountObject,IJclIterator<T>,const ,AItem,T,TCompare<T>, overload;)}
    class {$JPPEXPANDMACRO COUNTOBJECTEQINT(CountObject,IJclIterator<T>,const ,AItem,T,TEqualityCompare<T>, overload;)}
    class {$JPPEXPANDMACRO COPYINT(Copy,IJclIterator<T>,)}
    class {$JPPEXPANDMACRO GENERATEINT(Generate,IJclList<T>,const ,AItem,T,)}
    class {$JPPEXPANDMACRO FILLINT(Fill,IJclIterator<T>,const ,AItem,T,)}
    class {$JPPEXPANDMACRO REVERSEINT(Reverse,IJclIterator<T>,)}
    class {$JPPEXPANDMACRO SORTINT(QuickSort,IJclList<T>,L,R,TCompare<T>,)}
    class {$JPPEXPANDMACRO SORTINT(Sort,IJclList<T>,First,Last,TCompare<T>,)}
    //class property SortProc: TSortProc<T> read FSortProc write FSortProc;
  end;
{$ENDIF SUPPORTS_GENERICS}

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\common';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNIT_ANSISTRINGS}
  AnsiStrings,
  {$ENDIF HAS_UNIT_ANSISTRINGS}
  SysUtils;

function IntfSimpleCompare(const Obj1, Obj2: IInterface): Integer;
begin
  if Integer(Obj1) < Integer(Obj2) then
    Result := -1
  else
  if Integer(Obj1) > Integer(Obj2) then
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

{$IFDEF SUPPORTS_UNICODE_STRING}
function UnicodeStrSimpleCompare(const Obj1, Obj2: UnicodeString): Integer;
begin
  Result := CompareStr(Obj1, Obj2);
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

function StrSimpleCompare(const Obj1, Obj2: string): Integer;
begin
  case SizeOf(Obj1[1]) of
    SizeOf(AnsiChar):
      Result := CompareStr(Obj1, Obj2);
    SizeOf(WideChar):
      {$IFDEF SUPPORTS_UNICODE}
      Result := CompareStr(Obj1, Obj2);
      {$ELSE ~SUPPORTS_UNICODE}
      Result := WideCompareStr(Obj1, Obj2);
      {$ENDIF ~SUPPORTS_UNICODE}
  else
    raise EJclOperationNotSupportedError.Create;
  end;
end;

function SingleSimpleCompare(const Obj1, Obj2: Single): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

function DoubleSimpleCompare(const Obj1, Obj2: Double): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

function ExtendedSimpleCompare(const Obj1, Obj2: Extended): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

function FloatSimpleCompare(const Obj1, Obj2: Float): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

function IntegerSimpleCompare(Obj1, Obj2: Integer): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

function CardinalSimpleCompare(Obj1, Obj2: Cardinal): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

function Int64SimpleCompare(const Obj1, Obj2: Int64): Integer;
begin
  if Obj1 < Obj2 then
    Result := -1
  else
  if Obj1 > Obj2 then
    Result := 1
  else
    Result := 0;
end;

function PtrSimpleCompare(Obj1, Obj2: Pointer): Integer;
begin
  if Integer(Obj1) < Integer(Obj2) then
    Result := -1
  else
  if Integer(Obj1) > Integer(Obj2) then
    Result := 1
  else
    Result := 0;
end;

function SimpleCompare(Obj1, Obj2: TObject): Integer;
begin
  if Integer(Obj1) < Integer(Obj2) then
    Result := -1
  else
  if Integer(Obj1) > Integer(Obj2) then
    Result := 1
  else
    Result := 0;
end;

function IntegerCompare(Obj1, Obj2: TObject): Integer;
begin
  if Integer(Obj1) < Integer(Obj2) then
    Result := -1
  else
  if Integer(Obj1) > Integer(Obj2) then
    Result := 1
  else
    Result := 0;
end;

function IntfSimpleEqualityCompare(const Obj1, Obj2: IInterface): Boolean;
begin
  Result := Integer(Obj1) = Integer(Obj2);
end;

function AnsiStrSimpleEqualityCompare(const Obj1, Obj2: AnsiString): Boolean;
begin
  // (rom) changed to case sensitive compare
  Result := CompareStr(Obj1, Obj2) = 0;
end;

function WideStrSimpleEqualityCompare(const Obj1, Obj2: WideString): Boolean;
begin
  // (rom) changed to case sensitive compare
  Result := WideCompareStr(Obj1, Obj2) = 0;
end;

{$IFDEF SUPPORTS_UNICODE_STRING}
function UnicodeStrSimpleEqualityCompare(const Obj1, Obj2: UnicodeString): Boolean;
begin
  Result := CompareStr(Obj1, Obj2) = 0;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

function StrSimpleEqualityCompare(const Obj1, Obj2: string): Boolean;
begin
  case SizeOf(Obj1[1]) of
    SizeOf(AnsiChar):
      Result := CompareStr(Obj1, Obj2) = 0;
    SizeOf(WideChar):
      Result := WideCompareStr(Obj1, Obj2) = 0;
  else
    raise EJclOperationNotSupportedError.Create;
  end;
end;

function SingleSimpleEqualityCompare(const Obj1, Obj2: Single): Boolean;
begin
  Result := Obj1 = Obj2;
end;

function DoubleSimpleEqualityCompare(const Obj1, Obj2: Double): Boolean;
begin
  Result := Obj1 = Obj2;
end;

function ExtendedSimpleEqualityCompare(const Obj1, Obj2: Extended): Boolean;
begin
  Result := Obj1 = Obj2;
end;

function FloatSimpleEqualityCompare(const Obj1, Obj2: Float): Boolean;
begin
  Result := Obj1 = Obj2;
end;

function IntegerSimpleEqualityCompare(Obj1, Obj2: Integer): Boolean;
begin
  Result := Obj1 = Obj2;
end;

function CardinalSimpleEqualityCompare(Obj1, Obj2: Cardinal): Boolean;
begin
  Result := Obj1 = Obj2;
end;

function Int64SimpleEqualityCompare(const Obj1, Obj2: Int64): Boolean;
begin
  Result := Obj1 = Obj2;
end;

function PtrSimpleEqualityCompare(Obj1, Obj2: Pointer): Boolean;
begin
  Result := Integer(Obj1) = Integer(Obj2);
end;

function SimpleEqualityCompare(Obj1, Obj2: TObject): Boolean;
begin
  Result := Integer(Obj1) = Integer(Obj2);
end;

(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO APPLYIMP(Apply,,,)}
*)
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO FINDIMP(Find,,,,,)}

{$JPPEXPANDMACRO FINDEQIMP(Find,,,,,)}
*)
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO COUNTOBJECTIMP(CountObject,,,,,)}

{$JPPEXPANDMACRO COUNTOBJECTEQIMP(CountObject,,,,,)}
*)
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO COPYIMP(Copy,,)}
*)
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO GENERATEIMP(Generate,,,,)}
*)
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO FILLIMP(Fill,,,,,)}
*)
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO REVERSEIMP(Reverse,,,,)}
*)
(*$JPPLOOP TRUETYPEINDEX TRUETYPECOUNT
{$JPPEXPANDMACRO QUICKSORTIMP(QuickSort,,,,,,,)}
*)

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

{$IFDEF SUPPORTS_UNICODE_STRING}
procedure Sort(const AList: IJclUnicodeStrList; First, Last: Integer; AComparator: TUnicodeStrCompare);
begin
  UnicodeStrSortProc(AList, First, Last, AComparator);
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

procedure Sort(const AList: IJclSingleList; First, Last: Integer; AComparator: TSingleCompare);
begin
  SingleSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclDoubleList; First, Last: Integer; AComparator: TDoubleCompare);
begin
  DoubleSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclExtendedList; First, Last: Integer; AComparator: TExtendedCompare);
begin
  ExtendedSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclIntegerList; First, Last: Integer; AComparator: TIntegerCompare);
begin
  IntegerSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclCardinalList; First, Last: Integer; AComparator: TCardinalCompare);
begin
  CardinalSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclInt64List; First, Last: Integer; AComparator: TInt64Compare);
begin
  Int64SortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclPtrList; First, Last: Integer; AComparator: TPtrCompare);
begin
  PtrSortProc(AList, First, Last, AComparator);
end;

procedure Sort(const AList: IJclList; First, Last: Integer; AComparator: TCompare);
begin
  SortProc(AList, First, Last, AComparator);
end;

{$IFDEF SUPPORTS_GENERICS}
class {$JPPEXPANDMACRO APPLYIMP(TJclAlgorithms<T>.Apply,IJclIterator<T>,TApplyFunction<T>,SetItem)}

class {$JPPEXPANDMACRO FINDIMP(TJclAlgorithms<T>.Find,IJclIterator<T>,const ,AItem,T,TCompare<T>)}

class {$JPPEXPANDMACRO FINDEQIMP(TJclAlgorithms<T>.Find,IJclIterator<T>,const ,AItem,T,TEqualityCompare<T>)}

class {$JPPEXPANDMACRO COUNTOBJECTIMP(TJclAlgorithms<T>.CountObject,IJclIterator<T>,const ,AItem,T,TCompare<T>)}

class {$JPPEXPANDMACRO COUNTOBJECTEQIMP(TJclAlgorithms<T>.CountObject,IJclIterator<T>,const ,AItem,T,TEqualityCompare<T>)}

class {$JPPEXPANDMACRO COPYIMP(TJclAlgorithms<T>.Copy,IJclIterator<T>,SetItem)}

class {$JPPEXPANDMACRO GENERATEIMP(TJclAlgorithms<T>.Generate,IJclList<T>,const ,AItem,T)}

class {$JPPEXPANDMACRO FILLIMP(TJclAlgorithms<T>.Fill,IJclIterator<T>,const ,AItem,T,SetItem)}

class {$JPPEXPANDMACRO REVERSEIMP(TJclAlgorithms<T>.Reverse,IJclIterator<T>,T,GetItem,SetItem)}

class {$JPPEXPANDMACRO QUICKSORTIMP(TJclAlgorithms<T>.QuickSort,IJclList<T>,L,R,TCompare<T>,T,GetItem,SetItem)}

class procedure TJclAlgorithms<T>.Sort(const AList: IJclList<T>; First, Last: Integer;
  AComparator: TCompare<T>);
begin
  TJclAlgorithms<T>.QuickSort(AList, First, Last, AComparator);
end;
{$ENDIF SUPPORTS_GENERICS}


{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.