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
{ The Original Code is JclStatistics.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is ESB Consultancy.                                   }
{ Portions created by ESB Consultancy are Copyright ESB Consultancy. All rights reserved.          }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel, ESB Consultancy, Robert Marquardt, Robert Rossmair, Matthias Thoma,         }
{   Petr Vones                                                                                     }  
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Various common statistics routines to calculate, for example, the arithmetic mean, geometric     }
{ meanor median of a set of numbers.                                                               }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclStatistics;

{$I jcl.inc}

interface

uses
  JclBase;

{ Mean functions }

function ArithmeticMean(const X: TDynFloatArray): Float;
function GeometricMean(const X: TDynFloatArray): Float;
function HarmonicMean(const X: TDynFloatArray): Float;
function HeronianMean(const A, B: Float): Float;

{ Miscellanous }

function BinomialCoeff(N, R: Cardinal): Float;
function IsPositiveFloatArray(const X: TDynFloatArray): Boolean;
function MaxFloatArray(const B: TDynFloatArray): Float;
function MaxFloatArrayIndex(const B: TDynFloatArray): Integer;
function Median(const X: TDynFloatArray): Float;
function MedianUnsorted(const X: TDynFloatArray): Float;
function MinFloatArray(const B: TDynFloatArray): Float;
function MinFloatArrayIndex(const B: TDynFloatArray): Integer;
function Permutation(N, R: Cardinal): Float;
function PopulationVariance(const X: TDynFloatArray): Float;
procedure PopulationVarianceAndMean(const X: TDynFloatArray; var Variance, Mean: Float);
function SampleVariance(const X: TDynFloatArray): Float;
procedure SampleVarianceAndMean(const X: TDynFloatArray; var Variance, Mean: Float);
function SumFloatArray(const B: TDynFloatArray): Float;
function SumSquareDiffFloatArray(const B: TDynFloatArray; Diff: Float): Float;
function SumSquareFloatArray(const B: TDynFloatArray): Float;
function SumPairProductFloatArray(const X, Y: TDynFloatArray): Float;

implementation

uses
  JclLogic, JclMath, JclResources, JclSysUtils;

//==================================================================================================
// Local helpers
//==================================================================================================

function GetDynLength(const X: TDynFloatArray): Integer;
begin
  Result := Length(X);
end;

//--------------------------------------------------------------------------------------------------

function GetDynLengthNotNull(const X: TDynFloatArray): Integer;
begin
  Result := Length(X);
  if Result = 0 then
    raise EJclMathError.CreateResRec(@RsEmptyArray);
end;

//==================================================================================================
// Mean Functions
//==================================================================================================

function ArithmeticMean(const X: TDynFloatArray): Float;
begin
  Result := SumFloatArray(X) / Length(X);
end;

//--------------------------------------------------------------------------------------------------

function GeometricMean(const X: TDynFloatArray): Float;
var
  I, L: Integer;
begin
  L := GetDynLengthNotNull(X);
  Result := 1.0;
  for I := 0 to L - 1 do
  begin
    if X[I] <= PrecisionTolerance then
      raise EJclMathError.CreateResRec(@RsNonPositiveArray);
    Result := Result * X[I];
  end;
  Result := Power(Result, 1 / L);
end;

//--------------------------------------------------------------------------------------------------

function HarmonicMean(const X: TDynFloatArray): Float;
var
  I, L: Integer;
begin
  Result := 0.0;
  L := GetDynLengthNotNull(X);
  for I := 0 to L - 1 do
  begin
    if X[I] <= PrecisionTolerance then
      raise EJclMathError.CreateResRec(@RsNonPositiveArray);
    Result := Result + 1 / X[I];
  end;
  Result := L / Result;
end;

//--------------------------------------------------------------------------------------------------

function HeronianMean(const A, B: Float): Float;
begin
  Assert(A >= 0);
  Assert(B >= 0);
  Result := (A + Sqrt(A * B) + B) / 3;
end;

//==================================================================================================
// Miscellanous
//==================================================================================================

function BinomialCoeff(N, R: Cardinal): Float;
var
  I: Integer;
  K: LongWord;
begin
  if (N = 0) or (R > N) or (N > MaxFactorial) then
  begin
    Result := 0.0;
    Exit;
  end;
  Result := 1.0;
  if not ((R = 0) or (R = N)) then
  begin
    if R > N div 2 then
    R := N - R;
    K := 2;
    try
      for I := N - R + 1 to N do
      begin
        Result := Result * I;
        if K <= R then
        begin
          Result := Result / K;
          Inc(K);
        end;
      end;
      Result := Int(Result + 0.5);
    except
      Result := -1.0;
    end;
  end;
end;


//--------------------------------------------------------------------------------------------------

function IsPositiveFloatArray(const X: TDynFloatArray): Boolean;
var
  I, L: Integer;
begin
  Result := False;
  L := GetDynLengthNotNull(X);
  for I := 0 to L - 1 do
    if X[I] <= PrecisionTolerance then
      Exit;
  Result := True;
end;

//--------------------------------------------------------------------------------------------------

function MaxFloatArray(const B: TDynFloatArray): Float;
var
  I, L: Integer;
begin
  L := GetDynLengthNotNull(B);
  Result := B[0];
  for I := 1 to L - 1 do
    if B[I] > Result then
      Result := B[I];
end;

//--------------------------------------------------------------------------------------------------

function MaxFloatArrayIndex(const B: TDynFloatArray): Integer;
var
  I, L: Integer;
  Max: Float;
begin
  Result := 0;
  L := GetDynLengthNotNull(B);
  Max := B[0];
  for I := 1 to L - 1 do
    if B[I] > Max then
    begin
      Max := B[I];
      Result := I;
    end;
end;

//--------------------------------------------------------------------------------------------------

// The FloatArray X must be presorted so Median can calculate the correct value.
//            Y_{(n+1)/2}                     if N is odd
// Median = { 1/2 * (Y_{n/2} + Y_{1+(n/2) }   if N is even

function Median(const X: TDynFloatArray): Float;
var
  N: Integer;
begin
  N := GetDynLengthNotNull(X);
  if N = 1 then
    Result := X[0]
  else
  if Odd(N) then
    Result := X[N div 2]
  else
    Result := (X[N div 2 - 1] + X[N div 2]) / 2;
end;

//--------------------------------------------------------------------------------------------------

function MedianUnsorted(const X: TDynFloatArray): Float;
var
  SortedList: TDynFloatArray;

begin
  // We need to sort the values first
  SortedList := Copy(X);
  // type cast to Pointer for the sake of FPC
  SortDynArray(Pointer(SortedList), SizeOf(Float),DynArrayCompareFloat);

  // and call the median function afterwards
  Result := Median(SortedList);
end;

//--------------------------------------------------------------------------------------------------

function MinFloatArray(const B: TDynFloatArray): Float;
var
  I, L: Integer;
begin
  L := GetDynLengthNotNull(B);
  Result := B[0];
  for I := 1 to L - 1 do
    if B[I] < Result then
      Result := B[I];
end;

//--------------------------------------------------------------------------------------------------

function MinFloatArrayIndex(const B: TDynFloatArray): Integer;
var
  I, L: Integer;
  Min: Float;
begin
  Result := 0;
  L := GetDynLengthNotNull(B);
  Min := B[0];
  for I := 1 to L - 1 do
    if B[I] < Min then
    begin
      Min := B[I];
      Result := I;
    end;
end;

//--------------------------------------------------------------------------------------------------

function Permutation(N, R: Cardinal): Float;
var
  I : Integer;
begin
  if (N = 0) or (R > N) or (N > MaxFactorial) then
  begin
    Result := 0.0;
    Exit;
  end;
  Result := 1.0;
  if R <> 0 then
    try
      for I := N downto N - R + 1 do
        Result := Result * I;
      Result := Int(Result + 0.5);
    except
      Result := -1.0;
    end;
end;

//--------------------------------------------------------------------------------------------------

function PopulationVariance(const X: TDynFloatArray): Float;
var
  I, L: Integer;
  Sum: Float;
begin
  L := GetDynLengthNotNull(X);
  Result := Sqr(X[0]);
  Sum := X[0];
  for I := 1 to L - 1 do
  begin
    Result := Result + Sqr(X[I]);
    Sum := Sum + X[I];
  end;
  Result := Result / L;
  Result := Result - Sqr(Sum / L);
end;

//--------------------------------------------------------------------------------------------------

procedure PopulationVarianceAndMean(const X: TDynFloatArray; var Variance, Mean: Float);
var
  I, L: Integer;
  Sum, SumSq: Float;
begin
  L := GetDynLengthNotNull(X);
  SumSq := Sqr(X[0]);
  Sum := X[0];
  for I := 1 to L - 1 do
  begin
    SumSq := SumSq + Sqr(X[I]);
    Sum := Sum + X[I];
  end;
  Mean := Sum / L;
  Variance := (SumSq / L) - Sqr(Mean);
end;

//--------------------------------------------------------------------------------------------------

function SampleVariance(const X: TDynFloatArray): Float;
var
  I, L: Integer;
  Sum: Float;
begin
  L := GetDynLengthNotNull(X);
  Result := Sqr(X[0]);
  Sum := X[0];
  for I := 1 to L - 1 do
  begin
    Result := Result + Sqr(X[I]);
    Sum := Sum + X[I];
  end;
  if L > 1 then
  begin
    Result := Result / (L - 1);
    Result := Result - Sqr(Sum / (L - 1));
  end
  else
    Result := 0.0;
end;

//--------------------------------------------------------------------------------------------------

procedure SampleVarianceAndMean(const X: TDynFloatArray; var Variance, Mean: Float);
var
  I, L: Integer;
  Sum, SumSq: Float;
begin
  L := GetDynLengthNotNull(X);
  SumSq := Sqr(X[0]);
  Sum := X[0];
  for I := 1 to L - 1 do
  begin
    SumSq := SumSq + Sqr(X[I]);
    Sum := Sum + X[I];
  end;
  Mean := Sum / L;
  if L > 1 then
    Variance := (SumSq / (L - 1)) - Sqr(Sum / (L - 1))
  else
    Variance := 0.0;
end;

//--------------------------------------------------------------------------------------------------

function SumFloatArray(const B: TDynFloatArray): Float;
var
  I, L: Integer;
begin
  Result := 0.0;
  L := GetDynLength(B);
  if L <> 0 then
  begin
    Result := B[0];
    for I := 1 to L - 1 do
      Result := Result + B[I];
  end;
end;

//--------------------------------------------------------------------------------------------------

function SumSquareDiffFloatArray(const B: TDynFloatArray; Diff: Float): Float;
var
  I, L: Integer;
begin
  Result := 0.0;
  L := GetDynLength(B);
  if L <> 0 then
  begin
    Result := Sqr(B[0] - Diff);
    for I := 1 to L - 1 do
      Result := Result + Sqr(B[I] - Diff);
  end;
end;

//--------------------------------------------------------------------------------------------------

function SumSquareFloatArray(const B: TDynFloatArray): Float;
var
  I, L: Integer;
begin
  Result := 0.0;
  L := GetDynLength(B);
  if L <> 0 then
  begin
    Result := Sqr(B[0]);
    for I := 1 to L - 1 do
      Result := Result + Sqr(B[I]);
  end;
end;

//--------------------------------------------------------------------------------------------------

function SumPairProductFloatArray(const X, Y: TDynFloatArray): Float;
var
  I, L: Integer;
begin
  Result := 0.0;
  L := Min(Length(X), Length(Y));
  if L <> 0 then
  begin
    Result := X[0] * Y[0];
    for I := 1 to L - 1 do
      Result := Result + X[I] * Y[I];
  end;
end;

//--------------------------------------------------------------------------------------------------

function ChiSquare(const X: TDynFloatArray): Float;  { TODO : ChiSquare }
var
  I, L: Integer;
  Sum: Float;
begin
  L := GetDynLengthNotNull(X);
  Result := Sqr(X[0]);
  Sum := X[0];
  for I := 1 to L - 1 do
  begin
    Result := Result + Sqr(X[I]);
    Sum := Sum + X[I];
  end;
end;

// History:

// $Log$
// Revision 1.8  2004/07/29 15:16:51  marquardt
// simple style cleaning
//
// Revision 1.7  2004/05/05 07:18:31  rrossmair
// MedianUnsorted: type cast for FPC compatibility
//
// Revision 1.6  2004/05/05 00:09:59  mthoma
// Updated headers: Added donors as contributors, adjusted the initial authors, added cvs names when they were not obvious. Changed $data to $date where necessary,
//
// Revision 1.5  2004/04/08 17:14:46  mthoma
// no message
//
// Revision 1.4  2004/04/08 16:57:21  mthoma
// Fixed #1268. Introduced new function MedianUnsorted
//
// Revision 1.3  2004/04/06 04:53:18  peterjhaas
// adapt compiler conditions, add log entry
//

end.
