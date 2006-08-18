{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JclStatistics.pas.                                      }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ Various common statistics routines to calculate, for example, the arithmetic }
{ mean, geometric meanor median of a set of numbers.                           }
{                                                                              }
{ Unit owner:                                                                  }
{ Last modified: January 31, 2001                                              }
{                                                                              }
{******************************************************************************}

unit JclStatistics;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  JclBase;

function ArithmeticMean(const X: TDynFloatArray): Float;
function BinomialCoeff(N, R: Cardinal): Float;
function GeometricMean(const X: TDynFloatArray): Float;
function HarmonicMean(const X: TDynFloatArray): Float;
function IsPositiveFloatArray(const X: TDynFloatArray): Boolean;
function MaxFloatArray(const B: TDynFloatArray): Float;
function MaxFloatArrayIndex(const B: TDynFloatArray): Integer;
function Median(const X: TDynFloatArray): Float;
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
  JclLogic, JclMath, JclResources;

//==============================================================================
// Local helpers
//==============================================================================

function GetDynLength(const X: TDynFloatArray): Integer;
begin
  {$IFDEF SUPPORTS_DYNAMICARRAYS}
  Result := Length(X);
  {$ELSE}
  Result := DynArrayLength(X);
  {$ENDIF SUPPORTS_DYNAMICARRAYS}
end;

//------------------------------------------------------------------------------

function GetDynLengthNotNull(const X: TDynFloatArray): Integer;
begin
  {$IFDEF SUPPORTS_DYNAMICARRAYS}
  Result := Length(X);
  {$ELSE}
  Result := DynArrayLength(X);
  {$ENDIF SUPPORTS_DYNAMICARRAYS}
  if Result = 0 then
    raise EJclMathError.CreateResRec(@RsEmptyArray);
end;

//==============================================================================
// Statistics
//==============================================================================

function ArithmeticMean(const X: TDynFloatArray): Float;
begin
  {$IFDEF SUPPORTS_DYNAMICARRAYS}
  Result := SumFloatArray(X) / Length(X);
  {$ELSE}
  Result := SumFloatArray(X) / DynArrayLength(X);
  {$ENDIF SUPPORTS_DYNAMICARRAYS}
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function GeometricMean(const X: TDynFloatArray): Float;
var
  I, L: Integer;
begin
  L := GetDynLengthNotNull(X);
  Result := 1.0;
  {$IFDEF SUPPORTS_DYNAMICARRAYS}
  for I := 0 to L - 1 do
  begin
    if X[I] <= PrecisionTolerance then
      raise EJclMathError.CreateResRec(@RsNonPositiveArray);
    Result := Result * X[I];
  end;
  {$ELSE}
  for I := 0 to L - 1 do
  begin
    if X^[I] <= PrecisionTolerance then
      raise EJclMathError.CreateResRec(@RsNonPositiveArray);
    Result := Result * X^[I];
  end;
  {$ENDIF SUPPORTS_DYNAMICARRAYS}
  Result := Power(Result, 1 / L);
end;

//------------------------------------------------------------------------------

function HarmonicMean(const X: TDynFloatArray): Float;
var
  I, L: Integer;
begin
  Result := 0.0;
  L := GetDynLengthNotNull(X);
  for I := 0 to L - 1 do
  begin
    {$IFDEF SUPPORTS_DYNAMICARRAYS}
    if X[I] <= PrecisionTolerance then
      raise EJclMathError.CreateResRec(@RsNonPositiveArray);
    Result := Result + 1 / X[I];
    {$ELSE}
    if X^[I] <= PrecisionTolerance then
      raise EJclMathError.CreateResRec(@RsNonPositiveArray);
    Result := Result + 1 / X^[I];
    {$ENDIF SUPPORTS_DYNAMICARRAYS}
  end;
  Result := L / Result;
end;

//------------------------------------------------------------------------------

function IsPositiveFloatArray(const X: TDynFloatArray): Boolean;
var
  I, L: Integer;
begin
  Result := False;
  L := GetDynLengthNotNull(X);
  for I := 0 to L - 1 do
  begin
    {$IFDEF SUPPORTS_DYNAMICARRAYS}
    if X[I] <= PrecisionTolerance then
      Exit;
    {$ELSE}
    if X^[I] <= PrecisionTolerance then
      Exit;
    {$ENDIF SUPPORTS_DYNAMICARRAYS}
  end;
  Result := True;
end;

//------------------------------------------------------------------------------

function MaxFloatArray(const B: TDynFloatArray): Float;
var
  I, L: Integer;
begin
  L := GetDynLengthNotNull(B);
  {$IFDEF SUPPORTS_DYNAMICARRAYS}
  Result := B[0];
  for I := 1 to L - 1 do
    if B[I] > Result then
      Result := B[I];
  {$ELSE}
  Result := B^[0];
  for I := 1 to L - 1 do
    if B^[I] > Result then
      Result := B^[I];
  {$ENDIF SUPPORTS_DYNAMICARRAYS}
end;

//------------------------------------------------------------------------------

function MaxFloatArrayIndex(const B: TDynFloatArray): Integer;
var
  I, L: Integer;
  Max: Float;
begin
  Result := 0;
  L := GetDynLengthNotNull(B);
  {$IFDEF SUPPORTS_DYNAMICARRAYS}
  Max := B[0];
  for I := 1 to L - 1 do
    if B[I] > Max then
    begin
      Max := B[I];
      Result := I;
    end;
  {$ELSE}
  Max := B^[0];
  for I := 1 to L - 1 do
    if B^[I] > Max then
    begin
      Max := B^[I];
      Result := I;
    end;
  {$ENDIF SUPPORTS_DYNAMICARRAYS}
end;

//------------------------------------------------------------------------------

function Median(const X: TDynFloatArray): Float;
var
  N: Integer;
begin
  N := GetDynLengthNotNull(X);
  {$IFDEF SUPPORTS_DYNAMICARRAYS}
  if N = 1 then
    Result := X[0]
  else
  if Odd(N) then
    Result := X[N div 2]
  else
    Result := (X[N div 2 - 1] + X[N div 2]) / 2;
  {$ELSE}
  if N = 1 then
    Result := X^[0]
  else
  if Odd(N) then
    Result := X^[N div 2]
  else
    Result := (X^[N div 2 - 1] + X^[N div 2]) / 2;
  {$ENDIF SUPPORTS_DYNAMICARRAYS}
end;

//------------------------------------------------------------------------------

function MinFloatArray(const B: TDynFloatArray): Float;
var
  I, L: Integer;
begin
  L := GetDynLengthNotNull(B);
  {$IFDEF SUPPORTS_DYNAMICARRAYS}
  Result := B[0];
  for I := 1 to L - 1 do
    if B[I] < Result then
      Result := B[I];
  {$ELSE}
  Result := B^[0];
  for I := 1 to L - 1 do
    if B^[I] > Result then
      Result := B^[I];
  {$ENDIF SUPPORTS_DYNAMICARRAYS}
end;

//------------------------------------------------------------------------------

function MinFloatArrayIndex(const B: TDynFloatArray): Integer;
var
  I, L: Integer;
  Min: Float;
begin
  Result := 0;
  L := GetDynLengthNotNull(B);
  {$IFDEF SUPPORTS_DYNAMICARRAYS}
  Min := B[0];
  for I := 1 to L - 1 do
    if B[I] < Min then
    begin
      Min := B[I];
      Result := I;
    end;
  {$ELSE}
  Min := B^[0];
  for I := 1 to L - 1 do
    if B^[I] > Min then
    begin
      Min := B^[I];
      Result := I;
    end;
  {$ENDIF SUPPORTS_DYNAMICARRAYS}
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

function PopulationVariance(const X: TDynFloatArray): Float;
var
  I, L: Integer;
  Sum: Float;
begin
  L := GetDynLengthNotNull(X);
  {$IFDEF SUPPORTS_DYNAMICARRAYS}
  Result := Sqr(X[0]);
  Sum := X[0];
  for I := 1 to L - 1 do
  begin
    Result := Result + Sqr(X[I]);
    Sum := Sum + X[I];
  end;
  {$ELSE}
  Result := Sqr(X^[0]);
  Sum := X^[0];
  for I := 1 to L - 1 do
  begin
    Result := Result + Sqr(X^[I]);
    Sum := Sum + X^[I];
  end;
  {$ENDIF SUPPORTS_DYNAMICARRAYS}
  Result := Result / L;
  Result := Result - Sqr(Sum / L);
end;

//------------------------------------------------------------------------------

procedure PopulationVarianceAndMean(const X: TDynFloatArray; var Variance, Mean: Float);
var
  I, L: Integer;
  Sum, SumSq: Float;
begin
  L := GetDynLengthNotNull(X);
  {$IFDEF SUPPORTS_DYNAMICARRAYS}
  SumSq := Sqr(X[0]);
  Sum := X[0];
  for I := 1 to L - 1 do
  begin
    SumSq := SumSq + Sqr(X[I]);
    Sum := Sum + X[I];
  end;
  {$ELSE}
  SumSq := Sqr(X^[0]);
  Sum := X^[0];
  for I := 1 to L - 1 do
  begin
    SumSq := SumSq + Sqr(X^[I]);
    Sum := Sum + X^[I];
  end;
  {$ENDIF SUPPORTS_DYNAMICARRAYS}
  Mean := Sum / L;
  Variance := (SumSq / L) - Sqr(Mean);
end;

//------------------------------------------------------------------------------

function SampleVariance(const X: TDynFloatArray): Float;
var
  I, L: Integer;
  Sum: Float;
begin
  L := GetDynLengthNotNull(X);
  {$IFDEF SUPPORTS_DYNAMICARRAYS}
  Result := Sqr(X[0]);
  Sum := X[0];
  for I := 1 to L - 1 do
  begin
    Result := Result + Sqr(X[I]);
    Sum := Sum + X[I];
  end;
  {$ELSE}
  Result := Sqr(X^[0]);
  Sum := X^[0];
  for I := 1 to L - 1 do
  begin
    Result := Result + Sqr(X^[I]);
    Sum := Sum + X^[I];
  end;
  {$ENDIF SUPPORTS_DYNAMICARRAYS}
  if L > 1 then
  begin
    Result := Result / (L - 1);
    Result := Result - Sqr(Sum / (L - 1));
  end
  else
    Result := 0.0;
end;

//------------------------------------------------------------------------------

procedure SampleVarianceAndMean(const X: TDynFloatArray; var Variance, Mean: Float);
var
  I, L: Integer;
  Sum, SumSq: Float;
begin
  L := GetDynLengthNotNull(X);
  {$IFDEF SUPPORTS_DYNAMICARRAYS}
  SumSq := Sqr(X[0]);
  Sum := X[0];
  for I := 1 to L - 1 do
  begin
    SumSq := SumSq + Sqr(X[I]);
    Sum := Sum + X[I];
  end;
  {$ELSE}
  SumSq := Sqr(X^[0]);
  Sum := X^[0];
  for I := 1 to L - 1 do
  begin
    SumSq := SumSq + Sqr(X^[I]);
    Sum := Sum + X^[I];
  end;
  {$ENDIF SUPPORTS_DYNAMICARRAYS}
  Mean := Sum / L;
  if L > 1 then
    Variance := (SumSq / (L - 1)) - Sqr(Sum / (L - 1))
  else
    Variance := 0.0;
end;

//------------------------------------------------------------------------------

function SumFloatArray(const B: TDynFloatArray): Float;
var
  I, L: Integer;
begin
  Result := 0.0;
  L := GetDynLength(B);
  if L <> 0 then
  begin
    {$IFDEF SUPPORTS_DYNAMICARRAYS}
    Result := B[0];
    for I := 1 to L - 1 do
      Result := Result + B[I];
    {$ELSE}
    Result := B^[0];
    for I := 1 to L - 1 do
      Result := Result + B^[I];
    {$ENDIF SUPPORTS_DYNAMICARRAYS}
  end;
end;

//------------------------------------------------------------------------------

function SumSquareDiffFloatArray(const B: TDynFloatArray; Diff: Float): Float;
var
  I, L: Integer;
begin
  Result := 0.0;
  L := GetDynLength(B);
  if L <> 0 then
  begin
    {$IFDEF SUPPORTS_DYNAMICARRAYS}
    Result := Sqr(B[0] - Diff);
    for I := 1 to L - 1 do
      Result := Result + Sqr(B[I] - Diff);
    {$ELSE}
    Result := Sqr(B^[0] - Diff);
    for I := 1 to L - 1 do
      Result := Result + Sqr(B^[I] - Diff);
    {$ENDIF SUPPORTS_DYNAMICARRAYS}
  end;
end;

//------------------------------------------------------------------------------

function SumSquareFloatArray(const B: TDynFloatArray): Float;
var
  I, L: Integer;
begin
  Result := 0.0;
  L := GetDynLength(B);
  if L <> 0 then
  begin
    {$IFDEF SUPPORTS_DYNAMICARRAYS}
    Result := Sqr(B[0]);
    for I := 1 to L - 1 do
      Result := Result + Sqr(B[I]);
    {$ELSE}
    Result := Sqr(B^[0]);
    for I := 1 to L - 1 do
      Result := Result + Sqr(B^[I]);
    {$ENDIF SUPPORTS_DYNAMICARRAYS}
  end;
end;

//------------------------------------------------------------------------------

function SumPairProductFloatArray(const X, Y: TDynFloatArray): Float;
var
  I, L: Integer;
begin
  Result := 0.0;
  {$IFDEF SUPPORTS_DYNAMICARRAYS}
  L := Min(Length(X), Length(Y));
  {$ELSE}
  L := Min(DynArrayLength(X), DynArrayLength(Y));
  {$ENDIF SUPPORTS_DYNAMICARRAYS}
  if L <> 0 then
  begin
    {$IFDEF SUPPORTS_DYNAMICARRAYS}
    Result := X[0] * Y[0];
    for I := 1 to L - 1 do
      Result := Result + X[I] * Y[I];
    {$ELSE}
    Result := X^[0] * Y^[0];
    for I := 1 to L - 1 do
      Result := Result + X^[I] * Y^[I];
    {$ENDIF SUPPORTS_DYNAMICARRAYS}
  end;
end;

//------------------------------------------------------------------------------

function ChiSquare(const X: TDynFloatArray): Float;     // TODO
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

end.
