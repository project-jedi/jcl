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
{ The Original Code is JclUnitConv.pas.                                        }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2000 of these individuals.                                                   }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ Contains routines to perform conversion between various units such as length }
{ coordinate, temperature, angle, mass and pressure conversions.               }
{                                                                              }
{ Unit owner: Marcel van Brakel                                                }
{ Last modified: December 17, 2000                                             }
{                                                                              }
{******************************************************************************}

unit JclUnitConv;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  JclBase;

const

  { Temperature constants }

  CelsiusFreezingPoint    = 0.0;
  FahrenheitFreezingPoint = 32.0;
  KelvinFreezingPoint     = 273.15;
  CelsiusBoilingPoint     = 100.0 + CelsiusFreezingPoint;
  FahrenheitBoilingPoint  = 180.0 + FahrenheitFreezingPoint;
  KelvinBoilingPoint      = 100.0 + KelvinFreezingPoint;
  CelsiusAbsoluteZero     = -273.15;
  FahrenheitAbsoluteZero  = -459.67;
  KelvinAbsoluteZero      = 0.0;

  { Mathematical constants }

  DegPerCycle: Float  = 360.0;
  DegPerGrad: Float   = 0.9;
  DegPerRad: Float    = 57.295779513082320876798154814105;
  GradPerCycle: Float = 400.0;
  GradPerDeg: Float   = 1.1111111111111111111111111111111;
  GradPerRad: Float   = 63.661977236758134307553505349006;
  RadPerCycle: Float  = 6.283185307179586476925286766559;
  RadPerDeg: Float    = 0.017453292519943295769236907684886;
  RadPerGrad: Float   = 0.015707963267948966192313216916398;
  CyclePerDeg: Float  = 0.0027777777777777777777777777777778;
  CyclePerGrad: Float = 0.0025;
  CyclePerRad: Float  = 0.15915494309189533576888376337251;

function HowAOneLinerCanBiteYou(const Step, Max: Longint): Longint;
function MakePercentage(const Step, Max: Longint): Longint;

{ Temperature conversion }

function CelsiusToKelvin(const T: Float): Float;
function CelsiusToFahrenheit(const T: Float): Float;
function KelvinToCelsius(const T: Float): Float;
function KelvinToFahrenheit(const T: Float): Float;
function FahrenheitToCelsius(const T: Float): Float;
function FahrenheitToKelvin(const T: Float): Float;

{ Angle conversion }

function CycleToDeg(const Cycles: Float): Float;
function CycleToGrad(const Cycles: Float): Float;
function CycleToRad(const Cycles: Float): Float;
function DegToCycle(const Degrees: Float): Float;
function DegToGrad(const Degrees: Float): Float;
function DegToRad(const Degrees: Float): Float;
function GradToCycle(const Grads: Float): Float;
function GradToDeg(const Grads: Float): Float;
function GradToRad(const Grads: Float): Float;
function RadToCycle(const Radians: Float): Float;
function RadToDeg(const Radians: Float): Float;
function RadToGrad(const Radians: Float): Float;

{ Coordinate conversion }

procedure CartesianToPolar(const X, Y: Float; var R, Phi: Float);
procedure PolarToCartesian(const R, Phi: Float; var X, Y: Float);
procedure CartesianToCylinder(const X, Y, Z: Float; var R, Phi, Zeta: Float);
procedure CartesianToSpheric(const X, Y, Z: Float; var Rho, Theta, Phi: Float);
procedure CylinderToCartesian(const R, Phi, Zeta: Float; var X, Y, Z: Float);
procedure SphericToCartesian(const Rho, Theta, Phi: Float; var X, Y, Z: Float);

{ Length conversion }

function CmToInch(const Cm: Float): Float;
function InchToCm(const Inch: Float): Float;
function FeetToMetre(const Feet: Float): Float;
function MetreToFeet(const Metre: Float): Float;
function YardToMetre(const Yard: Float): Float;
function MetreToYard(const Metre: Float): Float;
function NmToKm(const Nm: Float): Float;
function KmToNm(const Km: Float): Float;
function KmToSm(const Km: Float): Float;
function SmToKm(const Sm: Float): Float;

{ Volume conversion }

function LitreToGalUs(const Litre: Float): Float;
function GalUsToLitre(const GalUs: Float): Float;
function GalUsToGalCan(const GalUs: Float): Float;
function GalCanToGalUs(const GalCan: Float): Float;
function GalUsToGalUk(const GalUs: Float): Float;
function GalUkToGalUs(const GalUk: Float): Float;
function LitreToGalCan(const Litre: Float): Float;
function GalCanToLitre(const GalCan: Float): Float;
function LitreToGalUk(const Litre: Float): Float;
function GalUkToLitre(const GalUk: Float): Float;

{ Mass conversion }

function KgToLb(const Kg: Float): Float;
function LbToKg(const Lb: Float): Float;
function KgToOz(const Kg: Float): Float;
function OzToKg(const Oz: Float): Float;
function CwtUsToKg(const Cwt: Float): Float;
function CwtUkToKg(const Cwt: Float): Float;
function KaratToKg(const Karat: Float): Float;
function KgToCwtUs(const Kg: Float): Float;
function KgToCwtUk(const Kg: Float): Float;
function KgToKarat(const Kg: Float): Float;
function KgToSton(const Kg: Float): Float;
function KgToLton(const Kg: Float): Float;
function StonToKg(const STon: Float): Float;
function LtonToKg(const Lton: Float): Float;
function QrUsToKg(const Qr: Float): Float;
function QrUkToKg(const Qr: Float): Float;
function KgToQrUs(const Kg: Float): Float;
function KgToQrUk(const Kg: Float): Float;

{ Pressure conversion }

function PascalToBar(const Pa: Float): Float;
function PascalToAt(const Pa: Float): Float;
function PascalToTorr(const Pa: Float): Float;
function BarToPascal(const Bar: Float): Float;
function AtToPascal(const At: Float): Float;
function TorrToPascal(const Torr: Float): Float;

{ Other conversion }

function KnotToMs(const Knot: Float): Float;
function HpElectricToWatt(const HpE: Float): Float;
function HpMetricToWatt(const HpM: Float): Float;
function MsToKnot(const ms: Float): Float;
function WattToHpElectric(const W: Float): Float;
function WattToHpMetric(const W: Float): Float;

implementation

uses
  JclMath;

//------------------------------------------------------------------------------

function HowAOneLinerCanBiteYou(const Step, Max: Longint): Longint;
begin
  Result := MakePercentage(Step, Max);
end;

function MakePercentage(const Step, Max: Longint): Longint;
begin
  Assert(Max <> 0);
  Result := Round((Step * 100.0) / Max);
end;

//------------------------------------------------------------------------------

function KelvinToFahrenheit(const T: Float): Float;
begin
  Result := CelsiusToFahrenheit(T - KelvinFreezingPoint);
end;

//------------------------------------------------------------------------------

function FahrenheitToKelvin(const T: Float): Float;
begin
  Result := FahrenheitToCelsius(T) + KelvinFreezingPoint;
end;

//------------------------------------------------------------------------------

function CelsiusToKelvin(const T: Float): Float;
begin
  Result := T + KelvinFreezingPoint;
end;

//------------------------------------------------------------------------------

function KelvinToCelsius(const T: Float): Float;
begin
  Result := T - KelvinFreezingPoint;
end;

//------------------------------------------------------------------------------

function CelsiusToFahrenheit(const T: Float): Float;
begin
  Result := (((FahrenheitBoilingPoint-FahrenheitFreezingPoint) /
    CelsiusBoilingPoint) * T) + FahrenheitFreezingPoint;
end;

//------------------------------------------------------------------------------

function FahrenheitToCelsius(const T: Float): Float;
begin
  Result := (CelsiusBoilingPoint /
    (FahrenheitBoilingPoint-FahrenheitFreezingPoint)) *
    (T - FahrenheitFreezingPoint);
end;

//==============================================================================
// Angle conversion
//==============================================================================

function CycleToDeg(const Cycles: Float): Float;
begin
  Result := Cycles * DegPerCycle;
end;

//------------------------------------------------------------------------------

function CycleToGrad(const Cycles: Float): Float;
begin
  Result := Cycles * GradPerCycle;
end;

//------------------------------------------------------------------------------

function CycleToRad(const Cycles: Float): Float;
begin
  Result := Cycles * RadPerCycle;
end;

//------------------------------------------------------------------------------

function DegToGrad(const Degrees: Float): Float;
begin
  Result := Degrees * GradPerDeg;
end;

//------------------------------------------------------------------------------

function DegToCycle(const Degrees: Float): Float;
begin
  Result := Degrees * CyclePerDeg;
end;

//------------------------------------------------------------------------------

function DegToRad(const Degrees: Float): Float;
begin
  Result := Degrees * RadPerDeg;
end;

//------------------------------------------------------------------------------

function GradToCycle(const Grads: Float): Float;
begin
  Result := Grads * CyclePerGrad;
end;

//------------------------------------------------------------------------------

function GradToDeg(const Grads: Float): Float;
begin
  Result := Grads * DegPerGrad;
end;

//------------------------------------------------------------------------------

function GradToRad(const Grads: Float): Float;
begin
  Result := Grads * RadPerGrad;
end;

//------------------------------------------------------------------------------

function RadToCycle(const Radians: Float): Float;
begin
  Result := Radians * CyclePerRad;
end;

//------------------------------------------------------------------------------

function RadToDeg(const Radians: Float): Float;
begin
  Result := Radians * DegPerRad;
end;

//------------------------------------------------------------------------------

function RadToGrad(const Radians: Float): Float;
begin
  Result := Radians * GradPerRad;
end;

//==============================================================================
// Coordinate conversion
//==============================================================================

procedure CartesianToCylinder(const X, Y, Z: Float; var R, Phi, Zeta: Float);
begin
  Zeta := Z;
  CartesianToPolar(X, Y, R, Phi);
end;

//------------------------------------------------------------------------------

procedure CartesianToPolar(const X, Y: Float; var R, Phi: Float);
begin
  R := Sqrt(Sqr(X) + Sqr(Y));
  if Abs(X) > PrecisionTolerance then
  begin
    Phi := ArcTan(Abs(Y) / Abs(X));
    if Sgn(X) = 1 then
      if Sgn(Y) = -1 then
        Phi := TwoPi - Phi
      else
      begin
        if Sgn(Y) = 1 then
          Phi := Pi - Phi
        else
          Phi := Pi + Phi;
      end;
  end
  else
    Phi := Sgn(Y) * PiOn2;
end;

//------------------------------------------------------------------------------

procedure CartesianToSpheric(const X, Y, Z: Float; var Rho, Theta, Phi: Float);
begin
  Rho := Sqrt(X*X+Y*Y+Z*Z);
  Phi := ArcTan(Y/X);
  Theta := ArcCos(Z/Rho);
end;

//------------------------------------------------------------------------------

procedure CylinderToCartesian(const R, Phi, Zeta: Float; var X, Y, Z: Float);
var
  Sine, CoSine: Float;
begin
  SinCos(Phi, Sine, Cosine);
  X := R * CoSine;
  Y := R * Sine;
  Z := Zeta;
end;

//------------------------------------------------------------------------------

procedure PolarToCartesian(const R, Phi: Float; var X, Y: Float);
var
  Sine, CoSine: Float;
begin
  SinCos(Phi, Sine, CoSine);
  X := R * CoSine;
  Y := R * Sine;
end;

//------------------------------------------------------------------------------

procedure SphericToCartesian(const Rho, Theta, Phi: Float; var X, Y, Z: Float);
var
  SineTheta, CoSineTheta: Float;
  SinePhi, CoSinePhi: Float;
begin
  SinCos(Theta, SineTheta, CoSineTheta);
  SinCos(Phi, SinePhi, CoSinePhi);
  X := Rho * SineTheta * CoSinePhi;
  Y := Rho * SineTheta * SinePhi;
  Z := Rho * CoSineTheta;
end;

//==============================================================================
// Length conversion
//==============================================================================

function CmToInch(const Cm: Float): Float;
begin
  Result := Cm / 2.54;
end;

//------------------------------------------------------------------------------

function InchToCm(const Inch: Float): Float;
begin
  Result := Inch * 2.54;
end;

//------------------------------------------------------------------------------

function FeetToMetre(const Feet: Float): Float;
begin
  Result := Feet * 0.3048;
end;

//------------------------------------------------------------------------------

function MetreToFeet(const Metre: Float): Float;
begin
  Result := Metre / 0.3048;
end;

//------------------------------------------------------------------------------

function YardToMetre(const Yard: Float): Float;
begin
  Result := Yard * 0.9144;
end;

//------------------------------------------------------------------------------

function MetreToYard(const Metre: Float): Float;
begin
  Result := Metre / 0.9144;
end;

//------------------------------------------------------------------------------

function NmToKm(const Nm: Float): Float;
begin
  Result := Nm * 1.852;
end;

//------------------------------------------------------------------------------

function KmToNm(const Km: Float): Float;
begin
  Result := Km / 1.852;
end;

//------------------------------------------------------------------------------

function KmToSm(const Km: Float): Float;
begin
  Result := Km / 1.609344;
end;

//------------------------------------------------------------------------------

function SmToKm(const Sm: Float): Float;
begin
  Result := Sm * 1.609344;
end;

//==============================================================================
// Volume conversion
//==============================================================================

function LitreToGalUs(const Litre: Float): Float;
begin
  Result := Litre / 3.785411784;
end;

//------------------------------------------------------------------------------

function GalUsToLitre(const GalUs: Float): Float;
begin
  Result := GalUs * 3.785411784;
end;

//------------------------------------------------------------------------------

function GalUsToGalCan(const GalUs: Float): Float;
begin
  Result := GalUs / 1.2009499255;
end;

//------------------------------------------------------------------------------

function GalCanToGalUs(const GalCan: Float): Float;
begin
  Result := GalCan * 1.2009499255;
end;

//------------------------------------------------------------------------------

function GalUsToGalUk(const GalUs: Float): Float;
begin
  Result := GalUs / 1.20095045385;
end;

//------------------------------------------------------------------------------

function GalUkToGalUs(const GalUk: Float): Float;
begin
  Result := GalUk * 1.20095045385;
end;

//------------------------------------------------------------------------------

function LitreToGalCan(const Litre: Float): Float;
begin
  Result := Litre / 4.54609;
end;

//------------------------------------------------------------------------------

function GalCanToLitre(const GalCan: Float): Float;
begin
  Result := GalCan * 4.54609;
end;

//------------------------------------------------------------------------------

function LitreToGalUk(const Litre: Float): Float;
begin
  Result := Litre / 4.54609;
end;

//------------------------------------------------------------------------------

function GalUkToLitre(const GalUk: Float): Float;
begin
  Result := GalUk * 4.54609;
end;

//==============================================================================
// Mass conversion
//==============================================================================

function KgToLb(const Kg: Float): Float;
begin
  Result := Kg * 0.45359237;
end;

//------------------------------------------------------------------------------

function LbToKg(const Lb: Float): Float;
begin
  Result := Lb / 0.45359237;
end;

//------------------------------------------------------------------------------

function KgToOz(const Kg: Float): Float;
begin
  Result := Kg * 35.2739619496;
end;

//------------------------------------------------------------------------------

function OzToKg(const Oz: Float): Float;
begin
  Result := Oz / 35.2739619496;
end;

//------------------------------------------------------------------------------

function QrUsToKg(const Qr: Float) : Float;
begin
  Result := Qr * 11.34;
end;

//------------------------------------------------------------------------------

function QrUkToKg(const Qr: Float) : Float;
begin
  Result := Qr * 12.7;
end;

//------------------------------------------------------------------------------

function KgToQrUs(const Kg: Float) : Float;
begin
  Result := Kg / 11.34;
end;

//------------------------------------------------------------------------------

function KgToQrUk(const Kg: Float) : Float;
begin
  Result := Kg / 12.7;
end;

//------------------------------------------------------------------------------

function CwtUsToKg(const Cwt: Float) : Float;
begin
  Result := Cwt * 45.35924;
end;

//------------------------------------------------------------------------------

function CwtUkToKg(const Cwt: Float) : Float;
begin
  Result := Cwt * 50.80235;
end;

//------------------------------------------------------------------------------

function KgToCwtUs(const Kg: Float) : Float;
begin
  Result := Kg / 45.35924;
end;

//------------------------------------------------------------------------------

function KgToCwtUk(const Kg: Float) : Float;
begin
  Result := Kg / 50.80235;
end;

//------------------------------------------------------------------------------

function LtonToKg(const Lton: Float) : Float;
begin
  Result := Lton * 1016.047;
end;

//------------------------------------------------------------------------------

function StonToKg(const Ston: Float) : Float;
begin
  Result := Ston * 907.1847;
end;

//------------------------------------------------------------------------------

function KgToLton(const Kg: Float) : Float;
begin
  Result := Kg / 1016.047;
end;

//------------------------------------------------------------------------------

function KgToSton(const Kg: Float) : Float;
begin
  Result := Kg / 907.1847;
end;

//------------------------------------------------------------------------------

function KgToKarat(const Kg: Float) : Float;
begin
  Result := Kg / 0.0002;
end;

//------------------------------------------------------------------------------

function KaratToKg(const Karat: Float) : Float;
begin
  Result := Karat * 0.0002;
end;


//==============================================================================
// Pressure conversion
//==============================================================================

function PascalToBar(const Pa: Float): Float;
begin
  Result := Pa / 100000.0;
end;

//------------------------------------------------------------------------------

function PascalToAt(const Pa: Float): Float;
begin
  Result := Pa / (9.80665 * 10000.0);
end;

//------------------------------------------------------------------------------

function PascalToTorr(const Pa: Float): Float;
begin
  Result := Pa / 133.3224;
end;

//------------------------------------------------------------------------------

function BarToPascal(const Bar: Float): Float;
begin
  Result := Bar * 100000.0;
end;

//------------------------------------------------------------------------------

function AtToPascal(const At: Float): Float;
begin
  Result := At * (9.80665 * 10000.0);
end;

//------------------------------------------------------------------------------

function TorrToPascal(const Torr: Float): Float;
begin
  Result := Torr * 133.3224;
end;

//==============================================================================
// Other conversion
//==============================================================================

function KnotToMs(const Knot: Float): Float;
begin
  Result := Knot * 0.514444444444;
end;

//------------------------------------------------------------------------------

function HpElectricToWatt(const HpE: Float): Float;
begin
  Result := HpE * 746.0;
end;

//------------------------------------------------------------------------------

function HpMetricToWatt(const HpM: Float): Float;
begin
  Result := HpM * 735.4988;
end;

//------------------------------------------------------------------------------

function MsToKnot(const ms: Float): Float;
begin
  Result := ms / 0.514444444444;
end;

//------------------------------------------------------------------------------

function WattToHpElectric(const W: Float): Float;
begin
  Result := W / 746.0;
end;

//------------------------------------------------------------------------------

function WattToHpMetric(const W: Float): Float;
begin
  Result := W / 735.4988;
end;

//------------------------------------------------------------------------------

end.