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
{ The Original Code is JclMath.pas.                                            }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: December 10, 2000                                             }
{                                                                              }
{******************************************************************************}

unit JclMath;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  Classes, SysUtils,
  JclBase;

{ Mathematical constants }

const
  Cbrt2: Float   = 1.2599210498948731647672106072782;  // CubeRoot(2)
  Cbrt3: Float   = 1.4422495703074083823216383107801;  // CubeRoot(3)
  Cbrt10: Float  = 2.1544346900318837217592935665194;  // CubeRoot(10)
  Cbrt100: Float = 4.6415888336127788924100763509194;  // CubeRoot(100)
  CbrtPi: Float  = 1.4645918875615232630201425272638;  // CubeRoot(PI)
  Pi: Float      = 3.1415926535897932384626433832795;  // PI
  PiOn2: Float   = 1.5707963267948966192313216916398;  // PI / 2
  PiOn3: Float   = 1.0471975511965977461542144610932;  // PI / 3
  PiOn4: Float   = 0.78539816339744830961566084581988; // PI / 4
  Sqrt2: Float   = 1.4142135623730950488016887242097;  // Sqrt(2)
  Sqrt3: Float   = 1.7320508075688772935274463415059;  // Sqrt(3)
  Sqrt5: Float   = 2.2360679774997896964091736687313;  // Sqrt(5)
  Sqrt10: Float  = 3.1622776601683793319988935444327;  // Sqrt(10)
  SqrtPi: Float  = 1.7724538509055160272981674833411;  // Sqrt(PI)
  Sqrt2Pi: Float = 2.506628274631000502415765284811;   // Sqrt(2 * PI)
  TwoPi: Float   = 6.283185307179586476925286766559;   // 2 * PI
  ThreePi: Float = 9.4247779607693797153879301498385;  // 3 * PI
  Ln2: Float     = 0.69314718055994530941723212145818; // Ln(2)
  Ln10: Float    = 2.3025850929940456840179914546844;  // Ln(10)
  LnPi: Float    = 1.1447298858494001741434273513531;  // Ln(PI)
  Log2: Float    = 0.30102999566398119521373889472449; // Log10(2)
  Log3: Float    = 0.47712125471966243729502790325512; // Log10(3)
  LogPi: Float   = 0.4971498726941338543512682882909;  // Log10(PI)
  LogE: Float    = 0.43429448190325182765112891891661; // Log10(E)
  E: Float       = 2.7182818284590452353602874713527;  // Natural constant
  hLn2Pi: Float  = 0.91893853320467274178032973640562; // Ln(2*PI)/2
  inv2Pi: Float  = 0.159154943091895;                  // 0.5 / Pi
  TwoToPower63: Float = 9223372036854775808.0;         // 2^63

const
  NaNSignalingBits: Int64 = $7FF0000000000001;

var
  NaNSignaling: Double absolute NANSignalingBits;

const
{$IFDEF MATH_ANGLES_DEGREES}
  MaxAngle: Float = 528460290590760220769.3238897268; // 2^63 * DegPerRad
{$ENDIF}
{$IFDEF MATH_ANGLES_GRADS}
  MaxAngle: Float = 587178100656400245299.24876636311; // 2^63 * GradPerRad
{$ENDIF}
{$IFDEF MATH_ANGLES_RADIANS}
  MaxAngle: Float = 9223372036854775808.0; // 2^63 Rad
{$ENDIF}

{$IFDEF MATH_EXTENDED_PRECISION}
  MaxTanH: Float = 5678.2617031470719747459655389854; // Ln(2^16384)/2
  MaxFactorial   = 1754;
  MaxFloatingPoint: Float = 1.189731495357231765085759326628E+4932; // 2^16384
  MinFloatingPoint: Float = 3.3621031431120935062626778173218E-4932; // 2^(-16382)
{$ELSE}
{$IFDEF MATH_Double_PRECISION}
  MaxTanH: Float = 354.89135644669199842162284618659; // Ln(2^1024)/2
  MaxFactorial   = 170;
  MaxFloatingPoint: Float = 1.797693134862315907729305190789E+308; // 2^1024
  MinFloatingPoint: Float = 2.2250738585072013830902327173324E-308; // 2^(-1022)
{$ELSE}
{$IFDEF MATH_SINGLE_PRECISION}
  MaxTanH: Float = 44.361419555836499802702855773323; // Ln(2^128)/2
  MaxFactorial   = 33;
  MaxFloatingPoint: Float = 3.4028236692093846346337460743177E+38; // 2^128
  MinFloatingPoint: Float = 1.1754943508222875079687365372222E-38; // 2^(-126)
{$ENDIF}{$ENDIF}{$ENDIF}

var
  PrecisionTolerance: Float = 0.0000001;
  EpsSingle: Single;
  EpsDouble: Double;
  EpsExtended: Extended;
  Epsilon: Float;
  ThreeEpsSingle: Single;
  ThreeEpsDouble: Double;
  ThreeEpsExtended: Extended;
  ThreeEpsilon: Float;

{ Logarithmic }

function LogBase10(X: Float): Float;
function LogBase2(X: Float): Float;
function LogBaseN(Base, X: Float): Float;

{ Transcendental }

function ArcCos(X: Float): Float;
function ArcCot(X: Float): Float;
function ArcCsc(X: Float): Float;
function ArcSec(X: Float): Float;
function ArcSin(X: Float): Float;
function ArcTan(X: Float): Float;
function ArcTan2(Y, X: Float): Float;
function Cos(X: Float): Float;
function Cot(X: Float): Float;
function Csc(X: Float): Float;
function Sec(X: Float): Float;
function Sin(X: Float): Float;
procedure SinCos(X: Float; var Sin, Cos: Float);
function Tan(X: Float): Float;

{ Hyperbolic }

function ArcCosH(X: Float): Float;
function ArcCotH(X: Float): Float;
function ArcCscH(X: Float): Float;
function ArcSecH(X: Float): Float;
function ArcSinH(X: Float): Float;
function ArcTanH(X: Float): Float;
function CosH(X: Float): Float;
function CotH(X: Float): Float;
function CscH(X: Float): Float;
function SecH(X: Float): Float;
function SinH(X: Float): Float;
function TanH(X: Float): Float;

{ Coordinate conversion }

function DegMinSecToFloat(const Degs, Mins, Secs: Float): Float;
procedure FloatToDegMinSec(const X: Float; var Degs, Mins, Secs: Float);

{ Exponential }

function Power(const Base, Exponent: Float): Float;
function PowerInt(const X: Float; N: Integer): Float;
function TenToY(const Y: Float): Float;
function TwoToY(const Y: Float): Float;

{ Floating point support routines }

function IsFloatZero(const X: Float): Boolean;
function FloatsEqual(const X1, X2: Float): Boolean;
function MaxFloat(const X, Y: Float): Float;
function MinFloat(const X, Y: Float): Float;
function ModFloat(const X, Y: Float): Float;
function RemainderFloat(const X, Y: Float): Float;
function SetPrecisionTolerance(NewTolerance: Float): Float;
procedure SwapFloats(var X, Y: Float);
procedure CalcMachineEpsSingle;
procedure CalcMachineEpsDouble;
procedure CalcMachineEpsExtended;
procedure CalcMachineEps;
procedure SetPrecisionToleranceToEpsilon;

{ Miscellaneous }

function Ceiling(const X: Float): Integer;
function Factorial(const N: Integer): Float;
function Floor(const X: Float): Float;
function GCD(const X, Y: Cardinal): Cardinal;
function ISqrt(const I: Smallint): Smallint;
function LCM(const X, Y: Cardinal): Cardinal;
function NormalizeAngle(const Angle: Float): Float;
function Pythagoras(const X, Y: Float): Float;
function Sgn(const X: Float): Integer;
function Signe(const X, Y: Float): Float;

{ Prime numbers }

function IsRelativePrime(const X, Y: Cardinal): Boolean;
function IsPrime(const N: Integer): Boolean;
function IsPrimeFactor(const F, N: Integer): Boolean;
function PrimeFactors(const N: Integer): TDynIntegerArray;

{ NaN and INF support }

const
  Infinity = 1/0; // tricky
  NaN      = 0/0; // tricky

//function IsNaN(const d: Single): Boolean; overload;
 function IsNaN(const d: Double): Boolean; overload;
// function IsNaN(const d: Extended): Boolean; overload;
//function IsInfinity(const d: Single): Boolean; overload;
function IsInfinity(const d: Double): Boolean; overload;
//function IsInfinity(const d: Extended): Boolean; overload;
//function IsRealIndeterminate(const d: Single): Boolean; overload;
//function IsRealIndeterminate(const d: Double): Boolean; overload;
//function IsRealIndeterminate(const d: Extended): Boolean; overload;

{ Set support }

type
  ASet = class (TObject)
  private
    function GetBit(const Idx: Integer): Boolean; virtual; abstract;
    procedure SetBit(const Idx: Integer; const Value: Boolean); virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure Invert; virtual; abstract;
    function GetRange(const Low, High: Integer; const Value: Boolean): Boolean; virtual; abstract;
    procedure SetRange(const Low, High: Integer; const Value: Boolean); virtual; abstract;
  end;

type
  TFlatSet = class (ASet)
  private
    FBits: TBits;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Invert; override;
    procedure SetRange(const Low, High: Integer; const Value: Boolean); override;
    function GetBit(const Idx: Integer): Boolean; override;
    function GetRange(const Low, High: Integer; const Value: Boolean): Boolean; override;
    procedure SetBit(const Idx: Integer; const Value: Boolean); override;
  end;

type
  TRangeSet = class (ASet)
  private
    FRanges: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Invert; override;
    procedure SetRange(const Low, High: Integer; const Value: Boolean); override;
    function GetBit(const Idx: Integer): Boolean; override;
    function GetRange(const Low, High: Integer; const Value: Boolean): Boolean; override;
    procedure SetBit(const Idx: Integer; const Value: Boolean); override;
  end;

type
  TRange = class (TObject)
  protected
    FLow: Integer;
    FHigh: Integer;
  public
    constructor Create(const Low, High: Integer);
    function HasElement(const I: Integer): Boolean;
    function Overlap(const Low, High: Integer): Boolean;
    function Touch(const Low, High: Integer): Boolean;
    function Inside(const Low, High: Integer): Boolean;
    procedure Merge(const Low, High: Integer);
  end;

type
  TPointerArray = array [0..MaxLongint div 256] of Pointer;
  PPointerArray = ^TPointerArray;
  TDelphiSet = set of Byte; // 256 elements
  PDelphiSet = ^TDelphiSet;

const
  EmptyDelphiSet: TDelphiSet = [];
  CompleteDelphiSet: TDelphiSet = [0..255];

type
  TSparseFlatSet = class (ASet)
  private
    FSetList: PPointerArray;
    FSetListEntries: Integer;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure Invert; override;
    function GetBit(const Idx: Integer): Boolean; override;
    procedure SetBit(const Idx: Integer; const Value: Boolean); override;
    procedure SetRange(const Low, High: Integer; const Value: Boolean); override;
  end;

{ Rational numbers }

type
  TJclRational = class (TObject)
  private
    FT: Integer;
    FN: Integer;
    function GetAsString: string;
    procedure SetAsString(const S: string);
    function GetAsFloat: Float;
    procedure SetAsFloat(const R: Float);
  protected
    procedure Simplify;
  public
    constructor Create; overload;
    constructor Create(const Numerator: Integer;
      const Denominator: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}); overload;
    constructor Create(const R: Float); overload;

    property Numerator: Integer read FT;
    property Denominator: Integer read FN;
    property AsString: string read GetAsString write SetAsString;
    property AsFloat: Float read GetAsFloat write SetAsFloat;

    procedure Assign(const R: TJclRational); overload;
    procedure Assign(const R: Float); overload;
    procedure Assign(const Numerator: Integer; const Denominator: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}); overload;

    procedure AssignZero;
    procedure AssignOne;
    function Duplicate: TJclRational;

    function IsEqual(const R: TJclRational): Boolean; reintroduce; overload;
    function IsEqual(const Numerator: Integer;
      const Denominator: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Boolean; reintroduce; overload;
    function IsEqual(const R: Float): Boolean; reintroduce; overload;

    function IsZero: Boolean;
    function IsOne: Boolean;

    procedure Add(const R: TJclRational); overload;
    procedure Add(const V: Float); overload;
    procedure Add(const V: Integer); overload;

    procedure Subtract(const R: TJclRational); overload;
    procedure Subtract(const V: Float); overload;
    procedure Subtract(const V: Integer); overload;

    procedure Negate;
    procedure Abs;
    function Sgn: Integer;

    procedure Multiply(const R: TJclRational); overload;
    procedure Multiply(const V: Float); overload;
    procedure Multiply(const V: Integer); overload;

    procedure Reciprocal;

    procedure Divide(const R: TJclRational); overload;
    procedure Divide(const V: Float); overload;
    procedure Divide(const V: Integer); overload;

    procedure Sqrt;
    procedure Sqr;

    procedure Power(const R: TJclRational); overload;
    procedure Power(const V: Integer); overload;
    procedure Power(const V: Float); overload;
  end;


{ CRC }

function Crc32(const X: array of Byte; N: Integer; Crc: Cardinal): Cardinal;
function Crc16(const X: array of Byte; N: Integer; Crc: Word): Word;
function CheckCrc32(var X: array of Byte; N: Integer; Crc: Cardinal): Integer;
function InternetChecksum(const X: array of Byte; N: Integer): Cardinal;


type
  EJclMathError = class (EJclError);

implementation

uses
  JclResources;

//==============================================================================
// Internal helper routines
//==============================================================================

// to be independent from JCLLogic

function Min(const x, y: Integer): Integer;
begin
  if x < y then
    Result := x
  else
    result := y;
end;

//------------------------------------------------------------------------------

// to be independent from JCLLogic

procedure SwapOrd(var x, y: Integer);
var
  Temp: Integer;
begin
  Temp := x;
  x := y;
  y := Temp;
end;

//------------------------------------------------------------------------------

function DoubleToHex(const d: Double): string;
var
  Overlay: array [1..2] of LongInt absolute d;
begin
  // Look at element 2 before element 1 because of "Little Endian" order.
  Result := IntToHex(Overlay[2], 8) + IntToHex(Overlay[1], 8);
end;

//------------------------------------------------------------------------------

function HexToDouble(const hex: string): Double;
var
  d: Double;
  Overlay: array [1..2] of LongInt absolute d;
begin
  if Length(hex) <> 16 then
    raise EJclMathError.CreateResRec(@RsUnexpectedValue);

  Overlay[1] := StrToInt('$' + Copy(hex, 9, 8));
  Overlay[2] := StrToInt('$' + Copy(hex, 1, 8));

  Result := d;
end;

//------------------------------------------------------------------------------

const
  _180: Integer = 180;
  _200: Integer = 200;

// Converts degrees to radians. Expects degrees in ST(0), leaves radians in ST(0)
// ST(0) := ST(0) * PI / 180

procedure FDegToRad; assembler;
asm
        FLDPI
        FIDIV   [_180]
        FMUL
        FWAIT
end;

//------------------------------------------------------------------------------

// Converts radians to degrees. Expects radians in ST(0), leaves degrees in ST(0)
// ST(0) := ST(0) * (180 / PI);

procedure FRadToDeg; assembler;
asm
        FLD1
        FLDPI
        FDIV
        FLD     [_180]
        FMUL
        FMUL
        FWAIT
end;

//------------------------------------------------------------------------------

// Converts grads to radians. Expects grads in ST(0), leaves radians in ST(0)
// ST(0) := ST(0) * PI / 200

procedure FGradToRad; assembler;
asm
        FLDPI
        FIDIV   [_200]
        FMUL
        FWAIT
end;

//------------------------------------------------------------------------------

// Converts radians to grads. Expects radians in ST(0), leaves grads in ST(0)
// ST(0) := ST(0) * (200 / PI);

procedure FRadToGrad; assembler;
asm
        FLD1
        FLDPI
        FDIV
        FLD     [_200]
        FMUL
        FMUL
        FWAIT
end;

//==============================================================================
// Logarithmic
//==============================================================================

function LogBase10(X: Float): Float;

  function FLogBase10(X: Float): Float; assembler;
  asm
          FLDLG2
          FLD     X
          FYL2X
          FWAIT
  end;

begin
  if X <= 0.0 then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  Result := FLogBase10(X);
end;

//------------------------------------------------------------------------------

function LogBase2(X: Float): Float;

  function FLogBase2(X: Float): Float; assembler;
  asm
          FLD1
          FLD     X
          FYL2X
          FWAIT
  end;

begin
  if X <= 0.0 then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  Result := FLogBase2(X);
end;

//------------------------------------------------------------------------------

function LogBaseN(Base, X: Float): Float;

  function FLogBaseN(Base, X: Float): Float; assembler;
  asm
          FLD1
          FLD     X
          FYL2X
          FLD1
          FLD     Base
          FYL2X
          FDIV
          FWAIT
  end;

begin
  if (X <= 0.0) or (Base <= 0.0) or (Base = 1.0) then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  Result := FLogBaseN(Base, X);
end;

//==============================================================================
// Transcendental
//==============================================================================

function ArcCos(X: Float): Float;

  function FArcCos(X: Float): Float; assembler;
  asm
          FLD     X
          FLD     ST(0)
          FMUL    ST(0), ST
          FLD1
          FSUBRP  ST(1), ST
          FSQRT
          FXCH
          FPATAN
          FWAIT
          {$IFDEF MATH_ANGLES_DEGREES}
          CALL    FRadToDeg
          {$ENDIF}
          {$IFDEF MATH_ANGLES_GRADS}
          CALL    FRadToGrad
          {$ENDIF}
  end;

begin
  if Abs(X) > 1.0 then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  Result := FArcCos(X);
end;

//------------------------------------------------------------------------------

function ArcCot(X: Float): Float;
begin
  Result := -Arctan(X) + PiOn2;
  {$IFDEF MATH_ANGLES_DEGREES}
  Result := RadToDeg(Result);
  {$ENDIF}
  {$IFDEF MATH_ANGLES_GRADS}
  Result := RadToGrad(Result);
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function ArcCsc(X: Float): Float;
begin
  if Abs(X) >= 1.0 then
    raise EJclMathError.CreateResRec(@RsMathDomainError);

  Result := Arctan(1.0 / Sqrt(1.0 - Sqr(X))) + (Sgn(X) - 1.0) * PiOn2;
  {$IFDEF MATH_ANGLES_DEGREES}
  Result := RadToDeg(Result);
  {$ENDIF}
  {$IFDEF MATH_ANGLES_GRADS}
  Result := RadToGrad(Result);
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function ArcSec(X: Float): Float;

  function FArcTan(X: Float): Float; assembler;
  asm
          FLD     X
          FLD1
          FPATAN
          FWAIT
  end;

begin
  if Abs(X) >= 1.0 then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  Result := FArcTan(X / Sqrt(1.0 - Sqr(X))) + (Sgn(X) - 1.0) * PiOn2;
  {$IFDEF MATH_ANGLES_DEGREES}
  Result := RadToDeg(Result);
  {$ENDIF}
  {$IFDEF MATH_ANGLES_GRADS}
  Result := RadToGrad(Result);
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function ArcSin(X: Float): Float;

  function FArcSin(X: Float): Float; assembler;
  asm
          FLD     X
          FLD     ST(0)
          FMUL    ST(0), ST
          FLD1
          FSUBRP  ST(1), ST
          FSQRT
          FPATAN
          FWAIT
          {$IFDEF MATH_ANGLES_DEGREES}
          CALL    FRadToDeg
          {$ENDIF}
          {$IFDEF MATH_ANGLES_GRADS}
          CALL    FRadToGrad
          {$ENDIF}
  end;

begin
  if Abs(X) > 1.0 then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  Result := FArcSin(X);
end;

//------------------------------------------------------------------------------

function ArcTan(X: Float): Float;

  function FArcTan(X: Float): Float; assembler;
  asm
          FLD     X
          FLD1
          FPATAN
          FWAIT
          {$IFDEF MATH_ANGLES_DEGREES}
          CALL    FRadToDeg
          {$ENDIF}
          {$IFDEF MATH_ANGLES_GRADS}
          CALL    FRadToGrad
          {$ENDIF}
  end;

begin
  if X < 0.0 then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  Result := FArcTan(X);
end;

//------------------------------------------------------------------------------

function ArcTan2(Y, X: Float): Float;

  function FArcTan2(Y, X: Float): Float; assembler;
  asm
          FLD     Y
          FLD     X
          FPATAN
          FWAIT
          {$IFDEF MATH_ANGLES_DEGREES}
          CALL    FRadToDeg
          {$ENDIF}
          {$IFDEF MATH_ANGLES_GRADS}
          CALL    FRadToGrad
          {$ENDIF}
  end;

begin
  if not ((Y >= 0.0) and (X >= Y)) then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  Result := FArcTan2(Y, X);
end;

//------------------------------------------------------------------------------

function Cos(X: Float): Float;

  function FCos(X: Float): Float; assembler;
  asm
          FLD     X
          {$IFDEF MATH_ANGLES_DEGREES}
          CALL    FDegToRad
          {$ENDIF}
          {$IFDEF MATH_ANGLES_GRADS}
          CALL    FGradToRad
          {$ENDIF}
          FCOS
          FWAIT
  end;

begin
  if Abs(X) > MaxAngle then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  Result := FCos(X);
end;

//------------------------------------------------------------------------------

function Cot(X: Float): Float;

  function FCot(X: Float): Float; assembler;
  asm
          FLD     X
          {$IFDEF MATH_ANGLES_DEGREES}
          CALL    FDegToRad
          {$ENDIF}
          {$IFDEF MATH_ANGLES_GRADS}
          CALL    FGradToRad
          {$ENDIF}
          FPTAN
          FDIVRP
          FWAIT
  end;

begin
  if Abs(X) > MaxAngle then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  // TODO Cot = 1 / Tan -> Tan(X) <> 0.0
  Result := FCot(X);
end;

//------------------------------------------------------------------------------

function Csc(X: Float): Float;
var
  Y: Float;
begin
  if Abs(X) > MaxAngle then
    raise EJclMathError.CreateResRec(@RsMathDomainError);

  {$IFDEF MATH_ANGLES_DEGREES}
  X := X * RadPerDeg;
  {$ENDIF}
  {$IFDEF MATH_ANGLES_GRADS}
  X := X * RadPerGrad;
  {$ENDIF}

  Y := Sin(X);
  if Y = 0.0 then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  Result := 1 / Y;
end;

//------------------------------------------------------------------------------

function Sec(X: Float): Float;

  function FSec(X: Float): Float; assembler;
  asm
          FLD     X
          {$IFDEF MATH_ANGLES_DEGREES}
          CALL    FDegToRad
          {$ENDIF}
          {$IFDEF MATH_ANGLES_GRADS}
          CALL    FGradToRad
          {$ENDIF}
          FCOS
          FLD1
          FDIVRP
          FWAIT
  end;

begin
  if Abs(X) > MaxAngle then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  // TODO Sec = 1 / Cos -> Cos(X) <> 0!
  Result := FSec(X);
end;

//------------------------------------------------------------------------------

function Sin(X: Float): Float;

  function FSin(X: Float): Float; assembler;
  asm
          FLD     X
          {$IFDEF MATH_ANGLES_DEGREES}
          CALL    FDegToRad
          {$ENDIF}
          {$IFDEF MATH_ANGLES_GRADS}
          CALL    FGradToRad
          {$ENDIF}
          FSIN
          FWAIT
  end;

begin
  if Abs(X) > MaxAngle then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  Result := FSin(X);
end;

//------------------------------------------------------------------------------

procedure SinCos(X: Float; var Sin, Cos: Float);

  procedure FSinCos(X: Float; var Sin, Cos: Float); assembler;
  asm
          FLD     X
          {$IFDEF MATH_ANGLE_DEGREES}
          CALL    FDegToRad
          {$ENDIF}
          {$IFDEF MATH_ANGLES_GRADS}
          CALL    FGradToRad
          {$ENDIF}
          FSINCOS
          FSTP    TByte PTR [EDX]
          FSTP    TByte PTR [EAX]
          FWAIT
  end;

begin
  if Abs(X) > MaxAngle then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  FSinCos(X, Sin, Cos);
end;

//------------------------------------------------------------------------------

function Tan(X: Float): Float;

  function FTan(X: Float): Float; assembler;
  asm
          FLD     X
          {$IFDEF MATH_ANGLES_DEGREES}
          CALL    FDegToRad
          {$ENDIF}
          {$IFDEF MATH_ANGLES_GRADS}
          CALL    FGradToRad
          {$ENDIF}
          FPTAN
          FSTP    ST(0)
          FWAIT
  end;

begin
  if Abs(X) > MaxAngle then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  Result := FTan(X);
end;

//==============================================================================
// Hyperbolic
//==============================================================================

function ArcCosH(X: Float): Float;

  function FArcCosH(X: Float): Float; assembler;
  asm
          FLDLN2
          FLD     X
          FLD     ST(0)
          FMUL    ST(0), ST
          FLD1
          FSUBP   ST(1), ST
          FSQRT
          FADDP   ST(1), ST
          FYL2X
  end;

begin
  if X < 1.0 then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  Result := FArcCosH(X);
end;

//------------------------------------------------------------------------------

function ArcCotH(X: Float): Float;
begin
  if Abs(X) = 1.0 then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  Result := 0.5 * Ln((X + 1.0) / (X - 1.0));
end;

//------------------------------------------------------------------------------

function ArcCscH(X: Float): Float;
begin
  if X = 0 then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  Result := Ln((Sgn(X) * Sqrt(Sqr(X) + 1.0) + 1.0) / X);
end;

//------------------------------------------------------------------------------

function ArcSecH(X: Float): Float;
begin
  if Abs(X) > 1.0 then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  Result := Ln((Sqrt(1.0 - Sqr(X)) + 1.0) / X);
end;

//------------------------------------------------------------------------------

function ArcSinH(X: Float): Float; assembler;
asm
        FLDLN2
        FLD     X
        FLD     ST(0)
        FMUL    ST(0), ST
        FLD1
        FADDP   ST(1), ST
        FSQRT
        FADDP   ST(1), ST
        FYL2X
end;

//------------------------------------------------------------------------------

function ArcTanH(X: Float): Float;

  function FArcTanH(X: Float): Float; assembler;
  asm
          FLDLN2
          FLD     X
          FLD     ST(0)
          FLD1
          FADDP   ST(1), ST
          FXCH
          FLD1
          FSUBRP  ST(1), ST
          FDIVP   ST(1), ST
          FSQRT
          FYL2X
          FWAIT
  end;

begin
  if Abs(X) >= 1.0 then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  Result := FArcTanH(X);
end;

//------------------------------------------------------------------------------

function CosH(X: Float): Float;
begin
  Result := 0.5 * (Exp(X) + Exp(-X));
end;

//------------------------------------------------------------------------------

function CotH(X: Float): Float; assembler;
const
  RoundDown: Word = $177F;
  OneHalf: Float = 0.5;
var
  ControlWW: Word;
asm
        FLD     X  // TODO Legal values for X?
        FLDL2E
        FMULP   ST(1), ST
        FSTCW   ControlWW
        FLDCW   RoundDown
        FLD     ST(0)
        FRNDINT
        FLDCW   ControlWW
        FXCH
        FSUB    ST, ST(1)
        F2XM1
        FLD1
        FADDP   ST(1), ST
        FSCALE
        FST     ST(1)
        FLD1
        FDIVRP  ST(1), ST
        FADDP   ST(1), ST
        FLD     OneHalf
        FMULP   ST(1), ST
        FWAIT
end;

//------------------------------------------------------------------------------

function CscH(X: Float): Float;
begin
  Result := Exp(X) - Exp(-X);
  if Result = 0.0 then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  Result := 2.0 / Result;
end;

//------------------------------------------------------------------------------

function SecH(X: Float): Float;
begin
  Result := Exp(X) + Exp(-X);
  if Result = 0.0 then
    raise EJclMathError.CreateResRec(@RsMathDomainError);
  Result := 2.0 / Result;
end;

//------------------------------------------------------------------------------

function SinH(X: Float): Float; assembler;
const
  RoundDown: Word = $177F;
  OneHalf: Float = 0.5;
var
  ControlWW: Word;
asm
        FLD     X // TODO Legal values for X?
        FLDL2E
        FMULP   ST(1), ST
        FSTCW   ControlWW
        FLDCW   RoundDown
        FLD     ST(0)
        FRNDINT
        FLDCW   ControlWW
        FXCH
        FSUB    ST, ST(1)
        F2XM1
        FLD1
        FADDP   ST(1), ST
        FSCALE
        FST     ST(1)
        FLD1
        FDIVRP  ST(1), ST
        FSUBP   ST(1), ST
        FLD     OneHalf
        FMULP   ST(1), ST
        FWAIT
end;

//------------------------------------------------------------------------------

function TanH(X: Float): Float;
begin
  if X > MaxTanH then
    Result := 1.0
  else
  begin
    if X < -MaxTanH then
      Result := -1.0
    else
    begin
      Result := Exp(X);
      Result := Result * Result;
      Result := (Result - 1.0) / (Result + 1.0);
    end;
  end;
end;

//==============================================================================
// Coordinate conversion
//==============================================================================

function DegMinSecToFloat(const Degs, Mins, Secs: Float): Float;
begin
  Result := Degs + (Mins / 60.0) + (Secs / 3600.0);
end;

//------------------------------------------------------------------------------

procedure FloatToDegMinSec(const X: Float; var Degs, Mins, Secs: Float);
var
  Y: Float;
begin
  Degs := Int(X);
  Y := Frac(X) * 60;
  Mins := Int(Y);
  Secs := Frac(Y) * 60;
end;

//==============================================================================
// Exponential
//==============================================================================

function Power(const Base, Exponent: Float): Float;
var
  R1, R2, R3: Float;
begin
  R1 := Abs(Exponent);
  R2 := Abs(Base);
  if Exponent = 0.0 then
    R3 := 1.0 // Always 1 for 0 exponent
  else
  begin
    if Base = 0.0 then
      R3 := 0.0 // Always 0 for 0 X
    else
    begin
      R3 := Exp(Ln(R2) * R1); // Basic calculation
      if Base < 0.0 then
        R3 := -R3; // Negate if X < 0
      if Exponent < 0.0 then
        R3 := 1.0 / R3; // Flip over if exponent negative
    end;
  end;
  Result := R3;
end;

//------------------------------------------------------------------------------

function PowerInt(const X: Float; N: Integer): Float;
var
  M: Integer;
  T: Float;
  Xc: Float;
begin
  if X = 0.0 then
  begin
    if N = 0 then
      Result := 1.0
    else
    if N > 0 then
      Result := 0.0
    else
      Result := MaxFloatingPoint;
    Exit;
  end;

  if N = 0 then
  begin
    Result := 1.0;
    Exit;
  end;

  // Legendre's algorithm for minimizing the number of multiplications

  T := 1.0;
  M := Abs(N);
  Xc := X;
  repeat
    if Odd(M) then
    begin
      Dec(M);
      T := T * Xc;
    end
    else
    begin
      M := M div 2;
      Xc := Sqr(Xc);
    end;
  until M = 0;

  if N > 0 then
    Result := T
  else
    Result := 1.0 / T;
end;

//------------------------------------------------------------------------------

function TenToY(const Y: Float): Float;
begin
  if Y = 0.0 then
    Result := 1.0
  else
    Result := Exp(Y * Ln10);
end;

//------------------------------------------------------------------------------

function TwoToY(const Y: Float): Float;
begin
  if Y = 0.0 then
    Result := 1.0
  else
    Result := Exp(Y * Ln2);
end;

//==============================================================================
// Floating point support routines
//==============================================================================

function IsFloatZero(const X: Float): Boolean;
begin
  Result := Abs(X) < PrecisionTolerance;
end;

//------------------------------------------------------------------------------

function FloatsEqual(const X1, X2: Float): Boolean;
begin
  try
    if X1 = 0 then
      result := (X1 = X2) or     // catch exact equality
                (Abs(1 - X1/X2 ) <= PrecisionTolerance)
    else
      result := (X1 = X2) or     // catch exact equality
                (Abs(1 - X1/X2 ) <= PrecisionTolerance);
  except
    Result := False;  // catch real rare overflow e.g.  1.0e3000/1.0e-3000
  end
end;

//------------------------------------------------------------------------------

function MaxFloat(const X, Y: Float): Float;
begin
  if X < Y then
    Result := Y
  else
    Result := X;
end;

//------------------------------------------------------------------------------

function MinFloat(const X, Y: Float): Float;
begin
  if X > Y then
    Result := Y
  else
    Result := X;
end;

//------------------------------------------------------------------------------

function ModFloat(const X, Y: Float): Float;
var
  Z: Float;
begin
  Result := X / Y;
  Z := Int(Result);
  if Result < 0.0 then
    Z := Z - 1.0;

  Result := X - Z * Y;
end;

//------------------------------------------------------------------------------

function RemainderFloat(const X, Y: Float): Float;
begin
  Result := X - Int(X / Y) * Y;
end;

//------------------------------------------------------------------------------

procedure SwapFloats(var X, Y: Float);
var
  T: Float;
begin
  T := X;
  X := Y;
  Y := T;
end;

//------------------------------------------------------------------------------

procedure CalcMachineEpsSingle;
var
  One: Single;
  T: Single;

begin
  One := 1.0;
  EpsSingle := One;

  repeat
    EpsSingle := 0.5 * EpsSingle;
    T := One + EpsSingle;
  until One = T;

  EpsSingle := 2.0 * EpsSingle;
  ThreeEpsSingle := 3.0 * EpsSingle;
end;

//------------------------------------------------------------------------------

procedure CalcMachineEpsDouble;
var
  One: Double;
  T: Double;
begin
  One := 1.0;
  EpsDouble := One;
  repeat
    EpsDouble := 0.5 * EpsDouble;
    T := One + EpsDouble;
  until One = T;

  EpsDouble := 2.0 * EpsDouble;
  ThreeEpsDouble := 3.0 * EpsDouble;
end;

//------------------------------------------------------------------------------

procedure CalcMachineEpsExtended;
var
  One: Extended;
  T: Extended;
begin
  One := 1.0;
  EpsExtended := One;
  repeat
    EpsExtended := 0.5 * EpsExtended;
    T := One + EpsExtended;
  until One = T;

  EpsExtended := 2.0 * EpsExtended;
  ThreeEpsExtended := 3.0 * EpsExtended;
end;

//------------------------------------------------------------------------------

procedure CalcMachineEps;
begin
{$IFDEF MATH_EXTENDED_PRECISION}
  CalcMachineEpsExtended;
  Epsilon := EpsExtended;
  ThreeEpsilon := ThreeEpsExtended;
{$ELSE}
{$IFDEF MATH_Double_PRECISION}
  CalcMachineEpsDouble;
  Epsilon := EpsDouble;
  ThreeEpsilon := ThreeEpsDouble;
{$ELSE}
{$IFDEF MATH_SINGLE_PRECISION}
  CalcMachineEpsSingle;
  Epsilon := EpsSingle;
  ThreeEpsilon := ThreeEpsSingle;
{$ENDIF}{$ENDIF}{$ENDIF}
end;

//------------------------------------------------------------------------------

procedure SetPrecisionToleranceToEpsilon;
begin
  CalcMachineEps;
  PrecisionTolerance := Epsilon;
end;

//------------------------------------------------------------------------------

function SetPrecisionTolerance(NewTolerance: Float): Float;
begin
  Result := PrecisionTolerance;
  PrecisionTolerance := NewTolerance;
end;

//==============================================================================
// Miscellaneous
//==============================================================================

function Ceiling(const X: Float): Integer;
begin
  Result := Integer(Trunc(X));
  if Frac(X) > 0 then
    Inc(Result);
end;

//------------------------------------------------------------------------------

const
  PreCompFactsCount = 33; // all factorials that fit in a Single

var
  PreCompFacts: array [0..PreCompFactsCount] of Float;
  FactsPreComp: Boolean = False;

procedure PreComputeFactorials;
var
  I: Integer;
begin
  if not FactsPreComp then // sanity check
  begin
    PreCompFacts[0] := 1.0;
    PreCompFacts[1] := 1.0;
    PreCompFacts[2] := 2.0;
    for I := 3 to PreCompFactsCount do
      PreCompFacts[I] := PreCompFacts[I - 1] * I;
    FactsPreComp := True;
  end;
end;

//------------------------------------------------------------------------------

function Factorial(const N: Integer): Float;
var
  I: Integer;
begin
  if (N < 0) or (N > MaxFactorial) then
    Result := 0.0
  else
  begin
    PreComputeFactorials;
    if N <= PreCompFactsCount then
      Result := PreCompFacts[N]
    else
    begin // TODO Change following by: Gamma(N + 1)
      Result := PreCompFacts[PreCompFactsCount];
      for I := PreCompFactsCount + 1 to N do
        Result := Result * I;
    end;
  end;
end;

//------------------------------------------------------------------------------

function Floor(const X: Float): Float;
begin
  Result := Int(X);
end;

//------------------------------------------------------------------------------

function GCD(const X, Y: Cardinal): Cardinal; assembler;
asm
        JMP   @01        // We start with EAX <- X, EDX <- Y, and check to see if Y=0
@00:
        MOV     ECX, EDX // ECX <- EDX prepare for division
        XOR     EDX, EDX // clear EDX for Division
        DIV     ECX      // EAX <- EDX:EAX div ECX, EDX <- EDX:EAX mod ECX
        MOV     EAX, ECX // EAX <- ECX, and repeat if EDX <> 0
@01:
        AND     EDX, EDX // test to see if EDX is zero, without changing EDX
        JNZ     @00      // when EDX is zero EAX has the Result
end;

//------------------------------------------------------------------------------

function ISqrt(const I: Smallint): Smallint; assembler;
asm
        PUSH    EBX

        MOV     CX, AX  // load argument
        MOV     AX, -1  // init Result
        CWD             // init odd numbers to -1
        XOR     BX, BX  // init perfect squares to 0
@LOOP:
        INC     AX      // increment Result
        INC     DX      // compute
        INC     DX      // next odd number
        ADD     BX, DX  // next perfect square
        CMP     BX, CX  // perfect square > argument ?
        JBE     @LOOP   // until square greater than argument

        POP     EBX
end;

//------------------------------------------------------------------------------

function LCM(const X, Y: Cardinal): Cardinal;
begin
  Result := (X div GCD(Abs(X), Abs(Y))) * Y;
end;

//------------------------------------------------------------------------------

function NormalizeAngle(const Angle: Float): Float;
begin
  Result := Angle;
  {$IFDEF MATH_ANGLE_DEGREES}
  Result := DegToRad(Result);
  {$ENDIF}
  {$IFDEF MATH_ANGLE_GRADS}
  Result := GradToRad(Result);
  {$ENDIF}

  Result := Frac(Result * Inv2Pi);
  if Result < -0.5 then
    Result := Result + 1 else

  if Result >= 0.5 then
     Result := Result - 1;

  Result := Result * TwoPi;

  {$IFDEF MATH_ANGLE_DEGREES}
  Result := RadToDeg(Result);
  {$ENDIF}
  {$IFDEF MATH_ANGLE_GRADS}
  Result := RadToGrad(Result);
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function Pythagoras(const X, Y: Float): Float;
var
  AbsX, AbsY: Float;
begin
  AbsX := Abs(X);
  AbsY := Abs(Y);

  if AbsX > AbsY then
    Result := AbsX * Sqrt(1.0 + Sqr(AbsY / AbsX))
  else
  if AbsY = 0.0 then
    Result := 0.0
  else
    Result := AbsY * Sqrt(1.0 + Sqr(AbsX / AbsY));
end;

//------------------------------------------------------------------------------

function Sgn(const X: Float): Integer;
begin
  if X > 0.0 then
    Result := 1
  else
  if X < 0.0 then
    Result := -1
  else
    Result := 0;
end;

//------------------------------------------------------------------------------

function Signe(const X, Y: Float): Float;
begin
  if X > 0.0 then
  begin
    if Y > 0.0 then
      Result := X
    else
      Result := -X;
  end
  else
  begin
    if Y < 0.0 then
      Result := X
    else
      Result := -X;
  end;
end;

//==============================================================================
// Set support
//==============================================================================

constructor TRange.Create(const Low, High: Integer);
begin
  inherited Create;
  FLow := Low;
  FHigh := High;
end;

//------------------------------------------------------------------------------

{  5..10                                     }
{  R       F     RL<L   RH>H   RH>=L   RL<=H }
{ 0..4     0      1      0      0       1    }
{ 11..12   0      0      1      1       0    }
{ 6..11    1      0      1      1       1    }
{ 10..11   1      0      1      1       1    }
{ 0..5     1      1      0      1       1    }
{ 0..11    1      1      1      1       1    }
{ 5..10    1      0      0      1       1    }
{ 6..7     1      0      0      1       1    }

function TRange.Overlap(const Low, High: Integer): Boolean;
begin
  Result := (High >= FLow) xor (Low <= FHigh);
end;

//------------------------------------------------------------------------------

function TRange.Touch(const Low, High: Integer): Boolean;
begin
  Result := (High >= FLow - 1) xor (Low <= FHigh + 1);
end;

//------------------------------------------------------------------------------

function TRange.Inside(const Low, High: Integer): Boolean;
begin
  Result := (FLow >= Low) and (FHigh <= High);
end;

//------------------------------------------------------------------------------

function TRange.HasElement(const I: Integer): Boolean;
begin
  Result := (I >= FLow) and (I <= FHigh);
end;

//------------------------------------------------------------------------------

procedure TRange.Merge(const Low, High: Integer);
begin
  if (High >= FLow - 1) xor (Low <= FHigh + 1) then
  begin
    if Low < FLow then
      FLow := Low;

    if High > FHigh then
      FHigh := High;
  end
  else
    raise EJclMathError.CreateResRec(@RsRangeError);
end;

//------------------------------------------------------------------------------

constructor TRangeSet.Create;
begin
  inherited Create;
  FRanges := TList.Create;
end;

//------------------------------------------------------------------------------

destructor TRangeSet.Destroy;
begin
  Clear;
  FRanges.Free;
  FRanges := nil;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TRangeSet.Clear;
var
  I: Integer;
begin
  for I := 0 to FRanges.Count - 1 do
    TRange(FRanges[I]).Free;
  FRanges.Clear;
end;

//------------------------------------------------------------------------------

procedure TRangeSet.Invert;
var
  I: Integer;
  R, Pre: TRange;
begin
  if (FRanges.Count > 0) and (TRange(FRanges[0]).FLow > 0) then
    Pre := TRange.Create(0, TRange(FRanges[0]).FLow - 1)
  else
    Pre := nil;
  for I := 0 to FRanges.Count - 2 do
  begin
    R := TRange(FRanges[I]);
    R.FLow := R.FHigh + 1;
    R.FHigh := TRange(FRanges[I + 1]).FLow - 1;
  end;
  if FRanges.Count > 0 then
    if TRange(FRanges[FRanges.Count - 1]).FHigh = MaxInt then
    begin
      TRange(FRanges[FRanges.Count - 1]).Free;
      FRanges.Delete(FRanges.Count - 1);
    end
    else
    begin
      TRange(FRanges[FRanges.Count - 1]).FLow := TRange(FRanges[FRanges.Count - 1]).FHigh + 1;
      TRange(FRanges[FRanges.Count - 1]).FHigh := MaxInt;
    end;
  if Pre <> nil then
    FRanges.Insert(0, Pre);
end;

//------------------------------------------------------------------------------

procedure TRangeSet.SetRange(const Low, High: Integer; const Value: Boolean);
var
  I, J, K: Integer;
  R: TRange;
begin
  I := 0;
  K := FRanges.Count;
  while (I <= K - 1) and (Low > TRange(FRanges[I]).FHigh) do
    Inc(I);
  if (I = K) or // append
    ((Low < TRange(FRanges[I]).FLow) and (High < TRange(FRanges[I]).FLow)) then // insert
  begin
    if Value then
      FRanges.Insert(I, TRange.Create(Low, High));
    Exit;
  end;
  // I = first block that overlaps

  J := I;
  while (J < K - 1) and (TRange(FRanges[J + 1]).FLow <= High) do
    Inc(J);
  // J = last block that overlaps

  if not Value then // clear range (NOT IMPL)
  begin
    if I = J then
    begin
      R := FRanges[I];
      if R.Inside(Low, High) then
      begin
        R.Free;
        FRanges.Delete(I);
        Exit;
      end;
    end
    else
    begin
      for K := I + 1 to J - 1 do
      begin
        TRange(FRanges[I + 1]).Free;
        FRanges.Delete(I + 1);
      end;
    end;
  end
  else // set range
  begin
    R := FRanges[I];
    if I = J then
    begin
      if Low < R.FLow then
        R.FLow := Low;
      if High > R.FHigh then
        R.FHigh := High;
    end
    else
    begin
      R.FLow := Low;
      R.FHigh := High;
      for K := I + 1 to J do
      begin
        TRange(FRanges[I + 1]).Free;
        FRanges.Delete(I + 1);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TRangeSet.GetBit(const Idx: Integer): Boolean;
var
  I, J: Integer;
begin
  I := 0;
  J := FRanges.Count - 1;
  while (I <= J) and (Idx > TRange(FRanges[I]).FHigh) do
    Inc(I);
  Result := (I <= J) and (Idx >= TRange(FRanges[I]).FLow);
end;

//------------------------------------------------------------------------------

function TRangeSet.GetRange(const Low, High: Integer; const Value: Boolean): Boolean;
var
  I, J: Integer;
begin
  I := 0;
  J := FRanges.Count - 1;
  while (I <= J) and (Low > TRange(FRanges[I]).FHigh) do
    Inc(I);
  Result := (I <= J) and (Low >= TRange(FRanges[I]).FLow) and (High <= TRange(FRanges[I]).FHigh);
end;

//------------------------------------------------------------------------------

procedure TRangeSet.SetBit(const Idx: Integer; const Value: Boolean);
begin
  SetRange(Idx, Idx, Value);
end;

//------------------------------------------------------------------------------

constructor TFlatSet.Create;
begin
  inherited Create;
  FBits := TBits.Create;
end;

//------------------------------------------------------------------------------

destructor TFlatSet.Destroy;
begin
  FBits.Free;
  FBits := nil;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TFlatSet.Clear;
begin
  FBits.Size := 0;
end;

//------------------------------------------------------------------------------

procedure TFlatSet.Invert;
var
  I: Integer;
begin
  for I := 0 to FBits.Size - 1 do
    FBits[I] := not FBits[I];
end;

//------------------------------------------------------------------------------

procedure TFlatSet.SetRange(const Low, High: Integer; const Value: Boolean);
var
  I: Integer;
begin
  for I := High downto Low do
    FBits[I] := Value;
end;

//------------------------------------------------------------------------------

function TFlatSet.GetBit(const Idx: Integer): Boolean;
begin
  Result := FBits[Idx];
end;

//------------------------------------------------------------------------------

function TFlatSet.GetRange(const Low, High: Integer; const Value: Boolean): Boolean;
var
  I: Integer;
begin
  if not Value and (High >= FBits.Size) then
  begin
    Result := False;
    Exit;
  end;
  for I := Low to Min(High, FBits.Size - 1) do
    if FBits[I] <> Value then
    begin
      Result := False;
      Exit;
    end;
  Result := True;
end;

//------------------------------------------------------------------------------

procedure TFlatSet.SetBit(const Idx: Integer; const Value: Boolean);
begin
  FBits[Idx] := Value;
end;

//------------------------------------------------------------------------------

destructor TSparseFlatSet.Destroy;
begin
  Clear;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TSparseFlatSet.Clear;
var
  F: Integer;
begin
  if FSetList <> nil then
  begin
    for F := 0 to FSetListEntries - 1 do
      if FSetList^[F] <> nil then
        Release(PDelphiSet(FSetList^[F]));
    FreeMem(FSetList, FSetListEntries * SizeOf(Pointer));
    FSetList := nil;
    FSetListEntries := 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TSparseFlatSet.Invert;
var
  F: Integer;
begin
  for F := 0 to FSetListEntries - 1 do
    if FSetList^[F] <> nil then
      PDelphiSet(FSetList^[F])^ := CompleteDelphiSet - PDelphiSet(FSetList^[F])^;
end;

//------------------------------------------------------------------------------

function TSparseFlatSet.GetBit(const Idx: Integer): Boolean;
var
  SetIdx: Integer;
begin
  SetIdx := Idx shr 8;
  Result := (SetIdx < FSetListEntries) and (FSetList^[SetIdx] <> nil) and
    (Byte(Idx and $FF) in PDelphiSet(FSetList^[SetIdx])^);
end;

//------------------------------------------------------------------------------

procedure TSparseFlatSet.SetBit(const Idx: Integer; const Value: Boolean);
var
  I, SetIdx: Integer;
  S: PDelphiSet;
begin
  SetIdx := Idx shr 8;
  if SetIdx >= FSetListEntries then
    if Value then
    begin
      I := FSetListEntries;
      FSetListEntries := SetIdx + 1;
      ReallocMem(FSetList, FSetListEntries * SizeOf(Pointer));
      FillChar(FSetList^[I], (FSetListEntries - I) * SizeOf(Pointer), #0);
    end
    else
      Exit;
  S := FSetList^[SetIdx];
  if S = nil then
    if Value then
    begin
      New(S);
      S^ := [];
      FSetList^[SetIdx] := S;
    end
    else
      Exit;
  Include(S^, Byte(Idx and $FF));
end;

//------------------------------------------------------------------------------

procedure TSparseFlatSet.SetRange(const Low, High: Integer; const Value: Boolean);
var
  I, LowSet, HighSet: Integer;

  procedure SetValue(const S: TDelphiSet; const SetIdx: Integer);
  var
    D: PDelphiSet;
  begin
    D := FSetList^[SetIdx];
    if D = nil then
    begin
      if Value then
      begin
        New(D);
        D^ := S;
        FSetList^[SetIdx] := D;
      end;
    end
    else
    if Value then
      D^ := D^ + S
    else
      D^ := D^ - S;
  end;

begin
  LowSet := Low shr 8;
  HighSet := High shr 8;
  if HighSet >= FSetListEntries then
  begin
    I := FSetListEntries;
    FSetListEntries := HighSet + 1;
    ReallocMem(FSetList, FSetListEntries * SizeOf(Pointer));
    FillChar(FSetList^[I], (FSetListEntries - I) * SizeOf(Pointer), #0);
  end;
  if LowSet = HighSet then
    SetValue([Byte(Low and $FF)..Byte(High and $FF)], LowSet)
  else
  begin
    SetValue([Byte(Low and $FF)..$FF], LowSet);
    SetValue([0..Byte(High and $FF)], HighSet);
    for I := LowSet + 1 to HighSet - 1 do
      SetValue(CompleteDelphiSet, I);
  end;
end;

//==============================================================================
// Prime numbers
//==============================================================================

const
  PrimeCacheLimit = 65537; // 8K lookup table. Note: Sqr(65537) > MaxLongint

var
  PrimeSet: TFlatSet = nil;

procedure InitPrimeSet;
var
  I, J: Integer;
begin
  PrimeSet := TFlatSet.Create;
  PrimeSet.SetRange(2, PrimeCacheLimit, True);
  PrimeSet.SetRange(0, 1, False);
  for I := 2 to System.Trunc(System.Sqrt(PrimeCacheLimit)) do
    if PrimeSet.GetBit(I) then
      for J := 2 to (PrimeCacheLimit div I) do
        PrimeSet.SetBit(I * J,False);
end;

//------------------------------------------------------------------------------

function IsPrime(const N: Integer): Boolean;
var
  I: Integer;
  R: Extended;
begin
  if N < 0 then
    Result := IsPrime(-N)
  else
  if N < 2 then
    Result := False
    else
    begin
      if PrimeSet = nil then // initialize look-up table
        InitPrimeSet;
      if N <= PrimeCacheLimit then // do look-up
        Result := PrimeSet.GetBit(N)
        else
        begin // calculate
          R := N;
          for I := 2 to Round(Sqrt (R)) do
            if ((I > PrimeCacheLimit) or PrimeSet.GetBit(N)) and (N mod I = 0) then
            begin
              Result := False;
              Exit;
            end;
          Result := True;
        end;
    end;
end;

//------------------------------------------------------------------------------

function PrimeFactors(const N: Integer): TDynIntegerArray;
var
  I, L: Integer;
  J: Integer;
  R: Extended;
begin
  SetLength(Result, 0);
  if N < 0 then
    Result := PrimeFactors(-N)
  else
  if N = 1 then
    Exit
  else
  begin
    if PrimeSet = nil then // initialize look-up table
      InitPrimeSet;

    L := 0;
    J := N;
    R := N;
    for I := 2 to Round(Sqrt(R)) do
      if ((I > PrimeCacheLimit) or PrimeSet.GetBit(I)) and (N mod I = 0) then
      begin // I is a prime factor
        Inc(L);
        SetLength(Result, L);
        Result[L - 1] := I;

        repeat
          J := J div I;
          if J = 1 then // no more factors
            Exit;
        until J mod I <> 0;
      end;
  end;
end;

//------------------------------------------------------------------------------

function IsPrimeFactor(const F, N: Integer): Boolean;
begin
  Result := (N mod F = 0) and IsPrime(F);
end;

//------------------------------------------------------------------------------

function IsRelativePrime(const X, Y: Cardinal): Boolean;
begin
  Result := GCD(X, Y) = 1;
end;

//==============================================================================
// CRC
//==============================================================================

const
  CRCPolynom = $04C11DB7;

  Crc32Table: array [0..255] of Cardinal = (
    $00000000, $04C11DB7, $09823B6E, $0D4326D9, $130476DC, $17C56B6B, $1A864DB2, $1E475005,
    $2608EDB8, $22C9F00F, $2F8AD6D6, $2B4BCB61, $350C9B64, $31CD86D3, $3C8EA00A, $384FBDBD,
    $4C11DB70, $48D0C6C7, $4593E01E, $4152FDA9, $5F15ADAC, $5BD4B01B, $569796C2, $52568B75,
    $6A1936C8, $6ED82B7F, $639B0DA6, $675A1011, $791D4014, $7DDC5DA3, $709F7B7A, $745E66CD,
    $9823B6E0, $9CE2AB57, $91A18D8E, $95609039, $8B27C03C, $8FE6DD8B, $82A5FB52, $8664E6E5,
    $BE2B5B58, $BAEA46EF, $B7A96036, $B3687D81, $AD2F2D84, $A9EE3033, $A4AD16EA, $A06C0B5D,
    $D4326D90, $D0F37027, $DDB056FE, $D9714B49, $C7361B4C, $C3F706FB, $CEB42022, $CA753D95,
    $F23A8028, $F6FB9D9F, $FBB8BB46, $FF79A6F1, $E13EF6F4, $E5FFEB43, $E8BCCD9A, $EC7DD02D,
    $34867077, $30476DC0, $3D044B19, $39C556AE, $278206AB, $23431B1C, $2E003DC5, $2AC12072,
    $128E9DCF, $164F8078, $1B0CA6A1, $1FCDBB16, $018AEB13, $054BF6A4, $0808D07D, $0CC9CDCA,
    $7897AB07, $7C56B6B0, $71159069, $75D48DDE, $6B93DDDB, $6F52C06C, $6211E6B5, $66D0FB02,
    $5E9F46BF, $5A5E5B08, $571D7DD1, $53DC6066, $4D9B3063, $495A2DD4, $44190B0D, $40D816BA,
    $ACA5C697, $A864DB20, $A527FDF9, $A1E6E04E, $BFA1B04B, $BB60ADFC, $B6238B25, $B2E29692,
    $8AAD2B2F, $8E6C3698, $832F1041, $87EE0DF6, $99A95DF3, $9D684044, $902B669D, $94EA7B2A,
    $E0B41DE7, $E4750050, $E9362689, $EDF73B3E, $F3B06B3B, $F771768C, $FA325055, $FEF34DE2,
    $C6BCF05F, $C27DEDE8, $CF3ECB31, $CBFFD686, $D5B88683, $D1799B34, $DC3ABDED, $D8FBA05A,
    $690CE0EE, $6DCDFD59, $608EDB80, $644FC637, $7A089632, $7EC98B85, $738AAD5C, $774BB0EB,
    $4F040D56, $4BC510E1, $46863638, $42472B8F, $5C007B8A, $58C1663D, $558240E4, $51435D53,
    $251D3B9E, $21DC2629, $2C9F00F0, $285E1D47, $36194D42, $32D850F5, $3F9B762C, $3B5A6B9B,
    $0315D626, $07D4CB91, $0A97ED48, $0E56F0FF, $1011A0FA, $14D0BD4D, $19939B94, $1D528623,
    $F12F560E, $F5EE4BB9, $F8AD6D60, $FC6C70D7, $E22B20D2, $E6EA3D65, $EBA91BBC, $EF68060B,
    $D727BBB6, $D3E6A601, $DEA580D8, $DA649D6F, $C423CD6A, $C0E2D0DD, $CDA1F604, $C960EBB3,
    $BD3E8D7E, $B9FF90C9, $B4BCB610, $B07DABA7, $AE3AFBA2, $AAFBE615, $A7B8C0CC, $A379DD7B,
    $9B3660C6, $9FF77D71, $92B45BA8, $9675461F, $8832161A, $8CF30BAD, $81B02D74, $857130C3,
    $5D8A9099, $594B8D2E, $5408ABF7, $50C9B640, $4E8EE645, $4A4FFBF2, $470CDD2B, $43CDC09C,
    $7B827D21, $7F436096, $7200464F, $76C15BF8, $68860BFD, $6C47164A, $61043093, $65C52D24,
    $119B4BE9, $155A565E, $18197087, $1CD86D30, $029F3D35, $065E2082, $0B1D065B, $0FDC1BEC,
    $3793A651, $3352BBE6, $3E119D3F, $3AD08088, $2497D08D, $2056CD3A, $2D15EBE3, $29D4F654,
    $C5A92679, $C1683BCE, $CC2B1D17, $C8EA00A0, $D6AD50A5, $D26C4D12, $DF2F6BCB, $DBEE767C,
    $E3A1CBC1, $E760D676, $EA23F0AF, $EEE2ED18, $F0A5BD1D, $F464A0AA, $F9278673, $FDE69BC4,
    $89B8FD09, $8D79E0BE, $803AC667, $84FBDBD0, $9ABC8BD5, $9E7D9662, $933EB0BB, $97FFAD0C,
    $AFB010B1, $AB710D06, $A6322BDF, $A2F33668, $BCB4666D, $B8757BDA, $B5365D03, $B1F740B4
    );

  CrcStart = $FFFFFFFF;
  CrcBytes = 4;
  CrcHighBit = $80000000;
  NotCrcHighBit = $7FFFFFFF;

//------------------------------------------------------------------------------

function UpdateCrc32(CurByte: Byte; CurCrc: Cardinal): Cardinal; assembler;
asm
//Result := Crc32Table[Byte(CurCrc shr 24)] xor (CurCrc shl 8) xor CurByte;
        MOV     ECX, EAX
        MOV     EAX, EDX
        SHR     EAX, 24
        MOV     EAX, DWORD PTR [4 * EAX + Crc32Table]
        SHL     EDX, 8
        XOR     EAX, EDX
        XOR     AL, CL
end;

//------------------------------------------------------------------------------

function Crc32Corr(Crc: Cardinal; N: Integer): Integer;
var
  I: Integer;
begin
  //calculate Syndrome
  for I := 1 to CrcBytes do
    Crc := UpdateCrc32(0, Crc);
  I := -1;
  repeat
    Inc(I);
    if (Crc and 1) <> 0 then
      Crc := ((Crc xor CrcPolynom) shr 1) or CrcHighBit
    else
      Crc := (Crc shr 1) and NotCrcHighBit;
  until (Crc = CrcHighBit) or (I = (N + CrcBytes) * 8);
  if Crc <> CrcHighBit then
    Result := -1000 // not correctable
  else
      // I = No. of single faulty bit
      // (high bit first,
      // starting from lowest with CRC bits)
    Result := I - (CrcBytes * 8);
      // Result <  0 faulty CRC-bit
      // Result >= 0 No. of faulty data bit
end;

//------------------------------------------------------------------------------

function Crc32(const X: array of Byte; N: Integer; Crc: Cardinal): Cardinal;
var
  I: Integer;
begin
  Result := CrcStart;
  for I := 0 to N - 1 do // The CRC Bytes are located at the end of the information
  begin
    Result := UpdateCrc32(X[I], Result);
  end;
  for i := 0 to CrcBytes - 1 do
  begin
    Result := UpdateCrc32(Byte(Crc shr (8 * (CrcBytes - 1))), Result);
    Crc := Crc shl 8;
  end;
end;

//------------------------------------------------------------------------------

function CheckCrc32(var X: array of Byte; N: Integer; Crc: Cardinal): Integer;
var
  I, J: Integer;
  C: Byte;
begin
  Crc := Crc32(X, N, Crc);
  if Crc = 0 then
    Result := 0 // No CRC-error
  else
  begin
    J := Crc32Corr(Crc, N);
    if J < -(CrcBytes * 8 + 1) then
      Result := -1 // non-correctable error (more than one wrong bit)
    else
    begin
      if J < 0 then
        Result := 1 // one faulty Bit in CRC itself
      else
      begin // Bit j is faulty
        I := J and 7; // I <= 7 (faulty Bit in Byte)
        C := 1 shl I; // C <= 128
        I := J shr 3; // I: Index of faulty Byte
        X[N - 1 - I] := X[N - 1 - I] xor C; // correct faulty bit
        Result := 1; // Correctable error
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function Crc16(const X: array of Byte; N: Integer; Crc: Word): Word;

  function FCrc16(CRC: Word; Data: Pointer; DataSize: LongWord): Word; assembler;
  asm
           AND    EDX,EDX
           JZ     @@2
           AND    ECX,ECX
           JLE    @@2
           PUSH   EBX
           PUSH   EDI
           XOR    EBX,EBX
           LEA    EDI,CS:[OFFSET @CRC16]
  @@1:     MOV    BL,[EDX]
           XOR    BL,AL
           SHR    AX,8
           INC    EDX
           XOR    AX,[EDI + EBX * 2]
           DEC    ECX
           JNZ    @@1
           POP    EDI
           POP    EBX
  @@2:     RET
           NOP
  @CRC16:  DW     00000h, 0C0C1h, 0C181h, 00140h, 0C301h, 003C0h, 00280h, 0C241h
           DW     0C601h, 006C0h, 00780h, 0C741h, 00500h, 0C5C1h, 0C481h, 00440h
           DW     0CC01h, 00CC0h, 00D80h, 0CD41h, 00F00h, 0CFC1h, 0CE81h, 00E40h
           DW     00A00h, 0CAC1h, 0CB81h, 00B40h, 0C901h, 009C0h, 00880h, 0C841h
           DW     0D801h, 018C0h, 01980h, 0D941h, 01B00h, 0DBC1h, 0DA81h, 01A40h
           DW     01E00h, 0DEC1h, 0DF81h, 01F40h, 0DD01h, 01DC0h, 01C80h, 0DC41h
           DW     01400h, 0D4C1h, 0D581h, 01540h, 0D701h, 017C0h, 01680h, 0D641h
           DW     0D201h, 012C0h, 01380h, 0D341h, 01100h, 0D1C1h, 0D081h, 01040h
           DW     0F001h, 030C0h, 03180h, 0F141h, 03300h, 0F3C1h, 0F281h, 03240h
           DW     03600h, 0F6C1h, 0F781h, 03740h, 0F501h, 035C0h, 03480h, 0F441h
           DW     03C00h, 0FCC1h, 0FD81h, 03D40h, 0FF01h, 03FC0h, 03E80h, 0FE41h
           DW     0FA01h, 03AC0h, 03B80h, 0FB41h, 03900h, 0F9C1h, 0F881h, 03840h
           DW     02800h, 0E8C1h, 0E981h, 02940h, 0EB01h, 02BC0h, 02A80h, 0EA41h
           DW     0EE01h, 02EC0h, 02F80h, 0EF41h, 02D00h, 0EDC1h, 0EC81h, 02C40h
           DW     0E401h, 024C0h, 02580h, 0E541h, 02700h, 0E7C1h, 0E681h, 02640h
           DW     02200h, 0E2C1h, 0E381h, 02340h, 0E101h, 021C0h, 02080h, 0E041h
           DW     0A001h, 060C0h, 06180h, 0A141h, 06300h, 0A3C1h, 0A281h, 06240h
           DW     06600h, 0A6C1h, 0A781h, 06740h, 0A501h, 065C0h, 06480h, 0A441h
           DW     06C00h, 0ACC1h, 0AD81h, 06D40h, 0AF01h, 06FC0h, 06E80h, 0AE41h
           DW     0AA01h, 06AC0h, 06B80h, 0AB41h, 06900h, 0A9C1h, 0A881h, 06840h
           DW     07800h, 0B8C1h, 0B981h, 07940h, 0BB01h, 07BC0h, 07A80h, 0BA41h
           DW     0BE01h, 07EC0h, 07F80h, 0BF41h, 07D00h, 0BDC1h, 0BC81h, 07C40h
           DW     0B401h, 074C0h, 07580h, 0B541h, 07700h, 0B7C1h, 0B681h, 07640h
           DW     07200h, 0B2C1h, 0B381h, 07340h, 0B101h, 071C0h, 07080h, 0B041h
           DW     05000h, 090C1h, 09181h, 05140h, 09301h, 053C0h, 05280h, 09241h
           DW     09601h, 056C0h, 05780h, 09741h, 05500h, 095C1h, 09481h, 05440h
           DW     09C01h, 05CC0h, 05D80h, 09D41h, 05F00h, 09FC1h, 09E81h, 05E40h
           DW     05A00h, 09AC1h, 09B81h, 05B40h, 09901h, 059C0h, 05880h, 09841h
           DW     08801h, 048C0h, 04980h, 08941h, 04B00h, 08BC1h, 08A81h, 04A40h
           DW     04E00h, 08EC1h, 08F81h, 04F40h, 08D01h, 04DC0h, 04C80h, 08C41h
           DW     04400h, 084C1h, 08581h, 04540h, 08701h, 047C0h, 04680h, 08641h
           DW     08201h, 042C0h, 04380h, 08341h, 04100h, 081C1h, 08081h, 04040h
  end;

begin
  Result := not FCRC16(Crc, pointer(X[0]), N);
end;

//------------------------------------------------------------------------------

function InternetChecksum(const X: array of Byte; N: Integer): Cardinal;
begin
  Result := 0;

  while(N > 1) do
  begin
    Result := Result + Word(X[N] shl 8 + X[N]) ;
    N := N - 2;
  end;

  if (N > 0) then
     Result := Result + X[1];

  while ((Result shr 16)<>0) do
    Result := (Result and $ffff) + (Result shr 16);

  Result := not(Result);
end;


//==============================================================================
// NAN and Infinity support
//==============================================================================

const
  NANQuietBits: Int64 = $7FFFFFFFFFFFFFFF;
  PositiveInfinityBits: Int64 = $7FF0000000000000;
  NegativeInfinityBits: Int64 = $FFF0000000000000;

var
  dNANQuiet: Double absolute NANQuietBits;
  dPositiveInfinity: Double = 1/0;
  dNegativeInfinity: Double absolute NegativeInfinityBits;



function IsNAN(const d: Double): Boolean;
var
  Overlay: Int64 absolute d;
begin
  Result := ((Overlay and $7FF0000000000000) = $7FF0000000000000) and
    ((Overlay and $000FFFFFFFFFFFFF) <> $0000000000000000)
end;

//------------------------------------------------------------------------------

function IsInfinity(const d: Double): Boolean;
var
  Overlay: Int64 absolute d;
begin
  Result := (Overlay and $7FF0000000000000) = $7FF0000000000000;
end;

//------------------------------------------------------------------------------
(*
function IsNAN(const d: Extended): Boolean;
var
  Overlay: Int64 absolute d;
begin
  Result := ((Overlay and $7FF0000000000000) = $7FF0000000000000) and
    ((Overlay and $000FFFFFFFFFFFFF) <> $0000000000000000)
end;
*)
//------------------------------------------------------------------------------

function IsIndeterminate(const d: Double): Boolean;
var
  Overlay: Int64 absolute d;
begin
  Result := (Overlay and $FFF8000000000000) = $FFF8000000000000;
end;

//------------------------------------------------------------------------------

function NegativeInfinity: Double;
begin
  Result := dNegativeInfinity
end;

//==============================================================================
// Rational Numbers
//==============================================================================

constructor TJclRational.Create(const Numerator: Integer; const Denominator: Integer);
begin
  inherited Create;
  Assign(Numerator, Denominator);
end;

//------------------------------------------------------------------------------

constructor TJclRational.Create;
begin
  inherited Create;
  AssignZero;
end;

//------------------------------------------------------------------------------

constructor TJclRational.Create(const R: Float);
begin
  inherited Create;
  Assign(R);
end;

//------------------------------------------------------------------------------

procedure TJclRational.Simplify;
var
  I: Integer;
begin
  if FN < 0 then
  begin
    FT := -FT;
    FN := -FN;
  end;

  if (FT = 1) or (FN = 1) or (FT = 0) then
    Exit;

  I := GCD(FT, FN);
  FT := FT div I;
  FN := FN div I;
end;

//------------------------------------------------------------------------------

procedure TJclRational.Assign(const Numerator: Integer; const Denominator: Integer);
begin
  if Denominator = 0 then
    raise EJclMathError.CreateResRec(@RsInvalidRational);
  FT := Numerator;
  FN := Denominator;
  if FN <> 1 then
    Simplify;
end;

//------------------------------------------------------------------------------

procedure TJclRational.Assign(const R: TJclRational);
begin
  FT := R.FT;
  FN := R.FN;
end;

//------------------------------------------------------------------------------

procedure TJclRational.Assign(const R: Float);
var
  T: TJclRational;
  Z: Integer;

  function CalcFrac(const R: Float; const Level: Integer): TJclRational;
  var
    I: Float;
    Z: Integer;
  begin
    if IsFloatZero(R) or (Level = 12) then // 0 (if Level = 12 we get an approximation)
      Result := TJclRational.Create
    else
    if FloatsEqual(R, 1.0) then // 1
    begin
      Result := TJclRational.Create;
      Result.AssignOne;
    end
    else
    if IsFloatZero(Frac(R * 1E8)) then // terminating decimal (<8)
      Result := TJclRational.Create(Trunc(R * 1E8), 100000000)
    else
    begin // recursive process
      I := 1.0 / R;
      Result := CalcFrac(Frac(I), Level + 1);
      Z := Trunc(I);
      Result.Add(Z);
      Result.Reciprocal;
    end;
  end;

begin
  T := CalcFrac(Frac(R), 1);
  try
    Z := Trunc(R);
    T.Add(Z);
    Assign(T);
  finally
    T.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclRational.AssignOne;
begin
  FT := 1;
  FN := 1;
end;

//------------------------------------------------------------------------------

procedure TJclRational.AssignZero;
begin
  FT := 0;
  FN := 1;
end;

//------------------------------------------------------------------------------

function TJclRational.IsEqual(const Numerator: Integer; const Denominator: Integer): Boolean;
var
  R: TJclRational;
begin
  R := TJclRational.Create(Numerator, Denominator);
  Result := IsEqual(R);
  R.Free;
end;

//------------------------------------------------------------------------------

function TJclRational.IsEqual(const R: TJclRational): Boolean;
begin
  Result := (FT = R.FT) and (FN = R.FN);
end;

//------------------------------------------------------------------------------

function TJclRational.IsEqual(const R: Float): Boolean;
begin
  Result := FloatsEqual(R, GetAsFloat);
end;

//------------------------------------------------------------------------------

function TJclRational.IsOne: Boolean;
begin
  Result := (FT = 1) and (FN = 1);
end;

//------------------------------------------------------------------------------

function TJclRational.IsZero: Boolean;
begin
  Result := FT = 0;
end;

//------------------------------------------------------------------------------

function TJclRational.Duplicate: TJclRational;
begin
  Result := TJclRational.Create(FT, FN);
end;

//------------------------------------------------------------------------------

procedure TJclRational.SetAsFloat(const R: Float);
begin
  Assign(R);
end;

//------------------------------------------------------------------------------

procedure TJclRational.SetAsString(const S: string);
var
  F: Integer;
begin
  F := Pos('/', S);
  if F = 0 then
    Assign(StrToFloat(S))
  else
    Assign(StrToInt(Copy(S,1,F - 1)), StrToInt(Copy(S, F + 1,Length(s))));
end;

//------------------------------------------------------------------------------

function TJclRational.GetAsFloat: Float;
begin
  Result := FT / FN;
end;

//------------------------------------------------------------------------------

function TJclRational.GetAsString: string;
begin
  Result := IntToStr(FT) + '/' + IntToStr(FN);
end;

//------------------------------------------------------------------------------

procedure TJclRational.Add(const R: TJclRational);
begin
  FT := FT * R.FN + R.FT;
  FN := FN * R.FN;
  Simplify;
end;

//------------------------------------------------------------------------------

procedure TJclRational.Add(const V: Integer);
begin
  Inc(FT, FN * V);
end;

//------------------------------------------------------------------------------

procedure TJclRational.Add(const V: Float);
begin
  Assign(GetAsFloat + V);
end;

//------------------------------------------------------------------------------

procedure TJclRational.Subtract(const V: Float);
begin
  Assign(GetAsFloat - V);
end;

//------------------------------------------------------------------------------

procedure TJclRational.Subtract(const R: TJclRational);
begin
  FT := FT * R.FN - R.FT;
  FN := FN * R.FN;
  Simplify;
end;

//------------------------------------------------------------------------------

procedure TJclRational.Subtract(const V: Integer);
begin
  Dec(FT, FN * V);
end;

//------------------------------------------------------------------------------

procedure TJclRational.Negate;
begin
  FT := -FT;
end;

//------------------------------------------------------------------------------

procedure TJclRational.Abs;
begin
  FT := System.Abs(FT);
  FN := System.Abs(FN);
end;

//------------------------------------------------------------------------------

function TJclRational.Sgn: Integer;
begin
  if FT = 0 then
    Result := 0
  else
  begin
    if JclMath.Sgn(FT) = JclMath.Sgn(FN) then
      Result := 1
    else
      Result := -1;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclRational.Divide(const V: Integer);
begin
  if V = 0 then
    raise EJclMathError.CreateResRec(@RsDivByZero);

  FN := FN * V;
  Simplify;
end;

//------------------------------------------------------------------------------

procedure TJclRational.Divide(const R: TJclRational);
begin
  if R.FT = 0 then
    raise EJclMathError.CreateResRec(@RsRationalDivByZero);

  FT := FT * R.FN;
  FN := FN * R.FT;
  Simplify;
end;

//------------------------------------------------------------------------------

procedure TJclRational.Divide(const V: Float);
begin
  Assign(GetAsFloat / V);
end;

//------------------------------------------------------------------------------

procedure TJclRational.Reciprocal;
begin
  if FT = 0 then
    raise EJclMathError.CreateResRec(@RsRationalDivByZero);
  SwapOrd(FT, FN);
end;

//------------------------------------------------------------------------------

procedure TJclRational.Multiply(const R: TJclRational);
begin
  FT := FT * R.FT;
  FN := FN * R.FN;
  Simplify;
end;

//------------------------------------------------------------------------------

procedure TJclRational.Multiply(const V: Integer);
begin
  FT := FT * V;
  Simplify;
end;

//------------------------------------------------------------------------------

procedure TJclRational.Multiply(const V: Float);
begin
  Assign(GetAsFloat * V);
end;

//------------------------------------------------------------------------------

procedure TJclRational.Power(const R: TJclRational);
begin
  Assign(JclMath.Power(GetAsFloat, R.GetAsFloat));
end;

//------------------------------------------------------------------------------

procedure TJclRational.Power(const V: Integer);
var
  T, N: Extended;
begin
  T := FT;
  N := FN;
  FT := Round(JclMath.Power(T, V));
  FN := Round(JclMath.Power(N, V));
end;

//------------------------------------------------------------------------------

procedure TJclRational.Power(const V: Float);
begin
  Assign(JclMath.Power(FT, V) / JclMath.Power(FN, V));
end;

//------------------------------------------------------------------------------

procedure TJclRational.Sqrt;
begin
  Assign(System.Sqrt(FT / FN));
end;

//------------------------------------------------------------------------------

procedure TJclRational.Sqr;
begin
  FT := System.Sqr(FT);
  FN := System.Sqr(FN);
end;

end.
