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
{ The Original Code is JclComplex.pas.                                         }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ Class for working with complex numbers.                                      }
{                                                                              }
{ Unit owner: Alexei Koudinov                                                  }
{ Last modified: January 30, 2001                                              }
{                                                                              }
{******************************************************************************}

unit JclComplex;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  SysUtils,
  JclBase, JclMath, JclResources, JclStrings, JclSysUtils;

const
  TComplex_VERSION = 5.01;

type
  TComplexKind = (crRectangular, crPolar);

  TCoords = record
    x: Float;     // rectangular real
    y: Float;     // rectangular imaginary
    r: Float;     // polar 1
    theta: Float; // polar 2
  end;

  TRectCoord = record
    x: Float;
    y: Float;
  end;

  TJclComplex = class (TObject)
  private   {z = x + yi}
    // -----------------------------
    FCoord: TCoords;
    FFracLen: Byte;
    function MiscalcSingle(const X: Float): Float;
    procedure MiscalcComplex; // eliminates miscalculation
    procedure FillCoords(const CompexType: TComplexKind);
    function GetRectangularString: string;
    function GetPolarString: string;
    procedure SetRectangularString(StrToParse: string);
    procedure SetPolarString(StrToParse: string);
    procedure SetFracLen(const X: Byte);
    function GetRadius: Float;
    function GetAngle: Float;
    function NormalizeAngle(Value: Float): Float;
  protected
    function Assign(const Coord: TCoords; const ComplexType: TComplexKind
      {$IFDEF SUPPORTS_DEFAULTPARAMS} = crRectangular {$ENDIF}): TJclComplex; overload;
    function CoreAdd(const First, Second: TRectCoord): TRectCoord;
    function CoreDiv(const First, Second: TRectCoord): TRectCoord;
    function CoreMul(const First, Second: TRectCoord): TRectCoord;
    function CoreSub(const First, Second: TRectCoord): TRectCoord;
    function CoreLn (const LnValue: TRectCoord): TRectCoord;
    function CoreExp(const ExpValue: TRectCoord): TRectCoord;
    function CorePwr(First, Second, Polar: TRectCoord): TRectCoord;
    function CoreIntPwr(First: TRectCoord; const Polar: TRectCoord; const Pwr: Integer): TRectCoord;
    function CoreRealPwr(First: TRectCoord; const Polar: TRectCoord; const Pwr: Float): TRectCoord;
    function CoreRoot(First: TRectCoord; const Polar: TRectCoord; const K, N: Word): TRectCoord;
    function CoreCos(const Value: TRectCoord): TRectCoord;
    function CoreSin(const Value: TRectCoord): TRectCoord;
    function CoreTan(const Value: TRectCoord): TRectCoord;
    function CoreCot(const Value: TRectCoord): TRectCoord;
    function CoreSec(const Value: TRectCoord): TRectCoord;
    function CoreCsc(const Value: TRectCoord): TRectCoord;
    function CoreCosH(const Value: TRectCoord): TRectCoord;
    function CoreSinH(const Value: TRectCoord): TRectCoord;
    function CoreTanH(const Value: TRectCoord): TRectCoord;
    function CoreCotH(const Value: TRectCoord): TRectCoord;
    function CoreSecH(const Value: TRectCoord): TRectCoord;
    function CoreCscH(const Value: TRectCoord): TRectCoord;
    function CoreI0(const Value: TRectCoord): TRectCoord;
    function CoreJ0(const Value: TRectCoord): TRectCoord;
    function CoreApproxLnGamma(const Value: TRectCoord): TRectCoord;
    function CoreLnGamma(Value: TRectCoord): TRectCoord;
    function CoreGamma(const Value: TRectCoord): TRectCoord;
  public
    property FracLength: Byte read FFracLen write SetFracLen default 8;

    //----------- constructors
    constructor Create; overload;
    constructor Create(const X, Y: Float; const ComplexType: TComplexKind
      {$IFDEF SUPPORTS_DEFAULTPARAMS} = crRectangular {$ENDIF}); overload;

    //----------- getting different parts of the number
    property RealPart: Float read FCoord.x;
    property ImaginaryPart: Float read FCoord.y;
    property Radius: Float read GetRadius;
    property Angle: Float read GetAngle;

    //----------- format output
    property AsString: string read GetRectangularString write SetRectangularString;
    property AsPolarString: string read GetPolarString write SetPolarString;

    //----------- complex numbers assignment routines
    function Assign(const X, Y: Float; const ComplexType: TComplexKind
      {$IFDEF SUPPORTS_DEFAULTPARAMS} = crRectangular {$ENDIF}): TJclComplex; overload;
    function AssignZero: TJclComplex;
    function AssignOne: TJclComplex;
    function Duplicate: TJclComplex;

    //----------- arithmetics -- modify the object itself
    function CAdd(const AddValue: TJclComplex): TJclComplex; overload;
    function CAdd(const X, Y: Float; const ComplexType: TComplexKind
      {$IFDEF SUPPORTS_DEFAULTPARAMS} = crRectangular {$ENDIF}): TJclComplex; overload;
    function CDiv(const DivValue: TJclComplex): TJclComplex; overload;
    function CDiv(const X, Y: Float; const ComplexType: TComplexKind
      {$IFDEF SUPPORTS_DEFAULTPARAMS} = crRectangular {$ENDIF}): TJclComplex; overload;
    function CMul(const MulValue: TJclComplex): TJclComplex; overload;
    function CMul(const X, Y: Float; const ComplexType: TComplexKind
      {$IFDEF SUPPORTS_DEFAULTPARAMS} = crRectangular {$ENDIF}): TJclComplex; overload;
    function CSub(const SubValue: TJclComplex): TJclComplex; overload;
    function CSub(const X, Y: Float; const ComplexType: TComplexKind
      {$IFDEF SUPPORTS_DEFAULTPARAMS} = crRectangular {$ENDIF}): TJclComplex; overload;
    function CNeg: TJclComplex;
    function CConjugate: TJclComplex;

    //----------- arithmetics -- creates new resulting object
    function CNewAdd(const AddValue: TJclComplex): TJclComplex; overload;
    function CNewAdd(const X, Y: Float; const ComplexType: TComplexKind
      {$IFDEF SUPPORTS_DEFAULTPARAMS} = crRectangular {$ENDIF}): TJclComplex; overload;
    function CNewDiv(const DivValue: TJclComplex): TJclComplex; overload;
    function CNewDiv(const X, Y: Float; const ComplexType: TComplexKind
      {$IFDEF SUPPORTS_DEFAULTPARAMS} = crRectangular {$ENDIF}): TJclComplex; overload;
    function CNewMul(const MulValue: TJclComplex): TJclComplex; overload;
    function CNewMul(const X, Y: Float; const ComplexType: TComplexKind
      {$IFDEF SUPPORTS_DEFAULTPARAMS} = crRectangular {$ENDIF}): TJclComplex; overload;
    function CNewSub(const SubValue: TJclComplex): TJclComplex; overload;
    function CNewSub(const X, Y: Float; const ComplexType: TComplexKind
      {$IFDEF SUPPORTS_DEFAULTPARAMS} = crRectangular {$ENDIF}): TJclComplex; overload;
    function CNewNeg: TJclComplex;
    function CNewConjugate: TJclComplex;

    //----------- natural log and exponential functions
    function CLn: TJclComplex;
    function CNewLn: TJclComplex;
    function CExp: TJclComplex;
    function CNewExp: TJclComplex;
    function CPwr(const PwrValue: TJclComplex): TJclComplex; overload;
    function CPwr(const X, Y: Float; const ComplexType: TComplexKind
      {$IFDEF SUPPORTS_DEFAULTPARAMS} = crRectangular {$ENDIF}): TJclComplex; overload;
    function CNewPwr(PwrValue: TJclComplex): TJclComplex; overload;
    function CNewPwr(const X, Y: Float; const ComplexType: TComplexKind
      {$IFDEF SUPPORTS_DEFAULTPARAMS} = crRectangular {$ENDIF}): TJclComplex; overload;
    function CIntPwr(const Pwr: Integer): TJclComplex; overload;
    function CNewIntPwr(const Pwr: Integer): TJclComplex; overload;
    function CRealPwr(const Pwr: Float): TJclComplex; overload;
    function CNewRealPwr(const Pwr: Float): TJclComplex; overload;
    function CRoot(const K, N: Word): TJclComplex; overload;
    function CNewRoot(const K, N: Word): TJclComplex; overload;
    function CSqrt: TJclComplex; overload;
    function CNewSqrt: TJclComplex; overload;

    //----------- trigonometric functions
    function CCos: TJclComplex;
    function CNewCos: TJclComplex;
    function CSin: TJclComplex;
    function CNewSin: TJclComplex;
    function CTan: TJclComplex;
    function CNewTan: TJclComplex;
    function CCot: TJclComplex;
    function CNewCot: TJclComplex;
    function CSec: TJclComplex;
    function CNewSec: TJclComplex;
    function CCsc: TJclComplex;
    function CNewCsc: TJclComplex;

    //----------- complex hyperbolic functions
    function CCosH: TJclComplex;
    function CNewCosH: TJclComplex;
    function CSinH: TJclComplex;
    function CNewSinH: TJclComplex;
    function CTanH: TJclComplex;
    function CNewTanH: TJclComplex;
    function CCotH: TJclComplex;
    function CNewCotH: TJclComplex;
    function CSecH: TJclComplex;
    function CNewSecH: TJclComplex;
    function CCscH: TJclComplex;
    function CNewCscH: TJclComplex;

    //----------- complex Bessel functions of order zero
    function CI0: TJclComplex;
    function CNewI0: TJclComplex;
    function CJ0: TJclComplex;
    function CNewJ0: TJclComplex;

    function CApproxLnGamma: TJclComplex;
    function CNewApproxLnGamma: TJclComplex;
    function CLnGamma: TJclComplex;
    function CNewLnGamma: TJclComplex;
    function CGamma: TJclComplex;
    function CNewGamma: TJclComplex;

    //----------- miscellaneous routines
    function AbsoluteValue: Float; overload;
    function AbsoluteValue(const Coord: TRectCoord): Float; overload;
    function AbsoluteValueSqr: Float; overload;
    function AbsoluteValueSqr(const Coord: TRectCoord): Float; overload;
    function FormatExtended(const X: Float): string;
  end;

var
  ComplexPrecision: Float = 1E-14;

const
  MaxTerm: Byte = 35;
  EpsilonSqr: Float = 1E-20;

implementation

const
  MaxFracLen = 18;
  RectOne: TRectCoord = (x: 1.0; y: 0.0);
  RectZero: TRectCoord = (x: 0.0; y: 0.0);
  RectInfinity: TRectCoord = (x: Infinity; y: Infinity);

//------------------------------------------------------------------------------

function Coordinates(const cX, cY: Float; CoordType: TComplexKind): TCoords;
begin
  case CoordType of
    crRectangular:
      begin
        Result.x := cX;
        Result.y := cY;
        Result.r := 0.0;
        Result.theta := 0.0;
      end;
    crPolar:
      begin
        Result.x := 0.0;
        Result.y := 0.0;
        Result.r := cX;
        Result.theta := cY;
      end;
  end;
end;

//------------------------------------------------------------------------------

function RectCoord(X, Y: Float): TRectCoord; overload;
begin
  Result.x := X;
  Result.y := Y;
end;

//------------------------------------------------------------------------------

function RectCoord(Value: TJclComplex): TRectCoord; overload;
begin
  Result.x := Value.FCoord.x;
  Result.y := Value.FCoord.y;
end;

//------------------------------------------------------------------------------

constructor TJclComplex.Create;
begin
  inherited Create;
  AssignZero;
  FFracLen := MaxFracLen;
end;

//------------------------------------------------------------------------------

constructor TJclComplex.Create(const X, Y: Float; const ComplexType: TComplexKind);
begin
  inherited Create;

  Assign(X, Y, ComplexType);
  FFracLen := MaxFracLen;
end;

//------------------------------------------------------------------------------

procedure TJclComplex.FillCoords(const CompexType: TComplexKind);
begin
  MiscalcComplex;
  case CompexType of
    crPolar:
      begin
        FCoord.x := FCoord.r * Cos(FCoord.theta);
        FCoord.y := FCoord.r * Sin(FCoord.theta);
      end;
    crRectangular:
      begin
        if FCoord.x = 0.0 then
        begin
          FCoord.r := Abs(FCoord.y);
          FCoord.theta := PiOn2 * Sgn(FCoord.y);
        end
        else
        begin
          FCoord.r := AbsoluteValue;
          FCoord.theta := System.ArcTan(FCoord.y / FCoord.x);
          if FCoord.x < 0.0 then
            FCoord.theta := FCoord.theta + Pi * Sgn(FCoord.y);
        end;
      end;
  end;
  MiscalcComplex;
end;

//------------------------------------------------------------------------------

function TJclComplex.MiscalcSingle(const X: Float): Float;
begin
  Result := X;
  if Abs(Result) < ComplexPrecision then
    Result := 0.0;
end;

//------------------------------------------------------------------------------

procedure TJclComplex.MiscalcComplex; // eliminates miscalculation
begin
  FCoord.x := MiscalcSingle(FCoord.x);
  FCoord.y := MiscalcSingle(FCoord.y);
  FCoord.r := MiscalcSingle(FCoord.r);
  if FCoord.r = 0.0 then
    FCoord.theta := 0.0
  else
    FCoord.theta := MiscalcSingle(FCoord.theta);
end;

//------------------------------------------------------------------------------

function TJclComplex.Assign(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
begin
  Result := Assign(Coordinates(X, Y, ComplexType), ComplexType);
end;

//------------------------------------------------------------------------------

function TJclComplex.Assign(const Coord: TCoords; const ComplexType: TComplexKind): TJclComplex;
begin
  FCoord := Coord;
  FillCoords(ComplexType);
  MiscalcComplex;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.AssignZero: TJclComplex;
begin
  Result := Assign(0.0, 0.0, crRectangular);
end;

//------------------------------------------------------------------------------

function TJclComplex.AssignOne: TJclComplex;
begin
  Result := Assign(1.0, 0.0, crRectangular);
end;

//------------------------------------------------------------------------------

function TJclComplex.GetRectangularString: string;
begin
  MiscalcComplex;
  if (FCoord.x = 0.0) and (FCoord.y = 0.0) then
    Result := '0'
  else
  if FCoord.x <> 0.0 then
  begin
    Result := FormatExtended(FCoord.x);
    if FCoord.y > 0.0 then
      Result := Result + '+'
    else
    if FCoord.y < 0.0 then
      Result := Result + '-';
    if FCoord.y <> 0.0 then
      Result := Result + FormatExtended(Abs(FCoord.y)) + 'i';
  end
  else
    Result := FormatExtended(FCoord.y) + 'i';
end;

//------------------------------------------------------------------------------

function TJclComplex.GetPolarString: string;
begin
  FillCoords(crRectangular);
  Result := FormatExtended(FCoord.r) + '*CIS(' + FormatExtended(FCoord.theta) + ')';
end;

//------------------------------------------------------------------------------

procedure TJclComplex.SetRectangularString(StrToParse: string);
var
  SignPos: Integer;
  RealPart, ImagPart: Float;
begin
  StrToParse := StrRemoveChars(StrToParse, [' ']);
  SignPos := StrFind('+', StrToParse, 2);
  if SignPos = 0 then
    SignPos := StrFind('-', StrToParse, 2);
  if SignPos > 0 then
  begin
    try
      RealPart := StrToFloat(Copy(StrToParse, 1, SignPos - 1));
    except
      raise EJclMathError.CreateResRec(@RsComplexInvalidString);
    end;
    try
      ImagPart := StrToFloat(Copy(StrToParse, SignPos, Length(StrToParse) - SignPos));
    except
      raise EJclMathError.CreateResRec(@RsComplexInvalidString);
    end;
  end
  else
  begin
    if (StrRight(StrToParse, 1) = 'i') or (StrRight(StrToParse, 1) = 'I') then
    begin
      RealPart := 0.0;
      try
        ImagPart := StrToFloat(Copy(StrToParse, 1, Length(StrToParse) - 1));
      except
        raise EJclMathError.CreateResRec(@RsComplexInvalidString);
      end;
    end
    else
    begin
      try
        RealPart := StrToFloat(StrToParse);
      except
        raise EJclMathError.CreateResRec(@RsComplexInvalidString);
      end;
      ImagPart := 0.0;
    end;
  end;
  Assign(RealPart, ImagPart, crRectangular);
end;

//------------------------------------------------------------------------------

procedure TJclComplex.SetPolarString(StrToParse: string);
var
  AstPos: Integer;
  Radius, Angle: Float;
begin
  StrToParse := AnsiUpperCase(StrRemoveChars(StrToParse, [' ']));
  AstPos := Pos('*', StrToParse);
  if AstPos = 0 then
    raise EJclMathError.CreateResRec(@RsComplexInvalidString);
  try
    Radius := StrToFloat(StrLeft(StrToParse, AstPos - 1));
  except
    raise EJclMathError.CreateResRec(@RsComplexInvalidString);
  end;
  AstPos := Pos('(', StrToParse);
  if AstPos = 0 then
    raise EJclMathError.CreateResRec(@RsComplexInvalidString);
  try
    Angle := StrToFloat(Copy(StrToParse, AstPos + 1, Length(StrToParse) - AstPos - 1));
  except
    raise EJclMathError.CreateResRec(@RsComplexInvalidString);
  end;
  Assign(Radius, Angle, crPolar);
end;

//------------------------------------------------------------------------------

function TJclComplex.Duplicate: TJclComplex;
begin
  Result := TJclComplex.Create(FCoord.x, FCoord.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//==============================================================================
// arithmetics
//==============================================================================

function TJclComplex.CoreAdd(const First, Second: TRectCoord): TRectCoord;
begin
  Result.x := First.x + Second.x;
  Result.y := First.y + Second.y;
end;

//------------------------------------------------------------------------------

function TJclComplex.CAdd(const AddValue: TJclComplex): TJclComplex;
var
  ResCoord: TRectCoord;
begin
  ResCoord := CoreAdd(RectCoord(Self), RectCoord(AddValue));
  FCoord.x := ResCoord.x;
  FCoord.y := ResCoord.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CAdd(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
var
  NewComplex: TJclComplex;
begin
  NewComplex := TJclComplex.Create(X, Y, ComplexType);
  try
    Result := CAdd(NewComplex);
  finally
    NewComplex.Free;
  end;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewAdd(const AddValue: TJclComplex): TJclComplex;
var
  ResCoord: TRectCoord;
begin
  ResCoord := CoreAdd(RectCoord(Self), RectCoord(AddValue));
  Result := TJclComplex.Create(ResCoord.x, ResCoord.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewAdd(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
var
  NewComplex: TJclComplex;
begin
  NewComplex := TJclComplex.Create(X, Y, ComplexType);
  try
    Result := CNewAdd(NewComplex);
  finally
    NewComplex.Free;
  end;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreDiv(const First, Second: TRectCoord): TRectCoord;
var
  Denom: Float;
begin
  Denom := Sqr(Second.x) + Sqr(Second.y);
  Result.x := (First.x * Second.x + First.y * Second.y) / Denom;
  Result.y := (First.y * Second.x - First.x * Second.y) / Denom;
end;

//------------------------------------------------------------------------------

function TJclComplex.CDiv(const DivValue: TJclComplex): TJclComplex;
var
  ResCoord: TRectCoord;
begin
  ResCoord := CoreDiv(RectCoord(Self), RectCoord(DivValue));
  FCoord.x := ResCoord.x;
  FCoord.y := ResCoord.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CDiv(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
var
  NewComplex: TJclComplex;
begin
  NewComplex := TJclComplex.Create(X, Y, ComplexType);
  try
    Result := CDiv(NewComplex);
  finally
    NewComplex.Free;
  end;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewDiv(const DivValue: TJclComplex): TJclComplex;
var
  ResCoord: TRectCoord;
begin
  ResCoord := CoreDiv(RectCoord(Self), RectCoord(DivValue));
  Result := TJclComplex.Create(ResCoord.x, ResCoord.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewDiv(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
var
  NewComplex: TJclComplex;
begin
  NewComplex := TJclComplex.Create(X, Y, ComplexType);
  try
    Result := CNewDiv(NewComplex);
  finally
    NewComplex.Free;
  end;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreMul(const First, Second: TRectCoord): TRectCoord;
begin
  Result.x := First.x * Second.x - First.y * Second.y;
  Result.y := First.x * Second.y + First.y * Second.x;
end;

//------------------------------------------------------------------------------

function TJclComplex.CMul(const MulValue: TJclComplex): TJclComplex;
var
  ResCoord: TRectCoord;
begin
  ResCoord := CoreMul(RectCoord(Self), RectCoord(MulValue));
  FCoord.x := ResCoord.x;
  FCoord.y := ResCoord.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CMul(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
var
  NewComplex: TJclComplex;
begin
  NewComplex := TJclComplex.Create(X, Y, ComplexType);
  try
    Result := CMul(NewComplex);
  finally
    NewComplex.Free;
  end;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewMul(const MulValue: TJclComplex): TJclComplex;
var
  ResCoord: TRectCoord;
begin
  ResCoord := CoreMul(RectCoord(Self), RectCoord(MulValue));
  Result := TJclComplex.Create(ResCoord.x, ResCoord.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewMul(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
var
  NewComplex: TJclComplex;
begin
  NewComplex := TJclComplex.Create(X, Y, ComplexType);
  try
    Result := CNewMul(NewComplex);
  finally
    NewComplex.Free;
  end;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreSub(const First, Second: TRectCoord): TRectCoord;
begin
  Result.x := First.x - Second.x;
  Result.y := First.y - Second.y;
end;

//------------------------------------------------------------------------------

function TJclComplex.CSub(const SubValue: TJclComplex): TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreSub(RectCoord(Self), RectCoord(SubValue));
  FCoord.x := ResValue.x;
  FCoord.y := ResValue.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CSub(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
var
  NewComplex: TJclComplex;
begin
  NewComplex := TJclComplex.Create(X, Y, ComplexType);
  try
    Result := CSub(NewComplex);
  finally
    NewComplex.Free;
  end;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewSub(const SubValue: TJclComplex): TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreSub(RectCoord(Self), RectCoord(SubValue));
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewSub(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
var
  NewComplex: TJclComplex;
begin
  NewComplex := TJclComplex.Create(X, Y, ComplexType);
  try
    Result := CNewSub(NewComplex);
  finally
    NewComplex.Free;
  end;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNeg;
begin
  FCoord.x := -FCoord.x;
  FCoord.y := -FCoord.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewNeg;
begin
  Result := TJclComplex.Create(-FCoord.x, -FCoord.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CConjugate;
begin
  FCoord.y := -FCoord.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewConjugate;
begin
  Result := TJclComplex.Create(FCoord.x, -FCoord.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//==============================================================================
// natural log and exponential functions
//==============================================================================

function TJclComplex.CoreLn(const LnValue: TRectCoord): TRectCoord;
begin
  Result.x := Ln(LnValue.x);
  Result.y := NormalizeAngle(LnValue.y);
end;

//------------------------------------------------------------------------------

function TJclComplex.CLn: TJclComplex;
var
  ResCoord: TRectCoord;
begin
  FillCoords(crRectangular);
  ResCoord := CoreLn(RectCoord(FCoord.r, FCoord.theta));
  FCoord.x := ResCoord.x;
  FCoord.y := ResCoord.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewLn: TJclComplex;
var
  ResCoord: TRectCoord;
begin
  FillCoords(crRectangular);
  ResCoord := CoreLn(RectCoord(FCoord.r, FCoord.theta));
  Result := TJclComplex.Create(ResCoord.x, ResCoord.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreExp(const ExpValue: TRectCoord): TRectCoord;
var
  ExpX: Float;
begin
  ExpX := Exp(ExpValue.x);
  Result.x := ExpX * Cos(ExpValue.y);
  Result.y := ExpX * Sin(ExpValue.y);
end;

//------------------------------------------------------------------------------

function TJclComplex.CExp: TJclComplex;
var
  ResCoord: TRectCoord;
begin
  ResCoord := CoreExp(RectCoord(FCoord.x, FCoord.y));
  FCoord.x := ResCoord.x;
  FCoord.y := ResCoord.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewExp: TJclComplex;
var
  ResCoord: TRectCoord;
begin
  ResCoord := CoreExp(RectCoord(FCoord.x, FCoord.y));
  Result := TJclComplex.Create(ResCoord.x, ResCoord.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CorePwr(First, Second, Polar: TRectCoord): TRectCoord;
begin
  First.x := MiscalcSingle(First.x);
  First.y := MiscalcSingle(First.y);
  Second.x := MiscalcSingle(Second.x);
  Second.y := MiscalcSingle(Second.y);
  if AbsoluteValueSqr(First) = 0.0 then
    if AbsoluteValueSqr(Second) = 0.0 then
      Result := RectOne
    else
      Result := RectZero
  else
    Result := CoreExp(CoreMul(Second, CoreLn(Polar)));
end;

//------------------------------------------------------------------------------

function TJclComplex.CPwr(const PwrValue: TJclComplex): TJclComplex;
var
  ResValue: TRectCoord;
begin
  FillCoords(crRectangular);
  ResValue := CorePwr(RectCoord(Self), RectCoord(PwrValue), RectCoord(FCoord.r, FCoord.theta));
  FCoord.x := ResValue.x;
  FCoord.y := ResValue.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CPwr(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
var
  NewComplex: TJclComplex;
begin
  NewComplex := TJclComplex.Create(X, Y, ComplexType);
  try
    Result := CPwr(NewComplex);
  finally
    NewComplex.Free;
  end;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewPwr(PwrValue: TJclComplex): TJclComplex;
var
  ResValue: TRectCoord;
begin
  FillCoords(crRectangular);
  ResValue := CorePwr(RectCoord(Self), RectCoord(PwrValue), RectCoord(FCoord.r, FCoord.theta));
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewPwr(const X, Y: Float; const ComplexType: TComplexKind): TJclComplex;
var
  NewComplex: TJclComplex;
begin
  NewComplex := TJclComplex.Create(X, Y, ComplexType);
  try
    Result := CNewPwr(NewComplex);
  finally
    NewComplex.Free;
  end;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreIntPwr(First: TRectCoord; const Polar: TRectCoord; const Pwr: Integer): TRectCoord;
begin
  First.x := MiscalcSingle(First.x);
  First.y := MiscalcSingle(First.y);
  if AbsoluteValueSqr(First) = 0.0 then
    if Pwr = 0 then
      Result := RectOne
    else
      Result := RectZero
  else
    Result := RectCoord(PowerInt(Polar.x, Pwr), NormalizeAngle(Pwr * Polar.y));
end;

//------------------------------------------------------------------------------

function TJclComplex.CIntPwr(const Pwr: Integer): TJclComplex;
var
  ResValue: TRectCoord;
begin
  FillCoords(crRectangular);
  ResValue := CoreIntPwr(RectCoord(Self), RectCoord(FCoord.r, FCoord.theta), Pwr);
  FCoord.r := ResValue.x;
  FCoord.theta := ResValue.y;
  FillCoords(crPolar);
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewIntPwr(const Pwr: Integer): TJclComplex;
var
  ResValue: TRectCoord;
begin
  FillCoords(crRectangular);
  ResValue := CoreIntPwr(RectCoord(Self), RectCoord(FCoord.r, FCoord.theta), Pwr);
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crPolar);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreRealPwr(First: TRectCoord; const Polar: TRectCoord; const Pwr: Float): TRectCoord;
begin
  First.x := MiscalcSingle(First.x);
  First.y := MiscalcSingle(First.y);
  if AbsoluteValueSqr(First) = 0.0 then
    if MiscalcSingle(Pwr) = 0.0 then
      Result := RectOne
    else
      Result := RectZero
  else
    Result := RectCoord(Power(Polar.x, Pwr), NormalizeAngle(Pwr * Polar.y));
end;

//------------------------------------------------------------------------------

function TJclComplex.CRealPwr(const Pwr: Float): TJclComplex;
var
  ResValue: TRectCoord;
begin
  FillCoords(crRectangular);
  ResValue := CoreRealPwr(RectCoord(Self), RectCoord(FCoord.r, FCoord.theta), Pwr);
  FCoord.r := ResValue.x;
  FCoord.theta := ResValue.y;
  FillCoords(crPolar);
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewRealPwr(const Pwr: Float): TJclComplex;
var
  ResValue: TRectCoord;
begin
  FillCoords(crRectangular);
  ResValue := CoreRealPwr(RectCoord(Self), RectCoord(FCoord.r, FCoord.theta), Pwr);
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crPolar);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreRoot(First: TRectCoord; const Polar: TRectCoord; const K, N: Word): TRectCoord;
begin
  First.x := MiscalcSingle(First.x);
  First.y := MiscalcSingle(First.y);
  if AbsoluteValue(First) = 0.0 then
    Result := RectZero
  else
    Result := RectCoord(Power(Polar.x, 1.0 / N), NormalizeAngle((Polar.y + K * TwoPi) / N));
end;

//------------------------------------------------------------------------------

function TJclComplex.CRoot(const K, N: Word): TJclComplex;
var
  ResValue: TRectCoord;
begin
  FillCoords(crRectangular);
  ResValue := CoreRoot(RectCoord(Self), RectCoord(FCoord.r, FCoord.theta), K, N);
  FCoord.r := ResValue.x;
  FCoord.theta := ResValue.y;
  FillCoords(crPolar);
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewRoot(const K, N: Word): TJclComplex;
var
  ResValue: TRectCoord;
begin
  FillCoords(crRectangular);
  ResValue := CoreRoot(RectCoord(Self), RectCoord(FCoord.r, FCoord.theta), K, N);
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crPolar);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CSqrt: TJclComplex;
begin
  Result := CRoot(0, 2);
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewSqrt: TJclComplex;
begin
  Result := CNewRoot(0, 2);
end;

//------------------------------------------------------------------------------
// trigonometric functions
//------------------------------------------------------------------------------

function TJclComplex.CoreCos(const Value: TRectCoord): TRectCoord;
begin
  Result := RectCoord(Cos(FCoord.x) * CosH(FCoord.y), -Sin(FCoord.x) * SinH(FCoord.y));
end;

//------------------------------------------------------------------------------

function TJclComplex.CCos: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCos(RectCoord(Self));
  FCoord.x := ResValue.x;
  FCoord.y := ResValue.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewCos: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCos(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreSin(const Value: TRectCoord): TRectCoord;
begin
  Result := RectCoord(Sin(FCoord.x) * CosH(FCoord.y), Cos(FCoord.x) * SinH(FCoord.y));
end;

//------------------------------------------------------------------------------

function TJclComplex.CSin: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreSin(RectCoord(Self));
  FCoord.x := ResValue.x;
  FCoord.y := ResValue.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewSin: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreSin(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreTan(const Value: TRectCoord): TRectCoord;
var
  TempValue: Float;
begin
  TempValue := Cos(2.0 * Value.x) + CosH(2.0 * Value.y);
  if MiscalcSingle(TempValue) <> 0.0 then
    Result := RectCoord(Sin(2.0 * Value.x) / TempValue, SinH(2.0 * Value.y) / TempValue)
  else
    Result := RectInfinity;
end;

//------------------------------------------------------------------------------

function TJclComplex.CTan: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreTan(RectCoord(Self));
  FCoord.x := ResValue.x;
  FCoord.y := ResValue.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewTan: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreTan(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreCot(const Value: TRectCoord): TRectCoord;
var
  TempValue: Float;
begin
  TempValue := Cosh(2.0 * Value.y) - Cos(2.0 * Value.x);
  if MiscalcSingle(TempValue) <> 0.0 then
    Result := RectCoord(Sin(2.0 * Value.x) / TempValue, -SinH(2.0 * Value.y) / TempValue)
  else
    Result := RectInfinity;
end;

//------------------------------------------------------------------------------

function TJclComplex.CCot: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCot(RectCoord(Self));
  FCoord.x := ResValue.x;
  FCoord.y := ResValue.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewCot: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCot(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreSec(const Value: TRectCoord): TRectCoord;
var
  TempValue: TRectCoord;
begin
  TempValue := CoreCos(Value);
  if MiscalcSingle(AbsoluteValue(TempValue)) <> 0.0 then
    Result := CoreDiv(RectOne, TempValue)
  else
    Result := RectInfinity;
end;

//------------------------------------------------------------------------------

function TJclComplex.CSec: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreSec(RectCoord(Self));
  FCoord.x := ResValue.x;
  FCoord.y := ResValue.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewSec: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreSec(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreCsc(const Value: TRectCoord): TRectCoord;
var
  TempValue: TRectCoord;
begin
  TempValue := CoreSin(Value);
  if MiscalcSingle(AbsoluteValue(TempValue)) <> 0.0 then
    Result := CoreDiv(RectOne, TempValue)
  else
    Result := RectInfinity;
end;

//------------------------------------------------------------------------------

function TJclComplex.CCsc: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCsc(RectCoord(Self));
  FCoord.x := ResValue.x;
  FCoord.y := ResValue.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewCsc: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCsc(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//==============================================================================
// hyperbolic functions
//==============================================================================

function TJclComplex.CoreCosH(const Value: TRectCoord): TRectCoord;
begin
  Result := RectCoord(CosH(FCoord.x) * Cos(FCoord.y), SinH(FCoord.x) * Sin(FCoord.y));
end;

//------------------------------------------------------------------------------

function TJclComplex.CCosH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCosH(RectCoord(Self));
  FCoord.x := ResValue.x;
  FCoord.y := ResValue.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewCosH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCosH(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreSinH(const Value: TRectCoord): TRectCoord;
begin
  Result := RectCoord(SinH(FCoord.x) * Cos(FCoord.y), CosH(FCoord.x) * Sin(FCoord.y));
end;

//------------------------------------------------------------------------------

function TJclComplex.CSinH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreSinH(RectCoord(Self));
  FCoord.x := ResValue.x;
  FCoord.y := ResValue.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewSinH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreSinH(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreTanH(const Value: TRectCoord): TRectCoord;
var
  TempValue: Float;
begin
  TempValue := CosH(2.0 * Value.x) + Cos(2.0 * Value.y);
  if MiscalcSingle(TempValue) <> 0.0 then
    Result := RectCoord(SinH(2.0 * Value.x) / TempValue, Sin(2.0 * Value.y) / TempValue)
  else
    Result := RectInfinity;
end;

//------------------------------------------------------------------------------

function TJclComplex.CTanH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreTanH(RectCoord(Self));
  FCoord.x := ResValue.x;
  FCoord.y := ResValue.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewTanH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreTanH(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreCotH(const Value: TRectCoord): TRectCoord;
var
  TempValue: Float;
begin
  TempValue := Cosh(2.0 * Value.x) - Cos(2.0 * Value.y);
  if MiscalcSingle(TempValue) <> 0.0 then
    Result := RectCoord(SinH(2.0 * Value.x) / TempValue, -Sin(2.0 * Value.y) / TempValue)
  else
    Result := RectInfinity;
end;

//------------------------------------------------------------------------------

function TJclComplex.CCotH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCotH(RectCoord(Self));
  FCoord.x := ResValue.x;
  FCoord.y := ResValue.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewCotH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCotH(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreSecH(const Value: TRectCoord): TRectCoord;
var
  TempValue: TRectCoord;
begin
  TempValue := CoreCosH(Value);
  if MiscalcSingle(AbsoluteValue(TempValue)) <> 0.0 then
    Result := CoreDiv(RectOne, TempValue)
  else
    Result := RectInfinity;
end;

//------------------------------------------------------------------------------

function TJclComplex.CSecH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreSecH(RectCoord(Self));
  FCoord.x := ResValue.x;
  FCoord.y := ResValue.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewSecH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreSecH(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreCscH(const Value: TRectCoord): TRectCoord;
var
  TempValue: TRectCoord;
begin
  TempValue := CoreSinH(Value);
  if MiscalcSingle(AbsoluteValue(TempValue)) <> 0.0 then
    Result := CoreDiv(RectOne, TempValue)
  else
    Result := RectInfinity;
end;

//------------------------------------------------------------------------------

function TJclComplex.CCscH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCscH(RectCoord(Self));
  FCoord.x := ResValue.x;
  FCoord.y := ResValue.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewCscH: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreCscH(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//==============================================================================
// complex Bessel functions of order zero
//==============================================================================

function TJclComplex.CoreI0(const Value: TRectCoord): TRectCoord;
var
  zSQR25, term: TRectCoord;
  i: Integer;
  SizeSqr: Float;
begin
  Result := RectOne;
  zSQR25 := CoreMul(Value, Value);
  zSQR25 := RectCoord(0.25 * zSQR25.x, 0.25 * zSQR25.y);
  term := zSQR25;
  Result := CoreAdd(Result, zSQR25);
  i := 1;
  repeat
    term := CoreMul(zSQR25, term);
    Inc(i);
    term := RectCoord(term.x / Sqr(i), term.y / Sqr(i));
    Result := CoreAdd(Result, term);
    SizeSqr := Sqr(term.x) + Sqr(term.y);
  until (i > MaxTerm) or (SizeSqr < EpsilonSqr)
end;

//------------------------------------------------------------------------------

function TJclComplex.CI0: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreI0(RectCoord(Self));
  FCoord.x := ResValue.x;
  FCoord.y := ResValue.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewI0: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreI0(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreJ0(const Value: TRectCoord): TRectCoord;
var
  zSQR25, term: TRectCoord;
  i: Integer;
  SizeSqr: Float;
  addFlag: Boolean;
begin
  Result := RectOne;
  zSQR25 := CoreMul(Value, Value);
  zSQR25 := RectCoord(0.25 * zSQR25.x, 0.25 * zSQR25.y);
  term := zSQR25;
  Result := CoreSub(Result, zSQR25);
  addFlag := False;
  i := 1;
  repeat
    term := CoreMul(zSQR25, term);
    Inc(i);
    addFlag := not addFlag;
    term := RectCoord(term.x / Sqr(i), term.y / Sqr(i));
    if addFlag then
      Result := CoreAdd(Result, term)
    else
      Result := CoreSub(Result, term);
    SizeSqr := Sqr(term.x) + Sqr(term.y);
  until (i > MaxTerm) or (SizeSqr < EpsilonSqr)
end;

//------------------------------------------------------------------------------

function TJclComplex.CJ0: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreJ0(RectCoord(Self));
  FCoord.x := ResValue.x;
  FCoord.y := ResValue.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewJ0: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreJ0(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreApproxLnGamma(const Value: TRectCoord): TRectCoord;
const
  c: array [1..8] of Float =
  (1.0 / 12.0, -1.0 / 360.0, 1.0 / 1260.0, -1.0 / 1680.0,
   1.0 / 1188.0, -691.0 / 360360.0, 1.0 / 156.0, -3617.0 / 122400.0);
var
  i: Integer;
  Powers: array [1..8] of TRectCoord;
  temp1, temp2: TRectCoord;
begin
  temp1 := CoreLn(Value);
  temp2 := RectCoord(Value.x - 0.5, Value.y);
  Result := CoreAdd(temp1, temp2);
  Result := CoreSub(Result, Value);
  Result.x := Result.x + hLn2PI;

  temp1 := RectOne;
  Powers[1] := CoreDiv(temp1, Value);
  temp2 := CoreMul(powers[1], powers[1]);
  for i := 2 to 8 do
    Powers[i] := CoreMul(powers[i - 1], temp2);
  for i := 8 downto 1 do
  begin
    temp1 := RectCoord(c[i] * Powers[i].x, c[i] * Powers[i].y);
    Result := CoreAdd(Result, temp1);
  end;
end;

//------------------------------------------------------------------------------

function TJclComplex.CApproxLnGamma: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreApproxLnGamma(RectCoord(Self));
  FCoord.x := ResValue.x;
  FCoord.y := ResValue.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewApproxLnGamma: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreApproxLnGamma(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreLnGamma(Value: TRectCoord): TRectCoord;
var
  lna, temp: TRectCoord;
begin
  if (Value.x <= 0.0) and (MiscalcSingle(Value.y) = 0.0) then
    if MiscalcSingle(Int(Value.x - 1E-8) - Value.x) = 0.0 then
    begin
      Result := RectInfinity;
      Exit;
    end;

  if Value.y < 0.0 then
  begin
    Value := RectCoord(Value.x, -Value.y);
    Result := CoreLnGamma(Value);
    Result := RectCoord(Result.x, -Result.y);
  end
  else
  begin
    if Value.x < 9.0 then
    begin
      lna := CoreLn(Value);
      Value := RectCoord(Value.x + 1, Value.y);
      temp := CoreLnGamma(Value);
      Result := CoreSub(temp, lna);
    end
    else
      CoreApproxLnGamma(Value);
  end;
end;

//------------------------------------------------------------------------------

function TJclComplex.CLnGamma: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreLnGamma(RectCoord(Self));
  FCoord.x := ResValue.x;
  FCoord.y := ResValue.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewLnGamma: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreLnGamma(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//------------------------------------------------------------------------------

function TJclComplex.CoreGamma(const Value: TRectCoord): TRectCoord;
var
  lnz: TRectCoord;
begin
  lnz := CoreLnGamma(Value);
  if lnz.x > 75.0 then
    Result := RectInfinity
  else
    if lnz.x < -200.0 then
      Result := RectZero
    else
      Result := CoreExp(lnz);
end;

//------------------------------------------------------------------------------

function TJclComplex.CGamma: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreGamma(RectCoord(Self));
  FCoord.x := ResValue.x;
  FCoord.y := ResValue.y;
  Result := Self;
end;

//------------------------------------------------------------------------------

function TJclComplex.CNewGamma: TJclComplex;
var
  ResValue: TRectCoord;
begin
  ResValue := CoreGamma(RectCoord(Self));
  Result := TJclComplex.Create(ResValue.x, ResValue.y, crRectangular);
  Result.FFracLen := FFracLen;
end;

//==============================================================================
// miscellaneous
//==============================================================================

function TJclComplex.AbsoluteValue: Float;
begin
  Result := Sqrt(Sqr(FCoord.x) + Sqr(FCoord.y));
end;

//------------------------------------------------------------------------------

function TJclComplex.AbsoluteValue(const Coord: TRectCoord): Float;
begin
  Result := Sqrt(Sqr(Coord.x) + Sqr(Coord.y));
end;

//------------------------------------------------------------------------------

function TJclComplex.AbsoluteValueSqr: Float;
begin
  Result := Sqr(FCoord.x) + Sqr(FCoord.y);
end;

//------------------------------------------------------------------------------

function TJclComplex.AbsoluteValueSqr(const Coord: TRectCoord): Float;
begin
  Result := Sqr(Coord.x) + Sqr(Coord.y);
end;

//------------------------------------------------------------------------------

function TJclComplex.FormatExtended(const X: Float): string;
begin
  Result := FloatToStrF(X, ffFixed, FFracLen, FFracLen);
end;

//------------------------------------------------------------------------------

procedure TJclComplex.SetFracLen(const X: Byte);
begin
  if X > MaxFracLen then
    FFracLen := MaxFracLen
  else
    FFracLen := X;
end;

//------------------------------------------------------------------------------

function TJclComplex.GetRadius: Float;
begin
  FillCoords(crRectangular);
  Result := FCoord.r;
end;

//------------------------------------------------------------------------------

function TJclComplex.GetAngle: Float;
begin
  FillCoords(crRectangular);
  Result := FCoord.theta;
end;

//------------------------------------------------------------------------------

function TJclComplex.NormalizeAngle(Value: Float): Float;
begin
  FillCoords(crRectangular);
  while Value > Pi do
    Value := Value - TwoPi;
  while Value < -Pi do
    Value := Value + TwoPi;
  Value := MiscalcSingle(Value);
  Result := Value;
end;

end.

