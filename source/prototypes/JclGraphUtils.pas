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
{ The Original Code is JclGraphUtils.pas.                                      }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: May 17, 2001                                                  }
{                                                                              }
{******************************************************************************}
//
// 28-MAR-2001 ml:
//   - ShortenString included

unit JclGraphUtils;

interface

{$I JCL.INC}

uses
  {$IFDEF WIN32}
  Windows, Graphics,
  {$ENDIF WIN32}
  JclBase;

type
  PColor32 = ^TColor32;
  TColor32 = type LongWord;
  PColor32Array = ^TColor32Array;
  TColor32Array = array [0..0] of TColor32;
  PPalette32 = ^TPalette32;
  TPalette32 = array [Byte] of TColor32;
  TArrayOfColor32 = array of TColor32;

  { Blending Function Prototypes }
  TCombineReg  = function (X, Y, W: TColor32): TColor32;
  TCombineMem  = procedure (F: TColor32; var B: TColor32; W: TColor32);
  TBlendReg    = function (F, B: TColor32): TColor32;
  TBlendMem    = procedure (F: TColor32; var B: TColor32);
  TBlendRegEx  = function (F, B, M: TColor32): TColor32;
  TBlendMemEx  = procedure (F: TColor32; var B: TColor32; M: TColor32);
  TBlendLine   = procedure (Src, Dst: PColor32; Count: Integer);
  TBlendLineEx = procedure (Src, Dst: PColor32; Count: Integer; M: TColor32);

  { Auxiliary structure to support TColor manipulation}
  TColorRec = packed record
    case Integer of
      0: (Value: Longint);
      1: (Red, Green, Blue: Byte);
      2: (R, G, B, Flag: Byte);
      {$IFDEF WIN32}
      3: (Index: Word); // GetSysColor, PaletteIndex
      {$ENDIF WIN32}
  end;

const
  { Some predefined color constants }
  clBlack32:     TColor32 = $FF000000;
  clDimGray32:   TColor32 = $FF3F3F3F;
  clGray32:      TColor32 = $FF7F7F7F;
  clLightGray32: TColor32 = $FFBFBFBF;
  clWhite32:     TColor32 = $FFFFFFFF;
  clMaroon32:    TColor32 = $FF7F0000;
  clGreen32:     TColor32 = $FF007F00;
  clOlive32:     TColor32 = $FF7F7F00;
  clNavy32:      TColor32 = $FF00007F;
  clPurple32:    TColor32 = $FF7F007F;
  clTeal32:      TColor32 = $FF007F7F;
  clRed32:       TColor32 = $FFFF0000;
  clLime32:      TColor32 = $FF00FF00;
  clYellow32:    TColor32 = $FFFFFF00;
  clBlue32:      TColor32 = $FF0000FF;
  clFuchsia32:   TColor32 = $FFFF00FF;
  clAqua32:      TColor32 = $FF00FFFF;

  { Some semi-transparent color constants }
  clTrWhite32:   TColor32 = $7FFFFFFF;
  clTrBlack32:   TColor32 = $7F000000;
  clTrRed32:     TColor32 = $7FFF0000;
  clTrGreen32:   TColor32 = $7F00FF00;
  clTrBlue32:    TColor32 = $7F0000FF;

procedure EMMS;

//------------------------------------------------------------------------------
// Dialog Functions
//------------------------------------------------------------------------------

{$IFDEF WIN32}

function DialogUnitsToPixelsX(DialogUnits: Word): Word;
function DialogUnitsToPixelsY(DialogUnits: Word): Word;
function PixelsToDialogUnitsX(PixelUnits: Word): Word;
function PixelsToDialogUnitsY(PixelUnits: Word): Word;

{$ENDIF WIN32}

//------------------------------------------------------------------------------
// Points
//------------------------------------------------------------------------------

function NullPoint: TPoint;

function PointAssign(X, Y: Integer): TPoint;
procedure PointCopy(var Dest: TPoint; const Source: TPoint);
function PointEqual(const P1, P2: TPoint): Boolean;
function PointIsNull(const P: TPoint): Boolean;
procedure PointMove(var P: TPoint; const DeltaX, DeltaY: Integer);

//------------------------------------------------------------------------------
// Rectangles
//------------------------------------------------------------------------------

function NullRect: TRect;

function RectAssign(Left, Top, Right, Bottom: Integer): TRect;
function RectAssignPoints(const TopLeft, BottomRight: TPoint): TRect;
function RectBounds(Left, Top, Width, Height: Integer): TRect;
function RectCenter(const R: TRect): TPoint;
procedure RectCopy(var Dest: TRect; const Source: TRect);
procedure RectFitToScreen(var R: TRect); // TODO DOC
procedure RectGrow(var R: TRect; Delta: Integer);
procedure RectGrowX(var R: TRect; Delta: Integer);
procedure RectGrowY(var R: TRect; Delta: Integer);
function RectEqual(const R1, R2: TRect): Boolean;
function RectHeight(const R: TRect): Integer;
function RectIncludesPoint(const R: TRect; const Pt: TPoint): Boolean;
function RectIncludesRect(const R1, R2: TRect): Boolean;
function RectIntersection(const R1, R2: TRect): TRect;
function RectIntersectRect(const R1, R2: TRect): Boolean;
function RectIsEmpty(const R: TRect): Boolean;
function RectIsNull(const R: TRect): Boolean;
function RectIsSquare(const R: TRect): Boolean;
function RectIsValid(const R: TRect): Boolean;
procedure RectMove(var R: TRect; DeltaX, DeltaY: Integer);
procedure RectMoveTo(var R: TRect; X, Y: Integer);
procedure RectNormalize(var R: TRect);
function RectsAreValid(R: array of TRect): Boolean;
function RectUnion(const R1, R2: TRect): TRect;
function RectWidth(const R: TRect): Integer;

//------------------------------------------------------------------------------
// Color
//------------------------------------------------------------------------------

procedure GetRGBValue(const Color: TColor; out Red, Green, Blue: Byte);
function SetRGBValue(Red, Green, Blue: Byte): TColor;
function GetColorBlue(const Color: TColor): Byte;
function GetColorFlag(const Color: TColor): Byte;
function GetColorGreen(const Color: TColor): Byte;
function GetColorRed(const Color: TColor): Byte;
function SetColorBlue(const Color: TColor; Blue: Byte): TColor;
function SetColorFlag(const Color: TColor; Flag: Byte): TColor;
function SetColorGreen(const Color: TColor; Green: Byte): TColor;
function SetColorRed(const Color: TColor; Red: Byte): TColor;

function BrightColor(Color: TColor; Pct: Extended): TColor;
function BrightColorChannel(Channel: Byte; Pct: Extended): Byte;
function DarkColor(Color: TColor; Pct: Extended): TColor;
function DarkColorChannel(Channel: Byte; Pct: Extended): Byte;

procedure CIED65ToCIED50(var X, Y, Z: Extended);
procedure CMYKToBGR(Source, Target: Pointer; BitsPerSample: Byte; Count: Cardinal); overload;
procedure CMYKToBGR(C, M, Y, K, Target: Pointer; BitsPerSample: Byte; Count: Cardinal); overload;
procedure CIELABToBGR(Source, Target: Pointer; Count: Cardinal); overload;
procedure CIELABToBGR(LSource, aSource, bSource: PByte; Target: Pointer; Count: Cardinal); overload;
procedure RGBToBGR(Source, Target: Pointer; BitsPerSample: Byte; Count: Cardinal); overload;
procedure RGBToBGR(R, G, B, Target: Pointer; BitsPerSample: Byte; Count: Cardinal); overload;
procedure RGBAToBGRA(Source, Target: Pointer; BitsPerSample: Byte; Count: Cardinal);

procedure WinColorToOpenGLColor(Color: TColor; out Red, Green, Blue: Float);
function OpenGLColorToWinColor(Red, Green, Blue: Float): TColor;

function Color32(WinColor: TColor): TColor32; overload;
function Color32(R, G, B: Byte; A: Byte {$IFDEF SUPPORTS_DEFAULTPARAMS} = $FF {$ENDIF}): TColor32; overload;
function Color32(Index: Byte; const Palette: TPalette32): TColor32; overload;
function Gray32(Intensity: Byte; Alpha: Byte {$IFDEF SUPPORTS_DEFAULTPARAMS} = $FF {$ENDIF}): TColor32;
function WinColor(Color32: TColor32): TColor;

function RedComponent(Color32: TColor32): Integer;
function GreenComponent(Color32: TColor32): Integer;
function BlueComponent(Color32: TColor32): Integer;
function AlphaComponent(Color32: TColor32): Integer;
function Intensity(Color32: TColor32): Integer;

function SetAlpha(Color32: TColor32; NewAlpha: Integer): TColor32;

function HSLToRGB(H, S, L: Single): TColor32;
procedure RGBToHSL(RGB: TColor32; out H, S, L: Single);

//------------------------------------------------------------------------------
// Misc
//------------------------------------------------------------------------------

{$IFDEF WIN32}
// Petr Vones
function DottedLineTo(Canvas: TCanvas; X, Y: Integer): Boolean;
{$ENDIF WIN32}
function ShortenString(DC: HDC; const S: WideString; Width: Integer; TriplePointWidth: Integer = 0): WideString;

var
 { Blending Function Variables }
  CombineReg: TCombineReg;
  CombineMem: TCombineMem;

  BlendReg: TBlendReg;
  BlendMem: TBlendMem;

  BlendRegEx: TBlendRegEx;
  BlendMemEx: TBlendMemEx;

  BlendLine: TBlendLine;
  BlendLineEx: TBlendLineEx;

implementation

uses
  Math,
  JclLogic, JclSysInfo;

type
  // resampling support types
  TRGBInt = record
    R: Integer;
    G: Integer;
    B: Integer;
  end;

  PRGBWord = ^TRGBWord;
  TRGBWord = record
    R: Word;
    G: Word;
    B: Word;
  end;

  PRGBAWord = ^TRGBAWord;
  TRGBAWord = record
    R: Word;
    G: Word;
    B: Word;
    A: Word;
  end;

  PBGR = ^TBGR;
  TBGR = packed record
    B: Byte;
    G: Byte;
    R: Byte;
  end;

  PBGRA = ^TBGRA;
  TBGRA = packed record
    B: Byte;
    G: Byte;
    R: Byte;
    A: Byte;
  end;

  PRGB = ^TRGB;
  TRGB = packed record
    R: Byte;
    G: Byte;
    B: Byte;
  end;

  PRGBA = ^TRGBA;
  TRGBA = packed record
    R: Byte;
    G: Byte;
    B: Byte;
    A: Byte;
  end;

const
  { Component masks }
  _R:   TColor32 = $00FF0000;
  _G:   TColor32 = $0000FF00;
  _B:   TColor32 = $000000FF;
  _RGB: TColor32 = $00FFFFFF;
  Bias = $00800080;

var
  MMX_ACTIVE: Boolean;

//==============================================================================
// Internal LowLevel
//==============================================================================

function ColorSwap(WinColor: TColor): TColor32; assembler;
asm
// EAX = WinColor
        MOV     ECX, EAX        // this function swaps R and B bytes in ABGR
        SHR     EAX, 16
        XCHG    AL, CL
        MOV     AH, $FF         // and writes $FF into A component
        SHL     EAX, 16
        MOV     AX,  CX
end;

//==============================================================================
// Blending routines
//==============================================================================

function _CombineReg(X, Y, W: TColor32): TColor32; assembler;
asm
  // combine RGBA channels of colors X and Y with the weight of X given in W
  // Result Z = W * X + (1 - W) * Y (all channels are combined, including alpha)
  // EAX <- X
  // EDX <- Y
  // ECX <- W

  // W = 0 or $FF?
        JCXZ    @1              // CX = 0 ?  => Result := EDX
        CMP     ECX, $FF        // CX = $FF ?  => Result := EDX
        JE      @2

        PUSH    EBX

  // P = W * X
        MOV     EBX, EAX        // EBX  <-  Xa Xr Xg Xb
        AND     EAX, $00FF00FF  // EAX  <-  00 Xr 00 Xb
        AND     EBX, $FF00FF00  // EBX  <-  Xa 00 Xg 00
        IMUL    EAX, ECX        // EAX  <-  Pr ** Pb **
        SHR     EBX, 8          // EBX  <-  00 Xa 00 Xg
        IMUL    EBX, ECX        // EBX  <-  Pa ** Pg **
        ADD     EAX, bias
        AND     EAX, $FF00FF00  // EAX  <-  Pa 00 Pg 00
        SHR     EAX, 8          // EAX  <-  00 Pr 00 Pb
        ADD     EBX, bias
        AND     EBX, $FF00FF00  // EBX  <-  Pa 00 Pg 00
        OR      EAX, EBX        // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W; Q = W * Y
        XOR     ECX, $000000FF  // ECX  <-  1 - ECX
        MOV     EBX, EDX        // EBX  <-  Ya Yr Yg Yb
        AND     EDX, $00FF00FF  // EDX  <-  00 Yr 00 Yb
        AND     EBX, $FF00FF00  // EBX  <-  Ya 00 Yg 00
        IMUL    EDX, ECX        // EDX  <-  Qr ** Qb **
        SHR     EBX, 8          // EBX  <-  00 Ya 00 Yg
        IMUL    EBX, ECX        // EBX  <-  Qa ** Qg **
        ADD     EDX, bias
        AND     EDX, $FF00FF00  // EDX  <-  Qr 00 Qb 00
        SHR     EDX, 8          // EDX  <-  00 Qr ** Qb
        ADD     EBX, bias
        AND     EBX, $FF00FF00  // EBX  <-  Qa 00 Qg 00
        OR      EBX, EDX        // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX, EBX        // EAX  <-  Za Zr Zg Zb

        POP     EBX
        RET

@1:     MOV     EAX, EDX
@2:     RET
end;

//------------------------------------------------------------------------------

procedure _CombineMem(F: TColor32; var B: TColor32; W: TColor32); assembler;
asm
  // EAX <- F
  // [EDX] <- B
  // ECX <- W

  // Check W
        JCXZ    @1              // W = 0 ?  => write nothing
        CMP     ECX, $FF        // W = 255? => write F
        JZ      @2

        PUSH    EBX
        PUSH    ESI

  // P = W * F
        MOV     EBX, EAX        // EBX  <-  ** Fr Fg Fb
        AND     EAX, $00FF00FF  // EAX  <-  00 Fr 00 Fb
        AND     EBX, $0000FF00  // EBX  <-  00 00 Fg 00
        IMUL    EAX, ECX        // EAX  <-  Pr ** Pb **
        SHR     EBX, 8          // EBX  <-  00 00 00 Fg
        IMUL    EBX, ECX        // EBX  <-  00 00 Pg **
        ADD     EAX, bias
        AND     EAX, $FF00FF00  // EAX  <-  Pr 00 Pb 00
        SHR     EAX, 8          // EAX  <-  00 Pr 00 Pb
        ADD     EBX, bias
        AND     EBX, $0000FF00  // EBX  <-  00 00 Pg 00
        OR      EAX, EBX        // EAX  <-  00 Pr Pg Pb

  // W = 1 - W; Q = W * B
        MOV     ESI, [EDX]
        XOR     ECX, $000000FF  // ECX  <-  1 - ECX
        MOV     EBX, ESI        // EBX  <-  00 Br Bg Bb
        AND     ESI, $00FF00FF  // ESI  <-  00 Br 00 Bb
        AND     EBX, $0000FF00  // EBX  <-  00 00 Bg 00
        IMUL    ESI, ECX        // ESI  <-  Qr ** Qb **
        SHR     EBX, 8          // EBX  <-  00 00 00 Bg
        IMUL    EBX, ECX        // EBX  <-  00 00 Qg **
        ADD     ESI, bias
        AND     ESI, $FF00FF00  // ESI  <-  Qr 00 Qb 00
        SHR     ESI, 8          // ESI  <-  00 Qr ** Qb
        ADD     EBX, bias
        AND     EBX, $0000FF00  // EBX  <-  00 00 Qg 00
        OR      EBX, ESI        // EBX  <-  00 Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX, EBX        // EAX  <-  00 Zr Zg Zb

        MOV     [EDX], EAX

        POP     ESI
        POP     EBX
@1:     RET

@2:     MOV     [EDX], EAX
        RET
end;

//------------------------------------------------------------------------------

function _BlendReg(F, B: TColor32): TColor32; assembler;
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
  // Result Z = Fa * Frgb + (1 - Fa) * Brgb
  // EAX <- F
  // EDX <- B

  // Test Fa = 255 ?
        CMP     EAX, $FF000000  // Fa = 255 ? => Result = EAX
        JNC     @2

  // Test Fa = 0 ?
        TEST    EAX, $FF000000  // Fa = 0 ?   => Result = EDX
        JZ      @1

  // Get weight W = Fa * M
        MOV     ECX, EAX        // ECX  <-  Fa Fr Fg Fb
        SHR     ECX, 24         // ECX  <-  00 00 00 Fa

        PUSH    EBX

  // P = W * F
        MOV     EBX, EAX        // EBX  <-  Fa Fr Fg Fb
        AND     EAX, $00FF00FF  // EAX  <-  00 Fr 00 Fb
        AND     EBX, $FF00FF00  // EBX  <-  Fa 00 Fg 00
        IMUL    EAX, ECX        // EAX  <-  Pr ** Pb **
        SHR     EBX, 8          // EBX  <-  00 Fa 00 Fg
        IMUL    EBX, ECX        // EBX  <-  Pa ** Pg **
        ADD     EAX, bias
        AND     EAX, $FF00FF00  // EAX  <-  Pr 00 Pb 00
        SHR     EAX, 8          // EAX  <-  00 Pr ** Pb
        ADD     EBX, bias
        AND     EBX, $FF00FF00  // EBX  <-  Pa 00 Pg 00
        OR      EAX, EBX        // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W; Q = W * B
        XOR     ECX, $000000FF  // ECX  <-  1 - ECX
        MOV     EBX, EDX        // EBX  <-  Ba Br Bg Bb
        AND     EDX, $00FF00FF  // EDX  <-  00 Br 00 Bb
        AND     EBX, $FF00FF00  // EBX  <-  Ba 00 Bg 00
        IMUL    EDX, ECX        // EDX  <-  Qr ** Qb **
        SHR     EBX, 8          // EBX  <-  00 Ba 00 Bg
        IMUL    EBX, ECX        // EBX  <-  Qa ** Qg **
        ADD     EDX, bias
        AND     EDX, $FF00FF00  // EDX  <-  Qr 00 Qb 00
        SHR     EDX, 8          // EDX  <-  00 Qr ** Qb
        ADD     EBX, bias
        AND     EBX, $FF00FF00  // EBX  <-  Qa 00 Qg 00
        OR      EBX, EDX        // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX, EBX        // EAX  <-  Za Zr Zg Zb

        POP     EBX
        RET

@1:     MOV     EAX, EDX
@2:     RET
end;

//------------------------------------------------------------------------------

procedure _BlendMem(F: TColor32; var B: TColor32); assembler;
asm
  // EAX <- F
  // [EDX] <- B

  // Test Fa = 0 ?
        TEST    EAX, $FF000000  // Fa = 0 ?   => do not write
        JZ      @2

  // Get weight W = Fa * M
        MOV     ECX, EAX        // ECX  <-  Fa Fr Fg Fb
        SHR     ECX, 24         // ECX  <-  00 00 00 Fa

  // Test Fa = 255 ?
        CMP     ECX, $FF
        JZ      @1

        PUSH    EBX
        PUSH    ESI

  // P = W * F
        MOV     EBX, EAX         // EBX  <-  Fa Fr Fg Fb
        AND     EAX, $00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX, $FF00FF00   // EBX  <-  Fa 00 Fg 00
        IMUL    EAX, ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX, 8           // EBX  <-  00 Fa 00 Fg
        IMUL    EBX, ECX         // EBX  <-  Pa ** Pg **
        ADD     EAX, bias
        AND     EAX, $FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX, 8           // EAX  <-  00 Pr ** Pb
        ADD     EBX, bias
        AND     EBX, $FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX, EBX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W; Q = W * B
        MOV     ESI, [EDX]
        XOR     ECX, $000000FF  // ECX  <-  1 - ECX
        MOV     EBX, ESI        // EBX  <-  Ba Br Bg Bb
        AND     ESI, $00FF00FF  // ESI  <-  00 Br 00 Bb
        AND     EBX, $FF00FF00  // EBX  <-  Ba 00 Bg 00
        IMUL    ESI, ECX        // ESI  <-  Qr ** Qb **
        SHR     EBX, 8          // EBX  <-  00 Ba 00 Bg
        IMUL    EBX, ECX        // EBX  <-  Qa ** Qg **
        ADD     ESI, bias
        AND     ESI, $FF00FF00  // ESI  <-  Qr 00 Qb 00
        SHR     ESI, 8          // ESI  <-  00 Qr ** Qb
        ADD     EBX, bias
        AND     EBX, $FF00FF00  // EBX  <-  Qa 00 Qg 00
        OR      EBX, ESI        // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX, EBX        // EAX  <-  Za Zr Zg Zb
        MOV     [EDX], EAX

        POP     ESI
        POP     EBX
        RET

@1:     MOV     [EDX], EAX
@2:     RET
end;

//------------------------------------------------------------------------------

function _BlendRegEx(F, B, M: TColor32): TColor32; assembler;
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F multiplied by master alpha (M)
  // no checking for M = $FF, if this is the case Graphics32 uses BlendReg
  // Result Z = Fa * M * Frgb + (1 - Fa * M) * Brgb
  // EAX <- F
  // EDX <- B
  // ECX <- M

  // Check Fa > 0 ?
        TEST    EAX, $FF000000  // Fa = 0? => Result := EDX
        JZ      @1

        PUSH    EBX

  // Get weight W = Fa * M
        MOV     EBX, EAX        // EBX  <-  Fa Fr Fg Fb
        SHR     EBX, 24         // EBX  <-  00 00 00 Fa
        IMUL    ECX, EBX        // ECX  <-  00 00  W **
        SHR     ECX, 8          // ECX  <-  00 00 00  W
        JZ      @1              // W = 0 ?  => Result := EDX

  // P = W * F
        MOV     EBX, EAX        // EBX  <-  ** Fr Fg Fb
        AND     EAX, $00FF00FF  // EAX  <-  00 Fr 00 Fb
        AND     EBX, $0000FF00  // EBX  <-  00 00 Fg 00
        IMUL    EAX, ECX        // EAX  <-  Pr ** Pb **
        SHR     EBX, 8          // EBX  <-  00 00 00 Fg
        IMUL    EBX, ECX        // EBX  <-  00 00 Pg **
        ADD     EAX, bias
        AND     EAX, $FF00FF00  // EAX  <-  Pr 00 Pb 00
        SHR     EAX, 8          // EAX  <-  00 Pr ** Pb
        ADD     EBX, bias
        AND     EBX, $0000FF00  // EBX  <-  00 00 Pg 00
        OR      EAX, EBX        // EAX  <-  00 Pr Pg Pb

  // W = 1 - W; Q = W * B
        XOR     ECX, $000000FF  // ECX  <-  1 - ECX
        MOV     EBX, EDX        // EBX  <-  00 Br Bg Bb
        AND     EDX, $00FF00FF  // EDX  <-  00 Br 00 Bb
        AND     EBX, $0000FF00  // EBX  <-  00 00 Bg 00
        IMUL    EDX, ECX        // EDX  <-  Qr ** Qb **
        SHR     EBX, 8          // EBX  <-  00 00 00 Bg
        IMUL    EBX, ECX        // EBX  <-  00 00 Qg **
        ADD     EDX, bias
        AND     EDX, $FF00FF00  // EDX  <-  Qr 00 Qb 00
        SHR     EDX, 8          // EDX  <-  00 Qr ** Qb
        ADD     EBX, bias
        AND     EBX, $0000FF00  // EBX  <-  00 00 Qg 00
        OR      EBX, EDX        // EBX  <-  00 Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX, EBX        // EAX  <-  00 Zr Zg Zb

        POP     EBX
        RET

@1:     MOV     EAX, EDX
        RET
end;

//------------------------------------------------------------------------------

procedure _BlendMemEx(F: TColor32; var B: TColor32; M: TColor32); assembler;
asm
  // EAX <- F
  // [EDX] <- B
  // ECX <- M

  // Check Fa > 0 ?
        TEST    EAX, $FF000000  // Fa = 0? => write nothing
        JZ      @2

        PUSH    EBX

  // Get weight W = Fa * M
        MOV     EBX, EAX        // EBX  <-  Fa Fr Fg Fb
        SHR     EBX, 24         // EBX  <-  00 00 00 Fa
        IMUL    ECX, EBX        // ECX  <-  00 00  W **
        SHR     ECX, 8          // ECX  <-  00 00 00  W
        JZ      @1              // W = 0 ?  => write nothing

        PUSH    ESI

  // P = W * F
        MOV     EBX, EAX         // EBX  <-  ** Fr Fg Fb
        AND     EAX, $00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX, $0000FF00   // EBX  <-  00 00 Fg 00
        IMUL    EAX, ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX, 8           // EBX  <-  00 00 00 Fg
        IMUL    EBX, ECX         // EBX  <-  00 00 Pg **
        ADD     EAX, bias
        AND     EAX, $FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX, 8           // EAX  <-  00 Pr ** Pb
        ADD     EBX, bias
        AND     EBX, $0000FF00   // EBX  <-  00 00 Pg 00
        OR      EAX, EBX         // EAX  <-  00 Pr Pg Pb

  // W = 1 - W; Q = W * B
        MOV     ESI, [EDX]
        XOR     ECX, $000000FF   // ECX  <-  1 - ECX
        MOV     EBX, ESI         // EBX  <-  00 Br Bg Bb
        AND     ESI, $00FF00FF   // ESI  <-  00 Br 00 Bb
        AND     EBX, $0000FF00   // EBX  <-  00 00 Bg 00
        IMUL    ESI, ECX         // ESI  <-  Qr ** Qb **
        SHR     EBX, 8           // EBX  <-  00 00 00 Bg
        IMUL    EBX, ECX         // EBX  <-  00 00 Qg **
        ADD     ESI, bias
        AND     ESI, $FF00FF00   // ESI  <-  Qr 00 Qb 00
        SHR     ESI, 8           // ESI  <-  00 Qr ** Qb
        ADD     EBX, bias
        AND     EBX, $0000FF00   // EBX  <-  00 00 Qg 00
        OR      EBX, ESI         // EBX  <-  00 Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX, EBX         // EAX  <-  00 Zr Zg Zb

        MOV     [EDX], EAX
        POP     ESI

@1:     POP     EBX
@2:     RET
end;

//------------------------------------------------------------------------------

procedure _BlendLine(Src, Dst: PColor32; Count: Integer); assembler;
asm
  // EAX <- Src
  // EDX <- Dst
  // ECX <- Count

  // test the counter for zero or negativity
        TEST    ECX, ECX
        JS      @4

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI, EAX        // ESI <- Src
        MOV     EDI, EDX        // EDI <- Dst

  // loop start
@1:     MOV     EAX, [ESI]
        TEST    EAX, $FF000000
        JZ      @3              // complete transparency, proceed to next point

        PUSH    ECX             // store counter

  // Get weight W = Fa * M
        MOV     ECX, EAX        // ECX  <-  Fa Fr Fg Fb
        SHR     ECX, 24         // ECX  <-  00 00 00 Fa

  // Test Fa = 255 ?
        CMP     ECX, $FF
        JZ      @2

  // P = W * F
        MOV     EBX, EAX         // EBX  <-  Fa Fr Fg Fb
        AND     EAX, $00FF00FF   // EAX  <-  00 Fr 00 Fb
        AND     EBX, $FF00FF00   // EBX  <-  Fa 00 Fg 00
        IMUL    EAX, ECX         // EAX  <-  Pr ** Pb **
        SHR     EBX, 8           // EBX  <-  00 Fa 00 Fg
        IMUL    EBX, ECX         // EBX  <-  Pa ** Pg **
        ADD     EAX, bias
        AND     EAX, $FF00FF00   // EAX  <-  Pr 00 Pb 00
        SHR     EAX, 8           // EAX  <-  00 Pr ** Pb
        ADD     EBX, bias
        AND     EBX, $FF00FF00   // EBX  <-  Pa 00 Pg 00
        OR      EAX, EBX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W; Q = W * B
        MOV     EDX, [EDI]
        XOR     ECX, $000000FF   // ECX  <-  1 - ECX
        MOV     EBX, EDX         // EBX  <-  Ba Br Bg Bb
        AND     EDX, $00FF00FF   // ESI  <-  00 Br 00 Bb
        AND     EBX, $FF00FF00   // EBX  <-  Ba 00 Bg 00
        IMUL    EDX, ECX         // ESI  <-  Qr ** Qb **
        SHR     EBX, 8           // EBX  <-  00 Ba 00 Bg
        IMUL    EBX, ECX         // EBX  <-  Qa ** Qg **
        ADD     EDX, bias
        AND     EDX, $FF00FF00   // ESI  <-  Qr 00 Qb 00
        SHR     EDX, 8           // ESI  <-  00 Qr ** Qb
        ADD     EBX, bias
        AND     EBX, $FF00FF00   // EBX  <-  Qa 00 Qg 00
        OR      EBX, EDX         // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
        ADD     EAX, EBX        // EAX  <-  Za Zr Zg Zb
@2:     MOV     [EDI], EAX

        POP     ECX             // restore counter

@3:     ADD     ESI, 4
        ADD     EDI, 4

  // loop end
        DEC     ECX
        JNZ     @1

        POP     EDI
        POP     ESI
        POP     EBX

@4:     RET
end;

//------------------------------------------------------------------------------

procedure _BlendLineEx(Src, Dst: PColor32; Count: Integer; M: TColor32);
begin
  while Count > 0 do
  begin
    _BlendMemEx(Src^, Dst^, M);
    Inc(Src);
    Inc(Dst);
    Dec(Count);
  end;
end;

//------------------------------------------------------------------------------

{ MMX versions }

var
  AlphaTable: Pointer;
  bias_ptr: Pointer;
  alpha_ptr: Pointer;

procedure GenAlphaTable;
var
  I: Integer;
  L: LongWord;
  P: ^LongWord;
begin
  GetMem(AlphaTable, 257 * 8);
  alpha_ptr := Pointer(Integer(AlphaTable) and $FFFFFFF8);
  if Integer(alpha_ptr) < Integer(AlphaTable) then
    alpha_ptr := Pointer(Integer(alpha_ptr) + 8);
  P := alpha_ptr;
  for I := 0 to 255 do
  begin
    L := I + I shl 16;
    P^ := L;
    Inc(P);
    P^ := L;
    Inc(P);
  end;
  bias_ptr := Pointer(Integer(alpha_ptr) + $80 * 8);
end;

//------------------------------------------------------------------------------

procedure FreeAlphaTable;
begin
  FreeMem(AlphaTable);
end;

//------------------------------------------------------------------------------

procedure EMMS;
begin
  if MMX_ACTIVE then
  asm
          db      $0F, $77               // EMMS
  end;
end;

//------------------------------------------------------------------------------

function M_CombineReg(X, Y, W: TColor32): TColor32; assembler;
asm
  // EAX - Color X
  // EDX - Color Y
  // ECX - Weight of X [0..255]
  // Result := W * (X - Y) + Y

        db $0F,$EF,$C0           /// PXOR      MM0,MM0
        db $0F,$6E,$C8           /// MOVD      MM1,EAX
        SHL       ECX, 3
        db $0F,$6E,$D2           /// MOVD      MM2,EDX
        db $0F,$60,$C8           /// PUNPCKLBW MM1,MM0
        db $0F,$60,$D0           /// PUNPCKLBW MM2,MM0
        ADD       ECX, alpha_ptr
        db $0F,$F9,$CA           /// PSUBW     MM1,MM2
        db $0F,$D5,$09           /// PMULLW    MM1,[ECX]
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        MOV       ECX, bias_ptr
        db $0F,$FD,$11           /// PADDW     MM2,[ECX]
        db $0F,$FD,$CA           /// PADDW     MM1,MM2
        db $0F,$71,$D1,$08       /// PSRLW     MM1,8
        db $0F,$67,$C8           /// PACKUSWB  MM1,MM0
        db $0F,$7E,$C8           /// MOVD      EAX,MM1
end;

//------------------------------------------------------------------------------

procedure M_CombineMem(F: TColor32; var B: TColor32; W: TColor32); assembler;
asm
  // EAX - Color X
  // [EDX] - Color Y
  // ECX - Weight of X [0..255]
  // Result := W * (X - Y) + Y

        JCXZ      @1
        CMP       ECX, $FF
        JZ        @2

        db $0F,$EF,$C0           /// PXOR      MM0,MM0
        db $0F,$6E,$C8           /// MOVD      MM1,EAX
        SHL       ECX, 3
        db $0F,$6E,$12           /// MOVD      MM2,[EDX]
        db $0F,$60,$C8           /// PUNPCKLBW MM1,MM0
        db $0F,$60,$D0           /// PUNPCKLBW MM2,MM0
        ADD       ECX, alpha_ptr
        db $0F,$F9,$CA           /// PSUBW     MM1,MM2
        db $0F,$D5,$09           /// PMULLW    MM1,[ECX]
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        MOV       ECX, bias_ptr
        db $0F,$FD,$11           /// PADDW     MM2,[ECX]
        db $0F,$FD,$CA           /// PADDW     MM1,MM2
        db $0F,$71,$D1,$08       /// PSRLW     MM1,8
        db $0F,$67,$C8           /// PACKUSWB  MM1,MM0
        db $0F,$7E,$0A           /// MOVD      [EDX],MM1
@1:     RET

@2:     MOV       [EDX], EAX
end;

//------------------------------------------------------------------------------

function M_BlendReg(F, B: TColor32): TColor32; assembler;
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // EDX <- B
  // Result := Fa * (Frgb - Brgb) + Brgb
        db $0F,$EF,$DB           /// PXOR      MM3,MM3
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$D2           /// MOVD      MM2,EDX
        db $0F,$60,$C3           /// PUNPCKLBW MM0,MM3
        MOV     ECX, bias_ptr
        db $0F,$60,$D3           /// PUNPCKLBW MM2,MM3
        db $0F,$6F,$C8           /// MOVQ      MM1,MM0
        db $0F,$69,$C9           /// PUNPCKHWD MM1,MM1
        db $0F,$F9,$C2           /// PSUBW     MM0,MM2
        db $0F,$6A,$C9           /// PUNPCKHDQ MM1,MM1
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        db $0F,$D5,$C1           /// PMULLW    MM0,MM1
        db $0F,$FD,$11           /// PADDW     MM2,[ECX]
        db $0F,$FD,$D0           /// PADDW     MM2,MM0
        db $0F,$71,$D2,$08       /// PSRLW     MM2,8
        db $0F,$67,$D3           /// PACKUSWB  MM2,MM3
        db $0F,$7E,$D0           /// MOVD      EAX,MM2
end;

//------------------------------------------------------------------------------

procedure M_BlendMem(F: TColor32; var B: TColor32); assembler;
asm
  // EAX - Color X
  // [EDX] - Color Y
  // Result := W * (X - Y) + Y

        TEST      EAX, $FF000000
        JZ        @1
        CMP       EAX, $FF000000
        JNC       @2

        db $0F,$EF,$DB           /// PXOR      MM3,MM3
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$12           /// MOVD      MM2,[EDX]
        db $0F,$60,$C3           /// PUNPCKLBW MM0,MM3
        MOV       ECX, bias_ptr
        db $0F,$60,$D3           /// PUNPCKLBW MM2,MM3
        db $0F,$6F,$C8           /// MOVQ      MM1,MM0
        db $0F,$69,$C9           /// PUNPCKHWD MM1,MM1
        db $0F,$F9,$C2           /// PSUBW     MM0,MM2
        db $0F,$6A,$C9           /// PUNPCKHDQ MM1,MM1
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        db $0F,$D5,$C1           /// PMULLW    MM0,MM1
        db $0F,$FD,$11           /// PADDW     MM2,[ECX]
        db $0F,$FD,$D0           /// PADDW     MM2,MM0
        db $0F,$71,$D2,$08       /// PSRLW     MM2,8
        db $0F,$67,$D3           /// PACKUSWB  MM2,MM3
        db $0F,$7E,$12           /// MOVD      [EDX],MM2
@1:     RET

@2:     MOV       [EDX], EAX
end;

//------------------------------------------------------------------------------

function M_BlendRegEx(F, B, M: TColor32): TColor32; assembler;
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // EDX <- B
  // ECX <- M
  // Result := M * Fa * (Frgb - Brgb) + Brgb
        PUSH      EBX
        MOV       EBX, EAX
        SHR       EBX, 24
        IMUL      ECX, EBX
        SHR       ECX, 8
        JZ        @1

        db $0F,$EF,$C0           /// PXOR      MM0,MM0
        db $0F,$6E,$C8           /// MOVD      MM1,EAX
        SHL       ECX, 3
        db $0F,$6E,$D2           /// MOVD      MM2,EDX
        db $0F,$60,$C8           /// PUNPCKLBW MM1,MM0
        db $0F,$60,$D0           /// PUNPCKLBW MM2,MM0
        ADD       ECX, alpha_ptr
        db $0F,$F9,$CA           /// PSUBW     MM1,MM2
        db $0F,$D5,$09           /// PMULLW    MM1,[ECX]
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        MOV       ECX, bias_ptr
        db $0F,$FD,$11           /// PADDW     MM2,[ECX]
        db $0F,$FD,$CA           /// PADDW     MM1,MM2
        db $0F,$71,$D1,$08       /// PSRLW     MM1,8
        db $0F,$67,$C8           /// PACKUSWB  MM1,MM0
        db $0F,$7E,$C8           /// MOVD      EAX,MM1

@1:     MOV       EAX, EDX
        POP       EBX
end;

//------------------------------------------------------------------------------

procedure M_BlendMemEx(F: TColor32; var B: TColor32; M: TColor32); assembler;
asm
  // blend foreground color (F) to a background color (B),
  // using alpha channel value of F
  // EAX <- F
  // [EDX] <- B
  // ECX <- M
  // Result := M * Fa * (Frgb - Brgb) + Brgb
        TEST      EAX, $FF000000
        JZ        @2

        PUSH      EBX
        MOV       EBX, EAX
        SHR       EBX, 24
        IMUL      ECX, EBX
        SHR       ECX, 8
        JZ        @1

        db $0F,$EF,$C0           /// PXOR      MM0,MM0
        db $0F,$6E,$C8           /// MOVD      MM1,EAX
        SHL       ECX, 3
        db $0F,$6E,$12           /// MOVD      MM2,[EDX]
        db $0F,$60,$C8           /// PUNPCKLBW MM1,MM0
        db $0F,$60,$D0           /// PUNPCKLBW MM2,MM0
        ADD       ECX, alpha_ptr
        db $0F,$F9,$CA           /// PSUBW     MM1,MM2
        db $0F,$D5,$09           /// PMULLW    MM1,[ECX]
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        MOV       ECX, bias_ptr
        db $0F,$FD,$11           /// PADDW     MM2,[ECX]
        db $0F,$FD,$CA           /// PADDW     MM1,MM2
        db $0F,$71,$D1,$08       /// PSRLW     MM1,8
        db $0F,$67,$C8           /// PACKUSWB  MM1,MM0
        db $0F,$7E,$0A           /// MOVD      [EDX],MM1
@1:     POP       EBX
@2:
end;

//------------------------------------------------------------------------------

procedure M_BlendLine(Src, Dst: PColor32; Count: Integer); assembler;
asm
  // EAX <- Src
  // EDX <- Dst
  // ECX <- Count

  // test the counter for zero or negativity
        TEST      ECX, ECX
        JS        @4

        PUSH      ESI
        PUSH      EDI

        MOV       ESI, EAX        // ESI <- Src
        MOV       EDI, EDX        // EDI <- Dst

  // loop start
@1:     MOV       EAX, [ESI]
        TEST      EAX, $FF000000
        JZ        @3              // complete transparency, proceed to next point
        CMP       EAX, $FF000000
        JNC       @2              // opaque pixel, copy without blending

  // blend
        db $0F,$EF,$DB           /// PXOR      MM3,MM3
        db $0F,$6E,$C0           /// MOVD      MM0,EAX
        db $0F,$6E,$17           /// MOVD      MM2,[EDI]
        db $0F,$60,$C3           /// PUNPCKLBW MM0,MM3
        MOV       EAX, bias_ptr
        db $0F,$60,$D3           /// PUNPCKLBW MM2,MM3
        db $0F,$6F,$C8           /// MOVQ      MM1,MM0
        db $0F,$69,$C9           /// PUNPCKHWD MM1,MM1
        db $0F,$F9,$C2           /// PSUBW     MM0,MM2
        db $0F,$6A,$C9           /// PUNPCKHDQ MM1,MM1
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        db $0F,$D5,$C1           /// PMULLW    MM0,MM1
        db $0F,$FD,$10           /// PADDW     MM2,[EAX]
        db $0F,$FD,$D0           /// PADDW     MM2,MM0
        db $0F,$71,$D2,$08       /// PSRLW     MM2,8
        db $0F,$67,$D3           /// PACKUSWB  MM2,MM3
        db $0F,$7E,$D0           /// MOVD      EAX,MM2

@2:     MOV       [EDI], EAX

@3:     ADD       ESI, 4
        ADD       EDI, 4

  // loop end
        DEC       ECX
        JNZ       @1

        POP       EDI
        POP       ESI

@4:     RET
end;

//------------------------------------------------------------------------------

procedure M_BlendLineEx(Src, Dst: PColor32; Count: Integer; M: TColor32); assembler;
asm
  // EAX <- Src
  // EDX <- Dst
  // ECX <- Count

  // test the counter for zero or negativity
        TEST      ECX, ECX
        JS        @4

        PUSH      ESI
        PUSH      EDI
        PUSH      EBX

        MOV       ESI, EAX        // ESI <- Src
        MOV       EDI, EDX        // EDI <- Dst
        MOV       EDX, M          // EDX <- Master Alpha

  // loop start
@1:     MOV       EAX, [ESI]
        TEST      EAX, $FF000000
        JZ        @3              // complete transparency, proceed to next point
        MOV       EBX, EAX
        SHR       EBX, 24
        IMUL      EBX, EDX
        SHR       EBX, 8
        JZ        @3              // complete transparency, proceed to next point

  // blend
        db $0F,$EF,$C0           /// PXOR      MM0,MM0
        db $0F,$6E,$C8           /// MOVD      MM1,EAX
        SHL       EBX, 3
        db $0F,$6E,$17           /// MOVD      MM2,[EDI]
        db $0F,$60,$C8           /// PUNPCKLBW MM1,MM0
        db $0F,$60,$D0           /// PUNPCKLBW MM2,MM0
        ADD       EBX, alpha_ptr
        db $0F,$F9,$CA           /// PSUBW     MM1,MM2
        db $0F,$D5,$0B           /// PMULLW    MM1,[EBX]
        db $0F,$71,$F2,$08       /// PSLLW     MM2,8
        MOV       EBX, bias_ptr
        db $0F,$FD,$13           /// PADDW     MM2,[EBX]
        db $0F,$FD,$CA           /// PADDW     MM1,MM2
        db $0F,$71,$D1,$08       /// PSRLW     MM1,8
        db $0F,$67,$C8           /// PACKUSWB  MM1,MM0
        db $0F,$7E,$C8           /// MOVD      EAX,MM1

@2:     MOV       [EDI], EAX

@3:     ADD       ESI, 4
        ADD       EDI, 4

  // loop end
        DEC       ECX
        JNZ       @1

        POP       EBX
        POP       EDI
        POP       ESI
@4:
end;

{ MMX Detection and linking }

procedure SetupFunctions;
var
  CpuInfo: TCpuInfo;
begin
  //WIMDC
  GetCpuInfo(CpuInfo);
  MMX_ACTIVE := CpuInfo.MMX;
  if MMX_ACTIVE then
  begin
    // link MMX functions
    CombineReg := M_CombineReg;
    CombineMem := M_CombineMem;
    BlendReg := M_BlendReg;
    BlendMem := M_BlendMem;
    BlendRegEx := M_BlendRegEx;
    BlendMemEx := M_BlendMemEx;
    BlendLine := M_BlendLine;
    BlendLineEx := M_BlendLineEx;
  end
  else
  begin
    // link non-MMX functions
    CombineReg := _CombineReg;
    CombineMem := _CombineMem;
    BlendReg := _BlendReg;
    BlendMem := _BlendMem;
    BlendRegEx := _BlendRegEx;
    BlendMemEx := _BlendMemEx;
    BlendLine := _BlendLine;
    BlendLineEx := _BlendLineEx;
  end;
end;

//==============================================================================
// Dialog functions
//==============================================================================

{$IFDEF WIN32}

function DialogUnitsToPixelsX(DialogUnits: Word): Word;
begin
  Result := (DialogUnits * LoWord(GetDialogBaseUnits)) div 4;
end;

//------------------------------------------------------------------------------

function DialogUnitsToPixelsY(DialogUnits: Word): Word;
begin
  Result := (DialogUnits * HiWord(GetDialogBaseUnits)) div 8;
end;

//------------------------------------------------------------------------------

function PixelsToDialogUnitsX(PixelUnits: Word): Word;
begin
  Result := PixelUnits * 4 div LoWord(GetDialogBaseUnits);
end;

//------------------------------------------------------------------------------

function PixelsToDialogUnitsY(PixelUnits: Word): Word;
begin
  Result := PixelUnits * 8 div HiWord(GetDialogBaseUnits);
end;

{$ENDIF WIN32}

//==============================================================================
// Points
//==============================================================================

function NullPoint: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

//------------------------------------------------------------------------------

function PointAssign(X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

//------------------------------------------------------------------------------

procedure PointCopy(var Dest: TPoint; const Source: TPoint);
begin
  Dest.X := Source.X;
  Dest.Y := Source.Y;
end;

//------------------------------------------------------------------------------

function PointEqual(const P1, P2: TPoint): Boolean;
begin
  Result := (P1.X = P2.X) and (P1.Y = P2.Y);
end;

//------------------------------------------------------------------------------

function PointIsNull(const P: TPoint): Boolean;
begin
  Result := (P.X = 0) and (P.Y = 0);
end;

//------------------------------------------------------------------------------

procedure PointMove(var P: TPoint; const DeltaX, DeltaY: Integer);
begin
  P.X := P.X + DeltaX;
  P.Y := P.Y + DeltaY;
end;

//==============================================================================
// Rectangles
//==============================================================================

function NullRect: TRect;
begin
  with Result do
  begin
    Top := 0;
    Left := 0;
    Bottom := 0;
    Right := 0;
  end;
end;

//------------------------------------------------------------------------------

function RectAssign(Left, Top, Right, Bottom: Integer): TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

//------------------------------------------------------------------------------

function RectAssignPoints(const TopLeft, BottomRight: TPoint): TRect;
begin
  Result.TopLeft := TopLeft;
  Result.BottomRight := BottomRight;
end;

//------------------------------------------------------------------------------

function RectBounds(Left, Top, Width, Height: Integer): TRect;
begin
  Result := RectAssign(Left, Top, Left + Width, Top + Height);
end;

//------------------------------------------------------------------------------

function RectCenter(const R: TRect): TPoint;
begin
  Result.X := R.Left + (RectWidth(R) div 2);
  Result.Y := R.Top + (RectHeight(R) div 2);
end;

//------------------------------------------------------------------------------

procedure RectCopy(var Dest: TRect; const Source: TRect);
begin
  Dest := Source;
end;

//------------------------------------------------------------------------------

procedure RectFitToScreen(var R: TRect);
var
  X, Y: Integer;
  Delta: Integer;
begin
  X := GetSystemMetrics(SM_CXSCREEN);
  Y := GetSystemMetrics(SM_CYSCREEN);
  with R do
  begin
    if Right > X then
    begin
      Delta := Right - Left;
      Right := X;
      Left := Right - Delta;
    end;
    if Left < 0 then
    begin
      Delta := Right - Left;
      Left := 0;
      Right := Left + Delta;
    end;
    if Bottom > Y then
    begin
      Delta := Bottom - Top;
      Bottom := Y;
      Top := Bottom - Delta;
    end;
    if Top < 0 then
    begin
      Delta := Bottom - Top;
      Top := 0;
      Bottom := Top + Delta;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure RectGrow(var R: TRect; Delta: Integer);
begin
  with R do
  begin
    Dec(Left, Delta);
    Dec(Top, Delta);
    Inc(Right, Delta);
    Inc(Bottom, Delta);
  end;
end;

//------------------------------------------------------------------------------

procedure RectGrowX(var R: TRect; Delta: Integer);
begin
  with R do
  begin
    Dec(Left, Delta);
    Inc(Right, Delta);
  end;
end;

//------------------------------------------------------------------------------

procedure RectGrowY(var R: TRect; Delta: Integer);
begin
  with R do
  begin
    Dec(Top, Delta);
    Inc(Bottom, Delta);
  end;
end;

//------------------------------------------------------------------------------

function RectEqual(const R1, R2: TRect): Boolean;
begin
  Result := (R1.Left = R2.Left) and (R1.Top = R2.Top) and
    (R1.Right = R2.Right) and (R1.Bottom = R2.Bottom);
end;

//------------------------------------------------------------------------------

function RectHeight(const R: TRect): Integer;
begin
  Result := Abs(R.Bottom - R.Top);
end;

//------------------------------------------------------------------------------

function RectIncludesPoint(const R: TRect; const Pt: TPoint): Boolean;
begin
  Result := (Pt.X > R.Left) and (Pt.X < R.Right) and
    (Pt.Y > R.Top) and (Pt.Y < R.Bottom);
end;

//------------------------------------------------------------------------------

function RectIncludesRect(const R1, R2: TRect): Boolean;
begin
  Result := (R1.Left >= R2.Left) and (R1.Top >= R2.Top) and
    (R1.Right <= R2.Right) and (R1.Bottom <= R2.Bottom);
end;

//------------------------------------------------------------------------------

function RectIntersection(const R1, R2: TRect): TRect;
begin
  with Result do
  begin
    Left := JclLogic.Max(R1.Left, R2.Left);
    Top := JclLogic.Max(R1.Top, R2.Top);
    Right := JclLogic.Min(R1.Right, R2.Right);
    Bottom := JclLogic.Min(R1.Bottom, R2.Bottom);
  end;
  if not RectIsValid(Result) then
    Result := NullRect;
end;

//------------------------------------------------------------------------------

function RectIntersectRect(const R1, R2: TRect): Boolean;
begin
  Result := not RectIsNull(RectIntersection(R1, R2));
end;

//------------------------------------------------------------------------------

function RectIsEmpty(const R: TRect): Boolean;
begin
  Result := (R.Right = R.Left) and (R.Bottom = R.Top);
end;

//------------------------------------------------------------------------------

function RectIsNull(const R: TRect): Boolean;
begin
  with R do
    Result := (Left = 0) and (Right = 0) and (Top = 0) and (Bottom = 0);
end;

//------------------------------------------------------------------------------

function RectIsSquare(const R: TRect): Boolean;
begin
  Result := (RectHeight(R) = RectWidth(R));
end;

//------------------------------------------------------------------------------

function RectIsValid(const R: TRect): Boolean;
begin
  with R do
    Result := (Left <= Right) and (Top <= Bottom);
end;

//------------------------------------------------------------------------------

procedure RectMove(var R: TRect; DeltaX, DeltaY: Integer);
begin
  with R do
  begin
    Inc(Left, DeltaX);
    Inc(Right, DeltaX);
    Inc(Top, DeltaY);
    Inc(Bottom, DeltaY);
  end;
end;

//------------------------------------------------------------------------------

procedure RectMoveTo(var R: TRect; X, Y: Integer);
begin
  with R do
  begin
    Right := (Right - Left) + X;
    Bottom := (Bottom - Top) + Y;
    Left := X;
    Top := Y;
  end;
end;

//------------------------------------------------------------------------------

procedure RectNormalize(var R: TRect);
var
  Temp: Integer;
begin
  if R.Left > R.Right then
  begin
    Temp := R.Left;
    R.Left := R.Right;
    R.Right := Temp;
  end;
  if R.Top > R.Bottom then
  begin
    Temp := R.Top;
    R.Top := R.Bottom;
    R.Bottom := Temp;
  end;
end;

//------------------------------------------------------------------------------

function RectsAreValid(R: array of TRect): Boolean;
var
  I: Integer;
begin
  if Length(R) = 0 then
  begin
    Result := False;
    Exit;
  end;
  for I := Low(R) to High(R) do
  begin
    with R[I] do
      Result := (Left <= Right) and (Top <= Bottom);
    if not Result then
      Exit;
  end;
  Result := True;
end;

//------------------------------------------------------------------------------

function RectUnion(const R1, R2: TRect): TRect;
begin
  with Result do
  begin
    Left := JclLogic.Min(R1.Left, R2.Left);
    Top := JclLogic.Min(R1.Top, R2.Top);
    Right := JclLogic.Max(R1.Right, R2.Right);
    Bottom := JclLogic.Max(R1.Bottom, R2.Bottom);
  end;
  if not RectIsValid(Result) then
    Result := NullRect;
end;

//------------------------------------------------------------------------------

function RectWidth(const R: TRect): Integer;
begin
  Result := Abs(R.Right - R.Left);
end;

//==============================================================================
// Color
//==============================================================================

const
  MaxBytePercent = High(Byte) * 0.01;

procedure GetRGBValue(const Color: TColor; out Red, Green, Blue: Byte);
var
  Temp: TColorRec;
begin
  Temp.Value := ColorToRGB(Color);
  Red := Temp.R;
  Green := Temp.G;
  Blue := Temp.B;
end;

//------------------------------------------------------------------------------

function SetRGBValue(Red, Green, Blue: Byte): TColor;
begin
  TColorRec(Result).Red := Red;
  TColorRec(Result).Green := Green;
  TColorRec(Result).Blue := Blue;
  TColorRec(Result).Flag := 0;
end;

//------------------------------------------------------------------------------

function SetColorFlag(const Color: TColor; Flag: Byte): TColor;
begin
  Result := Color;
  TColorRec(Result).Flag := Flag;
end;

//------------------------------------------------------------------------------

function GetColorFlag(const Color: TColor): Byte;
begin
  Result := TColorRec(Color).Flag;
end;

//------------------------------------------------------------------------------

function SetColorRed(const Color: TColor; Red: Byte): TColor;
begin
  Result := ColorToRGB(Color);
  TColorRec(Result).Red := Red;
end;

//------------------------------------------------------------------------------

function GetColorRed(const Color: TColor): Byte;
var
  Temp: TColorRec;
begin
  Temp.Value := ColorToRGB(Color);
  Result := Temp.Red;
end;

//------------------------------------------------------------------------------

function SetColorGreen(const Color: TColor; Green: Byte): TColor;
begin
  Result := ColorToRGB(Color);
  TColorRec(Result).Green := Green;
end;

//------------------------------------------------------------------------------

function GetColorGreen(const Color: TColor): Byte;
var
  Temp: TColorRec;
begin
  Temp.Value := ColorToRGB(Color);
  Result := Temp.Green;
end;

//------------------------------------------------------------------------------

function SetColorBlue(const Color: TColor; Blue: Byte): TColor;
begin
  Result := ColorToRGB(Color);
  TColorRec(Result).Blue := Blue;
end;

//------------------------------------------------------------------------------

function GetColorBlue(const Color: TColor): Byte;
var
  Temp: TColorRec;
begin
  Temp.Value := ColorToRGB(Color);
  Result := Temp.Blue;
end;

//------------------------------------------------------------------------------

function BrightColor(Color: TColor; Pct: Extended): TColor;
var
  Temp: TColorRec;
begin
  Temp.Value := ColorToRGB(Color);
  Temp.R := BrightColorChannel(Temp.R, Pct);
  Temp.G := BrightColorChannel(Temp.G, Pct);
  Temp.B := BrightColorChannel(Temp.B, Pct);
  Result := Temp.Value;
end;

//------------------------------------------------------------------------------

function BrightColorChannel(Channel: Byte; Pct: Extended): Byte;
var
  Temp: Integer;
begin
  if Pct < 0 then
    Result := DarkColorChannel(Channel, -Pct)
  else
  begin
    Temp := Round(Channel + Pct * MaxBytePercent);
    if Temp > High(Result) then
      Result := High(Result)
    else
      Result := Temp;
  end;
end;

//------------------------------------------------------------------------------

function DarkColor(Color: TColor; Pct: Extended): TColor;
var
  Temp: TColorRec;
begin
  Temp.Value := ColorToRGB(Color);
  Temp.R := DarkColorChannel(Temp.R, Pct);
  Temp.G := DarkColorChannel(Temp.G, Pct);
  Temp.B := DarkColorChannel(Temp.B, Pct);
  Result := Temp.Value;
end;

//------------------------------------------------------------------------------

function DarkColorChannel(Channel: Byte; Pct: Extended): Byte;
var
  Temp: Integer;
begin
  if Pct < 0 then
    Result := BrightColorChannel(Channel, -Pct)
  else
  begin
    Temp := Round(Channel - Pct * MaxBytePercent);
    if Temp < Low(Result) then
      Result := Low(Result)
    else
      Result := Temp;
  end;
end;

//------------------------------------------------------------------------------

procedure CIED65ToCIED50(var X, Y, Z: Extended);
// Converts values of the XYZ color space using the D65 white point to D50 white point.
// The values were taken from www.srgb.com/hpsrgbprof/sld005.htm
var
  Xn, Yn, Zn: Extended;
begin
  Xn := 1.0479 * X + 0.0299 * Y - 0.0502 * Z;
  Yn := 0.0296 * X + 0.9904 * Y - 0.0171 * Z;
  Zn := -0.0092 * X + 0.0151 * Y + 0.7519 * Z;
  X := Xn;
  Y := Yn;
  Z := Zn;
end;

//------------------------------------------------------------------------------

procedure Gray16(Source, Target: Pointer; Count: Cardinal);
// converts each color component from a 16bits per sample to 8 bit used in Windows DIBs
// Count is the number of entries in Source and Target
var
  SourceRun: PWord;
  TargetRun: PByte;
begin
  SourceRun := Source;
  TargetRun := Target;
  while Count > 0 do
  begin
    TargetRun^ := SourceRun^ shr 8;
    Inc(SourceRun);
    Inc(TargetRun);
    Dec(Count);
  end;
end;

//------------------------------------------------------------------------------

type
  PCMYK = ^TCMYK;
  TCMYK = packed record
    C: Byte;
    M: Byte;
    Y: Byte;
    K: Byte;
  end;

  PCMYK16 = ^TCMYK16;
  TCMYK16 = packed record
    C: Word;
    M: Word;
    Y: Word;
    K: Word;
  end;

//------------------------------------------------------------------------------

procedure CMYKToBGR(Source, Target: Pointer; BitsPerSample: Byte; Count: Cardinal); overload;
// converts a stream of Count CMYK values to BGR
// BitsPerSample : 8 or 16
// CMYK is C,M,Y,K 4 byte record or 4 word record
// Target is always 3 byte record B, R, G
var
  R, G, B, K: Integer;
  I: Integer;
  SourcePtr: PCMYK;
  SourcePtr16: PCMYK16;
  TargetPtr: PByte;
begin
  case BitsPerSample of
    8:
      begin
        SourcePtr := Source;
        TargetPtr := Target;
        Count := Count div 4;
        for I := 0 to Count - 1 do
        begin
          K := SourcePtr.K;
          R := 255 - (SourcePtr.C - MulDiv(SourcePtr.C, K, 255) + K);
          G := 255 - (SourcePtr.M - MulDiv(SourcePtr.M, K, 255) + K);
          B := 255 - (SourcePtr.Y - MulDiv(SourcePtr.Y, K, 255) + K);
          TargetPtr^ := Max(0, Min(255, Byte(B)));
          Inc(TargetPtr);
          TargetPtr^ := Max(0, Min(255, Byte(G)));
          Inc(TargetPtr);
          TargetPtr^ := Max(0, Min(255, Byte(R)));
          Inc(TargetPtr);
          Inc(SourcePtr);
        end;
      end;
    16:
      begin
        SourcePtr16 := Source;
        TargetPtr := Target;
        Count := Count div 4;
        for I := 0 to Count - 1 do
        begin
          K := SourcePtr16.K;
          R := 255 - (SourcePtr16.C - MulDiv(SourcePtr16.C, K, 65535) + K) shr 8;
          G := 255 - (SourcePtr16.M - MulDiv(SourcePtr16.M, K, 65535) + K) shr 8;
          B := 255 - (SourcePtr16.Y - MulDiv(SourcePtr16.Y, K, 65535) + K) shr 8;
          TargetPtr^ := Max(0, Min(255, Byte(B)));
          Inc(TargetPtr);
          TargetPtr^ := Max(0, Min(255, Byte(G)));
          Inc(TargetPtr);
          TargetPtr^ := Max(0, Min(255, Byte(R)));
          Inc(TargetPtr);
          Inc(SourcePtr16);
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure CMYKToBGR(C, M, Y, K, Target: Pointer; BitsPerSample: Byte; Count: Cardinal); overload;
// converts a stream of Count CMYK values to BGR
var
  R, G, B: Integer;
  C8, M8, Y8, K8: PByte;
  C16, M16, Y16, K16: PWord;
  I: Integer;
  TargetPtr: PByte;
begin
  case BitsPerSample of
    8:
      begin
        C8 := C;
        M8 := M;
        Y8 := Y;
        K8 := K;
        TargetPtr := Target;
        Count := Count div 4;
        for I := 0 to Count - 1 do
        begin
          R := 255 - (C8^ - MulDiv(C8^, K8^, 255) + K8^);
          G := 255 - (M8^ - MulDiv(M8^, K8^, 255) + K8^);
          B := 255 - (Y8^ - MulDiv(Y8^, K8^, 255) + K8^);
          TargetPtr^ := Max(0, Min(255, Byte(B)));
          Inc(TargetPtr);
          TargetPtr^ := Max(0, Min(255, Byte(G)));
          Inc(TargetPtr);
          TargetPtr^ := Max(0, Min(255, Byte(R)));
          Inc(TargetPtr);
          Inc(C8);
          Inc(M8);
          Inc(Y8);
          Inc(K8);
        end;
      end;
    16:
      begin
        C16 := C;
        M16 := M;
        Y16 := Y;
        K16 := K;
        TargetPtr := Target;
        Count := Count div 4;
        for I := 0 to Count - 1 do
        begin
          R := 255 - (C16^ - MulDiv(C16^, K16^, 65535) + K16^) shr 8;
          G := 255 - (M16^ - MulDiv(M16^, K16^, 65535) + K16^) shr 8;
          B := 255 - (Y16^ - MulDiv(Y16^, K16^, 65535) + K16^) shr 8;
          TargetPtr^ := Max(0, Min(255, Byte(B)));
          Inc(TargetPtr);
          TargetPtr^ := Max(0, Min(255, Byte(G)));
          Inc(TargetPtr);
          TargetPtr^ := Max(0, Min(255, Byte(R)));
          Inc(TargetPtr);
          Inc(C16);
          Inc(M16);
          Inc(Y16);
          Inc(K16);
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure CIELABToBGR(Source, Target: Pointer; Count: Cardinal); overload;
// conversion of the CIE L*a*b color space to RGB using a two way approach assuming a D65 white point,
// first a conversion to CIE XYZ is performed and then from there to RGB
var
  FinalR,
  FinalG,
  FinalB: Integer;
  L, a, b,
  X, Y, Z, // color values in float format
  T,
  YYn3: Double;  // intermediate results
  SourcePtr,
  TargetPtr: PByte;
  PixelCount: Cardinal;
begin
  SourcePtr := Source;
  TargetPtr := Target;
  PixelCount := Count div 3;

  while PixelCount > 0 do
  begin
    // L should be in the range of 0..100 but at least Photoshop stores the luminance
    // in the range of 0..255
    L := SourcePtr^ / 2.55;
    Inc(SourcePtr);
    a := Shortint(SourcePtr^);
    Inc(SourcePtr);
    b := Shortint(SourcePtr^);
    Inc(SourcePtr);

    // CIE L*a*b can be calculated from CIE XYZ by:
    // L = 116 * ((Y / Yn)^1/3) - 16   if (Y / Yn) > 0.008856
    // L = 903.3 * Y / Yn              if (Y / Yn) <= 0.008856
    // a = 500 * (f(X / Xn) - f(Y / Yn))
    // b = 200 * (f(Y / Yn) - f(Z / Zn))
    //   where f(t) = t^(1/3) with (Y / Yn) > 0.008856
    //         f(t) = 7.787 * t + 16 / 116 with (Y / Yn) <= 0.008856
    //
    // by reordering the above equations we can calculate CIE L*a*b -> XYZ as follows:
    // L is in the range 0..100 and a as well as b in -127..127
    YYn3 := (L + 16) / 116; // this corresponds to (Y/Yn)^1/3
    if L < 7.9996 then
    begin
      Y := L / 903.3;
      X := a / 3893.5 + Y;
      Z := Y - b / 1557.4;
    end
    else
    begin
      T := YYn3 + a / 500;
      X := T * T * T;
      Y := YYn3 * YYn3 * YYn3;
      T := YYn3 - b / 200;
      Z := T * T * T;
    end;

    // once we have CIE XYZ it is easy (yet quite expensive) to calculate RGB values from this
    FinalR := Round(255.0 * ( 2.998 * X - 1.458 * Y - 0.541 * Z));
    FinalG := Round(255.0 * (-0.952 * X + 1.893 * Y + 0.059 * Z));
    FinalB := Round(255.0 * ( 0.099 * X - 0.198 * Y + 1.099 * Z));

    TargetPtr^ := Max(0, Min(255, Byte(FinalB)));
    Inc(TargetPtr);
    TargetPtr^ := Max(0, Min(255, Byte(FinalG)));
    Inc(TargetPtr);
    TargetPtr^ := Max(0, Min(255, Byte(FinalR)));
    Inc(TargetPtr);

    Dec(PixelCount);
  end;
end;

//------------------------------------------------------------------------------

procedure CIELABToBGR(LSource, aSource, bSource: PByte; Target: Pointer; Count: Cardinal); overload;
// conversion of the CIE L*a*b color space to RGB using a two way approach assuming a D65 white point,
// first a conversion to CIE XYZ is performed and then from there to RGB
// The bitspersample are not used so why leave it here.
var
  FinalR,
  FinalG,
  FinalB: Integer;
  L, a, b,
  X, Y, Z, // color values in float format
  T,
  YYn3: Double;  // intermediate results
  TargetPtr: PByte;
  PixelCount: Cardinal;
begin
  TargetPtr := Target;
  PixelCount := Count div 3;

  while PixelCount > 0 do
  begin
    // L should be in the range of 0..100 but at least Photoshop stores the luminance
    // in the range of 0..256
    L := LSource^ / 2.55;
    Inc(LSource);
    a := Shortint(aSource^);
    Inc(aSource);
    b := Shortint(bSource^);
    Inc(bSource);

    // CIE L*a*b can be calculated from CIE XYZ by:
    // L = 116 * ((Y / Yn)^1/3) - 16   if (Y / Yn) > 0.008856
    // L = 903.3 * Y / Yn              if (Y / Yn) <= 0.008856
    // a = 500 * (f(X / Xn) - f(Y / Yn))
    // b = 200 * (f(Y / Yn) - f(Z / Zn))
    //   where f(t) = t^(1/3) with (Y / Yn) > 0.008856
    //         f(t) = 7.787 * t + 16 / 116 with (Y / Yn) <= 0.008856
    //
    // by reordering the above equations we can calculate CIE L*a*b -> XYZ as follows:
    // L is in the range 0..100 and a as well as b in -127..127
    YYn3 := (L + 16) / 116; // this corresponds to (Y/Yn)^1/3
    if L < 7.9996 then
    begin
      Y := L / 903.3;
      X := a / 3893.5 + Y;
      Z := Y - b / 1557.4;
    end
    else
    begin
      T := YYn3 + a / 500;
      X := T * T * T;
      Y := YYn3 * YYn3 * YYn3;
      T := YYn3 - b / 200;
      Z := T * T * T;
    end;

    // once we have CIE XYZ it is easy (yet quite expensive) to calculate RGB values from this
    FinalR := Round(255.0 * ( 2.998 * X - 1.458 * Y - 0.541 * Z));
    FinalG := Round(255.0 * (-0.952 * X + 1.893 * Y + 0.059 * Z));
    FinalB := Round(255.0 * ( 0.099 * X - 0.198 * Y + 1.099 * Z));

    TargetPtr^ := Max(0, Min(255, Byte(FinalB)));
    Inc(TargetPtr);
    TargetPtr^ := Max(0, Min(255, Byte(FinalG)));
    Inc(TargetPtr);
    TargetPtr^ := Max(0, Min(255, Byte(FinalR)));
    Inc(TargetPtr);

    Dec(PixelCount);
  end;
end;

//------------------------------------------------------------------------------

procedure RGBToBGR(Source, Target: Pointer; BitsPerSample: Byte; Count: Cardinal); overload;
// reorders a stream of "Count" RGB values to BGR, additionally an eventual sample size adjustment is done
var
  SourceRun16: PRGBWord;
  SourceRun8: PRGB;
  TargetRun: PBGR;
begin
  Count := Count div 3;
  // usually only 8 bit samples are used but Photoshop allows for 16 bit samples
  case BitsPerSample of
    8:
      begin
        SourceRun8 := Source;
        TargetRun := Target;
        while Count > 0 do
        begin
          TargetRun.R := SourceRun8.R;
          TargetRun.G := SourceRun8.G;
          TargetRun.B := SourceRun8.B;
          Inc(SourceRun8);
          Inc(TargetRun);
          Dec(Count);
        end;
      end;
    16:
      begin
        SourceRun16 := Source;
        TargetRun := Target;
        while Count > 0 do
        begin
          TargetRun.R := SourceRun16.R shr 8;
          TargetRun.G := SourceRun16.G shr 8;
          TargetRun.B := SourceRun16.B shr 8;
          Inc(SourceRun16);
          Inc(TargetRun);
          Dec(Count);
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure RGBToBGR(R, G, B, Target: Pointer; BitsPerSample: Byte; Count: Cardinal); overload;
// reorders a stream of "Count" RGB values to BGR, additionally an eventual sample size adjustment is done
var
  R8, G8, B8: PByte;
  R16, G16, B16: PWord;
  TargetRun: PByte;
begin
  Count := Count div 3;
  // usually only 8 bits samples are used but Photoshop allows 16 bits samples too
  case BitsPerSample of
    8:
      begin
        R8 := R;
        G8 := G;
        B8 := B;
        TargetRun := Target;
        while Count > 0 do
        begin
          TargetRun^ := B8^;
          Inc(B8);
          Inc(TargetRun);
          TargetRun^ := G8^;
          Inc(G8);
          Inc(TargetRun);
          TargetRun^ := R8^;
          Inc(R8);
          Inc(TargetRun);
          Dec(Count);
        end;
      end;
    16:
      begin
        R16 := R;
        G16 := G;
        B16 := B;
        TargetRun := Target;
        while Count > 0 do
        begin
          TargetRun^ := B16^ shr 8;
          Inc(B16);
          Inc(TargetRun);
          TargetRun^ := G16^ shr 8;
          Inc(G16);
          Inc(TargetRun);
          TargetRun^ := R16^ shr 8;
          Inc(R16);
          Inc(TargetRun);
          Dec(Count);
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure RGBAToBGRA(Source, Target: Pointer; BitsPerSample: Byte; Count: Cardinal);
// reorders a stream of "Count" RGBA values to BGRA, additionally an eventual sample
// size adjustment is done
var
  SourceRun16: PRGBAWord;
  SourceRun8: PRGBA;
  TargetRun: PBGRA;
begin
  Count := Count div 4;
  // usually only 8 bit samples are used but Photoshop allows for 16 bit samples
  case BitsPerSample of
    8:
      begin
        SourceRun8 := Source;
        TargetRun := Target;
        while Count > 0 do
        begin
          TargetRun.R := SourceRun8.R;
          TargetRun.G := SourceRun8.G;
          TargetRun.B := SourceRun8.B;
          TargetRun.A := SourceRun8.A;
          Inc(SourceRun8);
          Inc(TargetRun);
          Dec(Count);
        end;
      end;
    16:
      begin
        SourceRun16 := Source;
        TargetRun := Target;
        while Count > 0 do
        begin
          TargetRun.R := SourceRun16.B shr 8;
          TargetRun.G := SourceRun16.G shr 8;
          TargetRun.B := SourceRun16.R shr 8;
          TargetRun.A := SourceRun16.A shr 8;
          Inc(SourceRun16);
          Inc(TargetRun);
          Dec(Count);
        end;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure WinColorToOpenGLColor(Color: TColor; out Red, Green, Blue: Float);
var
  Temp: TColorRec;
begin
  Temp.Value := ColorToRGB(Color);
  Red   := (Temp.R / High(Temp.R));
  Green := (Temp.G / High(Temp.G));
  Blue  := (Temp.B / High(Temp.B));
end;

//------------------------------------------------------------------------------

function OpenGLColorToWinColor(Red, Green, Blue: Float): TColor;
var
  Temp: TColorRec;
begin
  Temp.R := Round(Red   * High(Temp.R));
  Temp.G := Round(Green * High(Temp.G));
  Temp.B := Round(Blue  * High(Temp.B));
  Temp.Flag := 0;
  Result := Temp.Value;
end;

//------------------------------------------------------------------------------

function Color32(WinColor: TColor): TColor32; overload;
begin
  WinColor := ColorToRGB(WinColor);
  Result := ColorSwap(WinColor);
end;

//------------------------------------------------------------------------------

function Color32(R, G, B: Byte; A: Byte): TColor32; overload;
begin
  Result := A shl 24 + R shl 16 + G shl 8 + B;
end;

//------------------------------------------------------------------------------

function Color32(Index: Byte; const Palette: TPalette32): TColor32; overload;
begin
  Result := Palette[Index];
end;

//------------------------------------------------------------------------------

function Gray32(Intensity: Byte; Alpha: Byte): TColor32;
begin
  Result := TColor32(Alpha) shl 24 + TColor32(Intensity) shl 16 +
    TColor32(Intensity) shl 8 + TColor32(Intensity);
end;

//------------------------------------------------------------------------------

function WinColor(Color32: TColor32): TColor;
begin
  // the alpha channel byte is set to zero
  Result := (Color32 and _R shr 16) or (Color32 and _G) or
    (Color32 and _B shl 16);
end;

//------------------------------------------------------------------------------

function RedComponent(Color32: TColor32): Integer;
begin
  Result := Color32 and _R shr 16;
end;

//------------------------------------------------------------------------------

function GreenComponent(Color32: TColor32): Integer;
begin
  Result := Color32 and _G shr 8;
end;

//------------------------------------------------------------------------------

function BlueComponent(Color32: TColor32): Integer;
begin
  Result := Color32 and _B;
end;

//------------------------------------------------------------------------------

function AlphaComponent(Color32: TColor32): Integer;
begin
  Result := Color32 shr 24;
end;

//------------------------------------------------------------------------------

function Intensity(Color32: TColor32): Integer; assembler;
asm
// input:  RGB components
// outut: R * 61 + G * 174 + B * 20
        MOV     ECX, EAX
        AND     EAX, $00FF00FF      // EAX <-   0 R 0 B
        IMUL    EAX, $003D0014
        AND     ECX, $0000FF00      // ECX <-   0 0 G 0
        IMUL    ECX, $0000AE00
        MOV     EDX, EAX
        SHR     ECX, 8
        SHR     EDX, 16
        ADD     EAX, ECX
        ADD     EAX, EDX
        SHR     EAX, 8
end;

//------------------------------------------------------------------------------

function SetAlpha(Color32: TColor32; NewAlpha: Integer): TColor32;
begin
   Result := (Color32 and _RGB) or (TColor32(NewAlpha) shl 24);
end;

//------------------------------------------------------------------------------

function HSLToRGB(H, S, L: Single): TColor32;
var
  M1, M2: Single;
  R, G, B: Byte;

  function HueToColourValue(Hue: Single): Byte;
  var
    V: Double;
  begin
    Hue := Hue - Floor(Hue);

    if 6 * Hue < 1 then
      V := M1 + (M2 - M1) * Hue * 6
    else
    if 2 * Hue < 1 then
      V := M2
    else
    if 3 * Hue < 2 then
      V := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else
      V := M1;
    Result := Round(255 * V);
  end;
begin
  if S = 0 then
  begin
    R := Round(255 * L);
    G := R;
    B := R;
  end
  else
  begin
    if L <= 0.5 then
      M2 := L * (1 + S)
    else
      M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColourValue(H - 1 / 3);
    G := HueToColourValue(H);
    B := HueToColourValue(H + 1 / 3)
  end;
  Result := Color32(R, G, B, 255);
end;

//------------------------------------------------------------------------------

procedure RGBToHSL(RGB: TColor32; out H, S, L: Single);
var
  R, G, B, D, Cmax, Cmin: Single;
begin
  R := RedComponent(RGB) / 255;
  G := GreenComponent(RGB) / 255;
  B := BlueComponent(RGB) / 255;
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));
  L := (Cmax + Cmin) / 2;

  if Cmax = Cmin then
  begin
    H := 0;
    S := 0
  end
  else
  begin
    D := Cmax - Cmin;
    if L < 0.5 then
      S := D / (Cmax + Cmin)
    else
      S := D / (2 - Cmax - Cmin);
    if R = Cmax then
      H := (G - B) / D
    else
      if G = Cmax then
        H := 2 + (B - R) /D
      else
        H := 4 + (R - G) / D;
    H := H / 6;
    if H < 0 then
      H := H + 1;
  end;
end;

//==============================================================================
// Misc
//==============================================================================

{$IFDEF WIN32}

function DottedLineTo(Canvas: TCanvas; X, Y: Integer): Boolean;
const
  DotBits: array [0..7] of Word = ($AA, $55, $AA, $55, $AA, $55, $AA, $55);
var
  Bitmap: HBitmap;
  Brush: HBrush;
  SaveTextColor, SaveBkColor: TColorRef;
  LastPos: TPoint;
  R: TRect;
  DC: HDC;
begin
  DC := Canvas.Handle;
  GetCurrentPositionEx(DC, @LastPos);
  Result := False;
  if LastPos.X = X then
    R := RectAssign(LastPos.X, LastPos.Y, LastPos.X + 1, Y)
  else
  if LastPos.Y = Y then
    R := RectAssign(LastPos.X, LastPos.Y, X, LastPos.Y + 1)
  else
    Exit;
  Bitmap := CreateBitmap(8, 8, 1, 1, @DotBits);
  Brush := CreatePatternBrush(Bitmap);
  SaveTextColor := SetTextColor(DC, ColorToRGB(Canvas.Pen.Color));
  SaveBkColor := SetBkColor(DC, ColorToRGB(Canvas.Brush.Color));
  FillRect(DC, R, Brush);
  MoveToEx(DC, X, Y, nil);
  SetBkColor(DC, SaveBkColor);
  SetTextColor(DC, SaveTextColor);
  DeleteObject(Brush);
  DeleteObject(Bitmap);
  Result := True;
end;

{$ENDIF WIN32}

//------------------------------------------------------------------------------

function ShortenString(DC: HDC; const S: WideString; Width: Integer; TriplePointWidth: Integer = 0): WideString;

// Adjusts the given string S so that it fits into the given width. TriplePointWidth gives the width of
// the three points to be added to the shorted string. I this value is 0 then it will be determined implicitely.
// For higher speed (and multiple entries to be shorted) specify this value explicitely.
// Note: It is assumed that the string really needs shortage. Check this in advance.

var
  Size: TSize;
  Len: Integer;
  L, H, N, W: Integer;

begin
  Len := Length(S);
  if (Len = 0) or (Width <= 0) then
    Result := ''
  else
  begin
    // Determine width of triple point using the current DC settings (if not already done).
    if TriplePointWidth = 0 then
    begin
      GetTextExtentPoint32W(DC, '...', 3, Size);
      TriplePointWidth := Size.cx;
    end;

    if Width <= TriplePointWidth then
      Result := ''
    else
    begin
      // Do a binary search for the optimal string length which fits into the given width.
      L := 0;
      H := Len;
      N := 0;
      while L <= H do
      begin
        N := (L + H) shr 1;
        GetTextExtentPoint32W(DC, PWideChar(S), N, Size);
        W := Size.cx + TriplePointWidth;
        if W < Width then
          L := N + 1
        else
        begin
          H := N - 1;
          if W = Width then
            L := N;
        end;
      end;
      // Right-to-left directionality is automatically handled by DrawTextW, inclusive the three points
      // if ETO_RTLREADING is set for the canvas.
      Result := Copy(S, 1, N - 1) + '...';
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  SetupFunctions;
  if MMX_ACTIVE then
    GenAlphaTable;

finalization
  if MMX_ACTIVE then
    FreeAlphaTable;

end.
