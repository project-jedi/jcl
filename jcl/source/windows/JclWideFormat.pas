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
{ The Original Code is FormatW.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rob Kennedy, rkennedy att cs dott wisc dott edu.   }
{ Portions created by Rob Kennedy are Copyright Rob Kennedy. All rights reserved.                  }
{                                                                                                  }
{ Contributors (in alphabetical order):                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Comments by Rob Kennedy:                                                                         }
{                                                                                                  }
{ This unit provides a Unicode version of the SysUtils.Format function for                         }
{ Delphi 5. Later Delphi versions already have such a function. To the best of                     }
{ my knowledge, this function is bug-free. (Famous last words?) If there are any                   }
{ questions regarding the workings of the format parser's state machine, please                    }
{ do not hesitate to contact me. I understand all the state transitions, but                       }
{ find it hard to document en masse.                                                               }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

{ TODO : Replacing the calls to MultibytetoWideChar is all what's needed to make this crossplatform }
{ TODO : Fix Internal Error DBG1384 in BCB 6 compilation }

unit JclWideFormat;

{$I jcl.inc}
{$I windowsonly.inc}

interface

{ With FORMAT_EXTENSIONS defined, WideFormat will accept more argument types
  than Borland's Format function. In particular, it will accept Variant
  arguments for the D, E, F, G, M, N, U, and X format types, it will accept
  Boolean and TClass arguments for the S format type, and it will accept PChar,
  PWideChar, interface, and object arguments for the P format type.
  In addition, WideFormat can use Int64 and Variant arguments for index, width,
  and precision specifiers used by the asterisk character. }
{$DEFINE FORMAT_EXTENSIONS}

{ If the format type is D, U, or X, and if the format string contains a
  precision specifier greater than 16, then the precision specifier is ignored.
  This is consistent with observed Format behavior, although it is not so
  documented. Likewise, if the format type is E, F, G, M, or N and the precision
  specifier is greater than 18, then it too will be ignored.

  There is one known difference between the behaviors of Format and WideFormat.
  WideFormat interprets a width specifier as a signed 32-bit integer. If it is
  negative, then it will be treated as 0. Format interprets it as a very large
  unsigned integer, which can lead to an access violation or buffer overrun.

  WideFormat detects the same errors as Format, but it reports them differently.
  Because of differences in the parsers, WideFormat is unable to provide the
  entire format string in the error message every time. When the full string is
  not available, it will provide the offending character index instead. In the
  case of an invalid argument type, WideFormat will include the allowed types
  and the argument index in the error message. Despite the different error
  messages, the exception class is still EConvertError. }
function WideFormat(const Format: WideString; const Args: array of const): WideString;

implementation

uses
  Windows,              // for MultibytetoWideChar
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF ~HAS_UNIT_VARIANTS}
  SysUtils,             // for exceptions and FloatToText
  Classes,              // for TStrings, in error-reporting code
  JclBase,              // for PByte and PCardinal
  JclStrings,           // for BooleanToStr, StrLen
  JclMath,              // for TDelphiSet
  JclWideStrings;       // for StrLenW, MoveWideChar

type
  { WideFormat uses a finite-state machine to do its parsing. The states are
    represented by the TState type below. The progression from one state to the
    next is determined by the StateTable constant, which combines the previous
    state with the class of the current character (represented by the TCharClass
    type).

    Some anomolies: It's possible to go directly from stDot to one of the
    specifier states, which according to the documentation should be a syntax
    error, but SysUtils.Format accepts it and uses the default -1 for Prec.
    Therefore, there are special stPrecDigit and stPrecStar modes that differ
    from stDigit and stStar by checking for and overriding the default Prec
    value when necessary. }
  TState = (stError, stBeginAcc, stAcc, stPercent, stDigit, stPrecDigit, stStar, stPrecStar, stColon, stDash, stDot, stFloat, stInt, stPointer, stString);
  TCharClass = (ccOther, ccPercent, ccDigit, ccStar, ccColon, ccDash, ccDot, ccSpecF, ccSpecI, ccSpecP, ccSpecS);

const
  WidePercent = WideChar('%');
  WideLittleX = WideChar('x');
  WideSpace = WideChar(' '); // Also defined in JclUnicode

  { This array classifies characters within the range of characters considered
    special to the format syntax. Characters outside the range are all
    classified as ccOther. The value from this table combines with the current
    state to yield the next state, as determined by StateTable below. }
  CharClassTable: array [WidePercent..WideLittleX] of TCharClass = (
    {%}ccPercent, {&}ccOther, {'}ccOther, {(}ccOther, {)}ccOther, {*}ccStar,
    {+}ccOther,   {,}ccOther, {-}ccDash,  {.}ccDot,   {/}ccOther, {0}ccDigit,
    {1}ccDigit,   {2}ccDigit, {3}ccDigit, {4}ccDigit, {5}ccDigit, {6}ccDigit,
    {7}ccDigit,   {8}ccDigit, {9}ccDigit, {:}ccColon, {;}ccOther, {<}ccOther,
    {=}ccOther,   {>}ccOther, {?}ccOther, {@}ccOther, {A}ccOther, {B}ccOther,
    {C}ccOther,   {D}ccSpecI, {E}ccSpecF, {F}ccSpecF, {G}ccSpecF, {H}ccOther,
    {I}ccOther,   {J}ccOther, {K}ccOther, {L}ccOther, {M}ccSpecF, {N}ccSpecF,
    {O}ccOther,   {P}ccSpecP, {Q}ccOther, {R}ccOther, {S}ccSpecS, {T}ccOther,
    {U}ccSpecI,   {V}ccOther, {W}ccOther, {X}ccSpecI, {Y}ccOther, {Z}ccOther,
    {[}ccOther,   {\}ccOther, {]}ccOther, {^}ccOther, {_}ccOther, {`}ccOther,
    {a}ccOther,   {b}ccOther, {c}ccOther, {d}ccSpecI, {e}ccSpecF, {f}ccSpecF,
    {g}ccSpecF,   {h}ccOther, {i}ccOther, {j}ccOther, {k}ccOther, {l}ccOther,
    {m}ccSpecF,   {n}ccSpecF, {o}ccOther, {p}ccSpecP, {q}ccOther, {r}ccOther,
    {s}ccSpecS,   {t}ccOther, {u}ccSpecI, {v}ccOther, {w}ccOther, {x}ccSpecI
  );
  { Given the previous state and the class of the current character, this table
    determines what the next state should be. }
  StateTable: array [TState{old state}, TCharClass{new char}] of TState {new state}= (
    {             ccOther,    ccPercent,  ccDigit,     ccStar,     ccColon,    ccDash,     ccDot,      ccSpecF,    ccSpecI,    ccSpecP,    ccSpecS }
    {stError}    (stBeginAcc, stPercent,  stBeginAcc,  stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc),
    {stBeginAcc} (stAcc,      stPercent,  stAcc,       stAcc,      stAcc,      stAcc,      stAcc,      stAcc,      stAcc,      stAcc,      stAcc),
    {stAcc}      (stAcc,      stPercent,  stAcc,       stAcc,      stAcc,      stAcc,      stAcc,      stAcc,      stAcc,      stAcc,      stAcc),
    {stPercent}  (stError,    stBeginAcc, stDigit,     stStar,     stError,    stDash,     stDot,      stFloat,    stInt,      stPointer,  stString),
    {stDigit}    (stError,    stError,    stDigit,     stError,    stColon,    stError,    stDot,      stFloat,    stInt,      stPointer,  stString),
    {stPrecDigit}(stError,    stError,    stPrecDigit, stError,    stError,    stError,    stError,    stFloat,    stInt,      stPointer,  stString),
    {stStar}     (stError,    stError,    stError,     stError,    stColon,    stError,    stDot,      stFloat,    stInt,      stPointer,  stString),
    {stPrecStar} (stError,    stError,    stError,     stError,    stError,    stError,    stError,    stFloat,    stInt,      stPointer,  stString),
    {stColon}    (stError,    stError,    stDigit,     stStar,     stError,    stDash,     stDot,      stFloat,    stInt,      stPointer,  stString),
    {stDash}     (stError,    stError,    stDigit,     stStar,     stError,    stError,    stDot,      stFloat,    stInt,      stPointer,  stString),
    {stDot}      (stError,    stError,    stPrecDigit, stPrecStar, stError,    stError,    stError,    stFloat,    stInt,      stPointer,  stString),
    {stFloat}    (stBeginAcc, stPercent,  stBeginAcc,  stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc),
    {stInt}      (stBeginAcc, stPercent,  stBeginAcc,  stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc),
    {stPointer}  (stBeginAcc, stPercent,  stBeginAcc,  stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc),
    {stString}   (stBeginAcc, stPercent,  stBeginAcc,  stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc, stBeginAcc)
  );
  { This table is used in converting an ordinal value to a string in either
    decimal or hexadecimal format. }
  ConvertChars: array [0..$f] of WideChar = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');

function FillWideChar(var X; Count: Cardinal; const Value: WideChar): Cardinal; forward;
function GetPClassName(const Cls: TClass): Pointer; forward;
function ConvertInt32(Value: Cardinal; const Base: Cardinal; var Buffer: PWideChar): Cardinal; forward;
function ConvertInt64(Value: Int64; const Base: Cardinal; var Buffer: PWideChar): Cardinal; forward;
procedure SafeNegate32(var Int: Integer); forward;
procedure SafeNegate64(var Int: Int64); forward;

{ Using separate functions for creating exceptions helps to streamline the
  WideFormat code. The stack is not cluttered with space for temporary strings
  and open arrays needed for calling the exceptions' constructors, and the
  function's prologue and epilogue don't execute code for initializing and
  finalizing those hidden stack variables. The extra stack space is thus only
  used in the case when WideFormat actually needs to raise an exception. }
function FormatNoArgumentError(const ArgIndex: Cardinal): Exception; forward;
function FormatNoArgumentErrorEx(const Format: WideString; const FormatStart, FormatEnd, ArgIndex: Cardinal): Exception; forward;
function FormatSyntaxError(const CharIndex: Cardinal): Exception; forward;
function FormatBadArgumentTypeError(const VType: Byte; const ArgIndex: Cardinal; const Allowed: TDelphiSet): Exception; forward;
function FormatBadArgumentTypeErrorEx(const Format: WideString; const FormatStart, FormatEnd: Cardinal; const VType: Byte; const ArgIndex: Cardinal; const Allowed: TDelphiSet): Exception; forward;

function WideFormat(const Format: WideString; const Args: array of const): WideString;
const
  NoPrecision = $ffffffff;
  // For converting strings
  DefaultCodePage = cp_ACP;
  // For converting integers
  MaxIntPrecision = 16;
  // For converting floats
  DefaultGeneralPrecision = 15;
  GeneralDigits = 3;
  DefaultFixedDigits = 2;
  FixedPrecision = 18;
  MaxFloatPrecision = 18;
  // Mostly used for error reporting
  AllowedFloatTypes: TDelphiSet = [vtExtended, vtCurrency{$IFDEF FORMAT_EXTENSIONS}, vtVariant{$ENDIF}];
  AllowedIntegerTypes: TDelphiSet = [vtInteger, vtInt64{$IFDEF FORMAT_EXTENSIONS}, vtVariant{$ENDIF}];
  AllowedStarTypes: TDelphiSet = [vtInteger{$IFDEF FORMAT_EXTENSIONS}, vtInt64, vtVariant{$ENDIF}];
  AllowedPointerTypes: TDelphiSet = [vtPointer{$IFDEF FORMAT_EXTENSIONS}, vtInterface, vtObject, vtPChar, vtPWideChar{$ENDIF}];
  AllowedStringTypes: TDelphiSet = [vtChar, vtWideChar, vtString, vtPChar, vtPWideChar, vtVariant, vtAnsiString, vtWideString{$IFDEF FORMAT_EXTENSIONS}, vtBoolean, vtClass{$ENDIF}];
var
  // Basic parsing values
  State: TState; // Maintain the finite-state machine
  C: WideChar;               // Cache value of Format[Src]
  Src, Dest: Cardinal;       // Indices into Format and Result
  FormatLen: Cardinal;       // Alias for Length(Format)
  ResultLen: Cardinal;       // Alias for Length(Result)
  // Formatting variables
  ArgIndex: Cardinal; // Which argument to read from the Args array
  Arg: PVarRec; // Pointer to current argument
  LeftAlign: Boolean; // Whether the "-" character is present
  Width: Cardinal;
  Prec: Cardinal; // Precision specifier
  PrecWidth: PCardinal; // Reading Prec and Width are similar; this helps consolidate some code.

  FormatStart: Cardinal; // First character of a format string; for error reporting

  P: Pointer; // Pointer to character buffer. Either Wide or Ansi.
  Wide: Boolean; // Tells whether P is PWideChar or PAnsiChar
  CharCount: Cardinal; // How many characters are pointed to by P
  AnsiCount: Cardinal; //
  Buffer: array [0..63] of Byte; // Buffer for numerical conversions
  TempWS: WideString;
  MinWidth, SpacesNeeded: Cardinal;
  // Integer-conversion variables
  Base: Cardinal; // For decimal or hexadecimal
  Neg: Boolean;
  Temp32: Cardinal;
  Temp64: Int64;
  // Float-conversion variables
  ValueType: TFloatValue;
  FloatVal: Pointer;
  FloatFormat: TFloatFormat;
  {$IFDEF FORMAT_EXTENSIONS}
  TempExt: Extended;
  TempCurr: Currency;
  {$ENDIF FORMAT_EXTENSIONS}

  procedure EnsureResultLen(const NeededLen: Cardinal; var AResultLen: Cardinal);
  begin
    if NeededLen > AResultLen then
    begin
      repeat
        AResultLen := AResultLen * 2;
      until NeededLen <= AResultLen;
      SetLength(Result, AResultLen);
    end;
  end;

begin
  FormatLen := Length(Format);
  // Start with an estimated result length
  ResultLen := FormatLen * 4;
  SetLength(Result, ResultLen);
  if FormatLen = 0 then
    Exit;

  Dest := 1;
  State := stError;
  ArgIndex := 0;
  CharCount := 0;

  // Avoid compiler warnings
  LeftAlign := False;
  AnsiCount := 0;
  FormatStart := 0;

  for Src := 1 to FormatLen do
  begin
    C := Format[Src];
    if (Low(CharClassTable) <= C) and (C <= High(CharClassTable)) then
      State := StateTable[State, CharClassTable[C]]
    else
      State := StateTable[State, ccOther];
    case State of
      stError:
        raise FormatSyntaxError(Src); // syntax error at index [Src]
      stBeginAcc:
        begin
          // Begin accumulating characters to copy to Result
          P := @Format[Src];
          CharCount := 1;
        end;
      stAcc:
        Inc(CharCount);
      stPercent:
        begin
          if CharCount > 0 then
          begin
            // Copy accumulated characters into result
            EnsureResultLen(Dest + CharCount - 1, ResultLen);
            MoveWideChar(P^, Result[Dest], CharCount);
            Inc(Dest, CharCount);
            CharCount := 0;
          end;
          // Prepare a new format string
          Width := 0;
          Prec := NoPrecision;
          FormatStart := Src;
          LeftAlign := False;
        end;
      stDigit:
        begin
          // We read into Width, but we might actually be reading the ArgIndex
          // value. If that turns out to be the case, it gets addressed in the
          // stColon state below and Width is reset to its default value, 0.
          Width := Width * 10 + Cardinal(Ord(C) - Ord('0'));
        end;
      stPrecDigit:
        begin
          if Prec = NoPrecision then
            Prec := 0;
          Prec := Prec * 10 + Cardinal(Ord(C) - Ord('0'));
        end;
      stStar, stPrecStar:
        begin
          if ArgIndex > Cardinal(High(Args)) then
            raise FormatNoArgumentError(ArgIndex);
          Arg := @Args[ArgIndex];
          if State = stStar then
            PrecWidth := @Width
          else
            PrecWidth := @Prec;
          // PrecWidth^ := Args[ArgIndex++]
          case Arg^.VType of
            vtInteger:
              PrecWidth^ := Arg^.VInteger;
            {$IFDEF FORMAT_EXTENSIONS}
            vtVariant:
              PrecWidth^ := Arg^.VVariant^;
            vtInt64:
              PrecWidth^ := Arg^.VInt64^;
            {$ENDIF FORMAT_EXTENSIONS}
          else
            raise FormatBadArgumentTypeError(Arg.VType, ArgIndex, AllowedStarTypes);
          end;
          Inc(ArgIndex);
        end;
      stColon:
        begin
          ArgIndex := Width;
          Width := 0;
        end;
      stDash:
        LeftAlign := True;
      stDot: ;
      stFloat, stInt, stPointer, stString:
        begin
          if ArgIndex > Cardinal(High(Args)) then
            raise FormatNoArgumentErrorEx(Format, FormatStart, Src, ArgIndex);
          Arg := @Args[ArgIndex];
          case State of
            stFloat:
              begin
                // The floating-point formats are all similar. The conversion
                // eventually happens in FloatToText.
                case Arg.VType of
                  vtExtended:
                    begin
                      ValueType := fvExtended;
                      FloatVal := Arg.VExtended;
                    end;
                  vtCurrency:
                    begin
                      ValueType := fvCurrency;
                      FloatVal := Arg.VCurrency;
                    end;
                  {$IFDEF FORMAT_EXTENSIONS}
                  vtVariant:
                    begin
                      // We can't give FloatToText a pointer to a Variant, so we
                      // extract the Variant's value and point to a temporary value
                      // instead.
                      if VarType(Arg.VVariant^) and varCurrency <> 0 then
                      begin
                        TempCurr := Arg.VVariant^;
                        FloatVal := @TempCurr;
                        ValueType := fvCurrency;
                      end
                      else
                      begin
                        TempExt := Arg.VVariant^;
                        FloatVal := @TempExt;
                        ValueType := fvExtended;
                      end;
                    end;
                  {$ENDIF FORMAT_EXTENSIONS}
                else
                  raise FormatBadArgumentTypeErrorEx(Format, FormatStart, Src, Arg.VType, ArgIndex, AllowedFloatTypes);
                end; // case Arg.VType
                case C of
                  'e', 'E':
                    FloatFormat := ffExponent;
                  'f', 'F':
                    FloatFormat := ffFixed;
                  'g', 'G':
                    FloatFormat := ffGeneral;
                  'm', 'M':
                    FloatFormat := ffCurrency;
                else {'n', 'N':}
                  FloatFormat := ffNumber;
                end;
                P := @Buffer;
                // Prec is interpeted differently depending on the format.
                if FloatFormat in [ffGeneral, ffExponent] then
                begin
                  if (Prec = NoPrecision) or (Prec > MaxFloatPrecision) then
                    Prec := DefaultGeneralPrecision;
                  AnsiCount := FloatToText(P, FloatVal^, ValueType, FloatFormat, Prec, GeneralDigits);
                end
                else {[ffFixed, ffNumber, ffCurrency]}
                begin
                  if (Prec = NoPrecision) or (Prec > MaxFloatPrecision) then
                  begin
                    if FloatFormat = ffCurrency then
                      Prec := SysUtils.CurrencyDecimals
                    else
                      Prec := DefaultFixedDigits;
                  end;
                  AnsiCount := FloatToText(P, FloatVal^, ValueType, FloatFormat, FixedPrecision, Prec);
                end;
                CharCount := AnsiCount;
                Wide := False;
              end;
            stInt:
              begin
                if (C = 'x') or (C = 'X') then
                  Base := 16
                else
                  Base := 10;
                case Arg^.VType of
                  vtInteger {$IFDEF FORMAT_EXTENSIONS}, vtVariant {$ENDIF}:
                    begin
                      {$IFDEF FORMAT_EXTENSIONS}
                      if Arg^.VType <> vtInteger then
                        Temp32 := Arg^.VVariant^
                      else
                      {$ENDIF FORMAT_EXTENSIONS}
                        Temp32 := Cardinal(Arg^.VInteger);
                      // The value may be signed and negative, but the converter only
                      // interprets unsigned values.
                      Neg := ((C = 'd') or (C = 'D')) and (Integer(Temp32) < 0);
                      if Neg then
                        SafeNegate32(Integer(Temp32));
                      P := @Buffer[High(Buffer)];
                      CharCount := ConvertInt32(Temp32, Base, PWideChar(P));
                    end;
                  vtInt64:
                    begin
                      Temp64 := Arg^.VInt64^;
                      // The value may be signed and negative, but the converter only
                      // interprets unsigned values.
                      Neg := ((C = 'd') or (C = 'D')) and (Temp64 < 0);
                      if Neg then
                        SafeNegate64(Temp64);
                      P := @Buffer[High(Buffer)];
                      CharCount := ConvertInt64(Temp64, Base, PWideChar(P));
                    end;
                else
                  raise FormatBadArgumentTypeErrorEx(Format, FormatStart, Src, Arg.VType, ArgIndex, AllowedIntegerTypes);
                end;
                // If Prec was specified, then we need to see whether any
                // zero-padding is necessary
                if Prec > MaxIntPrecision then
                  Prec := NoPrecision;
                if Prec <> NoPrecision then
                  while Prec > CharCount do
                  begin
                    Dec(PWideChar(P));
                    PWideChar(P)^ := '0';
                    Inc(CharCount);
                  end;
                if Neg then
                begin
                  Dec(PWideChar(P));
                  PWideChar(P)^ := '-';
                  Inc(CharCount);
                end;
                Assert(PWideChar(P) >= @Buffer);
                Wide := True;
              end;
            stPointer:
              begin
                // The workings are similar to the integer-converting code above,
                // but the pointer specifier accepts a few more types that make it
                // worth writing separate code.
                if Arg.VType in AllowedPointerTypes then
                begin
                  P := @Buffer[High(Buffer)];
                  CharCount := ConvertInt32(Cardinal(Arg.VInteger), 16, PWideChar(P));
                end
                else
                  raise FormatBadArgumentTypeErrorEx(Format, FormatStart, Src, Arg.VType, ArgIndex, AllowedPointerTypes);
                // Prec is ignored. Alternatively, it is assumed to be 8
                while (2 * SizeOf(Pointer)) > CharCount do
                begin
                  Dec(PWideChar(P));
                  PWideChar(P)^ := '0';
                  Inc(CharCount);
                end;
                Assert(PWideChar(P) >= @Buffer);
                Wide := True;
              end; // stPointer case
          else {stString:}
            begin
              Wide := Arg^.VType in [vtWideChar, vtPWideChar, vtBoolean, vtVariant, vtWideString];
              case Arg^.VType of
                vtChar, vtWideChar:
                  begin
                    Assert(@Arg^.VChar = @Arg^.VWideChar);
                    P := @Arg^.VChar;
                    CharCount := 1;
                  end;
                vtString:
                  begin
                    CharCount := Length(Arg^.VString^);
                    P := @Arg^.VString^[1];
                  end;
                vtPChar, vtPWideChar:
                  begin
                    P := Arg^.VPChar;
                    if Wide then
                      CharCount := StrLenW(P)
                    else
                      CharCount := StrLen(P);
                  end;
                vtVariant{$IFDEF FORMAT_EXTENSIONS}, vtBoolean{$ENDIF}:
                  begin
                    {$IFDEF FORMAT_EXTENSIONS}
                    if Arg^.VType = vtBoolean then
                      TempWS := BooleanToStr(Arg^.VBoolean)
                    else
                    {$ENDIF FORMAT_EXTENSIONS}
                      TempWS := Arg^.VVariant^;
                    CharCount := Length(TempWS);
                    P := Pointer(TempWS);
                  end;
                {$IFDEF FORMAT_EXTENSIONS}
                vtClass:
                  begin
                    P := GetPClassName(Arg^.VClass);
                    CharCount := PByte(P)^;
                    Inc(PAnsiChar(P));
                  end;
                {$ENDIF FORMAT_EXTENSIONS}
                vtAnsiString, vtWideString:
                  begin
                    P := Arg^.VAnsiString;
                    if Wide then
                      CharCount := Length(WideString(P))
                    else
                      CharCount := Length(AnsiString(P));
                  end;
              else
                raise FormatBadArgumentTypeErrorEx(Format, FormatStart, Src, Arg.VType, ArgIndex, AllowedStringTypes);
              end;
              // We want the length in WideChars, not AnsiChars; they aren't
              // necessarily the same.
              if (not Wide) and (CharCount > 0) then
              begin
                AnsiCount := CharCount;
                CharCount := MultibyteToWideChar(DefaultCodePage, 0, P, AnsiCount, nil, 0);
              end;
              // For strings, Prec can only truncate, never lengthen.
              if Prec < CharCount then
                CharCount := Prec;
            end; // stString case
          end; // case State

          Inc(ArgIndex);
          if Integer(Width) < 0 then
            Width := 0;

          // This code prepares for the buffer-copying code.
          MinWidth := CharCount;
          if Width > MinWidth then
            SpacesNeeded := Width - MinWidth
          else
            SpacesNeeded := 0;
          EnsureResultLen(Dest - 1 + MinWidth + SpacesNeeded, ResultLen);

          // This code fills the resultant buffer.
          if (SpacesNeeded > 0) and not LeftAlign then
            Inc(Dest, FillWideChar(Result[Dest], SpacesNeeded, WideSpace));
          if Wide then
            MoveWideChar(P^, Result[Dest], CharCount)
          else
            MultibyteToWideChar(DefaultCodePage, 0, P, AnsiCount, @Result[Dest], CharCount);
          Inc(Dest, CharCount);
          CharCount := 0;
          if (SpacesNeeded > 0) and LeftAlign then
            Inc(Dest, FillWideChar(Result[Dest], SpacesNeeded, WideSpace));
        end; // case stFloat, stInt, stPointer, stString
    end; // case
  end; // for
  if CharCount > 0 then
  begin
    // Copy accumulated characters into result
    SetLength(Result, Dest + CharCount - 1);
    MoveWideChar(P^, Result[Dest], CharCount);
  end
  else
    if ResultLen >= Dest then
      SetLength(Result, Dest - 1);
end;

function FillWideChar(var X; Count: Cardinal; const Value: WideChar): Cardinal;
var
  PW: PWideChar;
begin
  Result := Count;
  PW := @X;
  for Count := Count downto 1 do
  begin
    PW^ := Value;
    Inc(PW);
  end;
end;

{ GetPClassName is similar to calling Cls.ClassName, but avoids the necessary
  memory copy inherent in the function call. It also avoids a conversion from
  ShortString to AnsiString, which would happen when the function's result got
  type cast to PChar. Since all we really need is a pointer to the first byte
  of the string, the bytes in the VMT are just as good as the bytes in a normal
  AnsiString. }
function GetPClassName(const Cls: TClass): Pointer;
asm
        mov    eax, [eax].vmtClassName
  // Result := JclSysUtils.GetVirtualMethod(Cls, vmtClassName div SizeOf(Pointer));
end;

function ModDiv32(const Dividend, Divisor: Cardinal; out Quotient: Cardinal): Cardinal;
// Returns the quotient and modulus of the two inputs
// Quotient := Dividend div Divisor;
// Result := Dividend mod Divisor;
asm
        push   ecx
        mov    ecx, edx
        xor    edx, edx
        div    ecx
        pop    ecx
        mov    [ecx], eax
        mov    eax, edx
end;

function ConvertInt32(Value: Cardinal; const Base: Cardinal; var Buffer: PWideChar): Cardinal;
// Buffer: Pointer to the END of the buffer to be filled. Upon return, Buffer
//  will point to the first character in the string. The buffer will NOT be
//  null-terminated.
// Result: Number of characters filled in buffer
begin
  Result := 0;
  repeat
    Inc(Result);
    Dec(Buffer);
    Buffer^ := ConvertChars[ModDiv32(Value, Base, Value)];
  until Value = 0;
end;

function ModDiv64(var Dividend: Int64; const Divisor: Cardinal; out Quotient: Int64): Int64;
{ Returns the quotient and modulus of the two inputs using unsigned division
  Unsigned 64-bit division is not available in Delphi 5, but the System unit
  does provide division and modulus functions.
  Quotient := Dividend div Divisor;
  Result := Dividend mod Divisor; }
asm
        push   0 // prepare for second division
        push   edx

        push   dword ptr [eax] // save dividend
        push   dword ptr [eax+4]

        push   ecx // save quotient

        push   0 // prepare for first division
        push   edx
        mov    edx, [eax+4]
        mov    eax, [eax]
        call   System.@_lludiv
        pop    ecx // restore quotient
        mov    [ecx], eax // store quotient
        mov    [ecx+4], edx

        pop    edx // restore dividend
        pop    eax
        call   System.@_llumod
end;

function ConvertInt64(Value: Int64; const Base: Cardinal; var Buffer: PWideChar): Cardinal;
{ Result: Number of characters filled in buffer
  Buffer: Pointer to first valid character in buffer
  Written in assembler to use unsigned division instead of signed. Otherwise,
  the code would be exactly the same as for ConvertInt32. }
begin
  Result := 0;
  repeat
    Inc(Result);
    Dec(Buffer);
    Buffer^ := ConvertChars[ModDiv64(Value, Base, Value)];
  until Value = 0;
end;

{ The compiler's overflow checking must be disabled for the following two
  procedures. These compiler directives temporarily disable overflow checking
  for just these two routines. For the rest of the code in this unit, overflow
  isn't relevant. }

{$Q-}

// These functions negate integers without the danger of overflow errors.
procedure SafeNegate32(var Int: Integer);
begin
  Int := -Int;
end;

procedure SafeNegate64(var Int: Int64);
begin
  Int := -Int;
end;

{$IFDEF OVERFLOWCHECKS_ON}
{$Q+}
{$ENDIF OVERFLOWCHECKS_ON}

resourcestring
  RsFormatSyntaxError = 'Syntax error at index %u';
  RsFormatNoArgument = 'No argument at index %u';
  RsFormatBadArgumentType = 'Invalid argument type (%s) at index %u. Expected [%s]';
  RsFormatBadArgumentTypeEx = 'Invalid argument type (%s) at index %u for format ''%s''. Expected [%s]';
  RsFormatNoArgumentEx = 'No argument at index %u for format ''%s''';

function FormatNoArgumentError(const ArgIndex: Cardinal): Exception;
begin
  Result := EConvertError.CreateResFmt(PResStringRec(@RsFormatNoArgument), [ArgIndex]);
end;

function FormatNoArgumentErrorEx(const Format: WideString; const FormatStart, FormatEnd, ArgIndex: Cardinal): Exception;
begin
  Result := EConvertError.CreateResFmt(PResStringRec(@RsFormatNoArgumentEx), [ArgIndex, Copy(Format, FormatStart, FormatStart - FormatEnd + 1)]);
end;

function FormatSyntaxError(const CharIndex: Cardinal): Exception;
begin
  Result := EConvertError.CreateResFmt(PResStringRec(@RsFormatSyntaxError), [CharIndex]);
end;

const
  VarRecTypes: array [vtInteger..vtInt64] of PChar = (
    'Integer', 'Boolean', 'Char', 'Extended', 'ShortString', 'Pointer', 'PChar',
    'TObject', 'TClass', 'WideChar', 'PWideChar', 'AnsiString', 'Currency',
    'Variant', 'IUnknown', 'WideString', 'Int64'
  );

function GetTypeList(const Types: TDelphiSet): string;
var
  T: Byte;
  List: TStrings;
begin
  List := TStringList.Create;
  try
    for T := Low(VarRecTypes) to High(VarRecTypes) do
    begin
      if T in Types then
        List.Add(VarRecTypes[T]);
    end;
    Result := List.CommaText;
  finally
    List.Free;
  end;
end;

function FormatBadArgumentTypeError(const VType: Byte; const ArgIndex: Cardinal; const Allowed: TDelphiSet): Exception;
var
  FoundType, AllowedTypes: string;
begin
  FoundType := VarRecTypes[VType];
  AllowedTypes := GetTypeList(Allowed);
  Result := EConvertError.CreateResFmt(PResStringRec(@RsFormatBadArgumentType), [FoundType, ArgIndex, AllowedTypes]);
end;

function FormatBadArgumentTypeErrorEx(const Format: WideString; const FormatStart, FormatEnd: Cardinal; const VType: Byte; const ArgIndex: Cardinal; const Allowed: TDelphiSet): Exception;
var
  FoundType, AllowedTypes: string;
begin
  FoundType := VarRecTypes[VType];
  AllowedTypes := GetTypeList(Allowed);
  Result := EConvertError.CreateResFmt(PResStringRec(@RsFormatBadArgumentTypeEx), [FoundType, ArgIndex, Copy(Format, FormatStart, FormatEnd - FormatStart + 1), AllowedTypes]);
end;

// History:

// $Log$
// Revision 1.3  2005/02/24 07:36:25  marquardt
// resolved the compiler warnings, style cleanup, removed code from JclContainerIntf.pas
//
// Revision 1.2  2005/02/22 07:55:18  rrossmair
// - issue #2662 fixed (internal error C6662 when compiling with D2005)
//
// Revision 1.1  2005/02/14 00:45:50  rrossmair
// - initial check-in
//

end.
