{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JclLogic.pas.                                           }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: December 10, 2000                                             }
{                                                                              }
{******************************************************************************}

unit JclLogic;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

//------------------------------------------------------------------------------
// Conversion
//------------------------------------------------------------------------------

function OrdToBinary(const Value: Byte): string; overload;
function OrdToBinary(const Value: Word): string; overload;
function OrdToBinary(const Value: Integer): string; overload;
function OrdToBinary(const Value: Cardinal): string; overload;
function OrdToBinary(const Value: Int64): string; overload;

//------------------------------------------------------------------------------
// Bit manipulation
//------------------------------------------------------------------------------

type
  TBitRange = Byte;
  TBooleanArray = array of Boolean;

// TODOC TOTEST Added new overloads

function BitsHighest(X: Byte): Integer; overload;
function BitsHighest(X: Word): Integer; overload;
function BitsHighest(X: SmallInt): Integer; overload;
function BitsHighest(X: ShortInt): Integer; overload;
function BitsHighest(X: Integer): Integer; overload;
function BitsHighest(X: Cardinal): Integer; assembler; overload;
function BitsHighest(X: Int64): Integer; overload;

function BitsLowest(X: Byte): Integer; overload;
function BitsLowest(X: Word): Integer; overload;
function BitsLowest(X: Integer): Integer; overload;
function BitsLowest(X: Cardinal): Integer; assembler; overload;
function BitsLowest(X: Int64): Integer; overload;

function ClearBit(const Value: Byte; const Bit: TBitRange): Byte; overload;
function ClearBit(const Value: Word; const Bit: TBitRange): Word; overload;
function ClearBit(const Value: Integer; const Bit: TBitRange): Integer; overload;
function ClearBit(const Value: Int64; const Bit: TBitRange): Int64; overload;

// TODOC TOTEST Added new overloads

function CountBitsSet(X: Byte): Integer; assembler; overload;
function CountBitsSet(X: Word): Integer; assembler; overload;
function CountBitsSet(X: Smallint): Integer; overload;
function CountBitsSet(X: ShortInt): Integer; overload;
function CountBitsSet(X: Integer): Integer; overload;
function CountBitsSet(X: Cardinal): Integer; assembler; overload;
function CountBitsSet(X: Int64): Integer; overload;

function CountBitsCleared(X: Byte): Integer; overload;
function CountBitsCleared(X: Word): Integer; overload;
function CountBitsCleared(X: Integer): Integer; overload;
function CountBitsCleared(X: Int64): Integer; overload;

function LRot(const Value: Byte; const Count: TBitRange): Byte; overload;
function LRot(const Value: Word; const Count: TBitRange): Word; overload;
function LRot(const Value: Integer; const Count: TBitRange): Integer; overload;

function ReverseBits(const Value: Byte): Byte; overload;
function ReverseBits(const Value: Word): Word; overload;
function ReverseBits(const Value: Integer): Integer; overload;
function ReverseBits(const Value: Int64): Int64; overload;
function ReverseBits(P: Pointer; Count: Integer): Pointer; overload;

function RRot(const Value: Byte; const Count: TBitRange): Byte; overload;
function RRot(const Value: Word; const Count: TBitRange): Word; overload;
function RRot(const Value: Integer; const Count: TBitRange): Integer; overload;

function Sar(const Value: Shortint; const Count: TBitRange): Shortint; overload;
function Sar(const Value: Smallint; const Count: TBitRange): Smallint; overload;
function Sar(const Value: Integer; const Count: TBitRange): Integer; overload;

function SetBit(const Value: Byte; const Bit: TBitRange): Byte; overload;
function SetBit(const Value: Word; const Bit: TBitRange): Word; overload;
function SetBit(const Value: Integer; const Bit: TBitRange): Integer; overload;
function SetBit(const Value: Int64; const Bit: TBitRange): Int64; overload;

function TestBit(const Value: Byte; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: Word; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: Integer; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: Int64; const Bit: TBitRange): Boolean; overload;

function TestBits(const Value, Mask: Byte): Boolean; overload;
function TestBits(const Value, Mask: Word): Boolean; overload;
function TestBits(const Value, Mask: Integer): Boolean; overload;
function TestBits(const Value, Mask: Int64): Boolean; overload;

function ToggleBit(const Value: Byte; const Bit: TBitRange): Byte; overload;
function ToggleBit(const Value: Word; const Bit: TBitRange): Word; overload;
function ToggleBit(const Value: Integer; const Bit: TBitRange): Integer; overload;
function ToggleBit(const Value: Int64; const Bit: TBitRange): Int64; overload;

procedure BooleansToBits(var Dest: Byte; const B: array of Boolean); overload;
procedure BooleansToBits(var Dest: Word; const B: array of Boolean); overload;
procedure BooleansToBits(var Dest: Integer; const B: array of Boolean); overload;
procedure BooleansToBits(var Dest: Int64; const B: array of Boolean); overload;

procedure BitsToBooleans(const Bits: Byte; var B: TBooleanArray; AllBits: Boolean {$IFDEF SUPPORTS_DEFAULTPARAMS}= False{$ENDIF}); overload;
procedure BitsToBooleans(const Bits: Word; var B: TBooleanArray; AllBits: Boolean {$IFDEF SUPPORTS_DEFAULTPARAMS}= False{$ENDIF}); overload;
procedure BitsToBooleans(const Bits: Integer; var B: TBooleanArray; AllBits: Boolean {$IFDEF SUPPORTS_DEFAULTPARAMS}= False{$ENDIF}); overload;
procedure BitsToBooleans(const Bits: Int64; var B: TBooleanArray; AllBits: Boolean {$IFDEF SUPPORTS_DEFAULTPARAMS}= False{$ENDIF}); overload;

function BitsNeeded(const X: Byte): Integer; overload;
function BitsNeeded(const X: Word): Integer; overload;
function BitsNeeded(const X: Integer): Integer; overload;
function BitsNeeded(const X: Int64): Integer; overload;

function Digits(const X: Cardinal): Integer;

//------------------------------------------------------------------------------
// Arithmetic
//------------------------------------------------------------------------------

procedure SwapOrd(var I, J: Byte); overload;
procedure SwapOrd(var I, J: Shortint); overload;
procedure SwapOrd(var I, J: Smallint); overload;
procedure SwapOrd(var I, J: Word); overload;
procedure SwapOrd(var I, J: Integer); overload;
procedure SwapOrd(var I, J: Cardinal); overload;
procedure SwapOrd(var I, J: Int64); overload;

function IncLimit(var B: Byte; const Limit: Byte; const Incr: Byte {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Byte; overload;
function IncLimit(var B: Shortint; const Limit: Shortint; const Incr: Shortint {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Shortint; overload;
function IncLimit(var B: Word; const Limit: Word; const Incr: Word {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Word; overload;
function IncLimit(var B: Integer; const Limit: Integer; const Incr: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Integer; overload;
function IncLimit(var B: Cardinal; const Limit: Cardinal; const Incr: Cardinal {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Cardinal; overload;
function IncLimit(var B: Int64; const Limit: Int64; const Incr: Int64 {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Int64; overload;

function DecLimit(var B: Byte; const Limit: Byte; const Decr: Byte {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Byte; overload;
function DecLimit(var B: Shortint; const Limit: Shortint; const Decr: Shortint {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Shortint; overload;
function DecLimit(var B: Word; const Limit: Word; const Decr: Word {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Word; overload;
function DecLimit(var B: Integer; const Limit: Integer; const Decr: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Integer; overload;
function DecLimit(var B: Cardinal; const Limit: Cardinal; const Decr: Cardinal {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Cardinal; overload;
function DecLimit(var B: Int64; const Limit: Int64; const Decr: Int64 {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Int64; overload;

function IncLimitClamp(var B: Byte; const Limit: Byte; const Incr: Byte {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Byte; overload;
function IncLimitClamp(var B: Shortint; const Limit: Shortint; const Incr: Shortint {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Shortint; overload;
function IncLimitClamp(var B: Word; const Limit: Word; const Incr: Word {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Word; overload;
function IncLimitClamp(var B: Integer; const Limit: Integer; const Incr: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Integer; overload;
function IncLimitClamp(var B: Cardinal; const Limit: Cardinal; const Incr: Cardinal {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Cardinal; overload;
function IncLimitClamp(var B: Int64; const Limit: Int64; const Incr: Int64 {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Int64; overload;

function DecLimitClamp(var B: Byte; const Limit: Byte; const Decr: Byte {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Byte; overload;
function DecLimitClamp(var B: Shortint; const Limit: Shortint; const Decr: Shortint {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Shortint; overload;
function DecLimitClamp(var B: Word; const Limit: Word; const Decr: Word {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Word; overload;
function DecLimitClamp(var B: Integer; const Limit: Integer; const Decr: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Integer; overload;
function DecLimitClamp(var B: Cardinal; const Limit: Cardinal; const Decr: Cardinal {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Cardinal; overload;
function DecLimitClamp(var B: Int64; const Limit: Int64; const Decr: Int64 {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}): Int64; overload;

function Max(const B1, B2: Byte): Byte; overload;
function Max(const B1, B2: Shortint): Shortint; overload;
function Max(const B1, B2: Word): Word; overload;
function Max(const B1, B2: Integer): Integer; overload;
function Max(const B1, B2: Cardinal): Cardinal; overload;
function Max(const B1, B2: Int64): Int64; overload;

function Min(const B1, B2: Byte): Byte; overload;
function Min(const B1, B2: Shortint): Shortint; overload;
function Min(const B1, B2: Word): Word; overload;
function Min(const B1, B2: Integer): Integer; overload;
function Min(const B1, B2: Cardinal): Cardinal; overload;
function Min(const B1, B2: Int64): Int64; overload;

implementation

uses
  {$IFDEF WIN32}
  Windows, { for PByte }
  {$ENDIF}
  JclBase, JclResources;

//==============================================================================
// Conversion
//==============================================================================

function OrdToBinary(const Value: Byte): string;
var
  I: Integer;
  B: Byte;
  P: PChar;
begin
  SetLength(Result, 8);
  P := PChar(Result) + (7 * SizeOf(Char));
  B := Value;
  for I := 0 to 7 do
  begin
    P^ := Chr(48 + (B and $00000001));
    Dec(P);
    B := B shr 1;
  end;
end;

//------------------------------------------------------------------------------

function OrdToBinary(const Value: Word): string;
var
  I: Integer;
  W: Word;
  P: PChar;
begin
  SetLength(Result, 16);
  P := PChar(Result) + (15 * SizeOf(Char));
  W := Value;
  for I := 0 to 15 do
  begin
    P^ := Chr(48 + (W and $00000001));
    Dec(P);
    W := W shr 1;
  end;
end;

//------------------------------------------------------------------------------

function OrdToBinary(const Value: Integer): string;
var
  I, J: Integer;
  P: PChar;
begin
  SetLength(Result, 32);
  P := PChar(Result) + (31 * SizeOf(Char));
  J := Value;
  for I := 0 to 31 do
  begin
    P^ := Chr(48 + (J and $00000001));
    Dec(P);
    J := J shr 1;
  end;
end;

//------------------------------------------------------------------------------

function OrdToBinary(const Value: Cardinal): string;
var
  I: Integer;
  J: Cardinal;
  P: PChar;
begin
  SetLength(Result, 32);
  P := PChar(Result) + (31 * SizeOf(Char));
  J := Value;
  for I := 0 to 31 do
  begin
    P^ := Chr(48 + (J and $00000001));
    Dec(P);
    J := J shr 1;
  end;
end;

//------------------------------------------------------------------------------

function OrdToBinary(const Value: Int64): string;
var
  I: Integer;
  I64: Int64;
  P: PChar;
begin
  SetLength(Result, 64);
  P := PChar(Result) + (63 * SizeOf(Char));
  I64 := Value;
  for I := 0 to 63 do
  begin
    P^ := Chr(48 + (I64 and Int64(1)));
    Dec(P);
    I64 := I64 shr Int64(1);
  end;
end;


//==============================================================================
// Bit manipulation
//==============================================================================

function BitsHighest(X: Cardinal): Integer; assembler; overload;
asm
        MOV     ECX, EAX
        MOV     EAX, -1
        BSR     EAX, ECX
end;

//------------------------------------------------------------------------------

function BitsHighest(X: Integer): Integer; overload;
begin
  Result := BitsHighest(Cardinal(X));
end;

//------------------------------------------------------------------------------

function BitsHighest(X: Byte): Integer; overload;
begin
  Result := BitsHighest(Cardinal(X) and $FF);
end;

//------------------------------------------------------------------------------

function BitsHighest(X: Word): Integer; overload;
begin
  Result := BitsHighest(Cardinal(X) and $FFFF);
end;

//------------------------------------------------------------------------------

function BitsHighest(X: SmallInt): Integer; overload;
begin
  Result := BitsHighest(Word(X));
end;


//------------------------------------------------------------------------------

function BitsHighest(X: ShortInt): Integer; overload;
begin
  Result := BitsHighest(Cardinal(Byte(X)));
end;

//------------------------------------------------------------------------------

function BitsHighest(X: Int64): Integer; overload;
begin
  if TLargeInteger(X).HighPart = 0 then
  begin
    if TLargeInteger(X).LowPart = 0 then
      Result := -1
    else
      Result := BitsHighest(TLargeInteger(X).LowPart);
  end
  else
    Result := BitsHighest(TLargeInteger(X).HighPart) + 32;
end;

//------------------------------------------------------------------------------

function BitsLowest(X: Cardinal): Integer; assembler; overload;
asm
        MOV     ECX, EAX
        MOV     EAX, -1
        BSF     EAX, ECX
end;

//------------------------------------------------------------------------------

function BitsLowest(X: Byte): Integer; overload;
begin
  Result := BitsLowest(Cardinal(X) and $FF);
end;

//------------------------------------------------------------------------------

function BitsLowest(X: Word): Integer; overload;
begin
  Result := BitsLowest(Cardinal(X) and $FFFF);
end;

//------------------------------------------------------------------------------

function BitsLowest(X: Integer): Integer; overload;
begin
  Result := BitsLowest(Cardinal(X));
end;

//------------------------------------------------------------------------------

function BitsLowest(X: Int64): Integer; overload;
begin
  if TLargeInteger(X).LowPart = 0 then
  begin
    if TLargeInteger(X).HighPart = 0 then
      Result := -1
    else
      Result := BitsLowest(TLargeInteger(X).HighPart) + 32;
  end
  else
    Result := BitsLowest(TLargeInteger(X).LowPart);
end;

//------------------------------------------------------------------------------

function ClearBit(const Value: Byte; const Bit: TBitRange): Byte;
begin
  Result := Value and not (1 shl (Bit mod 8));
end;

//------------------------------------------------------------------------------

function ClearBit(const Value: Word; const Bit: TBitRange): Word;
begin
  Result := Value and not (1 shl (Bit mod 16));
end;

//------------------------------------------------------------------------------

function ClearBit(const Value: Integer; const Bit: TBitRange): Integer;
begin
  Result := Value and not (1 shl (Bit mod 32));
end;

//------------------------------------------------------------------------------

function ClearBit(const Value: Int64; const Bit: TBitRange): Int64;
begin
  Result := Value and not (Int64(1) shl (Bit mod 64));
end;

//------------------------------------------------------------------------------

function CountBitsSet(X: Cardinal): Integer; assembler; overload;
asm
        MOV     ECX, 32
        XOR     EDX, EDX
@COUNTBITS:
        SHR     EAX, 1
        ADC     DL, 0
        LOOP    @COUNTBITS
        MOV     EAX, EDX
end;

//------------------------------------------------------------------------------

function CountBitsSet(X: Byte): Integer; assembler; overload;
asm
        MOV     ECX, 8
        XOR     EDX, EDX
@CountBits:
        SHR     AL, 1
        ADC     DL, 0
        LOOP    @CountBits
        MOV     EAX, EDX
end;

//------------------------------------------------------------------------------

function CountBitsSet(X: Word): Integer; assembler; overload;
asm
        MOV     ECX, 16
        XOR     EDX, EDX
@CountBits:
        SHR     AX, 1
        ADC     DL, 0
        LOOP    @CountBits
        MOV     EAX, EDX
end;

//------------------------------------------------------------------------------

function CountBitsSet(X: Smallint): Integer; overload;
begin
  Result := CountBitsSet(Word(X));
end;

//------------------------------------------------------------------------------

function CountBitsSet(X: ShortInt): Integer; overload;
begin
  Result := CountBitsSet(Byte(X));
end;

//------------------------------------------------------------------------------

function CountBitsSet(X: Integer): Integer; overload;
begin
  Result := CountBitsSet(Cardinal(X));
end;

//------------------------------------------------------------------------------

function CountBitsSet(X: Int64): Integer; overload;
begin
  Result := CountBitsSet(TLargeInteger(X).LowPart) + CountBitsSet(TLargeInteger(X).HighPart);
end;

//------------------------------------------------------------------------------

function CountBitsCleared(X: Byte): Integer; overload;
begin
  Result := 8 - CountBitsSet(Cardinal(X));
end;

//------------------------------------------------------------------------------

function CountBitsCleared(X: Word): Integer; overload;
begin
  Result := 16 - CountBitsSet(Cardinal(X));
end;

//------------------------------------------------------------------------------

function CountBitsCleared(X: Integer): Integer; overload;
begin
  Result := 32 - CountBitsSet(Cardinal(X));
end;

//------------------------------------------------------------------------------

function CountBitsCleared(X: Int64): Integer; overload;
begin
  Result := 64 - CountBitsSet(Cardinal(X));
end;

//------------------------------------------------------------------------------

function LRot(const Value: Byte; const Count: TBitRange): Byte; assembler;
asm
        MOV     CL, Count
        MOV     AL, Value
        ROL     AL, CL
        MOV     Result, AL
end;

//------------------------------------------------------------------------------

function LRot(const Value: Word; const Count: TBitRange): Word; assembler;
asm
        MOV     CL, Count
        MOV     AX, Value
        ROL     AX, CL
        MOV     Result, AX
end;

//------------------------------------------------------------------------------

function LRot(const Value: Integer; const Count: TBitRange): Integer; assembler;
asm
        MOV     CL, Count
        MOV     EAX, Value
        ROL     EAX, CL
        MOV     Result, EAX
end;

//------------------------------------------------------------------------------

function ReverseBits(const Value: Byte): Byte;
var
  B, I: Byte;
begin
  Result := 0;
  B := Value;
  for I := 0 to 7 do
  begin
    Result := (Result shl 1) or (B and $01);
    B := B shr 1;
  end;
end;

//------------------------------------------------------------------------------

function ReverseBits(const Value: Word): Word;
var
  I: Byte;
  W: Word;
begin
  Result := 0;
  W := Value;
  for I := 0 to 15 do
  begin
    Result := (Result shl 1) or (W and $0001);
    W := W shr 1;
  end;
end;

//------------------------------------------------------------------------------

function ReverseBits(const Value: Integer): Integer;
var
  I: Byte;
  J: Integer;
begin
  Result := 0;
  J := Value;
  for I := 0 to 31 do
  begin
    Result := (Result shl 1) or (J and $00000001);
    J := J shr 1;
  end;
end;

//------------------------------------------------------------------------------


function ReverseBits(const Value: Int64): Int64;
var
  I: Byte;
  I64: Int64;
begin
  Result := 0;
  I64 := Value;
  for I := 0 to 63 do
  begin
    Result := (Result shl Int64(1)) or (I64 and Int64(1));
    I64 := I64 shr Int64(1);
  end;
end;

//------------------------------------------------------------------------------

const
  ReverseTable: array [0..255] of Byte = (
    $00, $80, $40, $C0, $20, $A0, $60, $E0,
    $10, $90, $50, $D0, $30, $B0, $70, $F0,
    $08, $88, $48, $C8, $28, $A8, $68, $E8,
    $18, $98, $58, $D8, $38, $B8, $78, $F8,
    $04, $84, $44, $C4, $24, $A4, $64, $E4,
    $14, $94, $54, $D4, $34, $B4, $74, $F4,
    $0C, $8C, $4C, $CC, $2C, $AC, $6C, $EC,
    $1C, $9C, $5C, $DC, $3C, $BC, $7C, $FC,
    $02, $82, $42, $C2, $22, $A2, $62, $E2,
    $12, $92, $52, $D2, $32, $B2, $72, $F2,
    $0A, $8A, $4A, $CA, $2A, $AA, $6A, $EA,
    $1A, $9A, $5A, $DA, $3A, $BA, $7A, $FA,
    $06, $86, $46, $C6, $26, $A6, $66, $E6,
    $16, $96, $56, $D6, $36, $B6, $76, $F6,
    $0E, $8E, $4E, $CE, $2E, $AE, $6E, $EE,
    $1E, $9E, $5E, $DE, $3E, $BE, $7E, $FE,
    $01, $81, $41, $C1, $21, $A1, $61, $E1,
    $11, $91, $51, $D1, $31, $B1, $71, $F1,
    $09, $89, $49, $C9, $29, $A9, $69, $E9,
    $19, $99, $59, $D9, $39, $B9, $79, $F9,
    $05, $85, $45, $C5, $25, $A5, $65, $E5,
    $15, $95, $55, $D5, $35, $B5, $75, $F5,
    $0D, $8D, $4D, $CD, $2D, $AD, $6D, $ED,
    $1D, $9D, $5D, $DD, $3D, $BD, $7D, $FD,
    $03, $83, $43, $C3, $23, $A3, $63, $E3,
    $13, $93, $53, $D3, $33, $B3, $73, $F3,
    $0B, $8B, $4B, $CB, $2B, $AB, $6B, $EB,
    $1B, $9B, $5B, $DB, $3B, $BB, $7B, $FB,
    $07, $87, $47, $C7, $27, $A7, $67, $E7,
    $17, $97, $57, $D7, $37, $B7, $77, $F7,
    $0F, $8F, $4F, $CF, $2F, $AF, $6F, $EF,
    $1F, $9F, $5F, $DF, $3F, $BF, $7F, $FF);

function ReverseBits(P: Pointer; Count: Integer): Pointer;
var
  P1, P2: PByte;
  T: Byte;
begin
  if (P <> nil) and (Count > 0) then
  begin
    P1 := P;
    P2 := PByte(Integer(P) + Count - 1);
    while Integer(P1) < Integer(P2) do
    begin
      T := ReverseTable[P1^];
      P1^ := ReverseTable[P2^];
      P2^ := T;
      Inc(P1);
      Dec(P2);
    end;
    if (P1 = P2) then
      P1^ := ReverseTable[P1^];
  end;
  Result := P;
end;

//------------------------------------------------------------------------------

function RRot(const Value: Byte; const Count: TBitRange): Byte; assembler;
asm
        MOV     CL, Count
        MOV     AL, Value
        ROR     AL, CL
        MOV     Result, AL
end;

//------------------------------------------------------------------------------

function RRot(const Value: Word; const Count: TBitRange): Word; assembler;
asm
        MOV     CL, Count
        MOV     AX, Value
        ROR     AX, CL
        MOV     Result, AX
end;

//------------------------------------------------------------------------------

function RRot(const Value: Integer; const Count: TBitRange): Integer; assembler;
asm
        MOV     CL, Count
        MOV     EAX, Value
        ROR     EAX, CL
        MOV     Result, EAX
end;

//------------------------------------------------------------------------------

function Sar(const Value: Shortint; const Count: TBitRange): Shortint; assembler;
asm
        MOV     CL, DL
        SAR     AL, CL
end;

//------------------------------------------------------------------------------

function Sar(const Value: Smallint; const Count: TBitRange): Smallint; assembler;
asm
        MOV     CL, DL
        SAR     AX, CL
end;

//------------------------------------------------------------------------------

function Sar(const Value: Integer; const Count: TBitRange): Integer; assembler;
asm
        MOV     CL, DL
        SAR     EAX, CL
end;

//------------------------------------------------------------------------------

function SetBit(const Value: Byte; const Bit: TBitRange): Byte;
begin
  Result := Value or (1 shl (Bit mod 8));
end;

//------------------------------------------------------------------------------

function SetBit(const Value: Word; const Bit: TBitRange): Word;
begin
  Result := Value or (1 shl (Bit mod 16));
end;

//------------------------------------------------------------------------------

function SetBit(const Value: Integer; const Bit: TBitRange): Integer;
begin
  Result := Value or (1 shl (Bit mod 32));
end;

//------------------------------------------------------------------------------

function SetBit(const Value: Int64; const Bit: TBitRange): Int64;
begin
  Result := Value or (Int64(1) shl (Bit mod 64));
end;

//------------------------------------------------------------------------------

function TestBit(const Value: Byte; const Bit: TBitRange): Boolean;
begin
  Result := (Value and (1 shl (Bit mod 8))) <> 0;
end;

//------------------------------------------------------------------------------

function TestBit(const Value: Word; const Bit: TBitRange): Boolean;
begin
  Result := (Value and (1 shl (Bit mod 16))) <> 0;
end;

//------------------------------------------------------------------------------

function TestBit(const Value: Integer; const Bit: TBitRange): Boolean;
begin
  Result := (Value and (1 shl (Bit mod 32))) <> 0;
end;

//------------------------------------------------------------------------------

function TestBit(const Value: Int64; const Bit: TBitRange): Boolean;
begin
  Result := (Value and (Int64(1) shl (Bit mod 64))) <> 0;
end;


//------------------------------------------------------------------------------

function TestBits(const Value, Mask: Byte): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

//------------------------------------------------------------------------------

function TestBits(const Value, Mask: Word): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

//------------------------------------------------------------------------------

function TestBits(const Value, Mask: Integer): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

//------------------------------------------------------------------------------

function TestBits(const Value, Mask: Int64): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

//------------------------------------------------------------------------------

function ToggleBit(const Value: Byte; const Bit: TBitRange): Byte;
begin
  Result := Value xor (1 shl (Bit mod 8));
end;

//------------------------------------------------------------------------------

function ToggleBit(const Value: Word; const Bit: TBitRange): Word;
begin
  Result := Value xor (1 shl (Bit mod 16));
end;

//------------------------------------------------------------------------------

function ToggleBit(const Value: Integer; const Bit: TBitRange): Integer;
begin
  Result := Value xor (1 shl (Bit mod 32));
end;

//------------------------------------------------------------------------------

function ToggleBit(const Value: Int64; const Bit: TBitRange): Int64;
begin
  Result := Value xor (Int64(1) shl (Bit mod 64));
end;

//------------------------------------------------------------------------------

procedure BooleansToBits(var Dest: Byte; const B: array of Boolean);
var
  I, H: Integer;
begin
  Dest := 0;
  H := Min(Byte(7), High(B));
  for I := 0 to H do
    if B[I] then
      SetBit(Dest, I);
end;

//------------------------------------------------------------------------------

procedure BooleansToBits(var Dest: Word; const B: array of Boolean);
var
  I, H: Integer;
begin
  Dest := 0;
  H := Min(Word(15), High(B));
  for I := 0 to H do
    if B[I] then
      SetBit(Dest, I);
end;

//------------------------------------------------------------------------------

procedure BooleansToBits(var Dest: Integer; const B: array of Boolean);
var
  I, H: Integer;
begin
  Dest := 0;
  H := Min(Integer(31), High(B));
  for I := 0 to H do
    if B[I] then
      SetBit(Dest, I);
end;

//------------------------------------------------------------------------------

procedure BooleansToBits(var Dest: Int64; const B: array of Boolean);
var
  I, H: Integer;
begin
  Dest := 0;
  H := Min(Int64(63), High(B));
  for I := 0 to H do
    if B[I] then
      SetBit(Dest, I);
end;

//------------------------------------------------------------------------------

procedure BitsToBooleans(const Bits: Byte; var B: TBooleanArray; AllBits: Boolean);
var
  I: Integer;
begin
  if AllBits then
    SetLength(B, SizeOf(Bits) * 8)
  else
    SetLength(B, BitsNeeded(Bits));
  for I := 0 to High(B) do
    B[I] := TestBit(Bits, TBitRange(I));
end;

//------------------------------------------------------------------------------

procedure BitsToBooleans(const Bits: Word; var B: TBooleanArray; AllBits: Boolean);
var
  I: Integer;
begin
  if AllBits then
    SetLength(B, SizeOf(Bits) * 8)
  else
    SetLength(B, BitsNeeded(Bits));
  for I := 0 to High(B) do
    B[I] := TestBit(Bits, TBitRange(I));
end;

//------------------------------------------------------------------------------

procedure BitsToBooleans(const Bits: Integer; var B: TBooleanArray; AllBits: Boolean);
var
  I: Integer;
begin
  if AllBits then
    SetLength(B, SizeOf(Bits) * 8)
  else
    SetLength(B, BitsNeeded(Bits));
  for I := 0 to High(B) do
    B[I] := TestBit(Bits, TBitRange(I));
end;

//------------------------------------------------------------------------------

procedure BitsToBooleans(const Bits: Int64; var B: TBooleanArray; AllBits: Boolean);
var
  I: Integer;
begin
  if AllBits then
    SetLength(B, SizeOf(Bits) * 8)
  else
    SetLength(B, BitsNeeded(Bits));
  for I := 0 to High(B) do
    B[I] := TestBit(Bits, TBitRange(I));
end;

//------------------------------------------------------------------------------

function Digits(const X: Cardinal): Integer;
var
  Val: Cardinal;
begin
  Result := 0;
  Val := X;
  repeat
    Inc(Result);
    Val := Val div 10;
  until Val = 0;
end;

//------------------------------------------------------------------------------

function BitsNeeded(const X: Byte): Integer;
begin
  Result := BitsHighest(X) + 1;
  if Result = 0 then
    Result := 1;
end;

//------------------------------------------------------------------------------

function BitsNeeded(const X: Word): Integer;
begin
  Result := BitsHighest(X) + 1;
  if Result = 0 then
    Result := 1;
end;

//------------------------------------------------------------------------------

function BitsNeeded(const X: Integer): Integer;
begin
  Result := BitsHighest(X) + 1;
  if Result = 0 then
    Result := 1;
end;

//------------------------------------------------------------------------------

function BitsNeeded(const X: Int64): Integer;
begin
  Result := BitsHighest(X) + 1;
  if Result = 0 then
    Result := 1;
end;


//==============================================================================
// Arithmetic
//==============================================================================

procedure SwapOrd(var I, J: Byte);
var
  T: Byte;
begin
  T := I;
  I := J;
  J := T;
end;

//------------------------------------------------------------------------------

procedure SwapOrd(var I, J: Cardinal);
var
  T: Cardinal;
begin
  T := I;
  I := J;
  J := T;
end;

//------------------------------------------------------------------------------

procedure SwapOrd(var I, J: Integer);
var
  T: Integer;
begin
  T := I;
  I := J;
  J := T;
end;

//------------------------------------------------------------------------------

procedure SwapOrd(var I, J: Int64);
var
  T: Int64;
begin
  T := I;
  I := J;
  J := T;
end;

//------------------------------------------------------------------------------

procedure SwapOrd(var I, J: Shortint);
var
  T: Shortint;
begin
  T := I;
  I := J;
  J := T;
end;

//------------------------------------------------------------------------------

procedure SwapOrd(var I, J: Smallint);
var
  T: Smallint;
begin
  T := I;
  I := J;
  J := T;
end;

//------------------------------------------------------------------------------

procedure SwapOrd(var I, J: Word);
var
  T: Word;
begin
  T := I;
  I := J;
  J := T;
end;

//------------------------------------------------------------------------------

function IncLimit(var B: Byte; const Limit, Incr: Byte): Byte;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;    
end;

//------------------------------------------------------------------------------

function IncLimit(var B: Shortint; const Limit, Incr: Shortint): Shortint;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

//------------------------------------------------------------------------------

function IncLimit(var B: Word; const Limit, Incr: Word): Word;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

//------------------------------------------------------------------------------

function IncLimit(var B: Integer; const Limit, Incr: Integer): Integer;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

//------------------------------------------------------------------------------

function IncLimit(var B: Cardinal; const Limit, Incr: Cardinal): Cardinal;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

//------------------------------------------------------------------------------

function IncLimit(var B: Int64; const Limit, Incr: Int64): Int64;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

//------------------------------------------------------------------------------

function DecLimit(var B: Byte; const Limit, Decr: Byte): Byte;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

//------------------------------------------------------------------------------

function DecLimit(var B: Shortint; const Limit, Decr: Shortint): shortint;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

//------------------------------------------------------------------------------

function DecLimit(var B: Word; const Limit, Decr: Word): Word;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

//------------------------------------------------------------------------------

function DecLimit(var B: Integer; const Limit, Decr: Integer): Integer;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

//------------------------------------------------------------------------------

function DecLimit(var B: Cardinal; const Limit, Decr: Cardinal): Cardinal;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

//------------------------------------------------------------------------------

function DecLimit(var B: Int64; const Limit, Decr: Int64): Int64;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

//------------------------------------------------------------------------------

function IncLimitClamp(var B: Byte; const Limit, Incr: Byte): Byte;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

//------------------------------------------------------------------------------

function IncLimitClamp(var B: Shortint; const Limit, Incr: Shortint): Shortint;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

//------------------------------------------------------------------------------

function IncLimitClamp(var B: Word; const Limit, Incr: Word): Word;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

//------------------------------------------------------------------------------

function IncLimitClamp(var B: Integer; const Limit, Incr: Integer): Integer;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

//------------------------------------------------------------------------------

function IncLimitClamp(var B: Cardinal; const Limit, Incr: Cardinal): Cardinal;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

//------------------------------------------------------------------------------

function IncLimitClamp(var B: Int64; const Limit, Incr: Int64): Int64;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

//------------------------------------------------------------------------------

function DecLimitClamp(var B: Byte; const Limit, Decr: Byte): Byte;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

//------------------------------------------------------------------------------

function DecLimitClamp(var B: Shortint; const Limit, Decr: Shortint): Shortint;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

//------------------------------------------------------------------------------

function DecLimitClamp(var B: Word; const Limit, Decr: Word): Word;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

//------------------------------------------------------------------------------

function DecLimitClamp(var B: Integer; const Limit, Decr: Integer): Integer;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

//------------------------------------------------------------------------------

function DecLimitClamp(var B: Cardinal; const Limit, Decr: Cardinal): Cardinal;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

//------------------------------------------------------------------------------

function DecLimitClamp(var B: Int64; const Limit, Decr: Int64): Int64;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

//------------------------------------------------------------------------------

function Max(const B1, B2: Byte): Byte;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

//------------------------------------------------------------------------------

function Min(const B1, B2: Byte): Byte;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

//------------------------------------------------------------------------------

function Max(const B1, B2: Shortint): Shortint;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

//------------------------------------------------------------------------------

function Min(const B1, B2: Shortint): Shortint;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

//------------------------------------------------------------------------------

function Max(const B1, B2: Word): Word;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

//------------------------------------------------------------------------------

function Max(const B1, B2: Int64): Int64;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

//------------------------------------------------------------------------------

function Min(const B1, B2: Word): Word;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

//------------------------------------------------------------------------------

function Max(const B1, B2: Integer): Integer;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

//------------------------------------------------------------------------------

function Min(const B1, B2: Integer): Integer;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

//------------------------------------------------------------------------------

function Max(const B1, B2: Cardinal): Cardinal;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

//------------------------------------------------------------------------------

function Min(const B1, B2: Cardinal): Cardinal;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

//------------------------------------------------------------------------------

function Min(const B1, B2: Int64): Int64;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

end.
