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
{ Last modified: May 27, 2000                                                  }
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
{$IFDEF SUPPORTS_INT64}
function OrdToBinary(const Value: Int64): string; overload;
{$ENDIF}

//------------------------------------------------------------------------------
// Bit manipulation
//------------------------------------------------------------------------------

type
  TBitRange = Byte;
  TBooleanArray = array of Boolean;

function BitsHighest(X: Byte): Integer; overload;
function BitsHighest(X: Word): Integer; overload;
function BitsHighest(X: Integer): Integer; overload;
function BitsHighest(X: Cardinal): Integer; assembler; overload;
{$IFDEF SUPPORTS_INT64}
function BitsHighest(X: Int64): Integer; overload;
{$ENDIF}

function BitsLowest(X: Byte): Integer; overload;
function BitsLowest(X: Word): Integer; overload;
function BitsLowest(X: Integer): Integer; overload;
function BitsLowest(X: Cardinal): Integer; assembler; overload;
{$IFDEF SUPPORTS_INT64}
function BitsLowest(X: Int64): Integer; overload;
{$ENDIF}

function ClearBit(const Value: Byte; const Bit: TBitRange): Byte; overload;
function ClearBit(const Value: Word; const Bit: TBitRange): Word; overload;
function ClearBit(const Value: Integer; const Bit: TBitRange): Integer; overload;
{$IFDEF SUPPORTS_INT64}
function ClearBit(const Value: Int64; const Bit: TBitRange): Int64; overload;
{$ENDIF}

function CountBitsSet(X: Byte): Integer; overload;
function CountBitsSet(X: Word): Integer; overload;
function CountBitsSet(X: Integer): Integer; overload;
function CountBitsSet(X: Cardinal): Integer; assembler; overload;
{$IFDEF SUPPORTS_INT64}
function CountBitsSet(X: Int64): Integer; overload;
{$ENDIF}

function CountBitsCleared(X: Byte): Integer; overload;
function CountBitsCleared(X: Word): Integer; overload;
function CountBitsCleared(X: Integer): Integer; overload;
{$IFDEF SUPPORTS_INT64}
function CountBitsCleared(X: Int64): Integer; overload;
{$ENDIF}

function LRot(const Value: Byte; const Count: TBitRange): Byte; overload;
function LRot(const Value: Word; const Count: TBitRange): Word; overload;
function LRot(const Value: Integer; const Count: TBitRange): Integer; overload;

function ReverseBits(const Value: Byte): Byte; overload;
function ReverseBits(const Value: Word): Word; overload;
function ReverseBits(const Value: Integer): Integer; overload;
{$IFDEF SUPPORTS_INT64}
function ReverseBits(const Value: Int64): Int64; overload;
{$ENDIF}

function RRot(const Value: Byte; const Count: TBitRange): Byte; overload;
function RRot(const Value: Word; const Count: TBitRange): Word; overload;
function RRot(const Value: Integer; const Count: TBitRange): Integer; overload;

function Sar(const Value: Shortint; const Count: TBitRange): Shortint; overload;
function Sar(const Value: Smallint; const Count: TBitRange): Smallint; overload;
function Sar(const Value: Integer; const Count: TBitRange): Integer; overload;

function SetBit(const Value: Byte; const Bit: TBitRange): Byte; overload;
function SetBit(const Value: Word; const Bit: TBitRange): Word; overload;
function SetBit(const Value: Integer; const Bit: TBitRange): Integer; overload;
{$IFDEF SUPPORTS_INT64}
function SetBit(const Value: Int64; const Bit: TBitRange): Int64; overload;
{$ENDIF}

function TestBit(const Value: Byte; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: Word; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: Integer; const Bit: TBitRange): Boolean; overload;
{$IFDEF SUPPORTS_INT64}
function TestBit(const Value: Int64; const Bit: TBitRange): Boolean; overload;
{$ENDIF}

function TestBits(const Value, Mask: Byte): Boolean; overload;
function TestBits(const Value, Mask: Word): Boolean; overload;
function TestBits(const Value, Mask: Integer): Boolean; overload;
{$IFDEF SUPPORTS_INT64}
function TestBits(const Value, Mask: Int64): Boolean; overload;
{$ENDIF}

function ToggleBit(const Value: Byte; const Bit: TBitRange): Byte; overload;
function ToggleBit(const Value: Word; const Bit: TBitRange): Word; overload;
function ToggleBit(const Value: Integer; const Bit: TBitRange): Integer; overload;
{$IFDEF SUPPORTS_INT64}
function ToggleBit(const Value: Int64; const Bit: TBitRange): Int64; overload;
{$ENDIF}

procedure BooleansToBits(var Dest: Byte; const B: array of Boolean); overload;
procedure BooleansToBits(var Dest: Word; const B: array of Boolean); overload;
procedure BooleansToBits(var Dest: Integer; const B: array of Boolean); overload;
{$IFDEF SUPPORTS_INT64}
procedure BooleansToBits(var Dest: Int64; const B: array of Boolean); overload;
{$ENDIF}

procedure BitsToBooleans(const Bits: Byte; B: TBooleanArray); overload;
procedure BitsToBooleans(const Bits: Word; B: TBooleanArray); overload;
procedure BitsToBooleans(const Bits: Integer; B: TBooleanArray); overload;
{$IFDEF SUPPORTS_INT64}
procedure BitsToBooleans(const Bits: Int64; B: TBooleanArray); overload;
{$ENDIF}

function BitsNeeded(const X: Byte): Integer; overload;
function BitsNeeded(const X: Word): Integer; overload;
function BitsNeeded(const X: Integer): Integer; overload;
{$IFDEF SUPPORTS_INT64}
function BitsNeeded(const X: Int64): Integer; overload;
{$ENDIF}

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
{$IFDEF SUPPORTS_INT64}
procedure SwapOrd(var I, J: Int64); overload;
{$ENDIF}

procedure IncLimit(var B: Byte; const Limit: Byte; const Incr: Byte {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}); overload;
procedure IncLimit(var B: Shortint; const Limit: Shortint; const Incr: Shortint {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}); overload;
procedure IncLimit(var B: Word; const Limit: Word; const Incr: Word {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}); overload;
procedure IncLimit(var B: Integer; const Limit: Integer; const Incr: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}); overload;
procedure IncLimit(var B: Cardinal; const Limit: Cardinal; const Incr: Cardinal {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}); overload;
{$IFDEF SUPPORTS_INT64}
procedure IncLimit(var B: Int64; const Limit: Int64; const Incr: Int64 {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}); overload;
{$ENDIF}

procedure DecLimit(var B: Byte; const Limit: Byte; const Decr: Byte {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}); overload;
procedure DecLimit(var B: Shortint; const Limit: Shortint; const Decr: Shortint {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}); overload;
procedure DecLimit(var B: Word; const Limit: Word; const Decr: Word {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}); overload;
procedure DecLimit(var B: Integer; const Limit: Integer; const Decr: Integer {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}); overload;
procedure DecLimit(var B: Cardinal; const Limit: Cardinal; const Decr: Cardinal {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}); overload;
{$IFDEF SUPPORTS_INT64}
procedure DecLimit(var B: Int64; const Limit: Int64; const Decr: Int64 {$IFDEF SUPPORTS_DEFAULTPARAMS} = 1 {$ENDIF}); overload;
{$ENDIF}

function Max(const B1, B2: Byte): Byte; overload;
function Max(const B1, B2: Shortint): Shortint; overload;
function Max(const B1, B2: Word): Word; overload;
function Max(const B1, B2: Integer): Integer; overload;
function Max(const B1, B2: Cardinal): Cardinal; overload;
{$IFDEF SUPPORTS_INT64}
function Max(const B1, B2: Int64): Int64; overload;
{$ENDIF}

function Min(const B1, B2: Byte): Byte; overload;
function Min(const B1, B2: Shortint): Shortint; overload;
function Min(const B1, B2: Word): Word; overload;
function Min(const B1, B2: Integer): Integer; overload;
function Min(const B1, B2: Cardinal): Cardinal; overload;
{$IFDEF SUPPORTS_INT64}
function Min(const B1, B2: Int64): Int64; overload;
{$ENDIF}

implementation

uses
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

{$IFDEF SUPPORTS_INT64}

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

{$ENDIF}

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

{$IFDEF SUPPORTS_INT64}

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

{$ENDIF}

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

{$IFDEF SUPPORTS_INT64}

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

{$ENDIF}

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

{$IFDEF SUPPORTS_INT64}

function ClearBit(const Value: Int64; const Bit: TBitRange): Int64;
begin
  Result := Value and not (Int64(1) shl (Bit mod 64));
end;

{$ENDIF}

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

function CountBitsSet(X: Byte): Integer; overload;
begin
  Result := CountBitsSet(Cardinal(X) and $FF);
end;

//------------------------------------------------------------------------------

function CountBitsSet(X: Word): Integer; overload;
begin
  Result := CountBitsSet(Cardinal(X) and $FFFF);
end;

//------------------------------------------------------------------------------

function CountBitsSet(X: Integer): Integer; overload;
begin
  Result := CountBitsSet(Cardinal(X));
end;

//------------------------------------------------------------------------------

{$IFDEF SUPPORTS_INT64}

function CountBitsSet(X: Int64): Integer; overload;
begin
  Result := CountBitsSet(TLargeInteger(X).LowPart) + CountBitsSet(TLargeInteger(X).HighPart);
end;

{$ENDIF}

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

{$IFDEF SUPPORTS_INT64}

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

{$ENDIF}

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

{$IFDEF SUPPORTS_INT64}

function SetBit(const Value: Int64; const Bit: TBitRange): Int64;
begin
  Result := Value or (Int64(1) shl (Bit mod 64));
end;

{$ENDIF}

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

{$IFDEF SUPPORTS_INT64}

function TestBit(const Value: Int64; const Bit: TBitRange): Boolean;
begin
  Result := (Value and (Int64(1) shl (Bit mod 64))) <> 0;
end;

{$ENDIF}

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

{$IFDEF SUPPORTS_INT64}

function TestBits(const Value, Mask: Int64): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

{$ENDIF}

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

{$IFDEF SUPPORTS_INT64}

function ToggleBit(const Value: Int64; const Bit: TBitRange): Int64;
begin
  Result := Value xor (Int64(1) shl (Bit mod 64));
end;

{$ENDIF}

//------------------------------------------------------------------------------

procedure BooleansToBits(var Dest: Byte; const B: array of Boolean);
var
  I: Integer;
begin
  Dest := 0;
  for I := 0 to High(B) do
    if B[I] then
      SetBit(Dest, I);
end;

//------------------------------------------------------------------------------

procedure BooleansToBits(var Dest: Word; const B: array of Boolean);
var
  I: Integer;
begin
  Dest := 0;
  for I := 0 to High(B) do
    if B[I] then
      SetBit(Dest, I);
end;

//------------------------------------------------------------------------------

procedure BooleansToBits(var Dest: Integer; const B: array of Boolean);
var
  I: Integer;
begin
  Dest := 0;
  for I := 0 to High(B) do
    if B[I] then
      SetBit(Dest, I);
end;

//------------------------------------------------------------------------------

{$IFDEF SUPPORTS_INT64}

procedure BooleansToBits(var Dest: Int64; const B: array of Boolean);
var
  I: Integer;
begin
  Dest := 0;
  for I := 0 to High(B) do
    if B[I] then
      SetBit(Dest, I);
end;

{$ENDIF}

//------------------------------------------------------------------------------

procedure BitsToBooleans(const Bits: Byte; B: TBooleanArray);
var
  I: Integer;
begin
  SetLength(B, BitsNeeded(Bits));
  for I := 0 to High(B) do
    B[I] := TestBit(Bits, I);
end;

//------------------------------------------------------------------------------

procedure BitsToBooleans(const Bits: Word; B: TBooleanArray);
var
  I: Integer;
begin
  SetLength(B, BitsNeeded(Bits));
  for I := 0 to High(B) do
    B[I] := TestBit(Bits, I);
end;

//------------------------------------------------------------------------------

procedure BitsToBooleans(const Bits: Integer; B: TBooleanArray);
var
  I: Integer;
begin
  SetLength(B, BitsNeeded(Bits));
  for I := 0 to High(B) do
    B[I] := TestBit(Bits, I);
end;

//------------------------------------------------------------------------------

{$IFDEF SUPPORTS_INT64}

procedure BitsToBooleans(const Bits: Int64; B: TBooleanArray);
var
  I: Integer;
begin
  SetLength(B, BitsNeeded(Bits));
  for I := 0 to High(B) do
    B[I] := TestBit(Bits, I);
end;

{$ENDIF}

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

{$IFDEF SUPPORTS_INT64}

function BitsNeeded(const X: Int64): Integer;
begin
  Result := BitsHighest(X) + 1;
  if Result = 0 then
    Result := 1;
end;

{$ENDIF}

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

{$IFDEF SUPPORTS_INT64}

procedure SwapOrd(var I, J: Int64);
var
  T: Int64;
begin
  T := I;
  I := J;
  J := T;
end;

{$ENDIF}

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

procedure IncLimit(var B: Byte; const Limit, Incr: Byte);
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
end;

//------------------------------------------------------------------------------

procedure IncLimit(var B: Shortint; const Limit, Incr: Shortint);
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
end;

//------------------------------------------------------------------------------

procedure IncLimit(var B: Word; const Limit, Incr: Word);
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
end;

//------------------------------------------------------------------------------

procedure IncLimit(var B: Integer; const Limit, Incr: Integer);
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
end;

//------------------------------------------------------------------------------

procedure IncLimit(var B: Cardinal; const Limit, Incr: Cardinal);
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
end;

//------------------------------------------------------------------------------

{$IFDEF SUPPORTS_INT64}

procedure IncLimit(var B: Int64; const Limit, Incr: Int64);
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
end;

{$ENDIF}

//------------------------------------------------------------------------------

procedure DecLimit(var B: Byte; const Limit, Decr: Byte);
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
end;

//------------------------------------------------------------------------------

procedure DecLimit(var B: Shortint; const Limit, Decr: Shortint);
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
end;

//------------------------------------------------------------------------------

procedure DecLimit(var B: Word; const Limit, Decr: Word);
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
end;

//------------------------------------------------------------------------------

procedure DecLimit(var B: Integer; const Limit, Decr: Integer);
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
end;

//------------------------------------------------------------------------------

procedure DecLimit(var B: Cardinal; const Limit, Decr: Cardinal);
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
end;

//------------------------------------------------------------------------------

{$IFDEF SUPPORTS_INT64}

procedure DecLimit(var B: Int64; const Limit, Decr: Int64);
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
end;

{$ENDIF}

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

{$IFDEF SUPPORTS_INT64}

function Max(const B1, B2: Int64): Int64;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

{$ENDIF}

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

{$IFDEF SUPPORTS_INT64}

function Min(const B1, B2: Int64): Int64;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

{$ENDIF}

end.
