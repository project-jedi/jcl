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
{ The Original Code is JclLogic.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel. All rights reserved.  }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel Bestebroer (marcelb), Marcel van Brakel, ESB Consultancy,                               }
{   Peter J. Haas (PeterJHaas) jediplus@pjh2.de, Martin Kimmings, Robert Marquardt, Chris Morris,  }
{   Michael Schnell, Matthias Thoma, Petr Vones                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Various routines to perform various arithmetic and logical operations on one or more ordinal     }
{ values (integer numbers). This includes various bit manipulation routines, min/max testing and   }
{ conversion to string.                                                                            }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

{.$DEFINE PUREPASCAL}

unit JclLogic;

{$I jcl.inc}
{$RANGECHECKS OFF}

interface

uses
  JclBase;

//--------------------------------------------------------------------------------------------------
// Conversion
//--------------------------------------------------------------------------------------------------

function OrdToBinary(const Value: Byte): string; overload;
function OrdToBinary(const Value: ShortInt): string; overload;
function OrdToBinary(const Value: SmallInt): string; overload;
function OrdToBinary(const Value: Word): string; overload;
function OrdToBinary(const Value: LongWord): string; overload;
function OrdToBinary(const Value: LongInt): string; overload;
function OrdToBinary(const Value: Int64): string; overload;

//--------------------------------------------------------------------------------------------------
// Bit manipulation
//--------------------------------------------------------------------------------------------------

type
  TBitRange = Byte;
  TBooleanArray = TDynBooleanArray;

function BitsHighest(X: Byte): Integer; overload;
function BitsHighest(X: ShortInt): Integer; overload;
function BitsHighest(X: Word): Integer; overload;
function BitsHighest(X: SmallInt): Integer; overload;
function BitsHighest(X: LongWord): Integer; overload;
function BitsHighest(X: LongInt): Integer; overload;
function BitsHighest(X: Int64): Integer; overload;

function BitsLowest(X: Byte): Integer; overload;
function BitsLowest(X: ShortInt): Integer; overload;
function BitsLowest(X: Word): Integer; overload;
function BitsLowest(X: SmallInt): Integer; overload;
function BitsLowest(X: LongWord): Integer; overload;
function BitsLowest(X: LongInt): Integer; overload;
function BitsLowest(X: Int64): Integer; overload;

function ClearBit(const Value: Byte; const Bit: TBitRange): Byte; overload;
function ClearBit(const Value: ShortInt; const Bit: TBitRange): ShortInt; overload;
function ClearBit(const Value: Word; const Bit: TBitRange): Word; overload;
function ClearBit(const Value: SmallInt; const Bit: TBitRange): SmallInt; overload;
function ClearBit(const Value: LongWord; const Bit: TBitRange): LongWord; overload;
function ClearBit(const Value: LongInt; const Bit: TBitRange): LongInt; overload;
function ClearBit(const Value: Int64; const Bit: TBitRange): Int64; overload;
{ TODO : Bit should be Cardinal }
procedure ClearBitBuffer(var Value; const Bit: TBitRange); { TODO -cHelp : document }

function CountBitsSet(X: Byte): Integer; overload;
function CountBitsSet(X: ShortInt): Integer; overload;
function CountBitsSet(X: Word): Integer; overload;
function CountBitsSet(X: SmallInt): Integer; overload;
function CountBitsSet(X: LongWord): Integer; overload;
function CountBitsSet(X: LongInt): Integer; overload;
function CountBitsSet(X: Int64): Integer; overload;

function CountBitsCleared(X: Byte): Integer; overload;
function CountBitsCleared(X: ShortInt): Integer; overload;
function CountBitsCleared(X: Word): Integer; overload;
function CountBitsCleared(X: SmallInt): Integer; overload;
function CountBitsCleared(X: LongWord): Integer; overload;
function CountBitsCleared(X: LongInt): Integer; overload;
function CountBitsCleared(X: Int64): Integer; overload;

function LRot(const Value: Byte; const Count: TBitRange): Byte; overload;
function LRot(const Value: ShortInt; const Count: TBitRange): ShortInt; overload;
function LRot(const Value: Word; const Count: TBitRange): Word; overload;
function LRot(const Value: SmallInt; const Count: TBitRange): SmallInt; overload;
function LRot(const Value: LongWord; const Count: TBitRange): LongWord; overload;
function LRot(const Value: LongInt; const Count: TBitRange): LongInt; overload;
function LRot(const Value: Int64; const Count: TBitRange): Int64; overload;

function ReverseBits(Value: Byte): Byte; overload;
function ReverseBits(Value: ShortInt): ShortInt; overload;
function ReverseBits(Value: Word): Word; overload;
function ReverseBits(Value: SmallInt): SmallInt; overload;
function ReverseBits(Value: LongWord): LongWord; overload;
function ReverseBits(Value: LongInt): LongInt; overload;
function ReverseBits(Value: Int64): Int64; overload;
{ TODO : Count should be Cardinal }
function ReverseBits(P: Pointer; Count: Integer): Pointer; overload;

function RRot(const Value: Byte; const Count: TBitRange): Byte; overload;
function RRot(const Value: ShortInt; const Count: TBitRange): ShortInt; overload;
function RRot(const Value: Word; const Count: TBitRange): Word; overload;
function RRot(const Value: SmallInt; const Count: TBitRange): SmallInt; overload;
function RRot(const Value: LongWord; const Count: TBitRange): LongWord; overload;
function RRot(const Value: LongInt; const Count: TBitRange): LongInt; overload;
function RRot(const Value: Int64; const Count: TBitRange): Int64; overload;

function Sar(const Value: ShortInt; const Count: TBitRange): ShortInt; overload;
function Sar(const Value: SmallInt; const Count: TBitRange): SmallInt; overload;
function Sar(const Value: LongInt; const Count: TBitRange): LongInt; overload;
function Sar(const Value: Int64; const Count: TBitRange): Int64; overload;

function SetBit(const Value: Byte; const Bit: TBitRange): Byte; overload;
function SetBit(const Value: ShortInt; const Bit: TBitRange): ShortInt; overload;
function SetBit(const Value: Word; const Bit: TBitRange): Word; overload;
function SetBit(const Value: SmallInt; const Bit: TBitRange): SmallInt; overload;
function SetBit(const Value: LongWord; const Bit: TBitRange): LongWord; overload;
function SetBit(const Value: LongInt; const Bit: TBitRange): LongInt; overload;
function SetBit(const Value: Int64; const Bit: TBitRange): Int64; overload;
{ TODO : Bit should be Cardinal }
procedure SetBitBuffer(var Value; const Bit: TBitRange); { TODO -cHelp : document }

function TestBit(const Value: Byte; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: ShortInt; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: Word; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: SmallInt; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: LongWord; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: LongInt; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: Int64; const Bit: TBitRange): Boolean; overload;
{ TODO : Bit should be Cardinal }
function TestBitBuffer(const Value; const Bit: TBitRange): Boolean; { TODO -cHelp : document }

function TestBits(const Value, Mask: Byte): Boolean; overload;
function TestBits(const Value, Mask: ShortInt): Boolean; overload;
function TestBits(const Value, Mask: Word): Boolean; overload;
function TestBits(const Value, Mask: SmallInt): Boolean; overload;
function TestBits(const Value, Mask: LongWord): Boolean; overload;
function TestBits(const Value, Mask: LongInt): Boolean; overload;
function TestBits(const Value, Mask: Int64): Boolean; overload;

function ToggleBit(const Value: Byte; const Bit: TBitRange): Byte; overload;
function ToggleBit(const Value: ShortInt; const Bit: TBitRange): ShortInt; overload;
function ToggleBit(const Value: Word; const Bit: TBitRange): Word; overload;
function ToggleBit(const Value: SmallInt; const Bit: TBitRange): SmallInt; overload;
function ToggleBit(const Value: LongWord; const Bit: TBitRange): LongWord; overload;
function ToggleBit(const Value: LongInt; const Bit: TBitRange): LongInt; overload;
function ToggleBit(const Value: Int64; const Bit: TBitRange): Int64; overload;
{ TODO : Bit should be Cardinal }
procedure ToggleBitBuffer(var Value; const Bit: TBitRange); { TODO -cHelp : document }

procedure BooleansToBits(var Dest: Byte; const B: TBooleanArray); overload;
procedure BooleansToBits(var Dest: ShortInt; const B: TBooleanArray); overload;
procedure BooleansToBits(var Dest: Word; const B: TBooleanArray); overload;
procedure BooleansToBits(var Dest: SmallInt; const B: TBooleanArray); overload;
procedure BooleansToBits(var Dest: LongWord; const B: TBooleanArray); overload;
procedure BooleansToBits(var Dest: LongInt; const B: TBooleanArray); overload;
procedure BooleansToBits(var Dest: Int64; const B: TBooleanArray); overload;

procedure BitsToBooleans(const Bits: Byte; var B: TBooleanArray; AllBits: Boolean = False); overload;
procedure BitsToBooleans(const Bits: ShortInt; var B: TBooleanArray; AllBits: Boolean = False); overload;
procedure BitsToBooleans(const Bits: Word; var B: TBooleanArray; AllBits: Boolean = False); overload;
procedure BitsToBooleans(const Bits: SmallInt; var B: TBooleanArray; AllBits: Boolean = False); overload;
procedure BitsToBooleans(const Bits: LongWord; var B: TBooleanArray; AllBits: Boolean = False); overload;
procedure BitsToBooleans(const Bits: LongInt; var B: TBooleanArray; AllBits: Boolean = False); overload;
procedure BitsToBooleans(const Bits: Int64; var B: TBooleanArray; AllBits: Boolean = False); overload;

function BitsNeeded(const X: Byte): Integer; overload;
function BitsNeeded(const X: ShortInt): Integer; overload;
function BitsNeeded(const X: Word): Integer; overload;
function BitsNeeded(const X: SmallInt): Integer; overload;
function BitsNeeded(const X: LongWord): Integer; overload;
function BitsNeeded(const X: LongInt): Integer; overload;
function BitsNeeded(const X: Int64): Integer; overload;

function Digits(const X: Cardinal): Integer;  { TODO : should named DecimalDigits }

function ReverseBytes(Value: Word): Word; overload;
function ReverseBytes(Value: SmallInt): SmallInt; overload;
function ReverseBytes(Value: LongWord): LongWord; overload;
function ReverseBytes(Value: LongInt): LongInt; overload;
function ReverseBytes(Value: Int64): Int64; overload;
function ReverseBytes(P: Pointer; Count: Integer): Pointer; overload;

//--------------------------------------------------------------------------------------------------
// Arithmetic
//--------------------------------------------------------------------------------------------------

procedure SwapOrd(var I, J: Byte); overload;
procedure SwapOrd(var I, J: ShortInt); overload;
procedure SwapOrd(var I, J: Word); overload;
procedure SwapOrd(var I, J: SmallInt); overload;
procedure SwapOrd(var I, J: LongWord); overload;
procedure SwapOrd(var I, J: LongInt); overload;
procedure SwapOrd(var I, J: Int64); overload;

function IncLimit(var B: Byte; const Limit: Byte; const Incr: Byte = 1): Byte; overload;
function IncLimit(var B: ShortInt; const Limit: ShortInt; const Incr: ShortInt = 1): ShortInt; overload;
function IncLimit(var B: Word; const Limit: Word; const Incr: Word = 1): Word; overload;
function IncLimit(var B: SmallInt; const Limit: SmallInt; const Incr: SmallInt = 1): SmallInt; overload;
function IncLimit(var B: LongWord; const Limit: LongWord; const Incr: LongWord = 1): LongWord; overload;
function IncLimit(var B: LongInt; const Limit: LongInt; const Incr: LongInt = 1): LongInt; overload;
function IncLimit(var B: Int64; const Limit: Int64; const Incr: Int64 = 1): Int64; overload;

function DecLimit(var B: Byte; const Limit: Byte; const Decr: Byte = 1): Byte; overload;
function DecLimit(var B: ShortInt; const Limit: ShortInt; const Decr: ShortInt = 1): ShortInt; overload;
function DecLimit(var B: Word; const Limit: Word; const Decr: Word = 1): Word; overload;
function DecLimit(var B: SmallInt; const Limit: SmallInt; const Decr: SmallInt = 1): SmallInt; overload;
function DecLimit(var B: LongWord; const Limit: LongWord; const Decr: LongWord = 1): LongWord; overload;
function DecLimit(var B: LongInt; const Limit: LongInt; const Decr: LongInt = 1): LongInt; overload;
function DecLimit(var B: Int64; const Limit: Int64; const Decr: Int64 = 1): Int64; overload;

function IncLimitClamp(var B: Byte; const Limit: Byte; const Incr: Byte = 1): Byte; overload;
function IncLimitClamp(var B: ShortInt; const Limit: ShortInt; const Incr: ShortInt = 1): ShortInt; overload;
function IncLimitClamp(var B: Word; const Limit: Word; const Incr: Word = 1): Word; overload;
function IncLimitClamp(var B: SmallInt; const Limit: SmallInt; const Incr: SmallInt = 1): SmallInt; overload;
function IncLimitClamp(var B: LongWord; const Limit: LongWord; const Incr: LongWord = 1): LongWord; overload;
function IncLimitClamp(var B: LongInt; const Limit: LongInt; const Incr: LongInt = 1): LongInt; overload;
function IncLimitClamp(var B: Int64; const Limit: Int64; const Incr: Int64 = 1): Int64; overload;

function DecLimitClamp(var B: Byte; const Limit: Byte; const Decr: Byte = 1): Byte; overload;
function DecLimitClamp(var B: ShortInt; const Limit: ShortInt; const Decr: ShortInt = 1): ShortInt; overload;
function DecLimitClamp(var B: Word; const Limit: Word; const Decr: Word = 1): Word; overload;
function DecLimitClamp(var B: SmallInt; const Limit: SmallInt; const Decr: SmallInt = 1): SmallInt; overload;
function DecLimitClamp(var B: LongWord; const Limit: LongWord; const Decr: LongWord = 1): LongWord; overload;
function DecLimitClamp(var B: LongInt; const Limit: LongInt; const Decr: LongInt = 1): LongInt; overload;
function DecLimitClamp(var B: Int64; const Limit: Int64; const Decr: Int64 = 1): Int64; overload;

function Max(const B1, B2: Byte): Byte; overload;
function Max(const B1, B2: ShortInt): ShortInt; overload;
function Max(const B1, B2: Word): Word; overload;
function Max(const B1, B2: SmallInt): SmallInt; overload;
function Max(const B1, B2: LongWord): LongWord; overload;
function Max(const B1, B2: LongInt): LongInt; overload;
function Max(const B1, B2: Int64): Int64; overload;

function Min(const B1, B2: Byte): Byte; overload;
function Min(const B1, B2: ShortInt): ShortInt; overload;
function Min(const B1, B2: Word): Word; overload;
function Min(const B1, B2: SmallInt): SmallInt; overload;
function Min(const B1, B2: LongWord): LongWord; overload;
function Min(const B1, B2: LongInt): LongInt; overload;
function Min(const B1, B2: Int64): Int64; overload;

implementation

type
  PByte = ^Byte;

const
  // Constants defining the number of bits in each Integer type

  BitsPerNibble   = 4;
  BitsPerByte     = 8;
  BitsPerShortInt = SizeOf(ShortInt) * BitsPerByte;
  BitsPerWord     = SizeOf(Word) * BitsPerByte;
  BitsPerSmallInt = SizeOf(SmallInt) * BitsPerByte;
  BitsPerLongWord = SizeOf(LongWord) * BitsPerByte;
  BitsPerLongInt  = SizeOf(LongInt) * BitsPerByte;
  BitsPerInt64    = SizeOf(Int64) * BitsPerByte;
  BitsPerCardinal = SizeOf(Cardinal) * BitsPerByte;
  BitsPerInteger  = SizeOf(Integer) * BitsPerByte;

  // Constants defining the number of nibbles in each Integer type

  NibblesPerByte     = BitsPerByte div BitsPerNibble;
  NibblesPerShortInt = SizeOf(ShortInt) * NibblesPerByte;
  NibblesPerWord     = SizeOf(Word) * NibblesPerByte;
  NibblesPerSmallInt = SizeOf(SmallInt) * NibblesPerByte;
  NibblesPerLongWord = SizeOf(LongWord) * NibblesPerByte;
  NibblesPerLongInt  = SizeOf(LongInt) * NibblesPerByte;
  NibblesPerInt64    = SizeOf(Int64) * NibblesPerByte;
  NibblesPerCardinal = SizeOf(Cardinal) * NibblesPerByte;
  NibblesPerInteger  = SizeOf(Integer) * NibblesPerByte;

  // Constants defining a mask with all bits set for each Integer type

  NibbleMask   = $F;
  ByteMask     = Byte($FF);
  ShortIntMask = ShortInt($FF);
  WordMask     = Word($FFFF);
  SmallIntMask = SmallInt($FFFF);
  LongWordMask = LongWord($FFFFFFFF);
  LongIntMask  = LongInt($FFFFFFFF);
  Int64Mask    = Int64($FFFFFFFFFFFFFFFF);
  CardinalMask = Cardinal(High(Cardinal));
  IntegerMask  = Integer(High(Cardinal));

//==================================================================================================
// Conversion
//==================================================================================================

function OrdToBinary(const Value: Byte): string;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  I: Integer;
  V: Cardinal;
  P: PChar;
begin
  SetLength(Result, BitCount);
  P := PChar(Result);
  Inc(P, BitCount);
  V := Value;
  for I := 0 to BitCount - 1 do
  begin
    P^ := Chr(Ord('0') + (V and $00000001));
    Dec(P);
    V := V shr 1;
  end;
end;

//--------------------------------------------------------------------------------------------------

function OrdToBinary(const Value: ShortInt): string;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  I: Integer;
  V: Cardinal;
  P: PChar;
begin
  SetLength(Result, BitCount);
  P := PChar(Result);
  Inc(P, BitCount);
  V := Value;
  for I := 0 to BitCount - 1 do
  begin
    P^ := Chr(Ord('0') + (V and $00000001));
    Dec(P);
    V := V shr 1;
  end;
end;

//--------------------------------------------------------------------------------------------------

function OrdToBinary(const Value: Word): string;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  I: Integer;
  V: Cardinal;
  P: PChar;
begin
  SetLength(Result, BitCount);
  P := PChar(Result);
  Inc(P, BitCount);
  V := Value;
  for I := 0 to BitCount - 1 do
  begin
    P^ := Chr(Ord('0') + (V and $00000001));
    Dec(P);
    V := V shr 1;
  end;
end;

//--------------------------------------------------------------------------------------------------

function OrdToBinary(const Value: SmallInt): string;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  I: Integer;
  V: Cardinal;
  P: PChar;
begin
  SetLength(Result, BitCount);
  P := PChar(Result);
  Inc(P, BitCount);
  V := Value;
  for I := 0 to BitCount - 1 do
  begin
    P^ := Chr(Ord('0') + (V and $00000001));
    Dec(P);
    V := V shr 1;
  end;
end;

//--------------------------------------------------------------------------------------------------

function OrdToBinary(const Value: LongWord): string;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  I: Integer;
  V: Cardinal;
  P: PChar;
begin
  SetLength(Result, BitCount);
  P := PChar(Result);
  Inc(P, BitCount);
  V := Value;
  for I := 0 to BitCount - 1 do
  begin
    P^ := Chr(Ord('0') + (V and $00000001));
    Dec(P);
    V := V shr 1;
  end;
end;

//--------------------------------------------------------------------------------------------------

function OrdToBinary(const Value: LongInt): string;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  I: Integer;
  V: Cardinal;
  P: PChar;
begin
  SetLength(Result, BitCount);
  P := PChar(Result);
  Inc(P, BitCount);
  V := Value;
  for I := 0 to BitCount - 1 do
  begin
    P^ := Chr(Ord('0') + (V and $00000001));
    Dec(P);
    V := V shr 1;
  end;
end;

//--------------------------------------------------------------------------------------------------

function OrdToBinary(const Value: Int64): string;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  I: Integer;
  V: Int64;
  P: PChar;
begin
  SetLength(Result, BitCount);
  P := PChar(Result);
  Inc(P, BitCount);
  V := Value;
  for I := 0 to BitCount - 1 do
  begin
    P^ := Chr(Ord('0') + (V and Int64($00000001)));
    Dec(P);
    V := V shr Int64(1);
  end;
end;


//==================================================================================================
// Bit manipulation
//==================================================================================================

function BitsHighest(X: Byte): Integer;
begin
  { TODO : do we need the mask? (BitsHighest, BitsLowest) }
  Result := BitsHighest(LongWord(X) and LongWord(ByteMask));
end;

//--------------------------------------------------------------------------------------------------

function BitsHighest(X: ShortInt): Integer;
begin
  Result := BitsHighest(LongWord(Byte(X)));
end;

//--------------------------------------------------------------------------------------------------

function BitsHighest(X: Word): Integer;
begin
  Result := BitsHighest(LongWord(X) and LongWord(WordMask));
end;

//--------------------------------------------------------------------------------------------------

function BitsHighest(X: SmallInt): Integer;
begin
  Result := BitsHighest(LongWord(X) and LongWord(WordMask));
end;


//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function BitsHighest(X: LongWord): Integer;
begin
  if X = 0 then
    Result := -1
  else
  begin
    Result := 31;
    while (X and $80000000) = 0 do
    begin
      X := X shl 1;
      Dec(Result);
    end;
  end;
end;
{$ELSE}
function BitsHighest(X: LongWord): Integer; assembler;
asm
        MOV     ECX, EAX
        MOV     EAX, -1
        BSR     EAX, ECX
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

function BitsHighest(X: LongInt): LongInt;
begin
  Result := BitsHighest(LongWord(X));
end;

//--------------------------------------------------------------------------------------------------

function BitsHighest(X: Int64): Integer;
begin
  if TULargeInteger(X).HighPart = 0 then
  begin
    if TULargeInteger(X).LowPart = 0 then
      Result := -1
    else
      Result := BitsHighest(LongWord(TULargeInteger(X).LowPart));
  end
  else
    Result := BitsHighest(LongWord(TULargeInteger(X).HighPart)) + 32;
end;

//--------------------------------------------------------------------------------------------------

function BitsLowest(X: Byte): Integer;
begin
  Result := BitsLowest(LongWord(X) and LongWord(ByteMask));
end;

//--------------------------------------------------------------------------------------------------

function BitsLowest(X: ShortInt): Integer;
begin
  Result := BitsLowest(LongWord(X) and LongWord(ByteMask));
end;

//--------------------------------------------------------------------------------------------------

function BitsLowest(X: Word): Integer;
begin
  Result := BitsLowest(LongWord(X) and LongWord(WordMask));
end;

//--------------------------------------------------------------------------------------------------

function BitsLowest(X: SmallInt): Integer;
begin
  Result := BitsLowest(LongWord(X) and LongWord(WordMask));
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function BitsLowest(X: LongWord): Integer;
begin
  if X = 0 then
    Result := -1
  else
  begin
    Result := 0;
    while (X and $00000001) = 0 do
    begin
      X := X shr 1;
      Inc(Result);
    end;
  end;
end;
{$ELSE}
function BitsLowest(X: LongWord): Integer; assembler;
asm
        MOV     ECX, EAX
        MOV     EAX, -1
        BSF     EAX, ECX
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

function BitsLowest(X: LongInt): Integer;
begin
  Result := BitsLowest(LongWord(X));
end;

//--------------------------------------------------------------------------------------------------

function BitsLowest(X: Int64): Integer;
begin
  if TULargeInteger(X).LowPart = 0 then
  begin
    if TULargeInteger(X).HighPart = 0 then
      Result := -1
    else
      Result := BitsLowest(LongWord(TULargeInteger(X).HighPart)) + 32;
  end
  else
    Result := BitsLowest(LongWord(TULargeInteger(X).LowPart));
end;

//--------------------------------------------------------------------------------------------------

function ClearBit(const Value: Byte; const Bit: TBitRange): Byte;
begin
  Result := Value and not (1 shl (Bit mod (SizeOf(Value) * BitsPerByte)));
end;

//--------------------------------------------------------------------------------------------------

function ClearBit(const Value: ShortInt; const Bit: TBitRange): ShortInt;
begin
  Result := Value and not (1 shl (Bit mod (SizeOf(Value) * BitsPerByte)));
end;

//--------------------------------------------------------------------------------------------------

function ClearBit(const Value: Word; const Bit: TBitRange): Word;
begin
  Result := Value and not (1 shl (Bit mod (SizeOf(Value) * BitsPerByte)));
end;

//--------------------------------------------------------------------------------------------------

function ClearBit(const Value: SmallInt; const Bit: TBitRange): SmallInt;
begin
  Result := Value and not (1 shl (Bit mod (SizeOf(Value) * BitsPerByte)));
end;

//--------------------------------------------------------------------------------------------------

function ClearBit(const Value: LongWord; const Bit: TBitRange): LongWord;
begin
  Result := Value and not (1 shl (Bit mod (SizeOf(Value) * BitsPerByte)));
end;

//--------------------------------------------------------------------------------------------------

function ClearBit(const Value: LongInt; const Bit: TBitRange): LongInt;
begin
  Result := Value and not (1 shl (Bit mod (SizeOf(Value) * BitsPerByte)));
end;

//--------------------------------------------------------------------------------------------------

function ClearBit(const Value: Int64; const Bit: TBitRange): Int64;
begin
  Result := Value and not (Int64(1) shl (Bit mod (SizeOf(Value) * BitsPerByte)));
end;

//--------------------------------------------------------------------------------------------------

procedure ClearBitBuffer(var Value; const Bit: TBitRange);
var
  P: PByte;
  BitOfs: TBitRange;
begin
  P := Addr(Value);
  Inc(P, Bit div BitsPerByte);
  BitOfs := Bit mod BitsPerByte;
  P^ := ClearBit(P^, BitOfs);
end;

//--------------------------------------------------------------------------------------------------

function CountBitsSet(X: Byte): Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 1 to BitsPerByte do
  begin
    if (X and 1) = 1 then
      Inc(Result);
    X := X shr 1;
  end;
end;

//--------------------------------------------------------------------------------------------------

function CountBitsSet(X: ShortInt): Integer;
begin
  Result := CountBitsSet(Byte(X));
end;

//--------------------------------------------------------------------------------------------------

function CountBitsSet(X: Word): Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 1 to BitsPerWord do
  begin
    if (X and 1) = 1 then
      Inc(Result);
    X := X shr 1;
  end;
end;

//--------------------------------------------------------------------------------------------------

function CountBitsSet(X: SmallInt): Integer;
begin
  Result := CountBitsSet(Word(X));
end;

//--------------------------------------------------------------------------------------------------

function CountBitsSet(X: LongWord): Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 1 to BitsPerCardinal do
  begin
    if (X and 1) = 1 then
      Inc(Result);
    X := X shr 1;
  end;
end;

//--------------------------------------------------------------------------------------------------

function CountBitsSet(X: LongInt): Integer;
begin
  Result := CountBitsSet(LongWord(X));
end;

//--------------------------------------------------------------------------------------------------

function CountBitsSet(X: Int64): Integer;
begin
  Result := CountBitsSet(LongWord(TULargeInteger(X).LowPart)) +
            CountBitsSet(LongWord(TULargeInteger(X).HighPart));
end;

//--------------------------------------------------------------------------------------------------

function CountBitsCleared(X: Byte): Integer;
begin
  Result := BitsPerByte - CountBitsSet(Byte(X));
end;

//--------------------------------------------------------------------------------------------------

function CountBitsCleared(X: ShortInt): Integer;
begin
  Result := BitsPerShortInt - CountBitsSet(Byte(X));
end;

//--------------------------------------------------------------------------------------------------

function CountBitsCleared(X: Word): Integer;
begin
  Result := BitsPerWord - CountBitsSet(Word(X));
end;

//--------------------------------------------------------------------------------------------------

function CountBitsCleared(X: SmallInt): Integer;
begin
  Result := BitsPerSmallInt - CountBitsSet(Word(X));
end;

//--------------------------------------------------------------------------------------------------

function CountBitsCleared(X: LongWord): Integer;
begin
  Result := BitsPerLongWord - CountBitsSet(LongWord(X));
end;

//--------------------------------------------------------------------------------------------------

function CountBitsCleared(X: LongInt): Integer;
begin
  Result := BitsPerLongInt - CountBitsSet(LongWord(X));
end;

//--------------------------------------------------------------------------------------------------

function CountBitsCleared(X: Int64): Integer;
begin
  Result := BitsPerInt64 - CountBitsSet(Int64(X));
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function LRot(const Value: Byte; const Count: TBitRange): Byte;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  C: Integer;
begin
  C := Count and (BitCount - 1);
  Result := (Value shl C) or (Value shr (BitCount - C));
end;
{$ELSE}
function LRot(const Value: Byte; const Count: TBitRange): Byte; assembler;
asm
        MOV     CL, Count
        ROL     AL, CL
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function LRot(const Value: ShortInt; const Count: TBitRange): ShortInt;
begin
  // direct implementation need typecast to prevent signed expand
  Result := LRot(Byte(Value), Count);
end;
{$ELSE}
function LRot(const Value: ShortInt; const Count: TBitRange): ShortInt; assembler;
asm
        MOV     CL, Count
        ROL     AL, CL
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function LRot(const Value: Word; const Count: TBitRange): Word;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  C: Integer;
begin
  C := Count and (BitCount - 1);
  Result := (Value shl C) or (Value shr (BitCount - C));
end;
{$ELSE}
function LRot(const Value: Word; const Count: TBitRange): Word; assembler;
asm
        MOV     CL, Count
        ROL     AX, CL
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function LRot(const Value: SmallInt; const Count: TBitRange): SmallInt;
begin
  // direct implementation need typecast to prevent signed expand
  Result := LRot(Word(Value), Count);
end;
{$ELSE}
function LRot(const Value: SmallInt; const Count: TBitRange): SmallInt; assembler;
asm
       MOV     CL, Count
       ROL     AX, CL
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function LRot(const Value: LongWord; const Count: TBitRange): LongWord;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  C: Integer;
begin
  C := Count and (BitCount - 1);
  Result := (Value shl C) or (Value shr (BitCount - C));
end;
{$ELSE}
function LRot(const Value: LongWord; const Count: TBitRange): LongWord; assembler;
asm
        MOV     CL, Count
        ROL     EAX, CL
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function LRot(const Value: LongInt; const Count: TBitRange): LongInt;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  C: Integer;
begin
  C := Count and (BitCount - 1);
  // for LongInt shr is a real shr
  Result := (Value shl C) or (Value shr (BitCount - C));
end;
{$ELSE}
function LRot(const Value: LongInt; const Count: TBitRange): LongInt; assembler;
asm
        MOV     CL, Count
        ROL     EAX, CL
end;
{$ENDIF}

{$IFDEF PUREPASCAL}
function LRot(const Value: Int64; const Count: TBitRange): Int64;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  C: Integer;
begin
  C := Count and (BitCount - 1);
  Result := (Value shl C) or (Value shr (BitCount - C));
end;
{$ELSE}
function LRot(const Value: Int64; const Count: TBitRange): Int64; assembler;
asm
        MOV     CL, AL
        MOV     EAX, [EBP + $08]    // EAX:EDX := Value
        MOV     EDX, [EBP + $0C]
        AND     CL, $3F             // Count mod 64
        JZ      @@05
        CMP     CL, $20
        JL      @@01

        MOV     EDX, EAX            // Count >= 32
        SHL     EDX, CL
        XOR     EAX, EAX
        JMP     @@02

@@01:   SHLD    EDX, EAX, CL        // Count < 32
        SHL     EAX, CL

@@02:   ADD     ESP, -$08
        MOV     [EBP - $08], EAX
        MOV     [EBP - $04], EDX

        NEG     CL                  // Count := 64 - Count
        ADD     CL, 64

        MOV     EAX, [EBP + $08]    // EAX:EDX := Value
        MOV     EDX, [EBP + $0C]
        CMP     CL, $20
        JL      @@03

        MOV     EAX, EDX            // Count >= 32
        XOR     EDX, EDX
        SHR     EAX, CL
        JMP     @@04

@@03:   SHRD    EAX, EDX, CL        // Count < 32
        SHR     EDX, CL

@@04:   OR      EAX, [EBP - $08]
        OR      EDX, [EBP - $04]
        POP     ECX
        POP     ECX
@@05:        
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

const
  // Lookup table of bit reversed nibbles, used by simple overloads of ReverseBits
  RevNibbles: array [0..NibbleMask] of Byte =
    ($0, $8, $4, $C, $2, $A, $6, $E, $1, $9, $5, $D, $3, $B, $7, $F);

function ReverseBits(Value: Byte): Byte;
begin
  Result := RevNibbles[Value shr BitsPerNibble] or
    (RevNibbles[Value and NibbleMask] shl BitsPerNibble);
end;

//--------------------------------------------------------------------------------------------------

function ReverseBits(Value: ShortInt): ShortInt;
begin
  Result := RevNibbles[Byte(Value) shr BitsPerNibble] or
    (RevNibbles[Value and NibbleMask] shl BitsPerNibble);
end;

//--------------------------------------------------------------------------------------------------

function ReverseBits(Value: Word): Word;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to NibblesPerWord - 1 do
  begin
    Result := (Result shl BitsPerNibble) or RevNibbles[Value and NibbleMask];
    Value := Value shr BitsPerNibble;
  end;
end;

//--------------------------------------------------------------------------------------------------

function ReverseBits(Value: SmallInt): SmallInt;
begin
  Result := ReverseBits(Word(Value));
end;

//--------------------------------------------------------------------------------------------------

function ReverseBits(Value: LongWord): LongWord;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to NibblesPerCardinal - 1 do
  begin
    Result := (Result shl BitsPerNibble) or RevNibbles[Value and NibbleMask];
    Value := Value shr BitsPerNibble;
  end;
end;

//--------------------------------------------------------------------------------------------------

function ReverseBits(Value: LongInt): LongInt;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to NibblesPerInteger - 1 do
  begin
    Result := (Result shl BitsPerNibble) or RevNibbles[Value and NibbleMask];
    Value := Value shr BitsPerNibble;
  end;
end;

//--------------------------------------------------------------------------------------------------

function ReverseBits(Value: Int64): Int64;
begin
  TULargeInteger(Result).LowPart := ReverseBits(TULargeInteger(Value).HighPart);
  TULargeInteger(Result).HighPart := ReverseBits(TULargeInteger(Value).LowPart);
end;

//--------------------------------------------------------------------------------------------------

const
  // Lookup table of reversed bytes, used by pointer overload of ReverseBits
  ReverseTable: array [Byte] of Byte = (
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
    if P1 = P2 then
      P1^ := ReverseTable[P1^];
  end;
  Result := P;
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function RRot(const Value: Byte; const Count: TBitRange): Byte;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  C: Integer;
begin
  C := Count and (BitCount - 1);
  Result := (Value shr C) or (Value shl (BitCount - C));
end;
{$ELSE}
function RRot(const Value: Byte; const Count: TBitRange): Byte; assembler;
asm
        MOV     ECX, EDX
        ROR     AL, CL
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function RRot(const Value: ShortInt; const Count: TBitRange): ShortInt;
begin
  // direct implementation need typecast to prevent signed expand
  Result := RRot(Byte(Value), Count);
end;
{$ELSE}
function RRot(const Value: ShortInt; const Count: TBitRange): ShortInt; assembler;
asm
        MOV     ECX, EDX
        ROR     AL, CL
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function RRot(const Value: Word; const Count: TBitRange): Word;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  C: Integer;
begin
  C := Count and (BitCount - 1);
  Result := (Value shr C) or (Value shl (BitCount - C));
end;
{$ELSE}
function RRot(const Value: Word; const Count: TBitRange): Word; assembler;
asm
        MOV     ECX, EDX
        ROR     AX, CL
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function RRot(const Value: SmallInt; const Count: TBitRange): SmallInt;
begin
  // direct implementation need typecast to prevent signed expand
  Result := RRot(Word(Value), Count);
end;
{$ELSE}
function RRot(const Value: SmallInt; const Count: TBitRange): SmallInt; assembler;
asm
        MOV     ECX, EDX
        ROR     AX, CL
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function RRot(const Value: LongWord; const Count: TBitRange): LongWord;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  C: Integer;
begin
  C := Count and (BitCount - 1);
  Result := (Value shr C) or (Value shl (BitCount - C));
end;
{$ELSE}
function RRot(const Value: LongWord; const Count: TBitRange): LongWord; assembler;
asm
        MOV     ECX, EDX
        ROR     EAX, CL
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function RRot(const Value: LongInt; const Count: TBitRange): LongInt;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  C: Integer;
begin
  C := Count and (BitCount - 1);
  Result := (Value shr C) or (Value shl (BitCount - C));
end;
{$ELSE}
function RRot(const Value: LongInt; const Count: TBitRange): LongInt; assembler;
asm
        MOV     ECX, EDX
        ROR     EAX, CL
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function RRot(const Value: Int64; const Count: TBitRange): Int64;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  C: Integer;
begin
  C := Count and (BitCount - 1);
  Result := (Value shr C) or (Value shl (BitCount - C));
end;
{$ELSE}
function RRot(const Value: Int64; const Count: TBitRange): Int64; assembler;
asm
        MOV     CL, AL
        MOV     EAX, [EBP + $08]    // EAX:EDX := Value
        MOV     EDX, [EBP + $0C]
        AND     CL, $3F             // Count mod 64
        JZ      @@05
        CMP     CL, $20
        JL      @@01
        MOV     EAX, EDX            // Count >= 32
        XOR     EDX, EDX
        SHR     EAX, CL
        JMP     @@02

@@01:   SHRD    EAX, EDX, CL        // Count < 32
        SHR     EDX, CL

@@02:   ADD     ESP, -$08
        MOV     [EBP - $08], EAX
        MOV     [EBP - $04], EDX

        NEG     CL                  // Count := 64 - Count
        ADD     CL, 64

        MOV     EAX, [EBP + $08]    // EAX:EDX := Value
        MOV     EDX, [EBP + $0C]
        CMP     CL, $20
        JL      @@03

        MOV     EDX, EAX            // Count >= 32
        SHL     EDX, CL
        XOR     EAX, EAX
        JMP     @@04

@@03:   SHLD    EDX, EAX, CL        // Count < 32
        SHL     EAX, CL

@@04:   OR      EAX, [EBP - $08]
        OR      EDX, [EBP - $04]
        POP     ECX
        POP     ECX
@@05:
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function Sar(const Value: ShortInt; const Count: TBitRange): ShortInt;
begin
  if Count >= 8 then
  begin
    if Value < 0 then
      Result := -1
    else
      Result := 0;
  end
  else
    Result := ShortInt(Integer(Value) shr Count);
end;
{$ELSE}
function Sar(const Value: ShortInt; const Count: TBitRange): ShortInt; assembler;
asm
        MOV     CL, DL
        CMP     CL, 8
        JL      @@01
        MOV     CL, 7
@@01:   SAR     AL, CL
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function Sar(const Value: SmallInt; const Count: TBitRange): SmallInt;
begin
  if Count >= 16 then
  begin
    if Value < 0 then
      Result := -1
    else
      Result := 0;
  end
  else
    Result := Integer(Value) shr Count;
end;
{$ELSE}
function Sar(const Value: SmallInt; const Count: TBitRange): SmallInt; assembler;
asm
        MOV     CL, DL
        CMP     CL, 16
        JL      @@01
        MOV     CL, 15
@@01:   SAR     AX, CL
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

const
  SignedExpandOrMasks: array[1..31] of LongWord = (
    $80000000, $C0000000, $E0000000, $F0000000,
    $F8000000, $FC000000, $FE000000, $FF000000,
    $FF800000, $FFC00000, $FFE00000, $FFF00000,
    $FFF80000, $FFFC0000, $FFFE0000, $FFFF0000,
    $FFFF8000, $FFFFC000, $FFFFE000, $FFFFF000,
    $FFFFF800, $FFFFFC00, $FFFFFE00, $FFFFFF00,
    $FFFFFF80, $FFFFFFC0, $FFFFFFE0, $FFFFFFF0,
    $FFFFFFF8, $FFFFFFFC, $FFFFFFFE);

{$IFDEF PUREPASCAL}
function Sar(const Value: LongInt; const Count: TBitRange): LongInt;
begin
  case Count of
    0:
      Result := Value;
    1..31:
      begin
        Result := Value shr Count;
        if Value < 0 then
          Result := LongWord(Result) or SignedExpandOrMasks[Count];
      end;
  else // > 31
    if Value < 0 then
      Result := -1
    else
      Result := 0;
  end;
end;
{$ELSE}
function Sar(const Value: LongInt; const Count: TBitRange): LongInt; assembler;
asm
        MOV     CL, DL
        CMP     CL, 32
        JL      @@01
        MOV     CL, 31
@@01:   SAR     EAX, CL
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function Sar(const Value: Int64; const Count: TBitRange): Int64;
var
  C: Integer;
begin
  case Count of
    0:
      Result := Value;
    1..31:
      begin
        Result := Value shr Count;
        if Value < 0 then
          TULargeInteger(Result).HighPart := TULargeInteger(Result).HighPart or SignedExpandOrMasks[Count];
      end;
    32..63:
      begin
        C := Count - 32;
        if C = 0 then
          TULargeInteger(Result).LowPart := TULargeInteger(Value).HighPart
        else
        begin
          TULargeInteger(Result).LowPart := TULargeInteger(Value).HighPart shr (C);
          if Value < 0 then
            TULargeInteger(Result).LowPart := TULargeInteger(Result).LowPart or SignedExpandOrMasks[C];
        end;
        if Value < 0 then
          TULargeInteger(Result).HighPart := High(LongWord)
        else
          TULargeInteger(Result).HighPart := 0;
      end;
  else // > 63
    if Value < 0 then
      Result := -1
    else
      Result := 0;
  end;
end;
{$ELSE}
function Sar(const Value: Int64; const Count: TBitRange): Int64; assembler;
asm
        MOV     CL, AL
        MOV     EAX, [EBP + $08]    // EAX:EDX := Value
        MOV     EDX, [EBP + $0C]
        TEST    CL, CL              // Count = 0 ->
        JZ      @@03
        CMP     CL, 32
        JL      @@02
        CMP     CL, 64
        JL      @@01
        SAR     EDX, $1F            // Count > 63
        MOV     EAX, EDX
        JMP     @@03

@@01:   MOV     EAX, EDX            // Count = 32..63
        CDQ                         // EDX:EAX := EDX (signed expanded)
        SAR     EAX, CL                                 
        JMP     @@03

@@02:   SHRD    EAX, EDX, CL        // Count = 1..31
        SAR     EDX, CL
@@03:
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

function SetBit(const Value: Byte; const Bit: TBitRange): Byte;
begin
  Result := Value or (1 shl (Bit mod BitsPerByte));
end;

//--------------------------------------------------------------------------------------------------

function SetBit(const Value: ShortInt; const Bit: TBitRange): ShortInt;
begin
  Result := Value or (1 shl (Bit mod BitsPerShortInt));
end;

//--------------------------------------------------------------------------------------------------

function SetBit(const Value: Word; const Bit: TBitRange): Word;
begin
  Result := Value or (1 shl (Bit mod BitsPerWord));
end;

//--------------------------------------------------------------------------------------------------

function SetBit(const Value: SmallInt; const Bit: TBitRange): SmallInt;
begin
  Result := Value or (1 shl (Bit mod BitsPerSmallInt));
end;

//--------------------------------------------------------------------------------------------------

function SetBit(const Value: LongWord; const Bit: TBitRange): LongWord;
begin
  Result := Value or (1 shl (Bit mod BitsPerLongWord));
end;

//--------------------------------------------------------------------------------------------------

function SetBit(const Value: LongInt; const Bit: TBitRange): LongInt;
begin
  Result := Value or (1 shl (Bit mod BitsPerLongInt));
end;

//--------------------------------------------------------------------------------------------------

function SetBit(const Value: Int64; const Bit: TBitRange): Int64;
begin
  Result := Value or (Int64(1) shl (Bit mod BitsPerInt64));
end;

//--------------------------------------------------------------------------------------------------

procedure SetBitBuffer(var Value; const Bit: TBitRange);
var
  P: PByte;
  BitOfs: TBitRange;
begin
  P := Addr(Value);
  Inc(P, Bit div BitsPerByte);
  BitOfs := Bit mod BitsPerByte;
  P^ := SetBit(P^, BitOfs);
end;

//--------------------------------------------------------------------------------------------------

function TestBit(const Value: Byte; const Bit: TBitRange): Boolean;
begin
  Result := (Value and (1 shl (Bit mod BitsPerByte))) <> 0;
end;

//--------------------------------------------------------------------------------------------------

function TestBit(const Value: ShortInt; const Bit: TBitRange): Boolean;
begin
  Result := (Value and (1 shl (Bit mod BitsPerShortInt))) <> 0;
end;

//--------------------------------------------------------------------------------------------------

function TestBit(const Value: Word; const Bit: TBitRange): Boolean;
begin
  Result := (Value and (1 shl (Bit mod BitsPerWord))) <> 0;
end;

//--------------------------------------------------------------------------------------------------

function TestBit(const Value: SmallInt; const Bit: TBitRange): Boolean;
begin
  Result := (Value and (1 shl (Bit mod BitsPerSmallInt))) <> 0;
end;

//--------------------------------------------------------------------------------------------------

function TestBit(const Value: LongWord; const Bit: TBitRange): Boolean;
begin
  Result := (Value and (1 shl (Bit mod BitsPerLongWord))) <> 0;
end;

//--------------------------------------------------------------------------------------------------

function TestBit(const Value: LongInt; const Bit: TBitRange): Boolean;
begin
  Result := (Value and (1 shl (Bit mod BitsPerLongInt))) <> 0;
end;

//--------------------------------------------------------------------------------------------------

function TestBit(const Value: Int64; const Bit: TBitRange): Boolean;
begin
  Result := (Value and (Int64(1) shl (Bit mod BitsPerInt64))) <> 0;
end;

//--------------------------------------------------------------------------------------------------

function TestBitBuffer(const Value; const Bit: TBitRange): Boolean;
var
  P: PByte;
  BitOfs: TBitRange;
begin
  P := Addr(Value);
  Inc(P, Bit div BitsPerByte);
  BitOfs := Bit mod BitsPerByte;
  Result := TestBit(P^, BitOfs);
end;

//--------------------------------------------------------------------------------------------------

function TestBits(const Value, Mask: Byte): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

//--------------------------------------------------------------------------------------------------

function TestBits(const Value, Mask: ShortInt): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

//--------------------------------------------------------------------------------------------------

function TestBits(const Value, Mask: Word): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

//--------------------------------------------------------------------------------------------------

function TestBits(const Value, Mask: SmallInt): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

//--------------------------------------------------------------------------------------------------

function TestBits(const Value, Mask: LongWord): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

//--------------------------------------------------------------------------------------------------

function TestBits(const Value, Mask: LongInt): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

//--------------------------------------------------------------------------------------------------

function TestBits(const Value, Mask: Int64): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

//--------------------------------------------------------------------------------------------------

function ToggleBit(const Value: Byte; const Bit: TBitRange): Byte;
begin
  Result := Value xor (1 shl (Bit mod BitsPerByte));
end;

//--------------------------------------------------------------------------------------------------

function ToggleBit(const Value: ShortInt; const Bit: TBitRange): ShortInt;
begin
  Result := Value xor (1 shl (Bit mod BitsPerShortInt));
end;

//--------------------------------------------------------------------------------------------------

function ToggleBit(const Value: Word; const Bit: TBitRange): Word;
begin
  Result := Value xor (1 shl (Bit mod BitsPerWord));
end;

//--------------------------------------------------------------------------------------------------

function ToggleBit(const Value: SmallInt; const Bit: TBitRange): SmallInt;
begin
  Result := Value xor (1 shl (Bit mod BitsPerSmallInt));
end;

//--------------------------------------------------------------------------------------------------

function ToggleBit(const Value: LongWord; const Bit: TBitRange): LongWord;
begin
  Result := Value xor (1 shl (Bit mod BitsPerLongWord));
end;

//--------------------------------------------------------------------------------------------------

function ToggleBit(const Value: LongInt; const Bit: TBitRange): LongInt;
begin
  Result := Value xor (1 shl (Bit mod BitsPerLongInt));
end;

//--------------------------------------------------------------------------------------------------

function ToggleBit(const Value: Int64; const Bit: TBitRange): Int64;
begin
  Result := Value xor (Int64(1) shl (Bit mod BitsPerInt64));
end;

//--------------------------------------------------------------------------------------------------

procedure ToggleBitBuffer(var Value; const Bit: TBitRange);
var
  P: PByte;
  BitOfs: TBitRange;
begin
  P := Addr(Value);
  Inc(P, Bit div BitsPerByte);
  BitOfs := Bit mod BitsPerByte;
  P^ := ToggleBit(P^, BitOfs);
end;

//--------------------------------------------------------------------------------------------------

procedure BooleansToBits(var Dest: Byte; const B: TBooleanArray);
var
  I, H: Integer;
begin
  Dest := 0;
  H := Min(SizeOf(Dest) * BitsPerByte - 1, High(B));
  for I := 0 to H do
    if B[I] then
      Dest := SetBit(Dest, TBitRange(I));
end;

//--------------------------------------------------------------------------------------------------

procedure BooleansToBits(var Dest: ShortInt; const B: TBooleanArray);
var
  I, H: Integer;
begin
  Dest := 0;
  H := Min(SizeOf(Dest) * BitsPerByte - 1, High(B));
  for I := 0 to H do
    if B[I] then
      Dest := SetBit(Dest, TBitRange(I));
end;

//--------------------------------------------------------------------------------------------------

procedure BooleansToBits(var Dest: Word; const B: TBooleanArray);
var
  I, H: Integer;
begin
  Dest := 0;
  H := Min(SizeOf(Dest) * BitsPerByte - 1, High(B));
  for I := 0 to H do
    if B[I] then
      Dest := SetBit(Dest, TBitRange(I));
end;

//--------------------------------------------------------------------------------------------------

procedure BooleansToBits(var Dest: SmallInt; const B: TBooleanArray);
var
  I, H: Integer;
begin
  Dest := 0;
  H := Min(SizeOf(Dest) * BitsPerByte - 1, High(B));
  for I := 0 to H do
    if B[I] then
      Dest := SetBit(Dest, TBitRange(I));
end;

//--------------------------------------------------------------------------------------------------

procedure BooleansToBits(var Dest: LongWord; const B: TBooleanArray);
var
  I, H: Integer;
begin
  Dest := 0;
  H := Min(SizeOf(Dest) * BitsPerByte - 1, High(B));
  for I := 0 to H do
    if B[I] then
      Dest := SetBit(Dest, TBitRange(I));
end;

//--------------------------------------------------------------------------------------------------

procedure BooleansToBits(var Dest: LongInt; const B: TBooleanArray);
var
  I, H: Integer;
begin
  Dest := 0;
  H := Min(SizeOf(Dest) * BitsPerByte - 1, High(B));
  for I := 0 to H do
    if B[I] then
      Dest := SetBit(Dest, TBitRange(I));
end;

//--------------------------------------------------------------------------------------------------

procedure BooleansToBits(var Dest: Int64; const B: TBooleanArray);
var
  I, H: Integer;
begin
  Dest := 0;
  H := Min(SizeOf(Dest) * BitsPerByte - 1, High(B));
  for I := 0 to H do
    if B[I] then
      Dest := SetBit(Dest, TBitRange(I));
end;

//--------------------------------------------------------------------------------------------------

procedure BitsToBooleans(const Bits: Byte; var B: TBooleanArray; AllBits: Boolean);
var
  I: Integer;
begin
  if AllBits then
    SetLength(B, SizeOf(Bits) * BitsPerByte)
  else
    SetLength(B, BitsNeeded(Bits));
  for I := 0 to High(B) do
    B[I] := TestBit(Bits, TBitRange(I));
end;

//--------------------------------------------------------------------------------------------------

procedure BitsToBooleans(const Bits: ShortInt; var B: TBooleanArray; AllBits: Boolean);
begin
  BitsToBooleans(Byte(Bits), B, AllBits);
end;

//--------------------------------------------------------------------------------------------------

procedure BitsToBooleans(const Bits: Word; var B: TBooleanArray; AllBits: Boolean);
var
  I: Integer;
begin
  if AllBits then
    SetLength(B, SizeOf(Bits) * BitsPerByte)
  else
    SetLength(B, BitsNeeded(Bits));
  for I := 0 to High(B) do
    B[I] := TestBit(Bits, TBitRange(I));
end;

//--------------------------------------------------------------------------------------------------

procedure BitsToBooleans(const Bits: SmallInt; var B: TBooleanArray; AllBits: Boolean);
begin
  BitsToBooleans(Word(Bits), B, AllBits);
end;

//--------------------------------------------------------------------------------------------------

procedure BitsToBooleans(const Bits: LongWord; var B: TBooleanArray; AllBits: Boolean);
var
  I: Integer;
begin
  if AllBits then
    SetLength(B, SizeOf(Bits) * BitsPerByte)
  else
    SetLength(B, BitsNeeded(Bits));
  for I := 0 to High(B) do
    B[I] := TestBit(Bits, TBitRange(I));
end;

//--------------------------------------------------------------------------------------------------

procedure BitsToBooleans(const Bits: LongInt; var B: TBooleanArray; AllBits: Boolean);
begin
  BitsToBooleans(LongWord(Bits), B, AllBits);
end;

//--------------------------------------------------------------------------------------------------

procedure BitsToBooleans(const Bits: Int64; var B: TBooleanArray; AllBits: Boolean);
var
  I: Integer;
begin
  if AllBits then
    SetLength(B, SizeOf(Bits) * BitsPerByte)
  else
    SetLength(B, BitsNeeded(Bits));
  for I := 0 to High(B) do
    B[I] := TestBit(Bits, TBitRange(I));
end;

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

function BitsNeeded(const X: Byte): Integer;
begin
  Result := BitsHighest(X) + 1;
  if Result = 0 then
    Result := 1;
end;

//--------------------------------------------------------------------------------------------------

function BitsNeeded(const X: ShortInt): Integer;
begin
  Result := BitsHighest(X) + 1;
  if Result = 0 then
    Result := 1;
end;

//--------------------------------------------------------------------------------------------------

function BitsNeeded(const X: Word): Integer;
begin
  Result := BitsHighest(X) + 1;
  if Result = 0 then
    Result := 1;
end;

//--------------------------------------------------------------------------------------------------

function BitsNeeded(const X: SmallInt): Integer;
begin
  Result := BitsHighest(X) + 1;
  if Result = 0 then
    Result := 1;
end;

//--------------------------------------------------------------------------------------------------

function BitsNeeded(const X: LongWord): Integer;
begin
  Result := BitsHighest(X) + 1;
  if Result = 0 then
    Result := 1;
end;

//--------------------------------------------------------------------------------------------------

function BitsNeeded(const X: LongInt): Integer;
begin
  Result := BitsHighest(X) + 1;
  if Result = 0 then
    Result := 1;
end;

//--------------------------------------------------------------------------------------------------

function BitsNeeded(const X: Int64): Integer;
begin
  Result := BitsHighest(X) + 1;
  if Result = 0 then
    Result := 1;
end;

//--------------------------------------------------------------------------------------------------

function ReverseBytes(Value: Word): Word;
begin
  Result := ((Value and Word($FF00)) shr BitsPerByte) or ((Value and Word($00FF)) shl BitsPerByte);
end;

//--------------------------------------------------------------------------------------------------

function ReverseBytes(Value: SmallInt): SmallInt;
begin
  Result := ReverseBytes(Word(Value));
end;

//--------------------------------------------------------------------------------------------------

function ReverseBytes(Value: LongWord): LongWord;
var
  I: Integer;
begin
  Result := Value and LongWord(ByteMask);
  Value := Value shr BitsPerByte;
  for I := 0 to SizeOf(LongWord) - 2 do
  begin
    Result := (Result shl BitsPerByte) or (Value and LongWord(ByteMask));
    Value := Value shr BitsPerByte;
  end;
end;

//--------------------------------------------------------------------------------------------------

function ReverseBytes(Value: LongInt): LongInt;
begin
  Result := ReverseBytes(LongWord(Value));
end;

//--------------------------------------------------------------------------------------------------

{ TODO : find better solution }
function ReverseBytes(Value: Int64): Int64;
var
  I: Integer;
begin
  Result := Value and ByteMask;
  Value := Value shr BitsPerByte;
  for I := 0 to SizeOf(Int64) - 2 do
  begin
    Result := (Result shl BitsPerByte) or (Value and ByteMask);
    Value := Value shr BitsPerByte;
  end;
end;

//--------------------------------------------------------------------------------------------------

function ReverseBytes(P: Pointer; Count: Integer): Pointer;
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
      T := P1^;
      P1^ := P2^;
      P2^ := T;
      Inc(P1);
      Dec(P2);
    end;
  end;
  Result := P;
end;

//==================================================================================================
// Arithmetic
//==================================================================================================

procedure SwapOrd(var I, J: Byte);
var
  T: Byte;
begin
  T := I;
  I := J;
  J := T;
end;

//--------------------------------------------------------------------------------------------------

procedure SwapOrd(var I, J: ShortInt);
var
  T: ShortInt;
begin
  T := I;
  I := J;
  J := T;
end;

//--------------------------------------------------------------------------------------------------

procedure SwapOrd(var I, J: Word);
var
  T: Word;
begin
  T := I;
  I := J;
  J := T;
end;

//--------------------------------------------------------------------------------------------------

procedure SwapOrd(var I, J: SmallInt);
var
  T: SmallInt;
begin
  T := I;
  I := J;
  J := T;
end;

//--------------------------------------------------------------------------------------------------

procedure SwapOrd(var I, J: LongWord);
var
  T: LongWord;
begin
  T := I;
  I := J;
  J := T;
end;

//--------------------------------------------------------------------------------------------------

procedure SwapOrd(var I, J: LongInt);
var
  T: LongInt;
begin
  T := I;
  I := J;
  J := T;
end;

//--------------------------------------------------------------------------------------------------

procedure SwapOrd(var I, J: Int64);
var
  T: Int64;
begin
  T := I;
  I := J;
  J := T;
end;

//--------------------------------------------------------------------------------------------------

{ TODO : potential overflow problem }
function IncLimit(var B: Byte; const Limit, Incr: Byte): Byte;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function IncLimit(var B: ShortInt; const Limit, Incr: ShortInt): ShortInt;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function IncLimit(var B: Word; const Limit, Incr: Word): Word;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function IncLimit(var B: SmallInt; const Limit, Incr: SmallInt): SmallInt;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function IncLimit(var B: LongWord; const Limit, Incr: LongWord): LongWord;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function IncLimit(var B: LongInt; const Limit, Incr: LongInt): LongInt;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function IncLimit(var B: Int64; const Limit, Incr: Int64): Int64;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

{ TODO : potential underflow problem }
function DecLimit(var B: Byte; const Limit, Decr: Byte): Byte;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function DecLimit(var B: ShortInt; const Limit, Decr: ShortInt): ShortInt;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function DecLimit(var B: Word; const Limit, Decr: Word): Word;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function DecLimit(var B: SmallInt; const Limit, Decr: SmallInt): SmallInt;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function DecLimit(var B: LongWord; const Limit, Decr: LongWord): LongWord;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function DecLimit(var B: LongInt; const Limit, Decr: LongInt): LongInt;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function DecLimit(var B: Int64; const Limit, Decr: Int64): Int64;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

{ TODO : potential overflow problem }
function IncLimitClamp(var B: Byte; const Limit, Incr: Byte): Byte;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function IncLimitClamp(var B: ShortInt; const Limit, Incr: ShortInt): ShortInt;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function IncLimitClamp(var B: Word; const Limit, Incr: Word): Word;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function IncLimitClamp(var B: SmallInt; const Limit, Incr: SmallInt): SmallInt;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function IncLimitClamp(var B: LongWord; const Limit, Incr: LongWord): LongWord;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function IncLimitClamp(var B: LongInt; const Limit, Incr: LongInt): LongInt;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function IncLimitClamp(var B: Int64; const Limit, Incr: Int64): Int64;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

{ TODO : potential underflow problem }
function DecLimitClamp(var B: Byte; const Limit, Decr: Byte): Byte;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function DecLimitClamp(var B: ShortInt; const Limit, Decr: ShortInt): ShortInt;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function DecLimitClamp(var B: Word; const Limit, Decr: Word): Word;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function DecLimitClamp(var B: SmallInt; const Limit, Decr: SmallInt): SmallInt;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function DecLimitClamp(var B: LongWord; const Limit, Decr: LongWord): LongWord;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function DecLimitClamp(var B: LongInt; const Limit, Decr: LongInt): LongInt;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function DecLimitClamp(var B: Int64; const Limit, Decr: Int64): Int64;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function Max(const B1, B2: Byte): Byte;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

//--------------------------------------------------------------------------------------------------

function Min(const B1, B2: Byte): Byte;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

//--------------------------------------------------------------------------------------------------

function Max(const B1, B2: ShortInt): ShortInt;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

//--------------------------------------------------------------------------------------------------

function Min(const B1, B2: ShortInt): ShortInt;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

//--------------------------------------------------------------------------------------------------

function Max(const B1, B2: Word): Word;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

//--------------------------------------------------------------------------------------------------

function Min(const B1, B2: Word): Word;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

//--------------------------------------------------------------------------------------------------

function Max(const B1, B2: SmallInt): SmallInt;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

//--------------------------------------------------------------------------------------------------

function Min(const B1, B2: SmallInt): SmallInt;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

//--------------------------------------------------------------------------------------------------

function Max(const B1, B2: LongWord): LongWord;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

//--------------------------------------------------------------------------------------------------

function Min(const B1, B2: LongWord): LongWord;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

//--------------------------------------------------------------------------------------------------

function Max(const B1, B2: LongInt): LongInt;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

//--------------------------------------------------------------------------------------------------

function Min(const B1, B2: LongInt): LongInt;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

//--------------------------------------------------------------------------------------------------

function Max(const B1, B2: Int64): Int64;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

//--------------------------------------------------------------------------------------------------

function Min(const B1, B2: Int64): Int64;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

// History:

// $Log$
// Revision 1.7  2004/05/05 00:09:59  mthoma
// Updated headers: Added donors as contributors, adjusted the initial authors, added cvs names when they were not obvious. Changed $data to $date where necessary,
//
// Revision 1.6  2004/04/14 23:07:34  peterjhaas
// add missing types to different functions
// add pure pascal implementations
// some bugfixes, a.o. sar with Count >= bit count
//
// Revision 1.5  2004/04/06 04:53:18  peterjhaas
// adapt compiler conditions, add log entry
//

end.
