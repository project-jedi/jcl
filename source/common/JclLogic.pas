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
{   Peter J. Haas (PeterJHaas) jediplus att pjh2 dott de, Martin Kimmings, Robert Marquardt,       }
{   Chris Morris,  Michael Schnell, Matthias Thoma, Petr Vones                                     }
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
function OrdToBinary(const Value: Shortint): string; overload;
function OrdToBinary(const Value: Smallint): string; overload;
function OrdToBinary(const Value: Word): string; overload;
function OrdToBinary(const Value: Longword): string; overload;
function OrdToBinary(const Value: Longint): string; overload;
function OrdToBinary(const Value: Int64): string; overload;

//--------------------------------------------------------------------------------------------------
// Bit manipulation
//--------------------------------------------------------------------------------------------------

type
  TBitRange = Byte;
  TBooleanArray = TDynBooleanArray;

function BitsHighest(X: Byte): Integer; overload;
function BitsHighest(X: Shortint): Integer; overload;
function BitsHighest(X: Word): Integer; overload;
function BitsHighest(X: Smallint): Integer; overload;
function BitsHighest(X: Longword): Integer; overload;
function BitsHighest(X: Longint): Integer; overload;
function BitsHighest(X: Int64): Integer; overload;

function BitsLowest(X: Byte): Integer; overload;
function BitsLowest(X: Shortint): Integer; overload;
function BitsLowest(X: Word): Integer; overload;
function BitsLowest(X: Smallint): Integer; overload;
function BitsLowest(X: Longword): Integer; overload;
function BitsLowest(X: Longint): Integer; overload;
function BitsLowest(X: Int64): Integer; overload;

function ClearBit(const Value: Byte; const Bit: TBitRange): Byte; overload;
function ClearBit(const Value: Shortint; const Bit: TBitRange): Shortint; overload;
function ClearBit(const Value: Word; const Bit: TBitRange): Word; overload;
function ClearBit(const Value: Smallint; const Bit: TBitRange): Smallint; overload;
function ClearBit(const Value: Longword; const Bit: TBitRange): Longword; overload;
function ClearBit(const Value: Longint; const Bit: TBitRange): Longint; overload;
function ClearBit(const Value: Int64; const Bit: TBitRange): Int64; overload;
{ TODO : Bit should be Cardinal }
procedure ClearBitBuffer(var Value; const Bit: TBitRange); { TODO -cHelp : document }

function CountBitsSet(X: Byte): Integer; overload;
function CountBitsSet(X: Shortint): Integer; overload;
function CountBitsSet(X: Word): Integer; overload;
function CountBitsSet(X: Smallint): Integer; overload;
function CountBitsSet(X: Longword): Integer; overload;
function CountBitsSet(X: Longint): Integer; overload;
function CountBitsSet(X: Int64): Integer; overload;

function CountBitsCleared(X: Byte): Integer; overload;
function CountBitsCleared(X: Shortint): Integer; overload;
function CountBitsCleared(X: Word): Integer; overload;
function CountBitsCleared(X: Smallint): Integer; overload;
function CountBitsCleared(X: Longword): Integer; overload;
function CountBitsCleared(X: Longint): Integer; overload;
function CountBitsCleared(X: Int64): Integer; overload;

function LRot(const Value: Byte; const Count: TBitRange): Byte; overload;
function LRot(const Value: Shortint; const Count: TBitRange): Shortint; overload;
function LRot(const Value: Word; const Count: TBitRange): Word; overload;
function LRot(const Value: Smallint; const Count: TBitRange): Smallint; overload;
function LRot(const Value: Longword; const Count: TBitRange): Longword; overload;
function LRot(const Value: Longint; const Count: TBitRange): Longint; overload;
function LRot(const Value: Int64; const Count: TBitRange): Int64; overload;

function ReverseBits(Value: Byte): Byte; overload;
function ReverseBits(Value: Shortint): Shortint; overload;
function ReverseBits(Value: Word): Word; overload;
function ReverseBits(Value: Smallint): Smallint; overload;
function ReverseBits(Value: Longword): Longword; overload;
function ReverseBits(Value: Longint): Longint; overload;
function ReverseBits(Value: Int64): Int64; overload;
{ TODO : Count should be Cardinal }
function ReverseBits(P: Pointer; Count: Integer): Pointer; overload;

function RRot(const Value: Byte; const Count: TBitRange): Byte; overload;
function RRot(const Value: Shortint; const Count: TBitRange): Shortint; overload;
function RRot(const Value: Word; const Count: TBitRange): Word; overload;
function RRot(const Value: Smallint; const Count: TBitRange): Smallint; overload;
function RRot(const Value: Longword; const Count: TBitRange): Longword; overload;
function RRot(const Value: Longint; const Count: TBitRange): Longint; overload;
function RRot(const Value: Int64; const Count: TBitRange): Int64; overload;

function Sar(const Value: Shortint; const Count: TBitRange): Shortint; overload;
function Sar(const Value: Smallint; const Count: TBitRange): Smallint; overload;
function Sar(const Value: Longint; const Count: TBitRange): Longint; overload;
function Sar(const Value: Int64; const Count: TBitRange): Int64; overload;

function SetBit(const Value: Byte; const Bit: TBitRange): Byte; overload;
function SetBit(const Value: Shortint; const Bit: TBitRange): Shortint; overload;
function SetBit(const Value: Word; const Bit: TBitRange): Word; overload;
function SetBit(const Value: Smallint; const Bit: TBitRange): Smallint; overload;
function SetBit(const Value: Longword; const Bit: TBitRange): Longword; overload;
function SetBit(const Value: Longint; const Bit: TBitRange): Longint; overload;
function SetBit(const Value: Int64; const Bit: TBitRange): Int64; overload;
{ TODO : Bit should be Cardinal }
procedure SetBitBuffer(var Value; const Bit: TBitRange); { TODO -cHelp : document }

function TestBit(const Value: Byte; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: Shortint; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: Word; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: Smallint; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: Longword; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: Longint; const Bit: TBitRange): Boolean; overload;
function TestBit(const Value: Int64; const Bit: TBitRange): Boolean; overload;
{ TODO : Bit should be Cardinal }
function TestBitBuffer(const Value; const Bit: TBitRange): Boolean; { TODO -cHelp : document }

function TestBits(const Value, Mask: Byte): Boolean; overload;
function TestBits(const Value, Mask: Shortint): Boolean; overload;
function TestBits(const Value, Mask: Word): Boolean; overload;
function TestBits(const Value, Mask: Smallint): Boolean; overload;
function TestBits(const Value, Mask: Longword): Boolean; overload;
function TestBits(const Value, Mask: Longint): Boolean; overload;
function TestBits(const Value, Mask: Int64): Boolean; overload;

function ToggleBit(const Value: Byte; const Bit: TBitRange): Byte; overload;
function ToggleBit(const Value: Shortint; const Bit: TBitRange): Shortint; overload;
function ToggleBit(const Value: Word; const Bit: TBitRange): Word; overload;
function ToggleBit(const Value: Smallint; const Bit: TBitRange): Smallint; overload;
function ToggleBit(const Value: Longword; const Bit: TBitRange): Longword; overload;
function ToggleBit(const Value: Longint; const Bit: TBitRange): Longint; overload;
function ToggleBit(const Value: Int64; const Bit: TBitRange): Int64; overload;
{ TODO : Bit should be Cardinal }
procedure ToggleBitBuffer(var Value; const Bit: TBitRange); { TODO -cHelp : document }

procedure BooleansToBits(var Dest: Byte; const B: TBooleanArray); overload;
procedure BooleansToBits(var Dest: Shortint; const B: TBooleanArray); overload;
procedure BooleansToBits(var Dest: Word; const B: TBooleanArray); overload;
procedure BooleansToBits(var Dest: Smallint; const B: TBooleanArray); overload;
procedure BooleansToBits(var Dest: Longword; const B: TBooleanArray); overload;
procedure BooleansToBits(var Dest: Longint; const B: TBooleanArray); overload;
procedure BooleansToBits(var Dest: Int64; const B: TBooleanArray); overload;

procedure BitsToBooleans(const Bits: Byte; var B: TBooleanArray; AllBits: Boolean = False); overload;
procedure BitsToBooleans(const Bits: Shortint; var B: TBooleanArray; AllBits: Boolean = False); overload;
procedure BitsToBooleans(const Bits: Word; var B: TBooleanArray; AllBits: Boolean = False); overload;
procedure BitsToBooleans(const Bits: Smallint; var B: TBooleanArray; AllBits: Boolean = False); overload;
procedure BitsToBooleans(const Bits: Longword; var B: TBooleanArray; AllBits: Boolean = False); overload;
procedure BitsToBooleans(const Bits: Longint; var B: TBooleanArray; AllBits: Boolean = False); overload;
procedure BitsToBooleans(const Bits: Int64; var B: TBooleanArray; AllBits: Boolean = False); overload;

function BitsNeeded(const X: Byte): Integer; overload;
function BitsNeeded(const X: Shortint): Integer; overload;
function BitsNeeded(const X: Word): Integer; overload;
function BitsNeeded(const X: Smallint): Integer; overload;
function BitsNeeded(const X: Longword): Integer; overload;
function BitsNeeded(const X: Longint): Integer; overload;
function BitsNeeded(const X: Int64): Integer; overload;

function Digits(const X: Cardinal): Integer;  { TODO : should named DecimalDigits }

function ReverseBytes(Value: Word): Word; overload;
function ReverseBytes(Value: Smallint): Smallint; overload;
function ReverseBytes(Value: Longword): Longword; overload;
function ReverseBytes(Value: Longint): Longint; overload;
function ReverseBytes(Value: Int64): Int64; overload;
function ReverseBytes(P: Pointer; Count: Integer): Pointer; overload;

//--------------------------------------------------------------------------------------------------
// Arithmetic
//--------------------------------------------------------------------------------------------------

procedure SwapOrd(var I, J: Byte); overload;
procedure SwapOrd(var I, J: Shortint); overload;
procedure SwapOrd(var I, J: Word); overload;
procedure SwapOrd(var I, J: Smallint); overload;
procedure SwapOrd(var I, J: Longword); overload;
procedure SwapOrd(var I, J: Longint); overload;
procedure SwapOrd(var I, J: Int64); overload;

function IncLimit(var B: Byte; const Limit: Byte; const Incr: Byte = 1): Byte; overload;
function IncLimit(var B: Shortint; const Limit: Shortint; const Incr: Shortint = 1): Shortint; overload;
function IncLimit(var B: Word; const Limit: Word; const Incr: Word = 1): Word; overload;
function IncLimit(var B: Smallint; const Limit: Smallint; const Incr: Smallint = 1): Smallint; overload;
function IncLimit(var B: Longword; const Limit: Longword; const Incr: Longword = 1): Longword; overload;
function IncLimit(var B: Longint; const Limit: Longint; const Incr: Longint = 1): Longint; overload;
function IncLimit(var B: Int64; const Limit: Int64; const Incr: Int64 = 1): Int64; overload;

function DecLimit(var B: Byte; const Limit: Byte; const Decr: Byte = 1): Byte; overload;
function DecLimit(var B: Shortint; const Limit: Shortint; const Decr: Shortint = 1): Shortint; overload;
function DecLimit(var B: Word; const Limit: Word; const Decr: Word = 1): Word; overload;
function DecLimit(var B: Smallint; const Limit: Smallint; const Decr: Smallint = 1): Smallint; overload;
function DecLimit(var B: Longword; const Limit: Longword; const Decr: Longword = 1): Longword; overload;
function DecLimit(var B: Longint; const Limit: Longint; const Decr: Longint = 1): Longint; overload;
function DecLimit(var B: Int64; const Limit: Int64; const Decr: Int64 = 1): Int64; overload;

function IncLimitClamp(var B: Byte; const Limit: Byte; const Incr: Byte = 1): Byte; overload;
function IncLimitClamp(var B: Shortint; const Limit: Shortint; const Incr: Shortint = 1): Shortint; overload;
function IncLimitClamp(var B: Word; const Limit: Word; const Incr: Word = 1): Word; overload;
function IncLimitClamp(var B: Smallint; const Limit: Smallint; const Incr: Smallint = 1): Smallint; overload;
function IncLimitClamp(var B: Longword; const Limit: Longword; const Incr: Longword = 1): Longword; overload;
function IncLimitClamp(var B: Longint; const Limit: Longint; const Incr: Longint = 1): Longint; overload;
function IncLimitClamp(var B: Int64; const Limit: Int64; const Incr: Int64 = 1): Int64; overload;

function DecLimitClamp(var B: Byte; const Limit: Byte; const Decr: Byte = 1): Byte; overload;
function DecLimitClamp(var B: Shortint; const Limit: Shortint; const Decr: Shortint = 1): Shortint; overload;
function DecLimitClamp(var B: Word; const Limit: Word; const Decr: Word = 1): Word; overload;
function DecLimitClamp(var B: Smallint; const Limit: Smallint; const Decr: Smallint = 1): Smallint; overload;
function DecLimitClamp(var B: Longword; const Limit: Longword; const Decr: Longword = 1): Longword; overload;
function DecLimitClamp(var B: Longint; const Limit: Longint; const Decr: Longint = 1): Longint; overload;
function DecLimitClamp(var B: Int64; const Limit: Int64; const Decr: Int64 = 1): Int64; overload;

function Max(const B1, B2: Byte): Byte; overload;
function Max(const B1, B2: Shortint): Shortint; overload;
function Max(const B1, B2: Word): Word; overload;
function Max(const B1, B2: Smallint): Smallint; overload;
function Max(const B1, B2: Longword): Longword; overload;
function Max(const B1, B2: Longint): Longint; overload;
function Max(const B1, B2: Int64): Int64; overload;

function Min(const B1, B2: Byte): Byte; overload;
function Min(const B1, B2: Shortint): Shortint; overload;
function Min(const B1, B2: Word): Word; overload;
function Min(const B1, B2: Smallint): Smallint; overload;
function Min(const B1, B2: Longword): Longword; overload;
function Min(const B1, B2: Longint): Longint; overload;
function Min(const B1, B2: Int64): Int64; overload;

implementation

type
  PByte = ^Byte;

const
  // Constants defining the number of bits in each Integer type

  BitsPerNibble   = 4;
  BitsPerByte     = 8;
  BitsPerShortInt = SizeOf(Shortint) * BitsPerByte;
  BitsPerWord     = SizeOf(Word) * BitsPerByte;
  BitsPerSmallInt = SizeOf(Smallint) * BitsPerByte;
  BitsPerLongWord = SizeOf(Longword) * BitsPerByte;
  BitsPerLongInt  = SizeOf(Longint) * BitsPerByte;
  BitsPerInt64    = SizeOf(Int64) * BitsPerByte;
  BitsPerCardinal = SizeOf(Cardinal) * BitsPerByte;
  BitsPerInteger  = SizeOf(Integer) * BitsPerByte;

  // Constants defining the number of nibbles in each Integer type

  NibblesPerByte     = BitsPerByte div BitsPerNibble;
  NibblesPerShortInt = SizeOf(Shortint) * NibblesPerByte;
  NibblesPerWord     = SizeOf(Word) * NibblesPerByte;
  NibblesPerSmallInt = SizeOf(Smallint) * NibblesPerByte;
  NibblesPerLongWord = SizeOf(Longword) * NibblesPerByte;
  NibblesPerLongInt  = SizeOf(Longint) * NibblesPerByte;
  NibblesPerInt64    = SizeOf(Int64) * NibblesPerByte;
  NibblesPerCardinal = SizeOf(Cardinal) * NibblesPerByte;
  NibblesPerInteger  = SizeOf(Integer) * NibblesPerByte;

  // Constants defining a mask with all bits set for each Integer type

  NibbleMask   = $F;
  ByteMask     = Byte($FF);
  ShortIntMask = Shortint($FF);
  WordMask     = Word($FFFF);
  SmallIntMask = Smallint($FFFF);
  LongWordMask = Longword($FFFFFFFF);
  LongIntMask  = Longint($FFFFFFFF);
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

function OrdToBinary(const Value: Shortint): string;
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

function OrdToBinary(const Value: Smallint): string;
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

function OrdToBinary(const Value: Longword): string;
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

function OrdToBinary(const Value: Longint): string;
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
  Result := BitsHighest(Longword(X) and Longword(ByteMask));
end;

//--------------------------------------------------------------------------------------------------

function BitsHighest(X: Shortint): Integer;
begin
  Result := BitsHighest(Longword(Byte(X)));
end;

//--------------------------------------------------------------------------------------------------

function BitsHighest(X: Word): Integer;
begin
  Result := BitsHighest(Longword(X) and Longword(WordMask));
end;

//--------------------------------------------------------------------------------------------------

function BitsHighest(X: Smallint): Integer;
begin
  Result := BitsHighest(Longword(X) and Longword(WordMask));
end;


//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function BitsHighest(X: Longword): Integer;
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
function BitsHighest(X: Longword): Integer; assembler;
asm
        MOV     ECX, EAX
        MOV     EAX, -1
        BSR     EAX, ECX
end;
{$ENDIF PUREPASCAL}

//--------------------------------------------------------------------------------------------------

function BitsHighest(X: Longint): Longint;
begin
  Result := BitsHighest(Longword(X));
end;

//--------------------------------------------------------------------------------------------------

function BitsHighest(X: Int64): Integer;
begin
  if TULargeInteger(X).HighPart = 0 then
  begin
    if TULargeInteger(X).LowPart = 0 then
      Result := -1
    else
      Result := BitsHighest(Longword(TULargeInteger(X).LowPart));
  end
  else
    Result := BitsHighest(Longword(TULargeInteger(X).HighPart)) + 32;
end;

//--------------------------------------------------------------------------------------------------

function BitsLowest(X: Byte): Integer;
begin
  Result := BitsLowest(Longword(X) and Longword(ByteMask));
end;

//--------------------------------------------------------------------------------------------------

function BitsLowest(X: Shortint): Integer;
begin
  Result := BitsLowest(Longword(X) and Longword(ByteMask));
end;

//--------------------------------------------------------------------------------------------------

function BitsLowest(X: Word): Integer;
begin
  Result := BitsLowest(Longword(X) and Longword(WordMask));
end;

//--------------------------------------------------------------------------------------------------

function BitsLowest(X: Smallint): Integer;
begin
  Result := BitsLowest(Longword(X) and Longword(WordMask));
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function BitsLowest(X: Longword): Integer;
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
function BitsLowest(X: Longword): Integer; assembler;
asm
        MOV     ECX, EAX
        MOV     EAX, -1
        BSF     EAX, ECX
end;
{$ENDIF PUREPASCAL}

//--------------------------------------------------------------------------------------------------

function BitsLowest(X: Longint): Integer;
begin
  Result := BitsLowest(Longword(X));
end;

//--------------------------------------------------------------------------------------------------

function BitsLowest(X: Int64): Integer;
begin
  if TULargeInteger(X).LowPart = 0 then
  begin
    if TULargeInteger(X).HighPart = 0 then
      Result := -1
    else
      Result := BitsLowest(Longword(TULargeInteger(X).HighPart)) + 32;
  end
  else
    Result := BitsLowest(Longword(TULargeInteger(X).LowPart));
end;

//--------------------------------------------------------------------------------------------------

function ClearBit(const Value: Byte; const Bit: TBitRange): Byte;
begin
  Result := Value and not (1 shl (Bit mod (SizeOf(Value) * BitsPerByte)));
end;

//--------------------------------------------------------------------------------------------------

function ClearBit(const Value: Shortint; const Bit: TBitRange): Shortint;
begin
  Result := Value and not (1 shl (Bit mod (SizeOf(Value) * BitsPerByte)));
end;

//--------------------------------------------------------------------------------------------------

function ClearBit(const Value: Word; const Bit: TBitRange): Word;
begin
  Result := Value and not (1 shl (Bit mod (SizeOf(Value) * BitsPerByte)));
end;

//--------------------------------------------------------------------------------------------------

function ClearBit(const Value: Smallint; const Bit: TBitRange): Smallint;
begin
  Result := Value and not (1 shl (Bit mod (SizeOf(Value) * BitsPerByte)));
end;

//--------------------------------------------------------------------------------------------------

function ClearBit(const Value: Longword; const Bit: TBitRange): Longword;
begin
  Result := Value and not (1 shl (Bit mod (SizeOf(Value) * BitsPerByte)));
end;

//--------------------------------------------------------------------------------------------------

function ClearBit(const Value: Longint; const Bit: TBitRange): Longint;
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

function CountBitsSet(X: Shortint): Integer;
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

function CountBitsSet(X: Smallint): Integer;
begin
  Result := CountBitsSet(Word(X));
end;

//--------------------------------------------------------------------------------------------------

function CountBitsSet(X: Longword): Integer;
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

function CountBitsSet(X: Longint): Integer;
begin
  Result := CountBitsSet(Longword(X));
end;

//--------------------------------------------------------------------------------------------------

function CountBitsSet(X: Int64): Integer;
begin
  Result := CountBitsSet(Longword(TULargeInteger(X).LowPart)) +
            CountBitsSet(Longword(TULargeInteger(X).HighPart));
end;

//--------------------------------------------------------------------------------------------------

function CountBitsCleared(X: Byte): Integer;
begin
  Result := BitsPerByte - CountBitsSet(Byte(X));
end;

//--------------------------------------------------------------------------------------------------

function CountBitsCleared(X: Shortint): Integer;
begin
  Result := BitsPerShortInt - CountBitsSet(Byte(X));
end;

//--------------------------------------------------------------------------------------------------

function CountBitsCleared(X: Word): Integer;
begin
  Result := BitsPerWord - CountBitsSet(Word(X));
end;

//--------------------------------------------------------------------------------------------------

function CountBitsCleared(X: Smallint): Integer;
begin
  Result := BitsPerSmallInt - CountBitsSet(Word(X));
end;

//--------------------------------------------------------------------------------------------------

function CountBitsCleared(X: Longword): Integer;
begin
  Result := BitsPerLongWord - CountBitsSet(Longword(X));
end;

//--------------------------------------------------------------------------------------------------

function CountBitsCleared(X: Longint): Integer;
begin
  Result := BitsPerLongInt - CountBitsSet(Longword(X));
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
{$ENDIF PUREPASCAL}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function LRot(const Value: Shortint; const Count: TBitRange): Shortint;
begin
  // direct implementation need typecast to prevent signed expand
  Result := LRot(Byte(Value), Count);
end;
{$ELSE}
function LRot(const Value: Shortint; const Count: TBitRange): Shortint; assembler;
asm
        MOV     CL, Count
        ROL     AL, CL
end;
{$ENDIF PUREPASCAL}

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
{$ENDIF PUREPASCAL}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function LRot(const Value: Smallint; const Count: TBitRange): Smallint;
begin
  // direct implementation need typecast to prevent signed expand
  Result := LRot(Word(Value), Count);
end;
{$ELSE}
function LRot(const Value: Smallint; const Count: TBitRange): Smallint; assembler;
asm
       MOV     CL, Count
       ROL     AX, CL
end;
{$ENDIF PUREPASCAL}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function LRot(const Value: Longword; const Count: TBitRange): Longword;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  C: Integer;
begin
  C := Count and (BitCount - 1);
  Result := (Value shl C) or (Value shr (BitCount - C));
end;
{$ELSE}
function LRot(const Value: Longword; const Count: TBitRange): Longword; assembler;
asm
        MOV     CL, Count
        ROL     EAX, CL
end;
{$ENDIF PUREPASCAL}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function LRot(const Value: Longint; const Count: TBitRange): Longint;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  C: Integer;
begin
  C := Count and (BitCount - 1);
  // for Longint shr is a real shr
  Result := (Value shl C) or (Value shr (BitCount - C));
end;
{$ELSE}
function LRot(const Value: Longint; const Count: TBitRange): Longint; assembler;
asm
        MOV     CL, Count
        ROL     EAX, CL
end;
{$ENDIF PUREPASCAL}

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
{$ENDIF PUREPASCAL}

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

function ReverseBits(Value: Shortint): Shortint;
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

function ReverseBits(Value: Smallint): Smallint;
begin
  Result := ReverseBits(Word(Value));
end;

//--------------------------------------------------------------------------------------------------

function ReverseBits(Value: Longword): Longword;
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

function ReverseBits(Value: Longint): Longint;
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
{$ENDIF PUREPASCAL}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function RRot(const Value: Shortint; const Count: TBitRange): Shortint;
begin
  // direct implementation need typecast to prevent signed expand
  Result := RRot(Byte(Value), Count);
end;
{$ELSE}
function RRot(const Value: Shortint; const Count: TBitRange): Shortint; assembler;
asm
        MOV     ECX, EDX
        ROR     AL, CL
end;
{$ENDIF PUREPASCAL}

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
{$ENDIF PUREPASCAL}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function RRot(const Value: Smallint; const Count: TBitRange): Smallint;
begin
  // direct implementation need typecast to prevent signed expand
  Result := RRot(Word(Value), Count);
end;
{$ELSE}
function RRot(const Value: Smallint; const Count: TBitRange): Smallint; assembler;
asm
        MOV     ECX, EDX
        ROR     AX, CL
end;
{$ENDIF PUREPASCAL}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function RRot(const Value: Longword; const Count: TBitRange): Longword;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  C: Integer;
begin
  C := Count and (BitCount - 1);
  Result := (Value shr C) or (Value shl (BitCount - C));
end;
{$ELSE}
function RRot(const Value: Longword; const Count: TBitRange): Longword; assembler;
asm
        MOV     ECX, EDX
        ROR     EAX, CL
end;
{$ENDIF PUREPASCAL}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function RRot(const Value: Longint; const Count: TBitRange): Longint;
const
  BitCount = SizeOf(Value) * BitsPerByte;
var
  C: Integer;
begin
  C := Count and (BitCount - 1);
  Result := (Value shr C) or (Value shl (BitCount - C));
end;
{$ELSE}
function RRot(const Value: Longint; const Count: TBitRange): Longint; assembler;
asm
        MOV     ECX, EDX
        ROR     EAX, CL
end;
{$ENDIF PUREPASCAL}

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
{$ENDIF PUREPASCAL}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function Sar(const Value: Shortint; const Count: TBitRange): Shortint;
begin
  if Count >= 8 then
  begin
    if Value < 0 then
      Result := -1
    else
      Result := 0;
  end
  else
    Result := Shortint(Integer(Value) shr Count);
end;
{$ELSE}
function Sar(const Value: Shortint; const Count: TBitRange): Shortint; assembler;
asm
        MOV     CL, DL
        CMP     CL, 8
        JL      @@01
        MOV     CL, 7
@@01:   SAR     AL, CL
end;
{$ENDIF PUREPASCAL}

//--------------------------------------------------------------------------------------------------

{$IFDEF PUREPASCAL}
function Sar(const Value: Smallint; const Count: TBitRange): Smallint;
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
function Sar(const Value: Smallint; const Count: TBitRange): Smallint; assembler;
asm
        MOV     CL, DL
        CMP     CL, 16
        JL      @@01
        MOV     CL, 15
@@01:   SAR     AX, CL
end;
{$ENDIF PUREPASCAL}

//--------------------------------------------------------------------------------------------------

const
  SignedExpandOrMasks: array [1..31] of Longword = (
    $80000000, $C0000000, $E0000000, $F0000000,
    $F8000000, $FC000000, $FE000000, $FF000000,
    $FF800000, $FFC00000, $FFE00000, $FFF00000,
    $FFF80000, $FFFC0000, $FFFE0000, $FFFF0000,
    $FFFF8000, $FFFFC000, $FFFFE000, $FFFFF000,
    $FFFFF800, $FFFFFC00, $FFFFFE00, $FFFFFF00,
    $FFFFFF80, $FFFFFFC0, $FFFFFFE0, $FFFFFFF0,
    $FFFFFFF8, $FFFFFFFC, $FFFFFFFE);

{$IFDEF PUREPASCAL}
function Sar(const Value: Longint; const Count: TBitRange): Longint;
begin
  case Count of
    0:
      Result := Value;
    1..31:
      begin
        Result := Value shr Count;
        if Value < 0 then
          Result := Longword(Result) or SignedExpandOrMasks[Count];
      end;
  else // > 31
    if Value < 0 then
      Result := -1
    else
      Result := 0;
  end;
end;
{$ELSE}
function Sar(const Value: Longint; const Count: TBitRange): Longint; assembler;
asm
        MOV     CL, DL
        CMP     CL, 32
        JL      @@01
        MOV     CL, 31
@@01:   SAR     EAX, CL
end;
{$ENDIF PUREPASCAL}

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
          TULargeInteger(Result).HighPart := High(Longword)
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
{$ENDIF PUREPASCAL}

//--------------------------------------------------------------------------------------------------

function SetBit(const Value: Byte; const Bit: TBitRange): Byte;
begin
  Result := Value or (1 shl (Bit mod BitsPerByte));
end;

//--------------------------------------------------------------------------------------------------

function SetBit(const Value: Shortint; const Bit: TBitRange): Shortint;
begin
  Result := Value or (1 shl (Bit mod BitsPerShortInt));
end;

//--------------------------------------------------------------------------------------------------

function SetBit(const Value: Word; const Bit: TBitRange): Word;
begin
  Result := Value or (1 shl (Bit mod BitsPerWord));
end;

//--------------------------------------------------------------------------------------------------

function SetBit(const Value: Smallint; const Bit: TBitRange): Smallint;
begin
  Result := Value or (1 shl (Bit mod BitsPerSmallInt));
end;

//--------------------------------------------------------------------------------------------------

function SetBit(const Value: Longword; const Bit: TBitRange): Longword;
begin
  Result := Value or (1 shl (Bit mod BitsPerLongWord));
end;

//--------------------------------------------------------------------------------------------------

function SetBit(const Value: Longint; const Bit: TBitRange): Longint;
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

function TestBit(const Value: Shortint; const Bit: TBitRange): Boolean;
begin
  Result := (Value and (1 shl (Bit mod BitsPerShortInt))) <> 0;
end;

//--------------------------------------------------------------------------------------------------

function TestBit(const Value: Word; const Bit: TBitRange): Boolean;
begin
  Result := (Value and (1 shl (Bit mod BitsPerWord))) <> 0;
end;

//--------------------------------------------------------------------------------------------------

function TestBit(const Value: Smallint; const Bit: TBitRange): Boolean;
begin
  Result := (Value and (1 shl (Bit mod BitsPerSmallInt))) <> 0;
end;

//--------------------------------------------------------------------------------------------------

function TestBit(const Value: Longword; const Bit: TBitRange): Boolean;
begin
  Result := (Value and (1 shl (Bit mod BitsPerLongWord))) <> 0;
end;

//--------------------------------------------------------------------------------------------------

function TestBit(const Value: Longint; const Bit: TBitRange): Boolean;
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

function TestBits(const Value, Mask: Shortint): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

//--------------------------------------------------------------------------------------------------

function TestBits(const Value, Mask: Word): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

//--------------------------------------------------------------------------------------------------

function TestBits(const Value, Mask: Smallint): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

//--------------------------------------------------------------------------------------------------

function TestBits(const Value, Mask: Longword): Boolean;
begin
  Result := (Value and Mask) = Mask;
end;

//--------------------------------------------------------------------------------------------------

function TestBits(const Value, Mask: Longint): Boolean;
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

function ToggleBit(const Value: Shortint; const Bit: TBitRange): Shortint;
begin
  Result := Value xor (1 shl (Bit mod BitsPerShortInt));
end;

//--------------------------------------------------------------------------------------------------

function ToggleBit(const Value: Word; const Bit: TBitRange): Word;
begin
  Result := Value xor (1 shl (Bit mod BitsPerWord));
end;

//--------------------------------------------------------------------------------------------------

function ToggleBit(const Value: Smallint; const Bit: TBitRange): Smallint;
begin
  Result := Value xor (1 shl (Bit mod BitsPerSmallInt));
end;

//--------------------------------------------------------------------------------------------------

function ToggleBit(const Value: Longword; const Bit: TBitRange): Longword;
begin
  Result := Value xor (1 shl (Bit mod BitsPerLongWord));
end;

//--------------------------------------------------------------------------------------------------

function ToggleBit(const Value: Longint; const Bit: TBitRange): Longint;
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

procedure BooleansToBits(var Dest: Shortint; const B: TBooleanArray);
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

procedure BooleansToBits(var Dest: Smallint; const B: TBooleanArray);
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

procedure BooleansToBits(var Dest: Longword; const B: TBooleanArray);
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

procedure BooleansToBits(var Dest: Longint; const B: TBooleanArray);
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

procedure BitsToBooleans(const Bits: Shortint; var B: TBooleanArray; AllBits: Boolean);
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

procedure BitsToBooleans(const Bits: Smallint; var B: TBooleanArray; AllBits: Boolean);
begin
  BitsToBooleans(Word(Bits), B, AllBits);
end;

//--------------------------------------------------------------------------------------------------

procedure BitsToBooleans(const Bits: Longword; var B: TBooleanArray; AllBits: Boolean);
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

procedure BitsToBooleans(const Bits: Longint; var B: TBooleanArray; AllBits: Boolean);
begin
  BitsToBooleans(Longword(Bits), B, AllBits);
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

function BitsNeeded(const X: Shortint): Integer;
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

function BitsNeeded(const X: Smallint): Integer;
begin
  Result := BitsHighest(X) + 1;
  if Result = 0 then
    Result := 1;
end;

//--------------------------------------------------------------------------------------------------

function BitsNeeded(const X: Longword): Integer;
begin
  Result := BitsHighest(X) + 1;
  if Result = 0 then
    Result := 1;
end;

//--------------------------------------------------------------------------------------------------

function BitsNeeded(const X: Longint): Integer;
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

function ReverseBytes(Value: Smallint): Smallint;
begin
  Result := ReverseBytes(Word(Value));
end;

//--------------------------------------------------------------------------------------------------

function ReverseBytes(Value: Longword): Longword;
var
  I: Integer;
begin
  Result := Value and Longword(ByteMask);
  Value := Value shr BitsPerByte;
  for I := 0 to SizeOf(Longword) - 2 do
  begin
    Result := (Result shl BitsPerByte) or (Value and Longword(ByteMask));
    Value := Value shr BitsPerByte;
  end;
end;

//--------------------------------------------------------------------------------------------------

function ReverseBytes(Value: Longint): Longint;
begin
  Result := ReverseBytes(Longword(Value));
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

procedure SwapOrd(var I, J: Shortint);
var
  T: Shortint;
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

procedure SwapOrd(var I, J: Smallint);
var
  T: Smallint;
begin
  T := I;
  I := J;
  J := T;
end;

//--------------------------------------------------------------------------------------------------

procedure SwapOrd(var I, J: Longword);
var
  T: Longword;
begin
  T := I;
  I := J;
  J := T;
end;

//--------------------------------------------------------------------------------------------------

procedure SwapOrd(var I, J: Longint);
var
  T: Longint;
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

function IncLimit(var B: Shortint; const Limit, Incr: Shortint): Shortint;
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

function IncLimit(var B: Smallint; const Limit, Incr: Smallint): Smallint;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function IncLimit(var B: Longword; const Limit, Incr: Longword): Longword;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr);
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function IncLimit(var B: Longint; const Limit, Incr: Longint): Longint;
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

function DecLimit(var B: Shortint; const Limit, Decr: Shortint): Shortint;
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

function DecLimit(var B: Smallint; const Limit, Decr: Smallint): Smallint;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function DecLimit(var B: Longword; const Limit, Decr: Longword): Longword;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr);
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function DecLimit(var B: Longint; const Limit, Decr: Longint): Longint;
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

function IncLimitClamp(var B: Shortint; const Limit, Incr: Shortint): Shortint;
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

function IncLimitClamp(var B: Smallint; const Limit, Incr: Smallint): Smallint;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function IncLimitClamp(var B: Longword; const Limit, Incr: Longword): Longword;
begin
  if (B + Incr) <= Limit then
    Inc(B, Incr)
  else
    B := Limit;
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function IncLimitClamp(var B: Longint; const Limit, Incr: Longint): Longint;
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

function DecLimitClamp(var B: Shortint; const Limit, Decr: Shortint): Shortint;
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

function DecLimitClamp(var B: Smallint; const Limit, Decr: Smallint): Smallint;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function DecLimitClamp(var B: Longword; const Limit, Decr: Longword): Longword;
begin
  if (B - Decr) >= Limit then
    Dec(B, Decr)
  else
    B := Limit;
  Result := B;
end;

//--------------------------------------------------------------------------------------------------

function DecLimitClamp(var B: Longint; const Limit, Decr: Longint): Longint;
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

function Max(const B1, B2: Shortint): Shortint;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

//--------------------------------------------------------------------------------------------------

function Min(const B1, B2: Shortint): Shortint;
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

function Max(const B1, B2: Smallint): Smallint;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

//--------------------------------------------------------------------------------------------------

function Min(const B1, B2: Smallint): Smallint;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

//--------------------------------------------------------------------------------------------------

function Max(const B1, B2: Longword): Longword;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

//--------------------------------------------------------------------------------------------------

function Min(const B1, B2: Longword): Longword;
begin
  if B1 < B2 then
    Result := B1
  else
    Result := B2;
end;

//--------------------------------------------------------------------------------------------------

function Max(const B1, B2: Longint): Longint;
begin
  if B1 > B2 then
    Result := B1
  else
    Result := B2;
end;

//--------------------------------------------------------------------------------------------------

function Min(const B1, B2: Longint): Longint;
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
// Revision 1.10  2004/07/28 18:00:50  marquardt
// various style cleanings, some minor fixes
//
// Revision 1.9  2004/06/14 13:05:18  marquardt
// style cleaning ENDIF, Tabs
//
// Revision 1.8  2004/06/14 11:05:51  marquardt
// symbols added to all ENDIFs and some other minor style changes like removing IFOPT
//
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
