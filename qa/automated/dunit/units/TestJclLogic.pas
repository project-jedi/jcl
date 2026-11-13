{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{ DUnit Test                                                                                       }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{**************************************************************************************************}

unit TestJclLogic;

interface
uses
  TestFramework,
  JclLogic;

type
  TJclLogicClearBitSetBitTest = class(TTestCase)
  published
	procedure _Byte;
	procedure _Cardinal;
	procedure _Integer;
	procedure _Shortint;
	procedure _Smallint;
	procedure _Word;
  end;

  TJclLogicMiscTest = class(TTestCase)
  published
	procedure _BitsHighest;
	procedure _BitsLowest;
	procedure _ReverseBytes;
  end;

  TJclLogicRotateTest = class(TTestCase)
  published
	procedure _LRotByte;
	procedure _LRotInteger;
	procedure _LRotInt64;
	procedure _LRotWord;
	procedure _RRotByte;
	procedure _RRotInteger;
	procedure _RRotWord;
	procedure _SarShortint;
	procedure _SarSmallint;
	procedure _SarInteger;
  end;

implementation

//==================================================================================================
// ClearBit/SetBit
//==================================================================================================

procedure TJclLogicClearBitSetBitTest._Byte;
// 0..255
begin
  // ClearBit
  CheckEquals(0, ClearBit(Byte(0), 1));
  CheckEquals(0, ClearBit(Byte(1), 0));
  CheckEquals(2, ClearBit(Byte(6), 2));
  CheckEquals(128, ClearBit(Byte(192), 6));
  CheckEquals(127, ClearBit(Byte(255), 7));
  // SetBit
  CheckEquals(1, SetBit(Byte(0), 0));
  CheckEquals(2, SetBit(Byte(0), 1));
  CheckEquals(128, SetBit(Byte(0), 7));
  CheckEquals(192, SetBit(Byte(128), 6));
  CheckEquals($FF, SetBit(Byte($FF), 5));
  // Mod by bit length
  CheckEquals($FB, ClearBit(Byte($FF), 10));
  CheckEquals(4, SetBit(Byte(0), 10));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclLogicClearBitSetBitTest._Cardinal;
begin
  // ClearBit
  CheckEquals(0, ClearBit(Cardinal(0), 1));
  CheckEquals(0, ClearBit(Cardinal(1), 0));
  CheckEquals(2, ClearBit(Cardinal(6), 2));
  CheckEquals($1010, ClearBit(Cardinal($1011), 0));
  CheckEquals($FEFF, ClearBit(Cardinal($FFFF), 8));
  CheckEquals($10101010, ClearBit(Cardinal($10111010), 16));
  CheckEquals($7FFFFFFF, ClearBit(Cardinal($FFFFFFFF), 31));
  // SetBit
  CheckEquals(1, SetBit(Cardinal(0), 0));
  CheckEquals(2, SetBit(Cardinal(0), 1));
  CheckEquals($C0, SetBit(Cardinal($80), 6));
  CheckEquals($100, SetBit(Cardinal(0), 8));
  CheckEquals($1101, SetBit(Cardinal($1001), 8));
  CheckEquals($FFFF, SetBit(Cardinal($FFFF), 5));
  CheckEquals($10111010, SetBit(Cardinal($10101010), 16));
  CheckEquals($FFFFFFFF, SetBit(Cardinal($7FFFFFFF), 31));
  // Mod by bit length
  CheckEquals($FFFFFFFB, ClearBit(Cardinal($FFFFFFFF), 34));
  CheckEquals(4, SetBit(Cardinal(0), 34));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclLogicClearBitSetBitTest._Integer;
begin
  // ClearBit
  CheckEquals(0, ClearBit(Integer(0), 1));
  CheckEquals(0, ClearBit(Integer(1), 0));
  CheckEquals(2, ClearBit(Integer(6), 2));
  CheckEquals($1010, ClearBit(Integer($1011), 0));
  CheckEquals(0, ClearBit(Low(Integer), 31));
  CheckEquals(High(Integer), ClearBit(Integer(-1), 31));
  // SetBit
  CheckEquals(1, SetBit(Integer(0), 0));
  CheckEquals(2, SetBit(Integer(0), 1));
  CheckEquals(96, SetBit(Integer(32), 6));
  CheckEquals(Low(Integer), SetBit(Integer(0), 31));
  CheckEquals(-1073741824, SetBit(Low(Integer), 30));
  CheckEquals(-1, SetBit(High(Integer), 31));
  CheckEquals(-1, SetBit(Integer(-1), 5));
  // Mod by bit length
  CheckEquals(-5, ClearBit(Integer(-1), 34));
  CheckEquals(4, SetBit(Integer(0), 34));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclLogicClearBitSetBitTest._Shortint;
// -128..127
begin
  // ClearBit
  CheckEquals(0, ClearBit(Shortint(0), 1));
  CheckEquals(0, ClearBit(Shortint(1), 0));
  CheckEquals(2, ClearBit(Shortint(6), 2));
  CheckEquals(0, ClearBit(Shortint(-128), 7));
  CheckEquals(127, ClearBit(Shortint(-1), 7));
  // SetBit
  CheckEquals(1, SetBit(Shortint(0), 0));
  CheckEquals(2, SetBit(Shortint(0), 1));
  CheckEquals(96, SetBit(Shortint(32), 6));
  CheckEquals(-128, SetBit(Shortint(0), 7));
  CheckEquals(-96, SetBit(Shortint(-128), 5));
  CheckEquals(-1, SetBit(Shortint(127), 7));
  CheckEquals(-1, SetBit(Shortint(-1), 5));
  // Mod by bit length
  CheckEquals(-5, ClearBit(Shortint(-1), 10));
  CheckEquals(4, SetBit(Shortint(0), 10));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclLogicClearBitSetBitTest._Smallint;
// -32768..32767
begin
  // ClearBit
  CheckEquals(0, ClearBit(Smallint(0), 1));
  CheckEquals(0, ClearBit(Smallint(1), 0));
  CheckEquals(2, ClearBit(Smallint(6), 2));
  CheckEquals($1010, ClearBit(Smallint($1011), 0));
  CheckEquals(0, ClearBit(Smallint(-32768), 15));
  CheckEquals(32767, ClearBit(Smallint(-1), 15));
  // SetBit
  CheckEquals(1, SetBit(Smallint(0), 0));
  CheckEquals(2, SetBit(Smallint(0), 1));
  CheckEquals(96, SetBit(Smallint(32), 6));
  CheckEquals(-32768, SetBit(Smallint(0), 15));
  CheckEquals(-16384, SetBit(Smallint(-32768), 14));
  CheckEquals(-1, SetBit(Smallint(32767), 15));
  CheckEquals(-1, SetBit(Smallint(-1), 5));
  // Mod by bit length
  CheckEquals(-5, ClearBit(Smallint(-1), 18));
  CheckEquals(4, SetBit(Smallint(0), 18));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclLogicClearBitSetBitTest._Word;
// 0..65535
begin
  // ClearBit
  CheckEquals(0, ClearBit(Word(0), 1));
  CheckEquals(0, ClearBit(Word(1), 0));
  CheckEquals(2, ClearBit(Word(6), 2));
  CheckEquals($1010, ClearBit(Word($1011), 0));
  CheckEquals($FEFF, ClearBit(Word($FFFF), 8));
  // SetBit
  CheckEquals(1, SetBit(Word(0), 0));
  CheckEquals(2, SetBit(Word(0), 1));
  CheckEquals($C0, SetBit(Word($80), 6));
  CheckEquals($100, SetBit(Word(0), 8));
  CheckEquals($1101, SetBit(Word($1001), 8));
  CheckEquals($FFFF, SetBit(Word($FFFF), 5));
  // Mod by bit length
  CheckEquals($FFFB, ClearBit(Word($FFFF), 18));
  CheckEquals(4, SetBit(Word(0), 18));
end;


//==================================================================================================
// Misc Bit Manipulation
//==================================================================================================

procedure TJclLogicMiscTest._BitsHighest;
begin
  CheckEquals(-1, BitsHighest(Cardinal(0)));
  CheckEquals(0, BitsHighest(Cardinal(1)));
  CheckEquals(16, BitsHighest(Cardinal($1FFFF)));
  CheckEquals(31, BitsHighest(High(Cardinal)));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclLogicMiscTest._BitsLowest;
begin
  CheckEquals(-1, BitsLowest(Cardinal(0)));
  CheckEquals(0, BitsLowest(Cardinal(1)));
  CheckEquals(16, BitsLowest(Cardinal($FFF10000)));
  CheckEquals(31, BitsLowest(Cardinal($80000000)));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclLogicMiscTest._ReverseBytes;
begin
  CheckEquals(Smallint($0001), ReverseBytes(Smallint($0100)));
  CheckEquals(Smallint($CDAB), ReverseBytes(Smallint($ABCD)));
  CheckEquals(Smallint($00FF), ReverseBytes(Smallint($FF00)));
  CheckEquals(Smallint($FF00), ReverseBytes(Smallint($00FF)));
end;


//==================================================================================================
// Bit Rotation
//==================================================================================================

procedure TJclLogicRotateTest._LRotByte;
begin
  CheckEquals(0, LRot(Byte(0), 1));
  CheckEquals(2, LRot(Byte(1), 1));
  CheckEquals(2, LRot(Byte($80), 2));
  CheckEquals($F0, LRot(Byte($0F), 4));
  CheckEquals($0F, LRot(Byte($F0), 4));
  CheckEquals(2, LRot(Byte($80), 10));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclLogicRotateTest._LRotInt64;
begin
  CheckEquals(0, LRot(Int64(0), 1));
  CheckEquals(2, LRot(Int64(1), 1));
  CheckEquals(2, LRot(Low(Int64), 2));
  CheckEquals(Low(Int64), LRot(Int64($80000000), 32));
  CheckEquals($80000000, LRot(Low(Int64), 32));
  CheckEquals(2, LRot(Int64(1), 65));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclLogicRotateTest._LRotInteger;
begin
  CheckEquals(0, LRot(Integer(0), 1));
  CheckEquals(2, LRot(Integer(1), 1));
  CheckEquals(2, LRot(Low(Integer), 2));
  CheckEquals(Low(Integer), LRot(Integer($8000), 16));
  CheckEquals($8000, LRot(Low(Integer), 16));
  CheckEquals(2, LRot(Integer(1), 33));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclLogicRotateTest._LRotWord;
begin
  CheckEquals(0, LRot(Word(0), 1));
  CheckEquals(2, LRot(Word(1), 1));
  CheckEquals(2, LRot(Word($8000), 2));
  CheckEquals($8000, LRot(Word($80), 8));
  CheckEquals($80, LRot(Word($8000), 8));
  CheckEquals(2, LRot(Word(1), 17));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclLogicRotateTest._RRotByte;
begin
  CheckEquals(0, RRot(Byte(0), 1));
  CheckEquals(1, RRot(Byte(2), 1));
  CheckEquals($80, RRot(Byte(2), 2));
  CheckEquals($0F, RRot(Byte($F0), 4));
  CheckEquals($F0, RRot(Byte($0F), 4));
  CheckEquals($80, RRot(Byte(2), 10));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclLogicRotateTest._RRotInteger;
begin
  CheckEquals(0, RRot(Integer(0), 1));
  CheckEquals(1, RRot(Integer(2), 1));
  CheckEquals(Low(Integer), RRot(Integer(2), 2));
  CheckEquals(Integer($8000), RRot(Low(Integer), 16));
  CheckEquals(Low(Integer), RRot(Integer($8000), 16));
  CheckEquals(1, RRot(Integer(2), 33));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclLogicRotateTest._RRotWord;
begin
  CheckEquals(0, RRot(Word(0), 1));
  CheckEquals(1, RRot(Word(2), 1));
  CheckEquals($8000, RRot(Word(2), 2));
  CheckEquals($80, RRot(Word($8000), 8));
  CheckEquals($8000, RRot(Word($80), 8));
  CheckEquals(1, RRot(Word(2), 17));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclLogicRotateTest._SarInteger;
const
  arr: array[0..10] of Integer = (Low(Integer), Low(Integer) + 1, -5, -2, -1, 0, 1, 2, 5, High(Integer) - 1, High(Integer));
var
  i, shift: Byte;
begin
  for i := Low(arr) to High(arr) do
	for shift := 0 to 11 do
	  CheckEquals(Integer(Int64(arr[i]) shr shift), Sar(arr[i], shift));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclLogicRotateTest._SarShortint;
const
  arr: array[0..9] of Shortint = (-128, -127, -5, -2, -1, 0, 1, 2, 5, 127);
var
  i, shift: Byte;
begin
  for i := Low(arr) to High(arr) do
	for shift := 0 to 11 do
	  CheckEquals(Shortint(Smallint(arr[i]) shr shift), Sar(arr[i], shift));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclLogicRotateTest._SarSmallint;
const
  arr: array[0..9] of Smallint = (-32768, -32767, -5, -2, -1, 0, 1, 2, 5, 32767);
var
  i, shift: Byte;
begin
  for i := Low(arr) to High(arr) do
	for shift := 0 to 11 do
	  CheckEquals(Smallint(Integer(arr[i]) shr shift), Sar(arr[i], shift));
end;

initialization
  RegisterTest('JCLLogic', TJclLogicClearBitSetBitTest.Suite);
  RegisterTest('JCLLogic', TJclLogicMiscTest.Suite);
  RegisterTest('JCLLogic', TJclLogicRotateTest.Suite);

end.
