{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSIMDUtils.pas, released on 2004-10-11.

The Initial Developer of the Original Code is Florent Ouchet [ouchet dott florent att laposte dott net]
Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.
All Rights Reserved.

Contributor(s): -

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JclSIMDUtils;

{$I jedi.inc}

interface

uses
  Windows;

resourcestring
  RsSSE     = 'SSE';
  RsMMX     = 'MMX';
  RsSSE1    = 'SSE1';
  RsSSE2    = 'SSE2';
  RsSSE3    = 'SSE3';
  Rs3DNow   = '3DNow!';
  RsExMMX   = 'Extensions to MMX';
  RsEx3DNow = 'Extensions to 3DNow!';
  RsLong    = '64-bit Core';

  RsTrademarks = 'MMX is a trademark of Intel Corporation.'+#13+#10+
                 '3DNow! is a registered trademark of Advanced Micro Devices.';

  RsNoSIMD    = 'No SIMD registers found';
  RsNoSSE     = 'SSE are not supported on this processor';
  RsNo128SIMD = 'No 128-bit-register SIMD';
  RsNo64SIMD  = 'No 64-bit-register SIMD';

  RsVectorIE  = 'IE  ';
  RsVectorDE  = 'DE  ';
  RsVectorZE  = 'ZE  ';
  RsVectorOE  = 'OE  ';
  RsVectorUE  = 'UE  ';
  RsVectorPE  = 'PE  ';
  RsVectorDAZ = 'DAZ ';   //  (Only in Intel P4, Intel Xeon and AMD)
  RsVectorIM  = 'IM  ';
  RsVectorDM  = 'DM  ';
  RsVectorZM  = 'ZM  ';
  RsVectorOM  = 'OM  ';
  RsVectorUM  = 'UM  ';
  RsVectorPM  = 'PM  ';
  RsVectorRC  = 'RC  ';
  RsVectorFZ  = 'FZ  ';

  RsVectorIEText  = 'Invalid-operation exception';
  RsVectorDEText  = 'Denormal-operand exception';
  RsVectorZEText  = 'Zero-divide exception';
  RsVectorOEText  = 'Overflow exception';
  RsVectorUEText  = 'Underflow exception';
  RsVectorPEText  = 'Precision exception';
  RsVectorDAZText = 'Denormal are zeros';      //  (Only in Intel P4, Intel Xeon and AMD)
  RsVectorIMText  = 'Invalid-operation mask';
  RsVectorDMText  = 'Denormal-operand mask';
  RsVectorZMText  = 'Zero-divide mask';
  RsVectorOMText  = 'Overflow mask';
  RsVectorUMText  = 'Underflow mask';
  RsVectorPMText  = 'Precision mask';
  RsVectorRCText  = 'Rounding control';
  RsVectorFZText  = 'Flush to zero';

  RsRoundToNearest  = 'Round to nearest';
  RsRoundDown       = 'Round down';
  RsRoundUp         = 'Round up';
  RsRoundTowardZero = 'Round toward zero';

type
  TJclMMContentType = (mt8Bytes, mt4Words, mt2DWords);

  TJclMMRegister = packed record
    case TJclMMContentType of
      mt8Bytes  : ( Bytes  : array [0..7] of Byte; );
      mt4Words  : ( Words  : array [0..3] of Word; );
      mt2DWords : ( DWords : array [0..1] of Cardinal; );
  end;

  TJclFPUContentType = (ftFloat, ftSimd);

  TJclFPUData = packed record
    case TJclFPUContentType of
      ftFloat : ( FloatValue : Extended; );
      ftSimd  : ( MMRegister : TJclMMRegister;
                  Reserved   : Word );
  end;

  TJclFPURegister = packed record
    Data     : TJclFPUData;
    Reserved : array [0..5] of Byte;
  end;

  TJclFPURegisters = array [0..7] of TJclFPURegister;

  TJclXMMContentType = (xt16Bytes, xt8Words, xt4DWords,
                        xt2QWords, xt4Singles, xt2Doubles);

  TJclXMMRegister = packed record
    case TJclXMMContentType of
      xt16Bytes  : ( Bytes   : array [0..15] of Byte;    );
      xt8Words   : ( Words   : array [0..7] of Word;     );
      xt4DWords  : ( DWords  : array [0..3] of Cardinal; );
      xt2QWords  : ( QWords  : array [0..1] of Int64;    );
      xt4Singles : ( Singles : array [0..3] of Single;   );
      xt2Doubles : ( Doubles : array [0..1] of Double;   );
  end;

  TJclProcessorSize = (ps32Bits, ps64Bits);

  TJclXMMRegisters = packed record
    case TJclProcessorSize of
      ps32Bits : ( LegacyXMM      : array [0..7] of TJclXMMRegister;
                   LegacyReserved : array [0..127] of Byte; );
      ps64Bits : ( LongXMM        : array [0..15] of TJclXMMRegister; );
  end;

  TJclMXCSRBits = ( mbInvalidOperationException,  // = 0
                    mbDenormalException,          // = 1
                    mbDivideByZeroException,      // = 2
                    mbOverflowException,          // = 3
                    mbUnderflowException,         // = 4
                    mbPrecisionException,         // = 5
                    mbDenormalsAreZeros,          // = 6 (Only in Intel P4, Intel Xeon and AMD)
                    mbInvalidOperationMask,       // = 7
                    mbDenormalMask,               // = 8
                    mbDivideByZeroMask,           // = 9
                    mbOverflowMask,               // = 10
                    mbUnderflowMask,              // = 11
                    mbPrecisionMask,              // = 12
                    mbRoundingControl1,           // = 13
                    mbRoundingControl2,           // = 14
                    mbFlushToZero,                // = 15
                    mbReserved1,                  // = 16
                    mbReserved2,                  // = 17
                    mbReserved3,                  // = 18
                    mbReserved4,                  // = 19
                    mbReserved5,                  // = 20
                    mbReserved6,                  // = 21
                    mbReserved7,                  // = 22
                    mbReserved8,                  // = 23
                    mbReserved9,                  // = 24
                    mbReserved10,                 // = 25
                    mbReserved11,                 // = 26
                    mbReserved12,                 // = 27
                    mbReserved13,                 // = 28
                    mbReserved14,                 // = 29
                    mbReserved15,                 // = 30
                    mbReserved16);                // = 31

  TJclRoundingControl = (rcRoundToNearest,   //=0
                         rcRoundDown,        //=1
                         rcRoundUp,          //=2
                         rcRoundTowardZero); //=3

  TJclMXCSR = set of TJclMXCSRBits;

  TJclVectorFrame = packed record
    FCW          : Word;                     // bytes from 0   to 1
    FSW          : Word;                     // bytes from 2   to 3
    FTW          : Byte;                     // byte 4
    Reserved1    : Byte;                     // byte 5
    FOP          : Word;                     // bytes from 6   to 7
    FpuIp        : Cardinal;                 // bytes from 8   to 11
    CS           : Word;                     // bytes from 12  to 13
    Reserved2    : Word;                     // bytes from 14  to 15
    FpuDp        : Cardinal;                 // bytes from 16  to 19
    DS           : Word;                     // bytes from 20  to 21
    Reserved3    : Word;                     // bytes from 22  to 23
    MXCSR        : TJclMXCSR;                // bytes from 24  to 27
    MXCSRMask    : TJclMXCSR;                // bytes from 28  to 31
    FPURegisters : TJclFPURegisters;         // bytes from 32  to 159
    XMMRegisters : TJclXMMRegisters;         // bytes from 160 to 415
    Reserved4    : array [416..511] of Byte; // bytes from 416 to 512
  end;

  TJclContext = packed record
     ScalarContext : Windows.TContext;
     VectorContext : TJclVectorFrame;
  end;

  PJclContext = ^TJclContext;

type
  TBitDescription = record
    BitCount: 0..2;
    ShortName: string;            // only if BitCount>0
    LongName: string;             // only if BitCount>0
    Names: array [0..3] of string // only if BitCount=2
  end;

const
  MXCSRBitsDescription : array [mbInvalidOperationException..mbFlushToZero] of TBitDescription =
    (
      (BitCount:1; ShortName:RsVectorIE;  LongName:RsVectorIEText),   //mbInvalidOperationException
      (BitCount:1; ShortName:RsVectorDE;  LongName:RsVectorDEText),   //mbDenormalException
      (BitCount:1; ShortName:RsVectorZE;  LongName:RsVectorZEText),   //mbDivideByZeroException
      (BitCount:1; ShortName:RsVectorOE;  LongName:RsVectorOEText),   //mbOverflowException
      (BitCount:1; ShortName:RsVectorUE;  LongName:RsVectorUEText),   //mbUnderflowException
      (BitCount:1; ShortName:RsVectorPE;  LongName:RsVectorPEText),   //mbPrecisionException
      (BitCount:1; ShortName:RsVectorDAZ; LongName:RsVectorDAZText),  //mbDenormalsAreZeros
      (BitCount:1; ShortName:RsVectorIM;  LongName:RsVectorIMText),   //mbInvalidOperationMask
      (BitCount:1; ShortName:RsVectorDM;  LongName:RsVectorDMText),   //mbDenormalMask
      (BitCount:1; ShortName:RsVectorZM;  LongName:RsVectorZMText),   //mbDivideByZeroMask
      (BitCount:1; ShortName:RsVectorOM;  LongName:RsVectorOMText),   //mbOverflowMask
      (BitCount:1; ShortName:RsVectorUM;  LongName:RsVectorUMText),   //mbUnderflowMask
      (BitCount:1; ShortName:RsVectorPM;  LongName:RsVectorPMText),   //mbPrecisionMask
      (BitCount:2; ShortName:RsVectorRC;  LongName:RsVectorRCText; Names:(RsRoundToNearest,
                                                                          RsRoundDown,
                                                                          RsRoundUp,
                                                                          RsRoundTowardZero)), //mbRoundingControl1
      (BitCount:0),                                                   //mbRoundingControl2
      (BitCount:1; ShortName:RsVectorFZ;  LongName:RsVectorFZText)    //mbFlushToZero
    );

type
  TJclSIMDValue = packed record
    case Display: TJclXMMContentType of
      xt16Bytes  : (ValueByte: Byte;      );
      xt8Words   : (ValueWord: Word;      );
      xt4DWords  : (ValueDWord: Cardinal; );
      xt2QWords  : (ValueQWord: Int64;    );
      xt4Singles : (ValueSingle: Single;  );
      xt2Doubles : (ValueDouble: Double;  );
  end;

  TJclSIMDFormat = ( sfBinary, sfSigned, sfUnsigned, sfHexa );

function FormatValue(Value:TJclSIMDValue; Format: TJclSIMDFormat):string;
function ParseValue(const StringValue: string; var Value:TJclSIMDValue; Format: TJclSIMDFormat):Boolean;
function ReplaceXMMRegisters(var Expression: string; Is64Bits: Boolean;
  var XMMRegisters: TJclXMMRegisters): Boolean;

const
  CONTEXT_EXTENDED_REGISTERS = CONTEXT_i386 or $00000020;

// return the processor frame for the specified thread, this thread must be suspended
function GetThreadContext(hThread: THandle;
  var lpContext: TJclContext): BOOL; stdcall;

// set the processor frame for the specified thread, this thread must be suspended
function SetThreadContext(hThread: THandle;
  const lpContext: TJclContext): BOOL; stdcall;

// return the XMM registers for the specified thread, this thread must be suspended
function GetVectorContext(hThread: THandle; out VectorContext:TJclVectorFrame): Boolean;
// return the XMM registers for the specified thread, this thread must be suspended
function SetVectorContext(hThread: THandle; const VectorContext:TJclVectorFrame): Boolean;

implementation

uses
  SysUtils, Math, Dialogs;

function FormatBinary(Value: TJclSIMDValue): string;
var
  I:Byte;
const
  Width: array [xt16Bytes..xt2QWords] of Byte = (8, 16, 32, 64);
begin
  Assert(Value.Display<xt4Singles);
  Result:=StringOfChar('0',Width[Value.Display]);
  for I:=1 to Width[Value.Display] do
  begin
    if (Value.ValueQWord and 1)<>0
      then Result[Width[Value.Display]-I+1]:='1';
    Value.ValueQWord:=Value.ValueQWord shr 1;
  end;
end;

function FormatSigned(Value: TJclSIMDValue): string;
const
  Width: array [xt16Bytes..xt2QWords] of Byte = (4, 6, 11, 20);
begin
  Assert(Value.Display<xt4Singles);
  case Value.Display of
    xt16Bytes : Result:=IntToStr(ShortInt(Value.ValueByte));
    xt8Words  : Result:=IntToStr(SmallInt(Value.ValueWord));
    xt4DWords : Result:=IntToStr(Integer(Value.ValueDWord));
    xt2QWords : Result:=IntToStr(Value.ValueQWord);
    else        begin
                  Result := '';
                  Exit;
                end;
  end;
  Result:=StringOfChar(' ',Width[Value.Display]-Length(Result))+Result;
end;

function FormatUnsigned(Value: TJclSIMDValue): string;
const
  Width: array [xt16Bytes..xt2QWords] of Byte = (3, 5, 10, 20);
begin
  Assert(Value.Display<xt4Singles);
  case Value.Display of
    xt16Bytes  : Result:=IntToStr(Byte(Value.ValueByte));
    xt8Words  : Result:=IntToStr(Word(Value.ValueWord));
    xt4DWords : Result:=IntToStr(Cardinal(Value.ValueDWord));
    xt2QWords : Result:=IntToStr(Value.ValueQWord);
    else        begin
                  Result := '';
                  Exit;
                end;
  end;
  Result:=StringOfChar(' ',Width[Value.Display]-Length(Result))+Result;
end;

function FormatHexa(Value: TJclSIMDValue): string;
const
  Width: array [xt16Bytes..xt2QWords] of Byte = (2, 4, 8, 16);
begin
  Assert(Value.Display<xt4Singles);
  case Value.Display of
    xt16Bytes : Result := IntToHex(Value.ValueByte,Width[xt16Bytes]);
    xt8Words  : Result := IntToHex(Value.ValueWord,Width[xt8Words]);
    xt4DWords : Result := IntToHex(Value.ValueDWord,Width[xt4DWords]);
    xt2QWords : Result := IntToHex(Value.ValueQWord,Width[xt2QWords]);
    else        Result := '';
  end;
end;

function FormatFloat(Value:TJclSIMDValue): string;
begin
  Assert(Value.Display>=xt4Singles);
  case Value.Display of
    xt4Singles : Result := FloatToStr(Value.ValueSingle);
    xt2Doubles : Result := FloatToStr(Value.ValueDouble);
    else         Result := '';
  end;
  Result:=StringOfChar(' ',22-Length(Result))+Result;     // 22 = max string length of a double value
end;

function FormatValue(Value:TJclSIMDValue; Format: TJclSIMDFormat): string;
type
  TFormatFunction = function (Value:TJclSIMDValue): string;
var
  FormatFunction: TFormatFunction;
begin
  Result := '';
  case Format of
    sfBinary   : FormatFunction := FormatBinary;
    sfSigned   : FormatFunction := FormatSigned;
    sfUnsigned : FormatFunction := FormatUnsigned;
    sfHexa     : FormatFunction := FormatHexa;
    else         Exit;
  end;
  case Value.Display of
    xt16Bytes..xt2QWords   : Result := FormatFunction(Value);
    xt4Singles..xt2Doubles : Result := FormatFloat(Value);
  end;
end;

function ParseBinary(StringValue: string; var Value:TJclSIMDValue): Boolean;
var
  TestValue: Int64;
  Index: Integer;
begin
  TestValue := 0;
  Result := False;
  if Length(StringValue)>64 then
    Exit;
  for Index := 1 to Length(StringValue) do
  begin
    TestValue := TestValue shl 1;
    case StringValue[Index] of
      '0' : ;
      '1' : Inc(TestValue);
      else  Exit;
    end;
  end;
  Result := True;
  case Value.Display of
    xt16Bytes : if (TestValue>=Byte($00)) and (TestValue<=Byte($FF))
                  then Value.ValueByte := TestValue
                  else Result := False;
    xt8Words  : if (TestValue>=Word($0000)) and (TestValue<=Word($FFFF))
                  then Value.ValueWord := TestValue
                  else Result := False;
    xt4DWords : if (TestValue>=Cardinal($00000000)) and (TestValue<=Cardinal($FFFFFFFF))
                  then Value.ValueDWord := TestValue
                  else Result := False;
    xt2QWords : Value.ValueQWord := TestValue;
    else        Result := False;
  end;
end;

function ParseSigned(StringValue: string; var Value:TJclSIMDValue): Boolean;
var
  TestValue: Int64;
  ErrorCode: Integer;
begin
  Val(StringValue,TestValue,ErrorCode);
  Result := ErrorCode=0;
  if Result then
    case Value.Display of
      xt16Bytes : if (TestValue>=ShortInt($80)) and (TestValue<=ShortInt($7F))
                    then Value.ValueByte := TestValue
                    else Result := False;
      xt8Words  : if (TestValue>=SmallInt($8000)) and (TestValue<=SmallInt($7FFF))
                    then Value.ValueWord := TestValue
                    else Result := False;
      xt4DWords : if (TestValue>=Integer($80000000)) and (TestValue<=Integer($7FFFFFFF))
                    then Value.ValueDWord := TestValue
                    else Result := False;
      xt2QWords : Value.ValueQWord := TestValue;
      else        Result := False;
    end;
end;

function ParseUnsigned(StringValue: string; var Value:TJclSIMDValue): Boolean;
var
  TestValue: Int64;
  ErrorCode: Integer;
begin
  Val(StringValue,TestValue,ErrorCode);
  Result := ErrorCode=0;
  if Result then
    case Value.Display of
      xt16Bytes : if (TestValue>=Byte($00)) and (TestValue<=Byte($FF))
                    then Value.ValueByte := TestValue
                    else Result := False;
      xt8Words  : if (TestValue>=Word($0000)) and (TestValue<=Word($FFFF))
                    then Value.ValueWord := TestValue
                    else Result := False;
      xt4DWords : if (TestValue>=Cardinal($00000000)) and (TestValue<=Cardinal($FFFFFFFF))
                    then Value.ValueDWord := TestValue
                    else Result := False;
      xt2QWords : Value.ValueQWord := TestValue;
      else        Result := False;
    end;
end;

function ParseHexa(StringValue: string; var Value:TJclSIMDValue): Boolean;
var
  TestValue: Int64;
  Index: Integer;
begin
  TestValue := 0;
  Result := False;
  if Length(StringValue)>16 then
    Exit;
  for Index := 1 to Length(StringValue) do
  begin
    TestValue := TestValue shl 4;
    case StringValue[Index] of
      '0'      : ;
      '1'..'9' : Inc(TestValue,Ord(StringValue[Index])-Ord('0'));
      'A'..'F' : Inc(TestValue,Ord(StringValue[Index])-Ord('A')+10);
      'a'..'f' : Inc(TestValue,Ord(StringValue[Index])-Ord('a')+10);
      else  Exit;
    end;
  end;
  Result := True;
  case Value.Display of
    xt16Bytes : if (TestValue>=Byte($00)) and (TestValue<=Byte($FF))
                  then Value.ValueByte := TestValue
                  else Result := False;
    xt8Words  : if (TestValue>=Word($0000)) and (TestValue<=Word($FFFF))
                  then Value.ValueWord := TestValue
                  else Result := False;
    xt4DWords : if (TestValue>=Cardinal($00000000)) and (TestValue<=Cardinal($FFFFFFFF))
                  then Value.ValueDWord := TestValue
                  else Result := False;
    xt2QWords : Value.ValueQWord := TestValue;
    else        Result := False;
  end;
end;

function ParseFloat(StringValue: string; var Value:TJclSIMDValue): Boolean;
var
  TestValue: Extended;
  ErrorCode: Integer;
begin
  if (DecimalSeparator<>'.') then
    StringValue := StringReplace(StringValue,DecimalSeparator,'.',
      [rfReplaceAll,rfIgnoreCase]);
  Val(StringValue,TestValue,ErrorCode);
  Result := ErrorCode=0;
  if (Result) then
    case Value.Display of
      xt4Singles : if (TestValue>=-MaxSingle) and (TestValue<=MaxSingle)
                     then Value.ValueSingle := TestValue
                     else Result := False;
      xt2Doubles : if (TestValue>=MaxDouble) and (TestValue<=MaxDouble)
                     then Value.ValueDouble := TestValue
                     else Result := False;
      else         Result := False;
    end;
end;

function ParseValue(const StringValue: string; var Value:TJclSIMDValue;
  Format: TJclSIMDFormat):Boolean;
type
  TParseFunction = function (StringValue: string; var Value:TJclSIMDValue): Boolean;
var
  ParseFunction: TParseFunction;
begin
  Result := False;
  case Format of
    sfBinary   : ParseFunction := ParseBinary;
    sfSigned   : ParseFunction := ParseSigned;
    sfUnsigned : ParseFunction := ParseUnsigned;
    sfHexa     : ParseFunction := ParseHexa;
    else         Exit;
  end;
  case Value.Display of
    xt16Bytes..xt2QWords   : Result := ParseFunction(StringValue, Value);
    xt4Singles..xt2Doubles : Result := ParseFloat(StringValue, Value);
  end;
end;

function ReplaceXMMRegisters(var Expression: string; Is64Bits: Boolean;
  var XMMRegisters: TJclXMMRegisters): Boolean;
var
  LocalString: string;
  XMMPosition: Integer;
  DataPosition: Integer;
  DataType: string;
  Index: Integer;
  RegisterIndex: Integer;
  DataIndex: Integer;
  ErrorCode: Integer;
  NumberOfRegister: Integer;
  AValue: TJclSIMDValue;
  ValueStr: string;
  OldLength: Integer;
begin
  if (Is64Bits) then
    NumberOfRegister := 16
  else NumberOfRegister := 8;
  Result := False;
  LocalString := AnsiUpperCase(Expression);
  XMMPosition := AnsiPos('XMM',LocalString);
  while (XMMPosition > 0) do
  begin
    for Index := XMMPosition to Length(LocalString) do
      if (LocalString[Index] = '.') then
        Break;
    if (Index >= Length(LocalString)) then
      Exit;
    Val(Copy(LocalString,XMMPosition+3,Index-XMMPosition-3),RegisterIndex,ErrorCode);
    if   (ErrorCode<>0) or (RegisterIndex<0)
      or (RegisterIndex>=NumberOfRegister) then
      Exit;

    DataPosition := Index + 1;
    if (DataPosition > Length(LocalString)) then
      Exit;
    for Index := DataPosition to Length(LocalString) do
      if (LocalString[Index] in ['0'..'9']) then
        Break;
    if (Index > Length(LocalString)) then
      Exit;
    DataType := Copy(LocalString,DataPosition,Index-DataPosition);

    DataPosition := Index;
    for Index := DataPosition to Length(LocalString) do
      if not (LocalString[Index] in ['0'..'9']) then
        Break;
    Val(Copy(LocalString,DataPosition,Index-DataPosition),DataIndex,ErrorCode);
    if (ErrorCode<>0) or (DataIndex<0) then
      Exit;

    if (CompareStr(DataType,'BYTE') = 0) then
    begin
      if (DataIndex >= 16) then
        Exit;
      AValue.Display := xt16Bytes;
      AValue.ValueByte := XMMRegisters.LongXMM[RegisterIndex].Bytes[DataIndex];
    end else if (CompareStr(DataType,'WORD') = 0) then
    begin
      if (DataIndex >= 8) then
        Exit;
      AValue.Display := xt8Words;
      AValue.ValueWord := XMMRegisters.LongXMM[RegisterIndex].Words[DataIndex];
    end else if (CompareStr(DataType,'DWORD') = 0) then
    begin
      if (DataIndex >= 4) then
        Exit;
      AValue.Display := xt4DWords;
      AValue.ValueDWord := XMMRegisters.LongXMM[RegisterIndex].DWords[DataIndex];
    end else if (CompareStr(DataType,'QWORD') = 0) then
    begin
      if (DataIndex >= 2) then
        Exit;
      AValue.Display := xt2QWords;
      AValue.ValueQWord := XMMRegisters.LongXMM[RegisterIndex].QWords[DataIndex];
    end else if (CompareStr(DataType,'SINGLE') = 0) then
    begin
      if (DataIndex >= 4) then
        Exit;
      AValue.Display := xt4Singles;
      AValue.ValueSingle := XMMRegisters.LongXMM[RegisterIndex].Singles[DataIndex];
    end else if (CompareStr(DataType,'DOUBLE') = 0) then
    begin
      if (DataIndex >= 2) then
        Exit;
      AValue.Display := xt2Doubles;
      AValue.ValueDouble := XMMRegisters.LongXMM[RegisterIndex].Doubles[DataIndex];
    end else Exit;
    ValueStr := Trim(FormatValue(AValue,sfSigned));
    if (DecimalSeparator <> '.') then
      ValueStr := StringReplace(ValueStr,DecimalSeparator,'.',[rfReplaceAll, rfIgnoreCase]);
    if (Length(ValueStr) >= (Index-XMMPosition)) then
    begin
      OldLength := Length(Expression);
      SetLength(Expression,Length(Expression)+Length(ValueStr)-(Index-XMMPosition));
      if (Length(ValueStr)>(Index-XMMPosition)) then
        Move(Expression[Index],Expression[XMMPosition+Length(ValueStr)],OldLength-Index+1);
      Move(ValueStr[1],Expression[XMMPosition],Length(ValueStr));
    end else
    begin
      Move(ValueStr[1],Expression[XMMPosition],Length(ValueStr));
      Move(Expression[Index],Expression[XMMPosition+Length(ValueStr)],Length(Expression)-Index+1);
      SetLength(Expression,Length(Expression)+Length(ValueStr)-(Index-XMMPosition));
    end;
    LocalString := AnsiUpperCase(Expression);
    XMMPosition := AnsiPos('XMM',LocalString);
  end;
  Result := True;
end;

function GetThreadContext(hThread: THandle;
  var lpContext: TJclContext): BOOL;
  stdcall; external kernel32 name 'GetThreadContext';

function SetThreadContext(hThread: THandle;
  const lpContext: TJclContext): BOOL;
  stdcall; external kernel32 name 'SetThreadContext';

function GetVectorContext(hThread: THandle; out VectorContext:TJclVectorFrame): Boolean;
var
  ContextMemory: Pointer;
  JvContext: PJclContext;
begin
  GetMem(ContextMemory,sizeof(TJclContext)+15);
  if ((Cardinal(ContextMemory) and 15)<>0) then
    JvContext := PJclContext((Cardinal(ContextMemory)+16) and $FFFFFFF0)
  else JvContext := ContextMemory;
  JvContext^.ScalarContext.ContextFlags := CONTEXT_EXTENDED_REGISTERS;
  Result :=    GetThreadContext(hThread,JvContext^)
           and ((JvContext^.ScalarContext.ContextFlags and CONTEXT_EXTENDED_REGISTERS)<>0);
  if (Result) then
    VectorContext := JvContext^.VectorContext
  else FillChar(VectorContext,sizeof(VectorContext),0);
  FreeMem(ContextMemory);
end;

function SetVectorContext(hThread: THandle; const VectorContext:TJclVectorFrame): Boolean;
var
  ContextMemory: Pointer;
  JvContext:PJclContext;
begin
  GetMem(ContextMemory,sizeof(TJclContext)+15);
  if ((Cardinal(ContextMemory) and 15)<>0) then
    JvContext := PJclContext((Cardinal(ContextMemory)+16) and $FFFFFFF0)
  else JvContext := ContextMemory;
  JvContext^.ScalarContext.ContextFlags := CONTEXT_EXTENDED_REGISTERS;
  Result :=    GetThreadContext(hThread,JvContext^)
           and ((JvContext^.ScalarContext.ContextFlags and CONTEXT_EXTENDED_REGISTERS)<>0);
  if (Result) then
  begin
    JvContext^.ScalarContext.ContextFlags := CONTEXT_EXTENDED_REGISTERS;
    JvContext^.VectorContext := VectorContext;
    Result := SetThreadContext(hThread,JvContext^);
  end;
  FreeMem(ContextMemory);
end;

end.
