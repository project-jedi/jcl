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
{ The Original Code is: JvSIMDUtils.pas, released on 2004-10-11.                                   }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet                                     }
{ [ouchet dott florent att laposte dott net]                                                       }
{ Portions created by Florent Ouchet are Copyright (C) 2004 Florent Ouchet.                        }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{ You may retrieve the latest version of this file at the Project JEDI's JCL home page,            }
{ located at http://jcl.sourceforge.net                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclSIMDUtils;

{$I jcl.inc}

interface

uses
  Windows,
  ToolsAPI,
  JclSysInfo,
  JclOtaResources;

type
  TJclMMContentType = (mt8Bytes, mt4Words, mt2DWords, mt1QWord, mt2Singles);

  TJclMMRegister = packed record
    case TJclMMContentType of
      mt8Bytes:
        (Bytes: array [0..7] of Byte;);
      mt4Words:
        (Words: array [0..3] of Word;);
      mt2DWords:
        (DWords: array [0..1] of Cardinal;);
      mt1QWord:
        (QWords: Int64;);
      mt2Singles:
        (Singles: array [0..1] of Single;);
  end;

  TJclFPUContentType = (ftExtended, ftMM);

  TJclFPUData = packed record
    case TJclFPUContentType of
      ftExtended:
        (FloatValue: Extended;);
      ftMM:
        (MMRegister: TJclMMRegister;
         Reserved: Word;);
  end;

  TJclFPURegister = packed record
    Data: TJclFPUData;
    Reserved: array [0..5] of Byte;
  end;

  TJclFPURegisters = array [0..7] of TJclFPURegister;

  TJclXMMContentType = (xt16Bytes, xt8Words, xt4DWords, xt2QWords, xt4Singles, xt2Doubles);

  TJclXMMRegister = packed record
    case TJclXMMContentType of
      xt16Bytes:
        (Bytes: array [0..15] of Byte;);
      xt8Words:
        (Words: array [0..7] of Word;);
      xt4DWords:
        (DWords: array [0..3] of Cardinal;);
      xt2QWords:
        (QWords: array [0..1] of Int64;);
      xt4Singles:
        (Singles: array [0..3] of Single;);
      xt2Doubles:
        (Doubles: array [0..1] of Double;);
  end;

  TJclProcessorSize = (ps32Bits, ps64Bits);

  TJclXMMRegisters = packed record
    case TJclProcessorSize of
      ps32Bits:
        (LegacyXMM: array [0..7] of TJclXMMRegister;
         LegacyReserved: array [0..127] of Byte;);
      ps64Bits:
        (LongXMM: array [0..15] of TJclXMMRegister;);
  end;

  //TJclRoundingControl = (rcRoundToNearest,   //=0
  //                       rcRoundDown,        //=1
  //                       rcRoundUp,          //=2
  //                       rcRoundTowardZero); //=3

  TJclVectorFrame = packed record
    FCW: Word;                           // bytes from 0   to 1
    FSW: Word;                           // bytes from 2   to 3
    FTW: Byte;                           // byte 4
    Reserved1: Byte;                     // byte 5
    FOP: Word;                           // bytes from 6   to 7
    FpuIp: Cardinal;                     // bytes from 8   to 11
    CS: Word;                            // bytes from 12  to 13
    Reserved2: Word;                     // bytes from 14  to 15
    FpuDp: Cardinal;                     // bytes from 16  to 19
    DS: Word;                            // bytes from 20  to 21
    Reserved3: Word;                     // bytes from 22  to 23
    MXCSR: Cardinal;                     // bytes from 24  to 27
    MXCSRMask: Cardinal;                 // bytes from 28  to 31
    FPURegisters: TJclFPURegisters;      // bytes from 32  to 159
    XMMRegisters: TJclXMMRegisters;      // bytes from 160 to 415
    Reserved4: array [416..511] of Byte; // bytes from 416 to 512
  end;

  TJclContext = packed record
    ScalarContext: Windows.TContext;
    VectorContext: TJclVectorFrame;
  end;

  PJclContext = ^TJclContext;

  TBitDescription = record
    AndMask: Cardinal;
    Shifting: Cardinal;
    ShortName: string;
    LongName: string;
  end;

  TMXCSRRange = 0..14;

const
  MXCSRBitsDescriptions: array [TMXCSRRange] of TBitDescription =
   (
    (AndMask: MXCSR_IE;  Shifting: 0;  ShortName: RsVectorIE;  LongName: RsVectorIEText),
    (AndMask: MXCSR_DE;  Shifting: 1;  ShortName: RsVectorDE;  LongName: RsVectorDEText),
    (AndMask: MXCSR_ZE;  Shifting: 2;  ShortName: RsVectorZE;  LongName: RsVectorZEText),
    (AndMask: MXCSR_OE;  Shifting: 3;  ShortName: RsVectorOE;  LongName: RsVectorOEText),
    (AndMask: MXCSR_UE;  Shifting: 4;  ShortName: RsVectorUE;  LongName: RsVectorUEText),
    (AndMask: MXCSR_PE;  Shifting: 5;  ShortName: RsVectorPE;  LongName: RsVectorPEText),
    (AndMask: MXCSR_DAZ; Shifting: 6;  ShortName: RsVectorDAZ; LongName: RsVectorDAZText),
    (AndMask: MXCSR_IM;  Shifting: 7;  ShortName: RsVectorIM;  LongName: RsVectorIMText),
    (AndMask: MXCSR_DM;  Shifting: 8;  ShortName: RsVectorDM;  LongName: RsVectorDMText),
    (AndMask: MXCSR_ZM;  Shifting: 9;  ShortName: RsVectorZM;  LongName: RsVectorZMText),
    (AndMask: MXCSR_OM;  Shifting: 10; ShortName: RsVectorOM;  LongName: RsVectorOMText),
    (AndMask: MXCSR_UM;  Shifting: 11; ShortName: RsVectorUM;  LongName: RsVectorUMText),
    (AndMask: MXCSR_PM;  Shifting: 12; ShortName: RsVectorPM;  LongName: RsVectorPMText),
    (AndMask: MXCSR_RC;  Shifting: 13; ShortName: RsVectorRC;  LongName: RsVectorRCText),
    (AndMask: MXCSR_FZ;  Shifting: 15; ShortName: RsVectorFZ;  LongName: RsVectorFZText)
   );

type
  TJclSIMDValue = packed record
    case Display: TJclXMMContentType of
      xt16Bytes:
        (ValueByte: Byte;);
      xt8Words:
        (ValueWord: Word;);
      xt4DWords:
        (ValueDWord: Cardinal;);
      xt2QWords:
        (ValueQWord: Int64;);
      xt4Singles:
        (ValueSingle: Single;);
      xt2Doubles:
        (ValueDouble: Double;);
  end;

  TJclSIMDFormat = (sfBinary, sfSigned, sfUnsigned, sfHexa);

function FormatValue(Value: TJclSIMDValue; Format: TJclSIMDFormat): string;
function ParseValue(const StringValue: string; var Value: TJclSIMDValue;
  Format: TJclSIMDFormat): Boolean;
function ReplaceSIMDRegisters(var Expression: string; Is64Bits: Boolean;
  var VectorFrame: TJclVectorFrame): Boolean;

const
  CONTEXT_EXTENDED_REGISTERS = CONTEXT_i386 or $00000020;

// return the processor frame for the specified thread, this thread must be suspended
function GetThreadContext(hThread: THandle; var lpContext: TJclContext): BOOL; stdcall;

// set the processor frame for the specified thread, this thread must be suspended
function SetThreadContext(hThread: THandle; const lpContext: TJclContext): BOOL; stdcall;

// return the XMM registers for the specified thread, this thread must be suspended
function GetVectorContext(AThread: IOTAThread; out VectorContext: TJclVectorFrame): Boolean;
// return the XMM registers for the specified thread, this thread must be suspended
function SetVectorContext(AThread: IOTAThread; const VectorContext: TJclVectorFrame): Boolean;

implementation

uses
  SysUtils, Math,
  JclStrings,
  JclOtaUtils;

function FormatBinary(Value: TJclSIMDValue): string;
var
  I: Byte;
const
  Width: array [xt16Bytes..xt2QWords] of Byte = (8, 16, 32, 64);
begin
  if not (Value.Display in [xt16Bytes, xt8Words, xt4DWords, XT2QWords]) then
    raise EJclExpertException.CreateTrace(RsEBadRegisterDisplay);

  Assert(Value.Display < xt4Singles);
  Result := StringOfChar('0', Width[Value.Display]);
  for I := 1 to Width[Value.Display] do
  begin
    if (Value.ValueQWord and 1) <> 0 then
      Result[Width[Value.Display] - I + 1] := '1';
    Value.ValueQWord := Value.ValueQWord shr 1;
  end;
end;

function FormatSigned(Value: TJclSIMDValue): string;
const
  Width: array [xt16Bytes..xt2QWords] of Byte = (4, 6, 11, 20);
begin
  if not (Value.Display in [xt16Bytes, xt8Words, xt4DWords, XT2QWords]) then
    raise EJclExpertException.CreateTrace(RsEBadRegisterDisplay);
    
  case Value.Display of
    xt16Bytes:
      Result := IntToStr(Shortint(Value.ValueByte));
    xt8Words:
      Result := IntToStr(Smallint(Value.ValueWord));
    xt4DWords:
      Result := IntToStr(Integer(Value.ValueDWord));
    xt2QWords:
      Result := IntToStr(Value.ValueQWord);
  else
    Result := '';
    Exit;
  end;
  Result := StringOfChar(' ', Width[Value.Display] - Length(Result)) + Result;
end;

function FormatUnsigned(Value: TJclSIMDValue): string;
const
  Width: array [xt16Bytes..xt2QWords] of Byte = (3, 5, 10, 20);
begin
  if not (Value.Display in [xt16Bytes, xt8Words, xt4DWords, XT2QWords]) then
    raise EJclExpertException.CreateTrace(RsEBadRegisterDisplay);
    
  case Value.Display of
    xt16Bytes:
      Result := IntToStr(Byte(Value.ValueByte));
    xt8Words:
      Result := IntToStr(Word(Value.ValueWord));
    xt4DWords:
      Result := IntToStr(Cardinal(Value.ValueDWord));
    xt2QWords:
      Result := IntToStr(Value.ValueQWord);
  else
    Result := '';
    Exit;
  end;
  Result := StringOfChar(' ', Width[Value.Display] - Length(Result)) + Result;
end;

function FormatHexa(Value: TJclSIMDValue): string;
const
  Width: array [xt16Bytes..xt2QWords] of Byte = (2, 4, 8, 16);
begin
  if not (Value.Display in [xt16Bytes, xt8Words, xt4DWords, XT2QWords]) then
    raise EJclExpertException.CreateTrace(RsEBadRegisterDisplay);
    
  case Value.Display of
    xt16Bytes:
      Result := IntToHex(Value.ValueByte, Width[xt16Bytes]);
    xt8Words:
      Result := IntToHex(Value.ValueWord, Width[xt8Words]);
    xt4DWords:
      Result := IntToHex(Value.ValueDWord, Width[xt4DWords]);
    xt2QWords:
      Result := IntToHex(Value.ValueQWord, Width[xt2QWords]);
  else
    Result := '';
  end;
end;

function FormatFloat(Value: TJclSIMDValue): string;
begin
  if not (Value.Display in [xt4Singles, xt2Doubles]) then
    raise EJclExpertException.CreateTrace(RsEBadRegisterDisplay);
    
  case Value.Display of
    xt4Singles:
      Result := FloatToStr(Value.ValueSingle);
    xt2Doubles:
      Result := FloatToStr(Value.ValueDouble);
  else
    Result := '';
  end;
  Result := StringOfChar(' ', 22 - Length(Result)) + Result; // 22 = max string length of a double value
end;

function FormatValue(Value: TJclSIMDValue; Format: TJclSIMDFormat): string;
type
  TFormatFunction = function(Value: TJclSIMDValue): string;
var
  FormatFunction: TFormatFunction;
begin
  Result := '';
  case Format of
    sfBinary:
      FormatFunction := FormatBinary;
    sfSigned:
      FormatFunction := FormatSigned;
    sfUnsigned:
      FormatFunction := FormatUnsigned;
    sfHexa:
      FormatFunction := FormatHexa;
  else
    Exit;
  end;
  case Value.Display of
    xt16Bytes..xt2QWords:
      Result := FormatFunction(Value);
    xt4Singles..xt2Doubles:
      Result := FormatFloat(Value);
  end;
end;

function ParseBinary(StringValue: string; var Value: TJclSIMDValue): Boolean;
var
  TestValue: Int64;
  Index: Integer;
begin
  TestValue := 0;
  Result := False;
  if Length(StringValue) > 64 then
    Exit;
  for Index := 1 to Length(StringValue) do
  begin
    TestValue := TestValue shl 1;
    case StringValue[Index] of
      '0':
        ;
      '1':
        Inc(TestValue);
    else
      Exit;
    end;
  end;
  Result := True;
  case Value.Display of
    xt16Bytes:
      if (TestValue >= Byte($00)) and (TestValue <= Byte($FF)) then
        Value.ValueByte := TestValue
      else
        Result := False;
    xt8Words:
      if (TestValue >= Word($0000)) and (TestValue <= Word($FFFF)) then
        Value.ValueWord := TestValue
      else
        Result := False;
    xt4DWords:
      if (TestValue >= Cardinal($00000000)) and (TestValue <= Cardinal($FFFFFFFF)) then
        Value.ValueDWord := TestValue
      else
        Result := False;
    xt2QWords:
      Value.ValueQWord := TestValue;
  else
    Result := False;
  end;
end;

function ParseSigned(StringValue: string; var Value: TJclSIMDValue): Boolean;
var
  TestValue: Int64;
  ErrorCode: Integer;
begin
  Val(StringValue, TestValue, ErrorCode);
  Result := ErrorCode = 0;
  if Result then
    case Value.Display of
      xt16Bytes:
        if (TestValue >= Shortint($80)) and (TestValue <= Shortint($7F)) then
          Value.ValueByte := TestValue
        else
          Result := False;
      xt8Words:
        if (TestValue >= Smallint($8000)) and (TestValue <= Smallint($7FFF)) then
          Value.ValueWord := TestValue
        else
          Result := False;
      xt4DWords:
        if (TestValue >= Integer($80000000)) and (TestValue <= Integer($7FFFFFFF)) then
          Value.ValueDWord := TestValue
        else
          Result := False;
      xt2QWords:
        Value.ValueQWord := TestValue;
    else
      Result := False;
    end;
end;

function ParseUnsigned(StringValue: string; var Value: TJclSIMDValue): Boolean;
var
  TestValue: Int64;
  ErrorCode: Integer;
begin
  Val(StringValue, TestValue, ErrorCode);
  Result := ErrorCode = 0;
  if Result then
    case Value.Display of
      xt16Bytes:
        if (TestValue >= Byte($00)) and (TestValue <= Byte($FF)) then
          Value.ValueByte := TestValue
        else
          Result := False;
      xt8Words:
        if (TestValue >= Word($0000)) and (TestValue <= Word($FFFF)) then
          Value.ValueWord := TestValue
        else
          Result := False;
      xt4DWords:
        if (TestValue >= Cardinal($00000000)) and (TestValue <= Cardinal($FFFFFFFF)) then
          Value.ValueDWord := TestValue
        else
          Result := False;
      xt2QWords:
        Value.ValueQWord := TestValue;
    else
      Result := False;
    end;
end;

function ParseHexa(StringValue: string; var Value: TJclSIMDValue): Boolean;
var
  TestValue: Int64;
  Index: Integer;
begin
  TestValue := 0;
  Result := False;
  if Length(StringValue) > 16 then
    Exit;
  for Index := 1 to Length(StringValue) do
  begin
    TestValue := TestValue shl 4;
    case StringValue[Index] of
      '0':
        ;
      '1'..'9':
        Inc(TestValue, Ord(StringValue[Index]) - Ord('0'));
      'A'..'F':
        Inc(TestValue, Ord(StringValue[Index]) - Ord('A') + 10);
      'a'..'f':
        Inc(TestValue, Ord(StringValue[Index]) - Ord('a') + 10);
    else
      Exit;
    end;
  end;
  Result := True;
  case Value.Display of
    xt16Bytes:
      if (TestValue >= Byte($00)) and (TestValue <= Byte($FF)) then
        Value.ValueByte := TestValue
      else
        Result := False;
    xt8Words:
      if (TestValue >= Word($0000)) and (TestValue <= Word($FFFF)) then
        Value.ValueWord := TestValue
      else
        Result := False;
    xt4DWords:
      if (TestValue >= Cardinal($00000000)) and (TestValue <= Cardinal($FFFFFFFF)) then
        Value.ValueDWord := TestValue
      else
        Result := False;
    xt2QWords:
      Value.ValueQWord := TestValue;
  else
    Result := False;
  end;
end;

function ParseFloat(StringValue: string; var Value: TJclSIMDValue): Boolean;
var
  TestValue: Extended;
  ErrorCode: Integer;
begin
  if DecimalSeparator <> '.' then
    StringValue := StringReplace(StringValue, DecimalSeparator, '.', [rfReplaceAll, rfIgnoreCase]);
  Val(StringValue, TestValue, ErrorCode);
  Result := ErrorCode = 0;
  if Result then
    case Value.Display of
      xt4Singles:
        if (TestValue >= -MaxSingle) and (TestValue <= MaxSingle) then
          Value.ValueSingle := TestValue
        else
          Result := False;
      xt2Doubles:
        if (TestValue >= MaxDouble) and (TestValue <= MaxDouble) then
          Value.ValueDouble := TestValue
        else
          Result := False;
    else
      Result := False;
    end;
end;

function ParseValue(const StringValue: string; var Value: TJclSIMDValue;
  Format: TJclSIMDFormat): Boolean;
type
  TParseFunction = function(StringValue: string; var Value: TJclSIMDValue): Boolean;
var
  ParseFunction: TParseFunction;
begin
  Result := False;
  case Format of
    sfBinary:
      ParseFunction := ParseBinary;
    sfSigned:
      ParseFunction := ParseSigned;
    sfUnsigned:
      ParseFunction := ParseUnsigned;
    sfHexa:
      ParseFunction := ParseHexa;
  else
    Exit;
  end;
  case Value.Display of
    xt16Bytes..xt2QWords:
      Result := ParseFunction(StringValue, Value);
    xt4Singles..xt2Doubles:
      Result := ParseFloat(StringValue, Value);
  end;
end;

function ReplaceSIMDRegisters(var Expression: string; Is64Bits: Boolean;
  var VectorFrame: TJclVectorFrame): Boolean;
var
  LocalString: string;
  RegisterPosition: Integer;
  DataPosition: Integer;
  DataType: string;
  Index: Integer;
  RegisterIndex: Integer;
  DataIndex: Integer;
  ErrorCode: Integer;
  NumberOfXMMRegister: Integer;
  AValue: TJclSIMDValue;
  ValueStr: string;
  OldLength: Integer;
begin
  if Is64Bits then
    NumberOfXMMRegister := 16
  else
    NumberOfXMMRegister := 8;
  Result := False;
  LocalString := AnsiUpperCase(Expression);

  RegisterPosition := AnsiPos('XMM', LocalString);
  while (RegisterPosition > 0) do
  begin
    for Index := RegisterPosition to Length(LocalString) do
      if LocalString[Index] = '.' then
        Break;
    if Index >= Length(LocalString) then
      Exit;
    Val(Copy(LocalString, RegisterPosition + 3, Index - RegisterPosition - 3), RegisterIndex, ErrorCode);
    if (ErrorCode <> 0) or (RegisterIndex < 0) or (RegisterIndex >= NumberOfXMMRegister) then
      Exit;

    DataPosition := Index + 1;
    if DataPosition > Length(LocalString) then
      Exit;
    for Index := DataPosition to Length(LocalString) do
      if CharIsDigit(LocalString[Index]) then
        Break;
    if Index > Length(LocalString) then
      Exit;
    DataType := Copy(LocalString, DataPosition, Index - DataPosition);

    DataPosition := Index;
    for Index := DataPosition to Length(LocalString) do
      if not CharIsDigit(LocalString[Index]) then
        Break;
    Val(Copy(LocalString, DataPosition, Index - DataPosition), DataIndex, ErrorCode);
    if (ErrorCode <> 0) or (DataIndex < 0) then
      Exit;

    if CompareStr(DataType, 'BYTE') = 0 then
    begin
      if DataIndex >= 16 then
        Exit;
      AValue.Display := xt16Bytes;
      AValue.ValueByte := VectorFrame.XMMRegisters.LongXMM[RegisterIndex].Bytes[DataIndex];
    end
    else
    if CompareStr(DataType, 'WORD') = 0 then
    begin
      if DataIndex >= 8 then
        Exit;
      AValue.Display := xt8Words;
      AValue.ValueWord := VectorFrame.XMMRegisters.LongXMM[RegisterIndex].Words[DataIndex];
    end
    else
    if CompareStr(DataType, 'DWORD') = 0 then
    begin
      if DataIndex >= 4 then
        Exit;
      AValue.Display := xt4DWords;
      AValue.ValueDWord := VectorFrame.XMMRegisters.LongXMM[RegisterIndex].DWords[DataIndex];
    end
    else
    if CompareStr(DataType, 'QWORD') = 0 then
    begin
      if DataIndex >= 2 then
        Exit;
      AValue.Display := xt2QWords;
      AValue.ValueQWord := VectorFrame.XMMRegisters.LongXMM[RegisterIndex].QWords[DataIndex];
    end
    else
    if CompareStr(DataType, 'SINGLE') = 0 then
    begin
      if DataIndex >= 4 then
        Exit;
      AValue.Display := xt4Singles;
      AValue.ValueSingle := VectorFrame.XMMRegisters.LongXMM[RegisterIndex].Singles[DataIndex];
    end
    else
    if CompareStr(DataType, 'DOUBLE') = 0 then
    begin
      if DataIndex >= 2 then
        Exit;
      AValue.Display := xt2Doubles;
      AValue.ValueDouble := VectorFrame.XMMRegisters.LongXMM[RegisterIndex].Doubles[DataIndex];
    end
    else
      Exit;
    ValueStr := Trim(FormatValue(AValue, sfSigned));
    if DecimalSeparator <> '.' then
      ValueStr := StringReplace(ValueStr, DecimalSeparator, '.', [rfReplaceAll, rfIgnoreCase]);
    if Length(ValueStr) >= Index - RegisterPosition then
    begin
      OldLength := Length(Expression);
      SetLength(Expression, Length(Expression) + Length(ValueStr) - (Index - RegisterPosition));
      if Length(ValueStr) > Index - RegisterPosition then
        Move(Expression[Index], Expression[RegisterPosition + Length(ValueStr)], OldLength - Index + 1);
      Move(ValueStr[1], Expression[RegisterPosition], Length(ValueStr));
    end
    else
    begin
      Move(ValueStr[1], Expression[RegisterPosition], Length(ValueStr));
      Move(Expression[Index], Expression[RegisterPosition + Length(ValueStr)], Length(Expression) - Index + 1);
      SetLength(Expression, Length(Expression) + Length(ValueStr) - (Index - RegisterPosition));
    end;
    LocalString := AnsiUpperCase(Expression);
    RegisterPosition := AnsiPos('XMM', LocalString);
  end;

  RegisterPosition := AnsiPos('MM', LocalString);
  while (RegisterPosition > 0) do
  begin
    for Index := RegisterPosition to Length(LocalString) do
      if LocalString[Index] = '.' then
        Break;
    if Index >= Length(LocalString) then
      Exit;
    Val(Copy(LocalString, RegisterPosition + 2, Index - RegisterPosition - 2), RegisterIndex, ErrorCode);
    if (ErrorCode <> 0) or (RegisterIndex < 0) or (RegisterIndex >= 8) then
      Exit;

    DataPosition := Index + 1;
    if DataPosition > Length(LocalString) then
      Exit;
    for Index := DataPosition to Length(LocalString) do
      if CharIsDigit(LocalString[Index]) then
        Break;
    if Index > Length(LocalString) then
      Exit;
    DataType := Copy(LocalString, DataPosition, Index - DataPosition);

    DataPosition := Index;
    for Index := DataPosition to Length(LocalString) do
      if not CharIsDigit(LocalString[Index]) then
        Break;
    Val(Copy(LocalString, DataPosition, Index - DataPosition), DataIndex, ErrorCode);
    if (ErrorCode <> 0) or (DataIndex < 0) then
      Exit;

    if CompareStr(DataType, 'BYTE') = 0 then
    begin
      if DataIndex >= 8 then
        Exit;
      AValue.Display := xt16Bytes;
      AValue.ValueByte := VectorFrame.FPURegisters[RegisterIndex].Data.MMRegister.Bytes[DataIndex];
    end
    else
    if CompareStr(DataType, 'WORD') = 0 then
    begin
      if DataIndex >= 4 then
        Exit;
      AValue.Display := xt8Words;
      AValue.ValueWord := VectorFrame.FPURegisters[RegisterIndex].Data.MMRegister.Words[DataIndex];
    end
    else
    if CompareStr(DataType, 'DWORD') = 0 then
    begin
      if DataIndex >= 2 then
        Exit;
      AValue.Display := xt4DWords;
      AValue.ValueDWord := VectorFrame.FPURegisters[RegisterIndex].Data.MMRegister.DWords[DataIndex];
    end
    else
    if CompareStr(DataType, 'QWORD') = 0 then
    begin
      if DataIndex >= 1 then
        Exit;
      AValue.Display := xt2QWords;
      AValue.ValueQWord := VectorFrame.FPURegisters[RegisterIndex].Data.MMRegister.QWords;
    end
    else
    if CompareStr(DataType, 'SINGLE') = 0 then
    begin
      if DataIndex >= 2 then
        Exit;
      AValue.Display := xt4Singles;
      AValue.ValueSingle := VectorFrame.FPURegisters[RegisterIndex].Data.MMRegister.Singles[DataIndex];
    end
    else
      Exit;
    ValueStr := Trim(FormatValue(AValue, sfSigned));
    if DecimalSeparator <> '.' then
      ValueStr := StringReplace(ValueStr, DecimalSeparator, '.', [rfReplaceAll, rfIgnoreCase]);
    if Length(ValueStr) >= Index - RegisterPosition then
    begin
      OldLength := Length(Expression);
      SetLength(Expression, Length(Expression) + Length(ValueStr) - (Index - RegisterPosition));
      if Length(ValueStr) > Index - RegisterPosition then
        Move(Expression[Index], Expression[RegisterPosition + Length(ValueStr)], OldLength - Index + 1);
      Move(ValueStr[1], Expression[RegisterPosition], Length(ValueStr));
    end
    else
    begin
      Move(ValueStr[1], Expression[RegisterPosition], Length(ValueStr));
      Move(Expression[Index], Expression[RegisterPosition + Length(ValueStr)], Length(Expression) - Index + 1);
      SetLength(Expression, Length(Expression) + Length(ValueStr) - (Index - RegisterPosition));
    end;
    LocalString := AnsiUpperCase(Expression);
    RegisterPosition := AnsiPos('MM', LocalString);
  end;

  Result := True;
end;

function GetThreadContext(hThread: THandle;
  var lpContext: TJclContext): BOOL; stdcall; external kernel32 name 'GetThreadContext';

function SetThreadContext(hThread: THandle;
  const lpContext: TJclContext): BOOL; stdcall; external kernel32 name 'SetThreadContext';

function GetVectorContext(AThread: IOTAThread; out VectorContext: TJclVectorFrame): Boolean;
{$IFDEF COMPILER9_UP}
var
  OTAXMMRegs: TOTAXMMRegs;
  OTAThreadContext: TOTAThreadContext;
begin
  Result := AThread.GetOTAXMMRegisters(OTAXMMRegs);
  if Result then
  begin
    VectorContext.MXCSR := OTAXMMRegs.MXCSR;
    VectorContext.MXCSRMask := $FFFFFFFF;
    Move(OTAXMMRegs,VectorContext.XMMRegisters, SizeOf(TOTAXMMReg) * 8);
    OTAThreadContext := AThread.OTAThreadContext;
    VectorContext.FCW := OTAThreadContext.FloatSave.ControlWord;
    VectorContext.FSW := OTAThreadContext.FloatSave.StatusWord;
    VectorContext.FTW := OTAThreadContext.FloatSave.TagWord;
    Move(OTAThreadContext.FloatSave.RegisterArea[00],VectorContext.FPURegisters[0],SizeOf(Extended));
    Move(OTAThreadContext.FloatSave.RegisterArea[10],VectorContext.FPURegisters[1],SizeOf(Extended));
    Move(OTAThreadContext.FloatSave.RegisterArea[20],VectorContext.FPURegisters[2],SizeOf(Extended));
    Move(OTAThreadContext.FloatSave.RegisterArea[30],VectorContext.FPURegisters[3],SizeOf(Extended));
    Move(OTAThreadContext.FloatSave.RegisterArea[40],VectorContext.FPURegisters[4],SizeOf(Extended));
    Move(OTAThreadContext.FloatSave.RegisterArea[50],VectorContext.FPURegisters[5],SizeOf(Extended));
    Move(OTAThreadContext.FloatSave.RegisterArea[60],VectorContext.FPURegisters[6],SizeOf(Extended));
    Move(OTAThreadContext.FloatSave.RegisterArea[70],VectorContext.FPURegisters[7],SizeOf(Extended));
  end;
end;
{$ELSE COMPILER9_UP}
var
  ContextMemory: Pointer;
  JvContext: PJclContext;
begin
  GetMem(ContextMemory, SizeOf(TJclContext) + 15);
  try
    if (Cardinal(ContextMemory) and 15) <> 0 then
      JvContext := PJclContext((Cardinal(ContextMemory) + 16) and $FFFFFFF0)
    else
      JvContext := ContextMemory;
    JvContext^.ScalarContext.ContextFlags := CONTEXT_EXTENDED_REGISTERS;
    Result := GetThreadContext(AThread.Handle,JvContext^) and
      ((JvContext^.ScalarContext.ContextFlags and CONTEXT_EXTENDED_REGISTERS)<>0);
    if Result then
      VectorContext := JvContext^.VectorContext
    else                                                  
      FillChar(VectorContext, SizeOf(VectorContext), 0);
  finally
    FreeMem(ContextMemory);
  end;
end;
{$ENDIF COMPILER9_UP}

function SetVectorContext(AThread: IOTAThread; const VectorContext: TJclVectorFrame): Boolean;
{$IFDEF COMPILER9_UP}
var
  OTAXMMRegs: TOTAXMMRegs;
begin
  Result := True;
  try
    OTAXMMRegs.MXCSR := VectorContext.MXCSR;
    Move(VectorContext.XMMRegisters,OTAXMMRegs,SizeOf(TOTAXMMReg) * 8);
    AThread.SetOTAXMMRegisters(OTAXMMRegs);
  except
    Result := False;
  end;
end;
{$ELSE COMPILER9_UP}
// MM registers can not saved (changes are overriden by the Borland's debugger)
{const                                      
  CONTEXT_FLAGS =    CONTEXT_CONTROL or CONTEXT_INTEGER or CONTEXT_SEGMENTS
                  or CONTEXT_FLOATING_POINT or CONTEXT_EXTENDED_REGISTERS;
var
  ContextMemory: Pointer;
  JvContext: PJclContext;
  Index: Integer;
begin
  GetMem(ContextMemory,SizeOf(TJclContext)+15);
  try
    if ((Cardinal(ContextMemory) and 15)<>0) then
      JvContext := PJclContext((Cardinal(ContextMemory)+16) and $FFFFFFF0)
    else
      JvContext := ContextMemory;
    JvContext^.ScalarContext.ContextFlags := CONTEXT_FLAGS;
    Result := GetThreadContext(hThread,JvContext^) and
             ((JvContext^.ScalarContext.ContextFlags and CONTEXT_FLAGS) = CONTEXT_FLAGS);
    if (Result) then
    begin
      JvContext^.ScalarContext.ContextFlags := CONTEXT_FLAGS;
      JvContext^.VectorContext := VectorContext;
      for Index := 0 to 7 do
        Move(VectorContext.FPURegisters[Index].Data.FloatValue,JvContext^.ScalarContext.FloatSave.RegisterArea[Index*SizeOf(Extended)],SizeOf(Extended));
      Result := SetThreadContext(hThread,JvContext^);
    end;
  finally
    FreeMem(ContextMemory);
  end;
end;}
var
  ContextMemory: Pointer;
  JvContext: PJclContext;
begin
  GetMem(ContextMemory, SizeOf(TJclContext) + 15);
  try
    if (Cardinal(ContextMemory) and 15) <> 0 then
      JvContext := PJclContext((Cardinal(ContextMemory) + 16) and $FFFFFFF0)
    else
      JvContext := ContextMemory;
    JvContext^.ScalarContext.ContextFlags := CONTEXT_EXTENDED_REGISTERS;
    Result := GetThreadContext(AThread.Handle,JvContext^) and
      ((JvContext^.ScalarContext.ContextFlags and CONTEXT_EXTENDED_REGISTERS) = CONTEXT_EXTENDED_REGISTERS);
    if Result then
      Result := SetThreadContext(AThread.Handle,JvContext^);
  finally
    FreeMem(ContextMemory);
  end;
end;
{$ENDIF COMPILER9_UP}

end.

