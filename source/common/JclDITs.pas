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
{ The Original Code is: DITs.pas.                                                                  }
{ The Initial Developer of the Original Code is Peter J. Haas. Portions created by Peter J. Haas   }
{ are Copyright (C) 2001 Peter J. Haas. All Rights Reserved.                                       }
{                                                                                                  }
{ The Original Code Version 2.0 is: JclDITs.pas.                                                   }
{ The Initial Developer of the Original Code V2.0 is Peter J. Haas. Portions created by            }
{ Peter J. Haas are Copyright (C) 2004 Peter J. Haas. All Rights Reserved.                         }
{ You may retrieve the latest version of the Original Code at the homepage                         }
{ of Peter J. Haas (delphi att pjh2 dott de) located at http://delphi.pjh2.de/                     }
{                                                                                                  }
{--------------------------------------------------------------------------------------------------}
{                                                                                                  }
{  NOTE: As of 2004-05-15, Peter J. Haas has stopped maintaining code he donated to the JCL.       }
{        He is not to be held responsible for modifications applied after this date.               }
{        Peter J. Haas no longer wants to be associated with Project JEDI.                         }
{                                                                                                  }
{--------------------------------------------------------------------------------------------------}
{                                                                                                  }
{  Contributor(s):                                                                                 }
{    Peter J. Haas (peterjhaas)                                                                    }
{    Robert Rossmair (rrossmair)                                                                   }
{                                                                                                  }
{ Alternatively, the contents of this file may be used under the terms of the GNU Lesser General   }
{ Public License (the  "LGPL License"), in which case the provisions of the LGPL License are       }
{ applicable instead of those above.                                                               }
{                                                                                                  }
{ If you wish to allow use of your version of this file only under the terms of the LGPL License   }
{ and not to allow others to use your version of this file under the MPL, indicate your decision by}
{ deleting the provisions above and replace them with the notice and other provisions required by  }
{ the LGPL License. If you do not delete the provisions above, a recipient may use your version of }
{ this file under either the MPL or the LGPL License.                                              }
{                                                                                                  }
{  For more information about the LGPL:                                                            }
{  http://www.gnu.org/copyleft/lesser.html                                                         }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclDITs;

{$I jcl.inc}

interface

uses
  SysUtils;

//--------------------------------------------------------------------------------------------------
// Date and Time Data Interchange (ISO 8601)
//--------------------------------------------------------------------------------------------------

type
  TISODateTimeOption = (dtoDate, dtoTime, dtoMilliseconds, dtoBasic);
  TISODateTimeOptions = set of TISODateTimeOption;
  TISODateTimeSeparator = (dtsT, dtsSpace);
  TISOFloatDecimalSeparator = (fdsComma, fdsPoint);

const
  // basic formats
  ISOBasicDateFormat = 'YYYYMMDD';
  ISOBasicTimeFormat = 'hhnnss';
  // extended formats
  ISODateFormat = 'YYYY"-"MM"-"DD';
  ISOTimeFormat = 'hh":"nn":"ss';
  // milliseconds
  ISOTimeMSec = '","zzz';
  // date time separator
  ISODateTimeSeparatorT = 'T';
  ISODateTimeSeparatorSpace = ' ';
  ISODateTimeSeparators: array [TISODateTimeSeparator] of Char =
    (ISODateTimeSeparatorT, ISODateTimeSeparatorSpace);
  // date time format
  ISOBasicDateTimeFormat = ISOBasicDateFormat + '"' + ISODateTimeSeparatorT + '"' + ISOBasicTimeFormat;
  ISODateTimeFormat = ISODateFormat + ISODateTimeSeparatorT + ISOTimeFormat;
  // float decimal separator
  ISOFloatDecimalSeparatorComma = ',';
  ISOFloatDecimalSeparatorPoint = '.';
  ISOFloatDecimalSeparators: array [TISOFloatDecimalSeparator] of Char =
    (ISOFloatDecimalSeparatorComma, ISOFloatDecimalSeparatorPoint);

// Convert TDateTime to string
function ISODateTimeToStrCustom(const Value: TDateTime;
  Options: TISODateTimeOptions;
  DateTimeSeparator: TISODateTimeSeparator = dtsT): string;
// Converts TDateTime to date string 'YYYY-MM-DD'
function ISODateToStr(const Value: TDateTime): string;
// Converts TDateTime to time string 'hh:mm:ss'
function ISOTimeToStr(const Value: TDateTime): string;
// Converts TDateTime to date time string 'YYYY-MM-DDThh:mm:ss'
function ISODateTimeToStr(const Value: TDateTime): string;
// Converts TDateTime to date string 'YYYYMMDD'
function ISOBasicDateToStr(const Value: TDateTime): string;
// Converts TDateTime to time string 'hhmmss'
function ISOBasicTimeToStr(const Value: TDateTime): string;
// Converts TDateTime to date time string 'YYYYMMDDThhmmss'
function ISOBasicDateTimeToStr(const Value: TDateTime): string;
// Converts an ISO date string to TDateTime and replaces the date part of Date
// Valid strings are
//   'YYYY-MM-DD' and 'YYYYMMDD'
function TryISOStrToDate(const Value: string; var Date: TDateTime): Boolean;
// Converts an ISO time string to TDateTime and replace the time part of Time
// Valid strings are
//   'hh:mm:ss,zzz', 'hh:mm:ss.zzz', 'hhmmss,zzz', 'hhmmss.zzz',
//   'hh:mm:ss', 'hhmmss', 'hh:mm' and 'hhmm'
function TryISOStrToTime(const Value: string; var Time: TDateTime): Boolean;
// Converts an ISO time stamp to a TDateTime,
// date and time are separated with 'T' or ' '
function TryISOStrToDateTime(const Value: string; out DateTime: TDateTime): Boolean;
// Converts an ISO date string to TDateTime
// Valid strings:
//   'YYYY-MM-DD' and 'YYYYMMDD'
function ISOStrToDate(const Value: string): TDateTime;
function ISOStrToDateDef(const Value: string; const Default: TDateTime): TDateTime;
// Converts an ISO time string to TDateTime
// Valid strings:
//   'hh:mm:ss,zzz', 'hh:mm:ss.zzz', 'hhmmss,zzz', 'hhmmss.zzz',
//   'hh:mm:ss', 'hhmmss', 'hh:mm' and 'hhmm'
function ISOStrToTime(const Value: string): TDateTime;
function ISOStrToTimeDef(const Value: string; const Default: TDateTime): TDateTime;
// Converts an ISO time stamp to a TDateTime,
// date and time are separated with 'T' or ' '
function ISOStrToDateTime(const Value: string): TDateTime;
function ISOStrToDateTimeDef(const Value: string; const Default: TDateTime): TDateTime;

//--------------------------------------------------------------------------------------------------
// Float Data Interchange (ISO 31-0)
//--------------------------------------------------------------------------------------------------

// Converts a float value to a string
// DecimalSeparator is decimal separator, no thousand separator
// Value: the value to convert
// Precision: precision of the result, 1..18, default: 15 digits
// DecimalSeparator: used separator
// if Abs(Value) < 10^-4 or >= 10^15 the function returns a string in the
// 'Scientific' format
// if Value is NAN, INF or -INF the function return 'NAN', 'INF' or '-INF'
function ISOFloatToStr(const Value: Extended;
  Precision: Integer = 15 ;
  DecimalSeparator: TISOFloatDecimalSeparator = fdsComma): string;
// Converts a string to a float value
// Decimal separators are ',' or '.'
// Thousands separator ' '
// The string can be a number in the 'Scientific' format
// 'NAN', 'INF', '-INF' are allowed
function ISOTextToFloat(Value: string; out Float: Extended): Boolean;
// Converts a string to a float value
// Decimal separators are ',' or '.'
// Thousands separator ' ' or ''
// The string can be a number in the 'Scientific' format
// 'NAN', 'INF', '-INF' are allowed
function ISOStrToFloat(const Value: string): Extended;
function ISOStrToFloatDef(const Value: string; const Default: Extended): Extended;

implementation

uses
  JclResources;

//==================================================================================================
// Date and Time Data Interchange (ISO 8601)
//==================================================================================================

// Convert TDateTime to string
function ISODateTimeToStrCustom(const Value: TDateTime;
  Options: TISODateTimeOptions;
  DateTimeSeparator: TISODateTimeSeparator = dtsT): string;
var
  DTFormat: string;
begin
  // Parameter check
  if Options = [] then
    Options := [dtoDate, dtoTime]
  else
  if Options = [dtoBasic] then
    Options := [dtoDate, dtoTime, dtoBasic]
  else
  if dtoMilliseconds in Options then
    Include(Options, dtoTime);
  // Build format string
  if dtoDate in Options then
  begin
    if dtoBasic in Options then
      DTFormat := ISOBasicDateFormat
    else
      DTFormat := ISODateFormat;
    if dtoTime in Options then
      DTFormat := DTFormat + '"' + ISODateTimeSeparators[DateTimeSeparator] + '"';
  end
  else
    DTFormat := '';
  if dtoTime in Options then
  begin
    if dtoBasic in Options then
      DTFormat := DTFormat + ISOBasicTimeFormat
    else
      DTFormat := DTFormat + ISOTimeFormat;
    
    if dtoMilliseconds in Options then
      DTFormat := DTFormat + ISOTimeMSec;
    
  end;
  // convert
  Result := FormatDateTime(DTFormat, Value);
end;

//--------------------------------------------------------------------------------------------------

// Converts TDateTime to date string 'YYYY-MM-DD'
function ISODateToStr(const Value: TDateTime): string;
begin
  Result := FormatDateTime(ISODateFormat, Value);
end;

//--------------------------------------------------------------------------------------------------

// Converts TDateTime to time string 'hh:mm:ss'
function ISOTimeToStr(const Value: TDateTime): string;
begin
  Result := FormatDateTime(ISOTimeFormat, Value);
end;

//--------------------------------------------------------------------------------------------------

// Converts TDateTime to date time string 'YYYY-MM-DDThh:mm:ss'
function ISODateTimeToStr(const Value: TDateTime): string;
begin
  Result := FormatDateTime(ISODateTimeFormat, Value);
end;

//--------------------------------------------------------------------------------------------------

// Converts TDateTime to date string 'YYYYMMDD'
function ISOBasicDateToStr(const Value: TDateTime): string;
begin
  Result := FormatDateTime(ISOBasicDateFormat, Value);
end;

//--------------------------------------------------------------------------------------------------

// Convert TDateTime to time string 'hhmmss'
function ISOBasicTimeToStr(const Value: TDateTime): string;
begin
  Result := FormatDateTime(ISOBasicTimeFormat, Value);
end;

//--------------------------------------------------------------------------------------------------

// Converts TDateTime to date time string 'YYYYMMDDThhmmss'
function ISOBasicDateTimeToStr(const Value: TDateTime): string;
begin
  Result := FormatDateTime(ISOBasicDateTimeFormat, Value);
end;

//--------------------------------------------------------------------------------------------------

function CheckDateTimeFormat(const Value, DTFormat: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Length(Value) <> Length(DTFormat) then
    Exit;
  for I := 1 to Length(Value) do
  begin
    if DTFormat[I] = '9' then    // Digit
    begin
      if not (Value[I] in ['0'..'9']) then
        Exit;
    end
    else
    begin
      if DTFormat[I] <> Value[I] then
        Exit;
    end;
  end;
  Result := True;
end;

//--------------------------------------------------------------------------------------------------

// Converts an ISO date string to TDateTime and replace the date part of Date
function TryISOStrToDate(const Value: string; var Date: TDateTime): Boolean;
var
  Offset: Integer;
  Year, Month, Day: Word;
begin
  Result := False;
  if CheckDateTimeFormat(Value, '9999-99-99') then
    Offset := 1
  else
  if CheckDateTimeFormat(Value, '99999999') then
    Offset := 0
  else
    Exit;
  Year  := StrToIntDef(Copy(Value, 1, 4), 0);
  Month := StrToIntDef(Copy(Value, 5 + 1 * Offset, 2), 0);
  Day   := StrToIntDef(Copy(Value, 7 + 2 * Offset, 2), 0);
  try
    Date := EncodeDate(Year, Month, Day) + Frac(Date);
    Result := True;
  except
    on EConvertError do{ Result := False };
  end;
end;

//--------------------------------------------------------------------------------------------------

// Converts an ISO time string to TDateTime and replace the time part of Time
function TryISOStrToTime(const Value: string; var Time: TDateTime): Boolean;
var
  S, MSecStr: string;
  I: Integer;
  Hours, Minutes, Seconds, Milliseconds: Word;
  Offset: Integer;
  WithSeconds: Boolean;
begin
  Result := False;
  // Milliseconds part
  I := Pos(ISOFloatDecimalSeparatorComma, Value);    // ','
  if I = 0 then
    I := Pos(ISOFloatDecimalSeparatorPoint, Value);  // '.'
  if I = 0 then
  begin
    S := Value;
    MSecStr := '';
  end
  else
  begin
    S := Copy(Value, 1, I - 1);
    MSecStr := Copy(Value, I + 1, MaxInt);
  end;
  if CheckDateTimeFormat(S, '99:99:99') then
  begin
    Offset := 1;
    WithSeconds := True;
  end
  else
  if CheckDateTimeFormat(S, '999999') then
  begin
    Offset := 0;
    WithSeconds := True;
  end
  else
  if CheckDateTimeFormat(S, '99:99') then
  begin
    Offset := 1;
    WithSeconds := False;
  end
  else
  if CheckDateTimeFormat(S, '9999') then
  begin
    Offset := 0;
    WithSeconds := False;
  end
  else
    Exit;

  Hours   := StrToIntDef(Copy(Value, 1, 2), 100);
  Minutes := StrToIntDef(Copy(Value, 3 + 1 * Offset, 2), 100);

  if WithSeconds then
  begin
    Seconds := StrToIntDef(Copy(Value, 5 + 2 * Offset, 2), 100);
  end
  else
  begin
    Seconds := 0;
    if Length(MSecStr) > 0 then  // Milliseconds without seconds -> error
      Exit;
  end;

  case Length(MSecStr) of
    0:
      Milliseconds := 0;
    3:
      Milliseconds := StrToIntDef(MSecStr, 10000);
  else
    Exit;
  end;

  try
    Time := EncodeTime(Hours, Minutes, Seconds, Milliseconds) + Int(Time);
    Result := True;
  except
    on EConvertError do{ Result := False };
  end;
end;

//--------------------------------------------------------------------------------------------------

// Converts an ISO time stamp to a TDateTime,
// date and time are separated with 'T' or ' '
function TryISOStrToDateTime(const Value: string; out DateTime: TDateTime): Boolean;
var
  DatePart, TimePart : string;
  I : Integer;
begin
  Result := False;
  DateTime := 0;
  I := Pos('T', Value);
  if I = 0 then
    I := Pos(' ', Value);
  if I > 0 then
  begin
    DatePart := Copy(Value, 1, I-1);
    TimePart := Copy(Value, I+1, MaxInt);
    Result := TryISOStrToDate(DatePart, DateTime) and TryISOStrToTime(TimePart, DateTime);
  end;
end;

//--------------------------------------------------------------------------------------------------

// Converts an ISO date string to TDateTime
// Valid strings:
//   'CCYY-MM-DD' and 'CCYYMMDD'
function ISOStrToDate(const Value: string): TDateTime;
begin
  Result := 0;
  if not TryISOStrToDate(Value, Result) then
    {$IFDEF DELPHI3}
    raise EConvertError.CreateFmt(RsDITInvalidISODate, [Value]);
    {$ELSE DELPHI3}
    raise EConvertError.CreateResFmt(@RsDITInvalidISODate, [Value]);
    {$ENDIF DELPHI3}
end;

//--------------------------------------------------------------------------------------------------

function ISOStrToDateDef(const Value: string; const Default: TDateTime): TDateTime;
begin
  Result := 0;
  if not TryISOStrToDate(Value, Result) then
    Result := Default;
end;

//--------------------------------------------------------------------------------------------------

// Converts an ISO time string to TDateTime
// Valid strings:
//   'hh:mm:ss,zzz', 'hh:mm:ss.zzz', 'hhmmss,zzz', 'hhmmss.zzz',
//   'hh:mm:ss', 'hhmmss', 'hh:mm' and 'hhmm'
function ISOStrToTime(const Value: string): TDateTime;
begin
  Result := 0;
  if not TryISOStrToTime(Value, Result) then
    {$IFDEF DELPHI3}
    raise EConvertError.CreateFmt(RsDITInvalidISOTime, [Value]);
    {$ELSE DELPHI3}
    raise EConvertError.CreateResFmt(@RsDITInvalidISOTime, [Value]);
    {$ENDIF DELPHI3}
end;

//--------------------------------------------------------------------------------------------------

function ISOStrToTimeDef(const Value: string; const Default: TDateTime): TDateTime;
begin
  Result := 0;
  if not TryISOStrToTime(Value, Result) then
    Result := Default;
end;

//--------------------------------------------------------------------------------------------------

// Converts an ISO time stamp to a TDateTime,
// date and time are separated with 'T' or ' '
function ISOStrToDateTime(const Value: string): TDateTime;
begin
  if not TryISOStrToDateTime(Value, Result) then
    {$IFDEF DELPHI3}
    raise EConvertError.CreateFmt(RsDITInvalidISODateTime, [Value]);
    {$ELSE DELPHI3}
    raise EConvertError.CreateResFmt(@RsDITInvalidISODateTime, [Value]);
    {$ENDIF DELPHI3}
end;

//--------------------------------------------------------------------------------------------------

function ISOStrToDateTimeDef(const Value: string; const Default: TDateTime): TDateTime;
begin
  if not TryISOStrToDateTime(Value, Result) then
    Result := Default;
end;

//==================================================================================================
// Float Data Interchange (ISO 31-0)
//==================================================================================================

function ISOFloatRecToStr(const Rec: TFloatRec;
  DecimalSeparator: TISOFloatDecimalSeparator = fdsComma): string;
var
  DecimalSeparatorPos: Integer;
  I: Integer;
begin
  case Rec.Exponent of
    Low(Rec.Exponent):
      Result := 'NAN';
    High(Rec.Exponent):
      begin
        if Rec.Negative then
          Result := '-INF'
        else
          Result := 'INF';
      end;
    Low(Rec.Exponent)+1..-4, 16..High(Rec.Exponent)-1:
      begin
        Result := Rec.Digits[0];
        if Rec.Digits[1] <> #0 then
          Result := Result + ISOFloatDecimalSeparators[DecimalSeparator] + PChar(@Rec.Digits[1]);
        if Rec.Exponent <> 1 then
          Result := Result + 'E' + IntToStr(Rec.Exponent - 1);
        if Rec.Negative then
          Result := '-' + Result;
      end;
  else
    Result := Rec.Digits;
    DecimalSeparatorPos := Rec.Exponent + 1;
    for I := DecimalSeparatorPos to 1 do   // Nullen vor dem Ergebnis
      Result := '0' + Result;
    if DecimalSeparatorPos < 2 then
      DecimalSeparatorPos := 2;
    for I := DecimalSeparatorPos - Length(Result) - 2 downto 0 do
      Result := Result + '0';
    if DecimalSeparatorPos <= Length(Result) then
      Insert(ISOFloatDecimalSeparators[DecimalSeparator], Result, DecimalSeparatorPos);
    if Rec.Negative then
      Result := '-' + Result;
  end;
end;

//--------------------------------------------------------------------------------------------------

// Convert a float value to string
// with DecimalSeparator as decimal separator and without thousand separator
function ISOFloatToStr(const Value: Extended;
  Precision: Integer = 15;
  DecimalSeparator: TISOFloatDecimalSeparator = fdsComma): string;
var
  FloatRec: TFloatRec;
begin
  {$IFDEF FPC}
  FloatToDecimal(FloatRec, Value, Precision, High(Integer) div 2);
  {$ELSE}
  FloatToDecimal(FloatRec, Value, fvExtended, Precision, High(Integer) div 2);
  {$ENDIF FPC}
  Result := ISOFloatRecToStr(FloatRec, DecimalSeparator);
end;

//--------------------------------------------------------------------------------------------------

// Convert a string to a float value
// Decimal separator ',' or '.'
// Thousands separator ' '
function ISOTextToFloat(Value: string; out Float: Extended): Boolean;
var
  I: Integer;
begin
  // replace ',' by '.'
  for I := 1 to Length(Value) do
    if Value[I] = ',' then
      Value[I] := '.';
  // delete spaces
  repeat
    I := Pos(' ', Value);
    if I > 0 then
      Delete(Value, I, 1);
  until I <= 0;
  // convert
  Val(Value, Float, I);
  Result := I = 0;
  if not Result then
  begin
    Result := Length(Value) > 0;
    if Result then
    begin
      if Value[1] = '+' then
        Delete(Value, 1, 1);
      Value := UpperCase(Value);
      if Value = 'NAN' then
        Float := 0/0
      else
      if Value = 'INF' then
        Float := 1/0
      else
      if Value = '-INF' then
        Float := -1/0
      else
        Result := False;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function ISOStrToFloat(const Value: string): Extended;
begin
  if not ISOTextToFloat(Value, Result) then
    {$IFDEF DELPHI3}
    raise EConvertError.CreateFmt(RsDITInvalidISOFloat, [Value]);
    {$ELSE DELPHI3}
    raise EConvertError.CreateResFmt(@RsDITInvalidISOFloat, [Value]);
    {$ENDIF DELPHI3}
end;

//--------------------------------------------------------------------------------------------------

function ISOStrToFloatDef(const Value: string; const Default: Extended): Extended;
begin
  if not ISOTextToFloat(Value, Result) then
    Result := Default;
end;

//  History:
//   2001-09-10  Version 1.0
//
//   2001-09-27  Version 1.01
//    - ISOStrToDateTime accept now a space as date/time separator
//    - ISOStrToFloat accept now spaces as thousands separator
//
//   2004-03-20  Version 2.0
//    - add ISOBasicDateToStr, ISOBasicTimeToStr, ISOBasicDateTimeToStr
//    - introduce TISODateTimeSeparator and TISOFloatDecimalSeparator
//      instead of arbitrary characters.
//
//   $Log$
//   Revision 1.9  2004/07/28 20:59:01  rrossmair
//   comments added to except blocks
//
//   Revision 1.8  2004/07/28 18:00:49  marquardt
//   various style cleanings, some minor fixes
//
//   Revision 1.7  2004/06/14 13:05:16  marquardt
//   style cleaning ENDIF, Tabs
//
//   Revision 1.6  2004/05/31 22:49:29  rrossmair
//   resolved $IFDEF DEFAULTPARAMS (always true in supported versions)
//
//   Revision 1.5  2004/05/31 22:04:22  rrossmair
//   added PJH disclaimer; some formatting. Not longer generated file.
//
//   Revision 1.4  2004/05/05 05:50:40  rrossmair
//   fixed typo: '}' instead of ')'
//
//   Revision 1.3  2004/05/05 05:28:00  rrossmair
//   ISOFloatToStr changed for FPC compatibility
//
//   Revision 1.2  2004/05/05 00:36:15  mthoma
//   Updated headers: Added donors as contributors, adjusted the initial authors, added cvs names when they were not obvious. Changed $data to $date where necessary,
//
//   Revision 1.1  2004/04/18 00:40:02  peterjhaas
//   add prototypes for standalone library / JCL
//   be careful with any modification to avoid breaks
//

end.
