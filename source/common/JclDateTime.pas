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
{ The Original Code is JclDateTime.pas.                                        }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: June 18, 2000                                                 }
{                                                                              }
{******************************************************************************}

{******************************************************************************}
{                                                                              }
{ Modifications by MSchnell:                                                   }
{ 000622:                                                                      }
{                                                                              }
{ Name changed GetCenturyOfDate -> CenturyOfDate                               }
{                                                                              }
{ Name changed GetCenturyBaseYear -> CenturyBaseYear                           }
{                                                                              }
{ function GetWeekNumber(Today: TDateTime): string;  ->                        }
{ function ISOWeekNumber(DateTime: TDateTime; var YearOfWeekDay: Integer): Integer;}
{                                                                              }
{ Added overload function IsLeapYear(Year: Integer): Boolean;                  }
{ to avoid wrong results if the user thinks he calls SysUtils.IsLeapYear       }
{ IsLeapYear is now using SysUtils.IsLeapYear                                  }
{                                                                              }
{ Changed function DateTimeToSeconds(DateTime: TDateTime): extended; ->        }
{ function TimeOfDateTimeToSeconds(DateTime: TDateTime): Integer;              }
{ now not calling DecodeTime any more                                          }
{                                                                              }
{ Added function TimeOfDateTimeToMSecs(DateTime: TDateTime): Integer           }
{                                                                              }
{ 000624:                                                                      }
{ DateTimeToDosDateTime performs the same action as SysUtils.DateTimeToFileDate}
{  so let's have Delphi do the work here                                       }
{ DosDateTimeToDateTime performs the same action as SysUtils.FileDateToDateTime}
{  so let's have Delphi do the work here                                       }
{                                                                              }
{ DosDateTimeToStr does not use FileTime any more                              }
{                                                                              }
{ Added function DateTimeToFileTime                                            }
{ Added function LocalDateTimeToFileTime                                       }
{ Changed function  FileTimeToDateTime                                         }
{           not using TSystemDate and avoid systemcalls                        }
{ Changed function  FileTimeToLocalDateTime                                    }
{           not using TSystemDate and avoid systemcalls                        }
{                                                                              }
{ 000625:                                                                      }
{ Added function SystemTimeToFileTime                                          }
{ Added function FieTimeToSystemTime                                           }
{ Added function Datetimetosystemtime                                          }
{ Added function DosDateTimeToFileTime                                         }
{ Added function FileTimeToDosDateTime                                         }
{ Added function SystemTimeToStr                                               }
{                                                                              }
{ 000706:                                                                      }
{ Formatted according to style rules                                           }
{                                                                              }
{ 000708:                                                                      }
{ Swapped function names CenturyOfDate and CenturyBaseYear                     }
{ those were obviously called wrong before                                     }
{ Attention: must be done in the Help, too                                     }
{                                                                              }
{ 000716:                                                                      }
{ Support for negative dates and Year >= 10000 added for DecodeDate and EncodeDate}
{                                                                              }
{ 000809:                                                                      }
{ added functions                                                              }
{ CreationDateTimeOfFile, LastAccessDateTimeOfFile and LastWriteDateTimeOfFile }


{ TODO:                                                                        }



{ Add in Help:                                                                 }
{  Type TDosDateTime is used by Delphi as "FileDate"                           }
{                                                                              }
{  We do all conversions (but thoses provided by Delphi anyway)  between       }
{  TDatetime, TDosDateTime, TFileTime and TSystemTime         plus             }
{  TDatetime, TDosDateTime, TFileTime, TSystemTime to string                   }
{                                                                              }
{  Swapped function names CenturyOfDate and CenturyBaseYear                    }
{         those were obviously called wrong before                             }
{                                                                              }
{  Support for negative dates and Year >= 10000 added for DecodeDate and EncodeDate}
{  With that for DayOfDate, MonthOfYear, YearOfDate,CenturyOfDate and CenturyBaseYear, too}
{   as people did not use 12 Months before year 0001, and supposedly will not after 9999,}
{   we just use the solar year and a month = 1/12 of a solar year here         }
{                                                                              }
{  CreationDateTimeOfFile, LastAccessDateTimeOfFile and LastWriteDateTimeOfFile}
{                                                                              }
{******************************************************************************}

unit JclDateTime;

{$I JCL.INC}

interface

uses
  Windows, SysUtils,
  JclBase;

function EncodeDate(Year: Integer; Month, Day: Word): TDateTime; 
procedure DecodeDate(Date: TDateTime; var Year, Month, Day: Word); overload;
procedure DecodeDate(Date: TDateTime; var Year: Integer; var Month, Day: Word); overload;

function CenturyOfDate(DateTime: TDateTime): Integer;
function CenturyBaseYear(DateTime: TDateTime): Integer;
function DayOfDate(DateTime: TDateTime): Integer;
function MonthOfDate(DateTime: TDateTime): Integer;
function YearOfDate(DateTime: TDateTime): Integer;

function HourOfTime(DateTime: TDateTime): Integer;
function MinuteOfTime(DateTime: TDateTime): Integer;
function SecondOfTime(DateTime: TDateTime): Integer;

function ISOWeekNumber(DateTime: TDateTime; var YearOfWeekNumber: Integer): Integer;
function IsLeapYear(Year: Integer): Boolean;  overload;
function IsLeapYear(DateTime: TDateTime): Boolean;  overload;
function DaysInMonth(DateTime: TDateTime): Integer;
function Make4DigitYear(Year, Pivot: Integer): Integer;
function EasterSunday(Year: Integer): TDateTime;

//------------------------------------------------------------------------------
// Conversion
//------------------------------------------------------------------------------

type
  TDosDateTime = Integer;

function HoursToMSecs(Hours: Integer): Integer;
function MinutesToMSecs(Minutes: Integer): Integer;
function SecondsToMSecs(Seconds: Integer): Integer;

function TimeOfDateTimeToSeconds(DateTime: TDateTime): Integer;
function TimeOfDateTimeToMSecs(DateTime: TDateTime): Integer;

function DateTimeToLocalDateTime(DateTime: TDateTime): TDateTime;
function DateTimeToDosDateTime(const DateTime: TDateTime): TDosDateTime;
function DateTimeToFileTime(DateTime: TDateTime): TFileTime;
function DateTimeToSystemTime(DateTime: TDateTime): TSystemTime; overload;

function LocalDateTimeToFileTime(DateTime: TDateTime): FileTime;
function LocalDateTimeToDateTime(DateTime: TDateTime): TDateTime;

function DosDateTimeToDateTime(const DosTime: TDosDateTime): TDateTime;
function DosDateTimeToFileTime(DosTime: TDosDateTime): TFileTime; overload;
function DosDateTimeToSystemTime(const DosTime: TDosDateTime): TSystemTime;
function DosDateTimeToStr(DateTime: Integer): string;

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
function FileTimeToLocalDateTime(const FileTime: TFileTime): TDateTime;
function FileTimeToDosDateTime(const FileTime: TFileTime): TDosDateTime; overload;
function FileTimeToSystemTime(const FileTime: TFileTime): TSystemTime; overload;
function FileTimeToStr(const FileTime: TFileTime): string;

function SystemTimeToDosDateTime(const SystemTime: TSystemTime): TDosDateTime;
function SystemTimeToFileTime(const SystemTime: TSystemTime): TFileTime; overload;
function SystemTimeToStr(const SystemTime: TSystemTime): string;

//------------------------------------------------------------------------------
// Filedates
//------------------------------------------------------------------------------

function CreationDateTimeOfFile(const sr: TSearchRec): TDateTime;
function LastAccessDateTimeOfFile(const sr: TSearchRec): TDateTime;
function LastWriteDateTimeOfFile(const sr: TSearchRec): TDateTime;


type
  EJclDateTimeError = class (EJclError);

implementation

const
  DaysInMonths: array [1..12] of Integer =
    (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

  MinutesPerDay  = 60 * 24;
  SecondsPerDay  = MinutesPerDay * 60;
  MsecsPerMinute = 60 * 1000;
  MsecsPerHour   = 60 * MsecsPerMinute;
  DaysPerYear    = 365.2422454;          // Solar Year
  DaysPerMonth   = DaysPerYear / 12;
  DateTimeBaseDay= -693593;              //  1/1/0001
  EncodeDateMaxYear = 9999;
  SolarDifference = 1.7882454;           //  Difference of Juliab Calendar to Solar Calendar at 1/1/10000
  DateTimeMaxDay = 2958466;              //  12/31/EncodeDateMaxYear + 1;
  FileTimeBase = -109205.0;
  FileTimeStep: Extended = 24.0 * 60.0 * 60.0 * 1000.0 * 1000.0 * 10.0; // 100 nSek per Day

{$I RESOURCES.INC}

//------------------------------------------------------------------------------

function EncodeDate(Year: Integer; Month, Day: Word): TDateTime; overload;
begin
  if (Year > 0) and (Year < EncodeDateMaxYear+1) then
    Result := SysUtils.EncodeDate(Year, Month, Day)
  else
  begin
    if Year <= 0 then
      Result := Year * DaysPerYear + DateTimeBaseDay
    else      // Year >= 10000
              // for some reason year 0 does not exist so we switch from
              // the last day of year -1 (-693594) to the first days of year 1
      Result := (Year-1) * DaysPerYear + DateTimeBaseDay // BaseDate is 1/1/1
                + SolarDifference;                       // guarantee a smooth transition at 1/1/10000
    Result := trunc (result);            
    Result := Result + (Month-1) * DaysPerMonth;
    Result := Result + (Day-1);
  end;
end;

//------------------------------------------------------------------------------

procedure DecodeDate(Date: TDateTime; var Year, Month, Day: Word); overload;
begin
  SysUtils.DecodeDate(Date, Year, Month, Day);
end;

//------------------------------------------------------------------------------

procedure DecodeDate(Date: TDateTime; var Year: Integer; var Month, Day: Word); overload;
var WYear : Word;
    RDays, RMonths : TDateTime;
begin
  if (Date >= DateTimeBaseDay) and (Date < DateTimeMaxDay) then
  begin
    SysUtils.DecodeDate(Date, WYear, Month, Day);
    Year := WYear;
  end
  else
  begin
    Year := Trunc((Date - DateTimeBaseDay) / DaysPerYear);
    if Year <= 0 then Year := Year - 1
              // for some historical reason year 0 does not exist so we switch from
              // the last day of year -1 (-693594) to the first days of year 1
    else                                      // Year >= 10000
      Date := Date  - SolarDifference;        // guarantee a smooth transition at 1/1/10000
    RDays   := Date - DateTimeBaseDay;        // Days relative to 1/1/0001
    RMonths := RDays / DaysPerMonth;          // "Months" relative to 1/1/0001 
    RMonths := RMonths - Year * 12.0;         // 12 "Months" per Year
    if RMonths < 0 then                       // possible truncation glitches
    begin
      RMonths := 11;
      Year := Year-1;
    end;
    Month   := Trunc(RMonths);
    Rmonths := Month;
    Month   := Month + 1;
    RDays   := RDays - Year * DaysPerYear;    // subtract Base Day ot the year
    RDays   := RDays - RMonths * DaysPerMonth;// subtract Base Day of the month
    Day     := Trunc (RDays)+ 1;
    if Year > 0 then                          // Year >= 10000
       Year := Year + 1;                      // BaseDate is 1/1/1
  end;
end;

//------------------------------------------------------------------------------

procedure ResultCheck(Val: LongBool);
begin
  if not Val then
    raise EJclDateTimeError.Create(RsDateConversion);
end;

//------------------------------------------------------------------------------

function CenturyBaseYear(DateTime: TDateTime): Integer;
var Y : integer;
begin
  Y := YearOfDate(DateTime);
  Result := (Y div 100) * 100;
  if y > 0 then
  else
    Result := Result - 100;
end;

//------------------------------------------------------------------------------

function CenturyOfDate(DateTime: TDateTime): Integer;
var Y : Integer;
begin
  Y := YearOfDate(DateTime);
  if y > 0 then
    Result := (Y div 100) + 1
  else
    Result := (Y div 100) - 1;
end;

//------------------------------------------------------------------------------

function DayOfDate(DateTime: TDateTime): Integer;
var
  Y : Integer;
  M, D: Word;
begin
  DecodeDate(DateTime, Y, M, D);
  Result := D;
end;

//------------------------------------------------------------------------------

function MonthOfDate(DateTime: TDateTime): Integer;
var
  Y : Integer;
  M, D: Word;
begin
  DecodeDate(DateTime, Y, M, D);
  Result := M;
end;

//------------------------------------------------------------------------------

function YearOfDate(DateTime: TDateTime): Integer;
var
  M, D: Word;
begin
 DecodeDate(DateTime, Result, M, D);
end;

//------------------------------------------------------------------------------

function HourOfTime(DateTime: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  DecodeTime(DateTime, H, M, S, MS);
  Result := H;
end;

//------------------------------------------------------------------------------

function MinuteOfTime(DateTime: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  DecodeTime(DateTime, H, M, S, MS);
  Result := M;
end;

//------------------------------------------------------------------------------

function SecondOfTime(DateTime: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  DecodeTime(DateTime, H, M, S, MS);
  Result := S;
end;

//------------------------------------------------------------------------------

function TimeOfDateTimeToSeconds(DateTime: TDateTime): Integer;
begin
  Result := Round(Frac(DateTime) * SecondsPerDay);
end;

//------------------------------------------------------------------------------

function TimeOfDateTimeToMSecs(DateTime: TDateTime): Integer;
begin
  Result := Round(Frac(DateTime) * MSecsPerDay);
end;

//------------------------------------------------------------------------------

function DaysInMonth(DateTime: TDateTime): Integer;
var
  M: Integer;
begin
  M := MonthOfDate(DateTime);
  if (M = 2) and IsLeapYear(DateTime) then
    Result := 29
  else
    Result := DaysInMonths[M];
end;

//------------------------------------------------------------------------------

// DayOfWeek function returns integer 1..7 equivalent to Sunday..Saturday.
// ISO 8601 weeks start with Monday and the first week of a year is the one which
// includes the first Thursday - Fiddle takes care of all this}

function ISOWeekNumber(DateTime: TDateTime; var YearOfWeekNumber: Integer): Integer;
const
  Fiddle: array [1..7] of Byte = (6, 7, 8, 9, 10, 4, 5);
var
  Present, StartOfYear: TDateTime;
  FirstDayOfYear, WeekNumber, NumberOfDays: Integer;
  Year, Month, Day: Word;
begin
  Present := Trunc(DateTime); // truncate to remove hours, mins and secs
  DecodeDate(Present, Year, Month, Day); // decode to find year
  StartOfYear := EncodeDate(Year, 1, 1);  // encode 1st Jan of the year
  // find what day of week 1st Jan is, then add days according to rule
  FirstDayOfYear := Fiddle[DayOfWeek(StartOfYear)];
  // calc number of days since beginning of year + additional according to rule
  NumberOfDays := Trunc(Present - StartOfYear) + FirstDayOfYear;
  // calc number of weeks
  WeekNumber := NumberOfDays div 7;
  if WeekNumber = 0 then
    // see if previous year end was week 52 or 53
    Result := ISOWeekNumber(EncodeDate(Year - 1, 12, 31), YearOfWeekNumber)
  else
  begin
    YearOfWeekNumber := Year;
    Result := WeekNumber;
    if WeekNumber = 53 then
    begin
      // if 31st December less than Thursday then must be week 01 of next year
      if DayOfWeek(EncodeDate(Year, 12, 31)) < 5 then
      begin
        YearOfWeekNumber := Year + 1;
        Result := 1;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function IsLeapYear(Year: Integer): Boolean;
begin
  // Result := (Year mod 4 = 0) and ((Year mod 100 <> 0) or (Year mod 400 = 0));
  Result :=  SysUtils.IsLeapYear(Year);
end;

function IsLeapYear(DateTime: TDateTime): Boolean;
begin
  Result := IsLeapYear(YearOfDate(DateTime));
end;

//------------------------------------------------------------------------------

function Make4DigitYear(Year, Pivot: Integer): Integer;
begin
  // TODO
  Assert((Year >= 0) and (Year <= 100) and (Pivot >= 0) and (Pivot <= 100));
  if Year = 100 then
    Year := 0;
  if Pivot = 100 then
    Pivot := 0;
  if Year < Pivot then
    Result := 2000 + Year
  else
    Result := 1900 + Year;
end;

//------------------------------------------------------------------------------

function EasterSunday(Year: Integer): TDateTime;
var
  A, B, C, D, F, G: Double;
  E: Integer;
begin
  A := Year mod 19;
  B := Year mod 4;
  C := Year mod 7;
  F := 19 * A + 24;
  D := F - (Int(F / 30) * 30);
  G := (5 + 2 * B + 4 * C + 6 * D);
  E := Trunc((G - (Int(G / 7) * 7)) + D + 22);
  if E <= 31 then
    Result := EncodeDate(Year, 3, E)
  else
  begin
    if E - 31 >= 26 then
      E := 19
    else
      Dec(E, 31);
    Result := EncodeDate(Year, 4, E);
  end;
end;

//==============================================================================
// Conversion
//==============================================================================

function DateTimeToLocalDateTime(DateTime: TDateTime): TDateTime;
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  FillChar(TimeZoneInfo, SizeOf(TimeZoneInfo), #0);
  case GetTimeZoneInformation(TimeZoneInfo) of
    TIME_ZONE_ID_STANDARD:
      Result := DateTime + (TimeZoneInfo.Bias / MinutesPerDay);
    TIME_ZONE_ID_DAYLIGHT:
      Result := DateTime - ((TimeZoneInfo.Bias + TimeZoneInfo.DaylightBias) / MinutesPerDay);
  else
    raise EJclDateTimeError.Create(RsMakeUTCTime);
  end;
end;

//------------------------------------------------------------------------------

function LocalDateTimeToDateTime(DateTime: TDateTime): TDateTime;
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  FillChar(TimeZoneInfo, SizeOf(TimeZoneInfo), #0);
  case GetTimeZoneInformation(TimeZoneInfo) of
    TIME_ZONE_ID_STANDARD:
      Result := DateTime - (TimeZoneInfo.Bias / MinutesPerDay);
    TIME_ZONE_ID_DAYLIGHT:
      Result := DateTime + ((TimeZoneInfo.Bias + TimeZoneInfo.DaylightBias) / MinutesPerDay);
  else
    raise EJclDateTimeError.Create(RsMakeUTCTime);
  end;
end;

//------------------------------------------------------------------------------

function HoursToMSecs(Hours: Integer): Integer;
begin
  Assert(Hours < MaxInt / MsecsPerHour);
  Result := Hours * MsecsPerHour;
end;

//------------------------------------------------------------------------------

function MinutesToMSecs(Minutes: Integer): Integer;
begin
  Assert(Minutes < MaxInt / MsecsPerMinute);
  Result := Minutes * MsecsPerMinute;
end;

//------------------------------------------------------------------------------

function SecondsToMSecs(Seconds: Integer): Integer;
begin
  Assert(Seconds < MaxInt / 1000);
  Result := Seconds * 1000;
end;

//------------------------------------------------------------------------------

// using system calls this can be done like this:
// var
//  SystemTime: TSystemTime;
// begin
//  ResultCheck(FileTimeToSystemTime(FileTime, SystemTime));
//  Result := SystemTimeToDateTime(SystemTime);

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
var
  F64: Int64 absolute FileTime;
begin
  Result := F64 / FileTimeStep;
  Result := Result + FileTimeBase;
end;

//------------------------------------------------------------------------------

function FileTimeToLocalDateTime(const FileTime: TFileTime): TDateTime;
var
  LocalFileTime: TFileTime;
begin
  ResultCheck(FileTimeToLocalFileTime(FileTime, LocalFileTime));
  Result := FileTimeToDateTime(LocalFileTime);            
end;

//------------------------------------------------------------------------------

function LocalDateTimeToFileTime(DateTime: TDateTime): FileTime;
var
  LocalFileTime: TFileTime;
begin
  LocalFileTime := DateTimeToFileTime(DateTime);
  ResultCheck(LocalFileTimeToFileTime(LocalFileTime, Result));
end;

//------------------------------------------------------------------------------
function DateTimeToFileTime(DateTime: TDateTime): TFileTime;
var
  E: Extended;
  F64: Int64 absolute Result;
begin
  E := (DateTime - FileTimeBase) * FileTimeStep;
  F64 := Round(E);
end;

//------------------------------------------------------------------------------

function DosDateTimeToSystemTime(const DosTime: TDosDateTime): TSystemTime;
var
  FileTime: TFileTime;
begin
  FileTime := DosDateTimeToFileTime(DosTime);
  Result := FileTimeToSystemTime(FileTime);
end;

//------------------------------------------------------------------------------

function SystemTimeToDosDateTime(const SystemTime: TSystemTime): TDosDateTime;
var
  FileTime: TFileTime;
begin
  FileTime := SystemTimeToFileTime(SystemTime);
  Result := FileTimeToDosDateTime(FileTime);
end;

//------------------------------------------------------------------------------

// DosDateTimeToDateTime performs the same action as SysUtils.FileDateToDateTime
// not using SysUtils.FileDateToDateTime this can be done like that:
// var
//  FileTime: TFileTime;
//  SystemTime: TSystemTime;
//  begin
//  ResultCheck(DosDateTimeToFileTime(HiWord(DosTime), LoWord(DosTime), FileTime));
//  ResultCheck(FileTimeToSystemTime(FileTime, SystemTime));
//  Result := SystemTimeToDateTime(SystemTime);

function DosDateTimeToDateTime(const DosTime: TDosDateTime): TDateTime;
begin
  Result := SysUtils.FileDateToDateTime(DosTime);
end;

//------------------------------------------------------------------------------

// DateTimeToDosDateTime performs the same action as SysUtils.DateTimeToFileDate
// not using SysUtils.DateTimeToDosDateTime this can be done like that:
// var
//  SystemTime: TSystemTime;
//  FileTime: TFileTime;
//  Date, Time: Word;
// begin
//  DateTimeToSystemTime(DateTime, SystemTime);
//  ResultCheck(SystemTimeToFileTime(SystemTime, FileTime));
//  ResultCheck(FileTimeToDosDateTime(FileTime, Date, Time));
//  Result := (Date shl 16) or Time;

function DateTimeToDosDateTime(const DateTime: TDateTime): TDosDateTime;
begin
  Result := SysUtils.DateTimeToFileDate(DateTime);
end;

//------------------------------------------------------------------------------

function FileTimeToSystemTime(const FileTime: TFileTime): TSystemTime;
begin
  ResultCheck(Windows.FileTimeToSystemTime(FileTime, Result));
end;

//------------------------------------------------------------------------------

function SystemTimeToFileTime(const SystemTime: TSystemTime): TFileTime;
begin
  ResultCheck(Windows.SystemTimeToFileTime(SystemTime, Result));
end;

//------------------------------------------------------------------------------

function DateTimeToSystemTime(DateTime: TDateTime): TSystemTime;
begin
   SysUtils.DateTimeToSystemTime(DateTime, Result);
end;

//------------------------------------------------------------------------------

function DosDateTimeToFileTime(DosTime: TDosDateTime): TFileTime; overload;
begin
  ResultCheck(Windows.DosDateTimeToFileTime(HiWord(DosTime), LoWord(DosTime), Result));
end;

//------------------------------------------------------------------------------

function FileTimeToDosDateTime(const FileTime: TFileTime): TDosDateTime; overload;
var
  Date, Time: Word;
begin
  ResultCheck(Windows.FileTimeToDosDateTime(FileTime, Date, Time));
  Result := (Date shl 16) or Time;
end;

//------------------------------------------------------------------------------


function FileTimeToStr(const FileTime: TFileTime): string;
var
  DateTime: TDateTime;
begin
  DateTime := FileTimeToDateTime(FileTime);
  Result := DateTimeToStr(DateTime);
end;

//------------------------------------------------------------------------------

function DosDateTimeToStr(DateTime: Integer): string;
begin
  Result := DateTimeToStr(DosDateTimeToDateTime(DateTime));
end;

//------------------------------------------------------------------------------

// we can't do this better without copying Borland-owned code from the Delphi VCL,
// as the straight forward conversion doing exactly this task is hidden
// deeply inside SysUtils.pas.
// So the date is converted forth and back to/from Julian date
// If someone needs a faster version please take a look at SysUtils.pas->DateTimeToStr.

function SystemTimeToStr(const SystemTime: TSystemTime): string;
begin
  Result := DateTimeToStr(SystemTimeToDateTime(SystemTime));
end;

//------------------------------------------------------------------------------

function CreationDateTimeOfFile(const sr: TSearchRec): TDateTime;
begin
  Result := FileTimeToDateTime(sr.FindData.ftCreationTime);
end;

//------------------------------------------------------------------------------

function LastAccessDateTimeOfFile(const sr: TSearchRec): TDateTime;
begin
  Result := FileTimeToDateTime(sr.FindData.ftLastAccessTime);
end;

//------------------------------------------------------------------------------

function LastWriteDateTimeOfFile(const sr: TSearchRec): TDateTime;
begin
  Result := FileTimeToDateTime(sr.FindData.ftLastWriteTime);
end;


end.


