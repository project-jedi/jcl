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
{ The Original Code is JclDateTime.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)                       }
{ of these individuals.                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Routines for working with dates and times. Mostly conversion between the                         }
{ different formats but also some date testing routines (is leap year? etc)                        }
{                                                                                                  }
{ Unit Owner: Michael Schnell                                                                      }
{ Last modified: June 18, 2000                                                                     }
{                                                                                                  }
{**************************************************************************************************}

{**************************************************************************************************}
{                                                                                                  }
{ Modifications by MSchnell:                                                                       }
{ 000622:                                                                                          }
{                                                                                                  }
{ Name changed GetCenturyOfDate -> CenturyOfDate                                                   }
{                                                                                                  }
{ Name changed GetCenturyBaseYear -> CenturyBaseYear                                               }
{                                                                                                  }
{ function GetWeekNumber(Today: TDateTime): string;  ->                                            }
{ function ISOWeekNumber(DateTime: TDateTime; var YearOfWeekDay: Integer): Integer;                }
{                                                                                                  }
{ Added overload function IsLeapYear(Year: Integer): Boolean;                                      }
{ to avoid wrong results if the user thinks he calls SysUtils.IsLeapYear                           }
{ IsLeapYear is now using SysUtils.IsLeapYear                                                      }
{                                                                                                  }
{ Changed function DateTimeToSeconds(DateTime: TDateTime): extended; ->                            }
{ function TimeOfDateTimeToSeconds(DateTime: TDateTime): Integer;                                  }
{ now not calling DecodeTime any more                                                              }
{                                                                                                  }
{ Added function TimeOfDateTimeToMSecs(DateTime: TDateTime): Integer                               }
{                                                                                                  }
{ 000624:                                                                                          }
{ DateTimeToDosDateTime performs the same action as SysUtils.DateTimeToFileDate                    }
{  so let's have Delphi do the work here                                                           }
{ DosDateTimeToDateTime performs the same action as SysUtils.FileDateToDateTime                    }
{  so let's have Delphi do the work here                                                           }
{                                                                                                  }
{ DosDateTimeToStr does not use FileTime any more                                                  }
{                                                                                                  }
{ Added function DateTimeToFileTime                                                                }
{ Added function LocalDateTimeToFileTime                                                           }
{ Changed function  FileTimeToDateTime                                                             }
{           not using TSystemDate and avoid systemcalls                                            }
{ Changed function  FileTimeToLocalDateTime                                                        }
{           not using TSystemDate and avoid systemcalls                                            }
{                                                                                                  }
{ 000625:                                                                                          }
{ Added function SystemTimeToFileTime                                                              }
{ Added function FieTimeToSystemTime                                                               }
{ Added function Datetimetosystemtime                                                              }
{ Added function DosDateTimeToFileTime                                                             }
{ Added function FileTimeToDosDateTime                                                             }
{ Added function SystemTimeToStr                                                                   }
{                                                                                                  }
{ 000706:                                                                                          }
{ Formatted according to style rules                                                               }
{                                                                                                  }
{ 000708:                                                                                          }
{ Swapped function names CenturyOfDate and CenturyBaseYear                                         }
{ those were obviously called wrong before                                                         }
{ Attention: must be done in the Help, too                                                         }
{                                                                                                  }
{ 000716:                                                                                          }
{ Support for negative dates and Year >= 10000 added for DecodeDate and EncodeDate                 }
{                                                                                                  }
{ 000809:                                                                                          }
{ added functions                                                                                  }
{ CreationDateTimeOfFile, LastAccessDateTimeOfFile and LastWriteDateTimeOfFile                     }
{                                                                                                  }
{ 000828:                                                                                          }
{ added function MakeYear4Digit                                                                    }
{                                                                                                  }
{ 000907:                                                                                          }
{ added ISOWeekNumber with 1 and 3 parameters                                                      }
{                                                                                                  }
{ 000912:                                                                                          }
{ more elegant code for ISOWeekNumber                                                              }
{ added ISOWeekToDateTime                                                                          }
{ added overload for ISOWeekNumber with three integer parameters                                   }
{                                                                                                  }
{ 000914                                                                                           }
{ added functions DayOfTheYear and DayOfTheYearToDateTime                                          }
{                                                                                                  }
{ 000918                                                                                           }
{ added function FormatDateTime                                                                    }
{                                                                                                  }
{ 001015                                                                                           }
{ avoiding "absolute" (in locations where stated)                                                  }
{ extended functionality for MakeYear4Digit: can pass Result unchanged if appropriate              }
{ added function FATDatesEqual                                                                     }
{                                                                                                  }
{ 001019                                                                                           }
{ changed EasterSunday to the code by Marc Convents (marc.convents@progen.be)                      }
{                                                                                                  }
{ 010210                                                                                           }
{ added overload procedures for compatibility:                                                     }
{    DateTimeToSystemTime, DosDateTimeToFileTime, FileTimeToDosDateTime,                           }
{    FileTimeToSystemTime, SystemTimeToFileTime                                                    }
{                                                                                                  }
{                                                                                                  }
{ TODO:                                                                                            }
{ Help for FATDatesEqual                                                                           }
{                                                                                                  }
{                                                                                                  }
{ in Help:                                                                                         }
{  We do all conversions (but thoses provided by Delphi anyway)  between                           }
{  TDatetime, TDosDateTime, TFileTime and TSystemTime         plus                                 }
{  TDatetime, TDosDateTime, TFileTime, TSystemTime to string                                       }
{                                                                                                  }
{                                                                                                  }
{**************************************************************************************************}

unit JclDateTime;

{$I jcl.inc}

{$WEAKPACKAGEUNIT ON}

interface

uses
  Windows, SysUtils,
  JclBase, JclResources;

{ Encode / Decode functions }

function EncodeDate(const Year: Integer; Month, Day: Word): TDateTime;
procedure DecodeDate(Date: TDateTime; var Year, Month, Day: Word); overload;
procedure DecodeDate(Date: TDateTime; var Year: Integer; var Month, Day: Word); overload;
procedure DecodeDate(Date: TDateTime; var Year, Month, Day: Integer); overload;

function CenturyOfDate(const DateTime: TDateTime): Integer;
function CenturyBaseYear(const DateTime: TDateTime): Integer;
function DayOfDate(const DateTime: TDateTime): Integer;
function MonthOfDate(const DateTime: TDateTime): Integer;
function YearOfDate(const DateTime: TDateTime): Integer;
function DayOfTheYear(const DateTime: TDateTime; var Year: Integer): Integer; overload;
function DayOfTheYear(const DateTime: TDateTime): Integer; overload;
function DayOfTheYearToDateTime(const Year, Day: Integer): TDateTime;
function HourOfTime(const DateTime: TDateTime): Integer;
function MinuteOfTime(const DateTime: TDateTime): Integer;
function SecondOfTime(const DateTime: TDateTime): Integer;

{ ISO 8601 support }

function ISOWeekNumber(DateTime: TDateTime; var YearOfWeekNumber, WeekDay: Integer): Integer; overload;
function ISOWeekNumber(DateTime: TDateTime; var YearOfWeekNumber: Integer): Integer; overload;
function ISOWeekNumber(DateTime: TDateTime): Integer; overload;
function ISOWeekToDateTime(const Year, Week, Day: Integer): TDateTime;

{ Miscellanous }

function IsLeapYear(const Year: Integer): Boolean; overload;
function IsLeapYear(const DateTime: TDateTime): Boolean; overload;
function DaysInMonth(const DateTime: TDateTime): Integer;
function Make4DigitYear(Year, Pivot: Integer): Integer;
function MakeYear4Digit(Year, WindowsillYear: Integer): Integer;
function EasterSunday(const Year: Integer): TDateTime;
function FormatDateTime(Form: string; DateTime: TDateTime): string;
function FATDatesEqual(const FileTime1, FileTime2: Int64): Boolean; overload;
function FATDatesEqual(const FileTime1, FileTime2: TFileTime): Boolean; overload;

//--------------------------------------------------------------------------------------------------
// Conversion
//--------------------------------------------------------------------------------------------------

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
procedure DateTimeToSystemTime(DateTime: TDateTime; var SysTime : TSystemTime); overload;

function LocalDateTimeToFileTime(DateTime: TDateTime): FileTime;
function LocalDateTimeToDateTime(DateTime: TDateTime): TDateTime;

function DosDateTimeToDateTime(const DosTime: TDosDateTime): TDateTime;
function DosDateTimeToFileTime(DosTime: TDosDateTime): TFileTime; overload;
procedure DosDateTimeToFileTime(DTH, DTL: Word; FT: TFileTime); overload;
function DosDateTimeToSystemTime(const DosTime: TDosDateTime): TSystemTime;
function DosDateTimeToStr(DateTime: Integer): string;

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
function FileTimeToLocalDateTime(const FileTime: TFileTime): TDateTime;
function FileTimeToDosDateTime(const FileTime: TFileTime): TDosDateTime; overload;
procedure FileTimeToDosDateTime(const FileTime: TFileTime; var Date, Time: Word); overload;
function FileTimeToSystemTime(const FileTime: TFileTime): TSystemTime; overload;
procedure  FileTimeToSystemTime(const FileTime: TFileTime; var ST: TSystemTime); overload
function FileTimeToStr(const FileTime: TFileTime): string;

function SystemTimeToDosDateTime(const SystemTime: TSystemTime): TDosDateTime;
function SystemTimeToFileTime(const SystemTime: TSystemTime): TFileTime; overload;
procedure SystemTimeToFileTime(const SystemTime: TSystemTime; FTime : TFileTime); overload;
function SystemTimeToStr(const SystemTime: TSystemTime): string;

//--------------------------------------------------------------------------------------------------
// Filedates
//--------------------------------------------------------------------------------------------------

function CreationDateTimeOfFile(const Sr: TSearchRec): TDateTime;
function LastAccessDateTimeOfFile(const Sr: TSearchRec): TDateTime;
function LastWriteDateTimeOfFile(const Sr: TSearchRec): TDateTime;

type
  EJclDateTimeError = class (EJclError);

implementation

const
  DaysInMonths: array [1..12] of Integer = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

  MinutesPerDay     = 60 * 24;
  SecondsPerDay     = MinutesPerDay * 60;
  MsecsPerMinute    = 60 * 1000;
  MsecsPerHour      = 60 * MsecsPerMinute;
  DaysPerYear       = 365.2422454;          // Solar Year
  DaysPerMonth      = DaysPerYear / 12;
  DateTimeBaseDay   = -693593;              //  1/1/0001
  EncodeDateMaxYear = 9999;
  SolarDifference   = 1.7882454;            //  Difference of Juliab Calendar to Solar Calendar at 1/1/10000
  DateTimeMaxDay    = 2958466;              //  12/31/EncodeDateMaxYear + 1;
  FileTimeBase      = -109205.0;
  FileTimeStep: Extended = 24.0 * 60.0 * 60.0 * 1000.0 * 1000.0 * 10.0; // 100 nSek per Day

  // Weekday to start the week
  //   1 : Sonday
  //   2 : Monday (according to ISO 8601)
  ISOFirstWeekDay = 2;

  // minmimum number of days of the year in the first week of the year week
  //   1 : week one starts at 1/1
  //   4 : first week has at least four days (according to ISO 8601)
  //   7 : first full week
  ISOFirstWeekMinDays = 4;

//--------------------------------------------------------------------------------------------------

function EncodeDate(const Year: Integer; Month, Day: Word): TDateTime; overload;
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
    Result := Trunc(Result);
    Result := Result + (Month-1) * DaysPerMonth;
    Result := Round(Result) + (Day-1);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure DecodeDate(Date: TDateTime; var Year, Month, Day: Word);
begin
  SysUtils.DecodeDate(Date, Year, Month, Day);
end;

//--------------------------------------------------------------------------------------------------

procedure DecodeDate(Date: TDateTime; var Year, Month, Day: Integer);
var
  WMonth, WDay: Word;
begin
  DecodeDate(Date, Year, WMonth, WDay);
  Month := Wmonth;
  Day := WDay;
end;

//--------------------------------------------------------------------------------------------------

procedure DecodeDate(Date: TDateTime; var Year: Integer; var Month, Day: Word); 
var
  WYear: Word;
  RDays, RMonths: TDateTime;
begin
  if (Date >= DateTimeBaseDay) and (Date < DateTimeMaxDay) then
  begin
    SysUtils.DecodeDate(Date, WYear, Month, Day);
    Year := WYear;
  end
  else
  begin
    Year := Trunc((Date - DateTimeBaseDay) / DaysPerYear);
    if Year <= 0 then
      Year := Year - 1
              // for some historical reason year 0 does not exist so we switch from
              // the last day of year -1 (-693594) to the first days of year 1
    else                                    // Year >= 10000
      Date := Date - SolarDifference;       // guarantee a smooth transition at 1/1/10000
    RDays := Date - DateTimeBaseDay;        // Days relative to 1/1/0001
    RMonths := RDays / DaysPerMonth;        // "Months" relative to 1/1/0001
    RMonths := RMonths - Year * 12.0;       // 12 "Months" per Year
    if RMonths < 0 then                     // possible truncation glitches
    begin
      RMonths := 11;
      Year := Year - 1;
    end;
    Month := Trunc(RMonths);
    Rmonths := Month;
    Month := Month + 1;
    RDays := RDays - Year * DaysPerYear;    // subtract Base Day ot the year
    RDays := RDays - RMonths * DaysPerMonth;// subtract Base Day of the month
    Day := Trunc (RDays)+ 1;
    if Year > 0 then                        // Year >= 10000
      Year := Year + 1;                     // BaseDate is 1/1/1
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure ResultCheck(Val: LongBool);
begin
  if not Val then
    raise EJclDateTimeError.Create(RsDateConversion);
end;

//--------------------------------------------------------------------------------------------------

function CenturyBaseYear(const DateTime: TDateTime): Integer;
var
  Y: Integer;
begin
  Y := YearOfDate(DateTime);
  Result := (Y div 100) * 100;
  if Y <= 0 then
    Result := Result - 100;
end;

//--------------------------------------------------------------------------------------------------

function CenturyOfDate(const DateTime: TDateTime): Integer;
var
  Y: Integer;
begin
  Y := YearOfDate(DateTime);
  if Y > 0 then
    Result := (Y div 100) + 1
  else
    Result := (Y div 100) - 1;
end;

//--------------------------------------------------------------------------------------------------

function DayOfDate(const DateTime: TDateTime): Integer;
var
  Y: Integer;
  M, D: Word;
begin
  DecodeDate(DateTime, Y, M, D);
  Result := D;
end;

//--------------------------------------------------------------------------------------------------

function MonthOfDate(const DateTime: TDateTime): Integer;
var
  Y: Integer;
  M, D: Word;
begin
  DecodeDate(DateTime, Y, M, D);
  Result := M;
end;

//--------------------------------------------------------------------------------------------------

function YearOfDate(const DateTime: TDateTime): Integer;
var
  M, D: Word;
begin
  DecodeDate(DateTime, Result, M, D);
end;

//--------------------------------------------------------------------------------------------------

function DayOfTheYear(const DateTime: TDateTime; var Year: Integer): Integer;
var
  Month, Day: Word;
  DT: TDateTime;
begin
  DecodeDate(DateTime, Year, Month, Day);
  DT := EncodeDate(Year, 1, 1);
  Result := Trunc(DateTime);
  Result := Result - Trunc(DT) + 1;
end;

//--------------------------------------------------------------------------------------------------

function DayOfTheYear(const DateTime: TDateTime): Integer;
var
  Year: Integer;
begin
  Result := DayOfTheYear(DateTime, Year);
end;

//--------------------------------------------------------------------------------------------------

function DayOfTheYearToDateTime(const Year, Day: Integer): TDateTime;
begin
  Result := EncodeDate(Year, 1, 1) + Day - 1;
end;

//--------------------------------------------------------------------------------------------------

function HourOfTime(const DateTime: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  DecodeTime(DateTime, H, M, S, MS);
  Result := H;
end;

//--------------------------------------------------------------------------------------------------

function MinuteOfTime(const DateTime: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  DecodeTime(DateTime, H, M, S, MS);
  Result := M;
end;

//--------------------------------------------------------------------------------------------------

function SecondOfTime(const DateTime: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  DecodeTime(DateTime, H, M, S, MS);
  Result := S;
end;

//--------------------------------------------------------------------------------------------------

function TimeOfDateTimeToSeconds(DateTime: TDateTime): Integer;
begin
  Result := Round(Frac(DateTime) * SecondsPerDay);
end;

//--------------------------------------------------------------------------------------------------

function TimeOfDateTimeToMSecs(DateTime: TDateTime): Integer;
begin
  Result := Round(Frac(DateTime) * MSecsPerDay);
end;

//--------------------------------------------------------------------------------------------------

function DaysInMonth(const DateTime: TDateTime): Integer;
var
  M: Integer;
begin
  M := MonthOfDate(DateTime);
  Result := DaysInMonths[M];
  if (M = 2) and IsLeapYear(DateTime) then
    Result := 29;
end;

//--------------------------------------------------------------------------------------------------

// DayOfWeek function returns integer 1..7 equivalent to Sunday..Saturday.
// ISO 8601 weeks start with Monday and the first week of a year is the one which
// includes the first Thursday

function ISOWeekNumber(DateTime: TDateTime; var YearOfWeekNumber, WeekDay: Integer): Integer;
var
  Month, Day: Word;
begin
  WeekDay := ((DayOfWeek(DateTime) - ISOFirstWeekDay + 7) mod 7) + 1;
  DateTime := DateTime - WeekDay + 8 - ISOFirstWeekMinDays;
  DecodeDate(DateTime, YearOfWeekNumber, Month, Day);
  Result := (Trunc(DateTime - EncodeDate(YearOfWeekNumber, 1, 1)) div 7) + 1;
end;

//--------------------------------------------------------------------------------------------------

function ISOWeekNumber(DateTime: TDateTime; var YearOfWeekNumber: Integer): Integer;
var
  Dummy: Integer;
begin
  Result := ISOWeekNumber(DateTime, YearOfWeekNumber, Dummy);
end;

//--------------------------------------------------------------------------------------------------

function ISOWeekNumber(DateTime: TDateTime): Integer;
var
  Dummy1, Dummy2: Integer;
begin
  Result := ISOWeekNumber(DateTime, Dummy1, Dummy2);
end;

//--------------------------------------------------------------------------------------------------

function ISOWeekToDateTime(const Year, Week, Day: Integer): TDateTime;
begin
  Result := EncodeDate(Year, 1, ISOFirstWeekMinDays);
  Result := Result + (Week - 1) * 7 - ((DayOfWeek(Result) + (7 - ISOFirstWeekDay)) mod 7) + Day - 1;
end;

//--------------------------------------------------------------------------------------------------

// The original Gregorian rule for all who want to learn it
// Result := (Year mod 4 = 0) and ((Year mod 100 <> 0) or (Year mod 400 = 0));

function IsLeapYear(const Year: Integer): Boolean;
begin
  Result := SysUtils.IsLeapYear(Year);
end;

//--------------------------------------------------------------------------------------------------

function IsLeapYear(const DateTime: TDateTime): Boolean;
begin
  Result := IsLeapYear(YearOfDate(DateTime));
end;

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

// "window" technique for years to translate 2 digits to 4 digits.
// The window is 100 years wide
// The windowsill year is the lower edge of the window
// A windowsill year of 1900 is equivalent to putting 1900 before every 2-digit year
// if WindowsillYear is 1940, then 40 is interpreted as 1940, 00 as 2000 and 39 as 2039
// The system default is 1950

function MakeYear4Digit(Year, WindowsillYear: Integer): Integer;
var
  CC, Y: Integer;
begin
  // have come across this specific problem : y2K read as year 100
  if Year = 100 then
    Year := 0;
  // turn 2 digit years to 4 digits
  Y := Year mod 100;
  CC := (WindowsillYear div 100) * 100;
  Result := Y + CC;  // give the result the same century as the windowsill
  if Result < WindowsillYear then   // cannot be lower than the windowsill
    Result := Result + 100;
  if (Year >= 100) or (Year < 0) then
    Assert(Year = Result);  // Assert: no unwanted century translation
end;

//--------------------------------------------------------------------------------------------------

// Calculates and returns Easter Day for specified year.
// Originally from Mark Lussier, AppVision <MLussier@best.com>.
// Corrected to prevent integer overflow if it is inadvertedly
// passed a year of 6554 or greater.

function EasterSunday(const Year: Integer): TDateTime;
var
  Month, Day, Moon, Epact, Sunday,
  Gold, Cent, Corx, Corz: Integer;
begin
  { The Golden Number of the year in the 19 year Metonic Cycle: }
  Gold := (Year mod 19) + 1;
  { Calculate the Century: }
  Cent := (Year div 100) + 1;
  { Number of years in which leap year was dropped in order... }
  { to keep in step with the sun: }
  Corx := (3 * Cent) div 4 - 12;
  { Special correction to syncronize Easter with moon's orbit: }
  Corz := (8 * Cent + 5) div 25 - 5;
  { Find Sunday: }
  Sunday := (Longint(5) * Year) div 4 - Corx - 10;
              { ^ To prevent overflow at year 6554}
  { Set Epact - specifies occurrence of full moon: }
  Epact := (11 * Gold + 20 + Corz - Corx) mod 30;
  if Epact < 0 then
    Epact := Epact + 30;
  if ((Epact = 25) and (Gold > 11)) or (Epact = 24) then
    Epact := Epact + 1;
  { Find Full Moon: }
  Moon := 44 - Epact;
  if Moon < 21 then
    Moon := Moon + 30;
  { Advance to Sunday: }
  Moon := Moon + 7 - ((Sunday + Moon) mod 7);
  if Moon > 31 then
  begin
    Month := 4;
    Day := Moon - 31;
  end
  else
  begin
    Month := 3;
    Day := Moon;
  end;
  Result := EncodeDate(Year, Month, Day);
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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

function HoursToMSecs(Hours: Integer): Integer;
begin
  Assert(Hours < MaxInt / MsecsPerHour);
  Result := Hours * MsecsPerHour;
end;

//--------------------------------------------------------------------------------------------------

function MinutesToMSecs(Minutes: Integer): Integer;
begin
  Assert(Minutes < MaxInt / MsecsPerMinute);
  Result := Minutes * MsecsPerMinute;
end;

//--------------------------------------------------------------------------------------------------

function SecondsToMSecs(Seconds: Integer): Integer;
begin
  Assert(Seconds < MaxInt / 1000);
  Result := Seconds * 1000;
end;

//--------------------------------------------------------------------------------------------------

// using system calls this can be done like this:
// var
//  SystemTime: TSystemTime;
// begin
//  ResultCheck(FileTimeToSystemTime(FileTime, SystemTime));
//  Result := SystemTimeToDateTime(SystemTime);

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
begin
  Result := Int64(FileTime) / FileTimeStep;
  Result := Result + FileTimeBase;
end;

//--------------------------------------------------------------------------------------------------

function FileTimeToLocalDateTime(const FileTime: TFileTime): TDateTime;
var
  LocalFileTime: TFileTime;
begin
  ResultCheck(FileTimeToLocalFileTime(FileTime, LocalFileTime));
  Result := FileTimeToDateTime(LocalFileTime);
end;

//--------------------------------------------------------------------------------------------------

function LocalDateTimeToFileTime(DateTime: TDateTime): FileTime;
var
  LocalFileTime: TFileTime;
begin
  LocalFileTime := DateTimeToFileTime(DateTime);
  ResultCheck(LocalFileTimeToFileTime(LocalFileTime, Result));
end;

//--------------------------------------------------------------------------------------------------

function DateTimeToFileTime(DateTime: TDateTime): TFileTime;
var
  E: Extended;
  F64: Int64;
begin
  E := (DateTime - FileTimeBase) * FileTimeStep;
  F64 := Round(E);
  Result := TFileTime(F64);
end;

//--------------------------------------------------------------------------------------------------

function DosDateTimeToSystemTime(const DosTime: TDosDateTime): TSystemTime;
var
  FileTime: TFileTime;
begin
  FileTime := DosDateTimeToFileTime(DosTime);
  Result := FileTimeToSystemTime(FileTime);
end;

//--------------------------------------------------------------------------------------------------

function SystemTimeToDosDateTime(const SystemTime: TSystemTime): TDosDateTime;
var
  FileTime: TFileTime;
begin
  FileTime := SystemTimeToFileTime(SystemTime);
  Result := FileTimeToDosDateTime(FileTime);
end;

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

function FileTimeToSystemTime(const FileTime: TFileTime): TSystemTime; overload;
begin
  ResultCheck(Windows.FileTimeToSystemTime(FileTime, Result));
end;

//--------------------------------------------------------------------------------------------------

procedure  FileTimeToSystemTime(const FileTime: TFileTime; var ST: TSystemTime); overload
begin
  Windows.FileTimeToSystemTime(FileTime, ST);
end;

//--------------------------------------------------------------------------------------------------

function SystemTimeToFileTime(const SystemTime: TSystemTime): TFileTime;  overload;
begin
  ResultCheck(Windows.SystemTimeToFileTime(SystemTime, Result));
end;

//--------------------------------------------------------------------------------------------------

procedure SystemTimeToFileTime(const SystemTime: TSystemTime; FTime: TFileTime); overload;
begin
  Windows.SystemTimeToFileTime(SystemTime, FTime);
end;

//--------------------------------------------------------------------------------------------------

function DateTimeToSystemTime(DateTime: TDateTime): TSystemTime;  overload;
begin
  SysUtils.DateTimeToSystemTime(DateTime, Result);
end;

//--------------------------------------------------------------------------------------------------

procedure DateTimeToSystemTime(DateTime: TDateTime; var SysTime : TSystemTime); overload;
begin
  SysUtils.DateTimeToSystemTime(DateTime, SysTime);
end;

//--------------------------------------------------------------------------------------------------

function DosDateTimeToFileTime(DosTime: TDosDateTime): TFileTime; overload;
begin
  ResultCheck(Windows.DosDateTimeToFileTime(HiWord(DosTime), LoWord(DosTime), Result));
end;

//--------------------------------------------------------------------------------------------------

procedure DosDateTimeToFileTime(DTH, DTL: Word; FT: TFileTime); overload;
begin
  Windows.DosDateTimeToFileTime(DTH, DTL, FT);
end;

//--------------------------------------------------------------------------------------------------

function FileTimeToDosDateTime(const FileTime: TFileTime): TDosDateTime; overload;
var
  Date, Time: Word;
begin
  ResultCheck(Windows.FileTimeToDosDateTime(FileTime, Date, Time));
  Result := (Date shl 16) or Time;
end;

//--------------------------------------------------------------------------------------------------

procedure FileTimeToDosDateTime(const FileTime: TFileTime; var Date, Time: Word); overload;
begin
  Windows.FileTimeToDosDateTime(FileTime, Date, Time);
end;

//--------------------------------------------------------------------------------------------------

function FileTimeToStr(const FileTime: TFileTime): string;
var
  DateTime: TDateTime;
begin
  DateTime := FileTimeToDateTime(FileTime);
  Result := DateTimeToStr(DateTime);
end;

//--------------------------------------------------------------------------------------------------

function DosDateTimeToStr(DateTime: Integer): string;
begin
  Result := DateTimeToStr(DosDateTimeToDateTime(DateTime));
end;

//--------------------------------------------------------------------------------------------------

// we can't do this better without copying Borland-owned code from the Delphi VCL,
// as the straight forward conversion doing exactly this task is hidden
// deeply inside SysUtils.pas.
// So the date is converted forth and back to/from Julian date
// If someone needs a faster version please take a look at SysUtils.pas->DateTimeToStr.

function SystemTimeToStr(const SystemTime: TSystemTime): string;
begin
  Result := DateTimeToStr(SystemTimeToDateTime(SystemTime));
end;

//--------------------------------------------------------------------------------------------------

function CreationDateTimeOfFile(const Sr: TSearchRec): TDateTime;
begin
  Result := FileTimeToDateTime(Sr.FindData.ftCreationTime);
end;

//--------------------------------------------------------------------------------------------------

function LastAccessDateTimeOfFile(const Sr: TSearchRec): TDateTime;
begin
  Result := FileTimeToDateTime(Sr.FindData.ftLastAccessTime);
end;

//--------------------------------------------------------------------------------------------------

function LastWriteDateTimeOfFile(const Sr: TSearchRec): TDateTime;
begin
  Result := FileTimeToDateTime(Sr.FindData.ftLastWriteTime);
end;

//--------------------------------------------------------------------------------------------------

// Additional format tokens (also available in upper case):
// w: Week no according to ISO
// ww: Week no according to ISO forced two digits
// i: Year of the ISO-week denoted by w (4 digits for 1000..9999)
// ii: Year of the ISO-week denoted by w forced two digits
// e: Number of the Day in the ISO-week denoted by w (ISO-Notation 1=Monday...)
// f: Number of the Day in the year denoted by y
// fff: Number of the Day in the year denoted by y forced three digits

function FormatDateTime(Form: string; DateTime: TDateTime): string;
var
  N: Integer;
  ISODay, ISOWeek, ISOYear, DayOfYear, YY: Integer;

  procedure Digest;
  begin
    if N > 1 then
    begin
      Result := Result + Copy(Form, 1, N - 1);
      System.Delete(Form, 1, N - 1);
      N := 1;
    end;
  end;

begin
  ISOWeek := 0;
  DayOfYear := 0;
  Result := '';
  N := 1;
  while N <= Length(Form) do
  begin
    case Form[N] of
      '"':
        begin
          Inc(N);
          Digest;
          N := Pos('"', Form);
          if N = 0 then
          begin
            Result := Result + Form;
            Form := '';
            N := 1;
          end
          else
          begin
            Inc(N);
            Digest;
          end;
        end;
      '''':
        begin
          Inc(N);
          Digest;
          N := Pos('''', Form);
          if N = 0 then
          begin
            Result := Result + Form;
            Form := '';
            N := 1;
          end
          else
          begin
            Inc(N);
            Digest;
          end;
        end;
      'i', 'I':             //ISO Week Year
        begin
          Digest;
          if ISOWeek = 0 then
            ISOWeek := ISOWeekNumber(DateTime, ISOYear, ISoDay);
          if (Length(Form) > 1) and ((Form[2] = 'i') or (Form[2] = 'I')) then
          begin              // <ii>
            if (Length(Form) > 2) and ((Form[3] = 'i') or (Form[3] = 'I')) then
            begin
              if (Length(Form) > 3) and ((Form[4] = 'i') or (Form[4] = 'I')) then
              begin        // <iiii>
                Delete(Form, 1, 4);
                Result := Result + '"' + IntToStr(ISOYear) + '"';
              end
              else
              begin        // <iii>
                Delete(Form, 1, 3);
                Result := Result + '"' + IntToStr(ISOYear) + '"';
              end;
            end
            else
            begin           // <ii>
              Delete(Form, 1, 2);
              Result := Result + '"';
              if ISOYear < 10 then
                Result := Result + '0';
              YY := ISOYear mod 100;
              if YY < 10 then
                Result := Result + '0';
              Result := Result + IntToStr(YY) + '"';
            end;
          end
          else
          begin               // <i>
            Delete(Form, 1, 1);
            Result := Result + '"' + IntToStr(ISOYear) + '"';
          end;
        end;
      'w', 'W':              // ISO Week
        begin
          Digest;
          if ISOWeek = 0 then
            ISOWeek := ISOWeekNumber(DateTime, ISOYear, ISoDay);
          if (Length(Form) > 1) and ((Form[2] = 'w') or (Form[2] = 'W')) then
          begin               // <ww>
            Delete(Form, 1, 2);
            Result := Result + '"';
            if ISOWeek < 10 then
              Result := Result + '0';
            Result := Result + IntToStr(ISOWeek) + '"';
          end
          else
          begin               // <w>
            Delete(Form, 1, 1);
            Result := Result + '"' + IntToStr(ISOWeek) + '"';
          end;
        end;
      'e', 'E':   // ISO Week Day
        begin
          Digest;
          if ISOWeek = 0 then
            ISOWeek := ISOWeekNumber(DateTime, ISOYear, ISODay);
          Delete(Form, 1, 1);
          Result := Result + '"' + IntToStr(ISODay) + '"';
        end;
      'f', 'F':   // Day of the Year
        begin
          Digest;
          if DayOfYear = 0 then
            DayOfYear := DayOfTheYear(DateTime);
          if (Length(Form) > 1) and ((Form[2] = 'f') or (Form[2] = 'F')) then
          begin
            if (Length(Form) > 2) and ((Form[3] = 'f') or (Form[3] = 'F')) then
            begin            // <fff>
              Delete(Form, 1, 3);
              Result := Result + '"';
              if DayOfYear < 10 then
                Result := Result + '0';
              if DayOfYear < 100 then
                Result := Result + '0';
              Result := Result + IntToStr(DayOfYear) + '"';
            end
            else
            begin            // <ff>
              Delete(Form, 1, 2);
              Result := Result + '"';
              if DayOfYear < 10 then
                Result := Result + '0';
              Result := Result + IntToStr(DayOfYear) + '"';
            end;
          end
          else
          begin               // <f>
            Delete(Form, 1, 1);
            Result := Result + '"' + IntToStr(DayOfYear) + '"';
          end
        end;
    else
      begin
        Inc(N);
      end;
    end; // case
  end; // while
  Result := SysUtils.FormatDateTime(Result + Form, DateTime);
end;

//--------------------------------------------------------------------------------------------------

// FAT has a granularity of 2 seconds
// The intervals are /10 of a second

function FATDatesEqual(const FileTime1, FileTime2: Int64): Boolean;
const
  ALLOWED_FAT_FILE_TIME_VARIATION = 20;
begin
  Result := Abs(FileTime1 - FileTime2) <= ALLOWED_FAT_FILE_TIME_VARIATION;
end;

//--------------------------------------------------------------------------------------------------

function FATDatesEqual(const FileTime1, FileTime2: TFileTime): Boolean;
begin
  Result := FATDatesEqual(Int64(FileTime1), Int64(FileTime2));
end;

end.
