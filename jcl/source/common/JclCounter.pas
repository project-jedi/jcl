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
{ The Original Code is JclCounter.pas.                                                             }
{                                                                                                  }
{ The Initial Developers of the Original Code are Theo Bebekis and Marcel van Brakel.              }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel. All Rights Reserved.  }
{ Portions created by Theo Bebekis are Copyright (C) Theo Bebekis. All Rights Reserved.            }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Theo Bebekis                                                                                   }
{   Marcel van Brakel                                                                              }
{   Robert Marquardt (marquardt)                                                                   }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains a high performance counter class which can be used for highly accurate timing }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclCounter;

{$I jcl.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  JclBase;

type
  TJclCounter = class(TObject)
  private
    FCounting: Boolean;
    FElapsedTime: Float;
    FOverhead: Int64;
    FOverallElapsedTime: Float;
    FFrequency: Int64;
    FStart: Int64;
    FStop: Int64;
    {$IFDEF LINUX}
    FTimeval: TTimeval;
    {$ENDIF LINUX}
  protected
    function GetRunElapsedTime: Float;
  public
    constructor Create(const Compensate: Boolean = False);
    procedure Continue;
    procedure Start;
    function Stop: Float;
    property Counting: Boolean read FCounting;
    property ElapsedTime: Float read FElapsedTime;
    property Overhead: Int64 read FOverhead;
    property RunElapsedTime: Float read GetRunElapsedTime;
  end;

procedure ContinueCount(var Counter: TJclCounter);
procedure StartCount(var Counter: TJclCounter; const Compensate: Boolean = False);
function StopCount(var Counter: TJclCounter): Float;

type
  EJclCounterError = class(EJclError);

implementation

uses
  SysUtils,
  JclResources;

constructor TJclCounter.Create(const Compensate: Boolean);
const
  Iterations: Integer = 10000;
var
  Count: Integer;
  TmpOverhead: Int64;
begin
  inherited Create;

  {$IFDEF MSWINDOWS}
  if not QueryPerformanceFrequency(FFrequency) then
    raise EJclCounterError.CreateRes(@RsNoCounter);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  FFrequency := 100000;  // 1 sec = 10E6 microseconds, therefore we have to divide by 10E5
  {$ENDIF LINUX}

  FCounting := False;
  FOverhead := 0;

  if Compensate then
  begin
    // Determine overhead associated with calling of the Start and Stop methods.
    // This allows the Stop method to compensate for it and return a more
    // accurate result. Thanks to John O'Harrow (john att elmcrest dott demon dott co dott uk)
    TmpOverhead := 0;
    for Count := 0 to Iterations do
    begin
      Start;
      Stop;
      TmpOverhead := TmpOverhead + (FStop - FStart);
    end;
    FOverHead := Round(TmpOverhead / Iterations);
  end;

  FOverallElapsedTime := 0;
  FElapsedTime := 0;
end;

procedure TJclCounter.Start;
begin
  FCounting := True;
  FElapsedTime := 0;
  FOverallElapsedTime := 0;
  {$IFDEF MSWINDOWS}
  if not QueryPerformanceCounter(FStart) then  
    raise EJclCounterError.CreateRes(@RsNoCounter);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  GetTimeOfDay(FTimeval, nil);
  FStart := FTimeval.tv_sec * 100000 + (FTimeval.tv_usec);
  {$ENDIF LINUX}
end;

function TJclCounter.Stop: Float;
begin
  {$IFDEF MSWINDOWS}
  if not QueryPerformanceCounter(FStop) then
    raise EJclCounterError.CreateRes(@RsNoCounter);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  GetTimeOfDay(FTimeval, nil);
  FStop := FTimeval.tv_sec * 100000 + (FTimeval.tv_usec);
  {$ENDIF LINUX}
  FCounting := False;
  FElapsedTime := FOverallElapsedTime + ((FStop - FStart - FOverhead) / FFrequency);
  FOverallElapsedTime := FElapsedTime;
  Result := FElapsedTime;
end;

function TJclCounter.GetRunElapsedTime: Float;
var
  TimeNow: Int64;
begin
  {$IFDEF MSWINDOWS}
  if not QueryPerformanceCounter(TimeNow) then
    raise EJclCounterError.CreateRes(@RsNoCounter);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  GetTimeOfDay(FTimeval, nil);
  TimeNow := FTimeval.tv_sec * 100000 + (FTimeval.tv_usec);
  {$ENDIF LINUX}
  Result := FOverallElapsedTime + ((TimeNow - FStart - FOverhead) / FFrequency);
end;

procedure TJclCounter.Continue;
var
  Overall: Float;
begin
   if not(FCounting) then
   begin
     Overall := FOverallElapsedTime;
     Start;
     FOverallElapsedTime := Overall;
   end;
end;

procedure StartCount(var Counter: TJclCounter; const Compensate: Boolean = False);
begin
  Counter := TJclCounter.Create(Compensate);
  Counter.Start;
end;

function StopCount(var Counter: TJclCounter): Float;
begin
  if Counter <> nil then
  begin
    Result := Counter.Stop;
    FreeAndNil(Counter);
  end
  else
    Result := 0.0;
end;

procedure ContinueCount(var Counter: TJclCounter);
begin
  if Counter <> nil then
    Counter.Continue;
end;

// History:

// $Log$
// Revision 1.12  2005/03/08 08:33:15  marquardt
// overhaul of exceptions and resourcestrings, minor style cleaning
//
// Revision 1.11  2005/02/24 16:34:39  marquardt
// remove divider lines, add section lines (unfinished)
//
// Revision 1.10  2004/10/17 20:25:21  mthoma
// style cleaning, adjusting contributors
//
// Revision 1.9  2004/07/28 18:00:49  marquardt
// various style cleanings, some minor fixes
//
// Revision 1.8  2004/06/14 13:05:16  marquardt
// style cleaning ENDIF, Tabs
//
// Revision 1.7  2004/06/14 11:05:50  marquardt
// symbols added to all ENDIFs and some other minor style changes like removing IFOPT
//
// Revision 1.6  2004/05/08 08:44:17  rrossmair
// introduced & applied symbol HAS_UNIT_LIBC
//
// Revision 1.5  2004/05/05 00:04:10  mthoma
// Updated headers: Added donors as contributors, adjusted the initial authors, added cvs names when they were not obvious. Changed $data to $date where necessary,
//
// Revision 1.4  2004/04/06 04:53:18
// adapt compiler conditions, add log entry
//

end.
