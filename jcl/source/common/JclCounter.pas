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
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains a high performance counter class which can be used for highly accurate timing }
{                                                                                                  }
{ Unit owner: Marcel van Brakel                                                                    }
{ Last modified: November 25, 2001                                                                 }
{                                                                                                  }
{**************************************************************************************************}

unit JclCounter;

{$I jcl.inc}

{$WEAKPACKAGEUNIT ON}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Libc,
  {$ENDIF UNIX}
  JclBase;

type
  TJclCounter = class (TObject)
  private
    FCounting: Boolean;
    FElapsedTime: Float;
    FOverhead: Int64;
    FOverallElapsedTime: Float;
    FFrequency: Int64;
    FStart: Int64;
    FStop: Int64;

    {$IFDEF UNIX}
    FTimeval: TTimeval;
    {$ENDIF}

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
  EJclCounterError = class (EJclError);

implementation

uses
  SysUtils,
  JclResources, JclSysUtils;

//--------------------------------------------------------------------------------------------------

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
    raise EJclCounterError.CreateResRec(@RsNoCounter);
  {$ENDIF}

  {$IFDEF UNIX}
  FFrequency := 100000;  // 1 sec = 10E6 microseconds, therefor we have to devide by 10E5
  {$ENDIF}

  FCounting := False;
  FOverhead := 0;

  if Compensate then
  begin
    // Determine overhead associated with calling of the Start and Stop methods.
    // This allows the Stop method to compensate for it and return a more
    // accurate result. Thanks to John O'Harrow (john@elmcrest.demon.co.uk)

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

//--------------------------------------------------------------------------------------------------

procedure TJclCounter.Start;
begin
  FCounting := True;
  FElapsedTime := 0;
  FOverallElapsedTime := 0;

  {$IFDEF MSWINDOWS}
   QueryPerformanceCounter(FStart);
  {$ENDIF}

  {$IFDEF UNIX}
   GetTimeOfDay(FTimeval, nil);
   FStart := FTimeval.tv_sec * 100000 + (FTimeval.tv_usec);
  {$ENDIF}
end;

//--------------------------------------------------------------------------------------------------

function TJclCounter.Stop: Float;
begin
  {$IFDEF MSWINDOWS}
  QueryPerformanceCounter(FStop);
  {$ENDIF}

  {$IFDEF UNIX}
  GetTimeOfDay(FTimeval, nil);
  FStop := FTimeval.tv_sec * 100000 + (FTimeval.tv_usec);
  {$ENDIF}

  FCounting := False;

  FElapsedTime := FOverallElapsedTime + ((FStop - FStart - FOverhead) / FFrequency);
  FOverallElapsedTime := FElapsedTime;
  Result := FElapsedTime;
end;

//--------------------------------------------------------------------------------------------------

function TJclCounter.GetRunElapsedTime: Float;
var
  TimeNow: Int64;

begin
  {$IFDEF MSWINDOWS}
  QueryPerformanceCounter(TimeNow);
  {$ENDIF}

  {$IFDEF UNIX}
  GetTimeOfDay(FTimeval, nil);
  TimeNow := FTimeval.tv_sec * 100000 + (FTimeval.tv_usec);
  {$ENDIF}

  Result := FOverallElapsedTime + ((TimeNow - FStart - FOverhead) / FFrequency);
end;

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

procedure StartCount(var Counter: TJclCounter; const Compensate: Boolean = False);
begin
  Counter := TJclCounter.Create(Compensate);
  Counter.Start;
end;

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

procedure ContinueCount(var Counter: TJclCounter);
begin
  if Counter <> nil then
    Counter.Continue;
end;

end.
