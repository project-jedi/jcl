{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JclCounter.pas.                                         }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ This unit contains a high performance counter class which can be used for    }
{ highly accurate timing.                                                      }
{                                                                              }
{ Unit owner: Marcel van Brakel                                                }
{ Last modified: February 06, 2001                                             }
{                                                                              }
{******************************************************************************}

unit JclCounter;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF WIN32}
  JclBase;

type
  TJclCounter = class (TObject)
  private
    FCounting: Boolean;
    FElapsedTime: Float;
    FFrequency: Int64;
    FStart: Int64;
    FStop: Int64;
    FOverhead: Int64;
  public
    constructor Create(Compensate: Boolean {$IFDEF SUPPORTS_DEFAULTPARAMS} = False {$ENDIF});
    procedure Start;
    function Stop: Float;
    property Counting: Boolean read FCounting;
    property ElapsedTime: Float read FElapsedTime;
  end;

procedure StartCount(var Counter: TJclCounter);
function StopCount(var Counter: TJclCounter): Float;

type
  EJclCounterError = class (EJclError);

implementation

uses
  SysUtils,
  JclResources, JclSysUtils;

//------------------------------------------------------------------------------
constructor TJclCounter.Create(Compensate: Boolean);
begin
  inherited Create;
  if not QueryPerformanceFrequency(FFrequency) then
    raise EJclCounterError.CreateResRec(@RsNoCounter);
  FCounting := False;
  FOverhead := 0;
  if Compensate then
  begin
    // Determine overhead associated with calling of the Start and Stop methods.
    // This allows the Stop method to compensate for it and return a more
    // accurate result. Thanks to John O'Harrow (john@elmcrest.demon.co.uk)
    Start;
    Stop;
    FOverhead := FStop - FStart;
  end;
  FElapsedTime := 0;
end;

//------------------------------------------------------------------------------

procedure TJclCounter.Start;
begin
  FCounting := True;
  FElapsedTime := 0;
  QueryPerformanceCounter(FStart);
end;

//------------------------------------------------------------------------------

function TJclCounter.Stop: Float;
begin
  QueryPerformanceCounter(FStop);
  FCounting := False;
  FElapsedTime := (FStop - FStart - FOverhead) / FFrequency;
  Result := FElapsedTime;
end;

//------------------------------------------------------------------------------

procedure StartCount(var Counter: TJclCounter);
begin
  Counter := TJclCounter.Create;
  Counter.Start;
end;

//------------------------------------------------------------------------------

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

end.
