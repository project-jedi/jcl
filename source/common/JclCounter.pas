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
{ Last modified: June 18, 2000                                                 }
{                                                                              }
{******************************************************************************}

unit JclCounter;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  JclBase;

type
  TJclCounter = class (TObject)
  private
    FFrequency: Int64;
    FStart: Int64;
    FStop: Int64;
    FCounting: Boolean;
    FElapsedTime: Float;
  public
    constructor Create;
    procedure Start;
    function Stop: Float;
    property ElapsedTime: Float read FElapsedTime;
    property Counting: Boolean read FCounting;
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

constructor TJclCounter.Create;
begin
  inherited Create;
  if not QueryPerformanceFrequency(FFrequency) then
    raise EJclCounterError.CreateResRec(@RsNoCounter);
  FCounting := False;
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
  // FElapsedTime := (FStop.QuadPart - FStart.QuadPart) / FFrequency.QuadPart;
  FElapsedTime := (FStop - FStart) / FFrequency;
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
  Result := 0.0;
  if Counter <> nil then
  begin
    Result := Counter.Stop;
    FreeAndNil(TObject(Counter));
  end;
end;

end.
