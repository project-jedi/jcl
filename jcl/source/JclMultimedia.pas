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
{ The Original Code is JclMultimedia.pas.                                      }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ Contains a high performance timer based on the MultiMedia API and a routine  }
{ to open or close the CD-ROM drive.                                           }
{                                                                              }
{ Unit owner: Jan Jacobs                                                       }
{ Last modified: January 30, 2001                                              }
{                                                                              }
{******************************************************************************}

unit JclMultimedia;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  Windows, Classes, MmSystem,
  JclBase, JclSynch;

type
  TMmTimerKind = (tkOneShot, tkPeriodic);
  TMmNotificationKind = (nkCallback, nkSetEvent, nkPulseEvent);

  TJclMultimediaTimer = class (TObject)
  private
    FEvent: TJclEvent;
    FKind: TMmTimerKind;
    FNotification: TMmNotificationKind;
    FOnTimer: TNotifyEvent;
    FPeriod: Cardinal;
    FStartTime: Cardinal;
    FTimeCaps: TTimeCaps;
    FTimerId: Cardinal;
    function GetMinMaxPeriod(Index: Integer): Cardinal;
    procedure SetPeriod(Value: Cardinal);
  protected
    procedure Timer(Id: Cardinal); virtual;
  public
    constructor Create(Kind: TMmTimerKind; Notification: TMmNotificationKind);
    destructor Destroy; override;
    class function GetTime: Cardinal;
    class function BeginPeriod(const Period: Cardinal): Boolean; // TODO DOC
    class function EndPeriod(const Period: Cardinal): Boolean;   // TODO DOC
    procedure BeginTimer(const Delay, Resolution: Cardinal);
    procedure EndTimer;
    function Elapsed(const Update: Boolean): Cardinal;
    function WaitFor(const TimeOut: Cardinal): TJclWaitResult;
    property Event: TJclEvent read FEvent;
    property Kind: TMmTimerKind read FKind;
    property MaxPeriod: Cardinal index 0 read GetMinMaxPeriod;
    property MinPeriod: Cardinal index 1 read GetMinMaxPeriod;
    property Notification: TMmNotificationKind read FNotification;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    property Period: Cardinal read FPeriod write SetPeriod;
  end;

type
  EJclMmTimerError = class (EJclError);

  EJclMciError = class (EJclError)
  private
    FMciErrorNo: DWORD;
    FMciErrorMsg: string;
  public
    constructor Create(MciErrNo: MCIERROR; const Msg: string);
    constructor CreateFmt(MciErrNo: MCIERROR; const Msg: string; const Args: array of const);
    constructor CreateRes(MciErrNo: MCIERROR; Ident: Integer);
    property MciErrorNo: DWORD read FMciErrorNo;
    property MciErrorMsg: string read FMciErrorMsg;
  end;

procedure OpenCloseCdDrive(const OpenMode: Boolean);
function GetMciErrorMessage(const MciErrNo: MCIERROR): string;

implementation

uses
  SysUtils,
  JclResources, JclSysUtils;

//------------------------------------------------------------------------------

procedure MmTimerCallback(TimerId, Msg: Cardinal; User, dw1, dw2: DWORD); stdcall;
begin
  TJclMultimediaTimer(User).Timer(TimerId);
end;

//------------------------------------------------------------------------------

class function TJclMultimediaTimer.BeginPeriod(const Period: Cardinal): Boolean;
begin
  Result := timeBeginPeriod(Period) = TIMERR_NOERROR;
end;

//------------------------------------------------------------------------------

procedure TJclMultimediaTimer.BeginTimer(const Delay, Resolution: Cardinal);
var
  Event: Cardinal;
  TimerCallback: TFNTimeCallBack;
begin
  if FTimerId <> 0 then
    raise EJclMmTimerError.CreateResRec(@RsMmTimerActive);
  Event := 0;
  TimerCallback := nil;
  case FKind of
    tkPeriodic:
      Event := TIME_PERIODIC;
    tkOneShot:
      Event := TIME_ONESHOT;
  end;
  case FNotification of
    nkCallback:
      begin
        Event := Event or TIME_CALLBACK_FUNCTION;
        TimerCallback := @MmTimerCallback;
      end;
    nkSetEvent:
      begin
        Event := Event or TIME_CALLBACK_EVENT_SET;
        TimerCallback := TFNTimeCallback(FEvent.Handle);
      end;
    nkPulseEvent:
      begin
        Event := Event or TIME_CALLBACK_EVENT_PULSE;
        TimerCallback := TFNTimeCallback(FEvent.Handle);
      end;
  end;
  FStartTime := GetTime;
  if timeBeginPeriod(FPeriod) = TIMERR_NOERROR then
    FTimerId := timeSetEvent(Delay, Resolution, TimerCallBack, DWORD(Self), Event);
  if FTimerId = 0 then
    raise EJclMmTimerError.CreateResRec(@RsMmSetEvent);
end;

//------------------------------------------------------------------------------

constructor TJclMultimediaTimer.Create(Kind: TMmTimerKind; Notification: TMmNotificationKind);
begin
  FKind := Kind;
  FNotification := Notification;
  FPeriod := 0;
  FTimerID := 0;
  FEvent := nil;
  FillChar(FTimeCaps, SizeOf(FTimeCaps), #0);
  if timeGetDevCaps(@FTimeCaps, SizeOf(FTimeCaps)) = TIMERR_STRUCT then
    raise EJclMmTimerError.CreateResRec(@RsMmTimerGetCaps);
  FPeriod := FTimeCaps.wPeriodMin;
  if Notification <> nkCallback then
    FEvent := TJclEvent.Create(nil, Notification = nkSetEvent, False, '');
end;

//------------------------------------------------------------------------------

destructor TJclMultimediaTimer.Destroy;
begin
  EndTimer;
  FreeAndNil(FEvent);
  FOnTimer := nil;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function TJclMultimediaTimer.Elapsed(const Update: Boolean): Cardinal;
var
  CurrentTime: Cardinal;
begin
  if FTimerId = 0 then
    Result := 0
  else
  begin
    CurrentTime := GetTime;
    if CurrentTime > FStartTime then
      Result := CurrentTime - FStartTime
    else
      Result := (High(Cardinal) - FStartTime) + CurrentTime;
    if Update then
      FStartTime := CurrentTime;
  end;
end;

//------------------------------------------------------------------------------

class function TJclMultimediaTimer.EndPeriod(const Period: Cardinal): Boolean;
begin
  Result := timeEndPeriod(Period) = TIMERR_NOERROR;
end;

//------------------------------------------------------------------------------

procedure TJclMultimediaTimer.EndTimer;
begin
  if (FTimerId <> 0) and (FKind = tkPeriodic) then
  begin
    timeKillEvent(FTimerId);
    timeEndPeriod(FPeriod);
    FTimerId := 0;
  end;
end;

//------------------------------------------------------------------------------

function TJclMultimediaTimer.GetMinMaxPeriod(Index: Integer): Cardinal;
begin
  case Index of
    0:
      Result := FTimeCaps.wPeriodMax;
    1:
      Result := FTimeCaps.wPeriodMin;
  else
    Result := 0;
  end;
end;

//------------------------------------------------------------------------------

class function TJclMultimediaTimer.GetTime: Cardinal;
begin
  Result := timeGetTime;
end;

//------------------------------------------------------------------------------

procedure TJclMultimediaTimer.SetPeriod(Value: Cardinal);
begin
  if FTimerId <> 0 then
    raise EJclMmTimerError.CreateResRec(@RsMmTimerActive);
  FPeriod := Value;
end;

//------------------------------------------------------------------------------

procedure TJclMultimediaTimer.Timer(Id: Cardinal);
begin
  if Id <> FTimerId then
    raise EJclMmTimerError.CreateResRec(@RsMmInconsistentId);
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

//------------------------------------------------------------------------------

function TJclMultimediaTimer.WaitFor(const TimeOut: Cardinal): TJclWaitResult;
begin
  if FNotification = nkCallback then
    Result := wrError
  else
    Result := FEvent.WaitFor(TimeOut);
end;

//------------------------------------------------------------------------------

// opens (OpenMode = True) or closes (OpenMode = False) the Cd-Audio-Drive
// TODO resolve problem when more than one CD-ROM is installed

procedure OpenCloseCdDrive(const OpenMode: Boolean);
var
  Mci: TMCI_Open_Parms;
  MciResult: MCIERROR;
begin
  FillChar(Mci, SizeOf(Mci), #0);
  Mci.lpstrDeviceType := 'cdaudio';
  MciResult := MciSendCommand(0, MCI_OPEN, MCI_OPEN_TYPE, Cardinal(@Mci));
  if mciResult <> 0 then
    raise EJclMciError.Create(MciResult, RsMmNoCdAudio);
  if OpenMode then
    MciSendCommand(Mci.wDeviceID, MCI_SET, MCI_SET_DOOR_OPEN, 0)
  else
    MciSendCommand(Mci.wDeviceID, MCI_SET, MCI_SET_DOOR_CLOSED, 0);
  MciSendCommand(Mci.wDeviceID, MCI_CLOSE, 0, 0);
end;

//------------------------------------------------------------------------------

function GetMciErrorMessage(const MciErrNo: MCIERROR): string;
var
  Buffer: array [0..MMSystem.MAXERRORLENGTH - 1] of Char;
begin
  if mciGetErrorString(MciErrNo, Buffer, SizeOf(Buffer)) then
    Result := Buffer
  else
    Result := Format(RsMmUnknownError, [MciErrNo]);
end;

//------------------------------------------------------------------------------

constructor EJclMciError.Create(MciErrNo: MCIERROR; const Msg: string);
begin
  FMciErrorNo := MciErrNo;
  FMciErrorMsg := GetMciErrorMessage(MciErrNo);
  inherited Create(Msg + #13 + RsMmMciErrorPrefix + FMciErrorMsg);
end;

//------------------------------------------------------------------------------

constructor EJclMciError.CreateFmt(MciErrNo: MCIERROR; const Msg: string;
  const Args: array of const);
begin
  FMciErrorNo := MciErrNo;
  FMciErrorMsg := GetMciErrorMessage(MciErrNo);
  inherited CreateFmt(Msg + #13 + RsMmMciErrorPrefix + FMciErrorMsg, Args);
end;

//------------------------------------------------------------------------------

constructor EJclMciError.CreateRes(MciErrNo: MCIERROR; Ident: Integer);
begin
  FMciErrorNo := MciErrNo;
  FMciErrorMsg := GetMciErrorMessage(MciErrNo);
  inherited Create(LoadStr(Ident)+ #13 + RsMmMciErrorPrefix + FMciErrorMsg);
end;

end.
