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
{ The Original Code is JclMultimedia.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Contains a high performance timer based on the MultiMedia API and a routine to open or close the }
{ CD-ROM drive.                                                                                    }
{                                                                                                  }
{ Unit owner: Jan Jacobs                                                                           }
{ Last modified: February 14, 2002                                                                 }
{                                                                                                  }
{**************************************************************************************************}

unit JclMultimedia;

{$I jcl.inc}

{$WEAKPACKAGEUNIT ON}

interface

uses
  Windows, Classes, MmSystem,
  JclBase, JclSynch;

type

//--------------------------------------------------------------------------------------------------
// Multimedia timer
//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------
// MCI Error checking
//--------------------------------------------------------------------------------------------------

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

function MMCheck(const MciError: MCIERROR; const Msg: string = ''): MCIERROR;
function GetMciErrorMessage(const MciErrNo: MCIERROR): string;

//--------------------------------------------------------------------------------------------------
// CD Drive MCI Routines
//--------------------------------------------------------------------------------------------------

function OpenCdMciDevice(var OpenParams: TMCI_Open_Parms; Drive: Char = #0): MCIERROR;
function CloseCdMciDevice(var OpenParams: TMCI_Open_Parms): MCIERROR;

//--------------------------------------------------------------------------------------------------
// CD Drive specific routines
//--------------------------------------------------------------------------------------------------

procedure OpenCloseCdDrive(OpenMode: Boolean; Drive: Char = #0);

function IsMediaPresentInDrive(Drive: Char = #0): Boolean;

type
  TJclCdMediaInfo = (miProduct, miIdentity, miUPC);

  TJclCdTrackType = (ttAudio, ttOther);
  TJclCdTrackInfo = record
    Minute: Byte;
    Second: Byte;
    TrackType: TJclCdTrackType;
  end;
  TJclCdTrackInfoArray = array of TJclCdTrackInfo;

function GetCdInfo(InfoType: TJclCdMediaInfo; Drive: Char = #0): string;

function GetCDAudioTrackList(var TrackList: TJclCdTrackInfoArray; Drive: Char = #0): TJclCdTrackInfo; overload;
function GetCDAudioTrackList(TrackList: TStrings; IncludeTrackType: Boolean = False; Drive: Char = #0): string; overload;

implementation

uses
  SysUtils,
  JclResources, JclSysUtils;

//==================================================================================================
// TJclMultimediaTimer
//==================================================================================================

procedure MmTimerCallback(TimerId, Msg: Cardinal; User, dw1, dw2: DWORD); stdcall;
begin
  TJclMultimediaTimer(User).Timer(TimerId);
end;

//--------------------------------------------------------------------------------------------------

class function TJclMultimediaTimer.BeginPeriod(const Period: Cardinal): Boolean;
begin
  Result := timeBeginPeriod(Period) = TIMERR_NOERROR;
end;

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

destructor TJclMultimediaTimer.Destroy;
begin
  EndTimer;
  FreeAndNil(FEvent);
  FOnTimer := nil;
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

class function TJclMultimediaTimer.EndPeriod(const Period: Cardinal): Boolean;
begin
  Result := timeEndPeriod(Period) = TIMERR_NOERROR;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMultimediaTimer.EndTimer;
begin
  if (FTimerId <> 0) and (FKind = tkPeriodic) then
  begin
    timeKillEvent(FTimerId);
    timeEndPeriod(FPeriod);
    FTimerId := 0;
  end;
end;

//--------------------------------------------------------------------------------------------------

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

//--------------------------------------------------------------------------------------------------

class function TJclMultimediaTimer.GetTime: Cardinal;
begin
  Result := timeGetTime;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMultimediaTimer.SetPeriod(Value: Cardinal);
begin
  if FTimerId <> 0 then
    raise EJclMmTimerError.CreateResRec(@RsMmTimerActive);
  FPeriod := Value;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclMultimediaTimer.Timer(Id: Cardinal);
begin
  if Id <> FTimerId then
    raise EJclMmTimerError.CreateResRec(@RsMmInconsistentId);
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

//--------------------------------------------------------------------------------------------------

function TJclMultimediaTimer.WaitFor(const TimeOut: Cardinal): TJclWaitResult;
begin
  if FNotification = nkCallback then
    Result := wrError
  else
    Result := FEvent.WaitFor(TimeOut);
end;

//==================================================================================================
// EJclMciError
//==================================================================================================

constructor EJclMciError.Create(MciErrNo: MCIERROR; const Msg: string);
begin
  FMciErrorNo := MciErrNo;
  FMciErrorMsg := GetMciErrorMessage(MciErrNo);
  inherited Create(Msg + #13 + RsMmMciErrorPrefix + FMciErrorMsg);
end;

//--------------------------------------------------------------------------------------------------

constructor EJclMciError.CreateFmt(MciErrNo: MCIERROR; const Msg: string;
  const Args: array of const);
begin
  FMciErrorNo := MciErrNo;
  FMciErrorMsg := GetMciErrorMessage(MciErrNo);
  inherited CreateFmt(Msg + #13 + RsMmMciErrorPrefix + FMciErrorMsg, Args);
end;

//--------------------------------------------------------------------------------------------------

constructor EJclMciError.CreateRes(MciErrNo: MCIERROR; Ident: Integer);
begin
  FMciErrorNo := MciErrNo;
  FMciErrorMsg := GetMciErrorMessage(MciErrNo);
  inherited Create(LoadStr(Ident)+ #13 + RsMmMciErrorPrefix + FMciErrorMsg);
end;

//--------------------------------------------------------------------------------------------------

function GetMciErrorMessage(const MciErrNo: MCIERROR): string;
var
  Buffer: array [0..MMSystem.MAXERRORLENGTH - 1] of Char;
begin
  if mciGetErrorString(MciErrNo, Buffer, SizeOf(Buffer)) then
    Result := Buffer
  else
    Result := Format(RsMmUnknownError, [MciErrNo]);
end;

//--------------------------------------------------------------------------------------------------

function MMCheck(const MciError: MCIERROR; const Msg: string): MCIERROR;
begin
  if MciError <> MMSYSERR_NOERROR then
    raise EJclMciError.Create(MciError, Msg);
  Result := MciError;
end;

//==================================================================================================
// CD Drive MCI Routines
//==================================================================================================

function OpenCdMciDevice(var OpenParams: TMCI_Open_Parms; Drive: Char): MCIERROR;
var
  OpenParam: DWORD;
  DriveName: array[0..2] of Char;
begin
  FillChar(OpenParams, SizeOf(OpenParams), 0);
  OpenParam := MCI_OPEN_TYPE or MCI_OPEN_TYPE_ID or MCI_OPEN_SHAREABLE;
  OpenParams.lpstrDeviceType := PChar(MCI_DEVTYPE_CD_AUDIO);
  if Drive <> #0 then
  begin
    OpenParams.lpstrElementName := StrFmt(DriveName, '%s:', [UpCase(Drive)]);
    Inc(OpenParam, MCI_OPEN_ELEMENT);
  end;
  Result := mciSendCommand(0, MCI_OPEN, OpenParam, Cardinal(@OpenParams));
end;

//--------------------------------------------------------------------------------------------------

function CloseCdMciDevice(var OpenParams: TMCI_Open_Parms): MCIERROR;
begin
  Result := mciSendCommand(OpenParams.wDeviceID, MCI_CLOSE, MCI_WAIT, 0);
  if Result = MMSYSERR_NOERROR then
    FillChar(OpenParams, SizeOf(OpenParams), 0);
end;

//==================================================================================================
// CD Drive specific routines
//==================================================================================================

procedure OpenCloseCdDrive(OpenMode: Boolean; Drive: Char);
const
  OpenCmd: array [Boolean] of DWORD =
    (MCI_SET_DOOR_CLOSED, MCI_SET_DOOR_OPEN);
var
  Mci: TMCI_Open_Parms;
begin
  MMCheck(OpenCdMciDevice(Mci, Drive), RsMmNoCdAudio);
  try
    MMCheck(mciSendCommand(Mci.wDeviceID, MCI_SET, OpenCmd[OpenMode], 0));
  finally
    CloseCdMciDevice(Mci);
  end;
end;

//--------------------------------------------------------------------------------------------------

function IsMediaPresentInDrive(Drive: Char): Boolean;
var
  Mci: TMCI_Open_Parms;
  StatusParams: TMCI_Status_Parms;
begin
  MMCheck(OpenCdMciDevice(Mci, Drive), RsMmNoCdAudio);
  try
    FillChar(StatusParams, SizeOf(StatusParams), 0);
    StatusParams.dwItem := MCI_STATUS_MEDIA_PRESENT;
    MMCheck(mciSendCommand(Mci.wDeviceID, MCI_STATUS, MCI_STATUS_ITEM or MCI_WAIT, Cardinal(@StatusParams)));
    Result := Boolean(StatusParams.dwReturn);
  finally
    CloseCdMciDevice(Mci);
  end;
end;

//--------------------------------------------------------------------------------------------------

function GetCdInfo(InfoType: TJclCdMediaInfo; Drive: Char): string;
const
  InfoConsts: array [TJclCdMediaInfo] of DWORD =
    (MCI_INFO_PRODUCT, MCI_INFO_MEDIA_IDENTITY, MCI_INFO_MEDIA_UPC);
var
  Mci: TMCI_Open_Parms;
  InfoParams: TMCI_Info_Parms;
  Buffer: array[0..255] of Char;
begin
  Result := '';
  MMCheck(OpenCdMciDevice(Mci, Drive), RsMmNoCdAudio);
  try
    InfoParams.dwCallback := 0;
    InfoParams.lpstrReturn := Buffer;
    InfoParams.dwRetSize := SizeOf(Buffer) - 1;
    if mciSendCommand(Mci.wDeviceID, MCI_INFO, InfoConsts[InfoType], Cardinal(@InfoParams)) = MMSYSERR_NOERROR then
      Result := Buffer;
  finally
    CloseCdMciDevice(Mci);
  end;
end;

//--------------------------------------------------------------------------------------------------

function GetCDAudioTrackList(var TrackList: TJclCdTrackInfoArray; Drive: Char): TJclCdTrackInfo;
var
  Mci: TMCI_Open_Parms;
  SetParams: TMCI_Set_Parms;
  TrackCnt, Ret: Cardinal;
  I: Integer;

  function GetTrackInfo(Command, Item, Track: DWORD): DWORD;
  var
    StatusParams: TMCI_Status_Parms;
  begin
    FillChar(StatusParams, SizeOf(StatusParams), 0);
    StatusParams.dwItem := Item;
    StatusParams.dwTrack := Track;
    if mciSendCommand(Mci.wDeviceID, MCI_STATUS, Command, Cardinal(@StatusParams)) = MMSYSERR_NOERROR then
      Result := StatusParams.dwReturn
    else
      Result := 0;
  end;

begin
  MMCheck(OpenCdMciDevice(Mci, Drive), RsMmNoCdAudio);
  try
    FillChar(SetParams, SizeOf(SetParams), 0);
    SetParams.dwTimeFormat := MCI_FORMAT_MSF;
    MMCheck(mciSendCommand(Mci.wDeviceID, MCI_SET, MCI_SET_TIME_FORMAT, Cardinal(@SetParams)));
    Result.TrackType := ttOther;
    TrackCnt := GetTrackInfo(MCI_STATUS_ITEM, MCI_STATUS_NUMBER_OF_TRACKS, 0);
    SetLength(TrackList, TrackCnt);
    for I := 0 to TrackCnt - 1 do
    begin
      Ret := GetTrackInfo(MCI_STATUS_ITEM or MCI_TRACK, MCI_STATUS_LENGTH, I + 1);
      TrackList[I].Minute := mci_MSF_Minute(Ret);
      TrackList[I].Second := mci_MSF_Second(Ret);
      Ret := GetTrackInfo(MCI_STATUS_ITEM or MCI_TRACK, MCI_CDA_STATUS_TYPE_TRACK, I + 1);
      if Ret = MCI_CDA_TRACK_AUDIO then
      begin
        Result.TrackType := ttAudio;
        TrackList[I].TrackType := ttAudio;
      end  
      else
        TrackList[I].TrackType := ttOther;
    end;
    Ret := GetTrackInfo(MCI_STATUS_ITEM, MCI_STATUS_LENGTH, 0);
    Result.Minute := mci_MSF_Minute(Ret);
    Result.Second := mci_MSF_Second(Ret);
  finally
    CloseCdMciDevice(Mci);
  end;
end;

//--------------------------------------------------------------------------------------------------

function GetCDAudioTrackList(TrackList: TStrings; IncludeTrackType: Boolean; Drive: Char): string;
var
  Tracks: TJclCdTrackInfoArray;
  TotalTime: TJclCdTrackInfo;
  I: Integer;
  S: string;
begin
  TotalTime := GetCDAudioTrackList(Tracks, Drive);
  for I := Low(Tracks) to High(Tracks) do
    with Tracks[I] do
    begin
      if IncludeTrackType then
      begin
        case TrackType of
          ttAudio:
            S := RsMMTrackAudio;
          ttOther:
            S := RsMMTrackOther;
        end;
        S := Format('[%s]', [S]); 
      end
      else
        S := '';
      S := Format(RsMmCdTrackNo, [I + 1]) + ' ' + S;
      S := S + ' ' + Format(RsMMCdTimeFormat, [I + 1, Minute, Second]);
      TrackList.Add(S);
    end;  
  Result := Format(RsMMCdTimeFormat, [TotalTime.Minute, TotalTime.Second]);
end;

//--------------------------------------------------------------------------------------------------

end.
