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
{ The Original Code is JclSvcCtrl.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Flier Lu (<flier_lu att yahoo dott com dott cn>).  }
{ Portions created by Flier Lu are Copyright (C) Flier Lu.  All Rights Reserved.                   }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Flier Lu (flier)                                                                               }
{   Peter J. Haas (peterjhaas)                                                                     }
{   Robert Rossmair (rrossmair)                                                                    }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains routines and classes to control Microsoft task schedule service               }
{                                                                                                  }
{ Unit owner: Flier Lu                                                                             }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$
// For history see end of file

unit JclTask;

interface

{$I jcl.inc}

uses
  Windows, Messages, Classes, SysUtils, Contnrs,
  MSTask, MSTaskError,
  JclBase, JclSysUtils, JclSysInfo, JclWideStrings;

type
  TDateTimeArray = array of TDateTime;

  TJclScheduledTaskStatus = (tsUnknown, tsReady, tsRunning, tsNotScheduled, tsHasNotRun);

  TJclScheduledTaskFlag =
   (tfInteractive, tfDeleteWhenDone, tfDisabled, tfStartOnlyIfIdle,
    tfKillOnIdleEndl, tfDontStartIfOnBatteries, tfKillIfGoingOnBatteries,
    tfRunOnlyIfDocked, tfHidden, tfRunIfConnectedToInternet,
    tfRestartOnIdleResume, tfSystemRequired, tfRunOnlyIfLoggedOn);
  TJclScheduledTaskFlags = set of TJclScheduledTaskFlag;

  TJclScheduleTaskPropertyPage = (ppTask, ppSchedule, ppSettings);
  TJclScheduleTaskPropertyPages = set of TJclScheduleTaskPropertyPage;

const
  JclScheduleTaskAllPages = [ppTask, ppSchedule, ppSettings];

  LocalSystemAccount = 'SYSTEM';  // Local system account name
  InfiniteTime = 0.0;

type
  TJclScheduledTask = class;

  TJclTaskSchedule = class(TObject)
  private
    FTaskScheduler: ITaskScheduler;
    FTasks: TObjectList;
    function GetTargetComputer: WideString;
    procedure SetTargetComputer(const Value: WideString);
    function GetTask(const Idx: Integer): TJclScheduledTask;
    function GetTaskCount: Integer;
  public
    constructor Create; overload;
    constructor Create(const ComputerName: WideString); overload;
    destructor Destroy; override;
    procedure Refresh;
    function Add(const TaskName: WideString): TJclScheduledTask;
    procedure Delete(const Idx: Integer);
    function Remove(const TaskName: WideString): Integer; overload;
    function Remove(const TaskIntf: ITask): Integer; overload;
    function Remove(const ATask: TJclScheduledTask): Integer; overload;
    property TaskScheduler: ITaskScheduler read FTaskScheduler;
    property TargetComputer: WideString read GetTargetComputer write SetTargetComputer;
    property Tasks[const Idx: Integer]: TJclScheduledTask read GetTask; default;
    property TaskCount: Integer read GetTaskCount;
  public
    class function IsRunning: Boolean;
    class procedure Start;
    class procedure Stop;
  end;

  TJclTaskTrigger = class(TCollectionItem)
  private
    FTaskTrigger: ITaskTrigger;
    procedure SetTaskTrigger(const Value: ITaskTrigger);
    function GetTrigger: TTaskTrigger;
    procedure SetTrigger(const Value: TTaskTrigger);
    function GetTriggerString: WideString;
  public
    property TaskTrigger: ITaskTrigger read FTaskTrigger;
    property Trigger: TTaskTrigger read GetTrigger write SetTrigger;
    property TriggerString: WideString read GetTriggerString;
  end;

  TJclScheduledWorkItem = class;

  TJclTaskTriggers = class(TCollection)
  public
    FWorkItem: TJclScheduledWorkItem;
    function GetItem(Index: Integer): TJclTaskTrigger;
    procedure SetItem(Index: Integer; Value: TJclTaskTrigger);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AWorkItem: TJclScheduledWorkItem);
    function Add(ATrigger: ITaskTrigger): TJclTaskTrigger; overload;
    function Add: TJclTaskTrigger; overload;
    function AddItem(Item: TJclTaskTrigger; Index: Integer): TJclTaskTrigger;
    function Insert(Index: Integer): TJclTaskTrigger;
    property Items[Index: Integer]: TJclTaskTrigger read GetItem write SetItem; default;
  end;

  TJclScheduledWorkItem = class(TPersistent)
  private
    FScheduledWorkItem: IScheduledWorkItem;
    FTaskName: WideString;
    FData: TMemoryStream;
    FTriggers: TJclTaskTriggers;
    function GetAccountName: WideString;
    procedure SetAccountName(const Value: WideString);
    procedure SetPassword(const Value: WideString);
    function GetComment: WideString;
    procedure SetComment(const Value: WideString);
    function GetCreator: WideString;
    procedure SetCreator(const Value: WideString);
    function GetExitCode: DWORD;
    function GetDeadlineMinutes: Word;
    function GetIdleMinutes: Word;
    function GetMostRecentRunTime: Windows.TSystemTime;
    function GetNextRunTime: Windows.TSystemTime;
    function GetStatus: TJclScheduledTaskStatus;
    function GetErrorRetryCount: Word;
    procedure SetErrorRetryCount(const Value: Word);
    function GetErrorRetryInterval: Word;
    procedure SetErrorRetryInterval(const Value: Word);
    function GetFlags: TJclScheduledTaskFlags;
    procedure SetFlags(const Value: TJclScheduledTaskFlags);
    function GetData: TStream;                                  { TODO : stream is owned by instance }
    procedure SetData(const Value: TStream);                    { TODO : stream is owned by caller (copy) }
    function GetTrigger(const Idx: Integer): TJclTaskTrigger;
    function GetTriggerCount: Integer;
  protected
    constructor Create(const ATaskName: WideString; const AScheduledWorkItem: IScheduledWorkItem);
  public
    destructor Destroy; override;
    procedure Save;
    procedure Refresh;
    procedure Run;
    procedure Terminate;
    procedure SetAccountInformation(const Name, Password: WideString);
    function GetRunTimes(const BeginTime: TDateTime; const EndTime: TDateTime = InfiniteTime): TDateTimeArray;
    property ScheduledWorkItem: IScheduledWorkItem read FScheduledWorkItem;
    property TaskName: WideString read FTaskName write FTaskName;
    property AccountName: WideString read GetAccountName write SetAccountName;
    property Password: WideString write SetPassword;
    property Comment: WideString read GetComment write SetComment;
    property Creator: WideString read GetCreator write SetCreator;
    property ErrorRetryCount: Word read GetErrorRetryCount write SetErrorRetryCount;
    property ErrorRetryInterval: Word read GetErrorRetryInterval write SetErrorRetryInterval;
    property ExitCode: DWORD read GetExitCode;
    property OwnerData: TStream read GetData write SetData;  { TODO : wrong design, get: stream is owned by instance, set stream is owned by caller }
    property IdleMinutes: Word read GetIdleMinutes;
    property DeadlineMinutes: Word read GetDeadlineMinutes;
    property MostRecentRunTime: Windows.TSystemTime read GetMostRecentRunTime;
    property NextRunTime: Windows.TSystemTime read GetNextRunTime;
    property Status: TJclScheduledTaskStatus read GetStatus;
    property Flags: TJclScheduledTaskFlags read GetFlags write SetFlags;
    property Triggers[const Idx: Integer]: TJclTaskTrigger read GetTrigger; default;
    property TriggerCount: Integer read GetTriggerCount;
  end;

  TJclScheduledTask = class(TJclScheduledWorkItem)
  private
    function GetApplicationName: WideString;
    procedure SetApplicationName(const Value: WideString);
    function GetMaxRunTime: DWORD;
    procedure SetMaxRunTime(const Value: DWORD);
    function GetParameters: WideString;
    procedure SetParameters(const Value: WideString);
    function GetPriority: DWORD;
    procedure SetPriority(const Value: DWORD);
    function GetTaskFlags: DWORD;
    procedure SetTaskFlags(const Value: DWORD);
    function GetWorkingDirectory: WideString;
    procedure SetWorkingDirectory(const Value: WideString);
    function GetTask: ITask;
  public
    function ShowPage(Pages: TJclScheduleTaskPropertyPages = JclScheduleTaskAllPages): Boolean;
    property Task: ITask read GetTask;
    property ApplicationName: WideString read GetApplicationName write SetApplicationName;
    property WorkingDirectory: WideString read GetWorkingDirectory write SetWorkingDirectory;
    property MaxRunTime: DWORD read GetMaxRunTime write SetMaxRunTime;
    property Parameters: WideString read GetParameters write SetParameters;
    property Priority: DWORD read GetPriority write SetPriority;
    property TaskFlags: DWORD read GetTaskFlags write SetTaskFlags;
  end;

type
  TTSServiceControlFunction = (scfQueryStatus, scfStop, scfStart, scfPause,
    scfContinue, scfStartAndContinue);

{ TODO -cHelp : Author Peter J. Haas }  
// TaskSchedulerServiceControl controls the status of the task scheduler service
// Func:
//   scfQueryStatus: Get the current status of the task scheduler service.
//   scfStop: Stop the task scheduler service.
//   scfStart: Start the task scheduler service.
//   scfPause: Pause the task scheduler service.
//   scfContinue: Continue the task scheduler service.
//   scfStartAndContinue: Start or continue the task scheduler service,
//     dependent of the current status.
// Result:
//   0: Error. To get extended error information, call GetLastError.
//      Potential errors are
//        ERROR_SERVICE_DOES_NOT_EXIST: The task scheduler is not installed.
//        ERROR_INVALID_SERVICE_CONTROL: Wrong value in Func.
//   SERVICE_STOPPED: The task scheduler service is stopped.
//   SERVICE_PAUSED: The task scheduler service is started but paused.
//   SERVICE_RUNNING: The task scheduler service is started and running.
// In case of a timeout other potential return values are:
//   SERVICE_START_PENDING, SERVICE_STOP_PENDING, SERVICE_CONTINUE_PENDING and
//   SERVICE_PAUSE_PENDING
function TaskSchedulerServiceControl(Func: TTSServiceControlFunction): Integer;

implementation

uses
  ActiveX, ComObj, CommCtrl,
  {$IFDEF FPC}
  JwaWinSvc,
  {$ELSE}
  WinSvc,
  {$ENDIF FPC}
  JclSvcCtrl;

const
  TaskFlagMapping: array [TJclScheduledTaskFlag] of DWORD =
   (TASK_FLAG_INTERACTIVE, TASK_FLAG_DELETE_WHEN_DONE, TASK_FLAG_DISABLED,
    TASK_FLAG_START_ONLY_IF_IDLE, TASK_FLAG_KILL_ON_IDLE_END,
    TASK_FLAG_DONT_START_IF_ON_BATTERIES, TASK_FLAG_KILL_IF_GOING_ON_BATTERIES,
    TASK_FLAG_RUN_ONLY_IF_DOCKED, TASK_FLAG_HIDDEN,
    TASK_FLAG_RUN_IF_CONNECTED_TO_INTERNET, TASK_FLAG_RESTART_ON_IDLE_RESUME,
    TASK_FLAG_SYSTEM_REQUIRED, TASK_FLAG_RUN_ONLY_IF_LOGGED_ON);

//==================================================================================================
// TJclTaskSchedule
//==================================================================================================

constructor TJclTaskSchedule.Create;
begin
  FTaskScheduler := CreateComObject(CLSID_CTaskScheduler) as ITaskScheduler;
  FTasks := TObjectList.Create;
end;

//--------------------------------------------------------------------------------------------------

constructor TJclTaskSchedule.Create(const ComputerName: WideString);
begin
  Create;
  SetTargetComputer(ComputerName);
end;

//--------------------------------------------------------------------------------------------------

destructor TJclTaskSchedule.Destroy;
begin
  FreeAndNil(FTasks);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TJclTaskSchedule.GetTargetComputer: WideString;
var
  ComputerName: PWideChar;
begin
  OleCheck(FTaskScheduler.GetTargetComputer(ComputerName));
  Result := ComputerName;
  CoTaskMemFree(ComputerName);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTaskSchedule.SetTargetComputer(const Value: WideString);
begin
  OleCheck(FTaskScheduler.SetTargetComputer(Pointer(Value)));
end;

//--------------------------------------------------------------------------------------------------

class function TJclTaskSchedule.IsRunning: Boolean;
begin
  Result := TaskSchedulerServiceControl(scfQueryStatus) = SERVICE_RUNNING;
end;

//--------------------------------------------------------------------------------------------------

class procedure TJclTaskSchedule.Start;
begin
  TaskSchedulerServiceControl(scfStartAndContinue);
end;

//--------------------------------------------------------------------------------------------------

class procedure TJclTaskSchedule.Stop;
begin
  TaskSchedulerServiceControl(scfStop);
end;

//--------------------------------------------------------------------------------------------------

function TJclTaskSchedule.GetTask(const Idx: Integer): TJclScheduledTask;
begin
  Result := TJclScheduledTask(FTasks.Items[Idx]);
end;

//--------------------------------------------------------------------------------------------------

function TJclTaskSchedule.GetTaskCount: Integer;
begin
  Result := FTasks.Count;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTaskSchedule.Refresh;
var
  EnumWorkItems: IEnumWorkItems;
  ItemName: PLPWSTR;    
  RealItemName: PWideChar;
  FetchedCount: DWORD;
  TaskIid: TIID;
  spUnk: IUnknown;
  ATask: TJclScheduledTask;
begin
  OleCheck(TaskScheduler.Enum(EnumWorkItems));
  TaskIid := IID_ITask;
  ItemName := nil;
  FTasks.Clear;
  while SUCCEEDED(EnumWorkItems.Next(1, ItemName, FetchedCount)) and (FetchedCount > 0) do
  begin
    RealItemName := ItemName^;
    OleCheck(TaskScheduler.Activate(RealItemName, TaskIid, spUnk));
    ATask := TJclScheduledTask.Create(RealItemName, spUnk as ITask);
    ATask.Refresh;
    FTasks.Add(ATask);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclTaskSchedule.Add(const TaskName: WideString): TJclScheduledTask;
var
  TaskClsId: TCLSID;
  TaskIid: TIID;
  spUnk: IUnknown;
begin
  TaskClsId := CLSID_CTask;
  TaskIid := IID_ITask;
  OleCheck(TaskScheduler.NewWorkItem(PWideChar(TaskName), TaskClsId, TaskIid, spUnk));
  Result := TJclScheduledTask.Create(TaskName, spUnk as ITask);
  Result.SetAccountInformation(LocalSystemAccount, '');
  Result.Save;
  Result.Refresh;
  FTasks.Add(Result);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTaskSchedule.Delete(const Idx: Integer);
begin
  Remove(Tasks[Idx]);
end;

//--------------------------------------------------------------------------------------------------

function TJclTaskSchedule.Remove(const TaskName: WideString): Integer;
begin
  for Result := 0 to TaskCount-1 do
    if WideCompareText(Tasks[Result].TaskName, TaskName) = 0 then
    begin
      Delete(Result);
      Exit;
    end;
  Result := -1;
end;

//--------------------------------------------------------------------------------------------------

function TJclTaskSchedule.Remove(const TaskIntf: ITask): Integer;
begin
  for Result := 0 to TaskCount-1 do
    if Tasks[Result].Task = TaskIntf then
    begin
      Delete(Result);
      Exit;
    end;
  Result := -1;
end;

//--------------------------------------------------------------------------------------------------

function TJclTaskSchedule.Remove(const ATask: TJclScheduledTask): Integer;
begin
  Result := FTasks.IndexOf(ATask);
  if Result <> -1 then
  begin
    FTaskScheduler.Delete(PWideChar(Tasks[Result].TaskName));
    FTasks.Delete(Result);
    Exit;
  end;
end;

//==================================================================================================
// TJclTaskTrigger
//==================================================================================================

procedure TJclTaskTrigger.SetTaskTrigger(const Value: ITaskTrigger);
begin
  FTaskTrigger := Value;
end;

//--------------------------------------------------------------------------------------------------

function TJclTaskTrigger.GetTrigger: TTaskTrigger;
begin
  Result.cbTriggerSize := SizeOf(Result);
  OleCheck(TaskTrigger.GetTrigger(Result));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTaskTrigger.SetTrigger(const Value: TTaskTrigger);
begin
  OleCheck(TaskTrigger.SetTrigger(Value));
end;

//--------------------------------------------------------------------------------------------------

function TJclTaskTrigger.GetTriggerString: WideString;
var
  Trigger: PWideChar;
begin
  OleCheck(TaskTrigger.GetTriggerString(Trigger));
  Result := Trigger;
  CoTaskMemFree(Trigger);
end;

//==================================================================================================
// TJclTaskTriggers
//==================================================================================================

constructor TJclTaskTriggers.Create(AWorkItem: TJclScheduledWorkItem);
begin
  inherited Create(TJclTaskTrigger);
  FWorkItem := AWorkItem;
end;

//--------------------------------------------------------------------------------------------------

function TJclTaskTriggers.GetItem(Index: Integer): TJclTaskTrigger;
begin
  Result := TJclTaskTrigger(inherited GetItem(Index));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclTaskTriggers.SetItem(Index: Integer; Value: TJclTaskTrigger);
begin
  inherited SetItem(Index, Value);
end;

//--------------------------------------------------------------------------------------------------

function TJclTaskTriggers.GetOwner: TPersistent;
begin
  Result := FWorkItem;
end;

//--------------------------------------------------------------------------------------------------

function TJclTaskTriggers.Add(ATrigger: ITaskTrigger): TJclTaskTrigger;
begin
  Result := Add;
  Result.SetTaskTrigger(ATrigger);
end;

//--------------------------------------------------------------------------------------------------

function TJclTaskTriggers.Add: TJclTaskTrigger;
begin
  Result := TJclTaskTrigger(inherited Add);
end;

//--------------------------------------------------------------------------------------------------

function TJclTaskTriggers.AddItem(Item: TJclTaskTrigger; Index: Integer): TJclTaskTrigger;
begin
  if Item = nil then
    Result := Add
  else
    Result := Item;

  if Assigned(Result) then
  begin
    Result.Collection := Self;
    if Index < 0 then
      Index := Count - 1;
    Result.Index := Index;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclTaskTriggers.Insert(Index: Integer): TJclTaskTrigger;
begin
  Result := AddItem(nil, Index);
end;

//==================================================================================================
// TJclScheduledWorkItem
//==================================================================================================

constructor TJclScheduledWorkItem.Create(const ATaskName: WideString;
  const AScheduledWorkItem: IScheduledWorkItem);
begin
  inherited Create;
  FScheduledWorkItem := AScheduledWorkItem;
  FTaskName := ATaskName;
  FData := TMemoryStream.Create;
  FTriggers := TJclTaskTriggers.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

destructor TJclScheduledWorkItem.Destroy;
begin
  FreeAndNil(FTriggers);
  FreeAndNil(FData);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclScheduledWorkItem.Save;
begin
  OleCheck((FScheduledWorkItem as IPersistFile).Save(nil, True));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclScheduledWorkItem.Run;
begin
  OleCheck(FScheduledWorkItem.Run);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclScheduledWorkItem.Terminate;
begin
  OleCheck(FScheduledWorkItem.Terminate);
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledWorkItem.GetAccountName: WideString;
var
  AccountName: PWideChar;
begin
  Result := '';
  if Win32Platform <> VER_PLATFORM_WIN32_NT then  // ignore this method in Win9x/ME
    Exit;
  try
    OleCheck(FScheduledWorkItem.GetAccountInformation(AccountName));
    Result := AccountName;
    CoTaskMemFree(AccountName);

    if Result = '' then
      Result := GetLocalComputerName + '\' + LocalSystemAccount;
  except
    Result := '';
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclScheduledWorkItem.SetAccountInformation(const Name, Password: WideString);
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then  // ignore this method in Win9x/ME
    Exit;
  if (Name = LocalSystemAccount) or (Name = '') then
    OleCheck(FScheduledWorkItem.SetAccountInformation('', nil))
  else
    OleCheck(FScheduledWorkItem.SetAccountInformation(PWideChar(Name), PWideChar(Password)));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclScheduledWorkItem.SetAccountName(const Value: WideString);
begin
  SetAccountInformation(Value, '');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclScheduledWorkItem.SetPassword(const Value: WideString);
begin
  SetAccountInformation(GetAccountName, Value);
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledWorkItem.GetComment: WideString;
var
  Comment: PWideChar;
begin
  OleCheck(FScheduledWorkItem.GetComment(Comment));
  Result := Comment;
  CoTaskMemFree(Comment);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclScheduledWorkItem.SetComment(const Value: WideString);
begin
  OleCheck(FScheduledWorkItem.SetComment(PWideChar(Value)));
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledWorkItem.GetCreator: WideString;
var
  Creator: PWideChar;
begin
  OleCheck(FScheduledWorkItem.GetCreator(Creator));
  Result := Creator;
  CoTaskMemFree(Creator);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclScheduledWorkItem.SetCreator(const Value: WideString);
begin
  OleCheck(FScheduledWorkItem.SetCreator(PWideChar(Value)));
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledWorkItem.GetExitCode: DWORD;
begin
  OleCheck(FScheduledWorkItem.GetExitCode(Result));
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledWorkItem.GetDeadlineMinutes: Word;
var
  Dummy: Word;
begin
  OleCheck(FScheduledWorkItem.GetIdleWait(Result, Dummy));
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledWorkItem.GetIdleMinutes: Word;
var
  Dummy: Word;
begin
  OleCheck(FScheduledWorkItem.GetIdleWait(Dummy, Result));
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledWorkItem.GetMostRecentRunTime: TSystemTime;
begin
  OleCheck(FScheduledWorkItem.GetMostRecentRunTime(Result));
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledWorkItem.GetNextRunTime: TSystemTime;
begin
  OleCheck(FScheduledWorkItem.GetNextRunTime(Result));
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledWorkItem.GetRunTimes(const BeginTime, EndTime: TDateTime): TDateTimeArray;
var
  BeginSysTime, EndSysTime: TSystemTime;
  I, Count: Word;
  TaskTimes: PSystemTime;
begin
  DateTimeToSystemTime(BeginTime, BeginSysTime);
  DateTimeToSystemTime(EndTime, EndSysTime);

  if EndTime = InfiniteTime then
    OleCheck(FScheduledWorkItem.GetRunTimes(@BeginSysTime, nil, Count, TaskTimes))
  else
    OleCheck(FScheduledWorkItem.GetRunTimes(@BeginSysTime, @EndSysTime, Count, TaskTimes));
  try
    SetLength(Result, Count);
    for I:=0 to Count-1 do
    begin
      Result[I] := SystemTimeToDateTime(Windows.PSystemTime(TaskTimes)^);
      Inc(TaskTimes);
    end;
  finally
    CoTaskMemFree(TaskTimes);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledWorkItem.GetStatus: TJclScheduledTaskStatus;
var
  Status: HRESULT;
begin
  OleCheck(FScheduledWorkItem.GetStatus(Status));
  case Status of
    SCHED_S_TASK_READY:
      Result := tsReady;
    SCHED_S_TASK_RUNNING:
      Result := tsRunning;
    SCHED_S_TASK_NOT_SCHEDULED:
      Result := tsNotScheduled;
    SCHED_S_TASK_HAS_NOT_RUN:
      Result := tsHasNotRun;
  else
    Result := tsUnknown;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledWorkItem.GetErrorRetryCount: Word;
begin
  OleCheck(FScheduledWorkItem.GetErrorRetryCount(Result));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclScheduledWorkItem.SetErrorRetryCount(const Value: Word);
begin
  OleCheck(FScheduledWorkItem.SetErrorRetryCount(Value));
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledWorkItem.GetErrorRetryInterval: Word;
begin
  OleCheck(FScheduledWorkItem.GetErrorRetryInterval(Result));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclScheduledWorkItem.SetErrorRetryInterval(const Value: Word);
begin
  OleCheck(FScheduledWorkItem.SetErrorRetryInterval(Value));
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledWorkItem.GetFlags: TJclScheduledTaskFlags;
var
  AFlags: DWORD;
  AFlag: TJclScheduledTaskFlag;
begin
  OleCheck(FScheduledWorkItem.GetFlags(AFlags));
  Result := [];
  for AFlag:=Low(TJclScheduledTaskFlag) to High(TJclScheduledTaskFlag) do
    if (AFlags and TaskFlagMapping[AFlag]) = TaskFlagMapping[AFlag] then
      Include(Result, AFlag);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclScheduledWorkItem.SetFlags(const Value: TJclScheduledTaskFlags);
var
  AFlags: DWORD;
  AFlag: TJclScheduledTaskFlag;
begin
  AFlags := 0;
  for AFlag:=Low(TJclScheduledTaskFlag) to High(TJclScheduledTaskFlag) do
    if AFlag in Value then
      AFlags := AFlags or TaskFlagMapping[AFlag];
  OleCheck(FScheduledWorkItem.SetFlags(AFlags));
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledWorkItem.GetData: TStream;
var
  Count: Word;
  Buf: PByte;
begin
  FData.Clear;
  Buf := nil;
  OleCheck(FScheduledWorkItem.GetWorkItemData(Count, Buf));
  try
    FData.Write(Buf^, Count);
    FData.Seek(0, soFromBeginning);
  finally
    CoTaskMemFree(Buf);
  end;
  Result := FData;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclScheduledWorkItem.SetData(const Value: TStream);
begin
  FData.Clear;
  FData.CopyFrom(Value, 0);
  OleCheck(FScheduledWorkItem.SetWorkItemData(FData.Size, PByte(FData.Memory)));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclScheduledWorkItem.Refresh;
var
  I, Count: Word;
  ATrigger: ITaskTrigger;
begin
  OleCheck(FScheduledWorkItem.GetTriggerCount(Count));

  FTriggers.Clear;
  if Count > 0 then
  for I:=0 to Count-1 do
  begin
    OleCheck(FScheduledWorkItem.GetTrigger(I, ATrigger));
    FTriggers.Add(ATrigger);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledWorkItem.GetTriggerCount: Integer;
begin
  Result := FTriggers.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledWorkItem.GetTrigger(const Idx: Integer): TJclTaskTrigger;
begin
  Result := TJclTaskTrigger(FTriggers.Items[Idx]);
end;

//==================================================================================================
// TJclScheduledTask
//==================================================================================================

function TJclScheduledTask.GetApplicationName: WideString;
var
  AppName: PWideChar;
begin
  OleCheck(Task.GetApplicationName(AppName));
  Result := AppName;
  CoTaskMemFree(AppName);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclScheduledTask.SetApplicationName(const Value: WideString);
begin
  OleCheck(Task.SetApplicationName(PWideChar(Value)));
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledTask.GetMaxRunTime: DWORD;
begin
  OleCheck(Task.GetMaxRunTime(Result));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclScheduledTask.SetMaxRunTime(const Value: DWORD);
begin
  OleCheck(Task.SetMaxRunTime(Value));
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledTask.GetParameters: WideString;
var
  Parameters: PWideChar;
begin
  OleCheck(Task.GetParameters(Parameters));
  Result := Parameters;
  CoTaskMemFree(Parameters);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclScheduledTask.SetParameters(const Value: WideString);
begin
  OleCheck(Task.SetParameters(PWideChar(Value)));
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledTask.GetPriority: DWORD;
begin
  OleCheck(Task.GetPriority(Result));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclScheduledTask.SetPriority(const Value: DWORD);
begin
  OleCheck(Task.SetPriority(Value));
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledTask.GetTaskFlags: DWORD;
begin
  OleCheck(Task.GetTaskFlags(Result));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclScheduledTask.SetTaskFlags(const Value: DWORD);
begin
  OleCheck(Task.SetTaskFlags(Value));
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledTask.GetWorkingDirectory: WideString;
var
  WorkingDir: PWideChar;
begin
  OleCheck(Task.GetWorkingDirectory(WorkingDir));
  Result := WorkingDir;
  CoTaskMemFree(WorkingDir);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclScheduledTask.SetWorkingDirectory(const Value: WideString);
begin
  OleCheck(Task.SetWorkingDirectory(PWideChar(Value)));
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledTask.ShowPage(Pages: TJclScheduleTaskPropertyPages): Boolean;
var
  hPropPages: array [0..2] of MSTask.HPropSheetPage;
  PropHeader: {Commctrl.}TPropSheetHeader;
begin
  OleCheck((FScheduledWorkItem as IProvideTaskPage).GetPage(TASKPAGE_TASK, True, hPropPages[0]));
  OleCheck((FScheduledWorkItem as IProvideTaskPage).GetPage(TASKPAGE_SCHEDULE, True, hPropPages[1]));
  OleCheck((FScheduledWorkItem as IProvideTaskPage).GetPage(TASKPAGE_SETTINGS, True, hPropPages[2]));

  FillChar(PropHeader, SizeOf(PropHeader), 0);
  PropHeader.dwSize := SizeOf(PropHeader);
  PropHeader.dwFlags := PSH_DEFAULT or PSH_NOAPPLYNOW;
  PropHeader.phpage := @hPropPages;
  PropHeader.nPages := Length(hPropPages);
  Result := PropertySheet(PropHeader) > 0;
end;

//--------------------------------------------------------------------------------------------------

function TJclScheduledTask.GetTask: ITask;
begin
  Result := ScheduledWorkItem as ITask;
end;

//--------------------------------------------------------------------------------------------------

const
  SCHED_CLASS            = 'SAGEWINDOWCLASS';
  SCHED_TITLE            = 'SYSTEM AGENT COM WINDOW';
  SCHED_SERVICE_APP_NAME = 'mstask.exe';
  SCHED_SERVICE_NAME     = 'Schedule';

const
  WM_TASK_SCHEDULER_SERVICE_QUERYSTATUS = WM_USER + 200;
  WM_TASK_SCHEDULER_SERVICE_STOP        = WM_USER + 201;
  WM_TASK_SCHEDULER_SERVICE_PAUSE       = WM_USER + 202;
  WM_TASK_SCHEDULER_SERVICE_CONTINUE    = WM_USER + 203;

function TaskSchedulerServiceControl(Func: TTSServiceControlFunction): Integer;

  function Win9x(Func: TTSServiceControlFunction): Integer;
  var
    WinHandle: HWnd;

    function QueryServiceStatus(WinHandle: HWnd): Integer;
    begin
      if WinHandle = 0 then
        Result := SERVICE_STOPPED
      else
        Result := SendMessage(WinHandle, WM_TASK_SCHEDULER_SERVICE_QUERYSTATUS, 0, 0);
    end;

    function StopService(WinHandle: HWnd): Integer;
    begin
      if WinHandle <> 0 then
        PostMessage(WinHandle, WM_COMMAND, 100, 0);
      Result := SERVICE_STOPPED;  // we assume this
    end;

    function StartService(WinHandle: HWnd): Integer;
    var
      StartupInfo: TStartupInfo;
      ProcessInfo: TProcessInformation;
      StartTickCount: DWord;
    begin
      // Start mstask.exe
      FillChar(StartupInfo, SizeOf(StartupInfo), 0);
      StartupInfo.cb := SizeOf(StartupInfo);
      // CreateProcess search for mstask.exe, see Platform-SDK for details
      if CreateProcess(nil, SCHED_SERVICE_APP_NAME, nil, nil, False,
          CREATE_NEW_CONSOLE or CREATE_NEW_PROCESS_GROUP, nil, nil,
          StartupInfo, ProcessInfo) then
      begin
        CloseHandle(ProcessInfo.hProcess);
        CloseHandle(ProcessInfo.hThread);
        // get service window handle; wait maximal 1 sec
        StartTickCount := GetTickCount;
        repeat
          WinHandle := FindWindow(SCHED_CLASS, SCHED_TITLE);
          if WinHandle = 0 then
            Sleep(100);
        until (WinHandle <> 0) and ((GetTickCount - StartTickCount) > 1000);
        Result := QueryServiceStatus(WinHandle);
      end
      else
      begin
        Result := 0;
        if GetLastError = ERROR_FILE_NOT_FOUND then
          SetLastError(ERROR_SERVICE_DOES_NOT_EXIST)
        else
          RaiseLastOSError;
      end;
    end;

    function PauseService(WinHandle: HWnd): Integer;
    begin
      if WinHandle = 0 then
        Result := SERVICE_STOPPED
      else
      begin
        PostMessage(WinHandle, WM_TASK_SCHEDULER_SERVICE_PAUSE, 0, 0);
        Result := SERVICE_PAUSED;  // we assume this
      end;
    end;

    function ContinueService(WinHandle: HWnd): Integer;
    begin
      if WinHandle = 0 then
        Result := SERVICE_STOPPED
      else
      begin
        PostMessage(WinHandle, WM_TASK_SCHEDULER_SERVICE_CONTINUE, 0, 0);
        Result := SERVICE_RUNNING;  // we assume this
      end;
    end;

    function StartAndContinueService(WinHandle: HWnd): Integer;
    begin
      if WinHandle = 0 then
        Result := StartService(WinHandle)
      else
      begin
        WinHandle := FindWindow(SCHED_CLASS, SCHED_TITLE);
        Result := QueryServiceStatus(WinHandle);
      end;
      if Result = SERVICE_PAUSED then
      begin
        PostMessage(WinHandle, WM_TASK_SCHEDULER_SERVICE_CONTINUE, 0, 0);
        Result := SERVICE_RUNNING;  // we assume this
      end;
    end;

  begin {Win9x}
    // get service window handle
    WinHandle := FindWindow(SCHED_CLASS, SCHED_TITLE);
    case Func of
      scfQueryStatus:
        Result := QueryServiceStatus(WinHandle);
      scfStop:
        Result := StopService(WinHandle);
      scfStart:
        Result := StartService(WinHandle);
      scfPause:
        Result := PauseService(WinHandle);
      scfContinue:
        Result := ContinueService(WinHandle);
      scfStartAndContinue:
        Result := StartAndContinueService(WinHandle);
    else
      SetLastError(ERROR_INVALID_SERVICE_CONTROL);
      Result := 0;
    end;
  end;

  function WinNT(Func: TTSServiceControlFunction): Integer;
  type
    PPChar = ^PChar;
  var
    ServiceManagerHandle, ServiceHandle: SC_HANDLE;
    SvcStatus: TServiceStatus;
  begin
    Result := 0;
    // get service handle
    ServiceManagerHandle := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
    if ServiceManagerHandle = 0 then
      RaiseLastOSError;
    ServiceHandle := OpenService(ServiceManagerHandle, SCHED_SERVICE_NAME,
      SERVICE_START or SERVICE_QUERY_STATUS or SERVICE_STOP or SERVICE_PAUSE_CONTINUE);
    CloseServiceHandle(ServiceManagerHandle);
    if ServiceHandle <> 0 then
    begin
      try  // ServiceHandle
        case Func of
          scfQueryStatus:
            Result := GetServiceStatusWaitingIfPending(ServiceHandle);
          scfStop:
            begin
              Result := GetServiceStatus(ServiceHandle);
              if not (Result in [SERVICE_STOPPED, SERVICE_STOP_PENDING]) then
                if not ControlService(ServiceHandle, SERVICE_CONTROL_STOP, SvcStatus) then
                  RaiseLastOSError;
              Result := GetServiceStatusWaitingIfPending(ServiceHandle);
            end;
          scfStart:
            begin
              Result := GetServiceStatus(ServiceHandle);
              if Result in [SERVICE_STOPPED, SERVICE_STOP_PENDING] then
              begin
                if not StartService(ServiceHandle, 0, PPChar(nil)^) then
                  RaiseLastOSError;
              end;
              Result := GetServiceStatusWaitingIfPending(ServiceHandle);
            end;
          scfPause:
            begin
              Result := GetServiceStatus(ServiceHandle);
              if not (Result in [SERVICE_STOPPED, SERVICE_STOP_PENDING,
                                 SERVICE_PAUSED, SERVICE_PAUSE_PENDING]) then
                if not ControlService(ServiceHandle, SERVICE_CONTROL_PAUSE, SvcStatus) then
                  RaiseLastOSError;
              Result := GetServiceStatusWaitingIfPending(ServiceHandle);
            end;
          scfContinue:
            begin
              Result := GetServiceStatus(ServiceHandle);
              if not (Result in [SERVICE_STOPPED, SERVICE_STOP_PENDING,
                                 SERVICE_RUNNING, SERVICE_CONTINUE_PENDING]) then
                if not ControlService(ServiceHandle, SERVICE_CONTROL_CONTINUE, SvcStatus) then
                  RaiseLastOSError;
              Result := GetServiceStatusWaitingIfPending(ServiceHandle);
            end;
          scfStartAndContinue:
            begin
              Result := GetServiceStatus(ServiceHandle);
              if Result in [SERVICE_STOPPED, SERVICE_STOP_PENDING] then
              begin
                // start, if stopped
                if not StartService(ServiceHandle, 0, PPChar(nil)^) then
                  RaiseLastOSError;
                Result := GetServiceStatusWaitingIfPending(ServiceHandle);
              end;
              if not (Result in [SERVICE_STOPPED, SERVICE_STOP_PENDING,
                                 SERVICE_RUNNING, SERVICE_CONTINUE_PENDING]) then
                if not ControlService(ServiceHandle, SERVICE_CONTROL_CONTINUE, SvcStatus) then
                  RaiseLastOSError;
              Result := GetServiceStatusWaitingIfPending(ServiceHandle);
            end;
        else
          SetLastError(ERROR_INVALID_SERVICE_CONTROL);
        end;
      finally
        CloseServiceHandle(ServiceHandle);
      end;
    end
    else
    begin  // ServiceHandle = 0
      if GetLastError <> ERROR_SERVICE_DOES_NOT_EXIST then
        RaiseLastOSError;
    end;
  end;

begin {TaskSchedulerServiceControl}
  if Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := WinNT(Func)
  else
    Result := Win9X(Func);
end;

// History:

// $Log$
// Revision 1.15  2004/10/08 20:13:03  rrossmair
// replaced JclUnicode routines by JclWideStrings equivalents
//
// Revision 1.14  2004/07/28 18:00:54  marquardt
// various style cleanings, some minor fixes
//
// Revision 1.13  2004/06/16 07:30:31  marquardt
// added tilde to all IFNDEF ENDIFs, inherited qualified
//
// Revision 1.12  2004/06/14 13:05:21  marquardt
// style cleaning ENDIF, Tabs
//
// Revision 1.11  2004/06/14 11:05:53  marquardt
// symbols added to all ENDIFs and some other minor style changes like removing IFOPT
//
// Revision 1.10  2004/06/02 03:23:47  rrossmair
// cosmetic changes in several units (code formatting, help TODOs processed etc.)
//
// Revision 1.9  2004/05/06 23:43:22  rrossmair
// minor improvements
//
// Revision 1.8  2004/05/05 07:33:49  rrossmair
// header updated according to new policy: initial developers & contributors listed
//
// Revision 1.7  2004/04/26 04:28:16  peterjhaas
// - add TaskSchedulerServiceControl
// - some bugfixes for Win9x
//
// Revision 1.6  2004/04/06 04:55:18  peterjhaas
// adapt compiler conditions, add log entry
//

end.
