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
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains routines and classes to control NT service                                    }
{                                                                                                  }
{ Unit owner: Flier Lu (flier_lu@yahoo.com.cn)                                                     }
{ Last modified: February 21, 2002                                                                 }
{                                                                                                  }
{**************************************************************************************************}

unit JclSvcCtrl;

interface

{$I jcl.inc}

{$WEAKPACKAGEUNIT ON}

uses
  Windows, Classes, SysUtils,
  {$IFDEF DELPHI5_UP}
  Contnrs,
  {$ENDIF DELPHI5_UP}
  WinSvc,
  JclBase;

{$DEFINE USE_STATE_MAPPING}
{ TODO -cDesign : Remove the conditional }

{ TODO -cDOC : Original code: "Flier Lu" <flier_lu@yahoo.com.cn> }

//--------------------------------------------------------------------------------------------------
// Service Types
//--------------------------------------------------------------------------------------------------

type
  TJclServiceType = (stKernelDriver,        // SERVICE_KERNEL_DRIVER
                     stFileSystemDriver,    // SERVICE_FILE_SYSTEM_DRIVER
                     stAdapter,             // SERVICE_ADAPTER
                     stRecognizerDriver,    // SERVICE_RECOGNIZER_DRIVER
                     stWin32OwnProcess,     // SERVICE_WIN32_OWN_PROCESS
                     stWin32ShareProcess,   // SERVICE_WIN32_SHARE_PROCESS
                     stInteractiveProcess); // SERVICE_INTERACTIVE_PROCESS
  TJclServiceTypes = set of TJclServiceType;

const
  stDriver  = [stKernelDriver, stFileSystemDriver, stRecognizerDriver];
  stWin32   = [stWin32OwnProcess, stWin32ShareProcess];
  stAllType = stDriver + stWin32 + [stAdapter, stInteractiveProcess];

//--------------------------------------------------------------------------------------------------
// Service State
//--------------------------------------------------------------------------------------------------

type
  TJclServiceState = (
                   {$IFNDEF USE_STATE_MAPPING}
                      ssUnknown,
                   {$ENDIF}
                      ssStopped,         // SERVICE_STOPPED
                      ssStartPending,    // SERVICE_START_PENDING
                      ssStopPending,     // SERVICE_STOP_PENDING
                      ssRunning,         // SERVICE_RUNNING
                      ssContinuePending, // SERVICE_CONTINUE_PENDING
                      ssPausePending,    // SERVICE_PAUSE_PENDING
                      ssPaused);         // SERVICE_PAUSED

//--------------------------------------------------------------------------------------------------
// Start Type
//--------------------------------------------------------------------------------------------------

type
  TJclServiceStartType = (sstBoot,      // SERVICE_BOOT_START
                          sstSystem,    // SERVICE_SYSTEM_START
                          sstAuto,      // SERVICE_AUTO_START
                          sstDemand,    // SERVICE_DEMAND_START
                          sstDisabled); // SERVICE_DISABLED

//--------------------------------------------------------------------------------------------------
// Error control type
//--------------------------------------------------------------------------------------------------

type
  TJclServiceErrorControlType = (ectIgnore,    // SSERVICE_ERROR_IGNORE
                                 ectNormal,    // SSERVICE_ERROR_NORMAL
                                 ectSevere,    // SSERVICE_ERROR_SEVERE
                                 ectCritical); // SERVICE_ERROR_CRITICAL


//--------------------------------------------------------------------------------------------------
// Controls Accepted
//--------------------------------------------------------------------------------------------------

type
  TJclServiceControlAccepted = (caStop,          // SERVICE_ACCEPT_STOP
                                caPauseContinue, // SERVICE_ACCEPT_PAUSE_CONTINUE
                                caShutdown);     // SERVICE_ACCEPT_SHUTDOWN
  TJclServiceControlAccepteds = set of TJclServiceControlAccepted;

//--------------------------------------------------------------------------------------------------
// Service sort type
//--------------------------------------------------------------------------------------------------

type
  TJclServiceSortOrderType = (sotServiceName,
                              sotDisplayName,
                              sotDescription,
                              sotFileName,
                              sotServiceState,
                              sotStartType,
                              sotErrorControlType,
                              sotLoadOrderGroup,
                              sotWin32ExitCode);

const
  DefaultSCMDesiredAccess = SC_MANAGER_CONNECT or
                            SC_MANAGER_ENUMERATE_SERVICE or
                            SC_MANAGER_QUERY_LOCK_STATUS;

  DefaultSvcDesiredAccess = SERVICE_ALL_ACCESS;

//--------------------------------------------------------------------------------------------------
// Service related classes
//--------------------------------------------------------------------------------------------------

type
  TJclServiceGroup = class;
  TJclSCManager = class;

  TJclNtService = class (TObject)
  private
    FSCManager: TJclSCManager;
    FHandle: SC_HANDLE;
    FDesiredAccess: DWORD;
    FServiceName: string;
    FDisplayName: string;
    FFileName: TFileName;
    FDependentServices: TList;
    FDependentGroups: TList;
    FDependentByServices: TList;
    FServiceTypes: TJclServiceTypes;
    FServiceState: TJclServiceState;
    FStartType: TJclServiceStartType;
    FErrorControlType: TJclServiceErrorControlType;
    FWin32ExitCode: DWORD;
    FGroup: TJclServiceGroup;
    FControlsAccepted: TJclServiceControlAccepteds;
    function GetDescription: string;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetDependentService(const Index: Integer): TJclNtService;
    function GetDependentServiceCount: Integer;
    function GetDependentGroup(const Index: Integer): TJclServiceGroup;
    function GetDependentGroupCount: Integer;
    function GetDependentByService(const Index: Integer): TJclNtService;
    function GetDependentByServiceCount: Integer;
  protected
    constructor Create(const ASCManager: TJclSCManager; const SvcStatus: TEnumServiceStatus);
    procedure Open(const ADesiredAccess: DWORD = DefaultSvcDesiredAccess);
    procedure Close;
    function GetServiceStatus: TServiceStatus;
    procedure UpdateDependents;
    procedure UpdateStatus(const SvcStatus: TServiceStatus);
    procedure UpdateConfig(const SvcConfig: TQueryServiceConfig);
  public
    destructor Destroy; override;
    procedure Refresh;
    procedure Delete;
    function Controls(const ControlType: DWORD; const ADesiredAccess: DWORD = DefaultSvcDesiredAccess): TServiceStatus;
    procedure Start(const Args: array of string; const Sync: Boolean = True); overload;
    procedure Start(const Sync: Boolean = True); overload;
    procedure Stop(const Sync: Boolean = True);
    procedure Pause(const Sync: Boolean = True);
    procedure Continue(const Sync: Boolean = True);
    function WaitFor(const State: TJclServiceState; const TimeOut: DWORD = INFINITE): Boolean;
    property SCManager: TJclSCManager read FSCManager;
    property Active: Boolean read GetActive write SetActive;
    property Handle: SC_HANDLE read FHandle;
    property ServiceName: string read FServiceName;
    property DisplayName: string read FDisplayName;
    property DesiredAccess: DWORD read FDesiredAccess;
    property Description: string read GetDescription; // Win2K or later
    property FileName: TFileName read FFileName;
    property DependentServices[const Index: Integer]: TJclNtService read GetDependentService;
    property DependentServiceCount: Integer read GetDependentServiceCount;
    property DependentGroups[const Index: Integer]: TJclServiceGroup read GetDependentGroup;
    property DependentGroupCount: Integer read GetDependentGroupCount;
    property DependentByServices[const Index: Integer]: TJclNtService read GetDependentByService;
    property DependentByServiceCount: Integer read GetDependentByServiceCount;
    property ServiceTypes: TJclServiceTypes read FServiceTypes;
    property ServiceState: TJclServiceState read FServiceState;
    property StartType: TJclServiceStartType read FStartType;
    property ErrorControlType: TJclServiceErrorControlType read FErrorControlType;
    property Win32ExitCode: DWORD read FWin32ExitCode;
    property Group: TJclServiceGroup read FGroup;
    property ControlsAccepted: TJclServiceControlAccepteds read FControlsAccepted;
  end;

  TJclServiceGroup = class (TObject)
  private
    FSCManager: TJclSCManager;
    FName: string;
    FOrder: Integer;
    FServices: TList;
    function GetService(const Index: Integer): TJclNtService;
    function GetServiceCount: Integer;
  protected
    constructor Create(const ASCManager: TJclSCManager; const AName: string; const AOrder: Integer);
    function Add(const AService: TJclNtService): Integer;
    function Remove(const AService: TJclNtService): Integer;
  public
    destructor Destroy; override;
    property SCManager: TJclSCManager read FSCManager;
    property Name: string read FName;
    property Order: Integer read FOrder;
    property Services[const Index: Integer]: TJclNtService read GetService;
    property ServiceCount: Integer read GetServiceCount;
  end;

  TJclSCManager = class (TObject)
  private
    FMachineName: string;
    FDatabaseName: string;
    FDesiredAccess: DWORD;
    FHandle: SC_HANDLE;
    FLock: SC_LOCK;
    FServices: TObjectList;
    FGroups: TObjectList;
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetService(const Index: Integer): TJclNtService;
    function GetServiceCount: Integer;
    function GetGroup(const Index: Integer): TJclServiceGroup;
    function GetGroupCount: Integer;
  protected
    m_sotOrderType: TJclServiceSortOrderType;
    m_fOrderAsc: Boolean;
    procedure Open;
    procedure Close;
    function AddService(const AService: TJclNtService): Integer;
    function AddGroup(const AGroup: TJclServiceGroup): Integer;
    function GetServiceLockStatus: PQueryServiceLockStatus;
  public
    constructor Create(const AMachineName: string = '';
      const ADesiredAccess: DWORD = DefaultSCMDesiredAccess;
      const ADatabaseName: string = SERVICES_ACTIVE_DATABASE);
    destructor Destroy; override;
    procedure Clear;
    procedure Refresh(const RefreshAll: Boolean = False);
    procedure Sort(const AOrderType: TJclServiceSortOrderType; const AOrderAsc: Boolean = True);
    function FindService(const SvcName: string; var NtSvc: TJclNtService): Boolean;
    function FindGroup(const GrpName: string; var SvcGrp: TJclServiceGroup;
      const AutoAdd: Boolean = True): Boolean;
    procedure Lock;
    procedure Unlock;
    function IsLocked: Boolean;
    function LockOwner: string;
    function LockDuration: DWORD;
    class function ServiceType(const SvcType: TJclServiceTypes): DWORD; overload;
    class function ServiceType(const SvcType: DWORD): TJclServiceTypes; overload;
    {$IFDEF USE_STATE_MAPPING}
    class function ServiceState(const SvcState: TJclServiceState): DWORD; overload;
    class function ServiceState(const SvcState: DWORD): TJclServiceState; overload;
    {$ENDIF}
    class function ControlAccepted(const CtrlAccepted: TJclServiceControlAccepteds): DWORD; overload;
    class function ControlAccepted(const CtrlAccepted: DWORD): TJclServiceControlAccepteds; overload;
    property MachineName: string read FMachineName;
    property DatabaseName: string read FDatabaseName;
    property DesiredAccess: DWORD read FDesiredAccess;
    property Active: Boolean read GetActive write SetActive;
    property Handle: SC_HANDLE read FHandle;
    property Services[const Index: Integer]: TJclNtService read GetService;
    property ServiceCount: Integer read GetServiceCount;
    property Groups[const Index: Integer]: TJclServiceGroup read GetGroup;
    property GroupCount: Integer read GetGroupCount;
  end;

implementation

uses
  Math,
  JclRegistry, JclResources, JclSysUtils, JclWin32;

const
  InvalidSCMHanlde = 0;

  ServiceTypeMapping: array[TJclServiceType] of DWORD =
    (SERVICE_KERNEL_DRIVER, SERVICE_FILE_SYSTEM_DRIVER, SERVICE_ADAPTER,
     SERVICE_RECOGNIZER_DRIVER, SERVICE_WIN32_OWN_PROCESS,
     SERVICE_WIN32_SHARE_PROCESS, SERVICE_INTERACTIVE_PROCESS);
{$IFDEF USE_STATE_MAPPING}
  ServiceStateMapping: array[TJclServiceState] of DWORD =
    (SERVICE_STOPPED, SERVICE_START_PENDING, SERVICE_STOP_PENDING, SERVICE_RUNNING,
     SERVICE_CONTINUE_PENDING, SERVICE_PAUSE_PENDING, SERVICE_PAUSED);
{$ENDIF}
  ServiceControlAcceptedMapping: array[TJclServiceControlAccepted] of DWORD =
    (SERVICE_ACCEPT_STOP, SERVICE_ACCEPT_PAUSE_CONTINUE, SERVICE_ACCEPT_SHUTDOWN);

resourcestring
  RsInvalidSvcState = 'Invalid service state: %.8x';
{ TODO -cRES : Move to JclResources }  

//==================================================================================================
// TJclNtService
//==================================================================================================

constructor TJclNtService.Create(const ASCManager: TJclSCManager; const SvcStatus: TEnumServiceStatus);
begin
  inherited Create;
  FSCManager := ASCManager;
  FHandle := InvalidSCMHanlde;
  FServiceName := SvcStatus.lpServiceName;
  FDisplayName := SvcStatus.lpDisplayName;
  FGroup := nil;
  FDependentServices := TList.Create;
  FDependentGroups := TList.Create;
  FDependentByServices := nil; // Create on demand
end;

//--------------------------------------------------------------------------------------------------

destructor TJclNtService.Destroy;
begin
  FreeAndNil(FDependentServices);
  FreeAndNil(FDependentGroups);
  FreeAndNil(FDependentByServices);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TJclNtService.GetDescription: string;
const
  keyServices    = '\SYSTEM\CurrentControlSet\Services\';
  valDescription = 'Description';
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, keyServices + ServiceName, valDescription, '');
end;

//--------------------------------------------------------------------------------------------------

function TJclNtService.GetActive: Boolean;
begin
  Result := FHandle <> InvalidSCMHanlde;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclNtService.SetActive(const Value: Boolean);
begin
  if Value <> GetActive then
  begin
    if Value then
      Open
    else
      Close;
    Assert(Value = GetActive);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclNtService.UpdateDependents;
var
  I: Integer;
  pBuf: Pointer;
  pEss: PEnumServiceStatus;
  NtSvc: TJclNtService;
  dwBytesNeeded, dwServicesReturned: DWORD;
begin
  Open(SERVICE_ENUMERATE_DEPENDENTS);
  try
    if Assigned(FDependentByServices) then
      FDependentByServices.Clear
    else
      FDependentByServices := TList.Create;
    EnumDependentServices(FHandle, SERVICE_STATE_ALL,
      PEnumServiceStatus(nil)^, 0, dwBytesNeeded, dwServicesReturned);
    if GetLastError = ERROR_MORE_DATA then
    begin
      GetMem(pBuf, dwBytesNeeded);
      try
        pEss := pBuf;
        Win32Check(EnumDependentServices(FHandle, SERVICE_STATE_ALL,
          pEss^, dwBytesNeeded, dwBytesNeeded, dwServicesReturned));
        for I := 0 to dwServicesReturned - 1 do
        begin
          if (pEss.lpServiceName[1] <> SC_GROUP_IDENTIFIER) and
             (SCManager.FindService(pEss.lpServiceName, NtSvc)) then
            FDependentByServices.Add(NtSvc);
          Inc(pEss);
        end;
      finally
        FreeMem(pBuf);
      end;
    end;
  finally
    Close;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclNtService.GetDependentService(const Index: Integer): TJclNtService;
begin
  Result := TJclNtService(FDependentServices.Items[Index]);
end;

//--------------------------------------------------------------------------------------------------

function TJclNtService.GetDependentServiceCount: Integer;
begin
  Result := FDependentServices.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclNtService.GetDependentGroup(const Index: Integer): TJclServiceGroup;
begin
  Result := TJclServiceGroup(FDependentGroups.Items[Index]);
end;

//--------------------------------------------------------------------------------------------------

function TJclNtService.GetDependentGroupCount: Integer;
begin
  Result := FDependentGroups.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclNtService.GetDependentByService(const Index: Integer): TJclNtService;
begin
  if not Assigned(FDependentByServices) then
    UpdateDependents;
  Result := TJclNtService(FDependentByServices.Items[Index])
end;

//--------------------------------------------------------------------------------------------------

function TJclNtService.GetDependentByServiceCount: Integer;
begin
  if not Assigned(FDependentByServices) then
    UpdateDependents;
  Result := FDependentByServices.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclNtService.GetServiceStatus: TServiceStatus;
begin
  Assert(Active);
  Assert((DesiredAccess and SERVICE_QUERY_STATUS) <> 0);
  Win32Check(QueryServiceStatus(FHandle, Result));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclNtService.UpdateStatus(const SvcStatus: TServiceStatus);
begin
  with SvcStatus do
  begin
    FServiceTypes := TJclSCManager.ServiceType(dwServiceType);
    {$IFDEF USE_STATE_MAPPING}
    FServiceState := TJclSCManager.ServiceState(dwCurrentState);
    {$ELSE}
    FServiceState := TJclServiceState(dwCurrentState);
    {$ENDIF}
    FControlsAccepted := TJclSCManager.ControlAccepted(dwControlsAccepted);
    FWin32ExitCode := dwWin32ExitCode;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclNtService.UpdateConfig(const SvcConfig: TQueryServiceConfig);

  procedure UpdateLoadOrderGroup;
  begin
    if not Assigned(FGroup) then
      SCManager.FindGroup(SvcConfig.lpLoadOrderGroup, FGroup)
    else
    if CompareText(Group.Name, SvcConfig.lpLoadOrderGroup) = 0 then
    begin
      FGroup.Remove(Self);
      SCManager.FindGroup(SvcConfig.lpLoadOrderGroup, FGroup);
      FGroup.Add(Self);
    end;
  end;

  procedure UpdateDependencies;
  var
    pch: PChar;
    NtSvc: TJclNtService;
    SvcGrp: TJclServiceGroup;
  begin
    pch := SvcConfig.lpDependencies;
    FDependentServices.Clear;
    FDependentGroups.Clear;
    if pch = nil then
      Exit;
    while pch^ <> #0 do
    begin
      if pch^ = SC_GROUP_IDENTIFIER then
      begin
        SCManager.FindGroup(pch + 1, SvcGrp);
        FDependentGroups.Add(SvcGrp);
      end
      else
      if SCManager.FindService(pch, NtSvc) then
        FDependentServices.Add(NtSvc);
      Inc(pch, StrLen(pch) + 1);
    end;
  end;

begin
  with SvcConfig do
  begin
    FFileName := lpBinaryPathName;
    FStartType := TJclServiceStartType(dwStartType);
    FErrorControlType := TJclServiceErrorControlType(dwErrorControl);
    UpdateLoadOrderGroup;
    UpdateDependencies;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclNtService.Open(const ADesiredAccess: DWORD);
begin
  Assert((ADesiredAccess and (not SERVICE_ALL_ACCESS)) = 0);
  Active := False;
  FDesiredAccess := ADesiredAccess;
  FHandle := OpenService(SCManager.Handle, PChar(ServiceName), DesiredAccess);
  Win32Check(FHandle <> InvalidSCMHanlde);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclNtService.Close;
begin
  Assert(Active);
  Win32Check(CloseServiceHandle(FHandle));
  FHandle := InvalidSCMHanlde;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclNtService.Refresh;
var
  dwBytesNeeded: DWORD;
  pQrySvcCnfg: PQueryServiceConfig;
begin
  Open(SERVICE_QUERY_STATUS or SERVICE_QUERY_CONFIG);
  try
    UpdateStatus(GetServiceStatus);
    QueryServiceConfig(FHandle, nil, 0, dwBytesNeeded);
    Assert(ERROR_INSUFFICIENT_BUFFER = GetLastError);
    GetMem(pQrySvcCnfg, dwBytesNeeded);
    try
      Win32Check(QueryServiceConfig(FHandle, pQrySvcCnfg, dwBytesNeeded, dwBytesNeeded));
      UpdateConfig(pQrySvcCnfg^);
    finally
      FreeMem(pQrySvcCnfg);
    end;
  finally
    Close;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclNtService.Delete;
begin
  Open(_DELETE);
  try
    Win32Check(DeleteService(FHandle));
  finally
    Close;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclNtService.Start(const Args: array of string; const Sync: Boolean);
type
  PStrArray = ^TStrArray;
  TStrArray = array[0..32767] of PChar;
var
  I: Integer;
  lpServiceArgVectors: PChar;
begin
  Open(SERVICE_START);
  try
    if Length(Args) = 0 then
      lpServiceArgVectors := nil
    else
    begin
      GetMem(lpServiceArgVectors, SizeOf(PChar)*Length(Args));
      for I := 0 to Length(Args) - 1 do
        PStrArray(lpServiceArgVectors)^[I] := PChar(Args[I]);
    end;
    Win32Check(StartService(FHandle, Length(Args), lpServiceArgVectors));
    FreeMem(lpServiceArgVectors);
  finally
    Close;
  end;
  if Sync then
    WaitFor(ssRunning);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclNtService.Start(const Sync: Boolean = True);
begin
  Start([], Sync);
end;

//--------------------------------------------------------------------------------------------------

function TJclNtService.Controls(const ControlType: DWORD; const ADesiredAccess: DWORD): TServiceStatus;
begin
  Open(ADesiredAccess);
  try
    Win32Check(ControlService(FHandle, ControlType, Result));
  finally
    Close;
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclNtService.Stop(const Sync: Boolean);
begin
  Controls(SERVICE_CONTROL_STOP, SERVICE_STOP);
  if Sync then
    WaitFor(ssStopped);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclNtService.Pause(const Sync: Boolean);
begin
  Controls(SERVICE_CONTROL_PAUSE, SERVICE_PAUSE_CONTINUE);
  if Sync then
    WaitFor(ssPaused);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclNtService.Continue(const Sync: Boolean);
begin
  Controls(SERVICE_CONTROL_CONTINUE, SERVICE_PAUSE_CONTINUE);
  if Sync then
    WaitFor(ssRunning);
end;

//--------------------------------------------------------------------------------------------------

function TJclNtService.WaitFor(const State: TJclServiceState; const TimeOut: DWORD): Boolean;
var
  ssStatus: TServiceStatus;
  dwWaitedState: DWORD;
  dwStartTickCount: DWORD;
  dwOldCheckPoint: DWORD;
  dwWaitTime: DWORD;
begin
  {$IFDEF USE_STATE_MAPPING}
  dwWaitedState := TJclSCManager.ServiceState(State);
  {$ELSE}
  dwWaitedState := DWORD(State);
  {$ENDIF}
  Open(SERVICE_QUERY_STATUS);
  try
    dwStartTickCount := GetTickCount;
    dwOldCheckPoint  := 0;
    while True do
    begin
      ssStatus := GetServiceStatus;
      if ssStatus.dwCurrentState = dwWaitedState then
        Break;
      if ssStatus.dwCheckPoint > dwOldCheckPoint then
      begin
        dwStartTickCount := GetTickCount;
        dwOldCheckPoint  := ssStatus.dwCheckPoint;
      end
      else
      begin
        if TimeOut <> INFINITE then
          if (GetTickCount - dwStartTickCount) > Max(ssStatus.dwWaitHint, TimeOut) then
            Break;
      end;
      dwWaitTime := ssStatus.dwWaitHint div 10;
      if dwWaitTime < 1000 then
        dwWaitTime := 1000
      else
      if dwWaitTime > 10000 then
        dwWaitTime := 10000;
      Sleep(dwWaitTime);
    end;
    Result := ssStatus.dwCurrentState = dwWaitedState;
  finally
    Close;
  end;
end;

//==================================================================================================
// TJclServiceGroup
//==================================================================================================

constructor TJclServiceGroup.Create(const ASCManager: TJclSCManager; const AName: string;
  const AOrder: Integer);
begin
  inherited Create;
  FSCManager := ASCManager;
  FName := AName;
  FOrder := AOrder;
  FServices := TList.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclServiceGroup.Destroy;
begin
  FreeAndNil(FServices);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TJclServiceGroup.Add(const AService: TJclNtService): Integer;
begin
  Result := FServices.Add(AService);
end;

//--------------------------------------------------------------------------------------------------

function TJclServiceGroup.Remove(const AService: TJclNtService): Integer;
begin
  Result := FServices.Remove(AService);
end;

//--------------------------------------------------------------------------------------------------

function TJclServiceGroup.GetService(const Index: Integer): TJclNtService;
begin
  Result := TJclNtService(FServices.Items[Index]);
end;

//--------------------------------------------------------------------------------------------------

function TJclServiceGroup.GetServiceCount: Integer;
begin
  Result := FServices.Count;
end;

//==================================================================================================
// TJclSCManager
//==================================================================================================

constructor TJclSCManager.Create(const AMachineName: string; const ADesiredAccess: DWORD;
  const ADatabaseName: string);
begin
  Assert((ADesiredAccess and (not SC_MANAGER_ALL_ACCESS)) = 0);
  inherited Create;
  FMachineName := AMachineName;
  FDatabaseName := ADatabaseName;
  FDesiredAccess := ADesiredAccess;
  FHandle := InvalidSCMHanlde;
  FServices := TObjectList.Create;
  FGroups := TObjectList.Create;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclSCManager.Destroy;
begin
  FreeAndNil(FGroups);
  FreeAndNil(FServices);
  Close;
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TJclSCManager.AddService(const AService: TJclNtService): Integer;
begin
  Result := FServices.Add(AService);
end;

//--------------------------------------------------------------------------------------------------

function TJclSCManager.GetService(const Index: Integer): TJclNtService;
begin
  Result := TJclNtService(FServices.Items[Index]);
end;

//--------------------------------------------------------------------------------------------------

function TJclSCManager.GetServiceCount: Integer;
begin
  Result := FServices.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclSCManager.AddGroup(const AGroup: TJclServiceGroup): Integer;
begin
  Result := FGroups.Add(AGroup);
end;

//--------------------------------------------------------------------------------------------------

function TJclSCManager.GetGroup(const Index: Integer): TJclServiceGroup;
begin
  Result := TJclServiceGroup(FGroups.Items[Index]);
end;

//--------------------------------------------------------------------------------------------------

function TJclSCManager.GetGroupCount: Integer;
begin
  Result := FGroups.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclSCManager.GetActive: Boolean;
begin
  Result := FHandle <> InvalidSCMHanlde;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclSCManager.SetActive(const Value: Boolean);
begin
  if Value <> GetActive then
  begin
    if Value then
      Open
    else
      Close;
    Assert(Value = GetActive);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclSCManager.Open;
begin
  FHandle := OpenSCManager(PChar(FMachineName), PChar(FDatabaseName), FDesiredAccess);
  Win32Check(FHandle <> InvalidSCMHanlde);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclSCManager.Close;
begin
  Win32Check(CloseServiceHandle(FHandle));
  FHandle := InvalidSCMHanlde;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclSCManager.Lock;
begin
  Assert((DesiredAccess and SC_MANAGER_LOCK) <> 0);
  Active := True;
  FLock := LockServiceDatabase(FHandle);
  Win32Check(FLock <> nil);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclSCManager.Unlock;
begin
  Assert(Active);
  Assert((DesiredAccess and SC_MANAGER_LOCK) <> 0);
  Assert(FLock <> nil);
  Win32Check(UnlockServiceDatabase(FLock));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclSCManager.Clear;
begin
  FServices.Clear;
  FGroups.Clear;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclSCManager.Refresh(const RefreshAll: Boolean);

  procedure EnumServices;
  var
    I: Integer;
    pBuf: Pointer;
    pEss: PEnumServiceStatus;
    NtSvc: TJclNtService;
    dwBytesNeeded, dwServicesReturned, dwResumeHandle: DWORD;
  begin
    Assert((DesiredAccess and SC_MANAGER_ENUMERATE_SERVICE) <> 0);
    // Enum the services
    dwResumeHandle := 0; // Must set this value to zero !!!
    EnumServicesStatus(FHandle, SERVICE_TYPE_ALL, SERVICE_STATE_ALL,
      PEnumServiceStatus(nil)^, 0, dwBytesNeeded, dwServicesReturned, dwResumeHandle);
    if GetLastError = ERROR_MORE_DATA then
    begin
      GetMem(pBuf, dwBytesNeeded);
      try
        pEss := pBuf;
        Win32Check(EnumServicesStatus(FHandle, SERVICE_TYPE_ALL, SERVICE_STATE_ALL,
          pEss^, dwBytesNeeded, dwBytesNeeded, dwServicesReturned, dwResumeHandle));
        for I := 0 to dwServicesReturned - 1 do
        begin
          NtSvc := TJclNtService.Create(Self, pEss^);
          Assert(Assigned(NtSvc));
          AddService(NtSvc);
          NtSvc.Refresh;
          Inc(pEss);
        end;
      finally
        FreeMem(pBuf);
      end;
    end;
  end;

  procedure EnumServiceGroups;
  const
    keyServiceGroupOrder = '\SYSTEM\CurrentControlSet\Control\ServiceGroupOrder';
    valList              = 'List';
  var
    Buf: array of Char;
    pch: PChar;
    DataSize: Integer;
  begin
    // Get the service groups
    DataSize := RegReadBinary(HKEY_LOCAL_MACHINE, keyServiceGroupOrder, valList, Buf[0], 0);
    SetLength(Buf, DataSize);
    DataSize := RegReadBinary(HKEY_LOCAL_MACHINE, keyServiceGroupOrder, valList, Buf[0], DataSize);
    if DataSize > 0 then
    begin
      pch := @Buf[0];
      while pch^ <> #0 do
      begin
        AddGroup(TJclServiceGroup.Create(Self, pch, GetGroupCount));
        Inc(pch, StrLen(pch) + 1);
      end;
    end;
  end;

  procedure RefreshAllServices;
  var
    I: Integer;
  begin
    for I := 0 to GetServiceCount - 1 do
      GetService(I).Refresh;
  end;

begin
  Active := True;
  if RefreshAll then
  begin
    Clear;
    EnumServiceGroups;
    EnumServices;
  end;
  RefreshAllServices;
end;

//--------------------------------------------------------------------------------------------------

function ServiceSortFunc(Item1, Item2: Pointer): Integer;
var
  Svc1, Svc2: TJclNtService;
begin
  Svc1 := Item1;
  Svc2 := Item2;
  case Svc1.SCManager.m_sotOrderType of
    sotServiceName:
      Result := AnsiCompareStr(Svc1.ServiceName, Svc2.ServiceName);
    sotDisplayName:
      Result := AnsiCompareStr(Svc1.DisplayName, Svc2.DisplayName);
    sotDescription:
      Result := AnsiCompareStr(Svc1.Description, Svc2.Description);
    sotFileName:
      Result := AnsiCompareStr(Svc1.FileName, Svc2.FileName);
    sotServiceState:
      Result := Integer(Svc1.ServiceState) - Integer(Svc2.ServiceState);
    sotStartType:
      Result := Integer(Svc1.StartType) - Integer(Svc2.StartType);
    sotErrorControlType:
      Result := Integer(Svc1.ErrorControlType) - Integer(Svc2.ErrorControlType);
    sotLoadOrderGroup:
      Result := Svc1.Group.Order - Svc2.Group.Order;
    sotWin32ExitCode:
      Result := Svc1.Win32ExitCode - Svc2.Win32ExitCode;
  else
    Result := 0;
  end;
  if not Svc1.SCManager.m_fOrderAsc and (Result <> 0) then
    Result := -Result;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclSCManager.Sort(const AOrderType: TJclServiceSortOrderType; const AOrderAsc: Boolean);
begin
  m_sotOrderType := AOrderType;
  m_fOrderAsc    := AOrderAsc;
  FServices.Sort(ServiceSortFunc);
end;

//--------------------------------------------------------------------------------------------------

function TJclSCManager.FindService(const SvcName: string; var NtSvc: TJclNtService): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to GetServiceCount - 1 do
  begin
    NtSvc := GetService(I);
    if CompareText(NtSvc.ServiceName, SvcName) = 0 then
    begin
      Result := True;
      Break;
    end;
  end;
  if not Result then
    NtSvc := nil;
end;

//--------------------------------------------------------------------------------------------------

function TJclSCManager.FindGroup(const GrpName: string; var SvcGrp: TJclServiceGroup;
  const AutoAdd: Boolean): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to GetGroupCount - 1 do
  begin
    if CompareText(GetGroup(I).Name, GrpName) = 0 then
    begin
      SvcGrp := GetGroup(I);
      Result := True;
      Exit;
    end;
  end;
  if AutoAdd then
  begin
    SvcGrp := TJclServiceGroup.Create(Self, GrpName, GetGroupCount);
    AddGroup(SvcGrp);
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclSCManager.GetServiceLockStatus: PQueryServiceLockStatus;
var
  dwBytesNeeded: DWORD;
begin
  Assert((DesiredAccess and SC_MANAGER_QUERY_LOCK_STATUS) <> 0);
  Active := True;
  Result := nil;
  QueryServiceLockStatus(FHandle, PQueryServiceLockStatus(nil)^, 0, dwBytesNeeded);
  if ERROR_INSUFFICIENT_BUFFER = GetLastError then
  begin
    GetMem(Result, dwBytesNeeded);
    Win32Check(QueryServiceLockStatus(FHandle, Result^, dwBytesNeeded, dwBytesNeeded));
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclSCManager.IsLocked: Boolean;
var
  pQsls: PQueryServiceLockStatus;
begin
  pQsls := GetServiceLockStatus;
  Result := Assigned(pQsls) and (pQsls.fIsLocked <> 0);
  FreeMem(pQsls);
end;

//--------------------------------------------------------------------------------------------------

function TJclSCManager.LockOwner: string;
var
  pQsls: PQueryServiceLockStatus;
begin
  pQsls := GetServiceLockStatus;
  if Assigned(pQsls) then
    Result := pQsls.lpLockOwner
  else
    Result := '';
  FreeMem(pQsls);
end;

//--------------------------------------------------------------------------------------------------

function TJclSCManager.LockDuration: DWORD;
var
  pQsls: PQueryServiceLockStatus;
begin
  pQsls := GetServiceLockStatus;
  if Assigned(pQsls) then
    Result := pQsls.dwLockDuration
  else
    Result := INFINITE;
  FreeMem(pQsls);
end;

//--------------------------------------------------------------------------------------------------

class function TJclSCManager.ServiceType(const SvcType: TJclServiceTypes): DWORD;
var
  AType: TJclServiceType;
begin
  Result := 0;
  for AType := Low(TJclServiceType) to High(TJclServiceType) do
    if AType in SvcType then
      Result := Result or ServiceTypeMapping[AType];
end;

//--------------------------------------------------------------------------------------------------

class function TJclSCManager.ServiceType(const SvcType: DWORD): TJclServiceTypes;
var
  AType: TJclServiceType;
begin
  Result := [];
  for AType := Low(TJclServiceType) to High(TJclServiceType) do
    if (SvcType and ServiceTypeMapping[AType]) <> 0 then
      Include(Result, AType);
end;

//--------------------------------------------------------------------------------------------------

{$IFDEF USE_STATE_MAPPING}
class function TJclSCManager.ServiceState(const SvcState: TJclServiceState): DWORD;
begin
  Result := ServiceStateMapping[SvcState];
end;

//--------------------------------------------------------------------------------------------------

class function TJclSCManager.ServiceState(const SvcState: DWORD): TJclServiceState;
var
  AState: TJclServiceState;
begin
  for AState := Low(TJclServiceState) to High(TJclServiceState) do
    if SvcState = ServiceStateMapping[AState] then
    begin
      Result := AState;
      Exit;
    end;
  raise EJclError.CreateResRecFmt(@RsInvalidSvcState, [SvcState]);
end;
{$ENDIF}

//--------------------------------------------------------------------------------------------------

class function TJclSCManager.ControlAccepted(const CtrlAccepted: TJclServiceControlAccepteds): DWORD;
var
  ACtrl: TJclServiceControlAccepted;
begin
  Result := 0;
  for ACtrl := Low(TJclServiceControlAccepted) to High(TJclServiceControlAccepted) do
    if ACtrl in CtrlAccepted then
      Result := Result or ServiceControlAcceptedMapping[ACtrl];
end;

//--------------------------------------------------------------------------------------------------

class function TJclSCManager.ControlAccepted(const CtrlAccepted: DWORD): TJclServiceControlAccepteds;
var
  ACtrl: TJclServiceControlAccepted;
begin
  Result := [];
  for ACtrl := Low(TJclServiceControlAccepted) to High(TJclServiceControlAccepted) do
    if (CtrlAccepted and ServiceControlAcceptedMapping[ACtrl]) <> 0 then
      Include(Result, ACtrl);
end;

//--------------------------------------------------------------------------------------------------

end.
