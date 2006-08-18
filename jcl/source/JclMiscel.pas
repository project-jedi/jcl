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
{ The Original Code is JclMiscel.pas.                                          }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2000 of these individuals.                                                   }
{                                                                              }
{******************************************************************************}
{                                                                              }
{ Various miscellanuous routines that do not (yet) fit nicely into other units }
{                                                                              }
{ Unit owner: Jeroen Speldekamp                                                }
{ Last modified: January 29, 2000                                              }
{                                                                              }
{******************************************************************************}

unit JclMiscel;

{$I JCL.INC}

{$WEAKPACKAGEUNIT ON}

interface

uses
  Windows,
  JclBase;

//------------------------------------------------------------------------------
// StrLstLoadSave
//------------------------------------------------------------------------------

const
  HKCR: HKEY = HKEY_CLASSES_ROOT;
  HKCU: HKEY = HKEY_CURRENT_USER;
  HKLM: HKEY = HKEY_LOCAL_MACHINE;
  HKUS: HKEY = HKEY_USERS;
  HKCC: HKEY = HKEY_CURRENT_CONFIG;
  HKPD: HKEY = HKEY_PERFORMANCE_DATA;

function SetDisplayResolution(const XRes, YRes: DWORD): Longint;

function CreateDOSProcessRedirected(const CommandLine, InputFile, OutputFile: string): Boolean;
function WinExec32(const Cmd: string; const CmdShow: Integer): Boolean;
function WinExec32AndWait(const Cmd: string; const CmdShow: Integer): Cardinal;

//------------------------------------------------------------------------------
// CreateProcAsUser
//------------------------------------------------------------------------------

type
  EJclCreateProcessError = class (EJclWin32Error);

procedure CreateProcAsUser(const UserDomain, UserName, PassWord, CommandLine: string);
procedure CreateProcAsUserEx(const UserDomain, UserName, Password, CommandLine: string;
  const Environment: PChar);

implementation

uses
  SysUtils,
  JclResources, JclSecurity, JclWin32;

//==============================================================================

function CreateDOSProcessRedirected(const CommandLine, InputFile, OutputFile: string): Boolean;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SecAtrrs: TSecurityAttributes;
  hInputFile, hOutputFile: THandle;
begin
  Result := False;
  hInputFile := CreateFile(PChar(InputFile), GENERIC_READ, FILE_SHARE_READ,
    CreateInheritable(SecAtrrs), OPEN_EXISTING, FILE_ATTRIBUTE_TEMPORARY, 0);
  if hInputFile <> INVALID_HANDLE_VALUE then
  begin
    hOutputFile := CreateFile(PChar(OutPutFile), GENERIC_READ or GENERIC_WRITE,
      FILE_SHARE_READ, CreateInheritable(SecAtrrs), CREATE_ALWAYS,
      FILE_ATTRIBUTE_TEMPORARY, 0);
    if hOutputFile <> INVALID_HANDLE_VALUE then
    begin
      FillChar(StartupInfo, SizeOf(StartupInfo), #0);
      StartupInfo.cb := SizeOf(StartupInfo);
      StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      StartupInfo.wShowWindow := SW_HIDE;
      StartupInfo.hStdOutput := hOutputFile;
      StartupInfo.hStdInput := hInputFile;
      Result := CreateProcess(nil, PChar(CommandLine), nil, nil, True,
        CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo,
        ProcessInfo);
      if Result then
      begin
        WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
        CloseHandle(ProcessInfo.hProcess);
        CloseHandle(ProcessInfo.hThread);
      end;
      CloseHandle(hOutputFile);
    end;
    CloseHandle(hInputFile);
  end;
end;

//------------------------------------------------------------------------------

function WinExec32(const Cmd: string; const CmdShow: Integer): Boolean;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  FillChar(StartupInfo, SizeOf(TStartupInfo), #0);
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := CmdShow;
  Result := CreateProcess(nil, PChar(Cmd), nil, nil, False,
    NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInfo);
  if Result then
  begin
    WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ProcessInfo.hProcess);
  end;
end;

//------------------------------------------------------------------------------

function WinExec32AndWait(const Cmd: string; const CmdShow: Integer): Cardinal;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  Result := Cardinal($FFFFFFFF);
  FillChar(StartupInfo, SizeOf(TStartupInfo), #0);
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := CmdShow;
  if CreateProcess(nil, PChar(Cmd), nil, nil, False, NORMAL_PRIORITY_CLASS,
    nil, nil, StartupInfo, ProcessInfo) then
  begin
    WaitForInputIdle(ProcessInfo.hProcess, INFINITE);
    if WaitForSingleObject(ProcessInfo.hProcess, INFINITE) = WAIT_OBJECT_0 then
    begin
      {$IFDEF DELPHI3}
      if not GetExitCodeProcess(ProcessInfo.hProcess, Integer(Result)) then
      {$ELSE}
      if not GetExitCodeProcess(ProcessInfo.hProcess, Result) then
      {$ENDIF DELPHI3}
        Result := Cardinal($FFFFFFFF);
    end;
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ProcessInfo.hProcess);
  end;
end;

//------------------------------------------------------------------------------

function SetDisplayResolution(const XRes, YRes: DWORD): Longint;
var
  DevMode: TDeviceMode;
begin
  Result := DISP_CHANGE_FAILED;
  FillChar(DevMode, SizeOf(DevMode), #0);
  DevMode.dmSize := SizeOf(DevMode);
  if EnumDisplaySettings(nil, 0, DevMode) then
  begin
    DevMode.dmFields := DM_PELSWIDTH or DM_PELSHEIGHT;
    DevMode.dmPelsWidth := XRes;
    DevMode.dmPelsHeight := YRes;
    Result := ChangeDisplaySettings(DevMode, 0);
  end;
end;

//------------------------------------------------------------------------------

procedure CheckOSVersion;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then
    raise EJclError.CreateResRec(@RsCreateProcNTRequiredError);
  if Win32BuildNumber < 1057 then
    raise EJclError.CreateResRec(@RsCreateProcBuild1057Error);
end;

//------------------------------------------------------------------------------

procedure CreateProcAsUser(const UserDomain, UserName, PassWord, CommandLine: string);
begin
  CreateProcAsUserEx(UserDomain, UserName, Password, CommandLine, nil);
end;

//------------------------------------------------------------------------------

procedure CreateProcAsUserEx(const UserDomain, UserName, Password, CommandLine: string;
  const Environment: PChar);
const
  // default values for window stations and desktops
  CreateProcDEFWINSTATION = 'WinSta0';
  CreateProcDEFDESKTOP    = 'Default';
  CreateProcDOMUSERSEP    = '\';
var
  ConsoleTitle: string;
  Help: string;
  WinStaName: string;
  DesktopName: string;
  hUserToken: THandle;
  hWindowStation: HWINSTA;
  hDesktop: HDESK;
  StartUpInfo: TStartUpInfo;
  ProcInfo: TProcessInformation;
begin

  // Step 1: check for the correct OS version
  CheckOSVersion;

  // Step 2: logon as the specified user
  if not LogonUser(PChar(UserName), PChar(UserDomain), PChar(Password),
    LOGON32_LOGON_INTERACTIVE, LOGON32_PROVIDER_DEFAULT, hUserToken) then
  begin
    case GetLastError of
      ERROR_PRIVILEGE_NOT_HELD:
        raise EJclCreateProcessError.CreateResRecFmt(@RsCreateProcPrivilegeMissing,
          [GetPrivilegeDisplayName(SE_TCB_NAME), SE_TCB_NAME]);
      ERROR_LOGON_FAILURE:
        raise EJclCreateProcessError.CreateResRec(@RsCreateProcLogonUserError);
      ERROR_ACCESS_DENIED:
        raise EJclCreateProcessError.CreateResRec(@RsCreateProcAccessDenied);
    else
      raise EJclCreateProcessError.CreateResRec(@RsCreateProcLogonFailed);
    end;
  end;

  // Step 3: give the new user access to the current WindowStation and Desktop
  hWindowStation:= GetProcessWindowStation;
  WinStaName := GetUserObjectName(hWindowStation);
  if WinStaName = '' then
    WinStaName := CreateProcDEFWINSTATION;

  if not SetUserObjectFullAccess(hWindowStation) then
  begin
    CloseHandle(hUserToken);
    raise EJclCreateProcessError.CreateResRecFmt(@RsCreateProcSetStationSecurityError, [WinStaName]);
  end;

  hDesktop := GetThreadDesktop(GetCurrentThreadId);
  DesktopName := GetUserObjectName(hDesktop);
  if DesktopName = '' then
    DesktopName := CreateProcDEFDESKTOP;

  if not SetUserObjectFullAccess(hDesktop) then
  begin
    CloseHandle(hUserToken);
    raise EJclCreateProcessError.CreateResRecFmt(@RsCreateProcSetDesktopSecurityError, [DesktopName]);
  end;

  // Step 4: set the startup info for the new process
  ConsoleTitle := UserDomain + UserName;
  FillChar(StartUpInfo, SizeOf(StartUpInfo), #0);
  with StartUpInfo do
  begin
    cb:= SizeOf(StartUpInfo);
    lpTitle:= PChar(ConsoleTitle);
    Help := WinStaName + '\' + DeskTopName;
    lpDesktop:= PChar(Help);
  end;

  // Step 5: create the child process
  if not CreateProcessAsUser(hUserToken, nil, PChar(CommandLine),
    nil, nil, False, CREATE_NEW_CONSOLE or CREATE_NEW_PROCESS_GROUP,
    Environment, nil, StartUpInfo, ProcInfo) then
  begin
    case GetLastError of
      ERROR_PRIVILEGE_NOT_HELD:
        raise EJclCreateProcessError.CreateResRecFmt(@RsCreateProcPrivilegesMissing,
          [GetPrivilegeDisplayName(SE_ASSIGNPRIMARYTOKEN_NAME), SE_ASSIGNPRIMARYTOKEN_NAME,
          GetPrivilegeDisplayName(SE_INCREASE_QUOTA_NAME), SE_INCREASE_QUOTA_NAME]);
      ERROR_FILE_NOT_FOUND:
        raise EJclCreateProcessError.CreateResRecFmt(@RsCreateProcCommandNotFound, [CommandLine]);
      else
        raise EJclCreateProcessError.CreateResRec(@RsCreateProcFailed);
    end;
  end;

  // clean up
  CloseWindowStation(hWindowStation);
  CloseDesktop(hDesktop);
  CloseHandle(hUserToken);

  // if this code should be called although there has
  // been an exception during invocation of CreateProcessAsUser,
  // it will quite surely fail. you should make sure this doesn't happen.
  // (it shouldn't happen due to the use of exceptions in the above lines)
  CloseHandle(ProcInfo.hThread);
  CloseHandle(ProcInfo.hProcess);

end;

end.
