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
{ The Original Code is JclAppInst.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) Petr Vones. All Rights Reserved.                                                   }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Marcel van Brakel                                                                              }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains a class and support routines for controlling the number of concurrent         }
{ instances of your application that can exist at any time. In addition there is support for       }
{ simple interprocess communication between these instance including a notification mechanism.     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclAppInst;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Classes, Messages,
  JclFileUtils, JclSynch;

// Message constants and types
type
  TJclAppInstDataKind = Integer;

const
  AI_INSTANCECREATED = $0001;
  AI_INSTANCEDESTROYED = $0002;
  AI_USERMSG = $0003;

  AppInstDataKindNoData = -1;
  AppInstCmdLineDataKind = 1;

// Application instances manager class
type
  TJclAppInstances = class(TObject)
  private
    FCPID: DWORD;
    FMapping: TJclSwapFileMapping;
    FMappingView: TJclFileMappingView;
    FMessageID: DWORD;
    FOptex: TJclOptex;
    function GetAppWnds(Index: Integer): THandle;
    function GetInstanceCount: Integer;
    function GetProcessIDs(Index: Integer): DWORD;
    function GetInstanceIndex(ProcessID: DWORD): Integer;
  protected
    procedure InitData;
    procedure NotifyInstances(const W, L: Longint);
    procedure RemoveInstance;
  public
    constructor Create;
    destructor Destroy; override;
    class function BringAppWindowToFront(const Wnd: THandle): Boolean;
    class function GetApplicationWnd(const ProcessID: DWORD): THandle;
    class procedure KillInstance;
    class function SetForegroundWindow98(const Wnd: THandle): Boolean;
    function CheckInstance(const MaxInstances: Word): Boolean;
    procedure CheckMultipleInstances(const MaxInstances: Word);
    procedure CheckSingleInstance;
    function SendCmdLineParams(const WindowClassName: string; const OriginatorWnd: THandle): Boolean;
    function SendData(const WindowClassName: string; const DataKind: TJclAppInstDataKind;
      Data: Pointer; const Size: Integer;
      OriginatorWnd: THandle): Boolean;
    function SendString(const WindowClassName: string; const DataKind: TJclAppInstDataKind;
      const S: string; OriginatorWnd: THandle): Boolean;
    function SendStrings(const WindowClassName: string; const DataKind: TJclAppInstDataKind;
      const Strings: TStrings; OriginatorWnd: THandle): Boolean;
    function SwitchTo(const Index: Integer): Boolean;
    procedure UserNotify(const Param: Longint);
    property AppWnds[Index: Integer]: THandle read GetAppWnds;
    property InstanceIndex[ProcessID: DWORD]: Integer read GetInstanceIndex;
    property InstanceCount: Integer read GetInstanceCount;
    property MessageID: DWORD read FMessageID;
    property ProcessIDs[Index: Integer]: DWORD read GetProcessIDs;
  end;

function JclAppInstances: TJclAppInstances; overload;
function JclAppInstances(const UniqueAppIdGuidStr: string): TJclAppInstances; overload;

// Interprocess communication routines
function ReadMessageCheck(var Message: TMessage; const IgnoredOriginatorWnd: THandle): TJclAppInstDataKind;
procedure ReadMessageData(const Message: TMessage; var Data: Pointer; var Size: Integer);
procedure ReadMessageString(const Message: TMessage; out S: string);
procedure ReadMessageStrings(const Message: TMessage; const Strings: TStrings);

function SendData(const Wnd, OriginatorWnd: HWND;
  const DataKind: TJclAppInstDataKind; const Data: Pointer; const Size: Integer): Boolean;
function SendStrings(const Wnd, OriginatorWnd: HWND;
  const DataKind: TJclAppInstDataKind; const Strings: TStrings): Boolean;
function SendCmdLineParams(const Wnd, OriginatorWnd: HWND): Boolean;
function SendString(const Wnd, OriginatorWnd: HWND;
  const DataKind: TJclAppInstDataKind; const S: string): Boolean;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\source\windows';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils,
  JclStrings;

{$IFDEF FPC}  // missing declaration from unit Messages
type
  TWMCopyData = record
      Msg: UINT;
      From: THandle;
      CopyDataStruct: PCopyDataStruct;
      Result : LRESULT;
    End;
{$ENDIF FPC}

const
  { strings to form a unique name for file mapping and optex objects }
  JclAIPrefix = 'Jcl';
  JclAIOptex = '_Otx';
  JclAIMapping = '_Map';

  { window message used for communication between instances }
  JclAIMessage = '_Msg';

  { maximum number of instance that may exist at any time }
  JclAIMaxInstances = 256;

  { name of the application window class }
  ClassNameOfTApplication = 'TApplication';

type
  { management data to keep track of application instances. this data is shared amongst all instances
    and must be appropriately protected from concurrent access at all time }

  PJclAISharedData = ^TJclAISharedData;
  TJclAISharedData = packed record
    MaxInst: Word;
    Count: Word;
    ProcessIDs: array [0..JclAIMaxInstances] of DWORD;
  end;

var
  { the single global TJclAppInstance instance }
  AppInstances: TJclAppInstances;
  ExplicitUniqueAppId: string;

//=== { TJclAppInstances } ===================================================

constructor TJclAppInstances.Create;
begin
  inherited Create;
  FCPID := GetCurrentProcessId;
  InitData;
end;

destructor TJclAppInstances.Destroy;
begin
  if (FMapping <> nil) and (FOptex <> nil) then
    RemoveInstance;
  FreeAndNil(FMapping);
  FreeAndNil(FOptex);
  inherited Destroy;
end;

class function TJclAppInstances.BringAppWindowToFront(const Wnd: THandle): Boolean;
begin
  if IsIconic(Wnd) then
    SendMessage(Wnd, WM_SYSCOMMAND, SC_RESTORE, 0);
  Result := SetForegroundWindow98(Wnd);
end;

function TJclAppInstances.CheckInstance(const MaxInstances: Word): Boolean;
var
  SharedData: PJclAISharedData;
begin
  FOptex.Enter;
  try
    SharedData := PJclAISharedData(FMappingView.Memory);
    if SharedData^.MaxInst = 0 then
      SharedData^.MaxInst := MaxInstances;
    Result := SharedData^.Count < SharedData^.MaxInst;
    SharedData^.ProcessIDs[SharedData^.Count] := GetCurrentProcessId;
    Inc(SharedData^.Count);
  finally
    FOptex.Leave;
  end;
  if Result then
    NotifyInstances(AI_INSTANCECREATED, Integer(FCPID));
end;

procedure TJclAppInstances.CheckMultipleInstances(const MaxInstances: Word);
begin
  if not CheckInstance(MaxInstances) then
  begin
    SwitchTo(0);
    KillInstance;
  end;
end;

procedure TJclAppInstances.CheckSingleInstance;
begin
  CheckMultipleInstances(1);
end;

type
  PTopLevelWnd = ^TTopLevelWnd;
  TTopLevelWnd = record
    ProcessID: DWORD;
    Wnd: THandle;
  end;

function EnumApplicationWinProc(Wnd: THandle; Param: PTopLevelWnd): BOOL; stdcall;
var
  PID: DWORD;
  C: array [0..Length(ClassNameOfTApplication) + 1] of Char;
begin
  GetWindowThreadProcessId(Wnd, @PID);
  if (PID = Param^.ProcessID) and (GetClassName(Wnd, C, Length(C)) > 0) and (C = ClassNameOfTApplication) then
  begin
    Result := False;
    Param^.Wnd := Wnd;
  end
  else
  begin
    Result := True;
  end;
end;

class function TJclAppInstances.GetApplicationWnd(const ProcessID: DWORD): THandle;
var
  TopLevelWnd: TTopLevelWnd;
begin
  TopLevelWnd.ProcessID := ProcessID;
  TopLevelWnd.Wnd := 0;
  EnumWindows(@EnumApplicationWinProc, LPARAM(@TopLevelWnd));
  Result := TopLevelWnd.Wnd;
end;

function TJclAppInstances.GetAppWnds(Index: Integer): THandle;
begin
  Result := GetApplicationWnd(GetProcessIDs(Index));
end;

function TJclAppInstances.GetInstanceCount: Integer;
begin
  FOptex.Enter;
  try
    Result := PJclAISharedData(FMappingView.Memory)^.Count;
  finally
    FOptex.Leave;
  end;
end;

function TJclAppInstances.GetInstanceIndex(ProcessID: DWORD): Integer;
var
  I: Integer;
  SharedData: PJclAISharedData;
begin
  Result := -1;
  FOptex.Enter;
  try
    SharedData := PJclAISharedData(FMappingView.Memory);
    for I := 0 to SharedData^.Count - 1 do
      if SharedData^.ProcessIDs[I] = ProcessID then
      begin
        Result := I;
        Break;
      end;
  finally
    FOptex.Leave;
  end;
end;

function TJclAppInstances.GetProcessIDs(Index: Integer): DWORD;
var
  SharedData: PJclAISharedData;
begin
  FOptex.Enter;
  try
    SharedData := PJclAISharedData(FMappingView.Memory);
    if Index >= SharedData^.Count then
      Result := 0
    else
      Result := SharedData^.ProcessIDs[Index];
  finally
    FOptex.Leave;
  end;
end;

procedure TJclAppInstances.InitData;
var
  UniqueAppID: string;
begin
  if ExplicitUniqueAppId <> '' then
    UniqueAppID := JclAIPrefix + ExplicitUniqueAppId
  else
    UniqueAppID := AnsiUpperCase(JclAIPrefix + ParamStr(0));
  CharReplace(UniqueAppID, '\', '_');
  FOptex := TJclOptex.Create(UniqueAppID + JclAIOptex, 4000);
  FOptex.Enter;
  try
    FMapping := TJclSwapFileMapping.Create(UniqueAppID + JclAIMapping,
      PAGE_READWRITE, SizeOf(TJclAISharedData), nil);
    FMappingView := FMapping.Views[FMapping.Add(FILE_MAP_ALL_ACCESS, SizeOf(TJclAISharedData), 0)];
    if not FMapping.Existed then
      FillChar(FMappingView.Memory^, SizeOf(TJclAISharedData), #0);
  finally
    FOptex.Leave;
  end;
  FMessageID := RegisterWindowMessage(PChar(UniqueAppID + JclAIMessage));
end;

class procedure TJclAppInstances.KillInstance;
begin
  Halt(0);
end;

function EnumNotifyWinProc(Wnd: THandle; Message: PMessage): BOOL; stdcall;
begin
  SendNotifyMessage(Wnd, Message^.Msg, Message^.WParam, Message^.LParam);
  Result := True;
end;

procedure TJclAppInstances.NotifyInstances(const W, L: Integer);
var
  I: Integer;
  Wnd: THandle;
  TID: DWORD;
  Msg: TMessage;
  SharedData: PJclAISharedData;
begin
  FOptex.Enter;
  try
    SharedData := PJclAISharedData(FMappingView.Memory);
    for I := 0 to SharedData^.Count - 1 do
    begin
      Wnd := GetApplicationWnd(SharedData^.ProcessIDs[I]);
      TID := GetWindowThreadProcessId(Wnd, nil);
      while Wnd <> 0 do
      begin // Send message to TApplication queue
        if PostThreadMessage(TID, FMessageID, W, L) or
          (GetLastError = ERROR_INVALID_THREAD_ID) then
          Break;
        Sleep(1);
      end;
      Msg.Msg := FMessageID;
      Msg.WParam := W;
      Msg.LParam := L;
      EnumThreadWindows(TID, @EnumNotifyWinProc, LPARAM(@Msg));
    end;
  finally
    FOptex.Leave;
  end;
end;

procedure TJclAppInstances.RemoveInstance;
var
  I: Integer;
  SharedData: PJclAISharedData;
begin
  FOptex.Enter;
  try
    SharedData := PJclAISharedData(FMappingView.Memory);
    for I := 0 to SharedData^.Count - 1 do
      if SharedData^.ProcessIDs[I] = FCPID then
      begin
        SharedData^.ProcessIDs[I] := 0;
        Move(SharedData^.ProcessIDs[I + 1], SharedData^.ProcessIDs[I], (SharedData^.Count - I) * SizeOf(DWORD));
        Dec(SharedData^.Count);
        Break;
      end;
  finally
    FOptex.Leave;
  end;
  NotifyInstances(AI_INSTANCEDESTROYED, Integer(FCPID));
end;

function TJclAppInstances.SendCmdLineParams(const WindowClassName: string; const OriginatorWnd: THandle): Boolean;
var
  TempList: TStringList;
  I: Integer;
begin
  TempList := TStringList.Create;
  try
    for I := 1 to ParamCount do
      TempList.Add(ParamStr(I));
    Result := SendStrings(WindowClassName, AppInstCmdLineDataKind, TempList, OriginatorWnd);
  finally
    TempList.Free;
  end;
end;

type
  PEnumWinRec = ^TEnumWinRec;
  TEnumWinRec = record
    WindowClassName: PChar;
    OriginatorWnd: THandle;
    CopyData: TCopyDataStruct;
    Self: TJclAppInstances;
  end;

function EnumWinProc(Wnd: THandle; Data: PEnumWinRec): BOOL; stdcall;
var
  ClassName: array [0..200] of Char;
  I: Integer;
  PID: DWORD;
  Found: Boolean;
  SharedData: PJclAISharedData;
begin
  if (GetClassName(Wnd, ClassName, Length(ClassName) - 1) > 0) and
    (StrComp(ClassName, Data.WindowClassName) = 0) then
  begin
    GetWindowThreadProcessId(Wnd, @PID);
    Found := False;
    Data.Self.FOptex.Enter;
    try
      SharedData := PJclAISharedData(Data.Self.FMappingView.Memory);
      for I := 0 to SharedData^.Count - 1 do
        if SharedData^.ProcessIDs[I] = PID then
        begin
          Found := True;
          Break;
        end;
    finally
      Data.Self.FOptex.Leave;
    end;
    if Found then
      SendMessage(Wnd, WM_COPYDATA, Data.OriginatorWnd, LPARAM(@Data.CopyData));
  end;
  Result := True;
end;

function TJclAppInstances.SendData(const WindowClassName: string;
  const DataKind: TJclAppInstDataKind;
  Data: Pointer; const Size: Integer;
  OriginatorWnd: THandle): Boolean;
var
  EnumWinRec: TEnumWinRec;
begin
  Assert(DataKind <> AppInstDataKindNoData);
  EnumWinRec.WindowClassName := PChar(WindowClassName);
  EnumWinRec.OriginatorWnd := OriginatorWnd;
  EnumWinRec.CopyData.dwData := DataKind;
  EnumWinRec.CopyData.cbData := Size;
  EnumWinRec.CopyData.lpData := Data;
  EnumWinRec.Self := Self;
  Result := EnumWindows(@EnumWinProc, LPARAM(@EnumWinRec));
end;

function TJclAppInstances.SendString(const WindowClassName: string;
  const DataKind: TJclAppInstDataKind; const S: string;
  OriginatorWnd: THandle): Boolean;
begin
  Result := SendData(WindowClassName, DataKind, PChar(S), Length(S) * SizeOf(Char), OriginatorWnd);
end;

function TJclAppInstances.SendStrings(const WindowClassName: string;
  const DataKind: TJclAppInstDataKind; const Strings: TStrings;
  OriginatorWnd: THandle): Boolean;
begin
  Result := SendString(WindowClassName, DataKind, Strings.Text, OriginatorWnd);
end;

class function TJclAppInstances.SetForegroundWindow98(const Wnd: THandle): Boolean;
var
  ForeThreadID, NewThreadID: DWORD;
begin
  if GetForegroundWindow <> Wnd then
  begin
    ForeThreadID := GetWindowThreadProcessId(GetForegroundWindow, nil);
    NewThreadID := GetWindowThreadProcessId(Wnd, nil);
    if ForeThreadID <> NewThreadID then
    begin
      AttachThreadInput(ForeThreadID, NewThreadID, True);
      Result := SetForegroundWindow(Wnd);
      AttachThreadInput(ForeThreadID, NewThreadID, False);
      if Result then
        Result := SetForegroundWindow(Wnd);
    end
    else
      Result := SetForegroundWindow(Wnd);
  end
  else
    Result := True;
end;

function TJclAppInstances.SwitchTo(const Index: Integer): Boolean;
begin
  Result := BringAppWindowToFront(AppWnds[Index]);
end;

procedure TJclAppInstances.UserNotify(const Param: Integer);
begin
  NotifyInstances(AI_USERMSG, Param);
end;

function JclAppInstances: TJclAppInstances;
begin
  if AppInstances = nil then
    AppInstances := TJclAppInstances.Create;
  Result := AppInstances;
end;

function JclAppInstances(const UniqueAppIdGuidStr: string): TJclAppInstances;
begin
  Assert(AppInstances = nil);
  ExplicitUniqueAppId := UniqueAppIdGuidStr;
  Result := JclAppInstances;
end;

// Interprocess communication routines
function ReadMessageCheck(var Message: TMessage; const IgnoredOriginatorWnd: THandle): TJclAppInstDataKind;
begin
  if (Message.Msg = WM_COPYDATA) and (TWMCopyData(Message).From <> IgnoredOriginatorWnd) then
  begin
    Message.Result := 1;
    Result := TJclAppInstDataKind(TWMCopyData(Message).CopyDataStruct^.dwData);
  end
  else
  begin
    Message.Result := 0;
    Result := AppInstDataKindNoData;
  end;
end;

procedure ReadMessageData(const Message: TMessage; var Data: Pointer; var Size: Integer);
begin
  if TWMCopyData(Message).Msg = WM_COPYDATA then
  begin
    Size := TWMCopyData(Message).CopyDataStruct^.cbData;
    GetMem(Data, Size);
    Move(TWMCopyData(Message).CopyDataStruct^.lpData^, Data^, Size);
  end;
end;

procedure ReadMessageString(const Message: TMessage; out S: string);
begin
  if TWMCopyData(Message).Msg = WM_COPYDATA then
    SetString(S, PChar(TWMCopyData(Message).CopyDataStruct^.lpData), TWMCopyData(Message).CopyDataStruct^.cbData div SizeOf(Char));
end;

procedure ReadMessageStrings(const Message: TMessage; const Strings: TStrings);
var
  S: string;
begin
  if TWMCopyData(Message).Msg = WM_COPYDATA then
  begin
    ReadMessageString(Message, S);
    Strings.Text := S;
  end;
end;

function SendData(const Wnd, OriginatorWnd: HWND;
  const DataKind: TJclAppInstDataKind; const Data: Pointer; const Size: Integer): Boolean;
var
  CopyData: TCopyDataStruct;
begin
  CopyData.dwData := DataKind;
  CopyData.cbData := Size;
  CopyData.lpData := Data;
  Result := Boolean(SendMessage(Wnd, WM_COPYDATA, OriginatorWnd, LPARAM(@CopyData)));
end;

function SendStrings(const Wnd, OriginatorWnd: HWND;
  const DataKind: TJclAppInstDataKind; const Strings: TStrings): Boolean;
begin
  Result := SendString(Wnd, OriginatorWnd, DataKind, Strings.Text);
end;

function SendCmdLineParams(const Wnd, OriginatorWnd: HWND): Boolean;
var
  TempList: TStringList;
  I: Integer;
begin
  TempList := TStringList.Create;
  try
    for I := 1 to ParamCount do
      TempList.Add(ParamStr(I));
    Result := SendStrings(Wnd, OriginatorWnd, AppInstCmdLineDataKind, TempList);
  finally
    TempList.Free;
  end;
end;

function SendString(const Wnd, OriginatorWnd: HWND;
  const DataKind: TJclAppInstDataKind; const S: string): Boolean;
begin
  Result := SendData(Wnd, OriginatorWnd, DataKind, PChar(S), Length(S) * SizeOf(Char));
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  FreeAndNil(AppInstances);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
