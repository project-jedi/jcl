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
{ The Original Code is ThreadExpertUnit.pas.                                                       }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Unit owner: Petr Vones                                                                           }
{ Last modified: March 17, 2002                                                                    }
{                                                                                                  }
{**************************************************************************************************}

unit ThreadExpertUnit;

{$I JCL.INC}

interface

uses
  Windows, Classes, SysUtils, ToolsAPI, ComCtrls, Dialogs, JclOtaUtils, ThreadExpertSharedNames,
  JclSynch;

type
  TNameChangeThread = class;

  TThreadsExpert = class(TJclOTAExpert)
  private
    DebuggerServices: IOTADebuggerServices;
    FProcessesCount: Integer;
    FNameChangeThread: TNameChangeThread;
    FNotifierIndex: Integer;
    FSharedThreadNames: TSharedThreadNames;
    FThreadsStatusListView: TListView;
    function GetThreadsStatusListView: TListView;
    function GetThreadsStatusListViewFound: Boolean;
    procedure ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    function UpdateItem(Item: TListItem): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateContent;
    property ProcessesCount: Integer read FProcessesCount;
    property ThreadsStatusListView: TListView read GetThreadsStatusListView;
    property ThreadsStatusListViewFound: Boolean read GetThreadsStatusListViewFound;
  end;

  TDebuggerNotifier = class(TNotifierObject, IOTADebuggerNotifier)
  private
    FExpert: TThreadsExpert;
  protected
    procedure BreakpointAdded(Breakpoint: IOTABreakpoint);
    procedure BreakpointDeleted(Breakpoint: IOTABreakpoint);
    procedure ProcessCreated(Process: IOTAProcess);
    procedure ProcessDestroyed(Process: IOTAProcess);
  public
    constructor Create(AExpert: TThreadsExpert);
  end;

  TNameChangeThread = class(TThread)
  private
    FExpert: TThreadsExpert;
    FNotifyEvent: TJclEvent;
    FTerminateEvent: THandle;
    procedure TryFindThreadsStatusListView;
    procedure UpdateRequest;
  protected
    procedure Execute; override;
  public
    constructor Create(AExpert: TThreadsExpert; ANotifyEvent: TJclEvent);
    destructor Destroy; override;
    procedure TerminateThread;
  end;

procedure Register;

implementation

uses
  Forms, Controls,
  JclSysUtils;

const
  ThreadsStatusListViewFindPeriod = 2000;
  ReadNameTimeout                 = 500;

//--------------------------------------------------------------------------------------------------

procedure Register;
begin
  RegisterPackageWizard(TThreadsExpert.Create);
end;

//==================================================================================================
// TThreadsExpert
//==================================================================================================

constructor TThreadsExpert.Create;
begin
  inherited;
  DebuggerServices := BorlandIDEServices as IOTADebuggerServices;
  FSharedThreadNames := TSharedThreadNames.Create(True);
  FNotifierIndex := DebuggerServices.AddNotifier(TDebuggerNotifier.Create(Self));
  FNameChangeThread := TNameChangeThread.Create(Self, FSharedThreadNames.NotifyEvent);
end;

//--------------------------------------------------------------------------------------------------

destructor TThreadsExpert.Destroy;
begin
  if FNotifierIndex <> -1 then
    DebuggerServices.RemoveNotifier(FNotifierIndex);
  if Assigned(FThreadsStatusListView) then
    FThreadsStatusListView.OnChange := nil;
  FNameChangeThread.TerminateThread;
  FreeAndNil(FNameChangeThread);
  FreeAndNil(FSharedThreadNames);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TThreadsExpert.GetThreadsStatusListView: TListView;
var
  I: Integer;
  F: TForm;
begin
  if FThreadsStatusListView = nil then
  begin
    F := nil;
    with Screen do
      for I := 0 to FormCount - 1 do
        if Forms[I].ClassName = 'TThreadStatus' then
        begin
          F := Forms[I];
          Break;
        end;
    if F <> nil then
      with F do
        for I := 0 to ControlCount -1 do
          if Controls[I] is TListView then
          begin
            FThreadsStatusListView := TListView(Controls[I]);
            Break;
          end;
    if FThreadsStatusListView <> nil then
      FThreadsStatusListView.OnChange := ListViewChange;
  end;
  Result := FThreadsStatusListView;
end;

//--------------------------------------------------------------------------------------------------

function TThreadsExpert.GetThreadsStatusListViewFound: Boolean;
begin
  Result := Assigned(FThreadsStatusListView);
end;

//--------------------------------------------------------------------------------------------------

procedure TThreadsExpert.ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  try
    if Change = ctText then
      UpdateItem(Item);
  except
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TThreadsExpert.UpdateContent;
var
  I: Integer;
begin
  try
    with ThreadsStatusListView do
    begin
      {Items.BeginUpdate;
      try}
        for I := 0 to Items.Count - 1 do
          if not UpdateItem(Items[I]) then
            Break;
      {finally
        Items.EndUpdate;
      end;}
    end;          
  except
  end;
end;

//--------------------------------------------------------------------------------------------------

var
  CaptionChanging: Boolean;

function TThreadsExpert.UpdateItem(Item: TListItem): Boolean;
var
  TID: DWORD;
  Caption, ThreadName: string;
begin
  Result := True;
  if CaptionChanging then
    Exit;
  Caption := Item.Caption;
  if (Length(Caption) >= 9) and (Caption[1] = '$') then
  begin
    Caption := Copy(Caption, 1, 9);
    TID := StrToInt(Caption);
    Result := FSharedThreadNames.ThreadNameTimoeut(TID, ReadNameTimeout, ThreadName);
    if Result then
    begin
      CaptionChanging := True;
      try
        Item.Caption := Format('%s  %s', [Caption, ThreadName]);
      finally
        CaptionChanging := False;
      end;
    end;
  end;
end;

//==================================================================================================
// TDebuggerNotifier
//==================================================================================================

procedure TDebuggerNotifier.BreakpointAdded(Breakpoint: IOTABreakpoint);
begin
end;

//--------------------------------------------------------------------------------------------------

procedure TDebuggerNotifier.BreakpointDeleted(Breakpoint: IOTABreakpoint);
begin
end;

//--------------------------------------------------------------------------------------------------

constructor TDebuggerNotifier.Create(AExpert: TThreadsExpert);
begin
  FExpert := AExpert;
end;

//--------------------------------------------------------------------------------------------------

procedure TDebuggerNotifier.ProcessCreated(Process: IOTAProcess);
begin
  FExpert.GetThreadsStatusListView;
  Inc(FExpert.FProcessesCount);
end;

//--------------------------------------------------------------------------------------------------

procedure TDebuggerNotifier.ProcessDestroyed(Process: IOTAProcess);
begin
  Dec(FExpert.FProcessesCount);
  FExpert.FSharedThreadNames.Cleanup(Process.ProcessId);
end;

//==================================================================================================
// TNameChangeThread
//==================================================================================================

constructor TNameChangeThread.Create(AExpert: TThreadsExpert; ANotifyEvent: TJclEvent);
begin
  inherited Create(True);
  Priority := tpLowest;
  FExpert := AExpert;
  FNotifyEvent := ANotifyEvent;
  FTerminateEvent := CreateEvent(nil, True, False, nil);
  Resume;
end;

//--------------------------------------------------------------------------------------------------

destructor TNameChangeThread.Destroy;
begin
  CloseHandle(FTerminateEvent);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

procedure TNameChangeThread.Execute;
var
  WaitHandles: array[0..1] of THandle;
  WaitTimeout: DWORD;
begin
  WaitHandles[0] := FTerminateEvent;
  WaitHandles[1] := FNotifyEvent.Handle;
  WaitTimeout := ThreadsStatusListViewFindPeriod;
  repeat
    case Windows.WaitForMultipleObjects(2, @WaitHandles, False, WaitTimeout) of
      WAIT_OBJECT_0:
        Break;
      WAIT_OBJECT_0 + 1:
        begin
          Synchronize(UpdateRequest);
          Sleep(30); // To prevent overload the IDE by many update requests
        end;
      WAIT_TIMEOUT:
        if FExpert.ProcessesCount > 0 then
        begin
          if not FExpert.ThreadsStatusListViewFound then
            Synchronize(TryFindThreadsStatusListView);
          if FExpert.ThreadsStatusListViewFound then
            WaitTimeout := INFINITE;
        end;
    end;
  until Terminated;
end;

//--------------------------------------------------------------------------------------------------

procedure TNameChangeThread.TerminateThread;
begin
  Terminate;
  SetEvent(FTerminateEvent);
  WaitFor;
end;

//--------------------------------------------------------------------------------------------------

procedure TNameChangeThread.TryFindThreadsStatusListView;
begin
  if FExpert.GetThreadsStatusListView <> nil then
    FExpert.UpdateContent;
end;

//--------------------------------------------------------------------------------------------------

procedure TNameChangeThread.UpdateRequest;
begin
  FExpert.UpdateContent;
end;

//--------------------------------------------------------------------------------------------------

end.
