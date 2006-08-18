{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL) extension                                    }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is ThreadExpertSharedNames.pas.                            }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: July 15, 2001                                                 }
{                                                                              }
{******************************************************************************}

unit ThreadExpertSharedNames;

{$I JCL.INC}

interface

uses
  Windows, SysUtils, Classes,
  JclBase, JclFileUtils, JclSynch;

type
  TSharedThreadNames = class(TObject)
  private
    FIdeMode: Boolean;
    FMapping: TJclSwapFileMapping;
    FMutex: TJclMutex;
    FNotifyEvent: TJclEvent;
    FProcessID: DWORD;
    FView: TJclFileMappingView;
    function GetThreadName(ThreadID: DWORD): string;
    procedure InternalRegisterThread(ThreadID: DWORD; const ThreadName: string; UpdateOnly: Boolean);
    procedure SetThreadName(ThreadID: DWORD; const Value: string);
  protected
    procedure EnterMutex;
  public
    constructor Create(IdeMode: Boolean);
    destructor Destroy; override;
    procedure Cleanup(ProcessID: DWORD);
    class function Exists: Boolean;
    procedure RegisterThread(ThreadID: DWORD; const ThreadName: string);
    procedure UnregisterThread(ThreadID: DWORD);
    property ThreadName[ThreadID: DWORD]: string read GetThreadName write SetThreadName; default;
    property NotifyEvent: TJclEvent read FNotifyEvent;
  end;

implementation

uses
  JclSysUtils;

resourcestring
  RsEnterMutexTimeout = 'JCL Thread Name IDE Expert Mutex Timeout';

const
  MaxThreadCount       = 512;
  IdeEnterMutexTimeout = 10000;

  MutexName   = 'DebugThreadNamesMutex';
  MappingName = 'DebugThreadNamesMapping';
  EventName   = 'DebugThreadNamesEvent';

type
  TThreadName = record
    ThreadID: DWORD;
    ProcessID: DWORD;
    ThreadName: ShortString;
  end;

  PThreadNames = ^TThreadNames;
  TThreadNames = record
    Count: Integer;
    Threads: array[0..MaxThreadCount - 1] of TThreadName;
  end;

//==============================================================================
// TSharedThreadNames 
//==============================================================================

procedure TSharedThreadNames.Cleanup(ProcessID: DWORD);
var
  I: Integer;
begin
  EnterMutex;
  try
    with PThreadNames(FView.Memory)^ do
      for I := Low(Threads) to High(Threads) do
        with Threads[I] do
          if ProcessID = ProcessID then
          begin
            ProcessID := 0;
            ThreadID := 0;
            ThreadName := '';
          end;
  finally
    FMutex.Release;
  end;
end;

//------------------------------------------------------------------------------

constructor TSharedThreadNames.Create(IdeMode: Boolean);
begin
  FIdeMode := IdeMode;
  FMutex := TJclMutex.Create(nil, False, MutexName);
  FMapping := TJclSwapFileMapping.Create(MappingName, PAGE_READWRITE, SizeOf(TThreadNames), nil);
  FView := TJclFileMappingView.Create(FMapping, FILE_MAP_ALL_ACCESS, 0, 0);
  FNotifyEvent := TJclEvent.Create(nil, False, False, EventName);
  FProcessID := GetCurrentProcessId;
end;

//------------------------------------------------------------------------------

destructor TSharedThreadNames.Destroy;
begin
  Cleanup(FProcessID);
  FreeAndNil(FMapping);
  FreeAndNil(FMutex);
  FreeAndNil(FNotifyEvent);
  inherited;
end;

//------------------------------------------------------------------------------

procedure TSharedThreadNames.EnterMutex;
begin
  if FIdeMode then
  begin
    if FMutex.WaitFor(IdeEnterMutexTimeout) = wrTimeout then
      raise Exception.Create(RsEnterMutexTimeout);
  end
  else
    FMutex.WaitForever;
end;

//------------------------------------------------------------------------------

class function TSharedThreadNames.Exists: Boolean;
var
  H: THandle;
begin
  H := OpenMutex(MUTEX_ALL_ACCESS, False, PChar(MutexName));
  Result := (H <> 0);
  if Result then
    CloseHandle(H);
end;

//------------------------------------------------------------------------------

function TSharedThreadNames.GetThreadName(ThreadID: DWORD): string;
var
  I: Integer;
begin
  Result := '';
  EnterMutex;
  try
    with PThreadNames(FView.Memory)^ do
      for I := Low(Threads) to High(Threads) do
        if Threads[I].ThreadID = ThreadID then
        begin
          Result := Threads[I].ThreadName;
          Break;
        end;
  finally
    FMutex.Release;
  end;
end;

//------------------------------------------------------------------------------

procedure TSharedThreadNames.InternalRegisterThread(ThreadID: DWORD; const ThreadName: string; UpdateOnly: Boolean);
var
  I, Slot: Integer;
  NeedNotify: Boolean;
begin
  EnterMutex;
  try
    Slot := -1;
    NeedNotify := ThreadID = MainThreadID;
    with PThreadNames(FView.Memory)^ do
    begin
      for I := Low(Threads) to High(Threads) do
        if Threads[I].ThreadID = ThreadID then
        begin
          Slot := I;
          NeedNotify := True;
          Break;
        end
        else
        if (not UpdateOnly) and (Slot = -1) and (Threads[I].ThreadID = 0) then
          Slot := I;
      if Slot <> -1 then
      begin
        Threads[Slot].ProcessID := FProcessID;
        Threads[Slot].ThreadID := ThreadID;
        Threads[Slot].ThreadName := ThreadName;
      end;
    end;
    if NeedNotify then
      FNotifyEvent.SetEvent;
  finally
    FMutex.Release;
  end;
end;

//------------------------------------------------------------------------------

procedure TSharedThreadNames.RegisterThread(ThreadID: DWORD; const ThreadName: string);
begin
  InternalRegisterThread(ThreadID, ThreadName, False);
end;

//------------------------------------------------------------------------------

procedure TSharedThreadNames.SetThreadName(ThreadID: DWORD; const Value: string);
begin
  InternalRegisterThread(ThreadID, Value, True);
end;

//------------------------------------------------------------------------------

procedure TSharedThreadNames.UnregisterThread(ThreadID: DWORD);
var
  I: Integer;
begin
  EnterMutex;
  try
    with PThreadNames(FView.Memory)^ do
      for I := Low(Threads) to High(Threads) do
        if Threads[I].ThreadID = ThreadID then
        begin
          Threads[I].ProcessID := 0;
          Threads[I].ThreadID := 0;
          Threads[I].ThreadName := '';
          Break;
        end;
  finally
    FMutex.Release;
  end;
end;

//------------------------------------------------------------------------------

end.
