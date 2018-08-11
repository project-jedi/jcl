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
{ The Original Code is JclSysUtils.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is Marcel van Brakel.                                 }
{ Portions created by Marcel van Brakel are Copyright (C) Marcel van Brakel. All rights reserved.  }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Alexander Radchenko,                                                                           }
{   Andreas Hausladen (ahuser)                                                                     }
{   Anthony Steele                                                                                 }
{   Bernhard Berger                                                                                }
{   Heri Bender                                                                                    }
{   Jean-Fabien Connault (cycocrew)                                                                }
{   Jens Fudickar                                                                                  }
{   Jeroen Speldekamp                                                                              }
{   Marcel van Brakel                                                                              }
{   Peter Friese                                                                                   }
{   Petr Vones (pvones)                                                                            }
{   Python                                                                                         }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert R. Marsh                                                                                }
{   Robert Rossmair (rrossmair)                                                                    }
{   Rudy Velthuis                                                                                  }
{   Uwe Schuster (uschuster)                                                                       }
{   Wayne Sherman                                                                                  }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Description: Win32 process related routines.                                         }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}


unit JclWin32Process;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, JclBase, JclSynch, JclSysUtils;

// Child processes
type
  // e.g. TStrings.Append
  TTextHandler = procedure(const Text: string) of object;
  TJclProcessPriority = (ppIdle, ppNormal, ppHigh, ppRealTime, ppBelowNormal, ppAboveNormal);

const
  ABORT_EXIT_CODE = ERROR_CANCELLED;

function Execute(const CommandLine: string; OutputLineCallback: TTextHandler; RawOutput: Boolean = False;
  AbortPtr: PBoolean = nil; ProcessPriority: TJclProcessPriority = ppNormal): Cardinal; overload;
function Execute(const CommandLine: string; AbortEvent: TJclEvent;
  OutputLineCallback: TTextHandler; RawOutput: Boolean = False; ProcessPriority: TJclProcessPriority = ppNormal): Cardinal; overload;
function Execute(const CommandLine: string; var Output: string; RawOutput: Boolean = False;
  AbortPtr: PBoolean = nil; ProcessPriority: TJclProcessPriority = ppNormal): Cardinal; overload;
function Execute(const CommandLine: string; AbortEvent: TJclEvent;
  var Output: string; RawOutput: Boolean = False; ProcessPriority: TJclProcessPriority = ppNormal): Cardinal; overload;

function Execute(const CommandLine: string; OutputLineCallback, ErrorLineCallback: TTextHandler;
  RawOutput: Boolean = False; RawError: Boolean = False; AbortPtr: PBoolean = nil; ProcessPriority: TJclProcessPriority = ppNormal): Cardinal; overload;
function Execute(const CommandLine: string; AbortEvent: TJclEvent;
  OutputLineCallback, ErrorLineCallback: TTextHandler; RawOutput: Boolean = False; RawError: Boolean = False; ProcessPriority: TJclProcessPriority = ppNormal): Cardinal; overload;
function Execute(const CommandLine: string; var Output, Error: string;
  RawOutput: Boolean = False; RawError: Boolean = False; AbortPtr: PBoolean = nil; ProcessPriority: TJclProcessPriority = ppNormal): Cardinal; overload;
function Execute(const CommandLine: string; AbortEvent: TJclEvent;
  var Output, Error: string; RawOutput: Boolean = False; RawError: Boolean = False; ProcessPriority: TJclProcessPriority = ppNormal): Cardinal; overload;

type
  IJclCommandLineTool = interface
    ['{A0034B09-A074-D811-847D-0030849E4592}']
    function GetExeName: string;
    function GetOptions: TStrings;
    function GetOutput: string;
    function GetOutputCallback: TTextHandler;
    procedure AddPathOption(const Option, Path: string);
    function Execute(const CommandLine: string): Boolean;
    procedure SetOutputCallback(const CallbackMethod: TTextHandler);
    property ExeName: string read GetExeName;
    property Options: TStrings read GetOptions;
    property OutputCallback: TTextHandler read GetOutputCallback write SetOutputCallback;
    property Output: string read GetOutput;
  end;

  EJclCommandLineToolError = class(EJclError);

  TJclCommandLineTool = class(TInterfacedObject, IJclCommandLineTool)
  private
    FExeName: string;
    FOptions: TStringList;
    FOutput: string;
    FOutputCallback: TTextHandler;
  public
    constructor Create(const AExeName: string);
    destructor Destroy; override;
    { IJclCommandLineTool }
    function GetExeName: string;
    function GetOptions: TStrings;
    function GetOutput: string;
    function GetOutputCallback: TTextHandler;
    procedure AddPathOption(const Option, Path: string);
    function Execute(const CommandLine: string): Boolean;
    procedure SetOutputCallback(const CallbackMethod: TTextHandler);
    property ExeName: string read GetExeName;
    property Options: TStrings read GetOptions;
    property OutputCallback: TTextHandler read GetOutputCallback write SetOutputCallback;
    property Output: string read GetOutput;
  end;


implementation

uses
  JclStrings, JclFileUtils;

//=== Child processes ========================================================

const
  BufferSize = 255;
type
  TBuffer = array [0..BufferSize] of AnsiChar;

  TPipeInfo = record
    PipeRead, PipeWrite: THandle;
    Buffer: TBuffer;
    Line: string;
    TextHandler: TTextHandler;
    RawOutput: Boolean;
    Event: TJclEvent;
  end;
  PPipeInfo = ^TPipeInfo;

// MuteCRTerminatedLines was "outsourced" from Win32ExecAndRedirectOutput

function InternalExecuteMuteCRTerminatedLines(const RawOutput: string): string;
const
  Delta = 1024;
var
  BufPos, OutPos, LfPos, EndPos: Integer;
  C: Char;
begin
  SetLength(Result, Length(RawOutput));
  OutPos := 1;
  LfPos := OutPos;
  EndPos := OutPos;
  for BufPos := 1 to Length(RawOutput) do
  begin
    if OutPos >= Length(Result)-2 then
      SetLength(Result, Length(Result) + Delta);
    C := RawOutput[BufPos];
    case C of
      NativeCarriageReturn:
        OutPos := LfPos;
      NativeLineFeed:
        begin
          OutPos := EndPos;
          Result[OutPos] := NativeCarriageReturn;
          Inc(OutPos);
          Result[OutPos] := C;
          Inc(OutPos);
          EndPos := OutPos;
          LfPos := OutPos;
        end;
    else
      Result[OutPos] := C;
      Inc(OutPos);
      EndPos := OutPos;
    end;
  end;
  SetLength(Result, OutPos - 1);
end;

procedure InternalExecuteProcessLine(const PipeInfo: TPipeInfo; LineEnd: Integer);
begin
  if PipeInfo.RawOutput or (PipeInfo.Line[LineEnd] <> NativeCarriageReturn) then
  begin
    while (LineEnd > 0) and CharIsReturn(PipeInfo.Line[LineEnd]) do
      Dec(LineEnd);
    PipeInfo.TextHandler(Copy(PipeInfo.Line, 1, LineEnd));
  end;
end;

procedure InternalExecuteProcessBuffer(var PipeInfo: TPipeInfo; PipeBytesRead: Cardinal);
var
  CR, LF: Integer;
begin
  PipeInfo.Buffer[PipeBytesRead] := #0;
  PipeInfo.Line := PipeInfo.Line + string(PipeInfo.Buffer);
  if Assigned(PipeInfo.TextHandler) then
  repeat
    CR := Pos(NativeCarriageReturn, PipeInfo.Line);
    if CR = Length(PipeInfo.Line) then
      CR := 0;        // line feed at CR + 1 might be missing
    LF := Pos(NativeLineFeed, PipeInfo.Line);
    if (CR > 0) and ((LF > CR + 1) or (LF = 0)) then
      LF := CR;       // accept CR as line end
    if LF > 0 then
    begin
      InternalExecuteProcessLine(PipeInfo, LF);
      Delete(PipeInfo.Line, 1, LF);
    end;
  until LF = 0;
end;

procedure InternalExecuteReadPipe(var PipeInfo: TPipeInfo; var Overlapped: TOverlapped);
var
  NullDWORD: ^DWORD; // XE4 broke PDWORD
  Res: DWORD;
begin
  NullDWORD := nil;
  if not ReadFile(PipeInfo.PipeRead, PipeInfo.Buffer[0], BufferSize, NullDWORD^, @Overlapped) then
  begin
    Res := GetLastError;
    case Res of
      ERROR_BROKEN_PIPE:
        begin
          CloseHandle(PipeInfo.PipeRead);
          PipeInfo.PipeRead := 0;
        end;
      ERROR_IO_PENDING:
        ;
    else
      {$IFDEF DELPHI11_UP}
      RaiseLastOSError(Res);
      {$ELSE}
      RaiseLastOSError;
      {$ENDIF DELPHI11_UP}
    end;
  end;
end;

procedure InternalExecuteHandlePipeEvent(var PipeInfo: TPipeInfo; var Overlapped: TOverlapped);
var
  PipeBytesRead: DWORD;
begin
  if GetOverlappedResult(PipeInfo.PipeRead, Overlapped, PipeBytesRead, False) then
  begin
    InternalExecuteProcessBuffer(PipeInfo, PipeBytesRead);
    // automatically launch the next read
    InternalExecuteReadPipe(PipeInfo, Overlapped);
  end
  else
  if GetLastError = ERROR_BROKEN_PIPE then
  begin
    CloseHandle(PipeInfo.PipeRead);
    PipeInfo.PipeRead := 0;
  end
  else
    RaiseLastOSError;
end;

procedure InternalExecuteFlushPipe(var PipeInfo: TPipeInfo; var Overlapped: TOverlapped);
var
  PipeBytesRead: DWORD;
begin
  CancelIo(PipeInfo.PipeRead);
  GetOverlappedResult(PipeInfo.PipeRead, Overlapped, PipeBytesRead, True);
  if PipeBytesRead > 0 then
    InternalExecuteProcessBuffer(PipeInfo, PipeBytesRead);
  while PeekNamedPipe(PipeInfo.PipeRead, nil, 0, nil, @PipeBytesRead, nil) and (PipeBytesRead > 0) do
  begin
    if PipeBytesRead > BufferSize then
      PipeBytesRead := BufferSize;
    if not ReadFile(PipeInfo.PipeRead, PipeInfo.Buffer[0], PipeBytesRead, PipeBytesRead, nil) then
      RaiseLastOSError;
    InternalExecuteProcessBuffer(PipeInfo, PipeBytesRead);
  end;
end;

var
  AsyncPipeCounter: Integer;

// CreateAsyncPipe creates a pipe that uses overlapped reading.
function CreateAsyncPipe(var hReadPipe, hWritePipe: THandle;
  lpPipeAttributes: PSecurityAttributes; nSize: DWORD): BOOL;
var
  PipeName: string;
  Error: DWORD;
  PipeReadHandle, PipeWriteHandle: THandle;
begin
  Result := False;

  if (@hReadPipe = nil) or (@hWritePipe = nil) then
  begin
    SetLastError(ERROR_INVALID_PARAMETER);
    Exit;
  end;

  if nSize = 0 then
    nSize := 4096;

  InterlockedIncrement(AsyncPipeCounter);
  // In some (not so) rare instances there is a race condition
  // where the counter is the same for two threads at the same
  // time. This makes the CreateNamedPipe call below fail
  // because of the limit set to 1 in the call.
  // So, to be sure this call succeeds, we put both the process
  // and thread id in the name of the pipe.
  // This was found to happen while simply starting 7 instances
  // of the same exe file in parallel.
  PipeName := Format('\\.\Pipe\AsyncAnonPipe.%.8x.%.8x.%.8x', [GetCurrentProcessId, GetCurrentThreadId, AsyncPipeCounter]);

  PipeReadHandle := CreateNamedPipe(PChar(PipeName), PIPE_ACCESS_INBOUND or FILE_FLAG_OVERLAPPED,
      PIPE_TYPE_BYTE or PIPE_WAIT, 1, nSize, nSize, 120 * 1000, lpPipeAttributes);
  if PipeReadHandle = INVALID_HANDLE_VALUE then
    Exit;

  PipeWriteHandle := CreateFile(PChar(PipeName), GENERIC_WRITE, 0, lpPipeAttributes, OPEN_EXISTING,
      FILE_ATTRIBUTE_NORMAL {or FILE_FLAG_OVERLAPPED}, 0);
  if PipeWriteHandle = INVALID_HANDLE_VALUE then
  begin
    Error := GetLastError;
    CloseHandle(PipeReadHandle);
    SetLastError(Error);
    Exit;
  end;

  hReadPipe := PipeReadHandle;
  hWritePipe := PipeWriteHandle;

  Result := True;
end;

const
  BELOW_NORMAL_PRIORITY_CLASS = $00004000;
  ABOVE_NORMAL_PRIORITY_CLASS = $00008000;

  ProcessPriorities: array [TJclProcessPriority] of DWORD =
    (IDLE_PRIORITY_CLASS, NORMAL_PRIORITY_CLASS, HIGH_PRIORITY_CLASS, REALTIME_PRIORITY_CLASS,
     BELOW_NORMAL_PRIORITY_CLASS, ABOVE_NORMAL_PRIORITY_CLASS);

function InternalExecute(CommandLine: string; AbortPtr: PBoolean; AbortEvent: TJclEvent;
  var Output: string; OutputLineCallback: TTextHandler; RawOutput: Boolean;
  MergeError: Boolean; var Error: string; ErrorLineCallback: TTextHandler; RawError: Boolean;
  ProcessPriority: TJclProcessPriority): Cardinal;
var
  OutPipeInfo, ErrorPipeInfo: TPipeInfo;
  Index: Cardinal;
{$IFDEF MSWINDOWS}
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SecurityAttr: TSecurityAttributes;
  OutOverlapped, ErrorOverlapped: TOverlapped;
  ProcessEvent: TJclDispatcherObject;
  WaitEvents: array of TJclDispatcherObject;
  InternalAbort: Boolean;
  LastError: DWORD;
begin
  // hack to pass a null reference to the parameter lpNumberOfBytesRead of ReadFile
  Result := $FFFFFFFF;
  SecurityAttr.nLength := SizeOf(SecurityAttr);
  SecurityAttr.lpSecurityDescriptor := nil;
  SecurityAttr.bInheritHandle := True;

  ResetMemory(OutPipeInfo, SizeOf(OutPipeInfo));
  OutPipeInfo.TextHandler := OutputLineCallback;
  OutPipeInfo.RawOutput := RawOutput;
  if not CreateAsyncPipe(OutPipeInfo.PipeRead, OutPipeInfo.PipeWrite, @SecurityAttr, 0) then
  begin
    Result := GetLastError;
    Exit;
  end;
  OutPipeInfo.Event := TJclEvent.Create(@SecurityAttr, False {automatic reset}, False {not flagged}, '' {anonymous});
  ResetMemory(ErrorPipeInfo, SizeOf(ErrorPipeInfo));
  if not MergeError then
  begin
    ErrorPipeInfo.TextHandler := ErrorLineCallback;
    ErrorPipeInfo.RawOutput := RawError;
    if not CreateAsyncPipe(ErrorPipeInfo.PipeRead, ErrorPipeInfo.PipeWrite, @SecurityAttr, 0) then
    begin
      Result := GetLastError;
      CloseHandle(OutPipeInfo.PipeWrite);
      CloseHandle(OutPipeInfo.PipeRead);
      OutPipeInfo.Event.Free;
      Exit;
    end;
    ErrorPipeInfo.Event := TJclEvent.Create(@SecurityAttr, False {automatic reset}, False {not flagged}, '' {anonymous});
  end;

  ResetMemory(StartupInfo, SizeOf(TStartupInfo));
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  StartupInfo.wShowWindow := SW_HIDE;
  StartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
  StartupInfo.hStdOutput := OutPipeInfo.PipeWrite;
  if MergeError then
    StartupInfo.hStdError := OutPipeInfo.PipeWrite
  else
    StartupInfo.hStdError := ErrorPipeInfo.PipeWrite;
  UniqueString(CommandLine); // CommandLine must be in a writable memory block
  ProcessInfo.dwProcessId := 0;
  ProcessEvent := nil;
  try
    if CreateProcess(nil, PChar(CommandLine), nil, nil, True, ProcessPriorities[ProcessPriority],
      nil, nil, StartupInfo, ProcessInfo) then
    begin
      try
        // init out and error events
        CloseHandle(OutPipeInfo.PipeWrite);
        OutPipeInfo.PipeWrite := 0;
        if not MergeError then
        begin
          CloseHandle(ErrorPipeInfo.PipeWrite);
          ErrorPipeInfo.PipeWrite := 0;
        end;
        InternalAbort := False;
        if AbortPtr <> nil then
          AbortPtr^ := {$IFDEF FPC}Byte({$ENDIF}False{$IFDEF FPC}){$ENDIF}
        else
          AbortPtr := @InternalAbort;
        // init the array of events to wait for
        ProcessEvent := TJclDispatcherObject.Attach(ProcessInfo.hProcess);
        SetLength(WaitEvents, 2);
        // add the process first
        WaitEvents[0] := ProcessEvent;
        // add the output event
        WaitEvents[1] := OutPipeInfo.Event;
        // add the error event
        if not MergeError then
        begin
          SetLength(WaitEvents, 3);
          WaitEvents[2] := ErrorPipeInfo.Event;
        end;
        // add the abort event if any
        if AbortEvent <> nil then
        begin
          AbortEvent.ResetEvent;
          Index := Length(WaitEvents);
          SetLength(WaitEvents, Index + 1);
          WaitEvents[Index] := AbortEvent;
        end;
        // init the asynchronous reads
        ResetMemory(OutOverlapped, SizeOf(OutOverlapped));
        OutOverlapped.hEvent := OutPipeInfo.Event.Handle;
        InternalExecuteReadPipe(OutPipeInfo, OutOverlapped);
        if not MergeError then
        begin
          ResetMemory(ErrorOverlapped, SizeOf(ErrorOverlapped));
          ErrorOverlapped.hEvent := ErrorPipeInfo.Event.Handle;
          InternalExecuteReadPipe(ErrorPipeInfo, ErrorOverlapped);
        end;
        // event based loop
        while not {$IFDEF FPC}Boolean({$ENDIF}AbortPtr^{$IFDEF FPC}){$ENDIF} do
        begin
          Index := WaitAlertableForMultipleObjects(WaitEvents, False, INFINITE);
          if Index = WAIT_OBJECT_0 then
            // the subprocess has ended
            Break
          else
          if Index = (WAIT_OBJECT_0 + 1) then
          begin
            // event on output
            InternalExecuteHandlePipeEvent(OutPipeInfo, OutOverlapped);
          end
          else
          if (Index = (WAIT_OBJECT_0 + 2)) and not MergeError then
          begin
            // event on error
            InternalExecuteHandlePipeEvent(ErrorPipeInfo, ErrorOverlapped);
          end
          else
          if ((Index = (WAIT_OBJECT_0 + 2)) and MergeError) or
             ((Index = (WAIT_OBJECT_0 + 3)) and not MergeError) then
            // event on abort
            AbortPtr^ := {$IFDEF FPC}Byte({$ENDIF}True{$IFDEF FPC}){$ENDIF}
          else
            {$IFDEF DELPHI11_UP}
            RaiseLastOSError(Index);
            {$ELSE}
            RaiseLastOSError;
            {$ENDIF DELPHI11_UP}
        end;
        if {$IFDEF FPC}Boolean({$ENDIF}AbortPtr^{$IFDEF FPC}){$ENDIF} then
          TerminateProcess(ProcessEvent.Handle, Cardinal(ABORT_EXIT_CODE));
        if (ProcessEvent.WaitForever = {$IFDEF RTL280_UP}TJclWaitResult.{$ENDIF RTL280_UP}wrSignaled) and not GetExitCodeProcess(ProcessEvent.Handle, Result) then
          Result := $FFFFFFFF;
        CloseHandle(ProcessInfo.hThread);
        ProcessInfo.hThread := 0;
        if OutPipeInfo.PipeRead <> 0 then
          // read data remaining in output pipe
          InternalExecuteFlushPipe(OutPipeinfo, OutOverlapped);
        if not MergeError and (ErrorPipeInfo.PipeRead <> 0) then
          // read data remaining in error pipe
          InternalExecuteFlushPipe(ErrorPipeInfo, ErrorOverlapped);
      except
        // always terminate process in case of an exception.
        // This is especially useful when an exception occured in one of
        // the texthandler but only do it if the process actually started,
        // this prevents eating up the last error value by calling those
        // three functions with an invalid handle
        // Note that we don't do it in the finally block because these
        // calls would also then eat up the last error value which we tried
        // to avoid in the first place
        if ProcessInfo.hProcess <> 0 then
        begin
          TerminateProcess(ProcessInfo.hProcess, Cardinal(ABORT_EXIT_CODE));
          WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
          GetExitCodeProcess(ProcessInfo.hProcess, Result);
        end;

        raise;
      end;
    end;
  finally
    LastError := GetLastError;
    try
      if OutPipeInfo.PipeRead <> 0 then
        CloseHandle(OutPipeInfo.PipeRead);
      if OutPipeInfo.PipeWrite <> 0 then
        CloseHandle(OutPipeInfo.PipeWrite);
      if ErrorPipeInfo.PipeRead <> 0 then
        CloseHandle(ErrorPipeInfo.PipeRead);
      if ErrorPipeInfo.PipeWrite <> 0 then
        CloseHandle(ErrorPipeInfo.PipeWrite);
      if ProcessInfo.hThread <> 0 then
        CloseHandle(ProcessInfo.hThread);

      if Assigned(ProcessEvent) then
        ProcessEvent.Free // this calls CloseHandle(ProcessInfo.hProcess)
      else if ProcessInfo.hProcess <> 0 then
        CloseHandle(ProcessInfo.hProcess);
      OutPipeInfo.Event.Free;
      ErrorPipeInfo.Event.Free;
    finally
      SetLastError(LastError);
    end;
  end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
var
  PipeBytesRead: Cardinal;
  Pipe: PIOFile;
  Cmd: string;
begin
  Cmd := Format('%s 2>&1', [CommandLine]);
  Pipe := nil;
  try
    Pipe := Libc.popen(PChar(Cmd), 'r');
    { TODO : handle Abort }
    repeat
      PipeBytesRead := fread_unlocked(@OutBuffer, 1, BufferSize, Pipe);
      if PipeBytesRead > 0 then
        ProcessBuffer(OutBuffer, OutLine, PipeBytesRead);
    until PipeBytesRead = 0;
    Result := pclose(Pipe);
    Pipe := nil;
    wait(nil);
  finally
    if Pipe <> nil then
      pclose(Pipe);
    wait(nil);
  end;
{$ENDIF UNIX}
  if OutPipeInfo.Line <> '' then
    if Assigned(OutPipeInfo.TextHandler) then
      // output wasn't terminated by a line feed...
      // (shouldn't happen, but you never know)
      InternalExecuteProcessLine(OutPipeInfo, Length(OutPipeInfo.Line))
    else
      if RawOutput then
        Output := Output + OutPipeInfo.Line
      else
        Output := Output + InternalExecuteMuteCRTerminatedLines(OutPipeInfo.Line);
  if ErrorPipeInfo.Line <> '' then
    if Assigned(ErrorPipeInfo.TextHandler) then
      // error wasn't terminated by a line feed...
      // (shouldn't happen, but you never know)
      InternalExecuteProcessLine(ErrorPipeInfo, Length(ErrorPipeInfo.Line))
    else
      if RawError then
        Error := Error + ErrorPipeInfo.Line
      else
        Error := Error + InternalExecuteMuteCRTerminatedLines(ErrorPipeInfo.Line);
end;

{ TODO -cHelp :
RawOutput: Do not process isolated carriage returns (#13).
That is, for RawOutput = False, lines not terminated by a line feed (#10) are deleted from Output. }

function Execute(const CommandLine: string; var Output: string; RawOutput: Boolean;
  AbortPtr: PBoolean; ProcessPriority: TJclProcessPriority): Cardinal;
var
  Error: string;
begin
  Error := '';
  Result := InternalExecute(CommandLine, AbortPtr, nil, Output, nil, RawOutput, True, Error, nil, False, ProcessPriority);
end;

function Execute(const CommandLine: string; AbortEvent: TJclEvent; var Output: string; RawOutput: Boolean; ProcessPriority: TJclProcessPriority): Cardinal;
var
  Error: string;
begin
  Error := '';
  Result := InternalExecute(CommandLine, nil, AbortEvent, Output, nil, RawOutput, True, Error, nil, False, ProcessPriority);
end;

{ TODO -cHelp :
Author: Robert Rossmair
OutputLineCallback called once per line of output. }

function Execute(const CommandLine: string; OutputLineCallback: TTextHandler; RawOutput: Boolean;
  AbortPtr: PBoolean; ProcessPriority: TJclProcessPriority): Cardinal;
var
  Output, Error: string;
begin
  Output := '';
  Error := '';
  Result := InternalExecute(CommandLine, AbortPtr, nil, Output, OutputLineCallback, RawOutput, True, Error, nil, False, ProcessPriority);
end;

function Execute(const CommandLine: string; AbortEvent: TJclEvent; OutputLineCallback: TTextHandler; RawOutput: Boolean; ProcessPriority: TJclProcessPriority): Cardinal;
var
  Output, Error: string;
begin
  Output := '';
  Error := '';
  Result := InternalExecute(CommandLine, nil, AbortEvent, Output, OutputLineCallback, RawOutput, True, Error, nil, False, ProcessPriority);
end;

{ TODO -cHelp :
RawOutput: Do not process isolated carriage returns (#13).
That is, for RawOutput = False, lines not terminated by a line feed (#10) are deleted from Output. }

function Execute(const CommandLine: string; var Output, Error: string; RawOutput, RawError: Boolean;
  AbortPtr: PBoolean; ProcessPriority: TJclProcessPriority): Cardinal;
begin
  Result := InternalExecute(CommandLine, AbortPtr, nil, Output, nil, RawOutput, False, Error, nil, RawError, ProcessPriority);
end;

function Execute(const CommandLine: string; AbortEvent: TJclEvent; var Output, Error: string;
  RawOutput, RawError: Boolean; ProcessPriority: TJclProcessPriority): Cardinal;
begin
  Result := InternalExecute(CommandLine, nil, AbortEvent, Output, nil, RawOutput, False, Error, nil, RawError, ProcessPriority);
end;

{ TODO -cHelp :
Author: Robert Rossmair
OutputLineCallback called once per line of output. }

function Execute(const CommandLine: string; OutputLineCallback, ErrorLineCallback: TTextHandler;
  RawOutput, RawError: Boolean; AbortPtr: PBoolean; ProcessPriority: TJclProcessPriority): Cardinal;
var
  Output, Error: string;
begin
  Output := '';
  Error := '';
  Result := InternalExecute(CommandLine, AbortPtr, nil, Output, OutputLineCallback, RawOutput, False, Error, ErrorLineCallback, RawError, ProcessPriority);
end;

function Execute(const CommandLine: string; AbortEvent: TJclEvent; OutputLineCallback, ErrorLineCallback: TTextHandler;
  RawOutput, RawError: Boolean; ProcessPriority: TJclProcessPriority): Cardinal;
var
  Output, Error: string;
begin
  Output := '';
  Error := '';
  Result := InternalExecute(CommandLine, nil, AbortEvent, Output, OutputLineCallback, RawOutput, False, Error, ErrorLineCallback, RawError, ProcessPriority);
end;

//=== { TJclCommandLineTool } ================================================

constructor TJclCommandLineTool.Create(const AExeName: string);
begin
  inherited Create;
  FOptions := TStringList.Create;
  FExeName := AExeName;
end;

destructor TJclCommandLineTool.Destroy;
begin
  FreeAndNil(FOptions);
  inherited Destroy;
end;

procedure TJclCommandLineTool.AddPathOption(const Option, Path: string);
var
  S: string;
begin
  S := PathRemoveSeparator(Path);
  {$IFDEF MSWINDOWS}
  S := LowerCase(S); // file names are case insensitive
  {$ENDIF MSWINDOWS}
  S := Format('-%s%s', [Option, S]);
  // avoid duplicate entries (note that search is case sensitive)
  if GetOptions.IndexOf(S) = -1 then
    GetOptions.Add(S);
end;

function TJclCommandLineTool.Execute(const CommandLine: string): Boolean;
begin
  if Assigned(FOutputCallback) then
    Result := JclWin32Process.Execute(Format('"%s" %s', [ExeName, CommandLine]), FOutputCallback) = 0
  else
    Result := JclWin32Process.Execute(Format('"%s" %s', [ExeName, CommandLine]), FOutput) = 0;
end;

function TJclCommandLineTool.GetExeName: string;
begin
  Result := FExeName;
end;

function TJclCommandLineTool.GetOptions: TStrings;
begin
  Result := FOptions;
end;

function TJclCommandLineTool.GetOutput: string;
begin
  Result := FOutput;
end;

function TJclCommandLineTool.GetOutputCallback: TTextHandler;
begin
  Result := FOutputCallback;
end;

procedure TJclCommandLineTool.SetOutputCallback(const CallbackMethod: TTextHandler);
begin
  FOutputCallback := CallbackMethod;
end;


end.

