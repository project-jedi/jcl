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
{ The Original Code is JclBorlandToolsExtensions.pas.                                              }
{                                                                                                  }
{ The Initial Developers of the Original Code are documented in the accompanying help file         }
{ JCLHELP.hlp. Portions created by these individuals are Copyright (C) of these individuals.       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Contains extended wrapper classes for borlands tools                                             }
{                                                                                                  }
{ Known Issues:                                                                                    }
{   This is a preview - class and functionnames might be changed and this unit might me merged     }
{   into JclBorlandTools.pas.                                                                      }
{                                                                                                  }
{ Unit owner: Uwe Schuster                                                                         }
{ Last modified: April 07, 2004                                                                    }
{                                                                                                  }
{**************************************************************************************************}

{
Todo:
- style:
  - sort class members
  - insert markers 
- support Aborted and set current dir in ...ExecAndCapture
- IFDEF OS specific compiler switches and defines
- get settings from .cfg or .dof

- move ExecAndCapture to a unit where it fit's more
  (the same should be considered for JclBorlandTools.ExecAndRedirectOutput)  
}

unit JclBorlandToolsExtensions;

{$I jcl.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc, Types,
  {$ENDIF}
  SysUtils, Classes;

type
  TJclDCCMessageKind = (mkUnknown, mkHint, mkWarning, mkError, mkFatal);
  TJclDCCMapFileLevel = (mfloff, mflsegments, mflpublics, mfldetailed);

  TJclDCCEx = class(TObject)
  private
    FErrorCount: Integer;
    FHintCount: Integer;
    FWarnCount: Integer;
    FFatalCount: Integer;
    FFileToCompile: string;
    FPlainOutput: TStringList;
    FExeName: string;
    FCurrentFile: string;
    FCurrentLineNo: Integer;
    FMessages: TStringList;

    FEXEOutputDir: string;
    FDCUOutputDir: string;
    FSearchPaths: string;
    FCompileWithPackages: Boolean;
    FPackages: string;
    FDefines: string;
    FMapFileLevel: TJclDCCMapFileLevel;
    FCompilerSwitches: string;
    FUnitAliases: string;
    FOutputWarnings: Boolean;
    FOutputHints: Boolean;
    FConsoleApplication: Boolean;
    FTD32DebugInfo: Boolean;
    FRemoteDebugSymbols: Boolean;
    FMinStackSize: DWord;
    FMaxStackSize: DWord;
    FImageBase: DWord;
    FOnCompileUpdate: TNotifyEvent;
    procedure CaptureLine(const Line: string; var Aborted: Boolean);
    procedure ClearValues;
  public
    constructor Create;
    destructor Destroy; override;

    function Compile: Boolean;

    property ExeName: string read FExeName write FExeName;
    property CurrentFile: string read FCurrentFile;
    property CurrentLineNo: Integer read FCurrentLineNo;
    property ErrorCount: Integer read FErrorCount;
    property HintCount: Integer read FHintCount;
    property WarnCount: Integer read FWarnCount;
    property FatalCount: Integer read FFatalCount;
    property CompileWithPackages: Boolean read FCompileWithPackages write FCompileWithPackages;
    property DCUOutputDir: string read FDCUOutputDir write FDCUOutputDir;
    property Defines: string read FDefines write FDefines;
    property EXEOutputDir: string read FEXEOutputDir write FEXEOutputDir;
    property FileToCompile: string read FFileToCompile write FFileToCompile;
    property PlainOutput: TStringList read FPlainOutput;
    //(usc) better as StringList
    property SearchPaths: string read FSearchpaths write FSearchPaths;
    property MapFileLevel: TJclDCCMapFileLevel read FMapFileLevel write FMapFileLevel;
    //(usc) better as TJclDCCMessages
    property Messages: TStringList read FMessages;
    //(usc) better as StringList
    property Packages: string read FPackages write FPackages;
    //(usc) better as TJclDCCSwitches
    property CompilerSwitches: string read FCompilerSwitches write FCompilerSwitches;
    property UnitAliases: string read FUnitAliases write FUnitAliases;
    property OutputWarnings: Boolean read FOutputWarnings;
    property OutputHints: Boolean read FOutputHints;
    property ConsoleApplication: Boolean read FConsoleApplication write FConsoleApplication;
    property TD32DebugInfo: Boolean read FTD32DebugInfo write FTD32DebugInfo;
    property RemoteDebugSymbols: Boolean read FRemoteDebugSymbols write FRemoteDebugSymbols;
    property MinStackSize: DWord read FMinStackSize write FMinStackSize;
    property MaxStackSize: DWord read FMaxStackSize write FMaxStackSize;
    property ImageBase: DWord read FImageBase write FImageBase;
    //(usc) better split into OnCompileProgress and OnMessage
    property OnCompileUpdate: TNotifyEvent read FOnCompileUpdate write FOnCompileUpdate;
  end;

implementation

{$IFDEF DELPHI5}
uses
  FileCtrl;
{$ENDIF DELPHI5}

type
  TCaptureLine = procedure(const ALine: string; var Aborted: Boolean) of object;

{$IFDEF MSWINDOWS}
//(usc) this is a modified version of JclMiscel.WinExec32AndRedirectOutput
//(usc) TCaptureLine Aborted is not support so far
function WinExec32AndCapture(const ACommandLine: string; ACaptureLine: TCaptureLine): Cardinal;
const
  BufferSize = 1024;
var
  Buffer: array [0..BufferSize] of Char;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  SecurityAttr: TSecurityAttributes;
  PipeRead, PipeWrite: THandle;
  PipeBytesRead: Cardinal;
  TempOutput, LineOut: string;
  I: Integer;
  Aborted: Boolean;
begin
  Result := $FFFFFFFF;
  TempOutput := '';
  SecurityAttr.nLength := SizeOf(SecurityAttr);
  SecurityAttr.lpSecurityDescriptor := nil;
  SecurityAttr.bInheritHandle := True;
  if not CreatePipe(PipeRead, PipeWrite, @SecurityAttr, 0) then
  begin
    Result := GetLastError;
    Exit;
  end;
  FillChar(StartupInfo, SizeOf(TStartupInfo), #0);
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
  StartupInfo.wShowWindow := SW_HIDE;
  StartupInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
  StartupInfo.hStdOutput := PipeWrite;
  StartupInfo.hStdError := PipeWrite;
  if CreateProcess(nil, PChar(ACommandLine), nil, nil, True, NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo,
    ProcessInfo) then
  begin
    CloseHandle(PipeWrite);
    LineOut := '';
    while ReadFile(PipeRead, Buffer, BufferSize, PipeBytesRead, nil) and (PipeBytesRead > 0) do
    begin
      Buffer[PipeBytesRead] := #0;
      TempOutput := AdjustLineBreaks(Buffer);
      for I := 1 to Length(TempOutput) do
      begin
        if TempOutput[I] = #13 then
        begin
          ACaptureLine(LineOut, Aborted);
          LineOut := '';
        end
        else if TempOutput[I] <> #10 then
          LineOut := LineOut + TempOutput[I];
      end;
    end;
    if LineOut <> '' then
      ACaptureLine(LineOut, Aborted);
    if (WaitForSingleObject(ProcessInfo.hProcess, INFINITE) = WAIT_OBJECT_0) and
      not GetExitCodeProcess(ProcessInfo.hProcess, Result) then
        Result := $FFFFFFFF;
    CloseHandle(ProcessInfo.hThread);
    CloseHandle(ProcessInfo.hProcess);
  end
  else
    CloseHandle(PipeWrite);
  CloseHandle(PipeRead);
end;
{$ENDIF}

{$IFDEF LINUX}
//(usc) this is a modified version of JclBorlandTools.ExecAndRedirectOutput
//(usc) TCaptureLine Aborted is not support so far
function UnixExecAndCapture(const ACommandLine: string; ACaptureLine: TCaptureLine): Integer;
var
  Pipe: PIOFile;
  Count: Integer;
  Buffer: array [Byte] of Char;
  Cmd, TempOutput: string;
  I: Integer;
  LineOut: string;
  Aborted: Boolean;
begin
  Cmd := Format('%s 2>&1', [ACommandLine]);
  Pipe := Libc.popen(PChar(Cmd), 'r');
  repeat
    Count := fread_unlocked(@Buffer, 1, Length(Buffer) - 1, Pipe);
    if Count > 0 then
    begin
      Buffer[Count] := #0;
      TempOutput := AdjustLineBreaks(Buffer);
      for I := 1 to Length(TempOutput) do
      begin
        if TempOutput[I] = #10 then
        begin
          ACaptureLine(LineOut, Aborted);
          LineOut := '';
        end
        else if TempOutput[I] <> #13 then
          LineOut := LineOut + TempOutput[I];
      end;
    end;
  until Count < Length(Buffer) - 1;
  if LineOut <> '' then
    ACaptureLine(LineOut, Aborted);

  Result := pclose(Pipe);
  wait(nil);
end;
{$ENDIF LINUX}

function ExecAndCapture(const ACommandLine: string; ACaptureLine: TCaptureLine): Integer;
begin
  {$IFDEF MSWINDOWS}
  Result := WinExec32AndCapture(ACommandLine, ACaptureLine);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Result := UnixExecAndCapture(ACommandLine, ACaptureLine);
  {$ENDIF LINUX}
end;

type
  TJclMessageConversionRec = record
    MessageString: string;
    MessageKind: TJclDCCMessageKind;
  end;

const
  MessageConversionArray: array [0..9] of TJclMessageConversionRec =
  (
   //english
   (MessageString: 'hint: '; MessageKind: mkHint), //do not localize
   //german
   (MessageString: 'hinweis: '; MessageKind: mkHint), //do not localize
   //french
   (MessageString: 'suggestion: '; MessageKind: mkHint), //do not localize
   //english
   (MessageString: 'warning: '; MessageKind: mkWarning), //do not localize
   //german
   (MessageString: 'warnung: '; MessageKind: mkWarning), //do not localize
   //french
   (MessageString: 'avertissement: '; MessageKind: mkWarning), //do not localize
   //english
   (MessageString: 'error: '; MessageKind: mkError), //do not localize
   //german
   (MessageString: 'fehler: '; MessageKind: mkError), //do not localize
   //french
   (MessageString: 'erreur: '; MessageKind: mkError), //do not localize
   //english, german, french
   (MessageString: 'fatal: '; MessageKind: mkFatal) //do not localize
  );

function GetMessageFromLine(const ALine: string; const ACheckValue: string;
  var AMessage: string): Boolean;
var
  p1: Integer;
begin
  Result := False;
  p1 := Pos(ACheckValue, AnsiLowerCase(ALine));
  if p1 > 0 then
  begin
    p1 := p1 + Length(ACheckValue);
    AMessage := Copy(ALine, p1, Length(ALine) - p1 + 1);
    Result := True;
  end;
end;

function CheckLineForMessages(const ALine: string; var AMessage: string): TJclDCCMessageKind;
var
  I: Integer;
begin
  Result := mkUnknown;
  for I := Low(MessageConversionArray) to High(MessageConversionArray) do
    with MessageConversionArray[I] do
      if GetMessageFromLine(ALine, MessageString, AMessage) then
      begin
        Result := MessageKind;
        Break;
      end;
end;

function GetMessagePos(const ALine: string): Integer;
var
  I: Integer;
begin
  for I := Low(MessageConversionArray) to High(MessageConversionArray) do
    with MessageConversionArray[I] do
    begin
      Result := Pos(MessageString, AnsiLowerCase(ALine));
      if Result > 0 then
        Break;
    end;
end;

constructor TJclDCCEx.Create;
begin
  inherited Create;
  FPlainOutput := TStringList.Create;
  FMessages := TStringList.Create;
  ClearValues;
  FEXEOutputDir := '';
  FDCUOutputDir := '';
  FSearchPaths := '';
  FCompileWithPackages := False;
  FPackages := '';
  FDefines := '';
  FMapFileLevel := mfloff;
  FCompilerSwitches := '';
  FUnitAliases := '';
  FOutputWarnings := False;
  FOutputHints := False;
  FConsoleApplication := False;
  FTD32DebugInfo := False;
  FRemoteDebugSymbols := False;
  FMinStackSize := $4000;
  FMaxStackSize := $100000;
  FImageBase := $400000;
  FOnCompileUpdate := nil;
end;

destructor TJclDCCEx.Destroy;
begin
  FMessages.Free;
  FPlainOutput.Free;
  inherited Destroy;
end;

procedure TJclDCCEx.CaptureLine(const Line: string; var Aborted: Boolean);
var
  p1, p2: Integer;
  S: string;
  LineErr: TJclDCCMessageKind;
  HasPos: Boolean;
  LineMsg: string;
  FilePart: string;
begin
  FPlainOutput.Add(Line);
  HasPos := False;
  p1 := Pos('(', Line);
  if p1 > 0 then
  begin
    p2 := Pos(')', Line);
    if (p2 > p1) and
      ((p2 + 1 = Length(Line)) or (GetMessagePos(Line) - 2 = p2)) then
    begin
      S := Line;
      FCurrentFile := Copy(S, 1, p1 - 1);
      S := Copy(S, p1 + 1, p2 - p1 - 1);
      FCurrentLineNo := StrToIntDef(S, 0);
      HasPos := True;
    end;
  end;
  LineErr := CheckLineForMessages(Line, LineMsg);
  FilePart := '';
  if (LineErr <> mkUnknown) and HasPos then
    FilePart := Format('%s(%d): ', [ExtractFileName(FCurrentFile), FCurrentLineNo]);
  if LineErr = mkHint then
  begin
    Inc(FHintCount);
    FMessages.Add(Format('[Hint] %s%s', [FilePart, LineMsg]));
  end
  else
  if LineErr = mkWarning then
  begin
    Inc(FWarnCount);
    FMessages.Add(Format('[Warning] %s%s', [FilePart, LineMsg]));
  end
  else
  if LineErr = mkError then
  begin
    Inc(FErrorCount);
    FMessages.Add(Format('[Error] %s%s', [FilePart, LineMsg]));
  end
  else
  if LineErr = mkFatal then
  begin
    Inc(FErrorCount);
    FMessages.Add(Format('[Fatal Error] %s%s', [FilePart, LineMsg]));
  end;
  if Assigned(FOnCompileUpdate) and (HasPos or (LineErr <> mkUnknown)) then
    FOnCompileUpdate(nil);
end;

procedure TJclDCCEx.ClearValues;
begin
  FErrorCount := 0;
  FHintCount := 0;
  FWarnCount := 0;
  FFatalCount := 0;
  FPlainOutput.Clear;
  FCurrentFile := '';
  FCurrentLineNo := 0;
  FMessages.Clear;
end;

function TJclDCCEx.Compile: Boolean;
var
  Arguments: string;
begin
  ClearValues;
  Result := False;
  if FileExists(FFileToCompile) and (FExeName <> '') then
  begin
    Arguments := FFileToCompile;
    if DirectoryExists(FEXEOutputDir) then
      Arguments := Arguments + Format(' -E%s', [FEXEOutputDir]);
    if DirectoryExists(FDCUOutputDir) then
      Arguments := Arguments + Format(' -N%s', [FDCUOutputDir]);
    Arguments := Arguments + Format(' -I%s', [FSearchPaths]);
    Arguments := Arguments + Format(' -R%s', [FSearchPaths]);
    Arguments := Arguments + Format(' -U%s', [FSearchPaths]);
    if FCompileWithPackages and (FPackages <> '') then
      Arguments := Arguments + Format(' -LU%s', [FPackages]);
    if FDefines <> '' then
      Arguments := Arguments + Format(' -D%s', [FDefines]);
    case FMapFileLevel of
      mflsegments: Arguments := Arguments + ' -GS';
      mflpublics: Arguments := Arguments + ' -GP';
      mfldetailed: Arguments := Arguments + ' -GD';
    end;
    Arguments := Arguments + ' -B';  //(usc) make option
    Arguments := Arguments + ' ' + FCompilerSwitches;
    if FUnitAliases <> '' then
      Arguments := Arguments + ' -A' + FUnitAliases;
    if FOutputWarnings then
      Arguments := Arguments + ' -W'; //(usc) test - is enabled by default ?
    if FOutputHints then
      Arguments := Arguments + ' -H'; //(usc) test - is enabled by default ?
    if FConsoleApplication then
      Arguments := Arguments + ' -CC'
    else
      Arguments := Arguments + ' -CG';
    if FTD32DebugInfo then
      Arguments := Arguments + ' -V'; //(usc) check
    if FRemoteDebugSymbols then
      Arguments := Arguments + ' -VR';

                                        //(usc) no violation with -$M+ / -$M- ?
    //values must be decimal
    Arguments := Arguments + Format(' -$M%d,%d', [FMinStackSize, FMaxStackSize]);
    //value must be hexadecimal
    Arguments := Arguments + Format(' -K%.8x', [FImageBase]);

    ExecAndCapture(FExeName + ' ' + Arguments, CaptureLine);
    Result := (FErrorCount = 0) and (FFatalCount = 0);
  end;
end;

end.
