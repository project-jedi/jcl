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
{ Last modified: April 11, 2004                                                                    }
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
  SysUtils, Classes, IniFiles;

type
  TJclDCCMessageKind = (mkUnknown, mkHint, mkWarning, mkError, mkFatal);
  TJclDCCMapFileLevel = (mfloff, mflsegments, mflpublics, mfldetailed);

  //(usc) need optionen "MakeModifiedUnits" (-M) ?
  //   I guess -M is opposite of -B  
  TJclCustomDCCConfig = class(TObject)
  private
    FBPLOutputDirectory: string;
    FBuildAllUnits: Boolean;
    FConditionalDefines: string;
    FConsoleApplication: Boolean;
    FDCPOutputDirectory: string;
    FDCUOutputDir: string;
    FEXEOutputDir: string;
    FImageBaseAddr: DWord;
    FIncludeDirectories: string;
    FMapFileLevel: TJclDCCMapFileLevel;
    FMaxStackSize: Integer;
    FMinStackSize: Integer;
    FObjectDirectories: string;
    FOutputHints: Boolean;
    FOutputWarnings: Boolean;
    FPackages: string;
    FResourceDirectories: string;
    FUnitAliases: string;
    FUnitDirectories: string;
  public
    constructor Create;
    destructor Destroy; override;

    property BPLOutputDirectory: string read FBPLOutputDirectory write FBPLOutputDirectory;
    property BuildAllUnits: Boolean read FBuildAllUnits write FBuildAllUnits;
    //(usc) TJclDCCEx name is Defines only
    property ConditionalDefines: string read FConditionalDefines write FConditionalDefines;
    property ConsoleApplication: Boolean read FConsoleApplication write FConsoleApplication;
    property DCPOutputDirectory: string read FDCPOutputDirectory write FDCPOutputDirectory;
    //(usc) DCU & EXE dir are named DCU/EXEOutputDir in TJclDCCEx!
    property DCUOutputDirectory: string read FDCUOutputDir write FDCUOutputDir;
    property EXEOutputDirectory: string read FEXEOutputDir write FEXEOutputDir;
    property ImageBaseAddr: DWord read FImageBaseAddr write FImageBaseAddr;
    property IncludeDirectories: string read FIncludeDirectories write FIncludeDirectories;
    property MapFileLevel: TJclDCCMapFileLevel read FMapFileLevel write FMapFileLevel;    
    property MaxStackSize: Integer read FMaxStackSize write FMaxStackSize;
    property MinStackSize: Integer read FMinStackSize write FMinStackSize;
    property ObjectDirectories: string read FObjectDirectories write FObjectDirectories;
    property OutputHints: Boolean read FOutputHints write FOutputHints;
    property OutputWarnings: Boolean read FOutputWarnings write FOutputWarnings;
    property Packages: string read FPackages write FPackages;
    property ResourceDirectories: string read FResourceDirectories write FResourceDirectories;
    property UnitAliases: string read FUnitAliases write FUnitAliases;
    property UnitDirectories: string read FUnitDirectories write FUnitDirectories;
  end;

  TJclDCCConfigFile = class(TJclCustomDCCConfig)
  public
    procedure LoadFromFile(AFileName: string);
  end;

  TJclDOFFile = class(TJclCustomDCCConfig)
  public
    procedure LoadFromFile(AFileName: string);
  end;

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
    FBuildAllUnits: Boolean;
    FQuietCompile: Boolean;
    procedure CaptureLine(const Line: string; var Aborted: Boolean);
    procedure ClearValues;
  public
    constructor Create;
    destructor Destroy; override;

    function Compile: Boolean;

    property BuildAllUnits: Boolean read FBuildAllUnits write FBuildAllUnits;
    property QuietCompile: Boolean read FQuietCompile write FQuietCompile;
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

uses
  {$IFDEF DELPHI5}
  FileCtrl,
  {$ENDIF DELPHI5}
  JclStrings;

constructor TJclCustomDCCConfig.Create;
begin
  inherited Create;
  FBPLOutputDirectory := '';
  FBuildAllUnits := True;
  FConditionalDefines := '';
  FConsoleApplication := False;
  FDCPOutputDirectory := '';
  FDCUOutputDir:= '';
  FEXEOutputDir:= '';
  FImageBaseAddr := $400000;
  FIncludeDirectories := '';
  FMapFileLevel := mfloff;
  FMaxStackSize := 1048576;
  FMinStackSize := 16384;
  FObjectDirectories := '';
  FOutputHints := True;
  FOutputWarnings := True;
  FPackages := '';
  FResourceDirectories := '';
  FUnitAliases := '';
  FUnitDirectories := '';
end;

destructor TJclCustomDCCConfig.Destroy;
begin
  inherited Destroy;
end;

procedure TJclDCCConfigFile.LoadFromFile(AFileName: string);

  function CheckPathOption(const AOptionPrefix: string; AOption: string; var ADestPath: string): Boolean;
  begin
    Result := Pos(AOptionPrefix, AOption) = 1;
    if Result then
    begin
      Delete(AOption, 1, Length(AOptionPrefix));
      ADestPath := StrTrimQuotes(AOption);
    end;
  end;

var
  ConfigStrings: TStringList;
  I: Integer;
  S, S2: string;
begin
  ConfigStrings := TStringList.Create;
  try
    //(usc) do FileExists check ?
    ConfigStrings.LoadFromFile(AFileName);
    MapFileLevel := mfloff; //(usc) set to off ?
    for I := 0 to Pred(ConfigStrings.Count) do
    begin
      S := ConfigStrings[I];
      if Pos('-GS', S) = 1 then
        MapFileLevel := mflsegments
      else
      if Pos('-GP', S) = 1 then
        MapFileLevel := mflpublics
      else
      if Pos('-GD', S) = 1 then
        MapFileLevel := mfldetailed
      else

      // JPNE (linker output)

      if Pos('-cc', S) = 1 then
        ConsoleApplication := True
      else
      if Pos('-cg', S) = 1 then
        ConsoleApplication := False
      else

      // VN (TD32 debug info)
      // VR (remote debug symbols)

      if Pos('-A', S) = 1 then
      begin
        Delete(S, 1, 2);
        UnitAliases := S;
      end
      else
      if Pos('-H', S) = 1 then
        OutputHints := Pos('+', S) = 3
      else
      if Pos('-W', S) = 1 then
        OutputWarnings := Pos('+', S) = 3
      else
      if Pos('-M', S) = 1 then
        BuildAllUnits := False
      else
      if Pos('-B', S) = 1 then //(usc) not found in .cfg file - but used as opposite of -M ?
        BuildAllUnits := True
      else
      if (Pos('-$M', S) = 1) and (Pos('-$M-', S) = 0) and (Pos('-$M+', S) = 0) and
        (Pos(',', S) > 3) then
      begin
        Delete(S, 1, 3);
        S2 := Copy(S, 1, Pos(',', S) - 1);
        MinStackSize := StrToIntDef(S2, MinStackSize);
        S2 := S;
        Delete(S2, 1, Pos(',', S));
        MaxStackSize := StrToIntDef(S2, MaxStackSize);
      end
      else
      if Pos('-K', S) = 1 then
      begin
        Delete(S, 1, 2);
        ImageBaseAddr := StrToIntDef(S, ImageBaseAddr);
      end
      else
      if CheckPathOption('-E', S, S2) then
        EXEOutputDirectory := S2
      else
      if CheckPathOption('-N', S, S2) then
        DCUOutputDirectory := S2
      else
      if CheckPathOption('-LE', S, S2) then
        BPLOutputDirectory := S2
      else
      if CheckPathOption('-LN', S, S2) then
        DCPOutputDirectory := S2
      else
      if CheckPathOption('-U', S, S2) then
        UnitDirectories := S2
      else
      if CheckPathOption('-O', S, S2) then
        ObjectDirectories := S2
      else
      if CheckPathOption('-I', S, S2) then
        IncludeDirectories := S2
      else
      if CheckPathOption('-R', S, S2) then
        ResourceDirectories := S2
      else
      if Pos('-D', S) = 1 then
      begin
        Delete(S, 1, 2);
        ConditionalDefines := S;
      end
      else
      if Pos('-LU', S) = 1 then
      begin
        Delete(S, 1, 3);
        Packages := S;
      end;
    end;
  finally
    ConfigStrings.Free;
  end;
end;

procedure TJclDOFFile.LoadFromFile(AFileName: string);

  function CheckPathOption(const AOptionPrefix: string; AOption: string; var ADestPath: string): Boolean;
  begin
    Result := Pos(AOptionPrefix, AOption) = 1;
    if Result then
    begin
      Delete(AOption, 1, Length(AOptionPrefix));
      ADestPath := StrTrimQuotes(AOption);
    end;
  end;

const
  CompilerSection = 'Compiler';
  LinkerSection = 'Linker';
  DirectoriesSection = 'Directories';

var
  DOFFile: TIniFile;
  SearchPath: string;
begin
  //(usc) do FileExists check ?
  DOFFile := TIniFile.Create(AFileName);
  try
    //todo Usepackages
    case DOFFile.ReadInteger(LinkerSection, 'MapFile', 0) of
      0: MapFileLevel := mfloff;
      1: MapFileLevel := mflsegments;
      2: MapFileLevel := mflpublics;
      3: MapFileLevel := mfldetailed;
    end;
    // JPNE (linker output)
    ConsoleApplication := DOFFile.ReadInteger(LinkerSection, 'ConsoleApp', 0) = 1;
    // VN (TD32 debug info)
    // VR (remote debug symbols)
    UnitAliases := DOFFile.ReadString(CompilerSection, 'UnitAliases', '');
    OutputHints := DOFFile.ReadInteger(CompilerSection, 'ShowHints', 0) = 1;
    OutputWarnings := DOFFile.ReadInteger(CompilerSection, 'ShowWarnings', 0) = 1;
{
      if Pos('-M', S) = 1 then
        FBuildAllUnits := False
      else
      if Pos('-B', S) = 1 then //(usc) not found in .cfg file - but used as opposite of -M ?
        FBuildAllUnits := True
      else
}
    MinStackSize := DOFFile.ReadInteger(LinkerSection, 'MinStackSize', MinStackSize);
    MaxStackSize := DOFFile.ReadInteger(LinkerSection, 'MaxStackSize', MaxStackSize);
    ImageBaseAddr := DOFFile.ReadInteger(LinkerSection, 'ImageBase', ImageBaseAddr);

    EXEOutputDirectory := DOFFile.ReadString(DirectoriesSection, 'OutputDir', '');
    DCUOutputDirectory := DOFFile.ReadString(DirectoriesSection, 'UnitOutputDir', '');
    BPLOutputDirectory := DOFFile.ReadString(DirectoriesSection, 'PackageDLLOutputDir', '');
    DCPOutputDirectory := DOFFile.ReadString(DirectoriesSection, 'PackageDCPOutputDir', '');

    SearchPath := DOFFile.ReadString(DirectoriesSection, 'SearchPath', '');
    UnitDirectories := SearchPath;
    ObjectDirectories := SearchPath;
    IncludeDirectories := SearchPath;
    ResourceDirectories := SearchPath;

    ConditionalDefines := DOFFile.ReadString(DirectoriesSection, 'Conditionals', '');
    Packages := DOFFile.ReadString(DirectoriesSection, 'Packages', '');
  finally
    DOFFile.Free;
  end;
end;

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
  FBuildAllUnits := True;
  FQuietCompile := False;
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
{
 open arguments
 - F
 - J
 - JP
 - LE (not listed in dcc help)
 - LN (not listed in dcc help)
 - P
 - Z
}
  ClearValues;
  Result := False;
  if FileExists(FFileToCompile) and (FExeName <> '') then
  begin
    Arguments := FFileToCompile;
    if DirectoryExists(FEXEOutputDir) then
      Arguments := Arguments + Format(' -E%s', [FEXEOutputDir]);
    if DirectoryExists(FDCUOutputDir) then
      Arguments := Arguments + Format(' -N%s', [FDCUOutputDir]);
    Arguments := Arguments + Format(' -O%s', [FSearchPaths]);      
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
    if FBuildAllUnits then
      Arguments := Arguments + ' -B'
    else
      Arguments := Arguments + ' -M';  //(usc) is this necessary ?
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

    if FQuietCompile then
      Arguments := Arguments + ' -Q';

    Result := (ExecAndCapture(FExeName + ' ' + Arguments, CaptureLine) = 0) and
      (FErrorCount = 0) and (FFatalCount = 0);
  end;
end;

end.



