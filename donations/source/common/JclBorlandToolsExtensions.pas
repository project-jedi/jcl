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
{ Last modified: February 12, 2005                                                                 }
{                                                                                                  }
{**************************************************************************************************}

{
Todo:
- style:
  *done*- sort class members
  - insert markers
*done*- support Aborted and set current dir in ...ExecAndCapture
        (JclSysUtils.Execute does support it)
- IFDEF OS specific compiler switches and defines
*almost done*- get settings from .cfg or .dof

*done*- move ExecAndCapture to a unit where it fit's more
   (the same should be considered for JclBorlandTools.ExecAndRedirectOutput)  
}

unit JclBorlandToolsExtensions;

{$I jcl.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Libc, Types,
  {$ENDIF LINUX}
  {$IFNDEF RTL140_UP}
  JclBase,
  {$ENDIF ~RTL140_UP}
  SysUtils, Classes, IniFiles, Contnrs, JclSysUtils, JclBorlandTools;

type
  TJclDCCMessageKind = (mkUnknown, mkHint, mkWarning, mkError, mkFatal);
  TJclDCCMapFileLevel = (mfloff, mflsegments, mflpublics, mfldetailed);

  //(usc) need option "MakeModifiedUnits" (-M) ?
  //   I guess -M is opposite of -B
  TJclCustomDCCConfig = class(TObject)
  private
    FBPLOutputDirectory: string;
    FBuildAllUnits: Boolean;
    FCompilerSwitches: string;
    FCompileWithPackages: Boolean;
    FConditionalDefines: string;
    FConsoleApplication: Boolean;
    FDCPOutputDirectory: string;
    FDCUOutputDir: string;
    FEXEOutputDir: string;
    FImageBaseAddr: DWord;
    FIncludeDirectories: string;
    FMapFileLevel: TJclDCCMapFileLevel;
    FMaxStackSize: DWord;
    FMinStackSize: DWord;
    FObjectDirectories: string;
    FOutputHints: Boolean;
    FOutputWarnings: Boolean;
    FPackages: string;
    FRemoteDebugSymbols: Boolean;
    FResourceDirectories: string;
    FSearchPaths: string;
    FTD32DebugInfo: Boolean;
    FUnitAliases: string;
    FUnitDirectories: string;
    procedure SetSearchPaths(ASearchPaths: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(ACustomConfig: TJclCustomDCCConfig);

    property BPLOutputDirectory: string read FBPLOutputDirectory write FBPLOutputDirectory;
    property BuildAllUnits: Boolean read FBuildAllUnits write FBuildAllUnits;
    //(usc) better as TJclDCCSwitches
    property CompilerSwitches: string read FCompilerSwitches write FCompilerSwitches;
    property CompileWithPackages: Boolean read FCompileWithPackages write FCompileWithPackages;
    //(usc) TJclDCCEx name was Defines only
    property ConditionalDefines: string read FConditionalDefines write FConditionalDefines;
    property ConsoleApplication: Boolean read FConsoleApplication write FConsoleApplication;
    property DCPOutputDirectory: string read FDCPOutputDirectory write FDCPOutputDirectory;
    //(usc) DCU & EXE dir were named DCU/EXEOutputDir in TJclDCCEx!
    property DCUOutputDirectory: string read FDCUOutputDir write FDCUOutputDir;
    property EXEOutputDirectory: string read FEXEOutputDir write FEXEOutputDir;
    //(usc) was only ImageBase in TJclDCCEx
    property ImageBaseAddr: DWord read FImageBaseAddr write FImageBaseAddr;
    property IncludeDirectories: string read FIncludeDirectories write FIncludeDirectories;
    property MapFileLevel: TJclDCCMapFileLevel read FMapFileLevel write FMapFileLevel;
    property MaxStackSize: DWord read FMaxStackSize write FMaxStackSize;
    property MinStackSize: DWord read FMinStackSize write FMinStackSize;
    property ObjectDirectories: string read FObjectDirectories write FObjectDirectories;
    property OutputHints: Boolean read FOutputHints write FOutputHints;
    property OutputWarnings: Boolean read FOutputWarnings write FOutputWarnings;
    //(usc) better as StringList
    property Packages: string read FPackages write FPackages;
    property RemoteDebugSymbols: Boolean read FRemoteDebugSymbols write FRemoteDebugSymbols;
    property ResourceDirectories: string read FResourceDirectories write FResourceDirectories;
    //(usc) better as StringList
    property SearchPaths: string read FSearchPaths write SetSearchPaths;
    property TD32DebugInfo: Boolean read FTD32DebugInfo write FTD32DebugInfo;
    property UnitAliases: string read FUnitAliases write FUnitAliases;
    property UnitDirectories: string read FUnitDirectories write FUnitDirectories;
  end;

  TJclDCCConfigFile = class(TJclCustomDCCConfig)
  public
    procedure LoadFromFile(AFileName: string);
  end;

  TJclDOFSaveQueryEvent = function(ASender: TObject; const ASection, AIdent: string): Boolean of object;

  TJclDOFFile = class(TJclCustomDCCConfig)
  private
    FContent: TStringList;
    function StrippedSaveQuery(ASender: TObject; const ASection, AIdent: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(AFileName: string);
    procedure SaveToFile(AFileName: string);
    procedure SaveToFileQuery(AFileName: string; AQueryProc: TJclDOFSaveQueryEvent);
    procedure SaveToFileStripped(AFileName: string);
  end;

  TJclDCCMessage = class(TObject)
  private
    FKind: TJclDCCMessageKind;
    FMessageStr: string;
    function GetText: string;
  public
    constructor Create(AKind: TJclDCCMessageKind; AMessageStr: string);
    //(usc) add File and Line (currently part of MessageStr)
    property Kind: TJclDCCMessageKind read FKind;
    property MessageStr: string read FMessageStr;
    property Text: string read GetText;
  end;

  TJclDCCMessages = class(TObject)
  private
    FErrorCount: Integer;
    FFatalCount: Integer;
    FHintCount: Integer;
    FItems: TObjectList;
    FWarnCount: Integer;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TJclDCCMessage;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AKind: TJclDCCMessageKind; AMessageStr: string);
    procedure Clear;
    property Count: Integer read GetCount;
    property ErrorCount: Integer read FErrorCount;
    property FatalCount: Integer read FFatalCount;
    property HintCount: Integer read FHintCount;
    property Items[AIndex: Integer]: TJclDCCMessage read GetItems; default;
    property WarnCount: Integer read FWarnCount;
  end;

  TJclDCCEx = class(TObject)
  private
    FConfig: TJclCustomDCCConfig;
    FCurrentFile: string;
    FCurrentLineNo: Integer;
    FExeName: string;
    FFileToCompile: string;
    FMessages: TJclDCCMessages;
    FOnCompileProgress: TNotifyEvent;
    FOnMessage: TNotifyEvent;
    FPlainOutput: TStringList;
    FQuietCompile: Boolean;
    procedure CaptureLine(const Line: string);
    procedure ClearValues;
  public
    constructor Create;
    destructor Destroy; override;
    function Compile(AbortPtr: PBoolean = nil): Boolean;
    property Config: TJclCustomDCCConfig read FConfig;
    property CurrentFile: string read FCurrentFile;
    property CurrentLineNo: Integer read FCurrentLineNo;
    property ExeName: string read FExeName write FExeName;
    property FileToCompile: string read FFileToCompile write FFileToCompile;
    property Messages: TJclDCCMessages read FMessages;
    property OnCompileProgress: TNotifyEvent read FOnCompileProgress write FOnCompileProgress;
    property OnMessage: TNotifyEvent read FOnMessage write FOnMessage;
    property PlainOutput: TStringList read FPlainOutput;
    property QuietCompile: Boolean read FQuietCompile write FQuietCompile;
  end;

  TJclBorRADToolIdeExperts = class(TJclBorRADToolInstallationObject)
  private
    FExperts: TStringList;
    function GetCount: Integer;
    function GetFileNames(Index: Integer): string;
    function GetNames(Index: Integer): string;
  protected
    constructor Create(AInstallation: TJclBorRADToolInstallation);
    procedure ReadExperts;
  public
    destructor Destroy; override;
    function AddExpert(const AName, AFileName: string): Boolean;
    function RemoveExpert(const AName: string): Boolean;
    property Count: Integer read GetCount;
    property FileNames[Index: Integer]: string read GetFileNames;
    property Names[Index: Integer]: string read GetNames;
  end;

  TJclBorRADToolInstallationEx = class(TJclBorRADToolInstallation)
  private
    FIdeExperts: TJclBorRADToolIdeExperts;
    function GetIdeExperts: TJclBorRADToolIdeExperts;
    function GetVCSManager: string;
    procedure SetVCSManager(const Value: string);
  protected
    constructor Create(const AConfigDataLocation: string); override;
  public
    destructor Destroy; override;
    function SupportsVCSManager: Boolean;
    property IdeExperts: TJclBorRADToolIdeExperts read GetIdeExperts;
    property VCSManager: string read GetVCSManager write SetVCSManager;
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
  FCompilerSwitches := '';
  FCompileWithPackages := False;
  FConditionalDefines := '';
  FConsoleApplication := False;
  FDCPOutputDirectory := '';
  FDCUOutputDir:= '';
  FEXEOutputDir:= '';
  FImageBaseAddr := $400000;
  FIncludeDirectories := '';
  FMapFileLevel := mfloff;
  FMaxStackSize := 1048576; //old TJclDCCExValue $100000 (same value as hex)
  FMinStackSize := 16384; //old TJclDCCExValue $4000 (same value as hex)
  FObjectDirectories := '';
  FOutputHints := True; //old TJclDCCExValue False
  FOutputWarnings := True; //old TJclDCCExValue False
  FPackages := '';
  FRemoteDebugSymbols := False;
  FResourceDirectories := '';
  FSearchPaths := '';
  FTD32DebugInfo := False;
  FUnitAliases := '';
  FUnitDirectories := '';
end;

destructor TJclCustomDCCConfig.Destroy;
begin
  inherited Destroy;
end;

procedure TJclCustomDCCConfig.Assign(ACustomConfig: TJclCustomDCCConfig);
begin
  FBPLOutputDirectory := ACustomConfig.BPLOutputDirectory;
  FBuildAllUnits := ACustomConfig.BuildAllUnits;
  FCompilerSwitches := ACustomConfig.CompilerSwitches;
  FCompileWithPackages := ACustomConfig.CompileWithPackages;
  FConditionalDefines := ACustomConfig.ConditionalDefines;
  FConsoleApplication := ACustomConfig.ConsoleApplication;
  FDCPOutputDirectory := ACustomConfig.DCPOutputDirectory;
  FDCUOutputDir := ACustomConfig.DCUOutputDirectory;
  FEXEOutputDir := ACustomConfig.EXEOutputDirectory;
  FImageBaseAddr := ACustomConfig.ImageBaseAddr;
  FIncludeDirectories := ACustomConfig.IncludeDirectories;
  FMapFileLevel := ACustomConfig.MapFileLevel;
  FMaxStackSize := ACustomConfig.MaxStackSize;
  FMinStackSize := ACustomConfig.MinStackSize;
  FObjectDirectories := ACustomConfig.ObjectDirectories;
  FOutputHints := ACustomConfig.OutputHints;
  FOutputWarnings := ACustomConfig.OutputWarnings;
  FPackages := ACustomConfig.Packages;
  FRemoteDebugSymbols := ACustomConfig.RemoteDebugSymbols;
  FResourceDirectories := ACustomConfig.ResourceDirectories;
  FSearchPaths := ACustomConfig.SearchPaths;
  FTD32DebugInfo := ACustomConfig.TD32DebugInfo;
  FUnitAliases := ACustomConfig.UnitAliases;
  FUnitDirectories := ACustomConfig.UnitDirectories;
end;

procedure TJclCustomDCCConfig.SetSearchPaths(ASearchPaths: string);
begin
  if FSearchPaths <> ASearchPaths then
  begin
    FSearchPaths := ASearchPaths;
    FUnitDirectories := FSearchPaths;
    FObjectDirectories := FSearchPaths;
    FIncludeDirectories := FSearchPaths;
    FResourceDirectories := FSearchPaths;
  end;
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
    CompilerSwitches := '';
    for I := 0 to Pred(ConfigStrings.Count) do
    begin
      S := ConfigStrings[I];
      //(usc) read compiler switches
      if (Pos('-$', S) = 1) and (Length(S) = 4) and (Pos(S, CompilerSwitches) = 0) then
      begin
        if CompilerSwitches <> '' then
          CompilerSwitches := CompilerSwitches + ' ';
        CompilerSwitches := CompilerSwitches + S;
      end
      else
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
      if Pos('-vn', S) = 1 then
        TD32DebugInfo := True
      else
      if Pos('-vr', S) = 1 then
        RemoteDebugSymbols := True
      else
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
        CompileWithPackages := S <> '';
      end;
    end;
  finally
    ConfigStrings.Free;
  end;
end;

const
  JclDOFCompilerSection = 'Compiler';
  JclDOFLinkerSection = 'Linker';
  JclDOFDirectoriesSection = 'Directories';

  JclDOFUnitAliasesEntry = 'UnitAliases';
  JclDOFShowHintsEntry = 'ShowHints';
  JclDOFShowWarningsEntry = 'ShowWarnings';

  JclDOFMapFileEntry = 'MapFile';
  JclDOFConsoleAppEntry = 'ConsoleApp';
  JclDOFDebugInfoEntry = 'DebugInfo';
  JclDOFRemoteSymbolsEntry = 'RemoteSymbols';
  JclDOFMinStackSizeEntry = 'MinStackSize';
  JclDOFMaxStackSizeEntry = 'MaxStackSize';
  JclDOFImageBaseEntry = 'ImageBase';

  JclDOFOutputDirEntry = 'OutputDir';
  JclDOFUnitOutputDirEntry = 'UnitOutputDir';
  JclDOFPackageDLLOutputDirEntry = 'PackageDLLOutputDir';
  JclDOFPackageDCPOutputDirEntry = 'PackageDCPOutputDir';
  JclDOFSearchPathEntry = 'SearchPath';
  JclDOFConditionalsEntry = 'Conditionals';
  JclDOFPackagesEntry = 'Packages';
  JclDOFUsePackagesEntry = 'UsePackages';

constructor TJclDOFFile.Create;
begin
  inherited Create;
  FContent := TStringList.Create;
end;

destructor TJclDOFFile.Destroy;
begin
  FContent.Free;
  inherited Destroy;
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

var
  DOFFile: TMemIniFile;
begin
  //(usc) do FileExists check ?
  DOFFile := TMemIniFile.Create(AFileName);
  try
    FContent.Clear;
    DOFFile.GetStrings(FContent);
    //(usc) read compiler switches
    case DOFFile.ReadInteger(JclDOFLinkerSection, JclDOFMapFileEntry, 0) of
      0: MapFileLevel := mfloff;
      1: MapFileLevel := mflsegments;
      2: MapFileLevel := mflpublics;
      3: MapFileLevel := mfldetailed;
    end;
    // JPNE (linker output)
    ConsoleApplication := DOFFile.ReadInteger(JclDOFLinkerSection, JclDOFConsoleAppEntry, 0) = 1;
    TD32DebugInfo := DOFFile.ReadInteger(JclDOFLinkerSection, JclDOFDebugInfoEntry, 0) = 1;
    RemoteDebugSymbols := DOFFile.ReadInteger(JclDOFLinkerSection, JclDOFRemoteSymbolsEntry, 0) = 1;

    UnitAliases := DOFFile.ReadString(JclDOFCompilerSection, JclDOFUnitAliasesEntry, '');
    OutputHints := DOFFile.ReadInteger(JclDOFCompilerSection, JclDOFShowHintsEntry, 0) = 1;
    OutputWarnings := DOFFile.ReadInteger(JclDOFCompilerSection, JclDOFShowWarningsEntry, 0) = 1;
{
      if Pos('-M', S) = 1 then
        FBuildAllUnits := False
      else
      if Pos('-B', S) = 1 then //(usc) not found in .cfg file - but used as opposite of -M ?
        FBuildAllUnits := True
      else
}
    MinStackSize := DOFFile.ReadInteger(JclDOFLinkerSection, JclDOFMinStackSizeEntry, MinStackSize);
    MaxStackSize := DOFFile.ReadInteger(JclDOFLinkerSection, JclDOFMaxStackSizeEntry, MaxStackSize);
    ImageBaseAddr := DOFFile.ReadInteger(JclDOFLinkerSection, JclDOFImageBaseEntry, ImageBaseAddr);

    EXEOutputDirectory := DOFFile.ReadString(JclDOFDirectoriesSection, JclDOFOutputDirEntry, '');
    DCUOutputDirectory := DOFFile.ReadString(JclDOFDirectoriesSection, JclDOFUnitOutputDirEntry, '');
    BPLOutputDirectory := DOFFile.ReadString(JclDOFDirectoriesSection, JclDOFPackageDLLOutputDirEntry, '');
    DCPOutputDirectory := DOFFile.ReadString(JclDOFDirectoriesSection, JclDOFPackageDCPOutputDirEntry, '');

    SearchPaths := DOFFile.ReadString(JclDOFDirectoriesSection, JclDOFSearchPathEntry, '');

    ConditionalDefines := DOFFile.ReadString(JclDOFDirectoriesSection, JclDOFConditionalsEntry, '');
    Packages := DOFFile.ReadString(JclDOFDirectoriesSection, JclDOFPackagesEntry, '');
    CompileWithPackages := DOFFile.ReadInteger(JclDOFLinkerSection, JclDOFUsePackagesEntry, 0) = 1;
  finally
    DOFFile.Free;
  end;
end;

procedure TJclDOFFile.SaveToFile(AFileName: string);
begin
  SaveToFileQuery(AFileName, nil);
end;

procedure TJclDOFFile.SaveToFileQuery(AFileName: string; AQueryProc: TJclDOFSaveQueryEvent);

  function DoQuery(const ASection, AIdent: string): Boolean;
  begin
    Result := (not Assigned(AQueryProc)) or AQueryProc(Self, ASection, AIdent);
  end;

var
  DOFFile: TMemIniFile;
  tempInt: Integer;
  I, J: Integer;
  Sections, Idents: TStringList;
begin
  DOFFile := TMemIniFile.Create(AFileName);
  try
    DOFFile.SetStrings(FContent);
    DOFFile.WriteString(JclDOFCompilerSection, JclDOFUnitAliasesEntry, UnitAliases);
    DOFFile.WriteBool(JclDOFCompilerSection, JclDOFShowHintsEntry, OutputHints);
    DOFFile.WriteBool(JclDOFCompilerSection, JclDOFShowWarningsEntry, OutputWarnings);
    case MapFileLevel of
      mfloff: tempInt := 0;
      mflsegments: tempInt := 1;
      mflpublics: tempInt := 2;
      mfldetailed: tempInt := 3;
      else
        tempInt := 0;
    end;
    DOFFile.WriteInteger(JclDOFLinkerSection, JclDOFMapFileEntry, tempInt);
    DOFFile.WriteBool(JclDOFLinkerSection, JclDOFConsoleAppEntry, ConsoleApplication);
    DOFFile.WriteBool(JclDOFLinkerSection, JclDOFDebugInfoEntry, TD32DebugInfo);
    DOFFile.WriteBool(JclDOFLinkerSection, JclDOFRemoteSymbolsEntry, RemoteDebugSymbols);
    DOFFile.WriteInteger(JclDOFLinkerSection, JclDOFMinStackSizeEntry, MinStackSize);
    DOFFile.WriteInteger(JclDOFLinkerSection, JclDOFMaxStackSizeEntry, MaxStackSize);
    DOFFile.WriteInteger(JclDOFLinkerSection, JclDOFImageBaseEntry, ImageBaseAddr);

    DOFFile.WriteString(JclDOFDirectoriesSection, JclDOFOutputDirEntry, EXEOutputDirectory);
    DOFFile.WriteString(JclDOFDirectoriesSection, JclDOFUnitOutputDirEntry, DCUOutputDirectory);
    DOFFile.WriteString(JclDOFDirectoriesSection, JclDOFPackageDLLOutputDirEntry, BPLOutputDirectory);
    DOFFile.WriteString(JclDOFDirectoriesSection, JclDOFPackageDCPOutputDirEntry, DCPOutputDirectory);
    DOFFile.WriteString(JclDOFDirectoriesSection, JclDOFSearchPathEntry, SearchPaths);
    DOFFile.WriteString(JclDOFDirectoriesSection, JclDOFConditionalsEntry, ConditionalDefines);
    DOFFile.WriteString(JclDOFDirectoriesSection, JclDOFPackagesEntry, Packages);
    DOFFile.WriteBool(JclDOFLinkerSection, JclDOFUsePackagesEntry, CompileWithPackages);

    Sections := TStringList.Create;
    try
      DOFFile.ReadSections(Sections);
      for I := 0 to Sections.Count - 1 do
        if not DoQuery(Sections[I], '') then
          DOFFile.EraseSection(Sections[I])
        else
        begin
          Idents := TStringList.Create;
          try
            DOFFile.ReadSection(Sections[I], Idents);
            for J := 0 to Idents.Count - 1 do
              if not DoQuery(Sections[I], Idents[J]) then
                DOFFile.DeleteKey(Sections[I], Idents[J]);
          finally
            Idents.Free;
          end;
        end;
    finally
      Sections.Free;
    end;
    DOFFile.UpdateFile;
  finally
    DOFFile.Free;
  end;
end;

procedure TJclDOFFile.SaveToFileStripped(AFileName: string);
begin
  SaveToFileQuery(AFileName, StrippedSaveQuery);
end;

function TJclDOFFile.StrippedSaveQuery(ASender: TObject; const ASection, AIdent: string): Boolean;
begin
  Result := False;
  if ASection = JclDOFDirectoriesSection then
  begin
    if AIdent = '' then
      Result := True
    else
    if AIdent = JclDOFOutputDirEntry then
      Result := EXEOutputDirectory <> ''
    else
    if AIdent = JclDOFUnitOutputDirEntry then
      Result := DCUOutputDirectory <> ''
    else
    if AIdent = JclDOFPackageDLLOutputDirEntry then
      Result := BPLOutputDirectory <> ''
    else
    if AIdent = JclDOFPackageDCPOutputDirEntry then
      Result := DCPOutputDirectory <> ''
    else
    if AIdent = JclDOFSearchPathEntry then
      Result := SearchPaths <> ''
    else
    if AIdent = JclDOFConditionalsEntry then
      Result := ConditionalDefines <> ''
    else
    if (AIdent = JclDOFPackagesEntry) and (AIdent = JclDOFUsePackagesEntry) then
      Result := (Packages <> '') and CompileWithPackages;
  end;
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

constructor TJclDCCMessage.Create(AKind: TJclDCCMessageKind; AMessageStr: string);
begin
  inherited Create;
  FKind := AKind;
  FMessageStr := AMessageStr;
end;

function TJclDCCMessage.GetText: string;
var
  KindStr: string;
begin
  case FKind of
    mkHint: KindStr :='[Hint]';
    mkWarning: KindStr := '[Warning]';
    mkError: KindStr := '[Error]';
    mkFatal: KindStr := '[Fatal Error]';
    else
      KindStr := '';
  end;

  Result := Format('%s %s', [KindStr, FMessageStr]);
end;

constructor TJclDCCMessages.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
  Clear;
end;

destructor TJclDCCMessages.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TJclDCCMessages.Add(AKind: TJclDCCMessageKind; AMessageStr: string);
var
  DCCMessage: TJclDCCMessage;
begin
  DCCMessage := TJclDCCMessage.Create(AKind, AMessageStr);
  FItems.Add(DCCMessage);

  case AKind of
    mkHint: Inc(FHintCount);
    mkWarning: Inc(FWarnCount);
    mkError: Inc(FErrorCount);
    mkFatal: Inc(FErrorCount);
  end;
end;

procedure TJclDCCMessages.Clear;
begin
  FItems.Clear;
  FErrorCount := 0;
  FHintCount := 0;
  FWarnCount := 0;
  FFatalCount := 0;
end;

function TJclDCCMessages.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJclDCCMessages.GetItems(AIndex: Integer): TJclDCCMessage;
begin
  Result := TJclDCCMessage(FItems[AIndex]);
end;

constructor TJclDCCEx.Create;
begin
  inherited Create;
  FPlainOutput := TStringList.Create;
  FMessages := TJclDCCMessages.Create;
  ClearValues;
  FOnCompileProgress := nil;
  FQuietCompile := False;
  FOnMessage := nil;
  FConfig := TJclCustomDCCConfig.Create;
end;

destructor TJclDCCEx.Destroy;
begin
  FConfig.Free;
  FMessages.Free;
  FPlainOutput.Free;
  inherited Destroy;
end;

procedure TJclDCCEx.CaptureLine(const Line: string);
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
  if LineErr <> mkUnknown then
  begin
    FMessages.Add(LineErr, Format('%s%s', [FilePart, LineMsg]));
    if Assigned(FOnMessage) then
      FOnMessage(nil);
  end;
  if Assigned(FOnCompileProgress) and HasPos then
    FOnCompileProgress(nil);
end;

procedure TJclDCCEx.ClearValues;
begin
  FPlainOutput.Clear;
  FCurrentFile := '';
  FCurrentLineNo := 0;
  FMessages.Clear;
end;

function TJclDCCEx.Compile(AbortPtr: PBoolean = nil): Boolean;
var
  Arguments: string;
  DCCExitCode: Integer;
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
    if DirectoryExists(FConfig.EXEOutputDirectory) then
      Arguments := Arguments + Format(' -E%s', [FConfig.EXEOutputDirectory]);
    if DirectoryExists(FConfig.DCUOutputDirectory) then
      Arguments := Arguments + Format(' -N%s', [FConfig.DCUOutputDirectory]);
    Arguments := Arguments + Format(' -O%s', [FConfig.ObjectDirectories]);
    Arguments := Arguments + Format(' -I%s', [FConfig.IncludeDirectories]);
    Arguments := Arguments + Format(' -R%s', [FConfig.ResourceDirectories]);
    Arguments := Arguments + Format(' -U%s', [FConfig.UnitDirectories]);
    if FConfig.CompileWithPackages and (FConfig.Packages <> '') then
      Arguments := Arguments + Format(' -LU%s', [FConfig.Packages]);
    if FConfig.ConditionalDefines <> '' then
      Arguments := Arguments + Format(' -D%s', [FConfig.ConditionalDefines]);
    case FConfig.MapFileLevel of
      mflsegments: Arguments := Arguments + ' -GS';
      mflpublics: Arguments := Arguments + ' -GP';
      mfldetailed: Arguments := Arguments + ' -GD';
    end;
    if FConfig.BuildAllUnits then
      Arguments := Arguments + ' -B'
    else
      Arguments := Arguments + ' -M';  //(usc) is this necessary ?
    Arguments := Arguments + ' ' + FConfig.CompilerSwitches;
    if FConfig.UnitAliases <> '' then
      Arguments := Arguments + ' -A' + FConfig.UnitAliases;
    if FConfig.OutputWarnings then
      Arguments := Arguments + ' -W'; //(usc) test - is enabled by default ?
    if FConfig.OutputHints then
      Arguments := Arguments + ' -H'; //(usc) test - is enabled by default ?
    if FConfig.ConsoleApplication then
      Arguments := Arguments + ' -CC'
    else
      Arguments := Arguments + ' -CG';
    if FConfig.TD32DebugInfo then
      Arguments := Arguments + ' -V'; //(usc) check
    if FConfig.RemoteDebugSymbols then
      Arguments := Arguments + ' -VR';

                                        //(usc) no violation with -$M+ / -$M- ?
    //values must be decimal
    Arguments := Arguments + Format(' -$M%d,%d', [FConfig.MinStackSize, FConfig.MaxStackSize]);
    //value must be hexadecimal
    Arguments := Arguments + Format(' -K%.8x', [FConfig.ImageBaseAddr]);

    if FQuietCompile then
      Arguments := Arguments + ' -Q';

    DCCExitCode := JclSysUtils.Execute(FExeName + ' ' + Arguments, CaptureLine, True, AbortPtr);
    Result := (DCCExitCode = 0) and (FMessages.ErrorCount = 0) and (FMessages.FatalCount = 0);
  end;
end;

const
  ExpertsKeyName = 'Experts';
  VersionControlKeyName = 'Version Control';
  VersionControlVCSManager = 'VCSManager';

//==================================================================================================
// TJclBorRADToolExperts
//==================================================================================================

constructor TJclBorRADToolIdeExperts.Create(AInstallation: TJclBorRADToolInstallation);
begin
  inherited Create(AInstallation);
  FExperts := TStringList.Create;
  ReadExperts;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclBorRADToolIdeExperts.Destroy;
begin
  FreeAndNil(FExperts);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TJclBorRADToolIdeExperts.AddExpert(const AName, AFileName: string): Boolean;
begin
  Result := True;
  Installation.ConfigData.WriteString(ExpertsKeyName, AName, AFileName);
  ReadExperts;
end;

//--------------------------------------------------------------------------------------------------

function TJclBorRADToolIdeExperts.GetCount: Integer;
begin
  Result := FExperts.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclBorRADToolIdeExperts.GetFileNames(Index: Integer): string;
begin
  Result := FExperts.Values[FExperts.Names[Index]];
end;

//--------------------------------------------------------------------------------------------------

function TJclBorRADToolIdeExperts.GetNames(Index: Integer): string;
begin
  Result := FExperts.Names[Index];
end;

//--------------------------------------------------------------------------------------------------

procedure TJclBorRADToolIdeExperts.ReadExperts;
begin
  Installation.ConfigData.ReadSectionValues(ExpertsKeyName, FExperts);
end;

//--------------------------------------------------------------------------------------------------

function TJclBorRADToolIdeExperts.RemoveExpert(const AName: string): Boolean;
begin
  Result := Installation.ConfigData.ValueExists(ExpertsKeyName, AName);
  if Result then
  begin
    Installation.ConfigData.DeleteKey(ExpertsKeyName, AName);
    ReadExperts;
  end;
end;

//==================================================================================================
// TJclBorRADToolInstallationEx
//==================================================================================================

constructor TJclBorRADToolInstallationEx.Create(const AConfigDataLocation: string);
begin
  inherited Create(AConfigDataLocation);
  FIdeExperts := nil;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclBorRADToolInstallationEx.Destroy;
begin
  FreeAndNil(FIdeExperts);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

function TJclBorRADToolInstallationEx.GetIdeExperts: TJclBorRADToolIdeExperts;
begin
  if not Assigned(FIdeExperts) then
    FIdeExperts := TJclBorRADToolIdeExperts.Create(Self);
  Result := FIdeExperts;
end;

//--------------------------------------------------------------------------------------------------

function TJclBorRADToolInstallationEx.GetVCSManager: string;
begin
  Result := '';
  if SupportsVCSManager then
    Result := ConfigData.ReadString(VersionControlKeyName, VersionControlVCSManager, '');
end;

//--------------------------------------------------------------------------------------------------

procedure TJclBorRADToolInstallationEx.SetVCSManager(const Value: string);
begin
  if SupportsVCSManager then
    ConfigData.WriteString(VersionControlKeyName, VersionControlVCSManager, Value);
end;

//--------------------------------------------------------------------------------------------------

function TJclBorRADToolInstallationEx.SupportsVCSManager: Boolean;
begin
  {$IFDEF KYLIX}
  Result := False;
  {$ELSE}
  Result := VersionNumber <= 7;
  {$ENDIF KYLIX}
end;

end.



