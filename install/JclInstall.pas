{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) extension                                                        }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclInstall.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributor(s): Robert Rossmair (crossplatform & BCB support, refactoring)                       }
{                                                                                                  }
{ Last modified: $Date$                                                      }
{                                                                                                  }
{**************************************************************************************************}

// TODO/Issues:
// - Uninstall functionality lacking

unit JclInstall;

interface

{$I jcl.inc}
{$I crossplatform.inc}

uses
  SysUtils, Classes, IniFiles, Contnrs,
  JclBorlandTools, JediInstall;

type
  TJclDistribution = class;

  TJclInstallation = class
  private
    FDistribution: TJclDistribution;
    FTarget: TJclBorRADToolInstallation;
    FDebugDcuDir: string;
    FLibDir: string;
    FLibObjDir: string;
    procedure AddDialogToRepository(const DialogName: string; const DialogFileName: string;
      const DialogIconFileName: string; const Designer: string; const Ancestor: string = '');
    function GetProgressTotal: Integer;
    function GetTool: IJediInstallTool;
    procedure GetUnits(const SubDir: string; Units: TStrings);
    function InitOptions: Boolean;
    procedure InstallationStarted;
    procedure InstallationFinished;
    procedure InstallFailedOn(const InstallObj: string);
    function InstallPackage(const Name: string): Boolean;
    function InstallRunTimePackage(const BaseName: string): Boolean;
    function InstallOption(Option: TJediInstallOption): Boolean;
    function LogFileName: string;
    function MakeUnits(Debug: Boolean): Boolean;
    function MakePath(const FormatStr: string): string;
    function Description(Option: TJediInstallOption): string;
    procedure SaveOption(Option: TJediInstallOption; Value: Boolean);
    procedure SaveOptions;
    procedure Progress(Steps: Integer);
    function StoredOption(Option: TJediInstallOption; Default: Boolean = True): Boolean;
    procedure WriteLog(const Msg: string);
  protected
    constructor Create(JclDistribution: TJclDistribution; InstallTarget: TJclBorRADToolInstallation);
    function CompileLibraryUnits(const SubDir: string; Debug: Boolean): Boolean;
    {$IFDEF MSWINDOWS}
    procedure AddHelpToIdeTools;
    procedure AddHelpToOpenHelp;
    {$ENDIF MSWINDOWS}
    function BplPath: string;
    function DcpPath: string;
    procedure CleanupRepository;
    function InstallSelectedOptions: Boolean;
    function OptionSelected(Option: TJediInstallOption): Boolean;
    function ProgressWeight(Option: TJediInstallOption): Integer;
    function Run: Boolean;
    property Distribution: TJclDistribution read FDistribution;
    property Tool: IJediInstallTool read GetTool;
    property DebugDcuDir: string read FDebugDcuDir;
    property LibDir: string read FLibDir;
    property LibObjDir: string read FLibObjDir;
    property ProgressTotal: Integer read GetProgressTotal;
    property Target: TJclBorRADToolInstallation read FTarget;
  public
    destructor Destroy; override;
  end;

  TJclDistribution = class (TInterfacedObject, IJediInstall)
  private
    FJclPath: string;
    FLibDirMask: string;
    FLibDebugDirMask: string;
    FLibObjDirMask: string;
    FJclSourceDir: string;
    FJclSourcePath: string;
    FClxDialogFileName: string;
    FClxDialogIconFileName: string;
    {$IFDEF MSWINDOWS}
    FVclDialogFileName: string;
    FVclDialogSendFileName: string;
    FVclDialogIconFileName: string;
    FVclDialogSendIconFileName: string;
    {$ENDIF MSWINDOWS}
    FJclChmHelpFileName: string;
    FJclHlpHelpFileName: string;
    FJclReadmeFileName: string;
    FTool: IJediInstallTool;
    FTargetInstalls: TObjectList;
    FIniFile: TMemIniFile;
    FProgress: Integer;
    FProgressTotal: Integer;
    FProgressPercent: Integer;
    FOnStarting: TInstallationEvent;
    FOnEnding: TInstallationEvent;
    FOnProgress: TInstallationProgressEvent;
    function CreateInstall(Target: TJclBorRADToolInstallation): Boolean;
    procedure InitProgress;
    procedure InitInstallationTargets;
  protected
    constructor Create;
    function DocFileName(const BaseFileName: string): string;
    procedure InstallProgress(Steps: Integer);
    procedure SetTool(const Value: IJediInstallTool);
    procedure ShowProgress;
  public
    destructor Destroy; override;
    function FeatureInfoFileName(FeatureID: Cardinal): string;
    function InitInformation(const ApplicationFileName: string): Boolean;
    function Install: Boolean;
    function ReadmeFileName: string;
    procedure SetOnEnding(Value: TInstallationEvent);
    procedure SetOnProgress(Value: TInstallationProgressEvent);
    procedure SetOnStarting(Value: TInstallationEvent);
    function Supports(Target: TJclBorRADToolInstallation): Boolean;
    property ChmHelpFileName: string read FJclChmHelpFileName;
    property HlpHelpFileName: string read FJclHlpHelpFileName;
    property Path: string read FJclPath;
    property SourceDir: string read FJclSourceDir;
    property SourcePath: string read FJclSourcePath;
    property Tool: IJediInstallTool read FTool write SetTool;
  end;

function CreateJclInstall: IJediInstall;
function LogFileName(Target: TJclBorRADToolInstallation): string;

implementation

uses
  JclBase, JclSysInfo, JclFileUtils, JclStrings;

{ Install option data }

resourcestring
  // Products
  RsJCL             = 'JEDI Code Library';

  // Common features
  RsEnvironment     = 'Environment';
  RsEnvLibPath      = 'Add JCL to IDE Library Path';
  RsEnvBrowsingPath = 'Add JCL to IDE Browsing Path';
  RsEnvDebugDCUPath = 'Add JCL to Debug DCU Path';
  RsMake            = 'Make library units';
  RsMakeRelease     = 'Release';
  RsMakeDebug       = 'Debug';
  RsMakeVClx        = 'Visual CLX';

  RsHelpFiles       = 'Help files';
  RsIdeExperts      = 'IDE experts';
  RsJCLPackages     = 'Packages';
  RsIdeHelpHlp      = 'Add help file to IDE help system';
  RsIdeHelpChm      = 'Add HTML help to the Tools menu';
  RsCopyHppFiles    = 'Copy HPP files to %s';
  RsCopyPackagesHppFiles = 'Output HPP files to %s';

  // Product specific features
  RsJCLExceptDlg    = 'Sample Exception Dialogs in the Object Reporitory';
  RsJCLDialogVCL    = 'VCL Exception Dialog';
  RsJCLDialogVCLSnd = 'VCL Exception Dialog with Send button';
  RsJCLDialogCLX    = 'CLX Exception Dialog';
  RsJCLIdeDebug     = 'Debug Extension';
  RsJCLIdeAnalyzer  = 'Project Analyzer';
  RsJCLIdeFavorite  = 'Favorite combobox in Open/Save dialogs';
  RsJCLIdeThrNames  = 'Displaying thread names in Thread Status window';

const
  ioUndef = TJediInstallOption(-1);

  InitData: array[TJediInstallOption] of TInstallOptionData =
    (
      (Parent: ioUndef;                  // ioTarget
       Caption: ''),
      (Parent: ioTarget;                // ioJCL
       Caption: RsJCL),
      (Parent: ioJCL;                    // ioJclEnv
       Caption: RsEnvironment),
      (Parent: ioJclEnv;                 // ioJclEnvLibPath
       Caption: RsEnvLibPath),
      (Parent: ioJclEnv;                 // ioJclEnvBrowsingPath
       Caption: RsEnvBrowsingPath),
      (Parent: ioJclEnv;                 // ioJclEnvDebugDCUPath
       Caption: RsEnvDebugDCUPath),
      (Parent: ioJCL;                    // ioJclMake
       Caption: RsMake),
      (Parent: ioJclMake;                // ioJclMakeRelease
       Caption: RsMakeRelease),
      (Parent: ioJclMake;                // ioJclMakeReleaseVClx
       Caption: RsMakeVClx),
      (Parent: ioJclMake;                // ioJclMakeDebug
       Caption: RsMakeDebug),
      (Parent: ioJclMake;                // ioJclMakeDebugVClx
       Caption: RsMakeVClx),
      (Parent: ioJclMake;                // ioJclCopyHppFiles
       Caption: RsCopyHppFiles),
      (Parent: ioJCL;                    // ioJclPackages
       Caption: RsJCLPackages),
      (Parent: ioJclPackages;            // ioJclExperts
       Caption: RsIdeExperts),
      (Parent: ioJclExperts;             // ioJclExpertDebug
       Caption: RsJCLIdeDebug),
      (Parent: ioJclExperts;             // ioJclExpertAnalyzer
       Caption: RsJCLIdeAnalyzer),
      (Parent: ioJclExperts;             // ioJclExpertFavorite
       Caption: RsJCLIdeFavorite),
      (Parent: ioJclExperts;             // ioJclExpertsThrNames
       Caption: RsJCLIdeThrNames),
      (Parent: ioJclPackages;            // ioJclCopyPackagesHppFiles
       Caption: RsCopyPackagesHppFiles),
      (Parent: ioJCL;                    // ioJclExcDialog
       Caption: RsJCLExceptDlg),
      (Parent: ioJclExcDialog;           // ioJclExcDialogVCL
       Caption: RsJCLDialogVCL),
      (Parent: ioJclExcDialog;           // ioJclExcDialogVCLSnd
       Caption: RsJCLDialogVCLSnd),
      (Parent: ioJclExcDialog;           // ioJclExcDialogCLX
       Caption: RsJCLDialogCLX),
      (Parent: ioJCL;                    // ioJclHelp
       Caption: RsHelpFiles),
      (Parent: ioJclHelp;                // ioJclHelpHlp
       Caption: RsIdeHelpHlp),
      (Parent: ioJclHelp;                // ioJclHelpChm
       Caption: RsIdeHelpChm)
    );

const
  {$IFDEF KYLIX}
  VersionDir = '/k%d';
  VersionDirExp = '/k%%d';
  {$ELSE}
  VersionDir = '\%s%d';
  VersionDirExp = '\%%s%%d';
  {$ENDIF}

  JclSrcDirCommon   = 'common';
  JclSrcDirVisClx   = 'visclx';

  {$IFDEF MSWINDOWS}
  {$IFNDEF RTL140_UP}
  PathSep = ';';
  {$ENDIF RTL140_UP}
  VclDialogFileName = 'ExceptDlg.pas';
  VclDlgSndFileName = 'ExceptDlgMail.pas';
  VclDialogName     = 'Exception Dialog';
  VclDialogNameSend = 'Exception Dialog with Send';

  JclIdeDebugDpk    = 'examples\vcl\debugextension\JclDebugIde%d0.dpk';
  JclIdeAnalyzerDpk = 'examples\vcl\projectanalyzer\ProjectAnalyzer%d0.dpk';
  JclIdeFavoriteDpk = 'examples\vcl\idefavopendialogs\IdeOpenDlgFavorite%d0.dpk';
  JclIdeThrNamesDpk = 'examples\vcl\debugextension\threadnames\ThreadNameExpert%d0.dpk';

  JclSrcDirOS       = 'windows';
  JclSrcDirVcl      = 'vcl';
  JclSourceDirs: array[0..3] of string = (JclSrcDirCommon, JclSrcDirOS, JclSrcDirVcl, JclSrcDirVisClx);
  JclSourcePath     = '%0:s\' + JclSrcDirOS +
                     ';%0:s\' + JclSrcDirCommon +
                     ';%0:s\' + JclSrcDirVcl +
                     ';%0:s\' + JclSrcDirVisClx;
  BCBIncludePath    = '%s;%s;$(BCB)\include;$(BCB)\include\vcl';
  BCBObjectPath     = '%s;%s;$(BCB)\Lib\Obj';
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  JclSrcDirOS       = 'unix';
  JclSourceDirs: array[0..2] of string = (JclSrcDirCommon, JclSrcDirOS, JclSrcDirVisClx);
  JclSourcePath     = '%0:s\' + JclSrcDirOS +
                     ':%0:s\' + JclSrcDirCommon +
                     ':%0:s\' + JclSrcDirVisClx;
  BCBIncludePath    = '%s:%s:$(BCB)/include:$(BCB)/include/vcl';
  BCBObjectPath     = BCBIncludePath;
  {$ENDIF UNIX}

  DialogsPath       = 'examples' + PathSeparator + 'vcl' + PathSeparator + 'debugextension'
                      + PathSeparator + 'dialog' + PathSeparator;
  ClxDialogFileName = 'ClxExceptDlg.pas';
  ClxDialogName     = 'CLX Exception Dialog';
  DialogDescription = 'JCL Application exception dialog';
  DialogAuthor      = 'Project JEDI';
  DialogPage        = 'Dialogs';

  JclChmHelpFile    = 'help' + PathSeparator + 'JCLHelp.chm';
  JclHlpHelpFile    = 'help' + PathSeparator + 'JCLHelp.hlp';
  JclHelpTitle      = 'JCL %d.%d Help';
  JclHelpIndexName  = 'Jedi Code Library Reference';
  HHFileName        = 'HH.EXE';

resourcestring
  RsStatusMessage   = 'Installing %s...';
  RsStatusDetailMessage = 'Installing %s for %s...';
  RsInstallFailed   = 'Installation of %s failed, see %s-install.log for details.';
  RsLibDescriptor   = '%s library %sunits for %s';
  {$IFDEF VisualCLX}
  RsReadmeFileName  = 'Readme.html';
  {$ELSE}
  RsReadmeFileName  = 'Readme.txt';
  {$ENDIF}
  RsIniFileName = 'JCL-install.ini';

function CreateJclInstall: IJediInstall;
begin
  Result := TJclDistribution.Create as IJediInstall;
end;

function CopyFiles(Files: TStrings; const TargetDir: string; Overwrite: Boolean = True): Boolean;
var
  I: Integer;
  FileName: string;
begin
  Result := True;
  for I := 0 to Files.Count - 1 do
  begin
    FileName := Files[I];
    Result := Result and FileCopy(FileName, PathAddSeparator(TargetDir) + ExtractFileName(FileName), Overwrite);
  end;
end;

procedure CopyResFiles(TargetDir: string);
var
  FileList: TStringList;
begin
  FileList := TStringList.Create;
  try
    if BuildFileList('*.res', faAnyFile, FileList) then
      CopyFiles(FileList, TargetDir);
  finally
    FileList.Free;
  end;
end;

function CopyHppFiles(Units: TStrings; const TargetDir: string): Boolean;
var
  I: Integer;
  FileName: string;
begin
  Result := True;
  for I := 0 to Units.Count - 1 do
  begin
    FileName := Units[I] + '.hpp';
    Result := Result and FileCopy(FileName, TargetDir + FileName, True);
  end;
end;

function FullPackageFileName(Target: TJclBorRADToolInstallation; const BaseName: string): string;
const
  S = 'packages' + VersionDir + PathSeparator + '%s%s';
var
  Prefix: string;
begin
  with Target do
  begin
    Prefix := Prefixes[RADToolKind];
    if DCC.SupportsLibSuffix then
      Result := Format(S + '%s', [{$IFNDEF KYLIX}AnsiLowerCase(Prefix), {$ENDIF}VersionNumber, Prefix, BaseName, PackageSourceFileExtension])
    else
      Result := Format(S + '%1:d0%4:s', [AnsiLowerCase(Prefix), VersionNumber, Prefix, BaseName, PackageSourceFileExtension]);
  end;
end;

function LogFileName(Target: TJclBorRADToolInstallation): string;
begin
  with Target do
    Result := Format('%s%s%d.log', [PathAddSeparator(ExtractFileDir(ParamStr(0))), RADToolName, VersionNumber]);
end;

{ TJclInstallation }

constructor TJclInstallation.Create(JclDistribution: TJclDistribution;
  InstallTarget: TJclBorRADToolInstallation);
begin
  inherited Create;
  FDistribution := JclDistribution;
  FTarget := InstallTarget;
  FDebugDcuDir := MakePath(Distribution.FLibDebugDirMask);
  FLibDir := MakePath(Distribution.FLibDirMask);
  FLibObjDir := MakePath(Distribution.FLibObjDirMask);
end;

destructor TJclInstallation.Destroy;
begin
  inherited Destroy;
end;

procedure TJclInstallation.AddDialogToRepository(const DialogName: string;
  const DialogFileName: string; const DialogIconFileName: string; const Designer: string;
  const Ancestor: string = '');
begin
  WriteLog(Format(AnsiLineBreak + 'Installing %s...',
    [DialogName]));
  with Target.Repository do
    AddObject(
      DialogFileName,
      BorRADToolRepositoryFormTemplate,
      FindPage(DialogPage, 1),
      DialogName,
      DialogIconFileName,
      DialogDescription,
      DialogAuthor,
      BorRADToolRepositoryDesignerDfm);
  WriteLog(Format('-> %s' + AnsiLineBreak + '-> %s' + AnsiLineBreak +
    '...done.', [DialogFileName, DialogIconFileName]));
end;

{$IFDEF MSWINDOWS}
procedure TJclInstallation.AddHelpToIdeTools;
var
  ToolsIndex: Integer;
  HelpTitle: string;
begin
  HelpTitle := Format(JclHelpTitle, [JclVersionMajor, JclVersionMinor]);
  with Target.IdeTools do
    if IndexOfTitle(HelpTitle) = -1 then
    begin
      ToolsIndex := Count;
      Count := ToolsIndex + 1;
      Title[ToolsIndex] := HelpTitle;
      Path[ToolsIndex] := HHFileName;
      Parameters[ToolsIndex] := StrDoubleQuote(FDistribution.FJclChmHelpFileName);
      WorkingDir[ToolsIndex] := FDistribution.FJclPath;
    end;
end;

procedure TJclInstallation.AddHelpToOpenHelp;
begin
  Target.OpenHelp.AddHelpFile(Distribution.FJclHlpHelpFileName, JclHelpIndexName);
end;
{$ENDIF MSWINDOWS}

function TJclInstallation.BplPath: string;
begin
  Result := Tool.BplPath(Target);
end;

function TJclInstallation.DcpPath: string;
begin
  Result := Tool.DcpPath(Target);
end;

procedure TJclInstallation.CleanupRepository;
begin
  if OptionSelected(ioJCL) then
    with Target.Repository do
    begin
      RemoveObjects(DialogsPath, ClxDialogFileName, BorRADToolRepositoryFormTemplate);
      {$IFDEF MSWINDOWS}
      RemoveObjects(DialogsPath, VclDialogFileName, BorRADToolRepositoryFormTemplate);
      RemoveObjects(DialogsPath, VclDlgSndFileName, BorRADToolRepositoryFormTemplate);
      {$ENDIF MSWINDOWS}
    end;
end;

function TJclInstallation.CompileLibraryUnits(const SubDir: string; Debug: Boolean): Boolean;
var
  Units: TStringList;
  UnitType: string;
  LibDescriptor: string;
  SaveDir, UnitOutputDir: string;
  Path: string;

  function CompilationOptions: string;
  begin
    if FTarget.RADToolKind = brCppBuilder then
    begin
      Result := StringsToStr(Target.DCC.Options, ' ') + ' ';
      Result := StringReplace(Result, '$(BCB)', Target.RootDir, [rfReplaceAll]);
    end
    else
      Result := '';
  end;

  function CompileUnits: Boolean;
  {$IFDEF COMPILE_SEPARATE}
  var
    I: Integer;
  begin
    Result := True;
    for I := 0 to Units.Count - 1 do
    begin
      Result := Target.DCC.Execute({$IFNDEF KYLIX}CompilationOptions + {$ENDIF}Units[I]);
      if not Result then
        Break;
    end;
  end;
  {$ELSE}
  begin
    Result := Target.DCC.Execute({$IFNDEF KYLIX}CompilationOptions + {$ENDIF}StringsToStr(Units, ' '));
  end;
  {$ENDIF}

begin
  if Debug then
    UnitType := 'debug ';
  LibDescriptor := Format(RsLibDescriptor, [SubDir, UnitType, Target.Name]);
  WriteLog(Format(AnsiLineBreak + 'Making %s', [LibDescriptor]));
  Units := TStringList.Create;
  try
    Tool.UpdateStatus(Format('Compiling %s...', [LibDescriptor]));
    Path := Format('%s' + PathSeparator + '%s', [Distribution.SourceDir, SubDir]);
    GetUnits(SubDir, Units);
    with Target.DCC do
    begin
      Options.Clear;
      Options.Add('-M');
      if Target.RADToolKind = brCppBuilder then
      begin
        Options.Add('-D_RTLDLL;NO_STRICT;USEPACKAGES'); // $(SYSDEFINES)
        if Debug then
        begin
          Options.Add('-$Y+');
          Options.Add('-$W');
          Options.Add('-$O-');
          Options.Add('-v');
          UnitOutputDir := MakePath(Distribution.FLibDebugDirMask);
          AddPathOption('N2', MakePath(Distribution.FLibDirMask + PathSeparator + 'obj')); // .obj files
        end
        else
        begin
          Options.Add('-$YD');
          Options.Add('-$W+');
          Options.Add('-$O+');
          UnitOutputDir := MakePath(Distribution.FLibDirMask);
          AddPathOption('N2', UnitOutputDir + PathSeparator + 'obj'); // .obj files
        end;
        Options.Add('-v');
        Options.Add('-JPHNE');
        Options.Add('--BCB');
        {$IFDEF KYLIX}
        //Options.Add('-LUrtl -LUvisualclx');
        {$ELSE ~KYLIX}
        if Target.VersionNumber = 5 then
          Options.Add('-LUvcl50')
        else
          Options.Add('-LUrtl');
        {$ENDIF ~KYLIX}
        AddPathOption('N0', UnitOutputDir); // .dcu files
        AddPathOption('O', Format(BCBIncludePath, [Distribution.SourceDir, Distribution.SourcePath]));
        AddPathOption('U', Format(BCBObjectPath, [Distribution.SourceDir, Distribution.SourcePath]));
      end
      else // Delphi
      begin
        if Debug then
        begin
          Options.Add('-$O-');
          Options.Add('-$W+');
          Options.Add('-$R+');
          Options.Add('-$Q+');
          Options.Add('-$D+');
          Options.Add('-$L+');
          Options.Add('-$Y+');
          UnitOutputDir := MakePath(Distribution.FLibDebugDirMask);
        end
        else
        begin
          Options.Add('-$O+');
          Options.Add('-$R-');
          Options.Add('-$Q-');
          Options.Add('-$C-');
          Options.Add('-$D-');
          UnitOutputDir := MakePath(Distribution.FLibDirMask);
        end;
        AddPathOption('N', UnitOutputDir);
        AddPathOption('U', Distribution.SourcePath);
        AddPathOption('R', Distribution.SourcePath);
      end;
      AddPathOption('I', Distribution.SourceDir);
      SaveDir := GetCurrentDir;
      Result := SetCurrentDir(Path);
      {$IFDEF WIN32}
      Win32Check(Result);
      {$ELSE}
      if Result then
      {$ENDIF}
      try
        WriteLog('');
        WriteLog('Compiling .dcu files...');
        Result := Result and CompileUnits;
        CopyResFiles(UnitOutputDir);
        if OptionSelected(ioJclCopyHppFiles) then
        begin
          Result := Result and CopyHppFiles(Units, (Target as TJclBCBInstallation).VclIncludeDir);
          WriteLog('Copying .hpp files...');
        end;
        {$IFDEF KYLIX}
        Options.Add('-P');   // generate position independent code (PIC)
        WriteLog('');
        WriteLog('Compiling dpu files...');
        Result := Result and CompileUnits;
        {$ENDIF KYLIX}
      finally
        SetCurrentDir(SaveDir);
      end;
    end;
  finally
    Units.Free;
  end;
  if not Result then
    InstallFailedOn(LibDescriptor);
end;

function TJclInstallation.Description(Option: TJediInstallOption): string;
begin
  Result := InitData[Option].Caption;
  case Option of
    ioTarget:
      Result := Target.Description;
    ioJclCopyHppFiles:
      Result := Format(Result, [(Target as TJclBCBInstallation).VclIncludeDir]);
    ioJclCopyPackagesHppFiles:
      Result := Format(Result, [(Target as TJclBCBInstallation).VclIncludeDir]);
  end;
end;

function TJclInstallation.GetProgressTotal: Integer;
var
  Option: TJediInstallOption;
begin
  Result := 0;
  for Option := ioJCL to ioJclLast do
    if OptionSelected(Option) then
      Inc(Result, ProgressWeight(Option));
end;

function TJclInstallation.GetTool: IJediInstallTool;
begin
  Result := Distribution.Tool;
end;

procedure TJclInstallation.GetUnits(const SubDir: string; Units: TStrings);
var
  I, J: Integer;
  ExcludeList: TStringList;
  ExcludeListFileName: string;
  Editions, UnitName: string;
  FileMask: string;
begin
  FileMask := Format('%s' + PathSeparator + '%s' + PathSeparator + '*.pas', [Distribution.SourceDir, SubDir]);
  BuildFileList(FileMask, faAnyFile, Units);
  // check for units not to compile
  ExcludeListFileName := MakePath(Format('%s' + PathSeparator + '%s.exc', [Distribution.FLibDirMask, SubDir]));
  if FileExists(ExcludeListFileName) then
  begin
    ExcludeList := TStringList.Create;
    try
      ExcludeList.LoadFromFile(ExcludeListFileName);
      for I := 0 to ExcludeList.Count - 1 do
      begin
        UnitName := ExcludeList[I];
        J := Pos('=', UnitName);
        if J > 0 then
          SetLength(UnitName, J - 1);
        J := Units.IndexOf(UnitName);
        if J <> -1 then
        begin
          Editions := ExcludeList.Values[UnitName];
          if (Editions = '') or (StrIPos(BorRADToolEditionIDs[Target.Edition], Editions) > 0) then
            Units.Delete(J);
        end;
      end;
    finally
      ExcludeList.Free;
    end;
  end;
  for I := 0 to Units.Count -1 do
    Units[I] := Copy(Units[I], 1, Length(Units[I]) - Length('.pas'));
end;

function TJclInstallation.InitOptions: Boolean;
var
  GUI: TObject;
  InstallationNode, ProductNode, TempNode, MakeNode: TObject;

  function AddNode(Parent: TObject; Option: TJediInstallOption;
      StandAlone: Boolean = False; Checked: Boolean = True): TObject;
  begin
    Checked := StoredOption(Option, Checked);
    Result := Tool.GUIAddOption(GUI, Parent, Option, Description(Option), StandAlone, Checked);
  end;

  procedure AddMakeNodes(Parent: TObject; DebugSettings: Boolean);
  const
    Option: array[Boolean, Boolean] of TJediInstallOption = (
      (ioJclMakeRelease, ioJclMakeReleaseVClx),
      (ioJclMakeDebug, ioJclMakeDebugVClx));
  var
    Node: TObject;
  begin
    Node := AddNode(Parent, Option[DebugSettings, False], True);
    if Target.SupportsVisualCLX then
      AddNode(Node, Option[DebugSettings, True]);
  end;

begin
  Result := Assigned(Target) and Target.Valid;
  if not Result then
    Exit;

  GUI := Tool.OptionGUI(Target);
  InstallationNode := AddNode(nil, ioTarget);
  //InstallationNode.StateIndex := 0;
  ProductNode := AddNode(InstallationNode, ioJCL);
  TempNode := AddNode(ProductNode, ioJclEnv);
  AddNode(TempNode, ioJclEnvLibPath);
  AddNode(TempNode, ioJclEnvBrowsingPath);
  AddNode(TempNode, ioJclEnvDebugDCUPath);

  MakeNode := AddNode(ProductNode, ioJclMake);
  AddMakeNodes(MakeNode, False);
  AddMakeNodes(MakeNode, True);
  if (Target is TJclBCBInstallation) then
    AddNode(MakeNode, ioJclCopyHppFiles, False, False);
  {$IFDEF MSWINDOWS}
  with Distribution do
    if (HlpHelpFileName <> '') or (ChmHelpFileName <> '') then
    begin
      TempNode := AddNode(ProductNode, ioJclHelp);
      if HlpHelpFileName <> '' then
        AddNode(TempNode, ioJclHelpHlp);
      if ChmHelpFileName <> '' then
        AddNode(TempNode, ioJclHelpChm);
    end;
  {$ENDIF MSWINDOWS}
  TempNode := AddNode(ProductNode, ioJclExcDialog);
  {$IFDEF MSWINDOWS}
  AddNode(TempNode, ioJclExcDialogVCL);
  AddNode(TempNode, ioJclExcDialogVCLSnd);
  if Target.SupportsVisualCLX then
  {$ENDIF MSWINDOWS}
    AddNode(TempNode, ioJclExcDialogCLX);
  TempNode := AddNode(ProductNode, ioJclPackages, True);
  if (Target is TJclBCBInstallation) then
    AddNode(TempNode, ioJclCopyPackagesHppFiles, False, False);
  {$IFDEF MSWINDOWS}
  if not (Target is TJclBCBInstallation) then
  begin
    { TODO -orrossmair :
It has been reported that IDE experts don't work under Win98.
Leave these options unchecked for Win9x/WinME until that has been examined. }
    TempNode := AddNode(TempNode, ioJclExperts, False, IsWinNT);
    AddNode(TempNode, ioJclExpertDebug, False, IsWinNT);
    AddNode(TempNode, ioJclExpertAnalyzer, False, IsWinNT);
    AddNode(TempNode, ioJclExpertFavorite, False, IsWinNT);
    if Target.VersionNumber <= 6 then
      AddNode(TempNode, ioJclExpertsThrNames, False, IsWinNT);
  end;
  {$ENDIF MSWINDOWS}
end;

function TJclInstallation.InstallSelectedOptions: Boolean;

  procedure WriteBorRADToolVersionToLog;
  begin
    WriteLog(StrPadRight(Format('%s Build %s ', [Target.Name, Target.IdeExeBuildNumber]), 64, '='));
  end;

var
  Option: TJediInstallOption;
begin
  Result := True;
  Tool.UpdateStatus(Format(RsStatusMessage, [Target.Name]));
  WriteBorRADToolVersionToLog;
  CleanupRepository;
  for Option := ioJCL to ioJclLast do
    if OptionSelected(Option) then
      Result := Result and InstallOption(Option);
end;

function TJclInstallation.InstallOption(Option: TJediInstallOption): Boolean;
{$IFDEF MSWINDOWS}
const
  ExpertPaths: array[ioJclExpertDebug..ioJclExpertsThrNames] of string =
    (
      JclIdeDebugDpk,
      JclIdeAnalyzerDpk,
      JclIdeFavoriteDpk,
      JclIdeThrNamesDpk
    );
{$ENDIF MSWINDOWS}
begin
  Result := True;
  case Option of
    ioJclEnvLibPath:
      begin
        Target.AddToLibrarySearchPath(LibDir);
        Target.AddToLibrarySearchPath(Distribution.SourceDir);
      end;
    ioJclEnvBrowsingPath:
      Target.AddToLibraryBrowsingPath(Distribution.SourcePath);
    ioJclEnvDebugDCUPath:
      Target.AddToDebugDCUPath(DebugDcuDir);
    // ioJclMake:
    ioJclMakeRelease:
      Result := MakeUnits(False);
    // ioJclMakeReleaseVClx: handled with ioJclMakeRelease
    ioJclMakeDebug:
      Result := MakeUnits(True);
    // ioJclMakeDebugVClx: handled with ioJclMakeDebug
    // ioJclCopyHppFiles: handled by InstallPackage
    ioJclPackages:
      begin
        Result := InstallRunTimePackage('Jcl');
        if Target.SupportsVisualCLX then
          Result := Result and InstallRunTimePackage('JclVClx');
        if Target.VersionNumber >= 6 then
          Result := Result and InstallRunTimePackage('JclVcl');
      end;
    {$IFDEF MSWINDOWS}
    // ioJclExperts:
    ioJclExpertDebug..ioJclExpertsThrNames:
      Result := InstallPackage(ExpertPaths[Option]);
    // ioJclCopyPackagesHppFiles: handled by InstallPackage
    // ioJclExcDialog:
    ioJclExcDialogVCL:
      with Distribution do
        AddDialogToRepository(VclDialogName, FVclDialogFileName, FVclDialogIconFileName,
          BorRADToolRepositoryDesignerDfm);
    ioJclExcDialogVCLSnd:
      with Distribution do
        AddDialogToRepository(VclDialogNameSend, FVclDialogSendFileName,
          FVclDialogSendIconFileName, BorRADToolRepositoryDesignerDfm, FVclDialogFileName);
    {$ENDIF MSWINDOWS}
    ioJclExcDialogCLX:
      with Distribution do
        AddDialogToRepository(ClxDialogName, FClxDialogFileName, FClxDialogIconFileName,
          BorRADToolRepositoryDesignerXfm);
    {$IFDEF MSWINDOWS}
    // ioJclHelp:
    ioJclHelpHlp:
      AddHelpToOpenHelp;
    ioJclHelpChm:
      AddHelpToIdeTools;
    {$ENDIF MSWINDOWS}
  end;
  Progress(ProgressWeight(Option));
end;

procedure TJclInstallation.InstallationStarted;
begin
  with FDistribution do
    if Assigned(FOnStarting) then
      FOnStarting(Target);
end;

procedure TJclInstallation.InstallationFinished;
begin
  with FDistribution do
    if Assigned(FOnEnding) then
      FOnEnding(Target);
end;

procedure TJclInstallation.InstallFailedOn(const InstallObj: string);
begin
  Tool.Dialog(Format(RsInstallFailed, [InstallObj, LogFileName]), dtError);
end;

function TJclInstallation.InstallPackage(const Name: string): Boolean;
const
  {$IFDEF MSWINDOWS}
  Bcb2MakTemplate = '\BCB.bmk';
  {$ENDIF MSWINDOWS}
  {$IFDEF KYLIX}
  Bcb2MakTemplate = '/bcb.gmk';
  {$ENDIF KYLIX}
var
  PackageFileName: string;
begin
  Result := True;
  PackageFileName := Distribution.Path + Format(Name, [Target.VersionNumber]);
  WriteLog(Format(AnsiLineBreak + 'Installing package %s...', [PackageFileName]));
  Tool.UpdateStatus(Format(RsStatusDetailMessage, [ExtractFileName(PackageFileName), Target.Name]));
  if IsDelphiPackage(Name) then
  begin
    Result := Target.InstallPackage(PackageFileName, BplPath,
      DcpPath);
  end
  else
    if Target is TJclBCBInstallation then
    with TJclBCBInstallation(Target) do
    begin
      Bpr2Mak.Options.Clear;
      Bpr2Mak.Options.Add('-t..' + Bcb2MakTemplate);
      {$IFDEF KYLIX}
      SetEnvironmentVar('OBJDIR', LibObjDir);
      SetEnvironmentVar('BPILIBDIR', DcpPath);
      SetEnvironmentVar('BPLDIR', BplPath);
      {$ELSE}
      Make.Options.Clear;
      Make.AddPathOption('DBPILIBDIR=', DcpPath);
      Make.AddPathOption('DBPLDIR=', BplPath);
      if OptionSelected(ioJclCopyPackagesHppFiles) then
        Make.AddPathOption('DHPPDIR=', (Target as TJclBCBInstallation).VclIncludeDir);
      {$ENDIF}
      Result := Target.InstallPackage(PackageFileName, BplPath,
        DcpPath);
    end;
  WriteLog('...done.');
  if not Result then
    InstallFailedOn(PackageFileName);
end;

function TJclInstallation.InstallRunTimePackage(const BaseName: string): Boolean;
begin
  Result := InstallPackage(FullPackageFileName(Target, BaseName));
end;

function TJclInstallation.LogFileName: string;
begin
  Result := JclInstall.LogFileName(Target);
end;

function TJclInstallation.MakePath(const FormatStr: string): string;
begin
  {$IFDEF KYLIX}
  Result := Format(FormatStr, [Target.VersionNumber]);
  {$ELSE ~KYLIX}
  with Target do
    Result := Format(FormatStr, [Prefixes[RADToolKind], VersionNumber]);
  {$ENDIF ~KYLIX}
end;

function TJclInstallation.MakeUnits(Debug: Boolean): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := Low(JclSourceDirs) to High(JclSourceDirs) do
  begin
    {$IFDEF MSWINDOWS}
    if (JclSourceDirs[I] = 'visclx') and not OptionSelected(ioJclMakeReleaseVClx) then
      Continue;
    {$ENDIF MSWINDOWS}
    Result := Result and CompileLibraryUnits(JclSourceDirs[I], Debug);
  end;
end;

function TJclInstallation.OptionSelected(Option: TJediInstallOption): Boolean;
begin
  Result := Tool.FeatureChecked(Ord(Option), Target);
end;

procedure TJclInstallation.Progress(Steps: Integer);
begin
  Distribution.InstallProgress(Steps);
end;

function TJclInstallation.ProgressWeight(Option: TJediInstallOption): Integer;
begin
  case Option of
    ioJclEnvLibPath,
    ioJclEnvBrowsingPath,
    ioJclEnvDebugDCUPath:
      Result := 1;
    ioJclMakeRelease,
    ioJclMakeReleaseVClx,
    ioJclMakeDebug,
    ioJclMakeDebugVClx:
      Result := 10;
    ioJclCopyHppFiles:
      Result := 2;
    ioJclPackages:
      Result := 10;
    ioJclExpertDebug,
    ioJclExpertAnalyzer,
    ioJclExpertFavorite,
    ioJclExpertsThrNames:
      Result := 5;
    ioJclCopyPackagesHppFiles:
      Result := 2;
    ioJclExcDialog,
    ioJclExcDialogVCL,
    ioJclExcDialogVCLSnd,
    ioJclExcDialogCLX,
    ioJclHelpHlp,
    ioJclHelpChm:
      Result := 1;
  else
    Result := 0;
  end;
end;

function TJclInstallation.Run: Boolean;
begin
  Result := True;
  if OptionSelected(ioJCL) then
  begin
    InstallationStarted;
    Result := InstallSelectedOptions;
    if Result then
      InstallationFinished;
  end;
  SaveOptions;
end;

procedure TJclInstallation.SaveOption(Option: TJediInstallOption; Value: Boolean);
begin
  Distribution.FIniFile.WriteBool(Target.Name, OptionToStr(Option), Value);
end;

procedure TJclInstallation.SaveOptions;
var
  Option: TJediInstallOption;
begin
  SaveOption(ioTarget, OptionSelected(ioTarget));
  for Option := ioJCL to ioJclLast do
    SaveOption(Option, OptionSelected(Option));
end;

function TJclInstallation.StoredOption(Option: TJediInstallOption; Default: Boolean = True): Boolean;
begin
  Result := Distribution.FIniFile.ReadBool(Target.Name, OptionToStr(Option), Default);
end;

procedure TJclInstallation.WriteLog(const Msg: string);
begin
  Tool.WriteInstallLog(Target, Msg);
end;

{ TJclDistribution }

constructor TJclDistribution.Create;
begin
  inherited;
  FTargetInstalls := TObjectList.Create;
  FTargetInstalls.OwnsObjects := True;
  FIniFile := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + RsIniFileName);
end;

destructor TJclDistribution.Destroy;
begin
  FTargetInstalls.Free;
  if Assigned(FIniFile) then
  begin
    FIniFile.UpdateFile;
    FreeAndNil(FIniFile);
  end;
  inherited;
end;

function TJclDistribution.CreateInstall(Target: TJclBorRADToolInstallation): Boolean;
var
  Inst: TJclInstallation;
begin
  if Supports(Target) then
  begin
    Inst := TJclInstallation.Create(Self, Target);
    FTargetInstalls.Add(Inst);
    Inst.InitOptions;
  end;
  Result := True;
end;

function TJclDistribution.DocFileName(const BaseFileName: string): string;
const
  SDocFileMask = '%sdocs' + PathSeparator + '%s';
begin
  Result := Format(SDocFileMask, [FJclPath, BaseFileName]);
end;

function TJclDistribution.FeatureInfoFileName(FeatureID: Cardinal): string;
begin
  Result := DocFileName(Format('%.7x.info', [FeatureID]));
end;

procedure TJclDistribution.InitInstallationTargets;
begin
  Tool.GetBorRADToolInstallations.Iterate(CreateInstall);
end;

function TJclDistribution.InitInformation(const ApplicationFileName: string): Boolean;
var
  I: Integer;
begin
  FJclPath := PathAddSeparator(ExpandFileName(PathExtractFileDirFixed(ApplicationFileName) + '..'));
  FLibDirMask := Format('%slib' + VersionDirExp, [FJclPath]);
  FLibDebugDirMask := FLibDirMask + PathSeparator + 'debug';
  FLibObjDirMask := FLibDirMask + PathSeparator + 'obj';
  FJclSourceDir := FJclPath + 'source';

  FJclSourcePath := '';
  for I := Low(JclSourceDirs) to High(JclSourceDirs) do
    FJclSourcePath := FJclSourcePath +
      Format('%s' + PathSeparator + '%s' + PathSep, [FJclSourceDir, JclSourceDirs[I]]);

  {$IFDEF MSWINDOWS}
  FClxDialogFileName := AnsiUpperCase(FJclPath + DialogsPath + ClxDialogFileName);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  FClxDialogFileName := FJclPath + DialogsPath + ClxDialogFileName;
  {$ENDIF UNIX}
  FClxDialogIconFileName := ChangeFileExt(FClxDialogFileName, '.ico');
  {$IFDEF MSWINDOWS}
  FVclDialogFileName := AnsiUpperCase(FJclPath + DialogsPath + VclDialogFileName);
  FVclDialogSendFileName := AnsiUpperCase(FJclPath + DialogsPath + VclDlgSndFileName);
  FVclDialogIconFileName := ChangeFileExt(FVclDialogFileName, '.ico');
  FVclDialogSendIconFileName := ChangeFileExt(FVclDialogSendFileName, '.ico');
  {$ENDIF MSWINDOWS}
  FJclChmHelpFileName := FJclPath + JclChmHelpFile;
  FJclHlpHelpFileName := FJclPath + JclHlpHelpFile;
  if not FileExists(FJclChmHelpFileName) then
    FJclChmHelpFileName := '';
  if not FileExists(FJclHlpHelpFileName) then
    FJclHlpHelpFileName := '';
  {$IFDEF MSWINDOWS}
  // Reset ReadOnly flag for dialog forms
  FileSetAttr(FClxDialogFileName, faArchive);
  FileSetAttr(ChangeFileExt(FClxDialogFileName, '.xfm'), faArchive);
  FileSetAttr(FVclDialogFileName, faArchive);
  FileSetAttr(ChangeFileExt(FVclDialogFileName, '.dfm'), faArchive);
  FileSetAttr(FVclDialogSendFileName, faArchive);
  FileSetAttr(ChangeFileExt(FVclDialogSendFileName, '.dfm'), faArchive);
  {$ENDIF MSWINDOWS}
  Result := FileExists(FClxDialogFileName) and FileExists(FClxDialogIconFileName)
  {$IFDEF MSWINDOWS}
    and FileExists(FVclDialogFileName)  and FileExists(FVclDialogIconFileName)
  {$ENDIF MSWINDOWS};
  FJclReadmeFileName := DocFileName(RsReadmeFileName);
  if FileExists(FJclReadmeFileName) then
    Tool.Readme := FJclReadmeFileName;
end;

procedure TJclDistribution.InitProgress;
var
  I: Integer;
begin
  FProgressTotal := 0;
  for I := 0 to FTargetInstalls.Count - 1 do
    Inc(FProgressTotal, TJclInstallation(FTargetInstalls[I]).ProgressTotal);
end;

function TJclDistribution.Install: Boolean;
var
  I: Integer;
begin
  Result := True;
  try
    InitProgress;
    for I := 0 to FTargetInstalls.Count - 1 do
      Result := Result and TJclInstallation(FTargetInstalls[I]).Run;
  finally
    Tool.UpdateStatus('');
  end;
end;

procedure TJclDistribution.InstallProgress(Steps: Integer);
begin
  if Steps > 0 then
  begin
    Inc(FProgress, Steps);
    ShowProgress;
  end;
end;

function TJclDistribution.ReadmeFileName: string;
begin
  Result := FJclReadmeFileName;
end;

procedure TJclDistribution.SetOnEnding(Value: TInstallationEvent);
begin
  FOnEnding := Value;
end;

procedure TJclDistribution.SetOnProgress(Value: TInstallationProgressEvent);
begin
  FOnProgress := Value;
end;

procedure TJclDistribution.SetOnStarting(Value: TInstallationEvent);
begin
  FOnStarting := Value;
end;

procedure TJclDistribution.SetTool(const Value: IJediInstallTool);
begin
  FTool := Value;
  InitInformation(ParamStr(0));
  InitInstallationTargets;
end;

procedure TJclDistribution.ShowProgress;
var
  Percent: Integer;
begin
  if (FProgressTotal > 0) and Assigned(FOnProgress) then
  begin
    Percent := (FProgress * 100) div FProgressTotal;
    if Percent <> FProgressPercent then
    begin
      FProgressPercent := Percent;
      FOnProgress(Percent);
    end;
  end;
end;

function TJclDistribution.Supports(Target: TJclBorRADToolInstallation): Boolean;
begin
  {$IFDEF KYLIX}
  Result := Target.VersionNumber = 3;
  {$ELSE ~KYLIX}
  if Target.RADToolKind = brCppBuilder then
    Result := Target.VersionNumber in [5..6]
  else
    Result := Target.VersionNumber in [5..7];
  {$ENDIF ~KYLIX}
end;

// History:

// $Log$
// Revision 1.37  2004/11/10 05:18:11  rrossmair
// - fixed for Kylix
//
// Revision 1.36  2004/11/09 07:51:37  rrossmair
// - installer refactoring (incomplete)
//

end.
