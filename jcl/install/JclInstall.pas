
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

unit JclInstall;

interface

{$I jcl.inc}
{$I crossplatform.inc}

uses
  SysUtils, Classes, IniFiles, Contnrs,
  JclSysUtils, JclBorlandTools, JediInstall;

type
  TJclDistribution = class;

  TJclInstallation = class
  private
    FDistribution: TJclDistribution;
    FTarget: TJclBorRADToolInstallation;
    FDebugDcuDir: string;
    FLibDir: string;
    FLibObjDir: string;
    FDefines: TStringList;
    FUnits: TStringList;
    FDemos: TStringList;
    FDemoExclusionList: TStringList;
    FOnWriteLog: TTextHandler;
    FRelativeDemoPath: string;
    FDemoSectionName: string;
    procedure AddDemo(const Directory: string; const FileInfo: TSearchRec);
    procedure AddDemos(const Directory: string);
    procedure AddDialogToRepository(const DialogName: string; const DialogFileName: string;
      const DialogIconFileName: string; const Designer: string; const Ancestor: string = '');
    function GetDemoList: TStrings;
    procedure BuildUnitList(const SubDir: string; Units: TStrings);
    function GetDemoExclusionList: TStrings;
    function GetProgressTotal: Integer;
    function GetTool: IJediInstallTool;
    function GetUnits(const SourceDir: string): TStrings;
    function InitOptions: Boolean;
    procedure InstallationStarted;
    procedure InstallationFinished;
    {$IFDEF MSWINDOWS}
    function InstallExpert(const BaseName: string): Boolean;
    function UninstallExpert(const FileName: string): Boolean;
    {$ENDIF MSWINDOWS}
    procedure InstallFailedOn(const InstallObj: string);
    function InstallPackageSourceFile(const Name: string): Boolean;
    function InstallRunTimePackage(const BaseName: string): Boolean;
    function InstallOption(Option: TJediInstallOption): Boolean;
    procedure RemoveDialogFromRepository(const DialogName, DialogFileName: string);
    function UninstallPackage(const Name: string): Boolean;
    function UninstallRunTimePackage(const BaseName: string): Boolean;
    function UninstallOption(Option: TJediInstallOption): Boolean;
    function LogFileName: string;
    procedure MakeDemo(Index: Integer);
    function MakeDemos: Boolean; overload;
    function MakeUnits(Debug: Boolean): Boolean;
    function MakePath(const FormatStr: string): string;
    function Description(Option: TJediInstallOption): string;
    procedure SaveDemoOption(Index: Integer);
    procedure SaveOption(Option: TJediInstallOption);
    procedure SaveOptions;
    procedure Progress(Steps: Integer);
    function StoredOption(Option: TJediInstallOption; Default: Boolean = True): Boolean;
    function TotalUnitCount: Integer;
    procedure WriteLog(const Msg: string);
  protected
    constructor Create(JclDistribution: TJclDistribution; InstallTarget: TJclBorRADToolInstallation);
    function CompileLibraryUnits(const SubDir: string; Debug: Boolean): Boolean;
    {$IFDEF MSWINDOWS}
    procedure AddHelpToIdeTools;
    procedure AddHelpToOpenHelp;
    procedure RemoveHelpFromIdeTools;
    procedure RemoveHelpFromOpenHelp;
    procedure CopyFakeXmlRtlPackage;
    {$ENDIF MSWINDOWS}
    function BplPath: string;
    function DcpPath: string;
    function CheckDirectories: Boolean;
    procedure CleanupRepository;
    function DemoOption(DemoIndex: Integer): TJediInstallOption;
    function DemoOptionSelected(Index: Integer): Boolean;
    function ExcludeEdition(ExcludeList: TStrings; Index: Integer; out Name: string): Boolean;
    function InstallSelectedOptions: Boolean;
    function UninstallSelectedOptions: Boolean;
    function OptionSelected(Option: TJediInstallOption): Boolean;
    function ProgressWeight(Option: TJediInstallOption): Integer;
    function Run: Boolean;
    function Undo: Boolean;
    function StoredBplPath: string;
    function StoredDcpPath: string;
    property Defines: TStringList read FDefines;
    property Demos: TStrings read GetDemoList;
    property DemoSectionName: string read FDemoSectionName;
    property Distribution: TJclDistribution read FDistribution;
    property DemoExclusionList: TStrings read GetDemoExclusionList;
    property Tool: IJediInstallTool read GetTool;
    property DebugDcuDir: string read FDebugDcuDir;
    property LibDir: string read FLibDir;
    property LibObjDir: string read FLibObjDir;
    property ProgressTotal: Integer read GetProgressTotal;
    property RelativeDemoPath: string read FRelativeDemoPath;
    property Target: TJclBorRADToolInstallation read FTarget;
    property Units[const SourceDir: string]: TStrings read GetUnits;
  public
    destructor Destroy; override;
    property OnWriteLog: TTextHandler read FOnWriteLog write FOnWriteLog;
  end;

  TJclDistribution = class (TInterfacedObject, IJediInstall)
  private
    FJclPath: string;
    FJclBinDir: string;
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
    FInstalling: Boolean;
    function CreateInstall(Target: TJclBorRADToolInstallation): Boolean;
    function GetTargetInstall(Installation: TJclBorRADToolInstallation): TJclInstallation;
    procedure InitInstallationTargets;
    procedure InitProgress;
    function GetExamplesDir: string;
    function GetDemosPath: string;
  protected
    constructor Create;
    function DocFileName(const BaseFileName: string): string;
    procedure InstallProgress(Steps: Integer);
    procedure SetTool(const Value: IJediInstallTool);
    procedure ShowProgress;
    property TargetInstall[Target: TJclBorRADToolInstallation]: TJclInstallation read GetTargetInstall;
  public
    destructor Destroy; override;
    function FeatureInfoFileName(FeatureID: Cardinal): string;
    function GetHint(Option: TJediInstallOption): string;
    function InitInformation(const ApplicationFileName: string): Boolean;
    function Install: Boolean;
    function Uninstall: Boolean;
    function ReadmeFileName: string;
    procedure SetOnWriteLog(Installation: TJclBorRADToolInstallation; Value: TTextHandler);
    procedure SetOnEnding(Value: TInstallationEvent);
    procedure SetOnProgress(Value: TInstallationProgressEvent);
    procedure SetOnStarting(Value: TInstallationEvent);
    function Supports(Target: TJclBorRADToolInstallation): Boolean;
    property BinDir: string read FJclBinDir;
    property ExamplesDir: string read GetExamplesDir;
    property DemosPath: string read GetDemosPath;
    property ChmHelpFileName: string read FJclChmHelpFileName;
    property HlpHelpFileName: string read FJclHlpHelpFileName;
    property Installing: Boolean read FInstalling;
    property Path: string read FJclPath;
    property SourceDir: string read FJclSourceDir;
    property SourcePath: string read FJclSourcePath;
    property Tool: IJediInstallTool read FTool write SetTool;
  end;

function CreateJclInstall: IJediInstall;
function LogFileName(Target: TJclBorRADToolInstallation): string;

implementation

uses
  JclBase, JclResources, JclSysInfo, JclFileUtils, JclStrings;

{ Install option data }

resourcestring
// Captions

  // Products
  RsJCL                  = 'JEDI Code Library';

  // Common features
  RsDefThreadSafe        = 'Thread safe container classes';
  RsDefDropObsoleteCode  = 'Drop obsolete code';
  RsDefMathPrecSingle    = 'Single float precision';
  RsDefMathPrecDouble    = 'Double float precision';
  RsDefMathPrecExtended  = 'Extended float precision';
  RsEnvironment          = 'Environment';
  RsEnvLibPath           = 'Add JCL to IDE Library Path';
  RsEnvBrowsingPath      = 'Add JCL to IDE Browsing Path';
  RsEnvDebugDCUPath      = 'Add JCL to Debug DCU Path';
  RsMake                 = 'Make library units';
  RsMakeRelease          = 'Release';
  RsMakeDebug            = 'Debug';
  RsMakeVClx             = 'Visual CLX';
  RsMakeDemos            = 'Make demos';

  RsHelpFiles            = 'Help files';
  RsIdeExperts           = 'IDE experts';
  RsJCLPackages          = 'Packages';
  RsIdeHelpHlp           = 'Add help file to IDE help system';
  RsIdeHelpChm           = 'Add HTML help to the Tools menu';
  RsCopyHppFiles         = 'Copy HPP files to %s';
  RsCopyPackagesHppFiles = 'Output HPP files to %s';

  // Product specific features
  RsJCLExceptDlg         = 'Sample Exception Dialogs in the Object Reporitory';
  RsJCLDialogVCL         = 'VCL Exception Dialog';
  RsJCLDialogVCLSnd      = 'VCL Exception Dialog with Send button';
  RsJCLDialogCLX         = 'CLX Exception Dialog';
  RsJCLIdeDebug          = 'Debug Extension';
  RsJCLIdeAnalyzer       = 'Project Analyzer';
  RsJCLIdeFavorite       = 'Favorite combobox in Open/Save dialogs';
  RsJCLIdeThreadNames    = 'Displaying thread names in Thread Status window';
  RsJCLIdeUses           = 'Uses Wizard';
  RsJCLSimdView          = 'Debug window for XMM registers';

// Hints
  RsHintTarget = 'Installation target';
  RsHintJCL = 'Select to install JCL for this target.';
  RsHintJclDefThreadSafe        = 'Conditionally compile container classes to be thread safe';
  RsHintJclDefDropObsoleteCode  = 'Do not compile deprecated code';
  RsHintJclDefMathPrecSingle    = 'type Float = Single';
  RsHintJclDefMathPrecDouble    = 'type Float = Double';
  RsHintJclDefMathPrecExtended  = 'type Float = Extended';
  RsHintJclEnv = 'Set selected environment items';
  RsHintJclEnvLibPath = 'Add JCL precompiled unit directories to library path';
  RsHintJclEnvBrowsingPath = 'Add JCL source directories to browsing path';
  RsHintJclEnvDebugDCUPath = 'This is a prerequisite for using the precompiled JCL debug units ' +
    'by means of the respective'#13#10'Project Options|Compiler switch. See "Make library ' +
    'units/Debug" option below.';
  RsHintJclMake = 'Generate .dcu and .dpu (Kylix only) files.'#13#10'Recommended.';
  RsHintJclMakeRelease = 'Make precompiled units for release, i.e. optimized, w/o debug information.';
  RsHintJclMakeReleaseVcl = 'Make precompiled VCL units for release';
  RsHintJclMakeReleaseVClx = 'Make precompiled Visual CLX units for release';
  RsHintJclMakeDebug = 'Make precompiled units for debugging, i.e.optimization off, debug ' +
    'information included.'#13#10'When installed, available through Project Options|Compiler|Use ' +
    'Debug DCUs.';
  RsHintJclMakeDebugVcl = 'Make precompiled VCL units for debugging';
  RsHintJclMakeDebugVClx = 'Make precompiled Visual CLX units for debugging';
  RsHintJclCopyHppFiles = 'Copy .hhp files into C++Builder''s include path.';
  RsHintJclPackages = 'Build and eventually install JCL runtime packages (RTL, VCL and Visual ' +
    'CLX) and optional IDE experts.';
  RsHintJclExperts = 'Build and install selected IDE experts.';
  RsHintJclExpertDebug = 'Install IDE expert which assists to insert JCL Debug information into ' +
    'executable files.';
  RsHintJclExpertAnalyzer = 'Install IDE Project Analyzer.';
  RsHintJclExpertFavorite = 'Install "Favorites" combobox in IDE Open/Save dialogs.';
  RsHintJclExpertsThreadNames = 'Display thread names in Thread Status window IDE extension.';
  RsHintJclExpertUses = 'Install IDE Uses Wizard.';
  RsHintJclExpertSimdView = 'Install a debug window of XMM registers (used by SSE instructions)';
  RsHintJclCopyPackagesHppFiles = 'Output .hhp files into C++Builder''s include path instead of ' +
    'the source paths.';
  RsHintJclExcDialog = 'Add selected Exception dialogs to the Object Repository.';
  RsHintJclExcDialogVCL = 'Add VCL exception dialog to the Object Repository.';
  RsHintJclExcDialogVCLSnd = 'Add VCL exception dialog with "Send Button" to the Object Repository.';
  RsHintJclExcDialogCLX = 'Add CLX exception dialog (Windows only) to the Object Repository.';
  RsHintJclHelp = 'Install JCL help files.';
  RsHintJclHelpHlp = 'Customize Borland Open Help to include JCL help files.';
  RsHintJclHelpChm = '';
  RsHintJclMakeDemos = 'Make JCL demo applications';

const
  Invalid = -1;
  LineBreak = AnsiLineBreak;
  ioUndef = TJediInstallOption(Invalid);

  InitData: array[TJediInstallOption] of TInstallOptionData =
    (
      (Parent: ioUndef;                  // ioTarget
       Caption: '';
       Hint: RsHintTarget),
      (Parent: ioTarget;                 // ioJCL
       Caption: RsJCL;
       Hint: RsHintJcl),
      (Parent: ioJCL;                   // ioJclDefThreadSafe
       Caption: RsDefThreadSafe;
       Hint: RsHintJclDefThreadSafe),
      (Parent: ioJCL;                   // ioJclDefDropObsoleteCode
       Caption: RsDefDropObsoleteCode;
       Hint: RsHintJclDefDropObsoleteCode),
      (Parent: ioJCL;                   // ioJclDefMathPrecSingle
       Caption: RsDefMathPrecSingle;
       Hint: RsHintJclDefMathPrecSingle),
      (Parent: ioJCL;                   // ioJclDefMathPrecDouble
       Caption: RsDefMathPrecDouble;
       Hint: RsHintJclDefMathPrecDouble),
      (Parent: ioJCL;                   // ioJclDefMathPrecExtended
       Caption: RsDefMathPrecExtended;
       Hint: RsHintJclDefMathPrecExtended),
      (Parent: ioJCL;                    // ioJclEnv
       Caption: RsEnvironment;
       Hint: RsHintJclEnv),
      (Parent: ioJclEnv;                 // ioJclEnvLibPath
       Caption: RsEnvLibPath;
       Hint: RsHintJclEnvLibPath),
      (Parent: ioJclEnv;                 // ioJclEnvBrowsingPath
       Caption: RsEnvBrowsingPath;
       Hint: RsHintJclEnvBrowsingPath),
      (Parent: ioJclEnv;                 // ioJclEnvDebugDCUPath
       Caption: RsEnvDebugDCUPath;
       Hint: RsHintJclEnvDebugDCUPath),
      (Parent: ioJCL;                    // ioJclMake
       Caption: RsMake;
       Hint: RsHintJclMake),
      (Parent: ioJclMake;                // ioJclMakeRelease
       Caption: RsMakeRelease;
       Hint: RsHintJclMakeRelease),
      (Parent: ioJclMake;                // ioJclMakeReleaseVClx
       Caption: RsMakeVClx;
       Hint: RsHintJclMakeReleaseVClx),
      (Parent: ioJclMake;                // ioJclMakeDebug
       Caption: RsMakeDebug;
       Hint: RsHintJclMakeDebug),
      (Parent: ioJclMake;                // ioJclMakeDebugVClx
       Caption: RsMakeVClx;
       Hint: RsHintJclMakeDebugVClx),
      (Parent: ioJclMake;                // ioJclCopyHppFiles
       Caption: RsCopyHppFiles;
       Hint: RsHintJclCopyHppFiles),
      (Parent: ioJCL;                    // ioJclPackages
       Caption: RsJCLPackages;
       Hint: RsHintJclPackages),
      (Parent: ioJclPackages;            // ioJclExperts
       Caption: RsIdeExperts;
       Hint: RsHintJclExperts),
      (Parent: ioJclExperts;             // ioJclExpertDebug
       Caption: RsJCLIdeDebug;
       Hint: RsHintJclExpertDebug),
      (Parent: ioJclExperts;             // ioJclExpertAnalyzer
       Caption: RsJCLIdeAnalyzer;
       Hint: RsHintJclExpertAnalyzer),
      (Parent: ioJclExperts;             // ioJclExpertFavorite
       Caption: RsJCLIdeFavorite;
       Hint: RsHintJclExpertFavorite),
      (Parent: ioJclExperts;             // ioJclExpertThreadNames
       Caption: RsJCLIdeThreadNames;
       Hint: RsHintJclExpertsThreadNames),
      (Parent: ioJclExperts;             // ioJclExpertUses
       Caption: RsJCLIdeUses;
       Hint: RsHintJclExpertUses),
      (Parent: ioJclExperts;             // ioJclExpertSimdView
       Caption: RsJCLSimdView;
       Hint: RsHintJclExpertSimdView),
      (Parent: ioJclPackages;            // ioJclCopyPackagesHppFiles
       Caption: RsCopyPackagesHppFiles;
       Hint: RsHintJclCopyPackagesHppFiles),
      (Parent: ioJCL;                    // ioJclExcDialog
       Caption: RsJCLExceptDlg;
       Hint: RsHintJclExcDialog),
      (Parent: ioJclExcDialog;           // ioJclExcDialogVCL
       Caption: RsJCLDialogVCL;
       Hint: RsHintJclExcDialogVCL),
      (Parent: ioJclExcDialog;           // ioJclExcDialogVCLSnd
       Caption: RsJCLDialogVCLSnd;
       Hint: RsHintJclExcDialogVCLSnd),
      (Parent: ioJclExcDialog;           // ioJclExcDialogCLX
       Caption: RsJCLDialogCLX;
       Hint: RsHintJclExcDialogCLX),
      (Parent: ioJCL;                    // ioJclHelp
       Caption: RsHelpFiles;
       Hint: RsHintJclHelp),
      (Parent: ioJclHelp;                // ioJclHelpHlp
       Caption: RsIdeHelpHlp;
       Hint: RsHintJclHelpHlp),
      (Parent: ioJclHelp;                // ioJclHelpChm
       Caption: RsIdeHelpChm;
       Hint: RsHintJclHelpChm),
      (Parent: ioJCL;                    // ioJclMakeDemos
       Caption: RsMakeDemos;
       Hint: RsHintJclMakeDemos)
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

  JclIdeBaseDpk     = 'JclBaseExpert';
  JclIdeDebugDpk    = 'JclDebugExpert';
  JclIdeAnalyzerDpk = 'JclProjectAnalysisExpert';
  JclIdeFavoriteDpk = 'JclFavoriteFoldersExpert';
  JclIdeThrNamesDpk = 'JclThreadNameExpert';
  JclIdeUsesDpk     = 'JclUsesExpert';
  JclIdeSimdViewDpk = 'JclSIMDViewExpert';

  ExpertPaths: array[ioJclExperts..ioJclExpertSimdView] of string =
    (
      JclIdeBaseDpk,
      JclIdeDebugDpk,
      JclIdeAnalyzerDpk,
      JclIdeFavoriteDpk,
      JclIdeThrNamesDpk,
      JclIdeUsesDpk,
      JclIdeSimdViewDpk
    );

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

  DialogsPath       = 'experts' + PathSeparator + 'debug' + PathSeparator + 'dialog' + PathSeparator;
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
  RsStatusMessage                   = 'Installing %s...';
  RsStatusDetailMessage             = 'Installing %s for %s...';
  RsUninstallMessage                = 'Removing %s...';
  RsBuildingMessage                 = 'Building %s...';
  RsBuildingDemosMessage            = 'Building demo projects...';
  RsBuildingDemosByTargetMessage    = 'Building demo projects by %s...';
  RsCompilingMessage                = 'Compiling %s...';
  RsInstallFailed                   = 'Installation of %s failed, see %s for details.';
  RsInvalidBplPath                  = LineBreak + 'Invalid BPL path "%s"';
  RsInvalidDcpPath                  = LineBreak + 'Invalid DCP path "%s"';
  RsLibDescriptor                   = '%s library %sunits for %s';
  {$IFDEF VisualCLX}
  RsReadmeFileName                  = 'Readme.html';
  {$ELSE}
  RsReadmeFileName                  = 'Readme.txt';
  {$ENDIF}
  RsIniFileName                     = 'JCL-install.ini';

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
  S = 'packages' + VersionDir + PathSeparator + '%s';
var
  Prefix: string;
begin
  with Target do
  begin
    Prefix := Prefixes[RADToolKind];
    if DCC.SupportsLibSuffix then
      Result := Format(S + '%s', [{$IFNDEF KYLIX}AnsiLowerCase(Prefix), {$ENDIF}VersionNumber, BaseName, PackageSourceFileExtension])
    else
      Result := Format(S + '%s%1:d0%4:s', [AnsiLowerCase(Prefix), VersionNumber, BaseName, Prefix, PackageSourceFileExtension]);
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
  InstallTarget.DCC.OutputCallback := WriteLog;
  InstallTarget.Make.OutputCallback := WriteLog;
  FDebugDcuDir := MakePath(Distribution.FLibDebugDirMask);
  FLibDir := MakePath(Distribution.FLibDirMask);
  if InstallTarget is TJclBCBInstallation then
  begin
    FLibObjDir := MakePath(Distribution.FLibObjDirMask);
    TJclBCBInstallation(InstallTarget).Bpr2Mak.OutputCallback := WriteLog;
  end;
  FDefines := TStringList.Create;
  FUnits := TStringList.Create;
  FDemoSectionName := Target.Name + ' demos';
end;

destructor TJclInstallation.Destroy;
var
  I: Integer;
begin
  FDemoExclusionList.Free;
  FDemos.Free;
  for I := 0 to FUnits.Count - 1 do
    FUnits.Objects[I].Free;
  FUnits.Free;
  FDefines.Free;
  inherited Destroy;
end;

procedure TJclInstallation.AddDemo(const Directory: string; const FileInfo: TSearchRec);
var
  FileName: string;
begin
  FileName := FRelativeDemoPath + FileInfo.Name;
  if not DemoExclusionList.IndexOf(FileName) >= 0 then
    Demos.Append(FileName);
end;

procedure TJclInstallation.AddDemos(const Directory: string);
begin
  FRelativeDemoPath := PathAddSeparator(PathGetRelativePath(Distribution.DemosPath, Directory));
  EnumFiles(Directory + '*.dpr', AddDemo);
end;

procedure TJclInstallation.AddDialogToRepository(const DialogName: string;
  const DialogFileName: string; const DialogIconFileName: string; const Designer: string;
  const Ancestor: string = '');
begin
  WriteLog(Format(LineBreak + 'Installing %s...', [DialogName]));
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
  WriteLog(Format('-> %s' + LineBreak + '-> %s' + LineBreak +
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
    if IndexOfTitle(HelpTitle) = Invalid then
    begin
      ToolsIndex := Count;
      Count := ToolsIndex + 1;
      Title[ToolsIndex] := HelpTitle;
      Path[ToolsIndex] := HHFileName;
      Parameters[ToolsIndex] := StrDoubleQuote(FDistribution.FJclChmHelpFileName);
      WorkingDir[ToolsIndex] := Distribution.Path;
    end;
end;

procedure TJclInstallation.AddHelpToOpenHelp;
begin
  if Target.OpenHelp.AddHelpFile(Distribution.FJclHlpHelpFileName, JclHelpIndexName) then
    WriteLog(Format(LineBreak + 'Added %s to %s Online Help', [Distribution.FJclHlpHelpFileName, Target.RADToolName]));
end;
{$ENDIF MSWINDOWS}

function TJclInstallation.BplPath: string;
begin
  Result := Tool.BPLPath[Target];
  {$IFDEF MSWINDOWS}
  Result := PathGetShortName(Result);
  {$ENDIF MSWINDOWS}
end;

function TJclInstallation.DcpPath: string;
begin
  Result := Tool.DCPPath[Target];
  {$IFDEF MSWINDOWS}
  Result := PathGetShortName(Result);
  {$ENDIF MSWINDOWS}
end;

function TJclInstallation.CheckDirectories: Boolean;
begin
  Result := not OptionSelected(ioJclPackages);
  if not Result then
  begin
    Result := True;
    if not DirectoryExists(BplPath) then
    begin
      WriteLog(Format(RsInvalidBplPath, [BplPath]));
      Result := False;
    end;
    if not DirectoryExists(DcpPath) then
    begin
      WriteLog(Format(RsInvalidDcpPath, [DcpPath]));
      Result := False;
    end;
  end;
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
  UnitList: TStrings;
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
  {$IFDEF COMPILE_UNITS_SEPARATELY // gives better progress resolution }
  var
    I: Integer;
  begin
    Result := True;
    for I := 0 to UnitList.Count - 1 do
    begin
      Result := Target.DCC.Execute({$IFNDEF KYLIX}CompilationOptions + {$ENDIF}UnitList[I]);
      Progress(1);
      if not Result then
        Break;
    end;
  end;
  {$ELSE}
  begin
    Result := Target.DCC.Execute({$IFNDEF KYLIX}CompilationOptions + {$ENDIF}StringsToStr(UnitList, ' '));
    Progress(UnitList.Count);
  end;
  {$ENDIF}

begin
  if Debug then
    UnitType := 'debug ';
  LibDescriptor := Format(RsLibDescriptor, [SubDir, UnitType, Target.Name]);
  WriteLog(Format(LineBreak + 'Making %s', [LibDescriptor]));
  Tool.UpdateStatus(Format(RsCompilingMessage, [LibDescriptor]));
  Path := Format('%s' + PathSeparator + '%s', [Distribution.SourceDir, SubDir]);
  UnitList := Units[SubDir];
  with Target.DCC do
  begin
    SetDefaultOptions;
    Options.Add('-D' + StringsToStr(Defines, ';'));
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
      if (Target is TJclBCBInstallation) and OptionSelected(ioJclCopyHppFiles) then
      begin
        Result := Result and CopyHppFiles(UnitList, Target.RootDir + RsVclIncludeDir);
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
  if not Result then
    InstallFailedOn(LibDescriptor);
end;

function TJclInstallation.Description(Option: TJediInstallOption): string;
var
  BCBInstallation: TJclBCBInstallation;
begin
  Result := InitData[Option].Caption;

  BCBInstallation := nil;
  if Target is TJclBCBInstallation then
    BCBInstallation := Target as TJclBCBInstallation;

  case Option of
    ioTarget:
      Result := Target.Description;
    ioJclCopyHppFiles:
      Result := Format(Result, [BCBInstallation.VclIncludeDir]);
    ioJclCopyPackagesHppFiles:
      Result := Format(Result, [BCBInstallation.VclIncludeDir]);
  end;
end;

procedure TJclInstallation.CopyFakeXmlRtlPackage;
// replace missing xmlrtl.dcp in Delphi 2005 Personal by dummy package to allow expert installation
begin
  { TODO : implement copying of fake xmlrtl.dcp to $(BDS)\Lib }
end;

function TJclInstallation.ExcludeEdition(ExcludeList: TStrings; Index: Integer; out Name: string): Boolean;
var
  Editions: string;
begin
  Name := ExcludeList[Index];
  if Pos('=', Name) > 0 then
    Name := ExcludeList.Names[Index];
  Editions := ExcludeList.Values[Name];
  Result := (Editions = '') or (StrIPos(BorRADToolEditionIDs[Target.Edition], Editions) > 0);
end;

function TJclDistribution.GetExamplesDir: string;
begin
  Result := Path + 'examples';
end;

function TJclInstallation.GetDemoList: TStrings;
begin
  if not Assigned(FDemos) then
  begin
    FDemos := TStringList.Create;
    EnumDirectories(Distribution.ExamplesDir, AddDemos);
    //Demos.Sorted := True;
  end;
  Result := FDemos;
end;

function TJclDistribution.GetDemosPath: string;
begin
  Result := PathAddSeparator(Path + 'examples');
end;

function TJclDistribution.GetHint(Option: TJediInstallOption): string;
begin
  if Ord(Option) < Ord(ioJclLast) then
    Result := InitData[Option].Hint;
end;

function TJclInstallation.GetProgressTotal: Integer;
var
  Option: TJediInstallOption;
begin
  Result := 0;
  if OptionSelected(ioJCL) then
    for Option := Succ(ioJCL) to ioJclLast do
      if OptionSelected(Option) then
        Inc(Result, ProgressWeight(Option));
end;

function TJclInstallation.GetTool: IJediInstallTool;
begin
  Result := Distribution.Tool;
end;

procedure TJclInstallation.BuildUnitList(const SubDir: string; Units: TStrings);
var
  I, J: Integer;
  ExcludeList: TStringList;
  ExcludeListFileName: string;
  UnitName: string;
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
        if ExcludeEdition(ExcludeList, I, UnitName) then
        begin
          J := Units.IndexOf(UnitName);
          if J <> Invalid then
            Units.Delete(J);
        end;
    finally
      ExcludeList.Free;
    end;
  end;
  for I := 0 to Units.Count -1 do
    Units[I] := Copy(Units[I], 1, Length(Units[I]) - Length('.pas'));
end;

function TJclInstallation.DemoOption(DemoIndex: Integer): TJediInstallOption;
begin
  Result := TJediInstallOption(Ord(ioJclLast) + 1 + DemoIndex);
end;

function TJclInstallation.DemoOptionSelected(Index: Integer): Boolean;
begin
  Result := OptionSelected(DemoOption(Index));
end;

function TJclInstallation.GetDemoExclusionList: TStrings;
var
  I: Integer;
  Strings: TStrings;
  FileName: string;
begin
  if not Assigned(FDemoExclusionList) then
  begin
    FDemoExclusionList := TStringList.Create;
    FileName := MakePath(Distribution.DemosPath + '%s%d.exc');
    if FileExists(FileName) then
    begin
      FDemoExclusionList.LoadFromFile(FileName);
      Strings := TStringList.Create;
      try
        I := 0;
        while I < FDemoExclusionList.Count do
        begin
          if ExcludeEdition(FDemoExclusionList, I, FileName) then
            if ExtractFileExt(FileName) = '.exc' then
            begin
              Strings.LoadFromFile(Distribution.DemosPath + FileName);
              FDemoExclusionList.AddStrings(Strings);
              FDemoExclusionList.Delete(I);
            end
            else
            begin
              FDemoExclusionList[I] := FileName;
              Inc(I);
            end
          else
            FDemoExclusionList.Delete(I);
        end;
      finally
        Strings.Free;
      end;
    end;
  end;
  Result := FDemoExclusionList;
end;

function TJclInstallation.GetUnits(const SourceDir: string): TStrings;
var
  I: Integer;
begin
  I := FUnits.IndexOf(SourceDir);
  if I = Invalid then
  begin
    Result := TStringList.Create;
    try
      BuildUnitList(SourceDir, Result);
    except
      Result.Free;
    end;
    FUnits.AddObject(SourceDir, Result);
  end
  else
    Result := FUnits.Objects[I] as TStrings;
end;

function TJclInstallation.InitOptions: Boolean;
var
  GUI: TObject;
  {$IFDEF MSWINDOWS}
  ExpertOptions: TJediInstallGUIOptions;
  {$ENDIF MSWINDOWS}
  InstallationNode, ProductNode, TempNode, MakeNode: TObject;

  function AddNode(Parent: TObject; Option: TJediInstallOption;
    GUIOptions: TJediInstallGUIOptions = [goChecked]): TObject;
  begin
    if StoredOption(Option, goChecked in GUIOptions) then
      Include(GUIOptions, goChecked)
    else
      Exclude(GUIOptions, goChecked);
    Result := Tool.GUIAddOption(GUI, Parent, Option, Description(Option), GUIOptions);
  end;

  function AddDemoNode(Parent: TObject; Index: Integer;
    GUIOptions: TJediInstallGUIOptions = []): TObject;
  var
    Checked: Boolean;
  begin
    Checked := Distribution.FIniFile.ReadInteger(DemoSectionName, Demos[Index], 0) > 0;
    if Checked then
      Include(GUIOptions, goChecked)
    else
      Exclude(GUIOptions, goChecked);
    Result := Tool.GUIAddOption(GUI, Parent, DemoOption(Index),
      ExtractFileName(Demos[Index]), GUIOptions);
  end;

  procedure AddDemoNodes;
  var
    I: Integer;
  begin
    TempNode := AddNode(ProductNode, ioJclMakeDemos, [goExpandable, goNoAutoCheck]);
    for I := 0 to Demos.Count - 1 do
      AddDemoNode(TempNode, I);
  end;

  procedure AddMakeNodes(Parent: TObject; DebugSettings: Boolean);
  const
    Option: array[Boolean, Boolean] of TJediInstallOption = (
      (ioJclMakeRelease, ioJclMakeReleaseVClx),
      (ioJclMakeDebug, ioJclMakeDebugVClx));
  var
    Node: TObject;
  begin
    Node := AddNode(Parent, Option[DebugSettings, False], [goStandAloneParent, goChecked]);
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

  AddNode(ProductNode, ioJclDefThreadSafe);
  AddNode(ProductNode, ioJclDefDropObsoleteCode);
  AddNode(ProductNode, ioJclDefMathPrecSingle, [goRadioButton]);
  AddNode(ProductNode, ioJclDefMathPrecDouble, [goRadioButton]);
  AddNode(ProductNode, ioJclDefMathPrecExtended, [goRadioButton, goChecked]);

  TempNode := AddNode(ProductNode, ioJclEnv);
  AddNode(TempNode, ioJclEnvLibPath);
  AddNode(TempNode, ioJclEnvBrowsingPath);
  AddNode(TempNode, ioJclEnvDebugDCUPath);

  MakeNode := AddNode(ProductNode, ioJclMake, [goExpandable, goChecked]);
  AddMakeNodes(MakeNode, False);
  AddMakeNodes(MakeNode, True);
  if (Target is TJclBCBInstallation) then
    AddNode(MakeNode, ioJclCopyHppFiles);
  {$IFDEF MSWINDOWS}
  { TODO : Help integration for Delphi 2005 }
  if Target.VersionNumber <= 7 then
  with Distribution do
    if (HlpHelpFileName <> '') or (ChmHelpFileName <> '') then
    begin
      TempNode := AddNode(ProductNode, ioJclHelp);
      if HlpHelpFileName <> '' then
        AddNode(TempNode, ioJclHelpHlp);
      if ChmHelpFileName <> '' then
        AddNode(TempNode, ioJclHelpChm);
    end;
  { TODO : Object Repository access for D 2005 }
  if Target.VersionNumber <= 7 then
  {$ENDIF MSWINDOWS}
  begin
    TempNode := AddNode(ProductNode, ioJclExcDialog);
    {$IFDEF MSWINDOWS}
    AddNode(TempNode, ioJclExcDialogVCL);
    AddNode(TempNode, ioJclExcDialogVCLSnd);
    if Target.SupportsVisualCLX then
    {$ENDIF MSWINDOWS}
      AddNode(TempNode, ioJclExcDialogCLX);
  end;
  TempNode := AddNode(ProductNode, ioJclPackages, [goStandAloneParent, goChecked]);
  if (Target is TJclBCBInstallation) then
    AddNode(TempNode, ioJclCopyPackagesHppFiles);
  {$IFDEF MSWINDOWS}
  if (Target.VersionNumber = 9) and (Target.Edition = deStd) then
    CopyFakeXmlRtlPackage;
    { TODO :
      It has been reported that IDE experts don't work under Win98.
      Leave these options unchecked for Win9x/WinME until that has been examined. }
    if IsWinNT then
      ExpertOptions := [goChecked]
    else
      ExpertOptions := [];
    TempNode := AddNode(TempNode, ioJclExperts, [goExpandable, goChecked]);
    AddNode(TempNode, ioJclExpertDebug, ExpertOptions);
    AddNode(TempNode, ioJclExpertAnalyzer, ExpertOptions);
    AddNode(TempNode, ioJclExpertFavorite, ExpertOptions);
    if Target.VersionNumber <= 6 then
      AddNode(TempNode, ioJclExpertThreadNames, ExpertOptions);
    AddNode(TempNode, ioJclExpertUses, ExpertOptions);
    AddNode(TempNode, ioJclExpertSimdView, ExpertOptions);
  {$ENDIF MSWINDOWS}
  AddDemoNodes;
  Tool.BPLPath[Target] := StoredBplPath;
  Tool.DCPPath[Target] := StoredDcpPath;
end;

function TJclInstallation.InstallSelectedOptions: Boolean;

  function BorRADToolVersionStr: string;
  begin
    Result := Format('%s Build %s ', [Target.Name, Target.IdeExeBuildNumber]);
  end;

var
  Option: TJediInstallOption;
begin
  Tool.UpdateStatus(Format(RsStatusMessage, [Target.Name]));
  WriteLog(StrPadRight(BorRADToolVersionStr, 44, '='));
  Result := CheckDirectories;
  if Result then
  begin
    CleanupRepository;
    Defines.Clear;
    for Option := ioJCL to ioJclLast do
      if OptionSelected(Option) then
        Result := Result and InstallOption(Option);
  end;
  WriteLog('');
end;

function TJclInstallation.InstallOption(Option: TJediInstallOption): Boolean;
begin
  Result := True;
  case Option of
    ioJclDefThreadSafe:
      Defines.Add('THREAD_SAFE');
    ioJclDefDropObsoleteCode:
      Defines.Add('DROP_OBSOLETE_CODE');
    ioJclDefMathPrecSingle:
      Defines.Add('MATH_SINGLE_PRECISION');
    ioJclDefMathPrecDouble:
      Defines.Add('MATH_DOUBLE_PRECISION');
    ioJclDefMathPrecExtended:
      Defines.Add('MATH_EXTENDED_PRECISION');
    ioJclEnvLibPath:
      if Target.AddToLibrarySearchPath(LibDir) and Target.AddToLibrarySearchPath(Distribution.SourceDir) then
        WriteLog(Format(LineBreak + 'Added "%s;%s" to library path.', [LibDir, Distribution.SourceDir]));
    ioJclEnvBrowsingPath:
      if Target.AddToLibraryBrowsingPath(Distribution.SourcePath) then
        WriteLog(Format(LineBreak + 'Added "%s" to library browsing path.', [Distribution.SourcePath]));
    ioJclEnvDebugDCUPath:
      if Target.AddToDebugDCUPath(DebugDcuDir) then
        WriteLog(Format(LineBreak + 'Added "%s" to Debug DCU Path.', [DebugDcuDir]));
    // ioJclMake:
    ioJclMakeRelease:
      Result := MakeUnits(False);
    // ioJclMakeReleaseVClx: handled with ioJclMakeRelease
    ioJclMakeDebug:
      Result := MakeUnits(True);
    // ioJclMakeDebugVClx: handled with ioJclMakeDebug
    // ioJclCopyHppFiles: handled by InstallPackageSourceFile
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
    ioJclExperts..ioJclExpertSimdView:
      Result := InstallExpert(ExpertPaths[Option]);
    // ioJclCopyPackagesHppFiles: handled by InstallPackageSourceFile
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
    ioJclMakeDemos:
      MakeDemos;
    {$ENDIF MSWINDOWS}
  end;
  if not (Option in [ioJclMakeRelease, ioJclMakeDebug]) then
    Progress(ProgressWeight(Option));
end;

function TJclInstallation.UninstallOption(Option: TJediInstallOption): Boolean;
begin
  Result := True;
  case Option of
    ioJclEnvLibPath:
      if Target.RemoveFromLibrarySearchPath(LibDir) and Target.RemoveFromLibrarySearchPath(Distribution.SourceDir) then
        WriteLog(Format(LineBreak + 'Removed "%s;%s" from library path.', [LibDir, Distribution.SourceDir]));
    ioJclEnvBrowsingPath:
      if Target.RemoveFromLibraryBrowsingPath(Distribution.SourcePath) then
        WriteLog(Format(LineBreak + 'Removed "%s" from library browsing path.', [Distribution.SourcePath]));
    ioJclEnvDebugDCUPath:
      if Target.RemoveFromDebugDCUPath(DebugDcuDir) then
        WriteLog(Format(LineBreak + 'Removed "%s" from Debug DCU Path.', [DebugDcuDir]));
    // ioJclMake:
    ioJclMakeRelease: { TODO :  Delete generated files };
    ioJclMakeDebug: { TODO : Delete generated files  };
    ioJclCopyHppFiles: { TODO : Delete copied files };
    ioJclPackages:
      begin
        Result := UninstallRunTimePackage('Jcl');
        if Target.SupportsVisualCLX then
          Result := Result and UninstallRunTimePackage('JclVClx');
        if Target.VersionNumber >= 6 then
          Result := Result and UninstallRunTimePackage('JclVcl');
      end;
    {$IFDEF MSWINDOWS}
    ioJclExperts..ioJclExpertSimdView:
      Result := UninstallExpert(ExpertPaths[Option]);
    // ioJclCopyPackagesHppFiles: 
    // ioJclExcDialog:
    ioJclExcDialogVCL:
      with Distribution do
        RemoveDialogFromRepository(VclDialogName, VclDialogFileName);
    ioJclExcDialogVCLSnd:
      with Distribution do
        RemoveDialogFromRepository(VclDialogNameSend, VclDlgSndFileName);
    {$ENDIF MSWINDOWS}
    ioJclExcDialogCLX:
      with Distribution do
        RemoveDialogFromRepository(ClxDialogName, ClxDialogFileName);
    {$IFDEF MSWINDOWS}
    // ioJclHelp:
    ioJclHelpHlp:
      RemoveHelpFromOpenHelp;
    ioJclHelpChm:
      RemoveHelpFromIdeTools;
    ioJclMakeDemos:
      ;
    {$ENDIF MSWINDOWS}
  end;
  if not Distribution.Installing then
    if not (Option in [ioJclMakeRelease, ioJclMakeDebug]) then
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

{$IFDEF MSWINDOWS}
function TJclInstallation.InstallExpert(const BaseName: string): Boolean;
begin
  Result := InstallPackageSourceFile(FullPackageFileName(Target, BaseName));
end;
{$ENDIF MSWINDOWS}

procedure TJclInstallation.InstallFailedOn(const InstallObj: string);
begin
  Tool.Dialog(Format(RsInstallFailed, [InstallObj, LogFileName]), dtError);
end;

function TJclInstallation.InstallPackageSourceFile(const Name: string): Boolean;
const
  {$IFDEF MSWINDOWS}
  Bcb2MakTemplate = 'packages\BCB.bmk';
  {$ENDIF MSWINDOWS}
  {$IFDEF KYLIX}
  Bcb2MakTemplate = 'packages/bcb.gmk';
  {$ENDIF KYLIX}
var
  PackageFileName: string;
  PackageDirectory: string;
{$IFNDEF KYLIX}
  DpkPackageFileName: string;
{$ENDIF}
begin
  Result := True;
  PackageFileName := Distribution.Path + Format(Name, [Target.VersionNumber]);
  WriteLog(Format(LineBreak + 'Installing package %s...', [PackageFileName]));
  Tool.UpdateStatus(Format(RsStatusDetailMessage, [ExtractFileName(PackageFileName), Target.Name]));
  if IsDelphiPackage(Name) then
  begin
    Result := Target.InstallPackage(PackageFileName, BplPath, DcpPath);
  end
  else
  if Target is TJclBCBInstallation then
    with TJclBCBInstallation(Target) do
    begin
      PackageDirectory := PathAddSeparator(ExtractFileDir(PackageFileName));
      // now create .bpi & .lib
      Bpr2Mak.Options.Clear;
      Bpr2Mak.Options.Add('-t' + ExtractRelativePath(PackageDirectory,Distribution.Path + Bcb2MakTemplate));
      {$IFDEF KYLIX}
      SetEnvironmentVar('OBJDIR', LibObjDir);
      SetEnvironmentVar('BPILIBDIR', DcpPath);
      SetEnvironmentVar('BPLDIR', BplPath);
      {$ELSE}
      // to satisfy JVCL (and eventually other libraries), create a .dcp file;
      // Note: it is put out to .bpl path to make life easier for JVCL
      DpkPackageFileName := ChangeFileExt(PackageFileName, '.dpk');
      if FileExists(DpkPackageFileName) then
        Result := Target.InstallPackage(DpkPackageFileName, BplPath, BplPath);
      Make.Options.Clear;
      Make.AddPathOption('DBPILIBDIR=', DcpPath);
      Make.AddPathOption('DBPLDIR=', BplPath);
      if OptionSelected(ioJclCopyPackagesHppFiles) then
        Make.AddPathOption('DHPPDIR=', Target.RootDir + RsVclIncludeDir);
      {$ENDIF}
      Result := Result and Target.InstallPackage(PackageFileName, BplPath, DcpPath);
    end;
  WriteLog('...done.');
  if not Result then
    InstallFailedOn(PackageFileName);
end;

function TJclInstallation.InstallRunTimePackage(const BaseName: string): Boolean;
begin
  Result := InstallPackageSourceFile(FullPackageFileName(Target, BaseName));
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
  Result := PathGetShortName(Format(FormatStr, [Prefixes[Target.RADToolKind], Target.VersionNumber]));
  {$ENDIF ~KYLIX}
end;

procedure TJclInstallation.MakeDemo(Index: Integer);
var
  FileName: string;
  CfgFileName: string;
  Directory: string;
begin
  FileName := Demos[Index];
  Directory := Distribution.DemosPath + ExtractFileDir(FileName);
  FileName := ExtractFileName(FileName);
  WriteLog(Format(LineBreak + RsBuildingMessage, [FileName]));
  SetCurrentDir(Directory);
  CfgFileName := ChangeFileExt(FileName, '.cfg');
  StringToFile(CfgFileName, Format(
    '-e%s' + AnsiLineBreak +    // Exe output dir
    '-u%s' + AnsiLineBreak +    // Unit directories
    '-i%s',                     // Include path
    [Distribution.BinDir, LibDir, Distribution.SourceDir]));
  Target.DCC.Execute(FileName);
  FileDelete(CfgFileName);
end;

function TJclInstallation.MakeDemos: Boolean;
var
  I: Integer;
  SaveDir: string;
begin
  Tool.UpdateStatus(Format(RsBuildingDemosByTargetMessage, [Target.Name]));
  WriteLog(LineBreak + RsBuildingDemosMessage + LineBreak);
  Result := True;
  SaveDir := GetCurrentDir;
  for I := 0 to Demos.Count - 1 do
    if DemoOptionSelected(I) then
      MakeDemo(I);
  SetCurrentDir(SaveDir);
end;

function TJclInstallation.MakeUnits(Debug: Boolean): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := Low(JclSourceDirs) to High(JclSourceDirs) do
  begin
    {$IFDEF MSWINDOWS}
    if (JclSourceDirs[I] = 'visclx') and
      not (OptionSelected(ioJclMakeReleaseVClx) or OptionSelected(ioJclMakeDebugVClx)) then
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
    ioJclMakeDebug:
      begin
        Result := TotalUnitCount;
        {$IFDEF KYLIX}
        Result := Result * 2; // .dcu + .dpu
        {$ENDIF KYLIX}
      end;
    ioJclMakeReleaseVClx,
    ioJclMakeDebugVClx:
      Result := 0;
    ioJclCopyHppFiles:
      Result := 2;
    ioJclPackages:
      Result := 10;
    ioJclExpertDebug,
    ioJclExpertAnalyzer,
    ioJclExpertFavorite,
    ioJclExpertThreadNames,
    ioJclExpertUses,
    ioJclExpertSimdView:
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
    ioJclMakeDemos:
      Result := 50;
  else
    Result := 0;
  end;
end;

procedure TJclInstallation.RemoveDialogFromRepository(const DialogName, DialogFileName: string);
begin
  Target.Repository.RemoveObjects(DialogsPath, DialogFileName, BorRADToolRepositoryFormTemplate);
  WriteLog(Format(LineBreak + 'Removed %s.', [DialogName]));
end;

{$IFDEF MSWINDOWS}
procedure TJclInstallation.RemoveHelpFromIdeTools;
begin
  { TODO : Implement }
end;

procedure TJclInstallation.RemoveHelpFromOpenHelp;
begin
  if Target.OpenHelp.RemoveHelpFile(Distribution.FJclHlpHelpFileName, JclHelpIndexName) then
    WriteLog(Format(LineBreak + 'Removed %s from %s Online Help', [Distribution.FJclHlpHelpFileName, Target.RADToolName]));
end;
{$ENDIF MSWINDOWS}

function TJclInstallation.Run: Boolean;
begin
  Result := True;
  if OptionSelected(ioJCL) then
  begin
    InstallationStarted;
    try
      Result := InstallSelectedOptions;
    finally
      InstallationFinished;
    end;
  end;
  SaveOptions;
end;

function TJclInstallation.Undo: Boolean;
begin
  Result := True;
  if OptionSelected(ioJCL) then
    Result := UninstallSelectedOptions;
  SaveOptions;
end;

function TJclInstallation.UninstallPackage(const Name: string): Boolean;
var
  PackageFileName: string;
begin
  PackageFileName := Distribution.Path + Format(Name, [Target.VersionNumber]);
  Result := Target.UninstallPackage(PackageFileName, StoredBPLPath, StoredDCPPath);
  { TODO : evtl. remove .HPP Files }
  if Result then
    WriteLog(Format(LineBreak + 'Removed package %s.', [PackageFileName]));
end;

{$IFDEF MSWINDOWS}
function TJclInstallation.UninstallExpert(const FileName: string): Boolean;

  function OldExpertBPLFileName(Target: TJclBorRADToolInstallation; const BaseName: string): string;
  const
    OldExperts: array[ioJclExpertDebug..ioJclExpertSimdView] of string = (
      'JclDebugIde%s%d0.bpl',
      'ProjectAnalyzer%s%d0.bpl',
      'IdeOpenDlgFavorite%s%d0.bpl',
      'ThreadNameExpert%s%d0.bpl',
      'JediUses%s%d0.bpl',
      'JclSIMDView%s%d.bpl');

  var
    Prefix: string;
    I: TJediInstallOption;
  begin
    with Target do
    begin
      Prefix := Prefixes[RADToolKind];
      for I := Low(OldExperts) to High(OldExperts) do
        if BaseName = ExpertPaths[I] then
        begin
          Result := PathAddSeparator(StoredBPLPath) + Format(OldExperts[I], [Prefix, VersionNumber]);
          Break;
        end;
    end;
  end;

var
  BaseName: string;
  BPLFileName: string;
begin
  BaseName := ExtractFileName(FileName);
  BPLFileName := Format(BaseName, ['', '.bpl']);
  Target.IdePackages.RemovePackage(PathAddSeparator(StoredBPLPath) + Format(BPLFileName, [Target.VersionNumber]));
  Result := UninstallPackage(FullPackageFileName(Target, FileName));
  // eventually remove old expert packages to avoid annoying package conflicts during IDE startup;
  // for simplicity, .dcp files are not handled
  BPLFileName := OldExpertBPLFileName(Target, BaseName);
  FileDelete(BPLFileName);
  Target.IdePackages.RemovePackage(BPLFileName);
end;
{$ENDIF MSWINDOWS}

function TJclInstallation.UninstallRunTimePackage(const BaseName: string): Boolean;
begin
  Result := UninstallPackage(FullPackageFileName(Target, BaseName));
end;

function TJclInstallation.UninstallSelectedOptions: Boolean;

  function BorRADToolVersionStr: string;
  begin
    Result := Format('%s Build %s ', [Target.Name, Target.IdeExeBuildNumber]);
  end;

var
  Option: TJediInstallOption;
  Success: Boolean;
begin
  Result := True;
  Tool.UpdateStatus(Format(RsUninstallMessage, [Target.Name]));
  WriteLog(StrPadRight('Starting Uninstall process', 44, '.'));
  for Option := ioJCL to ioJclLast do
    if OptionSelected(Option) then
    begin
      // Don't stop uninstall process when one step fails
      Success := UninstallOption(Option);
      Result := Result and Success;
    end;
  WriteLog('');
end;

procedure TJclInstallation.SaveDemoOption(Index: Integer);
var
  Value: Integer;
begin
  Value := Invalid;
  if OptionSelected(DemoOption(Index)) then
    Value := JclBase.JclVersionBuild;
  Distribution.FIniFile.WriteInteger(DemoSectionName, Demos[Index], Value);
end;

procedure TJclInstallation.SaveOption(Option: TJediInstallOption);
var
  Value: Integer;
begin
  Value := Invalid;
  if OptionSelected(Option) then
    Value := JclBase.JclVersionBuild;
  Distribution.FIniFile.WriteInteger(Target.Name, OptionToStr(Option), Value);
end;

procedure TJclInstallation.SaveOptions;
var
  I: Integer;
  Option: TJediInstallOption;
begin
  SaveOption(ioTarget);
  for Option := ioJCL to ioJclLast do
    SaveOption(Option);
  for I := 0 to Demos.Count - 1 do
    SaveDemoOption(I);
  Distribution.FIniFile.WriteString(Target.Name, 'BPL-Path', Tool.BPLPath[Target]);
  Distribution.FIniFile.WriteString(Target.Name, 'DCP-Path', Tool.DCPPath[Target]);
end;

function TJclInstallation.StoredBplPath: string;
begin
  Result := Distribution.FIniFile.ReadString(Target.Name, 'BPL-Path', Target.BPLOutputPath);
end;

function TJclInstallation.StoredDcpPath: string;
begin
  Result := Distribution.FIniFile.ReadString(Target.Name, 'DCP-Path', Target.DCPOutputPath);
end;

function TJclInstallation.StoredOption(Option: TJediInstallOption; Default: Boolean = True): Boolean;
begin
  case Distribution.FIniFile.ReadInteger(Target.Name, OptionToStr(Option), 0) of
    Invalid:
      Result := False;
    0:
      Result := Default;
  else
    Result := True;
  end;
end;

function TJclInstallation.TotalUnitCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(JclSourceDirs) to High(JclSourceDirs) do
  begin
    {$IFDEF MSWINDOWS}
    if (JclSourceDirs[I] = 'visclx') and
      not (OptionSelected(ioJclMakeReleaseVClx) or OptionSelected(ioJclMakeDebugVClx)) then
      Continue;
    {$ENDIF MSWINDOWS}
    with Units[JclSourceDirs[I]] do
      Inc(Result, Count);
  end;
end;

procedure TJclInstallation.WriteLog(const Msg: string);
begin
  if Assigned(FOnWriteLog) then
    FOnWriteLog(Msg);
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

function TJclDistribution.GetTargetInstall(Installation: TJclBorRADToolInstallation): TJclInstallation;
var
  I: Integer;
begin
  for I := 0 to FTargetInstalls.Count - 1 do
  begin
    Result := TJclInstallation(FTargetInstalls[I]);
    if Result.Target = Installation then
      Exit;
  end;
  Result := nil;
end;

procedure TJclDistribution.InitInstallationTargets;
begin
  if not Tool.GetBorRADToolInstallations.Iterate(CreateInstall) then
    raise EJediInstallInitFailure.CreateRes(@RsNoInstall);
end;

function TJclDistribution.InitInformation(const ApplicationFileName: string): Boolean;
var
  I: Integer;
  ExceptDialogsPath: string;
begin
  FJclPath := PathAddSeparator(ExpandFileName(PathExtractFileDirFixed(ApplicationFileName) + '..'));
  {$IFDEF MSWINDOWS}
  FJclPath := PathGetShortName(FJclPath);
  {$ENDIF MSWINDOWS}
  FLibDirMask := Format('%slib' + VersionDirExp, [FJclPath]);
  FLibDebugDirMask := FLibDirMask + PathSeparator + 'debug';
  FLibObjDirMask := FLibDirMask + PathSeparator + 'obj';
  FJclBinDir := FJclPath + 'bin';
  FJclSourceDir := FJclPath + 'source';

  FJclSourcePath := '';
  for I := Low(JclSourceDirs) to High(JclSourceDirs) do
    FJclSourcePath := FJclSourcePath +
      Format('%s' + PathSeparator + '%s' + PathSep, [FJclSourceDir, JclSourceDirs[I]]);

  {$IFDEF MSWINDOWS}
  ExceptDialogsPath := PathGetShortName(FJclPath + DialogsPath);
  FClxDialogFileName := AnsiUpperCase(ExceptDialogsPath + ClxDialogFileName);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  ExceptDialogsPath := FJclPath + DialogsPath;
  FClxDialogFileName := ExceptDialogsPath + ClxDialogFileName;
  {$ENDIF UNIX}
  FClxDialogIconFileName := ChangeFileExt(FClxDialogFileName, '.ico');
  {$IFDEF MSWINDOWS}
  FVclDialogFileName := AnsiUpperCase(ExceptDialogsPath + VclDialogFileName);
  FVclDialogSendFileName := AnsiUpperCase(ExceptDialogsPath + VclDlgSndFileName);
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
  Result := FileExists(FClxDialogFileName) and FileExists(FClxDialogIconFileName)
    and FileExists(FVclDialogFileName)  and FileExists(FVclDialogIconFileName)
  {$ELSE ~MSWINDOWS}
  Result := True;
  {$ENDIF ~MSWINDOWS};
  FJclReadmeFileName := DocFileName(RsReadmeFileName);
  if FileExists(FJclReadmeFileName) then
    Tool.Readme := FJclReadmeFileName;
  if not Result then
    raise EJediInstallInitFailure.CreateRes(@RsCantFindFiles);
end;

procedure TJclDistribution.InitProgress;
var
  I: Integer;
begin
  FProgress := 0;
  FProgressTotal := 0;
  for I := 0 to FTargetInstalls.Count - 1 do
    Inc(FProgressTotal, TJclInstallation(FTargetInstalls[I]).ProgressTotal);
end;

function TJclDistribution.Install: Boolean;
var
  I: Integer;
begin
  FInstalling := True; // tell UninstallOption not to call Progress()
  Result := True;
  try
    InitProgress;
    for I := 0 to FTargetInstalls.Count - 1 do
    begin
      TJclInstallation(FTargetInstalls[I]).Undo;
      Result := Result and TJclInstallation(FTargetInstalls[I]).Run;
    end;
  finally
    Tool.UpdateStatus('');
    FInstalling := False;
  end;
end;

function TJclDistribution.Uninstall: Boolean;
var
  I: Integer;
  Success: Boolean;
begin
  Result := True;
  try
    InitProgress;
    for I := 0 to FTargetInstalls.Count - 1 do
    begin
      Success := TJclInstallation(FTargetInstalls[I]).Undo;
      Result := Result and Success;
    end;
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

procedure TJclDistribution.SetOnWriteLog(Installation: TJclBorRADToolInstallation; Value: TTextHandler);
begin
  TargetInstall[Installation].OnWriteLog := Value;
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
    Result := Target.VersionNumber in [5..7, 9];
  {$ENDIF ~KYLIX}
end;

// History:

// $Log$
// Revision 1.75  2005/10/26 06:30:38  rrossmair
// - TJclInstallation.UninstallExpert now also handles old expert package names
//
// Revision 1.74  2005/10/22 00:39:34  outchy
// JclBaseExpert is now correctly uninstalled.
//
// Revision 1.73  2005/10/20 23:13:32  outchy
// Experts are now generated by the package generator.
// No WEAKPACKAGEUNIT in design-time packages.
//
// Revision 1.72  2005/10/18 23:09:56  rrossmair
// - updated Installer for new package naming rules
//
// Revision 1.70  2005/09/23 22:46:31  rrossmair
// - changed to ensure that TInstallation.Demos is assigned when needed; likewise DemoExclusionList
// - some refactoring
//
// Revision 1.69  2005/09/18 20:13:10  rrossmair
// - several additions/minor fixes
//
// Revision 1.68  2005/08/22 19:30:58  rrossmair
// - TJclInstallation.BuildUnitList fault tolerance improved
//
// Revision 1.67  2005/08/22 02:08:40  rrossmair
// - implemented ability to specify which demos are to be built
//
// Revision 1.66  2005/08/06 11:19:34  rrossmair
// - demo building improved: handles exclusion files etc.
//
// Revision 1.65  2005/08/01 04:52:02  rrossmair
// - (basic) support for compilation of examples
//
// Revision 1.64  2005/07/28 21:57:49  outchy
// JEDI Installer can now install design-time packages for C++Builder 5 and 6
//
// Revision 1.63  2005/03/24 20:41:32  rrossmair
// - fixed installation progress computation.
//
// Revision 1.62  2005/03/23 04:28:49  rrossmair
// - removed make -fBCB5-dcc32.cfg.mak (handled by build.exe now)
//
// Revision 1.61  2005/03/22 03:23:18  rrossmair
// - fixed recent changes
//
// Revision 1.60  2005/03/21 11:09:49  obones
// Now calls BCB5-dcc32.cfg.mak if required
//
// Revision 1.59  2005/03/21 04:03:58  rrossmair
// - workarounds for DCC32 126 character path limit
//
// Revision 1.57  2005/03/16 18:11:33  rrossmair
// - "Copy HPP files to ..." options now checked by default.
//
// Revision 1.56  2005/03/14 16:10:43  rrossmair
// - compiler hints resolved
//
// Revision 1.55  2005/03/14 08:46:47  rrossmair
// - check-in in preparation for release 1.95
//
// Revision 1.54  2005/03/14 02:21:34  rrossmair
// - changed Expert naming convention to include Delphi/BCB infix (D|C)
//
// Revision 1.53  2005/03/05 06:33:17  rrossmair
// - support for some conditional defines added.
//
// Revision 1.52  2005/02/28 20:19:05  uschuster
// changes for Uses wizard
//
// Revision 1.51  2005/02/23 08:32:30  rrossmair
// - TJclInstallation: replaced Target.DCC.Options.Clear by Target.DCC.SetDefaultOptions.
//   Some cleanup.
//
// Revision 1.50  2005/02/05 05:16:18  rrossmair
// - check-in for release 1.94.1.1802
//
// Revision 1.49  2005/02/04 05:19:41  rrossmair
// - some uninstall support finally functional
//
// Revision 1.48  2005/02/03 06:15:41  rrossmair
// - fixed for Kylix
//
// Revision 1.47  2005/02/03 05:22:17  rrossmair
// - more uninstall support (still unfinished)
//
// Revision 1.46  2004/12/23 05:32:28  rrossmair
// - fixed for Kylix
//
// Revision 1.45  2004/12/23 05:09:26  rrossmair
// - except dialog and help integration disabled for D 2005
//
// Revision 1.44  2004/12/15 21:49:35  rrossmair
// - denotes D2005 as supported now
//
// Revision 1.43  2004/12/08 18:25:07  rrossmair
// - fixed TJclInstallation.StoredOption so that Default parameter gets evaluated
//
// Revision 1.42  2004/12/08 18:14:49  rrossmair
// - all install options now selected by default
// - minor fixes
//
// Revision 1.41  2004/11/18 10:14:54  rrossmair
// - changes for release 1.93
//
// Revision 1.40  2004/11/17 06:34:01  marquardt
// suppress warning about unused UninstallOption
//
// Revision 1.39  2004/11/14 12:08:05  rrossmair
// - some precautions & minor fixes
//
// Revision 1.38  2004/11/14 05:55:55  rrossmair
// - installer refactoring (continued)
//
// Revision 1.37  2004/11/10 05:18:11  rrossmair
// - fixed for Kylix
//
// Revision 1.36  2004/11/09 07:51:37  rrossmair
// - installer refactoring (incomplete)
//

end.
