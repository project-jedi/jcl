
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
    function GetDemoList: TStringList;
    procedure BuildUnitList(const SubDir: string; Units: TStrings);
    function GetDemoExclusionList: TStrings;
    function GetProgressTotal: Integer;
    function GetTool: IJediInstallTool;
    function GetUnits(const SourceDir: string): TStrings;
    function InitOptions: Boolean;
    procedure InstallationStarted;
    procedure InstallationFinished;
    procedure InstallFailedOn(const InstallObj: string);
    procedure ConfigureBpr2Mak(const PackageFileName: string);
    {$IFDEF MSWINDOWS}
    function CompileExpert(const Name: string; InstallExpert: Boolean): Boolean;
    {$ENDIF MSWINDOWS}
    function CompilePackage(const Name: string; InstallPackage: Boolean): Boolean;
    function CompilePackages: Boolean;
    function InstallOption(Option: TJediInstallOption): Boolean;
    procedure RemoveDialogFromRepository(const DialogName, DialogFileName: string);
    function UninstallPackage(const Name: string): Boolean;
    function UninstallPackages: Boolean;
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
    function GetJclDcpPath: string;
    {$IFDEF MSWINDOWS}
    function InstallExpert(const Option: TJediInstallOption): Boolean;
    function UninstallExpert(const Option: TJediInstallOption): Boolean;
    {$ENDIF MSWINDOWS}
  protected
    constructor Create(JclDistribution: TJclDistribution; InstallTarget: TJclBorRADToolInstallation);
    function CompileLibraryUnits(const SubDir: string; Debug: Boolean): Boolean;
    {$IFDEF MSWINDOWS}
    procedure AddHelpToIdeTools;
    procedure AddHelpToOpenHelp;
    procedure RemoveHelpFromIdeTools;
    procedure RemoveHelpFromOpenHelp;
    procedure RegisterHelp2Files;
    procedure UnregisterHelp2Files;
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
    function RemoveSettings: Boolean;
    function StoredBplPath: string;
    function StoredDcpPath: string;
    property Defines: TStringList read FDefines;
    property Demos: TStringList read GetDemoList;
    property DemoSectionName: string read FDemoSectionName;
    property DemoExclusionList: TStrings read GetDemoExclusionList;
    property Tool: IJediInstallTool read GetTool;
    property DebugDcuDir: string read FDebugDcuDir;
    property LibDir: string read FLibDir;
    property LibObjDir: string read FLibObjDir;
    property ProgressTotal: Integer read GetProgressTotal;
    property RelativeDemoPath: string read FRelativeDemoPath;
    property Target: TJclBorRADToolInstallation read FTarget;
    property Units[const SourceDir: string]: TStrings read GetUnits;
    property JclDcpPath: string read GetJclDcpPath;
  public
    destructor Destroy; override;
    property OnWriteLog: TTextHandler read FOnWriteLog write FOnWriteLog;
    property Distribution: TJclDistribution read FDistribution;
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
    FJclHxSHelpFileName: string;
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
    function GetVersion: string;
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
    property HxSHelpFileName: string read FJclHxSHelpFileName;
    property Installing: Boolean read FInstalling;
    property Path: string read FJclPath;
    property SourceDir: string read FJclSourceDir;
    property SourcePath: string read FJclSourcePath;
    property Tool: IJediInstallTool read FTool write SetTool;
    property Version: string read GetVersion;
  end;

function CreateJclInstall: IJediInstall;
function LogFileName(Target: TJclBorRADToolInstallation): string;

implementation

uses
  {$IFDEF VCL}
  Dialogs, Controls,
  {$ELSE VCL}
  QDialogs, QControls,
  {$ENDIF VCL}
  JclBase, JclResources, JclSysInfo,
  {$IFDEF MSWINDOWS}
  JclPeImage,
  JClRegistry,
  MSHelpServices_TLB,
  {$ENDIF MSWINDOWS}
  JclFileUtils, JclStrings,
  JediRegInfo;

{ Install option data }

resourcestring
// Captions

  // Products
  RsJCL                  = 'JEDI Code Library';

  // Common features
  RsDefThreadSafe        = 'Enable thread safe code';
  RsDefDropObsoleteCode  = 'Drop obsolete code';
  RsDefMathPrecSingle    = 'Single float precision';
  RsDefMathPrecDouble    = 'Double float precision';
  RsDefMathPrecExtended  = 'Extended float precision';

  RsMapCreate            = 'Create MAP files';
  RsMapLink              = 'Link MAP files';
  RsMapDelete            = 'Delete MAP files after the link';

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
  RsIdeHelpHxS           = 'Register help 2.0 files';
  RsIdeHelpHxSPlugin     = 'Plug help 2.0 files in the Borland help system';
  RsCopyHppFiles         = 'Copy HPP files to %s';
  RsDualPackages         = 'Dual packages';
  RsCopyPackagesHppFiles = 'Output HPP files to %s';

  // Product specific features
  RsJCLExceptDlg         = 'Sample Exception Dialogs in the Object Reporitory';
  RsJCLDialogVCL         = 'VCL Exception Dialog';
  RsJCLDialogVCLSnd      = 'VCL Exception Dialog with Send button';
  RsJCLDialogCLX         = 'CLX Exception Dialog';
  RsExpertsDsgnPackages  = 'Design packages';
  RsExpertsDLL           = 'DLL experts';
  RsJCLIdeDebug          = 'Debug Extension';
  RsJCLIdeAnalyzer       = 'Project Analyzer';
  RsJCLIdeFavorite       = 'Favorite combobox in Open/Save dialogs';
  RsJCLIdeThreadNames    = 'Displaying thread names in Thread Status window';
  RsJCLIdeUses           = 'Uses Wizard';
  RsJCLSimdView          = 'Debug window for XMM registers';
  RsJCLVersionControl    = 'Version control';

// Hints
  RsHintTarget = 'Installation target';
  RsHintJCL = 'Select to install JCL for this target.';
  RsHintJclDefThreadSafe        = 'Conditionally some pieces of code to be thread safe, the ThreadSafe.txt file contains more informations about this feature';
  RsHintJclDefDropObsoleteCode  = 'Do not compile deprecated code';
  RsHintJclDefMathPrecSingle    = 'type Float = Single';
  RsHintJclDefMathPrecDouble    = 'type Float = Double';
  RsHintJclDefMathPrecExtended  = 'type Float = Extended';
  RsHintJclMapCreate            = 'Create detailled MAP files for each libraries';
  RsHintJclMapLink              = 'Link MAP files as a resource in the output library or executable, the stack can be traced on exceptions';
  RsHintJclMapDelete            = 'Once linked in the binary, delete the original MAP file';
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
  RsHintJclDualPackages = 'The same package introduce component for Delphi Win32 and C++Builder Win32';
  RsHintJclPackages = 'Build and eventually install JCL runtime packages (RTL, VCL and Visual ' +
    'CLX) and optional IDE experts.';
  RsHintJclExperts = 'Build and install selected IDE experts.';
  RsHintJclExpertsDsgnPackages = 'Design packages containing JCL experts';
  RsHintJclExpertsDLL = 'DLLs containing JCL experts';
  RsHintJclExpertDebug = 'Install IDE expert which assists to insert JCL Debug information into ' +
    'executable files.';
  RsHintJclExpertAnalyzer = 'Install IDE Project Analyzer.';
  RsHintJclExpertFavorite = 'Install "Favorites" combobox in IDE Open/Save dialogs.';
  RsHintJclExpertsThreadNames = 'Display thread names in Thread Status window IDE extension.';
  RsHintJclExpertUses = 'Install IDE Uses Wizard.';
  RsHintJclExpertSimdView = 'Install a debug window of XMM registers (used by SSE instructions)';
  RsHintJclExpertVersionControl = 'Integration of TortoiseCVS and TortoiseSVN in the IDE';
  RsHintJclCopyPackagesHppFiles = 'Output .hhp files into C++Builder''s include path instead of ' +
    'the source paths.';
  RsHintJclExcDialog = 'Add selected Exception dialogs to the Object Repository.';
  RsHintJclExcDialogVCL = 'Add VCL exception dialog to the Object Repository.';
  RsHintJclExcDialogVCLSnd = 'Add VCL exception dialog with "Send Button" to the Object Repository.';
  RsHintJclExcDialogCLX = 'Add CLX exception dialog (Windows only) to the Object Repository.';
  RsHintJclHelp = 'Install JCL help files.';
  RsHintJclHelpHlp = 'Customize Borland Open Help to include JCL help files.';
  RsHintJclHelpChm = 'Compiled help files won''t be merged with the IDE help';
  RsHintJclHelpHxS = 'Register Help 2.0 files';
  RsHintJclHelpHxSPlugin = 'Register Help 2.0 files as a plugin for the Borland.BDS* namespace';
  RsHintJclMakeDemos = 'Make JCL demo applications';

// warning messages
  RsPackageNodeNotSelected = 'The "Packages" node is not selected.' + sLineBreak +
    'Various libraries (including the JVCL) require JCL packages to be compiled' + sLineBreak +
    'Do you want to continue without compiling JCL packages?';
  RsCreatePath = 'The path where %s files will be created doesn''t exists.' + sLineBreak +
    'Do you want the JCL installer to create it?';
  RsCantCreatePath = 'The path %s cannot be created';
  RsAddPathToEnvironment = 'The path where BPL are created must be present in the PATH' + sLineBreak +
    'environment variable, otherwise JCL packages won''t be found by the IDE.' + sLineBreak +
    'Do you want the JCL installer to add it?' + sLineBreak +
    'You will have to reboot your computer and/or to close your session to validate this change';

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
      (Parent: ioJCL;                    // ioJclDefThreadSafe
       Caption: RsDefThreadSafe;
       Hint: RsHintJclDefThreadSafe),
      (Parent: ioJCL;                    // ioJclDefDropObsoleteCode
       Caption: RsDefDropObsoleteCode;
       Hint: RsHintJclDefDropObsoleteCode),
      (Parent: ioJCL;                    // ioJclDefMathPrecSingle
       Caption: RsDefMathPrecSingle;
       Hint: RsHintJclDefMathPrecSingle),
      (Parent: ioJCL;                    // ioJclDefMathPrecDouble
       Caption: RsDefMathPrecDouble;
       Hint: RsHintJclDefMathPrecDouble),
      (Parent: ioJCL;                    // ioJclDefMathPrecExtended
       Caption: RsDefMathPrecExtended;
       Hint: RsHintJclDefMathPrecExtended),
      (Parent: ioJCL;                    // ioJclMapCreate
       Caption: RsMapCreate;
       Hint: RsHintJclMapCreate),
      (Parent: ioJclMapCreate;           // ioJclMapLink
       Caption: RsMapLink;
       Hint: RsHintJclMapLink),
      (Parent: ioJclMapLink;             // ioJclMapDelete
       Caption: RsMapDelete;
       Hint: RsHintJclMapDelete),
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
      (Parent: ioJclPackages;            // ioJclDualPackages
       Caption: RsDualPackages;
       Hint: RsHintJclDualPackages),
      (Parent: ioJCL;                    // ioJclPackages
       Caption: RsJCLPackages;
       Hint: RsHintJclPackages),
      (Parent: ioJclExperts;             // ioJclExpertsDesignPackages
       Caption: RsExpertsDsgnPackages;
       Hint: RsHintJclExpertsDsgnPackages),
      (Parent: ioJclExperts;             // ioJclExpertsDLL
       Caption: RsExpertsDLL;
       Hint: RsHintJclExpertsDLL),
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
      (Parent: ioJclExperts;             // ioJclExpertVersionControl
       Caption: RsJclVersionControl;
       Hint: RsHintJclExpertVersionControl),
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
      (Parent: ioJclHelp;                // ioJclHelpHxS
       Caption: RsIdeHelpHxS;
       Hint: RsHintJclHelpHxS),
      (Parent: ioJclHelpHxS;             // ioJclHelpHxSPlugin
       Caption: RsIdeHelpHxSPlugin;
       Hint: RsHintJclHelpHxSPlugin),
      (Parent: ioJCL;                    // ioJclMakeDemos
       Caption: RsMakeDemos;
       Hint: RsHintJclMakeDemos)
    );

const
  {$IFDEF KYLIX}
  VersionDir = '/k%d';
  VersionDirExp = '/k%%d';
  {$ELSE}
  VersionDir = '\%s';
  VersionDirExp = '\%%s';
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

  JclDpk     = 'Jcl';
  JclVclDpk  = 'JclVcl';
  JclVClxDpk = 'JclVClx';

  JclIdeBaseDpk     = 'JclBaseExpert';
  JclIdeDebugDpk    = 'JclDebugExpert';
  JclIdeAnalyzerDpk = 'JclProjectAnalysisExpert';
  JclIdeFavoriteDpk = 'JclFavoriteFoldersExpert';
  JclIdeThrNamesDpk = 'JclThreadNameExpert';
  JclIdeUsesDpk     = 'JclUsesExpert';
  JclIdeSimdViewDpk = 'JclSIMDViewExpert';
  JclIdeVersionControlDpk = 'JclVersionControlExpert';
  JclBdsExpertDpr   = 'JclBdsExpert';

  ExpertPaths: array[ioJclExperts..ioJclExpertVersionControl] of string =
    (
      JclIdeBaseDpk,
      JclIdeDebugDpk,
      JclIdeAnalyzerDpk,
      JclIdeFavoriteDpk,
      JclIdeThrNamesDpk,
      JclIdeUsesDpk,
      JclIdeSimdViewDpk,
      JclIdeVersionControlDpk
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

  DialogsPath       = 'experts' + DirDelimiter + 'debug' + DirDelimiter + 'dialog' + DirDelimiter;
  ClxDialogFileName = 'ClxExceptDlg.pas';
  ClxDialogName     = 'CLX Exception Dialog';
  DialogDescription = 'JCL Application exception dialog';
  DialogAuthor      = 'Project JEDI';
  DialogPage        = 'Dialogs';

  Help2NameSpace         = 'Jedi.Jcl';
  Help2Collection        = 'JCLHelp_COL_MASTER.HxC';
  Help2Description       = 'JEDI Code Library';
  Help2Identifier        = 'JCLHelp';
  Help2LangId            = 1033;         // en/english
  Help2HxSFile           = 'JCLHelp.HxS';
  Help2HxIFile           = 'JCLHelp.HxI';

  JclChmHelpFile    = 'help' + DirDelimiter + 'JCLHelp.chm';
  JclHlpHelpFile    = 'help' + DirDelimiter + 'JCLHelp.hlp';
  JclHxSHelpFile     = 'help' + DirDelimiter + 'JCLHelp.HxS';
  JclHelpTitle      = 'JCL %d.%d Help';
  JclHelpIndexName  = 'Jedi Code Library Reference';
  HHFileName        = 'HH.EXE';

  {$IFDEF MSWINDOWS}
  Bcb2MakTemplate = 'packages\BCB.bmk';
  {$ENDIF MSWINDOWS}
  {$IFDEF KYLIX}
  Bcb2MakTemplate = 'packages/bcb.gmk';
  {$ENDIF KYLIX}

  PathEnvironmentVar = 'PATH';
  RegHKCUEnvironmentVar = 'Environment';
  RegHKLMEnvironmentVar = 'SYSTEM\ControlSet001\Control\Session Manager\Environment';

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
    if FileExists(FileName) then
      Result := Result and FileCopy(FileName, TargetDir + FileName, True);
  end;
end;

function FullPackageFileName(Target: TJclBorRADToolInstallation; const BaseName: string): string;
const
  S = 'packages' + VersionDir + DirDelimiter + '%s';
begin
  with Target do
  begin
    {$IFDEF KYLIX}
    Result := Format(S + '%s', [VersionNumber, BaseName, PackageSourceFileExtension]);
    {$ELSE KYLIX}
    if SupportsLibSuffix then
      Result := Format(S + '%s', [VersionNumberStr, BaseName, PackageSourceFileExtension])
    else
      Result := Format(S + '%s0%3:s', [VersionNumberStr, BaseName, VersionNumberStr, PackageSourceFileExtension]);
    {$ENDIF KYLIX}
  end;
end;

{$IFDEF MSWINDOWS}
function FullLibraryFileName(Target: TJclBorRADToolInstallation; const BaseName: string): string;
const
  S = 'packages' + VersionDir + DirDelimiter + '%s';
begin
  with Target do
    if SupportsLibSuffix then
      Result := Format(S + 'DLL%s', [VersionNumberStr, BaseName, ProjectSourceFileExtension])
    else
      Result := Format(S + 'DLL%s0%3:s', [VersionNumberStr, BaseName, VersionNumberStr, ProjectSourceFileExtension]);
end;
{$ENDIF MSWINDOWS}

function LogFileName(Target: TJclBorRADToolInstallation): string;
begin
  with Target do
    Result := Format('%s%s.log', [PathAddSeparator(ExtractFileDir(ParamStr(0))), Target.Name]);
end;

{ TJclInstallation }

constructor TJclInstallation.Create(JclDistribution: TJclDistribution;
  InstallTarget: TJclBorRADToolInstallation);
begin
  inherited Create;
  FDistribution := JclDistribution;
  FTarget := InstallTarget;
  InstallTarget.OutputCallback := WriteLog;
  FDebugDcuDir := MakePath(Distribution.FLibDebugDirMask);
  FLibDir := MakePath(Distribution.FLibDirMask);
  if InstallTarget is TJclBCBInstallation then
    FLibObjDir := MakePath(Distribution.FLibObjDirMask);
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
  if Assigned(FUnits) then
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
      Result := StringsToStr(Target.DCC32.Options, ' ') + ' ';
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
      Result := Target.DCC32.Execute({$IFNDEF KYLIX}CompilationOptions + {$ENDIF}UnitList[I]);
      Progress(1);
      if not Result then
        Break;
    end;
  end;
  {$ELSE}
  begin
    Result := Target.DCC32.Execute({$IFNDEF KYLIX}CompilationOptions + {$ENDIF}StringsToStr(UnitList, ' '));
    Progress(UnitList.Count);
  end;
  {$ENDIF}

begin
  if Debug then
    UnitType := 'debug ';
  LibDescriptor := Format(RsLibDescriptor, [SubDir, UnitType, Target.Name]);
  WriteLog(Format(LineBreak + 'Making %s', [LibDescriptor]));
  Tool.UpdateStatus(Format(RsCompilingMessage, [LibDescriptor]));
  Path := Format('%s' + DirDelimiter + '%s', [Distribution.SourceDir, SubDir]);
  UnitList := Units[SubDir];
  with Target.DCC32 do
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
        AddPathOption('N2', MakePath(Distribution.FLibDirMask + DirDelimiter + 'obj')); // .obj files
      end
      else
      begin
        Options.Add('-$YD');
        Options.Add('-$W+');
        Options.Add('-$O+');
        UnitOutputDir := MakePath(Distribution.FLibDirMask);
        AddPathOption('N2', UnitOutputDir + DirDelimiter + 'obj'); // .obj files
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
      if OptionSelected(ioJclCopyHppFiles) then
      begin
        Result := Result and CopyHppFiles(UnitList, Target.VclIncludeDir);
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

procedure TJclInstallation.ConfigureBpr2Mak(const PackageFileName: string);
var
  PackageDirectory: string;
begin
  PackageDirectory := PathAddSeparator(ExtractFileDir(PackageFileName));
  if clProj2Mak in Target.CommandLineTools then
  begin
    Target.Bpr2Mak.Options.Clear;
    Target.Bpr2Mak.Options.Add('-t' + ExtractRelativePath(PackageDirectory,Distribution.Path + Bcb2MakTemplate));
  end;
  {$IFDEF KYLIX}
  SetEnvironmentVar('OBJDIR', LibObjDir);
  SetEnvironmentVar('BPILIBDIR', DcpPath);
  SetEnvironmentVar('BPLDIR', BplPath);
  {$ELSE KYLIX}
  if clMake in Target.CommandLineTools then
  begin
    Target.Make.Options.Clear;
    Target.Make.AddPathOption('DBPILIBDIR=', DcpPath);
    Target.Make.AddPathOption('DBPLDIR=', BplPath);
    if OptionSelected(ioJclCopyPackagesHppFiles) then
      Target.Make.AddPathOption('DHPPDIR=', Target.VclIncludeDir);
  end;
  {$ENDIF KYLIX}
end;

function TJclInstallation.Description(Option: TJediInstallOption): string;
begin
  Result := InitData[Option].Caption;

  case Option of
    ioTarget:
      Result := Target.Description;
    ioJclCopyHppFiles:
      Result := Format(Result, [Target.VclIncludeDir]);
    ioJclCopyPackagesHppFiles:
      Result := Format(Result, [Target.VclIncludeDir]);
  end;
end;

{$IFDEF MSWINDOWS}
procedure TJclInstallation.CopyFakeXmlRtlPackage;
// replace missing xmlrtl.dcp in Delphi 2005 Personal by dummy package to allow expert installation
begin
  { TODO : implement copying of fake xmlrtl.dcp to $(BDS)\Lib }
end;
{$ENDIF MSWINDOWS}

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

function DemoNameCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  Name1, Name2: string;
begin
  Name1 := ExtractFileName(List[Index1]);
  Name2 := ExtractFileName(List[Index2]);
  Result := CompareText(Name1, Name2);
end;

function TJclInstallation.GetDemoList: TStringList;
begin
  if not Assigned(FDemos) then
  begin
    FDemos := TStringList.Create;
    EnumDirectories(Distribution.ExamplesDir, AddDemos);
    Demos.CustomSort(DemoNameCompare);
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
  FileMask := Format('%s' + DirDelimiter + '%s' + DirDelimiter + '*.pas', [Distribution.SourceDir, SubDir]);
  BuildFileList(FileMask, faAnyFile, Units);
  // check for units not to compile
  ExcludeListFileName := MakePath(Format('%s' + DirDelimiter + '%s.exc', [Distribution.FLibDirMask, SubDir]));
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
    {$IFDEF KYLIX}
    FileName := MakePath(Distribution.DemosPath + 'k%d.exc');
    {$ELSE}
    FileName := MakePath(Distribution.DemosPath + '%s.exc');
    {$ENDIF KYLIX}
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

function TJclInstallation.GetJclDcpPath: string;
begin
  Result := Format('%slib\%s', [Distribution.Path, Target.VersionNumberStr]);
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
  InstallationNode, ProductNode, PackagesNode, ExpertsNode, DemosNode,
  MakeNode, EnvNode, HelpNode, HelpHxSNode, RepositoryNode, MapCreateNode,
  MapLinkNode, BCBNode: TObject;
  RunTimeInstallation: Boolean;

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
    DemosNode := AddNode(ProductNode, ioJclMakeDemos, [goExpandable, goNoAutoCheck]);
    for I := 0 to Demos.Count - 1 do
      AddDemoNode(DemosNode, I);
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

  RunTimeInstallation := (Target.RadToolKind <> brBorlandDevStudio)
    or ((Target.VersionNumber >= 3) and (bpDelphi32 in Target.Personalities));

  GUI := Tool.OptionGUI(Target);
  InstallationNode := AddNode(nil, ioTarget);
  //InstallationNode.StateIndex := 0;
  ProductNode := AddNode(InstallationNode, ioJCL);

  if RunTimeInstallation then
  begin
    AddNode(ProductNode, ioJclDefThreadSafe);
    AddNode(ProductNode, ioJclDefDropObsoleteCode);
    AddNode(ProductNode, ioJclDefMathPrecSingle, [goRadioButton]);
    AddNode(ProductNode, ioJclDefMathPrecDouble, [goRadioButton]);
    AddNode(ProductNode, ioJclDefMathPrecExtended, [goRadioButton, goChecked]);

    EnvNode := AddNode(ProductNode, ioJclEnv);
    AddNode(EnvNode, ioJclEnvLibPath);
    AddNode(EnvNode, ioJclEnvBrowsingPath);
    AddNode(EnvNode, ioJclEnvDebugDCUPath);
  end;

  if RunTimeInstallation then
  begin
    MakeNode := AddNode(ProductNode, ioJclMake, [goExpandable, goChecked]);
    AddMakeNodes(MakeNode, False);
    AddMakeNodes(MakeNode, True);

    if bpBCBuilder32 in Target.Personalities then
      AddNode(MakeNode, ioJclCopyHppFiles);
    {$IFDEF MSWINDOWS}
    if Target.RadToolKind = brBorlandDevStudio then
    begin
      {TODO: expert help}
      if (Target.VersionNumber >= 3) and (Distribution.HxSHelpFileName <> '') then
      begin
        HelpNode := AddNode(ProductNode, ioJclHelp, [goChecked]);
        HelpHxSNode := AddNode(HelpNode, ioJclhelpHxS, [goStandaloneParent, goChecked]);
        AddNode(HelpHxSNode, ioJclHelpHxSPlugin, [goChecked]);
      end;
    end
    else
    begin
      if (Distribution.HlpHelpFileName <> '') or (Distribution.ChmHelpFileName <> '') then
      begin
        HelpNode := AddNode(ProductNode, ioJclHelp);
        if Distribution.HlpHelpFileName <> '' then
          AddNode(HelpNode, ioJclHelpHlp);
        if Distribution.ChmHelpFileName <> '' then
          AddNode(HelpNode, ioJclHelpChm);
      end;
    end;

    { TODO : Object Repository access for BDS }
    if Target.RadToolKind <> brBorlandDevStudio then
    {$ENDIF MSWINDOWS}
    begin
      RepositoryNode := AddNode(ProductNode, ioJclExcDialog);
      {$IFDEF MSWINDOWS}
      AddNode(RepositoryNode, ioJclExcDialogVCL);
      AddNode(RepositoryNode, ioJclExcDialogVCLSnd);
      if Target.SupportsVisualCLX then
      {$ENDIF MSWINDOWS}
        AddNode(RepositoryNode, ioJclExcDialogCLX);
    end;
  end;

  PackagesNode := AddNode(ProductNode, ioJclPackages, [goStandAloneParent, goChecked]);

  if (bpBCBuilder32 in Target.Personalities) and RunTimeInstallation then
  begin
    if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 4) then
    begin
      BCBNode := AddNode(PackagesNode, ioJclDualPackages, [goStandAloneParent, goChecked]);
      AddNode(BCBNode, ioJclCopyPackagesHppFiles);
    end
    else
      AddNode(PackagesNode, ioJclCopyPackagesHppFiles);
  end;

  MapCreateNode := AddNode(PackagesNode, ioJclMapCreate, [goExpandable, goStandaloneParent, goNoAutoCheck]);

  {$IFDEF MSWINDOWS}
  MapLinkNode := AddNode(MapCreateNode, ioJclMapLink, [goExpandable, goStandaloneParent, goNoAutoCheck]);
  AddNode(MapLinkNode,ioJclMapDelete, [goNoAutoCheck]);

  if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber = 3)
    and (Target.Edition = deStd) then
    CopyFakeXmlRtlPackage;
  { TODO :
    It has been reported that IDE experts don't work under Win98.
    Leave these options unchecked for Win9x/WinME until that has been examined. }
  if IsWinNT then
    ExpertOptions := [goChecked]
  else
    ExpertOptions := [];
  ExpertsNode := AddNode(PackagesNode, ioJclExperts, [goExpandable, goChecked]);

  if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber <= 2) then
    // design packages are not loaded by C#Builder 1 and Delphi 8
    AddNode(ExpertsNode, ioJclExpertsDLL, [goRadioButton, goChecked])
  else if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 3) then
    // expert DLLs are unstable on Delphi 2005 and BDS 2006
    // (problems while adding menu items in menu not yet loaded)
    AddNode(ExpertsNode, ioJclExpertsDesignPackages, [goRadioButton, goChecked])
  else
  begin
    AddNode(ExpertsNode, ioJclExpertsDesignPackages, [goRadioButton, goChecked]);
    AddNode(ExpertsNode, ioJclExpertsDLL, [goRadioButton]);
  end;

  if RunTimeInstallation then
  begin
    AddNode(ExpertsNode, ioJclExpertDebug, ExpertOptions);
    AddNode(ExpertsNode, ioJclExpertAnalyzer, ExpertOptions);
    AddNode(ExpertsNode, ioJclExpertUses, ExpertOptions);
    AddNode(ExpertsNode, ioJclExpertSimdView, ExpertOptions);
  end;
  AddNode(ExpertsNode, ioJclExpertFavorite, ExpertOptions);
  AddNode(ExpertsNode, ioJclExpertVersionControl, [goNoAutoCheck]);
  if (Target.RadToolKind <> brBorlandDevStudio) and (Target.VersionNumber <= 6) then
    AddNode(ExpertsNode, ioJclExpertThreadNames, ExpertOptions);
  {$ENDIF MSWINDOWS}
  if RunTimeInstallation then
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
  Personality: TJclBorPersonality;
begin
  Tool.UpdateStatus(Format(RsStatusMessage, [Target.Name]));
  WriteLog(StrPadRight(BorRADToolVersionStr, 44, '='));
  WriteLog('');
  WriteLog('Installed personalities :');
  for Personality := Low(TJclBorPersonality) to High(TJclBorPersonality) do
    if Personality in Target.Personalities then
  begin
    WriteLog(JclBorPersonalityDescription[Personality]);
  end;
  WriteLog(StrRepeat('=', 44));
    
  Result := CheckDirectories;
  if Result then
  begin
    CleanupRepository;
    Defines.Clear;
    Target.MapCreate := False;
    Target.MapLink := False;
    Target.MapDelete := False;
    {$IFDEF MSWINDOWS}
    if Target is TJclBDSInstallation then
      TJclBDSInstallation(Target).DualPackageInstallation := False;
    {$ENDIF MSWINDOWS}
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
      Defines.Add('THREADSAFE');
    ioJclDefDropObsoleteCode:
      Defines.Add('DROP_OBSOLETE_CODE');
    ioJclDefMathPrecSingle:
      Defines.Add('MATH_SINGLE_PRECISION');
    ioJclDefMathPrecDouble:
      Defines.Add('MATH_DOUBLE_PRECISION');
    ioJclDefMathPrecExtended:
      Defines.Add('MATH_EXTENDED_PRECISION');
    ioJclMapCreate:
      Target.MapCreate := True;
    ioJclMapLink:
      Target.MapLink := True;
    ioJclMapDelete:
      Target.MapDelete := True;
    ioJclEnvLibPath:
      begin
        Result := Target.AddToLibrarySearchPath(LibDir) and Target.AddToLibrarySearchPath(Distribution.SourceDir);
        if Result then
        begin
          WriteLog(Format(LineBreak + 'Added "%s;%s" to library search path.', [LibDir, Distribution.SourceDir]));
          {$IFDEF MSWINDOWS}
          if (Target.RadToolKind = brBorlandDevStudio) and (bpBCBuilder32 in Target.Personalities) then
            with TJclBDSInstallation(Target) do
          begin
            Result := AddToCppSearchPath(LibDir) and AddToCppSearchPath(Distribution.SourceDir);
            if Result then
              WriteLog(Format(LineBreak + 'Added "%s;%s" to cpp search path.', [LibDir, Distribution.SourceDir]))
            else
              WriteLog(LineBreak + 'Failed to add cpp search paths.');
          end;
          {$ENDIF MSWINDOWS}
        end
        else
          WriteLog(LineBreak + 'Failed to add library search paths.');
      end;
    ioJclEnvBrowsingPath:
      begin
        Result := Target.AddToLibraryBrowsingPath(Distribution.SourcePath);
        if Result then
        begin
          WriteLog(Format(LineBreak + 'Added "%s" to library browsing path.', [Distribution.SourcePath]));
          {$IFDEF MSWINDOWS}
          if (Target.RadToolKind = brBorlandDevStudio) and (bpBCBuilder32 in Target.Personalities) then
            with TJclBDSInstallation(Target) do
          begin
            Result := AddToCppBrowsingPath(Distribution.SourcePath);
            if Result then
              WriteLog(Format(LineBreak + 'Added "%s" to cpp browsing path.', [Distribution.SourcePath]))
            else
              WriteLog(LineBreak + 'Failed to add cpp browsing paths.');
          end;
          {$ENDIF MSWINDOWS}
        end
        else
          WriteLog(LineBreak + 'Failed to add library browsing path.');
      end;
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
    // ioJclCopyHppFiles: handled by CompileLibraryUnits
    //{$IFDEF MSWINDOWS}
    //ioJclDualPackages: handled by CompilePackages and InstallExpert
    //{$ENDIF MSWINDOWS}
    ioJclPackages:
      Result := CompilePackages;
    {$IFDEF MSWINDOWS}
    // ioJclExperts:
    ioJclExperts..ioJclExpertVersionControl:
      Result := InstallExpert(Option);
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
    ioJclHelpHxS:
      RegisterHelp2Files;
    {$ENDIF MSWINDOWS}
    ioJclMakeDemos:
      MakeDemos;
  end;
  if not (Option in [ioJclMakeRelease, ioJclMakeDebug]) then
    Progress(ProgressWeight(Option));
end;

function TJclInstallation.UninstallOption(Option: TJediInstallOption): Boolean;
begin
  Result := True;
  case Option of
    ioJclEnvLibPath:
      begin
        if Target.RemoveFromLibrarySearchPath(LibDir) and Target.RemoveFromLibrarySearchPath(Distribution.SourceDir) then
          WriteLog(Format(LineBreak + 'Removed "%s;%s" from library search path.', [LibDir, Distribution.SourceDir]))
        else
          WriteLog(LineBreak + 'Failed to remove library search path.');
        {$IFDEF MSWINDOWS}
        if (Target.RadToolKind = brBorlandDevStudio) and (bpBCBuilder32 in Target.Personalities) then
          with TJclBDSInstallation(Target) do
        begin
          if RemoveFromCppSearchPath(LibDir) and RemoveFromCppSearchPath(Distribution.SourceDir) then
            WriteLog(Format(LineBreak + 'Removed "%s;%s" from cpp search path.', [LibDir, Distribution.SourceDir]))
          else
            WriteLog(LineBreak + 'Failed to remove cpp search path.');
        end;
        {$ENDIF MSWINDOWS}
      end;
    ioJclEnvBrowsingPath:
      begin
        if Target.RemoveFromLibraryBrowsingPath(Distribution.SourcePath) then
          WriteLog(Format(LineBreak + 'Removed "%s" from library browsing path.', [Distribution.SourcePath]))
        else
          WriteLog(LineBreak + 'Failed to remove library browsing path.');
        {$IFDEF MSWINDOWS}
        if (Target.RadToolKind = brBorlandDevStudio) and (bpBCBuilder32 in Target.Personalities) then
          with TJclBDSInstallation(Target) do
        begin
          if RemoveFromCppBrowsingPath(Distribution.SourcePath) then
            WriteLog(Format(LineBreak + 'Removed "%s" from cpp browsing path.', [Distribution.SourcePath]))
          else
            WriteLog(LineBreak + 'Failed to remove cpp browsing path.');
        end;
        {$ENDIF MSWINDOWS}
      end;
    ioJclEnvDebugDCUPath:
      if Target.RemoveFromDebugDCUPath(DebugDcuDir) then
        WriteLog(Format(LineBreak + 'Removed "%s" from Debug DCU Path.', [DebugDcuDir]));
    // ioJclMake:
    ioJclMakeRelease: { TODO :  Delete generated files };
    ioJclMakeDebug: { TODO : Delete generated files  };
    ioJclCopyHppFiles: { TODO : Delete copied files };
    ioJclPackages:
      Result := UninstallPackages;
    {$IFDEF MSWINDOWS}
    ioJclExperts..ioJclExpertVersionControl:
      Result := UninstallExpert(Option);
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
    ioJclHelpHxS:
      UnregisterHelp2Files;
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

function TJclInstallation.InstallExpert(
  const Option: TJediInstallOption): Boolean;
begin
  if Option in [Low(ExpertPaths)..High(ExpertPaths)] then
  begin
    if (Option = ioJclExperts) or OptionSelected(ioJclExpertsDesignPackages) then
    begin
      // dual packages installation is useless for design time packages
      {$IFDEF MSWINDOWS}
      if Target.RadToolKind = brBorlandDevStudio then
        TJclBDSInstallation(Target).DualPackageInstallation := False;
      {$ENDIF MSWINDOWS}
      Result := CompilePackage(FullPackageFileName(Target,ExpertPaths[Option]), True);
    end
    else
      Result := CompileExpert(FullLibraryFileName(Target, ExpertPaths[Option]), True);
  end
  else
    Result := False;
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

{$IFDEF MSWINDOWS}
function TJclInstallation.CompileExpert(const Name: string; InstallExpert: Boolean): Boolean;
var
  ProjectFileName, ProjectBinaryFileName, ProjectDEFFileName,
  ProjectDescription: string;
  LibraryPeImage: TJclPeImage;
  ExportFuncList: TJclPeExportFuncList;
  Index: Integer;
  DEFFile: TStrings;
  FirstCompilationOk: Boolean;
const
  WizardEntryPoint = 'INITWIZARD0001';
  // note (outchy) : I don't know if the parameter signature is constant
  // if constant, the complete signature would be
  // @*@JCLWizardInit$qqsx56System@%DelphiInterface$t28Toolsapi@IBorlandIDEServices%pqqrx47System@%DelphiInterface$t19Toolsapi@IOTAWizard%$orpqqrv$v
  InternalEntryPoint = '@JCLWizardInit$';
begin
  ProjectFileName := PathAddSeparator(Distribution.Path) + Name;

  if InstallExpert then
    WriteLog(Format(LineBreak + 'Installing expert %s...', [ProjectFileName]))
  else
    WriteLog(Format(LineBreak + 'Compiling expert %s...', [ProjectFileName]));
  Tool.UpdateStatus(Format(RsStatusDetailMessage, [ExtractFileName(ProjectFileName), Target.Name]));

  if IsDelphiProject(ProjectFileName) and (bpDelphi32 in Target.Personalities) then
  begin
    if InstallExpert then
      Result := Target.InstallExpert(ProjectFileName, BplPath, JclDcpPath)
    else
      Result := Target.CompileProject(ProjectFileName, BplPath, JclDcpPath);
  end
  else if IsBCBProject(ProjectFileName) and (bpBCBuilder32 in Target.Personalities) then
  begin
    ConfigureBpr2Mak(ProjectFileName);
    // the compilation is done in 2 steps:
    //   - first compilation without changes, we try to find the internal export name
    //     for the wizard entry point function
    //   - second compilation with creation of an alias between the internal export name
    //     and the excepted export name

    ProjectDEFFileName := ChangeFileExt(ProjectFileName, CompilerExtensionDEF);
    // first compilation
    DEFFile := TStringList.Create;
    try
      // the linker doesn't like empty def files
      DEFFile.Add('EXPORTS');
      DEFFile.SaveToFile(ProjectDEFFileName);
    finally
      DEFFile.Free;
    end;

    Result := Target.CompileProject(ProjectFileName, BplPath, JclDcpPath);

    if Result then
    begin
      WriteLog(LineBreak + 'First compilation ok');
      LibraryPeImage := TJclPeImage.Create;
      try
        GetBPRFileInfo(ProjectFileName, ProjectBinaryFileName, @ProjectDescription);
        ProjectBinaryFileName := PathAddSeparator(BplPath) + ProjectBinaryFileName;

        WriteLog(Format(LineBreak + 'Analysing expert %s for entry point %s...', [ProjectBinaryFileName, WizardEntryPoint]));
        LibraryPeImage.FileName := ProjectBinaryFileName;
        ExportFuncList := LibraryPeImage.ExportList;

        FirstCompilationOk := Assigned(ExportFuncList.ItemFromName[WizardEntryPoint]);
        // the expected export name doesn't exist
        if not FirstCompilationOk then
        begin
          Result := False;
          WriteLog(LineBreak + 'Entry point not found');

          // try to find the decorated entry point
          // export names for pascal functions are:
          // @UnitName@FunctionName$ParameterSignature

          for Index := 0 to ExportFuncList.Count - 1 do
            if Pos(StrUpper(InternalEntryPoint), StrUpper(ExportFuncList.Items[Index].Name)) > 0 then
          begin
            WriteLog(Format(LineBreak + 'Internal entry point found %s', [ExportFuncList.Items[Index].Name]));
            DEFFile := TStringList.Create;
            try
              DEFFile.Add('EXPORTS');
              DEFFile.Add(Format('%s=%s', [WizardEntryPoint, ExportFuncList.Items[Index].Name]));
              DEFFile.SaveToFile(ProjectDEFFileName);
            finally
              DEFFile.Free;
            end;
            Result := True;
            Break;
          end;
        end
        else
        begin
          WriteLog(LineBreak + 'Entry point found, registering expert...');
          Target.RegisterExpert(ProjectBinaryFileName, ProjectDescription);
        end;
      finally
        LibraryPeImage.Free;
      end;

      if Result and (not FirstCompilationOk) then
      begin
        // second compilation
        if InstallExpert then
          Result := Target.InstallExpert(ProjectFileName, BplPath, JclDcpPath)
        else
          Result := Target.CompileProject(ProjectFileName, BplPath, JclDcpPath);
      end
      else if not Result then
        WriteLog(LineBreak + 'Internal entry point not found');
    end
    else
      WriteLog(LineBreak + 'First compilation failed');
  end
  else
    Result := False;

  if Result then
    WriteLog('...done.')
  else
    InstallFailedOn(ProjectFileName);
end;
{$ENDIF MSWINDOWS}

function TJclInstallation.CompilePackage(const Name: string; InstallPackage: Boolean): Boolean;
var
  PackageFileName: string;
{$IFNDEF KYLIX}
  DpkPackageFileName: string;
{$ENDIF}
begin
  PackageFileName := PathAddSeparator(Distribution.Path) + Name;
  if InstallPackage then
    WriteLog(Format(LineBreak + 'Installing package %s...', [PackageFileName]))
  else
    WriteLog(Format(LineBreak + 'Compiling package %s...', [PackageFileName]));
  Tool.UpdateStatus(Format(RsStatusDetailMessage, [ExtractFileName(PackageFileName), Target.Name]));

  if IsDelphiPackage(PackageFileName) and (bpDelphi32 in Target.Personalities) then
  begin
    if InstallPackage then
      Result := Target.InstallPackage(PackageFileName, BplPath, DcpPath)
    else
    begin
      {$IFNDEF KYLIX}
      if Target.RadToolKind = brBorlandDevStudio then
        (Target as TJclBDSInstallation).CleanPackageCache(BinaryFileName(BplPath, PackageFileName));
      {$ENDIF KYLIX}
      Result := Target.CompilePackage(PackageFileName, BplPath, DcpPath);
    end;
  end
  else if IsBCBPackage(PackageFileName) and (bpBCBuilder32 in Target.Personalities) then
  begin
    ConfigureBpr2Mak(PackageFileName);
    {$IFDEF KYLIX}
    if InstallPackage then
      Result := Target.InstallPackage(PackageFileName, BplPath, DcpPath)
    else
      Result := Target.CompilePackage(PackageFileName, BplPath, DcpPath);
    {$ELSE}

    if Target.RadToolKind = brBorlandDevStudio then
      (Target as TJclBDSInstallation).CleanPackageCache(BinaryFileName(BplPath, PackageFileName));

    // to satisfy JVCL (and eventually other libraries), create a .dcp file;
    // Note: it is put out to .bpl path to make life easier for JVCL
    DpkPackageFileName := ChangeFileExt(PackageFileName, SourceExtensionDelphiPackage);
    if InstallPackage then
      Result := ((not FileExists(DpkPackageFileName))
                 or Target.InstallPackage(DpkPackageFileName, BplPath, DcpPath))
                and Target.InstallPackage(PackageFileName, BplPath, DcpPath)
    else
      Result := ((not FileExists(DpkPackageFileName))
                 or Target.CompilePackage(DpkPackageFileName, BplPath, DcpPath))
                and Target.CompilePackage(PackageFileName, BplPath, DcpPath);
    {$ENDIF}
  end
  else
  begin
    Result := False;
    WriteLog(Format(LineBreak + 'No personality supports the extension %s', [ExtractFileExt(PackageFileName)]));
  end;

  if Result then
    WriteLog('...done.')
  else
    InstallFailedOn(PackageFileName);
end;

function TJclInstallation.CompilePackages: Boolean;
begin
  {$IFDEF MSWINDOWS}
  if Target.RadToolKind = brBorlandDevStudio then
    TJclBDSInstallation(Target).DualPackageInstallation := OptionSelected(ioJclDualPackages);
  InstallJediRegInformation(Target.ConfigDataLocation, 'JCL', Distribution.Version,
    DcpPath, BplPath, Distribution.FJclPath);
  {$ENDIF MSWINDOWS}
  Result := CompilePackage(FullPackageFileName(Target, JclDpk), False);
  if Target.SupportsVisualCLX then
    Result := Result and CompilePackage(FullPackageFileName(Target, JclVClxDpk), False);
  if (Target.VersionNumber >= 6)
    or ((Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 3)) then
    Result := Result and CompilePackage(FullPackageFileName(Target, JclVclDpk), False);
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
  Result := PathGetShortName(Format(FormatStr, [Target.VersionNumberStr]));
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
  Target.DCC32.Execute(FileName);
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
    if (JclSourceDirs[I] = JclSrcDirVisClx) and
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
    ioJclExpertDebug..ioJclExpertVersionControl:
      Result := 5;
    ioJclCopyPackagesHppFiles:
      Result := 2;
    ioJclExcDialog,
    ioJclExcDialogVCL,
    ioJclExcDialogVCLSnd,
    ioJclExcDialogCLX,
    ioJclHelpHlp,
    ioJclHelpChm,
    ioJclHelpHxS:
      Result := 3;
    ioJclMakeDemos:
      Result := 50;
  else
    Result := 0;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TJclInstallation.RegisterHelp2Files;
var
  Help2Manager: TJclHelp2Manager;
  CurrentDir: string;
  NameSpace, Collection, Description, Identifier, HxSFile, HxIFile: WideString;
  LangId: Integer;
begin
  if (Target.RadToolKind <> brBorlandDevStudio) or (Target.VersionNumber < 3) then
    Exit;
  
  WriteLog('Registering help 2.0 files');

  // to avoid Write AV, data has to be copied in data segment
  NameSpace := Help2NameSpace;
  Collection := Help2Collection;
  Description := Help2Description;
  Identifier := Help2Identifier;
  LangId := Help2LangId;
  HxSFile := Help2HxSFile;
  HxIFile := Help2HxIFile;

  CurrentDir := GetCurrentDir;
  if SetCurrentDir(Distribution.Path + 'help\') then
  try
    Help2Manager := TJclBDSInstallation(Target).Help2Manager;

    if Help2Manager.CreateTransaction then
      WriteLog('Transaction created')
    else
      WriteLog('Failed to create a transaction');
      
    WriteLog('Registering namespace...');
    if Help2Manager.RegisterNameSpace(NameSpace, Collection, Description) then
      WriteLog('...success')
    else
      WriteLog('...failed');

    WriteLog('Registering help file...');
    if Help2Manager.RegisterHelpFile(NameSpace, Identifier, LangId, HxSFile, HxIFile) then
      WriteLog('...success')
    else
      WriteLog('...failed');

    if OptionSelected(ioJclHelpHxSPlugin) then
    begin
      WriteLog('Registering plugin...');
      if Help2Manager.PlugNameSpaceInBorlandHelp(NameSpace) then
        WriteLog('...success')
      else
        WriteLog('...failed');
    end;

    if Help2Manager.CommitTransaction then
      WriteLog('Transaction committed')
    else
      WriteLog('Failed to commit the transaction');
  finally
    SetCurrentDir(CurrentDir);
  end;
end;
{$ENDIF MSWINDOWS}

procedure TJclInstallation.RemoveDialogFromRepository(const DialogName, DialogFileName: string);
begin
  Target.Repository.RemoveObjects(DialogsPath, DialogFileName, BorRADToolRepositoryFormTemplate);
  WriteLog(Format(LineBreak + 'Removed %s.', [DialogName]));
end;

{$IFDEF MSWINDOWS}
procedure TJclInstallation.RemoveHelpFromIdeTools;
var
  HelpIndex: Integer;
  HelpTitle: string;
begin
  HelpTitle := Format(JclHelpTitle, [JclVersionMajor, JclVersionMinor]);
  with Target.IdeTools do
  begin
    HelpIndex := IndexOfTitle(HelpTitle);
    if HelpIndex <> Invalid then
      RemoveIndex(HelpIndex);
  end;
end;

procedure TJclInstallation.RemoveHelpFromOpenHelp;
begin
  if Target.OpenHelp.RemoveHelpFile(Distribution.FJclHlpHelpFileName, JclHelpIndexName) then
    WriteLog(Format(LineBreak + 'Removed %s from %s Online Help', [Distribution.FJclHlpHelpFileName, Target.RADToolName]));
end;
{$ENDIF MSWINDOWS}

function TJclInstallation.Run: Boolean;
  procedure EnsureDirectoryExists(const DirectoryName, DisplayName: string);
  begin
    if not DirectoryExists(DirectoryName) then
    begin
      if (MessageDlg(Format(RsCreatePath, [DisplayName]), mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
        Abort;
      if not ForceDirectories(DirectoryName) then
      begin
        MessageDlg(Format(RsCantCreatePath, [DirectoryName]), mtError, [mbAbort], 0);
        Abort;
      end;
    end;
  end;
var
  PathEnvVar: string;
begin
  Result := True;
  if OptionSelected(ioJCL) then
  begin
    if not OptionSelected(ioJclPackages)
      and (MessageDlg(RsPackageNodeNotSelected, mtWarning, [mbYes, mbNo], 0) <> mrYes) then
      Abort;

    EnsureDirectoryExists(BplPath, 'BPL');
    EnsureDirectoryExists(DcpPath, 'DCP');

    {$IFDEF MSWINDOWS}
    PathEnvVar := RegReadStringDef(HKCU, RegHKCUEnvironmentVar, PathEnvironmentVar, '');
    PathListIncludeItems(PathEnvVar, RegReadStringDef(HKLM, RegHKLMEnvironmentVar, PathEnvironmentVar, ''));
    if (PathListItemIndex(PathEnvVar, BplPath) = -1) and (PathListItemIndex(PathEnvVar, PathAddSeparator(BplPath)) = -1)
      and (MessageDlg(RsAddPathToEnvironment, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      PathEnvVar := RegReadStringDef(HKCU, RegHKCUEnvironmentVar, PathEnvironmentVar, '');
      PathListIncludeItems(PathEnvVar, BplPath);
      RegWriteString(HKCU, RegHKCUEnvironmentVar, PathEnvironmentVar, PathEnvVar);
    end;
    {$ENDIF MSWINDOWS}

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

function TJclInstallation.RemoveSettings: Boolean;
var
  JclSettingsKey: string;
begin
  JclSettingsKey := Target.ConfigDataLocation + '\Jedi\JCL';
  if RegKeyExists(HKCU, JclSettingsKey) then
    Result := RegDeleteKeyTree(HKCU, JclSettingsKey)
  else
    Result := True;
end;

function TJclInstallation.UninstallPackage(const Name: string): Boolean;
var
  PackageFileName: string;
begin
  PackageFileName := Distribution.Path + Format(Name, [Target.VersionNumberStr]);

  {$IFNDEF KYLIX}
  if Target.RadToolKind = brBorlandDevStudio then
    (Target as TJclBDSInstallation).CleanPackageCache(BinaryFileName(StoredBPLPath, PackageFileName));
  {$ENDIF KYLIX}

  Result := Target.UninstallPackage(PackageFileName, StoredBPLPath, StoredDCPPath);

  // delete DCP files that were created to bpl path (old behavior)
  FileDelete(PathAddSeparator(StoredBPLPath) + PathExtractFileNameNoExt(Name) + CompilerExtensionDCP);
  // delete DCP files that were created to target dcp path (old behavior)
  FileDelete(PathAddSeparator(Target.DCPOutputPath) + PathExtractFileNameNoExt(Name) + CompilerExtensionDCP);
  // delete BPI files that were created to target dcp path (old behavior)
  FileDelete(PathAddSeparator(Target.DCPOutputPath) + PathExtractFileNameNoExt(Name) + CompilerExtensionBPI);
  // delete LIB files that were created to target dcp path (old behaviour)
  FileDelete(PathAddSeparator(Target.DCPOutputPath) + PathExtractFileNameNoExt(Name) + CompilerExtensionLIB);

  { TODO : evtl. remove .HPP Files }
  if Result then
    WriteLog(Format(LineBreak + 'Removed package %s.', [Name]));
end;

function TJclInstallation.UninstallPackages: Boolean;
begin
  Result := UninstallPackage(FullPackageFileName(Target, JclDpk));
  if Target.SupportsVisualCLX then
    Result := Result and UninstallPackage(FullPackageFileName(Target, JclVClxDpk));
  if Target.VersionNumber >= 6 then
    Result := Result and UninstallPackage(FullPackageFileName(Target, JclVclDpk));
  {$IFDEF MSWINDOWS}
  RemoveJediRegInformation(Target.ConfigDataLocation, 'JCL');
  {$ENDIF MSWINDOWS}
end;

{$IFDEF MSWINDOWS}
function TJclInstallation.UninstallExpert(const Option: TJediInstallOption): Boolean;

  function OldExpertBPLFileName(const BaseName: string): string;
  const
    OldExperts: array[ioJclExpertDebug..ioJclExpertVersionControl] of string = (
      'JclDebugIde%s0.bpl',
      'ProjectAnalyzer%s0.bpl',
      'IdeOpenDlgFavorite%s0.bpl',
      'ThreadNameExpert%s0.bpl',
      'JediUses%s0.bpl',
      'JclSIMDView%s.bpl',
      'JclVersionControl');

  var
    I: TJediInstallOption;
  begin
    with Target do
      for I := Low(OldExperts) to High(OldExperts) do
        if BaseName = ExpertPaths[I] then
    begin
      Result := PathAddSeparator(StoredBPLPath) + Format(OldExperts[I], [VersionNumberStr]);
      Break;
    end;
  end;

var
  BaseName: string;
  BPLFileName: string;
  PackageFileName: string;
  LibraryFileName: string;
begin
  Result := False;

  BaseName := ExpertPaths[Option];
  // uninstall package if it exists
  PackageFileName := FullPackageFileName(Target, BaseName);
  LibraryFileName := FullLibraryFileName(Target, BaseName);

  if FileExists(Distribution.Path + PackageFileName) then
  begin
    Result := UninstallPackage(PackageFileName);
    // eventually remove old expert packages to avoid annoying package conflicts during IDE startup;
    // for simplicity, .dcp files are not handled
    BaseName := ExtractFileName(BaseName);
    BPLFileName := OldExpertBPLFileName(BaseName);
    FileDelete(BPLFileName);
    Target.IdePackages.RemovePackage(BPLFileName);
  end;

  if FileExists(Distribution.Path + LibraryFileName) then
  begin
    // delete DLL experts
    Result := Target.UninstallExpert(Distribution.Path + LibraryFileName, StoredBPLPath);
  end;
end;

{$ENDIF MSWINDOWS}

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

{$IFDEF MSWINDOWS}
procedure TJclInstallation.UnregisterHelp2Files;
var
  Help2Manager: TJclHelp2Manager;
  CurrentDir: string;
  NameSpace, Identifier, HxSFile, HxIFile: WideString;
  LangId: Integer;
begin
  if (Target.RadToolKind <> brBorlandDevStudio) or (Target.VersionNumber < 3) then
    Exit;

  WriteLog('Unregistering help 2.0 files');

  // to avoid Write AV, data has to be copied in data segment
  NameSpace := Help2NameSpace;
  Identifier := Help2Identifier;
  LangId := Help2LangId;
  HxSFile := Help2HxSFile;
  HxIFile := Help2HxIFile;

  CurrentDir := GetCurrentDir;
  if SetCurrentDir(Distribution.Path + 'help\') then
  try
    Help2Manager := TJclBDSInstallation(Target).Help2Manager;

    if Help2Manager.CreateTransaction then
      WriteLog('Transaction created')
    else
      WriteLog('Failed to create a transaction');

    WriteLog('Unregistering plugin...');
    if Help2Manager.UnPlugNameSpaceFromBorlandHelp(NameSpace) then
      WriteLog('...success')
    else
      WriteLog('...failed');

    WriteLog('Unregistering help file...');
    if Help2Manager.UnregisterHelpFile(NameSpace, Identifier, LangId) then
      WriteLog('...success')
    else
      WriteLog('...failed');

    WriteLog('Unregistering namespace...');
    if Help2Manager.UnregisterNameSpace(NameSpace) then
      WriteLog('...success')
    else
      WriteLog('...failed');

    if Help2Manager.CommitTransaction then
      WriteLog('Transaction committed')
    else
      WriteLog('Failed to commit the transaction');
  finally
    SetCurrentDir(CurrentDir);
  end;
end;
{$ENDIF MSWINDOWS}


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
  Result := Distribution.FIniFile.ReadString(Target.Name, 'DCP-Path', JclDcpPath);
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
  try
    Inst := TJclInstallation.Create(Self, Target);
    FTargetInstalls.Add(Inst);
    Inst.InitOptions;
  except
  end;
  Result := True;
end;

function TJclDistribution.DocFileName(const BaseFileName: string): string;
const
  SDocFileMask = '%sdocs' + DirDelimiter + '%s';
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

function TJclDistribution.GetVersion: string;
begin
  Result := Format('%d.%d.%d.%d', [JclVersionMajor, JclVersionMinor, JclVersionRelease, JclVersionBuild]);
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
  FLibDebugDirMask := FLibDirMask + DirDelimiter + 'debug';
  FLibObjDirMask := FLibDirMask + DirDelimiter + 'obj';
  FJclBinDir := FJclPath + 'bin';
  FJclSourceDir := FJclPath + 'source';

  FJclSourcePath := '';
  for I := Low(JclSourceDirs) to High(JclSourceDirs) do
    FJclSourcePath := FJclSourcePath +
      Format('%s' + DirDelimiter + '%s' + DirSeparator, [FJclSourceDir, JclSourceDirs[I]]);

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
  FJclHxSHelpFileName := FJclPath + JclHxSHelpFile;
  if not FileExists(FJclChmHelpFileName) then
    FJclChmHelpFileName := '';
  if not FileExists(FJclHlpHelpFileName) then
    FJclHlpHelpFileName := '';
  if not FileExists(FJclHxSHelpFileName) then
    FJclHxSHelpFileName := '';
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
  KeepSettings: Boolean;
begin
  FInstalling := True; // tell UninstallOption not to call Progress()
  KeepSettings := False;
  Result := True;
  try
    InitProgress;

    for I := 0 to FTargetInstalls.Count - 1 do
      if TJclInstallation(FTargetInstalls[I]).OptionSelected(ioJCL) then
    begin
      KeepSettings := MessageDlg('Do you want to keep JCL expert settings ?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes;
      Break;
    end;

    for I := 0 to FTargetInstalls.Count - 1 do
    begin
      if not KeepSettings then
        TJclInstallation(FTargetInstalls[I]).RemoveSettings;
      TJclInstallation(FTargetInstalls[I]).Undo;
      Result := TJclInstallation(FTargetInstalls[I]).Run;
      if not Result then
        Break;
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
      Success := TJclInstallation(FTargetInstalls[I]).RemoveSettings
                 and TJclInstallation(FTargetInstalls[I]).Undo;
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
  case Target.RadToolKind of
    brDelphi :
      Result := Target.VersionNumber in [5, 6, 7];
    brCppBuilder :
      Result := Target.VersionNumber in [5, 6];
    brBorlandDevStudio :
      Result := Target.VersionNumber in [1, 2, 3, 4];
    else
      Result := False;
  end;
  {$ENDIF ~KYLIX}
end;

// History:

// $Log$
// Revision 1.100  2006/03/23 21:29:59  outchy
// Help 2.0 code moved to runtime units
// Fixed compilation of TLB files for BCB
//
// Revision 1.99  2006/03/22 19:52:17  outchy
// Fixed c5 and d5 compilation
//
// Revision 1.98  2006/03/15 20:48:34  outchy
// Fixed thread safe support
//
// Revision 1.97  2006/03/13 22:15:00  outchy
// PathSeparator renamed to DirDelimiter
// Installer checks paths
//
// Revision 1.96  2006/03/04 21:22:10  outchy
// Jcl directories added to the C++ side of BDS 2006
//
// Revision 1.95  2006/03/02 13:28:48  obones
// Now compiles fine with C5/D5, the help2 functions doing nothing at all in this case
//
// Revision 1.94  2006/02/28 18:41:55  ahuser
// Fixed BplDir <-> DcpDir swap
//
// Revision 1.93  2006/02/28 16:30:20  ahuser
// Jedi Registry Information record
//
// Revision 1.92  2006/02/26 18:31:42  outchy
// Chm help can now be removed
// Alpha version for the help 2.0
//
// Revision 1.91  2006/02/26 12:41:20  outchy
// Minor style cleaning
//
// Revision 1.90  2006/02/09 13:57:33  outchy
// Delete old compiler files
//
// Revision 1.89  2006/02/05 13:26:14  outchy
// dcp, bpi and lib files are created in \lib\ver
//
// Revision 1.88  2006/02/02 20:33:39  outchy
// Package cache cleaned
//
// Revision 1.87  2006/01/15 00:51:22  outchy
// cvs support in version control expert
// version control expert integration in the installer
//
// Revision 1.86  2006/01/13 16:52:00  outchy
// Warning of packages are not installed.
//
// Revision 1.85  2006/01/06 18:15:15  outchy
// hpp node moved as a child of the dual package node when supported
//
// Revision 1.84  2005/12/26 18:03:41  outchy
// Enhanced bds support (including C#1 and D8)
// Introduction of dll experts
// Project types in templates
//
// Revision 1.83  2005/12/04 10:10:57  obones
// Borland Developer Studio 2006 support
//
// Revision 1.82  2005/11/13 17:05:01  uschuster
// some fixes for Kylix
//
// Revision 1.81  2005/11/12 19:00:32  outchy
// map-files node moved inside the packages node.
//
// Revision 1.80  2005/11/10 23:59:50  outchy
// Map-file operations not added when not runned on Windows.
//
// Revision 1.79  2005/11/10 22:16:31  outchy
// Added creation/link/deletion of MAP files for packages.
//
// Revision 1.78  2005/11/08 00:18:32  outchy
// Fixed AV when DCC32.exe is missing.
//
// Revision 1.77  2005/10/28 04:38:53  rrossmair
// - fixes related to package uninstallation, and more
//
// Revision 1.76  2005/10/27 01:50:28  rrossmair
// - sort demo list alphabethically
//
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
