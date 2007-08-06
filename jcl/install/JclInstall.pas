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
{ Contributor(s):                                                                                  }
{   - Robert Rossmair - crossplatform & BCB support, refactoring                                   }
{   - Florent Ouchet (outchy) - New installer core for .net compilation                            }
{                                                                                                  }
{ Last modified: $Date$                          }
{                                                                                                  }
{**************************************************************************************************}

unit JclInstall;

interface

{$I jcl.inc}
{$I crossplatform.inc}

uses
  SysUtils, Classes, Contnrs,
  JclSysUtils, JclBorlandTools, JediInstall;

type
  TJclOption = (
    joLibrary,
      joDef,
        joDefMath,
        joDefDebug,
        joDefEDI,
        joDefPCRE,
        joDefBZip2,
        joDefThreadSafe,
        joDefDropObsoleteCode,
        joDefUnitVersioning,
        joDefMathPrecSingle,
        joDefMathPrecDouble,
        joDefMathPrecExtended,
        joDefMathExtremeValues,
        joDefHookDllExceptions,
        joDefDebugNoBinary,
        joDefDebugNoTD32,
        joDefDebugNoMap,
        joDefDebugNoExports,
        joDefDebugNoSymbols,
        joDefEDIWeakPackageUnits,
        joDefPCREStaticLink,
        joDefPCRELinkDLL,
        joDefPCRELinkOnRequest,
        joDefBZip2StaticLink,
        joDefBZip2LinkDLL,
        joDefBZip2LinkOnRequest,
      joEnvironment,
        joEnvLibPath,
        joEnvBrowsingPath,
        joEnvDebugDCUPath,
      joMake,
        joMakeRelease,
          joMakeReleaseVClx,
          joMakeReleaseVCL,
        joMakeDebug,
          joMakeDebugVClx,
          joMakeDebugVCL,
        joCopyHppFiles,
        joCheckHppFiles,
      joPackages,
        joDualPackages,
        joCopyPackagesHppFiles,
        joPdbCreate,
        joMapCreate,
          joJdbgCreate,
          joJdbgInsert,
          joMapDelete,
        joExperts,
          joExpertsDsgnPackages,
          joExpertsDLL,
          joExpertDebug,
          joExpertAnalyzer,
          joExpertFavorite,
          joExpertRepository,
          joExpertThreadNames,
          joExpertUses,
          joExpertSimdView,
          joExpertVersionControl,
      joExceptDlg,
        joExceptDlgVCL,
        joExceptDlgVCLSnd,
        joExceptDlgCLX,
      joHelp,
        joHelpHlp,
        joHelpChm,
        joHelpHxS,
        joHelpHxSPlugin,
      joMakeDemos);

  TJclDistribution = class;

  TJclInstallation = class
  private
    // identification
    FDistribution: TJclDistribution;
    FTarget: TJclBorRADToolInstallation;
    FCLRVersion: string;
    FTargetName: string;
    FTargetPlatform: TJclBorPlatform;
    FGUIPage: IJediInstallPage;
    FGUI: IJediInstallGUI;
    FGUIBPLPathIndex: Integer;
    FGUIDCPPathIndex: Integer;
    FLibDebugDir: string;
    FLibReleaseDir: string;
    FJclDcpPath: string;
    FDemoList: TStringList;
    FLogLines: TJclSimpleLog;
    FDemoSectionName: string;
    FLogFileName: string;
    FSilent: Boolean;
    procedure AddDemo(const Directory: string; const FileInfo: TSearchRec);
    procedure AddDemos(const Directory: string);
    function GetDemoList: TStringList;
    function MakePath(const FormatStr: string): string;
    procedure WriteLog(const Msg: string);
    function GetEnabled: Boolean;
  protected
    // if CLRVersion = '' then it is a native installation
    constructor Create(JclDistribution: TJclDistribution;
      InstallTarget: TJclBorRADToolInstallation; const ACLRVersion: string = '';
      ATargetPlatform: TJclBorPlatform = bp32bit; AGUIPage: IJediInstallPage = nil);
    function CompileLibraryUnits(const SubDir: string; Debug: Boolean): Boolean;
    {$IFDEF MSWINDOWS}
    function CompileCLRPackage(const Name: string): Boolean;
    {$ENDIF MSWINDOWS}
    function CompilePackage(const Name: string; InstallPackage: Boolean): Boolean;
    function CompileApplication(FileName: string): Boolean;
    function UninstallPackage(const Name: string): Boolean;
    procedure ConfigureBpr2Mak(const PackageFileName: string);
    {$IFDEF MSWINDOWS}
    function CompileExpert(const Name: string; InstallExpert: Boolean): Boolean;
    function UninstallExpert(const Option: TJclOption): Boolean;
    {$ENDIF MSWINDOWS}
    
    function GetBplPath: string;
    function GetDcpPath: string;
    function GetOptionChecked(Option: TJclOption): Boolean; overload;
    function GetOptionCheckedById(Id: Integer): Boolean; overload;
    procedure MarkOptionBegin(Id: Integer); overload;
    procedure MarkOptionBegin(Option: TJclOption); overload;
    procedure MarkOptionEnd(Id: Integer; Success: Boolean); overload;
    procedure MarkOptionEnd(Option: TJclOption; Success: Boolean); overload;
  public
    destructor Destroy; override;
    procedure Close;
    procedure Init;
    function RemoveSettings: Boolean;
    function Install: Boolean;
    function Uninstall(AUninstallHelp: Boolean): Boolean;

    property Distribution: TJclDistribution read FDistribution;
    property Target: TJclBorRADToolInstallation read FTarget;
    property CLRVersion: string read FCLRVersion;
    property TargetName: string read FTargetName;
    property GUIPage: IJediInstallPage read FGUIPage;
    property GUI: IJediInstallGUI read FGUI;
    property TargetPlatform: TJclBorPlatform read FTargetPlatform;
    property Enabled: Boolean read GetEnabled;
    property OptionCheckedById[Id: Integer]: Boolean read GetOptionCheckedById;
    property OptionChecked[Option: TJclOption]: Boolean read GetOptionChecked;
    property LogFileName: string read FLogFileName;
    property Silent: Boolean read FSilent write FSilent;
  end;

  TJclDistribution = class (TInterfacedObject, IJediProduct)
  private
    FJclPath: string;
    FJclBinDir: string;
    FLibReleaseDirMask: string;
    FLibDebugDirMask: string;
    FJclSourceDir: string;
    FJclSourcePath: string;
    FJclExamplesDir: string;
    FClxDialogFileName: string;
    FClxDialogIconFileName: string;
    FVclDialogFileName: string;
    FVclDialogSendFileName: string;
    FVclDialogIconFileName: string;
    FVclDialogSendIconFileName: string;
    FJclChmHelpFileName: string;
    FJclHlpHelpFileName: string;
    FJclHxSHelpFileName: string;
    FJclReadmeFileName: string;
    FGUI: IJediInstallGUI;
    FNbEnabled: Integer;
    FNbInstalled: Integer;
    {$IFDEF MSWINDOWS}
    FCLRVersions: TStrings;
    FRegHelpCommands: TStrings;
    {$ENDIF MSWINDOWS}
    FRadToolInstallations: TJclBorRADToolInstallations;
    FTargetInstalls: TObjectList;
{    FIniFile: TMemIniFile;
    FOnStarting: TInstallationEvent;
    FOnEnding: TInstallationEvent;
    FInstalling: Boolean;
    function CreateInstall(Target: TJclBorRADToolInstallation): Boolean;
    function GetTargetInstall(Installation: TJclBorRADToolInstallation): TJclInstallation;
    procedure InitInstallationTargets; }
    function GetVersion: string;
  {protected
    constructor Create;
    function DocFileName(const BaseFileName: string): string;
    procedure SetTool(const Value: IJediInstallTool);
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
    procedure SetOnStarting(Value: TInstallationEvent);
    function Supports(Target: TJclBorRADToolInstallation): Boolean;
    property BinDir: string read FJclBinDir;
    property ChmHelpFileName: string read FJclChmHelpFileName;
    property HlpHelpFileName: string read FJclHlpHelpFileName;
    property HxSHelpFileName: string read FJclHxSHelpFileName;
    property Installing: Boolean read FInstalling;
    property Path: string read FJclPath;
    property SourceDir: string read FJclSourceDir;
    property Tool: IJediInstallTool read FTool write SetTool;}
    property Version: string read GetVersion;
    function CreateInstall(Target: TJclBorRADToolInstallation): Boolean;
    function GetTargetInstall(Index: Integer): TJclInstallation;
    function GetTargetInstallCount: Integer;
    {$IFDEF MSWINDOWS}
    procedure RegHelpInternalAdd(Command: Integer; Arguments: string; DoNotRepeatCommand: Boolean);
    function RegHelpExecuteCommands(DisplayErrors: Boolean): Boolean;
    procedure RegHelpClearCommands;
    {$ENDIF MSWINDOWS}
  public
    constructor Create;
    destructor Destroy; override;

    {$IFDEF MSWINDOWS}
    procedure RegHelpCreateTransaction;
    procedure RegHelpCommitTransaction;
    procedure RegHelpRegisterNameSpace(const Name, Collection, Description: WideString);
    procedure RegHelpUnregisterNameSpace(const Name: WideString);
    procedure RegHelpRegisterHelpFile(const NameSpace, Identifier: WideString;
      const LangId: Integer; const HxSFile, HxIFile: WideString);
    procedure RegHelpUnregisterHelpFile(const NameSpace, Identifier: WideString;
      const LangId: Integer);
    procedure RegHelpPlugNameSpaceIn(const SourceNameSpace, TargetNameSpace: WideString);
    procedure RegHelpUnPlugNameSpace(const SourceNameSpace, TargetNameSpace: WideString);
    {$ENDIF MSWINDOWS}

    // IJediProduct
    procedure Init;
    procedure Install;
    procedure Uninstall;
    procedure Close;

    property JclPath: string read FJclPath;
    property JclBinDir: string read FJclBinDir;
    property LibReleaseDirMask: string read FLibReleaseDirMask;
    property LibDebugDirMask: string read FLibDebugDirMask;
    property JclSourceDir: string read FJclSourceDir;
    property JclSourcePath: string read FJclSourcePath;
    property JclExamplesDir: string read FJclExamplesDir;
    property ClxDialogFileName: string read FClxDialogFileName;
    property ClxDialogIconFileName: string read FClxDialogIconFileName;
    property VclDialogFileName: string read FVclDialogFileName;
    property VclDialogSendFileName: string read FVclDialogSendFileName;
    property VclDialogIconFileName: string read FVclDialogIconFileName;
    property VclDialogSendIconFileName: string read FVclDialogSendIconFileName;
    property JclChmHelpFileName: string read FJclChmHelpFileName;
    property JclHlpHelpFileName: string read FJclHlpHelpFileName;
    property JclHxSHelpFileName: string read FJclHxSHelpFileName;
    property JclReadmeFileName: string read FJclReadmeFileName;
    {$IFDEF MSWINDOWS}
    property CLRVersions: TStrings read FCLRVersions;
    {$ENDIF MSWINDOWS}
    property RadToolInstallations: TJclBorRADToolInstallations read FRadToolInstallations;
    property TargetInstalls[Index: Integer]: TJclInstallation read GetTargetInstall;
    property TargetInstallCount: Integer read GetTargetInstallCount;

    property GUI: IJediInstallGUI read FGUI;
    property NbEnabled: Integer read FNbEnabled;
    property NbInstalled: Integer read FNbInstalled;
  end;

implementation

uses
  TypInfo,
  JclBase, JclResources, JclSysInfo,
  {$IFDEF MSWINDOWS}
  Windows,
  JclPeImage,
  JclRegistry,
  JclDebug,
  JclDotNet,
  JclSecurity,
  JediRegInfo,
  JclShell,
  {$ENDIF MSWINDOWS}
  JclFileUtils, JclStrings;

resourcestring
// Names
  RsNameBPLPath = 'BPL-Path';
  RsNameDCPPath = 'DCP-Path';
  RsNameBPIPath = 'BPI-Path';

// Captions
  RsCaptionOutputPath = '&Output path:';
  RsCaptionBPLPath    = '&BPL path:';
  RsCaptionDCPPath    = '&DCP path:';
  RsCaptionBPIPath    = 'BP&I path:';

  // Products
  RsCaptionLibrary = 'JEDI Code Library';

  // Conditional features
  RsCaptionDef                    = 'Conditional defines';
  RsCaptionDefThreadSafe          = 'Enable thread safe code';
  RsCaptionDefDropObsoleteCode    = 'Drop obsolete code';
  RsCaptionDefUnitVersioning      = 'Include Unit Versioning';
  // math options
  RsCaptionDefMath                = 'Math options';
  RsCaptionDefMathPrecSingle      = 'Single float precision';
  RsCaptionDefMathPrecDouble      = 'Double float precision';
  RsCaptionDefMathPrecExtended    = 'Extended float precision';
  RsCaptionDefMathExtremeValues   = 'Support for infinite and NaN';
  // debug options
  RsCaptionDefDebug               = 'Debug and exception hooking options';
  RsCaptionDefHookDllExceptions   = 'Hook exceptions in DLL';
  RsCaptionDefDebugNoBinary       = 'No debug source from JEDI debug informations';
  RsCaptionDefDebugNoTD32         = 'No debug source from TD32 debug symbols';
  RsCaptionDefDebugNoMap          = 'No debug source from Map files';
  RsCaptionDefDebugNoExports      = 'No debug source from function export table for libraries';
  RsCaptionDefDebugNoSymbols      = 'No debug source from Microsoft debug symbols';
  // EDI options
  RsCaptionDefEDI                 = 'EDI options';
  RsCaptionDefEDIWeakPackageUnits = 'EDI weak package units';
  // PCRE options
  RsCaptionDefPCRE                = 'PCRE options';
  RsCaptionDefPCREStaticLink      = 'Static link to PCRE code';
  RsCaptionDefPCRELinkDLL         = 'Static bind to pcre.dll';
  RsCaptionDefPCRELinkOnRequest   = 'Late bind to pcre.dll';
  // BZip2 options
  RsCaptionDefBZip2               = 'BZip2 options';
  RsCaptionDefBZip2StaticLink     = 'Static link to BZip2 code (experimental)';
  RsCaptionDefBZip2LinkDLL        = 'Static bind to bzip2.dll';
  RsCaptionDefBZip2LinkOnRequest  = 'Late bind to bzip2.dll';
  
  // post compilation
  RsCaptionPdbCreate  = 'Create PDB debug information';
  RsCaptionMapCreate  = 'Create MAP files';
  RsCaptionJdbgCreate = 'Create JEDI Debug Informations';
  RsCaptionJdbgInsert = 'Insert JEDI Debug Informations in the libraries';
  RsCaptionMapDelete  = 'Do not keep MAP files';

  // environment
  RsCaptionEnvironment     = 'Environment';
  RsCaptionEnvLibPath      = 'Add JCL to IDE Library Path';
  RsCaptionEnvBrowsingPath = 'Add JCL to IDE Browsing Path';
  RsCaptionEnvDebugDCUPath = 'Add JCL to Debug DCU Path';

  // make units
  RsCaptionMake          = 'Make library units';
  RsCaptionMakeRelease   = 'Release';
  RsCaptionMakeDebug     = 'Debug';
  RsCaptionMakeVClx      = 'Visual CLX';
  RsCaptionMakeVCL       = 'Visual Component Library';
  RsCaptionCopyHppFiles  = 'Copy HPP files to %s';
  RsCaptionCheckHppFiles = 'Check HPP files';

  // packages
  RsCaptionPackages             = 'Packages';
  RsCaptionDualPackages         = 'Dual packages';
  RsCaptionCopyPackagesHppFiles = 'Output HPP files to %s';

  // exception dialogs
  RsCaptionExceptDlg       = 'Sample Exception Dialogs in the Object Repository';
  RsCaptionExceptDlgVCL    = 'VCL Exception Dialog';
  RsCaptionExceptDlgVCLSnd = 'VCL Exception Dialog with Send button';
  RsCaptionExceptDlgCLX    = 'CLX Exception Dialog';

  // experts
  RsCaptionExperts              = 'IDE experts';
  RsCaptionExpertsDsgnPackages  = 'Design packages';
  RsCaptionExpertsDLL           = 'DLL experts';
  RsCaptionExpertDebug          = 'Debug Extension';
  RsCaptionExpertAnalyzer       = 'Project Analyzer';
  RsCaptionExpertFavorite       = 'Favorite combobox in Open/Save dialogs';
  RsCaptionExpertRepository     = 'Exception dialog expert';
  RsCaptionExpertThreadNames    = 'Displaying thread names in Thread Status window';
  RsCaptionExpertUses           = 'Uses Wizard';
  RsCaptionExpertSimdView       = 'Debug window for XMM registers';
  RsCaptionExpertVersionControl = 'Version control';

  // help
  RsCaptionHelp          = 'Help files';
  RsCaptionHelpHlp       = 'Add help file to IDE help system';
  RsCaptionHelpChm       = 'Add HTML help to the Tools menu';
  RsCaptionHelpHxS       = 'Register help 2.0 files';
  RsCaptionHelpHxSPlugin = 'Plug help 2.0 files in the Borland help system';

  // demos
  RsCaptionMakeDemos = 'Make demos';

// Hints
  // products
  RsHintLibrary = 'Select to install JCL for this target.';

  // conditional defines
  RsHintDef                    = 'Enable or disable specific features to be compiled';
  RsHintDefThreadSafe          = 'Conditionally some pieces of code to be thread safe, the ThreadSafe.txt file contains more informations about this feature';
  RsHintDefDropObsoleteCode    = 'Do not compile deprecated code';
  RsHintDefUnitVersioning      = 'Includes JCL Unit Versioning informations into each JCL unit (see also JclUnitVersioning.pas)';
  // math options
  RsHintDefMath                = 'Math specific options (JclMath.pas)';
  RsHintDefMathPrecSingle      = 'type Float = Single';
  RsHintDefMathPrecDouble      = 'type Float = Double';
  RsHintDefMathPrecExtended    = 'type Float = Extended';
  RsHintDefMathExtremeValues   = 'Exp en Power functions accept and return infinite and NaN';
  // Debug options
  RsHintDefDebug               = 'Debug and exception hooking specific options (JclDebug.pas and JclHookExcept.pas)';
  RsHintDefHookDllExceptions   = 'Hook exceptions raised in DLL compiled with the JCL';
  RsHintDefDebugNoBinary       = 'Disable support for JDBG files';
  RsHintDefDebugNoMap          = 'Disable support for MAP files';
  RsHintDefDebugNoTD32         = 'Disable support for TD32 informations';
  RsHintDefDebugNoExports      = 'Disable support for export names of libraries';
  RsHintDefDebugNoSymbols      = 'Disable support for Microsoft debug symbols (PDB and DBG files)';
  // EDI options
  RsHintDefEDI                 = 'EDI specific options (JclEDI*.pas)';
  RsHintDefEDIWeakPackageUnits = 'Mark EDI units as weak package units (check if you use the original EDI package)';
  // PCRE options
  RsHintDefPCRE                = 'PCRE specific options (pcre.pas and JclPCRE.pas)';
  RsHintDefPCREStaticLink      = 'Code from PCRE is linked into JCL binaries';
  RsHintDefPCRELinkDLL         = 'JCL binaries require pcre.dll to be present';
  RsHintDefPCRELinkOnRequest   = 'JCL binaries require pcre.dll when calling PCRE functions';
  // BZip2 options
  RsHintDefBZip2               = 'BZip2 specific options (bzip2.pas)';
  RsHintDefBZip2StaticLink     = 'Code from BZip2 is linked into JCL binaries';
  RsHintDefBZip2LinkDLL        = 'JCL binaries require bzip2.dll to be present';
  RsHintDefBZip2LinkOnRequest  = 'JCL binaries require bzip2.dll when calling PCRE functions';

  // post compilation
  RsHintPdbCreate  = 'Create detailed debug information for libraries';
  RsHintMapCreate  = 'Create detailed MAP files for each libraries';
  RsHintJdbgCreate = 'Create JEDI Debug Informations from the MAP files';
  RsHintJdbgInsert = 'Insert JEDI Debug Informations into the libraries (only the BPL has to be redistributed)';
  RsHintMapDelete  = 'The original MAP file is not kept once JEDI Debug Informations are generated';

  // environment
  RsHintEnvironment     = 'Set selected environment items';
  RsHintEnvLibPath      = 'Add JCL precompiled unit directories to library path';
  RsHintEnvBrowsingPath = 'Add JCL source directories to browsing path';
  RsHintEnvDebugDCUPath = 'This is a prerequisite for using the precompiled JCL debug units by means of the respective' + AnsiLineBreak +
    'Project Options|Compiler switch. See "Make library units/Debug" option below.';

  // make units
  RsHintMake            = 'Generate .dcu and .dpu (Kylix only) files.' + AnsiLineBreak + 'Recommended.';
  RsHintMakeRelease     = 'Make precompiled units for release, i.e. optimized, w/o debug information.';
  RsHintMakeReleaseVcl  = 'Make precompiled VCL units for release';
  RsHintMakeReleaseVClx = 'Make precompiled Visual CLX units for release';
  RsHintMakeDebug       = 'Make precompiled units for debugging, i.e.optimization off, debug information included.' + AnsiLineBreak +
    'When installed, available through Project Options|Compiler|Use Debug DCUs.';
  RsHintMakeDebugVcl    = 'Make precompiled VCL units for debugging';
  RsHintMakeDebugVClx   = 'Make precompiled Visual CLX units for debugging';
  RsHintCopyHppFiles    = 'Copy .hpp files into C++Builder''s include path.';
  RsHintCheckHppFiles   = 'Compile some C++ source files to verify JCL headers';

  // packages
  RsHintPackages             = 'Build and eventually install JCL runtime packages (RTL, VCL and Visual ' +
    'CLX) and optional IDE experts.';
  RsHintDualPackages         = 'The same package introduce component for Delphi Win32 and C++Builder Win32';
  RsHintCopyPackagesHppFiles = 'Output .hpp files into C++Builder''s include path instead of ' +
    'the source paths.';

  // exception dialogs
  RsHintExceptDlg       = 'Add selected Exception dialogs to the Object Repository.';
  RsHintExceptDlgVCL    = 'Add VCL exception dialog to the Object Repository.';
  RsHintExceptDlgVCLSnd = 'Add VCL exception dialog with "Send Button" to the Object Repository.';
  RsHintExceptDlgCLX    = 'Add CLX exception dialog (Windows only) to the Object Repository.';

  // experts
  RsHintExperts              = 'Build and install selected IDE experts.';
  RsHintExpertsDsgnPackages  = 'Design packages containing JCL experts';
  RsHintExpertsDLL           = 'DLLs containing JCL experts';
  RsHintExpertDebug          = 'Install IDE expert which assists to insert JCL Debug information into executable files.';
  RsHintExpertAnalyzer       = 'Install IDE Project Analyzer.';
  RsHintExpertFavorite       = 'Install "Favorites" combobox in IDE Open/Save dialogs.';
  RsHintExpertRepository     = 'Repository expert to easily create exception dialogs';
  RsHintExpertThreadNames    = 'Display thread names in Thread Status window IDE extension.';
  RsHintExpertUses           = 'Install IDE Uses Wizard.';
  RsHintExpertSimdView       = 'Install a debug window of XMM registers (used by SSE instructions)';
  RsHintExpertVersionControl = 'Integration of TortoiseCVS and TortoiseSVN in the IDE';

  // help
  RsHintHelp          = 'Install JCL help files.';
  RsHintHelpHlp       = 'Customize Borland Open Help to include JCL help files.';
  RsHintHelpChm       = 'Compiled help files won''t be merged with the IDE help';
  RsHintHelpHxS       = 'Register Help 2.0 files';
  RsHintHelpHxSPlugin = 'Register Help 2.0 files as a plugin for the Borland.BDS* namespace';

  // demos
  RsHintMakeDemos = 'Make JCL demo applications';

// warning messages
  RsWarningPackageNodeNotSelected = 'The "Packages" node is not selected.' + sLineBreak +
    'Various libraries (including the JVCL) require JCL packages to be compiled' + sLineBreak +
    'Do you want to continue without compiling JCL packages?';
  RsWarningCreatePath = 'The path where %s files will be created doesn''t exists.' + sLineBreak +
    'Do you want the JCL installer to create it?';
  RsErrorCantCreatePath = 'The path %s cannot be created';
  RsWarningAddPathToEnvironment = 'The path where BPL are created must be present in the PATH' + sLineBreak +
    'environment variable, otherwise JCL packages won''t be found by the IDE.' + sLineBreak +
    'Do you want the JCL installer to add it?' + sLineBreak +
    'You will have to reboot your computer and/or to close your session to validate this change';
  RsHtmlHelp2Credentials = 'Registering HTML Help 2.0 files requires administrator privilege to be performed' + sLineBreak +
    'The RegHelper.exe utility will make this operation';

type
  TOptionRec = record
    Id: Integer;
    Caption: string;
    Hint: string;
  end;

var
  OptionData: array[TJclOption] of TOptionRec =
    (
      (Id: -1; Caption: RsCaptionLibrary; Hint: RsHintLibrary), // joLibrary
      (Id: -1; Caption: RsCaptionDef; Hint: RsHintDef), // joDef
      (Id: -1; Caption: RsCaptionDefMath; Hint: RsHintDefMath), // joDefMath
      (Id: -1; Caption: RsCaptionDefDebug; Hint: RsHintDefDebug), // joDefDebug
      (Id: -1; Caption: RsCaptionDefEDI; Hint: RsHintDefEDI), // joDefEDI
      (Id: -1; Caption: RsCaptionDefPCRE; Hint: RsHintDefPCRE), // joDefPCRE
      (Id: -1; Caption: RsCaptionDefBZip2; Hint: RsHintDefBZip2), // joDefBZip2
      (Id: -1; Caption: RsCaptionDefThreadSafe; Hint: RsHintDefThreadSafe), // joDefThreadSafe
      (Id: -1; Caption: RsCaptionDefDropObsoleteCode; Hint: RsHintDefDropObsoleteCode), // joDefDropObsoleteCode
      (Id: -1; Caption: RsCaptionDefUnitVersioning; Hint: RsHintDefUnitVersioning), // joDefUnitVersioning
      (Id: -1; Caption: RsCaptionDefMathPrecSingle; Hint: RsHintDefMathPrecSingle), // ioDefMathPrecSingle
      (Id: -1; Caption: RsCaptionDefMathPrecDouble; Hint: RsHintDefMathPrecDouble), // joDefMathPrecDouble
      (Id: -1; Caption: RsCaptionDefMathPrecExtended; Hint: RsHintDefMathPrecExtended), // joDefMathPrecExtended
      (Id: -1; Caption: RsCaptionDefMathExtremeValues; Hint: RsHintDefMathExtremeValues), // joDefMathExtremeValues
      (Id: -1; Caption: RsCaptionDefHookDllExceptions; Hint: RsHintDefHookDllExceptions), // joDefHookDllExceptions
      (Id: -1; Caption: RsCaptionDefDebugNoBinary; Hint: RsHintDefDebugNoBinary), // joDefDebugNoBinary
      (Id: -1; Caption: RsCaptionDefDebugNoTD32; Hint: RsHintDefDebugNoTD32), // joDefDebugNoTD32
      (Id: -1; Caption: RsCaptionDefDebugNoMap; Hint: RsHintDefDebugNoMap), // joDefDebugNoMap
      (Id: -1; Caption: RsCaptionDefDebugNoExports; Hint: RsHintDefDebugNoExports), // joDefDebugNoExports
      (Id: -1; Caption: RsCaptionDefDebugNoSymbols; Hint: RsHintDefDebugNoSymbols), // joDefDebugNoSymbols
      (Id: -1; Caption: RsCaptionDefEDIWeakPackageUnits; Hint: RsHintDefEDIWeakPackageUnits), // joDefEDIWeakPackageUnits
      (Id: -1; Caption: RsCaptionDefPCREStaticLink; Hint: RsHintDefPCREStaticLink), // joDefPCREStaticLink
      (Id: -1; Caption: RsCaptionDefPCRELinkDLL; Hint: RsHintDefPCRELinkDLL), // joDefPCRELinkDLL
      (Id: -1; Caption: RsCaptionDefPCRELinkOnRequest; Hint: RsHintDefPCRELinkOnRequest), // joDefPCRELinkOnRequest
      (Id: -1; Caption: RsCaptionDefBZip2StaticLink; Hint: RsHintDefBZip2StaticLink), // joDefBZip2StaticLink
      (Id: -1; Caption: RsCaptionDefBZip2LinkDLL; Hint: RsHintDefBZip2LinkDLL), // joDefBZip2LinkDLL
      (Id: -1; Caption: RsCaptionDefBZip2LinkOnRequest; Hint: RsHintDefBZip2LinkOnRequest), // joDefBZip2LinkOnRequest
      (Id: -1; Caption: RsCaptionEnvironment; Hint: RsHintEnvironment), // joEnvironment
      (Id: -1; Caption: RsCaptionEnvLibPath; Hint: RsHintEnvLibPath), // joEnvLibPath
      (Id: -1; Caption: RsCaptionEnvBrowsingPath; Hint: RsHintEnvBrowsingPath), // joEnvBrowsingPath
      (Id: -1; Caption: RsCaptionEnvDebugDCUPath; Hint: RsHintEnvDebugDCUPath), // joEnvDebugDCUPath
      (Id: -1; Caption: RsCaptionMake; Hint: RsHintMake), // joMake
      (Id: -1; Caption: RsCaptionMakeRelease; Hint: RsHintMakeRelease), // joMakeRelease
      (Id: -1; Caption: RsCaptionMakeVClx; Hint: RsHintMakeReleaseVClx), // joMakeReleaseVClx
      (Id: -1; Caption: RsCaptionMakeVCL; Hint: RsHintMakeReleaseVCL), // joMakeReleaseVCL
      (Id: -1; Caption: RsCaptionMakeDebug; Hint: RsHintMakeDebug), // joMakeDebug
      (Id: -1; Caption: RsCaptionMakeVClx; Hint: RsHintMakeDebugVClx), // joMakeDebugVClx
      (Id: -1; Caption: RsCaptionMakeVCL; Hint: RsHintMakeDebugVCL), // joMakeDebugVCL
      (Id: -1; Caption: RsCaptionCopyHppFiles; Hint: RsHintCopyHppFiles), // joCopyHppFiles
      (Id: -1; Caption: RsCaptionCheckHppFiles; Hint: RsHintCheckHppFiles), // joCheckHppFiles
      (Id: -1; Caption: RsCaptionPackages; Hint: RsHintPackages), // joPackages
      (Id: -1; Caption: RsCaptionDualPackages; Hint: RsHintDualPackages), // joDualPackages
      (Id: -1; Caption: RsCaptionCopyPackagesHppFiles; Hint: RsHintCopyPackagesHppFiles), // joCopyPackagesHppFiles
      (Id: -1; Caption: RsCaptionPdbCreate; Hint: RsHintPdbCreate), // joPdbCreate
      (Id: -1; Caption: RsCaptionMapCreate; Hint: RsHintMapCreate), // joMapCreate
      (Id: -1; Caption: RsCaptionJdbgCreate; Hint: RsHintJdbgCreate), // joJdbgCreate
      (Id: -1; Caption: RsCaptionJdbgInsert; Hint: RsHintJdbgInsert), // joJdbgInsert
      (Id: -1; Caption: RsCaptionMapDelete; Hint: RsHintMapDelete), // joMapDelete
      (Id: -1; Caption: RsCaptionExperts; Hint: RsHintExperts), // joExperts
      (Id: -1; Caption: RsCaptionExpertsDsgnPackages; Hint: RsHintExpertsDsgnPackages), // joExpertsDsgnPackages
      (Id: -1; Caption: RsCaptionExpertsDLL; Hint: RsHintExpertsDLL), // joExpertsDLL
      (Id: -1; Caption: RsCaptionExpertDebug; Hint: RsHintExpertDebug), // joExpertDebug
      (Id: -1; Caption: RsCaptionExpertAnalyzer; Hint: RsHintExpertAnalyzer), // joExpertAnalyzer
      (Id: -1; Caption: RsCaptionExpertFavorite; Hint: RsHintExpertFavorite), // joExpertFavorite
      (Id: -1; Caption: RsCaptionExpertRepository; Hint: RsHintExpertRepository), // joExpertRepository
      (Id: -1; Caption: RsCaptionExpertThreadNames; Hint: RsHintExpertThreadNames), // joExpertThreadNames
      (Id: -1; Caption: RsCaptionExpertUses; Hint: RsHintExpertUses), // joExpertUses
      (Id: -1; Caption: RsCaptionExpertSimdView; Hint: RsHintExpertSimdView), // joExpertSimdView
      (Id: -1; Caption: RsCaptionExpertVersionControl; Hint: RsHintExpertVersionControl), // joExpertVersionControl
      (Id: -1; Caption: RsCaptionExceptDlg; Hint: RsHintExceptDlg), // joExceptDlg
      (Id: -1; Caption: RsCaptionExceptDlgVCL; Hint: RsHintExceptDlgVCL), // joExceptDlgVCL
      (Id: -1; Caption: RsCaptionExceptDlgVCLSnd; Hint: RsHintExceptDlgVCLSnd), // joExceptDlgVCLSnd
      (Id: -1; Caption: RsCaptionExceptDlgCLX; Hint: RsHintExceptDlgCLX), // joExceptDlgCLX
      (Id: -1; Caption: RsCaptionHelp; Hint: RsHintHelp), // joHelp
      (Id: -1; Caption: RsCaptionHelpHlp; Hint: RsHintHelpHlp), // joHelpHlp
      (Id: -1; Caption: RsCaptionHelpChm; Hint: RsHintHelpChm), // joHelpChm
      (Id: -1; Caption: RsCaptionHelpHxS; Hint: RsHintHelpHxS), // joHelpHxS
      (Id: -1; Caption: RsCaptionHelpHxSPlugin; Hint: RsHintHelpHxSPlugin), // joHelpHxSPlugin
      (Id: -1; Caption: RsCaptionMakeDemos; Hint: RsHintMakeDemos) // joMakeDemos
    );

const
  {$IFDEF KYLIX}
  VersionDir = '/k%d';
  VersionDirExp = '/k%%d';
  {$ELSE}
  VersionDir = '\%s';
  VersionDirExp = '\%%s';
  {$ENDIF}

  JclDpk     = 'Jcl';
  JclVclDpk  = 'JclVcl';
  JclVClxDpk = 'JclVClx';
  JediJclDpr = 'Jedi.Jcl';

  JclExpertBase           = 'JclBaseExpert';
  JclExpertDebug          = 'JclDebugExpert';
  JclExpertAnalyzer       = 'JclProjectAnalysisExpert';
  JclExpertFavorite       = 'JclFavoriteFoldersExpert';
  JclExpertRepository     = 'JclRepositoryExpert';
  JclExpertThrNames       = 'JclThreadNameExpert';
  JclExpertUses           = 'JclUsesExpert';
  JclExpertSimdView       = 'JclSIMDViewExpert';
  JclExpertVersionControl = 'JclVersionControlExpert';
//  JclExpertBdsExpertDpr   = 'JclBdsExpert';

  ExpertPaths: array[joExperts..joExpertVersionControl] of string =
    (
      JclExpertBase, '', '', JclExpertDebug, JclExpertAnalyzer,
      JclExpertFavorite, JclExpertRepository, JclExpertThrNames,
      JclExpertUses, JclExpertSimdView, JclExpertVersionControl
    );            

  JclSrcDirWindows  = 'windows';
  JclSrcDirUnix     = 'unix';
  JclSrcDirVcl      = 'vcl';
  JclSrcDirCommon   = 'common';
  JclSrcDirVisClx   = 'visclx';

  BCBIncludePath = '%s' + DirSeparator + '%s' + DirSeparator + '$(BCB)' + DirDelimiter + 'include;$(BCB)' + DirDelimiter + 'include' + DirDelimiter + 'vcl';
  {$IFDEF MSWINDOWS}
  BCBObjectPath  = '%s;%s;$(BCB)\Lib\Obj';
  JclSourceDirs: array[0..3] of string = (JclSrcDirCommon, JclSrcDirWindows, JclSrcDirVcl, JclSrcDirVisClx);
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  BCBObjectPath  = BCBIncludePath;
  JclSourceDirs: array[0..2] of string = (JclSrcDirCommon, JclSrcDirUnix, JclSrcDirVisClx);
  {$ENDIF UNIX}

  ExceptDlgPath = 'experts' + DirDelimiter + 'debug' + DirDelimiter + 'dialog' + DirDelimiter;
  ExceptDlgClxFileName    = 'ClxExceptDlg.pas';
  ExceptDlgVclFileName    = 'ExceptDlg.pas';
  ExceptDlgVclSndFileName = 'ExceptDlgMail.pas';

  ExceptDlgClxName    = 'CLX Exception Dialog';
  ExceptDlgVclName    = 'Exception Dialog';
  ExceptDlgVclSndName = 'Exception Dialog with Send';

  ExceptDlgDescription = 'JCL Application exception dialog';
  ExceptDlgAuthor      = 'Project JEDI';
  ExceptDlgPage        = 'Dialogs';

  JclChmHelpFile    = 'help' + DirDelimiter + 'JCLHelp.chm';
  JclHlpHelpFile    = 'help' + DirDelimiter + 'JCLHelp.hlp';
  JclHxSHelpFile    = 'help' + DirDelimiter + 'JCLHelp.HxS';

  Help2NameSpace         = 'Jedi.Jcl';
  Help2Collection        = 'JCLHelp_COL_MASTER.HxC';
  Help2Description       = 'JEDI Code Library';
  Help2Identifier        = 'JCLHelp';
  Help2LangId            = 1033;         // en/english
  Help2HxSFile           = 'JCLHelp.HxS';
  Help2HxIFile           = 'JCLHelp.HxI';

  JclHelpTitle      = 'JCL %d.%d Help';
  JclHelpIndexName  = 'JEDI Code Library Reference';
  HHFileName        = 'HH.EXE';

  {$IFDEF VisualCLX}
  ReadmeFileName = 'Readme.html';
  {$ELSE}
  ReadmeFileName = 'Readme.txt';
  {$ENDIF}

  DailyRevisionFileName = 'daily_revision.log';
  EntriesFileName1      = '.svn' + DirDelimiter + 'entries';
  EntriesFileName2      = '_svn' + DirDelimiter + 'entries';

  RsJclVersionMask     = 'JCL %d.%d %s %s %d';
  RsJclVersionBuild    = 'Build';
  RsJclVersionRevision = 'Revision';
  RsJclVersionTesting  = 'Testing';
  RsJclVersionRelease  = 'Release';

  {$IFDEF MSWINDOWS}
  Bcb2MakTemplate = 'packages\BCB.bmk';
  {$ENDIF MSWINDOWS}
  {$IFDEF KYLIX}
  Bcb2MakTemplate = 'packages/bcb.gmk';
  {$ENDIF KYLIX}

  PathEnvironmentVar = 'PATH';
  RegHKCUEnvironmentVar = 'Environment';
  RegHKLMEnvironmentVar = 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment';

resourcestring
  RsInstallMessage                   = 'Installing %s...';
  //RsStatusDetailMessage             = 'Installing %s for %s...';
  RsUninstallMessage                = 'Removing %s...';
  RsBuildingMessage                 = 'Building %s...';
  //RsBuildingDemosMessage            = 'Building demo projects...';
  //RsBuildingDemosByTargetMessage    = 'Building demo projects by %s...';
  RsCompilingMessage                = 'Compiling %s...';
  //RsInstallFailed                   = 'Installation of %s failed, see %s for details.';
  RsInvalidBplPath                  = 'Invalid BPL path "%s"';
  RsInvalidDcpPath                  = 'Invalid DCP path "%s"';
  RsLibDescriptor                   = '%s library %sunits for %s';

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

//=== { TJclInstallation } ===================================================

constructor TJclInstallation.Create(JclDistribution: TJclDistribution;
  InstallTarget: TJclBorRADToolInstallation; const ACLRVersion: string;
  ATargetPlatform: TJclBorPlatform; AGUIPage: IJediInstallPage);
begin
  inherited Create;

  FTarget := InstallTarget;
  if not Target.Valid then
    Abort;

  FDistribution := JclDistribution;
  FCLRVersion := ACLRVersion;
  FTargetPlatform := ATargetPlatform;
  FTargetName := Target.Name;
  if CLRVersion <> '' then
    FTargetName := Format('%s CLR %s', [FTargetName, CLRVersion]);

  case TargetPlatform of
    //bp32bit:
    //  begin
    //    FTargetName := Format('%s %s', [FTargetName, Personality32Bit]);
    //    LibDirMask := LibDirMask + '.x86';
    //  end;
    bp64bit:
      begin
        FTargetName := Format('%s %s', [FTargetName, Personality64Bit]);
      end;
  end;

  FLibReleaseDir := MakePath(Distribution.LibReleaseDirMask);
  FLibDebugDir := MakePath(Distribution.LibDebugDirMask);
  FJclDcpPath := PathAddSeparator(MakePath(Distribution.LibReleaseDirMask)); // packages are release

  FDemoSectionName := Target.Name + ' demos';
  FLogFileName := Format('%sbin%s%s.log', [Distribution.JclPath, DirDelimiter, TargetName]);
  FLogLines := TJclSimpleLog.Create(FLogFileName);
end;

destructor TJclInstallation.Destroy;
begin
  FDemoList.Free;
  FLogLines.Free;
  FGUI := nil;
  FGUIPage := nil;

  inherited Destroy;
end;

function TJclInstallation.GetEnabled: Boolean;
begin
  Result := OptionCheckedById[OptionData[joLibrary].Id];
end;

function TJclInstallation.GetOptionChecked(Option: TJclOption): Boolean;
begin
  Result := OptionCheckedById[OptionData[Option].Id];
end;

function TJclInstallation.GetOptionCheckedById(Id: Integer): Boolean;
var
  AConfiguration: IJediConfiguration;
begin
  if Assigned(GUIPage) then
    Result := GUIPage.OptionChecked[Id]
  else
  begin
    AConfiguration := InstallCore.Configuration;
    if Assigned(AConfiguration) then
      Result := AConfiguration.OptionAsBool[TargetName, Id]
    else
      Result := False;
  end;
end;

procedure TJclInstallation.MarkOptionBegin(Id: Integer);
begin
  if Assigned(GUIPage) then
    GUIPage.MarkOptionBegin(Id);
  if Assigned(GUI) then
    GUI.Status := InstallCore.InstallOptionName[Id];
end;

procedure TJclInstallation.MarkOptionBegin(Option: TJclOption);
begin
  if Assigned(GUIPage) then
    GUIPage.MarkOptionBegin(OptionData[Option].Id);
  if Assigned(GUI) then
    GUI.Status := OptionData[Option].Hint;
end;

procedure TJclInstallation.MarkOptionEnd(Id: Integer; Success: Boolean);
begin
  if Assigned(GUIPage) then
  begin
    GUIPage.MarkOptionEnd(Id, not Success);
    if Assigned(GUI) then
      GUI.Progress := Round(100 * (Distribution.NbInstalled + GUIPage.Progress / 100) / Distribution.NbEnabled);
  end;
end;

procedure TJclInstallation.MarkOptionEnd(Option: TJclOption; Success: Boolean);
begin
  if Assigned(GUIPage) then
  begin
    GUIPage.MarkOptionEnd(OptionData[Option].Id, not Success);
    if Assigned(GUI) then
      GUI.Progress := Round(100 * (Distribution.NbInstalled + GUIPage.Progress / 100) / Distribution.NbEnabled);
  end;
end;

procedure TJclInstallation.Init;
  procedure AddOption(Option: TJclOption; GUIOptions: TJediInstallGUIOptions;
    Parent: Integer; const Caption, Hint: string); overload;
  begin
    GUIPage.AddInstallOption(OptionData[Option].Id, GUIOptions, Caption, Hint, Parent);
  end;

  procedure AddOption(Option: TJclOption; GUIOptions: TJediInstallGUIOptions;
    Parent: Integer); overload;
  begin
    AddOption(Option, GUIOptions, Parent, OptionData[Option].Caption, OptionData[Option].Hint);
  end;

  procedure AddOption(Option: TJclOption; GUIOptions: TJediInstallGUIOptions;
    Parent: TJclOption); overload;
  begin
    AddOption(Option, GUIOptions, OptionData[Parent].Id, OptionData[Option].Caption, OptionData[Option].Hint);
  end;

  procedure AddDefOptions(Parent: TJclOption);
  begin
    AddOption(joDefThreadSafe, [goChecked], Parent);
    AddOption(joDefDropObsoleteCode, [goChecked], Parent);
    if CLRVersion = '' then
      AddOption(joDefUnitVersioning, [goChecked], Parent);
    AddOption(joDefMath, [goChecked], Parent);
    AddOption(joDefMathPrecSingle, [goRadioButton], joDefMath);
    AddOption(joDefMathPrecDouble, [goRadioButton], joDefMath);
    AddOption(joDefMathPrecExtended, [goRadioButton, goChecked], joDefMath);
    AddOption(joDefMathExtremeValues, [goChecked], joDefMath);
    if CLRVersion = '' then   // these units are not CLR compliant
    begin
      {$IFDEF MSWINDOWS}
      // debug options
      AddOption(joDefDebug, [goNoAutoCheck], Parent);
      AddOption(joDefHookDllExceptions, [goNoAutoCheck], joDefDebug);
      AddOption(joDefDebugNoBinary, [goNoAutoCheck], joDefDebug);
      AddOption(joDefDebugNoTD32, [goNoAutoCheck], joDefDebug);
      AddOption(joDefDebugNoMap, [goNoAutoCheck], joDefDebug);
      AddOption(joDefDebugNoExports, [goNoAutoCheck], joDefDebug);
      AddOption(joDefDebugNoSymbols, [goNoAutoCheck], joDefDebug);
      {$ENDIF MSWINDOWS}
      // EDI options
      AddOption(joDefEDI, [goNoAutoCheck], Parent);
      AddOption(joDefEDIWeakPackageUnits, [goNoAutoCheck], joDefEDI);
      // PCRE options
      AddOption(joDefPCRE, [goChecked], Parent);
      if Target.RadToolKind = brBorlandDevStudio then
      begin
        AddOption(joDefPCREStaticLink, [goRadioButton, goChecked], joDefPCRE);
        AddOption(joDefPCRELinkOnRequest, [goRadioButton], joDefPCRE);
      end
      else
        AddOption(joDefPCRELinkOnRequest, [goRadioButton, goChecked], joDefPCRE);
      AddOption(joDefPCRELinkDLL, [goRadioButton], joDefPCRE);
      // BZip2 options
      AddOption(joDefBZip2, [goChecked], Parent);
      {$IFDEF MSWINDOWS}
      AddOption(joDefBZip2StaticLink, [goRadioButton], joDefBZip2);
      {$ENDIF MSWINDOWS}
      AddOption(joDefBZip2LinkOnRequest, [goRadioButton, goChecked], joDefBZip2);
      AddOption(joDefBZip2LinkDLL, [goRadioButton], joDefBZip2);
    end;
  end;

  procedure AddEnvOptions(Parent: TJclOption);
  begin
    AddOption(joEnvLibPath, [goChecked], Parent);
    AddOption(joEnvBrowsingPath, [goChecked], Parent);
    if not Target.IsTurboExplorer then
      AddOption(joEnvDebugDCUPath, [goChecked], Parent);
  end;

  procedure AddMakeOptions(Parent: TJclOption);
  begin
    AddOption(joMakeRelease, [goStandAloneParent, goExpandable, goChecked], Parent);
    AddOption(joMakeDebug, [goStandAloneParent, goExpandable, goChecked], Parent);

    if CLRVersion = '' then
    begin
      if Target.SupportsVisualCLX then
      begin
        AddOption(joMakeReleaseVClx, [goChecked], joMakeRelease);
        AddOption(joMakeDebugVClx, [goChecked], joMakeDebug);
      end;

      if Target.SupportsVCL then
      begin
        AddOption(joMakeReleaseVCL, [goChecked], joMakeRelease);
        AddOption(joMakeDebugVCL, [goChecked], joMakeDebug);
      end;

      if bpBCBuilder32 in Target.Personalities then
      begin
        AddOption(joCopyHppFiles, [goChecked], OptionData[joMake].Id,
          Format(OptionData[joCopyHppFiles].Caption, [Target.VclIncludeDir]),
          OptionData[joCopyHppFiles].Hint);
        AddOption(joCheckHppFiles, [goChecked], joMake);
      end;
    end;
  end;

  procedure AddHelpOptions(Parent: TJclOption);
  begin
    {$IFDEF MSWINDOWS}
    if Target.RadToolKind = brBorlandDevStudio then
    begin
      // TODO: expert help
      if (Target.VersionNumber >= 3) and (Distribution.JclHxSHelpFileName <> '') then
      begin
        AddOption(joHelp, [goChecked], Parent);
        AddOption(johelpHxS, [goStandaloneParent,goChecked], joHelp);
        AddOption(joHelpHxSPlugin, [goNoAutoCheck], joHelpHxS);
      end;
    end
    else
    begin
      if (Distribution.JclHlpHelpFileName <> '') or (Distribution.JclChmHelpFileName <> '') then
      begin
        AddOption(joHelp, [goChecked], Parent);
        if Distribution.JclHlpHelpFileName <> '' then
          AddOption(joHelpHlp, [goChecked], joHelp);
        if Distribution.JclChmHelpFileName <> '' then
          AddOption(joHelpChm, [goChecked], joHelp);
      end;
    end;
    {$ENDIF MSWINDOWS}
  end;

  procedure AddRepositoryOptions(Parent: TJclOption);
  begin
    // BDS has an expert for objects in the repository
    if Target.RadToolKind <> brBorlandDevStudio then
    begin
      AddOption(joExceptDlg, [], Parent);
      if Target.SupportsVCL then
      begin
        AddOption(joExceptDlgVCL, [], joExceptDlg);
        {$IFDEF MSWINDOWS}
        AddOption(joExceptDlgVCLSnd, [], joExceptDlg);
        {$ENDIF MSWINDOWS}
      end;
      if Target.SupportsVisualCLX then
        AddOption(joExceptDlgCLX, [], joExceptDlg);
    end;
  end;

  procedure AddPackageOptions(Parent: TJclOption; RuntimeInstallation: Boolean);
  begin
    if (bpBCBuilder32 in Target.Personalities) and RunTimeInstallation and (CLRVersion = '') then
    begin
      if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 4) then
      begin
        AddOption(joDualPackages, [goStandAloneParent, goChecked], OptionData[Parent].Id,
          Format(OptionData[joCopyPackagesHppFiles].Caption, [Target.VclIncludeDir]),
          OptionData[joCopyPackagesHppFiles].Hint);
        AddOption(joCopyPackagesHppFiles, [goChecked], joDualPackages);
      end
      else
        AddOption(joCopyPackagesHppFiles, [goChecked], OptionData[Parent].Id,
          Format(OptionData[joCopyPackagesHppFiles].Caption, [Target.VclIncludeDir]),
          OptionData[joCopyPackagesHppFiles].Hint);
    end;

    if CLRVersion = '' then
    begin
      AddOption(joMapCreate, [goExpandable, goStandaloneParent, goNoAutoCheck], Parent);

      {$IFDEF MSWINDOWS}
      AddOption(joJdbgCreate, [goExpandable, goStandaloneParent], joMapCreate);
      AddOption(joJdbgInsert, [goNoAutoCheck], joMapCreate);
      AddOption(joMapDelete, [goNoAutoCheck], joMapCreate);

      {if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber = 3)
        and (Target.Edition = deStd) then
        CopyFakeXmlRtlPackage;
      TODO: CopyFakeXmlRtlPackage
      }
      {$ENDIF MSWINDOWS}
    end
    else // CLRVersion <> ''
      AddOption(joPdbCreate, [goNoAutoCheck], Parent);
  end;

  procedure AddExpertOptions(Parent: TJclOption; RuntimeInstallation: Boolean);
  {$IFDEF MSWINDOWS}
  var
    ExpertOptions: TJediInstallGUIOptions;
  {$ENDIF MSWINDOWS}
  begin
    // TODO :
    //  It has been reported that IDE experts don't work under Win98.
    //  Leave these options unchecked for Win9x/WinME until that has been examined.
    {$IFDEF MSWINDOWS}
    if IsWinNT then
      ExpertOptions := [goChecked]
    else
      ExpertOptions := [];

    AddOption(joExperts, [goExpandable, goChecked], Parent);

    if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber <= 2) then
      // design packages are not loaded by C#Builder 1 and Delphi 8
      AddOption(joExpertsDLL, [goRadioButton, goChecked], joExperts)
    else if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 3) then
      // expert DLLs are unstable on Delphi 2005 and BDS 2006
      // (problems while adding menu items in menu not loaded yet)
      AddOption(joExpertsDsgnPackages, [goRadioButton, goChecked], joExperts)
    else
    begin
      AddOption(joExpertsDLL, [goRadioButton, goChecked], joExperts);
      AddOption(joExpertsDsgnPackages, [goRadioButton], joExperts);
    end;

    if RunTimeInstallation then
    begin
      AddOption(joExpertDebug, ExpertOptions, joExperts);
      AddOption(joExpertAnalyzer, ExpertOptions, joExperts);
      if Target.RadToolKind <> brBorlandDevStudio then
        AddOption(joExpertUses, ExpertOptions, joExperts);
      AddOption(joExpertSimdView, ExpertOptions, joExperts);
      AddOption(joExpertRepository, ExpertOptions, joExperts);
    end;
    AddOption(joExpertFavorite, ExpertOptions, joExperts);
    AddOption(joExpertVersionControl, [goNoAutoCheck], joExperts);
    if (Target.RadToolKind <> brBorlandDevStudio) and (Target.VersionNumber <= 6) then
      AddOption(joExpertThreadNames, ExpertOptions, joExperts);
    {$ENDIF MSWINDOWS}
  end;

  procedure AddDemoNodes;
  var
    I: Integer;
    ADemoList: TStrings;
    DemoOption: Integer;
    FileName: string;
  begin
    AddOption(joMakeDemos, [goNoAutoCheck], joLibrary);
    ADemoList := GetDemoList;
    for I := 0 to ADemoList.Count - 1 do
    begin
      FileName := ExtractRelativePath(Distribution.JclExamplesDir, ADemoList.Strings[I]);
      DemoOption := InstallCore.AddInstallOption(FileName);
      ADemoList.Objects[I] := TObject(DemoOption);
      GUIPage.AddInstallOption(DemoOption, [], ExtractFileName(FileName), FileName, OptionData[joMakeDemos].Id);
    end;
  end;

  procedure LoadValues;
  var
    AConfiguration: IJediConfiguration;
    Option: TJclOption;
    Id, Index: Integer;
    StoredValue: string;
    ADemoList: TStrings;
  begin
    AConfiguration := InstallCore.Configuration;
    if not Assigned(AConfiguration) then
      Exit;
    if AConfiguration.SectionExists(TargetName) then
      for Option := Low(TJclOption) to High(TJclOption) do
    begin
      Id := OptionData[Option].Id;
      GUIPage.OptionChecked[Id] := AConfiguration.OptionAsBool[TargetName, Id];
    end;

    if not Target.IsTurboExplorer then
    begin
      ADemoList := GetDemoList;
      if AConfiguration.SectionExists(FDemoSectionName) then
        for Index := 0 to ADemoList.Count - 1 do
      begin
        Id := Integer(ADemoList.Objects[Index]);
        GUIPage.OptionChecked[Id] := AConfiguration.OptionAsBool[FDemoSectionName, Id];
      end;

      StoredValue := AConfiguration.OptionAsStringByName[TargetName, RsNameBPLPath];
      if StoredValue = '' then
        StoredValue := Target.BPLOutputPath;
      GUIPage.Directories[FGUIBPLPathIndex] := StoredValue;
      if Target.RadToolKind = brCppBuilder then
        StoredValue := AConfiguration.OptionAsStringByName[TargetName, RsNameBPIPath]
      else
        StoredValue := AConfiguration.OptionAsStringByName[TargetName, RsNameDCPPath];
      if StoredValue = '' then
        StoredValue := FJclDcpPath;
      GUIPage.Directories[FGUIDCPPathIndex] := StoredValue;
    end;
  end;

var
  RunTimeInstallation: Boolean;
begin
  FGUI := InstallCore.InstallGUI;
  if not Assigned(GUI) then
    Exit;

  FGUIPage := GUI.CreateInstallPage;
  GUIPage.Caption := TargetName;
  GUIPage.SetIcon(Target.IdeExeFileName);

  RunTimeInstallation := (Target.RadToolKind <> brBorlandDevStudio)
    or ((Target.VersionNumber >= 3) and (bpDelphi32 in Target.Personalities));

  AddOption(joLibrary, [goExpandable, goChecked], JediTargetOption);

  if RunTimeInstallation then
  begin
    // conditional defines
    AddOption(joDef, [goExpandable, goChecked], OptionData[joLibrary].Id);
    AddDefOptions(joDef);

    if CLRVersion = '' then
    begin
      AddOption(joEnvironment, [goExpandable, goChecked], OptionData[joLibrary].Id);
      AddEnvOptions(joEnvironment);
    end;

    if not Target.IsTurboExplorer then
    begin
      AddOption(joMake, [goExpandable, goChecked], OptionData[joLibrary].Id);
      AddMakeOptions(joMake);
    end;

    if CLRVersion = '' then
    begin
      AddHelpOptions(joLibrary);
      AddRepositoryOptions(joLibrary);
    end;
  end;

  if not Target.IsTurboExplorer then
  begin
    AddOption(joPackages, [goStandAloneParent, goExpandable, goChecked], joLibrary);
    AddPackageOptions(joPackages, RuntimeInstallation);

    if CLRVersion = '' then
    begin
      {$IFDEF MSWINDOWS}
      AddExpertOptions(joPackages, RunTimeInstallation);
      {$ENDIF MSWINDOWS}
      if RunTimeInstallation then
        AddDemoNodes;
    end;
  end;

  GUIPage.InitDisplay;

  if not Target.IsTurboExplorer then
  begin
    if (CLRVersion = '') then
    begin
      FGUIBPLPathIndex := GUIPage.AddDirectory(RsCaptionBPLPath);
      if Target.RadToolKind = brCppBuilder then
        FGUIDCPPathIndex := GUIPage.AddDirectory(RsCaptionBPIPath)
      else
        FGUIDCPPathIndex := GUIPage.AddDirectory(RsCaptionDCPPath);
    end
    else
      FGUIBPLPathIndex := GUIPage.AddDirectory(RsCaptionOutputPath);
  end;

  LoadValues;
end;

function TJclInstallation.Install: Boolean;

  procedure WriteIntroduction;
  var
    Personality: TJclBorPersonality;
  begin
    WriteLog(Distribution.Version);
    WriteLog('');
    WriteLog(StrPadRight(TargetName, 44, '='));
    WriteLog('');
    WriteLog('Installed personalities :');
    for Personality := Low(TJclBorPersonality) to High(TJclBorPersonality) do
      if Personality in Target.Personalities then
    begin
      WriteLog(JclBorPersonalityDescription[Personality]);
    end;
    WriteLog(StrRepeat('=', 44));
  end;

  function CheckDirectories: Boolean;
  {$IFDEF MSWINDOWS}
  var
    PathEnvVar: string;
  {$ENDIF MSWINDOWS}
  begin
    Result := not OptionChecked[joPackages];
    if not Result then
    begin
      Result := True;
      if not DirectoryExists(GetBplPath) then
      begin
        Result := False;
        if not Assigned(GUI) then
          WriteLog(Format(RsInvalidBplPath, [GetBplPath]))
        else if GUI.Dialog(Format(RsWarningCreatePath, ['BPL']), dtWarning, [drYes, drNo]) = drYes then
        begin
          Result := ForceDirectories(GetBplPath);
          if not Result then
            GUI.Dialog(Format(RsErrorCantCreatePath, [GetBplPath]), dtError, [drCancel]);
        end;
      end;
      if (CLRVersion = '') and not DirectoryExists(GetDcpPath) then
      begin
        Result := False;
        if not Assigned(GUI) then
          WriteLog(Format(RsInvalidDcpPath, [GetDcpPath]))
        else if GUI.Dialog(Format(RsWarningCreatePath, ['DCP']), dtWarning, [drYes, drNo]) = drYes then
        begin
          Result := ForceDirectories(GetDcpPath);
          if not Result then
            GUI.Dialog(Format(RsErrorCantCreatePath, [GetDcpPath]), dtError, [drCancel]);
        end;
      end;
      {$IFDEF MSWINDOWS}
      if CLRVersion = '' then
      begin
        PathEnvVar := RegReadStringDef(HKCU, RegHKCUEnvironmentVar, PathEnvironmentVar, '');
        PathListIncludeItems(PathEnvVar, RegReadStringDef(HKLM, RegHKLMEnvironmentVar, PathEnvironmentVar, ''));
        ExpandEnvironmentVar(PathEnvVar);
        if (PathListItemIndex(PathEnvVar, GetBplPath) = -1) and (PathListItemIndex(PathEnvVar, PathAddSeparator(GetBplPath)) = -1)
          and Assigned(GUI) and (GUI.Dialog(RsWarningAddPathToEnvironment, dtWarning, [drYes, drNo]) = drYes) then
        begin
          PathEnvVar := RegReadStringDef(HKCU, RegHKCUEnvironmentVar, PathEnvironmentVar, '');
          PathListIncludeItems(PathEnvVar, GetBplPath);
          RegWriteString(HKCU, RegHKCUEnvironmentVar, PathEnvironmentVar, PathEnvVar);
        end;
      end;
      {$ENDIF MSWINDOWS}
    end
    else if Assigned(GUI) and (CLRVersion = '') and not Target.IsTurboExplorer then
      Result := GUI.Dialog(RsWarningPackageNodeNotSelected, dtConfirmation, [drYes, drNo]) = drYes;
  end;

  function SetStaticOptions: Boolean;

    function SaveDefines(Defines: TStrings): Boolean;
    var
      TemplateFileName, IncludeFileName, IncludeLine, Symbol, CLRSuffix: string;
      IncludeFile: TStrings;
      IndexLine, DefinePos, SymbolEnd: Integer;
      Defined, NotDefined: Boolean;
    const
      DefineText = '$DEFINE';
      NotDefineText = '.' + DefineText;
    begin
      WriteLog('Saving conditional defines...');
      Result := True;
      if CLRVersion = '' then
        CLRSuffix := ''
      else
        CLRSuffix := '.net';
      TemplateFileName := PathAddSeparator(Distribution.JclSourceDir) + 'jcl.template.inc';
      IncludeFileName := Format('%sjcl%s%s.inc', [PathAddSeparator(Distribution.JclSourceDir), Target.IDEVersionNumberStr, CLRSuffix]);
      try
        IncludeFile := TStringList.Create;
        try
          IncludeFile.LoadFromFile(TemplateFileName);
          WriteLog(Format('Loaded template for include file %s', [TemplateFileName]));
    
          for IndexLine := 0 to IncludeFile.Count - 1 do
          begin
            IncludeLine := IncludeFile.Strings[IndexLine];
            DefinePos := AnsiPos(DefineText, UpperCase(IncludeLine));
            if DefinePos > 1 then
            begin
              Defined := IncludeLine[DefinePos - 1] = '{';
              NotDefined := IncludeLine[DefinePos - 1] = '.';
              if Defined or NotDefined then
              begin
                Inc(DefinePos, Length(DefineText));
                while IncludeLine[DefinePos] in AnsiWhiteSpace do
                  Inc(DefinePos);
                SymbolEnd := DefinePos;
                while IncludeLine[SymbolEnd] in AnsiValidIdentifierLetters do
                  Inc(SymbolEnd);
                Symbol := Copy(IncludeLine, DefinePos, SymbolEnd - DefinePos);
                DefinePos := Defines.IndexOf(Symbol);

                if (DefinePos >= 0) and NotDefined then
                  IncludeLine := StringReplace(IncludeLine, NotDefineText, DefineText, [rfIgnoreCase]);
                if (DefinePos < 0) and Defined then
                  IncludeLine := StringReplace(IncludeLine, DefineText, NotDefineText, [rfIgnoreCase]);
    
                IncludeFile.Strings[IndexLine] := IncludeLine;
              end;
            end;
          end;
          IncludeFile.SaveToFile(IncludeFileName);
          WriteLog(Format('Saved include file %s', [IncludeFileName]));
        finally
          IncludeFile.Free;
        end;
      except
        Result := False;
      end;
    end;

  const
    DefineNames: array [joDefThreadSafe..joDefBZip2LinkOnRequest] of string =
      ( 'THREADSAFE', 'DROP_OBSOLETE_CODE', 'UNITVERSIONING',
        'MATH_SINGLE_PRECISION', 'MATH_DOUBLE_PRECISION', 'MATH_EXTENDED_PRECISION',
        'MATH_EXT_EXTREMEVALUES',  'HOOK_DLL_EXCEPTIONS',
        'DEBUG_NO_BINARY', 'DEBUG_NO_TD32', 'DEBUG_NO_MAP', 'DEBUG_NO_EXPORTS',
        'DEBUG_NO_SYMBOLS', 'EDI_WEAK_PACKAGE_UNITS', 'PCRE_STATICLINK',
        'PCRE_LINKDLL', 'PCRE_LINKONREQUEST', 'BZIP2_STATICLINK',
        'BZIP2_LINKDLL', 'BZIP2_LINKONREQUEST' );
  var
    Option: TJclOption;
    Defines: TStrings;
  begin
    Defines := TStringList.Create;
    try
      if OptionChecked[joDef] then
      begin
        MarkOptionBegin(joDef);
        for Option := Low(DefineNames) to High(DefineNames) do
          if OptionChecked[Option] then
        begin
          MarkOptionBegin(Option);
          Defines.Add(DefineNames[Option]);
          MarkOptionEnd(Option, True);
        end;
        MarkOptionEnd(joDef, True);
      end;
      MarkOptionBegin(joMapCreate);
      Target.MapCreate := OptionChecked[joMapCreate];
      MarkOptionEnd(joMapCreate, True);
      {$IFDEF MSWINDOWS}
      MarkOptionBegin(joJdbgCreate);
      Target.JdbgCreate := OptionChecked[joJdbgCreate];
      MarkOptionEnd(joJdbgCreate, True);
      MarkOptionBegin(joJdbgInsert);
      Target.JdbgInsert := OptionChecked[joJdbgInsert];
      MarkOptionEnd(joJdbgInsert, True);
      MarkOptionBegin(joMapDelete);
      Target.MapDelete := OptionChecked[joMapDelete];
      MarkOptionEnd(joMapDelete, True);
      if Target is TJclBDSInstallation then
      begin
        MarkOptionBegin(joDualPackages);
        TJclBDSInstallation(Target).DualPackageInstallation := OptionChecked[joDualPackages];
        MarkOptionEnd(joDualPackages, True);
        MarkOptionBegin(joPdbCreate);
        TJclBDSInstallation(Target).PdbCreate := OptionChecked[joPdbCreate];
        MarkOptionEnd(joPdbCreate, True);
      end;
      {$ENDIF MSWINDOWS}

      // no conditional defines for C#Builder 1 and Delphi 8
      Result := ((Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber <= 2)) or SaveDefines(Defines);
    finally
      Defines.Free;
    end;
  end;

  function SetEnvironment: Boolean;
  begin
    Result := True;
    if OptionChecked[joEnvironment] then
    begin
      MarkOptionBegin(joEnvironment);
  
      if OptionChecked[joEnvLibPath] then
      begin
        MarkOptionBegin(joEnvLibPath);
        Result := Target.AddToLibrarySearchPath(FLibReleaseDir) and Target.AddToLibrarySearchPath(Distribution.JclSourceDir);
        if Result then
        begin
          WriteLog(Format('Added "%s;%s" to library search path.', [FLibReleaseDir, Distribution.JclSourceDir]));
          {$IFDEF MSWINDOWS}
          if (Target.RadToolKind = brBorlandDevStudio) and (bpBCBuilder32 in Target.Personalities)
            and OptionChecked[joDualPackages] then
            with TJclBDSInstallation(Target) do
          begin
            Result := AddToCppSearchPath(FLibReleaseDir) and AddToCppSearchPath(Distribution.JclSourceDir) and
                      ((IDEVersionNumber < 5) or AddToCppLibraryPath(FLibReleaseDir));
            if Result then
              WriteLog(Format('Added "%s;%s" to cpp search path.', [FLibReleaseDir, Distribution.JclSourceDir]))
            else
              WriteLog('Failed to add cpp search paths.');
          end;
          {$ENDIF MSWINDOWS}
          if Target.IsTurboExplorer then
          begin
            Result := Target.AddToLibrarySearchPath(Distribution.JclSourcePath);
            if Result then
              WriteLog(Format('Added "%s" to library search path.', [Distribution.JclSourcePath]))
            else
              WriteLog('Failed to add library search paths.');
          end;
        end
        else
          WriteLog('Failed to add library search paths.');
        MarkOptionEnd(joEnvLibPath, Result);
      end;
  
      if Result and OptionChecked[joEnvBrowsingPath] then
      begin
        MarkOptionBegin(joEnvBrowsingPath);
        if Result then
        begin
          Result := Target.AddToLibraryBrowsingPath(Distribution.JclSourcePath);
          if Result then
          begin
            WriteLog(Format('Added "%s" to library browsing path.', [Distribution.JclSourcePath]));
            {$IFDEF MSWINDOWS}
            if (Target.RadToolKind = brBorlandDevStudio) and (bpBCBuilder32 in Target.Personalities)
              and OptionChecked[joDualPackages] then
              with TJclBDSInstallation(Target) do
            begin
              Result := AddToCppBrowsingPath(Distribution.JclSourcePath);
              if Result then
                WriteLog(Format('Added "%s" to cpp browsing path.', [Distribution.JclSourcePath]))
              else
                WriteLog('Failed to add cpp browsing paths.');
            end;
            {$ENDIF MSWINDOWS}
          end
          else
            WriteLog('Failed to add library browsing path');
        end
        else
          WriteLog('Failed to add library browsing path.');
        MarkOptionEnd(joEnvBrowsingPath, Result);
      end;
  
      if Result and OptionChecked[joEnvDebugDCUPath] then
      begin
        MarkOptionBegin(joEnvDebugDCUPath);
        Result := Target.AddToDebugDCUPath(FLibDebugDir);
        if Result then
          WriteLog(Format('Added "%s" to Debug DCU Path.', [FLibDebugDir]))
        else
          WriteLog('Failed to add debug DCU path');
        MarkOptionEnd(joEnvDebugDCUPath, Result);
      end;

      MarkOptionEnd(joEnvironment, Result);
    end;
  end;

  function MakeUnits: Boolean;
    function CheckHppFiles: Boolean;
    var
      SaveDir, Options: string;
    begin
      SaveDir := GetCurrentDir;
      SetCurrentDir(Distribution.JclPath + 'install\HeaderTest');
      try
        Target.BCC32.Options.Clear;
        Target.BCC32.Options.Add('-c'); // compile only
        Target.BCC32.Options.Add('-Ve'); // compatibility
        Target.BCC32.Options.Add('-X'); // no autodependencies
        Target.BCC32.Options.Add('-a8'); // data alignment
        Target.BCC32.Options.Add('-b'); // enum to be at least 4 bytes
        Target.BCC32.Options.Add('-k-'); // no standard stack frame
        Target.BCC32.Options.Add('-tWM'); // code format
        Target.BCC32.Options.Add('-w-par'); // warning
        Target.BCC32.Options.Add('-w-aus'); // warning
        Target.BCC32.AddPathOption('I', Format('%sinclude;%s%sinclude;%s', [Distribution.JclPath, Target.RootDir, DirDelimiter, Target.VclIncludeDir]));
        Target.BCC32.Options.Add('-DTEST_COMMON');
        {$IFDEF MSWINDOWS}
        Target.BCC32.Options.Add('-DTEST_WINDOWS');
        {$ENDIF MSWINDOWS}
        {$IFDEF UNIX}
        Target.BCC32.Options.Add('-DTEST_UNIX');
        {$ENDIF UNIX}
        if OptionChecked[joMakeReleaseVCL] or OptionChecked[joMakeDebugVCL] then
          Target.BCC32.Options.Add('-DTEST_VCL');
        if OptionChecked[joMakeReleaseVClx] or OptionChecked[joMakeDebugVClx] then
          Target.BCC32.Options.Add('-DTEST_VISCLX');
        Options := StringsToStr(Target.BCC32.Options, AnsiSpace);
        Result := Target.BCC32.Execute(Options + ' "jcl_a2z.cpp"')
          and Target.BCC32.Execute(Options + ' "jcl_z2a.cpp"'); 
      finally
        SetCurrentDir(SaveDir);
      end;
    end;
  var
    I: Integer;
  begin
    Result := True;
    if OptionChecked[joMake] then
    begin
      MarkOptionBegin(joMake);

      if OptionChecked[joMakeRelease] then
      begin
        MarkOptionBegin(joMakeRelease);

        for I := Low(JclSourceDirs) to High(JclSourceDirs) do
        begin
          if (JclSourceDirs[I] = JclSrcDirVisClx) then
          begin
            if OptionChecked[joMakeReleaseVClx] then
              MarkOptionBegin(joMakeReleaseVClx)
            else
              Continue;
          end;
          if (JclSourceDirs[I] = JclSrcDirVcl) then
          begin
            if OptionChecked[joMakeReleaseVCL] or
              ((Target.VersionNumber <= 5) and (Target.RadToolKind <> brBorlandDevStudio)) then
              MarkOptionBegin(joMakeReleaseVCL)
            else
              Continue;
          end;
          Result := Result and CompileLibraryUnits(JclSourceDirs[I], False);
          if (JclSourceDirs[I] = JclSrcDirVisClx) then
            MarkOptionEnd(joMakeReleaseVClx, Result);
          if (JclSourceDirs[I] = JclSrcDirVcl) then
            MarkOptionEnd(joMakeReleaseVCL, Result);
        end;
        MarkOptionEnd(joMakeRelease, Result);
      end;

      if Result and OptionChecked[joMakeDebug] then
      begin
        MarkOptionBegin(joMakeDebug);
        for I := Low(JclSourceDirs) to High(JclSourceDirs) do
        begin
          if (JclSourceDirs[I] = JclSrcDirVisClx) then
          begin
            if OptionChecked[joMakeDebugVClx] then
              MarkOptionBegin(joMakeDebugVClx)
            else
              Continue;
          end;
          if (JclSourceDirs[I] = JclSrcDirVcl) then
          begin
            if OptionChecked[joMakeDebugVCL] or
              ((Target.VersionNumber <= 5) and (Target.RadToolKind <> brBorlandDevStudio)) then
              MarkOptionBegin(joMakeDebugVCL)
            else
              Continue;
          end;
          Result := Result and CompileLibraryUnits(JclSourceDirs[I], True);
          if (JclSourceDirs[I] = JclSrcDirVisClx) then
            MarkOptionEnd(joMakeDebugVClx, Result);
          if (JclSourceDirs[I] = JclSrcDirVcl) then
            MarkOptionEnd(joMakeDebugVCL, Result);
        end;
        MarkOptionEnd(joMakeDebug, Result);
      end;

      if OptionChecked[joCheckHppFiles] then
      begin
        MarkOptionBegin(joCheckHppFiles);
        WriteLog('Checking .hpp files');
        Result := Result and CheckHppFiles;
      end;

      MarkOptionEnd(joMake, Result);
    end;
  end;

  function CompilePackages: Boolean;
  begin
    Result := True;
    if OptionChecked[joPackages] then
    begin
      MarkOptionBegin(joPackages);
      if CLRVersion = '' then
      begin
        {$IFDEF MSWINDOWS}
        InstallJediRegInformation(Target.ConfigDataLocation, 'JCL',
          Format('%d.%d.%d.%d', [JclVersionMajor, JclVersionMinor, JclVersionRelease, JclVersionBuild]), 
          GetDcpPath, GetBplPath, Distribution.FJclPath);
        {$ENDIF MSWINDOWS}
        Result := CompilePackage(FullPackageFileName(Target, JclDpk), False);
        if Target.SupportsVisualCLX then
          Result := Result and CompilePackage(FullPackageFileName(Target, JclVClxDpk), False);
        if ((Target.VersionNumber >= 6) and (Target.RadToolKind <> brBorlandDevStudio))
          or ((Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 3)) then
          Result := Result and CompilePackage(FullPackageFileName(Target, JclVclDpk), False);
        MarkOptionEnd(joPackages, Result);
      end
      {$IFDEF MSWINDOWS}
      else
        // CLR installation
        Result := CompileCLRPackage(JediJclDpr);
      {$ENDIF MSWINDOWS}
    end;
  end;

  {$IFDEF MSWINDOWS}
  function InstallExperts: Boolean;
  var
    Option: TJclOption;
    DLLExperts: Boolean;
  begin
    Result := True;
    if OptionChecked[joExperts] then
    begin
      MarkOptionBegin(joExperts);
      DLLExperts := False;
      // dual packages useless for experts
      if Target.RadToolKind = brBorlandDevStudio then
        TJclBDSInstallation(Target).DualPackageInstallation := False;
      for Option := Low(ExpertPaths) to High(ExpertPaths) do
        if OptionChecked[Option] then
      begin
        MarkOptionBegin(Option);
        if Option = joExpertsDsgnPackages then
          // nothing, default value
        else if Option = joExpertsDLL then
          DLLExperts := OptionChecked[Option]
        else if DLLExperts then
          Result := CompileExpert(FullLibraryFileName(Target, ExpertPaths[Option]), True)
        else
          Result := CompilePackage(FullPackageFileName(Target,ExpertPaths[Option]), True);
        MarkOptionEnd(Option, Result);
        if not Result then
          Break;
      end;
      MarkOptionEnd(joExperts, Result);
    end;
  end;
  {$ENDIF MSWINDOWS}

  function InstallRepository: Boolean;
    function AddDialogToRepository(const DialogName: string;
      const DialogFileName: string; const DialogIconFileName: string;
      const Designer: string): Boolean;
    begin
      Result := True;
      try
        WriteLog(Format('Installing %s...', [DialogName]));
        Target.Repository.AddObject(DialogFileName, BorRADToolRepositoryFormTemplate,
          Target.Repository.FindPage(ExceptDlgPage, 1), DialogName, DialogIconFileName,
          ExceptDlgDescription, ExceptDlgAuthor, BorRADToolRepositoryDesignerDfm);
        WriteLog('-> ' + DialogFileName);
        WriteLog('-> ' + DialogIconFileName);
        WriteLog('...done.');
      except
        Result := False;
      end;
    end;
  begin
    Result := True;
    if OptionChecked[joExceptDlg] then
    begin
      MarkOptionBegin(joExceptDlg);
      {$IFDEF MSWINDOWS}
      if OptionChecked[joExceptDlgVCL] then
      begin
        MarkOptionBegin(joExceptDlgVCL);
        Result := AddDialogToRepository(ExceptDlgVclName, Distribution.VclDialogFileName,
          Distribution.VclDialogIconFileName, BorRADToolRepositoryDesignerDfm);
        MarkOptionEnd(joExceptDlgVCL, Result);
      end;
      if Result and OptionChecked[joExceptDlgVCLSnd] then
      begin
        MarkOptionBegin(joExceptDlgVCLSnd);
        Result := AddDialogToRepository(ExceptDlgVclSndName, Distribution.VclDialogSendFileName,
          Distribution.VclDialogSendIconFileName, BorRADToolRepositoryDesignerDfm);
        MarkOptionEnd(joExceptDlgVCLSnd, Result);
      end;
      {$ENDIF MSWINDOWS}
      if Result and OptionChecked[joExceptDlgCLX] then
      begin
        MarkOptionBegin(joExceptDlgCLX);
        Result := AddDialogToRepository(ExceptDlgClxName, Distribution.ClxDialogFileName,
          Distribution.ClxDialogIconFileName, BorRADToolRepositoryDesignerXfm);
        MarkOptionEnd(joExceptDlgCLX, Result);
      end;
      MarkOptionEnd(joExceptDlg, Result);
    end;
  end;

  {$IFDEF MSWINDOWS}
  function InstallHelpFiles: Boolean;
    function AddHelpToIdeTools: Boolean;
    var
      ToolsIndex: Integer;
      HelpTitle: string;
      IdeTool: TJclBorRADToolIdeTool;
    begin
      Result := True;
      try
        IdeTool := Target.IdeTools;
        HelpTitle := Format(JclHelpTitle, [JclVersionMajor, JclVersionMinor]);
        if IdeTool.IndexOfTitle(HelpTitle) = -1 then
        begin
          ToolsIndex := IdeTool.Count;
          IdeTool.Count := ToolsIndex + 1;
          IdeTool.Title[ToolsIndex] := HelpTitle;
          IdeTool.Path[ToolsIndex] := HHFileName;
          IdeTool.Parameters[ToolsIndex] := StrDoubleQuote(FDistribution.FJclChmHelpFileName);
          IdeTool.WorkingDir[ToolsIndex] := Distribution.JclPath;
        end;
      except
        Result := False;
      end;
    end;

    function AddHelpToOpenHelp: Boolean;
    begin
      Result := Target.OpenHelp.AddHelpFile(Distribution.FJclHlpHelpFileName, JclHelpIndexName);
      if Result then
        WriteLog(Format('Added %s to %s Online Help', [Distribution.FJclHlpHelpFileName, Target.RADToolName]))
      else
        WriteLog('failed to add help file to Online Help');
    end;

    function RegisterHelp2Files: Boolean;
    var
      //CurrentDir: string;
      NameSpace, Collection, Description, Identifier, HxSFile, HxIFile: WideString;
      LangId: Integer;
    begin
      Result := True;
      if (Target.RadToolKind <> brBorlandDevStudio) or (Target.VersionNumber < 3) then
        Exit;

      WriteLog('Registering help 2.0 files...');

      // to avoid Write AV, data have to be copied in data segment
      NameSpace := Help2NameSpace;
      Collection := Help2Collection;
      Description := Help2Description;
      Identifier := Help2Identifier;
      LangId := Help2LangId;
      HxSFile := Help2HxSFile;
      HxIFile := Help2HxIFile;

      Distribution.RegHelpCreateTransaction;
      Distribution.RegHelpRegisterNameSpace(NameSpace, Collection, Description);
      Distribution.RegHelpRegisterHelpFile(NameSpace, Identifier, LangId, HxSFile, HxIFile);
      if OptionChecked[joHelpHxSPlugin] then
      begin
        MarkOptionBegin(joHelpHxSPlugin);
        Distribution.RegHelpPlugNameSpaceIn(NameSpace, TJclBDSInstallation(Target).Help2Manager.IdeNamespace);
        MarkOptionEnd(joHelpHxSPlugin, Result);
      end;

      Distribution.RegHelpCommitTransaction;

      WriteLog('...defered');
    end;
  begin
    Result := True;
    if OptionChecked[joHelp] then
    begin
      MarkOptionBegin(joHelp);

      if OptionChecked[joHelpHlp] then
      begin
        MarkOptionBegin(joHelpHlp);
        Result := AddHelpToOpenHelp;
        MarkOptionEnd(joHelpHlp, Result);
      end;

      if Result and OptionChecked[joHelpChm] then
      begin
        MarkOptionBegin(joHelpChm);
        Result := AddHelpToIdeTools;
        MarkOptionEnd(joHelpChm, Result);
      end;

      if Result and OptionChecked[joHelpHxS] then
      begin
        MarkOptionBegin(joHelpHxS);
        Result := RegisterHelp2Files;
        MarkOptionEnd(joHelpHxS, Result);
      end;

      MarkOptionEnd(joHelp, Result);
    end;
  end;
  {$ENDIF MSWINDOWS}

  function MakeDemos: Boolean;
  var
    SaveDir: string;
    Index, ID: Integer;
    ADemoList: TStrings;
    DemoResult: Boolean;
  begin
    Result := True;
    if OptionChecked[joMakeDemos] then
    begin
      MarkOptionBegin(joMakeDemos);
      SaveDir := GetCurrentDir;
      try
        ADemoList := GetDemoList;
        for Index := 0 to ADemoList.Count - 1 do
        begin
          ID := Integer(ADemoList.Objects[Index]);
          if OptionCheckedById[ID] then
          begin
            MarkOptionBegin(ID);
            DemoResult := CompileApplication(ADemoList.Strings[Index]);
            MarkOptionEnd(ID, DemoResult);
            Result := Result and DemoResult;
          end;
        end;
      finally
        SetCurrentDir(SaveDir);
      end;

      MarkOptionEnd(joMakeDemos, Result);
    end;
  end;
  
begin
  try
    Target.OutputCallback := WriteLog;

    if Assigned(GUI) then
      GUI.Status := Format(RsInstallMessage, [TargetName]);

    if Assigned(GUIPage) then
    begin
      GUIPage.Show;
      GUIPage.BeginInstall;
    end;

    FLogLines.ClearLog;

    WriteIntroduction;
    Result := CheckDirectories and SetStaticOptions and SetEnvironment
      and MakeUnits and CompilePackages
      {$IFDEF MSWINDOWS} and InstallExperts and InstallHelpFiles {$ENDIF MSWINDOWS}
      and InstallRepository and MakeDemos;
    if not Result then
    begin
      Silent := True;
      Uninstall(False);
    end;

    FLogLines.CloseLog;
  finally
    Target.OutputCallback := nil;
    WriteLog('');
    if Assigned(GUIPage) then
      GUIPage.EndInstall;
  end;
end;

function TJclInstallation.MakePath(const FormatStr: string): string;
{$IFNDEF KYLIX}
var
  VersionStr: string;
{$ENDIF KYLIX}
begin
  {$IFDEF KYLIX}
  Result := Format(FormatStr, [Target.VersionNumber]);
  {$ELSE ~KYLIX}
  VersionStr := Target.VersionNumberStr;
  if CLRVersion <> '' then
    VersionStr := Format('%s.net', [VersionStr]);
  Result := PathGetShortName(Format(FormatStr, [VersionStr]));
  {$ENDIF ~KYLIX}
end;

function TJclInstallation.RemoveSettings: Boolean;
{$IFDEF MSWINDOWS}
var
  JclSettingsKey: string;
{$ENDIF MSWINDOWS}
begin
{$IFDEF MSWINDOWS}
  JclSettingsKey := Target.ConfigDataLocation + '\Jedi\JCL';
  if RegKeyExists(HKCU, JclSettingsKey) then
    Result := RegDeleteKeyTree(HKCU, JclSettingsKey)
  else
{$ENDIF MSWINDOWS}
    Result := True;
end;

function TJclInstallation.Uninstall(AUninstallHelp: Boolean): Boolean;
  procedure RemoveEnvironment;
  begin
    //ioJclEnvLibPath
    if CLRVersion = '' then
    begin
      if Target.RemoveFromLibrarySearchPath(FLibReleaseDir) and Target.RemoveFromLibrarySearchPath(Distribution.JclSourceDir) then
        WriteLog(Format('Removed "%s;%s" from library search path.', [FLibReleaseDir, Distribution.JclSourceDir]))
      else
        WriteLog('Failed to remove library search path.');
      {$IFDEF MSWINDOWS}
      if (Target.RadToolKind = brBorlandDevStudio) and (bpBCBuilder32 in Target.Personalities) then
        with TJclBDSInstallation(Target) do
      begin
        if RemoveFromCppSearchPath(FLibReleaseDir) and RemoveFromCppSearchPath(Distribution.JclSourceDir) and
           ((IDEVersionNumber < 5) or RemoveFromCppLibraryPath(FLibReleaseDir)) then
          WriteLog(Format('Removed "%s;%s" from cpp search path.', [FLibReleaseDir, Distribution.JclSourceDir]))
        else
          WriteLog('Failed to remove cpp search path.');
      end;
      {$ENDIF MSWINDOWS}

      //ioJclEnvBrowsingPath
      if Target.RemoveFromLibraryBrowsingPath(Distribution.JclSourcePath) then
        WriteLog(Format('Removed "%s" from library browsing path.', [Distribution.JclSourcePath]))
      else
        WriteLog('Failed to remove library browsing path.');
      {$IFDEF MSWINDOWS}
      if (Target.RadToolKind = brBorlandDevStudio) and (bpBCBuilder32 in Target.Personalities) then
        with TJclBDSInstallation(Target) do
      begin
        if RemoveFromCppBrowsingPath(Distribution.JclSourcePath) then
          WriteLog(Format('Removed "%s" from cpp browsing path.', [Distribution.JclSourcePath]))
        else
          WriteLog('Failed to remove cpp browsing path.');
      end;
      {$ENDIF MSWINDOWS}

      //ioJclEnvDebugDCUPath
      if Target.RemoveFromDebugDCUPath(FLibDebugDir) then
        WriteLog(Format('Removed "%s" from Debug DCU Path.', [FLibDebugDir]));
    end;
  end;

  procedure RemoveMake;
    procedure RemoveFileMask(const Directory, Extension: string);
    var
      FileList: TStrings;
      Index: Integer;
    begin
      FileList := TStringList.Create;
      try
        BuildFileList(Format('%s*%s', [PathAddSeparator(Directory), Extension]), faAnyFile, FileList);
        for Index := 0 to FileList.Count - 1 do
          FileDelete(PathAddSeparator(Directory) + FileList.Strings[Index]);
      finally
        FileList.Free;
      end;
    end;
  begin
    if CLRVersion <> '' then
    begin
      RemoveFileMask(FLibReleaseDir, '.dcuil');
      RemoveFileMask(FLibDebugDir, '.dcuil');
    end
    else
    begin
      RemoveFileMask(FLibReleaseDir, '.dcu');
      RemoveFileMask(FLibDebugDir, '.dcu');
      if bpBCBuilder32 in Target.Personalities then
      begin
        RemoveFileMask(FLibReleaseDir, '.obj'); // compatibility
        RemoveFileMask(FLibDebugDir, '.obj'); // compatibility
      end;
    end;
    //ioJclCopyHppFiles: ; // TODO : Delete copied files
    //ioJclCheckHppFiles: ; // nothing to do
  end;

  procedure UninstallPackages;
  begin
    if CLRVersion = '' then
    begin
      //ioJclPackages
      UninstallPackage(FullPackageFileName(Target, JclDpk));
      if (Target.RadToolKind = brBorlandDevStudio) and (Target.IDEVersionNumber = 5) then

      if Target.SupportsVisualCLX then
        UninstallPackage(FullPackageFileName(Target, JclVClxDpk));
      if ((Target.VersionNumber >= 6) and (Target.RadToolKind <> brBorlandDevStudio))
        or ((Target.VersionNumber >=3) and (Target.RadToolKind = brBorlandDevStudio)) then
        UninstallPackage(FullPackageFileName(Target, JclVclDpk));
      {$IFDEF MSWINDOWS}
      RemoveJediRegInformation(Target.ConfigDataLocation, 'JCL');
      {$ENDIF MSWINDOWS}
    end;
  end;
  {$IFDEF MSWINDOWS}
  procedure UninstallExperts;
  var
    Option: TJclOption;
  begin
    if CLRVersion = '' then
    begin
      for Option := Low(ExpertPaths) to High(ExpertPaths) do
        if not (Option in [joExpertsDsgnPackages, joExpertsDLL]) then
          UninstallExpert(Option);
    end;
  end;

  procedure UninstallHelp;
    procedure RemoveHelpFromIdeTools;
    var
      HelpIndex: Integer;
      HelpTitle: string;
    begin
      HelpTitle := Format(JclHelpTitle, [JclVersionMajor, JclVersionMinor]);
      with Target.IdeTools do
      begin
        HelpIndex := IndexOfTitle(HelpTitle);
        if HelpIndex <> -1 then
          RemoveIndex(HelpIndex);
      end;
    end;

    procedure RemoveHelpFromOpenHelp;
    begin
      WriteLog(Format('Removing %s from %s Online Help', [Distribution.FJclHlpHelpFileName, Target.RADToolName]));
      if Target.OpenHelp.RemoveHelpFile(Distribution.FJclHlpHelpFileName, JclHelpIndexName) then
        WriteLog('...done.')
      else
        WriteLog('...failed.');
    end;

    procedure UnregisterHelp2Files;
    var
      NameSpace, Identifier, HxSFile, HxIFile: WideString;
      LangId: Integer;
    begin
      if (Target.RadToolKind <> brBorlandDevStudio) or (Target.VersionNumber < 3) then
        Exit;

      WriteLog('Unregistering help 2.0 files...');

      // to avoid Write AV, data has to be copied in data segment
      NameSpace := Help2NameSpace;
      Identifier := Help2Identifier;
      LangId := Help2LangId;
      HxSFile := Help2HxSFile;
      HxIFile := Help2HxIFile;

      Distribution.RegHelpCreateTransaction;
      Distribution.RegHelpUnPlugNameSpace(NameSpace, TJclBDSInstallation(Target).Help2Manager.IdeNamespace);
      Distribution.RegHelpUnregisterHelpFile(NameSpace, Identifier, LangId);
      Distribution.RegHelpUnregisterNameSpace(NameSpace);
      Distribution.RegHelpCommitTransaction;

      WriteLog('...defered');
    end;

  begin
    if CLRVersion = '' then
    begin
      if Target.RadToolKind <> brBorlandDevStudio then
      begin
        RemoveHelpFromOpenHelp;
        RemoveHelpFromIdeTools;
      end
      else
        UnregisterHelp2Files;
    end;
  end;
  {$ENDIF MSWINDOWS}
  procedure UninstallRepository;
    procedure RemoveDialogFromRepository(const DialogName, DialogFileName: string);
    begin
      Target.Repository.RemoveObjects(ExceptDlgPath, DialogFileName, BorRADToolRepositoryFormTemplate);
      WriteLog(Format('Removed %s.', [DialogName]));
    end;
  begin
    if (CLRVersion = '') and (Target.RadToolKind <> brBorlandDevStudio) then
    begin
      {$IFDEF MSWINDOWS}
      // ioJclExcDialog
      // ioJclExcDialogVCL
      RemoveDialogFromRepository(ExceptDlgVclName, Distribution.VclDialogFileName);
      //ioJclExcDialogVCLSnd
      RemoveDialogFromRepository(ExceptDlgVclSndName, Distribution.VclDialogSendFileName);
      {$ENDIF MSWINDOWS}
      //ioJclExcDialogCLX
      RemoveDialogFromRepository(ExceptDlgClxName, Distribution.ClxDialogFileName);
    end;
  end;

begin
  try
    Target.OutputCallback := WriteLog;
    if Assigned(GUI) then
      GUI.Status := Format(RsUninstallMessage, [TargetName]);
    if Assigned(GUIPage) then
      GUIPage.Show;

    WriteLog(StrPadRight('Starting Uninstall process', 44, '.'));

    RemoveEnvironment;
    RemoveMake;
    if not Target.IsTurboExplorer then
      UninstallPackages;
    {$IFDEF MSWINDOWS}
    if not Target.IsTurboExplorer then
      UninstallExperts;
    if AUninstallHelp then
      UninstallHelp;
    {$ENDIF MSWINDOWS}
    // TODO: ioJclCopyPackagesHppFiles
    UninstallRepository;
    // TODO: ioJclMakeDemos:
  finally
    Target.OutputCallback := nil;
  end;

  Result := True;
end;

procedure TJclInstallation.WriteLog(const Msg: string);
var
  Line: string;
  LineType: TCompileLineType;
begin
  if not Silent then
  begin
    Line := InstallCore.ProcessLogLine(Msg, LineType, GUIPage);
    if Line <> '' then
      FLogLines.Write(Line);
  end;
end;

function TJclInstallation.GetBplPath: string;
var
  AConfiguration: IJediConfiguration;
begin
  if Assigned(GUIPage) then
    Result := GUIPage.Directories[FGUIBPLPathIndex]
  else
  begin
    AConfiguration := InstallCore.Configuration;
    if Assigned(AConfiguration) then
      Result := AConfiguration.OptionAsStringByName[TargetName, RsNameBPLPath]
    else
      Result := Target.BPLOutputPath;
  end;
  //{$IFDEF MSWINDOWS}
  //Result := PathGetShortName(Result);
  //{$ENDIF MSWINDOWS}
end;

function TJclInstallation.GetDcpPath: string;
var
  AConfiguration: IJediConfiguration;
begin
  if Assigned(GUIPage) then
    Result := GUIPage.Directories[FGUIDCPPathIndex]
  else
  begin
    AConfiguration := InstallCore.Configuration;
    if Assigned(AConfiguration) then
      Result := AConfiguration.OptionAsStringByName[TargetName, RsNameDCPPath]
    else
      Result := FJclDcpPath;
  end;
  //{$IFDEF MSWINDOWS}
  //Result := PathGetShortName(Result);
  //{$ENDIF MSWINDOWS}
end;

procedure TJclInstallation.Close;
  procedure SaveOptions;
  var
    AConfiguration: IJediConfiguration;
    Option: TJclOption;
    Id, Index: Integer;
    ADemoList: TStrings;
  begin
    AConfiguration := InstallCore.Configuration;
    if not (Assigned(AConfiguration) and Assigned(GUIPage)) then
      Exit;

    for Option := Low(TJclOption) to High(TJclOption) do
    begin
      Id := OptionData[Option].Id;
      AConfiguration.OptionAsBool[TargetName, Id] := GUIPage.OptionChecked[Id];
    end;

    if not Target.IsTurboExplorer then
    begin
      ADemoList := GetDemoList;
      for Index := 0 to ADemoList.Count - 1 do
      begin
        Id := Integer(ADemoList.Objects[Index]);
        AConfiguration.OptionAsBool[FDemoSectionName, Id] := GUIPage.OptionChecked[Id];
      end;

      AConfiguration.OptionAsStringByName[TargetName, RsNameBPLPath] := GUIPage.Directories[FGUIBPLPathIndex];
      if Target.RadToolKind = brCppBuilder then
        AConfiguration.OptionAsStringByName[TargetName, RsNameBPIPath] := GUIPage.Directories[FGUIDCPPathIndex]
      else
        AConfiguration.OptionAsStringByName[TargetName, RsNameDCPPath] := GUIPage.Directories[FGUIDCPPathIndex];
    end;
  end;
begin
  SaveOptions;

  FGUIPage := nil;
  FGUI := nil;
end;

function TJclInstallation.CompileLibraryUnits(const SubDir: string; Debug: Boolean): Boolean;
var
  UnitList: TStrings;
  Compiler: TJclDCC32;

  function CompilationOptions: string;
  begin
    if FTarget.RADToolKind = brCppBuilder then
    begin
      Result := StringsToStr(Compiler.Options, ' ') + ' ';
      Result := StringReplace(Result, '$(BCB)', Target.RootDir, [rfReplaceAll]);
    end
    else
      Result := '';
  end;

  function CompileUnits: Boolean;
  begin
    Result := Compiler.Execute({$IFNDEF KYLIX}CompilationOptions + {$ENDIF}StringsToStr(UnitList, ' '));
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

  function CopyHppFiles(const TargetDir: string): Boolean;
  var
    I: Integer;
    FileName: string;
  begin
    Result := True;
    for I := 0 to UnitList.Count - 1 do
    begin
      FileName := UnitList[I] + '.hpp';
      if FileExists(FileName) then
        Result := Result and FileCopy(FileName, TargetDir + FileName, True);
    end;
  end;

var
  UnitType, LibDescriptor, SaveDir, UnitOutputDir, Path, ExclusionFileName: string;
  Index, ExcIndex: Integer;
  Exclusions: TStrings;
begin
  Result := True;
  if Debug then
    UnitType := 'debug ';
  LibDescriptor := Format(RsLibDescriptor, [SubDir, UnitType, TargetName]);
  WriteLog(Format('Making %s', [LibDescriptor]));
  Path := Format('%s' + DirDelimiter + '%s', [Distribution.JclSourceDir, SubDir]);
  UnitList := TStringList.Create;
  try
    BuildFileList(PathAddSeparator(Path) + '*.pas', faAnyFile, UnitList);
    ExclusionFileName := PathAddSeparator(FLibReleaseDir) + SubDir + '.exc';
    if FileExists(ExclusionFileName) then
    begin
      Exclusions := TStringList.Create;
      try
        Exclusions.LoadFromFile(ExclusionFileName);
        for Index := 0 to Exclusions.Count - 1 do
        begin
          ExcIndex := UnitList.IndexOf(Exclusions.Strings[Index]);
          if ExcIndex >= 0 then
            UnitList.Delete(ExcIndex);
        end;
      finally
        Exclusions.Free;
      end;
    end;
    if UnitList.Count = 0 then
      Exit;
    for Index := 0 to UnitList.Count - 1 do
      UnitList.Strings[Index] := ChangeFileExt(UnitList.Strings[Index], '');

    {$IFDEF MSWINDOWS}
    if CLRVersion <> '' then
      Compiler := (Target as TJclBDSInstallation).DCCIL
    else
    {$ENDIF MSWINDOWS}
      Compiler := Target.DCC32;
    Compiler.SetDefaultOptions;
    //Options.Add('-D' + StringsToStr(Defines, ';'));
    Compiler.Options.Add('-M');
    if Debug then
    begin
      Compiler.Options.Add('-$C+'); // assertions
      Compiler.Options.Add('-$D+'); // debug informations
      Compiler.Options.Add('-$I+'); // I/O checking
      Compiler.Options.Add('-$L+'); // local debugging symbols
      Compiler.Options.Add('-$O-'); // optimizations
      Compiler.Options.Add('-$Q+'); // overflow checking
      Compiler.Options.Add('-$R+'); // range checking
      if CLRVersion = '' then
        Compiler.Options.Add('-$W+'); // stack frames
      Compiler.Options.Add('-$Y+'); // symbol reference info
    end
    else
    begin
      Compiler.Options.Add('-$C-'); // assertions
      Compiler.Options.Add('-$D-'); // debug informations
      Compiler.Options.Add('-$I-'); // I/O checking
      Compiler.Options.Add('-$L-'); // local debugging symbols
      Compiler.Options.Add('-$O+'); // optimizations
      Compiler.Options.Add('-$Q-'); // overflow checking
      Compiler.Options.Add('-$R-'); // range checking
      if CLRVersion = '' then
        Compiler.Options.Add('-$W-'); // stack frames
      Compiler.Options.Add('-$Y-'); // symbol reference info
    end;

    if (bpBCBuilder32 in Target.Personalities) and (CLRVersion = '') then
    begin
      Compiler.Options.Add('-D_RTLDLL;NO_STRICT;USEPACKAGES'); // $(SYSDEFINES)
      if Debug then
        UnitOutputDir := FLibDebugDir
      else
        UnitOutputDir := FLibReleaseDir;

      if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 4) then
      begin
        Compiler.AddPathOption('N0', UnitOutputDir); // .dcu files
        //Compiler.AddPathOption('NH', FIncludeDir);   // .hpp files
        Compiler.AddPathOption('NO', UnitOutputDir); // .obj files
      end
      else
      begin
        Compiler.AddPathOption('N0', UnitOutputDir); // .dcu files
        //Compiler.AddPathOption('N1', FIncludeDir);   // .hpp files
        Compiler.AddPathOption('N2', UnitOutputDir); // .obj files
      end;
      Compiler.Options.Add('-JPHNE');
      Compiler.Options.Add('--BCB');
      //Compiler.AddPathOption('O', Format(BCBIncludePath, [Distribution.JclSourceDir, Distribution.JclSourcePath]));
      //Compiler.AddPathOption('U', Format(BCBObjectPath, [Distribution.JclSourceDir, Distribution.JclSourcePath]));
    end
    else // Delphi
    begin
      if Debug then
        UnitOutputDir := FLibDebugDir
      else
        UnitOutputDir := FLibReleaseDir;

      Compiler.AddPathOption('N', UnitOutputDir); // .dcu files
      if CLRVersion <> '' then
        Compiler.Options.Add('--default-namespace:Jedi.Jcl');

    end;
    Compiler.AddPathOption('I', Distribution.JclSourceDir);
    Compiler.AddPathOption('U', Distribution.JclSourcePath);
    Compiler.AddPathOption('R', Distribution.JclSourcePath);
    
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
      if CLRVersion = '' then
      begin
        CopyResFiles(UnitOutputDir);
        if OptionChecked[joCopyHppFiles] then
        begin
          MarkOptionBegin(joCopyHppFiles);
          WriteLog('Copying .hpp files...');
          Result := Result and CopyHppFiles(Target.VclIncludeDir);
          MarkOptionEnd(joCopyHppFiles, Result);
        end;
        {$IFDEF KYLIX}
        Compiler.Options.Add('-P');   // generate position independent code (PIC)
        WriteLog('');
        WriteLog('Compiling dpu files...');
        Result := Result and CompileUnits;
        {$ENDIF KYLIX}
      end;
    finally
      SetCurrentDir(SaveDir);
    end;
  finally
    UnitList.Free;
  end;
  if not Result then
    WriteLog('Failed ' + LibDescriptor);
end;

{$IFDEF MSWINDOWS}
function TJclInstallation.CompileCLRPackage(const Name: string): Boolean;
var
  ProjectFileName: string;
begin
  ProjectFileName := Format('%spackages%s%s.net%s%s%s', [PathAddSeparator(Distribution.JclPath),
    DirDelimiter, Target.VersionNumberStr, DirDelimiter, Name, SourceExtensionDelphiProject]);
  WriteLog(Format('Compiling CLR package %s...', [ProjectFileName]));

  if Assigned(GUIPage) then
    GUIPage.CompilationStart(ExtractFileName(Name));

  Result := TJclBDSInstallation(Target).CompileDelphiDotNetProject(ProjectFileName,
    GetBplPath, TargetPlatform, CLRVersion);
end;
{$ENDIF MSWINDOWS}

function TJclInstallation.CompilePackage(const Name: string; InstallPackage: Boolean): Boolean;
var
  PackageFileName: string;
{$IFNDEF KYLIX}
  DpkPackageFileName: string;
{$ENDIF}
begin
  PackageFileName := PathAddSeparator(Distribution.JclPath) + Name;
  if InstallPackage then
    WriteLog(Format('Installing package %s...', [PackageFileName]))
  else
    WriteLog(Format('Compiling package %s...', [PackageFileName]));

  if Assigned(GUIPage) then
    GUIPage.CompilationStart(ExtractFileName(Name));

  if IsDelphiPackage(PackageFileName) and (bpDelphi32 in Target.Personalities) then
  begin
    if InstallPackage then
      Result := Target.InstallPackage(PackageFileName, GetBplPath, GetDcpPath)
    else
    begin
      {$IFNDEF KYLIX}
      if Target.RadToolKind = brBorlandDevStudio then
        (Target as TJclBDSInstallation).CleanPackageCache(BinaryFileName(GetBplPath, PackageFileName));
      {$ENDIF ~KYLIX}
      Result := Target.CompilePackage(PackageFileName, GetBplPath, GetDcpPath);
    end;
  end
  else if IsBCBPackage(PackageFileName) and (bpBCBuilder32 in Target.Personalities) then
  begin
    ConfigureBpr2Mak(PackageFileName);
    {$IFDEF KYLIX}
    if InstallPackage then
      Result := Target.InstallPackage(PackageFileName, GetBplPath, GetDcpPath)
    else
      Result := Target.CompilePackage(PackageFileName, GetBplPath, GetDcpPath);
    {$ELSE ~KYLIX}

    if Target.RadToolKind = brBorlandDevStudio then
      (Target as TJclBDSInstallation).CleanPackageCache(BinaryFileName(GetBplPath, PackageFileName));

    // to satisfy JVCL (and eventually other libraries), create a .dcp file;
    // Note: it is put out to .bpl path to make life easier for JVCL
    DpkPackageFileName := ChangeFileExt(PackageFileName, SourceExtensionDelphiPackage);
    if InstallPackage then
      Result := ((not FileExists(DpkPackageFileName))
                 or Target.InstallPackage(DpkPackageFileName, GetBplPath, GetDcpPath))
                and Target.InstallPackage(PackageFileName, GetBplPath, GetDcpPath)
    else
      Result := ((not FileExists(DpkPackageFileName))
                 or Target.CompilePackage(DpkPackageFileName, GetBplPath, GetDcpPath))
                and Target.CompilePackage(PackageFileName, GetBplPath, GetDcpPath);
    {$ENDIF ~KYLIX}
  end
  else
  begin
    Result := False;
    WriteLog(Format('No personality supports the extension %s', [ExtractFileExt(PackageFileName)]));
  end;

  if Result then
    WriteLog('...done.')
  else
    WriteLog('...failed');
end;

function TJclInstallation.CompileApplication(FileName: string): Boolean;
var
  CfgFileName, Directory: string;
begin
  Directory := ExtractFileDir(FileName);
  FileName := ExtractFileName(FileName);
  WriteLog(Format(RsBuildingMessage, [FileName]));
  SetCurrentDir(Directory);
  CfgFileName := ChangeFileExt(FileName, '.cfg');
  StringToFile(CfgFileName, Format(
    '-e%s' + AnsiLineBreak +    // Exe output dir
    '-n.' + AnsiLineBreak +    // Unit output dir
    '-u%s;%s' + AnsiLineBreak + // Unit directories
    '-i%s',                     // Include path
    [Distribution.JclBinDir, FLibReleaseDir, Distribution.JclSourcePath, Distribution.JclSourceDir]));
  Result := Target.DCC32.Execute(FileName);
  FileDelete(CfgFileName);
end;

function TJclInstallation.UninstallPackage(const Name: string): Boolean;
var
  PackageFileName: string;
begin
  WriteLog(Format('Removing package %s.', [Name]));
  PackageFileName := Distribution.JclPath + Format(Name, [Target.VersionNumberStr]);

  {$IFNDEF KYLIX}
  if Target.RadToolKind = brBorlandDevStudio then
    (Target as TJclBDSInstallation).CleanPackageCache(BinaryFileName(GetBPLPath, PackageFileName));
  {$ENDIF KYLIX}

  Result := Target.UninstallPackage(PackageFileName, GetBPLPath, GetDCPPath);

  // delete DCP files that were created to bpl path (old behavior)
  FileDelete(PathAddSeparator(GetBPLPath) + PathExtractFileNameNoExt(Name) + CompilerExtensionDCP);
  // delete DCP files that were created to target dcp path (old behavior)
  FileDelete(PathAddSeparator(Target.DCPOutputPath) + PathExtractFileNameNoExt(Name) + CompilerExtensionDCP);
  // delete BPI files that were created to target dcp path (old behavior)
  FileDelete(PathAddSeparator(Target.DCPOutputPath) + PathExtractFileNameNoExt(Name) + CompilerExtensionBPI);
  // delete LIB files that were created to target dcp path (old behaviour)
  FileDelete(PathAddSeparator(Target.DCPOutputPath) + PathExtractFileNameNoExt(Name) + CompilerExtensionLIB);

  // TODO : evtl. remove .HPP Files
  if Result then
    WriteLog('...done.')
  else
    WriteLog('...failed.');
end;

procedure TJclInstallation.ConfigureBpr2Mak(const PackageFileName: string);
var
  PackageDirectory: string;
begin
  PackageDirectory := PathAddSeparator(ExtractFileDir(PackageFileName));
  if clProj2Mak in Target.CommandLineTools then
  begin
    Target.Bpr2Mak.Options.Clear;
    Target.Bpr2Mak.Options.Add('-t' + ExtractRelativePath(PackageDirectory,Distribution.JclPath + Bcb2MakTemplate));
  end;
  {$IFDEF KYLIX}
  SetEnvironmentVar('OBJDIR', FLibObjDir);
  SetEnvironmentVar('BPILIBDIR', GetDcpPath);
  SetEnvironmentVar('BPLDIR', GetBplPath);
  {$ELSE ~KYLIX}
  if clMake in Target.CommandLineTools then
  begin
    Target.Make.Options.Clear;
    Target.Make.AddPathOption('DBPILIBDIR=', GetDcpPath);
    Target.Make.AddPathOption('DBPLDIR=', GetBplPath);
    if OptionChecked[joCopyPackagesHppFiles] then
    //begin
    //  MarkOptionBegin(joCopyPackagesHppFiles);
      Target.Make.AddPathOption('DHPPDIR=', Target.VclIncludeDir);
    //  MarkOptionEnd(joCopyPackagesHppFiles, True);
    //end;
  end;
  {$ENDIF ~KYLIX}
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
  // @*@JCLWizardInit$qqsx56System@%DelphiInterface$t28Toolsapi@IBorlandIDEServices%pqqrx47System@%DelphiInterface$t19Toolsapi@IOTAWizard%$orpqqrv$v
  InternalEntryPoint = '@JCLWizardInit$';
begin
  ProjectFileName := PathAddSeparator(Distribution.JclPath) + Name;

  if InstallExpert then
    WriteLog(Format('Installing expert %s...', [ProjectFileName]))
  else
    WriteLog(Format('Compiling expert %s...', [ProjectFileName]));

  if Assigned(GUIPage) then
    GUIPage.CompilationStart(ExtractFileName(Name));

  if IsDelphiProject(ProjectFileName) and (bpDelphi32 in Target.Personalities) then
  begin
    if InstallExpert then
      Result := Target.InstallExpert(ProjectFileName, GetBplPath, GetDcpPath)
    else
      Result := Target.CompileProject(ProjectFileName, GetBplPath, GetDcpPath);
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

    Result := Target.CompileProject(ProjectFileName, GetBplPath, GetDcpPath);

    if Result then
    begin
      WriteLog('First compilation ok');
      LibraryPeImage := TJclPeImage.Create;
      try
        GetBPRFileInfo(ProjectFileName, ProjectBinaryFileName, @ProjectDescription);
        ProjectBinaryFileName := PathAddSeparator(GetBplPath) + ProjectBinaryFileName;

        WriteLog(Format('Analysing expert %s for entry point %s...', [ProjectBinaryFileName, WizardEntryPoint]));
        LibraryPeImage.FileName := ProjectBinaryFileName;
        ExportFuncList := LibraryPeImage.ExportList;

        FirstCompilationOk := Assigned(ExportFuncList.ItemFromName[WizardEntryPoint]);
        // the expected export name doesn't exist
        if not FirstCompilationOk then
        begin
          Result := False;
          WriteLog('Entry point not found');

          // try to find the decorated entry point
          // export names for pascal functions are:
          // @UnitName@FunctionName$ParameterSignature

          for Index := 0 to ExportFuncList.Count - 1 do
            if Pos(StrUpper(InternalEntryPoint), StrUpper(ExportFuncList.Items[Index].Name)) > 0 then
          begin
            WriteLog(Format('Internal entry point found %s', [ExportFuncList.Items[Index].Name]));
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
          WriteLog('Entry point found, registering expert...');
          Target.RegisterExpert(ProjectBinaryFileName, ProjectDescription);
        end;
      finally
        LibraryPeImage.Free;
      end;

      if Result and (not FirstCompilationOk) then
      begin
        // second compilation
        if InstallExpert then
          Result := Target.InstallExpert(ProjectFileName, GetBplPath, GetDcpPath)
        else
          Result := Target.CompileProject(ProjectFileName, GetBplPath, GetDcpPath);
      end
      else if not Result then
        WriteLog('Internal entry point not found');
    end
    else
      WriteLog('First compilation failed');
  end
  else
    Result := False;

  if Result then
    WriteLog('...done.')
  else
    WriteLog('... failed ' + ProjectFileName);
end;

function TJclInstallation.UninstallExpert(const Option: TJclOption): Boolean;

  function OldExpertBPLFileName(const BaseName: string): string;
  const
    OldExperts: array[joExpertDebug..joExpertVersionControl] of string =
      (
        'JclDebugIde%s0.bpl', 'ProjectAnalyzer%s0.bpl',
        'IdeOpenDlgFavorite%s0.bpl', 'JclRepositoryExpert',
        'ThreadNameExpert%s0.bpl', 'JediUses%s0.bpl', 'JclSIMDView%s.bpl',
        'JclVersionControl'
      );

  var
    I: TJclOption;
  begin
    with Target do
      for I := Low(OldExperts) to High(OldExperts) do
        if BaseName = ExpertPaths[I] then
    begin
      Result := PathAddSeparator(GetBPLPath) + Format(OldExperts[I], [VersionNumberStr]);
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

  if FileExists(Distribution.JclPath + PackageFileName) then
  begin
    Result := UninstallPackage(PackageFileName);
    if (Target.RadToolKind = brBorlandDevStudio) and (Target.IDEVersionNumber = 5) then
      Target.IdePackages.RemovePackage(PathAddSeparator(GetBplPath) + PathExtractFileNameNoExt(PackageFileName) + '100.bpl');
    // eventually remove old expert packages to avoid annoying package conflicts during IDE startup;
    // for simplicity, .dcp files are not handled
    BaseName := ExtractFileName(BaseName);
    BPLFileName := OldExpertBPLFileName(BaseName);
    Target.IdePackages.RemovePackage(BPLFileName);
    FileDelete(BPLFileName);
  end;

  if FileExists(Distribution.JclPath + LibraryFileName) then
  begin
    WriteLog(Format('Removing expert %s', [LibraryFileName]));
    // delete DLL experts
    Result := Target.UninstallExpert(Distribution.JclPath + LibraryFileName, GetBPLPath);
    if Result then
      WriteLog('...done.')
    else
      WriteLog('...failed');
  end;
end;
{$ENDIF MSWINDOWS}

function DemoNameCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  Name1, Name2: string;
begin
  Name1 := ExtractFileName(List[Index1]);
  Name2 := ExtractFileName(List[Index2]);
  Result := CompareText(Name1, Name2);
end;

procedure TJclInstallation.AddDemo(const Directory: string; const FileInfo: TSearchRec);
begin
  FDemoList.Append(Directory + FileInfo.Name);
end;

procedure TJclInstallation.AddDemos(const Directory: string);
begin
  EnumFiles(Directory + '*.dpr', AddDemo);
end;

function TJclInstallation.GetDemoList: TStringList;
  procedure ProcessExcludeFile(const ExcFileName: string);
  var
    DemoExclusionList: TStrings;
    ExclusionFileName, FileName, Edition: string;
    IndexExc, IndexDemo, EditionPos: Integer;
  begin
    DemoExclusionList := TStringList.Create;
    try
      ExclusionFileName := MakePath(PathAddSeparator(Distribution.JclExamplesDir) + ExcFileName);
      if FileExists(ExclusionFileName) then
      begin
        DemoExclusionList.LoadFromFile(ExclusionFileName);
        for IndexExc := 0 to DemoExclusionList.Count - 1 do
        begin
          FileName := DemoExclusionList.Strings[IndexExc];
          EditionPos := Pos('=', FileName);
          if EditionPos > 0 then
          begin
            Edition := Copy(FileName, EditionPos + 1, Length(FileName) - EditionPos);
            SetLength(FileName, EditionPos - 1);
          end
          else
            Edition := '';
          if (Edition = '') or (StrIPos(BorRADToolEditionIDs[Target.Edition], Edition) = 0) then
          begin
            if ExtractFileExt(FileName) = '.exc' then
              ProcessExcludeFile(FileName)
            else
            begin
              for IndexDemo := FDemoList.Count - 1  downto 0 do
                if StrMatches(PathAddSeparator(Distribution.JclExamplesDir) + FileName, FDemoList.Strings[IndexDemo]) then
                  FDemoList.Delete(IndexDemo);
            end;
          end;
        end;
      end;
    finally
      DemoExclusionList.Free;
    end;
  end;
begin
  if not Assigned(FDemoList) then
  begin
    FDemoList := TStringList.Create;
    EnumDirectories(Distribution.JclExamplesDir, AddDemos);
    FDemoList.CustomSort(DemoNameCompare);

    {$IFDEF KYLIX}
    ProcessExcludeFile('k%d.exc');
    {$ELSE ~KYLIX}
    ProcessExcludeFile('%s.exc');
    {$ENDIF ~KYLIX}
  end;
  Result := FDemoList;
end;
{
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

    {$IFDEF MSWINDOWS
    PathEnvVar := RegReadStringDef(HKCU, RegHKCUEnvironmentVar, PathEnvironmentVar, '');
    PathListIncludeItems(PathEnvVar, RegReadStringDef(HKLM, RegHKLMEnvironmentVar, PathEnvironmentVar, ''));
    if (PathListItemIndex(PathEnvVar, BplPath) = -1) and (PathListItemIndex(PathEnvVar, PathAddSeparator(BplPath)) = -1)
      and (MessageDlg(RsAddPathToEnvironment, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      PathEnvVar := RegReadStringDef(HKCU, RegHKCUEnvironmentVar, PathEnvironmentVar, '');
      PathListIncludeItems(PathEnvVar, BplPath);
      RegWriteString(HKCU, RegHKCUEnvironmentVar, PathEnvironmentVar, PathEnvVar);
    end;
    {$ENDIF MSWINDOWS

    InstallationStarted;
    try
      Result := InstallSelectedOptions;
    finally
      InstallationFinished;
    end;
  end;
  SaveOptions;
end;
}

//=== { TJclDistribution } ===================================================

procedure TJclDistribution.Close;
var
  I: Integer;
begin
  for I := 0 to TargetInstallCount - 1 do
    TargetInstalls[I].Close;
  FGUI := nil;
end;

constructor TJclDistribution.Create;
  procedure RegisterJclOptions;
  var
    Option: TJclOption;
    AInstallCore: TJediInstallCore;
    OptionName: string;
  begin
    AInstallCore := InstallCore;
    for Option := Low(TJclOption) to High(TJclOption) do
    begin
      OptionName := GetEnumName(TypeInfo(TJclOption), Integer(Option));
      OptionName := 'Jcl' + Copy(OptionName, 3, Length(OptionName) - 2);
      OptionData[Option].Id := AInstallCore.AddInstallOption(OptionName);
    end;
  end;
begin
  inherited Create;

  RegisterJclOptions;

  {$IFDEF MSWINDOWS}
  FCLRVersions := TStringList.Create;
  FRegHelpCommands := TStringList.Create;
  {$ENDIF MSWINDOWS}
  FRadToolInstallations := TJclBorRADToolInstallations.Create;

  FTargetInstalls := TObjectList.Create;
  FTargetInstalls.OwnsObjects := True;
end;

function TJclDistribution.CreateInstall(Target: TJclBorRADToolInstallation): Boolean;
  function Supported: Boolean;
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
        Result := Target.VersionNumber in [1, 2, 3, 4, 5];
      else
        Result := False;
    end;
    {$ENDIF ~KYLIX}
  end;
var
  Inst: TJclInstallation;
  {$IFDEF MSWINDOWS}
  Index: Integer;
  CLRVersion: string;
  {$ENDIF MSWINDOWS}
begin
  if Supported then
  try
    Inst := TJclInstallation.Create(Self, Target);
    FTargetInstalls.Add(Inst);
    {$IFDEF MSWINDOWS}
    // .net "virtual" targets
    if (Target is TJclBDSInstallation) and (Target.IDEVersionNumber >= 3) and (not Target.IsTurboExplorer)
      and (bpDelphiNet32 in Target.Personalities) then
    begin
      for Index := 0 to FCLRVersions.Count - 1 do
      begin
        CLRVersion := FCLRVersions.Names[Index];
        if (CompareCLRVersions(CLRVersion, TJclBDSInstallation(Target).MaxDelphiCLRVersion) = 0)
          and (CompareCLRVersions(CLRVersion, 'v1.1.2344') >= 0)  then // CLR 1.0 not supported by the JCL
        begin
          Inst := TJclInstallation.Create(Self, Target, CLRVersion);
          FTargetInstalls.Add(Inst);
          {if Target.VersionNumber >= 4 then
          begin
            Inst := TJclInstallation.Create(Self, Target, CLRVersion, bp64bit);
            FTargetInstalls.Add(Inst);
          end;}
        end;
      end;
    end;
    {$ENDIF MSWINDOWS}
  except
  end;
  Result := True;
end;

destructor TJclDistribution.Destroy;
begin
  {$IFDEF MSWINDOWS}
  FCLRVersions.Free;
  FRegHelpCommands.Free;
  {$ENDIF MSWINDOWS}

  FRadToolInstallations.Free;

  FTargetInstalls.Free;

  inherited Destroy;
end;

function TJclDistribution.GetTargetInstall(Index: Integer): TJclInstallation;
begin
  Result := TJclInstallation(FTargetInstalls.Items[Index]);
end;

function TJclDistribution.GetTargetInstallCount: Integer;
begin
  Result := FTargetInstalls.Count;
end;

function TJclDistribution.GetVersion: string;
  function GetRevision: Integer;
  var
    DailyFileName, SvnEntriesFileName, RevisionText: string;
    TextFile: TJclMappedTextReader;
    Index: Integer;
  begin
    Result := 0;

    DailyFileName := FJclPath + DailyRevisionFileName;
    if FileExists(DailyFileName) then
    begin
      // directory from a daily zip
      TextFile := TJclMappedTextReader.Create(DailyFileName);
      try
        RevisionText := TextFile.ReadLn;
        if RevisionText <> '' then
        begin
          Index := Length(RevisionText) - 1; // skip the '.'
          while (Index > 1) and (RevisionText[Index] in AnsiDecDigits) do
            Dec(Index);
          Result := StrToIntDef(Copy(RevisionText, Index + 1, Length(RevisionText) - Index - 1), 0);
        end;
      finally
        TextFile.Free;
      end;
    end;

    if Result = 0 then
    begin
      SvnEntriesFileName := FJclPath + EntriesFileName1;
      if not FileExists(SvnEntriesFileName) then
        SvnEntriesFileName := FJclPath + EntriesFileName2;
      if FileExists(SvnEntriesFileName) then
      begin
        // directory from subversion
        TextFile := TJclMappedTextReader.Create(SvnEntriesFileName);
        try
          TextFile.ReadLn;
          TextFile.ReadLn;
          TextFile.ReadLn;
          RevisionText := TextFile.ReadLn;
          Result := StrToIntDef(RevisionText, 0);
        finally
          TextFile.Free;
        end;
      end;
    end;
  end;
var
  StableText, Source: string;
  Revision: Integer;
begin
  if JclVersionRelease = 0 then
  begin
    Revision := GetRevision;
    StableText := RsJclVersionTesting;
  end
  else
  begin
    Revision := 0;
    StableText := RsJclVersionRelease;
  end;

  if Revision = 0 then
  begin
    Source := RsJclVersionBuild;
    Revision := JclVersionBuild;
  end
  else
    Source := RsJclVersionRevision;

  Result := Format(RsJclVersionMask, [JclVersionMajor, JclVersionMinor, StableText, Source, Revision])
end;

procedure TJclDistribution.Init;
  procedure InitDistribution;
  var
    ExceptDialogsPath, InstallerFileName: string;
    ReadMePage: IJediReadMePage;
    Index: Integer;
  begin
    InstallerFileName := ParamStr(0);

    FJclPath := PathAddSeparator(ExpandFileName(PathExtractFileDirFixed(InstallerFileName) + '..'));
    {$IFDEF MSWINDOWS}
    FJclPath := PathGetShortName(FJclPath);
    {$ENDIF MSWINDOWS}
    FLibReleaseDirMask := Format('%slib' + VersionDirExp, [FJclPath]);
    FLibDebugDirMask := FLibReleaseDirMask + DirDelimiter + 'debug';
    FJclBinDir := FJclPath + 'bin';
    FJclSourceDir := FJclPath + 'source';
    FJclExamplesDir := FJclPath + 'examples';
    FJclSourcePath := '';
    for Index := Low(JclSourceDirs) to High(JclSourceDirs) do
      ListAddItems(FJclSourcePath, DirSeparator, PathAddSeparator(FJclSourceDir) + JclSourceDirs[Index]);

    ExceptDialogsPath := FJclPath + ExceptDlgPath;
    FClxDialogFileName := ExceptDialogsPath + ExceptDlgClxFileName;
    FClxDialogIconFileName := ChangeFileExt(FClxDialogFileName, '.ico');
    FVclDialogFileName := ExceptDialogsPath + ExceptDlgVclFileName;
    FVclDialogIconFileName := ChangeFileExt(FVclDialogFileName, '.ico');
    FVclDialogSendFileName := ExceptDialogsPath + ExceptDlgVclSndFileName;
    FVclDialogSendIconFileName := ChangeFileExt(FVclDialogSendFileName, '.ico');
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
    {$ENDIF MSWINDOWS}
    FJclReadmeFileName := FJclPath + 'docs' + DirDelimiter + ReadmeFileName;
    if Assigned(GUI) then
    begin
      ReadMePage := GUI.CreateReadmePage;
      ReadMePage.Caption := Version;
      ReadMePage.ReadmeFileName := FJclReadmeFileName;
    end;

    {$IFDEF MSWINDOWS}
    FCLRVersions.Clear;
    try
      JclDotNet.TJclClrHost.GetClrVersions(FCLRVersions);
    except
      // trap exceptions when no .net runtimes are installed
    end;
    {$ENDIF MSWINDOWS}
  end;

  procedure CreateInstallations;
  begin
    if not RADToolInstallations.Iterate(CreateInstall) then
      raise EJediInstallInitFailure.CreateRes(@RsNoInstall);
  end;

  procedure InitInstallations;
  var
    I: Integer;
  begin
    for I := 0 to TargetInstallCount - 1 do
      TargetInstalls[I].Init;
  end;
begin
  FGUI := InstallCore.InstallGUI;

  InitDistribution;
  CreateInstallations;
  InitInstallations;
end;

procedure TJclDistribution.Install;
var
  I: Integer;
  KeepSettings, Success: Boolean;
  AInstallation: TJclInstallation;
begin
  KeepSettings := True;
  try
    if RadToolInstallations.AnyInstanceRunning {$IFDEF MSWINDOWS} and not IsDebuggerAttached {$ENDIF} then
    begin
      if Assigned(GUI) then
        GUI.Dialog(RsCloseRADTool, dtError, [drCancel]);
      Exit;
    end;

    {$IFDEF MSWINDOWS}
    if Assigned(GUI) then
    begin
      GUI.Status := 'Initializing JCL installation process';

      for I := 0 to TargetInstallCount - 1 do
      begin
        AInstallation := TargetInstalls[I];
        if AInstallation.Enabled and (AInstallation.CLRVersion = '') then
        begin
          KeepSettings := GUI.Dialog('Do you want to keep JCL expert settings?',
            dtConfirmation, [drYes, drNo]) = drYes;
          Break;
        end;
      end;
    end;
    RegHelpClearCommands;
    {$ENDIF MSWINDOWS}

    FNbEnabled := 0;
    FNbInstalled := 0;

    for I := 0 to TargetInstallCount - 1 do
      if TargetInstalls[I].Enabled then
        Inc(FNbEnabled);

    Success := True;
    for I := 0 to TargetInstallCount - 1 do
    begin
      AInstallation := TargetInstalls[I];
      if AInstallation.Enabled then
      begin
        AInstallation.Silent := False;
        if (AInstallation.CLRVersion = '') and not KeepSettings then
          AInstallation.RemoveSettings;
        AInstallation.Uninstall(False);
        Success := AInstallation.Install;
        if not Success then
          Break;
        Inc(FNbInstalled);
      end;
    end;

    {$IFDEF MSWINDOWS}
    Success := Success and RegHelpExecuteCommands(True);
    {$ENDIF MSWINDOWS}

    if Assigned(GUI) then
    begin
      if Success then
        GUI.Dialog('Installation success', dtInformation, [drOK])
      else
        GUI.Dialog('Installation failed, see logs for details', dtError, [drOK]);
    end;
  finally
    if Assigned(GUI) then
      GUI.Status := 'Installation finished';
  end;
end;

{$IFDEF MSWINDOWS}
const
  // Reg Helper constant (chronological order)
  RHCreateTransaction   = 1;
  RHRegisterNameSpace   = 2;
  RHRegisterFile        = 3;
  RHPlugNameSpace       = 4;
  RHUnplugNameSpace     = 5;
  RHUnregisterFile      = 6;
  RHUnregisterNameSpace = 7;
  RHCommitTransaction   = 8;

procedure TJclDistribution.RegHelpClearCommands;
begin
  FRegHelpCommands.Clear;
end;

procedure TJclDistribution.RegHelpCommitTransaction;
begin
  RegHelpInternalAdd(RHCommitTransaction, 'commit', True);
end;

procedure TJclDistribution.RegHelpCreateTransaction;
begin
  RegHelpInternalAdd(RHCreateTransaction, 'create', True);
end;

function TJclDistribution.RegHelpExecuteCommands(DisplayErrors: Boolean): Boolean;
var
  Index: Integer;
  Parameters, LogFileName, ProgramResult, Verb: string;
  ResultLines: TJclMappedTextReader;
  TargetInstall: TJclInstallation;
begin
  Result := True;
  if FRegHelpCommands.Count = 0 then
    Exit;

  // step 1: compile the RegHelper utility

  for Index := TargetInstallCount - 1 downto 0 do // from the end (newer releases ready for vista)
  begin
    TargetInstall := TargetInstalls[Index];
    if TargetInstall.Enabled then
    begin
      Result := TargetInstall.CompileApplication(JclPath + 'install\RegHelper.dpr');
      if not Result then
      begin
        if Assigned(GUI) then
          GUI.Dialog('Failed to compile RegHelper utility', dtError, [drOK]);
        Exit;
      end;
      Break;
    end;
  end;

  // step 2: create parameters for the RegHelper utility

  LogFileName := JclBinDir + '\RegHelper.log';
  if FileExists(LogFileName) then
    FileDelete(LogFileName);
  Parameters := '-c -o' + LogFileName;
  for Index := 0 to FRegHelpCommands.Count - 1 do
  begin
    case Integer(FRegHelpCommands.Objects[Index]) of
      RHCreateTransaction:
        Parameters := Format('%s Create', [Parameters]);
      RHRegisterNameSpace:
        Parameters := Format('%s "RegNameSpace;%s"', [Parameters, FRegHelpCommands.Strings[Index]]);
      RHRegisterFile:
        Parameters := Format('%s "RegHelpFile;%s"', [Parameters, FRegHelpCommands.Strings[Index]]);
      RHPlugNameSpace:
        Parameters := Format('%s "PlugNameSpace;%s"', [Parameters, FRegHelpCommands.Strings[Index]]);
      RHUnplugNameSpace:
        Parameters := Format('%s "UnplugNameSpace;%s"', [Parameters, FRegHelpCommands.Strings[Index]]);
      RHUnregisterFile:
        Parameters := Format('%s "UnregHelpFile;%s"', [Parameters, FRegHelpCommands.Strings[Index]]);
      RHUnregisterNameSpace:
        Parameters := Format('%s "UnregNameSpace;%s"', [Parameters, FRegHelpCommands.Strings[Index]]);
      RHCommitTransaction:
        Parameters := Format('%s Commit', [Parameters]);
    else
      if Assigned(GUI) then
        GUI.Dialog('Fatal error: unknown reghelp command', dtError, [drOK]);
      Exit;
    end;
  end;

  // step 3:  inform the user and execute RegHelper

  // simple dialog explaining user why we need credentials
  if Assigned(GUI) and ((not IsAdministrator) or (IsWinVista or IsWinServer2008)) then
    GUI.Dialog(RsHTMLHelp2Credentials, dtInformation, [drOK]);

  // RegHelper.exe manifest requires elevation on Vista
  if IsAdministrator or IsWinVista or IsWinServer2008 then
    Verb := 'open'
  else
    Verb := 'runas';

  Result := JclShell.ShellExecAndWait(JclBinDir + '\RegHelper.exe', Parameters, Verb, SW_HIDE, JclPath + 'help\');

  // step 4: examine output
  if Result then
  begin
    if not DisplayErrors then
      Exit;
    Sleep(500); // wait possible antivirus lock
    ResultLines := TJclMappedTextReader.Create(LogFileName);
    try
      while not ResultLines.Eof do
      begin
        ProgramResult := ResultLines.ReadLn;
        if AnsiPos('ERROR', AnsiUpperCase(ProgramResult)) > 0 then
        begin
          Result := False;
          if Assigned(GUI) then
            GUI.Dialog('RegHelper raised an error while executing RegHelp command: ' + AnsiLineBreak + ProgramResult, dtError, [drCancel]);
        end;
      end;
    finally
      ResultLines.Free;
    end;
  end
  else
    GUI.Dialog('Fatal error: failed to execute RegHelp utility', dtError, [drOK]);
end;

procedure TJclDistribution.RegHelpInternalAdd(Command: Integer;
  Arguments: string; DoNotRepeatCommand: Boolean);
var
  Index: Integer;
  AObject: TObject;
begin
  Index := 0;
  while Index <= FRegHelpCommands.Count do
  begin
    if Index = FRegHelpCommands.Count then
    begin
      FRegHelpCommands.AddObject(Arguments, TObject(Command));
      Break;
    end;
    AObject := FRegHelpCommands.Objects[Index];
    if (Integer(AObject) = Command) and
      (DoNotRepeatCommand or (FRegHelpCommands.Strings[Index] = Arguments)) then
      Break;
    if Integer(AObject) > Command then
    begin
      FRegHelpCommands.InsertObject(Index, Arguments, TObject(Command));
      Break;
    end;
    Inc(Index);
  end;
end;

procedure TJclDistribution.RegHelpPlugNameSpaceIn(const SourceNameSpace,
  TargetNameSpace: WideString);
begin
  RegHelpInternalAdd(RHPlugNameSpace, Format('%s;%s', [SourceNameSpace, TargetNameSpace]), False);
end;

procedure TJclDistribution.RegHelpRegisterHelpFile(const NameSpace,
  Identifier: WideString; const LangId: Integer; const HxSFile,
  HxIFile: WideString);
begin
  RegHelpInternalAdd(RHRegisterFile, Format('%s;%s;%d;%s;%s', [NameSpace, Identifier, LangId, HxSFile, HxIFile]), False);
end;

procedure TJclDistribution.RegHelpRegisterNameSpace(const Name, Collection,
  Description: WideString);
begin
  RegHelpInternalAdd(RHRegisterNameSpace, Format('%s;%s;%s', [Name, Collection, Description]), False);
end;

procedure TJclDistribution.RegHelpUnPlugNameSpace(const SourceNameSpace,
  TargetNameSpace: WideString);
begin
  RegHelpInternalAdd(RHUnplugNameSpace, Format('%s;%s', [SourceNameSpace, TargetNameSpace]), False);
end;

procedure TJclDistribution.RegHelpUnregisterHelpFile(const NameSpace,
  Identifier: WideString; const LangId: Integer);
begin
  RegHelpInternalAdd(RHUnregisterFile, Format('%s;%s;%d', [NameSpace, Identifier, LangId]), False);
end;

procedure TJclDistribution.RegHelpUnregisterNameSpace(const Name: WideString);
begin
  RegHelpInternalAdd(RHUnregisterNameSpace, Name, False);
end;
{$ENDIF MSWINDOWS}

procedure TJclDistribution.Uninstall;
var
  I: Integer;
  Success: Boolean;
  AInstallation: TJclInstallation;
begin
  try
    if RadToolInstallations.AnyInstanceRunning {$IFDEF MSWINDOWS} and not IsDebuggerAttached {$ENDIF} then
    begin
      if Assigned(GUI) then
        GUI.Dialog(RsCloseRADTool, dtError, [drCancel]);
      Exit;
    end;

    if Assigned(GUI) then
      GUI.Status := 'Initializing JCL uninstallation process';

    {$IFDEF MSWINDOWS}
    RegHelpClearCommands;
    {$ENDIF MSWINDOWS}

    Success := True;
    for I := 0 to TargetInstallCount - 1 do
    begin
      AInstallation := TargetInstalls[I];
      AInstallation.Silent := False;
      if AInstallation.Enabled and (not AInstallation.RemoveSettings) or not AInstallation.Uninstall(True) then
        Success := False;
    end;

    {$IFDEF MSWINDOWS}
    RegHelpExecuteCommands(False);
    {$ENDIF MSWINDOWS}

    if Assigned(GUI) then
    begin
      if Success then
        GUI.Dialog('Uninstallation success', dtInformation, [drOK])
      else
        GUI.Dialog('Uninstallation failed, see logs for details', dtError, [drOK]);
    end;
  finally
    if Assigned(GUI) then
      GUI.Status := 'Uninstallation finished';
  end;
end;

initialization
  JediInstall.InstallCore.AddProduct(TJclDistribution.Create);

end.
