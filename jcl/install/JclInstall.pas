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
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
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
  TInstallerOption = (
    joJediCodeLibrary,
      joJCLDef,
        joJCLDefMath,
        joJCLDefDebug,
        joJCLDefEDI,
        joJCLDefPCRE,
        joJCLDefBZip2,
        joJCLDefZLib,
        joJCLDefUnicode,
        joJCLDefContainer,
        joJCLDef7z,
        joJCLDefThreadSafe,
        joJCLDefDropObsoleteCode,
        joJCLDefUnitVersioning,
        joJCLDefMathPrecSingle,
        joJCLDefMathPrecDouble,
        joJCLDefMathPrecExtended,
        joJCLDefMathExtremeValues,
        joJCLDefHookDllExceptions,
        joJCLDefDebugNoBinary,
        joJCLDefDebugNoTD32,
        joJCLDefDebugNoMap,
        joJCLDefDebugNoExports,
        joJCLDefDebugNoSymbols,
        joJCLDefEDIWeakPackageUnits,
        joJCLDefPCREStaticLink,
        joJCLDefPCRELinkDLL,
        joJCLDefPCRELinkOnRequest,
        joJCLDefBZip2StaticLink,
        joJCLDefBZip2LinkDLL,
        joJCLDefBZip2LinkOnRequest,
        joJCLDefZLibStaticLink,
        joJCLDefZLibLinkDLL,
        joJCLDefZLibLinkOnRequest,
        joJCLDefUnicodeSilentFailure,
        joJCLDefUnicodeRawData,
        joJCLDefUnicodeZLibData,
        joJCLDefUnicodeBZip2Data,
        joJCLDefContainerAnsiStr,
        joJCLDefContainerWideStr,
        joJCLDefContainerUnicodeStr,
        joJCLDefContainerNoStr,
        //joJCLDef7zStaticLink,
        joJCLDef7zLinkDLL,
        joJCLDef7zLinkOnRequest,
      joJCLEnvironment,
        joJCLEnvLibPath,
        joJCLEnvBrowsingPath,
        joJCLEnvDebugDCUPath,
      joJCLMake,
        joJCLMakeRelease,
          joJCLMakeReleaseVClx,
          joJCLMakeReleaseVCL,
        joJCLMakeDebug,
          joJCLMakeDebugVClx,
          joJCLMakeDebugVCL,
        joJCLCopyHppFiles,
        joJCLCheckHppFiles,
      joJCLPackages,
        joJCLVclPackage,
        joJCLClxPackage,
        joJCLDualPackages,
        joJCLCopyPackagesHppFiles,
        joJCLPdbCreate,
        joJCLMapCreate,
          joJCLJdbgCreate,
          joJCLJdbgInsert,
          joJCLMapDelete,
        joJCLExperts,
          joJCLExpertsDsgnPackages,
          joJCLExpertsDLL,
          joJCLExpertDebug,
          joJCLExpertAnalyzer,
          joJCLExpertFavorite,
          joJCLExpertRepository,
          joJCLExpertThreadNames,
          joJCLExpertUses,
          joJCLExpertSimdView,
          joJCLExpertVersionControl,
          joJCLExpertStackTraceViewer,
      joJCLExceptDlg,
        joJCLExceptDlgVCL,
        joJCLExceptDlgVCLSnd,
        joJCLExceptDlgCLX,
      joJCLHelp,
        joJCLHelpHlp,
        joJCLHelpChm,
        joJCLHelpHxS,
        joJCLHelpHxSPlugin,
      joJCLMakeDemos);

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
    FRuntimeInstallation: Boolean;
    FProfilesTargets: TObjectList;
    procedure AddDemo(const Directory: string; const FileInfo: TSearchRec);
    procedure AddDemos(const Directory: string);
    function GetDemoList: TStringList;
    function MakePath(const FormatStr: string): string;
    procedure WriteLog(const Msg: string);
    function GetEnabled: Boolean;
    function GetIsProfileEnabled(Index: Integer): Boolean;
    function GetProfilesTarget(Index: Integer): TJclBorRADToolInstallation;
  protected
    // if CLRVersion = '' then it is a native installation
    constructor Create(JclDistribution: TJclDistribution;
      InstallTarget: TJclBorRADToolInstallation; const ACLRVersion: string = '';
      ATargetPlatform: TJclBorPlatform = bp32bit; AGUIPage: IJediInstallPage = nil);
    function CompileLibraryUnits(const SubDir: string; Debug: Boolean): Boolean;
    {$IFDEF MSWINDOWS}
    function CompileCLRPackage(const Name: string): Boolean;
    {$ENDIF MSWINDOWS}
    function CompilePackage(const Name: string): Boolean;
    function CompileApplication(FileName: string): Boolean;
    function DeletePackage(const Name: string): Boolean;
    procedure ConfigureBpr2Mak(const PackageFileName: string);
    {$IFDEF MSWINDOWS}
    function CompileExpert(const Name: string): Boolean;
    {$ENDIF MSWINDOWS}
    
    function GetBplPath: string;
    function GetDcpPath: string;
    function GetOptionChecked(Option: TInstallerOption): Boolean; overload;
    function GetOptionCheckedById(Id: Integer): Boolean; overload;
    procedure MarkOptionBegin(Id: Integer); overload;
    procedure MarkOptionBegin(Option: TInstallerOption); overload;
    procedure MarkOptionEnd(Id: Integer; Success: Boolean); overload;
    procedure MarkOptionEnd(Option: TInstallerOption; Success: Boolean); overload;
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
    property OptionChecked[Option: TInstallerOption]: Boolean read GetOptionChecked;
    property LogFileName: string read FLogFileName;
    property Silent: Boolean read FSilent write FSilent;
    property RuntimeInstallation: Boolean read FRuntimeInstallation; // false for C#Builder 1, Delphi 8 and .net targets

    property IsProfileEnabled[Index: Integer]: Boolean read GetIsProfileEnabled;
    property ProfileTargets[Index: Integer]: TJclBorRADToolInstallation read GetProfilesTarget;
  end;

  TJclDistribution = class (TInterfacedObject, IJediProduct)
  private
    FJclPath: string;
    FJclBinDir: string;
    FLibReleaseDirMask: string;
    FLibDebugDirMask: string;
    FJclSourceDir: string;
    FJclIncludeDir: string;
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
    FProfilesPage: IJediProfilesPage;
    function GetVersion: string;
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
    function Install: Boolean;
    function Uninstall: Boolean;
    procedure Close;

    property JclPath: string read FJclPath;
    property JclBinDir: string read FJclBinDir;
    property LibReleaseDirMask: string read FLibReleaseDirMask;
    property LibDebugDirMask: string read FLibDebugDirMask;
    property JclSourceDir: string read FJclSourceDir;
    property JclIncludeDir: string read FJclIncludeDir;
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

    property ProfilesPage: IJediProfilesPage read FProfilesPage;
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
  RsCaptionDef                 = 'Conditional defines';
  RsCaptionDefThreadSafe       = 'Enable thread safe code';
  RsCaptionDefDropObsoleteCode = 'Drop obsolete code';
  RsCaptionDefUnitVersioning   = 'Include Unit Versioning';
  // math options
  RsCaptionDefMath              = 'Math options';
  RsCaptionDefMathPrecSingle    = 'Single float precision';
  RsCaptionDefMathPrecDouble    = 'Double float precision';
  RsCaptionDefMathPrecExtended  = 'Extended float precision';
  RsCaptionDefMathExtremeValues = 'Support for infinite and NaN';
  // debug options
  RsCaptionDefDebug             = 'Debug and exception hooking options';
  RsCaptionDefHookDllExceptions = 'Hook exceptions in DLL';
  RsCaptionDefDebugNoBinary     = 'No debug source from JEDI debug informations';
  RsCaptionDefDebugNoTD32       = 'No debug source from TD32 debug symbols';
  RsCaptionDefDebugNoMap        = 'No debug source from Map files';
  RsCaptionDefDebugNoExports    = 'No debug source from function export table for libraries';
  RsCaptionDefDebugNoSymbols    = 'No debug source from Microsoft debug symbols';
  // EDI options
  RsCaptionDefEDI                 = 'EDI options';
  RsCaptionDefEDIWeakPackageUnits = 'EDI weak package units';
  // PCRE options
  RsCaptionDefPCRE              = 'PCRE options';
  RsCaptionDefPCREStaticLink    = 'Static link to PCRE code';
  RsCaptionDefPCRELinkDLL       = 'Static bind to pcre.dll';
  RsCaptionDefPCRELinkOnRequest = 'Late bind to pcre.dll';
  // BZip2 options
  RsCaptionDefBZip2              = 'BZip2 options';
  RsCaptionDefBZip2StaticLink    = 'Static link to BZip2 code';
  RsCaptionDefBZip2LinkDLL       = 'Static bind to bzip2.dll';
  RsCaptionDefBZip2LinkOnRequest = 'Late bind to bzip2.dll';
  // ZLib options
  RsCaptionDefZLib              = 'ZLib options';
  RsCaptionDefZLibStaticLink    = 'Static link to ZLib code';
  RsCaptionDefZLibLinkDLL       = 'Static bind to zlib1.dll';
  RsCaptionDefZLibLinkOnRequest = 'Late bind to zlib1.dll';
  // Unicode options
  RsCaptionDefUnicode              = 'Unicode options';
  RsCaptionDefUnicodeSilentFailure = 'Silent failure';
  RsCaptionDefUnicodeRawData       = 'Uncompressed Unicode data';
  RsCaptionDefUnicodeZLibData      = 'Compressed data using zlib';
  RsCaptionDefUnicodeBZip2Data     = 'Compressed data using bzip2';
  // Container options
  RsCaptionDefContainer           = 'Container options';
  RsCaptionDefContainerAnsiStr    = 'Alias AnsiString containers to String containers';
  RsCaptionDefContainerWideStr    = 'Alias WideString containers to String containers';
  RsCaptionDefContainerUnicodeStr = 'Alias UnicodeString containers to String containers (Delphi 2008 only)';
  RsCaptionDefContainerNoStr      = 'Do not alias anything';
  // 7Z options
  RsCaptionDef7z               = 'Sevenzip options';
  //RsCaptionDef7zStaticLink     = 'Static link to Sevenzip code (not supported yet)';
  RsCaptionDef7zLinkDLL        = 'Static bind to 7z.dll';
  RsCaptionDef7zLinkOnRequest  = 'Late bind to 7z.dll';
  
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
  RsCaptionVclPackage           = 'VCL Package';
  RsCaptionClxPackage           = 'CLX package';
  RsCaptionDualPackages         = 'Dual packages';
  RsCaptionCopyPackagesHppFiles = 'Output HPP files to %s';

  // exception dialogs
  RsCaptionExceptDlg       = 'Sample Exception Dialogs in the Object Repository';
  RsCaptionExceptDlgVCL    = 'VCL Exception Dialog';
  RsCaptionExceptDlgVCLSnd = 'VCL Exception Dialog with Send button';
  RsCaptionExceptDlgCLX    = 'CLX Exception Dialog';

  // experts
  RsCaptionExperts                = 'IDE experts';
  RsCaptionExpertsDsgnPackages    = 'Design packages';
  RsCaptionExpertsDLL             = 'DLL experts';
  RsCaptionExpertDebug            = 'Debug Extension';
  RsCaptionExpertAnalyzer         = 'Project Analyzer';
  RsCaptionExpertFavorite         = 'Favorite combobox in Open/Save dialogs';
  RsCaptionExpertRepository       = 'Exception dialog expert';
  RsCaptionExpertThreadNames      = 'Displaying thread names in Thread Status window';
  RsCaptionExpertUses             = 'Uses Wizard';
  RsCaptionExpertSimdView         = 'Debug window for XMM registers';
  RsCaptionExpertVersionControl   = 'Version control';
  RsCaptionExpertStackTraceViewer = 'Stack Trace Viewer';

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
  RsHintDef                 = 'Enable or disable specific features to be compiled';
  RsHintDefThreadSafe       = 'Conditionally some pieces of code to be thread safe, the ThreadSafe.txt file contains more informations about this feature';
  RsHintDefDropObsoleteCode = 'Do not compile deprecated code';
  RsHintDefUnitVersioning   = 'Includes JCL Unit Versioning informations into each JCL unit (see also JclUnitVersioning.pas)';
  // math options
  RsHintDefMath              = 'Math specific options (JclMath.pas)';
  RsHintDefMathPrecSingle    = 'type Float = Single';
  RsHintDefMathPrecDouble    = 'type Float = Double';
  RsHintDefMathPrecExtended  = 'type Float = Extended';
  RsHintDefMathExtremeValues = 'Exp en Power functions accept and return infinite and NaN';
  // Debug options
  RsHintDefDebug             = 'Debug and exception hooking specific options (JclDebug.pas and JclHookExcept.pas)';
  RsHintDefHookDllExceptions = 'Hook exceptions raised in DLL compiled with the JCL';
  RsHintDefDebugNoBinary     = 'Disable support for JDBG files';
  RsHintDefDebugNoMap        = 'Disable support for MAP files';
  RsHintDefDebugNoTD32       = 'Disable support for TD32 informations';
  RsHintDefDebugNoExports    = 'Disable support for export names of libraries';
  RsHintDefDebugNoSymbols    = 'Disable support for Microsoft debug symbols (PDB and DBG files)';
  // EDI options
  RsHintDefEDI                 = 'EDI specific options (JclEDI*.pas)';
  RsHintDefEDIWeakPackageUnits = 'Mark EDI units as weak package units (check if you use the original EDI package)';
  // PCRE options
  RsHintDefPCRE              = 'PCRE specific options (pcre.pas and JclPCRE.pas)';
  RsHintDefPCREStaticLink    = 'Code from PCRE is linked into JCL binaries';
  RsHintDefPCRELinkDLL       = 'JCL binaries require pcre.dll to be present';
  RsHintDefPCRELinkOnRequest = 'JCL binaries require pcre.dll when calling PCRE functions';
  // BZip2 options
  RsHintDefBZip2              = 'BZip2 specific options (bzip2.pas)';
  RsHintDefBZip2StaticLink    = 'Code from BZip2 is linked into JCL binaries';
  RsHintDefBZip2LinkDLL       = 'JCL binaries require bzip2.dll to be present';
  RsHintDefBZip2LinkOnRequest = 'JCL binaries require bzip2.dll when calling BZip2 functions';
  // ZLib options
  RsHintDefZLib              = 'ZLib specific options (zlibh.pas)';
  RsHintDefZLibStaticLink    = 'Code from ZLib is linked into JCL binaries';
  RsHintDefZLibLinkDLL       = 'JCL binaries require zlib1.dll to be present';
  RsHintDefZLibLinkOnRequest = 'JCL binaries require zlib1.dll when calling ZLib functions';
  // Unicode options
  RsHintDefUnicode              = 'Unicode specific option (JclUnicode.pas)';
  RsHintDefUnicodeSilentFailure = 'Insert a replacement character if sequence is corrupted rather than raising an exception';
  RsHintDefUnicodeRawData       = 'Link resource containing uncompressed Unicode data (bigger executable size)';
  RsHintDefUnicodeZLibData      = 'Link resource containing Unicode data compressed with ZLib';
  RsHintDefUnicodeBZip2Data     = 'Link resource containing Unicode data compressed with BZip2';
  // Container options
  RsHintDefContainer           = 'Container specific options';
  RsHintDefContainerAnsiStr    = 'Define TJclStr* containers as alias of TJclAnsiStr* containers';
  RsHintDefContainerWideStr    = 'Define TJclStr* containers as alias of TJclWideStr* containers';
  RsHintDefContainerUnicodeStr = 'Define TJClStr* containers as alias of TJclUnicodeStr* containers';
  RsHintDefContainerNoStr      = 'Do not define TJclStr* containers';
  // 7Z options
  RsHintDef7z               = 'Sevenzip specific options (sevenzip.pas)';
  //RsHintDef7zStaticLink     = 'Code from Sevenzip is linked into JCL binaries';
  RsHintDef7zLinkDLL        = 'JCL binaries require 7z.dll to be present';
  RsHintDef7zLinkOnRequest  = 'JCL binaries require 7z.dll when calling Sevenzip functions';

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
  RsHintEnvDebugDCUPath = 'This is a prerequisite for using the precompiled JCL debug units by means of the respective' + NativeLineBreak +
    'Project Options|Compiler switch. See "Make library units/Debug" option below.';

  // make units
  RsHintMake            = 'Generate .dcu and .dpu (Kylix only) files.' + NativeLineBreak + 'Recommended.';
  RsHintMakeRelease     = 'Make precompiled units for release, i.e. optimized, w/o debug information.';
  RsHintMakeReleaseVcl  = 'Make precompiled VCL units for release';
  RsHintMakeReleaseVClx = 'Make precompiled Visual CLX units for release';
  RsHintMakeDebug       = 'Make precompiled units for debugging, i.e.optimization off, debug information included.' + NativeLineBreak +
    'When installed, available through Project Options|Compiler|Use Debug DCUs.';
  RsHintMakeDebugVcl    = 'Make precompiled VCL units for debugging';
  RsHintMakeDebugVClx   = 'Make precompiled Visual CLX units for debugging';
  RsHintCopyHppFiles    = 'Copy .hpp files into C++Builder''s include path.';
  RsHintCheckHppFiles   = 'Compile some C++ source files to verify JCL headers';

  // packages
  RsHintPackages             = 'Build and eventually install JCL runtime packages and optional IDE experts.';
  RsHintVclPackage           = 'Build JCL runtime package containing VCL extensions';
  RsHintClxPackage           = 'Build JCL runtime package containing Visual CLX extensions';
  RsHintDualPackages         = 'The same package introduce code for Delphi Win32 and C++Builder Win32';
  RsHintCopyPackagesHppFiles = 'Output .hpp files into C++Builder''s include path instead of ' +
    'the source paths.';

  // exception dialogs
  RsHintExceptDlg       = 'Add selected Exception dialogs to the Object Repository.';
  RsHintExceptDlgVCL    = 'Add VCL exception dialog to the Object Repository.';
  RsHintExceptDlgVCLSnd = 'Add VCL exception dialog with "Send Button" to the Object Repository.';
  RsHintExceptDlgCLX    = 'Add CLX exception dialog (Windows only) to the Object Repository.';

  // experts
  RsHintExperts                = 'Build and install selected IDE experts.';
  RsHintExpertsDsgnPackages    = 'Design packages containing JCL experts';
  RsHintExpertsDLL             = 'DLLs containing JCL experts';
  RsHintExpertDebug            = 'Install IDE expert which assists to insert JCL Debug information into executable files.';
  RsHintExpertAnalyzer         = 'Install IDE Project Analyzer.';
  RsHintExpertFavorite         = 'Install "Favorites" combobox in IDE Open/Save dialogs.';
  RsHintExpertRepository       = 'Repository expert to easily create exception dialogs';
  RsHintExpertThreadNames      = 'Display thread names in Thread Status window IDE extension.';
  RsHintExpertUses             = 'Install IDE Uses Wizard.';
  RsHintExpertSimdView         = 'Install a debug window of XMM registers (used by SSE instructions)';
  RsHintExpertVersionControl   = 'Integration of TortoiseCVS and TortoiseSVN in the IDE';
  RsHintExpertStackTraceViewer = 'Install an IDE expert which shows the JCL Debug stack trace information.';

  // help
  RsHintHelp          = 'Install JCL help files.';
  RsHintHelpHlp       = 'Customize Borland Open Help to include JCL help files.';
  RsHintHelpChm       = 'Compiled help files won''t be merged with the IDE help';
  RsHintHelpHxS       = 'Register Help 2.0 files';
  RsHintHelpHxSPlugin = 'Register Help 2.0 files as a plugin for the Borland.BDS* namespace';

  // demos
  RsHintMakeDemos = 'Make JCL demo applications';

// warning messages
  RsWarningPackageNodeNotSelected = 'The "Packages" or "VCL package" nodes are not selected.' + sLineBreak +
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
  OptionData: array[TInstallerOption] of TOptionRec =
    (
      (Id: -1; Caption: RsCaptionLibrary; Hint: RsHintLibrary), // joLibrary
      (Id: -1; Caption: RsCaptionDef; Hint: RsHintDef), // joDef
      (Id: -1; Caption: RsCaptionDefMath; Hint: RsHintDefMath), // joDefMath
      (Id: -1; Caption: RsCaptionDefDebug; Hint: RsHintDefDebug), // joDefDebug
      (Id: -1; Caption: RsCaptionDefEDI; Hint: RsHintDefEDI), // joDefEDI
      (Id: -1; Caption: RsCaptionDefPCRE; Hint: RsHintDefPCRE), // joDefPCRE
      (Id: -1; Caption: RsCaptionDefBZip2; Hint: RsHintDefBZip2), // joDefBZip2
      (Id: -1; Caption: RsCaptionDefZLib; Hint: RsHintDefZLib), // joDefZLib
      (Id: -1; Caption: RsCaptionDefUnicode; Hint: RsHintDefUnicode), // joDefUnicode
      (Id: -1; Caption: RsCaptionDefContainer; Hint: RsHintDefContainer), // joDefContainer
      (Id: -1; Caption: RsCaptionDef7z; Hint: RsHintDef7z), // joDef7z
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
      (Id: -1; Caption: RsCaptionDefZLibStaticLink; Hint: RsHintDefZLibStaticLink), // joDefZLibStaticLink
      (Id: -1; Caption: RsCaptionDefZLibLinkDLL; Hint: RsHintDefZLibLinkDLL), // joDefZLibLinkDLL
      (Id: -1; Caption: RsCaptionDefZLibLinkOnRequest; Hint: RsHintDefZLibLinkOnRequest), // joDefZLibLinkOnRequest
      (Id: -1; Caption: RsCaptionDefUnicodeSilentFailure; Hint: RsHintDefUnicodeSilentFailure), // joDefUnicodeSilentFailure
      (Id: -1; Caption: RsCaptionDefUnicodeRawData; Hint: RsHintDefUnicodeRawData), // joDefUnicodeRawData
      (Id: -1; Caption: RsCaptionDefUnicodeZLibData; Hint: RsHintDefUnicodeZLibData), // joDefUnicodeZLibData
      (Id: -1; Caption: RsCaptionDefUnicodeBZip2Data; Hint: RsHintDefUnicodeBZip2Data), // joDefUnicodeBZip2Data
      (Id: -1; Caption: RsCaptionDefContainerAnsiStr; Hint: RsHintDefContainerAnsiStr), // joDefContainerAnsiStr
      (Id: -1; Caption: RsCaptionDefContainerWideStr; Hint: RsHintDefContainerWideStr), // joDefContainerWideStr
      (Id: -1; Caption: RsCaptionDefContainerUnicodeStr; Hint: RsHintDefContainerUnicodeStr), // joDefContainerUnicodeStr
      (Id: -1; Caption: RsCaptionDefContainerNoStr; Hint: RsHintDefContainerNoStr), // joDefContainerNoStr
      //(Id: -1; Caption: RsCaptionDef7zStaticLink; Hint: RsHintDef7zStaticLink), // joDef7zStaticLink
      (Id: -1; Caption: RsCaptionDef7zLinkDLL; Hint: RsHintDef7zLinkDLL), // joDef7zLinkDLL
      (Id: -1; Caption: RsCaptionDef7zLinkOnRequest; Hint: RsHintDef7zLinkOnRequest), // joDef7zLinkOnRequest
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
      (Id: -1; Caption: RsCaptionVclPackage; Hint: RsHintVclPackage), // joVclPackage
      (Id: -1; Caption: RsCaptionClxPackage; Hint: RsHintClxPackage), // joClxPackage
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
      (Id: -1; Caption: RsCaptionExpertStackTraceViewer; Hint: RsHintExpertStackTraceViewer), //joExpertStackTraceViewer
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

  // native packages
  JclDpk           = 'Jcl';
  JclContainersDpk = 'JclContainers';
  JclVclDpk        = 'JclVcl';
  JclVClxDpk       = 'JclVClx';

  // .net packages
  JediJclDpk           = 'Jedi.Jcl';
  JediJclContainersDpk = 'Jedi.JclContainers';

  JclExpertBase             = 'JclBaseExpert';
  JclExpertDebug            = 'JclDebugExpert';
  JclExpertAnalyzer         = 'JclProjectAnalysisExpert';
  JclExpertFavorite         = 'JclFavoriteFoldersExpert';
  JclExpertRepository       = 'JclRepositoryExpert';
  JclExpertThrNames         = 'JclThreadNameExpert';
  JclExpertUses             = 'JclUsesExpert';
  JclExpertSimdView         = 'JclSIMDViewExpert';
  JclExpertVersionControl   = 'JclVersionControlExpert';
  JclExpertStackTraceViewer = 'JclStackTraceViewerExpert';

  SupportedExperts: array [joJCLExperts..joJCLExpertStackTraceViewer] of string =
    (
      JclExpertBase, '', '', JclExpertDebug, JclExpertAnalyzer,
      JclExpertFavorite, JclExpertRepository, JclExpertThrNames,
      JclExpertUses, JclExpertSimdView, JclExpertVersionControl, JclExpertStackTraceViewer
    );

  OldExperts: array [0..6] of string =
    ( 'JclDebugIde', 'ProjectAnalyzer', 'IdeOpenDlgFavorite', 'ThreadNameExpert', 'JediUses', 'JclSIMDView', 'JclVersionControl' );

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

  DailyRevisionFileName = 'jcl-revision.txt';
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

  ProfilesSectionName = 'Profiles';

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

  // exclude C#Builder 1, Delphi 8 and .net targets
  FRunTimeInstallation := (CLRVersion <> '') or (Target.RadToolKind <> brBorlandDevStudio)
    or ((Target.VersionNumber >= 3) and (bpDelphi32 in Target.Personalities));

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

  FProfilesTargets := TObjectList.Create;
  FProfilesTargets.Count := InstallCore.ProfilesManager.ProfileCount;
  FProfilesTargets.OwnsObjects := False; 
end;

destructor TJclInstallation.Destroy;
var
  Index: Integer;
begin
  if Assigned(FProfilesTargets) then
    for Index := 0 to FProfilesTargets.Count - 1 do
      if FProfilesTargets.Items[Index] <> Target then
        FProfilesTargets.Items[Index].Free;
  FProfilesTargets.Free;
  FDemoList.Free;
  FLogLines.Free;
  FGUI := nil;
  FGUIPage := nil;

  inherited Destroy;
end;

function TJclInstallation.GetEnabled: Boolean;
begin
  Result := OptionCheckedById[OptionData[joJediCodeLibrary].Id];
end;

function TJclInstallation.GetIsProfileEnabled(Index: Integer): Boolean;
var
  AProfilesPage: IJediProfilesPage;
  ASettings: IJediConfiguration;
begin
  AProfilesPage := FDistribution.ProfilesPage;
  ASettings := InstallCore.Configuration;
  if AProfilesPage <> nil then
    Result := AProfilesPage.IsProfileEnabled[Index]
  else
  if ASettings <> nil then
    Result := ASettings.OptionAsBoolByName[ProfilesSectionName, InstallCore.ProfilesManager.ProfileNames[Index]]
  else
    Result := True;
end;

function TJclInstallation.GetOptionChecked(Option: TInstallerOption): Boolean;
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

function TJclInstallation.GetProfilesTarget(Index: Integer): TJclBorRADToolInstallation;
{$IFDEF MSWINDOWS}
var
  RootKey: LongWord;
begin
  if FProfilesTargets.Items[Index] = nil then
  begin
    RootKey := InstallCore.ProfilesManager.GetProfileKey(Index);
    if RootKey <> HKCU then
    begin
      FProfilesTargets.Items[Index] := TJclBorRADToolInstallationClass(Target.ClassType).Create(Target.ConfigDataLocation, RootKey);
      TJclBorRADToolInstallation(FProfilesTargets.Items[Index]).OutputCallback := Target.OutputCallback;
    end
    else
      FProfilesTargets.Items[Index] := Target;
  end;
  Result := FProfilesTargets.Items[Index] as TJclBorRADToolInstallation;
end;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
begin
  Result := nil;
end;
{$ENDIF LINUX}

procedure TJclInstallation.MarkOptionBegin(Id: Integer);
begin
  if Assigned(GUIPage) then
    GUIPage.MarkOptionBegin(Id);
  if Assigned(GUI) then
    GUI.Status := InstallCore.InstallOptionName[Id];
end;

procedure TJclInstallation.MarkOptionBegin(Option: TInstallerOption);
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

procedure TJclInstallation.MarkOptionEnd(Option: TInstallerOption; Success: Boolean);
begin
  if Assigned(GUIPage) then
  begin
    GUIPage.MarkOptionEnd(OptionData[Option].Id, not Success);
    if Assigned(GUI) then
      GUI.Progress := Round(100 * (Distribution.NbInstalled + GUIPage.Progress / 100) / Distribution.NbEnabled);
  end;
end;

procedure TJclInstallation.Init;
  procedure AddOption(Option: TInstallerOption; GUIOptions: TJediInstallGUIOptions;
    Parent: Integer; const Caption, Hint: string); overload;
  begin
    GUIPage.AddInstallOption(OptionData[Option].Id, GUIOptions, Caption, Hint, Parent);
  end;

  procedure AddOption(Option: TInstallerOption; GUIOptions: TJediInstallGUIOptions;
    Parent: Integer); overload;
  begin
    AddOption(Option, GUIOptions, Parent, OptionData[Option].Caption, OptionData[Option].Hint);
  end;

  procedure AddOption(Option: TInstallerOption; GUIOptions: TJediInstallGUIOptions;
    Parent: TInstallerOption); overload;
  begin
    AddOption(Option, GUIOptions, OptionData[Parent].Id, OptionData[Option].Caption, OptionData[Option].Hint);
  end;

  procedure AddDefOptions(Parent: TInstallerOption);
  begin
    AddOption(joJCLDefThreadSafe, [goChecked], Parent);
    AddOption(joJCLDefDropObsoleteCode, [goChecked], Parent);
    if CLRVersion = '' then
      AddOption(joJCLDefUnitVersioning, [goChecked], Parent);

    AddOption(joJCLDefMath, [goChecked], Parent);
    AddOption(joJCLDefMathPrecSingle, [goRadioButton], joJCLDefMath);
    AddOption(joJCLDefMathPrecDouble, [goRadioButton], joJCLDefMath);
    AddOption(joJCLDefMathPrecExtended, [goRadioButton, goChecked], joJCLDefMath);
    AddOption(joJCLDefMathExtremeValues, [goChecked], joJCLDefMath);

    AddOption(joJCLDefContainer, [goChecked], Parent);
    if (Target.RadToolKind = brBorlandDevStudio) and (Target.IDEVersionNumber >= 6) then
    begin
      AddOption(joJCLDefContainerAnsiStr, [goRadioButton], joJCLDefContainer);
      AddOption(joJCLDefContainerWideStr, [goRadioButton], joJCLDefContainer);
      AddOption(joJCLDefContainerUnicodeStr, [goRadioButton, goChecked], joJCLDefContainer);
    end
    else
    if CLRVersion = '' then
    begin
      AddOption(joJCLDefContainerAnsiStr, [goRadioButton, goChecked], joJCLDefContainer);
      AddOption(joJCLDefContainerWideStr, [goRadioButton], joJCLDefContainer);
      AddOption(joJCLDefContainerUnicodeStr, [goRadioButton], joJCLDefContainer);
    end
    else
    begin
      AddOption(joJCLDefContainerAnsiStr, [goRadioButton], joJCLDefContainer);
      AddOption(joJCLDefContainerWideStr, [goRadioButton, goChecked], joJCLDefContainer);
      AddOption(joJCLDefContainerUnicodeStr, [goRadioButton], joJCLDefContainer);
    end;
    AddOption(joJCLDefContainerNoStr, [goRadioButton], joJCLDefContainer);

    if CLRVersion = '' then   // these units are not CLR compliant
    begin
      {$IFDEF MSWINDOWS}
      // debug options
      AddOption(joJCLDefDebug, [goNoAutoCheck], Parent);
      AddOption(joJCLDefHookDllExceptions, [goNoAutoCheck], joJCLDefDebug);
      AddOption(joJCLDefDebugNoBinary, [goNoAutoCheck], joJCLDefDebug);
      AddOption(joJCLDefDebugNoTD32, [goNoAutoCheck], joJCLDefDebug);
      AddOption(joJCLDefDebugNoMap, [goNoAutoCheck], joJCLDefDebug);
      AddOption(joJCLDefDebugNoExports, [goNoAutoCheck], joJCLDefDebug);
      AddOption(joJCLDefDebugNoSymbols, [goNoAutoCheck], joJCLDefDebug);
      {$ENDIF MSWINDOWS}
      // EDI options
      AddOption(joJCLDefEDI, [goNoAutoCheck], Parent);
      AddOption(joJCLDefEDIWeakPackageUnits, [goNoAutoCheck], joJCLDefEDI);
      // PCRE options
      AddOption(joJCLDefPCRE, [goChecked], Parent);
      if Target.RadToolKind = brBorlandDevStudio then
      begin
        AddOption(joJCLDefPCREStaticLink, [goRadioButton, goChecked], joJCLDefPCRE);
        AddOption(joJCLDefPCRELinkOnRequest, [goRadioButton], joJCLDefPCRE);
      end
      else
        AddOption(joJCLDefPCRELinkOnRequest, [goRadioButton, goChecked], joJCLDefPCRE);
      AddOption(joJCLDefPCRELinkDLL, [goRadioButton], joJCLDefPCRE);
      // BZip2 options
      AddOption(joJCLDefBZip2, [goChecked], Parent);
      AddOption(joJCLDefBZip2StaticLink, [goRadioButton, goChecked], joJCLDefBZip2);
      AddOption(joJCLDefBZip2LinkOnRequest, [goRadioButton], joJCLDefBZip2);
      AddOption(joJCLDefBZip2LinkDLL, [goRadioButton], joJCLDefBZip2);
      // ZLib options
      AddOption(joJCLDefZLib, [goChecked], Parent);
      AddOption(joJCLDefZLibStaticLink, [goRadioButton, goChecked], joJCLDefZLib);
      AddOption(joJCLDefZLibLinkOnRequest, [goRadioButton], joJCLDefZLib);
      AddOption(joJCLDefZLibLinkDLL, [goRadioButton], joJCLDefZLib);
      // Unicode options
      AddOption(joJCLDefUnicode, [goChecked], Parent);
      AddOption(joJCLDefUnicodeSilentFailure, [goChecked], joJCLDefUnicode);
      AddOption(joJCLDefUnicodeRawData, [goRadioButton, goChecked], joJCLDefUnicode);
      AddOption(joJCLDefUnicodeZLibData, [goRadioButton], joJCLDefUnicode);
      AddOption(joJCLDefUnicodeBZip2Data, [goRadioButton], joJCLDefUnicode);
      {$IFDEF MSWINDOWS}
      // Sevenzip options
      AddOption(joJCLDef7z, [goChecked], Parent);
      //AddOption(joJCLDef7zStaticLink, [goRadioButton], joDef7z);
      AddOption(joJCLDef7zLinkOnRequest, [goRadioButton, goChecked], joJCLDef7z);
      AddOption(joJCLDef7zLinkDLL, [goRadioButton], joJCLDef7z);
      {$ENDIF MSWINDOWS}
    end;
  end;

  procedure AddEnvOptions(Parent: TInstallerOption);
  begin
    AddOption(joJCLEnvLibPath, [goChecked], Parent);
    AddOption(joJCLEnvBrowsingPath, [goChecked], Parent);
    if not Target.IsTurboExplorer then
      AddOption(joJCLEnvDebugDCUPath, [goChecked], Parent);
  end;

  procedure AddMakeOptions(Parent: TInstallerOption);
  begin
    AddOption(joJCLMakeRelease, [goStandAloneParent, goExpandable, goChecked], Parent);
    AddOption(joJCLMakeDebug, [goStandAloneParent, goExpandable, goChecked], Parent);

    if CLRVersion = '' then
    begin
      if Target.SupportsVisualCLX then
      begin
        AddOption(joJCLMakeReleaseVClx, [goChecked], joJCLMakeRelease);
        AddOption(joJCLMakeDebugVClx, [goChecked], joJCLMakeDebug);
      end;

      if Target.SupportsVCL then
      begin
        AddOption(joJCLMakeReleaseVCL, [goChecked], joJCLMakeRelease);
        AddOption(joJCLMakeDebugVCL, [goChecked], joJCLMakeDebug);
      end;

      if bpBCBuilder32 in Target.Personalities then
      begin
        AddOption(joJCLCopyHppFiles, [goChecked], OptionData[joJCLMake].Id,
          Format(OptionData[joJCLCopyHppFiles].Caption, [Target.VclIncludeDir]),
          OptionData[joJCLCopyHppFiles].Hint);
        AddOption(joJCLCheckHppFiles, [goChecked], joJCLMake);
      end;
    end;
  end;

  procedure AddHelpOptions(Parent: TInstallerOption);
  begin
    {$IFDEF MSWINDOWS}
    if Target.RadToolKind = brBorlandDevStudio then
    begin
      // TODO: expert help
      if (Target.VersionNumber >= 3) and (Distribution.JclHxSHelpFileName <> '') then
      begin
        AddOption(joJCLHelp, [goChecked], Parent);
        AddOption(joJCLhelpHxS, [goStandaloneParent,goChecked], joJCLHelp);
        AddOption(joJCLHelpHxSPlugin, [goNoAutoCheck], joJCLHelpHxS);
      end;
    end
    else
    begin
      if (Distribution.JclHlpHelpFileName <> '') or (Distribution.JclChmHelpFileName <> '') then
      begin
        AddOption(joJCLHelp, [goChecked], Parent);
        if Distribution.JclHlpHelpFileName <> '' then
          AddOption(joJCLHelpHlp, [goChecked], joJCLHelp);
        if Distribution.JclChmHelpFileName <> '' then
          AddOption(joJCLHelpChm, [goChecked], joJCLHelp);
      end;
    end;
    {$ENDIF MSWINDOWS}
  end;

  procedure AddRepositoryOptions(Parent: TInstallerOption);
  begin
    // BDS has an expert for objects in the repository
    if Target.RadToolKind <> brBorlandDevStudio then
    begin
      AddOption(joJCLExceptDlg, [], Parent);
      if Target.SupportsVCL then
      begin
        AddOption(joJCLExceptDlgVCL, [], joJCLExceptDlg);
        {$IFDEF MSWINDOWS}
        AddOption(joJCLExceptDlgVCLSnd, [], joJCLExceptDlg);
        {$ENDIF MSWINDOWS}
      end;
      if Target.SupportsVisualCLX then
        AddOption(joJCLExceptDlgCLX, [], joJCLExceptDlg);
    end;
  end;

  procedure AddPackageOptions(Parent: TInstallerOption);
  begin
    if (CLRVersion = '') and RuntimeInstallation and Target.SupportsVCL then
      AddOption(joJCLVclPackage, [goChecked], Parent);
    if (CLRVersion = '') and RuntimeInstallation and Target.SupportsVisualCLX then
      AddOption(joJCLClxPackage, [goChecked], Parent);
    if (bpBCBuilder32 in Target.Personalities) and RunTimeInstallation and (CLRVersion = '') then
    begin
      if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 4) then
      begin
        AddOption(joJCLDualPackages, [goStandAloneParent, goChecked], Parent);
        AddOption(joJCLCopyPackagesHppFiles, [goChecked], OptionData[joJCLDualPackages].Id,
          Format(OptionData[joJCLCopyPackagesHppFiles].Caption, [Target.VclIncludeDir]),
          OptionData[joJCLCopyPackagesHppFiles].Hint);
      end
      else
        AddOption(joJCLCopyPackagesHppFiles, [goChecked], OptionData[Parent].Id,
          Format(OptionData[joJCLCopyPackagesHppFiles].Caption, [Target.VclIncludeDir]),
          OptionData[joJCLCopyPackagesHppFiles].Hint);
    end;

    if CLRVersion = '' then
    begin
      AddOption(joJCLMapCreate, [goExpandable, goStandaloneParent, goNoAutoCheck], Parent);

      {$IFDEF MSWINDOWS}
      AddOption(joJCLJdbgCreate, [goExpandable, goStandaloneParent], joJCLMapCreate);
      AddOption(joJCLJdbgInsert, [goNoAutoCheck], joJCLMapCreate);
      AddOption(joJCLMapDelete, [goNoAutoCheck], joJCLMapCreate);

      {if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber = 3)
        and (Target.Edition = deStd) then
        CopyFakeXmlRtlPackage;
      TODO: CopyFakeXmlRtlPackage
      }
      {$ENDIF MSWINDOWS}
    end
    else // CLRVersion <> ''
      AddOption(joJCLPdbCreate, [goNoAutoCheck], Parent);
  end;

  procedure AddExpertOptions(Parent: TInstallerOption);
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

    AddOption(joJCLExperts, [goExpandable, goChecked], Parent);

    if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber <= 2) then
      // design packages are not loaded by C#Builder 1 and Delphi 8
      AddOption(joJCLExpertsDLL, [goRadioButton, goChecked], joJCLExperts)
    else if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 3) then
      // expert DLLs are unstable on Delphi 2005 and BDS 2006
      // (problems while adding menu items in menu not loaded yet)
      AddOption(joJCLExpertsDsgnPackages, [goRadioButton, goChecked], joJCLExperts)
    else
    begin
      AddOption(joJCLExpertsDLL, [goRadioButton], joJCLExperts);
      AddOption(joJCLExpertsDsgnPackages, [goRadioButton, goChecked], joJCLExperts);
    end;

    if RunTimeInstallation then
    begin
      AddOption(joJCLExpertDebug, ExpertOptions, joJCLExperts);
      AddOption(joJCLExpertAnalyzer, ExpertOptions, joJCLExperts);
      if Target.RadToolKind <> brBorlandDevStudio then
        AddOption(joJCLExpertUses, ExpertOptions, joJCLExperts);
      AddOption(joJCLExpertSimdView, ExpertOptions, joJCLExperts);
      AddOption(joJCLExpertRepository, ExpertOptions, joJCLExperts);
      AddOption(joJCLExpertStackTraceViewer, ExpertOptions, joJCLExperts);
    end;
    AddOption(joJCLExpertFavorite, ExpertOptions, joJCLExperts);
    AddOption(joJCLExpertVersionControl, [goNoAutoCheck], joJCLExperts);
    if (Target.RadToolKind <> brBorlandDevStudio) and (Target.VersionNumber <= 6) then
      AddOption(joJCLExpertThreadNames, ExpertOptions, joJCLExperts);
    {$ENDIF MSWINDOWS}
  end;

  procedure AddDemoNodes;
  var
    I: Integer;
    ADemoList: TStrings;
    DemoOption: Integer;
    FileName: string;
  begin
    AddOption(joJCLMakeDemos, [goNoAutoCheck], joJediCodeLibrary);
    ADemoList := GetDemoList;
    for I := 0 to ADemoList.Count - 1 do
    begin
      FileName := ExtractRelativePath(Distribution.JclExamplesDir, ADemoList.Strings[I]);
      DemoOption := InstallCore.AddInstallOption(FileName);
      ADemoList.Objects[I] := TObject(DemoOption);
      GUIPage.AddInstallOption(DemoOption, [], ExtractFileName(FileName), FileName, OptionData[joJCLMakeDemos].Id);
    end;
  end;

  procedure LoadValues;
  var
    AConfiguration: IJediConfiguration;
    Option: TInstallerOption;
    Id, Index: Integer;
    StoredValue: string;
    ADemoList: TStrings;
    ResetDefaultValue: Boolean;
  begin
    AConfiguration := InstallCore.Configuration;
    if not Assigned(AConfiguration) then
      Exit;
    if AConfiguration.SectionExists(TargetName) then
    begin
      ResetDefaultValue := not AConfiguration.OptionAsBool[TargetName, OptionData[joJediCodeLibrary].Id];
      for Option := Low(TInstallerOption) to High(TInstallerOption) do
      begin
        Id := OptionData[Option].Id;
        if AConfiguration.ValueExists(TargetName, Id) then
          GUIPage.OptionChecked[Id] := AConfiguration.OptionAsBool[TargetName, Id]
        else
        if ResetDefaultValue then
          GUIPage.OptionChecked[Id] := False;
      end;
    end
    else
      GUIPage.OptionChecked[OptionData[joJediCodeLibrary].Id] := True;

    if not Target.IsTurboExplorer then
    begin
      if FRunTimeInstallation and (CLRVersion = '') then
      begin
        ADemoList := GetDemoList;
        if AConfiguration.SectionExists(FDemoSectionName) then
          for Index := 0 to ADemoList.Count - 1 do
        begin
          Id := Integer(ADemoList.Objects[Index]);
          GUIPage.OptionChecked[Id] := AConfiguration.OptionAsBool[FDemoSectionName, Id];
        end;
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

begin
  FGUI := InstallCore.InstallGUI;
  if not Assigned(GUI) then
    Exit;

  FGUIPage := GUI.CreateInstallPage;
  GUIPage.Caption := TargetName;
  GUIPage.SetIcon(Target.IdeExeFileName);

  AddOption(joJediCodeLibrary, [goExpandable, goChecked], JediTargetOption);

  if RunTimeInstallation then
  begin
    // conditional defines
    AddOption(joJCLDef, [goExpandable, goChecked], OptionData[joJediCodeLibrary].Id);
    AddDefOptions(joJCLDef);

    if CLRVersion = '' then
    begin
      AddOption(joJCLEnvironment, [goExpandable, goChecked], OptionData[joJediCodeLibrary].Id);
      AddEnvOptions(joJCLEnvironment);
    end;

    if not Target.IsTurboExplorer then
    begin
      AddOption(joJCLMake, [goExpandable, goChecked], OptionData[joJediCodeLibrary].Id);
      AddMakeOptions(joJCLMake);
    end;

    if CLRVersion = '' then
    begin
      AddHelpOptions(joJediCodeLibrary);
      AddRepositoryOptions(joJediCodeLibrary);
    end;
  end;

  if not Target.IsTurboExplorer then
  begin
    AddOption(joJCLPackages, [goStandAloneParent, goExpandable, goChecked], joJediCodeLibrary);
    AddPackageOptions(joJCLPackages);

    if CLRVersion = '' then
    begin
      {$IFDEF MSWINDOWS}
      AddExpertOptions(joJCLPackages);
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
var
  AProfilesManager: IJediProfilesManager;
  
  procedure WriteIntroduction;
  var
    Personality: TJclBorPersonality;
    Index: Integer;
  begin
    WriteLog(StrRepeat('=', 80));
    WriteLog(Distribution.Version);
    WriteLog('');
    WriteLog(StrPadRight(StrRepeat('=', 10) + TargetName, 80, '='));
    WriteLog('');
    WriteLog('Installed personalities :');
    for Personality := Low(TJclBorPersonality) to High(TJclBorPersonality) do
      if Personality in Target.Personalities then
    begin
      WriteLog(JclBorPersonalityDescription[Personality]);
    end;
    WriteLog('');
    WriteLog(StrRepeat('=', 80));
    WriteLog('');
    if AProfilesManager.MultipleProfileMode then
    begin
      for Index := 0 to AProfilesManager.ProfileCount - 1 do
        if IsProfileEnabled[Index] then
          WriteLog(AProfilesManager.ProfileNames[Index]);
    end
    else
      WriteLog('Single profile installation');
    WriteLog('');
    WriteLog(StrRepeat('=', 80));
    WriteLog('');
  end;

  function CheckDirectories: Boolean;
  begin
    Result := True;

    {$IFDEF MSWINDOWS}
    if (not OptionChecked[joJCLPackages] or (Target.SupportsVCL and not OptionChecked[joJCLVCLPackage])) and
      Assigned(GUI) and (CLRVersion = '') and not Target.IsTurboExplorer then
      Result := GUI.Dialog(RsWarningPackageNodeNotSelected, dtConfirmation, [drYes, drNo]) = drYes;
    {$ENDIF MSWINDOWS}

    if Result and OptionChecked[joJCLPackages] then
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
    end;
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
      TemplateFileName := PathAddSeparator(Distribution.JclIncludeDir) + 'jcl.template.inc';
      IncludeFileName := Format('%sjcl%s%s.inc', [PathAddSeparator(Distribution.JclIncludeDir), Target.IDEVersionNumberStr, CLRSuffix]);
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
                while CharIsWhiteSpace(IncludeLine[DefinePos]) do
                  Inc(DefinePos);
                SymbolEnd := DefinePos;
                while CharIsValidIdentifierLetter(IncludeLine[SymbolEnd]) do
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
    DefineNames: array [joJCLDefThreadSafe..joJCLDef7zLinkOnRequest] of string =
      ( 'THREADSAFE', 'DROP_OBSOLETE_CODE', 'UNITVERSIONING',
        'MATH_SINGLE_PRECISION', 'MATH_DOUBLE_PRECISION', 'MATH_EXTENDED_PRECISION',
        'MATH_EXT_EXTREMEVALUES',  'HOOK_DLL_EXCEPTIONS',
        'DEBUG_NO_BINARY', 'DEBUG_NO_TD32', 'DEBUG_NO_MAP', 'DEBUG_NO_EXPORTS',
        'DEBUG_NO_SYMBOLS', 'EDI_WEAK_PACKAGE_UNITS', 'PCRE_STATICLINK',
        'PCRE_LINKDLL', 'PCRE_LINKONREQUEST', 'BZIP2_STATICLINK',
        'BZIP2_LINKDLL', 'BZIP2_LINKONREQUEST', 'ZLIB_STATICLINK',
        'ZLIB_LINKDLL', 'ZLIB_LINKONREQUEST', 'UNICODE_SILENT_FAILURE',
        'UNICODE_RAW_DATA', 'UNICODE_ZLIB_DATA', 'UNICODE_BZIP2_DATA',
        'CONTAINER_ANSISTR', 'CONTAINER_WIDESTR', 'CONTAINER_UNICODESTR',
        'CONTAINER_NOSTR', {'7ZIP_STATICLINK',} '7ZIP_LINKDLL',
        '7ZIP_LINKONREQUEST' );
  var
    Option: TInstallerOption;
    Defines: TStrings;
  begin
    Defines := TStringList.Create;
    try
      if OptionChecked[joJCLDef] then
      begin
        MarkOptionBegin(joJCLDef);
        for Option := Low(DefineNames) to High(DefineNames) do
          if OptionChecked[Option] then
        begin
          MarkOptionBegin(Option);
          Defines.Add(DefineNames[Option]);
          MarkOptionEnd(Option, True);
        end;
        MarkOptionEnd(joJCLDef, True);
      end;
      if OptionChecked[joJCLMapCreate] then
      begin
        MarkOptionBegin(joJCLMapCreate);
        Target.MapCreate := True;
        MarkOptionEnd(joJCLMapCreate, True);
      end
      else
        Target.MapCreate := False;
      {$IFDEF MSWINDOWS}
      if OptionChecked[joJCLJdbgCreate] then
      begin
        MarkOptionBegin(joJCLJdbgCreate);
        Target.JdbgCreate := True;
        MarkOptionEnd(joJCLJdbgCreate, True);
      end
      else
        Target.JdbgCreate := False;
      if OptionChecked[joJCLJdbgInsert] then
      begin
        MarkOptionBegin(joJCLJdbgInsert);
        Target.JdbgInsert := True;
        MarkOptionEnd(joJCLJdbgInsert, True);
      end
      else
        Target.JdbgInsert := False;
      if OptionChecked[joJCLMapDelete] then
      begin
        MarkOptionBegin(joJCLMapDelete);
        Target.MapDelete := True;
        MarkOptionEnd(joJCLMapDelete, True);
      end
      else
        Target.MapDelete := False;
      if Target is TJclBDSInstallation then
      begin
        if OptionChecked[joJCLDualPackages] then
        begin
          MarkOptionBegin(joJCLDualPackages);
          TJclBDSInstallation(Target).DualPackageInstallation := True;
          if OptionChecked[joJCLCopyPackagesHppFiles] then
          begin
            MarkOptionBegin(joJCLCopyPackagesHppFiles);
            MarkOptionEnd(joJCLCopyPackagesHppFiles, True);
          end;
          MarkOptionEnd(joJCLDualPackages, True);
        end
        else
          TJclBDSInstallation(Target).DualPackageInstallation := False;
        if OptionChecked[joJCLPdbCreate] then
        begin
          MarkOptionBegin(joJCLPdbCreate);
          TJclBDSInstallation(Target).PdbCreate := True;
          MarkOptionEnd(joJCLPdbCreate, True);
        end
        else
          TJclBDSInstallation(Target).PdbCreate := False;
      end;
      {$ENDIF MSWINDOWS}

      // no conditional defines for C#Builder 1 and Delphi 8
      Result := ((Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber <= 2)) or SaveDefines(Defines);
    finally
      Defines.Free;
    end;
  end;

  function SetEnvironment(ATarget: TJclBorRADToolInstallation): Boolean;
  begin
    Result := True;
    if OptionChecked[joJCLEnvironment] then
    begin
      MarkOptionBegin(joJCLEnvironment);
  
      if OptionChecked[joJCLEnvLibPath] then
      begin
        MarkOptionBegin(joJCLEnvLibPath);
        Result := ATarget.AddToLibrarySearchPath(FLibReleaseDir) and ATarget.AddToLibrarySearchPath(Distribution.JclIncludeDir);
        if Result then
        begin
          WriteLog(Format('Added "%s;%s" to library search path.', [FLibReleaseDir, Distribution.JclIncludeDir]));
          {$IFDEF MSWINDOWS}
          if (ATarget.RadToolKind = brBorlandDevStudio) and (bpBCBuilder32 in ATarget.Personalities)
            and OptionChecked[joJCLDualPackages] then
            with TJclBDSInstallation(ATarget) do
          begin
            Result := AddToCppSearchPath(FLibReleaseDir) and AddToCppSearchPath(Distribution.JclIncludeDir) and
                      ((IDEVersionNumber < 5) or AddToCppLibraryPath(FLibReleaseDir));
            if Result then
              WriteLog(Format('Added "%s;%s" to cpp search path.', [FLibReleaseDir, Distribution.JclIncludeDir]))
            else
              WriteLog('Failed to add cpp search paths.');
          end;
          {$ENDIF MSWINDOWS}
          if ATarget.IsTurboExplorer then
          begin
            Result := ATarget.AddToLibrarySearchPath(Distribution.JclSourcePath);
            if Result then
              WriteLog(Format('Added "%s" to library search path.', [Distribution.JclSourcePath]))
            else
              WriteLog('Failed to add library search paths.');
          end;
        end
        else
          WriteLog('Failed to add library search paths.');
        MarkOptionEnd(joJCLEnvLibPath, Result);
      end;
  
      if Result and OptionChecked[joJCLEnvBrowsingPath] then
      begin
        MarkOptionBegin(joJCLEnvBrowsingPath);
        if Result then
        begin
          Result := ATarget.AddToLibraryBrowsingPath(Distribution.JclSourcePath);
          if Result then
          begin
            WriteLog(Format('Added "%s" to library browsing path.', [Distribution.JclSourcePath]));
            {$IFDEF MSWINDOWS}
            if (ATarget.RadToolKind = brBorlandDevStudio) and (bpBCBuilder32 in ATarget.Personalities)
              and OptionChecked[joJCLDualPackages] then
              with TJclBDSInstallation(ATarget) do
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
        MarkOptionEnd(joJCLEnvBrowsingPath, Result);
      end;
  
      if Result and OptionChecked[joJCLEnvDebugDCUPath] then
      begin
        MarkOptionBegin(joJCLEnvDebugDCUPath);
        Result := ATarget.AddToDebugDCUPath(FLibDebugDir);
        if Result then
          WriteLog(Format('Added "%s" to Debug DCU Path.', [FLibDebugDir]))
        else
          WriteLog('Failed to add debug DCU path');
        MarkOptionEnd(joJCLEnvDebugDCUPath, Result);
      end;

      MarkOptionEnd(joJCLEnvironment, Result);
    end;
  end;

  function MakeUnits: Boolean;
    function CheckHppFiles: Boolean;
    var
      SaveDir, Options: string;
    begin
      SaveDir := GetCurrentDir;
      SetCurrentDir(Format('%sinstall%sHeaderTest', [Distribution.JclPath, DirDelimiter]));
      try
        Target.BCC32.Options.Clear;
        Target.BCC32.Options.Add('-c'); // compile only
        Target.BCC32.Options.Add('-Ve'); // compatibility
        Target.BCC32.Options.Add('-X'); // no autodependencies
        Target.BCC32.Options.Add('-a8'); // data alignment
        Target.BCC32.Options.Add('-b'); // enum to be at least 4 bytes
        Target.BCC32.Options.Add('-k-'); // no standard stack frame
        {$IFDEF MSWINDOWS}
        Target.BCC32.Options.Add('-tWM'); // code format
        {$ELSE ~ MSWINDOWS}
        Target.BCC32.Options.Add('-tC'); // code format
        {$ENDIF ~MSWINDOWS}
        Target.BCC32.Options.Add('-w-par'); // warning
        Target.BCC32.Options.Add('-w-aus'); // warning
        Target.BCC32.AddPathOption('I', Format('%sinclude%s%s%s%s%sinclude%s%s', [Distribution.JclPath, DirSeparator, Distribution.JclSourcePath, DirSeparator, Target.RootDir, DirDelimiter, DirSeparator, Target.VclIncludeDir]));
        Target.BCC32.Options.Add('-DTEST_COMMON');
        {$IFDEF MSWINDOWS}
        Target.BCC32.Options.Add('-DTEST_WINDOWS');
        {$ENDIF MSWINDOWS}
        {$IFDEF UNIX}
        Target.BCC32.Options.Add('-DTEST_UNIX');
        {$ENDIF UNIX}
        if OptionChecked[joJCLMakeReleaseVCL] or OptionChecked[joJCLMakeDebugVCL] then
          Target.BCC32.Options.Add('-DTEST_VCL');
        if OptionChecked[joJCLMakeReleaseVClx] or OptionChecked[joJCLMakeDebugVClx] then
          Target.BCC32.Options.Add('-DTEST_VISCLX');
        Options := StringsToStr(Target.BCC32.Options, NativeSpace);
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
    if OptionChecked[joJCLMake] then
    begin
      MarkOptionBegin(joJCLMake);

      if OptionChecked[joJCLMakeRelease] then
      begin
        MarkOptionBegin(joJCLMakeRelease);

        for I := Low(JclSourceDirs) to High(JclSourceDirs) do
        begin
          if (JclSourceDirs[I] = JclSrcDirVisClx) then
          begin
            if OptionChecked[joJCLMakeReleaseVClx] then
              MarkOptionBegin(joJCLMakeReleaseVClx)
            else
              Continue;
          end;
          if (JclSourceDirs[I] = JclSrcDirVcl) then
          begin
            if OptionChecked[joJCLMakeReleaseVCL] or
              ((Target.VersionNumber <= 5) and (Target.RadToolKind <> brBorlandDevStudio)) then
              MarkOptionBegin(joJCLMakeReleaseVCL)
            else
              Continue;
          end;
          Result := Result and CompileLibraryUnits(JclSourceDirs[I], False);
          if (JclSourceDirs[I] = JclSrcDirVisClx) then
            MarkOptionEnd(joJCLMakeReleaseVClx, Result);
          if (JclSourceDirs[I] = JclSrcDirVcl) then
            MarkOptionEnd(joJCLMakeReleaseVCL, Result);
        end;
        MarkOptionEnd(joJCLMakeRelease, Result);
      end;

      if Result and OptionChecked[joJCLMakeDebug] then
      begin
        MarkOptionBegin(joJCLMakeDebug);
        for I := Low(JclSourceDirs) to High(JclSourceDirs) do
        begin
          if (JclSourceDirs[I] = JclSrcDirVisClx) then
          begin
            if OptionChecked[joJCLMakeDebugVClx] then
              MarkOptionBegin(joJCLMakeDebugVClx)
            else
              Continue;
          end;
          if (JclSourceDirs[I] = JclSrcDirVcl) then
          begin
            if OptionChecked[joJCLMakeDebugVCL] or
              ((Target.VersionNumber <= 5) and (Target.RadToolKind <> brBorlandDevStudio)) then
              MarkOptionBegin(joJCLMakeDebugVCL)
            else
              Continue;
          end;
          Result := Result and CompileLibraryUnits(JclSourceDirs[I], True);
          if (JclSourceDirs[I] = JclSrcDirVisClx) then
            MarkOptionEnd(joJCLMakeDebugVClx, Result);
          if (JclSourceDirs[I] = JclSrcDirVcl) then
            MarkOptionEnd(joJCLMakeDebugVCL, Result);
        end;
        MarkOptionEnd(joJCLMakeDebug, Result);
      end;

      if Result and OptionChecked[joJCLCheckHppFiles] then
      begin
        MarkOptionBegin(joJCLCheckHppFiles);
        WriteLog('Checking .hpp files');
        Result := Result and CheckHppFiles;
        MarkOptionEnd(joJCLCheckHppFiles, Result);
      end;

      MarkOptionEnd(joJCLMake, Result);
    end;
  end;

  function CompilePackages: Boolean;
  begin
    Result := True;
    if OptionChecked[joJCLPackages] then
    begin
      MarkOptionBegin(joJCLPackages);
      if CLRVersion = '' then
      begin
        Result := CompilePackage(FullPackageFileName(Target, JclDpk))
          and CompilePackage(FullPackageFileName(Target, JclContainersDpk));

        if Result and OptionChecked[joJCLVclPackage] then
        begin
          MarkOptionBegin(joJCLVclPackage);
          Result := Result and CompilePackage(FullPackageFileName(Target, JclVclDpk));
          MarkOptionEnd(joJCLVclPackage, Result);
        end;

        if Result and OptionChecked[joJCLClxPackage] then
        begin
          MarkOptionBegin(joJCLClxPackage);
          Result := Result and CompilePackage(FullPackageFileName(Target, JclVClxDpk));
          MarkOptionEnd(joJCLClxPackage, Result);
        end;

        MarkOptionEnd(joJCLPackages, Result);
      end
      {$IFDEF MSWINDOWS}
      else
        // CLR installation
        Result := CompileCLRPackage(JediJclDpk) and CompileCLRPackage(JediJclContainersDpk);
      {$ENDIF MSWINDOWS}
    end;
  end;

  function RegisterPackages(ATarget: TJclBorRADToolInstallation): Boolean;
  {$IFDEF MSWINDOWS}
  var
    PathEnvVar: string;
  {$ENDIF MSWINDOWS}
  begin
    {$IFDEF MSWINDOWS}
    if CLRVersion = '' then
    begin
      InstallJediRegInformation(ATarget.ConfigDataLocation, 'JCL',
        Format('%d.%d.%d.%d', [JclVersionMajor, JclVersionMinor, JclVersionRelease, JclVersionBuild]),
        GetDcpPath, GetBplPath, Distribution.FJclPath, ATarget.RootKey);

      PathEnvVar := RegReadStringDef(ATarget.RootKey, RegHKCUEnvironmentVar, PathEnvironmentVar, '');
      PathListIncludeItems(PathEnvVar, RegReadStringDef(HKLM, RegHKLMEnvironmentVar, PathEnvironmentVar, ''));
      ExpandEnvironmentVar(PathEnvVar);
      if (PathListItemIndex(PathEnvVar, GetBplPath) = -1) and (PathListItemIndex(PathEnvVar, PathAddSeparator(GetBplPath)) = -1)
        and Assigned(GUI) and (GUI.Dialog(RsWarningAddPathToEnvironment, dtWarning, [drYes, drNo]) = drYes) then
      begin
        PathEnvVar := RegReadStringDef(ATarget.RootKey, RegHKCUEnvironmentVar, PathEnvironmentVar, '');
        PathListIncludeItems(PathEnvVar, GetBplPath);
        RegWriteString(ATarget.RootKey, RegHKCUEnvironmentVar, PathEnvironmentVar, PathEnvVar);
      end;
    end;
    {$ENDIF MSWINDOWS}
    Result := True;
  end;

  {$IFDEF MSWINDOWS}
  function CompileExperts: Boolean;
  var
    Option: TInstallerOption;
    DLLExperts: Boolean;
  begin
    Result := True;
    if OptionChecked[joJCLExperts] then
    begin
      MarkOptionBegin(joJCLExperts);
      DLLExperts := False;
      // dual packages useless for experts
      if Target.RadToolKind = brBorlandDevStudio then
        TJclBDSInstallation(Target).DualPackageInstallation := False;
      for Option := Low(SupportedExperts) to High(SupportedExperts) do
        if OptionChecked[Option] then
      begin
        MarkOptionBegin(Option);
        if Option = joJCLExpertsDsgnPackages then
          // nothing, default value
        else if Option = joJCLExpertsDLL then
          DLLExperts := OptionChecked[Option]
        else if DLLExperts then
          Result := CompileExpert(FullLibraryFileName(Target, SupportedExperts[Option]))
        else
          Result := CompilePackage(FullPackageFileName(Target, SupportedExperts[Option]));
        MarkOptionEnd(Option, Result);
        if not Result then
          Break;
      end;
      MarkOptionEnd(joJCLExperts, Result);
    end;
  end;

  function RegisterExperts(ATarget: TJclBorRADToolInstallation): Boolean;
  var
    Option: TInstallerOption;
    DLLExperts: Boolean;
    ProjectFileName: string;
  begin
    Result := True;
    if OptionChecked[joJCLExperts] then
    begin
      MarkOptionBegin(joJCLExperts);
      DLLExperts := False;
      // dual packages useless for experts
      if ATarget.RadToolKind = brBorlandDevStudio then
        TJclBDSInstallation(ATarget).DualPackageInstallation := False;
      for Option := Low(SupportedExperts) to High(SupportedExperts) do
        if OptionChecked[Option] then
      begin
        MarkOptionBegin(Option);
        if Option = joJCLExpertsDsgnPackages then
          // nothing, default value
        else if Option = joJCLExpertsDLL then
          DLLExperts := OptionChecked[Option]
        else if DLLExperts then
        begin
          ProjectFileName := Distribution.JclPath + FullLibraryFileName(ATarget, SupportedExperts[Option]);
          Result := ATarget.RegisterExpert(ProjectFileName, GetBplPath, PathExtractFileNameNoExt(ProjectFileName));
        end
        else
        begin
          ProjectFileName := Distribution.JclPath + FullPackageFileName(ATarget, SupportedExperts[Option]);
          Result := ATarget.RegisterPackage(ProjectFileName, GetBplPath, PathExtractFileNameNoExt(ProjectFileName));
        end;
        MarkOptionEnd(Option, Result);
        if not Result then
          Break;
      end;
      MarkOptionEnd(joJCLExperts, Result);
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
    if OptionChecked[joJCLExceptDlg] then
    begin
      MarkOptionBegin(joJCLExceptDlg);
      {$IFDEF MSWINDOWS}
      if OptionChecked[joJCLExceptDlgVCL] then
      begin
        MarkOptionBegin(joJCLExceptDlgVCL);
        Result := AddDialogToRepository(ExceptDlgVclName, Distribution.VclDialogFileName,
          Distribution.VclDialogIconFileName, BorRADToolRepositoryDesignerDfm);
        MarkOptionEnd(joJCLExceptDlgVCL, Result);
      end;
      if Result and OptionChecked[joJCLExceptDlgVCLSnd] then
      begin
        MarkOptionBegin(joJCLExceptDlgVCLSnd);
        Result := AddDialogToRepository(ExceptDlgVclSndName, Distribution.VclDialogSendFileName,
          Distribution.VclDialogSendIconFileName, BorRADToolRepositoryDesignerDfm);
        MarkOptionEnd(joJCLExceptDlgVCLSnd, Result);
      end;
      {$ENDIF MSWINDOWS}
      if Result and OptionChecked[joJCLExceptDlgCLX] then
      begin
        MarkOptionBegin(joJCLExceptDlgCLX);
        Result := AddDialogToRepository(ExceptDlgClxName, Distribution.ClxDialogFileName,
          Distribution.ClxDialogIconFileName, BorRADToolRepositoryDesignerXfm);
        MarkOptionEnd(joJCLExceptDlgCLX, Result);
      end;
      MarkOptionEnd(joJCLExceptDlg, Result);
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
      if OptionChecked[joJCLHelpHxSPlugin] then
      begin
        MarkOptionBegin(joJCLHelpHxSPlugin);
        Distribution.RegHelpPlugNameSpaceIn(NameSpace, TJclBDSInstallation(Target).Help2Manager.IdeNamespace);
        MarkOptionEnd(joJCLHelpHxSPlugin, Result);
      end;

      Distribution.RegHelpCommitTransaction;

      WriteLog('...defered');
    end;
  begin
    Result := True;
    if OptionChecked[joJCLHelp] then
    begin
      MarkOptionBegin(joJCLHelp);

      if OptionChecked[joJCLHelpHlp] then
      begin
        MarkOptionBegin(joJCLHelpHlp);
        Result := AddHelpToOpenHelp;
        MarkOptionEnd(joJCLHelpHlp, Result);
      end;

      if Result and OptionChecked[joJCLHelpChm] then
      begin
        MarkOptionBegin(joJCLHelpChm);
        Result := AddHelpToIdeTools;
        MarkOptionEnd(joJCLHelpChm, Result);
      end;

      if Result and OptionChecked[joJCLHelpHxS] then
      begin
        MarkOptionBegin(joJCLHelpHxS);
        Result := RegisterHelp2Files;
        MarkOptionEnd(joJCLHelpHxS, Result);
      end;

      MarkOptionEnd(joJCLHelp, Result);
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
    if OptionChecked[joJCLMakeDemos] then
    begin
      MarkOptionBegin(joJCLMakeDemos);
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
            // ahuser: The installation shouldn't fail if some demos can't be compiled like
            //         outdated demos or CLX/Kylix demos. Otherwise the JVCL Installer will
            //         have a hard time finding a valid JCL installation
            //Result := Result and DemoResult;
          end;
        end;
      finally
        SetCurrentDir(SaveDir);
      end;

      MarkOptionEnd(joJCLMakeDemos, Result);
    end;
  end;

var
  Index: Integer;
  ATarget: TJclBorRADToolInstallation;
begin
  AProfilesManager := InstallCore.ProfilesManager;
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
    Result := CheckDirectories and SetStaticOptions and MakeUnits and CompilePackages and InstallRepository
      and MakeDemos {$IFDEF MSWINDOWS}and CompileExperts and InstallHelpFiles{$ENDIF MSWINDOWS};
    if Result then
    begin
      if AProfilesManager.MultipleProfileMode then
      begin
        for Index := 0 to AProfilesManager.ProfileCount - 1 do
          if IsProfileEnabled[Index] then
        begin
          ATarget := ProfileTargets[Index];
          if ATarget.Valid then
          begin
            WriteLog(StrPadRight(StrRepeat('=', 10) + InstallCore.ProfilesManager.ProfileNames[Index], 80, '='));
            Result := Result and SetEnvironment(ATarget) and RegisterPackages(ATarget)
              {$IFDEF MSWINDOWS}and RegisterExperts(ATarget){$ENDIF MSWINDOWS};
          end;
        end;
      end
      else
        Result := Result and SetEnvironment(Target) and RegisterPackages(Target)
          {$IFDEF MSWINDOWS}and RegisterExperts(Target){$ENDIF MSWINDOWS};
    end;

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
  procedure RemoveEnvironment(ATarget: TJclBorRADToolInstallation);
  begin
    //ioJclEnvLibPath
    if CLRVersion = '' then
    begin
      if ATarget.RemoveFromLibrarySearchPath(FLibReleaseDir) and
         ATarget.RemoveFromLibrarySearchPath(Distribution.JclSourceDir) and
         ATarget.RemoveFromLibrarySearchPath(Distribution.JclIncludeDir) then
        WriteLog(Format('Removed "%s;%s;%s" from library search path.', [FLibReleaseDir, Distribution.JclSourceDir, Distribution.JclIncludeDir]))
      else
        WriteLog('Failed to remove library search path.');
      {$IFDEF MSWINDOWS}
      if (ATarget.RadToolKind = brBorlandDevStudio) and (bpBCBuilder32 in ATarget.Personalities) then
        with TJclBDSInstallation(ATarget) do
      begin
        if RemoveFromCppSearchPath(FLibReleaseDir) and
           RemoveFromCppSearchPath(Distribution.JclSourceDir) and
           RemoveFromCppSearchPath(Distribution.JclIncludeDir) and
           ((IDEVersionNumber < 5) or RemoveFromCppLibraryPath(FLibReleaseDir)) then
          WriteLog(Format('Removed "%s;%s;%s" from cpp search path.', [FLibReleaseDir, Distribution.JclSourceDir, Distribution.JclIncludeDir]))
        else
          WriteLog('Failed to remove cpp search path.');
      end;
      {$ENDIF MSWINDOWS}

      //ioJclEnvBrowsingPath
      if ATarget.RemoveFromLibraryBrowsingPath(Distribution.JclSourcePath) then
        WriteLog(Format('Removed "%s" from library browsing path.', [Distribution.JclSourcePath]))
      else
        WriteLog('Failed to remove library browsing path.');
      {$IFDEF MSWINDOWS}
      if (ATarget.RadToolKind = brBorlandDevStudio) and (bpBCBuilder32 in ATarget.Personalities) then
        with TJclBDSInstallation(ATarget) do
      begin
        if RemoveFromCppBrowsingPath(Distribution.JclSourcePath) then
          WriteLog(Format('Removed "%s" from cpp browsing path.', [Distribution.JclSourcePath]))
        else
          WriteLog('Failed to remove cpp browsing path.');
      end;
      {$ENDIF MSWINDOWS}

      //ioJclEnvDebugDCUPath
      if ATarget.RemoveFromDebugDCUPath(FLibDebugDir) then
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

  procedure UnregisterPackages(ATarget: TJclBorRADToolInstallation);
  {$IFNDEF KYLIX}
  var
    ABDSTarget: TJclBDSInstallation;
  {$ENDIF ~KYLIX}
  begin
    if CLRVersion = '' then
    begin
      {$IFNDEF KYLIX}
      if ATarget.RadToolKind = brBorlandDevStudio then
      begin
        ABDSTarget := ATarget as TJclBDSInstallation;
        ABDSTarget.CleanPackageCache(BinaryFileName(GetBPLPath, Distribution.JclPath + FullPackageFileName(ATarget, JclDpk)));
        ABDSTarget.CleanPackageCache(BinaryFileName(GetBPLPath, Distribution.JclPath + FullPackageFileName(ATarget, JclContainersDpk)));
        if RuntimeInstallation and ATarget.SupportsVisualCLX then
          ABDSTarget.CleanPackageCache(BinaryFileName(GetBPLPath, Distribution.JclPath + FullPackageFileName(ATarget, JclVClxDpk)));
        if RuntimeInstallation and ATarget.SupportsVCL then
          ABDSTarget.CleanPackageCache(BinaryFileName(GetBPLPath, Distribution.JclPath + FullPackageFileName(ATarget, JclVclDpk)));
      end;
      {$ENDIF KYLIX}
      //ioJclPackages
      ATarget.UnregisterPackage(Distribution.JclPath + FullPackageFileName(ATarget, JclDpk), GetBplPath);
      ATarget.UnregisterPackage(Distribution.JclPath + FullPackageFileName(ATarget, JclContainersDpk), GetBplPath);
      if RuntimeInstallation and ATarget.SupportsVisualCLX then
        ATarget.UnregisterPackage(Distribution.JclPath + FullPackageFileName(ATarget, JclVClxDpk), GetBplPath);
      if RuntimeInstallation and ATarget.SupportsVCL then
        ATarget.UnregisterPackage(Distribution.JclPath + FullPackageFileName(ATarget, JclVclDpk), GetBplPath);
      {$IFDEF MSWINDOWS}
      RemoveJediRegInformation(Target.ConfigDataLocation, 'JCL', ATarget.RootKey);
      {$ENDIF MSWINDOWS}
    end;
  end;

  procedure DeletePackages;
  begin
    if CLRVersion = '' then
    begin
      DeletePackage(FullPackageFileName(Target, JclDpk));
      DeletePackage(FullPackageFileName(Target, JclContainersDpk));
      if RuntimeInstallation and Target.SupportsVisualCLX then
        DeletePackage(FullPackageFileName(Target, JclVClxDpk));
      if RuntimeInstallation and Target.SupportsVCL then
        DeletePackage(FullPackageFileName(Target, JclVclDpk));
    end;
  end;
  {$IFDEF MSWINDOWS}
  procedure UnregisterExperts(ATarget: TJclBorRADToolInstallation);
    procedure UnregisterExpert(const Name: string);
    var
      Index: Integer;
      FileName, ShortFileName: string;
    begin
      for Index := ATarget.IdePackages.Count - 1 downto 0 do
      begin
        FileName := ATarget.IdePackages.PackageFileNames[Index];
        ShortFileName := ChangeFileExt(ExtractFileName(FileName), '');
        if StrMatches(Name, ShortFileName)
          or StrMatches(Format('%sDLL%s', [Name, StrUpper(ATarget.VersionNumberStr)]), ShortFileName)
          or StrMatches(Format('%sDLL%d', [Name, ATarget.VersionNumber]), ShortFileName)
          or StrMatches(Format('%sDLL%s0', [Name, StrUpper(ATarget.VersionNumberStr)]), ShortFileName)
          or StrMatches(Format('%sDLL%d0', [Name, ATarget.VersionNumber]), ShortFileName) then
          ATarget.UnregisterPackage(FileName);
      end;
      for Index := ATarget.IdePackages.ExpertCount - 1 downto 0 do
      begin
        FileName := ATarget.IdePackages.ExpertFileNames[Index];
        ShortFileName := ChangeFileExt(ExtractFileName(FileName), '');
        if StrMatches(Name, ShortFileName)
          or StrMatches(Format('%sDLL%s', [Name, StrUpper(ATarget.VersionNumberStr)]), ShortFileName)
          or StrMatches(Format('%sDLL%d', [Name, ATarget.VersionNumber]), ShortFileName)
          or StrMatches(Format('%sDLL%s0', [Name, StrUpper(ATarget.VersionNumberStr)]), ShortFileName)
          or StrMatches(Format('%sDLL%d0', [Name, ATarget.VersionNumber]), ShortFileName) then
          ATarget.UnregisterExpert(FileName);
      end;
    end;
  var
    Option: TInstallerOption;
    IndexOldExpert: Integer;
  begin
    if CLRVersion = '' then
    begin
      for Option := Low(SupportedExperts) to High(SupportedExperts) do
        if not (Option in [joJCLExpertsDsgnPackages, joJCLExpertsDLL]) then
          UnregisterExpert(SupportedExperts[Option]);
      for IndexOldExpert := Low(OldExperts) to High(OldExperts) do
        UnregisterExpert(OldExperts[IndexOldExpert]);
    end;
  end;

  procedure DeleteExperts;
  var
    Option: TInstallerOption;
    ProjectFileName: string;
  begin
    if CLRVersion = '' then
    begin
      for Option := Low(SupportedExperts) to High(SupportedExperts) do
        if not (Option in [joJCLExpertsDsgnPackages, joJCLExpertsDLL]) then
      begin
        ProjectFileName := Distribution.JclPath + FullPackageFileName(Target, SupportedExperts[Option]);
        if FileExists(ProjectFileName) then
          Target.UninstallPackage(ProjectFileName, GetBplPath, GetDcpPath);
        ProjectFileName := Distribution.JclPath + FullLibraryFileName(Target, SupportedExperts[Option]);
        if FileExists(ProjectFileName) then
          Result := FileDelete(BinaryFileName(GetBplPath, ProjectFileName));
      end;
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

var
  Index: Integer;
  AProfilesManager: IJediProfilesManager;
  ATarget: TJclBorRADToolInstallation;
begin
  AProfilesManager := InstallCore.ProfilesManager;
  try
    Target.OutputCallback := WriteLog;
    if Assigned(GUI) then
      GUI.Status := Format(RsUninstallMessage, [TargetName]);
    if Assigned(GUIPage) then
      GUIPage.Show;

    WriteLog(StrPadRight('Starting Uninstall process', 44, '.'));

    if AProfilesManager.MultipleProfileMode then
    begin
      for Index := 0 to AProfilesManager.ProfileCount - 1 do
        if IsProfileEnabled[Index] then
      begin
        ATarget := ProfileTargets[Index];
        if ATarget.Valid then
        begin
          RemoveEnvironment(ATarget);
          {$IFDEF MSWINDOWS}
          if not Target.IsTurboExplorer then
            UnregisterExperts(ATarget);
          {$ENDIF MSWINDOWS}
          if not Target.IsTurboExplorer then
            UnregisterPackages(ATarget);
        end;
      end;
    end
    else
    begin
      RemoveEnvironment(Target);
      {$IFDEF MSWINDOWS}
      if not Target.IsTurboExplorer then
        UnregisterExperts(Target);
      {$ENDIF MSWINDOWS}
      if not Target.IsTurboExplorer then
        UnregisterPackages(Target);
    end;

    RemoveMake;
    if not Target.IsTurboExplorer then
      DeletePackages;
    {$IFDEF MSWINDOWS}
    DeleteExperts;
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
    Option: TInstallerOption;
    Id, Index: Integer;
    ADemoList: TStrings;
  begin
    AConfiguration := InstallCore.Configuration;
    if not (Assigned(AConfiguration) and Assigned(GUIPage)) then
      Exit;

    // clean section before saving options
    AConfiguration.DeleteSection(TargetName);
    AConfiguration.DeleteSection(FDemoSectionName);

    for Option := Low(TInstallerOption) to High(TInstallerOption) do
    begin
      Id := OptionData[Option].Id;
      AConfiguration.OptionAsBool[TargetName, Id] := GUIPage.OptionChecked[Id];
    end;

    if not Target.IsTurboExplorer then
    begin
      if FRuntimeInstallation and (CLRVersion = '') then
      begin
        ADemoList := GetDemoList;
        for Index := 0 to ADemoList.Count - 1 do
        begin
          Id := Integer(ADemoList.Objects[Index]);
          AConfiguration.OptionAsBool[FDemoSectionName, Id] := GUIPage.OptionChecked[Id];
        end;
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


  function CompileUnits: Boolean;
  begin
    Result := Compiler.Execute(StringsToStr(UnitList, ' '));
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
      begin
        Result := Result and FileCopy(FileName, TargetDir + FileName, True);

        // Always remove once copied because if they are left in place they
        // will clutter the source folder and might even prevent compilation
        // when multiple versions of C++ Builder are installed on the same
        // computer. The easiest way to see this is when checking HPP files.
        FileDelete(FileName);        
      end;
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
      Compiler.Options.Add('-D_RTLDLL' + DirSeparator + 'NO_STRICT' + DirSeparator + 'USEPACKAGES'); // $(SYSDEFINES)
      if Debug then
        UnitOutputDir := FLibDebugDir
      else
        UnitOutputDir := FLibReleaseDir;

      if (Target.RadToolKind = brBorlandDevStudio) and (Target.VersionNumber >= 4) then
      begin
        Compiler.AddPathOption('N0', UnitOutputDir); // .dcu files
        //Compiler.AddPathOption('NH', FIncludeDir);   // .hpp files
        Compiler.AddPathOption('NO', UnitOutputDir); // .obj files
        if TJclBDSInstallation(Target).DualPackageInstallation and OptionChecked[joJCLCopyPackagesHppFiles] then
          Compiler.AddPathOption('N1',Target.VclIncludeDir);
      end
      else
      begin
        Compiler.AddPathOption('N0', UnitOutputDir); // .dcu files
        //Compiler.AddPathOption('N1', FIncludeDir);   // .hpp files
        Compiler.AddPathOption('N2', UnitOutputDir); // .obj files
      end;
      Compiler.Options.Add('-JPHNE');
      Compiler.Options.Add('--BCB');
      //Compiler.AddPathOption('O', Format(BCBIncludePath, [Distribution.JclIncludeDir, Distribution.JclSourcePath]));
      //Compiler.AddPathOption('U', Format(BCBObjectPath, [Distribution.JclIncludeDir, Distribution.JclSourcePath]));
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
    Compiler.AddPathOption('I', Distribution.JclIncludeDir);
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
        if OptionChecked[joJCLCopyHppFiles] then
        begin
          MarkOptionBegin(joJCLCopyHppFiles);
          WriteLog('Copying .hpp files...');
          Result := Result and CopyHppFiles(Target.VclIncludeDir);
          MarkOptionEnd(joJCLCopyHppFiles, Result);
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
    DirDelimiter, Target.VersionNumberStr, DirDelimiter, Name, SourceExtensionDelphiPackage]);
  WriteLog(Format('Compiling CLR package %s...', [ProjectFileName]));

  if Assigned(GUIPage) then
    GUIPage.CompilationStart(ExtractFileName(Name));

  Result := TJclBDSInstallation(Target).CompileDelphiDotNetProject(ProjectFileName,
    GetBplPath, TargetPlatform, CLRVersion);
end;
{$ENDIF MSWINDOWS}

function TJclInstallation.CompilePackage(const Name: string): Boolean;
var
  PackageFileName: string;
{$IFNDEF KYLIX}
  DpkPackageFileName: string;
{$ENDIF}
begin
  PackageFileName := PathAddSeparator(Distribution.JclPath) + Name;
  WriteLog(Format('Compiling package %s...', [PackageFileName]));

  if Assigned(GUIPage) then
    GUIPage.CompilationStart(ExtractFileName(Name));

  if IsDelphiPackage(PackageFileName) and (bpDelphi32 in Target.Personalities) then
  begin
    {$IFNDEF KYLIX}
    if Target.RadToolKind = brBorlandDevStudio then
      (Target as TJclBDSInstallation).CleanPackageCache(BinaryFileName(GetBplPath, PackageFileName));
    {$ENDIF ~KYLIX}
    Result := Target.CompilePackage(PackageFileName, GetBplPath, GetDcpPath);
  end
  else if IsBCBPackage(PackageFileName) and (bpBCBuilder32 in Target.Personalities) then
  begin
    ConfigureBpr2Mak(PackageFileName);
    {$IFDEF KYLIX}
    Result := Target.CompilePackage(PackageFileName, GetBplPath, GetDcpPath);
    {$ELSE ~KYLIX}

    if Target.RadToolKind = brBorlandDevStudio then
      (Target as TJclBDSInstallation).CleanPackageCache(BinaryFileName(GetBplPath, PackageFileName));

    // to satisfy JVCL (and eventually other libraries), create a .dcp file;
    // Note: it is put out to .bpl path to make life easier for JVCL
    DpkPackageFileName := ChangeFileExt(PackageFileName, SourceExtensionDelphiPackage);
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
  OldDirectory, NewDirectory: string;
begin
  NewDirectory := ExtractFileDir(FileName);
  FileName := ExtractFileName(FileName);
  WriteLog(Format(RsBuildingMessage, [FileName]));
  OldDirectory := GetCurrentDir;
  try
    SetCurrentDir(NewDirectory);
    Target.DCC32.Options.Clear;
    Target.DCC32.SetDefaultOptions;
    Target.DCC32.AddPathOption('E', Distribution.JclBinDir);
    Target.DCC32.AddPathOption('N', '.');
    Target.DCC32.AddPathOption('U', FLibReleaseDir + DirSeparator + Distribution.JclSourcePath);
    Target.DCC32.AddPathOption('I', Distribution.JclIncludeDir);
    Result := Target.DCC32.Execute(FileName);
  finally
    SetCurrentDir(OldDirectory);
  end;
end;

function TJclInstallation.DeletePackage(const Name: string): Boolean;
var
  PackageFileName: string;
  BPLFileName: string;
begin
  WriteLog(Format('Deleting package %s.', [Name]));
  PackageFileName := Distribution.JclPath + Format(Name, [Target.VersionNumberStr]);

  BPLFileName := BinaryFileName(GetBplPath, PackageFileName);

  Result := FileDelete(BPLFileName);
  Result := FileDelete(ChangeFileExt(BPLFileName, CompilerExtensionMAP)) or Result;

  // delete DCP files that were created to bpl path (old behavior)
  Result := FileDelete(PathAddSeparator(GetBPLPath) + PathExtractFileNameNoExt(Name) + CompilerExtensionDCP) or Result;
  // delete DCP files that were created to target dcp path (old behavior)
  Result := FileDelete(PathAddSeparator(Target.DCPOutputPath) + PathExtractFileNameNoExt(Name) + CompilerExtensionDCP) or Result;
  // delete BPI files that were created to target dcp path (old behavior)
  Result := FileDelete(PathAddSeparator(Target.DCPOutputPath) + PathExtractFileNameNoExt(Name) + CompilerExtensionBPI) or Result;
  // delete LIB files that were created to target dcp path (old behaviour)
  Result := FileDelete(PathAddSeparator(Target.DCPOutputPath) + PathExtractFileNameNoExt(Name) + CompilerExtensionLIB) or Result;

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
  SetEnvironmentVar('OBJDIR', FLibReleaseDir);
  SetEnvironmentVar('BPILIBDIR', GetDcpPath);
  SetEnvironmentVar('BPLDIR', GetBplPath);
  {$ELSE ~KYLIX}
  if clMake in Target.CommandLineTools then
  begin
    Target.Make.Options.Clear;
    Target.Make.AddPathOption('DBPILIBDIR=', GetDcpPath);
    Target.Make.AddPathOption('DBPLDIR=', GetBplPath);
    if OptionChecked[joJCLCopyPackagesHppFiles] then
    //begin
    //  MarkOptionBegin(joJCLCopyPackagesHppFiles);
      Target.Make.AddPathOption('DHPPDIR=', Target.VclIncludeDir);
    //  MarkOptionEnd(joJCLCopyPackagesHppFiles, True);
    //end;
  end;
  {$ENDIF ~KYLIX}
end;

{$IFDEF MSWINDOWS}
function TJclInstallation.CompileExpert(const Name: string): Boolean;
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

  WriteLog(Format('Compiling expert %s...', [ProjectFileName]));

  if Assigned(GUIPage) then
    GUIPage.CompilationStart(ExtractFileName(Name));

  if IsDelphiProject(ProjectFileName) and (bpDelphi32 in Target.Personalities) then
    Result := Target.CompileProject(ProjectFileName, GetBplPath, GetDcpPath)
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
        // second compilation
        Result := Target.CompileProject(ProjectFileName, GetBplPath, GetDcpPath)
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
  if not StrSame(ExtractFileExt(FileInfo.Name), '.dproj') then
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
    ExclusionFileName, FileName, RequiredList, RequiredItem: string;
    IndexExc, IndexDemo, SepPos, IndexReq: Integer;
    ExcludeDemo: Boolean;
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
          SepPos := Pos('=', FileName);
          if SepPos > 0 then
          begin
            ExcludeDemo := False;
            RequiredList := Copy(FileName, SepPos + 1, Length(FileName) - SepPos);
            SetLength(FileName, SepPos - 1);
            for IndexReq := 0 to PathListItemCount(RequiredList) - 1 do
            begin
              RequiredItem := PathListGetItem(RequiredList, IndexReq);
              if AnsiSameText(ExtractFileExt(RequiredItem), '.dcu') then
              begin
                ExcludeDemo := not FileExists(PathAddSeparator(Target.LibFolderName) + RequiredItem);
                if ExcludeDemo then
                  Break;
              end;
            end;
          end
          else
            ExcludeDemo := True;

          if ExcludeDemo then
          begin
            if AnsiSameText(ExtractFileExt(FileName), '.exc') then
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
  Settings: IJediConfiguration;
begin
  Settings := InstallCore.Configuration;
  if Assigned(Settings) and Assigned(FProfilesPage) then
    for I := 0 to InstallCore.ProfilesManager.ProfileCount - 1 do
      Settings.OptionAsBoolByName[ProfilesSectionName, InstallCore.ProfilesManager.ProfileNames[I]] := FProfilesPage.IsProfileEnabled[I]; 
  for I := 0 to TargetInstallCount - 1 do
    TargetInstalls[I].Close;
  FGUI := nil;
end;

constructor TJclDistribution.Create;
  procedure RegisterJclOptions;
  var
    Option: TInstallerOption;
    AInstallCore: TJediInstallCore;
    OptionName: string;
  begin
    AInstallCore := InstallCore;
    for Option := Low(TInstallerOption) to High(TInstallerOption) do
    begin
      OptionName := GetEnumName(TypeInfo(TInstallerOption), Integer(Option));
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
        Result := ((Target.VersionNumber in [1, 2]) and (bpDelphi32 in Target.Personalities))
          or (Target.VersionNumber in [3, 4, 5, 6]);
      else
        Result := False;
    end;
    Result := Result and (Target.Personalities * [bpDelphi32, bpBCBuilder32, bpDelphiNet32, bpDelphiNet64] <> []);
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
    TextFile: TJclAnsiMappedTextReader;
  begin
    Result := 0;

    DailyFileName := FJclPath + DailyRevisionFileName;
    if FileExists(DailyFileName) then
    begin
      // directory from a daily zip
      TextFile := TJclAnsiMappedTextReader.Create(DailyFileName);
      try
        RevisionText := string(TextFile.ReadLn);
        Result := StrToIntDef(RevisionText, 0);
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
        TextFile := TJclAnsiMappedTextReader.Create(SvnEntriesFileName);
        try
          TextFile.ReadLn;
          TextFile.ReadLn;
          TextFile.ReadLn;
          RevisionText := string(TextFile.ReadLn);
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
    ExceptDialogsPath, InstallerFileName, ProfileName: string;
    ReadMePage: IJediReadMePage;
    Index: Integer;
    Settings: IJediConfiguration;
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
    FJclIncludeDir := PathAddSeparator(FJclSourceDir) + 'include';
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

      if InstallCore.ProfilesManager.MultipleProfileMode then
      begin
        FProfilesPage := GUI.CreateProfilesPage;
        FProfilesPage.Caption := 'Profiles';

        Settings := InstallCore.Configuration;
        if Settings <> nil then
          for Index := 0 to InstallCore.ProfilesManager.ProfileCount - 1 do
        begin
          ProfileName := InstallCore.ProfilesManager.ProfileNames[Index];
          if Settings.ValueExists(ProfilesSectionName, ProfileName) then
            FProfilesPage.IsProfileEnabled[Index] := Settings.OptionAsBoolByName[ProfilesSectionName, ProfileName];
        end;
      end;
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

function TJclDistribution.Install: Boolean;
var
  I: Integer;
  KeepSettings: Boolean;
  AInstallation: TJclInstallation;
begin
  KeepSettings := True;
  try
    if RadToolInstallations.AnyInstanceRunning {$IFDEF MSWINDOWS} and not IsDebuggerAttached {$ENDIF} then
    begin
      if Assigned(GUI) then
        GUI.Dialog(RsCloseRADTool, dtError, [drCancel]);
      Result := False;
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

    Result := True;
    for I := 0 to TargetInstallCount - 1 do
    begin
      AInstallation := TargetInstalls[I];
      if AInstallation.Enabled then
      begin
        AInstallation.Silent := False;
        if (AInstallation.CLRVersion = '') and not KeepSettings then
          AInstallation.RemoveSettings;
        AInstallation.Uninstall(False);
        Result := AInstallation.Install;
        if not Result then
          Break;
        Inc(FNbInstalled);
      end;
    end;

    {$IFDEF MSWINDOWS}
    Result := Result and RegHelpExecuteCommands(True);
    {$ENDIF MSWINDOWS}

    if Assigned(GUI) then
    begin
      if Result then
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
  ResultLines: TJclAnsiMappedTextReader;
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
  if Assigned(GUI) and not IsElevated then
    GUI.Dialog(RsHTMLHelp2Credentials, dtInformation, [drOK]);

  // RegHelper.exe manifest requires elevation on Vista
  if IsAdministrator or IsWinVista or IsWinServer2008 or IsWin7 or IsWinServer2008R2 then
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
    ResultLines := TJclAnsiMappedTextReader.Create(LogFileName);
    try
      while not ResultLines.Eof do
      begin
        ProgramResult := string(ResultLines.ReadLn);
        if AnsiPos('ERROR', AnsiUpperCase(ProgramResult)) > 0 then
        begin
          Result := False;
          if Assigned(GUI) then
            GUI.Dialog('RegHelper raised an error while executing RegHelp command: ' + NativeLineBreak + ProgramResult, dtError, [drCancel]);
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

function TJclDistribution.Uninstall: Boolean;
var
  I: Integer;
  AInstallation: TJclInstallation;
begin
  try
    if RadToolInstallations.AnyInstanceRunning {$IFDEF MSWINDOWS} and not IsDebuggerAttached {$ENDIF} then
    begin
      if Assigned(GUI) then
        GUI.Dialog(RsCloseRADTool, dtError, [drCancel]);
      Result := False;
      Exit;
    end;

    if Assigned(GUI) then
      GUI.Status := 'Initializing JCL uninstallation process';

    {$IFDEF MSWINDOWS}
    RegHelpClearCommands;
    {$ENDIF MSWINDOWS}

    Result := True;
    for I := 0 to TargetInstallCount - 1 do
    begin
      AInstallation := TargetInstalls[I];
      AInstallation.Silent := False;
      if AInstallation.Enabled and ((not AInstallation.RemoveSettings) or not AInstallation.Uninstall(True)) then
        Result := False;
    end;

    {$IFDEF MSWINDOWS}
    RegHelpExecuteCommands(False);
    {$ENDIF MSWINDOWS}

    if Assigned(GUI) then
    begin
      if Result then
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