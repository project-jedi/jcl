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
{ The Original Code is DelphiInstall.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Andreas Hausladen (ahuser)                                                                     }
{   Florent Ouchet (outchy)                                                                        }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair) - crossplatform & BCB support                                      }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Routines for getting information about installed versions of Delphi/C++Builder and performing    }
{ basic installation tasks.                                                                        }
{                                                                                                  }
{ Important notes for C#Builder 1 and Delphi 8:                                                    }
{ These products were not shipped with their native compilers, but the toolkit to build design     }
{ packages is available in codecentral (http://codecentral.borland.com):                           }
{  - "IDE Integration pack for C#Builder 1.0" http://codecentral.borland.com/Item.aspx?ID=21334    }
{  - "IDE Integration pack for Delphi 8" http://codecentral.borland.com/Item.aspx?ID=21333         }
{ It's recommended to extract zip files using the standard pattern of Delphi directories:          }
{  - Binary files go to \bin (DCC32.EXE, RLINK32.DLL and lnkdfm7*.dll)                             }
{  - Compiler files go to \lib (designide.dcp, rtl.dcp, SysInit.dcu, vcl.dcp, vclactnband.dcp,     }
{    vcljpg.dcp and vclx.dcp)                                                                      }
{  - ToolsAPI files go to \source\ToolsAPI (PaletteAPI.pas, PropInspAPI.pas and ToolsAPI.pas)      }
{ Don't mix C#Builder 1 files with Delphi 8 and vice-versa otherwise the compilation will fail     }
{ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                   }
{ !!!!!!!!      The DCPPath for these releases have to $(BDS)\lib      !!!!!!!!!                   }
{ !!!!!!!!    or the directory where compiler files were extracted     !!!!!!!!!                   }
{ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                   }
{ The default BPL output directory for these products is set to $(BDSPROJECTSDIR)\bpl, it may not  }
{ exist since the product installers don't create it                                               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Unit owner: Petr Vones                                                                           }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date$

unit JclBorlandTools;

{$I jcl.inc}
{$I crossplatform.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  MSHelpServices_TLB,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils, IniFiles, Contnrs,
  JclBase, JclSysUtils;

// Various definitions
type
  EJclBorRADException = class (Exception);

  TJclBorRADToolKind = (brDelphi, brCppBuilder, brBorlandDevStudio);
  {$IFDEF KYLIX}
  TJclBorRADToolEdition = (deOPEN, dePRO, deSVR);
  {$ELSE}
  TJclBorRADToolEdition = (deSTD, dePRO, deCSS, deARC);
  {$ENDIF KYLIX}
  TJclBorRADToolPath = string;

const
  SupportedDelphiVersions       = [5, 6, 7, 8, 9, 10];
  SupportedBCBVersions          = [5, 6, 10];
  SupportedBDSVersions          = [1, 2, 3, 4];

  // Object Repository
  BorRADToolRepositoryPagesSection    = 'Repository Pages';

  BorRADToolRepositoryDialogsPage     = 'Dialogs';
  BorRADToolRepositoryFormsPage       = 'Forms';
  BorRADToolRepositoryProjectsPage    = 'Projects';
  BorRADToolRepositoryDataModulesPage = 'Data Modules';

  BorRADToolRepositoryObjectType      = 'Type';
  BorRADToolRepositoryFormTemplate    = 'FormTemplate';
  BorRADToolRepositoryProjectTemplate = 'ProjectTemplate';
  BorRADToolRepositoryObjectName      = 'Name';
  BorRADToolRepositoryObjectPage      = 'Page';
  BorRADToolRepositoryObjectIcon      = 'Icon';
  BorRADToolRepositoryObjectDescr     = 'Description';
  BorRADToolRepositoryObjectAuthor    = 'Author';
  BorRADToolRepositoryObjectAncestor  = 'Ancestor';
  BorRADToolRepositoryObjectDesigner  = 'Designer'; // Delphi 6+ only
  BorRADToolRepositoryDesignerDfm     = 'dfm';
  BorRADToolRepositoryDesignerXfm     = 'xfm';
  BorRADToolRepositoryObjectNewForm   = 'DefaultNewForm';
  BorRADToolRepositoryObjectMainForm  = 'DefaultMainForm';

  SourceExtensionDelphiPackage = '.dpk';
  SourceExtensionBCBPackage    = '.bpk';
  SourceExtensionDelphiProject = '.dpr';
  SourceExtensionBCBProject    = '.bpr';
  BinaryExtensionPackage       = '.bpl';
  BinaryExtensionLibrary       = '.dll';
  BinaryExtensionExecutable    = '.exe';
  CompilerExtensionDCP         = '.dcp';
  CompilerExtensionBPI         = '.bpi';
  CompilerExtensionLIB         = '.lib';
  CompilerExtensionTDS         = '.tds';
  CompilerExtensionMAP         = '.map';
  CompilerExtensionDEF         = '.def';

  ProjectTypePackage = 'package';
  ProjectTypeLibrary = 'library';
  ProjectTypeProgram = 'program'; 

  PersonalityDelphi    = 'Delphi';
  PersonalityBCB       = 'C++Builder';
  PersonalityCSB       = 'C#Builder';
  PersonalityBDS       = 'Borland Developer Studio';

  DOFDirectoriesSection = 'Directories';
  DOFUnitOutputDirKey   = 'UnitOutputDir';
  DOFSearchPathName     = 'SearchPath';
  DOFLinkerSection      = 'Linker';
  DOFPackagesKey        = 'Packages';
  DOFCompilerSection    = 'Compiler';
  DOFPackageNoLinkKey   = 'PackageNoLink';

  {$IFDEF KYLIX}
  BorRADToolEditionIDs: array [TJclBorRADToolEdition] of PChar =
    ('OPEN', 'PRO', 'SVR');
  {$ELSE ~KYLIX}
  BorRADToolEditionIDs: array [TJclBorRADToolEdition] of PChar =
    ('STD', 'PRO', 'CSS', 'ARC'); // 'ARC' is an assumption
  {$ENDIF ~KYLIX}

// Installed versions information classes
type
  TJclBorPersonality = (bpDelphi32, bpBCBuilder32, bpDelphiNet32, bpDelphiNet64,
    bpCSBuilder32, bpCSBuilder64);
  //  bpDelphi64, bpBCBuilder64);
  
  TJclBorPersonalities = set of TJclBorPersonality;

  TJclBorRADToolInstallation = class;

  TJclBorRADToolInstallationObject = class(TInterfacedObject)
  private
    FInstallation: TJclBorRADToolInstallation;
  protected
    constructor Create(AInstallation: TJclBorRADToolInstallation);
  public
    property Installation: TJclBorRADToolInstallation read FInstallation;
  end;

  {$IFDEF MSWINDOWS}
  TJclBorlandOpenHelp = class(TJclBorRADToolInstallationObject)
  private
    function GetContentFileName: string;
    function GetIndexFileName: string;
    function GetLinkFileName: string;
    function GetGidFileName: string;
    function GetProjectFileName: string;
    function ReadFileName(const FormatName: string): string;
  public
    function AddHelpFile(const HelpFileName, IndexName: string): Boolean;
    function RemoveHelpFile(const HelpFileName, IndexName: string): Boolean;
    property ContentFileName: string read GetContentFileName;
    property GidFileName: string read GetGidFileName;
    property IndexFileName: string read GetIndexFileName;
    property LinkFileName: string read GetLinkFileName;
    property ProjectFileName: string read GetProjectFileName;
  end;

  TJclHelp2Object = (hoRegisterSession, hoRegister, hoPlugin);
  TJclHelp2Objects = set of TJclHelp2Object;

  TJclHelp2Manager = class(TJclBorRADToolInstallationObject)
  private
    FHxRegisterSession: IHxRegisterSession;
    FHxRegister: IHxRegister;
    FHxPlugin: IHxPlugIn;
    function RequireObject(HelpObjects: TJclHelp2Objects): Boolean;
    function GetHxPlugin: IHxPlugin;
    function GetHxRegister: IHxRegister;
    function GetHxRegisterSession: IHxRegisterSession;
  protected
    constructor Create(AInstallation: TJclBorRADToolInstallation); overload;
  public
    constructor Create; overload;
    destructor Destroy; override;
    function CreateTransaction: Boolean;
    function CommitTransaction: Boolean;
    function RegisterNameSpace(const Name, Collection,
      Description: WideString): Boolean;
    function UnregisterNameSpace(const Name: WideString): Boolean;
    function RegisterHelpFile(const NameSpace, Identifier: WideString;
      const LangId: Integer; const HxSFile, HxIFile: WideString): Boolean;
    function UnregisterHelpFile(const NameSpace, Identifier: WideString;
      const LangId: Integer): Boolean;
    function PlugNameSpaceIn(const SourceNameSpace,
      TargetNameSpace: WideString): Boolean;
    function UnPlugNameSpace(const SourceNameSpace,
      TargetNameSpace: WideString): Boolean;
    function PlugNameSpaceInBorlandHelp(const NameSpace: WideString): Boolean;
    function UnPlugNameSpaceFromBorlandHelp(const NameSpace: WideString): Boolean;
    property HxRegisterSession: IHxRegisterSession read GetHxRegisterSession;
    property HxRegister: IHxRegister read GetHxRegister;
    property HxPlugin: IHxPlugin read GetHxPlugin;
  end;
  {$ENDIF MSWINDOWS}

  TJclBorRADToolIdeTool = class(TJclBorRADToolInstallationObject)
  private
    FKey: string;
    function GetCount: Integer;
    function GetParameters(Index: Integer): string;
    function GetPath(Index: Integer): string;
    function GetTitle(Index: Integer): string;
    function GetWorkingDir(Index: Integer): string;
    procedure SetCount(const Value: Integer);
    procedure SetParameters(Index: Integer; const Value: string);
    procedure SetPath(Index: Integer; const Value: string);
    procedure SetTitle(Index: Integer; const Value: string);
    procedure SetWorkingDir(Index: Integer; const Value: string);
  protected
    constructor Create(AInstallation: TJclBorRADToolInstallation);
    procedure CheckIndex(Index: Integer);
  public
    property Count: Integer read GetCount write SetCount;
    function IndexOfPath(const Value: string): Integer;
    function IndexOfTitle(const Value: string): Integer;
    procedure RemoveIndex(const Index: Integer);
    property Key: string read FKey;
    property Title[Index: Integer]: string read GetTitle write SetTitle;
    property Path[Index: Integer]: string read GetPath write SetPath;
    property Parameters[Index: Integer]: string read GetParameters write SetParameters;
    property WorkingDir[Index: Integer]: string read GetWorkingDir write SetWorkingDir;
  end;

  TJclBorRADToolIdePackages = class(TJclBorRADToolInstallationObject)
  private
    FDisabledPackages: TStringList;
    FKnownPackages: TStringList;
    FKnownIDEPackages: TStringList;
    FExperts: TStringList;
    function GetCount: Integer;
    function GetIDECount: Integer;
    function GetExpertCount: Integer;
    function GetPackageDescriptions(Index: Integer): string;
    function GetIDEPackageDescriptions(Index: Integer): string;
    function GetExpertDescriptions(Index: Integer): string;
    function GetPackageDisabled(Index: Integer): Boolean;
    function GetPackageFileNames(Index: Integer): string;
    function GetIDEPackageFileNames(Index: Integer): string;
    function GetExpertFileNames(Index: Integer): string;
  protected
    constructor Create(AInstallation: TJclBorRADToolInstallation);
    function PackageEntryToFileName(const Entry: string): string;
    procedure ReadPackages;
    procedure RemoveDisabled(const FileName: string);
  public
    destructor Destroy; override;
    function AddPackage(const FileName, Description: string): Boolean;
    function AddIDEPackage(const FileName, Description: string): Boolean;
    function AddExpert(const FileName, Description: string): Boolean;
    function RemovePackage(const FileName: string): Boolean;
    function RemoveIDEPackage(const FileName: string): Boolean;
    function RemoveExpert(const FileName: string): Boolean;
    property Count: Integer read GetCount;
    property IDECount: Integer read GetIDECount;
    property ExpertCount: Integer read GetExpertCount;
    property PackageDescriptions[Index: Integer]: string read GetPackageDescriptions;
    property IDEPackageDescriptions[Index: Integer]: string read GetIDEPackageDescriptions;
    property ExpertDescriptions[Index: Integer]: string read GetExpertDescriptions;
    property PackageFileNames[Index: Integer]: string read GetPackageFileNames;
    property IDEPackageFileNames[Index: Integer]: string read GetIDEPackageFileNames;
    property ExpertFileNames[Index: Integer]: string read GetExpertFileNames;
    property PackageDisabled[Index: Integer]: Boolean read GetPackageDisabled;
  end;

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
    property OutputCallback: TTextHandler write SetOutputCallback;
    property Output: string read GetOutput;
  end;

  EJclCommandLineToolError = class(EJclError);

  TJclCommandLineTool = class(TInterfacedObject, IJclCommandLineTool)
  private
    FExeName: string;
    FOptions: TStringList;
    FOutput: string;
    FOutputCallback: TTextHandler;
  protected
    function GetExeName: string;
    function GetOutput: string;
    function GetOptions: TStrings;
    function GetOutputCallback: TTextHandler;
    procedure SetOutputCallback(const CallbackMethod: TTextHandler);
    constructor Create(const AExeName: string);
    procedure AddPathOption(const Option, Path: string);
    function Execute(const CommandLine: string): Boolean;
    property ExeName: string read GetExeName;
    property Output: string read GetOutput;
  public
    destructor Destroy; override;
  end;

  TJclBorlandCommandLineTool = class(TJclBorRADToolInstallationObject, IJclCommandLineTool)
  private
    FOptions: TStringList;
    FOutputCallback: TTextHandler;
    FOutput: string;
  protected
    constructor Create(AInstallation: TJclBorRADToolInstallation); virtual;
    procedure CheckOutputValid;
    function GetExeName: string; virtual;
    function GetFileName: string;
    function GetOptions: TStrings;
    function GetOutputCallback: TTextHandler;
    procedure SetOutputCallback(const CallbackMethod: TTextHandler);
    function GetOutput: string;
  public
    destructor Destroy; override;
    procedure AddPathOption(const Option, Path: string);
    function Execute(const CommandLine: string): Boolean; virtual;
    property FileName: string read GetFileName;
    property Output: string read GetOutput;
    property OutputCallback: TTextHandler read FOutputCallback write SetOutputCallback;
    property Options: TStrings read GetOptions;
  end;

  TJclBCC32 = class(TJclBorlandCommandLineTool)
  protected
    constructor Create(AInstallation: TJclBorRADToolInstallation); override;
    function GetExeName: string; override;
  public
    {$IFDEF KEEP_DEPRECATED}
    function SupportsLibSuffix: Boolean;
    {$ENDIF KEEP_DEPRECATED}
  end;

  TJclDCC32 = class(TJclBorlandCommandLineTool)
  protected
    constructor Create(AInstallation: TJclBorRADToolInstallation); override;
    function GetExeName: string; override;
    procedure SaveOptionsToFile(const ConfigFileName: string);
    procedure AddProjectOptions(const ProjectFileName, DCPPath: string);
    function Compile(const ProjectFileName: string): Boolean;
  public
    function Execute(const CommandLine: string): Boolean; override;
    function MakePackage(const PackageName, BPLPath, DCPPath: string; ExtraOptions: string = ''): Boolean;
    function MakeProject(const ProjectName, OutputDir, DcpSearchPath: string; ExtraOptions: string = ''): Boolean;
    procedure SetDefaultOptions;
    {$IFDEF KEEP_DEPRECATED}
    function SupportsLibSuffix: Boolean;
    {$ENDIF KEEP_DEPRECATED}
  end;
  {$IFDEF KEEP_DEPRECATED}
  TJclDCC = TJclDCC32;
  {$ENDIF KEEP_DEPRECATED}

  TJclBpr2Mak = class(TJclBorlandCommandLineTool)
  protected
    function GetExeName: string; override;
  end;

  TJclBorlandMake = class(TJclBorlandCommandLineTool)
  protected
    function GetExeName: string; override;
  end;

  TJclBorRADToolPalette = class(TJclBorRADToolInstallationObject)
  private
    FKey: string;
    FTabNames: TStringList;
    function GetComponentsOnTab(Index: Integer): string;
    function GetHiddenComponentsOnTab(Index: Integer): string;
    function GetTabNameCount: Integer;
    function GetTabNames(Index: Integer): string;
    procedure ReadTabNames;
  protected
    constructor Create(AInstallation: TJclBorRADToolInstallation);
  public
    destructor Destroy; override;
    procedure ComponentsOnTabToStrings(Index: Integer; Strings: TStrings; IncludeUnitName: Boolean = False;
      IncludeHiddenComponents: Boolean = True);
    function DeleteTabName(const TabName: string): Boolean;
    function TabNameExists(const TabName: string): Boolean;
    property ComponentsOnTab[Index: Integer]: string read GetComponentsOnTab;
    property HiddenComponentsOnTab[Index: Integer]: string read GetHiddenComponentsOnTab;
    property Key: string read FKey;
    property TabNames[Index: Integer]: string read GetTabNames;
    property TabNameCount: Integer read GetTabNameCount;
  end;

  TJclBorRADToolRepository = class(TJclBorRADToolInstallationObject)
  private
    FIniFile: TIniFile;
    FFileName: string;
    FPages: TStringList;
    function GetIniFile: TIniFile;
    function GetPages: TStrings;
  protected
    constructor Create(AInstallation: TJclBorRADToolInstallation);
  public
    destructor Destroy; override;
    procedure AddObject(const FileName, ObjectType, PageName, ObjectName, IconFileName, Description,
      Author, Designer: string; const Ancestor: string = '');
    procedure CloseIniFile;
    function FindPage(const Name: string; OptionalIndex: Integer): string;
    procedure RemoveObjects(const PartialPath, FileName, ObjectType: string);
    property FileName: string read FFileName;
    property IniFile: TIniFile read GetIniFile;
    property Pages: TStrings read GetPages;
  end;


  TCommandLineTool = (clAsm, clBcc32, clDcc32, clDccIL, clMake, clProj2Mak);
  TCommandLineTools = set of TCommandLineTool;

  TJclBorRADToolInstallationClass = class of TJclBorRADToolInstallation;

  TJclBorRADToolInstallation = class(TObject)
  private
    FConfigData: TCustomIniFile;
    FConfigDataLocation: string;
    FGlobals: TStringList;
    FRootDir: string;
    FBinFolderName: string;
    FBCC32: TJclBCC32;
    FDCC32: TJclDCC32;
    FBpr2Mak: TJclBpr2Mak;
    FMake: IJclCommandLineTool;
    FEditionStr: string;
    FEdition: TJclBorRADToolEdition;
    FEnvironmentVariables: TStringList;
    FIdePackages: TJclBorRADToolIdePackages;
    FIdeTools: TJclBorRADToolIdeTool;
    FInstalledUpdatePack: Integer;
    {$IFDEF MSWINDOWS}
    FOpenHelp: TJclBorlandOpenHelp;
    {$ENDIF MSWINDOWS}
    FPalette: TJclBorRADToolPalette;
    FRepository: TJclBorRADToolRepository;
    FVersionNumber: Integer;
    FVersionNumberStr: string;
    FIDEVersionNumber: Integer; // Delphi 2005: 3   -  Delphi 7: 7
    FMapCreate: Boolean;
    {$IFDEF MSWINDOWS}
    FMapLink: Boolean;
    FMapDelete: Boolean;
    {$ENDIF MSWINDOWS}
    FCommandLineTools: TCommandLineTools;
    FPersonalities: TJclBorPersonalities;
    FOutputCallback: TTextHandler;
    function GetSupportsLibSuffix: Boolean;
    function GetBCC32: TJclBCC32;
    function GetDCC32: TJclDCC32;
    function GetBpr2Mak: TJclBpr2Mak;
    function GetMake: IJclCommandLineTool;
    function GetDebugDCUPath: TJclBorRADToolPath;
    function GetDescription: string;
    function GetEditionAsText: string;
    function GetIdeExeFileName: string;
    function GetGlobals: TStrings;
    function GetIdeExeBuildNumber: string;
    function GetIdePackages: TJclBorRADToolIdePackages;
    function GetLatestUpdatePack: Integer;
    function GetLibrarySearchPath: TJclBorRADToolPath;
    function GetPalette: TJclBorRADToolPalette;
    function GetRepository: TJclBorRADToolRepository;
    function GetUpdateNeeded: Boolean;
    function GetValid: Boolean;
    procedure SetLibrarySearchPath(const Value: TJclBorRADToolPath);
    function GetLibraryBrowsingPath: TJclBorRADToolPath;
    procedure SetLibraryBrowsingPath(const Value: TJclBorRADToolPath);
    procedure SetDebugDCUPath(const Value: TJclBorRADToolPath);
    procedure SetOutputCallback(const Value: TTextHandler);
  protected
    constructor Create(const AConfigDataLocation: string); virtual;

    function LinkMapFile(const BinaryFileName: string): Boolean;

    // compilation functions
    function CompileDelphiPackage(const PackageName, BPLPath, DCPPath: string): Boolean; overload; virtual;
    function CompileDelphiPackage(const PackageName, BPLPath, DCPPath, ExtraOptions: string): Boolean; overload; virtual;
    function CompileDelphiProject(const ProjectName, OutputDir, DcpSearchPath: string): Boolean; virtual;
    function CompileBCBPackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function CompileBCBProject(const ProjectName, OutputDir, DcpSearchPath: string): Boolean; virtual;

    // installation (=compilation+registration) / uninstallation(=unregistration+deletion) functions
    function InstallDelphiPackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function UninstallDelphiPackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function InstallBCBPackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function UninstallBCBPackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function InstallDelphiIdePackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function UninstallDelphiIdePackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function InstallBCBIdePackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function UninstallBCBIdePackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function InstallDelphiExpert(const ProjectName, OutputDir, DcpSearchPath: string): Boolean; virtual;
    function UninstallDelphiExpert(const ProjectName, OutputDir: string): Boolean; virtual;
    function InstallBCBExpert(const ProjectName, OutputDir, DcpSearchPath: string): Boolean; virtual;
    function UninstallBCBExpert(const ProjectName, OutputDir: string): Boolean; virtual;

    procedure ReadInformation;
    //function AddMissingPathItems(var Path: string; const NewPath: string): Boolean;
    function RemoveFromPath(var Path: string; const ItemsToRemove: string): Boolean;
    function GetDCPOutputPath: string; virtual;
    function GetBPLOutputPath: string; virtual;
    function GetEnvironmentVariables: TStrings; virtual;
    function GetVclIncludeDir: string; virtual;
    function GetName: string; virtual;
    procedure OutputString(const AText: string);
    function OutputFileDelete(const FileName: string): Boolean;
  public
    destructor Destroy; override;
    class procedure ExtractPaths(const Path: TJclBorRADToolPath; List: TStrings);
    class function GetLatestUpdatePackForVersion(Version: Integer): Integer; virtual;
    class function PackageSourceFileExtension: string; virtual;
    class function ProjectSourceFileExtension: string; virtual;
    class function RadToolKind: TJclBorRadToolKind; virtual;
    {class} function RadToolName: string; virtual;
    function AnyInstanceRunning: Boolean;
    function AddToDebugDCUPath(const Path: string): Boolean;
    function AddToLibrarySearchPath(const Path: string): Boolean;
    function AddToLibraryBrowsingPath(const Path: string): Boolean;
    {$IFDEF KYLIX}
    function ConfigFileName(const Extension: string): string; virtual;
    {$ENDIF KYLIX}
    function FindFolderInPath(Folder: string; List: TStrings): Integer;
    // package functions
      // install = package compile + registration
      // uninstall = unregistration + deletion
    function CompilePackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function InstallPackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function UninstallPackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function InstallIDEPackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function UninstallIDEPackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;

    // project functions
    function CompileProject(const ProjectName, OutputDir, DcpSearchPath: string): Boolean; virtual;
    // expert functions
      // install = project compile + registration
      // uninstall = unregistration + deletion
    function InstallExpert(const ProjectName, OutputDir, DcpSearchPath: string): Boolean; virtual;
    function UninstallExpert(const ProjectName, OutputDir: string): Boolean; virtual;

    // registration/unregistration functions
    function RegisterPackage(const BinaryFileName, Description: string): Boolean; virtual;
    function UnregisterPackage(const BinaryFileName: string): Boolean; virtual;
    function RegisterIDEPackage(const BinaryFileName, Description: string): Boolean; virtual;
    function UnregisterIDEPackage(const BinaryFileName: string): Boolean; virtual;
    function RegisterExpert(const BinaryFileName, Description: string): Boolean; virtual;
    function UnregisterExpert(const BinaryFileName: string): Boolean; virtual;

    {$IFDEF KEEP_DEPRECATED}
    function IsBDSPersonality: Boolean;
    {$ENDIF KEEP_DEPRECATED}
    function GetDefaultProjectsDir: string; virtual;
    function RemoveFromDebugDCUPath(const Path: string): Boolean;
    function RemoveFromLibrarySearchPath(const Path: string): Boolean;
    function RemoveFromLibraryBrowsingPath(const Path: string): Boolean; 
    function SubstitutePath(const Path: string): string;
    {$IFDEF KEEP_DEPRECATED}
    function SupportsBCB: Boolean;
    {$ENDIF KEEP_DEPRECATED}
    function SupportsVisualCLX: Boolean;
    function LibFolderName: string;
    // Command line tools
    property CommandLineTools: TCommandLineTools read FCommandLineTools;
    property BCC32: TJclBCC32 read GetBCC32;
    property DCC32: TJclDCC32 read GetDCC32;
    property Bpr2Mak: TJclBpr2Mak read GetBpr2Mak;
    property Make: IJclCommandLineTool read GetMake;
    // Paths
    property BinFolderName: string read FBinFolderName;
    property BPLOutputPath: string read GetBPLOutputPath;
    property DebugDCUPath: TJclBorRADToolPath read GetDebugDCUPath write SetDebugDCUPath;
    property DCPOutputPath: string read GetDCPOutputPath;
    property DefaultProjectsDir: string read GetDefaultProjectsDir;
    //
    property Description: string read GetDescription;
    property Edition: TJclBorRADToolEdition read FEdition;
    property EditionAsText: string read GetEditionAsText;
    property EnvironmentVariables: TStrings read GetEnvironmentVariables;
    property IdePackages: TJclBorRADToolIdePackages read GetIdePackages;
    property IdeTools: TJclBorRADToolIdeTool read FIdeTools;
    property IdeExeBuildNumber: string read GetIdeExeBuildNumber;
    property IdeExeFileName: string read GetIdeExeFileName;
    property InstalledUpdatePack: Integer read FInstalledUpdatePack;
    property LatestUpdatePack: Integer read GetLatestUpdatePack;
    property LibrarySearchPath: TJclBorRADToolPath read GetLibrarySearchPath write SetLibrarySearchPath;
    property LibraryBrowsingPath: TJclBorRADToolPath read GetLibraryBrowsingPath write SetLibraryBrowsingPath;
    {$IFDEF MSWINDOWS}
    property OpenHelp: TJclBorlandOpenHelp read FOpenHelp;
    {$ENDIF MSWINDOWS}
    property MapCreate: Boolean read FMapCreate write FMapCreate;
    {$IFDEF MSWINDOWS}
    property MapLink: Boolean read FMapLink write FMapLink;
    property MapDelete: Boolean read FMapDelete write FMapDelete;
    {$ENDIF MSWINDOWS}
    property ConfigData: TCustomIniFile read FConfigData;
    property ConfigDataLocation: string read FConfigDataLocation;
    property Globals: TStrings read GetGlobals;
    property Name: string read GetName;
    property Palette: TJclBorRADToolPalette read GetPalette;
    property Repository: TJclBorRADToolRepository read GetRepository;
    property RootDir: string read FRootDir;
    property UpdateNeeded: Boolean read GetUpdateNeeded;
    property Valid: Boolean read GetValid;
    property VclIncludeDir: string read GetVclIncludeDir;
    property IDEVersionNumber: Integer read FIDEVersionNumber;
    property VersionNumber: Integer read FVersionNumber;
    property VersionNumberStr: string read FVersionNumberStr;
    property Personalities: TJclBorPersonalities read FPersonalities;
    {$IFDEF KEEP_DEPRECATED}
    property DCC: TJclDCC32 read GetDCC32;
    {$ENDIF KEEP_DEPRECATED}
    property SupportsLibSuffix: Boolean read GetSupportsLibSuffix;
    property OutputCallback: TTextHandler read FOutputCallback write SetOutputCallback;
  end;

  TJclBCBInstallation = class(TJclBorRADToolInstallation)
  protected
    constructor Create(const AConfigDataLocation: string); override;
    function GetEnvironmentVariables: TStrings; override;
  public
    destructor Destroy; override;
    class function PackageSourceFileExtension: string; override;
    class function ProjectSourceFileExtension: string; override;
    class function RadToolKind: TJclBorRadToolKind; override;
    {class }function RadToolName: string; override;
    class function GetLatestUpdatePackForVersion(Version: Integer): Integer; override;
    {$IFDEF KYLIX}
    function ConfigFileName(const Extension: string): string; override;
    {$ENDIF KYLIX}
  end;

  TJclDelphiInstallation = class(TJclBorRADToolInstallation)
  protected
    constructor Create(const AConfigDataLocation: string); override;
    function GetEnvironmentVariables: TStrings; override;
  public
    destructor Destroy; override;
    class function PackageSourceFileExtension: string; override;
    class function ProjectSourceFileExtension: string; override;
    class function RadToolKind: TJclBorRadToolKind; override;
    class function GetLatestUpdatePackForVersion(Version: Integer): Integer; override;
    function InstallPackage(const PackageName, BPLPath, DCPPath: string): Boolean; reintroduce;
    {class }function RadToolName: string; override;
    {$IFDEF KYLIX}
    function ConfigFileName(const Extension: string): string; override;
    {$ENDIF KYLIX}
  end;

  {$IFDEF MSWINDOWS}
  TJclBDSInstallation = class(TJclBorRADToolInstallation)
  private
    FDualPackageInstallation: Boolean;
    FHelp2Manager: TJclHelp2Manager;
    procedure SetDualPackageInstallation(const Value: Boolean);
    function GetCppBrowsingPath: TJclBorRADToolPath;
    function GetCppSearchPath: TJclBorRADToolPath;
    procedure SetCppBrowsingPath(const Value: TJclBorRADToolPath);
    procedure SetCppSearchPath(const Value: TJclBorRADToolPath);
  protected
    constructor Create(const AConfigDataLocation: string); override;
    function GetDCPOutputPath: string; override;
    function GetBPLOutputPath: string; override;
    function GetEnvironmentVariables: TStrings; override;
    function CompileDelphiPackage(const PackageName, BPLPath, DCPPath, ExtraOptions: string): Boolean; override;
    function CompileDelphiProject(const ProjectName, OutputDir: string;
      const DcpSearchPath: string): Boolean; override;
    function GetVclIncludeDir: string; override;
    function GetName: string; override;
  public
    destructor Destroy; override;
    class function PackageSourceFileExtension: string; override;
    class function ProjectSourceFileExtension: string; override;
    class function RadToolKind: TJclBorRadToolKind; override;
    class function GetLatestUpdatePackForVersion(Version: Integer): Integer; override;
    function GetBorlandStudioProjectsDir: string;
    function GetDefaultProjectsDir: string; override;
    {class }function RadToolName: string; override;

    function AddToCppSearchPath(const Path: string): Boolean; 
    function AddToCppBrowsingPath(const Path: string): Boolean;
    function RemoveFromCppSearchPath(const Path: string): Boolean;
    function RemoveFromCppBrowsingPath(const Path: string): Boolean;

    property CppSearchPath: TJclBorRADToolPath read GetCppSearchPath write SetCppSearchPath;
    property CppBrowsingPath: TJclBorRADToolPath read GetCppBrowsingPath write SetCppBrowsingPath;

    function RegisterPackage(const BinaryFileName, Description: string): Boolean; override;
    function UnregisterPackage(const BinaryFileName: string): Boolean; override;
    function CleanPackageCache(const BinaryFileName: string): Boolean;

    property DualPackageInstallation: Boolean read FDualPackageInstallation write SetDualPackageInstallation;
    property Help2Manager: TJclHelp2Manager read FHelp2Manager;
  end;
  {$ENDIF MSWINDOWS}

  TTraverseMethod = function (Installation: TJclBorRADToolInstallation): Boolean of object;

  TJclBorRADToolInstallations = class(TObject)
  private
    FList: TObjectList;
    function GetBDSInstallationFromVersion(
      VersionNumber: Integer): TJclBorRADToolInstallation;
    function GetBDSVersionInstalled(VersionNumber: Integer): Boolean;
    function GetCount: Integer;
    function GetInstallations(Index: Integer): TJclBorRADToolInstallation;
    function GetBCBVersionInstalled(VersionNumber: Integer): Boolean;
    function GetDelphiVersionInstalled(VersionNumber: Integer): Boolean;
    function GetBCBInstallationFromVersion(VersionNumber: Integer): TJclBorRADToolInstallation;
    function GetDelphiInstallationFromVersion(VersionNumber: Integer): TJclBorRADToolInstallation;
  protected
    procedure ReadInstallations;
  public
    constructor Create;
    destructor Destroy; override;
    function AnyInstanceRunning: Boolean;
    function AnyUpdatePackNeeded(var Text: string): Boolean;
    function Iterate(TraverseMethod: TTraverseMethod): Boolean;
    property Count: Integer read GetCount;
    property Installations[Index: Integer]: TJclBorRADToolInstallation read GetInstallations; default;
    property BCBInstallationFromVersion[VersionNumber: Integer]: TJclBorRADToolInstallation read GetBCBInstallationFromVersion;
    property DelphiInstallationFromVersion[VersionNumber: Integer]: TJclBorRADToolInstallation read GetDelphiInstallationFromVersion;
    property BDSInstallationFromVersion[VersionNumber: Integer]: TJclBorRADToolInstallation read GetBDSInstallationFromVersion;
    property BCBVersionInstalled[VersionNumber: Integer]: Boolean read GetBCBVersionInstalled;
    property DelphiVersionInstalled[VersionNumber: Integer]: Boolean read GetDelphiVersionInstalled;
    property BDSVersionInstalled[VersionNumber: Integer]: Boolean read GetBDSVersionInstalled;
  end;

{$IFDEF KEEP_DEPRECATED}
function BPLFileName(const BPLPath, PackageFileName: string): string;
{$ENDIF KEEP_DEPRECATE}
function BinaryFileName(const OutputPath, ProjectFileName: string): string;

function IsDelphiPackage(const FileName: string): Boolean;
function IsDelphiProject(const FileName: string): Boolean;
function IsBCBPackage(const FileName: string): Boolean;
function IsBCBProject(const FileName: string): Boolean;

procedure GetDPRFileInfo(const DPRFileName: string; out BinaryExtension: string;
  const LibSuffix: PString = nil);
procedure GetBPRFileInfo(const BPRFileName: string; out BinaryFileName: string;
  const Description: PString = nil);
procedure GetDPKFileInfo(const DPKFileName: string; out RunOnly: Boolean;
  const LibSuffix: PString = nil; const Description: PString = nil);
procedure GetBPKFileInfo(const BPKFileName: string; out RunOnly: Boolean;
  const BinaryFileName: PString = nil; const Description: PString = nil);

implementation

uses
  SysConst,
  {$IFDEF MSWINDOWS}
  Registry,
  JclRegistry,
  JclDebug,
  {$ENDIF MSWINDOWS}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  JclFileUtils, JclLogic, JclResources, JclStrings, JclSysInfo;

// Internal

type
  TUpdatePack = record
    Version: Byte;
    LatestUpdatePack: Integer;
  end;
  {$IFDEF KYLIX}
  TKylixVersion = 1..3;
  {$ENDIF KYLIX}

  {$IFDEF MSWINDOWS}
  TBDSVersionInfo = record
    Name: string;
    VersionStr: string;
    Version: Integer;
    CoreIdeVersion: string;
    ProjectsDirResId: Integer;
    Supported: Boolean;
  end;
  {$ENDIF MSWINDOWS}

const
  {$IFDEF MSWINDOWS}
  {$IFNDEF RTL140_UP}
  PathSep = ';';
  {$ENDIF ~RTL140_UP}

  MSHelpSystemKeyName = 'SOFTWARE\Microsoft\Windows\Help';

  BCBKeyName          = 'SOFTWARE\Borland\C++Builder';
  BDSKeyName          = 'SOFTWARE\Borland\BDS';
  DelphiKeyName       = 'SOFTWARE\Borland\Delphi';

  BDSVersions: array [1..4] of TBDSVersionInfo = (
    (
      Name: RsCSharpName;
      VersionStr: '1.0';
      Version: 1;
      CoreIdeVersion: '71';
      ProjectsDirResId: 64507;
      Supported: True),
    (
      Name: RsDelphiName;
      VersionStr: '8';
      Version: 8;
      CoreIdeVersion: '71';
      ProjectsDirResId: 64460;
      Supported: True),
    (
      Name: RsDelphiName;
      VersionStr: '2005';
      Version: 9;
      CoreIdeVersion: '90';
      ProjectsDirResId: 64431;
      Supported: True),
    (
      Name: RsBDSName;
      VersionStr: '2006';
      Version: 10;
      CoreIdeVersion: '100';
      ProjectsDirResId: 64719;
      Supported: True)
  );
  {$ENDIF MSWINDOWS}

  {$IFDEF KYLIX}
  RootDirValueName           = 'DelphiRoot';
  {$ELSE}
  RootDirValueName           = 'RootDir';
  {$ENDIF KYLIX}

  EditionValueName           = 'Edition';
  VersionValueName           = 'Version';

  DebuggingKeyName           = 'Debugging';
  DebugDCUPathValueName      = 'Debug DCUs Path';

  GlobalsKeyName             = 'Globals';

  LibraryKeyName             = 'Library';
  LibrarySearchPathValueName = 'Search Path';
  LibraryBrowsingPathValueName = 'Browsing Path';
  LibraryBPLOutputValueName  = 'Package DPL Output';
  LibraryDCPOutputValueName  = 'Package DCP Output';

  CppPathsKeyName            = 'CppPaths';
  CppBrowsingPathValueName   = 'BrowsingPath';
  CppSearchPathValueName     = 'SearchPath';

  TransferKeyName            = 'Transfer';
  TransferCountValueName     = 'Count';
  TransferPathValueName      = 'Path%d';
  TransferParamsValueName    = 'Params%d';
  TransferTitleValueName     = 'Title%d';
  TransferWorkDirValueName   = 'WorkingDir%d';

  DisabledPackagesKeyName    = 'Disabled Packages';
  EnvVariablesKeyName        = 'Environment Variables';
  EnvVariableBDSValueName    = 'BDS';
  EnvVariableBDSPROJDIRValueName = 'BDSPROJECTSDIR';
  KnownPackagesKeyName       = 'Known Packages';
  KnownIDEPackagesKeyName    = 'Known IDE Packages';
  ExpertsKeyName             = 'Experts';
  PackageCacheKeyName        = 'Package Cache';

  PaletteKeyName             = 'Palette';
  PaletteHiddenTag           = '.Hidden';

  {$IFDEF MSWINDOWS}
  AsmExeName                 = 'tasm32.exe';
  BCC32ExeName               = 'bcc32.exe';
  DCC32ExeName               = 'dcc32.exe';
  DCCILExeName               = 'dccil.exe';
  Bpr2MakExeName             = 'bpr2mak.exe';
  MakeExeName                = 'make.exe';
  DelphiOptionsFileExtension = '.dof';
  ConfigurationExtension     = '.cfg';
  {$IFDEF BCB}
  BorRADToolRepositoryFileName = 'bcb.dro';
  {$ELSE BCB}
  BorRADToolRepositoryFileName = 'delphi32.dro';
  {$ENDIF BCB}
  HelpContentFileName        = '%s\Help\%s%d.ohc';
  HelpIndexFileName          = '%s\Help\%s%d.ohi';
  HelpLinkFileName           = '%s\Help\%s%d.ohl';
  HelpProjectFileName        = '%s\Help\%s%d.ohp';
  HelpGidFileName            = '%s\Help\%s%d.gid';      
  {$ENDIF MSWINDOWS}

  {$IFDEF KYLIX}
  IDs: array [TKylixVersion] of Integer = (60, 65, 69);
  LibSuffixes: array [TKylixVersion] of string[3] = ('6.0', '6.5', '6.9');

  BCC32ExeName               = 'bcc';
  DCC32ExeName               = 'dcc';
  Bpr2MakExeName             = 'bpr2mak';
  MakeExeName                = 'make';

  DelphiIdeExeName           = 'delphi';
  BCBIdeExeName              = 'bcblin';
  DelphiOptionsFileExtension = '.kof';

  KylixHelpNamePart          = 'k%d';
  {$ENDIF KYLIX}

  DelphiLibSuffixOption   = '{$LIBSUFFIX ''';
  DelphiDescriptionOption = '{$DESCRIPTION ''';
  DelphiRunOnlyOption     = '{$RUNONLY}';
  DelphiBinaryExtOption   = '{$E ';
  BCBLFlagsOption     = '<LFLAGS ';
  BCBDSwitchOption    = '-D';
  BCBLibSuffixOption  = 'LibSuffix=';
  BCBGprSwitchOption  = '-Gpr';
  BCBProjectOption    = '<PROJECT ';

procedure GetDPRFileInfo(const DPRFileName: string; out BinaryExtension: string;
  const LibSuffix: PString = nil);
var
  Index: Integer;
  S: string;
  DPRFile: TStrings;
const
  ProgramText = 'program';
  LibraryText = 'library';
begin
  DPRFile := TStringList.Create;
  try
    DPRFile.LoadFromFile(DPRFileName);

    if Assigned(LibSuffix) then
      LibSuffix^ := '';

    BinaryExtension := '';

    for Index := 0 to DPRFile.Count - 1 do
    begin
      S := TrimRight(DPRFile.Strings[Index]);
      if (AnsiStrLIComp(PChar(S), ProgramText, Length(ProgramText)) = 0) and (BinaryExtension = '') then
        BinaryExtension := BinaryExtensionExecutable;
      if (AnsiStrLIComp(PChar(S), LibraryText, Length(LibraryText)) = 0) and (BinaryExtension = '') then
        BinaryExtension := BinaryExtensionLibrary;
      if AnsiStrLIComp(PChar(S), DelphiBinaryExtOption, Length(DelphiBinaryExtOption)) = 0 then
        BinaryExtension := StrTrimQuotes(Copy(S, Length(DelphiBinaryExtOption), Length(S) - Length(DelphiBinaryExtOption)));
      if Assigned(LibSuffix)
        and (AnsiStrLIComp(PChar(S), DelphiLibSuffixOption, Length(DelphiLibSuffixOption)) = 0) then
        LibSuffix^ := StrTrimQuotes(Copy(S, Length(DelphiLibSuffixOption), Length(S) - Length(DelphiLibSuffixOption)));
    end;
  finally
    DPRFile.Free;
  end;
end;

procedure GetBPRFileInfo(const BPRFileName: string; out BinaryFileName: string;
  const Description: PString = nil);
var
  I, J: Integer;
  S, SubS1, SubS2, SubS3: string;
  BPKFile: TStringList;
  LProjectPos, BinaryFileNamePos, EndFileNamePos, LFlagsPos, DSwitchPos,
  SemiColonPos, AmpPos: Integer;
begin
  BPKFile := TStringList.Create;
  try
    BPKFile.LoadFromFile(BPRFileName);
    BinaryFileName := '';
    if Assigned(Description) then
      Description^ := '';
    for I := 0 to BPKFile.Count - 1 do
    begin
      S := BPKFile[I];

      LProjectPos := Pos(BCBProjectOption,S);
      if (LProjectPos > 0) then
      begin
        SubS1 := Copy(S,LProjectPos,Length(S));
        J := 1;
        while (Pos('>',SubS1) = 0) and ((I+J)<BPKFile.Count) do
        begin
          SubS1 := SubS1 + BPKFile[I+J];
          Inc(J);
        end;

        BinaryFileNamePos := Pos('"', SubS1);
        if BinaryFileNamePos > 0 then
        begin
          SubS2 := Copy(SubS1, BinaryFileNamePos + 1, Length(SubS1) - BinaryFileNamePos);
          EndFileNamePos := Pos('"', SubS2);

          if EndFileNamePos > 0 then
            BinaryFileName := Copy(SubS2, 1, EndFileNamePos - 1);
        end;
      end;

      LFlagsPos := Pos(BCBLFlagsOption,S);
      if (LFlagsPos > 0) then
      begin
        SubS1 := Copy(S,LFlagsPos,Length(S));
        J := 1;
        while (Pos('>',SubS1) = 0) and ((I+J)<BPKFile.Count) do
        begin
          SubS1 := SubS1 + BPKFile[I+J];
          Inc(J);
        end;
        DSwitchPos := Pos(BCBDSwitchOption,SubS1);
        if DSwitchPos > 0 then
        begin
          SubS2 := Copy(SubS1,DSwitchPos,Length(SubS1));
          SemiColonPos := Pos(';',SubS2);
          if SemiColonPos > 0 then
          begin
            SubS3 := Copy(SubS2,SemiColonPos+1,Length(SubS2));
            AmpPos := Pos('&',SubS3);
            if (Description <> nil) and (AmpPos > 0) then
              Description^ := Copy(SubS3,1,AmpPos-1);
          end;
        end;
      end;
    end;
  finally
    BPKFile.Free;
  end;
end;

procedure GetDPKFileInfo(const DPKFileName: string; out RunOnly: Boolean;
  const LibSuffix: PString = nil; const Description: PString = nil);
var
  I: Integer;
  S: string;
  DPKFile: TStringList;
begin
  DPKFile := TStringList.Create;
  try
    DPKFile.LoadFromFile(DPKFileName);
    if Assigned(Description) then
      Description^ := '';
    if Assigned(LibSuffix) then
      LibSuffix^ := '';
    RunOnly := False;
    for I := 0 to DPKFile.Count - 1 do
    begin
      S := TrimRight(DPKFile.Strings[I]);
      if Assigned(Description) and (Pos(DelphiDescriptionOption, S) = 1) then
        Description^ := Copy(S, Length(DelphiDescriptionOption), Length(S) - Length(DelphiDescriptionOption))
      else
      if Assigned(LibSuffix) and (Pos(DelphiLibSuffixOption, S) = 1) then
        LibSuffix^ := StrTrimQuotes(Copy(S, Length(DelphiLibSuffixOption), Length(S) - Length(DelphiLibSuffixOption)))
      else
      if Pos(DelphiRunOnlyOption, S) = 1 then
        RunOnly := True;
    end;
  finally
    DPKFile.Free;
  end;
end;

procedure GetBPKFileInfo(const BPKFileName: string; out RunOnly: Boolean;
  const BinaryFileName: PString = nil; const Description: PString = nil);
var
  I, J: Integer;
  S, SubS1, SubS2, SubS3: string;
  BPKFile: TStringList;
  LFlagsPos, DSwitchPos, SemiColonPos, AmpPos, GprPos: Integer;
  LProjectPos, BinaryFileNamePos, EndFileNamePos: Integer;
begin
  BPKFile := TStringList.Create;
  try
    BPKFile.LoadFromFile(BPKFileName);
    if Assigned(Description) then
      Description^ := '';
    if Assigned(BinaryFileName) then
      BinaryFileName^ := '';
    RunOnly := False;
    for I := 0 to BPKFile.Count - 1 do
    begin
      S := BPKFile[I];

      LProjectPos := Pos(BCBProjectOption,S);
      if Assigned(BinaryFileName) and (LProjectPos > 0) then
      begin
        SubS1 := Copy(S,LProjectPos,Length(S));
        J := 1;
        while (Pos('>',SubS1) = 0) and ((I+J)<BPKFile.Count) do
        begin
          SubS1 := SubS1 + BPKFile[I+J];
          Inc(J);
        end;

        BinaryFileNamePos := Pos('"', SubS1);
        if BinaryFileNamePos > 0 then
        begin
          SubS2 := Copy(SubS1, BinaryFileNamePos + 1, Length(SubS1) - BinaryFileNamePos);
          EndFileNamePos := Pos('"', SubS2);

          if EndFileNamePos > 0 then
            BinaryFileName^ := Copy(SubS2, 1, EndFileNamePos - 1);
        end;
      end;

      LFlagsPos := Pos(BCBLFlagsOption,S);
      if (LFlagsPos > 0) then
      begin
        SubS1 := Copy(S,LFlagsPos,Length(S));
        J := 1;
        while (Pos('>',SubS1) = 0) and ((I+J)<BPKFile.Count) do
        begin
          SubS1 := SubS1 + BPKFile[I+J];
          Inc(J);
        end;
        DSwitchPos := Pos(BCBDSwitchOption,SubS1);
        GprPos := Pos(BCBGprSwitchOption,SubS1);
        if DSwitchPos > 0 then
        begin
          SubS2 := Copy(SubS1,DSwitchPos,Length(SubS1));
          SemiColonPos := Pos(';',SubS2);
          if SemiColonPos > 0 then
          begin
            SubS3 := Copy(SubS2,SemiColonPos+1,Length(SubS2));
            AmpPos := Pos('&',SubS3);
            if (Description <> nil) and (AmpPos > 0) then
              Description^ := Copy(SubS3,1,AmpPos-1);
          end;
        end;
        if GprPos > 0 then
          RunOnly := True;
      end;
    end;
  finally
    BPKFile.Free;
  end;
end;

function BPLFileName(const BPLPath, PackageFileName: string): string;
var
  PackageExtension, LibSuffix: string;
  RunOnly: Boolean;
begin
  PackageExtension := ExtractFileExt(PackageFileName);
  if SameText(PackageExtension, SourceExtensionDelphiPackage) then
  begin
    GetDPKFileInfo(PackageFileName, RunOnly, @LibSuffix);
    Result := PathExtractFileNameNoExt(PackageFileName) + LibSuffix + BinaryExtensionPackage;
  end else if SameText(PackageExtension, SourceExtensionBCBPackage) then
    GetBPKFileInfo(PackageFileName, RunOnly, @Result)
  else
    raise EJclBorRadException.CreateResFmt(@RsUnknownPackageExtension, [PackageExtension]);

  Result := PathAddSeparator(BPLPath) + Result;
end;

function BinaryFileName(const OutputPath, ProjectFileName: string): string;
var
  ProjectExtension, LibSuffix, BinaryExtension: string;
  RunOnly: Boolean;
begin
  ProjectExtension := ExtractFileExt(ProjectFileName);
  if SameText(ProjectExtension, SourceExtensionDelphiPackage) then
  begin
    GetDPKFileInfo(ProjectFileName, RunOnly, @LibSuffix);
    Result := PathExtractFileNameNoExt(ProjectFileName) + LibSuffix + BinaryExtensionPackage;
  end else if SameText(ProjectExtension, SourceExtensionDelphiProject) then
  begin
    GetDPRFileInfo(ProjectFileName, BinaryExtension, @LibSuffix);
    Result := PathExtractFileNameNoExt(ProjectFileName) + LibSuffix + BinaryExtension;
  end else if SameText(ProjectExtension, SourceExtensionBCBPackage) then
    GetBPKFileInfo(ProjectFileName, RunOnly, @Result)
  else if SameText(ProjectExtension, SourceExtensionBCBProject) then
    GetBPRFileInfo(ProjectFileName, Result)
  else
    raise EJclBorRadException.CreateResFmt(@RsUnknownProjectExtension, [ProjectExtension]);

  Result := PathAddSeparator(OutputPath) + Result;
end;

function IsDelphiPackage(const FileName: string): Boolean;
begin
  Result := SameText(ExtractFileExt(FileName), SourceExtensionDelphiPackage);
  { TODO : Add some plausibility tests }
  { like
  var
    F: TextFile;
    FirstLine: string;

  if FileExists(FileName) then
  begin
    AssignFile(F, FileName);
    Reset(F);
    ReadLn(F, FirstLine);
    Result := Pos('package ', FirstLine) = 1;
    CloseFile(F);
  end;
  }
end;

function IsDelphiProject(const FileName: string): Boolean;
begin
  Result := SameText(ExtractFileExt(FileName), SourceExtensionDelphiProject);
end;

function IsBCBPackage(const FileName: string): Boolean;
begin
  Result := SameText(ExtractFileExt(FileName), SourceExtensionBCBPackage);
end;

function IsBCBProject(const FileName: string): Boolean;
begin
  Result := SameText(ExtractFileExt(FileName), SourceExtensionBCBProject);
end;

{$IFDEF MSWINDOWS}
function RegGetValueNamesAndValues(const RootKey: HKEY; const Key: string; const List: TStrings): Boolean;
var
  I: Integer;
  TempList: TStringList;
  Name: string;
  DataType: DWORD;
begin
  TempList := TStringList.Create;
  try
    Result := RegKeyExists(RootKey, Key) and RegGetValueNames(RootKey, Key, TempList);
    if Result then
    begin
      for I := 0 to TempList.Count - 1 do
      begin
        Name := TempList[I];
        if RegGetDataType(RootKey, Key, Name, DataType) and
          ((DataType = REG_SZ) or (DataType = REG_EXPAND_SZ) or (DataType = REG_BINARY)) then
          TempList[I] := Name + '=' + RegReadStringDef(RootKey, Key, Name, '');
      end;
      List.AddStrings(TempList);
    end;
  finally
    TempList.Free;
  end;
end;
{$ENDIF MSWINDOWS}

//=== { TJclBorRADToolInstallationObject } ===================================

constructor TJclBorRADToolInstallationObject.Create(AInstallation: TJclBorRADToolInstallation);
begin
  FInstallation := AInstallation;
end;

{$IFDEF MSWINDOWS}

//=== { TJclBorlandOpenHelp } ================================================

function TJclBorlandOpenHelp.AddHelpFile(const HelpFileName, IndexName: string): Boolean;
var
  CntFileName, HelpName, CntName: string;
  List: TStringList;

  procedure AddToList(const FileName, Text: string);
  var
    I, Attr: Integer;
    Found: Boolean;
  begin
    List.LoadFromFile(FileName);
    Found := False;
    for I := 0 to List.Count - 1 do
      if AnsiSameText(Trim(List[I]), Text) then
      begin
        Found := True;
        Break;
      end;
    if not Found then
    begin
      List.Add(Text);
      Attr := FileGetAttr(FileName);
      FileSetAttr(FileName, faArchive);
      List.SaveToFile(FileName);
      FileSetAttr(FileName, Attr);
    end;
  end;

begin
  CntFileName := ChangeFileExt(HelpFileName, '.cnt');
  Result := FileExists(HelpFileName) and FileExists(CntFileName);
  if Result then
  begin
    HelpName := ExtractFileName(HelpFileName);
    CntName := ExtractFileName(CntFileName);
    RegWriteString(HKEY_LOCAL_MACHINE, MSHelpSystemKeyName, HelpName, ExtractFilePath(HelpFileName));
    RegWriteString(HKEY_LOCAL_MACHINE, MSHelpSystemKeyName, CntName, ExtractFilePath(CntFileName));
    List := TStringList.Create;
    try
      AddToList(ContentFileName, Format(':Include %s', [CntName]));
      AddToList(LinkFileName, Format(':Link %s', [HelpName]));
      AddToList(IndexFileName, Format(':Index %s=%s', [IndexName, HelpName]));
      SetFileLastWrite(ProjectFileName, Now);
      FileDelete(GidFileName);
    finally
      List.Free;
    end;
  end;
end;

function TJclBorlandOpenHelp.GetContentFileName: string;
begin
  Result := ReadFileName(HelpContentFileName);
end;

function TJclBorlandOpenHelp.GetGidFileName: string;
begin
  Result := ReadFileName(HelpGidFileName);
end;

function TJclBorlandOpenHelp.GetIndexFileName: string;
begin
  Result := ReadFileName(HelpIndexFileName);
end;

function TJclBorlandOpenHelp.GetLinkFileName: string;
begin
  Result := ReadFileName(HelpLinkFileName);
end;

function TJclBorlandOpenHelp.GetProjectFileName: string;
begin
  Result := ReadFileName(HelpProjectFileName);
end;

function TJclBorlandOpenHelp.ReadFileName(const FormatName: string): string;
var
  S: string;
begin
  with Installation do
  begin
    case RadToolKind of
      brDelphi :
        if VersionNumber <= 6 then
          S := 'delphi'
        else
          S := 'd';
      brCppBuilder :
        S := 'bcb';
      else
      //brBorlandDevStudio :
        raise EJclBorRadException.Create('open help not present in Borland Developer Studio');
    end;
    Result := Format(FormatName, [RootDir, S, VersionNumber]);
  end;
end;

function TJclBorlandOpenHelp.RemoveHelpFile(const HelpFileName, IndexName: string): Boolean;
var
  CntFileName, HelpName, CntName: string;
  List: TStringList;

  procedure RemoveFromList(const FileName, Text: string);
  var
    I, Attr: Integer;
    Found: Boolean;
  begin
    List.LoadFromFile(FileName);
    Found := False;
    for I := 0 to List.Count - 1 do
      if AnsiSameText(Trim(List[I]), Text) then
      begin
        Found := True;
        List.Delete(I);
        Break;
      end;
    if Found then
    begin
      Attr := FileGetAttr(FileName);
      FileSetAttr(FileName, faArchive);
      List.SaveToFile(FileName);
      FileSetAttr(FileName, Attr);
    end;
  end;

begin
  CntFileName := ChangeFileExt(HelpFileName, '.cnt');
  Result := FileExists(HelpFileName) and FileExists(CntFileName);
  if Result then
  begin
    HelpName := ExtractFileName(HelpFileName);
    CntName := ExtractFileName(CntFileName);
    //RegDeleteEntry(HKEY_LOCAL_MACHINE, MSHelpSystemKeyName, HelpName);
    //RegDeleteEntry(HKEY_LOCAL_MACHINE, MSHelpSystemKeyName, CntName);
    List := TStringList.Create;
    try
      RemoveFromList(ContentFileName, Format(':Include %s', [CntName]));
      RemoveFromList(LinkFileName, Format(':Link %s', [HelpName]));
      RemoveFromList(IndexFileName, Format(':Index %s=%s', [IndexName, HelpName]));
      SetFileLastWrite(ProjectFileName, Now);
      FileDelete(GidFileName);
    finally
      List.Free;
    end;
  end;
end;

//== { TJclHelp2Manager } ====================================================

const
  Help2BorlandNameSpace  = 'Borland.BDS%d';
  Help2DefaultKeyWord    = '_DEFAULT';

function TJclHelp2Manager.CommitTransaction: Boolean;
begin
  Result := RequireObject([hoRegisterSession]);
  if Result then
  begin
    try
      FHxRegisterSession.CommitTransaction;
    except
      Result := False;
    end;
  end;
end;

constructor TJclHelp2Manager.Create(AInstallation: TJclBorRADToolInstallation);
begin
  inherited Create(AInstallation);
  FHxRegisterSession := nil;
  FHxRegister := nil;
  FHxPlugin := nil;
end;

constructor TJclHelp2Manager.Create;
begin
  Create(nil);
end;

function TJclHelp2Manager.CreateTransaction: Boolean;
begin
  Result := RequireObject([hoRegisterSession]);
  if Result then
  begin
    try
      FHxRegisterSession.CreateTransaction('');
    except
      Result := False;
    end;
  end;
end;

destructor TJclHelp2Manager.Destroy;
begin
  FHxRegisterSession := nil;
  FHxRegister := nil;
  FHxPlugin := nil;
  inherited Destroy;
end;

function TJclHelp2Manager.GetHxPlugin: IHxPlugin;
begin
  RequireObject([hoPlugin]);
  Result := FHxPlugin;
end;

function TJclHelp2Manager.GetHxRegister: IHxRegister;
begin
  RequireObject([hoRegister]);
  Result := FHxRegister;
end;

function TJclHelp2Manager.GetHxRegisterSession: IHxRegisterSession;
begin
  RequireObject([hoRegisterSession]);
  Result := FHxRegisterSession;
end;

function TJclHelp2Manager.PlugNameSpaceIn(const SourceNameSpace,
  TargetNameSpace: WideString): Boolean;
var
  Help2Default: WideString;
begin
  Result := RequireObject([hoPlugin]);
  if Result then
  begin
    try
      Help2Default := Help2DefaultKeyWord;
      FHxPlugin.RegisterHelpPlugIn(TargetNameSpace, Help2Default,
        SourceNameSpace, Help2Default, '', 0);
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.PlugNameSpaceInBorlandHelp(
  const NameSpace: WideString): Boolean;
begin
  Result := Assigned(FInstallation) and (Installation.RadToolKind = brBorlandDevStudio)
    and PlugNameSpaceIn(NameSpace, Format(Help2BorlandNameSpace, [Installation.VersionNumber]));
end;

function TJclHelp2Manager.RegisterHelpFile(const NameSpace,
  Identifier: WideString; const LangId: Integer; const HxSFile,
  HxIFile: WideString): Boolean;
begin
  Result := RequireObject([hoRegister]);
  if Result then
  begin
    try
      FHxRegister.RegisterHelpFileSet(NameSpace, Identifier, LangId, HxSFile,
        HxIFile, '', '', 0, 0, 0, 0);
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.RegisterNameSpace(const Name, Collection,
  Description: WideString): Boolean;
begin
  Result := RequireObject([hoRegister]);
  if Result then
  begin
    try
      FHxRegister.RegisterNamespace(Name, Collection, Description);
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.RequireObject(HelpObjects: TJclHelp2Objects): Boolean;
begin
  // dependencies
  if (hoRegister in HelpObjects) or (hoPlugin in HelpObjects)then
    Include(HelpObjects, hoRegisterSession);

  Result := True;

  if (hoRegisterSession in HelpObjects) and not Assigned(FHxRegisterSession) then
  begin
    try
      FHxRegisterSession := CoHxRegisterSession.Create;
    except
      Result := False;
    end;
  end;

  if Result and (hoRegister in HelpObjects) and not Assigned(FHxRegister) then
  begin
    try
      Result := Supports(FHxRegisterSession.GetRegistrationObject(HxRegisterSession_IHxRegister), IHxRegister, FHxRegister);
    except
      Result := False;
    end;
  end;

  if Result and (hoPlugin in HelpObjects) and not Assigned(FHxPlugin) then
  begin
    try
      Result := Supports(FHxRegisterSession.GetRegistrationObject(HxRegisterSession_IHxPlugIn), IHxPlugin, FHxPlugin);
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.UnPlugNameSpace(const SourceNameSpace,
  TargetNameSpace: WideString): Boolean;
var
  Help2Default: WideString;
begin
  Result := RequireObject([hoPlugin]);
  if Result then
  begin
    try
      Help2Default := Help2DefaultKeyWord;
      FHxPlugin.RemoveHelpPlugIn(TargetNameSpace, Help2Default,
        SourceNameSpace, Help2Default, '');
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.UnPlugNameSpaceFromBorlandHelp(
  const NameSpace: WideString): Boolean;
begin
  Result := Assigned(FInstallation) and (Installation.RadToolKind = brBorlandDevStudio)
    and UnPlugNameSpace(NameSpace, Format(Help2BorlandNameSpace, [Installation.VersionNumber]));
end;

function TJclHelp2Manager.UnregisterHelpFile(const NameSpace,
  Identifier: WideString; const LangId: Integer): Boolean;
begin
  Result := RequireObject([hoRegister]);
  if Result then
  begin
    try
      FHxRegister.RemoveHelpFile(NameSpace, Identifier, LangId);
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.UnregisterNameSpace(const Name: WideString): Boolean;
begin
  Result := RequireObject([hoRegister]);
  if Result then
  begin
    try
      FHxRegister.RemoveNamespace(Name);
    except
      Result := False;
    end;
  end;
end;

{$ENDIF MSWINDOWS}

//== { TJclBorRADToolIdeTool } ===============================================

constructor TJclBorRADToolIdeTool.Create(AInstallation: TJclBorRADToolInstallation);
begin
  inherited Create(AInstallation);
  FKey := TransferKeyName;
end;

procedure TJclBorRADToolIdeTool.CheckIndex(Index: Integer);
begin
  if (Index < 0) or (Index >= Count) then
    raise EJclError.CreateRes(@RsIndexOufOfRange);
end;

function TJclBorRADToolIdeTool.GetCount: Integer;
begin
  Result := Installation.ConfigData.ReadInteger(Key, TransferCountValueName, 0);
end;

function TJclBorRADToolIdeTool.GetParameters(Index: Integer): string;
begin
  CheckIndex(Index);
  Result := Installation.ConfigData.ReadString(Key, Format(TransferParamsValueName, [Index]), '');
end;

function TJclBorRADToolIdeTool.GetPath(Index: Integer): string;
begin
  CheckIndex(Index);
  Result := Installation.ConfigData.ReadString(Key, Format(TransferPathValueName, [Index]), '');
end;

function TJclBorRADToolIdeTool.GetTitle(Index: Integer): string;
begin
  CheckIndex(Index);
  Result := Installation.ConfigData.ReadString(Key, Format(TransferTitleValueName, [Index]), '');
end;

function TJclBorRADToolIdeTool.GetWorkingDir(Index: Integer): string;
begin
  CheckIndex(Index);
  Result := Installation.ConfigData.ReadString(Key, Format(TransferWorkDirValueName, [Index]), '');
end;

function TJclBorRADToolIdeTool.IndexOfPath(const Value: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if SamePath(Path[I], Value) then
    begin
      Result := I;
      Break;
    end;
end;

function TJclBorRADToolIdeTool.IndexOfTitle(const Value: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Title[I] = Value then
    begin
      Result := I;
      Break;
    end;
end;

procedure TJclBorRADToolIdeTool.RemoveIndex(const Index: Integer);
var
  I: Integer;
begin
  for I := Index to Count - 2 do
  begin
    Parameters[I] := Parameters[I+1];
    Path[I] := Path[I+1];
    Title[I] := Title[I+1];
    WorkingDir[Index] := WorkingDir[I+1];
  end;
  Count := Count - 1;
end;

procedure TJclBorRADToolIdeTool.SetCount(const Value: Integer);
begin
  if Value > Count then
    Installation.ConfigData.WriteInteger(Key, TransferCountValueName, Value);
end;

procedure TJclBorRADToolIdeTool.SetParameters(Index: Integer; const Value: string);
begin
  CheckIndex(Index);
  Installation.ConfigData.WriteString(Key, Format(TransferParamsValueName, [Index]), Value);
end;

procedure TJclBorRADToolIdeTool.SetPath(Index: Integer; const Value: string);
begin
  CheckIndex(Index);
  Installation.ConfigData.WriteString(Key, Format(TransferPathValueName, [Index]), Value);
end;

procedure TJclBorRADToolIdeTool.SetTitle(Index: Integer; const Value: string);
begin
  CheckIndex(Index);
  Installation.ConfigData.WriteString(Key, Format(TransferTitleValueName, [Index]), Value);
end;

procedure TJclBorRADToolIdeTool.SetWorkingDir(Index: Integer; const Value: string);
begin
  CheckIndex(Index);
  Installation.ConfigData.WriteString(Key, Format(TransferWorkDirValueName, [Index]), Value);
end;

//=== { TJclBorRADToolIdePackages } ==========================================

constructor TJclBorRADToolIdePackages.Create(AInstallation: TJclBorRADToolInstallation);
begin
  inherited Create(AInstallation);
  FDisabledPackages := TStringList.Create;
  FDisabledPackages.Sorted := True;
  FDisabledPackages.Duplicates := dupIgnore;
  FKnownPackages := TStringList.Create;
  FKnownPackages.Sorted := True;
  FKnownPackages.Duplicates := dupIgnore;
  FKnownIDEPackages := TStringList.Create;
  FKnownIDEPackages.Sorted := True;
  FKnownIDEPackages.Duplicates := dupIgnore;
  FExperts := TStringList.Create;
  FExperts.Sorted := True;
  FExperts.Duplicates := dupIgnore;
  ReadPackages;
end;

destructor TJclBorRADToolIdePackages.Destroy;
begin
  FreeAndNil(FDisabledPackages);
  FreeAndNil(FKnownPackages);
  FreeAndNil(FKnownIDEPackages);
  FreeAndNil(FExperts);
  inherited Destroy;                 
end;

function TJclBorRADToolIdePackages.AddPackage(const FileName, Description: string): Boolean;
begin
  Result := True;
  RemoveDisabled(FileName);
  Installation.ConfigData.WriteString(KnownPackagesKeyName, FileName, Description);
  ReadPackages;
end;

function TJclBorRADToolIdePackages.AddExpert(const FileName,
  Description: string): Boolean;
begin
  Result := True;
  RemoveDisabled(FileName);
  Installation.ConfigData.WriteString(ExpertsKeyName, Description, FileName);
  ReadPackages;
end;

function TJclBorRADToolIdePackages.AddIDEPackage(const FileName, Description: string): Boolean;
begin
  Result := True;
  RemoveDisabled(FileName);
  Installation.ConfigData.WriteString(KnownIDEPackagesKeyName, FileName, Description);
  ReadPackages;
end;

function TJclBorRADToolIdePackages.GetCount: Integer;
begin
  Result := FKnownPackages.Count;
end;

function TJclBorRADToolIdePackages.GetExpertCount: Integer;
begin
  Result := FExperts.Count;
end;

function TJclBorRADToolIdePackages.GetExpertDescriptions(
  Index: Integer): string;
begin
  Result := FExperts.Names[Index];
end;

function TJclBorRADToolIdePackages.GetExpertFileNames(Index: Integer): string;
begin
   Result := PackageEntryToFileName(FExperts.Values[FExperts.Names[Index]]);
end;

function TJclBorRADToolIdePackages.GetIDECount: Integer;
begin
  Result := FKnownIDEPackages.Count;
end;

function TJclBorRADToolIdePackages.GetPackageDescriptions(Index: Integer): string;
begin
  Result := FKnownPackages.Values[FKnownPackages.Names[Index]];
end;

function TJclBorRADToolIdePackages.GetIDEPackageDescriptions(Index: Integer): string;
begin
  Result := FKnownPackages.Values[FKnownIDEPackages.Names[Index]];
end;

function TJclBorRADToolIdePackages.GetPackageDisabled(Index: Integer): Boolean;
begin
  Result := Boolean(FKnownPackages.Objects[Index]);
end;

function TJclBorRADToolIdePackages.GetPackageFileNames(Index: Integer): string;
begin
  Result := PackageEntryToFileName(FKnownPackages.Names[Index]);
end;

function TJclBorRADToolIdePackages.GetIDEPackageFileNames(Index: Integer): string;
begin
  Result := PackageEntryToFileName(FKnownIDEPackages.Names[Index]);
end;

function TJclBorRADToolIdePackages.PackageEntryToFileName(const Entry: string): string;
begin
  Result := Installation.SubstitutePath(Entry);
end;

procedure TJclBorRADToolIdePackages.ReadPackages;

  procedure ReadPackageList(const Name: string; List: TStringList);
  var
    ListIsSorted: Boolean;
  begin
    ListIsSorted := List.Sorted;
    List.Sorted := False;
    List.Clear;
    Installation.ConfigData.ReadSectionValues(Name, List);
    List.Sorted := ListIsSorted;
  end;

var
  I: Integer;
begin
  if (Installation.RadToolKind = brBorlandDevStudio) then
    ReadPackageList(KnownIDEPackagesKeyName, FKnownIDEPackages);
  ReadPackageList(KnownPackagesKeyName, FKnownPackages);
  ReadPackageList(DisabledPackagesKeyName, FDisabledPackages);
  ReadPackageList(ExpertsKeyName, FExperts);
  for I := 0 to Count - 1 do
    if FDisabledPackages.IndexOfName(FKnownPackages.Names[I]) <> -1 then
      FKnownPackages.Objects[I] := Pointer(True);
end;

procedure TJclBorRADToolIdePackages.RemoveDisabled(const FileName: string);
var
  I: Integer;
begin
  for I := 0 to FDisabledPackages.Count - 1 do
    if SamePath(FileName, PackageEntryToFileName(FDisabledPackages.Names[I])) then
    begin
      Installation.ConfigData.DeleteKey(DisabledPackagesKeyName, FDisabledPackages.Names[I]);
      ReadPackages;
      Break;
    end;
end;

function TJclBorRADToolIdePackages.RemoveExpert(
  const FileName: string): Boolean;
var
  I: Integer;
  KnownExpertDescription, KnownExpert, KnownExpertFileName: string;
begin
  Result := False;
  for I := 0 to FExperts.Count - 1 do
  begin
    KnownExpertDescription := FExperts.Names[I];
    KnownExpert := FExperts.Values[KnownExpertDescription];
    KnownExpertFileName := PackageEntryToFileName(KnownExpert);
    if SamePath(FileName, KnownExpertFileName) then
    begin
      RemoveDisabled(KnownExpertFileName);
      Installation.ConfigData.DeleteKey(ExpertsKeyName, KnownExpertDescription);
      ReadPackages;
      Result := True;
      Break;
    end;
  end;
end;

function TJclBorRADToolIdePackages.RemovePackage(const FileName: string): Boolean;
var
  I: Integer;
  KnownPackage, KnownPackageFileName: string;
begin
  Result := False;
  for I := 0 to FKnownPackages.Count - 1 do
  begin
    KnownPackage := FKnownPackages.Names[I];
    KnownPackageFileName := PackageEntryToFileName(KnownPackage);
    if SamePath(FileName, KnownPackageFileName) then
    begin
      RemoveDisabled(KnownPackageFileName);
      Installation.ConfigData.DeleteKey(KnownPackagesKeyName, KnownPackage);
      ReadPackages;
      Result := True;
      Break;
    end;
  end;
end;

function TJclBorRADToolIdePackages.RemoveIDEPackage(const FileName: string): Boolean;
var
  I: Integer;
  KnownIDEPackage, KnownIDEPackageFileName: string;
begin
  Result := False;
  for I := 0 to FKnownIDEPackages.Count - 1 do
  begin
    KnownIDEPackage := FKnownIDEPackages.Names[I];
    KnownIDEPackageFileName := PackageEntryToFileName(KnownIDEPackage);
    if SamePath(FileName, KnownIDEPackageFileName) then
    begin
      RemoveDisabled(KnownIDEPackageFileName);
      Installation.ConfigData.DeleteKey(KnownIDEPackagesKeyName, KnownIDEPackage);
      ReadPackages;
      Result := True;
      Break;
    end;
  end;
end;

//=== { TJclBorlandCommandLineTool } =========================================

constructor TJclBorlandCommandLineTool.Create(AInstallation: TJclBorRADToolInstallation);
begin
  inherited Create(AInstallation);
  FOptions := TStringList.Create;
end;

destructor TJclBorlandCommandLineTool.Destroy;
begin
  FreeAndNil(FOptions);
  inherited Destroy;
end;

procedure TJclBorlandCommandLineTool.AddPathOption(const Option, Path: string);
var
  S: string;

  {$IFDEF MSWINDOWS}
  // to avoid the 126 character limit of DCC32 (and eventually other command line tools)
  // which shows up with misleading error messages ("Fatal: System.pas not found") or
  // might even cause AVs
  procedure ConvertToShortPathNames(var Paths: string);
  var
    List: TStringList;
    I: Integer;
  begin
    List := TStringList.Create;
    try
      StrToStrings(Paths, PathSep, List);
      for I := 0 to List.Count - 1 do
        List[I] := PathGetShortName(List[I]);
      Paths := StringsToStr(List, PathSep);
    finally
      List.Free;
    end;
  end;
  {$ENDIF MSWINDOWS}

begin
  S := PathRemoveSeparator(Path);
  {$IFDEF MSWINDOWS}
  S := LowerCase(S); // file names are case insensitive
  ConvertToShortPathNames(S);
  {$ENDIF MSWINDOWS}
  { TODO : If we were sure that options are always case-insensitive
           for Borland tools, we could use UpperCase(Option) below. }
  S := Format('-%s"%s"', [Option, S]);
  // avoid duplicate entries
  if Options.IndexOf(S) = -1 then
    Options.Add(S);
end;

procedure TJclBorlandCommandLineTool.CheckOutputValid;
begin
  if Assigned(FOutputCallback) then
    raise EJclCommandLineToolError.CreateResFmt(@RsCmdLineToolOutputInvalid, [GetExeName]);
end;

function TJclBorlandCommandLineTool.Execute(const CommandLine: string): Boolean;
var
  LaunchCommand: string;
begin
  LaunchCommand := Format('%s %s', [FileName, CommandLine]);
  if Assigned(FOutputCallback) then
  begin
    FOutputCallback(LaunchCommand);
    Result := JclSysUtils.Execute(LaunchCommand, FOutputCallback) = 0
  end
  else
    Result := JclSysUtils.Execute(LaunchCommand, FOutput) = 0;
end;

function TJclBorlandCommandLineTool.GetExeName: string;
begin
  Result := '';
  {$IFDEF MSWINDOWS}
  raise EAbstractError.CreateResFmt(@SAbstractError, ['']); // BCB doesn't support abstract keyword
  {$ENDIF MSWINDOWS}
end;

function TJclBorlandCommandLineTool.GetFileName: string;
begin
  Result := Installation.BinFolderName + GetExeName;
  if Pos(' ', Result) > 0 then
    Result := AnsiQuotedStr(Result, '"');
end;

function TJclBorlandCommandLineTool.GetOptions: TStrings;
begin
  Result := FOptions;
end;

function TJclBorlandCommandLineTool.GetOutput: string;
begin
  CheckOutputValid;
  Result := FOutput;
end;

function TJclBorlandCommandLineTool.GetOutputCallback: TTextHandler;
begin
  Result := FOutputCallback;
end;

procedure TJclBorlandCommandLineTool.SetOutputCallback(const CallbackMethod: TTextHandler);
begin
  FOutputCallback := CallbackMethod;
end;

//=== { TJclBCC32 } ============================================================

constructor TJclBCC32.Create(AInstallation: TJclBorRADToolInstallation);
begin
  inherited Create(AInstallation);
end;

function TJclBCC32.GetExeName: string;
begin
  Result := BCC32ExeName;
end;

{$IFDEF KEEP_DEPRECATED}
function TJclBCC32.SupportsLibSuffix: Boolean;
begin
  Result := Installation.SupportsLibSuffix;
end;
{$ENDIF KEEP_DEPRECATED}

//=== { TJclDCC32 } ============================================================

procedure TJclDCC32.AddProjectOptions(const ProjectFileName, DCPPath: string);
var
  SearchPath, DynamicPackages, SearchDcpPath, ConfigurationFileName,
  OptionsFileName: string;
  OptionsFile: TIniFile;
begin
  ConfigurationFileName := ChangeFileExt(ProjectFileName, ConfigurationExtension);
  if FileExists(ConfigurationFileName) then
    FileDelete(ConfigurationFileName);

  OptionsFileName := ChangeFileExt(ProjectFileName, DelphiOptionsFileExtension);

  if FileExists(OptionsFileName) then
  begin
    OptionsFile := TIniFile.Create(OptionsFileName);
    try
      SearchPath := OptionsFile.ReadString(DOFDirectoriesSection, DOFSearchPathName, '');
      AddPathOption('N', OptionsFile.ReadString(DOFDirectoriesSection, DOFUnitOutputDirKey, ''));
      AddPathOption('I', SearchPath);
      AddPathOption('R', SearchPath);

      if SamePath(DCPPath, Installation.DCPOutputPath) then
        SearchDcpPath := DCPPath
      else
        SearchDcpPath := StrEnsureSuffix(PathSep, DCPPath) + Installation.DCPOutputPath;
      AddPathOption('U', StrEnsureSuffix(PathSep, SearchDcpPath) + SearchPath);
      
      if OptionsFile.ReadString(DOFCompilerSection,DOFPackageNoLinkKey,'') = '1' then
      begin
        DynamicPackages := OptionsFile.ReadString(DOFLinkerSection, DOFPackagesKey, '');
        if DynamicPackages <> '' then
          Options.Add(Format('-LU"%s"',[DynamicPackages]));
      end;
    finally
      OptionsFile.Free;
    end;
  end;
end;

function TJclDCC32.Compile(const ProjectFileName: string): Boolean;
begin
  {$IFDEF MSWINDOWS}
  // quotes not required with short path names
  Result := Execute(PathGetShortName(ExtractFileDir(ProjectFileName))
    + DirDelimiter + ExtractFileName(ProjectFileName));
  {$ELSE}
  Result := Execute(StrDoubleQuote(StrTrimQuotes(ProjectFileName)));
  {$ENDIF}
end;

constructor TJclDCC32.Create(AInstallation: TJclBorRADToolInstallation);
begin
  inherited Create(AInstallation);
  SetDefaultOptions; // in case $(DELPHI)\bin\dcc32.cfg (replace as appropriate) is invalid
end;

function TJclDCC32.Execute(const CommandLine: string): Boolean;
const
  {$IFDEF WIN32}
  ConfFileName = 'DCC32.CFG';
  {$ENDIF WIN32}
  {$IFDEF KYLIX}
  ConfFileName = 'dcc.conf';
  {$ENDIF KYLIX}
begin
  FOutput := '';
  SaveOptionsToFile(ConfFileName);
  Result := inherited Execute(CommandLine);
  FileDelete(ConfFileName);
end;

procedure TJclDCC32.SaveOptionsToFile(const ConfigFileName: string);
{$IFDEF MSWINDOWS}

  function IsPathOption(const S: string; out Len: Integer): Boolean;
  begin
    Result := False;
    if Length(S) >= 2 then
      case UpCase(S[2]) of
        'E', 'I', 'O', 'R', 'U':
          begin
            Result := True;
            Len := 2;
          end;
        'L':
          if Length(S) >= 3 then
          begin
            Result := UpCase(S[3]) in ['E', 'N'];
            Len := 3;
          end;
        'N':
          begin
            Result := True;
            if (Length(S) >= 3) and (S[3] in ['0'..'9']) then
              Len := 3
            else
              Len := 2;
          end;
      end;
  end;

var
  I, J: Integer;
  List: TStringList;
  S: string;
  F: TextFile;
begin
  AssignFile(F, ConfigFileName);
  Rewrite(F);
  List := TStringList.Create;
  try
    for I := 0 to Options.Count - 1 do
    begin
      S := Options[I];
      if IsPathOption(S, J) then
      begin
        Write(F, Copy(S, 1, J), '"');
        StrToStrings(StrTrimQuotes(PChar(@S[J + 1])), PathSep, List);
        // change to relative paths to avoid DCC32 126 character path limit
        for J := 0 to List.Count - 1 do
          List[J] := PathGetRelativePath(GetCurrentFolder, ExpandFileName(List[J]));
        if List.Count > 0 then
        begin
          for J := 0 to List.Count - 2 do
            Write(F, List[J], PathSep);
          WriteLn(F, List[List.Count - 1], '"');
        end;
      end
      else
        WriteLn(F, S);
    end;
  finally
    List.Free;
  end;
  CloseFile(F);
end;
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
begin
  FOptions.SaveToFile(ConfigFileName);
end;
{$ENDIF UNIX}

function TJclDCC32.GetExeName: string;
begin
  Result := DCC32ExeName;
end;

function TJclDCC32.MakePackage(const PackageName, BPLPath, DCPPath: string; ExtraOptions:string): Boolean;
var
  SaveDir: string;
begin
  SaveDir := GetCurrentDir;
  SetCurrentDir(ExtractFilePath(PackageName) + '.');
  try
    Options.Clear;
    AddProjectOptions(PackageName, DCPPath);
    AddPathOption('LN', DCPPath);
    AddPathOption('LE', BPLPath);
    Options.Add(ExtraOptions);
    Result := Compile(PackageName);
  finally
    SetCurrentDir(SaveDir);
  end;
end;

function TJclDCC32.MakeProject(const ProjectName, OutputDir, DcpSearchPath: string;
  ExtraOptions: string): Boolean;
var
  SaveDir: string;
begin
  SaveDir := GetCurrentDir;
  SetCurrentDir(ExtractFilePath(ProjectName) + '.');
  try
    Options.Clear;
    AddProjectOptions(ProjectName, DcpSearchPath);
    AddPathOption('E', OutputDir);
    Options.Add(ExtraOptions);
    Result := Compile(ProjectName);
  finally
    SetCurrentDir(SaveDir);
  end;
end;

procedure TJclDCC32.SetDefaultOptions;
begin
  Options.Clear;
  AddPathOption('U', Installation.LibFolderName);
  if Installation.RadToolKind = brCppBuilder then
  begin
    AddPathOption('U', Installation.LibFolderName + PathAddSeparator('obj'));
    {$IFNDEF KYLIX}
    if (Installation.RadToolKind <> brBorlandDevStudio)
      and (Installation.VersionNumber = 5) then
      Options.Add('-LUvcl50')
    else
      Options.Add('-LUrtl');
    {$ENDIF ~KYLIX}
  end;
end;

{$IFDEF KEEP_DEPRECATED}
function TJclDCC32.SupportsLibSuffix: Boolean;
begin
  Result := Installation.SupportsLibSuffix;
end;
{$ENDIF KEEP_DEPRECATED}

//=== { TJclBorlandMake } ====================================================

function TJclBorlandMake.GetExeName: string;
begin
  Result := MakeExeName;
end;

//=== { TJclBpr2Mak } ========================================================

function TJclBpr2Mak.GetExeName: string;
begin
  Result := Bpr2MakExeName;
end;

//=== { TJclBorRADToolPalette } ==============================================

constructor TJclBorRADToolPalette.Create(AInstallation: TJclBorRADToolInstallation);
begin
  inherited Create(AInstallation);
  FKey := PaletteKeyName;
  FTabNames := TStringList.Create;
  FTabNames.Sorted := True;
  ReadTabNames;
end;

destructor TJclBorRADToolPalette.Destroy;
begin
  FreeAndNil(FTabNames);
  inherited Destroy;
end;

procedure TJclBorRADToolPalette.ComponentsOnTabToStrings(Index: Integer; Strings: TStrings;
  IncludeUnitName: Boolean; IncludeHiddenComponents: Boolean);
var
  TempList: TStringList;

  procedure ProcessList(Hidden: Boolean);
  var
    D, I: Integer;
    List, S: string;
  begin
    if Hidden then
      List := HiddenComponentsOnTab[Index]
    else
      List := ComponentsOnTab[Index];
    List := StrEnsureSuffix(';', List);
    while Length(List) > 1 do
    begin
      D := Pos(';', List);
      S := Trim(Copy(List, 1, D - 1));
      if not IncludeUnitName then
        Delete(S, 1, Pos('.', S));
      if Hidden then
      begin
        I := TempList.IndexOf(S);
        if I = -1 then
          TempList.AddObject(S, Pointer(True))
        else
          TempList.Objects[I] := Pointer(True);
      end
      else
        TempList.Add(S);
      Delete(List, 1, D);
    end;
  end;

begin
  TempList := TStringList.Create;
  try
    TempList.Duplicates := dupError;
    ProcessList(False);
    TempList.Sorted := True;
    if IncludeHiddenComponents then
      ProcessList(True);
    Strings.AddStrings(TempList);
  finally
    TempList.Free;
  end;
end;

function TJclBorRADToolPalette.DeleteTabName(const TabName: string): Boolean;
var
  I: Integer;
begin
  I := FTabNames.IndexOf(TabName);
  Result := I >= 0;
  if Result then
  begin
    Installation.ConfigData.DeleteKey(Key, FTabNames[I]);
    Installation.ConfigData.DeleteKey(Key, FTabNames[I] + PaletteHiddenTag);
    FTabNames.Delete(I);
  end;
end;

function TJclBorRADToolPalette.GetComponentsOnTab(Index: Integer): string;
begin
  Result := Installation.ConfigData.ReadString(Key, FTabNames[Index], '');
end;

function TJclBorRADToolPalette.GetHiddenComponentsOnTab(Index: Integer): string;
begin
  Result := Installation.ConfigData.ReadString(Key, FTabNames[Index] + PaletteHiddenTag, '');
end;

function TJclBorRADToolPalette.GetTabNameCount: Integer;
begin
  Result := FTabNames.Count;
end;

function TJclBorRADToolPalette.GetTabNames(Index: Integer): string;
begin
  Result := FTabNames[Index];
end;

procedure TJclBorRADToolPalette.ReadTabNames;
var
  TempList: TStringList;
  I: Integer;
  S: string;
begin
  if Installation.ConfigData.SectionExists(Key) then
  begin
    TempList := TStringList.Create;
    try
      Installation.ConfigData.ReadSection(Key, TempList);
      for I := 0 to TempList.Count - 1 do
      begin
        S := TempList[I];
        if Pos(PaletteHiddenTag, S) = 0 then
          FTabNames.Add(S);
      end;
    finally
      TempList.Free;
    end;
  end;
end;

function TJclBorRADToolPalette.TabNameExists(const TabName: string): Boolean;
begin
  Result := FTabNames.IndexOf(TabName) <> -1;
end;

//=== { TJclBorRADToolRepository } ===========================================

constructor TJclBorRADToolRepository.Create(AInstallation: TJclBorRADToolInstallation);
begin
  inherited Create(AInstallation);
  {$IFDEF KYLIX}
  FFileName := AInstallation.ConfigFileName('dro');
  {$ELSE}
  FFileName := AInstallation.BinFolderName + BorRADToolRepositoryFileName;
  {$ENDIF KYLIX}
  FPages := TStringList.Create;
  IniFile.ReadSection(BorRADToolRepositoryPagesSection, FPages);
  CloseIniFile;
end;

destructor TJclBorRADToolRepository.Destroy;
begin
  FreeAndNil(FPages);
  FreeAndNil(FIniFile);
  inherited Destroy;
end;

procedure TJclBorRADToolRepository.AddObject(const FileName, ObjectType, PageName, ObjectName,
  IconFileName, Description, Author, Designer: string; const Ancestor: string);
var
  SectionName: string;
begin
  GetIniFile;
  SectionName := AnsiUpperCase(PathRemoveExtension(FileName));
  FIniFile.EraseSection(FileName);
  FIniFile.EraseSection(SectionName);
  FIniFile.WriteString(SectionName, BorRADToolRepositoryObjectType, ObjectType);
  FIniFile.WriteString(SectionName, BorRADToolRepositoryObjectName, ObjectName);
  FIniFile.WriteString(SectionName, BorRADToolRepositoryObjectPage, PageName);
  FIniFile.WriteString(SectionName, BorRADToolRepositoryObjectIcon, IconFileName);
  FIniFile.WriteString(SectionName, BorRADToolRepositoryObjectDescr, Description);
  FIniFile.WriteString(SectionName, BorRADToolRepositoryObjectAuthor, Author);
  if Ancestor <> '' then
    FIniFile.WriteString(SectionName, BorRADToolRepositoryObjectAncestor, Ancestor);
  if (Installation.RadToolKind = brBorlandDevStudio)
    or (Installation.VersionNumber >= 6) then
    FIniFile.WriteString(SectionName, BorRADToolRepositoryObjectDesigner, Designer);
  FIniFile.WriteBool(SectionName, BorRADToolRepositoryObjectNewForm, False);
  FIniFile.WriteBool(SectionName, BorRADToolRepositoryObjectMainForm, False);
  CloseIniFile;
end;

procedure TJclBorRADToolRepository.CloseIniFile;
begin
  FreeAndNil(FIniFile);
end;

function TJclBorRADToolRepository.FindPage(const Name: string; OptionalIndex: Integer): string;
var
  I: Integer;
begin
  I := Pages.IndexOf(Name);
  if I >= 0 then
    Result := Pages[I]
  else
  begin
    if OptionalIndex < Pages.Count then
      Result := Pages[OptionalIndex]
    else
      Result := '';
  end;
end;

function TJclBorRADToolRepository.GetIniFile: TIniFile;
begin
  if not Assigned(FIniFile) then
    FIniFile := TIniFile.Create(FileName);
  Result := FIniFile;
end;

function TJclBorRADToolRepository.GetPages: TStrings;
begin
  Result := FPages;
end;

procedure TJclBorRADToolRepository.RemoveObjects(const PartialPath, FileName, ObjectType: string);
var
  Sections: TStringList;
  I: Integer;
  SectionName, FileNamePart, PathPart, DialogFileName: string;
begin
  Sections := TStringList.Create;
  try
    GetIniFile;
    FIniFile.ReadSections(Sections);
    for I := 0 to Sections.Count - 1 do
    begin
      SectionName := Sections[I];
      if FIniFile.ReadString(SectionName, BorRADToolRepositoryObjectType, '') = ObjectType then
      begin
        FileNamePart := PathExtractFileNameNoExt(SectionName);
        PathPart := StrRight(PathAddSeparator(ExtractFilePath(SectionName)), Length(PartialPath));
        DialogFileName := PathExtractFileNameNoExt(FileName);
        if StrSame(FileNamePart, DialogFileName) and StrSame(PathPart, PartialPath) then
          FIniFile.EraseSection(SectionName);
      end;
    end;
  finally
    Sections.Free;
  end;
end;

//=== { TJclBorRADToolInstallation } =========================================

constructor TJclBorRADToolInstallation.Create;
begin
  inherited Create;
  FConfigDataLocation := AConfigDataLocation;
  {$IFDEF KYLIX}
  FConfigData := TMemIniFile.Create(AConfigDataLocation);
  {$ELSE ~KYLIX}
  FConfigData := TRegistryIniFile.Create(AConfigDataLocation);
  {$ENDIF ~KYLIX}
  FGlobals := TStringList.Create;
  ReadInformation;
  FIdeTools := TJclBorRADToolIdeTool.Create(Self);
  {$IFDEF MSWINDOWS}
  FOpenHelp := TJclBorlandOpenHelp.Create(Self);
  {$ENDIF ~MSWINDOWS}
  FMapCreate := False;
  {$IFDEF MSWINDOWS}
  FMapLink := False;
  FMapDelete := False;
  {$ENDIF ~MSWINDOWS}
  if FileExists(BinFolderName + AsmExeName) then
    Include(FCommandLineTools, clAsm);
  if FileExists(BinFolderName + BCC32ExeName) then
    Include(FCommandLineTools, clBcc32);
  if FileExists(BinFolderName + DCC32ExeName) then
    Include(FCommandLineTools, clDcc32);
  if FileExists(BinFolderName + DCCILExeName) then
    Include(FCommandLineTools, clDccIL);
  if FileExists(BinFolderName + MakeExeName) then
    Include(FCommandLineTools, clMake);
  if FileExists(BinFolderName + Bpr2MakExeName) then
    Include(FCommandLineTools, clProj2Mak);
end;

destructor TJclBorRADToolInstallation.Destroy;
begin
  FreeAndNil(FRepository);
  FreeAndNil(FDCC32);
  FreeAndNil(FBCC32);
  FreeAndNil(FBpr2Mak);
  FreeAndNil(FIdePackages);
  FreeAndNil(FIdeTools);
  {$IFDEF MSWINDOWS}
  FreeAndNil(FOpenHelp);
  {$ENDIF MSWINDOWS}
  FreeAndNil(FPalette);
  FreeAndNil(FGlobals);
  {$IFDEF KYLIX}
  FConfigData.UpdateFile; // TMemIniFile.Destroy doesn't call UpdateFile
  {$ENDIF KYLIX}
  FreeAndNil(FConfigData);
  inherited Destroy;
end;

function TJclBorRADToolInstallation.AddToDebugDCUPath(const Path: string): Boolean;
var
  TempDebugDCUPath: TJclBorRADToolPath;
begin
  TempDebugDCUPath := DebugDCUPath;
  PathListIncludeItems(TempDebugDCUPath, Path);
  Result := True;
  DebugDCUPath := TempDebugDCUPath;
end;

function TJclBorRADToolInstallation.AddToLibrarySearchPath(const Path: string): Boolean;
var
  TempLibraryPath: TJclBorRADToolPath;
begin
  TempLibraryPath := LibrarySearchPath;
  PathListIncludeItems(TempLibraryPath, Path);
  Result := True;
  LibrarySearchPath := TempLibraryPath;
end;

function TJclBorRADToolInstallation.AddToLibraryBrowsingPath(const Path: string): Boolean;
var
  TempLibraryPath: TJclBorRADToolPath;
begin
  TempLibraryPath := LibraryBrowsingPath;
  PathListIncludeItems(TempLibraryPath, Path);
  Result := True;
  LibraryBrowsingPath := TempLibraryPath;
end;

function TJclBorRADToolInstallation.AnyInstanceRunning: Boolean;
var
  Processes: TStringList;
  I: Integer;
begin
  Result := False;
  Processes := TStringList.Create;
  try
    if RunningProcessesList(Processes) then
    begin
      for I := 0 to Processes.Count - 1 do
        if AnsiSameText(IdeExeFileName, Processes[I]) then
        begin
          Result := True;
          Break;
        end;
    end;
  finally
    Processes.Free;
  end;
end;

{$IFDEF KYLIX}
function TJclBorRADToolInstallation.ConfigFileName(const Extension: string): string;
begin
  Result := '';
end;
{$ENDIF KYLIX}

class procedure TJclBorRADToolInstallation.ExtractPaths(const Path: TJclBorRADToolPath; List: TStrings);
begin
  StrToStrings(Path, PathSep, List);
end;

function TJclBorRADToolInstallation.CompileBCBPackage(const PackageName,
  BPLPath, DCPPath: string): Boolean;
var
  SaveDir, PackagePath, MakeFileName, BinaryFileName: string;
  RunOnly: Boolean;
begin
  OutputString(Format(RsCompilingPackage, [PackageName]));

  if not IsBCBPackage(PackageName) then
    raise EJclBorRADException.CreateResFmt(@RsNotABCBPackage, [PackageName]);

  PackagePath := PathRemoveSeparator(ExtractFilePath(PackageName));
  SaveDir := GetCurrentDir;
  SetCurrentDir(PackagePath);
  try
    MakeFileName := StrTrimQuotes(ChangeFileExt(PackageName, '.mak'));
    if clProj2Mak in CommandLineTools then       // let bpr2mak generate make file from .bpk
    begin
      // Kylix bpr2mak doesn't like full file names
      Result := Bpr2Mak.Execute(StringsToStr(Bpr2Mak.Options, ' ') + ' ' + ExtractFileName(PackageName))
    end
    else
      // If make file exists (and doesn't need to be created by bpr2mak)
      Result := FileExists(MakeFileName);

    if MapCreate then
      Make.Options.Add('-DMAPFLAGS=-s');

    GetBPKFileInfo(PackageName, RunOnly, @BinaryFileName);
      
    Result := Result and Make.Execute(Format('%s -f%s', [StringsToStr(Make.Options, ' '), StrDoubleQuote(MakeFileName)]))
      and LinkMapFile(PathAddSeparator(BPLPath) + BinaryFileName);
  finally
    SetCurrentDir(SaveDir);
  end;

  if Result then
    OutputString(RsCompilationOk)
  else
    OutputString(RsCompilationFailed);
end;

function TJclBorRADToolInstallation.CompileBCBProject(const ProjectName,
  OutputDir, DcpSearchPath: string): Boolean;
var
  SaveDir, PackagePath, MakeFileName, BinaryFileName: string;
begin
  OutputString(Format(RsCompilingProject, [ProjectName]));

  if not IsBCBProject(ProjectName) then
    raise EJclBorRADException.CreateResFmt(@RsNotADelphiProject, [ProjectName]);

  PackagePath := PathRemoveSeparator(ExtractFilePath(ProjectName));
  SaveDir := GetCurrentDir;
  SetCurrentDir(PackagePath);
  try
    MakeFileName := StrTrimQuotes(ChangeFileExt(ProjectName, '.mak'));
    if clProj2Mak in CommandLineTools then       // let bpr2mak generate make file from .bpk
      // Kylix bpr2mak doesn't like full file names
      Result := Bpr2Mak.Execute(StringsToStr(Bpr2Mak.Options, ' ') + ' ' + ExtractFileName(ProjectName))
    else
      // If make file exists (and doesn't need to be created by bpr2mak)
      Result := FileExists(MakeFileName);

    if MapCreate then
      Make.Options.Add('-DMAPFLAGS=-s');

    GetBPRFileInfo(ProjectName, BinaryFileName);

    Result := Result and Make.Execute(Format('%s -f%s', [StringsToStr(Make.Options, ' '), StrDoubleQuote(MakeFileName)]))
      and LinkMapFile(PathAddSeparator(OutputDir) + BinaryFileName);
  finally
    SetCurrentDir(SaveDir);
  end;

  if Result then
    OutputString(RsCompilationOk)
  else
    OutputString(RsCompilationFailed);
end;

function TJclBorRADToolInstallation.CompileDelphiPackage(const PackageName,
  BPLPath, DCPPath: string): Boolean;
begin
  Result := CompileDelphiPackage(PackageName, BPLPath, DCPPath, '');
end;

function TJclBorRADToolInstallation.CompileDelphiPackage(const PackageName,
  BPLPath, DCPPath, ExtraOptions: string): Boolean;
var
  NewOptions, LibSuffix, BinaryFileName: string;
  RunOnly: Boolean;
begin
  OutputString(Format(RsCompilingPackage, [PackageName]));

  if not IsDelphiPackage(PackageName) then
    raise EJclBorRADException.CreateResFmt(@RsNotADelphiPackage, [PackageName]);

  if MapCreate then
    NewOptions := ExtraOptions + ' -GD'
  else
    NewOptions := ExtraOptions;

  GetDPKFileInfo(PackageName, RunOnly, @LibSuffix);
  BinaryFileName := PathAddSeparator(BPLPath) + PathExtractFileNameNoExt(PackageName) + LibSuffix + BinaryExtensionPackage;

  Result := DCC32.MakePackage(PackageName, BPLPath, DCPPath, NewOptions)
    and LinkMapFile(BinaryFileName);

  if Result then
    OutputString(RsCompilationOk)
  else
    OutputString(RsCompilationFailed);
end;

function TJclBorRADToolInstallation.CompileDelphiProject(const ProjectName,
  OutputDir, DcpSearchPath: string): Boolean;
var
  ExtraOptions, BinaryExtension, LibSuffix, BinaryFileName: string;
begin
  OutputString(Format(RsCompilingProject, [ProjectName]));

  if not IsDelphiProject(ProjectName) then
    raise EJclBorRADException.CreateResFmt(@RsNotADelphiProject, [ProjectName]);

  if MapCreate then
    ExtraOptions := '-GD'
  else
    ExtraOptions := '';

  GetDPRFileInfo(ProjectName, BinaryExtension, @LibSuffix);
  BinaryFileName := PathAddSeparator(OutputDir) + PathExtractFileNameNoExt(ProjectName) + LibSuffix + BinaryExtension;

  Result := DCC32.MakeProject(ProjectName, OutputDir, DcpSearchPath, ExtraOptions)
    and LinkMapFile(BinaryFileName);

  if Result then
    OutputString(RsCompilationOk)
  else
    OutputString(RsCompilationFailed);
end;

function TJclBorRADToolInstallation.CompilePackage(const PackageName, BPLPath,
  DCPPath: string): Boolean;
var
  PackageExtension: string;
begin
  PackageExtension := ExtractFileExt(PackageName);
  if SameText(PackageExtension,SourceExtensionBCBPackage) then
    Result := CompileBCBPackage(PackageName, BPLPath, DCPPath)
  else if SameText(PackageExtension, SourceExtensionDelphiPackage) then
    Result := CompileDelphiPackage(PackageName, BPLPath, DCPPath)
  else
    raise EJclBorRadException.CreateResFmt(@RsUnknownPackageExtension, [PackageExtension]);
end;

function TJclBorRADToolInstallation.CompileProject(const ProjectName,
  OutputDir, DcpSearchPath: string): Boolean;
var
  ProjectExtension: string;
begin
  ProjectExtension := ExtractFileExt(ProjectName);
  if SameText(ProjectExtension,SourceExtensionBCBProject) then
    Result := CompileBCBProject(ProjectName, OutputDir, DcpSearchPath)
  else if SameText(ProjectExtension, SourceExtensionDelphiProject) then
    Result := CompileDelphiProject(ProjectName, OutputDir, DcpSearchPath)
  else
    raise EJclBorRadException.CreateResFmt(@RsUnknownProjectExtension, [ProjectExtension]);
end;

function TJclBorRADToolInstallation.FindFolderInPath(Folder: string; List: TStrings): Integer;
var
  I: Integer;
begin
  Result := -1;
  Folder := PathRemoveSeparator(Folder);
  for I := 0 to List.Count - 1 do
    if SamePath(Folder, PathRemoveSeparator(SubstitutePath(List[I]))) then
    begin
      Result := I;
      Break;
    end;
end;

function TJclBorRADToolInstallation.GetBPLOutputPath: string;
begin
  Result := SubstitutePath(ConfigData.ReadString(LibraryKeyName, LibraryBPLOutputValueName, ''));
end;

function TJclBorRADToolInstallation.GetBpr2Mak: TJclBpr2Mak;
begin
  if not Assigned(FBpr2Mak) then
  begin
    if not (clProj2Mak in CommandLineTools) then
      raise EJclBorRadException.CreateResFmt(@RsNotFound, [Bpr2MakExeName]);
    FBpr2Mak := TJclBpr2Mak.Create(Self);
  end;
  Result := FBpr2Mak;
end;

function TJclBorRADToolInstallation.GetBCC32: TJclBCC32;
begin
  if not Assigned(FBCC32) then
  begin
    if not (clBcc32 in CommandLineTools) then
      raise EJclBorRadException.CreateResFmt(@RsNotFound, [Bcc32ExeName]);
    FBCC32 := TJclBCC32.Create(Self);
  end;
  Result := FBCC32;
end;

function TJclBorRADToolInstallation.GetDCC32: TJclDCC32;
begin
  if not Assigned(FDCC32) then
  begin
    if not (clDcc32 in CommandLineTools) then
      raise EJclBorRadException.CreateResFmt(@RsNotFound, [Dcc32ExeName]);
    FDCC32 := TJclDCC32.Create(Self);
  end;
  Result := FDCC32;
end;

function TJclBorRADToolInstallation.GetDCPOutputPath: string;
begin
  Result := SubstitutePath(ConfigData.ReadString(LibraryKeyName, LibraryDCPOutputValueName, ''));
end;

function TJclBorRADToolInstallation.GetDebugDCUPath: string;
begin
  Result := ConfigData.ReadString(DebuggingKeyName, DebugDCUPathValueName, '');
end;

function TJclBorRADToolInstallation.GetDefaultProjectsDir: string;
begin
  {$IFDEF KYLIX}
  Result := GetPersonalFolder;
  {$ELSE ~KYLIX}
  Result := Globals.Values['DefaultProjectsDirectory'];
  if Result = '' then
    Result := PathAddSeparator(RootDir) + 'Projects';
  {$ENDIF ~KYLIX}
end;

function TJclBorRADToolInstallation.GetDescription: TJclBorRADToolPath;
begin
  Result := Format('%s %s', [Name, EditionAsText]);
  if InstalledUpdatePack > 0 then
    Result := Result + ' ' + Format(RsUpdatePackName, [InstalledUpdatePack]);
end;

function TJclBorRADToolInstallation.GetEditionAsText: string;
begin
  {$IFDEF KYLIX}
  case Edition of
    deOPEN:
      Result := RsOpenEdition;
    dePRO:
      Result := RsProfessional;
    deSVR:
      if VersionNumber >= 2 then
        Result := RsEnterprise
      else
        Result := RsServerDeveloper;
  end;
  {$ELSE}
  Result := FEditionStr;
  if Length(FEditionStr) = 3 then
    case Edition of
      deSTD:
        if (VersionNumber >= 6) or (RadToolKind = brBorlandDevStudio) then
          Result := RsPersonal
        else
          Result := RsStandard;
      dePRO:
        Result := RsProfessional;
      deCSS:
        if (VersionNumber >= 5) or (RadToolKind = brBorlandDevStudio) then
          Result := RsEnterprise
        else
          Result := RsClientServer;
      deARC:
        Result := RsArchitect;
    end;
  {$ENDIF KYLIX}
end;

function TJclBorRADToolInstallation.GetEnvironmentVariables: TStrings;
var
  EnvNames: TStringList;
  EnvVarKeyName: string;
  I: Integer;
begin
  if FEnvironmentVariables = nil then
  begin
    FEnvironmentVariables := TStringList.Create;
    if ((VersionNumber >= 6) or (RadToolKind = brBorlandDevStudio))
      and ConfigData.SectionExists(EnvVariablesKeyName) then
    begin
      EnvNames := TStringList.Create;
      try
        ConfigData.ReadSection(EnvVariablesKeyName, EnvNames);
        for I := 0 to EnvNames.Count - 1 do
        begin
          EnvVarKeyName := EnvNames[I];
          FEnvironmentVariables.Values[EnvVarKeyName] := ConfigData.ReadString(EnvVariablesKeyName, EnvVarKeyName, '');
        end;
      finally
        EnvNames.Free;
      end;
    end;
  end;
  Result := FEnvironmentVariables;
end;

function TJclBorRADToolInstallation.GetGlobals: TStrings;
begin
  Result := FGlobals;
end;

function TJclBorRADToolInstallation.GetIdeExeFileName: string;
{$IFDEF KYLIX}
const
  IdeFileNames: array [brDelphi..brCppBuilder] of string = (DelphiIdeExeName, BCBIdeExeName);
begin
  Result := FBinFolderName + IdeFileNames[RADToolKind];
end;
{$ENDIF KYLIX}
{$IFDEF MSWINDOWS}
begin
  Result := Globals.Values['App'];
end;
{$ENDIF MSWINDOWS}

function TJclBorRADToolInstallation.GetIdeExeBuildNumber: string;
begin
  {$IFDEF KYLIX}
  { TODO : determine Kylix IDE build # }
  Result := '?';
  {$ELSE}
  Result := VersionFixedFileInfoString(IdeExeFileName, vfFull);
  {$ENDIF KYLIX}
end;

function TJclBorRADToolInstallation.GetIdePackages: TJclBorRADToolIdePackages;
begin
  if not Assigned(FIdePackages) then
    FIdePackages := TJclBorRADToolIdePackages.Create(Self);
  Result := FIdePackages;
end;

function TJclBorRADToolInstallation.GetLatestUpdatePack: Integer;
begin
  Result := GetLatestUpdatePackForVersion(VersionNumber);
end;

class function TJclBorRADToolInstallation.GetLatestUpdatePackForVersion(Version: Integer): Integer;
begin
  {$IFDEF MSWINDOWS}
  raise EAbstractError.CreateResFmt(@SAbstractError, ['']); // BCB doesn't support abstract keyword
  // dummy; BCB doesn't like abstract class functions
  {$ELSE MSWINDOWS}
  Result := 0;
  {$ENDIF MSWINDOWS}
end;

function TJclBorRADToolInstallation.GetLibrarySearchPath: TJclBorRADToolPath;
begin
  Result := ConfigData.ReadString(LibraryKeyName, LibrarySearchPathValueName, '');
end;

function TJclBorRADToolInstallation.GetMake: IJclCommandLineTool;
begin
  if not Assigned(FMake) then
  begin
    if not (clMake in CommandLineTools) then
      raise EJclBorRadException.CreateResFmt(@RsNotFound, [MakeExeName]);
    {$IFDEF KYLIX}
    FMake := TJclCommandLineTool.Create(MakeExeName);
    {$ELSE ~KYLIX}
    FMake := TJclBorlandMake.Create(Self);
    // Set option "-l+", which enables use of long command lines.  Should be
    // default, but there have been reports indicating that's not always the case.
    FMake.Options.Add('-l+');
    {$ENDIF ~KYLIX}
  end;
  Result := FMake;
end;

function TJclBorRADToolInstallation.GetLibraryBrowsingPath: TJclBorRADToolPath;
begin
  Result := ConfigData.ReadString(LibraryKeyName, LibraryBrowsingPathValueName, '');
end;

function TJclBorRADToolInstallation.GetName: string;
begin
  {$IFDEF KYLIX}
  Result := Format(RsKylixVersionName, [VersionNumber, RADToolName]);
  {$ELSE ~KYLIX}
  Result := Format('%s %d', [RADToolName, VersionNumber]);
  {$ENDIF ~KYLIX}
end;

function TJclBorRADToolInstallation.GetPalette: TJclBorRADToolPalette;
begin
  if not Assigned(FPalette) then
    FPalette := TJclBorRADToolPalette.Create(Self);
  Result := FPalette;
end;

function TJclBorRADToolInstallation.GetRepository: TJclBorRADToolRepository;
begin
  if not Assigned(FRepository) then
    FRepository := TJclBorRADToolRepository.Create(Self);
  Result := FRepository;
end;

function TJclBorRADToolInstallation.GetSupportsLibSuffix: Boolean;
begin
{$IFDEF KYLIX}
  Result := True;
{$ELSE}
  Result := (RadToolKind = brBorlandDevStudio) or (VersionNumber >= 6);
{$ENDIF KYLIX}
end;

function TJclBorRADToolInstallation.GetUpdateNeeded: Boolean;
begin
  Result := InstalledUpdatePack < LatestUpdatePack;
end;

function TJclBorRADToolInstallation.GetValid: Boolean;
begin
  Result := (ConfigData.FileName <> '') and (RootDir <> '') and FileExists(IdeExeFileName);
end;

function TJclBorRADToolInstallation.GetVclIncludeDir: string;
begin
  Result := RootDir + RsVclIncludeDir;
  if not DirectoryExists(Result) then
    Result := '';
end;

function TJclBorRADToolInstallation.InstallBCBExpert(const ProjectName,
  OutputDir, DcpSearchPath: string): Boolean;
var
  BinaryFileName, Description: string;
begin
  OutputString(Format(RsExpertInstallationStarted, [ProjectName]));

  GetBPRFileInfo(ProjectName, BinaryFileName, @Description);
  BinaryFileName := PathAddSeparator(OutputDir) + BinaryFileName;

  Result :=    CompileBCBProject(ProjectName, OutputDir, DcpSearchPath)
           and RegisterExpert(BinaryFileName, Description);

  OutputString(RsExpertInstallationFinished);
end;

function TJclBorRADToolInstallation.InstallBCBIdePackage(const PackageName,
  BPLPath, DCPPath: string): Boolean;
var
  RunOnly: Boolean;
  BinaryFileName, Description: string;
begin
  OutputString(Format(RsIdePackageInstallationStarted, [PackageName]));

  GetBPKFileInfo(PackageName, RunOnly, @BinaryFileName, @Description);
  BinaryFileName := PathAddSeparator(BPLPath) + BinaryFileName;
  if RunOnly then
    raise EJclBorRadException.CreateResFmt(@RsCannotInstallRunOnly, [PackageName]);

  Result :=    CompileBCBPackage(PackageName, BPLPath, DCPPath)
           and RegisterIdePackage(BinaryFileName, Description);

  OutputString(RsIdePackageInstallationFinished);
end;

function TJclBorRADToolInstallation.InstallBCBPackage(const PackageName,
  BPLPath, DCPPath: string): Boolean;
var
  RunOnly: Boolean;
  BinaryFileName, Description: string;
begin
  OutputString(Format(RsPackageInstallationStarted, [PackageName]));

  GetBPKFileInfo(PackageName, RunOnly, @BinaryFileName, @Description);
  BinaryFileName := PathAddSeparator(BPLPath) + BinaryFileName;
  if RunOnly then
    raise EJclBorRadException.CreateResFmt(@RsCannotInstallRunOnly, [PackageName]);

  Result :=    CompileBCBPackage(PackageName, BPLPath, DCPPath)
           and RegisterPackage(BinaryFileName, Description);

  OutputString(RsPackageInstallationFinished);
end;

function TJclBorRADToolInstallation.InstallDelphiExpert(const ProjectName,
  OutputDir, DcpSearchPath: string): Boolean;
var
  LibSuffix, BinaryFileName, BinaryExtension, BaseName: string;
begin
  OutputString(Format(RsExpertInstallationStarted, [ProjectName]));

  BaseName := PathExtractFileNameNoExt(ProjectName);

  GetDPRFileInfo(ProjectName, BinaryExtension, @LibSuffix);
  if BinaryExtension = '' then
    BinaryExtension := BinaryExtensionLibrary;

  BinaryFileName := PathAddSeparator(OutputDir) + BaseName + LibSuffix + BinaryExtension;

  Result :=    CompileDelphiProject(ProjectName, OutputDir, DcpSearchPath)
           and RegisterExpert(BinaryFileName, BaseName);

  OutputString(RsExpertInstallationFinished);
end;

function TJclBorRADToolInstallation.InstallDelphiIdePackage(const PackageName,
  BPLPath, DCPPath: string): Boolean;
var
  RunOnly: Boolean;
  LibSuffix, BPLFileName, Description: string;
begin
  OutputString(Format(RsIdePackageInstallationStarted, [PackageName]));

  GetDPKFileInfo(PackageName, RunOnly, @LibSuffix, @Description);
  if RunOnly then
    raise EJclBorRadException.CreateResFmt(@RsCannotInstallRunOnly, [PackageName]);
  BPLFileName := PathAddSeparator(BPLPath) + PathExtractFileNameNoExt(PackageName) + LibSuffix + BinaryExtensionPackage;
  
  Result :=    CompileDelphiPackage(PackageName, BPLPath, DCPPath)
           and RegisterIdePackage(BPLFileName, Description);

  OutputString(RsIdePackageInstallationFinished);
end;

function TJclBorRADToolInstallation.InstallDelphiPackage(const PackageName,
  BPLPath, DCPPath: string): Boolean;
var
  RunOnly: Boolean;
  LibSuffix, BPLFileName, Description: string;
begin
  OutputString(Format(RsPackageInstallationStarted, [PackageName]));

  GetDPKFileInfo(PackageName, RunOnly, @LibSuffix, @Description);
  if RunOnly then
    raise EJclBorRadException.CreateResFmt(@RsCannotInstallRunOnly, [PackageName]);
  BPLFileName := PathAddSeparator(BPLPath) + PathExtractFileNameNoExt(PackageName) + LibSuffix + BinaryExtensionPackage;
  
  Result :=    CompileDelphiPackage(PackageName, BPLPath, DCPPath)
           and RegisterPackage(BPLFileName, Description);

  OutputString(RsPackageInstallationFinished);
end;

function TJclBorRADToolInstallation.InstallExpert(const ProjectName,
  OutputDir, DcpSearchPath: string): Boolean;
var
  ProjectExtension: string;
begin
  ProjectExtension := ExtractFileExt(ProjectName);
  if SameText(ProjectExtension, SourceExtensionBCBProject) then
    Result := InstallBCBExpert(ProjectName, OutputDir, DcpSearchPath)
  else if SameText(ProjectExtension, SourceExtensionDelphiProject) then
    Result := InstallDelphiExpert(ProjectName, OutputDir, DcpSearchPath)
  else
    raise EJclBorRADException.CreateResFmt(@RsUnknownProjectExtension, [ProjectExtension]);
end;

function TJclBorRADToolInstallation.InstallIDEPackage(const PackageName,
  BPLPath, DCPPath: string): Boolean;
var
  PackageExtension: string;
begin
  PackageExtension := ExtractFileExt(PackageName);
  if SameText(PackageExtension, SourceExtensionBCBPackage) then
    Result := InstallBCBIdePackage(PackageName, BPLPath, DCPPath)
  else if SameText(PackageExtension, SourceExtensionDelphiPackage) then
    Result := InstallDelphiIdePackage(PackageName, BPLPath, DCPPath)
  else
    raise EJclBorRADException.CreateResFmt(@RsUnknownIdePackageExtension, [PackageExtension]);
end;

function TJclBorRADToolInstallation.InstallPackage(const PackageName, BPLPath, DCPPath: string): Boolean;
var
  PackageExtension: string;
begin
  PackageExtension := ExtractFileExt(PackageName);
  if SameText(PackageExtension, SourceExtensionBCBPackage) then
    Result := InstallBCBPackage(PackageName, BPLPath, DCPPath)
  else if SameText(PackageExtension, SourceExtensionDelphiPackage) then
    Result := InstallDelphiPackage(PackageName, BPLPath, DCPPath)
  else
    raise EJclBorRADException.CreateResFmt(@RsUnknownPackageExtension, [PackageExtension]);
end;

{$IFDEF KEEP_DEPRECATED}
function TJclBorRADToolInstallation.IsBDSPersonality: Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := InheritsFrom(TJclBDSInstallation);
  {$ELSE}
  Result := False;
  {$ENDIF MSWINDOWS}
end;
{$ENDIF KEEP_DEPRECATED}

function TJclBorRADToolInstallation.LibFolderName: string;
begin
  Result := PathAddSeparator(RootDir) + PathAddSeparator('lib');
end;

function TJclBorRADToolInstallation.LinkMapFile(
  const BinaryFileName: string): Boolean;
var
  MAPFileName, LinkerBugUnit: string;
  MAPFileSize, JclDebugDataSize: Integer;
begin
  {$IFDEF MSWINDOWS}
  if MapLink then
  begin
    OutputString(Format(RsLinkingMap, [BinaryFileName]));
    MAPFileName := ChangeFileExt(BinaryFileName,'.MAP');
    Result := InsertDebugDataIntoExecutableFile(BinaryFileName, MAPFileName,
                LinkerBugUnit, MAPFileSize, JclDebugDataSize);
    if Result then
    begin
      OutputString(RsLinkMapOk);
      OutputString(Format(RsLinkMapInfo, [LinkerBugUnit, MAPFileSize, JclDebugDataSize]));
      if MapDelete then
        OutputFileDelete(MAPFileName);
    end
    else
      OutputString(RsLinkMapFailed);
  end
  else
    Result := True;
  {$ELSE MSWINDOWS}
  Result := True;
  {$ENDIF MSWINDOWS}
end;

function TJclBorRADToolInstallation.OutputFileDelete(
  const FileName: string): Boolean;
begin
  OutputString(Format(RsDeletingFile, [FileName]));
  Result := FileDelete(FileName);
  if Result then
    OutputString(RsFileDeletionOk)
  else
    OutputString(RsFileDeletionFailed);
end;

procedure TJclBorRADToolInstallation.OutputString(const AText: string);
begin
  if Assigned(FOutputCallback) then
    OutputCallback(AText);
end;

class function TJclBorRADToolInstallation.PackageSourceFileExtension: string;
begin
  {$IFDEF MSWINDOWS}
  raise EAbstractError.CreateResFmt(@SAbstractError, ['']); // BCB doesn't support abstract keyword
  {$ELSE MSWINDOWS}
  Result := '';
  {$ENDIF MSWINDOWS}
end;

class function TJclBorRADToolInstallation.ProjectSourceFileExtension: string;
begin
  {$IFDEF MSWINDOWS}
  raise EAbstractError.CreateResFmt(@SAbstractError, ['']); // BCB doesn't support abstract keyword
  {$ELSE MSWINDOWS}
  Result := '';
  {$ENDIF MSWINDOWS}
end;

class function TJclBorRADToolInstallation.RADToolKind: TJclBorRADToolKind;
begin
  {$IFDEF MSWINDOWS}
  raise EAbstractError.CreateResFmt(@SAbstractError, ['']); // BCB doesn't support abstract keyword
  {$ELSE MSWINDOWS}
  Result := brDelphi;
  {$ENDIF MSWINDOWS}
end;

{class }function TJclBorRADToolInstallation.RADToolName: string;
begin
  {$IFDEF MSWINDOWS}
  raise EAbstractError.CreateResFmt(@SAbstractError, ['']); // BCB doesn't support abstract keyword
  {$ELSE MSWINDOWS}
  Result := '';
  {$ENDIF MSWINDOWS}
end;

procedure TJclBorRADToolInstallation.ReadInformation;
const
  {$IFDEF KYLIX}
  BinDir = 'bin/';
  {$ELSE ~KYLIX}
  BinDir = 'bin\';
  {$ENDIF ~KYLIX}
  UpdateKeyName = 'Update #';
  BDSUpdateKeyName = 'UpdatePackInstalled';
var
  KeyLen, I: Integer;
  Key: string;
  Ed: TJclBorRADToolEdition;
begin
  Key := ConfigData.FileName;
  {$IFDEF KYLIX}
  ConfigData.ReadSectionValues(GlobalsKeyName, Globals);
  {$ELSE ~KYLIX}
  RegGetValueNamesAndValues(HKEY_LOCAL_MACHINE, Key, Globals);

  KeyLen := Length(Key);
  if (KeyLen > 3) and StrIsDigit(Key[KeyLen - 2]) and (Key[KeyLen - 1] = '.') and (Key[KeyLen] = '0') then
    FIDEVersionNumber := Ord(Key[KeyLen - 2]) - Ord('0')
  else
    FIDEVersionNumber := 0;

  FVersionNumber := FIDEVersionNumber;
  {$ENDIF ~KYLIX}

  case RadToolKind of
    brDelphi :
      FVersionNumberStr := Format('d%d', [VersionNumber]);
    brCppBuilder :
      FVersionNumberStr := Format('c%d', [VersionNumber]);
    brBorlandDevStudio :
      if VersionNumber = 1 then
        FVersionNumberStr := 'cs1'
      else
        FVersionNumberStr := Format('d%d', [VersionNumber+6]);  // BDS 2 goes to D8
  end;

  FRootDir := PathRemoveSeparator(Globals.Values[RootDirValueName]);
  FBinFolderName := PathAddSeparator(RootDir) + BinDir;

  FEditionStr := Globals.Values[EditionValueName];
  if FEditionStr = '' then
    FEditionStr := Globals.Values[VersionValueName];
  { TODO : Edition detection for BDS }
  for Ed := Low(Ed) to High(Ed) do
    if StrIPos(BorRADToolEditionIDs[Ed], FEditionStr) = 1 then
      FEdition := Ed;

  if RadToolKind = brBorlandDevStudio then
    FInstalledUpdatePack := StrToIntDef(Globals.Values[BDSUpdateKeyName], 0)
  else
  for I := 0 to Globals.Count - 1 do
  begin
    Key := Globals.Names[I];
    KeyLen := Length(UpdateKeyName);
    if (Pos(UpdateKeyName, Key) = 1) and (Length(Key) > KeyLen) and StrIsDigit(Key[KeyLen + 1]) then
      FInstalledUpdatePack := Max(FInstalledUpdatePack, Integer(Ord(Key[KeyLen + 1]) - 48));
  end;
end;

function TJclBorRADToolInstallation.RegisterExpert(const BinaryFileName,
  Description: string): Boolean;
var
  InternalDescription: string;
begin
  OutputString(Format(RsRegisteringExpert, [BinaryFileName]));

  if Description = '' then
    InternalDescription := PathExtractFileNameNoExt(BinaryFileName)
  else
    InternalDescription := Description;

  Result := IdePackages.AddExpert(BinaryFileName, InternalDescription);
  if Result then
    OutputString(RsRegistrationOk)
  else
    OutputString(RsRegistrationFailed);
end;

function TJclBorRADToolInstallation.RegisterIDEPackage(const BinaryFileName,
  Description: string): Boolean;
var
  InternalDescription: string;
begin
  OutputString(Format(RsRegisteringIdePackage, [BinaryFileName]));

  if Description = '' then
    InternalDescription := PathExtractFileNameNoExt(BinaryFileName)
  else
    InternalDescription := Description;

  Result := IdePackages.AddIDEPackage(BinaryFileName, InternalDescription);
  if Result then
    OutputString(RsRegistrationOk)
  else
    OutputString(RsRegistrationFailed);
end;

function TJclBorRADToolInstallation.RegisterPackage(const BinaryFileName,
  Description: string): Boolean;
var
  InternalDescription: string;
begin
  OutputString(Format(RsRegisteringPackage, [BinaryFileName]));

  if Description = '' then
    InternalDescription := PathExtractFileNameNoExt(BinaryFileName)
  else
    InternalDescription := Description;

  Result := IdePackages.AddPackage(BinaryFileName, InternalDescription);
  if Result then
    OutputString(RsRegistrationOk)
  else
    OutputString(RsRegistrationFailed);
end;

function TJclBorRADToolInstallation.RemoveFromDebugDCUPath(const Path: string): Boolean;
var
  TempDebugDCUPath: TJclBorRADToolPath;
begin
  TempDebugDCUPath := DebugDCUPath;
  Result := RemoveFromPath(TempDebugDCUPath, Path);
  DebugDCUPath := TempDebugDCUPath;
end;

function TJclBorRADToolInstallation.RemoveFromLibrarySearchPath(const Path: string): Boolean;
var
  TempLibraryPath: TJclBorRADToolPath;
begin
  TempLibraryPath := LibrarySearchPath;
  Result := RemoveFromPath(TempLibraryPath, Path);
  LibrarySearchPath := TempLibraryPath;
end;

function TJclBorRADToolInstallation.RemoveFromLibraryBrowsingPath(const Path: string): Boolean;
var
  TempLibraryPath: TJclBorRADToolPath;
begin
  TempLibraryPath := LibraryBrowsingPath;
  Result := RemoveFromPath(TempLibraryPath, Path);
  LibraryBrowsingPath := TempLibraryPath;
end;

function TJclBorRADToolInstallation.RemoveFromPath(var Path: string; const ItemsToRemove: string): Boolean;
var
  PathItems, RemoveItems: TStringList;
  Folder: string;
  I, J: Integer;
begin
  Result := False;
  PathItems := nil;
  RemoveItems := nil;
  try
    PathItems := TStringList.Create;
    RemoveItems := TStringList.Create;
    ExtractPaths(Path, PathItems);
    ExtractPaths(ItemsToRemove, RemoveItems);
    for I := 0 to RemoveItems.Count - 1 do
    begin
      Folder := RemoveItems[I];
      J := FindFolderInPath(Folder, PathItems);
      if J <> -1 then
      begin
        PathItems.Delete(J);
        Result := True;
      end;
    end;
    Path := StringsToStr(PathItems, PathSep, False);
  finally
    PathItems.Free;
    RemoveItems.Free;
  end;
end;

procedure TJclBorRADToolInstallation.SetDebugDCUPath(const Value: TJclBorRADToolPath);
begin
  ConfigData.WriteString(DebuggingKeyName, DebugDCUPathValueName, Value);
end;

procedure TJclBorRADToolInstallation.SetLibrarySearchPath(const Value: TJclBorRADToolPath);
begin
  ConfigData.WriteString(LibraryKeyName, LibrarySearchPathValueName, Value);
end;

procedure TJclBorRADToolInstallation.SetOutputCallback(
  const Value: TTextHandler);
begin
  FOutputCallback := Value;
  //if clAsm in CommandLineTools then
  //  Asm.OutputCallback := Value;
  if clBcc32 in CommandLineTools then
    Bcc32.OutputCallback := Value;
  if clDcc32 in CommandLineTools then
    Dcc32.OutputCallback := Value;
  //if clDccIL in CommandLineTools then
  //  DccIL.OutputCallback := Value;
  if clMake in CommandLineTools then
    Make.OutputCallback := Value;
  if clProj2Mak in CommandLineTools then
    Bpr2Mak.OutputCallback := Value;
end;

procedure TJclBorRADToolInstallation.SetLibraryBrowsingPath(const Value: TJclBorRADToolPath);
begin
  ConfigData.WriteString(LibraryKeyName, LibraryBrowsingPathValueName, Value);
end;

function TJclBorRADToolInstallation.SubstitutePath(const Path: string): string;
var
  I: Integer;
  Name: string;
begin
  Result := Path;
  if Pos('$(', Result) > 0 then
    with EnvironmentVariables do
      for I := 0 to Count - 1 do
      begin
        Name := Names[I];
        Result := StringReplace(Result, Format('$(%s)', [Name]), Values[Name], [rfReplaceAll, rfIgnoreCase]);
      end;
  // remove duplicate path delimiters '\\'
  Result := StringReplace(Result, DirDelimiter + DirDelimiter, DirDelimiter, [rfReplaceAll]);
end;

{$IFDEF KEEP_DEPRECATED}
function TJclBorRADToolInstallation.SupportsBCB: Boolean;
begin
  Result := clBCC32 in CommandLineTools;
end;
{$ENDIF KEEP_DEPRECATED}

function TJclBorRADToolInstallation.SupportsVisualCLX: Boolean;
begin
  {$IFDEF KYLIX}
  Result := True;
  {$ELSE}
  Result := (Edition <> deSTD) and (VersionNumber in [6, 7]) and (RadToolKind <> brBorlandDevStudio);
  {$ENDIF KYLIX}
end;

function TJclBorRADToolInstallation.UninstallBCBExpert(const ProjectName,
  OutputDir: string): Boolean;
var
  BinaryFileName: string;
begin
  OutputString(Format(RsExpertUninstallationStarted, [ProjectName]));

  if not IsBCBProject(ProjectName) then
    raise EJclBorRADException.CreateResFmt(@RsNotABCBProject, [ProjectName]);

  GetBPRFileInfo(ProjectName, BinaryFileName);
  BinaryFileName := PathAddSeparator(OutputDir) + BinaryFileName;

  // important: remove from experts /before/ deleting;
  //            otherwise PathGetLongPathName won't work
  Result := UnregisterExpert(BinaryFileName);

  if Result then
    OutputFileDelete(BinaryFileName);

  OutputString(RsExpertUninstallationFinished);
end;

function TJclBorRADToolInstallation.UninstallBCBIdePackage(const PackageName,
  BPLPath, DCPPath: string): Boolean;
var
  MAPFileName, TDSFileName,
  BPIFileName, LIBFileName, BPLFileName: string;
  BinaryFileName: string;
  RunOnly: Boolean;
begin
  OutputString(Format(RsIdePackageUninstallationStarted, [PackageName]));

  if not IsBCBPackage(PackageName) then
    raise EJclBorRADException.CreateResFmt(@RsNotABCBPackage, [PackageName]);

  GetBPKFileInfo(PackageName, RunOnly, @BinaryFileName);

  BPLFileName := PathAddSeparator(BPLPath) + BinaryFileName;

  // important: remove from IDE packages /before/ deleting;
  //            otherwise PathGetLongPathName won't work
  Result := (RunOnly or UnregisterIdePackage(BPLFileName));

  // Don't delete binaries if removal of design time package failed
  if Result then
  begin
    OutputFileDelete(BPLFileName);

    BPIFileName := PathAddSeparator(DCPPath) + PathExtractFileNameNoExt(PackageName) + CompilerExtensionBPI;
    OutputFileDelete(BPIFileName);

    LIBFileName := ChangeFileExt(BPIFileName, CompilerExtensionLIB);
    OutputFileDelete(LIBFileName);

    MAPFileName := ChangeFileExt(BPLFileName, CompilerExtensionMAP);
    OutputFileDelete(MAPFileName);

    TDSFileName := ChangeFileExt(BPLFileName, CompilerExtensionTDS);
    OutputFileDelete(TDSFileName);
  end;

  OutputString(RsIdePackageUninstallationFinished);
end;

function TJclBorRADToolInstallation.UninstallBCBPackage(const PackageName,
  BPLPath, DCPPath: string): Boolean;
var
  MAPFileName, TDSFileName,
  BPIFileName, LIBFileName, BPLFileName: string;
  BinaryFileName: string;
  RunOnly: Boolean;
begin
  OutputString(Format(RsPackageUninstallationStarted, [PackageName]));
  
  if not IsBCBPackage(PackageName) then
    raise EJclBorRADException.CreateResFmt(@RsNotABCBPackage, [PackageName]);
    
  GetBPKFileInfo(PackageName, RunOnly, @BinaryFileName);

  BPLFileName := PathAddSeparator(BPLPath) + BinaryFileName;

  // important: remove from IDE packages /before/ deleting;
  //            otherwise PathGetLongPathName won't work
  Result := (RunOnly or UnregisterPackage(BPLFileName));

  // Don't delete binaries if removal of design time package failed
  if Result then
  begin
    OutputFileDelete(BPLFileName);
    
    BPIFileName := PathAddSeparator(DCPPath) + PathExtractFileNameNoExt(PackageName) + CompilerExtensionBPI;
    OutputFileDelete(BPIFileName);

    LIBFileName := ChangeFileExt(BPIFileName, CompilerExtensionLIB);
    OutputFileDelete(LIBFileName);

    MAPFileName := ChangeFileExt(BPLFileName, CompilerExtensionMAP);
    OutputFileDelete(MAPFileName);

    TDSFileName := ChangeFileExt(BPLFileName, CompilerExtensionTDS);
    OutputFileDelete(TDSFileName);
  end;

  OutputString(RsPackageUninstallationFinished);
end;

function TJclBorRADToolInstallation.UninstallDelphiExpert(const ProjectName,
  OutputDir: string): Boolean;
var
  BinaryFileName: string;
  BaseName, LibSuffix, BinaryExtension: string;
begin
  OutputString(Format(RsExpertUninstallationStarted, [ProjectName]));

  if not IsDelphiProject(ProjectName) then
    raise EJclBorRADException.CreateResFmt(@RsNotADelphiProject, [ProjectName]);

  BaseName := PathExtractFileNameNoExt(ProjectName);
  GetDPRFileInfo(ProjectName, BinaryExtension, @LibSuffix);
  if BinaryExtension = '' then
    BinaryExtension := BinaryExtensionLibrary;
  BinaryFileName := PathAddSeparator(OutputDir) + BaseName + LibSuffix + BinaryExtension;

  // important: remove from experts /before/ deleting;
  //            otherwise PathGetLongPathName won't work
  Result := UnregisterExpert(BinaryFileName);

  if Result then
    OutputFileDelete(BinaryFileName);

  OutputString(RsExpertUninstallationFinished);
end;

function TJclBorRADToolInstallation.UninstallDelphiIdePackage(const PackageName,
  BPLPath, DCPPath: string): Boolean;
var
  MAPFileName,
  BPLFileName, DCPFileName: string;
  BaseName, LibSuffix: string;
  RunOnly: Boolean;
begin
  OutputString(Format(RsIdePackageUninstallationStarted, [PackageName]));
  
  if not IsDelphiPackage(PackageName) then
    raise EJclBorRADException.CreateResFmt(@RsNotADelphiPackage, [PackageName]);

  GetDPKFileInfo(PackageName, RunOnly, @LibSuffix);
  BaseName := PathExtractFileNameNoExt(PackageName);

  BPLFileName := PathAddSeparator(BPLPath) + BaseName + LibSuffix + BinaryExtensionPackage;

  // important: remove from IDE packages /before/ deleting;
  //            otherwise PathGetLongPathName won't work
  Result := RunOnly or UnregisterIdePackage(BPLFileName);

  // Don't delete binaries if removal of design time package failed
  if Result then
  begin
    OutputFileDelete(BPLFileName);

    DCPFileName := PathAddSeparator(DCPPath) + BaseName + CompilerExtensionDCP;
    OutputFileDelete(DCPFileName);

    MAPFileName := ChangeFileExt(BPLFileName, CompilerExtensionMAP);
    OutputFileDelete(MAPFileName);
  end;

  OutputString(RsIdePackageUninstallationFinished);
end;

function TJclBorRADToolInstallation.UninstallDelphiPackage(const PackageName,
  BPLPath, DCPPath: string): Boolean;
var
  MAPFileName,
  BPLFileName, DCPFileName: string;
  BaseName, LibSuffix: string;
  RunOnly: Boolean;
begin
  OutputString(Format(RsPackageUninstallationStarted, [PackageName]));
  
  if not IsDelphiPackage(PackageName) then
    raise EJclBorRADException.CreateResFmt(@RsNotADelphiPackage, [PackageName]);

  GetDPKFileInfo(PackageName, RunOnly, @LibSuffix);
  BaseName := PathExtractFileNameNoExt(PackageName);

  BPLFileName := PathAddSeparator(BPLPath) + BaseName + LibSuffix + BinaryExtensionPackage;

  // important: remove from IDE packages /before/ deleting;
  //            otherwise PathGetLongPathName won't work
  Result := RunOnly or UnregisterPackage(BPLFileName);

  // Don't delete binaries if removal of design time package failed
  if Result then
  begin
    OutputFileDelete(BPLFileName);

    DCPFileName := PathAddSeparator(DCPPath) + BaseName + CompilerExtensionDCP;
    OutputFileDelete(DCPFileName);

    MAPFileName := ChangeFileExt(BPLFileName, CompilerExtensionMAP);
    OutputFileDelete(MAPFileName);
  end;

  OutputString(RsPackageUninstallationFinished);
end;

function TJclBorRADToolInstallation.UninstallExpert(const ProjectName,
  OutputDir: string): Boolean;
var
  ProjectExtension: string;
begin
  ProjectExtension := ExtractFileExt(ProjectName);
  if SameText(ProjectExtension,SourceExtensionBCBProject) then
    Result := UninstallBCBExpert(ProjectName, OutputDir)
  else if SameText(ProjectExtension, SourceExtensionDelphiProject) then
    Result := UninstallDelphiExpert(ProjectName, OutputDir)
  else
    raise EJclBorRadException.CreateResFmt(@RsUnknownProjectExtension, [ProjectExtension]);
end;

function TJclBorRADToolInstallation.UninstallIDEPackage(const PackageName,
  BPLPath, DCPPath: string): Boolean;
var
  PackageExtension: string;
begin
  PackageExtension := ExtractFileExt(PackageName);
  if SameText(PackageExtension,SourceExtensionBCBPackage) then
    Result := UninstallBCBIdePackage(PackageName, BPLPath, DCPPath)
  else if SameText(PackageExtension, SourceExtensionDelphiPackage) then
    Result := UninstallDelphiIdePackage(PackageName, BPLPath, DCPPath)
  else
    raise EJclBorRadException.CreateResFmt(@RsUnknownIdePackageExtension, [PackageExtension]);
end;

function TJclBorRADToolInstallation.UninstallPackage(const PackageName, BPLPath, DCPPath: string): Boolean;
var
  PackageExtension: string;
begin
  PackageExtension := ExtractFileExt(PackageName);
  if SameText(PackageExtension,SourceExtensionBCBPackage) then
    Result := UninstallBCBPackage(PackageName, BPLPath, DCPPath)
  else if SameText(PackageExtension, SourceExtensionDelphiPackage) then
    Result := UninstallDelphiPackage(PackageName, BPLPath, DCPPath)
  else
    raise EJclBorRadException.CreateResFmt(@RsUnknownPackageExtension, [PackageExtension]);
end;

function TJclBorRADToolInstallation.UnregisterExpert(
  const BinaryFileName: string): Boolean;
begin
  OutputString(Format(RsUnregisteringExpert, [BinaryFileName]));

  Result := IdePackages.RemoveExpert(BinaryFileName);
  if Result then
    OutputString(RsUnregistrationOk)
  else
    OutputString(RsUnregistrationFailed);
end;

function TJclBorRADToolInstallation.UnregisterIDEPackage(
  const BinaryFileName: string): Boolean;
begin
  OutputString(Format(RsUnregisteringIDEPackage, [BinaryFileName]));

  Result := IdePackages.RemoveIDEPackage(BinaryFileName);
  if Result then
    OutputString(RsUnregistrationOk)
  else
    OutputString(RsUnregistrationFailed);
end;

function TJclBorRADToolInstallation.UnregisterPackage(
  const BinaryFileName: string): Boolean;
begin
  OutputString(Format(RsUnregisteringPackage, [BinaryFileName]));

  Result := IdePackages.RemovePackage(BinaryFileName);
  if Result then
    OutputString(RsUnregistrationOk)
  else
    OutputString(RsUnregistrationFailed);
end;

//=== { TJclBCBInstallation } ================================================

constructor TJclBCBInstallation.Create(const AConfigDataLocation: string);
begin
  inherited Create(AConfigDataLocation);
  FPersonalities := [bpBCBuilder32];
  if clDcc32 in CommandLineTools then
    Include(FPersonalities, bpDelphi32);
end;

destructor TJclBCBInstallation.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF KYLIX}
function TJclBCBInstallation.ConfigFileName(const Extension: string): string;
begin
  Result := Format('%s/.borland/bcb%d%s', [GetPersonalFolder, IDs[VersionNumber], Extension]);
end;
{$ENDIF KYLIX}

function TJclBCBInstallation.GetEnvironmentVariables: TStrings;
begin
  Result := inherited GetEnvironmentVariables;
  if Assigned(Result) then
    Result.Values['BCB'] := PathRemoveSeparator(RootDir);
end;

class function TJclBCBInstallation.GetLatestUpdatePackForVersion(Version: Integer): Integer;
begin
  case Version of
    5: Result := 0;
    6: Result := 4;
    10: Result := 0;
  else
    Result := 0;
  end;
end;

class function TJclBCBInstallation.PackageSourceFileExtension: string;
begin
  Result := SourceExtensionBCBPackage;
end;

class function TJclBCBInstallation.ProjectSourceFileExtension: string;
begin
  Result := SourceExtensionBCBProject;
end;

{class }class function TJclBCBInstallation.RadToolKind: TJclBorRadToolKind;
begin
  Result := brCppBuilder;
end;

function TJclBCBInstallation.RADToolName: string;
begin
  Result := RsBCBName;
end;

//=== { TJclDelphiInstallation } =============================================

{$IFDEF KYLIX}
function TJclDelphiInstallation.ConfigFileName(const Extension: string): string;
begin
  Result := Format('%s/.borland/delphi%d%s', [GetPersonalFolder, IDs[VersionNumber], Extension]);
end;
{$ENDIF KYLIX}

constructor TJclDelphiInstallation.Create(
  const AConfigDataLocation: string);
begin
  inherited Create(AConfigDataLocation);
  FPersonalities := [bpDelphi32];
end;

destructor TJclDelphiInstallation.Destroy;
begin
  inherited Destroy;
end;

function TJclDelphiInstallation.GetEnvironmentVariables: TStrings;
begin
  Result := inherited GetEnvironmentVariables;
  if Assigned(Result) then
    Result.Values['DELPHI'] := PathRemoveSeparator(RootDir);
end;

class function TJclDelphiInstallation.GetLatestUpdatePackForVersion(Version: Integer): Integer;
begin
  case Version of
    5: Result := 1;
    6: Result := 2;
    7: Result := 0;
  else
    Result := 0;
  end;
end;

function TJclDelphiInstallation.InstallPackage(const PackageName, BPLPath,
  DCPPath: string): Boolean;
begin
  Result := InstallDelphiPackage(PackageName, BPLPath, DCPPath);
end;

class function TJclDelphiInstallation.PackageSourceFileExtension: string;
begin
  Result := SourceExtensionDelphiPackage;
end;

class function TJclDelphiInstallation.ProjectSourceFileExtension: string;
begin
  Result := SourceExtensionDelphiProject;
end;

{class }class function TJclDelphiInstallation.RadToolKind: TJclBorRadToolKind;
begin
  Result := brDelphi;
end;

function TJclDelphiInstallation.RADToolName: string;
begin
  Result := RsDelphiName;
end;

//=== { TJclBDSInstallation } ==================================================

{$IFDEF MSWINDOWS}

function TJclBDSInstallation.AddToCppBrowsingPath(
  const Path: string): Boolean;
var
  TempCppPath: TJclBorRADToolPath;
begin
  if bpBCBuilder32 in Personalities then
  begin
    TempCppPath := CppBrowsingPath;
    PathListIncludeItems(TempCppPath, Path);
    Result := True;
    CppBrowsingPath := TempCppPath;
  end
  else
    Result := False;
end;

function TJclBDSInstallation.AddToCppSearchPath(
  const Path: string): Boolean;
var
  TempCppPath: TJclBorRADToolPath;
begin
  if bpBCBuilder32 in Personalities then
  begin
    TempCppPath := CppSearchPath;
    PathListIncludeItems(TempCppPath, Path);
    Result := True;
    CppSearchPath := TempCppPath;
  end
  else
    Result := False;
end;

function TJclBDSInstallation.CleanPackageCache(
  const BinaryFileName: string): Boolean;
var
  FileName: string;
begin
  Result := True;

  if VersionNumber >= 3 then
  begin
    FileName := ExtractFileName(BinaryFileName);

    try
      OutputString(Format(RsCleaningPackageCache, [FileName]));

      Result :=  RegDeleteKeyTree(HKCU, PathAddSeparator(ConfigDataLocation)
        + PackageCacheKeyName + '\' + FileName);

      if Result then
        OutputString(RsCleaningOk)
      else
        OutputString(RsCleaningFailed);
    except
      // trap possible exceptions
    end;
  end;
end;

function TJclBDSInstallation.CompileDelphiPackage(const PackageName, BPLPath,
  DCPPath, ExtraOptions: string): Boolean;
var
  NewOptions: string;
begin
  if DualPackageInstallation then
  begin
    if not (bpBCBuilder32 in Personalities) then
      raise EJclBorRadException.CreateResFmt(@RsDualPackageNotSupported, [Name]);

    NewOptions := Format('%s -JL -NB"%s" -NO"%s" -N1"%s"',
      [ExtraOptions, DcpPath, DcpPath, VclIncludeDir]);
  end
  else
    NewOptions := ExtraOptions;

  Result := inherited CompileDelphiPackage(PackageName, BPLPath, DCPPath, NewOptions);
end;

function TJclBDSInstallation.CompileDelphiProject(const ProjectName,
  OutputDir, DcpSearchPath: string): Boolean;
var
  ExtraOptions, BinaryExtension, LibSuffix, BinaryFileName: string;
begin
  if VersionNumber <= 2 then
  begin
    OutputString(Format(RsCompilingProject, [ProjectName]));

    if not IsDelphiProject(ProjectName) then
      raise EJclBorRADException.CreateResFmt(@RsNotADelphiProject, [ProjectName]);

    if MapCreate then
      ExtraOptions := '-GD'
    else
      ExtraOptions := '';

    GetDPRFileInfo(ProjectName, BinaryExtension, @LibSuffix);
    if BinaryExtension = '' then
      BinaryExtension := BinaryExtensionLibrary;
    BinaryFileName := PathAddSeparator(OutputDir) + PathExtractFileNameNoExt(ProjectName) + LibSuffix + BinaryExtension;

    Result := DCC32.MakeProject(ProjectName, OutputDir, DcpSearchPath, ExtraOptions)
      and LinkMapFile(BinaryFileName);

    if Result then
      OutputString(RsCompilationOk)
    else
      OutputString(RsCompilationFailed);
  end
  else
    Result := inherited CompileDelphiProject(ProjectName, DcpSearchPath, OutputDir);
end;

constructor TJclBDSInstallation.Create(const AConfigDataLocation: string);
const
  PersonalitiesSection = 'Personalities';
begin
  inherited Create(AConfigDataLocation);
  FHelp2Manager := TJclHelp2Manager.Create(Self);

  { TODO : .net 64 bit }
  if ConfigData.ReadString(PersonalitiesSection, 'C#Builder', '') <> '' then
    Include(FPersonalities, bpCSBuilder32);
  if ConfigData.ReadString(PersonalitiesSection, 'BCB', '') <> '' then
    Include(FPersonalities, bpBCBuilder32);
  if ConfigData.ReadString(PersonalitiesSection, 'Delphi.Win32', '') <> '' then
    Include(FPersonalities, bpDelphi32);
  if   (ConfigData.ReadString(PersonalitiesSection, 'Delphi.NET', '') <> '')
    or (ConfigData.ReadString(PersonalitiesSection, 'Delphi8', '') <> '') then
    Include(FPersonalities, bpDelphiNet32);

  if clDcc32 in CommandLineTools then
    Include(FPersonalities, bpDelphi32);

  if FPersonalities = [] then
    raise EJclBorRadException.CreateRes(@RsNoSupportedPersonality);
end;

destructor TJclBDSInstallation.Destroy;
begin
  FHelp2Manager.Free;
  inherited Destroy;
end;

{ TODO -cHelp : Donator: Adreas Hausladen }
function TJclBDSInstallation.GetBorlandStudioProjectsDir: string;
var
  h: HMODULE;
  LocaleName: array[0..4] of Char;
  Filename: string;
begin
  Result := 'Borland Studio Projects'; // do not localize

  FillChar(LocaleName, SizeOf(LocaleName[0]), 0);
  GetLocaleInfo(GetThreadLocale, LOCALE_SABBREVLANGNAME, LocaleName, SizeOf(LocaleName));
  if LocaleName[0] <> #0 then
  begin
    Filename := RootDir + '\Bin\coreide' + BDSVersions[IDEVersionNumber].CoreIdeVersion + '.';
    if FileExists(Filename + LocaleName) then
      Filename := Filename + LocaleName
    else
    begin
      LocaleName[2] := #0;
      if FileExists(Filename + LocaleName) then
        Filename := Filename + LocaleName
      else
        Filename := '';
    end;

    if Filename <> '' then
    begin
      h := LoadLibraryEx(PChar(Filename), 0,
        LOAD_LIBRARY_AS_DATAFILE or DONT_RESOLVE_DLL_REFERENCES);
      if h <> 0 then
      begin
        SetLength(Result, 1024);
        SetLength(Result, LoadString(h, BDSVersions[IDEVersionNumber].ProjectsDirResId, PChar(Result), Length(Result) - 1));
        FreeLibrary(h);
      end;
    end;
  end;

  Result := PathAddSeparator(GetPersonalFolder) + Result;
end;

function TJclBDSInstallation.GetBPLOutputPath: string;
begin
// BDS 1 (C#Builder 1) and BDS 2 (Delphi 8) don't have a valid BPL output path
// set in the registry
  if VersionNumber <= 2 then
    Result := PathAddSeparator(GetDefaultProjectsDir) + 'bpl'
  else
    Result := inherited GetBPLOutputPath;
end;

function TJclBDSInstallation.GetCppBrowsingPath: TJclBorRADToolPath;
begin
  Result := ConfigData.ReadString(CppPathsKeyName, CppBrowsingPathValueName, '');
end;

function TJclBDSInstallation.GetCppSearchPath: TJclBorRADToolPath;
begin
  Result := ConfigData.ReadString(CppPathsKeyName, CppSearchPathValueName, '');
end;

function TJclBDSInstallation.GetDCPOutputPath: string;
begin
  if VersionNumber <= 2 then
    Result := PathAddSeparator(RootDir) + 'lib'
  else
    Result := inherited GetDCPOutputPath;
end;

function TJclBDSInstallation.GetDefaultProjectsDir: string;
begin
  Result := GetBorlandStudioProjectsDir;
end;

function TJclBDSInstallation.GetEnvironmentVariables: TStrings;
begin
  Result := inherited GetEnvironmentVariables;
  if Assigned(Result) then
  begin
    // adding default values
    if Result.Values[EnvVariableBDSValueName] = '' then
      Result.Values[EnvVariableBDSValueName] := PathRemoveSeparator(RootDir);
    if Result.Values[EnvVariableBDSPROJDIRValueName] = '' then
      Result.Values[EnvVariableBDSPROJDIRValueName] := DefaultProjectsDir;
  end;
end;

class function TJclBDSInstallation.GetLatestUpdatePackForVersion(
  Version: Integer): Integer;
begin
  case Version of
    9: Result := 1;   // personal version is only update pack 1
    10: Result := 1;  // update 1 is out
  else
    Result := 0;
  end;
end;

function TJclBDSInstallation.GetName: string;
begin
  if VersionNumber in [Low(BDSVersions)..High(BDSVersions)] then
    Result := Format('%s %s', [RadToolName, BDSVersions[VersionNumber].VersionStr])
  else
    Result := Format('%s ***%s***', [RadToolName, VersionNumber]);
end;

function TJclBDSInstallation.GetVclIncludeDir: string;
begin
  if not (bpBCBuilder32 in Personalities) then
    raise EJclBorRadException.CreateResFmt(@RsDualPackageNotSupported, [Name]);
  Result := inherited GetVclIncludeDir;
end;

class function TJclBDSInstallation.PackageSourceFileExtension: string;
begin
  Result := SourceExtensionDelphiPackage;
end;

class function TJclBDSInstallation.ProjectSourceFileExtension: string;
begin
  Result := SourceExtensionDelphiProject;
end;

class function TJclBDSInstallation.RadToolKind: TJclBorRadToolKind;
begin
  Result := brBorlandDevStudio;
end;

function TJclBDSInstallation.RadToolName: string;
begin
  if VersionNumber in [Low(BDSVersions)..High(BDSVersions)] then
    Result := BDSVersions[VersionNumber].Name
  else
    Result := RsBDSName;
end;

function TJclBDSInstallation.RegisterPackage(const BinaryFileName,
  Description: string): Boolean;
begin
  if VersionNumber >= 3 then
    CleanPackageCache(BinaryFileName);
  
  Result := inherited RegisterPackage(BinaryFileName, Description);
end;

function TJclBDSInstallation.RemoveFromCppBrowsingPath(
  const Path: string): Boolean;
var
  TempCppPath: TJclBorRADToolPath;
begin
  if bpBCBuilder32 in Personalities then
  begin
    TempCppPath := CppBrowsingPath;
    Result := RemoveFromPath(TempCppPath, Path);
    CppBrowsingPath := TempCppPath;
  end
  else
    Result := False;
end;

function TJclBDSInstallation.RemoveFromCppSearchPath(
  const Path: string): Boolean;
var
  TempCppPath: TJclBorRADToolPath;
begin
  if bpBCBuilder32 in Personalities then
  begin
    TempCppPath := CppSearchPath;
    Result := RemoveFromPath(TempCppPath, Path);
    CppSearchPath := TempCppPath;
  end
  else
    Result := False;
end;

procedure TJclBDSInstallation.SetCppBrowsingPath(
  const Value: TJclBorRADToolPath);
begin
  ConfigData.WriteString(CppPathsKeyName, CppBrowsingPathValueName, Value);
end;

procedure TJclBDSInstallation.SetCppSearchPath(const Value: TJclBorRADToolPath);
begin
  ConfigData.WriteString(CppPathsKeyName, CppSearchPathValueName, Value);
end;

procedure TJclBDSInstallation.SetDualPackageInstallation(const Value: Boolean);
begin
  if Value and not (bpBCBuilder32 in Personalities) then
    raise EJclBorRadException.CreateResFmt(@RsDualPackageNotSupported, [Name]);
  FDualPackageInstallation := Value;
end;

function TJclBDSInstallation.UnregisterPackage(
  const BinaryFileName: string): Boolean;
begin
  if VersionNumber >= 3 then
    CleanPackageCache(BinaryFileName);

  Result := inherited UnregisterPackage(BinaryFileName);
end;

{$ENDIF MSWINDOWS}

//=== { TJclBorRADToolInstallations } ========================================

constructor TJclBorRADToolInstallations.Create;
begin
  FList := TObjectList.Create;
  ReadInstallations;
end;

destructor TJclBorRADToolInstallations.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TJclBorRADToolInstallations.AnyInstanceRunning: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if Installations[I].AnyInstanceRunning then
    begin
      Result := True;
      Break;
    end;
end;

function TJclBorRADToolInstallations.AnyUpdatePackNeeded(var Text: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if Installations[I].UpdateNeeded then
    begin
      Result := True;
      Text := Format(RsNeedUpdate, [Installations[I].LatestUpdatePack, Installations[I].Name]);
      Break;
    end;
end;

function TJclBorRADToolInstallations.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TJclBorRADToolInstallations.GetBCBInstallationFromVersion(VersionNumber: Integer): TJclBorRADToolInstallation;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  case Installations[I].RadToolKind of
    brCppBuilder:
      if Installations[I].VersionNumber = VersionNumber then
      begin
        Result := Installations[I];
        Break;
      end;
    brBorlandDevStudio:
      if (VersionNumber >= 10) and (Installations[I].VersionNumber = (VersionNumber-6)) then
      begin
        Result := Installations[I];
        Break;
      end;
  end;
end;

function TJclBorRADToolInstallations.GetDelphiInstallationFromVersion(VersionNumber: Integer): TJclBorRADToolInstallation;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    case Installations[I].RadToolKind of
      brDelphi:
        if Installations[I].VersionNumber = VersionNumber then
        begin
          Result := Installations[I];
          Break;
        end;
      brBorlandDevStudio:
        if (VersionNumber >= 8) and (Installations[I].VersionNumber = (VersionNumber-6)) then
        begin
          Result := Installations[I];
          Break;
        end;
    end;
end;

function TJclBorRADToolInstallations.GetInstallations(Index: Integer): TJclBorRADToolInstallation;
begin
  Result := TJclBorRADToolInstallation(FList[Index]);
end;

function TJclBorRADToolInstallations.GetBCBVersionInstalled(VersionNumber: Integer): Boolean;
begin
  Result := BCBInstallationFromVersion[VersionNumber] <> nil;
end;

function TJclBorRADToolInstallations.GetBDSInstallationFromVersion(
  VersionNumber: Integer): TJclBorRADToolInstallation;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if (Installations[I].VersionNumber = VersionNumber)
      and (Installations[I].RadToolKind = brBorlandDevStudio) then
    begin
      Result := Installations[I];
      Break;
    end;
end;

function TJclBorRADToolInstallations.GetBDSVersionInstalled(
  VersionNumber: Integer): Boolean;
begin
  Result := BDSInstallationFromVersion[VersionNumber] <> nil;
end;

function TJclBorRADToolInstallations.GetDelphiVersionInstalled(VersionNumber: Integer): Boolean;
begin
  Result := DelphiInstallationFromVersion[VersionNumber] <> nil;
end;

function TJclBorRADToolInstallations.Iterate(TraverseMethod: TTraverseMethod): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Count - 1 do
    Result := Result and TraverseMethod(Installations[I]);
end;

procedure TJclBorRADToolInstallations.ReadInstallations;
{$IFDEF KYLIX}
var
  I: Integer;

  procedure CheckForInstallation(RADToolKind: TJclBorRADToolKind; VersionNumber: Integer);
  const
    RcBaseFileNames: array [brDelphi..brCppBuilder] of string = ('delphi', 'bcb');
  var
    Item: TJclBorRADToolInstallation;
    RcFileName: string;
  begin
    RcFileName := Format('%s/.borland/%s%drc', [GetPersonalFolder, RcBaseFileNames[RADToolKind], IDs[VersionNumber]]);
    if FileExists(RcFileName) then
    begin
      if RADToolKind = brCppBuilder then
        Item := TJclBCBInstallation.Create(RcFileName)
      else
        Item := TJclDelphiInstallation.Create(RcFileName);
      Item.FVersionNumber := VersionNumber;
      FList.Add(Item);
    end;
  end;

begin
  FList.Clear;
  for I := Low(TKylixVersion) to High(TKylixVersion) do
    CheckForInstallation(brDelphi, I);
  CheckForInstallation(brCppBuilder, 3); // Kylix 3 only
end;
{$ELSE ~KYLIX}
var
  VersionNumbers: TStringList;

  function EnumVersions(const KeyName: string; const Personalities: array of string; CreateClass: TJclBorRADToolInstallationClass) : Boolean;
  var
    I, J: Integer;
    VersionKeyName: string;
    PersonalitiesList: TStrings;
  begin
    Result := False;
    if RegKeyExists(HKEY_LOCAL_MACHINE, KeyName) and
      RegGetKeyNames(HKEY_LOCAL_MACHINE, KeyName, VersionNumbers) then
      for I := 0 to VersionNumbers.Count - 1 do
        if StrIsSubSet(VersionNumbers[I], ['.', '0'..'9']) then
      begin
        VersionKeyName := KeyName + DirDelimiter + VersionNumbers[I];
        if RegKeyExists(HKEY_LOCAL_MACHINE, VersionKeyName) then
        begin
          if Length(Personalities) = 0 then
          begin
            try
              FList.Add(CreateClass.Create(VersionKeyName));
            finally
              Result := True;
            end;
          end
          else
          begin
            PersonalitiesList := TStringList.Create;
            try
              RegGetValueNames(HKEY_LOCAL_MACHINE, VersionKeyName + '\Personalities', PersonalitiesList);
              for J := Low(Personalities) to High(Personalities) do
                if PersonalitiesList.IndexOf(Personalities[J]) >= 0 then
              begin
                try
                  FList.Add(CreateClass.Create(VersionKeyName));
                finally
                  Result := True;
                end;
                Break;
              end;
            finally
              PersonalitiesList.Free;
            end;
          end;
        end;
      end;
  end;

begin
  FList.Clear;
  VersionNumbers := TStringList.Create;
  try
    EnumVersions(DelphiKeyName, [], TJclDelphiInstallation);
    EnumVersions(BCBKeyName, [], TJclBCBInstallation);
    EnumVersions(BDSKeyName, ['Delphi.Win32','BCB','Delphi8','C#Builder'], TJclBDSInstallation);
  finally
    VersionNumbers.Free;
  end;
end;
{$ENDIF ~KYLIX}

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
  S := Format('-%s"%s"', [Option, S]);
  // avoid duplicate entries (note that search is case sensitive)
  if GetOptions.IndexOf(S) = -1 then
    GetOptions.Add(S);
end;

function TJclCommandLineTool.Execute(const CommandLine: string): Boolean;
begin
  if Assigned(FOutputCallback) then
    Result := JclSysUtils.Execute(Format('"%s" %s', [ExeName, CommandLine]), FOutputCallback) = 0
  else
    Result := JclSysUtils.Execute(Format('"%s" %s', [ExeName, CommandLine]), FOutput) = 0;
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

// History:

// $Log$
// Revision 1.60  2006/03/26 20:13:13  outchy
// Fixed BCB repository name
//
// Revision 1.59  2006/03/23 21:30:01  outchy
// Help 2.0 code moved to runtime units
// Fixed compilation of TLB files for BCB
//
// Revision 1.58  2006/03/13 22:15:00  outchy
// PathSeparator renamed to DirDelimiter
// Installer checks paths
//
// Revision 1.57  2006/03/04 21:22:10  outchy
// Jcl directories added to the C++ side of BDS 2006
//
// Revision 1.56  2006/02/26 18:31:43  outchy
// Chm help can now be removed
// Alpha version for the help 2.0
//
// Revision 1.55  2006/02/08 19:45:58  outchy
// Command line is now added to output
//
// Revision 1.54  2006/02/05 13:26:15  outchy
// dcp, bpi and lib files are created in \lib\ver
//
// Revision 1.53  2006/02/02 20:33:40  outchy
// Package cache cleaned
//
// Revision 1.52  2005/12/26 20:18:02  uschuster
// fixed BDS Update Pack detection
//
// Revision 1.51  2005/12/26 20:02:09  outchy
// IT3363: overriden environment variables
//
// Revision 1.50  2005/12/26 18:03:51  outchy
// Enhanced bds support (including C#1 and D8)
// Introduction of dll experts
// Project types in templates
//
// Revision 1.48  2005/11/13 17:04:20  uschuster
// fix for Kylix
//
// Revision 1.47  2005/11/10 22:16:31  outchy
// Added creation/link/deletion of MAP files for packages.
//
// Revision 1.46  2005/10/28 04:38:53  rrossmair
// - fixes related to package uninstallation, and more
//
// Revision 1.45  2005/10/04 04:22:48  rrossmair
// - saved local function TJclDCC.SaveOptionsToFile.IsPathOption
//
// Revision 1.44  2005/08/07 13:22:09  outchy
// IT3116: Added REG_EXPAND_SZ and REG_BINARY to the list of valid keys.
//
// Revision 1.43  2005/08/06 11:33:50  rrossmair
// - TJclBorRADToolInstallations.ReadInstallations: fixed processing of HK*\Software\Borland\BDS\* registry keys
//
// Revision 1.42  2005/07/28 21:57:49  outchy
// JEDI Installer can now install design-time packages for C++Builder 5 and 6
//
// Revision 1.41  2005/03/22 03:36:09  rrossmair
// - fixed PathGetShortName usage for packages
// - TJclDCC.SetDefaultOptions extended for BCB
//
// Revision 1.40  2005/03/21 04:24:34  rrossmair
// - identifier mistake fixed (Kylix)
//
// Revision 1.39  2005/03/21 04:05:31  rrossmair
// - workarounds for DCC32 126 character path limit
//
// Revision 1.38  2005/03/14 04:03:21  rrossmair
// - fixed TJclBorRADToolIdePackages.RemovePackage
//
// Revision 1.37  2005/03/08 08:33:15  marquardt
// overhaul of exceptions and resourcestrings, minor style cleaning
//
// Revision 1.36  2005/02/27 07:27:47  marquardt
// changed interface names from I to IJcl, moved resourcestrings to JclResource.pas
//
// Revision 1.35  2005/02/24 16:34:39  marquardt
// remove divider lines, add section lines (unfinished)
//
// Revision 1.34  2005/02/23 07:53:13  rrossmair
// - added TJclDCC.SetDefaultOptions, which includes the path(s) normally found in $(DELPHI)\bin\dcc32.cfg.
// - AddPathOption() methods enhanced.
//
// Revision 1.33  2005/02/04 05:11:21  rrossmair
// - fixed TJclBorRADToolInstallation.UninstallPackage
//
// Revision 1.32  2005/02/04 04:49:08  rrossmair
// - fixed GetDPKFileInfo
// - more uninstall support
//
// Revision 1.31  2005/02/03 05:17:54  rrossmair
// - some uninstall support added
// - refactoring: TJclDCC.InstallPackage replaced by TJclDCC.MakelPackage, IDE installation part moved to TJclBorRADToolInstallation.InstallPackage
//
// Revision 1.30  2004/12/23 04:31:42  rrossmair
// - check-in for JCL 1.94 RC 1
//
// Revision 1.29  2004/12/20 05:15:48  rrossmair
// - fixed for Kylix ($IFDEFed GetBorlandStudioProjectsDir)
//
// Revision 1.28  2004/12/18 04:03:30  rrossmair
// - more D2005 support
//
// Revision 1.27  2004/12/16 19:56:58  rrossmair
// - fixed for Windows
//
// Revision 1.26  2004/12/15 22:54:04  rrossmair
// - fixed for Kylix
//
// Revision 1.25  2004/12/15 21:46:40  rrossmair
// - D2005 support (incomplete)
//
// Revision 1.24  2004/11/18 00:57:14  rrossmair
// - check-in for release 1.93
//
// Revision 1.23  2004/11/16 06:17:27  marquardt
// style cleaning
//
// Revision 1.22  2004/11/15 20:42:35  rrossmair
// - TJclBorRADToolInstallation.SubstitutePath: remove duplicate path delimiters
//
// Revision 1.21  2004/11/09 07:51:37  rrossmair
// - installer refactoring (incomplete)
//
// Revision 1.20  2004/10/25 06:58:44  rrossmair
// - fixed bug #0002065
// - outsourced JclMiscel.Win32ExecAndRedirectOutput() + JclBorlandTools.ExecAndRedirectOutput() code into JclSysUtils.Execute()
// - refactored this code
// - added overload to supply callback capability per line of output
//
// Revision 1.19  2004/10/17 05:23:06  rrossmair
// replaced PathGetLongName2() by PathGetLongName()
//
// Revision 1.18  2004/08/09 06:38:08  marquardt
// add JvWStrUtils.pas as JclWideStrings.pas
//
// Revision 1.17  2004/08/01 05:52:10  marquardt
// move constructors/destructors
//
// Revision 1.16  2004/07/30 07:20:24  marquardt
// fixing TStringLists, adding BeginUpdate/EndUpdate
//
// Revision 1.15  2004/07/28 18:00:48  marquardt
// various style cleanings, some minor fixes
//
// Revision 1.14  2004/07/14 03:36:20  rrossmair
// fixed bug #1897 ( TJclBorRADToolInstallation.GetEnvironmentVariables failure)
//
// Revision 1.13  2004/06/16 07:30:26  marquardt
// added tilde to all IFNDEF ENDIFs, inherited qualified
//
// Revision 1.12  2004/06/14 06:24:52  marquardt
// style cleaning IFDEF
//
// Revision 1.11  2004/05/13 16:38:45  rrossmair
// fixed for paths w/ spaces
//
// Revision 1.10  2004/05/11 11:55:43  rrossmair
// added TJclBCBInstallation.VclIncludeDir
//
// Revision 1.9  2004/05/08 08:44:17  rrossmair
// introduced & applied symbol HAS_UNIT_LIBC
//
// Revision 1.8  2004/05/05 00:04:10  mthoma
// Updated headers: Added donors as contributors, adjusted the initial authors, added cvs names when they were not obvious. Changed $data to $date where necessary,
//
// Revision 1.7  2004/04/18 05:15:07  rrossmair
// code clean-up
//

end.

