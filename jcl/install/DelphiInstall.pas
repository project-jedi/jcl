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
{ The Original Code is DelphiInstall.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributor(s): Robert Rossmair (crossplatform support)                                          }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Routines for getting information about installed versions of Delphi and performing basic         }
{ installation tasks.                                                                              }
{                                                                                                  }
{ Unit owner: Petr Vones                                                                           }
{ Last modified: February 16, 2004                                                                 }
{                                                                                                  }
{**************************************************************************************************}

unit DelphiInstall;

{$I jcl.inc}

{$IFNDEF KYLIX}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils, IniFiles, Contnrs,
  JclBase;

const

//--------------------------------------------------------------------------------------------------
// Various definitions
//--------------------------------------------------------------------------------------------------

  // Object Repository
  DelphiRepositoryPagesSection    = 'Repository Pages';

  DelphiRepositoryDialogsPage     = 'Dialogs';
  DelphiRepositoryFormsPage       = 'Forms';
  DelphiRepositoryProjectsPage    = 'Projects';
  DelphiRepositoryDataModulesPage = 'Data Modules';

  DelphiRepositoryObjectType      = 'Type';
  DelphiRepositoryFormTemplate    = 'FormTemplate';
  DelphiRepositoryProjectTemplate = 'ProjectTemplate';
  DelphiRepositoryObjectName      = 'Name';
  DelphiRepositoryObjectPage      = 'Page';
  DelphiRepositoryObjectIcon      = 'Icon';
  DelphiRepositoryObjectDescr     = 'Description';
  DelphiRepositoryObjectAuthor    = 'Author';
  DelphiRepositoryObjectAncestor  = 'Ancestor';
  DelphiRepositoryObjectDesigner  = 'Designer'; // Delphi 6+ only
  DelphiRepositoryDesignerDfm     = 'dfm';
  DelphiRepositoryDesignerXfm     = 'xfm';
  DelphiRepositoryObjectNewForm   = 'DefaultNewForm';
  DelphiRepositoryObjectMainForm  = 'DefaultMainForm';

//--------------------------------------------------------------------------------------------------
// Installed versions information classes
//--------------------------------------------------------------------------------------------------

type
  {$IFDEF KYLIX}
  TJclDelphiEdition = (deOPEN, dePRO, deSVR);
  {$ELSE}
  TJclDelphiEdition = (deSTD, dePRO, deCSS);
  {$ENDIF}
  TJclDelphiPath = string;

  TJclDelphiInstallation = class;

  TJclDelphiInstallationObject = class (TObject)
  private
    FInstallation: TJclDelphiInstallation;
  protected
    constructor Create(AInstallation: TJclDelphiInstallation);
  public
    property Installation: TJclDelphiInstallation read FInstallation;
  end;

  {$IFDEF MSWINDOWS}
  TJclDelphiOpenHelp = class (TJclDelphiInstallationObject)
  private
    function GetContentFileName: string;
    function GetIndexFileName: string;
    function GetLinkFileName: string;
    function GetGidFileName: string;
    function GetProjectFileName: string;
    function ReadFileName(const FormatName: string): string;
  public
    function AddHelpFile(const HelpFileName, IndexName: string): Boolean;
    property ContentFileName: string read GetContentFileName;
    property GidFileName: string read GetGidFileName;
    property IndexFileName: string read GetIndexFileName;
    property LinkFileName: string read GetLinkFileName;
    property ProjectFileName: string read GetProjectFileName;
  end;
  {$ENDIF MSWINDOWS}

  TJclDelphiIdeTool = class (TJclDelphiInstallationObject)
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
    constructor Create(AInstallation: TJclDelphiInstallation);
    procedure CheckIndex(Index: Integer);
  public
    property Count: Integer read GetCount write SetCount;
    function IndexOfPath(const Value: string): Integer;
    function IndexOfTitle(const Value: string): Integer;
    property Key: string read FKey;
    property Title[Index: Integer]: string read GetTitle write SetTitle;
    property Path[Index: Integer]: string read GetPath write SetPath;
    property Parameters[Index: Integer]: string read GetParameters write SetParameters;
    property WorkingDir[Index: Integer]: string read GetWorkingDir write SetWorkingDir;
  end;

  TJclDelphiIdePackages = class (TJclDelphiInstallationObject)
  private
    FDisabledPackages: TStringList;
    FKnownPackages: TStringList;
    function GetCount: Integer;
    function GetPackageDescriptions(Index: Integer): string;
    function GetPackageDisabled(Index: Integer): Boolean;
    function GetPackageFileNames(Index: Integer): string;
  protected
    constructor Create(AInstallation: TJclDelphiInstallation);
    function PackageEntryToFileName(const Entry: string): string;
    procedure ReadPackages;
    procedure RemoveDisabled(const FileName: string);
  public
    destructor Destroy; override;
    function AddPackage(const FileName, Description: string): Boolean;
    property Count: Integer read GetCount;
    property PackageDescriptions[Index: Integer]: string read GetPackageDescriptions;
    property PackageFileNames[Index: Integer]: string read GetPackageFileNames;
    property PackageDisabled[Index: Integer]: Boolean read GetPackageDisabled;
  end;

  TJclDelphiCompiler = class (TJclDelphiInstallationObject)
  private
    FDCCLocation: string;
    FDCCOutput: string;
    FOptions: TStrings;
  protected
    constructor Create(AInstallation: TJclDelphiInstallation);
  public
    destructor Destroy; override;
    procedure AddPathOption(const Option, Path: string);
    function Compile(const CommandLine: string): Boolean;
    function InstallPackage(const PackageName, BPLPath, DCPPath: string): Boolean;
    property DCCLocation: string read FDCCLocation;
    property DCCOutput: string read FDCCOutput;
    property Options: TStrings read FOptions;
  end;

  TJclDelphiPalette = class (TJclDelphiInstallationObject)
  private
    FKey: string;
    FTabNames: TStringList;
    function GetComponentsOnTab(Index: Integer): string;
    function GetHiddenComponentsOnTab(Index: Integer): string;
    function GetTabNameCount: Integer;
    function GetTabNames(Index: Integer): string;
    procedure ReadTabNames;
  protected
    constructor Create(AInstallation: TJclDelphiInstallation);
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

  TJclDelphiRepository = class (TJclDelphiInstallationObject)
  private
    FIniFile: TIniFile;
    FFileName: string;
    FPages: TStrings;
    function GetIniFile: TIniFile;
  protected
    constructor Create(AInstallation: TJclDelphiInstallation);
  public
    destructor Destroy; override;
    procedure AddObject(const FileName, ObjectType, PageName, ObjectName, IconFileName, Description,
      Author, Designer: string; const Ancestor: string = '');
    procedure CloseIniFile;
    function FindPage(const Name: string; OptionalIndex: Integer): string;
    procedure RemoveObjects(const PartialPath, FileName, ObjectType: string);
    property FileName: string read FFileName;
    property IniFile: TIniFile read GetIniFile;
    property Pages: TStrings read FPages;
  end;

  TJclDelphiInstallation = class (TObject)
  private
    FConfigData: TCustomIniFile;
    FGlobals: TStrings;
    FRootDir: string;
    FBinFolderName: string;
    FCompiler: TJclDelphiCompiler;
    FEdition: TJclDelphiEdition;
    FEnvironmentVariables: TStrings;
    FIdeExeFileName: string;
    FIdePackages: TJclDelphiIdePackages;
    FIdeTools: TJclDelphiIdeTool;
    FInstalledUpdatePack: Integer;
    FLatestUpdatePack: Integer;
    {$IFDEF MSWINDOWS}
    FOpenHelp: TJclDelphiOpenHelp;
    {$ENDIF}
    FPalette: TJclDelphiPalette;
    FRepository: TJclDelphiRepository;
    FVersionNumber: Byte;
    function GetBPLOutputPath: string;
    function GetCompiler: TJclDelphiCompiler;
    function GetDCPOutputPath: string;
    function GetDebugDCUPath: string;
    function GetEditionAsText: string;
    function GetEnvironmentVariables: TStrings;
    function GetIdeExeBuildNumber: string;
    function GetIdePackages: TJclDelphiIdePackages;
    function GetLibrarySearchPath: TJclDelphiPath;
    function GetName: string;
    function GetPalette: TJclDelphiPalette;
    function GetRepository: TJclDelphiRepository;
    function GetUpdateNeeded: Boolean;
    function GetValid: Boolean;
    procedure SetLibrarySearchPath(const Value: TJclDelphiPath);
    function GetLibraryBrowsingPath: TJclDelphiPath;
    procedure SetLibraryBrowsingPath(const Value: TJclDelphiPath);
    procedure SetDebugDCUPath(const Value: string);
  protected
    constructor Create(const AConfigDataLocation: string);
    procedure ReadInformation;
    function AddMissingPathElements(var Path: string; const NewPath: string): Boolean;
  public
    destructor Destroy; override;
    class procedure ExtractPaths(const Path: TJclDelphiPath; List: TStrings);
    function AnyInstanceRunning: Boolean;
    function AddToDebugDCUPath(const Path: string): Boolean;
    function AddToLibrarySearchPath(const Path: string): Boolean;
    function AddToLibraryBrowsingPath(const Path: string): Boolean;
    function FindFolderInDelphiPath(Folder: string; List: TStrings): Integer;
    function SubstitutePath(const Path: string): string;
    function SupportsVisualCLX: Boolean;
    property BinFolderName: string read FBinFolderName;
    property BPLOutputPath: string read GetBPLOutputPath;
    property Compiler: TJclDelphiCompiler read GetCompiler;
    property DebugDCUPath: string read GetDebugDCUPath write SetDebugDCUPath;
    property DCPOutputPath: string read GetDCPOutputPath;
    property Edition: TJclDelphiEdition read FEdition;
    property EditionAsText: string read GetEditionAsText;
    property EnvironmentVariables: TStrings read GetEnvironmentVariables;
    property IdePackages: TJclDelphiIdePackages read GetIdePackages;
    property IdeTools: TJclDelphiIdeTool read FIdeTools;
    property IdeExeBuildNumber: string read GetIdeExeBuildNumber;
    property IdeExeFileName: string read FIdeExeFileName;
    property InstalledUpdatePack: Integer read FInstalledUpdatePack;
    property LatestUpdatePack: Integer read FLatestUpdatePack;
    property LibrarySearchPath: TJclDelphiPath read GetLibrarySearchPath write SetLibrarySearchPath;
    property LibraryBrowsingPath: TJclDelphiPath read GetLibraryBrowsingPath write SetLibraryBrowsingPath;
    {$IFDEF MSWINDOWS}
    property OpenHelp: TJclDelphiOpenHelp read FOpenHelp;
    {$ENDIF}
    property ConfigData: TCustomIniFile read FConfigData;
    property Globals: TStrings read FGlobals;
    property Name: string read GetName;
    property Palette: TJclDelphiPalette read GetPalette;
    property Repository: TJclDelphiRepository read GetRepository;
    property RootDir: string read FRootDir;
    property UpdateNeeded: Boolean read GetUpdateNeeded;
    property Valid: Boolean read GetValid;
    property VersionNumber: Byte read FVersionNumber;
  end;

  TJclDelphiInstallations = class (TObject)
  private
    FList: TObjectList;
    function GetCount: Integer;
    function GetInstallations(Index: Integer): TJclDelphiInstallation;
    function GetVersionInstalled(VersionNumber: Byte): Boolean;
    function GetInstallationFromVersion(VersionNumber: Byte): TJclDelphiInstallation;
  protected
    procedure ReadInstallations;
  public
    constructor Create;
    destructor Destroy; override;
    function AnyInstanceRunning: Boolean;
    function AnyUpdatePackNeeded(var Text: string): Boolean;
    property Count: Integer read GetCount;
    property Installations[Index: Integer]: TJclDelphiInstallation read GetInstallations; default;
    property InstallationFromVersion[VersionNumber: Byte]: TJclDelphiInstallation read GetInstallationFromVersion;
    property VersionInstalled[VersionNumber: Byte]: Boolean read GetVersionInstalled;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Registry,
  JclRegistry,
  JclMiscel,
  {$ENDIF}
  {$IFDEF UNIX}
  Libc,
  {$ENDIF}
  JclFileUtils, JclLogic, JclStrings, JclSysInfo, JclSysUtils;

//==================================================================================================
// Internal
//==================================================================================================

type
  TUpdatePack = record
    DelphiVersion: Byte;
    LatestUpdatePack: Integer;
  end;
  {$IFDEF KYLIX}
  TKylixVersion = 1..3;
  {$ENDIF}

const
  {$IFDEF MSWINDOWS}
  {$IFNDEF COMPILER6_UP}
  PathSep = ';';
  {$ENDIF COMPILER6_UP}

  MSHelpSystemKeyName        = 'Software\Microsoft\Windows\Help';

  DelphiKeyName              = 'SOFTWARE\Borland\Delphi';
  {$ENDIF MSWINDOWS}

  {$IFDEF KYLIX}
  RootDirValueName           = 'DelphiRoot';
  {$ELSE}
  RootDirValueName           = 'RootDir';
  {$ENDIF}

  VersionValueName           = 'Version';

  DebuggingKeyName           = 'Debugging';
  DebugDCUPathValueName      = 'Debug DCUs Path';

  GlobalsKeyName             = 'Globals';

  LibraryKeyName             = 'Library';
  LibrarySearchPathValueName = 'Search Path';
  LibraryBrowsingPathValueName = 'Browsing Path';
  LibraryBPLOutputValueName  = 'Package DPL Output';
  LibraryDCPOutputValueName  = 'Package DCP Output';

  TransferKeyName            = 'Transfer';
  TransferCountValueName     = 'Count';
  TransferPathValueName      = 'Path%d';
  TransferParamsValueName    = 'Params%d';
  TransferTitleValueName     = 'Title%d';
  TransferWorkDirValueName   = 'WorkingDir%d';

  DisabledPackagesKeyName    = 'Disabled Packages';
  EnvVariablesKeyName        = 'Environment Variables';
  KnownPackagesKeyName       = 'Known Packages';

  PaletteKeyName             = 'Palette';
  PaletteHiddenTag           = '.Hidden';

  {$IFDEF MSWINDOWS}
  DelphiIdeFileName          = 'Bin\delphi32.exe';
  DelphiOptionsFileExtension = '.dof';
  DelphiRepositoryFileName   = 'Bin\delphi32.dro';
  DCCFileName                = 'Bin\dcc32.exe';
  DelphiHelpContentFileName  = 'Help\%s.ohc';
  DelphiHelpIndexFileName    = 'Help\%s.ohi';
  DelphiHelpLinkFileName     = 'Help\%s.ohl';
  DelphiHelpProjectFileName  = 'Help\%s.ohp';
  DelphiHelpGidFileName      = 'Help\%s.gid';
  DelphiHelpNamePart1        = 'delphi%d';
  DelphiHelpNamePart2        = 'd%d';
  {$ENDIF MSWINDOWS}

  {$IFDEF KYLIX}
  DelphiIdeFileName          = 'bin/delphi';
  DelphiOptionsFileExtension = '.kof';

  LibSuffixes: array[TKylixVersion] of Integer = (60, 65, 69);

  DelphiRcFileNames: array[TKylixVersion] of string =
    (
      'delphi60rc',
      'delphi65rc',
      'delphi69rc'
    );
  DelphiRepositoryFileNames: array[TKylixVersion] of string =
    (
      'delphi60dro',
      'delphi65dro',
      'delphi69dro'
    );
  DCCFileName              = 'bin/dcc';
  KylixHelpNamePart          = 'k%d';
  {$ENDIF KYLIX}

  LatestUpdatePacks: array [1..3] of TUpdatePack = ( // Updated Sep 5, 2002
    {$IFDEF KYLIX}
    (DelphiVersion: 1; LatestUpdatePack: 0),
    (DelphiVersion: 2; LatestUpdatePack: 0),
    (DelphiVersion: 3; LatestUpdatePack: 0)
    {$ELSE}
    (DelphiVersion: 5; LatestUpdatePack: 1),
    (DelphiVersion: 6; LatestUpdatePack: 2),
    (DelphiVersion: 7; LatestUpdatePack: 0)
    {$ENDIF}
  );

resourcestring
  RsIndexOufOfRange = 'Index out of range';
  RsNeedUpdate      = 'You should install latest Update Pack #%d for %s';
  RsUpdatePackName  = 'Update Pack #%d';
  {$IFDEF KYLIX}
  RsDelphiName      = 'Kylix %d for Delphi %s';
  RsOpenEdition     = 'Open Edition';
  RsServerDeveloper = 'Server Developer';
  {$ELSE}
  RsDelphiName      = 'Delphi %d %s';
  RsClientServer    = 'Client/Server';
  RsStandard        = 'Standard';
  {$ENDIF}
  RsArchitect       = 'Architect';
  RsEnterprise      = 'Enterprise';
  RsPersonal        = 'Personal';
  RsProfessional    = 'Professional';

//--------------------------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
function RegGetValueNamesAndValues(const RootKey: HKEY; const Key: string; const List: TStrings): Boolean;
var
  I: Integer;
  TempList: TStringList;
begin
  TempList := TStringList.Create;
  try
    Result := RegKeyExists(RootKey, Key) and RegGetValueNames(RootKey, Key, TempList);
    if Result then
    begin
      for I := 0 to TempList.Count - 1 do
        TempList[I] := TempList[I] + '=' + RegReadStringDef(RootKey, Key, TempList[I], '');
      List.AddStrings(TempList);
    end;
  finally
    TempList.Free;
  end;
end;
{$ENDIF MSWINDOWS}

//==================================================================================================
// TJclDelphiInstallationObject
//==================================================================================================

constructor TJclDelphiInstallationObject.Create(AInstallation: TJclDelphiInstallation);
begin
  FInstallation := AInstallation;
end;

//==================================================================================================
// TJclDelphiOpenHelp
//==================================================================================================

{$IFDEF MSWINDOWS}

function TJclDelphiOpenHelp.AddHelpFile(const HelpFileName, IndexName: string): Boolean;
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
      DeleteFile(GidFileName);
    finally
      List.Free;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiOpenHelp.GetContentFileName: string;
begin
  Result := ReadFileName(DelphiHelpContentFileName);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiOpenHelp.GetGidFileName: string;
begin
  Result := ReadFileName(DelphiHelpGidFileName);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiOpenHelp.GetIndexFileName: string;
begin
  Result := ReadFileName(DelphiHelpIndexFileName);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiOpenHelp.GetLinkFileName: string;
begin
  Result := ReadFileName(DelphiHelpLinkFileName);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiOpenHelp.GetProjectFileName: string;
begin
  Result := ReadFileName(DelphiHelpProjectFileName);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiOpenHelp.ReadFileName(const FormatName: string): string;
begin
  with Installation do
  begin
    Result := PathAddSeparator(RootDir) + Format(FormatName, [Format(DelphiHelpNamePart1, [VersionNumber])]);
    if not FileExists(Result) then
      Result := PathAddSeparator(RootDir) + Format(FormatName, [Format(DelphiHelpNamePart2, [VersionNumber])]);
  end;
end;

{$ENDIF MSWINDOWS}

//==================================================================================================
// TJclDelphiIdeTool
//==================================================================================================

procedure TJclDelphiIdeTool.CheckIndex(Index: Integer);
begin
  if (Index < 0) or (Index >= Count) then
    raise EJclError.CreateResRec(@RsIndexOufOfRange);
end;

//--------------------------------------------------------------------------------------------------

constructor TJclDelphiIdeTool.Create(AInstallation: TJclDelphiInstallation);
begin
  inherited;
  FKey := TransferKeyName;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiIdeTool.GetCount: Integer;
begin
  Result := Installation.ConfigData.ReadInteger(Key, TransferCountValueName, 0);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiIdeTool.GetParameters(Index: Integer): string;
begin
  CheckIndex(Index);
  Result := Installation.ConfigData.ReadString(Key, Format(TransferParamsValueName, [Index]), '');
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiIdeTool.GetPath(Index: Integer): string;
begin
  CheckIndex(Index);
  Result := Installation.ConfigData.ReadString(Key, Format(TransferPathValueName, [Index]), '');
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiIdeTool.GetTitle(Index: Integer): string;
begin
  CheckIndex(Index);
  Result := Installation.ConfigData.ReadString(Key, Format(TransferTitleValueName, [Index]), '');
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiIdeTool.GetWorkingDir(Index: Integer): string;
begin
  CheckIndex(Index);
  Result := Installation.ConfigData.ReadString(Key, Format(TransferWorkDirValueName, [Index]), '');
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiIdeTool.IndexOfPath(const Value: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if AnsiSameText(Title[I], Value) then
    begin
      Result := I;
      Break;
    end;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiIdeTool.IndexOfTitle(const Value: string): Integer;
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

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiIdeTool.SetCount(const Value: Integer);
begin
  if Value > Count then
    Installation.ConfigData.WriteInteger(Key, TransferCountValueName, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiIdeTool.SetParameters(Index: Integer; const Value: string);
begin
  CheckIndex(Index);
  Installation.ConfigData.WriteString(Key, Format(TransferParamsValueName, [Index]), Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiIdeTool.SetPath(Index: Integer; const Value: string);
begin
  CheckIndex(Index);
  Installation.ConfigData.WriteString(Key, Format(TransferPathValueName, [Index]), Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiIdeTool.SetTitle(Index: Integer; const Value: string);
begin
  CheckIndex(Index);
  Installation.ConfigData.WriteString(Key, Format(TransferTitleValueName, [Index]), Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiIdeTool.SetWorkingDir(Index: Integer; const Value: string);
begin
  CheckIndex(Index);
  Installation.ConfigData.WriteString(Key, Format(TransferWorkDirValueName, [Index]), Value);
end;

//==================================================================================================
// TJclDelphiIdePackages
//==================================================================================================

function TJclDelphiIdePackages.AddPackage(const FileName, Description: string): Boolean;
begin
  Result := True;
  RemoveDisabled(FileName);
  Installation.ConfigData.WriteString(KnownPackagesKeyName, FileName, Description);
  ReadPackages;
end;

//--------------------------------------------------------------------------------------------------

constructor TJclDelphiIdePackages.Create(AInstallation: TJclDelphiInstallation);
begin
  inherited;
  FDisabledPackages := TStringList.Create;
  FDisabledPackages.Sorted := True;
  FDisabledPackages.Duplicates := dupIgnore;
  FKnownPackages := TStringList.Create;
  FKnownPackages.Sorted := True;
  FKnownPackages.Duplicates := dupIgnore;
  ReadPackages;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclDelphiIdePackages.Destroy;
begin
  FreeAndNil(FDisabledPackages);
  FreeAndNil(FKnownPackages);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiIdePackages.GetCount: Integer;
begin
  Result := FKnownPackages.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiIdePackages.GetPackageDescriptions(Index: Integer): string;
begin
  Result := FKnownPackages.Values[FKnownPackages.Names[Index]];
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiIdePackages.GetPackageDisabled(Index: Integer): Boolean;
begin
  Result := Boolean(FKnownPackages.Objects[Index]);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiIdePackages.GetPackageFileNames(Index: Integer): string;
begin
  Result := PackageEntryToFileName(FKnownPackages.Names[Index]);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiIdePackages.PackageEntryToFileName(const Entry: string): string;
begin
  Result := {$IFDEF MSWINDOWS} PathGetLongName2 {$ENDIF} (Installation.SubstitutePath(Entry));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiIdePackages.ReadPackages;
var
  I: Integer;
begin
  FDisabledPackages.Clear;
  FKnownPackages.Clear;
  Installation.ConfigData.ReadSection(KnownPackagesKeyName, FKnownPackages);
  Installation.ConfigData.ReadSection(KnownPackagesKeyName, FDisabledPackages);
  for I := 0 to Count - 1 do
    if FDisabledPackages.IndexOfName(FKnownPackages.Names[I]) <> -1 then
      FKnownPackages.Objects[I] := Pointer(True);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiIdePackages.RemoveDisabled(const FileName: string);
var
  I: Integer;
begin
  for I := 0 to FDisabledPackages.Count - 1 do
    if AnsiSameText(FileName, PackageEntryToFileName(FDisabledPackages.Names[I])) then
    begin
      Installation.ConfigData.DeleteKey(DisabledPackagesKeyName, FDisabledPackages.Names[I]);
      ReadPackages;
      Break;
    end;
end;

//==================================================================================================
// TJclDelphiCompiler
//==================================================================================================

procedure TJclDelphiCompiler.AddPathOption(const Option, Path: string);
begin
  Options.Add(Format('-%s"%s"', [Option, PathRemoveSeparator(Path)]));
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiCompiler.Compile(const CommandLine: string): Boolean;
{$IFDEF WIN32}
const
  DCC32CFGFileName = 'DCC32.CFG';
var
  Cmd: string;
begin
  FDCCOutput := '';
  FOptions.SaveToFile(DCC32CFGFileName);
  Cmd := Format('"%s" "%s"', [DCCLocation, CommandLine]);
  Result := WinExec32AndRedirectOutput(Cmd, FDCCOutput) = 0;
  DeleteFile(DCC32CFGFileName);
end;
{$ENDIF WIN32}
{$IFDEF KYLIX}
const
  DCCConfFileName = 'dcc.conf';
var
  Cmd: string;
  Output: PIOFile;
  Count, ResultCode: Integer;
  Buffer: array[Byte] of Char;
  TempOutput: string;
begin
  FDCCOutput := '';
  FOptions.SaveToFile(DCCConfFileName);
  Cmd := Format('"%s" "%s"', [DCCLocation, CommandLine]);
  Output := Libc.popen(PChar(Cmd), 'r');
  repeat
    Count := fread_unlocked(@Buffer, 1, Length(Buffer) - 1, Output);
    if Count > 0 then
    begin
      Buffer[Count] := #0;
      TempOutput := TempOutput + Buffer;
    end;
  until Count < Length(Buffer) - 1;
  FDCCOutput := FDCCOutput + TempOutput;
  ResultCode := pclose(Output);
  Result := ResultCode = 0;
  DeleteFile(DCCConfFileName);
end;
{$ENDIF KYLIX}

//--------------------------------------------------------------------------------------------------

constructor TJclDelphiCompiler.Create(AInstallation: TJclDelphiInstallation);
begin
  inherited;
  FOptions := TStringList.Create;
  FDCCLocation := PathAddSeparator(Installation.RootDir) + DCCFileName;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclDelphiCompiler.Destroy;
begin
  FreeAndNil(FOptions);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiCompiler.InstallPackage(const PackageName, BPLPath, DCPPath: string): Boolean;
const
  DOFDirectoriesSection = 'Directories';
  UnitOutputDirName     = 'UnitOutputDir';
  SearchPathName        = 'SearchPath';
  DescriptionOption     = '{$DESCRIPTION ''';
  LibSuffixOption       = '{$LIBSUFFIX ''';
  RunOnlyOption         = '{$RUNONLY}';
var
  SaveDir, PackagePath, Description, LibSuffix, BPLFileName, S: string;
  RunOnly: Boolean;
  OptionsFile: TIniFile;
  DPKFile: TStringList;
  I: Integer;
begin
  PackagePath := PathRemoveSeparator(ExtractFilePath(PackageName));
  SaveDir := GetCurrentDir;
  SetCurrentDir(PackagePath);
  try
    OptionsFile := TIniFile.Create(ChangeFileExt(PackageName, DelphiOptionsFileExtension));
    try
      Options.Clear;
      S := OptionsFile.ReadString(DOFDirectoriesSection, SearchPathName, '');
      AddPathOption('N', OptionsFile.ReadString(DOFDirectoriesSection, UnitOutputDirName, ''));
      AddPathOption('I', S);
      AddPathOption('R', S);
      AddPathOption('LE', BPLPath);
      AddPathOption('LN', DCPPath);
      AddPathOption('U', StrEnsureSuffix(PathSep, DCPPath) + S);
    finally
      OptionsFile.Free;
    end;
    Result := Compile(PackageName);
  finally
    SetCurrentDir(SaveDir);
  end;
  if Result then
  begin
    DPKFile := TStringList.Create;
    try
      DPKFile.LoadFromFile(PackageName);
      Description := '';
      LibSuffix := '';
      RunOnly := False;
      for I := 0 to DPKFile.Count - 1 do
      begin
        S := TrimRight(DPKFile[I]);
        if Pos(DescriptionOption, S) = 1 then
          Description := Copy(S, Length(DescriptionOption), Length(S) - Length(DescriptionOption))
        else
        if Pos(LibSuffixOption, S) = 1 then
          LibSuffix := Copy(S, Length(LibSuffixOption), Length(S) - Length(LibSuffixOption))
        else
        if Pos(RunOnlyOption, S) = 1 then
          RunOnly := True;
      end;
      if not RunOnly then
      begin
        BPLFileName := PathAddSeparator(BPLPath) + PathExtractFileNameNoExt(PackageName) + LibSuffix + '.bpl';
        Result := Installation.IdePackages.AddPackage(BPLFileName, Description);
      end;  
    finally
      DPKFile.Free;
    end;
  end;
end;

//==================================================================================================
// TJclDelphiPalette
//==================================================================================================

procedure TJclDelphiPalette.ComponentsOnTabToStrings(Index: Integer; Strings: TStrings;
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

//--------------------------------------------------------------------------------------------------

constructor TJclDelphiPalette.Create(AInstallation: TJclDelphiInstallation);
begin
  inherited;
  FKey := PaletteKeyName;
  FTabNames := TStringList.Create;
  FTabNames.Sorted := True;
  ReadTabNames;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiPalette.DeleteTabName(const TabName: string): Boolean;
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

//--------------------------------------------------------------------------------------------------

destructor TJclDelphiPalette.Destroy;
begin
  FreeAndNil(FTabNames);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiPalette.GetComponentsOnTab(Index: Integer): string;
begin
  Result := Installation.ConfigData.ReadString(Key, FTabNames[Index], '');
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiPalette.GetHiddenComponentsOnTab(Index: Integer): string;
begin
  Result := Installation.ConfigData.ReadString(Key, FTabNames[Index] + PaletteHiddenTag, '');
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiPalette.GetTabNameCount: Integer;
begin
  Result := FTabNames.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiPalette.GetTabNames(Index: Integer): string;
begin
  Result := FTabNames[Index];
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiPalette.ReadTabNames;
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

//--------------------------------------------------------------------------------------------------

function TJclDelphiPalette.TabNameExists(const TabName: string): Boolean;
begin
  Result := FTabNames.IndexOf(TabName) <> -1;
end;

//==================================================================================================
// TJclDelphiRepository
//==================================================================================================

procedure TJclDelphiRepository.AddObject(const FileName, ObjectType, PageName, ObjectName,
  IconFileName, Description, Author, Designer: string; const Ancestor: string);
var
  SectionName: string;
begin
  GetIniFile;
  SectionName := AnsiUpperCase(PathRemoveExtension(FileName));
  FIniFile.EraseSection(FileName);
  FIniFile.EraseSection(SectionName);
  FIniFile.WriteString(SectionName, DelphiRepositoryObjectType, ObjectType);
  FIniFile.WriteString(SectionName, DelphiRepositoryObjectName, ObjectName);
  FIniFile.WriteString(SectionName, DelphiRepositoryObjectPage, PageName);
  FIniFile.WriteString(SectionName, DelphiRepositoryObjectIcon, IconFileName);
  FIniFile.WriteString(SectionName, DelphiRepositoryObjectDescr, Description);
  FIniFile.WriteString(SectionName, DelphiRepositoryObjectAuthor, Author);
  if Ancestor <> '' then
    FIniFile.WriteString(SectionName, DelphiRepositoryObjectAncestor, Ancestor);
  if Installation.VersionNumber >= 6 then
    FIniFile.WriteString(SectionName, DelphiRepositoryObjectDesigner, Designer);
  FIniFile.WriteBool(SectionName, DelphiRepositoryObjectNewForm, False);
  FIniFile.WriteBool(SectionName, DelphiRepositoryObjectMainForm, False);
  CloseIniFile;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiRepository.CloseIniFile;
begin
  FreeAndNil(FIniFile);
end;

//--------------------------------------------------------------------------------------------------

constructor TJclDelphiRepository.Create(AInstallation: TJclDelphiInstallation);
begin
  inherited;
  {$IFDEF KYLIX}
  FFileName := Format('%s/.borland/%s', [GetPersonalFolder, DelphiRepositoryFileNames[Installation.VersionNumber]]);
  {$ELSE}
  FFileName := PathAddSeparator(Installation.RootDir) + DelphiRepositoryFileName;
  {$ENDIF}
  FPages := TStringList.Create;
  IniFile.ReadSection(DelphiRepositoryPagesSection, FPages);
  CloseIniFile;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclDelphiRepository.Destroy;
begin
  FreeAndNil(FPages);
  FreeAndNil(FIniFile);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiRepository.FindPage(const Name: string; OptionalIndex: Integer): string;
var
  I: Integer;
begin
  I := FPages.IndexOf(Name);
  if I >= 0 then
    Result := FPages[I]
  else
  begin
    if OptionalIndex < FPages.Count then
      Result := FPages[OptionalIndex]
    else
      Result := '';
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiRepository.GetIniFile: TIniFile;
begin
  if not Assigned(FIniFile) then
    FIniFile := TIniFile.Create(FileName);
  Result := FIniFile;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiRepository.RemoveObjects(const PartialPath, FileName, ObjectType: string);
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
      if FIniFile.ReadString(SectionName, DelphiRepositoryObjectType, '') = ObjectType then
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

//==================================================================================================
// TJclDelphiInstallation
//==================================================================================================

function TJclDelphiInstallation.AddToDebugDCUPath(const Path: string): Boolean;
var
  TempDebugDCUPath: TJclDelphiPath;
begin
  TempDebugDCUPath := DebugDCUPath;
  Result := AddMissingPathElements(TempDebugDCUPath, Path);
  DebugDCUPath := TempDebugDCUPath;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.AddToLibrarySearchPath(const Path: string): Boolean;
var
  TempLibraryPath: TJclDelphiPath;
begin
  TempLibraryPath := LibrarySearchPath;
  Result := AddMissingPathElements(TempLibraryPath, Path);
  LibrarySearchPath := TempLibraryPath;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.AddToLibraryBrowsingPath(const Path: string): Boolean;
var
  TempLibraryPath: TJclDelphiPath;
begin
  TempLibraryPath := LibraryBrowsingPath;
  Result := AddMissingPathElements(TempLibraryPath, Path);
  LibraryBrowsingPath := TempLibraryPath;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.AnyInstanceRunning: Boolean;
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

//--------------------------------------------------------------------------------------------------

constructor TJclDelphiInstallation.Create;
begin
  {$IFDEF KYLIX}
  FConfigData := TMemIniFile.Create(AConfigDataLocation);
  {$ELSE}
  FConfigData := TRegistryIniFile.Create(AConfigDataLocation);
  {$ENDIF}
  FGlobals := TStringList.Create;
  ReadInformation;
  FIdeTools := TJclDelphiIdeTool.Create(Self);
  {$IFNDEF KYLIX}
  FOpenHelp := TJclDelphiOpenHelp.Create(Self);
  {$ENDIF}
end;

//--------------------------------------------------------------------------------------------------

destructor TJclDelphiInstallation.Destroy;
begin
  FreeAndNil(FGlobals);
  FreeAndNil(FRepository);
  FreeAndNil(FCompiler);
  FreeAndNil(FIdePackages);
  FreeAndNil(FIdeTools);
  {$IFDEF MSWINDOWS}
  FreeAndNil(FOpenHelp);
  {$ENDIF}
  FreeAndNil(FPalette);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

class procedure TJclDelphiInstallation.ExtractPaths(const Path: TJclDelphiPath; List: TStrings);
begin
  StrToStrings(Path, PathSep, List);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.AddMissingPathElements(var Path: string; const NewPath: string): Boolean;
var
  PathItems, NewItems: TStringList;
  Folder: string;
  I: Integer;
  Missing: Boolean;
begin
  Result := False;
  PathItems := nil;
  NewItems := nil;
  try
    PathItems := TStringList.Create;
    NewItems := TStringList.Create;
    ExtractPaths(Path, PathItems);
    ExtractPaths(NewPath, NewItems);
    for I := 0 to NewItems.Count - 1 do
    begin
      Folder := NewItems[I];
      Missing := FindFolderInDelphiPath(Folder, PathItems) = -1;
      if Missing then
      begin
        Path := StrEnsureSuffix(PathSep, Path) + Folder;
        Result := True;
      end;
    end;
  finally
    PathItems.Free;
    NewItems.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.FindFolderInDelphiPath(Folder: string; List: TStrings): Integer;
var
  I: Integer;
begin
  Result := -1;
  Folder := PathRemoveSeparator(Folder);
  for I := 0 to List.Count - 1 do
    if AnsiSameText(Folder, PathRemoveSeparator(SubstitutePath(List[I]))) then
    begin
      Result := I;
      Break;
    end;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetBPLOutputPath: string;
begin
  Result := SubstitutePath(ConfigData.ReadString(LibraryKeyName, LibraryBPLOutputValueName, ''));
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetCompiler: TJclDelphiCompiler;
begin
  if not Assigned(FCompiler) then
    FCompiler := TJclDelphiCompiler.Create(Self);
  Result := FCompiler;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetDCPOutputPath: string;
begin
  Result := SubstitutePath(ConfigData.ReadString(LibraryKeyName, LibraryDCPOutputValueName, ''));
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetDebugDCUPath: string;
begin
  Result := ConfigData.ReadString(DebuggingKeyName, DebugDCUPathValueName, '');
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetEditionAsText: string;
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
  {$ELSE KYLIX}
  case Edition of
    deSTD:
      if VersionNumber >= 6 then
        Result := RsPersonal
      else
        Result := RsStandard;
    dePRO:
      Result := RsProfessional;
    deCSS:
      if VersionNumber >= 5 then
        Result := RsEnterprise
      else
        Result := RsClientServer;
  end;
  {$ENDIF KYLIX}
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetEnvironmentVariables: TStrings;
var
  EnvNames: TStringList;
  EnvVarKeyName: string;
  I: Integer;
begin
  if FEnvironmentVariables = nil then
  begin
    FEnvironmentVariables := TStringList.Create;
    if (VersionNumber >= 6) and ConfigData.SectionExists(EnvVariablesKeyName) then
    begin
      EnvNames := TStringList.Create;
      try
        ConfigData.ReadSection(EnvVariablesKeyName, EnvNames);
        for I := 0 to EnvNames.Count - 1 do
          FEnvironmentVariables.Values[EnvNames[I]] := ConfigData.ReadString(EnvVarKeyName, EnvNames[I], '');
      finally
        EnvNames.Free;
      end;
    end;
    FEnvironmentVariables.Values['DELPHI'] := RootDir;
  end;
  Result := FEnvironmentVariables;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetIdeExeBuildNumber: string;
begin
  {$IFDEF KYLIX}
  { TODO : determine Kylix IDE build # }
  Result := '?';
  {$ELSE}
  Result := VersionFixedFileInfoString(IdeExeFileName, vfFull);
  {$ENDIF}
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetIdePackages: TJclDelphiIdePackages;
begin
  if not Assigned(FIdePackages) then
    FIdePackages := TJclDelphiIdePackages.Create(Self);
  Result := FIdePackages;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetLibrarySearchPath: TJclDelphiPath;
begin
  Result := ConfigData.ReadString(LibraryKeyName, LibrarySearchPathValueName, '');
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetLibraryBrowsingPath: TJclDelphiPath;
begin
  Result := ConfigData.ReadString(LibraryKeyName, LibraryBrowsingPathValueName, '');
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetName: string;
begin
  Result := Format(RsDelphiName, [VersionNumber, EditionAsText]);
  if InstalledUpdatePack > 0 then
    Result := Result + ' ' + Format(RsUpdatePackName, [InstalledUpdatePack]);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetPalette: TJclDelphiPalette;
begin
  if not Assigned(FPalette) then
    FPalette := TJclDelphiPalette.Create(Self);
  Result := FPalette;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetRepository: TJclDelphiRepository;
begin
  if not Assigned(FRepository) then
    FRepository := TJclDelphiRepository.Create(Self);
  Result := FRepository;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetUpdateNeeded: Boolean;
begin
  Result := InstalledUpdatePack < LatestUpdatePack;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetValid: Boolean;
begin
  Result := (ConfigData.FileName <> '') and (RootDir <> '') and FileExists(IdeExeFileName);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiInstallation.ReadInformation;
const
  {$IFDEF KYLIX}
  BinDir = 'bin/';
  EditionNames: array [TJclDelphiEdition] of PChar = ('OPEN', 'PRO', 'SVR');
  {$ELSE}
  BinDir = 'Bin\';
  EditionNames: array [TJclDelphiEdition] of PChar = ('STD', 'PRO', 'CSS');
  {$ENDIF}
  UpdateKeyName = 'Update #';
var
  KeyLen, I: Integer;
  Key, KeyName: string;
  Ed: TJclDelphiEdition;
begin
  Key := ConfigData.FileName;
  {$IFDEF KYLIX}
  ConfigData.ReadSectionValues(GlobalsKeyName, Globals);
  FRootDir := Globals.Values[RootDirValueName];

  for I := Low(DelphiRcFileNames) to High(DelphiRcFileNames) do
    if DelphiRcFileNames[I] = ExtractFileName(ConfigData.FileName) then
    begin
      FVersionNumber := I;
      Break;
    end;
  {$ELSE KYLIX}
  RegGetValueNamesAndValues(HKEY_LOCAL_MACHINE, Key, Globals);
  FRootDir := RegReadStringDef(HKEY_LOCAL_MACHINE, ConfigData.FileName, RootDirValueName, '');

  KeyLen := Length(Key);
  if (KeyLen > 3) and StrIsDigit(Key[KeyLen - 2]) and (Key[KeyLen - 1] = '.') and (Key[KeyLen] = '0') then
    FVersionNumber := Ord(Key[KeyLen - 2]) - 48
  else
    FVersionNumber := 0;
  {$ENDIF KYLIX}

  FBinFolderName := PathAddSeparator(RootDir) + BinDir;
  FIdeExeFileName := PathAddSeparator(RootDir) + DelphiIdeFileName;

  KeyName := Globals.Values[VersionValueName];
  for Ed := Low(Ed) to High(Ed) do
    if EditionNames[Ed] = KeyName then
      FEdition := Ed;

  for I := 0 to Globals.Count - 1 do
  begin
    KeyName := Globals.Names[I];
    KeyLen := Length(UpdateKeyName);
    if (Pos(UpdateKeyName, KeyName) = 1) and (Length(KeyName) > KeyLen) and StrIsDigit(KeyName[KeyLen + 1]) then
      FInstalledUpdatePack := Max(FInstalledUpdatePack, Integer(Ord(KeyName[KeyLen + 1]) - 48));
  end;

  for I := Low(LatestUpdatePacks) to High(LatestUpdatePacks) do
    if LatestUpdatePacks[I].DelphiVersion = VersionNumber then
    begin
      FLatestUpdatePack := LatestUpdatePacks[I].LatestUpdatePack;
      Break;
    end;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiInstallation.SetDebugDCUPath(const Value: string);
begin
  ConfigData.WriteString(DebuggingKeyName, DebugDCUPathValueName, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiInstallation.SetLibrarySearchPath(const Value: TJclDelphiPath);
begin
  ConfigData.WriteString(LibraryKeyName, LibrarySearchPathValueName, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiInstallation.SetLibraryBrowsingPath(const Value: TJclDelphiPath);
begin
  ConfigData.WriteString(LibraryKeyName, LibraryBrowsingPathValueName, Value);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.SubstitutePath(const Path: string): string;
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
end;

function TJclDelphiInstallation.SupportsVisualCLX: Boolean;
begin
  {$IFDEF KYLIX}
  Result := True;
  {$ELSE}
  Result := (Edition <> deSTD) and (VersionNumber >= 6);
  {$ENDIF}
end;

//==================================================================================================
// TDelphiInstallations
//==================================================================================================

function TJclDelphiInstallations.AnyInstanceRunning: Boolean;
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

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallations.AnyUpdatePackNeeded(var Text: string): Boolean;
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

//--------------------------------------------------------------------------------------------------

constructor TJclDelphiInstallations.Create;
begin
  FList := TObjectList.Create;
  ReadInstallations;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclDelphiInstallations.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallations.GetCount: Integer;
begin
  Result := FList.Count;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallations.GetInstallationFromVersion(VersionNumber: Byte): TJclDelphiInstallation;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Installations[I].VersionNumber = VersionNumber then
    begin
      Result := Installations[I];
      Break;
    end;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallations.GetInstallations(Index: Integer): TJclDelphiInstallation;
begin
  Result := TJclDelphiInstallation(FList[Index]);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallations.GetVersionInstalled(VersionNumber: Byte): Boolean;
begin
  Result := InstallationFromVersion[VersionNumber] <> nil;
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiInstallations.ReadInstallations;
{$IFDEF KYLIX}
var
  I: Integer;
  Item: TJclDelphiInstallation;
  RcFileName: string;
begin
  FList.Clear;
  for I := Low(DelphiRcFileNames) to High(DelphiRcFileNames) do
  begin
    RcFileName := Format('%s/.borland/%s', [GetPersonalFolder, DelphiRcFileNames[I]]);
    if FileExists(RcFileName) then
    begin
      Item := TJclDelphiInstallation.Create(RcFileName);
      FList.Add(Item);
    end;
  end;
end;
{$ELSE KYLIX}
var
  VersionNumbers: TStringList;
  I: Integer;
  VersionKeyName: string;
  Item: TJclDelphiInstallation;
begin
  FList.Clear;
  VersionNumbers := TStringList.Create;
  try
    if RegGetKeyNames(HKEY_LOCAL_MACHINE, DelphiKeyName, VersionNumbers) then
      for I := 0 to VersionNumbers.Count - 1 do
      begin
        VersionKeyName := DelphiKeyName + PathSeparator + VersionNumbers[I];
        if RegKeyExists(HKEY_LOCAL_MACHINE, VersionKeyName) then
        begin
          Item := TJclDelphiInstallation.Create(VersionKeyName);
          FList.Add(Item);
        end;
      end;
  finally
    VersionNumbers.Free;
  end;
end;
{$ENDIF KYLIX}

//--------------------------------------------------------------------------------------------------

end.
