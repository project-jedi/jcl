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
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Routines for getting infomation about installed versions of Delphi and peforming basic           }
{ instalation tasks.                                                                               }
{                                                                                                  }
{ Unit owner: Petr Vones                                                                           }
{ Last modified: July 8, 2002                                                                      }
{                                                                                                  }
{**************************************************************************************************}

unit DelphiInstall;

{$I jcl.inc}

{$WEAKPACKAGEUNIT ON}

interface

uses
  Windows, Classes, SysUtils, IniFiles,
  {$IFDEF DELPHI5_UP}
  Contnrs,
  {$ENDIF DELPHI5_UP}
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
  DelphiRepositoryObjectDesigner  = 'Designer'; // Delphi 6 only
  DelphiRepositoryDesignerDfm     = 'dfm';
  DelphiRepositoryDesignerXfm     = 'xfm';
  DelphiRepositoryObjectNewForm   = 'DefaultNewForm';
  DelphiRepositoryObjectMainForm  = 'DefaultMainForm';

  // Delphi path
  DelphiLibraryPathSeparator    = ';';

//--------------------------------------------------------------------------------------------------
// Installed versions information classes
//--------------------------------------------------------------------------------------------------

type
  TJclDelphiEdition = (deSTD, dePRO, deCSS);
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

  TJclDelphiOpenHelp = class (TJclDelphiInstallationObject)
  private
    function GetContentFileName: string;
    function GetIndexFileName: string;
    function GetLinkFileName: string;
    function GetGidFileName: string;
    function GetProjectFileName: string;
  public
    function AddHelpFile(const HelpFileName, IndexName: string): Boolean;
    property ContentFileName: string read GetContentFileName;
    property GidFileName: string read GetGidFileName;
    property IndexFileName: string read GetIndexFileName;
    property LinkFileName: string read GetLinkFileName;
    property ProjectFileName: string read GetProjectFileName;
  end;

  TJclDelphiIdeTool = class (TJclDelphiInstallationObject)
  private
    FRegKey: string;
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
    property Title[Index: Integer]: string read GetTitle write SetTitle;
    property Path[Index: Integer]: string read GetPath write SetPath;
    property RegKey: string read FRegKey;
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
    procedure ReadPackages;
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
    FDCC32Location: string;
    FOptions: TStrings;
  protected
    constructor Create(AInstallation: TJclDelphiInstallation);
  public
    destructor Destroy; override;
    procedure AddPathOption(const Option, Path: string);
    function Compile(const CommandLine: string): Boolean;
    function InstallPackage(const PackageName: string): Boolean;
    property DCC32Location: string read FDCC32Location;
    property Options: TStrings read FOptions;
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
      Author, Designer: string);
    procedure CloseIniFile;
    function FindPage(const Name: string; OptionalIndex: Integer): string;
    property FileName: string read FFileName;
    property IniFile: TIniFile read GetIniFile;
    property Pages: TStrings read FPages;
  end;

  TJclDelphiInstallation = class (TObject)
  private
    FBinFolderName: string;
    FCompiler: TJclDelphiCompiler;
    FEdition: TJclDelphiEdition;
    FEnvironmentVariables: TStrings;
    FIdeExeFileName: string;
    FIdePackages: TJclDelphiIdePackages;
    FIdeTools: TJclDelphiIdeTool;
    FInstalledUpdatePack: Integer;
    FLatestUpdatePack: Integer;
    FOpenHelp: TJclDelphiOpenHelp;
    FRegKey: string;
    FRegKeyValues: TStrings;
    FRepository: TJclDelphiRepository;
    FRootDir: string;
    FVersionNumber: Byte;
    function GetBPLOutputPath: string;
    function GetComplier: TJclDelphiCompiler;
    function GetDCPOutputPath: string;
    function GetEditionAsText: string;
    function GetEnvironmentVariables: TStrings;
    function GetIdeExeBuildNumber: string;
    function GetIdePackages: TJclDelphiIdePackages;
    function GetLibrarySearchPath: TJclDelphiPath;
    function GetName: string;
    function GetRepository: TJclDelphiRepository;
    function GetUpdateNeeded: Boolean;
    function GetValid: Boolean;
    procedure SetLibrarySearchPath(const Value: TJclDelphiPath);
  protected
    constructor Create(const ARegKey: string);
    procedure ReadInformation;
  public
    destructor Destroy; override;
    class procedure ExtractPaths(const Path: TJclDelphiPath; List: TStrings);
    function AnyInstanceRunning: Boolean;
    function AddToLibrarySearchPath(const Path: string): Boolean;
    function FindFolderInDelphiPath(Folder: string; List: TStrings): Integer;
    function SubstitutePath(const Path: string): string;
    property BinFolderName: string read FBinFolderName;
    property BPLOutputPath: string read GetBPLOutputPath;
    property Compiler: TJclDelphiCompiler read GetComplier;
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
    property OpenHelp: TJclDelphiOpenHelp read FOpenHelp;
    property Name: string read GetName;
    property RegKey: string read FRegKey;
    property RegKeyValues: TStrings read FRegKeyValues;
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
  JclFileUtils, JclLogic, JclMiscel, JclRegistry, JclStrings, JclSysInfo, JclSysUtils;

//==================================================================================================
// Internal
//==================================================================================================

type
  TUpdatePack = record
    DelphiVersion: Byte;
    LatestUpdatePack: Integer;
  end;

const
  MSHelpSystemKeyName        = 'Software\Microsoft\Windows\Help';

  DelphiKeyName              = 'SOFTWARE\Borland\Delphi';
  RootDirValueName           = 'RootDir';
  VersionValueName           = 'Version';

  LibraryKeyName             = 'Library';
  LibrarySearchPathValueName = 'Search Path';
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

  DelphiIdeFileName          = 'Bin\delphi32.exe';
  DelphiRepositoryFileName   = 'Bin\delphi32.dro';
  DCC32FileName              = 'Bin\dcc32.exe';
  DelphiHelpContentFileName  = 'Help\delphi%d.ohc';
  DelphiHelpIndexFileName    = 'Help\delphi%d.ohi';
  DelphiHelpLinkFileName     = 'Help\delphi%d.ohl';
  DelphiHelpProjectFileName  = 'Help\delphi%d.ohp';
  DelphiHelpGidFileName      = 'Help\delphi%d.gid';

  LatestUpdatePacks: array [1..3] of TUpdatePack = ( // Updated Mar 18, 2002
    (DelphiVersion: 4; LatestUpdatePack: 3),
    (DelphiVersion: 5; LatestUpdatePack: 1),
    (DelphiVersion: 6; LatestUpdatePack: 2)
  );

resourcestring
  RsIndexOufOfRange = 'Index out of range';
  RsDelphiName      = 'Delphi %d %s';
  RsNeedUpdate      = 'You should install latest Update Pack #%d for %s';
  RsUpdatePackName  = 'Update Pack #%d';

  RsStandard        = 'Standard';
  RsProfessional    = 'Professional';
  RsClientServer    = 'Client/Server';
  RsEnterprise      = 'Enterprise';
  RsPersonal        = 'Personal';

//--------------------------------------------------------------------------------------------------

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
  with Installation do
    Result := PathAddSeparator(RootDir) + Format(DelphiHelpContentFileName, [VersionNumber]);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiOpenHelp.GetGidFileName: string;
begin
  with Installation do
    Result := PathAddSeparator(RootDir) + Format(DelphiHelpGidFileName, [VersionNumber]);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiOpenHelp.GetIndexFileName: string;
begin
  with Installation do
    Result := PathAddSeparator(RootDir) + Format(DelphiHelpIndexFileName, [VersionNumber]);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiOpenHelp.GetLinkFileName: string;
begin
  with Installation do
    Result := PathAddSeparator(RootDir) + Format(DelphiHelpLinkFileName, [VersionNumber]);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiOpenHelp.GetProjectFileName: string;
begin
  with Installation do
    Result := PathAddSeparator(RootDir) + Format(DelphiHelpProjectFileName, [VersionNumber]);
end;

//--------------------------------------------------------------------------------------------------

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
  FRegKey := Installation.RegKey + '\' + TransferKeyName;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiIdeTool.GetCount: Integer;
begin
  Result := RegReadIntegerDef(HKEY_CURRENT_USER, RegKey, TransferCountValueName, 0);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiIdeTool.GetParameters(Index: Integer): string;
begin
  CheckIndex(Index);
  Result := RegReadStringDef(HKEY_CURRENT_USER, RegKey, Format(TransferParamsValueName, [Index]), '');
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiIdeTool.GetPath(Index: Integer): string;
begin
  CheckIndex(Index);
  Result := RegReadStringDef(HKEY_CURRENT_USER, RegKey, Format(TransferPathValueName, [Index]), '');
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiIdeTool.GetTitle(Index: Integer): string;
begin
  CheckIndex(Index);
  Result := RegReadStringDef(HKEY_CURRENT_USER, RegKey, Format(TransferTitleValueName, [Index]), '');
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiIdeTool.GetWorkingDir(Index: Integer): string;
begin
  CheckIndex(Index);
  Result := RegReadStringDef(HKEY_CURRENT_USER, RegKey, Format(TransferWorkDirValueName, [Index]), '');
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
    RegWriteInteger(HKEY_CURRENT_USER, RegKey, TransferCountValueName, Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiIdeTool.SetParameters(Index: Integer; const Value: string);
begin
  CheckIndex(Index);
  RegWriteString(HKEY_CURRENT_USER, RegKey, Format(TransferParamsValueName, [Index]), Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiIdeTool.SetPath(Index: Integer; const Value: string);
begin
  CheckIndex(Index);
  RegWriteString(HKEY_CURRENT_USER, RegKey, Format(TransferPathValueName, [Index]), Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiIdeTool.SetTitle(Index: Integer; const Value: string);
begin
  CheckIndex(Index);
  RegWriteString(HKEY_CURRENT_USER, RegKey, Format(TransferTitleValueName, [Index]), Value);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiIdeTool.SetWorkingDir(Index: Integer; const Value: string);
begin
  CheckIndex(Index);
  RegWriteString(HKEY_CURRENT_USER, RegKey, Format(TransferWorkDirValueName, [Index]), Value);
end;

//==================================================================================================
// TJclDelphiIdePackages
//==================================================================================================

function TJclDelphiIdePackages.AddPackage(const FileName, Description: string): Boolean;
begin
  Result := FDisabledPackages.IndexOfName(FileName) = -1;
  if Result then
  begin
    RegWriteString(HKEY_CURRENT_USER,  Installation.RegKey + '\' + KnownPackagesKeyName,
      FileName, Description); 
    ReadPackages;
  end;
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
  Result := PathGetLongName2(Installation.SubstitutePath(FKnownPackages.Names[Index]));
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiIdePackages.ReadPackages;
var
  I: Integer;
begin
  FDisabledPackages.Clear;
  FKnownPackages.Clear;
  if RegGetValueNamesAndValues(HKEY_CURRENT_USER, Installation.RegKey + '\' + KnownPackagesKeyName, FKnownPackages) and
    RegGetValueNamesAndValues(HKEY_CURRENT_USER, Installation.RegKey + '\' + DisabledPackagesKeyName, FDisabledPackages) then
      for I := 0 to Count - 1 do
        if FDisabledPackages.IndexOfName(FKnownPackages.Names[I]) <> -1 then
          FKnownPackages.Objects[I] := Pointer(True);
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
const
  DCC32CFGFileName = 'DCC32.CFG';
var
  Cmd: string;
begin
  FOptions.SaveToFile(DCC32CFGFileName);
  Cmd := Format('"%s" "%s"', [DCC32Location, CommandLine]);
  Result := WinExec32AndWait(Cmd, SW_HIDE) = 0;
  DeleteFile(DCC32CFGFileName);
end;

//--------------------------------------------------------------------------------------------------

constructor TJclDelphiCompiler.Create(AInstallation: TJclDelphiInstallation);
begin
  inherited;
  FOptions := TStringList.Create;
  FDCC32Location := PathAddSeparator(Installation.RootDir) + DCC32FileName;
end;

//--------------------------------------------------------------------------------------------------

destructor TJclDelphiCompiler.Destroy;
begin
  FreeAndNil(FOptions);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiCompiler.InstallPackage(const PackageName: string): Boolean;
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
  DOFFile: TIniFile;
  DPKFile: TStringList;
  I: Integer;
begin
  PackagePath := PathRemoveSeparator(ExtractFilePath(PackageName));
  SaveDir := GetCurrentDir;
  Win32Check(SetCurrentDir(PackagePath));
  try
    DOFFile := TIniFile.Create(ChangeFileExt(PackageName, '.dof'));
    try
      Options.Clear;
      AddPathOption('N', DOFFile.ReadString(DOFDirectoriesSection, UnitOutputDirName, ''));
      AddPathOption('I', DOFFile.ReadString(DOFDirectoriesSection, SearchPathName, ''));
      AddPathOption('R', DOFFile.ReadString(DOFDirectoriesSection, SearchPathName, ''));
      AddPathOption('LE', Installation.BPLOutputPath);
      AddPathOption('LN', Installation.DCPOutputPath);
      AddPathOption('U', Installation.DCPOutputPath);
    finally
      DOFFile.Free;
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
        BPLFileName := PathAddSeparator(Installation.BPLOutputPath) + PathExtractFileNameNoExt(PackageName) + LibSuffix + '.bpl';
        Result := Installation.IdePackages.AddPackage(BPLFileName, Description);
      end;  
    finally
      DPKFile.Free;
    end;
  end;
end;

//==================================================================================================
// TJclDelphiRepository
//==================================================================================================

procedure TJclDelphiRepository.AddObject(const FileName, ObjectType, PageName, ObjectName,
  IconFileName, Description, Author, Designer: string);
begin
  GetIniFile;
  FIniFile.WriteString(FileName, DelphiRepositoryObjectType, ObjectType);
  FIniFile.WriteString(FileName, DelphiRepositoryObjectName, ObjectName);
  FIniFile.WriteString(FileName, DelphiRepositoryObjectPage, PageName);
  FIniFile.WriteString(FileName, DelphiRepositoryObjectIcon, IconFileName);
  FIniFile.WriteString(FileName, DelphiRepositoryObjectDescr, Description);
  FIniFile.WriteString(FileName, DelphiRepositoryObjectAuthor, Author);
  if Installation.VersionNumber >= 6 then
    FIniFile.WriteString(FileName, DelphiRepositoryObjectDesigner, Designer);
  FIniFile.WriteBool(FileName, DelphiRepositoryObjectNewForm, False);
  FIniFile.WriteBool(FileName, DelphiRepositoryObjectMainForm, False);
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
  FFileName := PathAddSeparator(Installation.RootDir) + DelphiRepositoryFileName;
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

//==================================================================================================
// TJclDelphiInstallation
//==================================================================================================

function TJclDelphiInstallation.AddToLibrarySearchPath(const Path: string): Boolean;
var
  Items: TStringList;
  TempLibraryPath: TJclDelphiPath;
begin
  TempLibraryPath := LibrarySearchPath;
  Items := TStringList.Create;
  try
    ExtractPaths(TempLibraryPath, Items);
    Result := FindFolderInDelphiPath(Path, Items) = -1;
    if Result then
    begin
      TempLibraryPath := StrEnsureSuffix(DelphiLibraryPathSeparator, TempLibraryPath) + Path;
      LibrarySearchPath := TempLibraryPath;
    end;
  finally
    Items.Free;
  end;
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

constructor TJclDelphiInstallation.Create(const ARegKey: string);
begin
  FRegKey := ARegKey;
  FRegKeyValues := TStringList.Create;
  ReadInformation;
  FIdeTools := TJclDelphiIdeTool.Create(Self);
  FOpenHelp := TJclDelphiOpenHelp.Create(Self);
end;

//--------------------------------------------------------------------------------------------------

destructor TJclDelphiInstallation.Destroy;
begin
  FreeAndNil(FRegKeyValues);
  FreeAndNil(FRepository);
  FreeAndNil(FCompiler);
  FreeAndNil(FIdePackages);
  FreeAndNil(FIdeTools);
  FreeAndNil(FOpenHelp);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

class procedure TJclDelphiInstallation.ExtractPaths(const Path: TJclDelphiPath; List: TStrings);
begin
  StrToStrings(Path, DelphiLibraryPathSeparator, List);
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
  Result := SubstitutePath(RegReadStringDef(HKEY_CURRENT_USER, RegKey + '\' + LibraryKeyName,
    LibraryBPLOutputValueName, ''));
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetComplier: TJclDelphiCompiler;
begin
  if not Assigned(FCompiler) then
    FCompiler := TJclDelphiCompiler.Create(Self);
  Result := FCompiler;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetDCPOutputPath: string;
begin
  Result := SubstitutePath(RegReadStringDef(HKEY_CURRENT_USER, RegKey + '\' + LibraryKeyName,
    LibraryDCPOutputValueName, ''));
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetEditionAsText: string;
begin
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
    if VersionNumber >= 6 then
    begin
      EnvNames := TStringList.Create;
      try
        EnvVarKeyName := RegKey + '\' + EnvVariablesKeyName;
        if RegKeyExists(HKEY_CURRENT_USER, EnvVarKeyName) and RegGetValueNames(HKEY_CURRENT_USER, EnvVarKeyName, EnvNames) then
          for I := 0 to EnvNames.Count - 1 do
            FEnvironmentVariables.Values[EnvNames[I]] := RegReadStringDef(HKEY_CURRENT_USER, EnvVarKeyName, EnvNames[I], '');
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
var
  FixedInfo: TVSFixedFileInfo;
begin
  if VersionFixedFileInfo(IdeExeFileName, FixedInfo) then
    Result := FormatVersionString(LongRec(FixedInfo.dwFileVersionLS).Hi, LongRec(FixedInfo.dwFileVersionLS).Lo)
  else
    Result := '';   
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
  Result := RegReadStringDef(HKEY_CURRENT_USER, RegKey + '\' + LibraryKeyName, LibrarySearchPathValueName, '');
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetName: string;
begin
  Result := Format(RsDelphiName, [VersionNumber, EditionAsText]);
  if InstalledUpdatePack > 0 then
    Result := Result + ' ' + Format(RsUpdatePackName, [InstalledUpdatePack]);
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
  Result := (RegKey <> '') and (RootDir <> '') and FileExists(IdeExeFileName);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclDelphiInstallation.ReadInformation;
const
  UpdateKeyName = 'Update #';
  EditionNames: array [TJclDelphiEdition] of PChar = ('STD', 'PRO', 'CSS');
var
  Ed: TJclDelphiEdition;
  KeyLen, I: Integer;
  KeyName: string;
begin
  FRootDir := RegReadStringDef(HKEY_LOCAL_MACHINE, RegKey, RootDirValueName, '');
  FBinFolderName := PathAddSeparator(RootDir) + 'Bin\';
  FIdeExeFileName := PathAddSeparator(RootDir) + DelphiIdeFileName;
  KeyName := RegReadStringDef(HKEY_LOCAL_MACHINE, RegKey, VersionValueName, '');
  for Ed := Low(Ed) to High(Ed) do
    if EditionNames[Ed] = KeyName then
      FEdition := Ed;
  KeyLen := Length(FRegKey);
  if (KeyLen > 3) and StrIsDigit(FRegKey[KeyLen - 2]) and (FRegKey[KeyLen - 1] = '.') and (FRegKey[KeyLen] = '0') then
    FVersionNumber := Ord(FRegKey[KeyLen - 2]) - 48
  else
    FVersionNumber := 0;
  if RegGetValueNamesAndValues(HKEY_LOCAL_MACHINE, RegKey, FRegKeyValues) then
    for I := 0 to RegKeyValues.Count - 1 do
    begin
      KeyName := RegKeyValues.Names[I];
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

procedure TJclDelphiInstallation.SetLibrarySearchPath(const Value: TJclDelphiPath);
begin
  RegWriteString(HKEY_CURRENT_USER, RegKey + '\' + LibraryKeyName, LibrarySearchPathValueName, Value);
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
var
  List: TStringList;
  I: Integer;
  VersionKeyName: string;
  Item: TJclDelphiInstallation;
begin
  FList.Clear;
  List := TStringList.Create;
  try
    if RegGetKeyNames(HKEY_LOCAL_MACHINE, DelphiKeyName, List) then
      for I := 0 to List.Count - 1 do
      begin
        VersionKeyName := DelphiKeyName + '\' + List[I];
        if RegKeyExists(HKEY_LOCAL_MACHINE, VersionKeyName) then
        begin
          Item := TJclDelphiInstallation.Create(VersionKeyName);
          FList.Add(Item);
        end;
      end;
  finally
    List.Free;
  end;
end;

//--------------------------------------------------------------------------------------------------


end.
