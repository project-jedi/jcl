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
{ Routines for getting infomation about installed versions of Delphi                               }
{                                                                                                  }
{ Unit owner: Petr Vones                                                                           }
{ Last modified: March 25, 2002                                                                    }
{                                                                                                  }
{**************************************************************************************************}

unit DelphiInstall;

{$I jcl.inc}

{$WEAKPACKAGEUNIT ON}

interface

uses
  Windows, Classes, SysUtils,
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

  TJclDelphiOpenHelp = class (TObject)
  private
    FInstallation: TJclDelphiInstallation;
    function GetContentFileName: string;
    function GetIndexFileName: string;
    function GetLinkFileName: string;
    function GetGidFileName: string;
    function GetProjectFileName: string;
  protected
    constructor Create(AInstallation: TJclDelphiInstallation);
  public
    function AddHelpFile(const HelpFileName, IndexName: string): Boolean;
    property ContentFileName: string read GetContentFileName;
    property GidFileName: string read GetGidFileName;
    property IndexFileName: string read GetIndexFileName;
    property LinkFileName: string read GetLinkFileName;
    property ProjectFileName: string read GetProjectFileName;
  end;

  TJclDelphiIdeTool = class (TObject)
  private
    FInstallation: TJclDelphiInstallation;
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

  TJclDelphiInstallation = class (TObject)
  private
    FBinFolderName: string;
    FEdition: TJclDelphiEdition;
    FEnvironmentVariables: TStrings;
    FIdeExeFileName: string;
    FIdeTools: TJclDelphiIdeTool;
    FInstalledUpdatePack: Integer;
    FLatestUpdatePack: Integer;
    FOpenHelp: TJclDelphiOpenHelp;
    FRegKey: string;
    FRegKeyValues: TStrings;
    FRootDir: string;
    FVersionNumber: Byte;
    function GetEnvironmentVariables: TStrings;
    function GetIdeExeBuildNumber: string;
    function GetLibrarySearchPath: TJclDelphiPath;
    function GetObjectRepositoryFileName: string;
    function GetName: string;
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
    function FindFolderInDelphiPath(Folder: string; List: TStrings): Integer;
    function GetKnownPackages(List: TStrings): Boolean;
    function SubstitutePath(const Path: string): string;
    property BinFolderName: string read FBinFolderName;
    property Edition: TJclDelphiEdition read FEdition;
    property EnvironmentVariables: TStrings read GetEnvironmentVariables;
    property IdeTools: TJclDelphiIdeTool read FIdeTools;
    property IdeExeBuildNumber: string read GetIdeExeBuildNumber;
    property IdeExeFileName: string read FIdeExeFileName;
    property InstalledUpdatePack: Integer read FInstalledUpdatePack;
    property LatestUpdatePack: Integer read FLatestUpdatePack;
    property LibrarySearchPath: TJclDelphiPath read GetLibrarySearchPath write SetLibrarySearchPath;
    property ObjectRepositoryFileName: string read GetObjectRepositoryFileName;
    property OpenHelp: TJclDelphiOpenHelp read FOpenHelp;
    property Name: string read GetName;
    property RegKey: string read FRegKey;
    property RegKeyValues: TStrings read FRegKeyValues;
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
  JclFileUtils, JclLogic, JclRegistry, JclStrings, JclSysInfo, JclSysUtils;

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
  KnowPackagesValueName      = 'Known Packages';
  VersionValueName           = 'Version';

  LibraryKeyName             = 'Library';
  LibrarySearchPathValueName = 'Search Path';

  TransferKeyName            = 'Transfer';
  TransferCountValueName     = 'Count';
  TransferPathValueName      = 'Path%d';
  TransferParamsValueName    = 'Params%d';
  TransferTitleValueName     = 'Title%d';
  TransferWorkDirValueName   = 'WorkingDir%d';

  EnvVariablesKeyName        = 'Environment Variables';

  DelphiIdeFileName          = 'Bin\delphi32.exe';
  DelphiRepositoryFileName   = 'Bin\delphi32.dro';
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
  RsDelphiName      = 'Delphi %d';
  RsNeedUpdate      = 'You should install latest Update Pack #%d for %s';
  RsUpdatePackName  = 'Update Pack #%d';

//--------------------------------------------------------------------------------------------------

function RegGetValueNamesAndValues(const RootKey: HKEY; const Key: string; const List: TStrings): Boolean;
var
  OldPos, I: Integer;
begin
  OldPos := List.Count;
  Result := RegGetValueNames(RootKey, Key, List);
  if Result then
    for I := OldPos to List.Count - 1 do
      List[I] := List[I] + '=' + RegReadStringDef(RootKey, Key, List[I], '');
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

constructor TJclDelphiOpenHelp.Create(AInstallation: TJclDelphiInstallation);
begin
  FInstallation := AInstallation;
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiOpenHelp.GetContentFileName: string;
begin
  with FInstallation do
    Result := PathAddSeparator(RootDir) + Format(DelphiHelpContentFileName, [VersionNumber]);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiOpenHelp.GetGidFileName: string;
begin
  with FInstallation do
    Result := PathAddSeparator(RootDir) + Format(DelphiHelpGidFileName, [VersionNumber]);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiOpenHelp.GetIndexFileName: string;
begin
  with FInstallation do
    Result := PathAddSeparator(RootDir) + Format(DelphiHelpIndexFileName, [VersionNumber]);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiOpenHelp.GetLinkFileName: string;
begin
  with FInstallation do
    Result := PathAddSeparator(RootDir) + Format(DelphiHelpLinkFileName, [VersionNumber]);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiOpenHelp.GetProjectFileName: string;
begin
  with FInstallation do
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
  FInstallation := AInstallation;
  FRegKey := FInstallation.RegKey + '\' + TransferKeyName;
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
// TJclDelphiInstallation
//==================================================================================================

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

function TJclDelphiInstallation.GetKnownPackages(List: TStrings): Boolean;
begin
  Result := RegGetValueNamesAndValues(HKEY_CURRENT_USER, RegKey + '\' + KnowPackagesValueName, List);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetLibrarySearchPath: TJclDelphiPath;
begin
  Result := RegReadStringDef(HKEY_CURRENT_USER, RegKey + '\' + LibraryKeyName, LibrarySearchPathValueName, '');
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetName: string;
begin
  Result := Format(RsDelphiName, [VersionNumber]);
  if InstalledUpdatePack > 0 then
    Result := Result + ' ' + Format(RsUpdatePackName, [InstalledUpdatePack]);
end;

//--------------------------------------------------------------------------------------------------

function TJclDelphiInstallation.GetObjectRepositoryFileName: string;
begin
  Result := PathAddSeparator(RootDir) + DelphiRepositoryFileName;
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
