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
{ The Original Code is JclOtaUtils.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Unit owner: Petr Vones                                                                           }
{ Last modified: July 20, 2003                                                                     }
{                                                                                                  }
{**************************************************************************************************}

unit JclOtaUtils;

interface

{$I jcl.inc}
{$I windowsonly.inc}

{$WEAKPACKAGEUNIT ON}

uses
  Windows, Classes, IniFiles, ToolsAPI;

const
  MapFileOptionName      = 'MapFile';
  OutputDirOptionName    = 'OutputDir';
  RuntimeOnlyOptionName  = 'RuntimeOnly';
  PkgDllDirOptionName    = 'PkgDllDir';
  BPLOutputDirOptionName = 'PackageDPLOutput';
  LIBPREFIXOptionName    = 'SOPrefix';
  LIBSUFFIXOptionName    = 'SOSuffix';

  MapFileOptionDetailed  = 3;

  BPLExtension           = '.bpl';
  DPKExtension           = '.dpk';
  MAPExtension           = '.map';
  DRCExtension           = '.drc';
  DPRExtention           = '.dpr';

type
  TJclOTAUtils = class (TInterfacedObject)
  private
    FBaseRegistryKey: string;
    FEnvVariables: TStringList;
    FJediIniFile: TIniFile;
    FRootDir: string;
    FServices: IOTAServices;
    function GetActiveProject: IOTAProject;
    function GetJediIniFile: TIniFile;
    function GetProjectGroup: IOTAProjectGroup;
    function GetRootDir: string;
    procedure ReadEnvVariables;
  public
    constructor Create;
    destructor Destroy; override;
    function FindExecutableName(const MapFileName, OutputDirectory: string; var ExecutableFileName: string): Boolean;
    function GetDrcFileName(const Project: IOTAProject): string;
    function GetMapFileName(const Project: IOTAProject): string;
    function GetOutputDirectory(const Project: IOTAProject): string;
    function IsInstalledPackage(const Project: IOTAProject): Boolean;
    function IsPackage(const Project: IOTAProject): Boolean;
    function SubstitutePath(const Path: string): string;
    property ActiveProject: IOTAProject read GetActiveProject;
    property BaseRegistryKey: string read FBaseRegistryKey;
    property JediIniFile: TIniFile read GetJediIniFile; 
    property ProjectGroup: IOTAProjectGroup read GetProjectGroup;
    property RootDir: string read GetRootDir;
    property Services: IOTAServices read FServices;
  end;

  TJclOTAExpert = class(TJclOTAUtils, IOTAWizard)
  protected
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    procedure Execute;
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
  end;

procedure SaveOptions(const Options: IOTAOptions; const FileName: string);

implementation

uses
  {$IFDEF DELPHI6_UP}
  Variants,
  {$ENDIF DELPHI6_UP}
  SysUtils, ImageHlp,
  JclFileUtils, JclRegistry, JclStrings;

//==================================================================================================
// TJclOTAUtils
//==================================================================================================

constructor TJclOTAUtils.Create;
begin
  FEnvVariables := TStringList.Create;
  FServices := BorlandIDEServices as IOTAServices;
  FBaseRegistryKey := StrEnsureSuffix('\', FServices.GetBaseRegistryKey);
end;

//--------------------------------------------------------------------------------------------------

destructor TJclOTAUtils.Destroy;
begin
  FreeAndNil(FEnvVariables);
  FreeAndNil(FJediIniFile);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

function TJclOTAUtils.FindExecutableName(const MapFileName, OutputDirectory: string;
  var ExecutableFileName: string): Boolean;
var
  Se: TSearchRec;
  Res: Integer;
  LatestTime: Integer;
  FileName: TFileName;
  LI: LoadedImage;
begin
  LatestTime := 0;
  ExecutableFileName := '';
  // the latest executable file is very likely our file
  Res := FindFirst(ChangeFileExt(MapFileName, '.*'), faArchive, Se);
  while Res = 0 do
  begin
    FileName := PathAddSeparator(OutputDirectory) + Se.Name;
    if MapAndLoad(PChar(FileName), nil, @LI, False, True) then
    begin
      if (not LI.fDOSImage) and (Se.Time > LatestTime) then
      begin
        ExecutableFileName := FileName;
        LatestTime := Se.Time;
      end;
      UnMapAndLoad(@LI);
    end;
    Res := FindNext(Se);
  end;
  FindClose(Se);
  Result := (ExecutableFileName <> '');
end;

//--------------------------------------------------------------------------------------------------

function TJclOTAUtils.GetActiveProject: IOTAProject;
var
  TempProjectGroup: IOTAProjectGroup;
begin
  TempProjectGroup := ProjectGroup;
  if Assigned(TempProjectGroup) then
    Result := TempProjectGroup.ActiveProject
  else
    Result := nil;
end;

//--------------------------------------------------------------------------------------------------

function TJclOTAUtils.GetDrcFileName(const Project: IOTAProject): string;
begin
  Result := ChangeFileExt(Project.FileName, DRCExtension);
end;

//--------------------------------------------------------------------------------------------------

function TJclOTAUtils.GetJediIniFile: TIniFile;
const
  JediIniFileName = 'Bin\JediOTA.ini';
begin
  if not Assigned(FJediIniFile) then
    FJediIniFile := TIniFile.Create(PathAddSeparator(RootDir) + JediIniFileName);
  Result := FJediIniFile;  
end;

//--------------------------------------------------------------------------------------------------

function TJclOTAUtils.GetMapFileName(const Project: IOTAProject): string;
var
  ProjectFileName, OutputDirectory, LibPrefix, LibSuffix: string;
begin
  ProjectFileName := Project.FileName;
  OutputDirectory := GetOutputDirectory(Project);
  {$IFDEF DELPHI6_UP}
  LibPrefix := Trim(VarToStr(Project.ProjectOptions.Values[LIBPREFIXOptionName]));
  LibSuffix := Trim(VarToStr(Project.ProjectOptions.Values[LIBSUFFIXOptionName]));
  {$ELSE DELPHI6_UP}
  LibPrefix := '';
  LibSuffix := '';
  {$ENDIF DELPHI6_UP}
  Result := PathAddSeparator(OutputDirectory) + LibPrefix + PathExtractFileNameNoExt(ProjectFileName) +
    LibSuffix + MAPExtension;
end;

//--------------------------------------------------------------------------------------------------

function TJclOTAUtils.GetOutputDirectory(const Project: IOTAProject): string;
begin
  if IsPackage(Project) then
  begin
    Result := VarToStr(Project.ProjectOptions.Values[PkgDllDirOptionName]);
    if Result = '' then
      Result := FServices.GetEnvironmentOptions.Values[BPLOutputDirOptionName];
  end
  else
    Result := VarToStr(Project.ProjectOptions.Values[OutputDirOptionName]);
  Result := SubstitutePath(Trim(Result));
  if Result = '' then
    Result := ExtractFilePath(Project.FileName);
end;

//--------------------------------------------------------------------------------------------------

function TJclOTAUtils.GetProjectGroup: IOTAProjectGroup;
var
  IModuleServices: IOTAModuleServices;
  I: Integer;
begin
  IModuleServices := BorlandIDEServices as IOTAModuleServices;
  for I := 0 to IModuleServices.ModuleCount - 1 do
    if IModuleServices.Modules[I].QueryInterface(IOTAProjectGroup, Result) = S_OK then
      Exit;
  Result := nil;
end;

//--------------------------------------------------------------------------------------------------

function TJclOTAUtils.GetRootDir: string;
const
  cRootDir = 'RootDir';
begin
  if FRootDir = '' then
  begin
    FRootDir := RegReadStringDef(HKEY_LOCAL_MACHINE, BaseRegistryKey, cRootDir, '');
    // (rom) bugfix if using -r switch of D9 by Dan Miser
    if FRootDir = '' then
      FRootDir := RegReadStringDef(HKEY_CURRENT_USER, BaseRegistryKey, cRootDir, '');
    Assert(FRootDir <> '');
  end;  
  Result := FRootDir;
end;

//--------------------------------------------------------------------------------------------------

function TJclOTAUtils.IsInstalledPackage(const Project: IOTAProject): Boolean;
var
  PackageFileName, ExecutableNameNoExt: string;
  PackageServices: IOTAPackageServices;
  I: Integer;
begin
  Result := IsPackage(Project);
  if Result then
  begin
    Result := False;
    if not Project.ProjectOptions.Values[RuntimeOnlyOptionName] then
    begin
      ExecutableNameNoExt := ChangeFileExt(GetMapFileName(Project), '');
      PackageServices := BorlandIDEServices as IOTAPackageServices;
      for I := 0 to PackageServices.PackageCount - 1 do
      begin
        PackageFileName := ChangeFileExt(PackageServices.PackageNames[I], BPLExtension);
        PackageFileName := GetModulePath(GetModuleHandle(PChar(PackageFileName)));
        if AnsiSameText(ChangeFileExt(PackageFileName, ''), ExecutableNameNoExt) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  end;
end;

//--------------------------------------------------------------------------------------------------

function TJclOTAUtils.IsPackage(const Project: IOTAProject): Boolean;
begin
  Result := AnsiSameText(ExtractFileExt(Project.FileName), DPKExtension);
end;

//--------------------------------------------------------------------------------------------------

procedure TJclOTAUtils.ReadEnvVariables;
{$IFDEF DELPHI6_UP}
const
  EnvironmentVarsKey = 'Environment Variables';
var
  EnvNames: TStringList;
  I: Integer;
  EnvVarKeyName: string;
{$ENDIF DELPHI6_UP}
begin
  FEnvVariables.Clear;
  {$IFDEF DELPHI6_UP}
  EnvNames := TStringList.Create;
  try
    EnvVarKeyName := BaseRegistryKey + EnvironmentVarsKey;
    if RegKeyExists(HKEY_CURRENT_USER, EnvVarKeyName) and RegGetValueNames(HKEY_CURRENT_USER, EnvVarKeyName, EnvNames) then
      for I := 0 to EnvNames.Count - 1 do
        FEnvVariables.Values[EnvNames[I]] := RegReadStringDef(HKEY_CURRENT_USER, EnvVarKeyName, EnvNames[I], '');
  finally
    EnvNames.Free;
  end;
  {$ENDIF DELPHI6_UP}
  FEnvVariables.Values['DELPHI'] := RootDir;
end;

//--------------------------------------------------------------------------------------------------

function TJclOTAUtils.SubstitutePath(const Path: string): string;
var
  I: Integer;
  Name: string;
begin
  if FEnvVariables.Count = 0 then
    ReadEnvVariables;
  Result := Path;
  if Pos('$(', Result) > 0 then
    for I := 0 to FEnvVariables.Count - 1 do
    begin
      Name := FEnvVariables.Names[I];
      Result := StringReplace(Result, Format('$(%s)', [Name]), FEnvVariables.Values[Name], [rfReplaceAll, rfIgnoreCase]);
    end;
end;

//==================================================================================================
// TJclOTAExpert
//==================================================================================================

procedure TJclOTAExpert.AfterSave;
begin
end;

//--------------------------------------------------------------------------------------------------

procedure TJclOTAExpert.BeforeSave;
begin
end;

//--------------------------------------------------------------------------------------------------

procedure TJclOTAExpert.Destroyed;
begin
end;

//--------------------------------------------------------------------------------------------------

procedure TJclOTAExpert.Execute;
begin
end;

//--------------------------------------------------------------------------------------------------

function TJclOTAExpert.GetIDString: string;
begin
  Result := 'Jedi.' + ClassName;
end;

//--------------------------------------------------------------------------------------------------

function TJclOTAExpert.GetName: string;
begin
  Result := ClassName;
end;

//--------------------------------------------------------------------------------------------------

function TJclOTAExpert.GetState: TWizardState;
begin
  Result := [];
end;

//--------------------------------------------------------------------------------------------------

procedure TJclOTAExpert.Modified;
begin
end;

//==================================================================================================
// Helper routines
//==================================================================================================

procedure SaveOptions(const Options: IOTAOptions; const FileName: string);
var
  OptArray: TOTAOptionNameArray;
  I: Integer;
begin
  OptArray := Options.GetOptionNames;
  with TStringList.Create do
  try
    for I := Low(OptArray) to High(OptArray) do
      Add(OptArray[I].Name + '=' + VarToStr(Options.Values[OptArray[I].Name]));
    SaveToFile(FileName);
  finally
    Free;
  end;
end;

//--------------------------------------------------------------------------------------------------


end.
