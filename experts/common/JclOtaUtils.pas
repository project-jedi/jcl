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
{ The Initial Developer of the Original Code is Petr Vones.                                        }
{ Portions created by Petr Vones are Copyright (C) of Petr Vones.                                  }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Florent Ouchet (outchy)                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Unit owner: Florent Ouchet                                                                       }
{ Last modified: $Date$                                                      }
{                                                                                                  }
{**************************************************************************************************}

unit JclOtaUtils;

interface

{$I jcl.inc}
{$I crossplatform.inc}

uses
  SysUtils, Classes, Windows, 
  ComCtrls, ActnList,
{$IFDEF MSWINDOWS}
  JclDebug,
{$ENDIF MSWINDOWS}
  ToolsAPI;

const
  MapFileOptionDetailed = 3;

type
// note to developers
// to avoid JCL exceptions to be reported as Borland's exceptions in automatic
// bug reports, all entry points should be protected with this code model:
// uses
//   JclOtaUtils;
// try
//   <code to execute here>
// except
//   on ExceptionObj: TObject do
//   begin
//     JclExpertShowExceptionDialog(ExceptionObj);
//     raise;
//   end;
// end;
// entry points for experts are usually:
//  - initialization sections
//  - finalization sections
//  - Register procedures
//  - expert entry point
//  - Action update events
//  - Action execute events
//  - notifier callback functions
//  - ... (non exhaustive list)

  EJclExpertException = class (Exception)
  {$IFDEF MSWINDOWS}
  private
    FStackInfo: TJclStackInfoList;
  {$ENDIF MSWINDOWS}
  public
    constructor CreateTrace(const Msg: string);
  {$IFDEF MSWINDOWS}
    destructor Destroy; override;
    property StackInfo: TJclStackInfoList read FStackInfo;
  {$ENDIF MSWINDOWS}
  end;

  TJclOTAExpertBase = class(TInterfacedObject)
  private
    FBaseRegistryKey: string;
    FExpertRegistryKey: string;
    FEnvVariables: TStringList;
    FRootDir: string;
    FServices: IOTAServices;
    FName: string;
    FNTAServices: INTAServices;
    function GetModuleHInstance: Cardinal;
    function GetActiveProject: IOTAProject;
    function GetProjectGroup: IOTAProjectGroup;
    function GetRootDir: string;
    procedure ReadEnvVariables;

    procedure CheckToolBarButton(AToolBar: TToolBar; AAction: TCustomAction);
  public
    constructor Create(AName: string); virtual;
    destructor Destroy; override;
    function FindExecutableName(const MapFileName, OutputDirectory: string;
      var ExecutableFileName: string): Boolean;
    function GetDrcFileName(const Project: IOTAProject): string;
    function GetMapFileName(const Project: IOTAProject): string;
    function GetOutputDirectory(const Project: IOTAProject): string;
    function IsInstalledPackage(const Project: IOTAProject): Boolean;
    function IsPackage(const Project: IOTAProject): Boolean;
    function SubstitutePath(const Path: string): string;

    procedure RegisterAction(Action: TCustomAction);
    procedure UnregisterAction(Action: TCustomAction);
    procedure RegisterCommands; virtual;
    procedure UnregisterCommands; virtual;

    function LoadBool(Name: string; Def: Boolean): Boolean;
    function LoadString(Name: string; Def: string): string;
    function LoadInteger(Name: string; Def: Integer): Integer;
    procedure LoadStrings(Name: string; List: TStrings);
    procedure SaveBool(Name: string; Value: Boolean);
    procedure SaveString(Name: string; Value: string);
    procedure SaveInteger(Name: string; Value: Integer);
    procedure SaveStrings(Name: string; List: TStrings);

    property ActiveProject: IOTAProject read GetActiveProject;
    property BaseRegistryKey: string read FBaseRegistryKey;
    property ExpertRegistryKey: string read FExpertRegistryKey;
    property Name: string read FName;
    property NTAServices: INTAServices read FNTAServices;
    property ProjectGroup: IOTAProjectGroup read GetProjectGroup;
    property RootDir: string read GetRootDir;
    property Services: IOTAServices read FServices;

    property ModuleHInstance: Cardinal read GetModuleHInstance;
  end;

  TJclOTAExpert = class(TJclOTAExpertBase, IOTAWizard)
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

// procedure SaveOptions(const Options: IOTAOptions; const FileName: string);
function JclExpertShowExceptionDialog(AExceptionObj: TObject): Boolean;

{$IFDEF BDS}
procedure RegisterSplashScreen;
procedure RegisterAboutBox;
{$ENDIF BDS}

implementation

uses
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  Forms,
  {$IFDEF MSWINDOWS}
  ImageHlp, JclRegistry,
  {$ENDIF MSWINDOWS}
  {$IFDEF KYLIX}
  JclBorlandTools,
  {$ENDIF KYLIX}
  JclFileUtils, JclStrings, JclSysInfo,
  JclOtaConsts, JclOtaResources, JclOtaExceptionForm;

{$IFDEF BDS}
{$R 'JclImages.res'}
{$ENDIF BDS}

var
  ActionList: TList = nil;
  {$IFNDEF COMPILER6_UP}
  OldFindGlobalComponentProc: TFindGlobalComponent = nil;
  {$ENDIF COMPILER6_UP}

function FindActions(const Name: string): TComponent;
var
  Index: Integer;
  TestAction: TCustomAction;
begin
  Result := nil;
  if Assigned(ActionList) then
    for Index := 0 to ActionList.Count-1 do
    begin
      TestAction := TCustomAction(ActionList.Items[Index]);
      if (CompareText(Name,TestAction.Name) = 0) then
        Result := TestAction;
    end;
  {$IFNDEF COMPILER6_UP}
  if (not Assigned(Result)) and Assigned(OldFindGlobalComponentProc) then
    Result := OldFindGlobalComponentProc(Name)
  {$ENDIF COMPILER6_UP}
end;

function JclExpertShowExceptionDialog(AExceptionObj: TObject): Boolean;
var
  AJclExpertExceptionForm: TJclExpertExceptionForm;
begin
  AJclExpertExceptionForm := TJclExpertExceptionForm.Create(Application);
  try
    AJclExpertExceptionForm.ShowException(AExceptionObj);
    Result := AJclExpertExceptionForm.Execute;
  finally
    AJclExpertExceptionForm.Free;
  end;
end;

//=== { EJclExpertException } ================================================

constructor EJclExpertException.CreateTrace(const Msg: string);
begin
  inherited Create(Msg);
{$IFDEF MSWINDOWS}
  FStackInfo := JclCreateStackList(False, 0, nil);
{$ENDIF MSWINDOWS}
end;

{$IFDEF MSWINDOWS}
destructor EJclExpertException.Destroy;
begin
  FreeAndNil(FStackInfo);
  inherited Destroy;
end;
{$ENDIF MSWINDOWS}

//=== { TJclOTAExpertBase } ==================================================

constructor TJclOTAExpertBase.Create(AName: string);
begin
  {$IFDEF BDS}
  RegisterSplashScreen;
  RegisterAboutBox;
  {$ENDIF BDS}
  
  Supports(BorlandIDEServices,IOTAServices,FServices);
  if not Assigned(FServices) then
    raise EJclExpertException.CreateTrace(RsENoIDEServices);

  Supports(FServices,INTAServices,FNTAServices);
  if not Assigned(FNTAServices) then
    raise EJclExpertException.CreateTrace(RsENoNTAServices);

  FName := AName;
  FEnvVariables := TStringList.Create;
  FBaseRegistryKey := StrEnsureSuffix('\', FServices.GetBaseRegistryKey);
  FExpertRegistryKey := FBaseRegistryKey + JediIDESubKey + FName;

  RegisterCommands;
end;

destructor TJclOTAExpertBase.Destroy;
begin
  UnRegisterCommands;

  FreeAndNil(FEnvVariables);

  FServices := nil;
  FNTAServices := nil;

  inherited Destroy;
end;

function TJclOTAExpertBase.FindExecutableName(const MapFileName, OutputDirectory: string;
  var ExecutableFileName: string): Boolean;
var
  Se: TSearchRec;
  Res: Integer;
  LatestTime: Integer;
  FileName: TFileName;
  {$IFDEF MSWINDOWS}
  LI: LoadedImage;
  {$ENDIF MSWINDOWS}
begin
  LatestTime := 0;
  ExecutableFileName := '';
  // the latest executable file is very likely our file
  Res := SysUtils.FindFirst(ChangeFileExt(MapFileName, '.*'), faArchive, Se);
  while Res = 0 do
  begin
    FileName := PathAddSeparator(OutputDirectory) + Se.Name;
    {$IFDEF MSWINDOWS}
    if MapAndLoad(PChar(FileName), nil, @LI, False, True) then
    begin
      if (not LI.fDOSImage) and (Se.Time > LatestTime) then
      begin
        ExecutableFileName := FileName;
        LatestTime := Se.Time;
      end;
      UnMapAndLoad(@LI);
    end;
    {$ELSE}
    if Se.Time > LatestTime then
    begin
      ExecutableFileName := FileName;
      LatestTime := Se.Time;
    end;
    {$ENDIF MSWINDOWS}
    Res := SysUtils.FindNext(Se);
  end;
  SysUtils.FindClose(Se);
  Result := (ExecutableFileName <> '');
end;

function TJclOTAExpertBase.GetActiveProject: IOTAProject;
var
  TempProjectGroup: IOTAProjectGroup;
begin
  TempProjectGroup := ProjectGroup;
  if Assigned(TempProjectGroup) then
    Result := TempProjectGroup.ActiveProject
  else
    Result := nil;
end;

function TJclOTAExpertBase.GetDrcFileName(const Project: IOTAProject): string;
begin
  if not Assigned(Project) then
    raise EJclExpertException.CreateTrace(RsENoActiveProject);
    
  Result := ChangeFileExt(Project.FileName, DRCExtension);
end;

function TJclOTAExpertBase.GetMapFileName(const Project: IOTAProject): string;
var
  ProjectFileName, OutputDirectory, LibPrefix, LibSuffix: string;
begin
  if not Assigned(Project) then
    raise EJclExpertException.CreateTrace(RsENoActiveProject);

  ProjectFileName := Project.FileName;
  OutputDirectory := GetOutputDirectory(Project);
  {$IFDEF RTL140_UP}
  if not Assigned(Project.ProjectOptions) then
    raise EJclExpertException.CreateTrace(RsENoProjectOptions);
  LibPrefix := Trim(VarToStr(Project.ProjectOptions.Values[LIBPREFIXOptionName]));
  LibSuffix := Trim(VarToStr(Project.ProjectOptions.Values[LIBSUFFIXOptionName]));
  {$ELSE ~RTL140_UP}
  LibPrefix := '';
  LibSuffix := '';
  {$ENDIF ~RTL140_UP}
  Result := PathAddSeparator(OutputDirectory) + LibPrefix +
    PathExtractFileNameNoExt(ProjectFileName) + LibSuffix + MAPExtension;
end;

function TJclOTAExpertBase.GetModuleHInstance: Cardinal;
begin
  Result := FindClassHInstance(ClassType);
  if Result = 0 then
    raise EJclExpertException.CreateTrace(RsBadModuleHInstance);
end;

function TJclOTAExpertBase.GetOutputDirectory(const Project: IOTAProject): string;
begin
  if not Assigned(Project) then
    raise EJclExpertException.CreateTrace(RsENoActiveProject);
  if not Assigned(Project.ProjectOptions) then
      raise EJclExpertException.CreateTrace(RsENoProjectOptions);

  if IsPackage(Project) then
  begin
    Result := VarToStr(Project.ProjectOptions.Values[PkgDllDirOptionName]);

    if Result = '' then
    begin
      if not Assigned(FServices.GetEnvironmentOptions) then
        raise EJclExpertException.CreateTrace(RsENoEnvironmentOptions);
      Result := FServices.GetEnvironmentOptions.Values[BPLOutputDirOptionName];
    end;
  end
  else
    Result := VarToStr(Project.ProjectOptions.Values[OutputDirOptionName]);

  Result := SubstitutePath(Trim(Result));
  if Result = '' then
    Result := ExtractFilePath(Project.FileName);
end;

function TJclOTAExpertBase.GetProjectGroup: IOTAProjectGroup;
var
  AModuleServices: IOTAModuleServices;
  AModule: IOTAModule;
  I: Integer;
begin
  Supports(BorlandIDEServices, IOTAModuleServices, AModuleServices);
  if not Assigned(AModuleServices) then
    raise EJclExpertException.CreateTrace(RsENoModuleServices);

  for I := 0 to AModuleServices.ModuleCount - 1 do
  begin
    AModule := AModuleServices.Modules[I];
    if not Assigned(AModule) then
      raise EJclExpertException.CreateTrace(RsENoModule);
    if AModule.QueryInterface(IOTAProjectGroup, Result) = S_OK then
      Exit;
  end;

  Result := nil;
end;

function TJclOTAExpertBase.GetRootDir: string;
{$IFDEF KYLIX}
var
  RADToolsInstallations: TJclBorRADToolInstallations;
  RADToolInstallation: TJclBorRADToolInstallation;
{$ENDIF KYLIX}
begin
  if FRootDir = '' then
  begin
    //(usc) another possibility for D7 or higher is to use IOTAServices.GetRootDirectory
    {$IFDEF MSWINDOWS}
    FRootDir := RegReadStringDef(HKEY_LOCAL_MACHINE, BaseRegistryKey, DelphiRootDirKeyValue, '');
    // (rom) bugfix if using -r switch of D9 by Dan Miser
    if FRootDir = '' then
      FRootDir := RegReadStringDef(HKEY_CURRENT_USER, BaseRegistryKey, DelphiRootDirKeyValue, '');
    {$ENDIF MSWINDOWS}
    {$IFDEF KYLIX}
    RADToolsInstallations := TJclBorRADToolInstallations.Create;
    try
      {$IFDEF KYLIX3}
      {$IFDEF BCB}
      RADToolInstallation := RADToolsInstallations.BCBInstallationFromVersion[3];
      {$ELSE}
      RADToolInstallation := RADToolsInstallations.DelphiInstallationFromVersion[3];
      {$ENDIF BCB}
      {$ELSE}
      RADToolInstallation := nil;
      {$ENDIF KYLIX3}
      if Assigned(RADToolInstallation) then
        FRootDir := RADToolInstallation.RootDir;
    finally
      RADToolsInstallations.Free;
    end;
    {$ENDIF KYLIX}
    if FRootDir = '' then
      raise EJclExpertException.CreateTrace(RsENoRootDir);
  end;
  Result := FRootDir;
end;

function TJclOTAExpertBase.IsInstalledPackage(const Project: IOTAProject): Boolean;
var
  PackageFileName, ExecutableNameNoExt: string;
  APackageServices: IOTAPackageServices;
  I: Integer;
begin
  if not Assigned(Project) then
    raise EJclExpertException.CreateTrace(RsENoActiveProject);

  Result := IsPackage(Project);
  if Result then
  begin
    Result := False;

    if not Assigned(Project.ProjectOptions) then
      raise EJclExpertException.CreateTrace(RsENoProjectOptions);

    if not Project.ProjectOptions.Values[RuntimeOnlyOptionName] then
    begin
      ExecutableNameNoExt := ChangeFileExt(GetMapFileName(Project), '');

      Supports(BorlandIDEServices, IOTAPackageServices, APackageServices);

      if not Assigned(APackageServices) then
        raise EJclExpertException.CreateTrace(RsENoPackageServices);

      for I := 0 to APackageServices.PackageCount - 1 do
      begin
        PackageFileName := ChangeFileExt(APackageServices.PackageNames[I], BPLExtension);
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

function TJclOTAExpertBase.IsPackage(const Project: IOTAProject): Boolean;
var
  FileExtension: string;
  Index, SourceNodePosition: Integer;
  ProjectFile: TStrings;
  ProjectContent: string;
begin
  if not Assigned(Project) then
    raise EJclExpertException.CreateTrace(RsENoActiveProject);

  FileExtension := ExtractFileExt(Project.FileName);

  if AnsiSameText(FileExtension, BDSPROJExtension) then
  begin
    ProjectFile := TStringList.Create;
    try
      ProjectFile.LoadFromFile(Project.FileName);
      ProjectContent := ProjectFile.Text;
      SourceNodePosition := AnsiPos('</Source', ProjectContent);
      for Index := SourceNodePosition-1 downto 1 do
        if ProjectContent[Index] = '.' then
          Break;
      Result := AnsiSameText(Copy(ProjectContent, Index, SourceNodePosition-Index), DPKExtension);
    finally
      ProjectFile.Free;
    end;
  end
  else
    Result := AnsiSameText(FileExtension, DPKExtension);
end;

procedure TJclOTAExpertBase.ReadEnvVariables;
{$IFDEF COMPILER6_UP}
var
  I: Integer;
  EnvNames: TStringList;
  {$IFDEF MSWINDOWS}
  EnvVarKeyName: string;
  {$ENDIF MSWINDOWS}
  {$IFDEF KYLIX}
  RADToolsInstallations: TJclBorRADToolInstallations;
  RADToolInstallation: TJclBorRADToolInstallation;
  {$ENDIF KYLIX}
{$ENDIF COMPÏLER6_UP}
begin
  FEnvVariables.Clear;

  // read user and system environment variables
  GetEnvironmentVars(FEnvVariables, False);

  // read Delphi environment variables
  {$IFDEF COMPILER6_UP}
  EnvNames := TStringList.Create;
  try
    {$IFDEF MSWINDOWS}
    EnvVarKeyName := BaseRegistryKey + EnvironmentVarsKey;
    if RegKeyExists(HKEY_CURRENT_USER, EnvVarKeyName) and
      RegGetValueNames(HKEY_CURRENT_USER, EnvVarKeyName, EnvNames) then
      for I := 0 to EnvNames.Count - 1 do
        FEnvVariables.Values[EnvNames[I]] :=
          RegReadStringDef(HKEY_CURRENT_USER, EnvVarKeyName, EnvNames[I], '');
    {$ENDIF MSWINDOWS}
    {$IFDEF KYLIX}
    RADToolsInstallations := TJclBorRADToolInstallations.Create;
    try
      {$IFDEF KYLIX3}
      {$IFDEF BCB}
      RADToolInstallation := RADToolsInstallations.BCBInstallationFromVersion[3];
      {$ELSE}
      RADToolInstallation := RADToolsInstallations.DelphiInstallationFromVersion[3];
      {$ENDIF BCB}
      {$ELSE}
      RADToolInstallation := nil;
      {$ENDIF KYLIX3}
      if Assigned(RADToolInstallation) then
      begin
        for I := 0 to RADToolInstallation.EnvironmentVariables.Count - 1 do
          EnvNames.Add(RADToolInstallation.EnvironmentVariables.Names[I]);
        for I := 0 to EnvNames.Count - 1 do
          FEnvVariables.Values[EnvNames[I]] :=
            RADToolInstallation.EnvironmentVariables.Values[EnvNames[I]];
      end;
    finally
      RADToolsInstallations.Free;
    end;
    {$ENDIF KYLIX}
  finally
    EnvNames.Free;
  end;
  {$ENDIF COMPILER6_UP}

  // add the Delphi directory
  FEnvVariables.Values[DelphiEnvironmentVar] := RootDir;
end;

function TJclOTAExpertBase.SubstitutePath(const Path: string): string;
var
  I: Integer;
  Name: string;
begin
  if FEnvVariables.Count = 0 then
    ReadEnvVariables;
  Result := Path;
  while Pos('$(', Result) > 0 do
    for I := 0 to FEnvVariables.Count - 1 do
    begin
      Name := FEnvVariables.Names[I];
      Result := StringReplace(Result, Format('$(%s)', [Name]),
        FEnvVariables.Values[Name], [rfReplaceAll, rfIgnoreCase]);
    end;
end;

procedure TJclOTAExpertBase.RegisterAction(Action: TCustomAction);
begin
  if not Assigned(ActionList) then
  begin
    ActionList := TList.Create;
    {$IFDEF COMPILER6_UP}
    RegisterFindGlobalComponentProc(FindActions);
    {$ELSE COMPILER6_UP}
    if not Assigned(OldFindGlobalComponentProc) then
    begin
      OldFindGlobalComponentProc := FindGlobalComponent;
      FindGlobalComponent := FindActions;
    end;
    {$ENDIF COMPILER6_UP}
  end;

  ActionList.Add(Action);
end;

procedure TJclOTAExpertBase.UnregisterAction(Action: TCustomAction);
begin
  if Assigned(ActionList) then
  begin
    ActionList.Remove(Action);
    if (ActionList.Count = 0) then
    begin
      FreeAndNil(ActionList);
      {$IFDEF COMPILER6_UP}
      UnRegisterFindGlobalComponentProc(FindActions);
      {$ELSE COMPILER6_UP}
      FindGlobalComponent := OldFindGlobalComponentProc;
      {$ENDIF COMPILER6_UP}
    end;
  end;

  // remove action from toolbar to avoid crash when recompile package inside the IDE.
  CheckToolBarButton(FNTAServices.ToolBar[sCustomToolBar], Action);
  CheckToolBarButton(FNTAServices.ToolBar[sStandardToolBar], Action);
  CheckToolBarButton(FNTAServices.ToolBar[sDebugToolBar], Action);
  CheckToolBarButton(FNTAServices.ToolBar[sViewToolBar], Action);
  CheckToolBarButton(FNTAServices.ToolBar[sDesktopToolBar], Action);
  {$IFDEF COMPILER7_UP}
  CheckToolBarButton(FNTAServices.ToolBar[sInternetToolBar], Action);
  CheckToolBarButton(FNTAServices.ToolBar[sCORBAToolBar], Action);
  {$ENDIF COMPILER7_UP}
end;

type
  TAccessToolButton = class(TToolButton);
  
procedure TJclOTAExpertBase.CheckToolBarButton(AToolBar: TToolBar; AAction: TCustomAction);
var
  Index: Integer;
  AButton: TAccessToolButton;
begin
  if Assigned(AToolBar) then
    for Index := AToolBar.ButtonCount - 1 downto 0 do
    begin
      AButton := TAccessToolButton(AToolBar.Buttons[Index]);
      if AButton.Action = AAction then
      begin
        AButton.SetToolBar(nil);
        AButton.Free;
      end;
    end;
end;

procedure TJclOTAExpertBase.RegisterCommands;
begin
  // override to add actions and menu items
end;

procedure TJclOTAExpertBase.UnregisterCommands;
begin
  // override to remove actions and menu items
end;

function TJclOTAExpertBase.LoadBool(Name: string; Def: Boolean): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := RegReadBoolDef(HKCU, ExpertRegistryKey, Name, Def);
  {$ELSE MSWINDOWS}
  Result := Def;
  {$ENDIF MSWINDOWS}
end;

function TJclOTAExpertBase.LoadString(Name: string; Def: string): string;
begin
  {$IFDEF MSWINDOWS}
  Result := RegReadStringDef(HKCU, ExpertRegistryKey, Name, Def);
  {$ELSE MSWINDOWS}
  Result := Def;
  {$ENDIF MSWINDOWS}
end;

function TJclOTAExpertBase.LoadInteger(Name: string; Def: Integer): Integer;
begin
  {$IFDEF MSWINDOWS}
  Result := RegReadIntegerDef(HKCU, ExpertRegistryKey, Name, Def);
  {$ELSE MSWINDOWS}
  Result := Def;
  {$ENDIF MSWINDOWS}
end;

procedure TJclOTAExpertBase.LoadStrings(Name: string; List: TStrings);
begin
  {$IFDEF MSWINDOWS}
  RegLoadList(HKCU, ExpertRegistryKey, Name, List);
  {$ELSE MSWINDOWS}
  List.Clear;
  {$ENDIF MSWINDOWS}
end;

procedure TJclOTAExpertBase.SaveBool(Name: string; Value: Boolean);
begin
  {$IFDEF MSWINDOWS}
  RegWriteBool(HKCU, ExpertRegistryKey, Name, Value);
  {$ENDIF MSWINDOWS}
end;

procedure TJclOTAExpertBase.SaveString(Name: string; Value: string);
begin
  {$IFDEF MSWINDOWS}
  RegWriteString(HKCU, ExpertRegistryKey, Name, Value);
  {$ENDIF MSWINDOWS}
end;

procedure TJclOTAExpertBase.SaveInteger(Name: string; Value: Integer);
begin
  {$IFDEF MSWINDOWS}
  RegWriteInteger(HKCU, ExpertRegistryKey, Name, Value);
  {$ENDIF MSWINDOWS}
end;

procedure TJclOTAExpertBase.SaveStrings(Name: string; List: TStrings);
begin
  {$IFDEF MSWINDOWS}
  RegSaveList(HKCU, ExpertRegistryKey, Name, List);
  {$ENDIF MSWINDOWS}
end;

//=== { TJclOTAExpert } ======================================================

procedure TJclOTAExpert.AfterSave;
begin
end;

procedure TJclOTAExpert.BeforeSave;
begin
end;

procedure TJclOTAExpert.Destroyed;
begin
end;

procedure TJclOTAExpert.Execute;
begin
end;

function TJclOTAExpert.GetIDString: string;
begin
  Result := 'Jedi.' + ClassName;
end;

function TJclOTAExpert.GetName: string;
begin
  Result := ClassName;
end;

function TJclOTAExpert.GetState: TWizardState;
begin
  Result := [];
end;

procedure TJclOTAExpert.Modified;
begin
end;

{$IFDEF BDS}

var
  AboutBoxServices: IOTAAboutBoxServices = nil;
  AboutBoxIndex: Integer = -1;
  SplashScreenInitialized: Boolean = False;

procedure RegisterAboutBox;
var
  ProductImage: HBITMAP;
begin
  if AboutBoxIndex = -1 then
  begin
    Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);
    if not Assigned(AboutBoxServices) then
      raise EJclExpertException.CreateTrace(RsENoAboutServices);
    ProductImage := LoadBitmap(FindResourceHInstance(HInstance), 'JCLSPLASH');
    if ProductImage = 0 then
      raise EJclExpertException.CreateTrace(RsENoBitmapResources);
    AboutBoxIndex := AboutBoxServices.AddProductInfo(RsAboutDialogTitle,
      RsAboutCopyright, RsAboutTitle, RsAboutDescription, 0,
      ProductImage, False, RsAboutLicenceStatus);
  end;
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> -1) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemoveProductInfo(AboutBoxIndex);
    AboutBoxIndex := -1;
    AboutBoxServices := nil;
  end;
end;

procedure RegisterSplashScreen;
var
  ProductImage: HBITMAP;
begin
  if Assigned(SplashScreenServices) and not SplashScreenInitialized then
  begin
    ProductImage := LoadBitmap(FindResourceHInstance(HInstance), 'JCLSPLASH');
    if ProductImage = 0 then
      raise EJclExpertException.CreateTrace(RsENoBitmapResources);
    // C#Builder 1 doesn't display AddProductBitmap
    //SplashScreenServices.AddProductBitmap(RsAboutDialogTitle, ProductImage,
    //  False, RsAboutLicenceStatus);
    SplashScreenServices.AddPluginBitmap(RsAboutDialogTitle, ProductImage,
      False, RsAboutLicenceStatus);
    SplashScreenInitialized := True;
  end;
end;

initialization

finalization

try
  UnregisterAboutBox;
except
  on ExceptionObj: TObject do
  begin
    JclExpertShowExceptionDialog(ExceptionObj);
    raise;
  end;
end;

{$ENDIF BDS}

//=== Helper routines ========================================================

{ (rom) disabled, unused
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
}

// History:

// $Log$
// Revision 1.14  2005/12/26 18:03:40  outchy
// Enhanced bds support (including C#1 and D8)
// Introduction of dll experts
// Project types in templates
//
// Revision 1.13  2005/12/16 23:46:25  outchy
// Added expert stack form.
// Added code to display call stack on expert exception.
// Fixed package extension for D2006.
//
// Revision 1.12  2005/10/28 04:34:27  rrossmair
// - replaced {$IFDEF RTL170_UP} by more appropriate {$IFDEF BDS}
//
// Revision 1.11  2005/10/27 13:50:39  rrossmair
// - cleaned up mistakenly expanded check-in comments
//
// Revision 1.10  2005/10/27 11:00:43  marquardt
// cleaned up the sources and created a .rc file
//
// Revision 1.9  2005/10/27 08:31:08  outchy
// Items add in the splash screen and in the about box of Delphi (requires at least D2005)
//
// Revision 1.8  2005/10/26 08:29:53  marquardt
// Kylix dummy Load results fixed
//
// Revision 1.7  2005/10/26 03:29:44  rrossmair
// - improved header information, added Date and Log CVS tags.
//
// Revision 1.6  2005/10/25 14:45:22  uschuster
// some changes for Kylix
//
// Revision 1.5  2005/10/25 13:00:12  marquardt
// Load and Save methods for TJclOTAExpertBase
//
// Revision 1.4  2005/10/25 08:27:22  marquardt
// minor cleanups, deactivated unused function
//
// Revision 1.3  2005/10/24 12:05:51  marquardt
// further cleanup
//
// Revision 1.2  2005/10/23 12:53:36  marquardt
// further expert cleanup and integration, use of JclRegistry
//
// Revision 1.1  2005/10/21 12:24:41  marquardt
// experts reorganized with new directory common
//
// Revision 1.3  2005/10/20 22:55:17  outchy
// Experts are now generated by the package generator.
// No WEAKPACKAGEUNIT in design-time packages.
//
// Revision 1.2  2005/10/20 17:19:30  outchy
// Moving function calls out of Asserts
//
// Revision 1.1  2005/10/03 16:15:58  rrossmair
// - moved over from jcl\examples\vcl\debugextension
//
// Revision 1.10  2005/09/17 23:01:46  outchy
// user's settings are now stored in the registry (HKEY_CURRENT_USER)
//
// Revision 1.9  2005/08/07 13:42:38  outchy
// IT3115: Adding system and user environment variables.
//
// Revision 1.8  2005/07/26 17:41:06  outchy
// Icons can now be placed in the IDE's toolbars via the customize dialog. They are restored at the IDE's startup.
//
// Revision 1.7  2005/05/08 15:43:28  outchy
// Compiler conditions modified for C++Builder
//
// Revision 1.6  2005/03/14 05:56:27  rrossmair
// - fixed issue #2752 (TJclOTAUtils.SubstitutePath does not support nested environment variables) as proposed by the reporter.
//

end.
