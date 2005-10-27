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
  Windows, Classes, ToolsAPI, ComCtrls, ActnList;

const
  MapFileOptionDetailed = 3;

type
  TJclOTAExpertBase = class(TInterfacedObject)
  private
    FBaseRegistryKey: string;
    FExpertRegistryKey: string;
    FEnvVariables: TStringList;
    FRootDir: string;
    FServices: IOTAServices;
    FName: string;
    FNTAServices: INTAServices;
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

implementation

uses
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  SysUtils,
  {$IFDEF MSWINDOWS}
  ImageHlp, JclRegistry,
  {$ENDIF MSWINDOWS}
  {$IFDEF KYLIX}
  JclBorlandTools,
  {$ENDIF KYLIX}
  JclFileUtils, JclStrings, JclSysInfo,
  JclOtaConsts, JclOtaResources;

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

//=== { TJclOTAExpertBase } ==================================================

constructor TJclOTAExpertBase.Create(AName: string);
begin
  Supports(BorlandIDEServices,IOTAServices,FServices);
  Assert(Assigned(FServices), RsENoIDEServices);

  Supports(FServices,INTAServices,FNTAServices);
  Assert(Assigned(FNTAServices), RsENoNTAServices);

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
  Res := FindFirst(ChangeFileExt(MapFileName, '.*'), faArchive, Se);
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
    Res := FindNext(Se);
  end;
  FindClose(Se);
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
  Result := ChangeFileExt(Project.FileName, DRCExtension);
end;

function TJclOTAExpertBase.GetMapFileName(const Project: IOTAProject): string;
var
  ProjectFileName, OutputDirectory, LibPrefix, LibSuffix: string;
begin
  ProjectFileName := Project.FileName;
  OutputDirectory := GetOutputDirectory(Project);
  {$IFDEF RTL140_UP}
  LibPrefix := Trim(VarToStr(Project.ProjectOptions.Values[LIBPREFIXOptionName]));
  LibSuffix := Trim(VarToStr(Project.ProjectOptions.Values[LIBSUFFIXOptionName]));
  {$ELSE ~RTL140_UP}
  LibPrefix := '';
  LibSuffix := '';
  {$ENDIF ~RTL140_UP}
  Result := PathAddSeparator(OutputDirectory) + LibPrefix +
    PathExtractFileNameNoExt(ProjectFileName) + LibSuffix + MAPExtension;
end;

function TJclOTAExpertBase.GetOutputDirectory(const Project: IOTAProject): string;
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

function TJclOTAExpertBase.GetProjectGroup: IOTAProjectGroup;
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
    Assert(FRootDir <> '');
  end;
  Result := FRootDir;
end;

function TJclOTAExpertBase.IsInstalledPackage(const Project: IOTAProject): Boolean;
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

function TJclOTAExpertBase.IsPackage(const Project: IOTAProject): Boolean;
begin
  Result := AnsiSameText(ExtractFileExt(Project.FileName), DPKExtension);
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
  {$ELSE}
  Result := Def;
  {$ENDIF MSWINDOWS}
end;

function TJclOTAExpertBase.LoadString(Name: string; Def: string): string;
begin
  {$IFDEF MSWINDOWS}
  Result := RegReadStringDef(HKCU, ExpertRegistryKey, Name, Def);
  {$ELSE}
  Result := Def;
  {$ENDIF MSWINDOWS}
end;

function TJclOTAExpertBase.LoadInteger(Name: string; Def: Integer): Integer;
begin
  {$IFDEF MSWINDOWS}
  Result := RegReadIntegerDef(HKCU, ExpertRegistryKey, Name, Def);
  {$ELSE}
  Result := Def;
  {$ENDIF MSWINDOWS}
end;

procedure TJclOTAExpertBase.LoadStrings(Name: string; List: TStrings);
begin
  {$IFDEF MSWINDOWS}
  RegLoadList(HKCU, ExpertRegistryKey, Name, List);
  {$ELSE}
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

{$IFDEF RTL170_UP}

{$R 'JclImages.res'}

var
  AboutBoxServices: IOTAAboutBoxServices = nil;
  AboutBoxIndex: Integer = 0;

procedure RegisterAboutBox;
var
  ProductImage: HBITMAP;
begin
  Supports(BorlandIDEServices,IOTAAboutBoxServices,AboutBoxServices);
  Assert(Assigned(AboutBoxServices), RsENoAboutServices);
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), 'JCLSPLASH');
  AboutBoxIndex := AboutBoxServices.AddProductInfo(RsAboutDialogTitle,
    RsAboutCopyright, RsAboutTitle, RsAboutDescription, 0,
    ProductImage, False, RsAboutLicenceStatus);
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> 0) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemoveProductInfo(AboutBoxIndex);
    AboutBoxIndex := 0;
    AboutBoxServices := nil;
  end;
end;

procedure RegisterSplashScreen;
var
  ProductImage: HBITMAP;
begin
  Assert(Assigned(SplashScreenServices), RsENoSplashServices);
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), 'JCLSPLASH');
  SplashScreenServices.AddProductBitmap(RsAboutDialogTitle,ProductImage,
    False,RsAboutLicenceStatus);
end;

initialization

RegisterSplashScreen;
RegisterAboutBox;

finalization

UnRegisterAboutBox;

{$ENDIF RTL170_UP}

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
// Revision 1.9  2005/10/27 08:31:08  outchy
// Items add in the splash screen and in the about box of Delphi (requires at least D2005)
//
// Revision 1.8  2005/10/26 08:29:53  marquardt
// Kylix dummy Load results fixed
//
// Revision 1.7  2005/10/26 03:29:44  rrossmair
// - improved header information, added $Date$ and $Log$
// - improved header information, added $Date: 2005/10/26 08:29:53 $ and Revision 1.9  2005/10/27 08:31:08  outchy
// - improved header information, added $Date: 2005/10/26 08:29:53 $ and Items add in the splash screen and in the about box of Delphi (requires at least D2005)
// - improved header information, added $Date: 2005/10/26 08:29:53 $ and
// - improved header information, added $Date$ and Revision 1.8  2005/10/26 08:29:53  marquardt
// - improved header information, added $Date$ and Kylix dummy Load results fixed
// - improved header information, added $Date$ and CVS tags.
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
