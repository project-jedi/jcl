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
  Controls, ComCtrls, ActnList, Menus,
{$IFDEF MSWINDOWS}
  JclDebug,
{$ENDIF MSWINDOWS}
  JclBorlandTools,
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

  TJclOTASettings = class (TObject)
  private
    FKeyName: string;
    FBaseKeyName: string;
  public
    constructor Create(ExpertName: string);
    function LoadBool(Name: string; Def: Boolean): Boolean;
    function LoadString(Name: string; Def: string): string;
    function LoadInteger(Name: string; Def: Integer): Integer;
    procedure LoadStrings(Name: string; List: TStrings);
    procedure SaveBool(Name: string; Value: Boolean);
    procedure SaveString(Name: string; Value: string);
    procedure SaveInteger(Name: string; Value: Integer);
    procedure SaveStrings(Name: string; List: TStrings);
    property KeyName: string read FKeyName;
    property BaseKeyName: string read FBaseKeyName;
  end;

  // Note: we MUST use an interface as the type of the Expert parameter
  // and not an object to avoid a bug in C++ Builder 5 compiler. If we 
  // used an object, the compiler would crash or give internal error GH4148
  // being obviously lost trying to resolve almost circular references 
  // between this unit and the JclOtaConfigurationForm unit.
  IJclOTAOptionsCallback = interface;

  TJclOTAAddPageFunc = procedure (AControl: TControl; PageName: string;
    Expert: IJclOTAOptionsCallback) of object;

  IJclOTAOptionsCallback = interface
    procedure AddConfigurationPages(AddPageFunc: TJclOTAAddPageFunc);
    procedure ConfigurationClosed(AControl: TControl; SaveChanges: Boolean);
  end;

  TJclOTAExpertBase = class(TInterfacedObject, IJclOTAOptionsCallback)
  private
    FEnvVariables: TStringList;
    FRootDir: string;
    FServices: IOTAServices;
    FNTAServices: INTAServices;
    FOTAModuleServices: IOTAModuleServices;
    FSettings: TJclOTASettings;
    {$IFDEF BDS}
    FOTAPersonalityServices: IOTAPersonalityServices;
    {$ENDIF BDS}
    function GetModuleHInstance: Cardinal;
    function GetActiveProject: IOTAProject;
    function GetProjectGroup: IOTAProjectGroup;
    function GetRootDir: string;
    procedure ReadEnvVariables;
    procedure ConfigurationActionUpdate(Sender: TObject);
    procedure ConfigurationActionExecute(Sender: TObject);
    function GetActivePersonality: TJclBorPersonality;
    function GetDesigner: string;
  public
    class procedure AddExpert(AExpert: TJclOTAExpertBase);
    class procedure RemoveExpert(AExpert: TJclOTAExpertBase);
    class function GetExpertCount: Integer;
    class function GetExpert(Index: Integer): TJclOTAExpertBase;
    class function ConfigurationDialog(StartName: string = ''): Boolean;
    class procedure CheckToolBarButton(AToolBar: TToolBar; AAction: TCustomAction);
    class function GetActionCount: Integer;
    class function GetAction(Index: Integer): TAction;
    class function ActionSettings: TJclOtaSettings;
  public
    constructor Create(AName: string); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    
    function FindExecutableName(const MapFileName, OutputDirectory: string;
      var ExecutableFileName: string): Boolean;
    function GetDrcFileName(const Project: IOTAProject): string;
    function GetMapFileName(const Project: IOTAProject): string;
    function GetOutputDirectory(const Project: IOTAProject): string;
    function IsInstalledPackage(const Project: IOTAProject): Boolean;
    function IsPackage(const Project: IOTAProject): Boolean;
    function SubstitutePath(const Path: string): string;

    procedure AddConfigurationPages(AddPageFunc: TJclOTAAddPageFunc); virtual;
    procedure ConfigurationClosed(AControl: TControl; SaveChanges: Boolean); virtual;

    procedure RegisterCommands; virtual;
    procedure UnregisterCommands; virtual;
    procedure RegisterAction(Action: TCustomAction);
    procedure UnregisterAction(Action: TCustomAction);

    property ActiveProject: IOTAProject read GetActiveProject;
    property Settings: TJclOTASettings read FSettings;
    property NTAServices: INTAServices read FNTAServices;
    property ProjectGroup: IOTAProjectGroup read GetProjectGroup;
    property RootDir: string read GetRootDir;
    property Services: IOTAServices read FServices;
    property OTAModuleServices: IOTAModuleServices read FOTAModuleServices;
    {$IFDEF BDS}
    property OTAPersonalityServices: IOTAPersonalityServices read FOTAPersonalityServices;
    {$ENDIF BDS}

    property ActivePersonality: TJclBorPersonality read GetActivePersonality;
    property Designer: string read GetDesigner;

    property ModuleHInstance: Cardinal read GetModuleHInstance;
  end;

  TJclOTAExpert = class(TJclOTAExpertBase, IOTAWizard, IOTANotifier)
  protected
    procedure AfterSave; virtual;
    procedure BeforeSave; virtual;
    procedure Destroyed; virtual;
    procedure Modified; virtual;
    procedure Execute; virtual;
    function GetIDString: string; virtual;
    function GetName: string; virtual;
    function GetState: TWizardState; virtual;
  end;

// procedure SaveOptions(const Options: IOTAOptions; const FileName: string);
function JclExpertShowExceptionDialog(AExceptionObj: TObject): Boolean;
{$IFDEF BDS}
function PersonalityTextToId(const PersonalityText: string): TJclBorPersonality;
{$ENDIF BDS}

{$IFDEF BDS}
procedure RegisterSplashScreen;
procedure RegisterAboutBox;
{$ENDIF BDS}

implementation

uses
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  Forms, Graphics, Dialogs, ActiveX,
  {$IFDEF MSWINDOWS}
  ImageHlp, JclRegistry,
  {$ENDIF MSWINDOWS}
  {$IFDEF KYLIX}
  JclBorlandTools,
  {$ENDIF KYLIX}
  JclFileUtils, JclStrings, JclSysInfo,
  JclOtaConsts, JclOtaResources, JclOtaExceptionForm, JclOtaConfigurationForm,
  JclOtaActionConfigureSheet, JclOtaWizardForm, JclOtaWizardFrame;

{$R 'JclImages.res'}

var
  GlobalActionList: TList = nil;
  GlobalActionSettings: TJclOtaSettings = nil;
  GlobalExpertList: TList = nil;
  ConfigurationAction: TAction = nil;
  ConfigurationMenuItem: TMenuItem = nil;
  ActionConfigureSheet: TJclOtaActionConfigureFrame = nil;
  {$IFNDEF COMPILER6_UP}
  OldFindGlobalComponentProc: TFindGlobalComponent = nil;
  {$ENDIF COMPILER6_UP}

function FindActions(const Name: string): TComponent;
var
  Index: Integer;
  TestAction: TCustomAction;
begin
  try
    Result := nil;
    if Assigned(GlobalActionList) then
      for Index := 0 to GlobalActionList.Count-1 do
      begin
        TestAction := TCustomAction(GlobalActionList.Items[Index]);
        if (CompareText(Name,TestAction.Name) = 0) then
          Result := TestAction;
      end;
    {$IFNDEF COMPILER6_UP}
    if (not Assigned(Result)) and Assigned(OldFindGlobalComponentProc) then
      Result := OldFindGlobalComponentProc(Name)
    {$ENDIF COMPILER6_UP}
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
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

{$IFDEF BDS}
function PersonalityTextToId(const PersonalityText: string): TJclBorPersonality;
begin
  if SameText(PersonalityText, sDelphiPersonality) then
    Result := bpDelphi32
  else if SameText(PersonalityText, sDelphiDotNetPersonality) then
    Result := bpDelphiNet32
  else if SameText(PersonalityText, sCBuilderPersonality) then
    Result := bpBCBuilder32
  else if SameText(PersonalityText, sCSharpPersonality) then
    Result := bpCSBuilder32
  else if SameText(PersonalityText, sVBPersonality) then
    Result := bpVisualBasic32
  {$IFDEF COMPILER10_UP}
  else if SameText(PersonalityText, sDesignPersonality) then
    Result := bpDesign
  {$ENDIF COMPILER10_UP}
  else
    Result := bpUnknown;
end;
{$ENDIF BDS}

//=== { EJclExpertException } ================================================

constructor EJclExpertException.CreateTrace(const Msg: string);
begin
  inherited Create(Msg);
{$IFDEF MSWINDOWS}
  FStackInfo := JclCreateStackList(False, 0, nil, False);
{$ENDIF MSWINDOWS}
end;

{$IFDEF MSWINDOWS}
destructor EJclExpertException.Destroy;
begin
  FreeAndNil(FStackInfo);
  inherited Destroy;
end;
{$ENDIF MSWINDOWS}

{ TJclOTASettings }

constructor TJclOTASettings.Create(ExpertName: string);
var
  OTAServices: IOTAServices;
begin
  inherited Create;

  Supports(BorlandIDEServices,IOTAServices,OTAServices);
  if not Assigned(OTAServices) then
    raise EJclExpertException.CreateTrace(RsENoIDEServices);

  FBaseKeyName := StrEnsureSuffix('\', OTAServices.GetBaseRegistryKey);
  
  FKeyName := BaseKeyName + RegJclIDEKey + ExpertName;
end;

function TJclOTASettings.LoadBool(Name: string; Def: Boolean): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := RegReadBoolDef(HKCU, KeyName, Name, Def);
  {$ELSE MSWINDOWS}
  Result := Def;
  {$ENDIF MSWINDOWS}
end;

function TJclOTASettings.LoadInteger(Name: string; Def: Integer): Integer;
begin
  {$IFDEF MSWINDOWS}

  Result := RegReadIntegerDef(HKCU, KeyName, Name, Def);
  {$ELSE MSWINDOWS}
  Result := Def;
  {$ENDIF MSWINDOWS}
end;

function TJclOTASettings.LoadString(Name, Def: string): string;
begin
  {$IFDEF MSWINDOWS}
  Result := RegReadStringDef(HKCU, KeyName, Name, Def);
  {$ELSE MSWINDOWS}
  Result := Def;
  {$ENDIF MSWINDOWS}
end;

procedure TJclOTASettings.LoadStrings(Name: string; List: TStrings);
begin
  {$IFDEF MSWINDOWS}
  RegLoadList(HKCU, KeyName, Name, List);
  {$ELSE MSWINDOWS}
  List.Clear;
  {$ENDIF MSWINDOWS}
end;

procedure TJclOTASettings.SaveBool(Name: string; Value: Boolean);
begin
  {$IFDEF MSWINDOWS}
  RegWriteBool(HKCU, KeyName, Name, Value);
  {$ENDIF MSWINDOWS}
end;

procedure TJclOTASettings.SaveInteger(Name: string; Value: Integer);
begin
  {$IFDEF MSWINDOWS}
  RegWriteInteger(HKCU, KeyName, Name, Value);
  {$ENDIF MSWINDOWS}
end;

procedure TJclOTASettings.SaveString(Name, Value: string);
begin
  {$IFDEF MSWINDOWS}
  RegWriteString(HKCU, KeyName, Name, Value);
  {$ENDIF MSWINDOWS}
end;

procedure TJclOTASettings.SaveStrings(Name: string; List: TStrings);
begin
  {$IFDEF MSWINDOWS}
  RegSaveList(HKCU, KeyName, Name, List);
  {$ENDIF MSWINDOWS}
end;

//=== { TJclOTAExpertBase } ==================================================

class function TJclOTAExpertBase.ConfigurationDialog(
  StartName: string): Boolean;
var
  OptionsForm: TJclOtaOptionsForm;
  Index: Integer;
begin
  OptionsForm := TJclOtaOptionsForm.Create(nil);
  try
    for Index := 0 to GetExpertCount - 1 do
      GetExpert(Index).AddConfigurationPages(OptionsForm.AddPage);
    Result := OptionsForm.Execute(StartName);
  finally
    OptionsForm.Free;
  end;
end;

class function TJclOTAExpertBase.GetExpert(Index: Integer): TJclOTAExpertBase;
begin
  if Assigned(GlobalExpertList) then
    Result := TJclOTAExpertBase(GlobalExpertList.Items[Index])
  else
    Result := nil;
end;

class function TJclOTAExpertBase.GetExpertCount: Integer;
begin
  if Assigned(GlobalExpertList) then
    Result := GlobalExpertList.Count
  else
    Result := 0;
end;

class procedure TJclOTAExpertBase.AddExpert(AExpert: TJclOTAExpertBase);
begin
  if not Assigned(GlobalExpertList) then
    GlobalExpertList := TList.Create;
  GlobalExpertList.Add(AExpert);
end;

procedure TJclOTAExpertBase.AfterConstruction;
begin
  inherited AfterConstruction;

  RegisterCommands;
  AddExpert(Self);
end;

procedure TJclOTAExpertBase.BeforeDestruction;
begin
  RemoveExpert(Self);
  UnregisterCommands;

  inherited BeforeDestruction;
end;

class procedure TJclOTAExpertBase.RemoveExpert(AExpert: TJclOTAExpertBase);
begin
  if Assigned(GlobalExpertList) then
    GlobalExpertList.Remove(AExpert);
end;

class function TJclOTAExpertBase.GetAction(Index: Integer): TAction;
begin
  if Assigned(GlobalActionList) then
    Result := TAction(GlobalActionList.Items[Index])
  else
    Result := nil;
end;

class function TJclOTAExpertBase.GetActionCount: Integer;
begin
  if Assigned(GlobalActionList) then
    Result := GlobalActionList.Count
  else
    Result := 0;
end;

type
  TAccessToolButton = class(TToolButton);

class procedure TJclOTAExpertBase.CheckToolBarButton(AToolBar: TToolBar; AAction: TCustomAction);
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

class function TJclOTAExpertBase.ActionSettings: TJclOtaSettings;
begin
  if not Assigned(GlobalActionSettings) then
    GlobalActionSettings := TJclOTASettings.Create(JclActionSettings);
  Result := GlobalActionSettings;
end;

procedure TJclOTAExpertBase.ConfigurationActionExecute(Sender: TObject);
begin
  try
    ConfigurationDialog('');
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TJclOTAExpertBase.ConfigurationActionUpdate(Sender: TObject);
begin
  try
    (Sender as TAction).Enabled := True;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TJclOTAExpertBase.AddConfigurationPages(  
  AddPageFunc: TJclOTAAddPageFunc);
begin
  // AddPageFunc uses '\' as a separator in PageName to build a tree
  if not Assigned(ActionConfigureSheet) then
  begin
    ActionConfigureSheet := TJclOtaActionConfigureFrame.Create(Application);
    AddPageFunc(ActionConfigureSheet, RsActionSheet, Self);
  end;
  // override to customize
end;

procedure TJclOTAExpertBase.ConfigurationClosed(AControl: TControl;
  SaveChanges: Boolean);
begin
  if Assigned(AControl) and (AControl = ActionConfigureSheet) then
  begin
    if SaveChanges then
      ActionConfigureSheet.SaveChanges;
    FreeAndNil(ActionConfigureSheet);
  end
  else
    AControl.Free;
  // override to customize
end;

constructor TJclOTAExpertBase.Create(AName: string);
begin
  inherited Create;

  {$IFDEF BDS}
  RegisterSplashScreen;
  RegisterAboutBox;
  {$ENDIF BDS}
  
  Supports(BorlandIDEServices, IOTAServices, FServices);
  if not Assigned(FServices) then
    raise EJclExpertException.CreateTrace(RsENoIDEServices);

  Supports(BorlandIDEServices, INTAServices, FNTAServices);
  if not Assigned(FNTAServices) then
    raise EJclExpertException.CreateTrace(RsENoNTAServices);

  {$IFDEF BDS}
  Supports(BorlandIDEServices, IOTAPersonalityServices, FOTAPersonalityServices);
  if not Assigned(FOTAPersonalityServices) then
    raise EJclExpertException.CreateTrace(RsENoPersonalityServices);
  {$ENDIF BDS}

  Supports(BorlandIDEServices, IOTAModuleServices, FOTAModuleServices);
  if not Assigned(FOTAModuleServices) then
    raise EJclExpertException.CreateTrace(RsENoModuleServices);

  FEnvVariables := TStringList.Create;
  FSettings := TJclOTASettings.Create(AName);
end;

destructor TJclOTAExpertBase.Destroy;
begin
  FreeAndNil(FSettings);
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

function TJclOTAExpertBase.GetDesigner: string;
begin
  {$IFDEF COMPILER6_UP}
  Result := Services.GetActiveDesignerType;
  {$ELSE COMPILER6_UP}
  Result := JclDesignerAny;
  {$ENDIF COMPILER6_UP}
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
    Result := ExtractFilePath(Project.FileName)
  else if not PathIsAbsolute(Result) then
    Result := PathGetRelativePath(ExtractFilePath(Project.FileName), Result);
end;

function TJclOTAExpertBase.GetActivePersonality: TJclBorPersonality;
{$IFDEF BDS}
var
  PersonalityText: string;
  {$IFDEF COMPILER9_UP}
  CurrentProject: IOTAProject;
  {$ENDIF COMPILER9_UP}
{$ENDIF BDS}
begin
{$IFDEF BDS}
  {$IFDEF COMPILER9_UP}
  CurrentProject := ActiveProject;
  if Assigned(CurrentProject) then
    PersonalityText := CurrentProject.Personality
  else
  {$ENDIF COMPILER9_UP}
    PersonalityText := OTAPersonalityServices.CurrentPersonality;
  Result := PersonalityTextToId(PersonalityText);
{$ELSE BDS}
{$IFDEF DELPHI}
  Result := bpDelphi32;
{$ENDIF DELPHI}
{$IFDEF BCB}
  Result := bpBCBuilder32;
{$ENDIF BCB}
{$ENDIF BDS}
end;

function TJclOTAExpertBase.GetProjectGroup: IOTAProjectGroup;
var
  AModule: IOTAModule;
  I: Integer;
begin
  for I := 0 to OTAModuleServices.ModuleCount - 1 do
  begin
    AModule := OTAModuleServices.Modules[I];
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
    FRootDir := RegReadStringDef(HKEY_LOCAL_MACHINE, Settings.BaseKeyName, DelphiRootDirKeyValue, '');
    // (rom) bugfix if using -r switch of D9 by Dan Miser
    if FRootDir = '' then
      FRootDir := RegReadStringDef(HKEY_CURRENT_USER, Settings.BaseKeyName, DelphiRootDirKeyValue, '');
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
  FileName, FileExtension, ProjectContent: string;
  Index, SourceNodePosition: Integer;
  ProjectFile: TStrings;
begin
  if not Assigned(Project) then
    raise EJclExpertException.CreateTrace(RsENoActiveProject);

  FileName := Project.FileName;
  FileExtension := ExtractFileExt(FileName);

  if AnsiSameText(FileExtension, BDSPROJExtension) and FileExists(FileName) then
  begin
    ProjectFile := TStringList.Create;
    try
      ProjectFile.LoadFromFile(FileName);
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
    EnvVarKeyName := Settings.BaseKeyName + EnvironmentVarsKey;
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
  While Pos('\\', Result) > 0 do
    Result := StringReplace(Result, '\\', DirDelimiter, [rfReplaceAll]);
end;

procedure TJclOTAExpertBase.RegisterAction(Action: TCustomAction);
begin
  if Action.Name <> '' then
  begin
    Action.Tag := Action.ShortCut;  // to restore settings
    Action.ShortCut := ActionSettings.LoadInteger(Action.Name, Action.ShortCut);
  end;

  if not Assigned(GlobalActionList) then
  begin
    GlobalActionList := TList.Create;
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

  GlobalActionList.Add(Action);
end;

procedure TJclOTAExpertBase.UnregisterAction(Action: TCustomAction);
begin
  if Action.Name <> '' then
    ActionSettings.SaveInteger(Action.Name, Action.ShortCut);
    
  if Assigned(GlobalActionList) then
  begin
    GlobalActionList.Remove(Action);
    if (GlobalActionList.Count = 0) then
    begin
      FreeAndNil(GlobalActionList);
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

procedure TJclOTAExpertBase.RegisterCommands;
var
  JclIcon: TIcon;
  Category: string;
  Index: Integer;
  IDEMenuItem, ToolsMenuItem: TMenuItem;
begin
  if not Assigned(ConfigurationAction) then
  begin
    Category := '';
    for Index := 0 to NTAServices.ActionList.ActionCount - 1 do
      if CompareText(NTAServices.ActionList.Actions[Index].Name, 'ToolsOptionsCommand') = 0 then
        Category := NTAServices.ActionList.Actions[Index].Category;

    ConfigurationAction := TAction.Create(nil);
    JclIcon := TIcon.Create;
    try
      // not ModuleHInstance because the resource is in JclBaseExpert.bpl                                                                  
      JclIcon.Handle := LoadIcon(HInstance, 'JCLCONFIGURE');
      ConfigurationAction.ImageIndex := NTAServices.ImageList.AddIcon(JclIcon);
    finally
      JclIcon.Free;
    end;
    ConfigurationAction.Caption := RsJCLOptions;
    ConfigurationAction.Name := JclConfigureActionName;
    ConfigurationAction.Category := Category;
    ConfigurationAction.Visible := True;
    ConfigurationAction.OnUpdate := ConfigurationActionUpdate;
    ConfigurationAction.OnExecute := ConfigurationActionExecute;

    ConfigurationAction.ActionList := NTAServices.ActionList;
    RegisterAction(ConfigurationAction);
  end;
  
  if not Assigned(ConfigurationMenuItem) then
  begin
    IDEMenuItem := NTAServices.MainMenu.Items;
    if not Assigned(IDEMenuItem) then
      raise EJclExpertException.CreateTrace(RsENoIDEMenu);

    ToolsMenuItem := nil;
    for Index := 0 to IDEMenuItem.Count - 1 do
      if CompareText(IDEMenuItem.Items[Index].Name, 'ToolsMenu') = 0 then
        ToolsMenuItem := IDEMenuItem.Items[Index];
    if not Assigned(ToolsMenuItem) then
      raise EJclExpertException.CreateTrace(RsENoToolsMenu);

    ConfigurationMenuItem := TMenuItem.Create(nil);
    ConfigurationMenuItem.Action := ConfigurationAction;

    ToolsMenuItem.Insert(0, ConfigurationMenuItem);
  end;

  // override to add actions and menu items
end;

procedure TJclOTAExpertBase.UnregisterCommands;
begin
  if GetExpertCount = 0 then
  begin
    UnregisterAction(ConfigurationAction);
    FreeAndNil(ConfigurationAction);
    FreeAndNil(ConfigurationMenuItem);
  end;

  // override to remove actions and menu items
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

{$ENDIF BDS}

initialization

Classes.RegisterClass(TJclWizardForm);
Classes.RegisterClass(TJclWizardFrame);

finalization

try
  {$IFDEF BDS}
  UnregisterAboutBox;
  {$ENDIF BDS}
  FreeAndNil(GlobalActionList);
  FreeAndNil(GlobalActionSettings);
  FreeAndNil(GlobalExpertList);
except
  on ExceptionObj: TObject do
  begin
    JclExpertShowExceptionDialog(ExceptionObj);
    raise;
  end;
end;

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

end.
