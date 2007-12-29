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
{ The Original Code is JclDebugIdeImpl.pas.                                                        }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                      $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclDebugIdeImpl;

{$I jcl.inc}
                              
interface

uses
  Windows, Classes, Menus, ActnList, SysUtils, Graphics, Dialogs, Controls, Forms, ToolsAPI,
  JclOtaUtils, JclOtaConsts,
  JclDebugIdeConfigFrame;

type
  TJclDebugDataInfo = record
    ProjectName: string;
    ExecutableFileName: TFileName;
    MapFileSize, JclDebugDataSize: Integer;
    LinkerBugUnit: string;
    LineNumberErrors: Integer;
    Success: Boolean;
  end;

  TDebugExpertAction = (deGenerateJdbg, deInsertJdbg, deDeleteMapFile);
  TDebugExpertActions = set of TDebugExpertAction;

  TJclDebugExtension = class(TJclOTAExpert)
  private
    FResultInfo: array of TJclDebugDataInfo;
    FStoreResults: Boolean;
    FBuildError: Boolean;
    FDebugExpertAction: TDropDownAction;
    FDebugExpertItem: TMenuItem;
    FGenerateJdbgAction: TDropDownAction;
    FGenerateJdbgItem: TMenuItem;
    FInsertJdbgAction: TDropDownAction;
    FInsertJdbgItem: TMenuItem;
    FDeleteMapFileAction: TDropDownAction;
    FDeleteMapFileItem: TMenuItem;
    FDebugImageIndex: Integer;
    FNoDebugImageIndex: Integer;
    FGenerateJdbgImageIndex: Integer;
    FNoGenerateJdbgImageIndex: Integer;
    FInsertJdbgImageIndex: Integer;
    FNoInsertJdbgImageIndex: Integer;
    FDeleteMapFileImageIndex: Integer;
    FNoDeleteMapFileImageIndex: Integer;
    FCurrentProject: IOTAProject;
    FSaveBuildProjectAction: TCustomAction;
    FSaveBuildProjectActionExecute: TNotifyEvent;
    FSaveBuildAllProjectsAction: TCustomAction;
    FSaveBuildAllProjectsActionExecute: TNotifyEvent;
    FIDENotifierIndex: Integer;
    {$IFDEF BDS4_UP}
    ProjectManager: IOTAProjectManager;
    FProjectManagerNotifierIndex: Integer;
    {$ENDIF BDS4_UP}
    FConfigFrame: TJclDebugIdeConfigFrame;
    FGlobalStates: array [TDebugExpertAction] of TDebugExpertState;
    procedure DebugExpertActionExecute(Sender: TObject);
    procedure DebugExpertActionUpdate(Sender: TObject);
    procedure DebugExpertMenuClick(Sender: TObject);
    procedure DebugExpertMenuDropDown(Sender: TObject);
    procedure DebugExpertSubMenuClick(Sender: TObject);
    procedure GenerateJdbgActionExecute(Sender: TObject);
    procedure GenerateJdbgActionUpdate(Sender: TObject);
    procedure GenerateJdbgMenuClick(Sender: TObject);
    procedure GenerateJdbgMenuDropDown(Sender: TObject);
    procedure GenerateJdbgSubMenuClick(Sender: TObject);
    procedure InsertJdbgActionExecute(Sender: TObject);
    procedure InsertJdbgActionUpdate(Sender: TObject);
    procedure InsertJdbgMenuClick(Sender: TObject);
    procedure InsertJdbgMenuDropDown(Sender: TObject);
    procedure InsertJdbgSubMenuClick(Sender: TObject);
    procedure DeleteMapFileActionExecute(Sender: TObject);
    procedure DeleteMapFileActionUpdate(Sender: TObject);
    procedure DeleteMapFileMenuClick(Sender: TObject);
    procedure DeleteMapFileMenuDropDown(Sender: TObject);
    procedure DeleteMapFileSubMenuClick(Sender: TObject);
    procedure LoadExpertValues;
    procedure SaveExpertValues;
    procedure BuildAllProjects(Sender: TObject);
    procedure BuildProject(Sender: TObject);
    procedure BeginStoreResults;
    procedure DisplayResults;
    procedure EndStoreResults;
    function GetGlobalState(Index: TDebugExpertAction): TDebugExpertState;
    procedure SetGlobalState(Index: TDebugExpertAction; Value: TDebugExpertState);
    function GetProjectState(Index: TDebugExpertAction; const AProject: IOTAProject): TDebugExpertState;
    procedure SetProjectState(Index: TDebugExpertAction; const AProject: IOTAProject; Value: TDebugExpertState);
    function GetProjectActions(const AProject: IOTAProject): TDebugExpertActions;
  public
    constructor Create; reintroduce;
    procedure AfterCompile(Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
    procedure RegisterCommands; override;
    procedure UnregisterCommands; override;
    procedure AddConfigurationPages(AddPageFunc: TJclOTAAddPageFunc); override;
    procedure ConfigurationClosed(AControl: TControl; SaveChanges: Boolean); override;
    procedure DisableExpert(const AProject: IOTAProject);
    procedure ChangeProjectAction(const AProject: IOTAProject; AAction: TDebugExpertAction; AEnabled: Boolean);
    property GlobalStates[Index: TDebugExpertAction]: TDebugExpertState read GetGlobalState
      write SetGlobalState;
    property ProjectStates[Index: TDebugExpertAction; const AProject: IOTAProject]: TDebugExpertState
      read GetProjectState write SetProjectState;
    property ProjectActions[const AProject: IOTAProject]: TDebugExpertActions read GetProjectActions;
  end;

  TIdeNotifier = class(TNotifierObject, IOTANotifier, IOTAIDENotifier, IOTAIDENotifier50)
  private
    FDebugExtension: TJclDebugExtension;
  protected
    { IOTAIDENotifier }
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean); overload;
    { IOTAIDENotifier50 }
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;
  public
    constructor Create(ADebugExtension: TJclDebugExtension);
  end;

  {$IFDEF BDS4_UP}
  TProjectManagerNotifier = class(TNotifierObject, IOTANotifier, INTAProjectMenuCreatorNotifier)
  private
    FDebugExtension: TJclDebugExtension;
    FOTAProjectManager: IOTAProjectManager;
    FNTAServices: INTAServices;
    procedure GenerateJdbgSubMenuClick(Sender: TObject);
    procedure InsertJdbgSubMenuClick(Sender: TObject);
    procedure DeleteMapFileSubMenuClick(Sender: TObject);
  protected
    { INTAProjectMenuCreatorNotifier }
    function AddMenu(const Ident: string): TMenuItem;
    function CanHandle(const Ident: string): Boolean;
  public
    constructor Create(ADebugExtension: TJclDebugExtension; const ANTAServices: INTAServices;
      const AOTAProjectManager: IOTAProjectManager);
  end;
  {$ENDIF BDS4_UP}

// design package entry point
procedure Register;

// expert DLL entry point
function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var TerminateProc: TWizardTerminateProc): Boolean; stdcall;

const
  DebugActionNames: array [TDebugExpertAction] of string =
    ( JclDebugGenerateJdbgSetting, // deGenerateJdbg
      JclDebugInsertJdbgSetting,   // deInsertJdbg
      JclDebugDeleteMapfileSetting // deDeleteMapFile);
    );
  DebugActionValues: array [False..True] of string =
    ( 'OFF', 'ON' );

implementation

{$R JclDebugIdeIcon.res}

uses
  TypInfo,
  JclBase, JclBorlandTools, JclDebug, JclDebugIdeResult,
  JclOtaResources;

procedure Register;
begin
  try
    RegisterPackageWizard(TJclDebugExtension.Create);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

var
  JCLWizardIndex: Integer = -1;

procedure JclWizardTerminate;
var
  OTAWizardServices: IOTAWizardServices;
begin
  try
    if JCLWizardIndex <> -1 then
    begin
      Supports(BorlandIDEServices, IOTAWizardServices, OTAWizardServices);
      if not Assigned(OTAWizardServices) then
        raise EJclExpertException.CreateTrace(RsENoWizardServices);

      OTAWizardServices.RemoveWizard(JCLWizardIndex);
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
    RegisterProc: TWizardRegisterProc;
    var TerminateProc: TWizardTerminateProc): Boolean stdcall;
var
  OTAWizardServices: IOTAWizardServices;
begin
  try
    TerminateProc := JclWizardTerminate;

    Supports(BorlandIDEServices, IOTAWizardServices, OTAWizardServices);
    if not Assigned(OTAWizardServices) then
      raise EJclExpertException.CreateTrace(RsENoWizardServices);

    JCLWizardIndex := OTAWizardServices.AddWizard(TJclDebugExtension.Create);

    Result := True;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      Result := False;
    end;
  end;
end;

//=== { TJclDebugExtension } =================================================

procedure TJclDebugExtension.ChangeProjectAction(const AProject: IOTAProject; AAction: TDebugExpertAction;
  AEnabled: Boolean);
var
  PropIDs, PropValues: TDynAnsiStringArray;
begin
  SetLength(PropIDs, 1);
  PropIDs[0] := DebugActionNames[AAction];
  SetLength(PropValues, 1);
  PropValues[0] := DebugActionValues[AEnabled];
  SetProjectProperties(AProject, PropIDs, PropValues);
end;

procedure TJclDebugExtension.ConfigurationClosed(AControl: TControl;
  SaveChanges: Boolean);
begin
  if Assigned(AControl) and (AControl = FConfigFrame) then
  begin
    if SaveChanges then
    begin
      GlobalStates[deGenerateJdbg] := FConfigFrame.GenerateJdbgState;
      GlobalStates[deInsertJdbg] := FConfigFrame.InsertJdbgState;
      GlobalStates[deDeleteMapFile] := FConfigFrame.DeleteMapFileState;
    end;
    FreeAndNil(FConfigFrame);
  end
  else
    inherited ConfigurationClosed(AControl, SaveChanges);
end;

constructor TJclDebugExtension.Create;
begin
  inherited Create(JclDebugExpertRegKey);
  {$IFDEF BDS4_UP}
  if not Supports(BorlandIDEServices, IOTAProjectManager, ProjectManager) then
    raise EJclExpertException.CreateRes(@RsENoProjectManager);
  {$ENDIF BDS4_UP}
end;

procedure TJclDebugExtension.AddConfigurationPages(
  AddPageFunc: TJclOTAAddPageFunc);
begin
  inherited AddConfigurationPages(AddPageFunc);
  FConfigFrame := TJclDebugIdeConfigFrame.Create(nil);
  FConfigFrame.GenerateJdbgState := GlobalStates[deGenerateJdbg];
  FConfigFrame.InsertJdbgState := GlobalStates[deInsertJdbg];
  FConfigFrame.DeleteMapFileState := GlobalStates[deDeleteMapFile];
  AddPageFunc(FConfigFrame, RsDebugConfigPageCaption, Self);
end;

procedure TJclDebugExtension.AfterCompile(Succeeded: Boolean);
var
  ProjectFileName, MapFileName, DrcFileName, ExecutableFileName, JdbgFileName: string;
  OutputDirectory, LinkerBugUnit: string;
  Succ: Boolean;
  MapFileSize, JclDebugDataSize, LineNumberErrors, C: Integer;
  EnabledActions: TDebugExpertActions;

  procedure OutputToolMessage(const Msg: string);
  begin
    if Assigned(FCurrentProject) then
      OTAMessageServices.AddToolMessage(FCurrentProject.FileName, Msg,
        JclDebugMessagePrefix, 1, 1)
    else
      OTAMessageServices.AddToolMessage('', Msg, JclDebugMessagePrefix, 1, 1);
  end;

begin
  if JclDisablePostCompilationProcess or (FCurrentProject = nil) then
    Exit;

  EnabledActions := GetProjectActions(FCurrentProject);
  if EnabledActions <> [] then
  begin
    ProjectFileName := FCurrentProject.FileName;
    OutputDirectory := GetOutputDirectory(FCurrentProject);
    MapFileName := GetMapFileName(FCurrentProject);
    DrcFileName := GetDrcFileName(FCurrentProject);
    JdbgFileName := ChangeFileExt(MapFileName, JclDbgFileExtension);

    if Succeeded then
    begin
      Screen.Cursor := crHourGlass;
      try
        LinkerBugUnit := '';
        LineNumberErrors := 0;

        Succ := FileExists(MapFileName);
        if not Succ then
          OutputToolMessage(Format(RsEMapFileNotFound, [MapFileName, ProjectFileName]));

        // creation of .jdbg
        if Succ and (deGenerateJdbg in EnabledActions) then
        begin
          Succ := ConvertMapFileToJdbgFile(MapFileName, LinkerBugUnit, LineNumberErrors,
            MapFileSize, JclDebugDataSize);
          if Succ then
            OutputToolMessage(Format(RsConvertedMapToJdbg, [MapFileName, MapFileSize, JclDebugDataSize]))
          else
            OutputToolMessage(Format(RsEMapConversion, [MapFileName]));
        end;

        // insertion of JEDI Debug Information into the binary
        if Succ and (deInsertJdbg in EnabledActions) then
        begin
          Succ := FindExecutableName(MapFileName, OutputDirectory, ExecutableFileName);
          if Succ then
          begin
            Succ := InsertDebugDataIntoExecutableFile(ExecutableFileName, MapFileName,
              LinkerBugUnit, MapFileSize, JclDebugDataSize, LineNumberErrors);
            if Succ then
              OutputToolMessage(Format(RsInsertedJdbg, [MapFileName, MapFileSize, JclDebugDataSize]))
            else
              OutputToolMessage(Format(RsEMapConversion, [MapFileName]));
          end
          else
            OutputToolMessage(Format(RsEExecutableNotFound, [ProjectFileName]));
        end;

        // deletion of MAP files
        if Succ and (deDeleteMapFile in EnabledActions) then
        begin
          Succ := DeleteFile(MapFileName);
          if Succ then
            OutputToolMessage(Format(RsDeletedMapFile, ['MAP', MapFileName]))
          else
            OutputToolMessage(Format(RsEFailedToDeleteMapFile, ['MAP', MapFileName]));
          if DeleteFile(DrcFileName) then
            OutputToolMessage(Format(RsDeletedMapFile, ['DRC', DrcFileName]))
          else
            OutputToolMessage(Format(RsEFailedToDeleteMapFile, ['DRC', DrcFileName]));
        end;

        Screen.Cursor := crDefault;
      except
        Screen.Cursor := crDefault;
        raise;
      end;

      if FStoreResults then
      begin
        C := Length(FResultInfo);
        SetLength(FResultInfo, C + 1);
        FResultInfo[C].ProjectName := ExtractFileName(ProjectFileName);
        FResultInfo[C].ExecutableFileName := ExecutableFileName;
        FResultInfo[C].MapFileSize := MapFileSize;
        FResultInfo[C].JclDebugDataSize := JclDebugDataSize;
        FResultInfo[C].LinkerBugUnit := LinkerBugUnit;
        FResultInfo[C].LineNumberErrors := LineNumberErrors;
        FResultInfo[C].Success := Succ;
      end;
    end
    else
      FBuildError := True;
    FCurrentProject := nil;
  end;
end;

procedure TJclDebugExtension.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
var
  ProjOptions: IOTAProjectOptions;
  EnabledActions: TDebugExpertActions;
begin
  EnabledActions := GetProjectActions(Project);
  if EnabledActions <> [] then
  begin
    if IsInstalledPackage(Project) then
    begin
      if MessageDlg(Format(RsCantInsertToInstalledPackage, [Project.FileName]), mtError, [mbYes, mbNo], 0) = mrYes then
      begin
        DisableExpert(Project);
        MessageDlg(RsDisabledDebugExpert, mtInformation, [mbOK], 0);
      end
      else
      begin
        Cancel := True;
        MessageDlg(RsCompilationAborted, mtError, [mbOK], 0);
      end;
    end
    else
    begin
      FCurrentProject := Project;
      ProjOptions := Project.ProjectOptions;
      if not Assigned(ProjOptions) then
        raise EJclExpertException.CreateTrace(RsENoProjectOptions);


      if ProjOptions.Values[MapFileOptionName] <> MapFileOptionDetailed then
      begin
        if MessageDlg(Format(RsChangeMapFileOption, [ExtractFileName(Project.FileName)]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          ProjOptions.Values[MapFileOptionName] := MapFileOptionDetailed;
          ProjOptions.ModifiedState := True;
        end
        else
        begin
          DisableExpert(Project);
          MessageDlg(RsDisabledDebugExpert, mtInformation, [mbOK], 0);
        end;
      end;
    end;
  end;
end;

procedure TJclDebugExtension.BeginStoreResults;
begin
  FBuildError := False;
  FStoreResults := True;
  FResultInfo := nil;
end;

procedure TJclDebugExtension.BuildAllProjects(Sender: TObject);
begin
  BeginStoreResults;
  try
    try
      FSaveBuildAllProjectsActionExecute(Sender);
      DisplayResults;
    except
      on ExceptionObj: TObject do
        JclExpertShowExceptionDialog(ExceptionObj);
      // raise is useless because trapped by the finally section
    end;
  finally
    EndStoreResults;
  end;
end;

procedure TJclDebugExtension.BuildProject(Sender: TObject);
begin
  BeginStoreResults;
  try
    try
      FSaveBuildProjectActionExecute(Sender);
      DisplayResults;
    except
      on ExceptionObj: TObject do
        JclExpertShowExceptionDialog(ExceptionObj);
      // raise is useless because trapped by the finally section
    end;
  finally
    EndStoreResults;
  end;
end;

procedure TJclDebugExtension.DisableExpert(const AProject: IOTAProject);
begin
  ProjectStates[deGenerateJdbg, AProject] := DisableDebugExpertState(ProjectStates[deGenerateJdbg, AProject]);
  ProjectStates[deInsertJdbg, AProject] := DisableDebugExpertState(ProjectStates[deInsertJdbg, AProject]);
  ProjectStates[deDeleteMapFile, AProject] := DisableDebugExpertState(ProjectStates[deDeleteMapFile, AProject]);
end;

procedure TJclDebugExtension.DisplayResults;
var
  I: Integer;
begin
  if FBuildError or (Length(FResultInfo) = 0) then
    Exit;
  with TJclDebugResultForm.Create(Application, Settings) do
  try
    for I := 0 to Length(FResultInfo) - 1 do
      with ResultListView.Items.Add, FResultInfo[I] do
      begin
        Caption := ProjectName;
        if Success then
        begin
          SubItems.Add(IntToStr(MapFileSize));
          SubItems.Add(IntToStr(JclDebugDataSize));
          SubItems.Add(Format('%3.1f', [JclDebugDataSize * 100 / MapFileSize]));
          SubItems.Add(ExecutableFileName);
          SubItems.Add(LinkerBugUnit);
          if LineNumberErrors > 0 then
            SubItems.Add(IntToStr(LineNumberErrors))
          else
            SubItems.Add('');
          ImageIndex := 0;
        end
        else
        begin
          SubItems.Add('');
          SubItems.Add('');
          SubItems.Add('');
          SubItems.Add(ExecutableFileName);
          SubItems.Add(LinkerBugUnit);
          SubItems.Add('');
          ImageIndex := 1;
        end;
      end;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TJclDebugExtension.EndStoreResults;
begin
  FStoreResults := False;
  FResultInfo := nil;
end;

procedure TJclDebugExtension.DebugExpertActionExecute(Sender: TObject);
var
  AProject: IOTAProject;
begin
  try
    AProject := ActiveProject;
    if AProject <> nil then
    begin
      if ProjectActions[AProject] <> [] then
      begin
        // disable all actions
        ProjectStates[deGenerateJdbg, AProject] := DisableDebugExpertState(ProjectStates[deGenerateJdbg, AProject]);
        ProjectStates[deInsertJdbg, AProject] := DisableDebugExpertState(ProjectStates[deInsertJdbg, AProject]);
        ProjectStates[deDeleteMapFile, AProject] := DisableDebugExpertState(ProjectStates[deDeleteMapFile, AProject]);
      end
      else
      begin
        // enable all actions
        ProjectStates[deGenerateJdbg, AProject] := EnableDebugExpertState(ProjectStates[deGenerateJdbg, AProject]);
        ProjectStates[deInsertJdbg, AProject] := EnableDebugExpertState(ProjectStates[deInsertJdbg, AProject]);
        ProjectStates[deDeleteMapFile, AProject] := EnableDebugExpertState(ProjectStates[deDeleteMapFile, AProject]);
      end;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TJclDebugExtension.DebugExpertActionUpdate(Sender: TObject);
var
  AAction: TCustomAction;
  AEnabled: Boolean;
  AProject: IOTAProject;
begin
  try
    AAction := Sender as TCustomAction;
    AProject := ActiveProject;
    AEnabled := AProject <> nil;
    AAction.Enabled := AEnabled;
    if AEnabled then
    begin
      AAction.Checked := ProjectActions[AProject] <> [];
      AAction.ImageIndex := FDebugImageIndex;
    end
    else
      AAction.ImageIndex := FNoDebugImageIndex;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclDebugExtension.DebugExpertMenuClick(Sender: TObject);
var
  EnabledActions: TDebugExpertActions;
  AProject: IOTAProject;
begin
  try
    AProject := ActiveProject;
    if AProject <> nil then
      EnabledActions := ProjectActions[AProject]
    else
      EnabledActions := [];
    FGenerateJdbgItem.Checked := deGenerateJdbg in EnabledActions;
    FInsertJdbgItem.Checked := deInsertJdbg in EnabledActions;
    FDeleteMapFileItem.Checked := deDeleteMapFile in EnabledActions;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclDebugExtension.DebugExpertMenuDropDown(Sender: TObject);
var
  CheckTag, Index: Integer;
  APopupMenu: TPopupMenu;
  AMenuItem: TMenuItem;
  AProject: IOTAProject;
  TestState: TDebugExpertState;
  IndexAction: TDebugExpertAction;
begin
  try
    AProject := ActiveProject;
    if AProject <> nil then
    begin
      TestState := ProjectStates[Low(TDebugExpertAction), AProject];
      CheckTag := DebugExpertStateToInt(TestState);
      for IndexAction := Succ(Low(TDebugExpertAction)) to High(TDebugExpertAction) do
        if TestState <> ProjectStates[IndexAction, AProject] then
      begin
        CheckTag := -1;
        Break;
      end;
    end
    else
    begin
      TestState := GlobalStates[Low(TDebugExpertAction)];
      CheckTag := DebugExpertStateToInt(TestState);
      for IndexAction := Succ(Low(TDebugExpertAction)) to High(TDebugExpertAction) do
        if TestState <> GlobalStates[IndexAction] then
      begin
        CheckTag := -1;
        Break;
      end;
    end;
    APopupMenu := Sender as TPopupMenu;
    for Index := 0 to APopupMenu.Items.Count - 1 do
    begin
      AMenuItem := APopupMenu.Items.Items[Index];
      AMenuItem.Enabled := (AProject <> nil) or (AMenuItem.Tag = DebugExpertStateToInt(deAlwaysDisabled))
        or (AMenuItem.Tag = DebugExpertStateToInt(deAlwaysEnabled));
      AMenuItem.Checked := AMenuItem.Tag = CheckTag;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclDebugExtension.DebugExpertSubMenuClick(Sender: TObject);
var
  AState: TDebugExpertState;
  AProject: IOTAProject;
begin
  try
    AState := IntToDebugExpertState((Sender as TComponent).Tag);
    AProject := ActiveProject;
    if AProject <> nil then
    begin
      ProjectStates[deGenerateJdbg, AProject] := AState;
      ProjectStates[deInsertJdbg, AProject] := AState;
      ProjectStates[deDeleteMapFile, AProject] := AState;
    end
    else
    begin
      GlobalStates[deGenerateJdbg] := AState;
      GlobalStates[deInsertJdbg] := AState;
      GlobalStates[deDeleteMapFile] := AState;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclDebugExtension.DeleteMapFileActionExecute(Sender: TObject);
var
  AProject: IOTAProject;
begin
  try
    AProject := ActiveProject;
    if AProject <> nil then
      ProjectStates[deDeleteMapFile, AProject] := ToggleDebugExpertState(ProjectStates[deDeleteMapFile, AProject])
    else
      GlobalStates[deDeleteMapFile] := ToggleDebugExpertState(GlobalStates[deDeleteMapFile]);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TJclDebugExtension.DeleteMapFileActionUpdate(Sender: TObject);
var
  AAction: TCustomAction;
  AEnabled: Boolean;
  AProject: IOTAProject;
begin
  try
    AAction := Sender as TCustomAction;
    AProject := ActiveProject;
    AEnabled := AProject <> nil;
    AAction.Enabled := AEnabled;
    if AEnabled then
    begin
      AAction.Checked := ProjectStates[deDeleteMapFile, AProject] in [deAlwaysEnabled, deProjectEnabled];
      AAction.ImageIndex := FDeleteMapFileImageIndex;
    end
    else
    begin
      AAction.Checked := False;
      AAction.ImageIndex := FNoDeleteMapFileImageIndex;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclDebugExtension.DeleteMapFileMenuClick(Sender: TObject);
var
  AMenuItem, BMenuItem: TMenuItem;
  CheckTag, Index: Integer;
  AProject: IOTAProject;
begin
  try
    AProject := ActiveProject;
    if AProject <> nil then
      CheckTag := DebugExpertStateToInt(ProjectStates[deDeleteMapFile, AProject])
    else
      CheckTag := DebugExpertStateToInt(GlobalStates[deDeleteMapFile]);
    AMenuItem := Sender as TMenuItem;
    for Index := 0 to AMenuItem.Count - 1 do
    begin
      BMenuItem := AMenuItem.Items[Index];
      BMenuItem.Enabled := (AProject <> nil) or (BMenuItem.Tag = DebugExpertStateToInt(deAlwaysDisabled))
        or (BMenuItem.Tag = DebugExpertStateToInt(deAlwaysEnabled));
      BMenuItem.Checked := BMenuItem.Tag = CheckTag;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclDebugExtension.DeleteMapFileMenuDropDown(Sender: TObject);
var
  AMenu: TPopupMenu;
  AMenuItem: TMenuItem;
  CheckTag, Index: Integer;
  AProject: IOTAProject;
begin
  try
    AProject := ActiveProject;
    if AProject <> nil then
      CheckTag := DebugExpertStateToInt(ProjectStates[deDeleteMapFile, AProject])
    else
      CheckTag := DebugExpertStateToInt(GlobalStates[deDeleteMapFile]);
    AMenu := Sender as TPopupMenu;
    for Index := 0 to AMenu.Items.Count - 1 do
    begin
      AMenuItem := AMenu.Items.Items[Index];
      AMenuItem.Enabled := (AProject <> nil) or (AMenuItem.Tag = DebugExpertStateToInt(deAlwaysDisabled))
        or (AMenuItem.Tag = DebugExpertStateToInt(deAlwaysEnabled));
      AMenuItem.Checked := AMenuItem.Tag = CheckTag;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclDebugExtension.DeleteMapFileSubMenuClick(Sender: TObject);
var
  AState: TDebugExpertState;
  AProject: IOTAProject;
begin
  try
    AState := IntToDebugExpertState((Sender as TComponent).Tag);
    AProject := ActiveProject;
    if AProject <> nil then
      ProjectStates[deDeleteMapFile, AProject] := AState
    else
      GlobalStates[deDeleteMapFile] := AState; 
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclDebugExtension.GenerateJdbgActionExecute(Sender: TObject);
var
  AProject: IOTAProject;
begin
  try
    AProject := ActiveProject;
    if AProject <> nil then
      ProjectStates[deGenerateJdbg, AProject] := ToggleDebugExpertState(ProjectStates[deGenerateJdbg, AProject])
    else
      GlobalStates[deGenerateJdbg] := ToggleDebugExpertState(GlobalStates[deGenerateJdbg]);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TJclDebugExtension.GenerateJdbgActionUpdate(Sender: TObject);
var
  AAction: TCustomAction;
  AEnabled: Boolean;
  AProject: IOTAProject;
begin
  try
    AAction := Sender as TCustomAction;
    AProject := ActiveProject;
    AEnabled := AProject <> nil;
    AAction.Enabled := AEnabled;
    if AEnabled then
    begin
      AAction.Checked := ProjectStates[deGenerateJdbg, AProject] in [deAlwaysEnabled, deProjectEnabled];
      AAction.ImageIndex := FGenerateJdbgImageIndex;
    end
    else
    begin
      AAction.Checked := False;
      AAction.ImageIndex := FNoGenerateJdbgImageIndex;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclDebugExtension.GenerateJdbgMenuClick(Sender: TObject);
var
  AMenuItem, BMenuItem: TMenuItem;
  CheckTag, Index: Integer;
  AProject: IOTAProject;
begin
  try
    AProject := ActiveProject;
    if AProject <> nil then
      CheckTag := DebugExpertStateToInt(ProjectStates[deGenerateJdbg, AProject])
    else
      CheckTag := DebugExpertStateToInt(GlobalStates[deGenerateJdbg]);
    AMenuItem := Sender as TMenuItem;
    for Index := 0 to AMenuItem.Count - 1 do
    begin
      BMenuItem := AMenuItem.Items[Index];
      BMenuItem.Enabled := (AProject <> nil) or (BMenuItem.Tag = DebugExpertStateToInt(deAlwaysDisabled))
        or (BMenuItem.Tag = DebugExpertStateToInt(deAlwaysEnabled));
      BMenuItem.Checked := BMenuItem.Tag = CheckTag;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclDebugExtension.GenerateJdbgMenuDropDown(Sender: TObject);
var
  AMenu: TPopupMenu;
  AMenuItem: TMenuItem;
  CheckTag, Index: Integer;
  AProject: IOTAProject;
begin
  try
    AProject := ActiveProject;
    if AProject <> nil then
      CheckTag := DebugExpertStateToInt(ProjectStates[deGenerateJdbg, AProject])
    else
      CheckTag := DebugExpertStateToInt(GlobalStates[deGenerateJdbg]);
    AMenu := Sender as TPopupMenu;
    for Index := 0 to AMenu.Items.Count - 1 do
    begin
      AMenuItem := AMenu.Items.Items[Index];
      AMenuItem.Enabled := (AProject <> nil) or (AMenuItem.Tag = DebugExpertStateToInt(deAlwaysDisabled))
        or (AMenuItem.Tag = DebugExpertStateToInt(deAlwaysEnabled));
      AMenuItem.Checked := AMenuItem.Tag = CheckTag;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclDebugExtension.GenerateJdbgSubMenuClick(Sender: TObject);
var
  AState: TDebugExpertState;
  AProject: IOTAProject;
begin
  try
    AState := IntToDebugExpertState((Sender as TComponent).Tag);
    AProject := ActiveProject;
    if AProject <> nil then
      ProjectStates[deGenerateJdbg, AProject] := AState
    else
      GlobalStates[deGenerateJdbg] := AState;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

function TJclDebugExtension.GetGlobalState(Index: TDebugExpertAction): TDebugExpertState;
begin
  Result := FGlobalStates[Index];
end;

function TJclDebugExtension.GetProjectActions(const AProject: IOTAProject): TDebugExpertActions;
var
  PropIDs, PropValues: TDynAnsiStringArray;
  Index: TDebugExpertAction;
begin
  SetLength(PropIDs, Integer(High(TDebugExpertAction)) - Integer(Low(TDebugExpertAction)) + 1);
  for Index := Low(TDebugExpertAction) to High(TDebugExpertAction) do
    PropIDs[Integer(Index)] := DebugActionNames[Index];
  PropValues := GetProjectProperties(AProject, PropIDs);
  Result := [];
  for Index := Low(TDebugExpertAction) to High(TDebugExpertAction) do
    case FGlobalStates[Index] of
      deAlwaysEnabled:
        Include(Result, Index);
      deProjectEnabled:
        if PropValues[Integer(Index)] <> DebugActionValues[False] then
          Include(Result, Index);
      deProjectDisabled:
        if PropValues[Integer(Index)] = DebugActionValues[True] then
          Include(Result, Index);
    end;
end;

function TJclDebugExtension.GetProjectState(Index: TDebugExpertAction; const AProject: IOTAProject): TDebugExpertState;
var
  PropIDs: TDynAnsiStringArray;
begin
  case FGlobalStates[Index] of
    deAlwaysDisabled:
      Result := deAlwaysDisabled;
    deProjectDisabled:
      begin
        SetLength(PropIDs, 1);
        PropIDs[0] := DebugActionNames[Index];
        if GetProjectProperties(AProject, PropIDs)[0] = DebugActionValues[True] then
          Result := deProjectEnabled
        else
          Result := deProjectDisabled;
      end;
    deProjectEnabled:
      begin
        SetLength(PropIDs, 1);
        PropIDs[0] := DebugActionNames[Index];
        if GetProjectProperties(AProject, PropIDs)[0] <> DebugActionValues[False] then
          Result := deProjectEnabled
        else
          Result := deProjectDisabled;
      end;
    deAlwaysEnabled:
      Result := deAlwaysEnabled;
  else
    raise EJclExpertException.CreateResFmt(@RsEInvalidDebugExpertState, [Integer(FGlobalStates[Index])]);
  end;
end;

procedure TJclDebugExtension.InsertJdbgActionExecute(Sender: TObject);
var
  AProject: IOTAProject;
begin
  try
    AProject := ActiveProject;
    if AProject <> nil then
      ProjectStates[deInsertJdbg, AProject] := ToggleDebugExpertState(ProjectStates[deInsertJdbg, AProject])
    else
      GlobalStates[deInsertJdbg] := ToggleDebugExpertState(GlobalStates[deInsertJdbg]);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TJclDebugExtension.InsertJdbgActionUpdate(Sender: TObject);
var
  AAction: TCustomAction;
  AEnabled: Boolean;
  AProject: IOTAProject;
begin
  try
    AAction := Sender as TCustomAction;
    AProject := ActiveProject;
    AEnabled := AProject <> nil;
    AAction.Enabled := AEnabled;
    if AEnabled then
    begin
      AAction.Checked := ProjectStates[deInsertJdbg, AProject] in [deAlwaysEnabled, deProjectEnabled];
      AAction.ImageIndex := FInsertJdbgImageIndex
    end
    else
    begin
      AAction.Checked := False;
      AAction.ImageIndex := FNoInsertJdbgImageIndex;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclDebugExtension.InsertJdbgMenuClick(Sender: TObject);
var
  AMenuItem, BMenuItem: TMenuItem;
  CheckTag, Index: Integer;
  AProject: IOTAProject;
begin
  try
    AProject := ActiveProject;
    if AProject <> nil then
      CheckTag := DebugExpertStateToInt(ProjectStates[deInsertJdbg, AProject])
    else
      CheckTag := DebugExpertStateToInt(GlobalStates[deInsertJdbg]);
    AMenuItem := Sender as TMenuItem;
    for Index := 0 to AMenuItem.Count - 1 do
    begin
      BMenuItem := AMenuItem.Items[Index];
      BMenuItem.Enabled := (AProject <> nil) or (BMenuItem.Tag = DebugExpertStateToInt(deAlwaysDisabled))
        or (BMenuItem.Tag = DebugExpertStateToInt(deAlwaysEnabled));
      BMenuItem.Checked := BMenuItem.Tag = CheckTag;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclDebugExtension.InsertJdbgMenuDropDown(Sender: TObject);
var
  AMenu: TPopupMenu;
  AMenuItem: TMenuItem;
  CheckTag, Index: Integer;
  AProject: IOTAProject;
begin
  try
    AProject := ActiveProject;
    if AProject <> nil then
      CheckTag := DebugExpertStateToInt(ProjectStates[deInsertJdbg, AProject])
    else
      CheckTag := DebugExpertStateToInt(GlobalStates[deInsertJdbg]);
    AMenu := Sender as TPopupMenu;
    for Index := 0 to AMenu.Items.Count - 1 do
    begin
      AMenuItem := AMenu.Items.Items[Index];
      AMenuItem.Enabled := (AProject <> nil) or (AMenuItem.Tag = DebugExpertStateToInt(deAlwaysDisabled))
        or (AMenuItem.Tag = DebugExpertStateToInt(deAlwaysEnabled));
      AMenuItem.Checked := AMenuItem.Tag = CheckTag;
    end;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclDebugExtension.InsertJdbgSubMenuClick(Sender: TObject);
var
  AState: TDebugExpertState;
  AProject: IOTAProject;
begin
  try
    AState := IntToDebugExpertState((Sender as TComponent).Tag);
    AProject := ActiveProject;
    if AProject <> nil then
      ProjectStates[deInsertJdbg, AProject] := AState
    else
      GlobalStates[deInsertJdbg] := AState;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
    end;
  end;
end;

procedure TJclDebugExtension.LoadExpertValues;
begin
  GlobalStates[deGenerateJdbg] := IntToDebugExpertState(Settings.LoadInteger(JclDebugGenerateJdbgRegValue, 0));
  GlobalStates[deInsertJdbg] := IntToDebugExpertState(Settings.LoadInteger(JclDebugInsertJdbgRegValue, 0));
  GlobalStates[deDeleteMapFile] := IntToDebugExpertState(Settings.LoadInteger(JclDebugDeleteMapFileRegValue, 0));
end;

procedure TJclDebugExtension.SaveExpertValues;
begin
  Settings.SaveInteger(JclDebugGenerateJdbgRegValue, DebugExpertStateToInt(GlobalStates[deGenerateJdbg]));
  Settings.SaveInteger(JclDebugInsertJdbgRegValue, DebugExpertStateToInt(GlobalStates[deInsertJdbg]));
  Settings.SaveInteger(JclDebugDeleteMapFileRegValue, DebugExpertStateToInt(GlobalStates[deDeleteMapFile]));
end;

procedure TJclDebugExtension.SetGlobalState(Index: TDebugExpertAction; Value: TDebugExpertState);
begin
  FGlobalStates[Index] := Value;
end;

procedure TJclDebugExtension.SetProjectState(Index: TDebugExpertAction; const AProject: IOTAProject;
  Value: TDebugExpertState);
var
  PropIDs, PropValues: TDynAnsiStringArray;
begin
  case Value of
    deAlwaysDisabled:
      FGlobalStates[Index] := deAlwaysDisabled;
    deProjectDisabled:
      begin
        if not (GlobalStates[Index] in [deProjectDisabled, deProjectEnabled]) then
          FGlobalStates[Index] := deProjectDisabled;
        SetLength(PropIDs, 1);
        PropIDs[0] := DebugActionNames[Index];
        SetLength(PropValues, 1);
        PropValues[0] := DebugActionValues[False];
        SetProjectProperties(AProject, PropIDs, PropValues);
      end;
    deProjectEnabled:
      begin
        if not (GlobalStates[Index] in [deProjectDisabled, deProjectEnabled]) then
          FGlobalStates[Index] := deProjectEnabled;
        SetLength(PropIDs, 1);
        PropIDs[0] := DebugActionNames[Index];
        SetLength(PropValues, 1);
        PropValues[0] := DebugActionValues[True];
        SetProjectProperties(AProject, PropIDs, PropValues);
      end;
    deAlwaysEnabled:
      FGlobalStates[Index] := deAlwaysEnabled;
  end;
end;

procedure TJclDebugExtension.RegisterCommands;
  procedure FillMenu(AMenuItem: TMenuItem; AEvent: TNotifyEvent);
  var
    BMenuItem: TMenuItem;
  begin
    BMenuItem := TMenuItem.Create(AMenuItem);
    BMenuItem.Caption := RsAlwaysEnabled;
    BMenuItem.RadioItem := True;
    BMenuItem.Tag := DebugExpertStateToInt(deAlwaysEnabled);
    BMenuItem.OnClick := AEvent;
    AMenuItem.Add(BMenuItem);

    BMenuItem := TMenuItem.Create(AMenuItem);
    BMenuItem.Caption := RsProjectEnabled;
    BMenuItem.RadioItem := True;
    BMenuItem.Tag := DebugExpertStateToInt(deProjectEnabled);
    BMenuItem.OnClick := AEvent;
    AMenuItem.Add(BMenuItem);

    BMenuItem := TMenuItem.Create(AMenuItem);
    BMenuItem.Caption := RsProjectDisabled;
    BMenuItem.RadioItem := True;
    BMenuItem.Tag := DebugExpertStateToInt(deProjectDisabled);
    BMenuItem.OnClick := AEvent;
    AMenuItem.Add(BMenuItem);

    BMenuItem := TMenuItem.Create(AMenuItem);
    BMenuItem.Caption := RsAlwaysDisabled;
    BMenuItem.RadioItem := True;
    BMenuItem.Tag := DebugExpertStateToInt(deAlwaysDisabled);
    BMenuItem.OnClick := AEvent;
    AMenuItem.Add(BMenuItem);
  end;
var
  IDEMainMenu: TMainMenu;
  IDEProjectItem: TMenuItem;
  IDEActionList: TActionList;
  I: Integer;
  ImageBmp: TBitmap;
begin
  inherited RegisterCommands;
  IDEActionList := TActionList(NTAServices.ActionList);
  IDEMainMenu := NTAServices.MainMenu;
  ImageBmp := TBitmap.Create;
  try
    // load images
    ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'JCLDEBUG');
    FDebugImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);
    ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'JCLNODEBUG');
    FNoDebugImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);
    ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'JCLGENERATEJDBG');
    FGenerateJdbgImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);
    ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'JCLNOGENERATEJDBG');
    FNoGenerateJdbgImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);
    ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'JCLINSERTJDBG');
    FInsertJdbgImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);
    ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'JCLNOINSERTJDBG');
    FNoInsertJdbgImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);
    ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'JCLDELETEMAP');
    FDeleteMapFileImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);
    ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'JCLNODELETEMAP');
    FNoDeleteMapFileImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);

    // create actions
    FDebugExpertAction := TDropDownAction.Create(nil);
    FDebugExpertAction.Caption := RsDebugExpertCaption;
    FDebugExpertAction.Visible := True;
    FDebugExpertAction.ImageIndex := FDebugImageIndex;
    FDebugExpertAction.OnUpdate := DebugExpertActionUpdate;
    FDebugExpertAction.OnExecute := DebugExpertActionExecute;
    FDebugExpertAction.ActionList := IDEActionList;
    FDebugExpertAction.Name := JclDebugExpertActionName;
    FDebugExpertAction.DropdownMenu := TPopupMenu.Create(nil);
    FDebugExpertAction.DropdownMenu.OnPopup := DebugExpertMenuDropDown;
    FDebugExpertAction.DropdownMenu.AutoPopup := True;
    FillMenu(FDebugExpertAction.DropDownMenu.Items, DebugExpertSubMenuClick);
    RegisterAction(FDebugExpertAction);

    FGenerateJdbgAction := TDropDownAction.Create(nil);
    FGenerateJdbgAction.Caption := RsDebugGenerateJdbg;
    FGenerateJdbgAction.Visible := True;
    FGenerateJdbgAction.ImageIndex := FGenerateJdbgImageIndex;
    FGenerateJdbgAction.OnUpdate := GenerateJdbgActionUpdate;
    FGenerateJdbgAction.OnExecute := GenerateJdbgActionExecute;
    FGenerateJdbgAction.ActionList := IDEActionList;
    FGenerateJdbgAction.Name := JclGenerateJdbgActionName;
    FGenerateJdbgAction.DropdownMenu := TPopupMenu.Create(nil);
    FGenerateJdbgAction.DropdownMenu.OnPopup := GenerateJdbgMenuDropDown;
    FGenerateJdbgAction.DropdownMenu.AutoPopup := True;
    FillMenu(FGenerateJdbgAction.DropDownMenu.Items, GenerateJdbgSubMenuClick);
    RegisterAction(FGenerateJdbgAction);

    FInsertJdbgAction := TDropDownAction.Create(nil);
    FInsertJdbgAction.Caption := RsDebugInsertJdbg;
    FInsertJdbgAction.Visible := True;
    FInsertJdbgAction.ImageIndex := FInsertJdbgImageIndex;
    FInsertJdbgAction.OnUpdate := InsertJdbgActionUpdate;
    FInsertJdbgAction.OnExecute := InsertJdbgActionExecute;
    FInsertJdbgAction.ActionList := IDEActionList;
    FInsertJdbgAction.Name := JclInsertJdbgActionName;
    FInsertJdbgAction.DropdownMenu := TPopupMenu.Create(nil);
    FInsertJdbgAction.DropdownMenu.OnPopup := InsertJdbgMenuDropDown;
    FInsertJdbgAction.DropdownMenu.AutoPopup := True;
    FillMenu(FInsertJdbgAction.DropDownMenu.Items, InsertJdbgSubMenuClick);
    RegisterAction(FInsertJdbgAction);

    FDeleteMapFileAction := TDropDownAction.Create(nil);
    FDeleteMapFileAction.Caption := RsDeleteMapFile;
    FDeleteMapFileAction.Visible := True;
    FDeleteMapFileAction.ImageIndex := FDeleteMapFileImageIndex;
    FDeleteMapFileAction.OnUpdate := DeleteMapFileActionUpdate;
    FDeleteMapFileAction.OnExecute := DeleteMapFileActionExecute;
    FDeleteMapFileAction.ActionList := IDEActionList;
    FDeleteMapFileAction.Name := JclDeleteMapFileActionName;
    FDeleteMapFileAction.DropdownMenu := TPopupMenu.Create(nil);
    FDeleteMapFileAction.DropdownMenu.OnPopup := DeleteMapFileMenuDropDown;
    FDeleteMapFileAction.DropdownMenu.AutoPopup := True;
    FillMenu(FDeleteMapFileAction.DropDownMenu.Items, DeleteMapFileSubMenuClick);
    RegisterAction(FDeleteMapFileAction);

    // create menu items
    FDebugExpertItem := TMenuItem.Create(nil);
    FDebugExpertItem.Name := JclDebugExpertMenuName;
    FDebugExpertItem.Caption := RsDebugExpertCaption;
    FDebugExpertItem.OnClick := DebugExpertMenuClick;
    FDebugExpertItem.ImageIndex := FDebugImageIndex;

    FGenerateJdbgItem := TMenuItem.Create(nil);
    FGenerateJdbgItem.Name := JclGenerateJdbgMenuName;
    FGenerateJdbgItem.Caption := RsDebugGenerateJdbg;
    FGenerateJdbgItem.OnClick := GenerateJdbgMenuClick;
    FGenerateJdbgItem.ImageIndex := FGenerateJdbgImageIndex;
    FillMenu(FGenerateJdbgItem, GenerateJdbgSubMenuClick);
    FDebugExpertItem.Add(FGenerateJdbgItem);

    FInsertJdbgItem := TMenuItem.Create(nil);
    FInsertJdbgItem.Name := JclInsertJdbgMenuName;
    FInsertJdbgItem.Caption := RsDebugInsertJdbg;
    FInsertJdbgItem.OnClick := InsertJdbgMenuClick;
    FInsertJdbgItem.ImageIndex := FInsertJdbgImageIndex;
    FillMenu(FInsertJdbgItem, InsertJdbgSubMenuClick);
    FDebugExpertItem.Add(FInsertJdbgItem);

    FDeleteMapFileItem := TMenuItem.Create(nil);
    FDeleteMapFileItem.Name := JclDeleteMapFileMenuName;
    FDeleteMapFileItem.Caption := RsDeleteMapFile;
    FDeleteMapFileItem.OnClick := DeleteMapFileMenuClick;
    FDeleteMapFileItem.ImageIndex := FDeleteMapFileImageIndex;
    FillMenu(FDeleteMapFileItem, DeleteMapFileSubMenuClick);
    FDebugExpertItem.Add(FDeleteMapFileItem);
  finally
    ImageBmp.Free;
  end;

  // register notifiers
  FIDENotifierIndex := Services.AddNotifier(TIdeNotifier.Create(Self));
  {$IFDEF BDS4_UP}
  FProjectManagerNotifierIndex := ProjectManager.AddMenuCreatorNotifier(TProjectManagerNotifier.Create(Self,
    NTAServices, ProjectManager));
  {$ENDIF BDS4_UP}

  LoadExpertValues;

  // insert menus
  IDEProjectItem := nil;
  with IDEMainMenu do
    for I := 0 to Items.Count - 1 do
      if Items[I].Name = 'ProjectMenu' then
      begin
        IDEProjectItem := Items[I];
        Break;
      end;
  if not Assigned(IDEProjectItem) then
    raise EJclExpertException.CreateTrace(RsENoProjectMenuItem);

  with IDEProjectItem do
    for I := 0 to Count - 1 do
      if Items[I].Name = 'ProjectOptionsItem' then
      begin
        if Assigned(Items[I].Action) then
        begin
          FDebugExpertAction.Category := TContainedAction(Items[I].Action).Category;
          FGenerateJdbgAction.Category := FDebugExpertAction.Category;
          FInsertJdbgAction.Category := FDebugExpertAction.Category;
          FDeleteMapFileAction.Category := FDebugExpertAction.Category;
        end;
        IDEProjectItem.Insert(I + 1, FDebugExpertItem);
        System.Break;
      end;
  if not Assigned(FDebugExpertItem.Parent) then
     raise EJclExpertException.CreateTrace(RsEInsertDataMenuItemNotInserted);

  // hook actions
  FSaveBuildProjectAction := nil;
  with IDEActionList do
    for I := 0 to ActionCount - 1 do
      if Actions[I].Name = 'ProjectBuildCommand' then
      begin
        FSaveBuildProjectAction := TCustomAction(Actions[I]);
        FSaveBuildProjectActionExecute := FSaveBuildProjectAction.OnExecute;
        FSaveBuildProjectAction.OnExecute := BuildProject;
        Break;
      end;
  if not Assigned(FSaveBuildProjectAction) then
     raise EJclExpertException.CreateTrace(RsENoBuildAction);

  FSaveBuildAllProjectsAction := nil;
  with IDEActionList do
    for I := 0 to ActionCount - 1 do
      if Actions[I].Name = 'ProjectBuildAllCommand' then
      begin
        FSaveBuildAllProjectsAction := TCustomAction(Actions[I]);
        FSaveBuildAllProjectsActionExecute := FSaveBuildAllProjectsAction.OnExecute;
        FSaveBuildAllProjectsAction.OnExecute := BuildAllProjects;
        Break;
      end;
  if not Assigned(FSaveBuildAllProjectsAction) then
     raise EJclExpertException.CreateTrace(RsENoBuildAllAction);
end;

procedure TJclDebugExtension.UnregisterCommands;
begin
  inherited UnregisterCommands;
  {$IFDEF BDS4_UP}
  if FProjectManagerNotifierIndex <> -1 then
    ProjectManager.RemoveMenuCreatorNotifier(FProjectManagerNotifierIndex);
  {$ENDIF BDS4_UP}
  if FIDENotifierIndex <> -1 then
    Services.RemoveNotifier(FIDENotifierIndex);
  // save settings
  SaveExpertValues;

  // unhook actions
  FSaveBuildProjectAction.OnExecute := FSaveBuildProjectActionExecute;
  FSaveBuildAllProjectsAction.OnExecute := FSaveBuildAllProjectsActionExecute;

  // remove menu items
  if FDebugExpertAction <> nil then
    FDebugExpertAction.DropdownMenu.Free;
  if FGenerateJdbgAction <> nil then
    FGenerateJdbgAction.DropdownMenu.Free;
  if FInsertJdbgAction <> nil then
    FInsertJdbgAction.DropdownMenu.Free;
  if FDeleteMapFileAction <> nil then
    FDeleteMapFileAction.DropdownMenu.Free;
  FGenerateJdbgItem.Free;
  FInsertJdbgItem.Free;
  FDeleteMapFileItem.Free;
  FDebugExpertItem.Free;

  // remove actions
  UnregisterAction(FDeleteMapFileAction);
  UnregisterAction(FInsertJdbgAction);
  UnregisterAction(FGenerateJdbgAction);
  UnregisterAction(FDebugExpertAction);
  FDeleteMapFileAction.Free;
  FInsertJdbgAction.Free;
  FGenerateJdbgAction.Free;
  FDebugExpertAction.Free;
end;

//=== { TIdeNotifier } =======================================================

constructor TIdeNotifier.Create(ADebugExtension: TJclDebugExtension);
begin
  inherited Create;
  FDebugExtension := ADebugExtension;
end;

procedure TIdeNotifier.AfterCompile(Succeeded: Boolean);
begin
end;

procedure TIdeNotifier.AfterCompile(Succeeded, IsCodeInsight: Boolean);
begin
  try
    if not IsCodeInsight then
      FDebugExtension.AfterCompile(Succeeded);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean);
begin
  try
    if not IsCodeInsight then
      FDebugExtension.BeforeCompile(Project, Cancel);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
end;

procedure TIdeNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
end;

{$IFDEF BDS4_UP}

constructor TProjectManagerNotifier.Create(ADebugExtension: TJclDebugExtension;
  const ANTAServices: INTAServices; const AOTAProjectManager: IOTAProjectManager);
begin
  inherited Create;
  FDebugExtension := ADebugExtension;
  FNTAServices := ANTAServices;
  FOTAProjectManager := AOTAProjectManager;
end;

function TProjectManagerNotifier.AddMenu(const Ident: string): TMenuItem;
  procedure FillSubMenu(AMenuItem: TMenuItem; const AOnClickEvent: TNotifyEvent; AState: TDebugExpertState);
  var
    SubMenuItem: TMenuItem;
  begin
    SubMenuItem := TMenuItem.Create(AMenuItem);
    SubMenuItem.Visible := True;
    SubMenuItem.Caption := RsAlwaysEnabled;
    SubMenuItem.RadioItem := True;
    SubMenuItem.Checked := AState = deAlwaysEnabled;
    SubMenuItem.Tag := DebugExpertStateToInt(deAlwaysEnabled);
    SubMenuItem.OnClick := AOnClickEvent;
    AMenuItem.Add(SubMenuItem);

    SubMenuItem := TMenuItem.Create(AMenuItem);
    SubMenuItem.Visible := True;
    SubMenuItem.Caption := RsProjectEnabled;
    SubMenuItem.RadioItem := True;
    SubMenuItem.Checked := AState = deProjectEnabled;
    SubMenuItem.Tag := DebugExpertStateToInt(deProjectEnabled);
    SubMenuItem.OnClick := AOnClickEvent;
    AMenuItem.Add(SubMenuItem);

    SubMenuItem := TMenuItem.Create(AMenuItem);
    SubMenuItem.Visible := True;
    SubMenuItem.Caption := RsProjectDisabled;
    SubMenuItem.RadioItem := True;
    SubMenuItem.Checked := AState = deProjectDisabled;
    SubMenuItem.Tag := DebugExpertStateToInt(deProjectDisabled);
    SubMenuItem.OnClick := AOnClickEvent;
    AMenuItem.Add(SubMenuItem);

    SubMenuItem := TMenuItem.Create(AMenuItem);
    SubMenuItem.Visible := True;
    SubMenuItem.Caption := RsAlwaysDisabled;
    SubMenuItem.RadioItem := True;
    SubMenuItem.Checked := AState = deAlwaysDisabled;
    SubMenuItem.Tag := DebugExpertStateToInt(deAlwaysDisabled);
    SubMenuItem.OnClick := AOnClickEvent;
    AMenuItem.Add(SubMenuItem);
  end;
var
  SelectedIdent: string;
  AProject: IOTAProject;
  ADeleteMapFileState, AGenerateJdbgState, AInsertJdbgState: TDebugExpertState;
  ActionMenuItem: TMenuItem;
begin
  try
    SelectedIdent := Ident;
    AProject := FOTAProjectManager.GetCurrentSelection(SelectedIdent);
    if AProject <> nil then
    begin
      ADeleteMapFileState := FDebugExtension.ProjectStates[deDeleteMapFile, AProject];
      AGenerateJdbgState := FDebugExtension.ProjectStates[deGenerateJdbg, AProject];
      AInsertJdbgState := FDebugExtension.ProjectStates[deInsertJdbg, AProject];

      // root item
      Result := TMenuItem.Create(nil);
      Result.Visible := True;
      Result.Caption := RsDebugExpertCaption;
      if (ADeleteMapFileState in [deAlwaysEnabled, deProjectEnabled])
        or (AGenerateJdbgState in [deAlwaysEnabled, deProjectEnabled])
        or (AInsertJdbgState in [deAlwaysEnabled, deProjectEnabled]) then
      begin
        Result.Checked := True;
        Result.ImageIndex := FDebugExtension.FDebugImageIndex
      end
      else
        Result.ImageIndex := FDebugExtension.FNoDebugImageIndex;
      Result.SubMenuImages := FNTAServices.ImageList;

      // actions items
      ActionMenuItem := TMenuItem.Create(Result);
      ActionMenuItem.Visible := True;
      ActionMenuItem.Caption := RsDebugGenerateJdbg;
      if AGenerateJdbgState in [deAlwaysEnabled, deProjectEnabled] then
      begin
        ActionMenuItem.Checked := True;
        ActionMenuItem.ImageIndex := FDebugExtension.FGenerateJdbgImageIndex;
      end
      else
        ActionMenuItem.ImageIndex := FDebugExtension.FNoGenerateJdbgImageIndex;
      FillSubMenu(ActionMenuItem, GenerateJdbgSubMenuClick, AGenerateJdbgState);
      Result.Add(ActionMenuItem);

      ActionMenuItem := TMenuItem.Create(Result);
      ActionMenuItem.Visible := True;
      ActionMenuItem.Caption := RsDebugInsertJdbg;
      if AInsertJdbgState in [deAlwaysEnabled, deProjectEnabled] then
      begin
        ActionMenuItem.Checked := True;
        ActionMenuItem.ImageIndex := FDebugExtension.FInsertJdbgImageIndex;
      end
      else
        ActionMenuItem.ImageIndex := FDebugExtension.FNoInsertJdbgImageIndex;
      FillSubMenu(ActionMenuItem, InsertJdbgSubMenuClick, AInsertJdbgState);
      Result.Add(ActionMenuItem);

      ActionMenuItem := TMenuItem.Create(Result);
      ActionMenuItem.Visible := True;
      ActionMenuItem.Caption := RsDeleteMapFile;
      if ADeleteMapFileState in [deAlwaysEnabled, deProjectEnabled] then
      begin
        ActionMenuItem.Checked := True;
        ActionMenuItem.ImageIndex := FDebugExtension.FDeleteMapFileImageIndex;
      end
      else
        ActionMenuItem.ImageIndex := FDebugExtension.FNoDeleteMapFileImageIndex;
      FillSubMenu(ActionMenuItem, DeleteMapFileSubMenuClick, ADeleteMapFileState);
      Result.Add(ActionMenuItem);
    end
    else
      raise EJclExpertException.CreateRes(@RsENoActiveProject);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

function TProjectManagerNotifier.CanHandle(const Ident: string): Boolean;
begin
  Result := Ident = sProjectContainer;
end;

procedure TProjectManagerNotifier.DeleteMapFileSubMenuClick(Sender: TObject);
var
  AProject: IOTAProject;
  Ident: string;
begin
  try
    Ident := '';
    AProject := FOTAProjectManager.GetCurrentSelection(Ident);
    if AProject <> nil then
      FDebugExtension.ProjectStates[deDeleteMapFile, AProject] := IntToDebugExpertState((Sender as TMenuItem).Tag)
    else
      raise EJclExpertException.CreateRes(@RsENoActiveProject);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TProjectManagerNotifier.GenerateJdbgSubMenuClick(Sender: TObject);
var
  AProject: IOTAProject;
  Ident: string;
begin
  try
    Ident := '';
    AProject := FOTAProjectManager.GetCurrentSelection(Ident);
    if AProject <> nil then
      FDebugExtension.ProjectStates[deGenerateJdbg, AProject] := IntToDebugExpertState((Sender as TMenuItem).Tag)
    else
      raise EJclExpertException.CreateRes(@RsENoActiveProject);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TProjectManagerNotifier.InsertJdbgSubMenuClick(Sender: TObject);
var
  AProject: IOTAProject;
  Ident: string;
begin
  try
    Ident := '';
    AProject := FOTAProjectManager.GetCurrentSelection(Ident);
    if AProject <> nil then
      FDebugExtension.ProjectStates[deInsertJdbg, AProject] := IntToDebugExpertState((Sender as TMenuItem).Tag)
    else
      raise EJclExpertException.CreateRes(@RsENoActiveProject);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

{$ENDIF BDS4_UP}

end.
