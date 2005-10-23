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
{ Unit owner: Petr Vones                                                                           }
{ Last modified: August 28, 2002                                                                   }
{                                                                                                  }
{**************************************************************************************************}

unit JclDebugIdeImpl;

{$I jcl.inc}
                              
{$UNDEF OldStyleExpert}

// Delphi 5 can use both kind of the expert
// Delphi 6 and + can use New expert style only

// BCB 5 can use both kind of the expert
// BCB 6 and + can use New expert style only

interface

uses
  Classes, Menus, ActnList, SysUtils, Graphics, Dialogs, Controls, Forms, ToolsAPI,
  JclOtaUtils;

type
  TJclDebugDataInfo = record
    ProjectName: string;
    ExecutableFileName: TFileName;
    MapFileSize, JclDebugDataSize: Integer;
    LinkerBugUnit: string;
    LineNumberErrors: Integer;
    Success: Boolean;
  end;

  TJclDebugExtension = class(TJclOTAExpert)
  private
    FResultInfo: array of TJclDebugDataInfo;
    FStoreResults: Boolean;
    FImageIndex: Integer;
    FBuildError: Boolean;
    {$IFNDEF OldStyleExpert}
    FInsertDataItem: TMenuItem;
    FInsertDataAction: TAction;
    FDisabledImageIndex: Integer;
    FCurrentProject: IOTAProject;
    FSaveBuildProject: TAction;
    FSaveBuildProjectExecute: TNotifyEvent;
    FSaveBuildAllProjects: TAction;
    FSaveBuildAllProjectsExecute: TNotifyEvent;
    FNotifierIndex: Integer;
    FOptionsModifiedState: Boolean;
    FSaveMapFile: Integer;
    procedure ExpertActive(Active: Boolean);
    procedure HookBuildActions(Enable: Boolean);
    procedure InsertDataExecute(Sender: TObject);
    procedure LoadExpertValues;
    procedure SaveExpertValues;
    procedure BuildAllProjects(Sender: TObject);       // (New) Build All Projects command hook
    procedure BuildProject(Sender: TObject);           // (New) Build Project command hook
    {$ELSE OldStyleExpert}
    FBuildAllMenuItem: TMenuItem;
    FBuildAllAction: TAction;
    FBuildMenuItem: TMenuItem;
    FBuildAction: TAction;
    procedure BuildActionExecute(Sender: TObject);     // (Old) Build JCL Debug command
    procedure BuildActionUpdate(Sender: TObject);
    procedure BuildAllActionExecute(Sender: TObject);  // (Old) Build JCL Debug All Projects command
    procedure BuildAllActionUpdate(Sender: TObject);
    function InsertDataToProject(const ActiveProject: IOTAProject): Boolean;
    {$ENDIF OldStyleExpert}
    procedure BeginStoreResults;
    procedure DisplayResults;
    procedure EndStoreResults;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    {$IFNDEF OldStyleExpert}
    procedure AfterCompile(Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
    {$ENDIF OldStyleExpert}
    procedure RegisterCommands; override;
    procedure UnregisterCommands; override;
  end;

  {$IFNDEF OldStyleExpert}
  TIdeNotifier = class(TNotifierObject, IOTANotifier, IOTAIDENotifier, IOTAIDENotifier50)
  private
    FDebugExtension: TJclDebugExtension;
  protected
    procedure AfterCompile(Succeeded: Boolean); overload;
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean); overload;
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
  public
    constructor Create(ADebugExtension: TJclDebugExtension);
  end;
  {$ENDIF OldStyleExpert}

procedure Register;

implementation

{$R JclDebugIdeIcon.res}

uses
  IniFiles, 
  JclDebug, JclDebugIdeResult, JclOtaConsts, JclRegistry;

procedure Register;
begin
  RegisterPackageWizard(TJclDebugExtension.Create);
end;

resourcestring
  {$IFDEF OldStyleExpert}
  RsActionCaption = 'Build JCL Debug %s';
  RsBuildAllCaption = 'Build JCL Debug All Projects';
  RsProjectNone = '[none]';
  RsBuildActionName = 'ProjectJCLBuildCommand';
  RsBuildAllActionName = 'ProjectJCLBuildAllCommand';
  {$ELSE OldStyleExpert}
  RsCantInsertToInstalledPackage = 'JCL Debug IDE Expert: Can not insert debug information to installed package' +
    #13#10'%s'#13#10#10'Would you like to disable inserting JCL Debug data ?';
  RsInsertDataCaption = 'Insert JCL Debug data';
  RsInsertDataActionName = 'ProjectJCLInsertDataCommand';
  {$ENDIF OldStyleExpert}
  RsExecutableNotFound = 'Executable file (*.exe or *.dll) not found.' +
    'JCL debug data can''t be added to the project.';

//=== { TJclDebugExtension } =================================================

constructor TJclDebugExtension.Create;
begin
  inherited Create(JclDebugExpertRegKey);
  {$IFNDEF OldStyleExpert}
  FNotifierIndex := Services.AddNotifier(TIdeNotifier.Create(Self));
  LoadExpertValues;
  {$ENDIF OldStyleExpert}
end;

destructor TJclDebugExtension.Destroy;
begin
  {$IFNDEF OldStyleExpert}
  if FNotifierIndex <> -1 then
    Services.RemoveNotifier(FNotifierIndex);
  SaveExpertValues;
  {$ENDIF OldStyleExpert}
  inherited Destroy;
end;

{$IFNDEF OldStyleExpert}
procedure TJclDebugExtension.AfterCompile(Succeeded: Boolean);
var
  ProjectFileName, MapFileName, ExecutableFileName: string;
  OutputDirectory, LinkerBugUnit: string;
  ProjOptions: IOTAProjectOptions;
  ExecutableNotFound, Succ: Boolean;
  MapFileSize, JclDebugDataSize, LineNumberErrors, C: Integer;

  procedure DeleteMapAndDrcFile;
  begin
    if FSaveMapFile <> MapFileOptionDetailed then
    begin // delete MAP and DRC file
      DeleteFile(MapFileName);
      DeleteFile(ChangeFileExt(ProjectFileName, DrcFileExtension));
    end;
  end;

begin
  if FInsertDataAction.Checked and Assigned(FCurrentProject) then
  begin
    ProjOptions := FCurrentProject.ProjectOptions;
{    if FSaveMapFile <> MapFileOptionDetailed then
    begin
      ProjOptions.Values[MapFileOptionName] := FSaveMapFile;
      ProjOptions.ModifiedState := FOptionsModifiedState;
    end;}
{ TODO -oPV : Temporarily removed due Delphi 6 IDE problems }      
    ProjectFileName := FCurrentProject.FileName;
    OutputDirectory := GetOutputDirectory(FCurrentProject);
    MapFileName := GetMapFileName(FCurrentProject);
    if Succeeded then
    begin
      ExecutableNotFound := False;
      LinkerBugUnit := '';
      LineNumberErrors := 0;
      Succ := FileExists(MapFileName);
      if Succ then
      begin
        Screen.Cursor := crHourGlass;
        try
          if FindExecutableName(MapFileName, OutputDirectory, ExecutableFileName) then
          begin
            Succ := InsertDebugDataIntoExecutableFile(ExecutableFileName, MapFileName,
              LinkerBugUnit, MapFileSize, JclDebugDataSize, LineNumberErrors);
          end
          else
            ExecutableNotFound := True;
        finally
          Screen.Cursor := crDefault;
        end;
      end;

      DeleteMapAndDrcFile;

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

      if ExecutableNotFound then
        MessageDlg(Format(RsExecutableNotFound, [ProjectFileName]), mtError, [mbOk], 0);
    end
    else
    begin
      FBuildError := True;
      DeleteMapAndDrcFile;
    end;
    Pointer(FCurrentProject) := nil;
  end;
end;

procedure TJclDebugExtension.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
var
  ProjOptions: IOTAProjectOptions;
begin
  if FInsertDataAction.Checked then
  begin
    if IsInstalledPackage(Project) then
    begin
      if MessageDlg(Format(RsCantInsertToInstalledPackage, [Project.FileName]), mtError, [mbYes, mbNo], 0) = mrYes then
        ExpertActive(False);
      Cancel := True;
    end
    else
    begin
      FCurrentProject := Project;
      ProjOptions := Project.ProjectOptions;
      Assert(Assigned(ProjOptions), 'IOTAProjectOptions not available');
      FOptionsModifiedState := ProjOptions.ModifiedState;
      FSaveMapFile := ProjOptions.Values[MapFileOptionName];
      if FSaveMapFile <> MapFileOptionDetailed then
        ProjOptions.Values[MapFileOptionName] := MapFileOptionDetailed;
    end;
  end;
end;

{$ENDIF OldStyleExpert}

procedure TJclDebugExtension.BeginStoreResults;
begin
  FBuildError := False;
  FStoreResults := True;
  FResultInfo := nil;
end;

{$IFDEF OldStyleExpert}

procedure TJclDebugExtension.BuildActionExecute(Sender: TObject);
begin
  BeginStoreResults;
  try
    if InsertDataToProject(ActiveProject) then
      DisplayResults;
  finally
    EndStoreResults;
  end;
end;

procedure TJclDebugExtension.BuildActionUpdate(Sender: TObject);
var
  TempActiveProject: IOTAProject;
  ProjectName: string;
begin
  TempActiveProject := ActiveProject;
  FBuildAction.Enabled := Assigned(TempActiveProject);
  if Assigned(ActiveProject) then
    ProjectName := ExtractFileName(TempActiveProject.FileName)
  else
    ProjectName := RsProjectNone;
  FBuildAction.Caption := Format(RsActionCaption, [ProjectName]);
end;

procedure TJclDebugExtension.BuildAllActionExecute(Sender: TObject);
var
  I: Integer;
  TempActiveProject: IOTAProject;
  TempProjectGroup: IOTAProjectGroup;
  Error: Boolean;
begin
  TempProjectGroup := ProjectGroup;
  if not Assigned(TempProjectGroup) then
    Exit;
  Error := False;
  BeginStoreResults;
  try
    for I := 0 to TempProjectGroup.ProjectCount - 1 do
    begin
      TempActiveProject := TempProjectGroup.Projects[I];
      TempProjectGroup.ActiveProject := TempActiveProject;
      Error := not InsertDataToProject(TempActiveProject);
      if Error then
        Break;
    end;
    if not Error then
      DisplayResults;
  finally
    EndStoreResults;
  end;
end;

procedure TJclDebugExtension.BuildAllActionUpdate(Sender: TObject);
begin
  FBuildAllAction.Enabled := ProjectGroup <> nil;
end;

{$ENDIF OldStyleExpert}

{$IFNDEF OldStyleExpert}

procedure TJclDebugExtension.BuildAllProjects(Sender: TObject);
begin
  BeginStoreResults;
  try
    FSaveBuildAllProjectsExecute(Sender);
    DisplayResults;
  finally
    EndStoreResults;
  end;
end;

procedure TJclDebugExtension.BuildProject(Sender: TObject);
begin
  BeginStoreResults;
  try
    FSaveBuildProjectExecute(Sender);
    DisplayResults;
  finally
    EndStoreResults;
  end;
end;

{$ENDIF OldStyleExpert}

procedure TJclDebugExtension.DisplayResults;
var
  I: Integer;
begin
  if FBuildError or (Length(FResultInfo) = 0) then
    Exit;
  with TJclDebugResultForm.Create(Application) do
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

{$IFNDEF OldStyleExpert}

procedure TJclDebugExtension.ExpertActive(Active: Boolean);
begin
  if (Active) then
    FInsertDataAction.ImageIndex := FImageIndex
  else
    FInsertDataAction.ImageIndex := FDisabledImageIndex;
  FInsertDataAction.Checked := Active;
  HookBuildActions(Active);
end;

procedure TJclDebugExtension.HookBuildActions(Enable: Boolean);
begin
  if Enable then
  begin
    if Assigned(FSaveBuildProject) then
      FSaveBuildProject.OnExecute := BuildProject;
    if Assigned(FSaveBuildAllProjects) then
      FSaveBuildAllProjects.OnExecute := BuildAllProjects;
  end
  else
  begin
    if Assigned(FSaveBuildProject) then
      FSaveBuildProject.OnExecute := FSaveBuildProjectExecute;
    if Assigned(FSaveBuildAllProjects) then
      FSaveBuildAllProjects.OnExecute := FSaveBuildAllProjectsExecute;
  end;
end;

procedure TJclDebugExtension.InsertDataExecute(Sender: TObject);
begin
  ExpertActive(not FInsertDataAction.Checked);
  
  SaveExpertValues;
end;

procedure TJclDebugExtension.LoadExpertValues;
begin
  ExpertActive(RegReadBoolDef(HKCU, ExpertRegistryKey, JclDebugEnabledRegValue, False));
end;

procedure TJclDebugExtension.SaveExpertValues;
begin
  RegWriteBool(HKCU, ExpertRegistryKey, JclDebugEnabledRegValue, FInsertDataAction.Checked);
end;

{$ENDIF OldStyleExpert}

{$IFDEF OldStyleExpert}

function TJclDebugExtension.InsertDataToProject(const ActiveProject: IOTAProject): Boolean;
var
  BuildOk, Succ: Boolean;
  ProjOptions: IOTAProjectOptions;
  SaveMapFile: Variant;
  ProjectFileName, MapFileName, ExecutableFileName: string;
  OutputDirectory, LinkerBugUnit: string;
  MapFileSize, JclDebugDataSize, LineNumberErrors, C: Integer;
  ExecutableNotFound: Boolean;
  OptionsModifiedState: Boolean;
begin
  Assert(Assigned(ActiveProject));
  ProjectFileName := ActiveProject.FileName;
  ProjOptions := ActiveProject.ProjectOptions;
  // read output directory
  OutputDirectory := GetOutputDirectory(ActiveProject);
  MapFileName := GetMapFileName(ActiveProject);

  OptionsModifiedState := ProjOptions.ModifiedState;
  SaveMapFile := ProjOptions.Values[MapFileOptionName];
  ProjOptions.Values[MapFileOptionName] := MapFileOptionDetailed;
  BuildOk := ActiveProject.ProjectBuilder.BuildProject(cmOTABuild, False);
  ProjOptions.Values[MapFileOptionName] := SaveMapFile;
  ProjOptions.ModifiedState := OptionsModifiedState;

  ExecutableNotFound := False;
  LinkerBugUnit := '';
  LineNumberErrors := 0;
  if BuildOk then
  begin
    Succ := FileExists(MapFileName);
    if Succ then
    begin
      if FindExecutableName(MapFileName, OutputDirectory, ExecutableFileName) then
      begin
        Succ := InsertDebugDataIntoExecutableFile(ExecutableFileName, MapFileName,
          LinkerBugUnit, MapFileSize, JclDebugDataSize, LineNumberErrors);
      end
      else
      begin
        ExecutableNotFound := True;
        Succ := False;
      end;
    end;
  end
  else
    Succ := False;
  if SaveMapFile <> MapFileOptionDetailed then
  begin
    DeleteFile(MapFileName);
    DeleteFile(ChangeFileExt(ProjectFileName, DrcFileExtension));
  end;
  Result := BuildOk and not ExecutableNotFound;
  C := Length(FResultInfo);
  SetLength(FResultInfo, C + 1);
  FResultInfo[C].ProjectName := ExtractFileName(ProjectFileName);
  FResultInfo[C].ExecutableFileName := ExecutableFileName;
  FResultInfo[C].MapFileSize := MapFileSize;
  FResultInfo[C].JclDebugDataSize := JclDebugDataSize;
  FResultInfo[C].LinkerBugUnit := LinkerBugUnit;
  FResultInfo[C].LineNumberErrors := LineNumberErrors;
  FResultInfo[C].Success := Succ;
  if ExecutableNotFound then
    MessageDlg(Format(RsExecutableNotFound, [ProjectFileName]), mtError, [mbOk], 0);
end;

{$ENDIF OldStyleExpert}

procedure TJclDebugExtension.RegisterCommands;
var
  IDEMainMenu: TMainMenu;
  IDEProjectItem: TMenuItem;
  IDEActionList: TActionList;
  I: Integer;
  ImageBmp: TBitmap;
begin
  IDEActionList := TActionList(NTAServices.ActionList);
  IDEMainMenu := NTAServices.MainMenu;
  ImageBmp := TBitmap.Create;
  try
    ImageBmp.LoadFromResourceName(FindResourceHInstance(HInstance), 'JCLDEBUG');
    FImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);
    {$IFDEF OldStyleExpert}
    FBuildAction := TAction.Create(nil);
    FBuildAction.Caption := Format(RsActionCaption, [RsProjectNone]);
    FBuildAction.ImageIndex := FImageIndex;
    FBuildAction.Visible := True;
    FBuildAction.OnExecute := BuildActionExecute;
    FBuildAction.OnUpdate := BuildActionUpdate;
    FBuildAction.Name := RsBuildActionName;
    FBuildAction.ActionList := IDEActionList;
    RegisterAction(FBuildAction);
    FBuildMenuItem := TMenuItem.Create(nil);
    FBuildMenuItem.Action := FBuildAction;
    FBuildAllAction := TAction.Create(nil);
    FBuildAllAction.Caption := RsBuildAllCaption;
    FBuildAllAction.ImageIndex := FImageIndex;
    FBuildAllAction.Visible := True;
    FBuildAllAction.OnExecute := BuildAllActionExecute;     
    FBuildAllAction.OnUpdate := BuildAllActionUpdate;
    FBuildAllAction.Name := RsBuildAllActionName;
    FBuildAllAction.ActionList := IDEActionList;
    RegisterAction(FBuildAllAction);
    FBuildAllMenuItem := TMenuItem.Create(nil);
    FBuildAllMenuItem.Action := FBuildAllAction;
    {$ELSE OldStyleExpert}
    ImageBmp.LoadFromResourceName(FindResourceHInstance(HInstance), 'JCLNODEBUG');
    FDisabledImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);
    FInsertDataAction := TAction.Create(nil);
    FInsertDataAction.Caption := RsInsertDataCaption;
    FInsertDataAction.Visible := True;
    FInsertDataAction.OnExecute := InsertDataExecute;
    FInsertDataAction.ActionList := IDEActionList;
    FInsertDataAction.Name := RsInsertDataActionName;
    RegisterAction(FInsertDataAction);
    FInsertDataItem := TMenuItem.Create(nil);
    FInsertDataItem.Action := FInsertDataAction;
    {$ENDIF OldStyleExpert}
  finally
    ImageBmp.Free;
  end;

  IDEProjectItem := nil;
  with IDEMainMenu do
    for I := 0 to Items.Count - 1 do
      if Items[I].Name = 'ProjectMenu' then
      begin
        IDEProjectItem := Items[I];
        Break;
      end;
  Assert(IDEProjectItem <> nil);

  {$IFDEF OldStyleExpert}
  with IDEProjectItem do
    for I := 0 to Count - 1 do
      if Items[I].Name = 'ProjectBuildItem' then
      begin
        if Assigned(Items[I].Action) then
          FBuildAction.Category := TContainedAction(Items[I].Action).Category;
        IDEProjectItem.Insert(I + 1, FBuildMenuItem);
        System.Break;
      end;
  Assert(FBuildMenuItem.Parent <> nil);
  with IDEProjectItem do
    for I := 0 to Count - 1 do
      if Items[I].Name = 'ProjectBuildAllItem' then
      begin
        if Assigned(Items[I].Action) then
          FBuildAllAction.Category := TContainedAction(Items[I].Action).Category;
        IDEProjectItem.Insert(I + 1, FBuildAllMenuItem);
        System.Break;
      end;
  Assert(FBuildMenuItem.Parent <> nil);
  {$ELSE OldStyleExpert}
  with IDEProjectItem do
    for I := 0 to Count - 1 do
      if Items[I].Name = 'ProjectOptionsItem' then
      begin
        if Assigned(Items[I].Action) then
          FInsertDataAction.Category := TContainedAction(Items[I].Action).Category;
        IDEProjectItem.Insert(I + 1, FInsertDataItem);
        System.Break;
      end;
  Assert(FInsertDataItem.Parent <> nil);
  FSaveBuildProject := nil;
  with IDEActionList do
    for I := 0 to ActionCount - 1 do
      if Actions[I].Name = 'ProjectBuildCommand' then
      begin
        FSaveBuildProject := TAction(Actions[I]);
        FSaveBuildProjectExecute := Actions[I].OnExecute;
        Break;
      end;
  Assert(Assigned(FSaveBuildProject), 'Build action not found');
  FSaveBuildAllProjects := nil;
  with IDEActionList do
    for I := 0 to ActionCount - 1 do
      if Actions[I].Name = 'ProjectBuildAllCommand' then
      begin
        FSaveBuildAllProjects := TAction(Actions[I]);
        FSaveBuildAllProjectsExecute := Actions[I].OnExecute;
        Break;
      end;
  Assert(Assigned(FSaveBuildAllProjects), 'Build All action not found');
  {$ENDIF OldStyleExpert}
end;

procedure TJclDebugExtension.UnregisterCommands;
begin
  {$IFNDEF OldStyleExpert}
  HookBuildActions(False);
  UnregisterAction(FInsertDataAction);
  FreeAndNil(FInsertDataItem);
  FreeAndNil(FInsertDataAction);
  {$ELSE OldStyleExpert}
  UnregisterAction(FBuildAction);
  UnregisterAction(FBuildAllAction);
  FreeAndNil(FBuildMenuItem);
  FreeAndNil(FBuildAction);
  FreeAndNil(FBuildAllMenuItem);
  FreeAndNil(FBuildAllAction);
  {$ENDIF OldStyleExpert}
end;

//=== { TIdeNotifier } =======================================================

{$IFNDEF OldStyleExpert}

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
  if not IsCodeInsight then
    FDebugExtension.AfterCompile(Succeeded);
end;

procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean);
begin
  if not IsCodeInsight then
    FDebugExtension.BeforeCompile(Project, Cancel);
end;

procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
end;

procedure TIdeNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
end;

{$ENDIF OldStyleExpert}

end.
