{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL) extension                                    }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JclDebugIdeImpl.pas.                                    }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Last modified: July 2, 2001                                                  }
{                                                                              }
{******************************************************************************}

unit JclDebugIdeImpl;

{$I JCL.INC}

{$DEFINE OldStyleExpert}

interface

uses
  Windows, Classes, Menus, ActnList, SysUtils, Graphics, Dialogs, Controls,
  Forms, ToolsAPI;

type
  {$IFDEF DELPHI4}
  {$DEFINE OldStyleExpert}
  TNotifierObject = class(TInterfacedObject)
  public
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  end;
  {$ENDIF DELPHI4}

  TJclDebugDataInfo = record
    ProjectName: string;
    ExecutableFileName: TFileName;
    MapFileSize, JclDebugDataSize: Integer;
    LinkerBugUnit: string;
  end;

  TJclDebugExtension = class(TNotifierObject, IOTAWizard)
  private
    FBuildAllMenuItem: TMenuItem;
    FBuildAllAction: TAction;
    FBuildMenuItem: TMenuItem;
    FBuildAction: TAction;
    FCurrentProject: IOTAProject;
    FInsertDataItem: TMenuItem;
    FInsertDataAction: TAction;
    FResultInfo: array of TJclDebugDataInfo;
    {$IFNDEF DELPHI5_UP}
    FSaveAllAction: TAction;
    {$ENDIF DELPHI5_UP}
    FSaveBuildProject: TAction;
    FSaveBuildProjectExecute: TNotifyEvent;
    FSaveBuildAllProjects: TAction;
    FSaveBuildAllProjectsExecute: TNotifyEvent;
    FStoreResults: Boolean;
    FNotifierIndex: Integer;
    FOptionsModifiedState: Boolean;
    FSaveMapFile: Integer;
    Services: IOTAServices;
    procedure ActionExecute(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
    procedure BeginStoreResults;
    procedure BuildAllProjects(Sender: TObject);
    procedure BuildProject(Sender: TObject);
    procedure BuildAllActionExecute(Sender: TObject);
    procedure BuildAllActionUpdate(Sender: TObject);
    procedure DisplayResults;
    procedure EndStoreResults;
    procedure InsertDataExecute(Sender: TObject);
    function InsertDataToProject(const ActiveProject: IOTAProject): Boolean;
    function GetActiveProject: IOTAProject;
    function GetProjectGroup: IOTAProjectGroup;
  protected
    procedure Execute;
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AfterCompile(Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject);
    procedure RegisterCommand;
    procedure UnregisterCommand;
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

implementation

{$R JclDebugIdeIcon.res}

uses
  ImageHlp,
  JclDebug, JclFileUtils, JclPeImage, JclSysUtils, JclWin32, JclDebugIdeResult,
  ComCtrls;

resourcestring
  RsActionCaption = 'Build JCL Debug %s';
  RsBuildAllCaption = 'Build JCL Debug All Projects';
  RsProjectNone = '[none]';
  RsExecutableNotFound = 'Executable file (*.exe or *.dll) not found.' +
    'JCL debug data can''t be added to the project.';
  RsInsertDataCaption = 'Insert JCL Debug data';

//------------------------------------------------------------------------------

const
  MapFileOptionDetailed = 3;

//------------------------------------------------------------------------------

function FindExecutableName(const MapFileName, OutputDirectory: string; var ExecutableFileName: TFileName): Boolean;
var
  Se: TSearchRec;
  Res: Integer;
  LatestTime: Integer;
  FileName: TFileName;
  LI: TLoadedImage;
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

//==============================================================================
// TNotifierObject for Delphi 4
//==============================================================================

{$IFDEF DELPHI4}
procedure TNotifierObject.AfterSave;
begin
end;

procedure TNotifierObject.BeforeSave;
begin
end;

procedure TNotifierObject.Destroyed;
begin
end;

procedure TNotifierObject.Modified;
begin
end;
{$ENDIF DELPHI4}

//==============================================================================
// TJclDebugExtension
//==============================================================================

procedure TJclDebugExtension.ActionExecute(Sender: TObject);
begin
  BeginStoreResults;
  try
    InsertDataToProject(GetActiveProject);
    DisplayResults;
  finally
    EndStoreResults;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclDebugExtension.ActionUpdate(Sender: TObject);
var
  ActiveProject: IOTAProject;
  ProjectName: string;
begin
  ActiveProject := GetActiveProject;
  {$IFDEF DELPHI5_UP}
  FBuildAction.Enabled := Assigned(ActiveProject);
  {$ELSE}
  FBuildAction.Enabled := Assigned(ActiveProject) and not FSaveAllAction.Enabled;
  {$ENDIF DELPHI5_UP}
  if FBuildAction.Enabled then
    ProjectName := ExtractFileName(ActiveProject.FileName)
  else
    ProjectName := RsProjectNone;
  FBuildAction.Caption := Format(RsActionCaption, [ProjectName]);
end;

//------------------------------------------------------------------------------

procedure TJclDebugExtension.AfterCompile(Succeeded: Boolean);
{$IFDEF OldStyleExpert}
begin
end;
{$ELSE OldStyleExpert}
var
  ProjectFileName, MapFileName, ExecutableFileName: TFileName;
  ProjectName, OutputDirectory, LinkerBugUnit: string;
  ProjOptions: IOTAProjectOptions;
  ExecutableNotFound, Succ: Boolean;
  MapFileSize, JclDebugDataSize, C: Integer;
begin
  if FInsertDataAction.Checked and Assigned(FCurrentProject) then
  begin
    ProjOptions := FCurrentProject.ProjectOptions;
    ProjOptions.Values['MapFile'] := FSaveMapFile;
    ProjOptions.ModifiedState := FOptionsModifiedState;
    if Succeeded then
    begin
      ProjectFileName := FCurrentProject.FileName;
      ProjectName := ExtractFileName(ProjectFileName);
      OutputDirectory := ProjOptions.Values['OutputDir'];
      if OutputDirectory = '' then
        OutputDirectory := ExtractFilePath(ProjectFileName);
      MapFileName := PathAddSeparator(OutputDirectory) + ChangeFileExt(ProjectName, '.map');

      ExecutableNotFound := False;
      LinkerBugUnit := '';
      Succ := FileExists(MapFileName);
      if Succ then
      begin
        if FindExecutableName(MapFileName, OutputDirectory, ExecutableFileName) then
        begin
          Succ := InsertDebugDataIntoExecutableFile(ExecutableFileName, MapFileName,
            LinkerBugUnit, MapFileSize, JclDebugDataSize);
        end
        else
          ExecutableNotFound := True;
      end;

      if FSaveMapFile <> MapFileOptionDetailed then
      begin // delete MAP and DRC file
        DeleteFile(MapFileName);
        DeleteFile(ChangeFileExt(MapFileName, '.drc'));
      end;

      if FStoreResults then
      begin
        C := Length(FResultInfo);
        SetLength(FResultInfo, C + 1);
        FResultInfo[C].ProjectName := ProjectName;
        FResultInfo[C].ExecutableFileName := ExecutableFileName;
        FResultInfo[C].MapFileSize := MapFileSize;
        FResultInfo[C].JclDebugDataSize := JclDebugDataSize;
        FResultInfo[C].LinkerBugUnit := LinkerBugUnit;
      end;

      if ExecutableNotFound then
        MessageDlg(Format(RsExecutableNotFound, [ProjectName]), mtError, [mbOk], 0);
    end;
    Pointer(FCurrentProject) := nil;
  end;
end;
{$ENDIF OldStyleExpert}

//------------------------------------------------------------------------------

procedure TJclDebugExtension.BeforeCompile(const Project: IOTAProject);
{$IFDEF OldStyleExpert}
begin
end;
{$ELSE OldStyleExpert}
var
  ProjOptions: IOTAProjectOptions;
begin
  if FInsertDataAction.Checked then
  begin
    FCurrentProject := Project;
    ProjOptions := Project.ProjectOptions;
    Assert(Assigned(ProjOptions), 'IOTAProjectOptions not available');
    FOptionsModifiedState := ProjOptions.ModifiedState;
    FSaveMapFile := ProjOptions.Values['MapFile'];
    ProjOptions.Values['MapFile'] := MapFileOptionDetailed;
  end;
end;
{$ENDIF OldStyleExpert}

//------------------------------------------------------------------------------

procedure TJclDebugExtension.BeginStoreResults;
begin
  FStoreResults := True;
  FResultInfo := nil;
end;

//------------------------------------------------------------------------------

procedure TJclDebugExtension.BuildAllActionExecute(Sender: TObject);
var
  I: Integer;
  ActiveProject: IOTAProject;
  ProjectGroup: IOTAProjectGroup;
  Error: Boolean;
begin
  ProjectGroup := GetProjectGroup;
  if not Assigned(ProjectGroup) then
    Exit;
  Error := False;
  BeginStoreResults;
  try
    for I := 0 to ProjectGroup.ProjectCount - 1 do
    begin
      ActiveProject := ProjectGroup.Projects[I];
      ProjectGroup.ActiveProject := ActiveProject;
      Error := not InsertDataToProject(ActiveProject);
      if Error then
        Break;
    end;
    if not Error then
      DisplayResults;
  finally
    EndStoreResults;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclDebugExtension.BuildAllActionUpdate(Sender: TObject);
begin
  FBuildAllAction.Enabled := GetProjectGroup <> nil;
end;

//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

constructor TJclDebugExtension.Create;
begin
  inherited Create;
  Services := BorlandIDEServices as IOTAServices;
  Assert(Assigned(Services), 'IOTAServices not available');
  {$IFNDEF OldStyleExpert}
  FNotifierIndex := Services.AddNotifier(TIdeNotifier.Create(Self));
  {$ENDIF OldStyleExpert}
  RegisterCommand;
end;

//------------------------------------------------------------------------------

destructor TJclDebugExtension.Destroy;
begin
  {$IFNDEF OldStyleExpert}
  if FNotifierIndex <> -1 then
    Services.RemoveNotifier(FNotifierIndex);
  {$ENDIF OldStyleExpert}
  UnregisterCommand;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TJclDebugExtension.DisplayResults;
var
  I: Integer;
begin
  if Length(FResultInfo) = 0 then
    Exit;
  with TJclDebugResultForm.Create(Application) do
  try
    for I := 0 to Length(FResultInfo) - 1 do
      with ResultListView.Items.Add, FResultInfo[I] do
      begin
        Caption := ProjectName;
        SubItems.Add(IntToStr(MapFileSize));
        SubItems.Add(IntToStr(JclDebugDataSize));
        SubItems.Add(Format('%3.1f', [JclDebugDataSize * 100 / MapFileSize]));
        SubItems.Add(ExecutableFileName);
        SubItems.Add(LinkerBugUnit);
        if LinkerBugUnit = '' then
          ImageIndex := 0
        else
          ImageIndex := 1;
      end;
    ShowModal;
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TJclDebugExtension.EndStoreResults;
begin
  FStoreResults := False;
  FResultInfo := nil;
end;

//------------------------------------------------------------------------------

procedure TJclDebugExtension.Execute;
begin
end;

//------------------------------------------------------------------------------

function TJclDebugExtension.GetActiveProject: IOTAProject;
var
  ProjectGroup: IOTAProjectGroup;
begin
  ProjectGroup := GetProjectGroup;
  if Assigned(ProjectGroup) then
    Result := ProjectGroup.ActiveProject
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

function TJclDebugExtension.GetIDString: string;
begin
  Result := 'JCL.DebugExtension';
end;

//------------------------------------------------------------------------------

function TJclDebugExtension.GetName: string;
begin
  Result := 'JEDI Code Library Debug IDE extension';
end;

//------------------------------------------------------------------------------

function TJclDebugExtension.GetProjectGroup: IOTAProjectGroup;
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

//------------------------------------------------------------------------------

function TJclDebugExtension.GetState: TWizardState;
begin
  Result := [];
end;

//------------------------------------------------------------------------------

procedure TJclDebugExtension.InsertDataExecute(Sender: TObject);
begin
  with FInsertDataAction do
    Checked := not Checked;
end;

//------------------------------------------------------------------------------

function TJclDebugExtension.InsertDataToProject(const ActiveProject: IOTAProject): Boolean;
var
  BuildOk, Succ: Boolean;
  ProjOptions: IOTAProjectOptions;
  SaveMapFile: Variant;
  ProjectFileName, MapFileName, ExecutableFileName: TFileName;
  ProjectName, OutputDirectory, LinkerBugUnit: string;
  MapFileSize, JclDebugDataSize, C: Integer;
  ExecutableNotFound: Boolean;
  {$IFDEF DELPHI5_UP}
  OptionsModifiedState: Boolean;
  {$ENDIF DELPHI5_UP}
begin
  Assert(Assigned(ActiveProject));
  ProjectFileName := ActiveProject.FileName;
  ProjectName := ExtractFileName(ProjectFileName);
  ProjOptions := ActiveProject.ProjectOptions;
  // read output directory
  OutputDirectory := ProjOptions.Values['OutputDir'];
  if OutputDirectory = '' then
    OutputDirectory := ExtractFilePath(ProjectFileName);
  MapFileName := PathAddSeparator(OutputDirectory) + ChangeFileExt(ProjectName, '.map');
  {$IFDEF DELPHI5_UP}
  OptionsModifiedState := ProjOptions.ModifiedState;
  {$ENDIF DELPHI5_UP}
  SaveMapFile := ProjOptions.Values['MapFile'];
  ProjOptions.Values['MapFile'] := MapFileOptionDetailed;
  BuildOk := ActiveProject.ProjectBuilder.BuildProject(cmOTABuild, False);
  ProjOptions.Values['MapFile'] := SaveMapFile;
  {$IFDEF DELPHI5_UP}
  ProjOptions.ModifiedState := OptionsModifiedState;
  {$ENDIF DELPHI5_UP}
  ExecutableNotFound := False;
  if BuildOk then
  begin
    Succ := FileExists(MapFileName);
    if Succ then
    begin
      if FindExecutableName(MapFileName, OutputDirectory, ExecutableFileName) then
      begin
        Succ := InsertDebugDataIntoExecutableFile(ExecutableFileName, MapFileName,
          LinkerBugUnit, MapFileSize, JclDebugDataSize);
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
    DeleteFile(ChangeFileExt(MapFileName, '.drc'));
  end;
  Result := Succ;
  if Succ then
  begin
    C := Length(FResultInfo);
    SetLength(FResultInfo, C + 1);
    FResultInfo[C].ProjectName := ProjectName;
    FResultInfo[C].ExecutableFileName := ExecutableFileName;
    FResultInfo[C].MapFileSize := MapFileSize;
    FResultInfo[C].JclDebugDataSize := JclDebugDataSize;
    FResultInfo[C].LinkerBugUnit := LinkerBugUnit;
  end;
  if ExecutableNotFound then
    MessageDlg(Format(RsExecutableNotFound, [ProjectName]), mtError, [mbOk], 0);
end;

//------------------------------------------------------------------------------

procedure TJclDebugExtension.RegisterCommand;
var
  IDEMainMenu: TMainMenu;
  IDEProjectItem: TMenuItem;
  IDEActionList: TActionList;
  I: Integer;
  ImageBmp: TBitmap;
begin
  IDEActionList := TActionList((BorlandIDEServices as INTAServices).ActionList);
  IDEMainMenu := (BorlandIDEServices as INTAServices).MainMenu;
  ImageBmp := TBitmap.Create;
  try
    ImageBmp.LoadFromResourceName(FindResourceHInstance(HInstance), 'JCLDEBUG');
    {$IFDEF OldStyleExpert}
    FBuildAction := TAction.Create(nil);
    FBuildAction.Caption := Format(RsActionCaption, [RsProjectNone]);
    FBuildAction.ImageIndex := (BorlandIDEServices as INTAServices).AddMasked(ImageBmp, clPurple);
    FBuildAction.Visible := True;
    FBuildAction.OnExecute := ActionExecute;
    FBuildAction.OnUpdate := ActionUpdate;
    FBuildAction.ActionList := IDEActionList;
    FBuildMenuItem := TMenuItem.Create(nil);
    FBuildMenuItem.Action := FBuildAction;
    FBuildAllAction := TAction.Create(nil);
    FBuildAllAction.Caption := RsBuildAllCaption;
    FBuildAllAction.ImageIndex := (BorlandIDEServices as INTAServices).AddMasked(ImageBmp, clPurple);
    FBuildAllAction.Visible := True;
    FBuildAllAction.OnExecute := BuildAllActionExecute;
    FBuildAllAction.OnUpdate := BuildAllActionUpdate;
    FBuildAllAction.ActionList := IDEActionList;
    FBuildAllMenuItem := TMenuItem.Create(nil);
    FBuildAllMenuItem.Action := FBuildAllAction;
    {$ELSE OldStyleExpert}
    FInsertDataAction := TAction.Create(nil);
    FInsertDataAction.Caption := RsInsertDataCaption;
    FInsertDataAction.ImageIndex := (BorlandIDEServices as INTAServices).AddMasked(ImageBmp, clPurple);
    FInsertDataAction.Visible := True;
    FInsertDataAction.OnExecute := InsertDataExecute;
    FInsertDataAction.ActionList := IDEActionList;
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
        IDEProjectItem.Insert(I + 1, FBuildMenuItem);
        System.Break;
      end;
  Assert(FBuildMenuItem.Parent <> nil);
  with IDEProjectItem do
    for I := 0 to Count - 1 do
      if Items[I].Name = 'ProjectBuildAllItem' then
      begin
        IDEProjectItem.Insert(I + 1, FBuildAllMenuItem);
        System.Break;
      end;
  Assert(FBuildMenuItem.Parent <> nil);
  {$IFNDEF DELPHI5_UP}
  FSaveAllAction := nil;
  with IDEActionList do
    for I := 0 to ActionCount - 1 do
      if Actions[I].Name = 'FileSaveAllCommand' then
      begin
        FSaveAllAction := TAction(Actions[I]);
        Break;
      end;
  Assert(FSaveAllAction <> nil);
  {$ENDIF DELPHI5_UP}
  {$ELSE OldStyleExpert}
  with IDEProjectItem do
    for I := 0 to Count - 1 do
      if Items[I].Name = 'ProjectOptionsItem' then
      begin
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
        Actions[I].OnExecute := BuildProject;
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
        Actions[I].OnExecute := BuildAllProjects;
        Break;
      end;
  Assert(Assigned(FSaveBuildAllProjects), 'Build All action not found');
  {$ENDIF OldStyleExpert}
end;

//------------------------------------------------------------------------------

procedure TJclDebugExtension.UnregisterCommand;
begin
  if Assigned(FSaveBuildProject) then
    FSaveBuildProject.OnExecute := FSaveBuildProjectExecute;
  if Assigned(FSaveBuildAllProjects) then
    FSaveBuildAllProjects.OnExecute := FSaveBuildAllProjectsExecute;
  FreeAndNil(FBuildMenuItem);
  FreeAndNil(FBuildAction);
  FreeAndNil(FBuildAllMenuItem);
  FreeAndNil(FBuildAllAction);
  FreeAndNil(FInsertDataItem);
  FreeAndNil(FInsertDataAction);
end;

//==============================================================================
// TIdeNotifier
//==============================================================================

{$IFNDEF OldStyleExpert}

procedure TIdeNotifier.AfterCompile(Succeeded: Boolean);
begin
end;

//------------------------------------------------------------------------------

procedure TIdeNotifier.AfterCompile(Succeeded, IsCodeInsight: Boolean);
begin
  if not IsCodeInsight then
    FDebugExtension.AfterCompile(Succeeded);
end;

//------------------------------------------------------------------------------

procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean);
begin
  if not IsCodeInsight then
    FDebugExtension.BeforeCompile(Project);
end;

//------------------------------------------------------------------------------

procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
end;

//------------------------------------------------------------------------------

constructor TIdeNotifier.Create(ADebugExtension: TJclDebugExtension);
begin
  inherited Create;
  FDebugExtension := ADebugExtension;
end;

//------------------------------------------------------------------------------

procedure TIdeNotifier.FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
begin
end;

{$ENDIF OldStyleExpert}

//------------------------------------------------------------------------------

end.
