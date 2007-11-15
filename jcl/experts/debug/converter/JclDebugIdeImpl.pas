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
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclDebugIdeImpl;

{$I jcl.inc}

interface

uses
  Windows, Classes, Menus, ActnList, SysUtils, Graphics, Dialogs, Controls, Forms, ToolsAPI,
  JclOtaUtils, JclDebugIdeConfigFrame;

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
    FInsertDataItem: TMenuItem;
    FInsertDataAction: TAction;
    FDisabledImageIndex: Integer;
    FCurrentProject: IOTAProject;
    FSaveBuildProject: TAction;
    FSaveBuildProjectExecute: TNotifyEvent;
    FSaveBuildAllProjects: TAction;
    FSaveBuildAllProjectsExecute: TNotifyEvent;
    FNotifierIndex: Integer;
    FSaveMapFile: Integer;
    FConfigFrame: TJclDebugIdeConfigFrame;
    FGenerateJdbg: Boolean;
    FInsertJdbg: Boolean;
    FEnableExpert: Boolean;
    procedure InsertDataExecute(Sender: TObject);
    procedure LoadExpertValues;
    procedure SaveExpertValues;
    procedure BuildAllProjects(Sender: TObject);       // (New) Build All Projects command hook
    procedure BuildProject(Sender: TObject);           // (New) Build Project command hook
    procedure BeginStoreResults;
    procedure DisplayResults;
    procedure EndStoreResults;
    procedure SetEnableExpert(const Value: Boolean);
  public
    constructor Create; reintroduce;
    procedure AfterCompile(Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
    procedure RegisterCommands; override;
    procedure UnregisterCommands; override;
    procedure AddConfigurationPages(AddPageFunc: TJclOTAAddPageFunc); override;
    procedure ConfigurationClosed(AControl: TControl; SaveChanges: Boolean); override;
    property GenerateJdbg: Boolean read FGenerateJdbg write FGenerateJdbg;
    property InsertJdbg: Boolean read FInsertJdbg write FInsertJdbg;
    property EnableExpert: Boolean read FEnableExpert write SetEnableExpert;
  end;

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

// design package entry point
procedure Register;

// expert DLL entry point
function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var TerminateProc: TWizardTerminateProc): Boolean; stdcall;

implementation

{$R JclDebugIdeIcon.res}

uses
  JclBorlandTools, JclDebug, JclDebugIdeResult,
  JclOtaConsts, JclOtaResources;

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

procedure TJclDebugExtension.ConfigurationClosed(AControl: TControl;
  SaveChanges: Boolean);
begin
  if Assigned(AControl) and (AControl = FConfigFrame) then
  begin
    if SaveChanges then
    begin
      EnableExpert := FConfigFrame.EnableExpert;
      GenerateJdbg := FConfigFrame.GenerateJdbg;
      InsertJdbg := FConfigFrame.InsertJdbg;
    end;
    FreeAndNil(FConfigFrame);
  end
  else
    inherited ConfigurationClosed(AControl, SaveChanges);
end;

constructor TJclDebugExtension.Create;
begin
  inherited Create(JclDebugExpertRegKey);
end;

procedure TJclDebugExtension.AddConfigurationPages(
  AddPageFunc: TJclOTAAddPageFunc);
begin
  inherited AddConfigurationPages(AddPageFunc);
  FConfigFrame := TJclDebugIdeConfigFrame.Create(nil);
  FConfigFrame.EnableExpert := EnableExpert;
  FConfigFrame.GenerateJdbg := GenerateJdbg;
  FConfigFrame.InsertJdbg := InsertJdbg;
  AddPageFunc(FConfigFrame, RsDebugConfigPageCaption, Self);
end;

procedure TJclDebugExtension.AfterCompile(Succeeded: Boolean);
var
  ProjectFileName, MapFileName, DrcFileName, ExecutableFileName, JdbgFileName: string;
  OutputDirectory, LinkerBugUnit: string;
  ProjOptions: IOTAProjectOptions;
  Succ: Boolean;
  MapFileSize, JclDebugDataSize, LineNumberErrors, C: Integer;

  procedure DeleteMapAndDrcFile;
  begin
    if FSaveMapFile <> MapFileOptionDetailed then
    begin // delete MAP and DRC file
      DeleteFile(MapFileName);
      DeleteFile(DrcFileName);
    end;
  end;

  procedure OutputToolMessage(const Msg: string);
  begin
    if Assigned(FCurrentProject) then
      OTAMessageServices.AddToolMessage(FCurrentProject.FileName, Msg,
        JclDebugMessagePrefix, 1, 1)
    else
      OTAMessageServices.AddToolMessage('', Msg, JclDebugMessagePrefix, 1, 1);
  end;

begin
  if EnableExpert and Assigned(FCurrentProject) then
  begin
    ProjOptions := FCurrentProject.ProjectOptions;

    if FSaveMapFile <> MapFileOptionDetailed then
    begin
      ProjOptions.Values[MapFileOptionName] := FSaveMapFile;
      // workaround for MsBuild, the project has to be saved (seems useless with Delphi 2007 update 1)
      ProjOptions.ModifiedState := True;
      //FCurrentProject.Save(False, True);
    end;
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
        if Succ and GenerateJdbg then
        begin
          Succ := ConvertMapFileToJdbgFile(MapFileName, LinkerBugUnit, LineNumberErrors,
            MapFileSize, JclDebugDataSize);
          if Succ then
            OutputToolMessage(Format(RsConvertedMapToJdbg, [MapFileName, MapFileSize, JclDebugDataSize]))
          else
            OutputToolMessage(Format(RsEMapConversion, [MapFileName]));
        end;

        // insertion of JEDI Debug Information into the binary
        if Succ and InsertJdbg then
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
        Screen.Cursor := crDefault;
      except
        Screen.Cursor := crDefault;
        raise;
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
    end
    else
    begin
      FBuildError := True;
      DeleteMapAndDrcFile;
    end;
    FCurrentProject := nil;
  end;
end;

procedure TJclDebugExtension.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
var
  ProjOptions: IOTAProjectOptions;
begin
  if EnableExpert then
  begin
    if IsInstalledPackage(Project) then
    begin
      if MessageDlg(Format(RsCantInsertToInstalledPackage, [Project.FileName]), mtError, [mbYes, mbNo], 0) = mrYes then
        EnableExpert := False
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

      FSaveMapFile := ProjOptions.Values[MapFileOptionName];
      if FSaveMapFile <> MapFileOptionDetailed then
        ProjOptions.Values[MapFileOptionName] := MapFileOptionDetailed;
      // workaround for MsBuild, the project has to be saved (seems useless with Delphi 2007 update 1)
      ProjOptions.ModifiedState := True;
      //Project.Save(False, True);
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
      FSaveBuildAllProjectsExecute(Sender);
      if EnableExpert then
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
      FSaveBuildProjectExecute(Sender);
      if EnableExpert then
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

procedure TJclDebugExtension.InsertDataExecute(Sender: TObject);
begin
  try
    EnableExpert := not FInsertDataAction.Checked;
    SaveExpertValues;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TJclDebugExtension.LoadExpertValues;
begin
  EnableExpert := Settings.LoadBool(JclDebugEnabledRegValue, False);
  GenerateJdbg := Settings.LoadBool(JclDebugGenerateJdbgRegValue, False);
  InsertJdbg := Settings.LoadBool(JclDebugInsertJdbgRegValue, True);
end;

procedure TJclDebugExtension.SaveExpertValues;
begin
  Settings.SaveBool(JclDebugEnabledRegValue, EnableExpert);
  Settings.SaveBool(JclDebugGenerateJdbgRegValue, GenerateJdbg);
  Settings.SaveBool(JclDebugInsertJdbgRegValue, InsertJdbg);
end;

procedure TJclDebugExtension.SetEnableExpert(const Value: Boolean);
begin
  FEnableExpert := Value;
  FInsertDataAction.Checked := Value;
  if (Value) then
  begin
    FInsertDataAction.ImageIndex := FImageIndex;
    if Assigned(FSaveBuildProject) then
      FSaveBuildProject.OnExecute := BuildProject;
    if Assigned(FSaveBuildAllProjects) then
      FSaveBuildAllProjects.OnExecute := BuildAllProjects;
  end
  else
  begin
    FInsertDataAction.ImageIndex := FDisabledImageIndex;
    if Assigned(FSaveBuildProject) then
      FSaveBuildProject.OnExecute := FSaveBuildProjectExecute;
    if Assigned(FSaveBuildAllProjects) then
      FSaveBuildAllProjects.OnExecute := FSaveBuildAllProjectsExecute;
  end;
end;

procedure TJclDebugExtension.RegisterCommands;
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
    ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'JCLDEBUG');
    FImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);
    ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'JCLNODEBUG');
    FDisabledImageIndex := NTAServices.AddMasked(ImageBmp, clPurple);
    FInsertDataAction := TAction.Create(nil);
    FInsertDataAction.Caption := RsInsertDataCaption;
    FInsertDataAction.Visible := True;
    FInsertDataAction.OnExecute := InsertDataExecute;
    FInsertDataAction.ActionList := IDEActionList;
    FInsertDataAction.Name := JclInsertDataActionName;
    RegisterAction(FInsertDataAction);
    FInsertDataItem := TMenuItem.Create(nil);
    FInsertDataItem.Name := JclInsertDataMenuName;
    FInsertDataItem.Action := FInsertDataAction;
  finally
    ImageBmp.Free;
  end;

  FNotifierIndex := Services.AddNotifier(TIdeNotifier.Create(Self));
  LoadExpertValues;

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
          FInsertDataAction.Category := TContainedAction(Items[I].Action).Category;
        IDEProjectItem.Insert(I + 1, FInsertDataItem);
        System.Break;
      end;
  if not Assigned(FInsertDataItem.Parent) then
    raise EJclExpertException.CreateTrace(RsEInsertDataMenuItemNotInserted);

  FSaveBuildProject := nil;
  with IDEActionList do
    for I := 0 to ActionCount - 1 do
      if Actions[I].Name = 'ProjectBuildCommand' then
      begin
        FSaveBuildProject := TAction(Actions[I]);
        FSaveBuildProjectExecute := Actions[I].OnExecute;
        Break;
      end;
  if not Assigned(FSaveBuildProject) then
    raise EJclExpertException.CreateTrace(RsENoBuildAction);

  FSaveBuildAllProjects := nil;
  with IDEActionList do
    for I := 0 to ActionCount - 1 do
      if Actions[I].Name = 'ProjectBuildAllCommand' then
      begin
        FSaveBuildAllProjects := TAction(Actions[I]);
        FSaveBuildAllProjectsExecute := Actions[I].OnExecute;
        Break;
      end;
  if not Assigned(FSaveBuildProject) then
    raise EJclExpertException.CreateTrace(RsENoBuildAllAction);
end;

procedure TJclDebugExtension.UnregisterCommands;
begin
  inherited UnregisterCommands;
  if FNotifierIndex <> -1 then
    Services.RemoveNotifier(FNotifierIndex);
  SaveExpertValues;
  EnableExpert := False;
  UnregisterAction(FInsertDataAction);
  FreeAndNil(FInsertDataItem);
  FreeAndNil(FInsertDataAction);
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

end.
