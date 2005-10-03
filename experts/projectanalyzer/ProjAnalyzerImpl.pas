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
{ The Original Code is ProjAnalyzerImpl.pas.                                                       }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Unit owner: Petr Vones                                                                           }
{ Last modified: March 17, 2002                                                                    }
{                                                                                                  }
{**************************************************************************************************}

unit ProjAnalyzerImpl;

{$I JCL.INC}

interface

uses
  Classes, Menus, ActnList, ToolsAPI, SysUtils, Graphics, Dialogs,
  Forms, JclOtaUtils;

type
  TProjectAnalyzerExpert = class(TJclOTAExpert)
  private
    FBuildMenuItem: TMenuItem;
    FBuildAction: TAction;
    procedure ActionExecute(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
  public
    destructor Destroy; override;
    procedure RegisterCommands; override;
    procedure UnregisterCommands; override;
  end;

procedure Register;

implementation

{$R ProjAnalyzerIcon.res}

uses
  JclDebug, JclFileUtils,
  ProjAnalyzerFrm;

resourcestring
  RsActionCaption = 'Analyze project %s';
  RsActionName = 'ProjectAnalyseCommand';
  RsProjectNone = '[none]';
  RsCantFindFiles = 'Can''t find MAP or executable file';
  RsBuildingProject = 'Building project %s ...';

//--------------------------------------------------------------------------------------------------

procedure Register;
begin
  RegisterPackageWizard(TProjectAnalyzerExpert.Create);
end;

//==================================================================================================
// TProjectAnalyzerExpert
//==================================================================================================

procedure TProjectAnalyzerExpert.ActionExecute(Sender: TObject);
var
  TempActiveProject: IOTAProject;
  BuildOK, Succ: Boolean;
  ProjOptions: IOTAProjectOptions;
  SaveMapFile: Variant;
  OutputDirectory, ProjectFileName, MapFileName, ExecutableFileName: string;
  ProjectName: string;
  OptionsModifiedState: Boolean;
begin
  TempActiveProject := ActiveProject;
  Assert(Assigned(TempActiveProject));
  ProjectFileName := TempActiveProject.FileName;
  ProjectName := ExtractFileName(ProjectFileName);
  Succ := False;

  ProjOptions := TempActiveProject.ProjectOptions;
  OutputDirectory := GetOutputDirectory(TempActiveProject);
  MapFileName := GetMapFileName(TempActiveProject);

  if ProjectAnalyzerForm = nil then
  begin
    ProjectAnalyzerForm := TProjectAnalyzerForm.Create(Application);
    ProjectAnalyzerForm.Show;
  end;
  ProjectAnalyzerForm.ClearContent;
  ProjectAnalyzerForm.StatusBarText := Format(RsBuildingProject, [ProjectName]);

  OptionsModifiedState := ProjOptions.ModifiedState;
  SaveMapFile := ProjOptions.Values[MapFileOptionName];
  ProjOptions.Values[MapFileOptionName] := MapFileOptionDetailed;
  BuildOK := TempActiveProject.ProjectBuilder.BuildProject(cmOTABuild, False);
  ProjOptions.Values[MapFileOptionName] := SaveMapFile;
  ProjOptions.ModifiedState := OptionsModifiedState;

  if BuildOK then
  begin // Build was successful, continue ...
    Succ := FileExists(MapFileName) and FindExecutableName(MapFileName, OutputDirectory, ExecutableFileName);
    if Succ then
    begin // MAP files was created
      ProjectAnalyzerForm.SetFileName(ExecutableFileName, MapFileName, ProjectName);
      ProjectAnalyzerForm.Show;
    end;
    if SaveMapFile <> MapFileOptionDetailed then
    begin // delete MAP and DRC file
      DeleteFile(MapFileName);
      DeleteFile(ChangeFileExt(MapFileName, DrcFileExtension));
    end;
  end;
  if not Succ then
  begin
    ProjectAnalyzerForm.StatusBarText := '';
    if BuildOK then
      MessageDlg(RsCantFindFiles, mtError, [mbOk], 0);
  end;
end;

//--------------------------------------------------------------------------------------------------

procedure TProjectAnalyzerExpert.ActionUpdate(Sender: TObject);
var
  TempActiveProject: IOTAProject;
  ProjectName: string;
begin
  TempActiveProject := ActiveProject;
  if Assigned(TempActiveProject) then
    ProjectName := ExtractFileName(TempActiveProject.FileName)
  else
    ProjectName := '';
  FBuildAction.Enabled := Assigned(TempActiveProject);
  if not FBuildAction.Enabled then
    ProjectName := RsProjectNone;
  FBuildAction.Caption := Format(RsActionCaption, [ProjectName]);
end;

//--------------------------------------------------------------------------------------------------

destructor TProjectAnalyzerExpert.Destroy;
begin
  FreeAndNil(ProjectAnalyzerForm);
  inherited Destroy;
end;

//--------------------------------------------------------------------------------------------------

procedure TProjectAnalyzerExpert.RegisterCommands;
var
  IDEMainMenu: TMainMenu;
  IDEProjectItem: TMenuItem;
  IDEActionList: TActionList;
  I: Integer;
  ImageBmp: TBitmap;
begin
  FBuildAction := TAction.Create(nil);
  FBuildAction.Caption := Format(RsActionCaption, [RsProjectNone]);
  FBuildAction.Visible := True;
  FBuildAction.OnExecute := ActionExecute;
  FBuildAction.OnUpdate := ActionUpdate;
  FBuildAction.Name := RsActionName;
  ImageBmp := TBitmap.Create;
  try
    ImageBmp.LoadFromResourceName(FindResourceHInstance(HInstance), 'PROJANALYZER');
    FBuildAction.ImageIndex := NTAServices.AddMasked(ImageBmp, clOlive);
  finally
    ImageBmp.Free;
  end;

  IDEMainMenu := NTAServices.MainMenu;
  IDEProjectItem := nil;
  with IDEMainMenu do
    for I := 0 to Items.Count - 1 do
      if Items[I].Name = 'ProjectMenu' then
      begin
        IDEProjectItem := Items[I];
        Break;
      end;
  Assert(IDEProjectItem <> nil);
  with IDEProjectItem do
    for I := 0 to Count - 1 do
      if Items[I].Name = 'ProjectInformationItem' then
      begin
        IDEActionList := TActionList(NTAServices.ActionList);
        if Assigned(Items[I].Action) then
          FBuildAction.Category := TContainedAction(Items[I].Action).Category;
        FBuildAction.ActionList := IDEActionList;
        RegisterAction(FBuildAction);
        FBuildMenuItem := TMenuItem.Create(nil);
        FBuildMenuItem.Action := FBuildAction;

        IDEProjectItem.Insert(I + 1, FBuildMenuItem);

        System.Break;
      end;
  Assert(FBuildMenuItem.Parent <> nil);
end;

//--------------------------------------------------------------------------------------------------

procedure TProjectAnalyzerExpert.UnregisterCommands;
begin
  UnregisterAction(FBuildAction);
  FreeAndNil(FBuildMenuItem);
  FreeAndNil(FBuildAction);
end;

//--------------------------------------------------------------------------------------------------

end.
