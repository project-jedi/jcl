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
    {$IFNDEF DELPHI5_UP}
    FSaveAllAction: TAction;
    {$ENDIF DELPHI5_UP}
    procedure ActionExecute(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterCommand;
    procedure UnregisterCommand;
  end;

procedure Register;

implementation

{$R ProjAnalyzerIcon.res}

uses
  JclDebug, JclFileUtils,
  ProjAnalyzerFrm;

resourcestring
  RsActionCaption = 'Analyze project %s';
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
{$IFDEF DELPHI5_UP}
  OptionsModifiedState: Boolean;
{$ENDIF DELPHI5_UP}
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

  {$IFDEF DELPHI5_UP}
  OptionsModifiedState := ProjOptions.ModifiedState;
  {$ENDIF DELPHI5_UP}
  SaveMapFile := ProjOptions.Values[MapFileOptionName];
  ProjOptions.Values[MapFileOptionName] := MapFileOptionDetailed;
  BuildOK := TempActiveProject.ProjectBuilder.BuildProject(cmOTABuild, False);
  ProjOptions.Values[MapFileOptionName] := SaveMapFile;
{$IFDEF DELPHI5_UP}
  ProjOptions.ModifiedState := OptionsModifiedState;
{$ENDIF DELPHI5_UP}

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
{$IFDEF DELPHI5_UP}
  FBuildAction.Enabled := Assigned(TempActiveProject);
{$ELSE}
  FBuildAction.Enabled := Assigned(TempActiveProject) and (not FSaveAllAction.Enabled);
{$ENDIF DELPHI5_UP}
  if not FBuildAction.Enabled then
    ProjectName := RsProjectNone;
  FBuildAction.Caption := Format(RsActionCaption, [ProjectName]);
end;

//--------------------------------------------------------------------------------------------------

constructor TProjectAnalyzerExpert.Create;
begin
  inherited Create;
  RegisterCommand;
end;

//--------------------------------------------------------------------------------------------------

destructor TProjectAnalyzerExpert.Destroy;
begin
  UnregisterCommand;
  FreeAndNil(ProjectAnalyzerForm);
  inherited;
end;

//--------------------------------------------------------------------------------------------------

procedure TProjectAnalyzerExpert.RegisterCommand;
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
  ImageBmp := TBitmap.Create;
  try
    ImageBmp.LoadFromResourceName(FindResourceHInstance(HInstance), 'PROJANALYZER');
    FBuildAction.ImageIndex := (BorlandIDEServices as INTAServices).AddMasked(ImageBmp, clOlive);
  finally
    ImageBmp.Free;
  end;
  IDEActionList := TActionList((BorlandIDEServices as INTAServices).ActionList);
  FBuildAction.ActionList := IDEActionList;
  FBuildMenuItem := TMenuItem.Create(nil);
  FBuildMenuItem.Action := FBuildAction;
  IDEMainMenu := (BorlandIDEServices as INTAServices).MainMenu;
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
        IDEProjectItem.Insert(I + 1, FBuildMenuItem);
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
end;

//--------------------------------------------------------------------------------------------------

procedure TProjectAnalyzerExpert.UnregisterCommand;
begin
  FreeAndNil(FBuildMenuItem);
  FreeAndNil(FBuildAction);
end;

//--------------------------------------------------------------------------------------------------

end.
