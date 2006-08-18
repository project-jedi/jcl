{******************************************************************************}
{                                                                              }
{ Project JEDI Code Library (JCL) extension                                    }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.1 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is ProjAnalyzerImpl.pas.                                   }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ of these individuals.                                                        }
{                                                                              }
{ Unit owner: Petr Vones                                                       }
{ Last modified: April 29, 2001                                                }
{                                                                              }
{******************************************************************************}

unit ProjAnalyzerImpl;

{$I JCL.INC}

interface

uses
  Windows, Classes, Menus, ActnList, ToolsAPI, SysUtils, Graphics, Dialogs,
  Forms;

type
  TMapViewImpl = class(TObject)
  private
    FBuildMenuItem: TMenuItem;
    FBuildAction: TAction;
    {$IFNDEF DELPHI5_UP}
    FSaveAllAction: TAction;
    {$ENDIF DELPHI5_UP}
    MessageServices: IOTAMessageServices;
    Services: IOTAServices;
    procedure ActionExecute(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
    function GetActiveProject: IOTAProject;
    function GetProjectGroup: IOTAProjectGroup;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterCommand;
    procedure UnregisterCommand;
  end;

implementation

{$R ProjAnalyzerIcon.res}

uses
  JclDebug, JclFileUtils, JclPeImage, JclStrings, JclSysUtils,
  ProjAnalyzerFrm;

resourcestring
  RsActionCaption = 'Analyze project %s';
  RsProjectNone = '[none]';
  RsCantFindFiles = 'Can''t find MAP or executable file';
  RsBuildingProject = 'Building project %s ...';

//------------------------------------------------------------------------------

var
  MapViewExpert: TMapViewImpl = nil;

const
  MapFileOptionDetailed = 3;

//==============================================================================
// TMapViewImpl
//==============================================================================

procedure TMapViewImpl.ActionExecute(Sender: TObject);
var
  ActiveProject: IOTAProject;
  BuildOK, Succ: Boolean;
  ProjOptions: IOTAProjectOptions;
  SaveMapFile: Variant;
  OutputDirectory, ProjectFileName, MapFileName, ExecutableFileName: TFileName;
  ProjectName: string;
{$IFDEF DELPHI5_UP}
  OptionsModifiedState: Boolean;
{$ENDIF DELPHI5_UP}

  function FindExecutableName: Boolean;
  var
    Se: TSearchRec;
    Res: Integer;
    LatestTime: Integer;
    FileName: TFileName;
  begin
    LatestTime := 0;
    ExecutableFileName := '';
    // the latest executable file is very likely our file
    Res := FindFirst(ChangeFileExt(MapFileName, '.*'), faArchive, Se);
    while Res = 0 do
    begin
      FileName := PathAddSeparator(OutputDirectory) + Se.Name;
      if (Se.Time > LatestTime) and IsValidPeFile(FileName) then
      begin
        ExecutableFileName := FileName;
        LatestTime := Se.Time;
      end;
      Res := FindNext(Se);
    end;
    FindClose(Se);
    Result := (ExecutableFileName <> '');
  end;

begin
  ActiveProject := GetActiveProject;
  Assert(Assigned(ActiveProject));
  ProjectFileName := ActiveProject.FileName;
  ProjectName := ExtractFileName(ProjectFileName);
  Succ := False;

  ProjOptions := ActiveProject.ProjectOptions;
  OutputDirectory := ProjOptions.Values['OutputDir'];
  if OutputDirectory = '' then
    OutputDirectory := ExtractFilePath(ProjectFileName);
  MapFileName := PathAddSeparator(OutputDirectory) + ChangeFileExt(ProjectName, '.map');

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
  SaveMapFile := ProjOptions.Values['MapFile'];
  ProjOptions.Values['MapFile'] := MapFileOptionDetailed;
  BuildOK := ActiveProject.ProjectBuilder.BuildProject(cmOTABuild, False);
  ProjOptions.Values['MapFile'] := SaveMapFile;
{$IFDEF DELPHI5_UP}
  ProjOptions.ModifiedState := OptionsModifiedState;
{$ENDIF DELPHI5_UP}

  if BuildOK then
  begin // Build was successful, continue ...
    Succ := FileExists(MapFileName) and FindExecutableName;
    if Succ then
    begin // MAP files was created
      ProjectAnalyzerForm.SetFileName(ExecutableFileName, MapFileName, ProjectName);
      ProjectAnalyzerForm.Show;
    end;
    if SaveMapFile <> MapFileOptionDetailed then
    begin // delete MAP and DRC file
      DeleteFile(MapFileName);
      DeleteFile(ChangeFileExt(MapFileName, '.drc'));
    end;
  end;
  if not Succ then
  begin
    ProjectAnalyzerForm.StatusBarText := '';
    if BuildOK then
      MessageDlg(RsCantFindFiles, mtError, [mbOk], 0);
  end;
end;

//------------------------------------------------------------------------------

procedure TMapViewImpl.ActionUpdate(Sender: TObject);
var
  ActiveProject: IOTAProject;
  ProjectName: string;
  IsDPK: Boolean;
begin
  ActiveProject := GetActiveProject;
  if Assigned(ActiveProject) then
    ProjectName := ExtractFileName(ActiveProject.FileName)
  else
    ProjectName := '';
  IsDPK := StrSame(ExtractFileExt(ProjectName), '.dpk');
{$IFDEF DELPHI5_UP}
  FBuildAction.Enabled := Assigned(ActiveProject) and (not IsDPK);
{$ELSE}
  FBuildAction.Enabled := Assigned(ActiveProject) and (not IsDPK) and (not FSaveAllAction.Enabled);
{$ENDIF DELPHI5_UP}
  if not FBuildAction.Enabled then
    ProjectName := RsProjectNone;
  FBuildAction.Caption := Format(RsActionCaption, [ProjectName]);
end;

//------------------------------------------------------------------------------

constructor TMapViewImpl.Create;
begin
  inherited Create;
  MessageServices := BorlandIDEServices as IOTAMessageServices;
  Services := BorlandIDEServices as IOTAServices;
  RegisterCommand;
end;

//------------------------------------------------------------------------------

destructor TMapViewImpl.Destroy;
begin
  UnregisterCommand;
  FreeAndNil(ProjectAnalyzerForm);
  inherited;
end;

//------------------------------------------------------------------------------

function TMapViewImpl.GetActiveProject: IOTAProject;
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

function TMapViewImpl.GetProjectGroup: IOTAProjectGroup;
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

procedure TMapViewImpl.RegisterCommand;
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

//------------------------------------------------------------------------------

procedure TMapViewImpl.UnregisterCommand;
begin
  FBuildMenuItem.Free;
  FBuildAction.Free;
end;

//------------------------------------------------------------------------------

initialization
  MapViewExpert := TMapViewImpl.Create;

finalization
  MapViewExpert.Free;

end.
