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
{ Last modified: $Date::                                                                       $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit ProjAnalyzerImpl;

{$I jcl.inc}

interface

uses
  Classes, Menus, ActnList, ToolsAPI, SysUtils, Graphics, Dialogs, Forms,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclOtaUtils, ProjAnalyzerFrm;

type
  TJclProjectAnalyzerExpert = class(TJclOTAExpert)
  private
    FBuildMenuItem: TMenuItem;
    FBuildAction: TAction;
    {$IFDEF BDS4_UP}
    FProjectManagerNotifierIndex: Integer;
    {$ENDIF BDS4_UP}
    procedure ActionExecute(Sender: TObject);
    procedure ActionUpdate(Sender: TObject);
    procedure AnalyzeProject(const AProject: IOTAProject);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure RegisterCommands; override;
    procedure UnregisterCommands; override;
  end;

  {$IFDEF BDS4_UP}
  TProjectManagerNotifier = class(TNotifierObject, IOTANotifier, INTAProjectMenuCreatorNotifier)
  private
    FProjectAnalyser: TJclProjectAnalyzerExpert;
    FOTAProjectManager: IOTAProjectManager;
    procedure AnalyzeProjectMenuClick(Sender: TObject);
  protected
    { INTAProjectMenuCreatorNotifier }
    function AddMenu(const Ident: string): TMenuItem;
    function CanHandle(const Ident: string): Boolean;
  public
    constructor Create(AProjectAnalyzer: TJclProjectAnalyzerExpert; const AOTAProjectManager: IOTAProjectManager);
  end;
  {$ENDIF BDS4_UP}

// design package entry point
procedure Register;

// expert DLL entry point
function JCLWizardInit(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var TerminateProc: TWizardTerminateProc): Boolean; stdcall;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\projectanalyser'
    );
{$ENDIF UNITVERSIONING}

implementation

{$R ProjAnalyzerIcon.res}

uses
  JclDebug, JclFileUtils, JclOtaConsts, 
  JclOtaResources;

procedure Register;
begin
  try
    RegisterPackageWizard(TJclProjectAnalyzerExpert.Create);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

var
  JCLWizardIndex: Integer;

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

    JCLWizardIndex := OTAWizardServices.AddWizard(TJclProjectAnalyzerExpert.Create);

    Result := True;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      Result := False;
    end;
  end;
end;

//=== { TJclProjectAnalyzerExpert } ==========================================

constructor TJclProjectAnalyzerExpert.Create;
begin
  inherited Create(JclProjectAnalyzerExpertName);
end;

destructor TJclProjectAnalyzerExpert.Destroy;
begin
  FreeAndNil(ProjectAnalyzerForm);
  inherited Destroy;
end;

procedure TJclProjectAnalyzerExpert.ActionExecute(Sender: TObject);
var
  TempActiveProject: IOTAProject;
begin
  try
    TempActiveProject := ActiveProject;
    if TempActiveProject <> nil then
      AnalyzeProject(TempActiveProject)
    else
      raise EJclExpertException.CreateTrace(RsENoActiveProject);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TJclProjectAnalyzerExpert.ActionUpdate(Sender: TObject);
var
  TempActiveProject: IOTAProject;
  ProjectName: string;
begin
  try
    TempActiveProject := ActiveProject;
    if Assigned(TempActiveProject) then
      ProjectName := ExtractFileName(TempActiveProject.FileName)
    else
      ProjectName := '';
    FBuildAction.Enabled := Assigned(TempActiveProject);
    if not FBuildAction.Enabled then
      ProjectName := RsProjectNone;
    FBuildAction.Caption := Format(RsAnalyzeActionCaption, [ProjectName]);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TJclProjectAnalyzerExpert.AnalyzeProject(const AProject: IOTAProject);
var
  BuildOK, Succ: Boolean;
  ProjOptions: IOTAProjectOptions;
  SaveMapFile: Variant;
  ProjectName, OutputDirectory: string;
  ProjectFileName, MapFileName, ExecutableFileName: TFileName;
begin
  try
    JclDisablePostCompilationProcess := True;

    ProjectFileName := AProject.FileName;
    ProjectName := ExtractFileName(ProjectFileName);
    Succ := False;

    ProjOptions := AProject.ProjectOptions;
    if not Assigned(ProjOptions) then
      raise EJclExpertException.CreateTrace(RsENoProjectOptions);
      
    OutputDirectory := GetOutputDirectory(AProject);
    MapFileName := GetMapFileName(AProject);

    if ProjectAnalyzerForm = nil then
    begin
      ProjectAnalyzerForm := TProjectAnalyzerForm.Create(Application, Settings);
      ProjectAnalyzerForm.Show;
    end;
    ProjectAnalyzerForm.ClearContent;
    ProjectAnalyzerForm.StatusBarText := Format(RsBuildingProject, [ProjectName]);

    SaveMapFile := ProjOptions.Values[MapFileOptionName];
    ProjOptions.Values[MapFileOptionName] := MapFileOptionDetailed;
    // workaround for MsBuild, the project has to be saved (seems useless with Delphi 2007 update 1)
    ProjOptions.ModifiedState := True;
    //TempActiveProject.Save(False, True);

    BuildOK := AProject.ProjectBuilder.BuildProject(cmOTABuild, False);

    ProjOptions.Values[MapFileOptionName] := SaveMapFile;
    // workaround for MsBuild, the project has to be saved (seems useless with Delphi 2007 update 1)
    ProjOptions.ModifiedState := True;
    //TempActiveProject.Save(False, True);

    if BuildOK then
    begin // Build was successful, continue ...
      Succ := FileExists(MapFileName) and FindExecutableName(MapFileName, OutputDirectory, ExecutableFileName);
      if Succ then
      begin // MAP files was created
        ProjectAnalyzerForm.SetFileName(ExecutableFileName, MapFileName, ProjectName);
        ProjectAnalyzerForm.Show;
      end;
      if Integer(SaveMapFile) <> MapFileOptionDetailed then
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
  finally
    JclDisablePostCompilationProcess := False;
  end;
end;

procedure TJclProjectAnalyzerExpert.RegisterCommands;
var
  IDEMainMenu: TMainMenu;
  IDEProjectItem: TMenuItem;
  IDEActionList: TActionList;
  I: Integer;
  ImageBmp: TBitmap;
begin
  inherited RegisterCommands;

  // create actions
  FBuildAction := TAction.Create(nil);
  FBuildAction.Caption := Format(RsAnalyzeActionCaption, [RsProjectNone]);
  FBuildAction.Visible := True;
  FBuildAction.OnExecute := ActionExecute;
  FBuildAction.OnUpdate := ActionUpdate;
  FBuildAction.Name := JclProjectAnalyzeActionName;
  ImageBmp := TBitmap.Create;
  try
    ImageBmp.LoadFromResourceName(FindResourceHInstance(ModuleHInstance), 'PROJANALYZER');
    FBuildAction.ImageIndex := NTAServices.AddMasked(ImageBmp, clOlive);
  finally
    ImageBmp.Free;
  end;

  // create project manager notifier
  {$IFDEF BDS4_UP}
  FProjectManagerNotifierIndex := ProjectManager.AddMenuCreatorNotifier(TProjectManagerNotifier.Create(Self,
    ProjectManager));
  {$ENDIF BDS4_UP}

  // create menu item
  IDEMainMenu := NTAServices.MainMenu;
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
      if Items[I].Name = 'ProjectInformationItem' then
      begin
        IDEActionList := TActionList(NTAServices.ActionList);
        if Assigned(Items[I].Action) then
          FBuildAction.Category := TContainedAction(Items[I].Action).Category;
        FBuildAction.ActionList := IDEActionList;
        RegisterAction(FBuildAction);
        FBuildMenuItem := TMenuItem.Create(nil);
        FBuildMenuItem.Name := JclProjectAnalyzeMenuName;
        FBuildMenuItem.Action := FBuildAction;

        IDEProjectItem.Insert(I + 1, FBuildMenuItem);

        System.Break;
      end;
  if not Assigned(FBuildMenuItem.Parent) then
    raise EJclExpertException.CreateTrace(RsAnalyseMenuItemNotInserted);
end;

procedure TJclProjectAnalyzerExpert.UnregisterCommands;
begin
  inherited UnregisterCommands;
  // remove notifier
  {$IFDEF BDS4_UP}
  if FProjectManagerNotifierIndex <> -1 then
    ProjectManager.RemoveMenuCreatorNotifier(FProjectManagerNotifierIndex);
  {$ENDIF BDS4_UP}

  UnregisterAction(FBuildAction);
  FreeAndNil(FBuildMenuItem);
  FreeAndNil(FBuildAction);
end;

{$IFDEF BDS4_UP}

//=== { TProjectManagerNotifier } ============================================

constructor TProjectManagerNotifier.Create(AProjectAnalyzer: TJclProjectAnalyzerExpert;
  const AOTAProjectManager: IOTAProjectManager);
begin
  inherited Create;
  FProjectAnalyser := AProjectAnalyzer;
  FOTAProjectManager := AOTAProjectManager;
end;

function TProjectManagerNotifier.AddMenu(const Ident: string): TMenuItem;
var
  SelectedIdent: string;
  AProject: IOTAProject;
begin
  try
    SelectedIdent := Ident;
    AProject := FOTAProjectManager.GetCurrentSelection(SelectedIdent);
    if AProject <> nil then
    begin
      // root item
      Result := TMenuItem.Create(nil);
      Result.Visible := True;
      Result.Caption := Format(RsAnalyzeActionCaption, [ExtractFileName(AProject.FileName)]);
      Result.OnClick := AnalyzeProjectMenuClick;
    end
    else
      raise EJclExpertException.CreateTrace(RsENoActiveProject);
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TProjectManagerNotifier.AnalyzeProjectMenuClick(Sender: TObject);
var
  TempProject: IOTAProject;
  SelectedIdent: string;
begin
  try
    SelectedIdent := '';
    TempProject := FOTAProjectManager.GetCurrentSelection(SelectedIdent);
    if TempProject <> nil then
      FProjectAnalyser.AnalyzeProject(TempProject)
    else
      raise EJclExpertException.CreateTrace(RsENoActiveProject);
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

{$ENDIF BDS4_UP}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
