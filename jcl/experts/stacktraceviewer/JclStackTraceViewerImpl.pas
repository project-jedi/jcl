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
{ The Original Code is JclStackTraceViewerImpl.pas.                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Uwe Schuster.                                      }
{ Portions created by Uwe Schuster are Copyright (C) 2009 Uwe Schuster. All rights reserved.       }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                        $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclStackTraceViewerImpl;

{$I jcl.inc}

interface

uses
  Windows, Classes, Menus, ActnList, ToolsAPI, SysUtils, Graphics, Dialogs, Controls, Forms,
  DeskUtil,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF BDS}
  {$IFDEF BDS7_UP}
  JclStackTraceViewerMainFormBDS7,
  {$ELSE ~BDS7_UP}
  JclStackTraceViewerMainFormBDS,
  {$ENDIF ~BDS7_UP}
  {$ELSE ~BDS}
  JclStackTraceViewerMainFormDelphi,
  {$ENDIF ~BDS}
  JclOtaUtils, JclStackTraceViewerConfigFrame, JclStackTraceViewerOptions;

type
  TJclStackTraceViewerExpert = class(TJclOTAExpert)
  private
    FIcon: TIcon;
    FOptions: TExceptionViewerOption;
    FOptionsFrame: TJclStackTraceViewerConfigFrame;
    FStackTraceViewMenuItem: TMenuItem;
    FStackTraceViewAction: TAction;
    procedure ActionExecute(Sender: TObject);
    procedure LoadExpertValues;
    procedure SaveExpertValues;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure RegisterCommands; override;
    procedure UnregisterCommands; override;
    procedure AddConfigurationPages(AddPageFunc: TJclOTAAddPageFunc); override;
    procedure ConfigurationClosed(AControl: TControl; SaveChanges: Boolean); override;
    property Icon: TIcon read FIcon;
    property Options: TExceptionViewerOption read FOptions;
  end;

// the expert var is required to get the icon and options in the MainForm/Frame create methods
// (the main form is a registered form and the IDE can create it too)
var
  StackTraceViewerExpert: TJclStackTraceViewerExpert = nil;

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
    LogPath: 'JCL\experts\stacktraceviewer';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

{$R JclStackTraceViewerIcon.res}

uses
  JclDebug, JclFileUtils,
  JclOtaConsts, JclOtaResources;

procedure Register;
begin
  try
    if Assigned(RegisterFieldAddress) then
      RegisterFieldAddress(JclStackTraceViewerDesktopIniSection, @frmStackView);
    RegisterDesktopFormClass(TfrmStackView, JclStackTraceViewerDesktopIniSection, JclStackTraceViewerDesktopIniSection);
    StackTraceViewerExpert := TJclStackTraceViewerExpert.Create;
    RegisterPackageWizard(StackTraceViewerExpert);
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
begin
  try
    if JCLWizardIndex <> -1 then
    begin
      StackTraceViewerExpert := nil;
      TJclOTAExpertBase.GetOTAWizardServices.RemoveWizard(JCLWizardIndex);
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
begin
  try
    TerminateProc := JclWizardTerminate;

    if Assigned(RegisterFieldAddress) then
      RegisterFieldAddress(JclStackTraceViewerDesktopIniSection, @frmStackView);
    RegisterDesktopFormClass(TfrmStackView, JclStackTraceViewerDesktopIniSection, JclStackTraceViewerDesktopIniSection);
    StackTraceViewerExpert := TJclStackTraceViewerExpert.Create;
    JCLWizardIndex := TJclOTAExpertBase.GetOTAWizardServices.AddWizard(StackTraceViewerExpert);

    Result := True;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      Result := False;
    end;
  end;
end;

//=== { TJclStackTraceViewerExpert } =========================================

constructor TJclStackTraceViewerExpert.Create;
begin
  inherited Create(JclStackTraceViewerExpertName);
  FOptions := TExceptionViewerOption.Create;
end;

destructor TJclStackTraceViewerExpert.Destroy;
begin
  FOptions.Free;
  FreeAndNil(frmStackView);
  inherited Destroy;
end;

procedure TJclStackTraceViewerExpert.ActionExecute(Sender: TObject);
begin
  try
    if not Assigned(frmStackView) then
    begin
      frmStackView := TfrmStackView.Create(Application);
      frmStackView.Show;
    end
    else
      frmStackView.Show;
  except
    on ExceptionObj: TObject do
    begin
      JclExpertShowExceptionDialog(ExceptionObj);
      raise;
    end;
  end;
end;

procedure TJclStackTraceViewerExpert.AddConfigurationPages(AddPageFunc: TJclOTAAddPageFunc);
begin
  inherited AddConfigurationPages(AddPageFunc);
  FOptionsFrame := TJclStackTraceViewerConfigFrame.Create(nil);
  FOptionsFrame.Options := FOptions;
  AddPageFunc(FOptionsFrame, rsStackTraceViewerOptionsPageName, Self);
end;

procedure TJclStackTraceViewerExpert.ConfigurationClosed(AControl: TControl; SaveChanges: Boolean);
begin
  if (AControl = FOptionsFrame) and Assigned(FOptionsFrame) then
  begin
    if SaveChanges then
      FOptions.Assign(FOptionsFrame.Options);
    FreeAndNil(FOptionsFrame);
    if SaveChanges and Assigned(frmStackView) then
      frmStackView.Options := FOptions;
  end
  else
    inherited ConfigurationClosed(AControl, SaveChanges);
end;

procedure TJclStackTraceViewerExpert.LoadExpertValues;
begin
  FOptions.ExpandTreeView := Settings.LoadBool('ExpandTreeView', FOptions.ExpandTreeView);
  FOptions.ModuleVersionAsRevision := Settings.LoadBool('ModuleVersionAsRevision', FOptions.ModuleVersionAsRevision);
end;

procedure TJclStackTraceViewerExpert.RegisterCommands;
var
  I, ViewDebugMenuIdx: Integer;
  IDEMenu: TMainMenu;
  ViewMenu: TMenuItem;
  Category: string;
  NTAServices: INTAServices;
begin
  inherited RegisterCommands;

  NTAServices := GetNTAServices;

  Category := '';
  for I := 0 to NTAServices.ActionList.ActionCount - 1 do
    if CompareText(NTAServices.ActionList.Actions[I].Name, 'ViewPrjMgrCommand') = 0 then
    begin
      Category := NTAServices.ActionList.Actions[I].Category;
      Break;
    end;

  FIcon := TIcon.Create;
  FIcon.Handle := LoadIcon(FindResourceHInstance(ModuleHInstance), 'JCLSTACKTRACEVIEWER');

  // create actions
  FStackTraceViewAction := TAction.Create(nil);
  FStackTraceViewAction.Caption := rsStackTraceViewerCaption;
  FStackTraceViewAction.Visible := True;
  FStackTraceViewAction.OnExecute := ActionExecute;
  FStackTraceViewAction.Category := Category;
  FStackTraceViewAction.Name := JclStackTraceViewerActionName;
  FStackTraceViewAction.ActionList := NTAServices.ActionList;
  FStackTraceViewAction.ImageIndex := NTAServices.ImageList.AddIcon(FIcon);

  FStackTraceViewMenuItem := TMenuItem.Create(nil);
  FStackTraceViewMenuItem.Name := JclStackTraceViewerMenuName;
  FStackTraceViewMenuItem.Action := FStackTraceViewAction;

  IDEMenu := NTAServices.MainMenu;

  LoadExpertValues;

  ViewMenu := nil;
  for I := 0 to IDEMenu.Items.Count - 1 do
    if CompareText(IDEMenu.Items[I].Name, 'ViewsMenu') = 0 then
      ViewMenu := IDEMenu.Items[I];
  if not Assigned(ViewMenu) then
    raise EJclExpertException.CreateTrace(RsENoViewMenuItem);

  ViewDebugMenuIdx := -1;
  for I := 0 to ViewMenu.Count - 1 do
    if CompareText(ViewMenu.Items[I].Name, 'ViewDebugItem') = 0 then
    begin
      ViewDebugMenuIdx := I;
      Break;
    end;
  if ViewDebugMenuIdx = -1 then
    raise EJclExpertException.CreateTrace(RsENoDebugWindowsMenuItem);

  ViewMenu.Insert(ViewDebugMenuIdx + 1, FStackTraceViewMenuItem);

  RegisterAction(FStackTraceViewAction);
end;

procedure TJclStackTraceViewerExpert.SaveExpertValues;
begin
  Settings.SaveBool('ExpandTreeView', FOptions.ExpandTreeView);
  Settings.SaveBool('ModuleVersionAsRevision', FOptions.ModuleVersionAsRevision);
end;

procedure TJclStackTraceViewerExpert.UnregisterCommands;
begin
  inherited UnregisterCommands;
  SaveExpertValues;
  UnregisterAction(FStackTraceViewAction);
  FreeAndNil(FIcon);
  FreeAndNil(FStackTraceViewMenuItem);
  FreeAndNil(FStackTraceViewAction);
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  if Assigned(UnRegisterFieldAddress) then
    UnRegisterFieldAddress(@frmStackView);
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.