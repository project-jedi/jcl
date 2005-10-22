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
{ The Original Code is JclOptionsFrame.pas.                                                        }
{                                                                                                  }
{ The Initial Developer of the Original Code is documented in the accompanying                     }
{ help file JCL.chm. Portions created by these individuals are Copyright (C) of these individuals. }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Unit owner: Robert Marquardt                                                                     }
{ Last modified: October 19, 2005                                                                  }
{                                                                                                  }
{**************************************************************************************************}

unit JclOptionsFrame;

{$I jcl.inc}
{$I windowsonly.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  TFrameJclOptions = class(TFrame)
    ButtonIniFile: TButton;
    CheckBoxWizardActive: TCheckBox;
    CheckBoxWizardConfirm: TCheckBox;
    EditIniFile: TEdit;
    GroupBoxWizard: TGroupBox;
    LabelIniFile: TLabel;
    OpenDialog: TOpenDialog;
    procedure ButtonIniFileClick(Sender: TObject);
  private
    FOKButtonClick: TNotifyEvent;
    function HookOKButton: Boolean;
    function LoadFromRegistry: Boolean;
    procedure OKButtonClick(Sender: TObject);
    function SaveToRegistry: Boolean;
  public
    constructor Create(ATab: TTabSheet); reintroduce;
  end;

implementation

uses
  ToolsAPI,
  JclOtaConsts, JclRegistry, JclUsesWizard;

{$R *.dfm}

constructor TFrameJclOptions.Create(ATab: TTabSheet);
begin
  inherited Create(ATab);
  Parent := ATab;
  Align := alClient;
  LoadFromRegistry;
  HookOKButton;
end;

function TFrameJclOptions.HookOKButton: Boolean;
var
  ParentForm: TCustomForm;
  Panel1: TPanel;
  OKButton: TButton;
begin
  Result := False;
  FOKButtonClick := nil;

  ParentForm := GetParentForm(Self);
  if not Assigned(ParentForm) then
    Exit;

  Panel1 := ParentForm.FindChildControl('Panel1') as TPanel;
  if not Assigned(Panel1) then
    Exit;

  OKButton := Panel1.FindChildControl('OKButton') as TButton;
  if not Assigned(OKButton) then
    Exit;

  FOKButtonClick := OKButton.OnClick;
  OKButton.OnClick := OKButtonClick;

  Result := True;
end;

function TFrameJclOptions.LoadFromRegistry: Boolean;
var
  S: string;
  Root: DelphiHKEY;
begin
  S := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\' + JediIDESubKey + SUsesExpertSubkey;
  Root := HKEY_CURRENT_USER;
  Result := RegKeyExists(Root, S);
  if not Result then
  begin
    Root := HKEY_LOCAL_MACHINE;
    Result := RegKeyExists(Root, S);
  end;
  CheckBoxWizardActive.Checked := RegReadBoolDef(Root, S, SRegWizardActive, False);
  CheckBoxWizardConfirm.Checked := RegReadBoolDef(Root, S, SRegWizardConfirm, True);
  EditIniFile.Text := RegReadStringDef(Root, S, SRegWizardIniFile, '');
end;

procedure TFrameJclOptions.OKButtonClick(Sender: TObject);
begin
  { TODO -oTOndrej -cJedi Uses Wizard : validate entered directories }
  SaveToRegistry;
  SettingsChanged;

  if Assigned(FOKButtonClick) then
    FOKButtonClick(Sender);
end;

function TFrameJclOptions.SaveToRegistry: Boolean;
var
  S: string;
begin
  S := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\' + JediIDESubKey + SUsesExpertSubkey;
  RegCreateKey(HKEY_CURRENT_USER, S);
  RegWriteBool(HKEY_CURRENT_USER, S, SRegWizardActive, CheckBoxWizardActive.Checked);
  RegWriteBool(HKEY_CURRENT_USER, S, SRegWizardConfirm, CheckBoxWizardConfirm.Checked);
  RegWriteString(HKEY_CURRENT_USER, S, SRegWizardIniFile, EditIniFile.Text);
end;

procedure TFrameJclOptions.ButtonIniFileClick(Sender: TObject);
begin
  with OpenDialog do
  begin
    InitialDir := ExtractFilePath(EditIniFile.Text);
    FileName := EditIniFile.Text;
    if Execute then
      EditIniFile.Text := FileName;
  end;
end;

end.
