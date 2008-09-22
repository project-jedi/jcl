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
{ The Original Code is JclOtaExcDlgFileFrame.pas.                                                  }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet                                     }
{         <outchy att users dott sourceforge dott net>                                             }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet. All rights reserved.     }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date::                                                                         $ }
{ Revision:      $Rev::                                                                          $ }
{ Author:        $Author::                                                                       $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclOtaExcDlgFileFrame;

interface

{$I jcl.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclBorlandTools, JclOtaWizardFrame, JclOtaExcDlgRepository;

type
  TJclOtaExcDlgFilePage = class(TJclWizardFrame)
    ComboBoxLanguage: TComboBox;
    LabelLanguage: TLabel;
    EditFormName: TEdit;
    LabelFormName: TLabel;
    EditFileName: TEdit;
    LabelFileName: TLabel;
    ButtonFileBrowse: TButton;
    EditFormAncestor: TEdit;
    LabelFormAncestor: TLabel;
    SaveDialogFileName: TSaveDialog;
    procedure ButtonFileBrowseClick(Sender: TObject);
    procedure ComboBoxLanguageClick(Sender: TObject);
  private
    FParams: TJclOtaExcDlgParams;
    procedure AdjustFileExtension;
    function GetSelectedLanguage: TJclBorPersonality;
  protected
    function GetSupportsNext: Boolean; override;
    property SelectedLanguage: TJclBorPersonality read GetSelectedLanguage;
  public
    constructor Create(AOwner: TComponent;
      AParams: TJclOtaExcDlgParams); reintroduce;

    procedure PageActivated(Direction: TJclWizardDirection); override;
    procedure PageDesactivated(Direction: TJclWizardDirection); override;

    property Params: TJclOtaExcDlgParams read FParams write FParams;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JCL\experts\repository'
    );
{$ENDIF UNITVERSIONING}

implementation

{$R *.dfm}

uses
  JclStrings, JclOtaResources;

//=== { TJclOtaExcDlgFilePage } ==============================================

procedure TJclOtaExcDlgFilePage.AdjustFileExtension;
var
  AFileName: string;
begin
  AFileName := EditFileName.Text;
  if AFileName <> '' then
  begin
    case SelectedLanguage of
      bpDelphi32:
        AFileName := ChangeFileExt(AFileName, SourceExtensionPAS);
      bpBCBuilder32:
        AFileName := ChangeFileExt(AFileName, SourceExtensionCPP);
    end;
    EditFileName.Text := AFileName;
  end;
end;

procedure TJclOtaExcDlgFilePage.ButtonFileBrowseClick(Sender: TObject);
  procedure AddFilter(const NewDescription, NewExtension: string);
  var
    AFilter: string;
  begin
    AFilter := SaveDialogFileName.Filter;
    if AFilter <> '' then
      AFilter := StrEnsureSuffix('|',AFilter);
    AFilter := Format('%s%s (*%s)|*%s',[AFilter, NewDescription, NewExtension, NewExtension]);
    SaveDialogFileName.Filter := AFilter;
  end;
begin
  SaveDialogFileName.FileName := EditFileName.Text;
  SaveDialogFileName.Title := RsFileNameDialog;

  SaveDialogFileName.Filter := '';
  AddFilter('All files', '.*');
  if (bpDelphi32 in Params.Languages) or (bpBCBuilder32 in Params.Languages) then
    AddFilter(SourceDescriptionPAS, SourceExtensionPAS);
  if bpBCBuilder32 in Params.Languages then
    AddFilter(SourceDescriptionCPP, SourceExtensionCPP);

  if ComboBoxLanguage.ItemIndex > -1 then
    case SelectedLanguage of
      bpDelphi32 :
        SaveDialogFileName.FilterIndex := 2;
      bpBCBuilder32 :
        SaveDialogFileName.FilterIndex := 3;
      else
        SaveDialogFileName.FilterIndex := 1;
    end
  else
    SaveDialogFileName.DefaultExt := '';

  if SaveDialogFileName.Execute then
    EditFileName.Text := SaveDialogFileName.FileName;
  AdjustFileExtension;
end;

procedure TJclOtaExcDlgFilePage.ComboBoxLanguageClick(Sender: TObject);
begin
  AdjustFileExtension;
end;

constructor TJclOtaExcDlgFilePage.Create(AOwner: TComponent;
  AParams: TJclOtaExcDlgParams);
begin
  FParams := AParams;
  inherited Create(AOwner);
  
  Caption := RsExcDlgFileOptions;
  LabelLanguage.Caption := RsLanguage;
  LabelFileName.Caption := RsFileName;
  LabelFormName.Caption := RsFormName;
  LabelFormAncestor.Caption := RsFormAncestor;
end;

function TJclOtaExcDlgFilePage.GetSelectedLanguage: TJclBorPersonality;
begin
  if ComboBoxLanguage.ItemIndex > -1 then
    Result := TJclBorPersonality(ComboBoxLanguage.Items.Objects[ComboBoxLanguage.ItemIndex])
  else
    Result := bpUnknown;
end;

function TJclOtaExcDlgFilePage.GetSupportsNext: Boolean;
begin
  Result := (ComboBoxLanguage.ItemIndex > -1) and (EditFormName.Text <> '') and (EditFormAncestor.Text <> '')
    and (( SelectedLanguage = Params.ActivePersonality)
         or (EditFileName.Text <> ''));
end;

procedure TJclOtaExcDlgFilePage.PageActivated(Direction: TJclWizardDirection);
var
  Language: TJclBorPersonality;
  ItemIndex: Integer;
begin
  inherited PageActivated(Direction);

  ComboBoxLanguage.Items.Clear;

  for Language := Low(TJclBorPersonality) to High(TJclBorPersonality) do
    if Language in Params.Languages then
  begin
    ItemIndex := ComboBoxLanguage.Items.AddObject(JclBorPersonalityDescription[Language], TObject(Language));
    if Language = Params.Language then
      ComboBoxLanguage.ItemIndex := ItemIndex;
  end;

  EditFileName.Text := Params.FileName;
  EditFormName.Text := Params.FormName;
  EditFormAncestor.Text := Params.FormAncestor;
end;

procedure TJclOtaExcDlgFilePage.PageDesactivated(
  Direction: TJclWizardDirection);
begin
  inherited PageDesactivated(Direction);

  if ComboBoxLanguage.ItemIndex > -1 then
    Params.Language := SelectedLanguage
  else
    Params.Language := bpUnknown;
  Params.FileName := EditFileName.Text;
  Params.FormName := EditFormName.Text;
  Params.FormAncestor := EditFormAncestor.Text;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
