unit JclOptionsFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls;

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

{$IFDEF VER140}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}

uses
  Registry, FileCtrl, ToolsAPI,
  JclUsesWizard;

{$R *.dfm}

//----------------------------------------------------------------------------
{ TFrameJCLOptions private }
//----------------------------------------------------------------------------

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

//----------------------------------------------------------------------------

function TFrameJclOptions.LoadFromRegistry: Boolean;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create(KEY_READ);
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    with BorlandIDEServices as IOTAServices do
    begin
      Result := Registry.OpenKey(GetBaseRegistryKey + '\' + SJCLRegSubkey, False);
      if not Result then
      begin
        Registry.RootKey := HKEY_LOCAL_MACHINE;
        Result := Registry.OpenKey(GetBaseRegistryKey + '\' + SJCLRegSubkey, False);
      end;
    end;

    if Result then
    begin
      CheckBoxWizardActive.Checked := Registry.ValueExists(SRegWizardActive) and
        Registry.ReadBool(SRegWizardActive);
      CheckBoxWizardConfirm.Checked := Registry.ValueExists(SRegWizardCofirm) and
        Registry.ReadBool(SRegWizardCofirm);
      EditIniFile.Text := Registry.ReadString(SRegWizardIniFile);
    end;
  finally
    Registry.Free;
  end;
end;

//----------------------------------------------------------------------------

procedure TFrameJclOptions.OKButtonClick(Sender: TObject);
begin
  { TODO -oTOndrej -cJedi Uses Wizard : validate entered directories }
  SaveToRegistry;
  SettingsChanged;

  if Assigned(FOKButtonClick) then
    FOKButtonClick(Sender);
end;

//----------------------------------------------------------------------------

function TFrameJclOptions.SaveToRegistry: Boolean;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    with BorlandIDEServices as IOTAServices do
      Result := Registry.OpenKey(GetBaseRegistryKey + '\' + SJCLRegSubkey, True);

    if Result then
    begin
      Registry.WriteBool(SRegWizardActive, CheckBoxWizardActive.Checked);
      Registry.WriteBool(SRegWizardCofirm, CheckBoxWizardConfirm.Checked);
      Registry.WriteString(SRegWizardIniFile, EditIniFile.Text);
    end;
  finally
    Registry.Free;
  end;
end;

//----------------------------------------------------------------------------
{ TFrameJCLOptions public }
//----------------------------------------------------------------------------

constructor TFrameJclOptions.Create(ATab: TTabSheet);
begin
  inherited Create(ATab);
  Parent := ATab;
  Align := alClient;
  LoadFromRegistry;
  HookOKButton;
end;

//----------------------------------------------------------------------------
{ TFrameJCLOptions event handlers }
//----------------------------------------------------------------------------

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
