unit JclInstallHelperMain;

{$I jcl.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DelphiInstall, Menus;

const
  UM_CHECKUPDATES = WM_USER + $100;

type
  TMainForm = class(TForm)
    D4GroupBox: TGroupBox;
    D5GroupBox: TGroupBox;
    D6GroupBox: TGroupBox;
    InstallBtn: TButton;
    CloseBtn: TButton;
    JediImage: TImage;
    Panel1: TPanel;
    Label4: TLabel;
    Label5: TLabel;
    CheckPopupMenu: TPopupMenu;
    Checkall1: TMenuItem;
    Uncheckall1: TMenuItem;
    GroupBox1: TGroupBox;
    D4InstallCheckBox: TCheckBox;
    Label1: TLabel;
    D4ReposPageComboBox: TComboBox;
    GroupBox2: TGroupBox;
    D4HlpHelpCheckBox: TCheckBox;
    D4ChmHelpCheckBox: TCheckBox;
    GroupBox3: TGroupBox;
    D4SourceLibCheckBox: TCheckBox;
    GroupBox4: TGroupBox;
    D5InstallCheckBox: TCheckBox;
    Label2: TLabel;
    D5ReposPageComboBox: TComboBox;
    GroupBox5: TGroupBox;
    D5HlpHelpCheckBox: TCheckBox;
    D5ChmHelpCheckBox: TCheckBox;
    GroupBox6: TGroupBox;
    D5SourceLibCheckBox: TCheckBox;
    GroupBox7: TGroupBox;
    D6InstallVclCheckBox: TCheckBox;
    D6InstallClxCheckBox: TCheckBox;
    Label3: TLabel;
    D6ReposPageComboBox: TComboBox;
    GroupBox8: TGroupBox;
    D6HlpHelpCheckBox: TCheckBox;
    D6ChmHelpCheckBox: TCheckBox;
    GroupBox9: TGroupBox;
    D6SourceLibCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure D4InstallCheckBoxClick(Sender: TObject);
    procedure InstallBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure JediImageClick(Sender: TObject);
    procedure Checkall1Click(Sender: TObject);
  private
    FDelphiInstallations: TJclDelphiInstallations;
    FJclPath: string;
    FJclSourcePath: string;
    FClxDialogFileName: string;
    FVclDialogFileName: string;
    FClxDialogIconFileName: string;
    FVclDialogIconFileName: string;
    FJclChmHelpFileName: string;
    FJclHlpHelpFileName: string;
    procedure CheckAllCheckBoxes(Check: Boolean);
    function CheckRunningInstances: Boolean;
    procedure CheckUpdatePacks;
    procedure DisableGroup(GroupBox: TGroupBox);
    procedure Install;
    procedure ReadDialogPath;
    procedure UpdateButtons;
    procedure UpdateControls;
    procedure UMCheckUpdates(var Message: TMessage); message UM_CHECKUPDATES;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  IniFiles,
  JclBase, JclFileUtils, JclStrings, JclSysUtils, JclShell;

const
  DialogsPath       = 'examples\debugextension\dialog\';
  ClxDialogFileName = 'ClxExceptDlg.pas';
  VclDialogFileName = 'ExceptDlg.pas';
  ClxDialogName     = 'CLX Exception Dialog';
  VclDialogName     = 'Exception Dialog';
  DialogDescription = 'JCL Application exception dialog';
  DialogAuthor      = 'Project JEDI';

  JclChmHelpFile    = 'Help\JCLHelp.chm';
  JclHlpHelpFile    = 'Help\JCLHelp.hlp';
  JclHelpTitle      = 'JCL 1.20 Help';
  JclHelpIndexName  = 'Jedi Code Library Reference';
  HHFileName        = 'HH.EXE';

  DelphiSupportURL  = 'http://www.borland.com/devsupport/delphi/';
  DelphiJediURL     = 'http://delphi-jedi.org';

resourcestring
  RsCantFindFiles   = 'Can not find installation files, check your installation.';
  RsCloseDelphi     = 'Please close all running instances of Delphi IDE before the installation.';
  RsInstallSuccess  = 'Installation was successful. You might want to install additional IDE experts located in $(JCL)\Packages\JclPackagesXX.bpg project group.';
  RsSourceLibHint   = 'Adds "%s" to the Library Path';
  RsUpdateNeeded    = '. Would you like to open Borland Delphi support web page ?';

{ TMainForm }

procedure TMainForm.CheckAllCheckBoxes(Check: Boolean);
var
  I: Integer;
begin
  for I := 0 to ComponentCount - 1 do
    if Components[I] is TCheckBox then
      with TCheckBox(Components[I]) do
        if Enabled then
          Checked := Check;
end;

function TMainForm.CheckRunningInstances: Boolean;
begin
  Result := FDelphiInstallations.AnyInstanceRunning;
  if Result then
    with Application do
      MessageBox(PChar(RsCloseDelphi), PChar(Title), MB_OK or MB_ICONWARNING);
end;

procedure TMainForm.CheckUpdatePacks;
var
  UpdateText: string;
begin
  if FDelphiInstallations.AnyUpdatePackNeeded(UpdateText) then
  begin
    UpdateText := UpdateText + RsUpdateNeeded;
    with Application do
      if MessageBox(PChar(UpdateText), PChar(Title), MB_YESNO or MB_DEFBUTTON1 or MB_ICONEXCLAMATION) = ID_YES then
        ShellExecEx(DelphiSupportURL);
  end;
end;

procedure TMainForm.DisableGroup(GroupBox: TGroupBox);

  procedure Disable(C: TWinControl);
  var
    I: Integer;
  begin
    for I := 0 to C.ControlCount - 1 do
      if C.Controls[I] is TWinControl then
      begin
        C.Controls[I].Enabled := False;
        Disable(C.Controls[I] as TWinControl);
      end;  
  end;

begin
  Disable(GroupBox);
end;

procedure TMainForm.Install;
var
  Installation: TJclDelphiInstallation;
  IniFile: TIniFile;

  procedure AddHelpToDelphiHelp;
  begin
    Installation.OpenHelp.AddHelpFile(FJclHlpHelpFileName, JclHelpIndexName);
  end;

  procedure AddHelpToIdeTools;
  var
    ToolsIndex: Integer;
  begin
    if Installation.IdeTools.IndexOfTitle(JclHelpTitle) = -1 then
    begin
      ToolsIndex := Installation.IdeTools.Count;
      Installation.IdeTools.Count := ToolsIndex + 1;
      Installation.IdeTools.Title[ToolsIndex] := JclHelpTitle;
      Installation.IdeTools.Path[ToolsIndex] := HHFileName;
      Installation.IdeTools.Parameters[ToolsIndex] := StrDoubleQuote(FJclChmHelpFileName);
      Installation.IdeTools.WorkingDir[ToolsIndex] := FJclPath;
    end;  
  end;

  procedure AddLibraryPath;
  var
    LibraryPath: TJclDelphiPath;
    Items: TStringList;
  begin
    LibraryPath := Installation.LibrarySearchPath;
    Items := TStringList.Create;
    try
      Installation.ExtractPaths(LibraryPath, Items);
      if Installation.FindFolderInDelphiPath(FJclSourcePath, Items) = -1 then
      begin
        LibraryPath := StrEnsureSuffix(DelphiLibraryPathSeparator, LibraryPath) + FJclSourcePath;
        Installation.LibrarySearchPath := LibraryPath;
      end;
    finally
      Items.Free;
    end;
  end;

begin
  IniFile := nil;
  // Reset ReadOnly flag for dialog forms
  FileSetAttr(FClxDialogFileName, faArchive);
  FileSetAttr(ChangeFileExt(FClxDialogFileName, '.xfm'), faArchive);
  FileSetAttr(FVclDialogFileName, faArchive);
  FileSetAttr(ChangeFileExt(FVclDialogFileName, '.dfm'), faArchive);
  try
    // Delphi 4
    Installation := FDelphiInstallations.InstallationFromVersion[4];
    if Assigned(Installation) and Installation.Valid then
    begin
      if D4InstallCheckBox.Checked then
      begin
        IniFile := TIniFile.Create(Installation.ObjectRepositoryFileName);
        IniFile.WriteString(FVclDialogFileName, DelphiRepositoryObjectType, DelphiRepositoryFormTemplate);
        IniFile.WriteString(FVclDialogFileName, DelphiRepositoryObjectName, VclDialogName);
        IniFile.WriteString(FVclDialogFileName, DelphiRepositoryObjectPage, D4ReposPageComboBox.Text);
        IniFile.WriteString(FVclDialogFileName, DelphiRepositoryObjectIcon, FVclDialogIconFileName);
        IniFile.WriteString(FVclDialogFileName, DelphiRepositoryObjectDescr, DialogDescription);
        IniFile.WriteString(FVclDialogFileName, DelphiRepositoryObjectAuthor, DialogAuthor);
        IniFile.WriteBool(FVclDialogFileName, DelphiRepositoryObjectNewForm, False);
        IniFile.WriteBool(FVclDialogFileName, DelphiRepositoryObjectMainForm, False);
        FreeAndNil(IniFile);
      end;
      if D4HlpHelpCheckBox.Checked then
        AddHelpToDelphiHelp;
      if D4ChmHelpCheckBox.Checked then
        AddHelpToIdeTools;
      if D4SourceLibCheckBox.Checked then
        AddLibraryPath;
    end;
    // Delphi 5
    Installation := FDelphiInstallations.InstallationFromVersion[5];
    if Assigned(Installation) and Installation.Valid then
    begin
      if D5InstallCheckBox.Checked then
      begin
        IniFile := TIniFile.Create(Installation.ObjectRepositoryFileName);
        IniFile.WriteString(FVclDialogFileName, DelphiRepositoryObjectType, DelphiRepositoryFormTemplate);
        IniFile.WriteString(FVclDialogFileName, DelphiRepositoryObjectName, VclDialogName);
        IniFile.WriteString(FVclDialogFileName, DelphiRepositoryObjectPage, D5ReposPageComboBox.Text);
        IniFile.WriteString(FVclDialogFileName, DelphiRepositoryObjectIcon, FVclDialogIconFileName);
        IniFile.WriteString(FVclDialogFileName, DelphiRepositoryObjectDescr, DialogDescription);
        IniFile.WriteString(FVclDialogFileName, DelphiRepositoryObjectAuthor, DialogAuthor);
        IniFile.WriteBool(FVclDialogFileName, DelphiRepositoryObjectNewForm, False);
        IniFile.WriteBool(FVclDialogFileName, DelphiRepositoryObjectMainForm, False);
        FreeAndNil(IniFile);
      end;
      if D5HlpHelpCheckBox.Checked then
        AddHelpToDelphiHelp;
      if D5ChmHelpCheckBox.Checked then
        AddHelpToIdeTools;
      if D5SourceLibCheckBox.Checked then
        AddLibraryPath;
    end;
    // Delphi 6
    Installation := FDelphiInstallations.InstallationFromVersion[6];
    if Assigned(Installation) and Installation.Valid then
    begin
      IniFile := TIniFile.Create(Installation.ObjectRepositoryFileName);
      if D6InstallVclCheckBox.Checked then
      begin
        IniFile.WriteString(FVclDialogFileName, DelphiRepositoryObjectType, DelphiRepositoryFormTemplate);
        IniFile.WriteString(FVclDialogFileName, DelphiRepositoryObjectName, VclDialogName);
        IniFile.WriteString(FVclDialogFileName, DelphiRepositoryObjectPage, D6ReposPageComboBox.Text);
        IniFile.WriteString(FVclDialogFileName, DelphiRepositoryObjectIcon, FVclDialogIconFileName);
        IniFile.WriteString(FVclDialogFileName, DelphiRepositoryObjectDescr, DialogDescription);
        IniFile.WriteString(FVclDialogFileName, DelphiRepositoryObjectAuthor, DialogAuthor);
        IniFile.WriteString(FVclDialogFileName, DelphiRepositoryObjectDesigner, DelphiRepositoryDesignerDfm);
        IniFile.WriteBool(FVclDialogFileName, DelphiRepositoryObjectNewForm, False);
        IniFile.WriteBool(FVclDialogFileName, DelphiRepositoryObjectMainForm, False);
      end;
      if D6InstallClxCheckBox.Checked then
      begin
        IniFile.WriteString(FClxDialogFileName, DelphiRepositoryObjectType, DelphiRepositoryFormTemplate);
        IniFile.WriteString(FClxDialogFileName, DelphiRepositoryObjectName, ClxDialogName);
        IniFile.WriteString(FClxDialogFileName, DelphiRepositoryObjectPage, D6ReposPageComboBox.Text);
        IniFile.WriteString(FClxDialogFileName, DelphiRepositoryObjectIcon, FClxDialogIconFileName);
        IniFile.WriteString(FClxDialogFileName, DelphiRepositoryObjectDescr, DialogDescription);
        IniFile.WriteString(FClxDialogFileName, DelphiRepositoryObjectAuthor, DialogAuthor);
        IniFile.WriteString(FClxDialogFileName, DelphiRepositoryObjectDesigner, DelphiRepositoryDesignerXfm);
        IniFile.WriteBool(FClxDialogFileName, DelphiRepositoryObjectNewForm, False);
        IniFile.WriteBool(FClxDialogFileName, DelphiRepositoryObjectMainForm, False);
      end;
      FreeAndNil(IniFile);
      if D6HlpHelpCheckBox.Checked then
        AddHelpToDelphiHelp;
      if D6ChmHelpCheckBox.Checked then
        AddHelpToIdeTools;
      if D6SourceLibCheckBox.Checked then
        AddLibraryPath;
    end;
    with Application do
      MessageBox(PChar(RsInstallSuccess), PChar(Title), MB_OK or MB_ICONINFORMATION);
    CloseBtn.SetFocus;
  finally
    IniFile.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDelphiInstallations := TJclDelphiInstallations.Create;
  JediImage.Hint := DelphiJediURL;
  ReadDialogPath;
  UpdateControls;
  UpdateButtons;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDelphiInstallations);
end;

procedure TMainForm.ReadDialogPath;
begin
  FJclPath := PathAddSeparator(PathCanonicalize(PathExtractFileDirFixed(Application.ExeName) + '..'));
  FJclSourcePath := FJclPath + 'Source';
  FClxDialogFileName := AnsiUpperCase(FJclPath + DialogsPath + ClxDialogFileName);
  FVclDialogFileName := AnsiUpperCase(FJclPath + DialogsPath + VclDialogFileName);
  FClxDialogIconFileName := ChangeFileExt(FClxDialogFileName, '.ICO');
  FVclDialogIconFileName := ChangeFileExt(FVclDialogFileName, '.ICO');
  FJclChmHelpFileName := FJclPath + JclChmHelpFile;
  FJclHlpHelpFileName := FJclPath + JclHlpHelpFile;
  if not FileExists(FJclChmHelpFileName) then
    FJclChmHelpFileName := '';
  if not FileExists(FJclHlpHelpFileName) then
    FJclHlpHelpFileName := '';
  if not (FileExists(FClxDialogFileName) and FileExists(FVclDialogFileName) and
    FileExists(FClxDialogIconFileName) and FileExists(FVclDialogIconFileName)) then
  begin
    with Application do
      MessageBox(PChar(RsCantFindFiles), PChar(Title), MB_OK or MB_ICONERROR);
    Application.ShowMainForm := False;  
    Application.Terminate;
  end;
end;

procedure TMainForm.UpdateButtons;
var
  I: Integer;
  InstallEnabled: Boolean;
begin
  InstallEnabled := False;
  for I := 0 to ComponentCount - 1 do
    if (Components[I] is TCheckBox) and TCheckBox(Components[I]).Checked then
      begin
        InstallEnabled := True;
        Break;
      end;
  InstallBtn.Enabled := InstallEnabled;
end;

procedure TMainForm.UpdateControls;

  procedure UpdateForVersion(VersionNumber: Byte; VersionGroupBox: TGroupBox; PagesCombo: TComboBox;
    ChmHelpCheckBox, HelpHelpCheckBox: TCheckBox);
  var
    Installation: TJclDelphiInstallation;
    IniFile: TIniFile;
    I: Integer;
  begin
    Installation := FDelphiInstallations.InstallationFromVersion[VersionNumber];
    if Installation = nil then
      DisableGroup(VersionGroupBox)
    else
    begin
      // Update Pack caption
      VersionGroupBox.Caption := Installation.Name;
      IniFile := TIniFile.Create(Installation.ObjectRepositoryFileName);
      try
        // Repository pages
        IniFile.ReadSection(DelphiRepositoryPagesSection, PagesCombo.Items);
        I := PagesCombo.Items.IndexOf(DelphiRepositoryDialogsPage);
        if (I = -1) and (PagesCombo.Items.Count >= 2) then
          I := 1;
        PagesCombo.ItemIndex := I;
        // JCL CHM Help
        if FJclHlpHelpFileName = '' then
          HelpHelpCheckBox.Enabled := False;
        if FJclChmHelpFileName = '' then
          ChmHelpCheckBox.Enabled := False;
        // D6 CLX Exception dialog on Delphi 6 Personal
        if (Installation.VersionNumber = 6) and (Installation.Edition = deSTD) then
          D6InstallClxCheckBox.Enabled := False;
      finally
        IniFile.Free;
      end;
    end;
  end;

begin
  UpdateForVersion(4, D4GroupBox, D4ReposPageComboBox, D4ChmHelpCheckBox, D4HlpHelpCheckBox);
  UpdateForVersion(5, D5GroupBox, D5ReposPageComboBox, D5ChmHelpCheckBox, D5HlpHelpCheckBox);
  UpdateForVersion(6, D6GroupBox, D6ReposPageComboBox, D6ChmHelpCheckBox, D6HlpHelpCheckBox);
  D4SourceLibCheckBox.Hint := Format(RsSourceLibHint, [FJclSourcePath]);
  D5SourceLibCheckBox.Hint := Format(RsSourceLibHint, [FJclSourcePath]);
  D6SourceLibCheckBox.Hint := Format(RsSourceLibHint, [FJclSourcePath]);
  CheckAllCheckBoxes(True);
end;

procedure TMainForm.UMCheckUpdates(var Message: TMessage);
begin
  CheckUpdatePacks;
  Message.Result := 0;
end;

procedure TMainForm.CloseBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.D4InstallCheckBoxClick(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TMainForm.InstallBtnClick(Sender: TObject);
begin
  if not CheckRunningInstances then
    Install;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  PostMessage(Handle, UM_CHECKUPDATES, 0, 0);
end;

procedure TMainForm.JediImageClick(Sender: TObject);
begin
  ShellExecEx(DelphiJediURL);
end;

procedure TMainForm.Checkall1Click(Sender: TObject);
begin
  CheckAllCheckBoxes(Boolean((Sender as TMenuItem).Tag));
end;

end.
