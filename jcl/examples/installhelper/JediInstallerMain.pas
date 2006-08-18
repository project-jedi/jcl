unit JediInstallerMain;

{$I jcl.inc}

{$IFDEF COMPILER6_UP}
  {$WARN UNIT_PLATFORM OFF}  
{$ENDIF COMPILER6_UP}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DelphiInstall, Menus, ComCtrls, ImgList, JediInstallIntf;

const
  UM_CHECKUPDATES = WM_USER + $100;

type
  TMainForm = class(TForm, IJediInstallTool)
    InstallBtn: TButton;
    CloseBtn: TButton;
    JediImage: TImage;
    TitlePanel: TPanel;
    Label4: TLabel;
    ProductsPageControl: TPageControl;
    D4TabSheet: TTabSheet;
    D5TabSheet: TTabSheet;
    D6TabSheet: TTabSheet;
    ImageList: TImageList;
    StatusBevel: TBevel;
    StatusLabel: TLabel;
    Bevel1: TBevel;
    D4ComponentsTreePanel: TPanel;
    Label1: TLabel;
    D4TreeView: TTreeView;
    D5ComponentsTreePanel: TPanel;
    Label2: TLabel;
    D5TreeView: TTreeView;
    D6ComponentsTreePanel: TPanel;
    Label3: TLabel;
    D6TreeView: TTreeView;
    D4Splitter: TSplitter;
    D5Splitter: TSplitter;
    D6Splitter: TSplitter;
    D4InfoPanel: TPanel;
    D5InfoPanel: TPanel;
    D6InfoPanel: TPanel;
    D4InfoMemo: TRichEdit;
    Label5: TLabel;
    Label6: TLabel;
    D5InfoMemo: TRichEdit;
    Label7: TLabel;
    D6InfoMemo: TRichEdit;
    D7TabSheet: TTabSheet;
    D7ComponentsTreePanel: TPanel;
    Label8: TLabel;
    D7TreeView: TTreeView;
    D7InfoPanel: TPanel;
    Label9: TLabel;
    D7InfoMemo: TRichEdit;
    Splitter1: TSplitter;
    D4OptionsGroupBox: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    D4BplPathEdit: TEdit;
    D4DcpPathEdit: TEdit;
    D4BplPathSelectBtn: TButton;
    D4DcpPathSelectBtn: TButton;
    D5OptionsGroupBox: TGroupBox;
    Label12: TLabel;
    Label13: TLabel;
    D5BplPathEdit: TEdit;
    D5DcpPathEdit: TEdit;
    Button1: TButton;
    Button2: TButton;
    D6OptionsGroupBox: TGroupBox;
    Label14: TLabel;
    Label15: TLabel;
    D6BplPathEdit: TEdit;
    D6DcpPathEdit: TEdit;
    Button3: TButton;
    Button4: TButton;
    D7OptionsGroupBox: TGroupBox;
    Label16: TLabel;
    Label17: TLabel;
    D7BplPathEdit: TEdit;
    D7DcpPathEdit: TEdit;
    Button5: TButton;
    Button6: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure InstallBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure JediImageClick(Sender: TObject);
    procedure D6TreeViewCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure D6TreeViewKeyPress(Sender: TObject; var Key: Char);
    procedure D6TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure D6TreeViewCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure D6TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure D6SplitterCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure D4BplPathSelectBtnClick(Sender: TObject);
    procedure D4BplPathEditChange(Sender: TObject);
    procedure D4DcpPathEditChange(Sender: TObject);
  private
    FDelphiInstallations: TJclDelphiInstallations;
    FJediInstall: IJediInstall;
    FSystemPaths: TStringList;
    procedure UMCheckUpdates(var Message: TMessage); message UM_CHECKUPDATES;
    function GetNodeChecked(Node: TTreeNode): Boolean;
    procedure SetNodeChecked(Node: TTreeNode; const Value: Boolean);
    procedure ReadSystemPaths;
  public
    function CheckRunningInstances: Boolean;
    procedure CheckUpdatePacks;
    procedure Install;
    procedure PopulatePaths;
    function PopulateTreeViews: Boolean;
    function SystemPathValid(const Path: string): Boolean;
    procedure ToggleNodeChecked(Node: TTreeNode);
    procedure UpdateButtons;
    property NodeChecked[Node: TTreeNode]: Boolean read GetNodeChecked write SetNodeChecked;
    // IJediInstallTool
    function ActiveVersionNumberPage: Integer;
    function BPLPath(VersionNumber: Integer): string;
    function DCPPath(VersionNumber: Integer): string;
    function FeatureChecked(FeatureID: Cardinal; VersionNumber: Integer): Boolean;
    function GetDelphiInstallations: TJclDelphiInstallations;
    function MessageBox(const Text: string; Flags: Integer): Integer;
    procedure UpdateInfo(VersionNumber: Integer; const InfoText: String);
    procedure UpdateStatus(const Text: string);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  FileCtrl,
  {$IFDEF JCLINSTALL}
  JclInstall,
  {$ENDIF JCLINSTALL}
  {$IFDEF JEDIINSTALL}
  JediPackInstall,
  {$ENDIF JEDIINSTALL}
  JclBase, JclDebug, JclFileUtils, JclStrings, JclSysInfo, JclSysUtils, JclShell;

const
  DelphiSupportURL  = 'http://www.borland.com/devsupport/delphi/';
  DelphiJediURL     = 'http://delphi-jedi.org';

resourcestring
  RsCantFindFiles   = 'Can not find installation files, check your installation.';
  RsCloseDelphi     = 'Please close all running instances of Delphi IDE before the installation.';
  RsConfirmInstall  = 'Are you sure to install all selected features ?';
  RsEnterValidPath  = '(Enter valid path)';
  RsInstallSuccess  = 'Installation finished';
  RsNoInstall       = 'There is no Delphi installation on this machine. Installer will close.';
  RsUpdateNeeded    = '. Would you like to open Borland Delphi support web page ?';
  RsSelectPath      = 'Select path';

{ TMainForm }

function TMainForm.CheckRunningInstances: Boolean;
begin
  Result := FDelphiInstallations.AnyInstanceRunning;
  if Result then
    MessageBox(RsCloseDelphi, MB_OK or MB_ICONWARNING);
end;

procedure TMainForm.CheckUpdatePacks;
var
  UpdateText: string;
begin
  if FDelphiInstallations.AnyUpdatePackNeeded(UpdateText) then
  begin
    UpdateText := UpdateText + RsUpdateNeeded;
    if MessageBox(UpdateText, MB_YESNO or MB_DEFBUTTON1 or MB_ICONEXCLAMATION) = ID_YES then
      ShellExecEx(DelphiSupportURL);
  end;
end;

procedure TMainForm.Install;
var
  Res: Boolean;
begin
  Screen.Cursor := crHourGlass;
  try
    Res := FJediInstall.Install;
    Screen.Cursor := crDefault;
    if Res then
      MessageBox(RsInstallSuccess, MB_OK or MB_ICONINFORMATION);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.PopulatePaths;
var
  Edit: TEdit;
  I: Integer;
  Page: TTabSheet;
  Installation: TJclDelphiInstallation;

  function GetPathForEdit(const Path: string): string;
  begin
    if DirectoryExists(Path) then
      Result := Path
    else
      Result := RsEnterValidPath;
  end;

begin
  for I := 0 to ProductsPageControl.PageCount - 1 do
  begin
    Page := ProductsPageControl.Pages[I];
    Installation := FDelphiInstallations.InstallationFromVersion[Page.Tag];
    if Assigned(Installation) then
    begin
      Edit := FindComponent(Format('D%dBplPathEdit', [Page.Tag])) as TEdit;
      Edit.Text := GetPathForEdit(Installation.BPLOutputPath);
      Edit := FindComponent(Format('D%dDcpPathEdit', [Page.Tag])) as TEdit;
      Edit.Text := GetPathForEdit(Installation.DCPOutputPath);
    end;
  end;
end;

function TMainForm.PopulateTreeViews: Boolean;
var
  I: Integer;
  Page, ActivePage: TTabSheet;
  TreeView: TTreeView;
begin
  Result := False;
  ActivePage := nil;
  for I := 0 to ProductsPageControl.PageCount - 1 do
  begin
    Page := ProductsPageControl.Pages[I];
    TreeView := FindComponent(Format('D%dTreeView', [Page.Tag])) as TTreeView;
    if FJediInstall.PopulateTreeView(TreeView.Items, Page.Tag, Page) then
    begin
      Result := True;
      ActivePage := Page;
    end
    else
      Page.TabVisible := False;
  end;
  ProductsPageControl.ActivePage := ActivePage;
end;

procedure TMainForm.ReadSystemPaths;
var
  PathVar: string;
  I: Integer;
begin
  if GetEnvironmentVar('PATH', PathVar, False) then
  begin
    StrToStrings(PathVar, ';', FSystemPaths, False);
    for I := 0 to FSystemPaths.Count - 1 do
    begin
      PathVar := FSystemPaths[I];
      ExpandEnvironmentVar(PathVar);
      PathVar := AnsiUpperCase(PathRemoveSeparator(PathGetLongName2(PathVar)));
      FSystemPaths[I] := PathVar;
    end;
    FSystemPaths.Sorted := True;
  end;
end;

function TMainForm.SystemPathValid(const Path: string): Boolean;
begin
  Result := FSystemPaths.IndexOf(AnsiUpperCase(Path)) <> -1;
end;

procedure TMainForm.UpdateButtons;
begin
end;

procedure TMainForm.UpdateInfo(VersionNumber: Integer; const InfoText: String);
var
  InfoMemo: TRichEdit;
begin
  InfoMemo := FindComponent(Format('D%dInfoMemo', [VersionNumber])) as TRichEdit;
  if Assigned(InfoMemo) then
    InfoMemo.Text := InfoText;
end;

procedure TMainForm.UpdateStatus(const Text: string);
begin
  if Text = '' then
  begin
    StatusBevel.Visible := False;
    StatusLabel.Visible := False;
  end
  else
  begin
    StatusLabel.Caption := Text;
    StatusBevel.Visible := True;
    StatusLabel.Visible := True;
  end;
  Update;
end;

function TMainForm.GetNodeChecked(Node: TTreeNode): Boolean;
begin
  Result := Cardinal(Node.Data) and FID_Checked <> 0;
end;

procedure TMainForm.SetNodeChecked(Node: TTreeNode; const Value: Boolean);

  procedure InvalidateNodeRect(Node: TTreeNode);
  var
    FullRect, TextRect: TRect;
  begin
    FullRect := Node.DisplayRect(False);
    TextRect := Node.DisplayRect(True);
    FullRect.Right := TextRect.Left;
    InvalidateRect(Node.GetHandle, @FullRect, True);
  end;

  procedure UpdateNode(N: TTreeNode; C: Boolean);
  const
    CheckedState: array[Boolean] of Cardinal = (0, FID_Checked);
  begin
    N.Data := Pointer(Cardinal(N.Data) and (not FID_Checked) or CheckedState[C]);
    if C then
    begin
      N.ImageIndex := IcoChecked;
      N.SelectedIndex := IcoChecked;
    end
    else
    begin
      N.ImageIndex := IcoUnchecked;
      N.SelectedIndex := IcoUnchecked;
    end;
    {$IFDEF DELPHI4}
    InvalidateNodeRect(N);
    {$ENDIF DELPHI4}
  end;

  procedure UpdateTreeDown(N: TTreeNode; C: Boolean);
  var
    TempNode: TTreeNode;
  begin
    TempNode := N.getFirstChild;
    while Assigned(TempNode) do
    begin
      UpdateNode(TempNode, C);
      UpdateTreeDown(TempNode, C);
      TempNode := TempNode.getNextSibling;
    end;
  end;

  procedure UpdateTreeUp(N: TTreeNode; C: Boolean);
  var
    TempNode, SiblingNode: TTreeNode;
    AnyChecked: Boolean;
  begin
    TempNode := N;
    if C then
    while Assigned(TempNode) do
    begin
      UpdateNode(TempNode, True);
      TempNode := TempNode.Parent;
    end
    else
    while True do
    begin
      SiblingNode := TempNode;
      if SiblingNode.Parent <> nil then
        SiblingNode := TempNode.Parent.getFirstChild;
      AnyChecked := False;
      while Assigned(SiblingNode) do
        if NodeChecked[SiblingNode] then
        begin
          AnyChecked := True;
          Break;
        end
        else
          SiblingNode := SiblingNode.getNextSibling;
      TempNode := TempNode.Parent;
      if Assigned(TempNode) then
        UpdateNode(TempNode, AnyChecked)
      else
        Break;
    end;
  end;

begin
  UpdateNode(Node, Value);
  UpdateTreeDown(Node, Value);
  UpdateTreeUp(Node, Value);
end;

function TMainForm.ActiveVersionNumberPage: Integer;
var
  Page: TTabSheet;
begin
  Page := ProductsPageControl.ActivePage;
  if Assigned(Page) then
    Result := Page.Tag
  else
    Result := 0;
end;

function TMainForm.BPLPath(VersionNumber: Integer): string;
var
  Path: string;
begin
  Path := (FindComponent(Format('D%dBplPathEdit', [VersionNumber])) as TEdit).Text;
  Result := PathRemoveSeparator(FDelphiInstallations.InstallationFromVersion[VersionNumber].SubstitutePath(Path));
end;

function TMainForm.DCPPath(VersionNumber: Integer): string;
var
  Path: string;
begin
  Path := (FindComponent(Format('D%dDcpPathEdit', [VersionNumber])) as TEdit).Text;
  Result := PathRemoveSeparator(FDelphiInstallations.InstallationFromVersion[VersionNumber].SubstitutePath(Path));
end;

function TMainForm.FeatureChecked(FeatureID: Cardinal; VersionNumber: Integer): Boolean;
var
  I: Integer;
  TreeView: TTreeView;
  F: Cardinal;
begin
  TreeView := FindComponent(Format('D%dTreeView', [VersionNumber])) as TTreeView;
  Result := Assigned(TreeView);
  if Result then
  begin
    Result := False;
    for I := 0 to TreeView.Items.Count - 1 do
    begin
      F := Cardinal(TreeView.Items[I].Data);
      if F and FID_NumberMask = FeatureID then
      begin
        Result := F and FID_Checked <> 0;
        Break;
      end;
    end;
  end;
end;

procedure TMainForm.ToggleNodeChecked(Node: TTreeNode);
begin
  if Assigned(Node) then
    NodeChecked[Node] := not NodeChecked[Node];
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDelphiInstallations := TJclDelphiInstallations.Create;
  FSystemPaths := TStringList.Create;
  JediImage.Hint := DelphiJediURL;
  FJediInstall := CreateJediInstall;
  FJediInstall.SetTool(Self);
  UpdateStatus('');
  if not FJediInstall.InitInformation(Application.ExeName) then
  begin
    MessageBox(RsCantFindFiles, MB_OK or MB_ICONERROR);
    Application.ShowMainForm := False;
    Application.Terminate;
  end
  else
  if not PopulateTreeViews then
  begin
    MessageBox(RsNoInstall, MB_OK or MB_ICONINFORMATION);
    Application.ShowMainForm := False;
    Application.Terminate;
  end;
  ReadSystemPaths;
  PopulatePaths;
  TitlePanel.DoubleBuffered := True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDelphiInstallations);
  FreeAndNil(FSystemPaths);
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

procedure TMainForm.InstallBtnClick(Sender: TObject);
begin
  if (IsDebuggerAttached or not CheckRunningInstances) and
    (MessageBox(RsConfirmInstall, MB_ICONQUESTION or MB_OK or MB_YESNO or MB_DEFBUTTON2) = ID_YES) then
  begin
    Install;
    CloseBtn.SetFocus;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  PostMessage(Handle, UM_CHECKUPDATES, 0, 0);
end;

procedure TMainForm.JediImageClick(Sender: TObject);
begin
  ShellExecEx(DelphiJediURL);
end;

procedure TMainForm.D6TreeViewCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  case Node.Level of
    0: begin
         Sender.Canvas.Font.Style := [fsBold, fsUnderline];
       end;
    1: begin
         Sender.Canvas.Font.Style := [fsBold];
       end;
  end;
end;

procedure TMainForm.D6TreeViewKeyPress(Sender: TObject; var Key: Char);
begin
  with TTreeView(Sender) do
    if (Key = #32) then
    begin
      ToggleNodeChecked(Selected);
      Key := #0;
    end;
end;

procedure TMainForm.D6TreeViewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with TTreeView(Sender) do
    if (Button = mbLeft) and (htOnIcon in GetHitTestInfoAt(X, Y)) then
      ToggleNodeChecked(GetNodeAt(X, Y))
end;

procedure TMainForm.D6TreeViewCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  AllowCollapse := FJediInstall.SelectedNodeCollapsing(Node);
end;

function TMainForm.GetDelphiInstallations: TJclDelphiInstallations;
begin
  Result := FDelphiInstallations;
end;

function TMainForm.MessageBox(const Text: string; Flags: Integer): Integer;
begin
  with Application do
    Result := MessageBox(PChar(Text), PChar(Title), Flags);
end;

procedure TMainForm.D6TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  FJediInstall.SelectedNodeChanged(Node);
end;

procedure TMainForm.D6SplitterCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  Accept := NewSize > 150;
end;

procedure TMainForm.D4BplPathSelectBtnClick(Sender: TObject);
var
  I: Integer;
  Button: TButton;
  Edit: TEdit;
  Directory: string;
begin
  Button := Sender as TButton;
  Edit := nil;
  with Button.Parent do
    for I := 0 to ControlCount - 1 do
      if (Controls[I].Top = Button.Top) and (Controls[I] is TEdit) then
        Edit := TEdit(Controls[I]);
  if Assigned(Edit) and SelectDirectory(RsSelectPath, '', Directory) then
    Edit.Text := Directory;
end;

procedure TMainForm.D4BplPathEditChange(Sender: TObject);
begin
  with (Sender as TEdit) do
    if SystemPathValid(Text) then
      Font.Color := clWindowText
    else
      Font.Color := clRed;
end;

procedure TMainForm.D4DcpPathEditChange(Sender: TObject);
begin
  with (Sender as TEdit) do
    if DirectoryExists(Text) then
      Font.Color := clWindowText
    else
      Font.Color := clRed;
end;

end.
