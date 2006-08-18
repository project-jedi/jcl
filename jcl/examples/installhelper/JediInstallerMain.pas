unit JediInstallerMain;

{$I jcl.inc}

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
    Panel1: TPanel;
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
  private
    FDelphiInstallations: TJclDelphiInstallations;
    FJediInstall: IJediInstall;
    procedure UMCheckUpdates(var Message: TMessage); message UM_CHECKUPDATES;
    function GetNodeChecked(Node: TTreeNode): Boolean;
    procedure SetNodeChecked(Node: TTreeNode; const Value: Boolean);
  public
    function CheckRunningInstances: Boolean;
    procedure CheckUpdatePacks;
    procedure Install;
    function PopulateTreeViews: Boolean;
    procedure ToggleNodeChecked(Node: TTreeNode);
    procedure UpdateButtons;
    property NodeChecked[Node: TTreeNode]: Boolean read GetNodeChecked write SetNodeChecked;
    // IJediInstallTool
    function ActiveVersionNumberPage: Integer;
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
  {$IFDEF JCLINSTALL}
  JclInstall,
  {$ENDIF JCLINSTALL}
  {$IFDEF JEDIINSTALL}
  JediPackInstall,
  {$ENDIF JEDIINSTALL}
  JclBase, JclFileUtils, JclStrings, JclSysUtils, JclShell;

const
  DelphiSupportURL  = 'http://www.borland.com/devsupport/delphi/';
  DelphiJediURL     = 'http://delphi-jedi.org';

resourcestring
  RsCantFindFiles   = 'Can not find installation files, check your installation.';
  RsCloseDelphi     = 'Please close all running instances of Delphi IDE before the installation.';
  RsConfirmInstall  = 'Are you sure to install all selected features ?';
  RsInstallSuccess  = 'Installation finished';
  RsNoInstall       = 'There is no Delphi installation on this machine. Installer will close.';
  RsUpdateNeeded    = '. Would you like to open Borland Delphi support web page ?';

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

function TMainForm.PopulateTreeViews: Boolean;
var
  I: Integer;
  Page: TTabSheet;
  TreeView: TTreeView;
begin
  Result := False;
  for I := 0 to ProductsPageControl.PageCount - 1 do
  begin
    Page := ProductsPageControl.Pages[I];
    TreeView := FindComponent(Format('D%dTreeView', [Page.Tag])) as TTreeView;
    if FJediInstall.PopulateTreeView(TreeView.Items, Page.Tag, Page) then
      Result := True
    else
      Page.TabVisible := False;  
  end;
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
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDelphiInstallations);
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
  if not CheckRunningInstances and
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

end.
