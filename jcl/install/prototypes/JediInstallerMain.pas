{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL) extension                                                        }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JediInstallerMain.pas.                                                      }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributor(s): Robert Rossmair (crossplatform & BCB support, refactoring)                       }
{                                                                                                  }
{**************************************************************************************************}

// $Id$

{$IFNDEF Develop}unit {$IFDEF VisualCLX}QJediInstallerMain{$ELSE}JediInstallerMain{$ENDIF};{$ENDIF}

{$I jcl.inc}
{$I crossplatform.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, Messages,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes,
  {$IFDEF VisualCLX}
  Types,
  Qt, QGraphics, QControls, QForms, QDialogs, QStdCtrls, QExtCtrls, QMenus, QButtons, QComCtrls, QImgList,
  QProductFrames,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Menus, Buttons, ComCtrls, ImgList,
  ProductFrames,
  {$ENDIF}
  JclBorlandTools, JediInstall;

const
  {$IFDEF VisualCLX}
  QEventType_UMCheckUpdates = QEventType(Integer(QEventType_ClxUser) + $100);
  {$ELSE}
  UM_CHECKUPDATES = WM_USER + $100;
  {$ENDIF}

type
  TMainForm = class(TForm, IJediInstallTool)
    InstallBtn: TBitBtn;
    UninstallBtn: TBitBtn;
    QuitBtn: TBitBtn;
    JediImage: TImage;
    TitlePanel: TPanel;
    Title: TLabel;
    ProductsPageControl: TPageControl;
    StatusBevel: TBevel;
    StatusLabel: TLabel;
    Bevel1: TBevel;
    ProgressBar: TProgressBar;
    ImageList: TImageList;
    ReadmePage: TTabSheet;
    {$IFDEF VCL}
    ReadmePane: TRichEdit;
    {$ELSE ~VCL}
    ReadmePane: TTextViewer;
    {$ENDIF ~VCL}
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure QuitBtnClick(Sender: TObject);
    procedure InstallBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure JediImageClick(Sender: TObject);
    procedure TreeViewCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure BplPathEditChange(Sender: TObject);
  private
    FBorRADToolInstallations: TJclBorRADToolInstallations;
    FJclInstall: IJediInstall;
    FSystemPaths: TStringList;
    FFeatureNode: TTreeNode;
    FFeatureInfo: TStringList;
    FFeatureInfoChanged: Boolean;
    function ActiveView: TProductFrame;
    function CheckUpdatePack(Installation: TJclBorRADToolInstallation): Boolean;
    function CreateView(Installation: TJclBorRADToolInstallation): Boolean;
    function ExpandOptionTree(Installation: TJclBorRADToolInstallation): Boolean;
    function InfoFile(Node: TTreeNode): string;
    procedure InstallationStarted(Installation: TJclBorRADToolInstallation);
    procedure InstallationFinished(Installation: TJclBorRADToolInstallation);
    procedure InstallationProgress(Percent: Cardinal);
    procedure ReadSystemPaths;
    function View(Installation: TJclBorRADToolInstallation): TProductFrame;
    {$IFDEF VCL}
    procedure UMCheckUpdates(var Message: TMessage); message UM_CHECKUPDATES;
    {$ENDIF VCL}
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure TreeViewEnter(Sender: TObject);
    procedure TreeViewExit(Sender: TObject);
    procedure TreeViewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure UpdateFeatureInfo(Node: TTreeNode);
  protected
    {$IFDEF VisualCLX}
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
    {$ENDIF VisualCLX}
    function OptionGUI(Installation: TJclBorRADToolInstallation): TObject;
    function GUIAddOption(GUI, Parent: TObject; Option: TJediInstallOption; const Text: string;
      StandAlone: Boolean = False; Checked: Boolean = True): TObject;
    property JclDistribution: IJediInstall read FJclInstall;
  public
    procedure ShowFeatureHint(var HintStr: {$IFDEF VisualCLX}WideString{$ELSE}string{$ENDIF};
      var CanShow: Boolean; var HintInfo: THintInfo);
    function CheckRunningInstances: Boolean;
    procedure Install;
    function SystemPathValid(const Path: string): Boolean;
    // IJediInstallTool
    function BPLPath(Installation: TJclBorRADToolInstallation): string;
    function DCPPath(Installation: TJclBorRADToolInstallation): string;
    function FeatureChecked(FeatureID: Cardinal; Installation: TJclBorRADToolInstallation): Boolean;
    function GetBorRADToolInstallations: TJclBorRADToolInstallations;
    function Dialog(const Text: string; DialogType: TDialogType = dtInformation;
      Options: TDialogResponses = [drOK]): TDialogResponse;
    procedure SetReadme(const FileName: string);
    procedure UpdateInfo(Installation: TJclBorRADToolInstallation; const InfoText: String);
    procedure UpdateStatus(const Text: string);
    procedure WriteInstallLog(Installation: TJclBorRADToolInstallation; const Text: string);
    property BorRADToolInstallations: TJclBorRADToolInstallations read FBorRADToolInstallations;
  end;

var
  MainForm: TMainForm;

implementation

{$IFDEF VCL}
{$R *.dfm}
{$ELSE VisualCLX}
{$R *.xfm}
{$ENDIF VisualCLX}

uses
  {$IFDEF UNIX}
  Libc,
  {$ENDIF UNIX}
  {$IFDEF MSWINDOWS}
  FileCtrl,
  JclDebug, JclShell,
  {$ENDIF MSWINDOWS}
  JclBase, JclFileUtils, JclStrings, JclSysInfo, JclSysUtils,
  JclInstall;

const
  {$IFNDEF COMPILER6_UP}
  PathSep = ';';
  {$ENDIF COMPILER6_UP}
  {$IFDEF MSWINDOWS}
  SupportURLs: array[TJclBorRADToolKind] of string = (
                'http://www.borland.com/devsupport/delphi/',
                'http://www.borland.com/devsupport/bcppbuilder/');
  {$ENDIF MSWINDOWS}
  {$IFDEF KYLIX}
  KylixSupportURL     = 'http://www.borland.com/devsupport/kylix/';
  {$ENDIF KYLIX}
  DelphiJediURL     = 'http://delphi-jedi.org';
  VersionSignature  = 'D%d';
  BCBTag            = $10000;
  VersionMask       = $FFFF;

resourcestring
  RsCantFindFiles   = 'Can not find installation files, check your installation.';
  RsCloseRADTool    = 'Please close all running instances of Delphi/C++Builder IDE before the installation.';
  RsConfirmInstall  = 'Are you sure to install all selected features?';
  RsInstallSuccess  = 'Installation finished';
  RsInstallFailure  = 'Installation failed.'#10'Check compiler output for details.';
  RsNoInstall       = 'There is no Delphi/C++Builder installation on this machine. Installer will close.';
  RsUpdateNeeded    = 'You should install latest Update Pack #%d for %s.'#13#10 +
                      'Would you like to open Borland support web page?';

function Collapsable(Node: TTreeNode): Boolean;
begin
  Result := (Cardinal(Node.Data) and FID_Expandable) <> 0;
end;

function FeatureID(Node: TTreeNode): Cardinal;
begin
  Result := Cardinal(Node.Data) and FID_NumberMask;
end;

{ TMainForm }

function TMainForm.ActiveView: TProductFrame;
var
  Page: TTabSheet;
  Control: TControl;
begin
  Result := nil;
  Page := ProductsPageControl.ActivePage;
  Control := Page.Controls[0];
  if Control is TProductFrame then
    Result := TProductFrame(Control);
end;

function TMainForm.InfoFile(Node: TTreeNode): string;
begin
  if Assigned(Node) then
    Result := FJclInstall.FeatureInfoFileName(FeatureID(Node));
end;

function TMainForm.CreateView(Installation: TJclBorRADToolInstallation): Boolean;
var
  Page: TTabSheet;
  ProductFrame: TProductFrame;
begin
  Page := TTabSheet.Create(Self);
  with Installation do
  begin
    Page.Name := Format('%s%dPage', [Prefixes[RADToolKind], VersionNumber]);
    Page.Caption := Name;
  end;
  Page.PageControl := ProductsPageControl;
  ProductFrame := TProductFrame.Create(Self);
  ProductFrame.Installation := Installation;
  ProductFrame.TreeView.Images := ImageList;
  ProductFrame.TreeView.OnChanging := TreeViewChanging;
  ProductFrame.TreeView.OnChange := TreeViewChange;
  ProductFrame.TreeView.OnCollapsing := TreeViewCollapsing;
  ProductFrame.TreeView.OnEnter := TreeViewEnter;
  ProductFrame.TreeView.OnExit := TreeViewExit;
  ProductFrame.TreeView.OnMouseMove := TreeViewMouseMove;
  ProductFrame.Align := alClient;
  ProductFrame.Parent := Page;
  Result := True;
end;

function TMainForm.CheckRunningInstances: Boolean;
begin
  Result := FBorRADToolInstallations.AnyInstanceRunning;
  if Result then
    Dialog(RsCloseRADTool, dtWarning);
end;

function TMainForm.CheckUpdatePack(Installation: TJclBorRADToolInstallation): Boolean;
var
  Msg: string;
begin
  Result := True;
  with Installation do
    if UpdateNeeded then
    begin
      Msg := Format(RsUpdateNeeded, [LatestUpdatePack, Name]);
      if Dialog(Msg, dtWarning, [drYes, drNo]) = drYes then
      {$IFDEF MSWINDOWS}
        ShellExecEx(SupportURLs[RadToolKind]);
      {$ENDIF MSWINDOWS}
      {$IFDEF UNIX}
      { TODO : Analoguous function for Linux };
      // Exec(KylixSupportURL);
      {$ENDIF UNIX}
    end;
end;

function TMainForm.GUIAddOption(GUI, Parent: TObject; Option: TJediInstallOption;
  const Text: string; StandAlone: Boolean = False; Checked: Boolean = True): TObject;
const
  Icon: array[Boolean] of Integer = (IcoUnchecked, IcoChecked);
  Flag: array[Boolean] of Cardinal = (0, FID_Checked);
var
  FeatureID: Cardinal;
  Nodes: TTreeNodes;
  Node, ParentNode: TTreeNode;
begin
  ParentNode := TTreeNode(Parent);
  if Assigned(Parent) then
  begin
    Assert(Parent is TTreeNode);
    if Checked and Assigned(Parent) then
      Checked := ParentNode.ImageIndex = IcoChecked;
  end;
  FeatureID := Cardinal(Ord(Option)) + Flag[Checked];
  if StandAlone then
    FeatureID := FeatureID + FID_StandAloneParent;
  Nodes := TTreeNodes(GUI);
  if Parent = nil then
    Node := Nodes.AddObject(nil, Text, Pointer(FeatureID))
  else
    Node := Nodes.AddChildObject(ParentNode, Text, Pointer(FeatureID));
  Node.ImageIndex := Icon[Checked];
  Node.SelectedIndex := Icon[Checked];
  Result := Node;
end;

procedure TMainForm.Install;
var
  Res: Boolean;
begin
  ProgressBar.Position := 0;
  ProgressBar.Visible := True;
  Screen.Cursor := crHourGlass;
  try
    Res := FJclInstall.Install;
    Screen.Cursor := crDefault;
    if Res then
      Dialog(RsInstallSuccess)
    else
      Dialog(RsInstallFailure);
  finally
    ProgressBar.Visible := False;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.InstallationProgress(Percent: Cardinal);
begin
  ProgressBar.Position := Percent;
end;

function TMainForm.View(Installation: TJclBorRADToolInstallation): TProductFrame;
begin
  if not Assigned(Installation) then
    Result := nil
  else
    with Installation do
      Result := FindComponent(TProductFrame.GetName(Installation)) as TProductFrame;
end;

procedure TMainForm.ReadSystemPaths;
var
  PathVar: string;
  I: Integer;
begin
  if GetEnvironmentVar('PATH', PathVar, False) then
  begin
    StrToStrings(PathVar, PathSep, FSystemPaths, False);
    for I := 0 to FSystemPaths.Count - 1 do
    begin
      PathVar := StrTrimQuotes(FSystemPaths[I]);
      ExpandEnvironmentVar(PathVar);
      {$IFDEF MSWINDOWS}
      PathVar := AnsiUpperCase(PathRemoveSeparator(PathGetLongName(PathVar)));
      {$ENDIF MSWINDOWS}
      FSystemPaths[I] := PathVar;
    end;
    FSystemPaths.Sorted := True;
  end;
end;

function TMainForm.SystemPathValid(const Path: string): Boolean;
begin
  Result := FSystemPaths.IndexOf({$IFDEF MSWINDOWS}AnsiUpperCase{$ENDIF}(Path)) <> -1;
end;

procedure TMainForm.UpdateInfo(Installation: TJclBorRADToolInstallation; const InfoText: String);
var
  P: TProductFrame;
begin
  P := View(Installation);
  if Assigned(P) then
  begin
    P.InfoDisplay.Text := InfoText;
  end;
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
  Application.ProcessMessages;  //Update;
end;

procedure TMainForm.WriteInstallLog(Installation: TJclBorRADToolInstallation; const Text: string);
var
  P: TProductFrame;
begin
  P := View(Installation);
  if Assigned(P) then
    P.LogOutputLine(Text);
end;

function TMainForm.BPLPath(Installation: TJclBorRADToolInstallation): string;
var
  Path: string;
begin
  with Installation do
    Path := View(Installation).BplPathEdit.Text;
  Result := PathRemoveSeparator(Installation.SubstitutePath(Path));
end;

function TMainForm.DCPPath(Installation: TJclBorRADToolInstallation): string;
var
  Path: string;
begin
  with Installation do
    Path := View(Installation).DcpPathEdit.Text;
  Result := PathRemoveSeparator(Installation.SubstitutePath(Path));
end;

procedure TMainForm.BplPathEditChange(Sender: TObject);
begin
  with (Sender as TEdit) do
    if SystemPathValid(Text) then
      Font.Color := clWindowText
    else
      Font.Color := clRed;
end;

function TMainForm.FeatureChecked(FeatureID: Cardinal; Installation: TJclBorRADToolInstallation): Boolean;
begin
  Result := View(Installation).FeatureChecked(FeatureID);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FBorRADToolInstallations := TJclBorRADToolInstallations.Create;
  FSystemPaths := TStringList.Create;
  FFeatureInfo := TStringList.Create;
  JediImage.Hint := DelphiJediURL;
  FJclInstall := CreateJclInstall;
  FJclInstall.SetOnProgress(InstallationProgress);
  FJclInstall.SetOnStarting(InstallationStarted);
  FJclInstall.SetOnEnding(InstallationFinished);
  FJclInstall.SetTool(Self);
  BorRADToolInstallations.Iterate(ExpandOptionTree);

  UpdateStatus('');
  {
  if False then
  begin
    Dialog(RsCantFindFiles, dtError);
    Application.ShowMainForm := False;
    Application.Terminate;
  end
  else
  if False then
  begin
    Dialog(RsNoInstall);
    Application.ShowMainForm := False;
    Application.Terminate;
  end;
  }
  ReadSystemPaths;
  {$IFDEF VCL}
  TitlePanel.DoubleBuffered := True;
  {$ELSE}
  //WindowState := wsMaximized; // wouldn't work in Form resource
  {$ENDIF}
  Application.HintPause := 50;
  Application.OnShowHint := ShowFeatureHint;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBorRADToolInstallations);
  FreeAndNil(FFeatureInfo);
  FreeAndNil(FSystemPaths);
end;

{$IFDEF VisualCLX}
function TMainForm.EventFilter(Sender: QObjectH; Event: QEventH): Boolean;
begin
  if QEvent_type(Event) = QEventType_UMCheckUpdates then
  begin
    BorRADToolInstallations.Iterate(CheckUpdatePack);
    Result := True;
  end
  else
    Result := inherited EventFilter(Sender, Event);
end;
{$ENDIF VisualCLX}

{$IFDEF VCL}
procedure TMainForm.UMCheckUpdates(var Message: TMessage);
begin
  BorRADToolInstallations.Iterate(CheckUpdatePack);
  Message.Result := 0;
end;
{$ENDIF VCL}

procedure TMainForm.QuitBtnClick(Sender: TObject);
begin
  Close;
end;

function TMainForm.ExpandOptionTree(
  Installation: TJclBorRADToolInstallation): Boolean;
var
  P: TProductFrame;
begin
  Result := False;
  P := View(Installation);
  if Assigned(P) then
  begin
    P.UpdateTree;
    Result := True;
  end;
end;

procedure TMainForm.InstallBtnClick(Sender: TObject);
begin
  if ({$IFDEF MSWINDOWS} IsDebuggerAttached or {$ENDIF} not CheckRunningInstances) and
    (Dialog(RsConfirmInstall, dtConfirmation, [drYes, drNo]) = drYes) then
    //(MessageBox(RsConfirmInstall, mtConfirmation, [mbYes, mbNo]{$IFDEF VisualCLX}, mbNo{$ENDIF}) = mrYes) then
  begin
    Install;
    QuitBtn.SetFocus;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  {$IFDEF VisualCLX}
  QApplication_postEvent(Handle, QCustomEvent_create(QEventType_UMCheckUpdates, Self));
  {$ELSE}
  PostMessage(Handle, UM_CHECKUPDATES, 0, 0);
  {$ENDIF}
end;

procedure TMainForm.JediImageClick(Sender: TObject);
begin
  { TODO : implement for Unix }
  {$IFDEF MSWINDOWS}
  ShellExecEx(DelphiJediURL);
  {$ENDIF MSWINDOWS}
end;

procedure TMainForm.TreeViewCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  AllowCollapse := Collapsable(Node);
end;

function TMainForm.GetBorRADToolInstallations: TJclBorRADToolInstallations;
begin
  Result := FBorRADToolInstallations;
end;

procedure TMainForm.InstallationStarted(Installation: TJclBorRADToolInstallation);
var
  P: TProductFrame;
begin
  P := View(Installation);
  P.InfoDisplay.Lines.Clear;
  Installation.DCC.OutputCallback := P.LogOutputLine;
  Installation.Make.OutputCallback := P.LogOutputLine;
  if Installation is TJclBCBInstallation then
    TJclBCBInstallation(Installation).Bpr2Mak.OutputCallback := P.LogOutputLine;
  ProductsPageControl.ActivePage := P.Parent as TTabSheet;
end;

procedure TMainForm.InstallationFinished(Installation: TJclBorRADToolInstallation);
var
  P: TProductFrame;
begin
  P := View(Installation);
  P.InfoDisplay.Lines.SaveToFile(JclInstall.LogFileName(Installation));
end;

function TMainForm.Dialog(const Text: string; DialogType: TDialogType = dtInformation;
  Options: TDialogResponses = [drOK]): TDialogResponse;
const
  DlgType: array[TDialogType] of TMsgDlgType = (mtWarning, mtError, mtInformation, mtConfirmation);
  DlgButton: array[TDialogResponse] of TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel);
  DlgResult: array[TDialogResponse] of Word = (mrYes, mrNo, mrOK, mrCancel);
var
  Buttons: TMsgDlgButtons;
  Res: Integer;
begin
  Buttons := [];
  for Result := Low(TDialogResponse) to High(TDialogResponse) do
    if Result in Options then
      Include(Buttons, DlgButton[Result]);
  Res := MessageDlg(Text, DlgType[DialogType], Buttons, 0);
  for Result := Low(TDialogResponse) to High(TDialogResponse) do
    if DlgResult[Result] = Res then
      Break;
end;

procedure TMainForm.SetReadme(const FileName: string);
begin
  ReadmePane.{$IFDEF VCL}Lines.{$ENDIF}LoadFromFile(FileName);
end;

procedure TMainForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  UpdateFeatureInfo(Node);
end;

procedure TMainForm.TreeViewChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
begin
  {$IFDEF EDIT_INFO_FILES}
  {$IFDEF VCL}
  with ActiveView do
    if InfoDisplay.Modified then
    begin
      InfoDisplay.Lines.SaveToFile(InfoFile(TreeView.Selected));
      InfoDisplay.Modified := False;
    end;
  {$ENDIF}
  //FJclInstall.SelectedNodeChanging(Node);
  {$ENDIF EDIT_INFO_FILES}
end;

procedure TMainForm.TreeViewEnter(Sender: TObject);
begin
  with ActiveView  do
    {$IFDEF VCL}if InfoDisplay.ReadOnly then{$ENDIF}
      UpdateFeatureInfo(TreeView.Selected);
end;

procedure TMainForm.TreeViewExit(Sender: TObject);
begin
  //
end;

procedure TMainForm.TreeViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateFeatureInfo(ActiveView.TreeView.GetNodeAt(X, Y));
end;

procedure TMainForm.UpdateFeatureInfo(Node: TTreeNode);
const
  SFileNotFound = '%s: File not found';
var
  FileName: string;
begin
  if Assigned(Node) and (Node <> FFeatureNode) then
  begin
    FFeatureNode := Node;
    FileName := InfoFile(Node);
    if FileExists(FileName) then
      FFeatureInfo.LoadFromFile(FileName)
    else
    begin
      FFeatureInfo.Clear;
      FFeatureInfo.Add(Format(SFileNotFound, [FileName]));
    end;
    FFeatureInfoChanged := True;
  end;
end;

procedure TMainForm.ShowFeatureHint;
var
  View: TProductFrame;
begin
  View := ActiveView;
  if Assigned(View) and (HintInfo.HintControl = View.TreeView) then
  begin
    if FFeatureInfoChanged then
      HintInfo.HintStr := FFeatureInfo.Text;
    HintInfo.ReshowTimeout := 500;
  end;
end;

function TMainForm.OptionGUI(
  Installation: TJclBorRADToolInstallation): TObject;
begin
  Result := View(Installation);
  if Result = nil then
    CreateView(Installation);
  Result := View(Installation).TreeView.Items;
end;

end.
