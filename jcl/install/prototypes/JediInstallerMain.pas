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
  QProductFrames, QJediInstallIntf,
  {$ELSE}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Menus, Buttons, ComCtrls, ImgList,
  ProductFrames, JediInstallIntf,
  {$ENDIF}
  JclBorlandTools;

const
  {$IFDEF VisualCLX}
  QEventType_UMCheckUpdates = QEventType(Integer(QEventType_ClxUser) + $100);
  {$ELSE}
  UM_CHECKUPDATES = WM_USER + $100;
  {$ENDIF}

type
  TMainForm = class(TForm, IJediInstallTool)
    InstallBtn: TBitBtn;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure QuitBtnClick(Sender: TObject);
    procedure InstallBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure JediImageClick(Sender: TObject);
    procedure TreeViewCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure TreeViewEnter(Sender: TObject);
    procedure TreeViewExit(Sender: TObject);
    procedure BplPathEditChange(Sender: TObject);
  private
    FBorRADToolInstallations: TJclBorRADToolInstallations;
    FJediInstall: IJediInstall;
    FInstallLog: TFileStream;
    FSystemPaths: TStringList;
    function ActiveView: TProductFrame;
    function CheckUpdatePack(Installation: TJclBorRADToolInstallation): Boolean;
    function CreateView(Installation: TJclBorRADToolInstallation): Boolean;
    function InfoFile(Node: TTreeNode): string;
    procedure InstallationStarted(Installation: TJclBorRADToolInstallation);
    procedure InstallationFinished(Installation: TJclBorRADToolInstallation);
    procedure InstallationProgress(Percent: Cardinal);
    procedure ReadSystemPaths;
    function View(Installation: TJclBorRADToolInstallation): TProductFrame; overload;
    function View(RADToolKind: TJclBorRADToolKind; Version: Integer): TProductFrame; overload;
    {$IFDEF VCL}
    procedure UMCheckUpdates(var Message: TMessage); message UM_CHECKUPDATES;
    {$ENDIF VCL}
    procedure UpdateFeatureInfo(Node: TTreeNode);
  protected
    {$IFDEF VisualCLX}
    function EventFilter(Sender: QObjectH; Event: QEventH): Boolean; override;
    {$ENDIF VisualCLX}
  public
    function CheckRunningInstances: Boolean;
    procedure Install;
    function PopulateTreeViews: Boolean;
    function SystemPathValid(const Path: string): Boolean;
    procedure UpdateButtons;
    // IJediInstallTool
    function BPLPath(Installation: TJclBorRADToolInstallation): string;
    function DCPPath(Installation: TJclBorRADToolInstallation): string;
    function FeatureChecked(FeatureID: Cardinal; Installation: TJclBorRADToolInstallation): Boolean;
    function GetBorRADToolInstallations: TJclBorRADToolInstallations;
    function MessageBox(const Text: string; DlgType: TMsgDlgType = mtInformation;
      Buttons: TMsgDlgButtons = [mbOK]{$IFDEF VisualCLX}; DefaultBtn: TMsgDlgBtn = mbNone{$ENDIF}): Integer;
    procedure UpdateInfo(Installation: TJclBorRADToolInstallation; const InfoText: String);
    procedure UpdateStatus(const Text: string);
    procedure WriteInstallLog(const Text: string);
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
  {$IFDEF JCLINSTALL}
  {$IFDEF VisualCLX}QJclInstall{$ELSE}JclInstall{$ENDIF},
  {$ENDIF JCLINSTALL}
  {$IFDEF JEDIINSTALL}
  {$IFDEF VisualCLX}QJediPackInstall{$ELSE}JediPackInstall{$ENDIF},
  {$ENDIF JEDIINSTALL}
  JclBase, JclFileUtils, JclStrings, JclSysInfo, JclSysUtils;

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
begin
  Page := ProductsPageControl.ActivePage;
  Result := Page.Controls[0] as TProductFrame;
end;

function TMainForm.InfoFile(Node: TTreeNode): string;
begin
  Result := FJediInstall.FeatureInfoFileName(FeatureID(Node));
end;

function TMainForm.CreateView(Installation: TJclBorRADToolInstallation): Boolean;
var
  Page: TTabSheet;
  ProductFrame: TProductFrame;
begin
  Result := True;
  if FJediInstall.Supports(Installation) then
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
    ProductFrame.Align := alClient;
    ProductFrame.Parent := Page;
  end;
end;

function TMainForm.CheckRunningInstances: Boolean;
begin
  Result := FBorRADToolInstallations.AnyInstanceRunning;
  if Result then
    MessageBox(RsCloseRADTool, mtWarning);
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
      if MessageBox(Msg, mtWarning, [mbYes, mbNo]{$IFDEF VisualCLX}, mbYes{$ENDIF}) = mrYes then
      {$IFDEF MSWINDOWS}
        ShellExecEx(SupportURLs[RadToolKind]);
      {$ENDIF MSWINDOWS}
      {$IFDEF UNIX}
      { TODO : Analoguous function for Linux };
      // Exec(KylixSupportURL);
      {$ENDIF UNIX}
    end;
end;

procedure TMainForm.Install;
var
  Res: Boolean;
  LogFileName: string;
begin
  ProgressBar.Position := 0;
  ProgressBar.Visible := True;
  Screen.Cursor := crHourGlass;
  try
    LogFileName := ChangeFileExt(Application.ExeName, '.log');
    FInstallLog := TFileStream.Create(LogFileName, fmCreate);
    try
      Res := FJediInstall.Install;
    finally
      FreeAndNil(FInstallLog);
    end;
    Screen.Cursor := crDefault;
    if Res then
      MessageBox(RsInstallSuccess)
    else
      {$IFDEF UNIX}
      Libc.system(PChar(Format('xterm -e less "%s"&', [LogFileName])));
      {$ENDIF}
      {$IFDEF MSWINDOWS}
      ShellExecEx(LogFileName);
      {$ENDIF MSWINDOWS}
  finally
    ProgressBar.Visible := False;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.InstallationProgress(Percent: Cardinal);
begin
  ProgressBar.Position := Percent;
end;

function TMainForm.View(RADToolKind: TJclBorRADToolKind; Version: Integer): TProductFrame;
begin
  Result := FindComponent(Format('%s%dProduct', [Prefixes[RADToolKind], Version])) as TProductFrame;
end;

function TMainForm.View(Installation: TJclBorRADToolInstallation): TProductFrame;
begin
  with Installation do
    Result := View(RADToolKind, VersionNumber);
end;

function TMainForm.PopulateTreeViews: Boolean;
var
  I: Integer;
  Page, ActivePage: TTabSheet;
  TreeView: TTreeView;
  ProductFrame: TProductFrame;
begin
  Result := False;
  ActivePage := nil;
  for I := 0 to ProductsPageControl.PageCount - 1 do
  begin
    Page := ProductsPageControl.Pages[I];
    ProductFrame := Page.Controls[0] as TProductFrame;
    TreeView := ProductFrame.TreeView;
    if FJediInstall.PopulateTreeView(ProductFrame.Installation, TreeView.Items) then
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
    StrToStrings(PathVar, PathSep, FSystemPaths, False);
    for I := 0 to FSystemPaths.Count - 1 do
    begin
      PathVar := StrTrimQuotes(FSystemPaths[I]);
      ExpandEnvironmentVar(PathVar);
      {$IFDEF MSWINDOWS}
      PathVar := AnsiUpperCase(PathRemoveSeparator(PathGetLongName2(PathVar)));
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

procedure TMainForm.UpdateButtons;
begin
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

procedure TMainForm.WriteInstallLog(const Text: string);
var
  TextLine: string;
begin
  TextLine := Text + AnsiLineBreak;
  FInstallLog.WriteBuffer(Pointer(TextLine)^, Length(TextLine));
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
  JediImage.Hint := DelphiJediURL;
  FJediInstall := CreateJediInstall;
  FJediInstall.SetOnProgress(InstallationProgress);
  FJediInstall.SetOnStarting(InstallationStarted);
  FJediInstall.SetOnEnding(InstallationFinished);
  FBorRADToolInstallations.Iterate(CreateView);
  FJediInstall.SetTool(Self);
  UpdateStatus('');
  if not FJediInstall.InitInformation(Application.ExeName) then
  begin
    MessageBox(RsCantFindFiles, mtError);
    Application.ShowMainForm := False;
    Application.Terminate;
  end
  else
  if not PopulateTreeViews then
  begin
    MessageBox(RsNoInstall);
    Application.ShowMainForm := False;
    Application.Terminate;
  end;
  ReadSystemPaths;
  {$IFDEF VCL}
  TitlePanel.DoubleBuffered := True;
  {$ELSE}
  WindowState := wsMaximized; // wouldn't work in Form resource
  {$ENDIF}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBorRADToolInstallations);
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

procedure TMainForm.InstallBtnClick(Sender: TObject);
begin
  if ({$IFDEF MSWINDOWS} IsDebuggerAttached or {$ENDIF} not CheckRunningInstances) and
    (MessageBox(RsConfirmInstall, mtConfirmation, [mbYes, mbNo]{$IFDEF VisualCLX}, mbNo{$ENDIF}) = mrYes) then
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
begin
end;

procedure TMainForm.InstallationFinished(Installation: TJclBorRADToolInstallation);
begin
end;

function TMainForm.MessageBox(const Text: string; DlgType: TMsgDlgType = mtInformation;
  Buttons: TMsgDlgButtons = [mbOK]{$IFDEF VisualCLX}; DefaultBtn: TMsgDlgBtn = mbNone{$ENDIF}): Integer;
begin
  Result := MessageDlg(Text, DlgType, Buttons, 0{$IFDEF VisualCLX}, DefaultBtn{$ENDIF});
end;

procedure TMainForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  UpdateFeatureInfo(Node);
  FJediInstall.SelectedNodeChanged(Node);
end;

procedure TMainForm.TreeViewChanging(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
begin
  {$IFDEF VCL}
  with ActiveView do
    if InfoDisplay.Modified then
    begin
      InfoDisplay.Lines.SaveToFile(InfoFile(TreeView.Selected));
      InfoDisplay.Modified := False;
    end;
  {$ENDIF}
  //FJediInstall.SelectedNodeChanging(Node);
end;

procedure TMainForm.TreeViewEnter(Sender: TObject);
begin
  with ActiveView  do
    {$IFDEF VCL}if InfoDisplay.ReadOnly then{$ENDIF}
      UpdateFeatureInfo(TreeView.Selected);
end;

procedure TMainForm.TreeViewExit(Sender: TObject);
begin
  with ActiveView.InfoDisplay do
  begin
    {$IFDEF VCL}
    if ReadOnly then
      Lines.LoadFromFile(FJediInstall.ReadmeFileName);
    {$ELSE}
      FileName := FJediInstall.ReadmeFileName;
      TextColor := clBlack;
      Update;
    {$ENDIF}
  end;
end;

procedure TMainForm.UpdateFeatureInfo(Node: TTreeNode);
const
  SFileNotFound = '%s: File not found';
var
  FileName: string;
begin
  with ActiveView do
  begin
    if Assigned(Node) then
      FileName := InfoFile(Node);
    {$IFDEF VCL}
    if FileExists(FileName) then
    begin
      InfoDisplay.Font.Color := clBlack;
      InfoDisplay.Lines.LoadFromFile(FileName)
    end
    else
    begin
      InfoDisplay.Lines.Clear;
      InfoDisplay.Font.Color := clRed;
      InfoDisplay.Lines.Add(Format(SFileNotFound, [FileName]));
      InfoDisplay.Modified := False;
    end;
    {$ELSE}
    if FileExists(FileName) then
    begin
      InfoDisplay.TextColor := clBlack;
      InfoDisplay.Text := FileToString(FileName);
    end
    else
    begin
      InfoDisplay.TextColor := clRed;
      InfoDisplay.Text := Format(SFileNotFound, [FileName]);
    end;
    {$ENDIF}
  end;
end;

end.
