{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit. Manual modifications will be lost on next release.  }
{**************************************************************************************************}

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
{ Contributor(s): Robert Rossmair (crossplatform support)                                          }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: February 17, 2004                                                                 }
{                                                                                                  }
{**************************************************************************************************}

unit JediInstallerMain;

{$I jcl.inc}

interface

uses
  
  Windows, Messages,
  
  SysUtils, Classes,
  
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Menus, ComCtrls, ImgList,
  ProductFrames, JediInstallIntf,
  
  DelphiInstall;

const
  
  UM_CHECKUPDATES = WM_USER + $100;
  

type
  TMainForm = class(TForm, IJediInstallTool)
    InstallBtn: TButton;
    CloseBtn: TButton;
    JediImage: TImage;
    TitlePanel: TPanel;
    Title: TLabel;
    ProductsPageControl: TPageControl;
    StatusBevel: TBevel;
    StatusLabel: TLabel;
    Bevel1: TBevel;
    
    //
    D5TabSheet: TTabSheet;
    D5Product: TProductFrame;
    //
    D6TabSheet: TTabSheet;
    D6Product: TProductFrame;
    //
    D7TabSheet: TTabSheet;
    D7Product: TProductFrame;
    //
    ImageList: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure InstallBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure JediImageClick(Sender: TObject);
    procedure TreeViewCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure BplPathEditChange(Sender: TObject);
  private
    FDelphiInstallations: TJclDelphiInstallations;
    FJediInstall: IJediInstall;
    FInstallLog: TFileStream;
    FSystemPaths: TStringList;
    procedure ReadSystemPaths;
    function Product(Version: Integer): TProductFrame;
    
    procedure UMCheckUpdates(var Message: TMessage); message UM_CHECKUPDATES;
    
  protected
    
  public
    function CheckRunningInstances: Boolean;
    procedure CheckUpdatePacks;
    procedure Install;
    procedure PopulatePaths;
    function PopulateTreeViews: Boolean;
    function SystemPathValid(const Path: string): Boolean;
    procedure UpdateButtons;
    // IJediInstallTool
    function ActiveVersionNumberPage: Integer;
    function BPLPath(VersionNumber: Integer): string;
    function DCPPath(VersionNumber: Integer): string;
    function FeatureChecked(FeatureID: Cardinal; VersionNumber: Integer): Boolean;
    function GetDelphiInstallations: TJclDelphiInstallations;
    function MessageBox(const Text: string; DlgType: TMsgDlgType = mtInformation;
      Buttons: TMsgDlgButtons = [mbOK]): Integer;
    procedure UpdateInfo(VersionNumber: Integer; const InfoText: String);
    procedure UpdateStatus(const Text: string);
    procedure WriteInstallLog(const Text: string);
  end;

var
  MainForm: TMainForm;

implementation


{$R *.dfm}


uses
  
  FileCtrl,
  JclDebug, JclShell,
  
  {$IFDEF JCLINSTALL}
  JclInstall,
  {$ENDIF JCLINSTALL}
  {$IFDEF JEDIINSTALL}
  JediPackInstall,
  {$ENDIF JEDIINSTALL}
  JclBase, JclFileUtils, JclStrings, JclSysInfo, JclSysUtils;

const
  {$IFNDEF COMPILER6_UP}
  PathSep = ';';
  {$ENDIF COMPILER6_UP}
  DelphiSupportURL  = 'http://www.borland.com/devsupport/delphi/';
  DelphiJediURL     = 'http://delphi-jedi.org';
  VersionSignature  = 'D%d';

resourcestring
  RsCantFindFiles   = 'Can not find installation files, check your installation.';
  RsCloseDelphi     = 'Please close all running instances of Delphi IDE before the installation.';
  RsConfirmInstall  = 'Are you sure to install all selected features?';
  RsEnterValidPath  = '(Enter valid path)';
  RsInstallSuccess  = 'Installation finished';
  RsNoInstall       = 'There is no Delphi installation on this machine. Installer will close.';
  RsUpdateNeeded    = '. Would you like to open Borland Delphi support web page?';

{ TMainForm }

function TMainForm.CheckRunningInstances: Boolean;
begin
  Result := FDelphiInstallations.AnyInstanceRunning;
  if Result then
    MessageBox(RsCloseDelphi, mtWarning);
end;

procedure TMainForm.CheckUpdatePacks;
var
  UpdateText: string;
begin
  if FDelphiInstallations.AnyUpdatePackNeeded(UpdateText) then
  begin
    UpdateText := UpdateText + RsUpdateNeeded;
    if MessageBox(UpdateText, mtWarning, [mbYes, mbNo]) = mrYes then
    { TODO : Analoguous function for Linux }
    
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
      MessageBox(RsInstallSuccess);
  finally
    Screen.Cursor := crDefault;
  end;
end;

function TMainForm.Product(Version: Integer): TProductFrame;
begin
  Result := FindComponent(Format('D%dProduct', [Version])) as TProductFrame;
end;

procedure TMainForm.PopulatePaths;
var
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
      Product(Page.Tag).BplPathEdit.Text := GetPathForEdit(Installation.BPLOutputPath);
      Product(Page.Tag).DcpPathEdit.Text := GetPathForEdit(Installation.DCPOutputPath);
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
    TreeView := Product(Page.Tag).TreeView;
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
    StrToStrings(PathVar, PathSep, FSystemPaths, False);
    for I := 0 to FSystemPaths.Count - 1 do
    begin
      PathVar := StrTrimQuotes(FSystemPaths[I]);
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
  P: TProductFrame;
begin
  P := Product(VersionNumber);
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
  TextLine := Text + AnsiCrLf;
  FInstallLog.WriteBuffer(Pointer(TextLine)^, Length(TextLine));
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
  Path := Product(VersionNumber).BplPathEdit.Text;
  Result := PathRemoveSeparator(FDelphiInstallations.InstallationFromVersion[VersionNumber].SubstitutePath(Path));
end;

function TMainForm.DCPPath(VersionNumber: Integer): string;
var
  Path: string;
begin
  Path := Product(VersionNumber).DcpPathEdit.Text;
  Result := PathRemoveSeparator(FDelphiInstallations.InstallationFromVersion[VersionNumber].SubstitutePath(Path));
end;

function TMainForm.FeatureChecked(FeatureID: Cardinal; VersionNumber: Integer): Boolean;
begin
  Result := Product(VersionNumber).FeatureChecked(FeatureID, VersionNumber);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDelphiInstallations := TJclDelphiInstallations.Create;
  FInstallLog := TFileStream.Create(ChangeFileExt(Application.ExeName, '.log'), fmCreate);
  FSystemPaths := TStringList.Create;
  JediImage.Hint := DelphiJediURL;
  FJediInstall := CreateJediInstall;
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
  PopulatePaths;
  
  TitlePanel.DoubleBuffered := True;
  
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FDelphiInstallations);
  FreeAndNil(FInstallLog);
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
  if ( IsDebuggerAttached or  not CheckRunningInstances) and
    (MessageBox(RsConfirmInstall, mtConfirmation, [mbYes, mbNo]) = mrYes) then
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
  { TODO : implement for Unix }
  
  ShellExecEx(DelphiJediURL);
  
end;

procedure TMainForm.TreeViewCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  AllowCollapse := FJediInstall.SelectedNodeCollapsing(Node);
end;

function TMainForm.GetDelphiInstallations: TJclDelphiInstallations;
begin
  Result := FDelphiInstallations;
end;

function TMainForm.MessageBox(const Text: string; DlgType: TMsgDlgType = mtInformation;
  Buttons: TMsgDlgButtons = [mbOK]): Integer;
begin
  Result := MessageDlg(Text, DlgType, Buttons, 0);
end;

procedure TMainForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  FJediInstall.SelectedNodeChanged(Node);
end;

procedure TMainForm.BplPathEditChange(Sender: TObject);
begin
  with (Sender as TEdit) do
    if SystemPathValid(Text) then
      Font.Color := clWindowText
    else
      Font.Color := clRed;
end;

end.
