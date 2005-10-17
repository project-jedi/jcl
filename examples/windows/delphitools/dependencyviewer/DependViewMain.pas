unit DependViewMain;

{$I JCL.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ToolWin, ComCtrls, ImgList, ActnList, StdActns, ClipBrd, Registry;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    CoolBar1: TCoolBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ActionList1: TActionList;
    ToolbarImagesList: TImageList;
    OpenFileDialog: TOpenDialog;
    File1: TMenuItem;
    Exit1: TAction;
    Exit2: TMenuItem;
    Open1: TAction;
    Open2: TMenuItem;
    N1: TMenuItem;
    Window1: TMenuItem;
    WindowCascade1: TWindowCascade;
    WindowTileHorizontal1: TWindowTileHorizontal;
    WindowTileVertical1: TWindowTileVertical;
    Cascade1: TMenuItem;
    TileHorizontally1: TMenuItem;
    TileVertically1: TMenuItem;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ViewImageList: TImageList;
    ToolButton7: TToolButton;
    Copy1: TAction;
    Save1: TAction;
    Edit1: TMenuItem;
    Copy2: TMenuItem;
    Save2: TMenuItem;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    SelectAll1: TAction;
    Selectall2: TMenuItem;
    SaveDialog: TSaveDialog;
    Win32Help1: TAction;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    Help1: TMenuItem;
    Win32helpkeyword1: TMenuItem;
    N2: TMenuItem;
    About1: TAction;
    About2: TMenuItem;
    StatusBar: TStatusBar;
    DumpPe1: TAction;
    ToolButton2: TToolButton;
    N3: TMenuItem;
    DumpPEfile1: TMenuItem;
    SendMail1: TAction;
    Sendamessage1: TMenuItem;
    Find1: TAction;
    ToolButton6: TToolButton;
    N4: TMenuItem;
    Findtext1: TMenuItem;
    procedure Exit1Execute(Sender: TObject);
    procedure Open1Execute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SelectAll1Update(Sender: TObject);
    procedure SelectAll1Execute(Sender: TObject);
    procedure Copy1Update(Sender: TObject);
    procedure Copy1Execute(Sender: TObject);
    procedure Win32Help1Update(Sender: TObject);
    procedure Win32Help1Execute(Sender: TObject);
    procedure About1Execute(Sender: TObject);
    procedure DumpPe1Update(Sender: TObject);
    procedure DumpPe1Execute(Sender: TObject);
    procedure SendMail1Execute(Sender: TObject);
    procedure Find1Update(Sender: TObject);
    procedure Find1Execute(Sender: TObject);
    procedure CoolBar1Resize(Sender: TObject);
  private
    FPeViewer: Variant;
    FPeViewerRegistred: Boolean;
    FWin32Help: string;
    procedure InvokeWin32Help(const Name: string);
    function IsFileViewerChildActive: Boolean;
    function IsWin32Help: Boolean;
    procedure OnActiveFormChange(Sender: TObject);
  public
    procedure OpenFile(const FileName: TFileName);
  end;

var
  MainForm: TMainForm;

implementation

uses ToolsUtils, FileViewer, JclPeImage, JclRegistry, FindDlg;

{$R *.DFM}

resourcestring
  sNotValidFile = 'This is not a valid PE EXE file';

procedure TMainForm.InvokeWin32Help(const Name: string);
var
  S: string;
begin
  S := PeStripFunctionAW(Name);
  WinHelp(Application.Handle, PChar(FWin32Help), HELP_KEY, DWORD(S));
end;

procedure TMainForm.OpenFile(const FileName: TFileName);
begin
{  if IsPeExe(FileName) then
  begin}
    TFileViewerChild.Create(Self).FileName := FileName;
    OnActiveFormChange(nil);
{  end else
    MessBox(sNotValidFile, MB_ICONINFORMATION);}
end;

procedure TMainForm.Exit1Execute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.Open1Execute(Sender: TObject);
var
  I: Integer;
begin
  with OpenFileDialog do
  begin
    FileName := '';
    if Execute then
      for I := 0 to Files.Count - 1 do OpenFile(Files[I]);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FWin32Help := Win32HelpFileName;
  FPeViewerRegistred := IsPeViewerRegistred;
  Screen.OnActiveFormChange := OnActiveFormChange;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Screen.OnActiveFormChange := nil;
end;

procedure TMainForm.OnActiveFormChange(Sender: TObject);
begin
  if IsFileViewerChildActive then
    StatusBar.Panels[0].Text := TFileViewerChild(ActiveMDIChild).FileName
  else
    StatusBar.Panels[0].Text := '';
end;

procedure TMainForm.SelectAll1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := Screen.ActiveControl is TListView;
end;

procedure TMainForm.SelectAll1Execute(Sender: TObject);
begin
  ListViewSelectAll(Screen.ActiveControl as TListView);
end;

procedure TMainForm.Copy1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := Screen.ActiveControl is TListView;
end;

procedure TMainForm.Copy1Execute(Sender: TObject);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  Screen.Cursor := crHourGlass;
  try
    SL.Capacity := 256;
    ListViewToStrings(Screen.ActiveControl as TListView, SL, True);
    case TAction(Sender).Tag of
      0: Clipboard.AsText := SL.Text;
      1: with SaveDialog do
         begin
           FileName := '';
           if Execute then SL.SaveToFile(FileName);
         end;
    end;        
  finally
    Screen.Cursor := crDefault;
    SL.Free;
  end;
end;

procedure TMainForm.Win32Help1Update(Sender: TObject);
begin
  Win32Help1.Enabled := IsWin32Help and IsFileViewerChildActive and
    (TFileViewerChild(ActiveMDIChild).GetWin32Function <> '');
end;

procedure TMainForm.Win32Help1Execute(Sender: TObject);
begin
  InvokeWin32Help((ActiveMDIChild as TFileViewerChild).GetWin32Function);
end;

procedure TMainForm.About1Execute(Sender: TObject);
begin
  ShowToolsAboutBox;
end;

function TMainForm.IsFileViewerChildActive: Boolean;
begin
  Result := (ActiveMDIChild is TFileViewerChild);
end;

function TMainForm.IsWin32Help: Boolean;
begin
  Result := Length(FWin32Help) > 0;
end;

procedure TMainForm.DumpPe1Update(Sender: TObject);
begin
  DumpPe1.Enabled := FPeViewerRegistred and IsFileViewerChildActive and
    (TFileViewerChild(ActiveMDIChild).SelectedFileName <> '');
end;

procedure TMainForm.DumpPe1Execute(Sender: TObject);
begin
  FPeViewer := CreateOrGetOleObject(PeViewerClassName);
  FPeViewer.OpenFile((ActiveMDIChild as TFileViewerChild).SelectedFileName);
  FPeViewer.BringToFront;
end;

procedure TMainForm.SendMail1Execute(Sender: TObject);
begin
  SendEmail;
end;

procedure TMainForm.Find1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := TFindTextForm.CanExecuteFind;
end;

procedure TMainForm.Find1Execute(Sender: TObject);
begin
  ShowFindDialog(Screen.ActiveControl as TListView);
end;

procedure TMainForm.CoolBar1Resize(Sender: TObject);
begin
  D4FixCoolBarResizePaint(Sender);
end;

end.
