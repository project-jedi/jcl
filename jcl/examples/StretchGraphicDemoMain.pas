//
// Robert Rossmair, 2002-09-22
//
unit StretchGraphicDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Menus, ExtCtrls, ExtDlgs, JPEG,
  JclGraphics;

type
  TStretchDemoForm = class(TForm)
    OpenDialog: TOpenPictureDialog;
    PageControl: TPageControl;
    OriginalPage: TTabSheet;
    StretchedPage: TTabSheet;
    StretchedImage: TImage;
    MainMenu: TMainMenu;
    Fil1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    ExitItem: TMenuItem;
    Filter1: TMenuItem;
    Box1: TMenuItem;
    riangle1: TMenuItem;
    Hermite1: TMenuItem;
    Bell1: TMenuItem;
    Spline1: TMenuItem;
    Lanczos31: TMenuItem;
    Mitchell1: TMenuItem;
    Options1: TMenuItem;
    PreserveAspectRatio1: TMenuItem;
    PrevItem: TMenuItem;
    NextItem: TMenuItem;
    FilesPage: TTabSheet;
    FileListBox: TListBox;
    ScrollBox: TScrollBox;
    OriginalImage: TImage;
    procedure OpenFile(Sender: TObject);
    procedure SelectFilter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PreserveAspectRatio1Click(Sender: TObject);
    procedure ExitApp(Sender: TObject);
    procedure PrevFile(Sender: TObject);
    procedure NextFile(Sender: TObject);
    procedure FileListBoxClick(Sender: TObject);
    procedure LoadSelected;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FDir: string;
    FResamplingFilter: TResamplingFilter;
    FPreserveAspectRatio: Boolean;
    procedure DoStretch;
    procedure LoadFile(const FileName: string);
    procedure UpdateCaption;
    procedure UpdateFileList(const FileName: string);
    procedure UpdateNavButtons;
  end;

var
  StretchDemoForm: TStretchDemoForm;

implementation

{$R *.dfm}

uses JclFileUtils;

var
  FileMask: string;

function IsGraphicFile(const Attr: Integer; const FileInfo: TSearchRec): Boolean;
var
  Ext: string;
begin
  Ext := AnsiLowerCase(ExtractFileExt(FileInfo.Name));
  Result := (Pos(Ext, FileMask) > 0);
end;

procedure TStretchDemoForm.FormCreate(Sender: TObject);
begin
  FileMask := Format('%s;%s', [GraphicFileMask(TJPEGImage), GraphicFileMask(TBitmap)]);
  FResamplingFilter := rfSpline;
  FPreserveAspectRatio := True;
  UpdateNavButtons;
  UpdateCaption;
end;

procedure TStretchDemoForm.UpdateFileList(const FileName: string);
var
  Dir, D: string;
begin
  D := ExtractFileDir(FileName);
{$IFDEF COMPILER6_UP}
  Dir := IncludeTrailingPathDelimiter(D);
{$ELSE}
  Dir := IncludeTrailingBackslash(D);
{$ENDIF}
  if Dir <> FDir then
  begin
    FDir := Dir;
    FilesPage.Caption := Format('Files in %s', [D]);
    FileListBox.Items.Clear;
    AdvBuildFileList(Dir + '*.*', faAnyFile, FileListBox.Items, amCustom, [], '', IsGraphicFile);
    with FileListBox do
      ItemIndex := Items.IndexOf(ExtractFileName(OpenDialog.FileName))
  end;
end;

procedure TStretchDemoForm.LoadFile(const FileName: string);
begin
  OriginalImage.Picture.LoadFromFile(FileName);
  UpdateFileList(FileName);
  UpdateNavButtons;
  DoStretch;
end;

procedure TStretchDemoForm.OpenFile(Sender: TObject);
begin
  if OpenDialog.Execute then
    LoadFile(OpenDialog.FileName);
end;

procedure TStretchDemoForm.SelectFilter(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    Checked := True;
    FResamplingFilter := TResamplingFilter(Tag);
    DoStretch;
  end;
end;

procedure TStretchDemoForm.DoStretch;
var
  W, H: Integer;
begin
  if OriginalImage.Picture.Graphic = nil then
    Exit;
  W := StretchedPage.Width;
  H := StretchedPage.Height;
  if FPreserveAspectRatio then
    with OriginalImage.Picture.Graphic do
    begin
      if W * Height > H * Width then
        W := H * Width div Height
      else
        H := W * Height div Width;
    end;
  StretchedImage.SetBounds(0, 0, W, H);
  JclGraphics.Stretch(W, H, FResamplingFilter, 0, OriginalImage.Picture.Graphic,
    StretchedImage.Picture.Bitmap);
  PageControl.ActivePage := StretchedPage;
end;

procedure TStretchDemoForm.FormResize(Sender: TObject);
begin
  DoStretch;
  UpdateCaption;
end;

procedure TStretchDemoForm.PreserveAspectRatio1Click(Sender: TObject);
begin
  with Sender as TMenuItem do
  begin
    Checked := not Checked;
    FPreserveAspectRatio := Checked;
    DoStretch;
  end;
end;

procedure TStretchDemoForm.UpdateCaption;
begin
  Caption := Format('JclGraphics.Stretch To %d x %d', [StretchedPage.Width, StretchedPage.Height]);
end;

procedure TStretchDemoForm.ExitApp(Sender: TObject);
begin
  Close;
end;

procedure TStretchDemoForm.LoadSelected;
begin
  with FileListBox do
    if ItemIndex <> -1 then
      LoadFile(FDir + Items[ItemIndex]);
end;

procedure TStretchDemoForm.PrevFile(Sender: TObject);
begin
  with FileListBox do
    ItemIndex  := ItemIndex - 1;
  LoadSelected;
end;

procedure TStretchDemoForm.NextFile(Sender: TObject);
begin
  with FileListBox do
    ItemIndex  := ItemIndex + 1;
  LoadSelected;
end;

procedure TStretchDemoForm.UpdateNavButtons;
begin
  PrevItem.Enabled := FileListBox.ItemIndex > 0;
  NextItem.Enabled := FileListBox.ItemIndex < FileListBox.Items.Count - 1;
end;

procedure TStretchDemoForm.FileListBoxClick(Sender: TObject);
begin
  LoadSelected;
end;

procedure TStretchDemoForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_LEFT:
      begin
        PrevFile(Self);
        Key := 0;
      end;
    VK_RIGHT:
      begin
        NextFile(Self);
        Key := 0;
      end;
  end;
end;

end.
