unit About;

{$I JCL.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TAboutBox = class(TForm)
    IconPaintBox: TPaintBox;
    OkBtn: TButton;
    Bevel1: TBevel;
    ProductNameLabel: TLabel;
    VersionLabel: TLabel;
    CompanyLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure IconPaintBoxPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FLinks: array of string;
    FURLSpacing: Integer;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    function GetLinkCaption(Index: Integer): string;
    function GetLinkURL(Index: Integer): string;
    procedure OpenURL(const UrlName: String);
    procedure UpdateLinkLabel(L: LPARAM; Activate: Boolean);
    procedure UrlLinkLabelClick(Sender: TObject);
  end;

procedure ShowAbout(const Links: array of string; Spacing: Integer = 20);

var
  AboutBox: TAboutBox;

implementation

{$R *.DFM}

uses
  ShellAPI, JclFileUtils;

procedure ShowAbout(const Links: array of string; Spacing: Integer);
var
  I: Integer;
begin
  with TAboutBox.Create(Application) do
  try
    SetLength(FLinks, High(Links) + 1);
    for I := Low(Links) to High(Links) do
      FLinks[I] := Links[I];
    FURLSpacing := Spacing;
    ShowModal;
  finally
    Free;
  end;
end;

{ TAboutBox }

procedure TAboutBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  UpdateLinkLabel(Message.LParam, True);
end;

procedure TAboutBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  UpdateLinkLabel(Message.LParam, False);
end;

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  with IconPaintBox do
  begin
    Width := GetSystemMetrics(SM_CXICON);
    Height := GetSystemMetrics(SM_CYICON);
  end;
  with TJclFileVersionInfo.Create(Application.ExeName) do
  try
    ProductNameLabel.Caption := ProductName;
    VersionLabel.Caption := Format('Version: %s', [ProductVersion]);
    CompanyLabel.Caption := LegalCopyright;
  finally
    Free;
  end;
end;

procedure TAboutBox.FormShow(Sender: TObject);
var
  I: Integer;
begin
  I := Length(FLinks) * FURLSpacing - 20;
  if I > 0 then Height := Height + I;
  for I := 0 to Length(FLinks) - 1 do
    with TLabel.Create(Self) do
    begin
      Parent := Self;
      SetBounds(CompanyLabel.Left, I * FURLSpacing + CompanyLabel.Top + 25, 0, 0);
      Caption := GetLinkCaption(I);
      Cursor := crHandPoint;
      Font.Color := clBlue;
      Font.Style := [fsUnderline];
      Hint := GetLinkURL(I);
      Tag := I + 1;
      OnClick := UrlLinkLabelClick;
    end;
end;

function TAboutBox.GetLinkCaption(Index: Integer): string;
begin
  Result := FLinks[Index];
  Result := Copy(Result, 1, Pos(';', Result) - 1);
end;

function TAboutBox.GetLinkURL(Index: Integer): string;
begin
  Result := FLinks[Index];
  Delete(Result, 1, Pos(';', Result));
end;

procedure TAboutBox.IconPaintBoxPaint(Sender: TObject);
begin
  IconPaintBox.Canvas.Draw(0, 0, Application.Icon);
end;

procedure TAboutBox.OpenURL(const UrlName: String);
var
  Sei: TShellExecuteInfo;
begin
  ZeroMemory(@Sei, Sizeof(Sei));
  Sei.cbSize := Sizeof(Sei);
  Sei.Wnd := Application.Handle;
  Sei.lpFile := PChar(UrlName);
  Sei.nShow := SW_SHOWNORMAL;
  ShellExecuteEx(@Sei);
end;

procedure TAboutBox.UpdateLinkLabel(L: LPARAM; Activate: Boolean);
begin
  if (TObject(L) is TLabel) and (TLabel(L).Tag > 0) then
    with TLabel(L).Font do
      if Activate then Color := clPurple else Color := clBlue;
end;

procedure TAboutBox.UrlLinkLabelClick(Sender: TObject);
begin
  OpenURL(GetLinkURL(TLabel(Sender).Tag - 1));
end;


end.
