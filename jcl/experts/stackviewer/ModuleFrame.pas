unit ModuleFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, IniFiles, JclDebugStackUtils;

type
  TfrmModule = class(TFrame)
    lv: TListView;
  private
    FModuleList: TModuleList;
    procedure SetModuleList(const Value: TModuleList);
    { Private declarations }
  public
    { Public declarations }
    property ModuleList: TModuleList read FModuleList write SetModuleList;
    procedure LoadState(AIni: TCustomIniFile; const ASection: string);
    procedure SaveState(AIni: TCustomIniFile; const ASection: string);
  end;

implementation

{$R *.dfm}

{ TfrmModule }

procedure TfrmModule.LoadState(AIni: TCustomIniFile; const ASection: string);
var
  I: Integer;
begin
  for I := 0 to lv.Columns.Count - 1 do
    lv.Columns.Items[I].Width := AIni.ReadInteger(ASection,
      Format('ModuleFrameColumnWidth%d', [I]), lv.Columns.Items[I].Width);
end;

procedure TfrmModule.SaveState(AIni: TCustomIniFile; const ASection: string);
var
  I: Integer;
begin
  for I := 0 to lv.Columns.Count - 1 do
    AIni.WriteInteger(ASection, Format('ModuleFrameColumnWidth%d', [I]), lv.Columns.Items[I].Width);
end;

procedure TfrmModule.SetModuleList(const Value: TModuleList);
var
  I: Integer;
  ListItem: TListItem;
begin
  FModuleList := Value;
  lv.Items.Clear;
  for I := 0 to FModuleList.Count - 1 do
  begin
    ListItem := lv.Items.Add;
    ListItem.Caption := FModuleList[I].StartStr;
    ListItem.SubItems.Add(FModuleList[I].EndStr);
    ListItem.SubItems.Add(FModuleList[I].SystemModuleStr);
    ListItem.SubItems.Add(ExtractFileName(FModuleList[I].ModuleName));
    ListItem.SubItems.Add(FModuleList[I].BinFileVersion);
    ListItem.SubItems.Add(FModuleList[I].FileVersion);
    ListItem.SubItems.Add(FModuleList[I].FileDescription);
  end;
end;

end.

