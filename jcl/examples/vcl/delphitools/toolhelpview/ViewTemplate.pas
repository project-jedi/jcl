unit ViewTemplate;

{$I JCL.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ToolWin, ComCtrls, ActnList, Menus;

const
  UM_BUILD = WM_USER + $100;

type
  TViewForm = class(TForm)
    CoolBar: TCoolBar;
    ToolBar: TToolBar;
    ActionList: TActionList;
    PopupMenu: TPopupMenu;
    TextLabels1: TAction;
    ToolBarPopupMenu: TPopupMenu;
    Textlabels2: TMenuItem;
    Copy1: TAction;
    SaveToFile1: TAction;
    Refresh1: TAction;
    SelectAll1: TAction;
    Find1: TAction;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure TextLabels1Execute(Sender: TObject);
    procedure SelectAll1Update(Sender: TObject);
    procedure SelectAll1Execute(Sender: TObject);
    procedure Copy1Update(Sender: TObject);
    procedure Copy1Execute(Sender: TObject);
    procedure SaveToFile1Execute(Sender: TObject);
    procedure Find1Update(Sender: TObject);
    procedure Find1Execute(Sender: TObject);
  private
    procedure UpdateTextLabels;
    procedure UMBuild(var Msg: TMessage); message UM_BUILD;
  public
    procedure BuildContent; dynamic; abstract;
    procedure PostBuildContentMessage;
  end;

var
  ViewForm: TViewForm;

implementation

uses Main, Global, ToolsUtils, About, FindDlg;

{$R *.DFM}

procedure TViewForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MainForm.DeleteFromViewsMenu(Self);
  Action := caFree;
end;

procedure TViewForm.FormShow(Sender: TObject);
begin
  MainForm.AddToViewsMenu(Self, Caption);
end;

procedure TViewForm.TextLabels1Execute(Sender: TObject);
begin
  with TextLabels1 do Checked := not Checked;
  UpdateTextLabels;
end;

procedure TViewForm.UpdateTextLabels;
begin
  ToolBar.ShowCaptions := TextLabels1.Checked;
  if not ToolBar.ShowCaptions then
  begin
    ToolBar.ButtonHeight := 0;
    ToolBar.ButtonWidth := 0;
  end;
end;

procedure TViewForm.Copy1Update(Sender: TObject);
begin
  TAction(Sender).Enabled := ActiveControl is TListView;
end;

procedure TViewForm.SelectAll1Update(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    (ActiveControl is TListView) and TListView(ActiveControl).MultiSelect;
end;

procedure TViewForm.SelectAll1Execute(Sender: TObject);
begin
  ListViewSelectAll(ActiveControl as TListView);
end;

procedure TViewForm.Copy1Execute(Sender: TObject);
begin
  GlobalModule.ListViewToClipboard(ActiveControl as TListView);
end;

procedure TViewForm.SaveToFile1Execute(Sender: TObject);
begin
  GlobalModule.ListViewToFile(ActiveControl as TListView, Caption);
end;

procedure TViewForm.UMBuild(var Msg: TMessage);
begin
  Update;
  BuildContent;
end;

procedure TViewForm.PostBuildContentMessage;
begin
  PostMessage(Handle, UM_BUILD, 0, 0);
end;

procedure TViewForm.Find1Update(Sender: TObject);
begin
  TAction(Sender).Enabled :=
    (ActiveControl is TListView) and not TListView(ActiveControl).HideSelection;
end;

procedure TViewForm.Find1Execute(Sender: TObject);
begin
  ShowFindDialog(ActiveControl as TListView);
end;

end.
