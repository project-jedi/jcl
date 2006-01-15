unit JclOtaActionConfigureSheet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TJclOtaActionConfigureFrame = class(TFrame)
    ListViewActions: TListView;
    LabelActions: TLabel;
    HotKeyShortcut: THotKey;
    LabelShortcut: TLabel;
    ButtonRestore: TButton;
    procedure HotKeyShortcutExit(Sender: TObject);
    procedure ButtonRestoreClick(Sender: TObject);
    procedure ListViewActionsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
  public
    constructor Create(AOwner: TComponent); override;
    procedure SaveChanges;
  end;

implementation

{$R *.dfm}

uses
  ActnList, Menus,
  ToolsApi,
  JclOtaConsts, JclOtaResources, JclOtaUtils;

{ TFrameActions }

procedure TJclOtaActionConfigureFrame.ButtonRestoreClick(Sender: TObject);
var
  AListItem: TListItem;
  AAction: TAction;
begin
  AListItem := ListViewActions.Selected;
  if Assigned(AListItem) then
  begin
    AAction := TJclOTAExpertBase.GetAction(AListItem.Index);
    AListItem.SubItems.Strings[0] := ShortcutToText(TShortcut(AAction.Tag));
    HotKeyShortcut.HotKey := TShortcut(AAction.Tag);
  end;
end;

constructor TJclOtaActionConfigureFrame.Create(AOwner: TComponent);
var
  Index: Integer;
  ANTAServices: INTAServices;
  AListItem: TListItem;
  AAction: TAction;
begin
  inherited Create(AOwner);

  ButtonRestore.Caption := RsRestore;
  LabelActions.Caption := RsActions;
  LabelShortcut.Caption := RsShortcut;
  ListViewActions.Columns.Items[0].Caption := RsCaption;
  ListViewActions.Columns.Items[1].Caption := RsShortcut;

  Supports(BorlandIDEServices, INTAServices, ANTAServices);
  if not Assigned(ANTAServices) then
    raise EJclExpertException.CreateTrace(RsENoNTAServices);

  ListViewActions.SmallImages := ANTAServices.ImageList;

  for Index := 0 to TJclOTAExpertBase.GetActionCount - 1 do
  begin
    AListItem := ListViewActions.Items.Add;
    AAction := TJclOTAExpertBase.GetAction(Index);
    AListItem.ImageIndex := AAction.ImageIndex;
    AListItem.Caption := AAction.Caption;
    AListItem.Data := Pointer(AAction.ShortCut);
    AListItem.SubItems.Add(ShortcutToText(AAction.ShortCut));
  end;
end;

procedure TJclOtaActionConfigureFrame.HotKeyShortcutExit(Sender: TObject);
var
  AListItem: TListItem;
begin
  AListItem := ListViewActions.Selected;
  if Assigned(AListItem) then
  begin
    AListItem.Data := Pointer(HotKeyShortcut.HotKey);
    AListItem.SubItems.Strings[0] := ShortCutToText(HotKeyShortcut.HotKey);
  end;
end;

procedure TJclOtaActionConfigureFrame.ListViewActionsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Selected then
    HotKeyShortcut.HotKey := TShortcut(Item.Data)
  else
    HotKeyShortcut.HotKey := scNone;
end;

procedure TJclOtaActionConfigureFrame.SaveChanges;
var
  Index: Integer;
begin
  { (ahuser) In Delphi 7 the ListViewActions.Items.Count is 0 if the page was
    not shown. Something must delete the items that were filled in the constructor. }
  if ListViewActions.Items.Count = TJclOTAExpertBase.GetActionCount then
  begin
    for Index := 0 to TJclOTAExpertBase.GetActionCount - 1 do
      TJclOTAExpertBase.GetAction(Index).ShortCut :=
        TShortcut(ListViewActions.Items.Item[Index].Data);
  end;
end;

end.
