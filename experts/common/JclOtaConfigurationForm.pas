unit JclOtaConfigurationForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  JclOtaUtils;

type
  TJclOtaOptionsForm = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    PanelName: TPanel;
    PanelTree: TPanel;
    PanelOptions: TPanel;
    SplitterSep: TSplitter;
    TreeViewCategories: TTreeView;
    LabelSelectPage: TLabel;
    LabelHomePage: TLabel;
    procedure LabelHomePageClick(Sender: TObject);
    procedure TreeViewCategoriesChange(Sender: TObject; Node: TTreeNode);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FSettings: TJclOTASettings;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddPage(AControl: TControl; PageName: string;
      Expert: IJclOTAOptionsCallback);
    function Execute(PageName: string): Boolean;
    property Settings: TJclOTASettings read FSettings;
  end;

implementation

{$R *.dfm}

uses
  ShellApi,
  JclOtaConsts, JclOtaResources;

type
  TItemDataRec = class
  public
    AControl: TControl;
    Expert: IJclOTAOptionsCallback;
  end;

//=== TJclOtaOptionsForm =====================================================

procedure TJclOtaOptionsForm.AddPage(AControl: TControl; PageName: string;
  Expert: IJclOTAOptionsCallback);
var
  ParentNode, ChildNode: TTreeNode;
  NodeName: string;
  PosSeparator, Index: Integer;
  AItemDataRec: TItemDataRec;
begin
  ParentNode := TreeViewCategories.Items.GetFirstNode;
  ChildNode := ParentNode;

  repeat
    PosSeparator := Pos('\', PageName);
    if PosSeparator > 0 then
    begin
      NodeName := Copy(PageName, 1, PosSeparator - 1);
      PageName := Copy(PageName, PosSeparator + 1, Length(PageName) - PosSeparator);
      while Assigned(ChildNode) and (CompareText(NodeName, ChildNode.Text) <> 0) do
        ChildNode := ChildNode.getNextSibling;
      if not Assigned(ChildNode) then
      begin
        ChildNode := TreeViewCategories.Items.AddChild(ParentNode, NodeName);
        if Assigned(ParentNode) then
          ParentNode.Expand(False);
      end;
      ParentNode := ChildNode;
    end
    else
    begin
      while Assigned(ParentNode) and (CompareText(NodeName, ParentNode.Text) <> 0) do
        ParentNode := ParentNode.getNextSibling;
    end;
  until PosSeparator = 0;

  ChildNode := nil;
  if Assigned(ParentNode) then
    for Index := 0 to ParentNode.Count - 1 do
      if CompareText(ParentNode.Item[Index].Text, PageName) = 0 then
        ChildNode := ParentNode.Item[Index];

  if not Assigned(ChildNode) then
  begin
    ChildNode := TreeViewCategories.Items.AddChild(ParentNode, PageName);
    if Assigned(ParentNode) then
      ParentNode.Expand(False);
  end;

  AControl.Parent := PanelOptions;
  AControl.SetBounds(8, 8, PanelOptions.ClientWidth - 16, PanelOptions.ClientHeight - 16);
  AControl.Visible := False;

  AItemDataRec := TItemDataRec.Create;
  AItemDataRec.AControl := AControl;
  AItemDataRec.Expert := Expert;
  ChildNode.Data := Pointer(AItemDataRec);
end;

constructor TJclOtaOptionsForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSettings := TJclOTASettings.Create(JclConfigurationSettings);
end;

procedure TJclOtaOptionsForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  // Fixing the Window Ghosting "bug"
  Params.Style := params.Style or WS_POPUP;
  if Assigned(Screen.ActiveForm) then
    Params.WndParent := Screen.ActiveForm.Handle
  else if Assigned (Application.MainForm) then
    Params.WndParent := Application.MainForm.Handle
  else
    Params.WndParent := Application.Handle;
end;

destructor TJclOtaOptionsForm.Destroy;
begin
  FreeAndNil(FSettings);
  inherited Destroy;
end;

function TJclOtaOptionsForm.Execute(PageName: string): Boolean;
var
  ATreeNode: TTreeNode;
  AItemDataRec: TItemDataRec;
begin
  // TODO: use PageName
  ATreeNode := TreeViewCategories.Items.GetFirstNode;
  if Assigned(ATreeNode) then
    TreeViewCategories.Selected := ATreeNode;

  Result := ShowModal = mrOk;

  ATreeNode := TreeViewCategories.Items.GetFirstNode;
  while Assigned(ATreeNode) do
  begin
    AItemDataRec := TItemDataRec(ATreeNode.Data);
    if Assigned(AItemDataRec) then
    begin
      AItemDataRec.Expert.ConfigurationClosed(AItemDataRec.AControl, Result);
      AItemDataRec.Free;
    end;
    ATreeNode := ATreeNode.GetNext;
  end;
end;

procedure TJclOtaOptionsForm.FormCreate(Sender: TObject);
begin
  Caption := RsConfigurationCaption;
  ButtonOk.Caption := RsOk;
  ButtonCancel.Caption := RsCancel;
  LabelSelectPage.Caption := RsSelectPage;
  LabelHomePage.Caption := RsHomePage;

  SetBounds(Settings.LoadInteger(JclLeft, Left),
            Settings.LoadInteger(JclTop, Top),
            Settings.LoadInteger(JclWidth, Width),
            Settings.LoadInteger(JclHeight, Height));
  PanelTree.Width := Settings.LoadInteger(JclPanelTreeWidth, PanelTree.Width);
end;

procedure TJclOtaOptionsForm.FormDestroy(Sender: TObject);
begin
  Settings.SaveInteger(JclLeft, Left);
  Settings.SaveInteger(JclTop, Top);
  Settings.SaveInteger(JclWidth, Width);
  Settings.SaveInteger(JclHeight, Height);
  Settings.SaveInteger(JclPanelTreeWidth, PanelTree.Width);
end;

procedure TJclOtaOptionsForm.LabelHomePageClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', 'http://jcl.sf.net/', '', '', SW_SHOW);
end;

procedure TJclOtaOptionsForm.TreeViewCategoriesChange(Sender: TObject;
  Node: TTreeNode);
var
  Index: Integer;
  AControl: TControl;
begin
  if Assigned(Node.Data) then
    AControl := TItemDataRec(Node.Data).AControl
  else
    AControl := LabelSelectPage;
  for Index := 0 to PanelOptions.ControlCount - 1 do
    PanelOptions.Controls[Index].Visible := PanelOptions.Controls[Index] = AControl;
end;

end.