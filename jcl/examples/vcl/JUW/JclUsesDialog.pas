unit JclUsesDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ImgList;

type
  TFormUsesConfirm = class(TForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    TreeImages: TImageList;
    TreeViewChanges: TTreeView;
    procedure ButtonOKClick(Sender: TObject);
    procedure TreeViewChangesKeyPress(Sender: TObject; var Key: Char);
    procedure TreeViewChangesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FChangeList: TStrings;
    FErrors: TList;
    function ToggleNode(Node: TTreeNode): Boolean;
  public
    constructor Create(AOwner: TComponent; AChangeList: TStrings; Errors: TList); reintroduce;
  end;

implementation

uses
  CommCtrl,
  JclUsesWizard;

{$R *.dfm}

resourcestring
  SActionSkip = 'Skip';
  SActionAdd = 'Add';
  SActionMove = 'Move';
  SSectionImpl = 'to implementation uses';
  SSectionIntf = 'to interface uses';
  SUndeclIdent = '[Error] %s(%d) Undeclared identifier: ''%s''';
  SConfirmChanges = '%s: Confirm changes';

//----------------------------------------------------------------------------
{ TFormUsesConfirm private }
//----------------------------------------------------------------------------

function TFormUsesConfirm.ToggleNode(Node: TTreeNode): Boolean;
begin
  if Node.ImageIndex = 0 then
  begin
    Node.Imageindex := 1;
    Node.SelectedIndex := 1;
    Result := True;
  end
  else
  if Node.ImageIndex = 1 then
  begin
    Node.ImageIndex := 0;
    Node.SelectedIndex := 0;
    Result := True;
  end
  else
    Result := False;
end;

//----------------------------------------------------------------------------
{ TFormUsesConfirm public }
//----------------------------------------------------------------------------

constructor TFormUsesConfirm.Create(AOwner: TComponent; AChangeList: TStrings; Errors: TList);
const
  ActionStrings: array [TWizardAction] of string =
    (SActionSkip, SActionAdd, SActionAdd, SActionMove);
  SectionStrings: array [TWizardAction] of string =
    ('', SSectionImpl, SSectionIntf, SSectionIntf);
var
  I, J: Integer;
  Node: TTreeNode;
begin
  inherited Create(AOwner);
  FChangeList := AChangeList;
  FErrors := Errors;
  for I := 0 to FChangeList.Count - 1 do
  begin
    Node := TreeViewChanges.Items.AddChildObject(nil, Format('%d. %s %s %s',
      [I + 1, ActionStrings[TWizardAction(FChangeList.Objects[I])], FChangeList[I],
      SectionStrings[TWizardAction(FChangeList.Objects[I])]]), Pointer(I));
    for J := 0 to FErrors.Count - 1 do
      with PErrorInfo(FErrors[J])^ do
        if AnsiCompareText(UsesName, FChangeList[I]) = 0 then
          with TreeViewChanges.Items.AddChild(Node, Format(SUndeclIdent,
            [UnitName, LineNumber, Identifier, UsesName])) do
          begin
            ImageIndex := -1;
            SelectedIndex := -1;
          end;
    case TWizardAction(FChangeList.Objects[I]) of
      waSkip:
        Node.ImageIndex := 0;
      else
        Node.ImageIndex := 1;
    end;
    Node.SelectedIndex := Node.ImageIndex;

    Node.Expand(True);
  end;
  if FErrors.Count > 0 then
    with PErrorInfo(FErrors[0])^ do
      Caption := Format(SConfirmChanges, [UnitName]);
end;

//----------------------------------------------------------------------------
{ TFormUsesConfirm event handlers }
//----------------------------------------------------------------------------

procedure TFormUsesConfirm.ButtonOKClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  with TreeViewChanges do
  begin
    Node := Items.GetFirstNode;
    while Assigned(Node) do
    begin
      if Node.ImageIndex = 0 then
        FChangeList.Objects[Integer(Node.Data)] := TObject(waSkip);
      Node := Node.GetNextSibling;
    end;
  end;
end;

//----------------------------------------------------------------------------

procedure TFormUsesConfirm.TreeViewChangesKeyPress(Sender: TObject; var Key: Char);
var
  Node: TTreeNode;
begin
  if Key = ' ' then
  begin
    Node := TreeViewChanges.Selected;
    if Assigned(Node) then
    begin
      if Node.Level > 0 then
        Node := Node.Parent;
      ToggleNode(Node);
      Key := #0;
    end;
  end;
end;

//----------------------------------------------------------------------------

procedure TFormUsesConfirm.TreeViewChangesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  with TreeViewChanges do
    if htOnIcon in GetHitTestInfoAt(X, Y) then
    begin
      Node := GetNodeAt(X, Y);
      if Assigned(Node) then
        ToggleNode(Node);
    end;
end;

end.
