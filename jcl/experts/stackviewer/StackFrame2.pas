unit StackFrame2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JclDebug, StackViewUnit, StackCodeUtils;

type
  TfrmStack2 = class(TFrame)
    lbStack: TListBox;
    procedure lbStackDblClick(Sender: TObject);
    procedure lbStackDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure lbStackMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
  private
    { Private declarations }
    FStackList: TStackViewItemsList;
    procedure SetStackList(const Value: TStackViewItemsList);
    function GetSelected: TStackViewItem;
  public
    { Public declarations }
    property StackList: TStackViewItemsList read FStackList write SetStackList;
    property Selected: TStackViewItem read GetSelected;
  end;

implementation

{$R *.dfm}

function TfrmStack2.GetSelected: TStackViewItem;
begin
  if (lbStack.ItemIndex <> -1) and (lbStack.Items.Objects[lbStack.ItemIndex] is TStackViewItem) then
    Result := TStackViewItem(lbStack.Items.Objects[lbStack.ItemIndex])
  else
    Result := nil;
end;

procedure TfrmStack2.lbStackDblClick(Sender: TObject);
begin
  JumpToCode(Selected);
end;

procedure TfrmStack2.lbStackDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  ListboxCanvas: TCanvas;
  StackItem: TStackViewItem;
  BkgColor: TColor;
  S: string;
  P: Integer;
begin
  ListboxCanvas := TListBox(Control).Canvas;
  if TListBox(Control).Items.Objects[Index] is TStackViewItem then
  begin
    StackItem := TStackViewItem(TListBox(Control).Items.Objects[Index]);
    S := StackItem.AsString;
    P := Pos(']', S);
    if P > 0 then
    begin
      Delete(S, P + 1, 1);
      Insert(#13#10, S, P + 1);
    end;
    if StackItem.FoundFile then
      BkgColor := clMoneyGreen
    else
      BkgColor := clWindow;
  end
  else
  begin
    S := TListBox(Control).Items[Index];
    BkgColor := clWindow;
  end;
  if odSelected in State then
    ListboxCanvas.Brush.Color := clHighlight
  else
    ListboxCanvas.Brush.Color := BkgColor;
  ListboxCanvas.FillRect(Rect);
  Rect.Left := Rect.Left + 2;
  ListboxCanvas.TextRect(Rect, S);
end;

procedure TfrmStack2.lbStackMeasureItem(Control: TWinControl; Index: Integer;
  var Height: Integer);
var
  TextHeight: Integer;
begin
  TextHeight := TListBox(Control).Canvas.TextHeight('Ay');
  Height := TextHeight * 2 + 1;
end;

procedure TfrmStack2.SetStackList(const Value: TStackViewItemsList);
var
  I: Integer;
begin
  FStackList := Value;
  lbStack.Items.BeginUpdate;
  try
    lbStack.Items.Clear;
    if Assigned(FStackList) then
      for I := 0 to FStackList.Count - 1 do
        lbStack.Items.AddObject(FStackList[I].SourceName, FStackList[I]);
  finally
    lbStack.Items.EndUpdate;
  end;
end;

end.
