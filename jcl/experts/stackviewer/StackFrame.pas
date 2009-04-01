unit StackFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ComCtrls, JclDebug, StackViewUnit, StackCodeUtils;

type
  TfrmStack = class(TFrame)
    lv: TListView;
    procedure lvDblClick(Sender: TObject);
    procedure lvChange(Sender: TObject; Item: TListItem; Change: TItemChange);
  private
    { Private declarations }
    FStackList: TStackViewItemsList;
    FOnSelectStackLine: TNotifyEvent;
    procedure DoSelectStackLine;
    procedure SetStackList(const Value: TStackViewItemsList);
    function GetSelected: TStackViewItem;
  public
    { Public declarations }
    property StackList: TStackViewItemsList read FStackList write SetStackList;
    property Selected: TStackViewItem read GetSelected;
    property OnSelectStackLine: TNotifyEvent read FOnSelectStackLine write FOnSelectStackLine;
  end;

implementation

{$R *.dfm}

{ TfrmStack }

procedure TfrmStack.DoSelectStackLine;
begin
  if Assigned(FOnSelectStackLine) then
    FOnSelectStackLine(Self);
end;

function TfrmStack.GetSelected: TStackViewItem;
begin
  if Assigned(lv.Selected) and Assigned(lv.Selected.Data) and (TObject(lv.Selected.Data) is TStackViewItem) then
    Result := TStackViewItem(lv.Selected.Data)
  else
    Result := nil;
end;

procedure TfrmStack.lvChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  DoSelectStackLine;
end;

procedure TfrmStack.lvDblClick(Sender: TObject);
begin
  JumpToCode(Selected);
end;

procedure TfrmStack.SetStackList(const Value: TStackViewItemsList);
var
  I: Integer;
  ListItem: TListItem;
  S: string;
begin
  FStackList := Value;

  lv.Items.BeginUpdate;
  try
    lv.Items.Clear;
    if Assigned(FStackList) then
      for I := 0 to FStackList.Count - 1 do
      begin
        ListItem := lv.Items.Add;
        ListItem.Caption := FStackList[I].ModuleName;
        ListItem.SubItems.Add(FStackList[I].SourceUnitName);
        ListItem.SubItems.Add(FStackList[I].ProcedureName);
        ListItem.SubItems.Add(FStackList[I].SourceName);
        if FStackList[I].LineNumber > 0 then
          S := IntToStr(FStackList[I].LineNumber)
        else
          S := '';
        ListItem.SubItems.Add(S);
        if lievProcedureStartLocationInfo in FStackList[I].Values then
          S := IntToStr(FStackList[I].LineNumberOffsetFromProcedureStart)
        else
          S := '';
        ListItem.SubItems.Add(S);
        if FStackList[I].ProjectName <> '' then
          S := ExtractFileName(FStackList[I].ProjectName)
        else
          S := ExtractFileName(FStackList[I].FileName);
        ListItem.SubItems.Add(S);
        if FStackList[I].TranslatedLineNumber > 0 then
          S := IntToStr(FStackList[I].TranslatedLineNumber)
        else
          S := '';
        ListItem.SubItems.Add(S);
        ListItem.Data := FStackList[I];
      end;
  finally
    lv.Items.EndUpdate;
  end;
end;

end.
