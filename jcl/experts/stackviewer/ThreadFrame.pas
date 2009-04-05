unit ThreadFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, IniFiles, JclDebugStackUtils, StackViewUnit, StackFrame, ExceptInfoFrame;

type
  TfrmThread = class(TFrame)
    pnlExceptInfo: TPanel;
    pnlCreationStack: TPanel;
    pnlStack: TPanel;
    splCreationStack: TSplitter;
  private
    FCreationStackFrame: TfrmStack;
    FExceptionFrame: TfrmException;
    FStackFrame: TfrmStack;
    FCreationStackList: TStackViewItemsList;
    FStackList: TStackViewItemsList;
    FException: TException;
    FLastStackFrame: TObject;
    FCreationStackHeight: Integer;
    procedure SaveSplitterState;
    procedure SetCreationStackList(const Value: TStackViewItemsList);
    procedure SetException(const Value: TException);
    procedure SetStackList(const Value: TStackViewItemsList);
    function GetSelected: TStackViewItem;
    procedure HandleStackSelection(ASender: TObject);
    procedure UpdateSplitterState;
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure LoadState(AIni: TCustomIniFile; const ASection: string);
    procedure SaveState(AIni: TCustomIniFile; const ASection: string);
    property CreationStackList: TStackViewItemsList read FCreationStackList write SetCreationStackList;
    property Exception: TException read FException write SetException;
    property StackList: TStackViewItemsList read FStackList write SetStackList;
    property Selected: TStackViewItem read GetSelected;
  end;

implementation

{$R *.dfm}

{ TfrmThread }

constructor TfrmThread.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCreationStackFrame := TfrmStack.Create(Self);
  FCreationStackFrame.Name := 'ThreadCreationStackFrame';
  FCreationStackFrame.Parent := pnlCreationStack;
  FCreationStackFrame.Align := alClient;
  FCreationStackFrame.OnSelectStackLine := HandleStackSelection;
  FExceptionFrame := TfrmException.Create(Self);
  FExceptionFrame.Parent := pnlExceptInfo;
  FExceptionFrame.Align := alClient;
  FStackFrame := TfrmStack.Create(Self);
  FStackFrame.Name := 'ThreadStackFrame';
  FStackFrame.Parent := pnlStack;
  FStackFrame.Align := alClient;
  FStackFrame.OnSelectStackLine := HandleStackSelection;
  FCreationStackHeight := pnlCreationStack.Height;
  FLastStackFrame := nil;
end;

function TfrmThread.GetSelected: TStackViewItem;
begin
  if (FLastStackFrame = FStackFrame) and FStackFrame.Visible and Assigned(FStackFrame.Selected) then
    Result := FStackFrame.Selected
  else
  if (FLastStackFrame = FCreationStackFrame) and FCreationStackFrame.Visible and Assigned(FCreationStackFrame.Selected) then
    Result := FCreationStackFrame.Selected
  else
    Result := nil;
end;

procedure TfrmThread.HandleStackSelection(ASender: TObject);
begin
  FLastStackFrame := ASender;
end;

procedure TfrmThread.LoadState(AIni: TCustomIniFile; const ASection: string);
begin
  FCreationStackHeight := AIni.ReadInteger(ASection, 'CreationStackFrameHeight', FCreationStackHeight);
  UpdateSplitterState;
  FStackFrame.LoadState(AIni, ASection, 'StackFrameThread');
  FCreationStackFrame.LoadState(AIni, ASection, 'CreationStackFrameThread');
end;

procedure TfrmThread.SaveSplitterState;
begin
  if pnlStack.Visible and pnlCreationStack.Visible then
    FCreationStackHeight := pnlCreationStack.Height;
end;

procedure TfrmThread.SaveState(AIni: TCustomIniFile; const ASection: string);
begin
  SaveSplitterState;
  AIni.WriteInteger(ASection, 'CreationStackFrameHeight', FCreationStackHeight);
  FStackFrame.SaveState(AIni, ASection, 'StackFrameThread');
  FCreationStackFrame.SaveState(AIni, ASection, 'CreationStackFrameThread');
end;

procedure TfrmThread.SetCreationStackList(const Value: TStackViewItemsList);
begin
  FCreationStackList := Value;
  FCreationStackFrame.StackList := FCreationStackList;
  SaveSplitterState;
  pnlCreationStack.Visible := Assigned(FCreationStackList);
  UpdateSplitterState;
end;

procedure TfrmThread.SetException(const Value: TException);
begin
  FException := Value;
  FExceptionFrame.Exception := FException;
  pnlExceptInfo.Visible := Assigned(FException);
end;

procedure TfrmThread.SetStackList(const Value: TStackViewItemsList);
begin
  FStackList := Value;
  FStackFrame.StackList := FStackList;
  SaveSplitterState;
  pnlStack.Visible := Assigned(FStackList);
  UpdateSplitterState;
end;

procedure TfrmThread.UpdateSplitterState;
begin
  splCreationStack.Visible := pnlStack.Visible and pnlCreationStack.Visible;
  if splCreationStack.Visible then
  begin
    pnlCreationStack.Height := FCreationStackHeight;
    splCreationStack.Top := pnlCreationStack.Top - 1;
  end;
end;

end.
