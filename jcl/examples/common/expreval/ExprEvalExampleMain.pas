unit ExprEvalExampleMain;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  JclExprEval;

type
  TForm1 = class(TForm)
    ExpressionInput: TEdit;
    Memo1: TMemo;
    Label1: TLabel;
    EnterButton: TButton;
    FuncList: TComboBox;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EnterButtonClick(Sender: TObject);
    procedure FuncListClick(Sender: TObject);
  private
    { Private declarations }
    FEvaluator: TEasyEvaluator;
    FX: Extended;
    FY: Extended;
    FZ: Extended;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  ExprEvalExampleLogic;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FEvaluator := TEvaluator.Create;
  FEvaluator.AddVar('X', FX);
  FEvaluator.AddVar('Y', FY);
  FEvaluator.AddVar('Z', FZ);
  Init(FEvaluator, FuncList.Items);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FEvaluator.Free;
end;

procedure TForm1.EnterButtonClick(Sender: TObject);
begin
  Memo1.Lines.Add(ResultAsText(FEvaluator as TEvaluator, ExpressionInput.Text));
end;

procedure TForm1.FuncListClick(Sender: TObject);
begin
  ExpressionInput.Text := ExpressionInput.Text + FuncList.Text;
  ActiveControl := ExpressionInput;
  ExpressionInput.SelStart := Length(ExpressionInput.Text);
end;

end.
