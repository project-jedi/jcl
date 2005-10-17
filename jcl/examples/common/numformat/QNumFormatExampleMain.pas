unit QNumFormatExampleMain;

interface

uses
  Math, SysUtils, Classes,
  Qt, QGraphics, QControls, QForms, QStdCtrls, QComCtrls,
  JclSysUtils;

type
  TMainForm = class(TForm)
    ValueEdit: TEdit;
    EvalBtn: TButton;
    Label1: TLabel;
    RandBtn: TButton;
    PrecisionEdit: TSpinEdit;
    Label2: TLabel;
    Output: TMemo;
    BlockSeparatorSelector: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    BlockSizeEdit: TSpinEdit;
    cbShowPlusSign: TCheckBox;
    Label5: TLabel;
    ExpDivisionEdit: TSpinEdit;
    WidthEdit: TSpinEdit;
    Label6: TLabel;
    cbZeroPadding: TCheckBox;
    Label7: TLabel;
    MultiplierSelector: TComboBox;
    Label8: TLabel;
    FractionDigitsEdit: TSpinEdit;
    procedure EvalBtnClick(Sender: TObject);
    procedure RandBtnClick(Sender: TObject);
    procedure ValueEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BlockSizeEditChanged(Sender: TObject; NewValue: Integer);
    procedure BlockSeparatorSelectorChange(Sender: TObject);
    procedure PrecisionEditChanged(Sender: TObject; NewValue: Integer);
    procedure cbShowPlusSignClick(Sender: TObject);
    procedure ExpDivisionEditChanged(Sender: TObject; NewValue: Integer);
    procedure WidthEditChanged(Sender: TObject; NewValue: Integer);
    procedure cbZeroPaddingClick(Sender: TObject);
    procedure MultiplierSelectorChange(Sender: TObject);
    procedure FractionDigitsEditChanged(Sender: TObject;
      NewValue: Integer);
  private
    { Private declarations }
    FNumFormat: TJclNumericFormat;
    procedure Display;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.xfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FNumFormat := TJclNumericFormat.Create;
  FNumFormat.ExponentDivision := ExpDivisionEdit.Value;
  FNumFormat.NumberOfFractionalDigits := FractionDigitsEdit.Value;
  FNumFormat.Width := WidthEdit.Value;
  FNumFormat.DigitBlockSize := BlockSizeEdit.Value;
  BlockSeparatorSelector.Items[0] := FNumFormat.DigitBlockSeparator;
  Display;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FNumFormat.Free;
end;

procedure TMainForm.Display;
var
  Base: TNumericSystemBase;
  S, Mantissa: string;
  Exponent: Integer;
  X: Extended;
  C: TCaretPos;
begin
  if not Assigned(FNumFormat) then
    Exit;
  X := StrToFloat(ValueEdit.Text);
  FNumFormat.Precision := PrecisionEdit.Value;
  Output.Lines.BeginUpdate;
  try
    Output.Lines.Clear;
    for Base := Low(TNumericSystemBase) to High(TNumericSystemBase) do
    begin
      FNumFormat.Base := Base;
      FNumFormat.GetMantissaExp(X, Mantissa, Exponent);
      if Exponent = 0 then
        S := Mantissa
      else
        S := Format('%s %s %d^%d', [Mantissa, FNumFormat.Multiplier, Base, Exponent]);
      Output.Lines.Add(Format('Base %2d: %s', [Base, S]));
    end;
    C.Line := 0;
    C.Col := 0;
    Output.CaretPos := C;
  finally
    Output.Lines.EndUpdate;
  end;
end;

procedure TMainForm.EvalBtnClick(Sender: TObject);
begin
  Display;
end;

procedure TMainForm.RandBtnClick(Sender: TObject);
begin
  ValueEdit.Text := FloatToStr(Power(Random * 4 -2, Random(400)));
  EvalBtn.Enabled := False;
  Display;
end;

procedure TMainForm.ValueEditChange(Sender: TObject);
begin
  EvalBtn.Enabled := True;
end;

procedure TMainForm.BlockSizeEditChanged(Sender: TObject; NewValue: Integer);
begin
  if Assigned(FNumFormat) then
  begin
    FNumFormat.DigitBlockSize := BlockSizeEdit.Value;
    Display;
  end;
end;

procedure TMainForm.BlockSeparatorSelectorChange(Sender: TObject);
begin
  if Assigned(FNumFormat) then
  begin
    FNumFormat.DigitBlockSeparator := AnsiChar(BlockSeparatorSelector.Text[1]);
    Display;
  end;
end;

procedure TMainForm.PrecisionEditChanged(Sender: TObject; NewValue: Integer);
begin
  if Assigned(FNumFormat) then
  begin
    FNumFormat.Precision := PrecisionEdit.Value;
    Display;
  end;
end;

procedure TMainForm.cbShowPlusSignClick(Sender: TObject);
begin
  if Assigned(FNumFormat) then
  begin
    FNumFormat.ShowPositiveSign := cbShowPlusSign.Checked;
    Display;
  end;
end;

procedure TMainForm.ExpDivisionEditChanged(Sender: TObject;
  NewValue: Integer);
begin
  if Assigned(FNumFormat) then
  begin
    FNumFormat.ExponentDivision := ExpDivisionEdit.Value;
    Display;
  end;
end;

procedure TMainForm.WidthEditChanged(Sender: TObject; NewValue: Integer);
begin
  if Assigned(FNumFormat) then
  begin
    FNumFormat.Width := WidthEdit.Value;
    Display;
  end;
end;

procedure TMainForm.cbZeroPaddingClick(Sender: TObject);
begin
  if Assigned(FNumFormat) then
  begin
    if cbZeroPadding.Checked then
      FNumFormat.PaddingChar := '0'
    else
      FNumFormat.PaddingChar := ' ';
    Display;
  end;
end;

procedure TMainForm.MultiplierSelectorChange(Sender: TObject);
begin
  if Assigned(FNumFormat) then
  begin
    FNumFormat.Multiplier := MultiplierSelector.Text;
    Display;
  end;
end;

procedure TMainForm.FractionDigitsEditChanged(Sender: TObject;
  NewValue: Integer);
begin
  if Assigned(FNumFormat) then
  begin
    FNumFormat.NumberOfFractionalDigits := FractionDigitsEdit.Value;
    Display;
  end;
end;

initialization
  Randomize;
end.
