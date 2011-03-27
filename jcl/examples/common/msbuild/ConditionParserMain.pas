unit ConditionParserMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormMain = class(TForm)
    LabelProperties: TLabel;
    MemoProperties: TMemo;
    LabelConditions: TLabel;
    MemoConditions: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  JclMsBuild;

procedure TFormMain.FormCreate(Sender: TObject);
  procedure Evaluate(const Condition: string; Defines: TStrings);
  begin
    MemoConditions.Lines.Add('"' + Condition + '"  =  ' + BoolToStr(ParseCondition(Condition, Defines), True));
  end;
var
  Defines: TStringList;
begin
  Defines := TStringList.Create;
  try
    Defines.CaseSensitive := False;
    Defines.Assign(MemoProperties.Lines);
    Evaluate('''$(HUNDRED)'' == 99', Defines);
    Evaluate('''$(HUNDRED)'' == ''0x$(zero)64''', Defines);
    Evaluate('''$(HUNDRED)'' != 99', Defines);
    Evaluate('''$(HUNDRED)'' != 100', Defines);
    Evaluate('''$(HUNDRED)'' < 100', Defines);
    Evaluate('''$(HUNDRED)'' < 101', Defines);
    Evaluate('''$(HUNDRED)'' > 100', Defines);
    Evaluate('''$(HUNDRED)'' > 99', Defines);
    Evaluate('(99 < ''$(HUNDRED)'') and (''$(HUNDRED)'' < 101)', Defines);
    Evaluate('(99 < ''$(HUNDRED)'') and (''$(HUNDRED)'' < 100)', Defines);
    Evaluate('(100 < ''$(HUNDRED)'') and (''$(HUNDRED)'' < 101)', Defines);
    Evaluate('Exists(''' + Application.ExeName + ''')', Defines);
    Evaluate('!Exists(''' + Application.ExeName + ''')', Defines);
    Evaluate('Exists(toto)', Defines);
    Evaluate('HasTrailingSlash(toto)', Defines);
    Evaluate('!HasTrailingSlash(toto)', Defines);
    Evaluate('HasTrailingSlash(''toto\'')', Defines);
  finally
    Defines.Free;
  end;
end;

end.
