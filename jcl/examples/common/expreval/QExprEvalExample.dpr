program QExprEvalExample;

uses
  QForms,
  JclExprEval in '..\..\..\source\common\JclExprEval.pas',
  JclStrHashMap in '..\..\..\source\common\JclStrHashMap.pas',
  ExprEvalExampleLogic in 'ExprEvalExampleLogic.pas',
  QExprEvalExampleMain in 'QExprEvalExampleMain.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
