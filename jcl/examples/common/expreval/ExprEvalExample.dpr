program ExprEvalExample;

uses
  Forms,
  ExprEvalExampleMain in 'ExprEvalExampleMain.pas' {Form1},
  JclExprEval in '..\..\..\source\common\JclExprEval.pas',
  JclStrHashMap in '..\..\..\source\common\JclStrHashMap.pas',
  ExprEvalExampleLogic in 'ExprEvalExampleLogic.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
