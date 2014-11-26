program ExprEvalExampleFPC;

{$MODE Delphi}

uses
  Forms, Interfaces,
  ExprEvalExampleMain in 'ExprEvalExampleMain.pas' {ExprEvalForm},
  ExprEvalExampleLogic in 'ExprEvalExampleLogic.pas';


{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TExprEvalForm, ExprEvalForm);
  Application.Run;
end.
