program AlgorithmsExample;

{$I jcl.inc}

uses
  {$IFDEF MSWINDOWS}
  Forms,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  QForms,
  {$ENDIF LINUX}
  AlgorithmsExampleMain in 'AlgorithmsExampleMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
