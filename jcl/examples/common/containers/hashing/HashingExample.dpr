program HashingExample;

{$I jcl.inc}

uses
  {$IFDEF MSWINDOWS}
  Forms,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  QForms,
  {$ENDIF LINUX}
  HashingExampleMain in 'HashingExampleMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
