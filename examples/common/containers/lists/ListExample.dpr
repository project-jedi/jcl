program ListExample;

{$I jcl.inc}

uses
  {$IFDEF MSWINDOWS}
  Forms,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  QForms,
  {$ENDIF LINUX}
  ListExampleMain in 'ListExampleMain.pas' {MainForm},
  MyObjectList in 'MyObjectList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
