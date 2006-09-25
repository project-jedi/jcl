program ContainerPerformance;

{$I jcl.inc}

uses
  {$IFDEF MSWINDOWS}
  Forms,
  {$ENDIF MSWINDOWS}
  {$IFDEF KYLIX}
  QForms,
  {$ENDIF KYLIX}
  ContainerPerformanceMain in 'ContainerPerformanceMain.pas' {MainForm},
  ContainerPerformanceTests in 'ContainerPerformanceTests.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
