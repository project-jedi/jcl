program ContainerPerformance;

{$I jcl.inc}

uses
  {$IFDEF MSWINDOWS}
  Forms,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  QForms,
  {$ENDIF LINUX}
  ContainerPerformanceMain in 'ContainerPerformanceMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
