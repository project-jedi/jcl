program ThreadExceptExample;

uses
  Forms,
  ThreadExceptDemoMain in 'ThreadExceptDemoMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
