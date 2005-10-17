program TextReaderExample;

uses
  Forms,
  TextReaderDemoMain in 'TextReaderDemoMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
