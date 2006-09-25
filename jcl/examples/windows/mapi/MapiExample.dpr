program MapiExample;

uses
  Forms,
  MapiDemoMain in 'MapiDemoMain.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
