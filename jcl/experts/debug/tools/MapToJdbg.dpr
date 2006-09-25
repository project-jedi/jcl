program MapToJdbg;

uses
  Forms,
  MapToJdbgMain in 'MapToJdbgMain.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'MAP to JDBG';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
