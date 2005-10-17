program SysInfoExample;

uses
  Forms,
  SysInfoDemoMain in 'SysInfoDemoMain.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
