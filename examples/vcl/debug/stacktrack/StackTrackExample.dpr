program StackTrackExample;

uses
  Forms,
  StackTrackDemoMain in 'StackTrackDemoMain.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
