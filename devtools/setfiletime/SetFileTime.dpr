program SetFileTime;

uses
  Forms,
  SetFileTimeMain in 'SetFileTimeMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Set files time';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
