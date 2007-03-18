program MultiMediaExample;

uses
  Forms,
  MultimediaDemoMain in 'MultimediaDemoMain.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
