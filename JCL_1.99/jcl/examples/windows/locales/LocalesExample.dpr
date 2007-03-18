program LocalesExample;

uses
  Forms,
  LocalesDemoMain in 'LocalesDemoMain.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
