program NumFormatExample;

uses
  QForms,
  NumFormatExampleMain in 'NumFormatExampleMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
