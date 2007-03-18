program QNumFormatExample;

uses
  QForms,
  QNumFormatExampleMain in 'QNumFormatExampleMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
