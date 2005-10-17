program QFileSearchDemo;

uses
  QForms,
  QFileSearchDemoMain in 'QFileSearchDemoMain.pas' {FileSearchForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFileSearchForm, FileSearchForm);
  Application.Run;
end.
