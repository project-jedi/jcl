program FileSearchDemo;

uses
  QForms,
  FileSearchDemoMain in 'FileSearchDemoMain.pas' {FileSearchForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFileSearchForm, FileSearchForm);
  Application.Run;
end.
