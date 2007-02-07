program FileSummaryExample;

uses
  Forms,
  FileSummaryDemoMain in 'FileSummaryDemoMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
