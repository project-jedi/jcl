program QPCREDemo;

uses
  QForms,
  QPCREDemoMain in 'QPCREDemoMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JclPCRE Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
