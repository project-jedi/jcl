program PCREDemo;

uses
  Forms,
  PCREDemoMain in 'PCREDemoMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'JclPCRE Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
