program QPCREDemo;

{$I jcl.inc}

uses
  QForms,
  QPCREDemoMain in 'QPCREDemoMain.pas' {frmMain};

{$R *.res}
{$R ..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.Title := 'JclPCRE Demo';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
