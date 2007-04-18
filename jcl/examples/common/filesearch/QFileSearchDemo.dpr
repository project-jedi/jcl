program QFileSearchDemo;

{$I jcl.inc}

uses
  QForms,
  QFileSearchDemoMain in 'QFileSearchDemoMain.pas' {FileSearchForm};

{$R *.res}
{$R ..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TFileSearchForm, FileSearchForm);
  Application.Run;
end.
