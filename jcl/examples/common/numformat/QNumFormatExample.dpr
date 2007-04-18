program QNumFormatExample;

{$I jcl.inc}

uses
  QForms,
  QNumFormatExampleMain in 'QNumFormatExampleMain.pas' {MainForm};

{$R *.res}
{$R ..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
