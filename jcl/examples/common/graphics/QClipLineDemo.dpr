program QClipLineDemo;

{$I jcl.inc}

uses
  QStyle,
  QForms,
  QClipLineDemoMain in 'QClipLineDemoMain.pas' {Form1};

{$R *.res}
{$R ..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
