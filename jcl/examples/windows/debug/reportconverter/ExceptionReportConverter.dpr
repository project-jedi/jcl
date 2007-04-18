program ExceptionReportConverter;

{$I jcl.inc}

uses
  Forms,
  formConverter in 'formConverter.pas' {frmConverter},
  ExceptDlgMail in '..\..\..\..\EXPERTS\DEBUG\DIALOG\EXCEPTDLGMAIL.pas' {ExceptionDialogMail};

{$R *.res}
{$R ..\..\..\..\source\windows\JclCommCtrlAsInvoker.res}

begin
  Application.Initialize;
  Application.Title := 'Exception Report Converter';
  Application.CreateForm(TfrmConverter, frmConverter);
  Application.Run;
end.
