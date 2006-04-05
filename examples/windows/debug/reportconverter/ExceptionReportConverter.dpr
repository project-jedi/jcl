program ExceptionReportConverter;

uses
  Forms,
  formConverter in 'formConverter.pas' {frmConverter},
  ExceptDlgMail in '..\..\..\..\EXPERTS\DEBUG\DIALOG\EXCEPTDLGMAIL.pas' {ExceptionDialogMail};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Exception Report Converter';
  Application.CreateForm(TfrmConverter, frmConverter);
  Application.Run;
end.
