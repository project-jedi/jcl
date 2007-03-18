program ScreenJPG;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  About in '..\Common\About.pas' {AboutBox},
  ToolsUtils in '..\Common\ToolsUtils.pas',
  ExceptDlg in '..\..\..\..\experts\debug\dialog\ExceptDlg.pas' {ExceptionDialog};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'ScreenJPG';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
