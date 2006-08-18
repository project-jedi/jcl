program ResFix;

uses
  Forms,
  ResFixMain in 'ResFixMain.pas' {MainForm},
  About in '..\Common\About.pas' {AboutBox},
  ToolsUtils in '..\Common\ToolsUtils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'ResFix';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
