program HelpTool;

uses
  Forms,
  UnHelpTool in 'UnHelpTool.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Project JEDI';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
