program ThreadExceptExample;

uses
  Forms,
  JclIdeThreadStatus in '..\..\debugextension\threadnames\JclIdeThreadStatus.pas',
  ThreadExpertSharedNames in '..\..\debugextension\threadnames\ThreadExpertSharedNames.pas',
  ThreadExceptDemoMain in 'ThreadExceptDemoMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
